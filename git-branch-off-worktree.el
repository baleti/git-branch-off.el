;;; git-branch-off-worktree.el --- Create and delete worktrees  -*- lexical-binding: t; -*-

(require 'git-branch-off-log)

;;; Create worktree

(defvar-local git-branch-off--worktree-pick-source nil
  "Cons of (top . rel-path) for a pending worktree commit-pick in this log buffer.")

(define-minor-mode git-branch-off-worktree-pick-mode
  "Transient minor mode: RET picks a commit at point to create a detached worktree."
  :lighter nil
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "RET") #'git-branch-off--worktree-pick-commit)
            (define-key m (kbd "C-g") #'git-branch-off--worktree-pick-abort)
            m)
  (when (bound-and-true-p evil-local-mode)
    (evil-normalize-keymaps)))

(with-eval-after-load 'evil
  (evil-make-overriding-map git-branch-off-worktree-pick-mode-map))

(defun git-branch-off--worktree-pick-abort ()
  "Abort the pending worktree commit-pick."
  (interactive)
  (setq-local git-branch-off--worktree-pick-source nil)
  (git-branch-off-worktree-pick-mode -1)
  (setq-local header-line-format nil)
  (message "create-worktree: aborted"))

(defun git-branch-off--worktree-pick-commit ()
  "Pick the commit at point, create a detached worktree, and open the source file there."
  (interactive)
  (let ((commit (or (magit-section-value-if 'commit)
                    (user-error "No commit at point"))))
    (let ((source git-branch-off--worktree-pick-source))
      (setq-local git-branch-off--worktree-pick-source nil)
      (git-branch-off-worktree-pick-mode -1)
      (setq-local header-line-format nil)
      (git-branch-off--worktree-create commit (when source (cdr source))))))

(defun git-branch-off--worktree-create (commit &optional rel-file)
  "Create a detached worktree for COMMIT at .worktree/<full-hash> under the repo root.
Opens REL-FILE (path relative to repo root) in the new worktree when given;
otherwise opens dired at the worktree root.  Silently reuses an existing worktree."
  (let* ((top     (or (magit-toplevel) (user-error "Not in a git repository")))
         (full    (magit-git-string "rev-parse" commit))
         (wt-dir  (expand-file-name (concat ".worktree/" full) top)))
    (unless (file-exists-p wt-dir)
      (with-temp-buffer
        (let ((exit (call-process "git" nil t nil
                                  "worktree" "add" "--detach" wt-dir full)))
          (unless (= exit 0)
            (user-error "git worktree add --detach failed: %s"
                        (string-trim (buffer-string))))))
      (message "Created worktree at .worktree/%s" (substring full 0 8)))
    (if rel-file
        (find-file (expand-file-name rel-file wt-dir))
      (dired wt-dir))))

(defun git-branch-off-worktree-create ()
  "Create a detached worktree at .worktree/<commit-hash> for a selected commit.

Context-sensitive behaviour:
- magit-log: uses the commit at point, then opens dired at the worktree root.
- magit-revision: uses the buffer's revision, then opens dired at the worktree root.
- magit-blob: uses the buffer's revision and opens the same file in the worktree.
- File buffer: opens `git-branch-off-log' and enters pick mode — press RET on
  any commit to create the worktree and open that file at the same relative path."
  (interactive)
  (cond
   ((derived-mode-p 'magit-log-mode)
    (let ((commit (or (magit-section-value-if 'commit)
                      (user-error "No commit at point"))))
      (git-branch-off--worktree-create commit)))
   ((derived-mode-p 'magit-revision-mode)
    (let ((commit (or (and (bound-and-true-p magit-buffer-revision)
                           magit-buffer-revision)
                      (user-error "No revision in current buffer"))))
      (git-branch-off--worktree-create commit)))
   ((bound-and-true-p magit-blob-mode)
    (let* ((commit (or (and (bound-and-true-p magit-buffer-revision)
                            magit-buffer-revision)
                       (user-error "No revision in current buffer")))
           (top    (or (magit-toplevel) (user-error "Not in a git repository")))
           (rel    (file-relative-name (magit-buffer-file-name) top)))
      (git-branch-off--worktree-create commit rel)))
   (buffer-file-name
    (let* ((top (or (magit-toplevel) (user-error "Not in a git repository")))
           (rel (file-relative-name buffer-file-name top)))
      (git-branch-off-log)
      (let ((log-buf (or (magit-get-mode-buffer 'magit-log-mode)
                         (user-error "Could not find magit-log buffer"))))
        (with-current-buffer log-buf
          (setq-local git-branch-off--worktree-pick-source (cons top rel))
          (git-branch-off-worktree-pick-mode 1)
          (setq-local header-line-format
                      (list (format " Worktree ← %s — " rel)
                            (propertize "RET" 'face 'transient-key)
                            " create  "
                            (propertize "C-g" 'face 'transient-key)
                            " abort"))))))
   (t
    (user-error "Invoke from magit-log, magit-revision, magit-blob, or a file buffer"))))

;;; Delete worktree

(defface git-branch-off-worktree-delete-marked
  '((t :inherit magit-diff-removed :extend t))
  "Face for worktree entries marked for deletion.")

(defvar-local git-branch-off--worktree-delete-entries nil)
(defvar-local git-branch-off--worktree-delete-marks nil)
(defvar-local git-branch-off--worktree-delete-overlays nil)

(define-derived-mode git-branch-off-worktree-delete-mode special-mode "WTDelete"
  "Major mode for selecting git worktrees to remove."
  (setq truncate-lines t)
  (setq-local git-branch-off--worktree-delete-entries nil
              git-branch-off--worktree-delete-marks   nil
              git-branch-off--worktree-delete-overlays (make-hash-table :test #'equal)))

(with-eval-after-load 'evil
  (evil-make-overriding-map git-branch-off-worktree-delete-mode-map 'normal))

(defun git-branch-off--worktree-status (path)
  "Return a short status summary string for the worktree at PATH."
  (condition-case _
      (let ((lines (process-lines "git" "-C" path "status" "--short" "--ignore-submodules")))
        (if (null lines)
            "clean"
          (let ((staged 0) (modified 0) (untracked 0))
            (dolist (l lines)
              (when (>= (length l) 2)
                (let ((x (aref l 0)) (y (aref l 1)))
                  (cond ((char-equal x ??) (cl-incf untracked))
                        ((not (char-equal x ?\s)) (cl-incf staged))
                        ((not (char-equal y ?\s)) (cl-incf modified))))))
            (string-join
             (delq nil (list (when (> staged 0)    (format "%d staged"    staged))
                             (when (> modified 0)  (format "%d modified"  modified))
                             (when (> untracked 0) (format "%d untracked" untracked))))
             ", "))))
    (error "?")))

(defun git-branch-off--worktree-parse-raw ()
  "Return list of plists for all worktrees (path/head/branch/flags, no status check)."
  (let* ((top (or (magit-toplevel) (user-error "Not in a git repository")))
         (raw (with-temp-buffer
                (call-process "git" nil t nil "worktree" "list" "--porcelain")
                (buffer-string)))
         result)
    (dolist (block (split-string raw "\n\n" t))
      (let (path head branch detached locked)
        (dolist (line (split-string block "\n" t))
          (cond
           ((string-prefix-p "worktree " line) (setq path    (substring line 9)))
           ((string-prefix-p "HEAD "     line) (setq head    (substring line 5)))
           ((string-prefix-p "branch "   line) (setq branch  (string-remove-prefix
                                                                "refs/heads/"
                                                                (substring line 7))))
           ((string= "detached" (string-trim line)) (setq detached t))
           ((string-prefix-p "locked"    line) (setq locked  t))))
        (when path
          (push (list :path path :head head :branch branch :detached detached
                      :locked locked :current (file-equal-p path top)
                      :subject (when head
                                 (magit-git-string "log" "-1" "--format=%s" head)))
                result))))
    (nreverse result)))

(defun git-branch-off--worktree-gather ()
  "Return list of plists for all worktrees, including :status."
  (mapcar (lambda (e)
            (append e (list :status (git-branch-off--worktree-status
                                     (plist-get e :path)))))
          (git-branch-off--worktree-parse-raw)))

(defun git-branch-off--worktree-delete-mark-toggle ()
  "Toggle deletion mark on the worktree entry at point."
  (interactive)
  (let* ((idx   (get-text-property (point) 'git-branch-off-wt-idx))
         (entry (and (numberp idx) (nth idx git-branch-off--worktree-delete-entries)))
         (path  (plist-get entry :path)))
    (unless path (user-error "No worktree at point"))
    (when (plist-get entry :current)
      (user-error "Cannot delete the current worktree"))
    (let ((inhibit-read-only t)
          (bol (line-beginning-position))
          (eol (1+ (line-end-position))))
      (if (member path git-branch-off--worktree-delete-marks)
          (progn
            (setq git-branch-off--worktree-delete-marks
                  (delete path git-branch-off--worktree-delete-marks))
            (when-let ((ov (gethash path git-branch-off--worktree-delete-overlays)))
              (delete-overlay ov)
              (remhash path git-branch-off--worktree-delete-overlays))
            (save-excursion (goto-char bol) (delete-char 1) (insert " ")))
        (push path git-branch-off--worktree-delete-marks)
        (let ((ov (make-overlay bol eol)))
          (overlay-put ov 'face 'git-branch-off-worktree-delete-marked)
          (puthash path ov git-branch-off--worktree-delete-overlays))
        (save-excursion
          (goto-char bol)
          (delete-char 1)
          (insert (propertize "*" 'face 'git-branch-off-worktree-delete-marked)))))))

(defun git-branch-off--worktree-delete-confirm ()
  "Remove marked worktrees; prompts y/n before each with uncommitted changes."
  (interactive)
  (unless git-branch-off--worktree-delete-marks (user-error "No worktrees marked"))
  (let* ((entries  (copy-sequence git-branch-off--worktree-delete-entries))
         (paths    (copy-sequence git-branch-off--worktree-delete-marks))
         (approved (delq nil
                         (mapcar (lambda (path)
                                   (let* ((entry  (cl-find-if
                                                   (lambda (e)
                                                     (string= (plist-get e :path) path))
                                                   entries))
                                          (status (or (plist-get entry :status) "?")))
                                     (when (or (string= status "clean")
                                               (y-or-n-p
                                                (format "Worktree %s has changes (%s) — remove anyway? "
                                                        (file-name-nondirectory path) status)))
                                       path)))
                                 paths))))
    (if (null approved)
        (message "Nothing to remove.")
      (kill-buffer (current-buffer))
      (let (removed failed)
        (dolist (path approved)
          (with-temp-buffer
            (if (= 0 (call-process "git" nil t nil "worktree" "remove" path))
                (push path removed)
              (push (format "%s: %s" (file-name-nondirectory path)
                            (string-trim (buffer-string)))
                    failed))))
        (if failed
            (message "Removed %d; failed — %s" (length removed)
                     (string-join (nreverse failed) " | "))
          (message "Removed %d worktree(s)" (length removed)))))))

(defun git-branch-off--worktree-delete-abort ()
  "Abort the delete-worktree operation."
  (interactive)
  (kill-buffer (current-buffer))
  (message "delete-worktree: aborted"))

(let ((m git-branch-off-worktree-delete-mode-map))
  (define-key m (kbd "m")       #'git-branch-off--worktree-delete-mark-toggle)
  (define-key m (kbd "C-c C-c") #'git-branch-off--worktree-delete-confirm)
  (define-key m (kbd "C-g")     #'git-branch-off--worktree-delete-abort))

(defun git-branch-off--worktree-delete-interactive ()
  "Open the interactive worktree selection buffer."
  (let* ((entries (git-branch-off--worktree-gather))
         (buf     (get-buffer-create "*git-branch-off: delete worktrees*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (git-branch-off-worktree-delete-mode)
        (erase-buffer)
        (setq-local git-branch-off--worktree-delete-entries entries
                    git-branch-off--worktree-delete-marks   nil
                    git-branch-off--worktree-delete-overlays (make-hash-table :test #'equal))
        (setq header-line-format
              (list "  "
                    (propertize "m" 'face 'transient-key)
                    " mark  "
                    (propertize "C-c C-c" 'face 'transient-key)
                    " delete marked  "
                    (propertize "C-g" 'face 'transient-key)
                    " abort"))
        (cl-loop for entry in entries
                 for idx from 0 do
                 (let* ((path    (plist-get entry :path))
                        (branch  (plist-get entry :branch))
                        (head    (plist-get entry :head))
                        (det     (plist-get entry :detached))
                        (cur     (plist-get entry :current))
                        (locked  (plist-get entry :locked))
                        (status  (plist-get entry :status))
                        (ref-str (cond (branch branch)
                                       (det    (format "detached:%s" (substring head 0 8)))
                                       (t      "?")))
                        (tags    (string-join
                                  (delq nil (list (when cur    "current")
                                                  (when locked "locked")))
                                  " "))
                        (subject (plist-get entry :subject))
                        (start   (point)))
                   (insert " ")
                   (insert (propertize path 'face 'magit-filename))
                   (insert "  ")
                   (insert (propertize ref-str 'face
                                       (if det 'magit-hash 'magit-branch-local)))
                   (unless (string-empty-p tags)
                     (insert "  ")
                     (insert (propertize (format "[%s]" tags) 'face 'shadow)))
                   (insert "  ")
                   (insert (propertize status 'face
                                       (if (string= status "clean")
                                           'magit-dimmed
                                         'warning)))
                   (when (and subject (not (string-empty-p subject)))
                     (insert "  ")
                     (insert (propertize subject 'face 'magit-log-message)))
                   (add-text-properties start (point)
                                        (list 'git-branch-off-wt-idx idx))
                   (insert "\n")))
        (goto-char (point-min))))
    (display-buffer buf
                    '((display-buffer-at-bottom)
                      (window-height . (lambda (win)
                                         (fit-window-to-buffer win 20 5)))))))

(defun git-branch-off--worktree-delete-from-log ()
  "Delete worktrees for commits selected in magit-log.
Source priority: branch-off markers → visual selection → point."
  (let* ((full-hashes
          (cond
           ((bound-and-true-p git-branch-off--squash-marks)
            (copy-sequence git-branch-off--squash-marks))
           ((use-region-p)
            (mapcar (lambda (h) (magit-git-string "rev-parse" h))
                    (git-branch-off--squash-commits-in-region)))
           (t
            (when-let ((h (magit-section-value-if 'commit)))
              (list (magit-git-string "rev-parse" h))))))
         (_ (unless full-hashes (user-error "No commits selected")))
         (all-wt     (git-branch-off--worktree-parse-raw))
         (targets    (delq nil
                           (mapcar (lambda (full)
                                     (cl-find-if
                                      (lambda (e) (string= (plist-get e :head) full))
                                      all-wt))
                                   full-hashes)))
         (skipped    (cl-remove-if-not (lambda (e) (plist-get e :current)) targets))
         (candidates (cl-remove-if     (lambda (e) (plist-get e :current)) targets)))
    (when skipped
      (message "Skipping current worktree: %s"
               (mapconcat (lambda (e) (file-name-nondirectory (plist-get e :path)))
                          skipped ", ")))
    (unless candidates
      (user-error "No removable worktree found for the selected commit(s)"))
    (when (y-or-n-p
           (format "Remove worktree%s %s? "
                   (if (> (length candidates) 1) "s" "")
                   (mapconcat (lambda (e) (file-name-nondirectory (plist-get e :path)))
                              candidates " ")))
      (let (removed failed)
        (dolist (entry candidates)
          (let* ((path   (plist-get entry :path))
                 (status (git-branch-off--worktree-status path)))
            (when (or (string= status "clean")
                      (y-or-n-p
                       (format "Worktree %s has changes (%s) — remove anyway? "
                               (file-name-nondirectory path) status)))
              (with-temp-buffer
                (if (= 0 (call-process "git" nil t nil "worktree" "remove" path))
                    (push path removed)
                  (push (format "%s: %s" (file-name-nondirectory path)
                                (string-trim (buffer-string)))
                        failed))))))
        (magit-refresh)
        (if failed
            (message "Removed %d; failed — %s" (length removed)
                     (string-join (nreverse failed) " | "))
          (message "Removed %d worktree(s)" (length removed)))))))

(defun git-branch-off-worktree-delete ()
  "Delete git worktrees.

From magit-log: reads branch-off markers, visual selection, or commit at
point — in that order.  Asks y/n to confirm, then per dirty worktree if needed.
Otherwise: opens an interactive buffer — mark with m, C-c C-c to delete, C-g abort."
  (interactive)
  (if (derived-mode-p 'magit-log-mode)
      (git-branch-off--worktree-delete-from-log)
    (git-branch-off--worktree-delete-interactive)))

(provide 'git-branch-off-worktree)
;;; git-branch-off-worktree.el ends here

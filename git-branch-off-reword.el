;;; git-branch-off-reword.el --- Commit reword and remove  -*- lexical-binding: t; -*-

;;; Shared git plumbing

(defun git-branch-off--parse-commit (hash)
  "Return plist for HASH: :tree :parent :author-name :author-email :author-date
:committer-name :committer-email :committer-date."
  (let (result)
    (dolist (line (split-string
                   (with-temp-buffer
                     (call-process "git" nil t nil "cat-file" "commit" hash)
                     (buffer-string))
                   "\n"))
      (cond
       ((string-match "^tree \\(.+\\)$" line)
        (setq result (plist-put result :tree (match-string 1 line))))
       ((string-match "^parent \\(.+\\)$" line)
        (setq result (plist-put result :parent (match-string 1 line))))
       ((string-match "^author \\(.*\\) <\\(.*\\)> \\([0-9]+ [+-][0-9]+\\)$" line)
        (setq result (plist-put result :author-name  (match-string 1 line)))
        (setq result (plist-put result :author-email (match-string 2 line)))
        (setq result (plist-put result :author-date  (match-string 3 line))))
       ((string-match "^committer \\(.*\\) <\\(.*\\)> \\([0-9]+ [+-][0-9]+\\)$" line)
        (setq result (plist-put result :committer-name  (match-string 1 line)))
        (setq result (plist-put result :committer-email (match-string 2 line)))
        (setq result (plist-put result :committer-date  (match-string 3 line))))))
    result))

(defun git-branch-off--new-commit (info new-parent msg)
  "Create a commit object from INFO plist, overriding parent with NEW-PARENT and message with MSG.
NEW-PARENT nil keeps the :parent from INFO.  Return new hash string."
  (let* ((tree   (plist-get info :tree))
         (parent (or new-parent (plist-get info :parent)))
         (process-environment
          (append
           (list (format "GIT_AUTHOR_NAME=%s"     (plist-get info :author-name))
                 (format "GIT_AUTHOR_EMAIL=%s"    (plist-get info :author-email))
                 (format "GIT_AUTHOR_DATE=%s"     (plist-get info :author-date))
                 (format "GIT_COMMITTER_NAME=%s"  (plist-get info :committer-name))
                 (format "GIT_COMMITTER_EMAIL=%s" (plist-get info :committer-email))
                 (format "GIT_COMMITTER_DATE=%s"  (plist-get info :committer-date)))
           process-environment))
         (args (append (list "commit-tree" tree "-m" msg)
                       (when parent (list "-p" parent)))))
    (apply #'magit-git-string args)))

(defun git-branch-off--cascade (remap)
  "Rewrite all refs/branch-off/* whose parent changed according to REMAP.
Scans all branch-off refs; for each whose parent is a key in REMAP, creates
a new commit with the updated parent, replaces the ref, and adds the mapping
to REMAP.  Repeats until no further refs change.  Returns the augmented remap."
  (let ((bo-refs (split-string
                  (with-temp-buffer
                    (call-process "git" nil t nil "for-each-ref"
                                  "--format=%(refname)" "refs/branch-off/")
                    (buffer-string))
                  "\n" t))
        (changed nil))
    (dolist (ref bo-refs)
      (let* ((bo-hash   (magit-git-string "rev-parse" ref))
             (bo-info   (git-branch-off--parse-commit bo-hash))
             (bo-parent (plist-get bo-info :parent))
             (new-par   (cdr (assoc bo-parent remap))))
        (when new-par
          (let ((new-bo (git-branch-off--new-commit
                         bo-info new-par
                         (with-temp-buffer
                           (call-process "git" nil t nil "log" "-1" "--format=%B" bo-hash)
                           (buffer-string)))))
            (magit-call-git "update-ref" (format "refs/branch-off/%s" new-bo) new-bo)
            (magit-call-git "update-ref" "-d" ref)
            (push (cons bo-hash new-bo) remap)
            (setq changed t)))))
    (if changed
        (git-branch-off--cascade remap)
      remap)))

;;; Reword

(defvar-local git-branch-off--reword-commit nil)
(defvar-local git-branch-off--reword-source-buffer nil)
(defvar-local git-branch-off--reword-source-line nil)
(defvar-local git-branch-off--reword-from-revision nil)

(defun git-branch-off--reword-fix-highlight ()
  "Reposition highlights to the current line after a programmatic cursor move."
  (mapc #'delete-overlay magit-section-highlight-overlays)
  (setq magit-section-highlight-overlays nil)
  (hl-line-highlight))

(defun git-branch-off--reword-refresh-log (line)
  "Refresh the magit-log buffer and restore point to LINE."
  (when-let ((log-buf (magit-get-mode-buffer 'magit-log-mode)))
    (with-current-buffer log-buf (magit-refresh))
    (when-let ((win (get-buffer-window log-buf)))
      (with-selected-window win
        (goto-char (point-min))
        (forward-line (1- line))
        (git-branch-off--reword-fix-highlight))
      (run-with-timer 0 nil
        (lambda ()
          (when-let ((w (get-buffer-window log-buf)))
            (with-selected-window w
              (forward-line 1)
              (forward-line -1))))))))

(defun git-branch-off--reword-apply (hash new-msg)
  "Reword HASH with NEW-MSG using git plumbing.
For branch-off refs, rewrites the commit and cascades through any chained
branch-off descendants.  For current-branch commits, rebases all descendants,
updates the branch ref, then cascades through all affected branch-off refs."
  (let* ((full-hash      (magit-git-string "rev-parse" hash))
         (branch-off-ref (format "refs/branch-off/%s" full-hash))
         (is-branch-off  (equal full-hash
                                (magit-git-string "rev-parse" "--verify" branch-off-ref))))
    (if is-branch-off
        (let* ((info     (git-branch-off--parse-commit full-hash))
               (new-hash (git-branch-off--new-commit info nil new-msg)))
          (unless new-hash (user-error "git commit-tree failed"))
          (magit-call-git "update-ref" (format "refs/branch-off/%s" new-hash) new-hash)
          (magit-call-git "update-ref" "-d" branch-off-ref)
          (git-branch-off--cascade (list (cons full-hash new-hash))))
      (let ((branch (magit-git-string "symbolic-ref" "--short" "HEAD")))
        (unless branch
          (user-error "Cannot reword a branch commit in detached HEAD state"))
        (unless (= 0 (call-process "git" nil nil nil
                                   "merge-base" "--is-ancestor" full-hash "HEAD"))
          (user-error "Commit %s is not an ancestor of HEAD" (substring full-hash 0 8)))
        (let* ((chain  (split-string
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-list" "--reverse"
                                        (format "%s^..HEAD" full-hash))
                          (buffer-string))
                        "\n" t))
               (remap  nil)
               (target-info (git-branch-off--parse-commit full-hash))
               (new-target  (git-branch-off--new-commit target-info nil new-msg)))
          (unless new-target (user-error "git commit-tree failed"))
          (push (cons full-hash new-target) remap)
          (dolist (old-hash (cdr chain))
            (let* ((info       (git-branch-off--parse-commit old-hash))
                   (old-parent (plist-get info :parent))
                   (new-parent (or (cdr (assoc old-parent remap)) old-parent))
                   (msg        (with-temp-buffer
                                 (call-process "git" nil t nil "log" "-1" "--format=%B" old-hash)
                                 (buffer-string)))
                   (new-hash   (git-branch-off--new-commit info new-parent msg)))
              (push (cons old-hash new-hash) remap)))
          (magit-call-git "update-ref"
                          (format "refs/heads/%s" branch)
                          (cdr (assoc (car (last chain)) remap)))
          (git-branch-off--cascade remap))))))

(defun git-branch-off--reword-finish ()
  "Reword the target commit using git plumbing."
  (interactive)
  (let ((msg           (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (hash          git-branch-off--reword-commit)
        (dir           default-directory)
        (source        git-branch-off--reword-source-buffer)
        (line          git-branch-off--reword-source-line)
        (from-revision git-branch-off--reword-from-revision))
    (kill-buffer-and-window)
    (let ((default-directory dir))
      (git-branch-off--reword-apply hash msg)
      (when (and from-revision (buffer-live-p source))
        (kill-buffer source))
      (git-branch-off--reword-refresh-log line))))

(defun git-branch-off--reword-abort ()
  "Abort reword without applying changes."
  (interactive)
  (kill-buffer-and-window)
  (message "reword: aborted"))

(defun git-branch-off-reword (commit)
  "Reword COMMIT message, editing in a dedicated buffer.
Pre-fills the buffer with the current message.  C-c C-c applies, C-c C-k aborts.
Works for both branch commits (rebases descendants and updates branch-off refs)
and refs/branch-off/ commits directly."
  (interactive (list (or (magit-commit-at-point)
                         (and (derived-mode-p 'magit-revision-mode)
                              magit-buffer-revision)
                         (magit-read-branch-or-commit "Reword commit"))))
  (let* ((dir          (magit-toplevel))
         (source-buf   (current-buffer))
         (from-revision (derived-mode-p 'magit-revision-mode))
         (source-line  (if from-revision
                           (with-current-buffer (magit-get-mode-buffer 'magit-log-mode)
                             (line-number-at-pos))
                         (line-number-at-pos)))
         (msg          (with-temp-buffer
                         (magit-git-insert "log" "-1" "--format=%B" commit)
                         (buffer-string)))
         (buf          (get-buffer-create
                        (format "*reword %s*" (substring commit 0 (min 7 (length commit)))))))
    (with-current-buffer buf
      (erase-buffer)
      (insert msg)
      (git-commit-mode)
      (setq-local default-directory dir)
      (setq-local git-branch-off--reword-commit commit)
      (setq-local git-branch-off--reword-source-buffer source-buf)
      (setq-local git-branch-off--reword-source-line source-line)
      (setq-local git-branch-off--reword-from-revision from-revision)
      (local-set-key (kbd "C-c C-c") #'git-branch-off--reword-finish)
      (local-set-key (kbd "C-c C-k") #'git-branch-off--reword-abort)
      (setq-local header-line-format
                  (list " "
                        (propertize "C-c C-c" 'face 'transient-key)
                        " apply  "
                        (propertize "C-c C-k" 'face 'transient-key)
                        " abort"))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;; Remove

(defun git-branch-off--remove-tips-for (full-hash)
  "Return branch-off tip hashes that have FULL-HASH as ancestor (inclusive)."
  (cl-remove-if-not
   (lambda (tip)
     (= 0 (call-process "git" nil nil nil "merge-base" "--is-ancestor" full-hash tip)))
   (split-string
    (with-temp-buffer
      (call-process "git" nil t nil "for-each-ref" "--format=%(objectname)" "refs/branch-off/")
      (buffer-string))
    "\n" t)))

(defun git-branch-off--remove-one (full-hash top)
  "Remove FULL-HASH from its branch-off chain(s), rewriting descendants as needed.
Interior commits have their successors rebased onto the removed commit's parent.
TOP is the git toplevel.  Returns \\='skipped, \\='ref-and-wt, or \\='chain-rewrite."
  (let* ((short     (substring full-hash 0 8))
         (is-tip    (equal full-hash
                           (magit-git-string "rev-parse" "--verify"
                                             (format "refs/branch-off/%s" full-hash))))
         (wt-dir    (when (and top is-tip)
                      (expand-file-name (concat ".worktree/" full-hash) top)))
         (wt-exists (and wt-dir (file-exists-p wt-dir)))
         (tips      (git-branch-off--remove-tips-for full-hash))
         (proceed   t))
    (unless tips
      (user-error "Commit %s is not part of any branch-off chain" short))
    (when wt-exists
      (let ((status (git-branch-off--worktree-status wt-dir)))
        (when (and (not (string= status "clean"))
                   (not (y-or-n-p (format "Worktree %s has changes (%s) — remove anyway? "
                                          short status))))
          (setq proceed nil))))
    (if (not proceed)
        'skipped
      (let* ((removed-info   (git-branch-off--parse-commit full-hash))
             (removed-parent (plist-get removed-info :parent))
             (remap          (list (cons full-hash removed-parent))))
        (dolist (tip tips)
          (if (equal tip full-hash)
              (magit-call-git "update-ref" "-d" (format "refs/branch-off/%s" full-hash))
            (let ((path (split-string
                         (with-temp-buffer
                           (call-process "git" nil t nil "rev-list"
                                         "--ancestry-path" "--reverse"
                                         (format "%s..%s" full-hash tip))
                           (buffer-string))
                         "\n" t)))
              (dolist (c path)
                (let* ((info    (git-branch-off--parse-commit c))
                       (old-par (plist-get info :parent))
                       (new-par (or (cdr (assoc old-par remap)) old-par))
                       (msg     (with-temp-buffer
                                  (call-process "git" nil t nil "log" "-1" "--format=%B" c)
                                  (buffer-string)))
                       (new-c   (git-branch-off--new-commit info new-par msg)))
                  (push (cons c new-c) remap)))
              (let* ((new-tip (cdr (assoc tip remap)))
                     (old-ref (format "refs/branch-off/%s" tip)))
                (when new-tip
                  (magit-call-git "update-ref" (format "refs/branch-off/%s" new-tip) new-tip)
                  (magit-call-git "update-ref" "-d" old-ref))))))
        (when (and is-tip wt-exists)
          (with-temp-buffer
            (unless (= 0 (call-process "git" nil t nil "worktree" "remove" "--force" wt-dir))
              (message "Warning: could not remove worktree for %s: %s"
                       short (string-trim (buffer-string))))))
        (git-branch-off--cascade remap)
        (if (and is-tip wt-exists) 'ref-and-wt 'chain-rewrite)))))

(defun git-branch-off-remove (commit)
  "Remove commit(s) from their branch-off chain(s), rewriting descendants as needed.

From magit-log: reads m/M markers first, then visual selection, then the
commit at point.  Works for both chain tips and interior commits.  Commits
not part of any branch-off chain signal an error per commit.  Dirty
worktrees prompt y/n.  Marks are cleared after removal.

From magit-revision or anywhere else: operates on COMMIT only."
  (interactive
   (list (cond
          ((derived-mode-p 'magit-log-mode) nil)
          ((and (derived-mode-p 'magit-revision-mode)
                (bound-and-true-p magit-buffer-revision))
           magit-buffer-revision)
          (t (or (magit-commit-at-point)
                 (magit-read-branch-or-commit "Remove commit from branch-off chain"))))))
  (if (not (derived-mode-p 'magit-log-mode))
      (let* ((full-hash (magit-git-string "rev-parse" commit))
             (result    (git-branch-off--remove-one full-hash (magit-toplevel))))
        (magit-refresh)
        (pcase result
          ('skipped    (message "Skipped %s" (substring full-hash 0 8)))
          ('ref-and-wt (message "Removed branch-off ref and worktree for %s"
                                (substring full-hash 0 8)))
          (_           (message "Removed %s from branch-off chain" (substring full-hash 0 8)))))
    (let* ((raw (cond
                 ((bound-and-true-p git-branch-off--squash-marks)
                  git-branch-off--squash-marks)
                 ((use-region-p)
                  (mapcar (lambda (h) (magit-git-string "rev-parse" h))
                          (git-branch-off--squash-commits-in-region)))
                 (t (when-let ((h (magit-section-value-if 'commit)))
                      (list (magit-git-string "rev-parse" h))))))
           (_ (unless raw (user-error "No commits selected")))
           (top (magit-toplevel))
           (sorted (sort (copy-sequence raw)
                         (lambda (a b)
                           (= 0 (call-process "git" nil nil nil
                                              "merge-base" "--is-ancestor" b a))))))
      (let (removed failed)
        (dolist (full-hash sorted)
          (condition-case err
              (let ((result (git-branch-off--remove-one full-hash top)))
                (unless (eq result 'skipped)
                  (push (cons (substring full-hash 0 8) result) removed)))
            (error (push (format "%s: %s" (substring full-hash 0 8)
                                 (error-message-string err))
                         failed))))
        (when (bound-and-true-p git-branch-off--squash-marks)
          (setq git-branch-off--squash-marks nil)
          (git-branch-off--squash-clear-overlays))
        (magit-refresh)
        (cond
         (failed
          (message "Removed %d; errors — %s"
                   (length removed) (string-join (nreverse failed) " | ")))
         (removed
          (let* ((with-wt (cl-count 'ref-and-wt removed :key #'cdr))
                 (shorts  (mapcar #'car (nreverse removed))))
            (message "Removed from branch-off chain%s: %s"
                     (if (> with-wt 0) " (+worktree)" "")
                     (string-join shorts " "))))
         (t (message "Nothing removed")))))))

(provide 'git-branch-off-reword)
;;; git-branch-off-reword.el ends here

;;; git-branch-off-commit.el --- Commit operations  -*- lexical-binding: t; -*-

(require 'git-branch-off-stage)

;;; Stage-and-commit helpers

(defun git-branch-off--current-branch-off ()
  "Return the nearest branch-off hash in HEAD's ancestry when inside a branch-off worktree.
A branch-off worktree has a detached HEAD and lives at .worktree/<40-hex-chars>.
Returns the hash string, or nil when not in a branch-off worktree."
  (let ((top (magit-toplevel)))
    (when (and top
               (not (magit-git-string "symbolic-ref" "--short" "HEAD"))
               (string-match "/\\.worktree/\\([0-9a-f]\\{40\\}\\)/?$" top))
      (let* ((initial  (match-string 1 top))
             (commits  (split-string
                        (with-temp-buffer
                          (call-process "git" nil t nil
                                        "rev-list" (format "%s^..HEAD" initial))
                          (buffer-string))
                        "\n" t)))
        (cl-find-if (lambda (h)
                      (magit-git-string "rev-parse" "--verify"
                                        (format "refs/branch-off/%s" h)))
                    commits)))))

(defun git-branch-off-stage-and-commit ()
  "Stage the selected lines and open a new commit.
Requires an active region.  Stages only the +lines within the selection
(line precision), skipping context and deleted lines."
  (interactive)
  (unless (use-region-p)
    (user-error "Select lines first"))
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (let* ((top  (or (magit-toplevel) (user-error "Not in a git repository")))
         (rel  (file-relative-name buffer-file-name top))
         (range (git-branch-off--selection-lines)))
    (when (buffer-modified-p) (save-buffer))
    (let* ((default-directory top)
           (diff   (with-temp-buffer
                     (call-process "git" nil t nil "diff" "-U0" "--" rel)
                     (buffer-string)))
           (result (git-branch-off--patch-from-diff diff rel (car range) (cdr range))))
      (unless result
        (user-error "No changes in selection to stage"))
      (let* ((patch (car result))
             (count (cdr result))
             (tmp   (make-temp-file "magit-lines" nil ".patch")))
        (unwind-protect
            (with-temp-buffer
              (write-region patch nil tmp nil 'silent)
              (let ((exit (call-process "git" tmp (list t t) nil
                                        "apply" "--cached" "--unidiff-zero" "--")))
                (unless (and (integerp exit) (= exit 0))
                  (user-error "git apply --cached failed:\n%s" (buffer-string)))))
          (ignore-errors (delete-file tmp)))
        (message "Staged %d change%s" count (if (= count 1) "" "s"))
        (when-let ((old-bo (git-branch-off--current-branch-off)))
          (letrec ((hook (lambda ()
                           (remove-hook 'magit-post-commit-hook hook)
                           (let ((default-directory top))
                             (let* ((new-hash (magit-git-string "rev-parse" "HEAD"))
                                    (new-ref  (format "refs/branch-off/%s" new-hash))
                                    (old-ref  (format "refs/branch-off/%s" old-bo)))
                               (magit-call-git "update-ref" new-ref new-hash)
                               (magit-call-git "update-ref" "-d" old-ref))))))
            (add-hook 'magit-post-commit-hook hook)))
        (magit-commit-create)))))

;;; Commit-and-branch-off

(defun git-branch-off--only-additions-in-selection-p (diff sel-start sel-end)
  "Return nil if every line in [SEL-START..SEL-END] is a pure addition in DIFF.
Returns a human-readable error string describing the first problem found."
  (let ((new-cursor 0)
        (hunk-has-del nil)
        (covered nil)
        (result nil))
    (catch 'done
      (dolist (line (split-string diff "\n"))
        (cond
         ((string-match
           (rx bol "@@ -" (group (+ digit)) (? "," (group (+ digit)))
               " +" (group (+ digit)) (? "," (+ digit)) " @@")
           line)
          (let ((old-count (if (match-string 2 line)
                               (string-to-number (match-string 2 line))
                             1)))
            (setq new-cursor   (string-to-number (match-string 3 line))
                  hunk-has-del (> old-count 0))))
         ((string-prefix-p "+" line)
          (when (and (>= new-cursor sel-start) (<= new-cursor sel-end))
            (push new-cursor covered)
            (when hunk-has-del
              (setq result "selection contains modified lines — select only newly added lines")
              (throw 'done nil)))
          (setq new-cursor (1+ new-cursor)))
         ((string-prefix-p " " line)
          (when (and (>= new-cursor sel-start) (<= new-cursor sel-end))
            (setq result "selection contains unchanged lines — select only newly added lines")
            (throw 'done nil))
          (setq new-cursor (1+ new-cursor))))))
    (or result
        (let ((missing (cl-loop for p from sel-start to sel-end
                                unless (memq p covered) collect p)))
          (when missing
            "selection contains unchanged lines — select only newly added lines")))))

(defun git-branch-off-stage-and-commit-branch-off ()
  "Stage the selected newly-added lines, commit, preserve under refs/branch-off/, then rewind.

Requires an active region containing only pure additions.  Aborts with an
explanation if the selection contains modifications or deletions.

The commit is preserved under refs/branch-off/<full-hash>.  HEAD and the
index are rewound with --mixed.  The committed additions are removed from the
working tree by reversing the staging patch."
  (interactive)
  (unless (use-region-p)
    (user-error "Select lines first"))
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (let* ((top (or (magit-toplevel) (user-error "Not in a git repository")))
         (default-directory top)
         (rel (file-relative-name buffer-file-name top))
         (range     (git-branch-off--selection-lines)))
    (when (buffer-modified-p) (save-buffer))
    (let* ((sel-start (car range))
           (sel-end   (cdr range))
           (diff      (with-temp-buffer
                        (call-process "git" nil t nil "diff" "-U0" "--" rel)
                        (buffer-string)))
           (err-msg   (git-branch-off--only-additions-in-selection-p diff sel-start sel-end))
           (result    (unless err-msg
                        (git-branch-off--patch-from-diff diff rel sel-start sel-end))))
      (when err-msg   (user-error "%s" err-msg))
      (unless result  (user-error "No new additions in selection"))
      (let* ((commit-msg (read-string "Commit message: ")))
        (when (string-empty-p commit-msg)
          (user-error "Commit message cannot be empty"))
        (let ((staging-patch (make-temp-file "magit-lines" nil ".patch")))
          (write-region (car result) nil staging-patch nil 'silent)
          (unwind-protect
              (progn
                (with-temp-buffer
                  (let ((exit (call-process "git" staging-patch (list t t) nil
                                            "apply" "--cached" "--unidiff-zero" "--")))
                    (unless (and (integerp exit) (= exit 0))
                      (user-error "git apply --cached failed:\n%s" (buffer-string)))))
                (magit-call-git "commit" "-m" commit-msg)
                (let ((hash (magit-git-string "rev-parse" "HEAD")))
                  (magit-call-git "update-ref"
                                  (format "refs/branch-off/%s" hash) hash)
                  (let ((exit (call-process "git" nil nil nil
                                            "apply" "-R" "--unidiff-zero"
                                            "--" staging-patch)))
                    (if (and (integerp exit) (= exit 0))
                        (revert-buffer t t)
                      (message "Warning: could not remove additions from working tree; \
check %s manually" (file-name-nondirectory buffer-file-name))))
                  (condition-case err
                      (magit-call-git "reset" "--mixed" "HEAD~1")
                    (error
                     (magit-refresh)
                     (user-error "refs/branch-off/%s created but reset failed: %s"
                                 (substring hash 0 8) (error-message-string err))))
                  (magit-refresh)
                  (message "Branched off %s — additions removed from working tree"
                           (substring hash 0 8))))
            (ignore-errors (delete-file staging-patch))))))))

(provide 'git-branch-off-commit)
;;; git-branch-off-commit.el ends here

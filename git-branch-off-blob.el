;;; git-branch-off-blob.el --- Blob navigation with branch-off awareness  -*- lexical-binding: t; -*-

(defun git-branch-off--blob-current-hash ()
  "Return the full 40-char hash if the current blob buffer is on a branch-off commit, else nil."
  (when (and (bound-and-true-p magit-blob-mode)
             (bound-and-true-p magit-buffer-revision))
    (let ((full (magit-git-string "rev-parse" "--verify" magit-buffer-revision)))
      (when (and full
                 (equal full (magit-git-string "rev-parse" "--verify"
                                                (format "refs/branch-off/%s" full))))
        full))))

(defun git-branch-off--blob-parent-of (hash)
  "Return the full hash of HASH's parent commit, or nil for a root commit."
  (let ((p (magit-git-string "rev-parse" "--verify" (concat hash "^"))))
    (when (and p (not (string-empty-p p))) p)))

(defun git-branch-off--blob-branch-offs-by-date ()
  "Return all refs/branch-off/* hashes sorted by committer date ascending (oldest first)."
  (sort (magit-git-lines "for-each-ref" "--format=%(objectname)" "refs/branch-off/")
        (lambda (a b)
          (< (string-to-number
              (or (magit-git-string "log" "-1" "--format=%ct" a) "0"))
             (string-to-number
              (or (magit-git-string "log" "-1" "--format=%ct" b) "0"))))))

(defun git-branch-off--blob-touches-file-p (hash file-rel)
  "Return non-nil when HASH added or modified FILE-REL (path relative to repo root)."
  (magit-git-lines "log" "-1" "--format=%H" "--diff-filter=AM" hash "--" file-rel))

(defun git-branch-off-blob-next ()
  "Go to the next (more recent) blob revision, respecting the git DAG.
For branch-off commits: navigates to chain children first, then to
same-parent siblings with a newer committer date.  Falls through to
`magit-blob-next' for regular commits."
  (interactive)
  (if-let ((full (git-branch-off--blob-current-hash)))
      (let* ((file-abs      (magit-buffer-file-name))
             (file-rel      (file-relative-name file-abs (magit-toplevel)))
             (parent        (git-branch-off--blob-parent-of full))
             (all-bo        (git-branch-off--blob-branch-offs-by-date))
             (idx           (cl-position full all-bo :test #'equal))
             (chain-kids    (cl-remove-if-not
                             (lambda (h) (equal full (git-branch-off--blob-parent-of h)))
                             all-bo))
             (newer-siblings (when idx
                               (cl-remove-if-not
                                (lambda (h) (equal parent (git-branch-off--blob-parent-of h)))
                                (nthcdr (1+ idx) all-bo))))
             (succ (or (cl-find-if (lambda (h) (git-branch-off--blob-touches-file-p h file-rel))
                                   chain-kids)
                       (cl-find-if (lambda (h) (git-branch-off--blob-touches-file-p h file-rel))
                                   newer-siblings))))
        (if succ
            (magit-blob-visit succ file-rel)
          (user-error "No next blob")))
    (call-interactively #'magit-blob-next)))

(defun git-branch-off-blob-prev ()
  "Go to the previous (older) blob revision, respecting the git DAG.
For branch-off commits: navigates to same-parent siblings with an older
committer date.  Falls through to `magit-blob-previous' when no older
sibling exists, which follows git ancestry correctly."
  (interactive)
  (if-let ((full (git-branch-off--blob-current-hash)))
      (let* ((file-abs       (magit-buffer-file-name))
             (file-rel       (file-relative-name file-abs (magit-toplevel)))
             (parent         (git-branch-off--blob-parent-of full))
             (all-bo         (git-branch-off--blob-branch-offs-by-date))
             (idx            (cl-position full all-bo :test #'equal))
             (older-siblings (when idx
                               (cl-remove-if-not
                                (lambda (h) (equal parent (git-branch-off--blob-parent-of h)))
                                (reverse (seq-take all-bo idx)))))
             (pred           (cl-find-if
                              (lambda (h) (git-branch-off--blob-touches-file-p h file-rel))
                              older-siblings)))
        (if pred
            (magit-blob-visit pred file-rel)
          (call-interactively #'magit-blob-previous)))
    (call-interactively #'magit-blob-previous)))

(provide 'git-branch-off-blob)
;;; git-branch-off-blob.el ends here

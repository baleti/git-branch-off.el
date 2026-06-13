;;; git-branch-off-log.el --- Log, revision, and status navigation  -*- lexical-binding: t; -*-

;;; Faces

(defface git-branch-off-log-worktree-marker
  '((((class color) (background dark))  :foreground "cyan"      :weight bold)
    (((class color) (background light)) :foreground "dark cyan"  :weight bold))
  "Face for the @ symbol that marks a worktree HEAD commit in the log.")

(defface git-branch-off-log-worktree-hash
  '((((class color) (background dark))  :foreground "cyan3")
    (((class color) (background light)) :foreground "cyan4"))
  "Face for the hash text of a worktree HEAD commit in the log.")

;;; Log / revision navigation

(defvar git-branch-off--log-nav-overlay nil)

(defun git-branch-off--revision-navigate (move-fn)
  "Navigate the log buffer with MOVE-FN and preview the commit at point."
  (when-let* ((log-buf (magit-get-mode-buffer 'magit-log-mode)))
    (with-current-buffer log-buf
      (funcall move-fn)
      (mapc #'delete-overlay magit-section-highlight-overlays)
      (setq magit-section-highlight-overlays nil)
      (unless (overlayp git-branch-off--log-nav-overlay)
        (setq git-branch-off--log-nav-overlay (make-overlay 1 1))
        (overlay-put git-branch-off--log-nav-overlay 'face 'magit-section-highlight)
        (overlay-put git-branch-off--log-nav-overlay 'priority 200))
      (move-overlay git-branch-off--log-nav-overlay
                    (line-beginning-position)
                    (1+ (line-end-position)))
      (when-let ((commit (magit-section-value-if 'commit)))
        (let ((magit-display-buffer-noselect t))
          (magit-show-commit commit))))))

(defun git-branch-off-revision-next ()
  "Move to the next commit in the log and preview it."
  (interactive)
  (git-branch-off--revision-navigate #'magit-section-forward))

(defun git-branch-off-revision-prev ()
  "Move to the previous commit in the log and preview it."
  (interactive)
  (git-branch-off--revision-navigate #'magit-section-backward))

;;; Flat log view

(defvar-local git-branch-off--log-flat nil
  "Non-nil in log buffers opened with `git-branch-off-log'.")

(defvar git-branch-off--log-flat-pending nil
  "Dynamic flag set during `git-branch-off-log' so the hook fires correctly.")

(defun git-branch-off-log ()
  "Log all refs without --graph; branch-off commits are marked with depth-based indent."
  (interactive)
  (let ((git-branch-off--log-flat-pending t))
    (magit-log-setup-buffer (list "--all") (list "--color" "--decorate" "--topo-order" "-n256") nil)))

(defun git-branch-off--log-mark ()
  "Overlay depth-based indent on branch-off commits and @ on worktree HEADs."
  (when (derived-mode-p 'magit-log-mode)
    (when git-branch-off--log-flat-pending
      (setq-local git-branch-off--log-flat t))
    (remove-overlays (point-min) (point-max) 'git-branch-off-log-marker t)
    (remove-overlays (point-min) (point-max) 'git-branch-off-archive-marker t))
  (when (and (derived-mode-p 'magit-log-mode)
             (bound-and-true-p git-branch-off--log-flat))
    (let* ((raw (magit-git-lines "log" "--format=%H %P"
                                 "--all"
                                 "--not" "--glob=refs/heads/*"))
           (parent-map  (make-hash-table :test #'equal))
           (all-hashes  nil)
           (bo-ref-set  (let ((tbl (make-hash-table :test #'equal)))
                          (dolist (h (magit-git-lines "for-each-ref"
                                                      "--format=%(objectname)"
                                                      "refs/branch-off/"))
                            (puthash h t tbl))
                          tbl))
           (wt-hashes   (let (acc)
                          (dolist (line (magit-git-lines "worktree" "list" "--porcelain"))
                            (when (string-prefix-p "HEAD " line)
                              (push (substring line 5) acc)))
                          acc))
           (depth-cache (make-hash-table :test #'equal)))
      (dolist (line raw)
        (when (string-match
               "^\\([0-9a-f]\\{40\\}\\)\\(?: \\([0-9a-f]\\{40\\}\\)\\)?" line)
          (let ((h (match-string 1 line))
                (p (match-string 2 line)))
            (puthash h p parent-map)
            (push h all-hashes))))
      (when (or all-hashes wt-hashes)
        (cl-labels
            ((in-bo-p (h)
               (not (eq (gethash h parent-map 'absent) 'absent)))
             (depth-of (h)
               (or (gethash h depth-cache)
                   (let* ((p (gethash h parent-map))
                          (d (if (or (null p) (not (in-bo-p p)))
                                 1
                               (let ((pd (depth-of p)))
                                 (if (or (gethash p bo-ref-set)
                                         (gethash h bo-ref-set))
                                     (1+ pd)
                                   pd)))))
                     (puthash h d depth-cache)
                     d))))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when-let ((h (magit-section-value-if 'commit)))
                (let* ((bo-full (cl-find-if (lambda (f) (string-prefix-p h f)) all-hashes))
                       (wt-full (cl-find-if (lambda (f) (string-prefix-p h f)) wt-hashes))
                       (d       (when bo-full (depth-of bo-full)))
                       (bol     (line-beginning-position)))
                  (when d
                    (let ((ov (make-overlay bol bol)))
                      (overlay-put ov 'before-string (make-string (* d 2) ?\s))
                      (overlay-put ov 'priority 10)
                      (overlay-put ov 'git-branch-off-log-marker t)))
                  (when wt-full
                    (let ((ov-at   (make-overlay bol bol))
                          (ov-hash (make-overlay bol (+ bol (length h)))))
                      (overlay-put ov-at 'before-string
                                   (propertize "@" 'face 'git-branch-off-log-worktree-marker))
                      (overlay-put ov-at 'priority 20)
                      (overlay-put ov-at 'git-branch-off-log-marker t)
                      (overlay-put ov-hash 'face 'git-branch-off-log-worktree-hash)
                      (overlay-put ov-hash 'git-branch-off-log-marker t)))))
              (forward-line 1))))))))

;;; Status navigation

(defun git-branch-off-status-tab ()
  "On a commit section show it; otherwise toggle the section."
  (interactive)
  (if-let ((commit (magit-section-value-if 'commit)))
      (magit-show-commit commit)
    (call-interactively #'magit-section-toggle)))

(defun git-branch-off--status-navigate (move-fn)
  "Navigate the status buffer with MOVE-FN and preview any commit at point."
  (condition-case nil
      (progn
        (funcall move-fn)
        (when-let ((commit (magit-section-value-if 'commit)))
          (let ((magit-display-buffer-noselect t))
            (magit-show-commit commit))))
    (error nil)))

(defun git-branch-off-status-next ()
  "Move to the next section in status, previewing commits."
  (interactive)
  (git-branch-off--status-navigate #'magit-section-forward))

(defun git-branch-off-status-prev ()
  "Move to the previous section in status, previewing commits."
  (interactive)
  (git-branch-off--status-navigate #'magit-section-backward))

(provide 'git-branch-off-log)
;;; git-branch-off-log.el ends here

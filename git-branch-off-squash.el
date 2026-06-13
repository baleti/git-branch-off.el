;;; git-branch-off-squash.el --- Squash commits  -*- lexical-binding: t; -*-

(require 'git-branch-off-reword)

;;; Mark system

(defvar-local git-branch-off--squash-marks nil
  "Ordered list of full commit hashes marked in this log buffer.")

(defvar-local git-branch-off--squash-overlays nil
  "Alist of (full-hash . overlay) for marked commits in this log buffer.")

(defface git-branch-off-squash-marked
  '((t :extend t))
  "Face applied to commit lines marked for squashing.
Customize this face to match your theme; e.g. with a blend of orange and bg.")

(defun git-branch-off--squash-clear-overlays ()
  "Delete all squash-mark overlays in the current buffer."
  (dolist (pair git-branch-off--squash-overlays)
    (delete-overlay (cdr pair)))
  (setq git-branch-off--squash-overlays nil))

(defun git-branch-off--squash-mark-one (full bol)
  "Add FULL hash to marks with an overlay starting at BOL."
  (setq git-branch-off--squash-marks (append git-branch-off--squash-marks (list full)))
  (let* ((eol (save-excursion (goto-char bol) (end-of-line) (point)))
         (ov  (make-overlay bol (1+ eol) nil t nil)))
    (overlay-put ov 'face 'git-branch-off-squash-marked)
    (push (cons full ov) git-branch-off--squash-overlays)))

(defun git-branch-off--squash-unmark-one (full)
  "Remove FULL hash from marks and delete its overlay."
  (setq git-branch-off--squash-marks (delete full git-branch-off--squash-marks))
  (when-let ((ov (cdr (assoc full git-branch-off--squash-overlays))))
    (delete-overlay ov))
  (setq git-branch-off--squash-overlays
        (cl-remove full git-branch-off--squash-overlays :key #'car :test #'equal)))

(defun git-branch-off-mark ()
  "Toggle squash mark on the commit at point, or on all commits in the visual selection.
With an active region (evil V), marks every commit in the selection — or
unmarks them all when every one is already marked."
  (interactive)
  (if (not (use-region-p))
      (let ((hash (magit-section-value-if 'commit)))
        (unless hash (user-error "No commit at point"))
        (let ((full (magit-git-string "rev-parse" hash)))
          (if (member full git-branch-off--squash-marks)
              (progn
                (git-branch-off--squash-unmark-one full)
                (message "Unmarked %s (%d remaining)"
                         (substring full 0 8) (length git-branch-off--squash-marks)))
            (git-branch-off--squash-mark-one full (line-beginning-position))
            (message "Marked %s (%d total)"
                     (substring full 0 8) (length git-branch-off--squash-marks)))))
    (let* ((beg (region-beginning))
           (end (region-end))
           (end-pos (save-excursion
                      (goto-char end)
                      (when (and (bolp) (> end beg)) (forward-line -1))
                      (line-beginning-position)))
           entries)
      (save-excursion
        (goto-char beg)
        (beginning-of-line)
        (while (<= (point) end-pos)
          (when-let ((hash (magit-section-value-if 'commit)))
            (let ((full (magit-git-string "rev-parse" hash)))
              (cl-pushnew (cons full (line-beginning-position)) entries
                          :key #'car :test #'equal)))
          (forward-line 1)))
      (unless entries (user-error "No commits in selection"))
      (if (cl-every (lambda (e) (member (car e) git-branch-off--squash-marks)) entries)
          (progn
            (dolist (e entries) (git-branch-off--squash-unmark-one (car e)))
            (message "Unmarked %d commit%s (%d remaining)"
                     (length entries) (if (= (length entries) 1) "" "s")
                     (length git-branch-off--squash-marks)))
        (let ((newly 0))
          (dolist (e entries)
            (unless (member (car e) git-branch-off--squash-marks)
              (git-branch-off--squash-mark-one (car e) (cdr e))
              (cl-incf newly)))
          (message "Marked %d commit%s (%d total)"
                   newly (if (= newly 1) "" "s")
                   (length git-branch-off--squash-marks)))))))

(defun git-branch-off--squash-commits-in-region ()
  "Return commit hashes in the active region (display order)."
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (end-pos (save-excursion
                      (goto-char end)
                      (when (and (bolp) (> end beg)) (forward-line -1))
                      (line-beginning-position)))
           commits)
      (save-excursion
        (goto-char beg)
        (beginning-of-line)
        (while (<= (point) end-pos)
          (when-let ((hash (magit-section-value-if 'commit)))
            (cl-pushnew hash commits :test #'equal))
          (forward-line 1)))
      (nreverse commits))))

;;; Chain building

(defun git-branch-off--squash-build-chain (full-hashes)
  "Sort FULL-HASHES into a contiguous linear chain oldest-first.
Returns (SORTED-CHAIN . PARENT-OF-OLDEST) or signals `user-error'."
  (let ((parent-of (make-hash-table :test #'equal))
        (hash-set  (make-hash-table :test #'equal))
        (child-of  (make-hash-table :test #'equal)))
    (dolist (h full-hashes)
      (puthash h t hash-set)
      (let ((p (magit-git-string "rev-parse" "--verify" (format "%s^" h))))
        (puthash h (and p (not (string-empty-p p)) p) parent-of)))
    (dolist (h full-hashes)
      (let ((p (gethash h parent-of)))
        (when (and p (gethash p hash-set))
          (puthash p h child-of))))
    (let ((root (cl-find-if
                 (lambda (h) (not (gethash (gethash h parent-of) hash-set)))
                 full-hashes)))
      (unless root
        (user-error "Selected commits don't have a clear oldest commit (cycle?)"))
      (let ((chain nil) (cur root))
        (while cur
          (push cur chain)
          (setq cur (gethash cur child-of)))
        (let ((sorted (nreverse chain)))
          (unless (= (length sorted) (length full-hashes))
            (user-error "Selected commits are not a contiguous linear chain — cannot squash"))
          (cons sorted (gethash root parent-of)))))))

(defun git-branch-off--squash-try-chain (full-hashes)
  "Try `git-branch-off--squash-build-chain'; return nil instead of signaling on failure."
  (condition-case nil
      (git-branch-off--squash-build-chain full-hashes)
    (user-error nil)))

(defun git-branch-off--squash-sort-siblings (full-hashes)
  "Sort branch-off FULL-HASHES by committer date (oldest first) and verify a shared parent.
Returns (SORTED-LIST . COMMON-PARENT) or signals `user-error'."
  (let* ((dated (mapcar (lambda (h)
                          (cons (string-to-number
                                 (or (magit-git-string "log" "-1" "--format=%ct" h) "0"))
                                h))
                        full-hashes))
         (sorted (mapcar #'cdr (sort dated (lambda (a b) (< (car a) (car b))))))
         (parents (mapcar (lambda (h)
                            (let ((p (magit-git-string "rev-parse" "--verify"
                                                        (format "%s^" h))))
                              (and p (not (string-empty-p p)) p)))
                          sorted)))
    (unless (cl-every (lambda (p) (equal p (car parents))) (cdr parents))
      (user-error
       "Selected branch-off commits have different parents and don't form a chain — \
select commits that chain or that all branch from the same commit"))
    (cons sorted (car parents))))

(defun git-branch-off--squash-commit-tree (hash)
  "Return the tree SHA of commit HASH."
  (with-temp-buffer
    (call-process "git" nil t nil "rev-parse" (format "%s^{tree}" hash))
    (string-trim (buffer-string))))

;;; Conflict resolution via smerge

(defvar-local git-branch-off--squash-conflict-done-fn nil
  "Continuation called with the resolved blob SHA from the conflict buffer.")

(defvar git-branch-off--squash-verbose nil
  "When non-nil, append the squash diff as comments to the message buffer.")

(defun git-branch-off--squash-finish-conflict ()
  "Confirm conflict resolution and resume the in-progress squash."
  (interactive)
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
    (user-error "Unresolved conflicts remain — use smerge (C-c ^ n/p) or ediff (C-c ^ e)"))
  (funcall git-branch-off--squash-conflict-done-fn))

(defun git-branch-off--squash-abort-conflict ()
  "Abort the squash from the conflict resolution buffer."
  (interactive)
  (kill-buffer-and-window)
  (message "Squash aborted"))

(defun git-branch-off--squash-open-conflict-buffer (path commit-hash conflicted-content mode done-fn)
  "Open a smerge buffer for CONFLICTED-CONTENT of PATH.
DONE-FN is called with the resolved blob SHA when the user confirms with C-c C-c."
  (let ((buf (get-buffer-create (format "*squash conflict: %s*" path))))
    (with-current-buffer buf
      (erase-buffer)
      (insert conflicted-content)
      (smerge-mode 1)
      (setq-local git-branch-off--squash-conflict-done-fn
                  (lambda ()
                    (let ((sha (with-temp-buffer
                                 (insert-buffer-substring buf)
                                 (unless (= 0 (call-process-region
                                               (point-min) (point-max)
                                               "git" t t nil "hash-object" "-w" "--stdin"))
                                   (user-error "git hash-object failed for %s" path))
                                 (string-trim (buffer-string)))))
                      (kill-buffer-and-window)
                      (funcall done-fn sha))))
      (local-set-key (kbd "C-c C-c") #'git-branch-off--squash-finish-conflict)
      (local-set-key (kbd "C-c C-k") #'git-branch-off--squash-abort-conflict)
      (setq-local header-line-format
                  (list (format " Conflict in %s (from %s) — " path (substring commit-hash 0 8))
                        (propertize "C-c C-c" 'face 'transient-key) " done  "
                        (propertize "C-c C-k" 'face 'transient-key) " abort  "
                        (propertize "C-c ^ e" 'face 'transient-key) " ediff"))
      (goto-char (point-min))
      (ignore-errors (smerge-next)))
    (pop-to-buffer buf)))

(defun git-branch-off--squash-resolve-path-list (paths by-path commit-hash penv all-resolved-fn)
  "Open smerge for each path in PATHS in turn; when all done call ALL-RESOLVED-FN."
  (if (null paths)
      (funcall all-resolved-fn)
    (let* ((path      (car paths))
           (rest      (cdr paths))
           (entry     (gethash path by-path))
           (base-sha  (plist-get entry :base))
           (ours-sha  (plist-get entry :ours))
           (their-sha (plist-get entry :theirs))
           (mode      (plist-get entry :mode)))
      (unless (and base-sha ours-sha their-sha)
        (user-error "Conflict in %s — add/delete conflict; resolve manually" path))
      (let ((base-f (make-temp-file "sq-base-"))
            (ours-f (make-temp-file "sq-ours-"))
            (thrs-f (make-temp-file "sq-thrs-")))
        (dolist (pair `((,base-sha . ,base-f) (,ours-sha . ,ours-f) (,their-sha . ,thrs-f)))
          (with-temp-buffer
            (call-process "git" nil t nil "cat-file" "blob" (car pair))
            (write-region (point-min) (point-max) (cdr pair) nil 'silent)))
        (call-process "git" nil nil nil "merge-file" ours-f base-f thrs-f)
        (ignore-errors (delete-file base-f))
        (ignore-errors (delete-file thrs-f))
        (let ((conflicted (with-temp-buffer
                            (insert-file-contents ours-f)
                            (buffer-string))))
          (ignore-errors (delete-file ours-f))
          (git-branch-off--squash-open-conflict-buffer
           path commit-hash conflicted mode
           (lambda (resolved-sha)
             (let ((process-environment penv))
               (with-temp-buffer
                 (insert (format "0 %s 0\t%s\n" (make-string 40 ?0) path))
                 (call-process-region (point-min) (point-max) "git" t nil nil
                                      "update-index" "--index-info"))
               (with-temp-buffer
                 (insert (format "%s %s 0\t%s\n" mode resolved-sha path))
                 (unless (= 0 (call-process-region (point-min) (point-max) "git" t t nil
                                                    "update-index" "--index-info"))
                   (user-error "git update-index failed for %s" path))))
             (git-branch-off--squash-resolve-path-list rest by-path commit-hash penv
                                                        all-resolved-fn))))))))

(defun git-branch-off--squash-merge-commits (remaining parent-tree current-tree temp-index penv done-fn)
  "Iteratively 3-way-merge REMAINING sibling commits into CURRENT-TREE."
  (if (null remaining)
      (funcall done-fn current-tree)
    (let* ((process-environment penv)
           (h        (car remaining))
           (rest     (cdr remaining))
           (bon-tree (git-branch-off--squash-commit-tree h)))
      (call-process "git" nil nil nil "read-tree" "-i" "-m" parent-tree current-tree bon-tree)
      (let ((unmerged (with-temp-buffer
                        (call-process "git" nil t nil "ls-files" "--unmerged")
                        (buffer-string))))
        (if (string-empty-p (string-trim unmerged))
            (let ((new-tree (with-temp-buffer
                              (unless (= 0 (call-process "git" nil t nil "write-tree"))
                                (user-error "git write-tree failed merging %s" (substring h 0 8)))
                              (string-trim (buffer-string)))))
              (git-branch-off--squash-merge-commits rest parent-tree new-tree temp-index penv done-fn))
          (let ((by-path (make-hash-table :test #'equal)))
            (dolist (line (split-string unmerged "\n" t))
              (when (string-match "^\\([0-9]+\\) \\([0-9a-f]+\\) \\([123]\\)\t\\(.+\\)$" line)
                (let* ((mode  (match-string 1 line)) (sha (match-string 2 line))
                       (stage (string-to-number (match-string 3 line)))
                       (path  (match-string 4 line)))
                  (let ((e (or (gethash path by-path)
                               (let ((e (list :base nil :ours nil :theirs nil :mode nil)))
                                 (puthash path e by-path) e))))
                    (plist-put e (cl-case stage (1 :base) (2 :ours) (3 :theirs)) sha)
                    (plist-put e :mode mode)))))
            (let (paths)
              (maphash (lambda (k _) (push k paths)) by-path)
              (git-branch-off--squash-resolve-path-list
               paths by-path h penv
               (lambda ()
                 (let ((new-tree (let ((process-environment penv))
                                   (with-temp-buffer
                                     (unless (= 0 (call-process "git" nil t nil "write-tree"))
                                       (user-error "git write-tree failed after resolving %s"
                                                   (substring h 0 8)))
                                     (string-trim (buffer-string))))))
                   (git-branch-off--squash-merge-commits
                    rest parent-tree new-tree temp-index penv done-fn)))))))))))

(defun git-branch-off--squash-combined-tree (parent-hash sorted-commits done-fn)
  "Async: build a merged tree from sibling SORTED-COMMITS branched from PARENT-HASH.
Calls DONE-FN with the merged tree hash; may open smerge buffers for conflicts."
  (let* ((parent-tree (git-branch-off--squash-commit-tree parent-hash))
         (temp-index  (make-temp-file "git-squash" nil))
         (penv (append (list (format "GIT_INDEX_FILE=%s" temp-index)) process-environment)))
    (let ((process-environment penv))
      (with-temp-buffer
        (unless (= 0 (call-process "git" nil t nil "read-tree" parent-tree))
          (ignore-errors (delete-file temp-index))
          (user-error "git read-tree failed: %s" (buffer-string)))))
    (git-branch-off--squash-merge-commits
     sorted-commits parent-tree parent-tree temp-index penv
     (lambda (tree-hash)
       (ignore-errors (delete-file temp-index))
       (funcall done-fn tree-hash)))))

;;; Squash application

(defun git-branch-off--squash-make-info (tree-hash first-info)
  "Build a commit info plist: TREE-HASH as tree, identity from FIRST-INFO."
  (list :tree            tree-hash
        :author-name     (plist-get first-info :author-name)
        :author-email    (plist-get first-info :author-email)
        :author-date     (plist-get first-info :author-date)
        :committer-name  (plist-get first-info :committer-name)
        :committer-email (plist-get first-info :committer-email)
        :committer-date  (plist-get first-info :committer-date)))

(defun git-branch-off--squash-apply-branch-off (sorted-chain parent-of-first tree-hash new-msg)
  "Squash SORTED-CHAIN branch-off commits into one new branch-off commit."
  (let* ((first-info (git-branch-off--parse-commit (car sorted-chain)))
         (info       (git-branch-off--squash-make-info tree-hash first-info))
         (new-hash   (git-branch-off--new-commit info parent-of-first new-msg)))
    (unless new-hash (user-error "git commit-tree failed"))
    (magit-call-git "update-ref" (format "refs/branch-off/%s" new-hash) new-hash)
    (dolist (h sorted-chain)
      (magit-call-git "update-ref" "-d" (format "refs/branch-off/%s" h)))
    (git-branch-off--cascade
     (mapcar (lambda (h) (cons h new-hash)) sorted-chain))
    (magit-refresh)
    (message "Squashed %d branch-off commits → %s"
             (length sorted-chain) (substring new-hash 0 8))))

(defun git-branch-off--squash-apply-branch (sorted-chain parent-of-first tree-hash new-msg branch)
  "Squash SORTED-CHAIN branch commits into one, rebasing HEAD descendants."
  (let* ((first-info (git-branch-off--parse-commit (car sorted-chain)))
         (info       (git-branch-off--squash-make-info tree-hash first-info))
         (new-squash (git-branch-off--new-commit info parent-of-first new-msg)))
    (unless new-squash (user-error "git commit-tree failed"))
    (let* ((last-hash   (car (last sorted-chain)))
           (descendants (split-string
                         (with-temp-buffer
                           (call-process "git" nil t nil
                                         "rev-list" "--reverse"
                                         (format "%s..HEAD" last-hash))
                           (buffer-string))
                         "\n" t))
           (remap (mapcar (lambda (h) (cons h new-squash)) sorted-chain)))
      (dolist (old-hash descendants)
        (let* ((d-info   (git-branch-off--parse-commit old-hash))
               (old-par  (plist-get d-info :parent))
               (new-par  (or (cdr (assoc old-par remap)) old-par))
               (msg      (with-temp-buffer
                           (call-process "git" nil t nil "log" "-1" "--format=%B" old-hash)
                           (buffer-string)))
               (new-hash (git-branch-off--new-commit d-info new-par msg)))
          (push (cons old-hash new-hash) remap)))
      (let* ((head-hash (magit-git-string "rev-parse" "HEAD"))
             (new-head  (or (cdr (assoc head-hash remap)) new-squash)))
        (magit-call-git "update-ref" (format "refs/heads/%s" branch) new-head))
      (git-branch-off--cascade remap)
      (magit-refresh)
      (message "Squashed %d commits → %s"
               (length sorted-chain) (substring new-squash 0 8)))))

;;; Squash buffer local vars

(defvar-local git-branch-off--squash-chain nil)
(defvar-local git-branch-off--squash-parent nil)
(defvar-local git-branch-off--squash-tree nil)
(defvar-local git-branch-off--squash-bo-p nil)
(defvar-local git-branch-off--squash-branch nil)
(defvar-local git-branch-off--squash-source-line nil)
(defvar-local git-branch-off--squash-dir nil)
(defvar-local git-branch-off--squash-log-buf nil)

(defun git-branch-off--squash-finish ()
  "Apply the squash using the message in the current buffer."
  (interactive)
  (let* ((msg    (string-trim
                  (mapconcat #'identity
                             (seq-remove (lambda (l) (string-prefix-p "#" l))
                                         (split-string
                                          (buffer-substring-no-properties (point-min) (point-max))
                                          "\n"))
                             "\n")))
         (chain  git-branch-off--squash-chain)
         (parent git-branch-off--squash-parent)
         (tree   git-branch-off--squash-tree)
         (bo-p   git-branch-off--squash-bo-p)
         (branch git-branch-off--squash-branch)
         (line   git-branch-off--squash-source-line)
         (dir    git-branch-off--squash-dir))
    (when (string-empty-p msg)
      (user-error "Commit message cannot be empty"))
    (let ((log-buf git-branch-off--squash-log-buf))
      (kill-buffer-and-window)
      (when (buffer-live-p log-buf)
        (with-current-buffer log-buf
          (setq git-branch-off--squash-marks nil)
          (git-branch-off--squash-clear-overlays)))
      (let ((default-directory dir))
        (if bo-p
            (git-branch-off--squash-apply-branch-off chain parent tree msg)
          (git-branch-off--squash-apply-branch chain parent tree msg branch))
        (git-branch-off--reword-refresh-log line)))))

(defun git-branch-off--squash-abort ()
  "Abort the squash."
  (interactive)
  (kill-buffer-and-window)
  (message "squash: aborted"))

(defun git-branch-off-squash ()
  "Squash visually selected commits in the magit-log buffer into one.
Requires a visual selection or marks (m/M).  Opens a pre-filled message buffer
combining the selected commits' messages.  C-c C-c applies, C-c C-k aborts.

For branch-off commits: supports chained and sibling commits.
For regular branch commits: the selected range must be a contiguous chain."
  (interactive)
  (let ((log-buf (if (derived-mode-p 'magit-log-mode)
                     (current-buffer)
                   (or (magit-get-mode-buffer 'magit-log-mode)
                       (user-error "No magit-log buffer found")))))
    (with-current-buffer log-buf
      (let* ((raw  (if git-branch-off--squash-marks
                       git-branch-off--squash-marks
                     (unless (use-region-p)
                       (user-error
                        "No commits selected — mark commits with %s or visually select with V"
                        (substitute-command-keys "\\[git-branch-off-mark]")))
                     (git-branch-off--squash-commits-in-region))))
        (when (< (length raw) 2)
          (user-error "Select at least 2 commits to squash (got %d)" (length raw)))
        (let* ((full  (mapcar (lambda (h) (magit-git-string "rev-parse" h)) raw))
               (bo-p  (cl-every
                       (lambda (h)
                         (equal h (magit-git-string "rev-parse" "--verify"
                                                    (format "refs/branch-off/%s" h))))
                       full))
               (chain-try (and bo-p (git-branch-off--squash-try-chain full)))
               (sort-result
                (cond ((not bo-p) (git-branch-off--squash-build-chain full))
                      (chain-try  chain-try)
                      (t          (git-branch-off--squash-sort-siblings full))))
               (chain  (car sort-result))
               (par    (cdr sort-result))
               (branch (unless bo-p
                         (magit-git-string "symbolic-ref" "--short" "HEAD")))
               (on-branch
                (unless bo-p
                  (and branch
                       (cl-every
                        (lambda (h)
                          (= 0 (call-process "git" nil nil nil
                                             "merge-base" "--is-ancestor" h "HEAD")))
                        chain))))
               (source-line (line-number-at-pos))
               (dir  (magit-toplevel))
               (n    (length chain)))
          (unless (or bo-p on-branch)
            (user-error
             "Selected commits are not all branch-off commits or all on the current branch"))
          (let ((open-buf-fn
                 (lambda (tree-hash)
                   (let* ((combined
                           (mapconcat
                            (lambda (h)
                              (with-temp-buffer
                                (call-process "git" nil t nil "log" "-1" "--format=%B" h)
                                (string-trim (buffer-string))))
                            chain "\n\n"))
                          (buf (get-buffer-create (format "*squash %d commits*" n))))
                     (with-current-buffer buf
                       (erase-buffer)
                       (insert combined)
                       (when git-branch-off--squash-verbose
                         (insert "\n\n")
                         (let ((diff (with-temp-buffer
                                       (let ((default-directory dir))
                                         (call-process "git" nil t nil
                                                       "diff-tree" "-r" "-p" "--no-commit-id"
                                                       par tree-hash))
                                       (buffer-string))))
                           (dolist (line (split-string diff "\n"))
                             (insert "# " line "\n"))))
                       (git-commit-mode)
                       (setq-local default-directory dir)
                       (setq-local git-branch-off--squash-chain chain)
                       (setq-local git-branch-off--squash-parent par)
                       (setq-local git-branch-off--squash-tree tree-hash)
                       (setq-local git-branch-off--squash-bo-p bo-p)
                       (setq-local git-branch-off--squash-branch branch)
                       (setq-local git-branch-off--squash-source-line source-line)
                       (setq-local git-branch-off--squash-dir dir)
                       (setq-local git-branch-off--squash-log-buf log-buf)
                       (local-set-key (kbd "C-c C-c") #'git-branch-off--squash-finish)
                       (local-set-key (kbd "C-c C-k") #'git-branch-off--squash-abort)
                       (setq-local header-line-format
                                   (list " "
                                         (propertize "C-c C-c" 'face 'transient-key)
                                         " squash  "
                                         (propertize "C-c C-k" 'face 'transient-key)
                                         " abort"))
                       (goto-char (point-min)))
                     (pop-to-buffer buf)))))
            (if (and bo-p (null chain-try))
                (git-branch-off--squash-combined-tree par chain open-buf-fn)
              (funcall open-buf-fn
                       (plist-get (git-branch-off--parse-commit (car (last chain)))
                                  :tree)))))))))

;;; Transient suffix (defined here; registered in git-branch-off-setup)

(with-eval-after-load 'transient
  (transient-define-suffix git-branch-off-squash-verbose ()
    "Toggle whether the squash edit buffer shows the diff as comments."
    :transient t
    :description (lambda ()
                   (concat "show diff in edit buffer "
                           (if git-branch-off--squash-verbose
                               (propertize "(on) " 'face 'success)
                             (propertize "(off)" 'face 'shadow))))
    (interactive)
    (setq git-branch-off--squash-verbose (not git-branch-off--squash-verbose))))

(provide 'git-branch-off-squash)
;;; git-branch-off-squash.el ends here

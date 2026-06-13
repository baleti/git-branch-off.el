;;; git-branch-off-search.el --- Git history search  -*- lexical-binding: t; -*-

;; Four commands sharing a common consult-based preview mechanism:
;;   git-branch-off-search-filename-history  — file add/remove events
;;   git-branch-off-search-pickaxe-g         — commits where lines matching regex changed
;;   git-branch-off-search-pickaxe-s         — commits where literal match count changed
;;   git-branch-off-search-all-grep          — git grep across every committed blob

(defun git-branch-off--search-check-deps ()
  "Signal `user-error' if required packages are not loaded."
  (unless (require 'consult nil t)
    (user-error "Package `consult' is required for git-branch-off search commands"))
  (unless (require 'magit nil t)
    (user-error "Package `magit' is required for git-branch-off search commands")))

(defun git-branch-off--search-commit-cache ()
  "Return a hash table mapping full SHA → \"YYYY-MM-DD  author\" for all commits."
  (let ((tbl (make-hash-table :test #'equal :size 256)))
    (with-temp-buffer
      (call-process "git" nil t nil "log" "--all" "--format=%H\t%as\t%an")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match "^\\([0-9a-f]\\{40\\}\\)\t\\([^\t]*\\)\t\\(.*\\)$" line)
            (puthash (match-string 1 line)
                     (concat (match-string 2 line) "  " (match-string 3 line))
                     tbl)))
        (forward-line 1)))
    tbl))

(defun git-branch-off--search-parse-line (line cache)
  "Parse one `git grep -n' history line into a propertized candidate, or nil.
Expected format: <40-sha>:<file>:<lineno>:<content>"
  (when (string-match
         "^\\([0-9a-f]\\{40\\}\\):\\([^:\n]+\\):\\([0-9]+\\):\\(.*\\)$"
         line)
    (let* ((hash   (match-string 1 line))
           (file   (match-string 2 line))
           (lineno (string-to-number (match-string 3 line)))
           (cont   (match-string 4 line))
           (short  (substring hash 0 8))
           (info   (gethash hash cache ""))
           (cand   (concat (propertize short 'face 'magit-hash)
                           ":" (propertize file 'face 'consult-file)
                           ":" (propertize (number-to-string lineno)
                                           'face 'consult-line-number)
                           ": " cont)))
      (put-text-property 0 1 'git-branch-off-hash  hash   cand)
      (put-text-property 0 1 'git-branch-off-file  file   cand)
      (put-text-property 0 1 'git-branch-off-line  lineno cand)
      (put-text-property 0 1 'consult--prefix-group
                         (concat short "  " info) cand)
      cand)))

(defun git-branch-off--search-format-lines (lines cache)
  "Filter and format a batch of git grep output LINES into candidates using CACHE."
  (delq nil (mapcar (lambda (l) (git-branch-off--search-parse-line l cache)) lines)))

(defun git-branch-off--search-apply-highlights (query &optional cur-beg cur-end case-sensitive)
  "Highlight QUERY matches in current buffer; use `isearch' face at CUR-BEG..CUR-END."
  (let ((case-fold-search (not case-sensitive))
        (bg (or (and (fboundp 'doom-color) (doom-color 'orange))
                (face-background 'lazy-highlight nil t)
                "#af7800")))
    (when (and query (not (string-blank-p query)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (regexp-quote query) nil t)
          (let* ((mbeg (match-beginning 0))
                 (mend (match-end 0))
                 (current-p (and cur-beg cur-end
                                 (>= mbeg cur-beg) (<= mend cur-end)))
                 (ov (make-overlay mbeg mend)))
            (overlay-put ov 'face (if current-p 'isearch `(:background ,bg :extend nil)))
            (overlay-put ov 'priority (if current-p 4 3))
            (overlay-put ov 'git-branch-off-query-hl t)))))))

(defun git-branch-off--search-all-grep-builder (input)
  "Command builder: git grep across every committed blob for INPUT."
  (pcase-let ((`(,arg . ,_) (consult--command-split input)))
    (unless (string-blank-p arg)
      (cons (list "sh" "-c"
                  (format "git --no-pager grep -In -e %s \
$(git rev-list --all 2>/dev/null) 2>/dev/null"
                          (shell-quote-argument arg)))
            nil))))

(defun git-branch-off--search-pickaxe-s-builder (input)
  "Command builder: git grep limited to commits where literal count of INPUT changed."
  (pcase-let ((`(,arg . ,_) (consult--command-split input)))
    (unless (string-blank-p arg)
      (let ((q (shell-quote-argument arg)))
        (cons (list "sh" "-c"
                    (format "git --no-pager grep -In -e %s \
$(git log --all -S%s --format=%%H 2>/dev/null | head -n 500) 2>/dev/null"
                            q q))
              nil)))))

(defun git-branch-off--search-pickaxe-g-builder (input)
  "Command builder: git grep limited to commits where a line matching INPUT changed."
  (pcase-let ((`(,arg . ,_) (consult--command-split input)))
    (unless (string-blank-p arg)
      (let ((q (shell-quote-argument arg)))
        (cons (list "sh" "-c"
                    (format "git --no-pager grep -In -e %s \
$(git log --all -G%s --format=%%H 2>/dev/null | head -n 500) 2>/dev/null"
                            q q))
              nil)))))

(defun git-branch-off--search-filename-collect (cache)
  "Return propertized candidates for file add/remove events from git history."
  (let (result cur-hash)
    (with-temp-buffer
      (call-process "git" nil t nil "log" "--all"
                    "--diff-filter=AD" "--name-status"
                    "--format=COMMIT\t%H\t%as\t%an")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^COMMIT\t\\([0-9a-f]\\{40\\}\\)" line)
            (setq cur-hash (match-string 1 line)))
           ((and cur-hash (string-match "^\\([AD]\\)\t\\(.*\\)$" line))
            (let* ((status (match-string 1 line))
                   (file   (match-string 2 line))
                   (short  (substring cur-hash 0 8))
                   (info   (gethash cur-hash cache ""))
                   (label  (if (equal status "A") "Added" "Deleted"))
                   (cand   (concat (propertize short 'face 'magit-hash)
                                   ":" (propertize file 'face 'consult-file)
                                   ":1: [" label "] " file)))
              (put-text-property 0 1 'git-branch-off-hash   cur-hash cand)
              (put-text-property 0 1 'git-branch-off-file   file     cand)
              (put-text-property 0 1 'git-branch-off-line   1        cand)
              (put-text-property 0 1 'git-branch-off-status status   cand)
              (put-text-property 0 1 'consult--prefix-group
                                 (concat short "  " info) cand)
              (push cand result)))))
        (forward-line 1)))
    (nreverse result)))

(defun git-branch-off--search-make-state (on-return &optional highlight-query case-sensitive)
  "Return a consult state function; ON-RETURN is called with the selected candidate."
  (let ((pbuf (get-buffer-create " *git-branch-off-preview*"))
        restore-fn
        line-ov)
    (lambda (action cand)
      (pcase action
        ('preview
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when cand
           (let* ((hash   (get-text-property 0 'git-branch-off-hash cand))
                  (file   (get-text-property 0 'git-branch-off-file cand))
                  (line   (get-text-property 0 'git-branch-off-line cand))
                  (status (get-text-property 0 'git-branch-off-status cand))
                  (rev    (if (equal status "D")
                              (concat hash "^")
                            hash))
                  (win    (selected-window)))
             (when (and hash file line)
               (with-current-buffer pbuf
                 (let* ((inhibit-read-only t)
                        (ext (downcase (or (file-name-extension file) "")))
                        (binary-p (member ext '("pdf" "png" "jpg" "jpeg" "gif"
                                                "bmp" "webp" "ico" "tiff" "svg"
                                                "zip" "gz" "tar" "bz2" "xz"
                                                "jar" "class" "so" "dylib"
                                                "exe" "dll" "o" "elc" "pyc"))))
                   (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
                   (erase-buffer)
                   (when (and (not binary-p)
                              (= 0 (call-process "git" nil t nil "show"
                                                 (format "%s:%s" rev file))))
                     (pcase-let ((`(,fl-defs . ,syn-tbl)
                                  (condition-case nil
                                      (with-temp-buffer
                                        (let ((buffer-file-name file))
                                          (delay-mode-hooks (set-auto-mode))
                                          (setq delayed-mode-hooks nil))
                                        (cons font-lock-defaults (syntax-table)))
                                    (error (cons nil nil)))))
                       (when fl-defs
                         (set-syntax-table syn-tbl)
                         (setq-local font-lock-defaults fl-defs)
                         (font-lock-mode 1)
                         (font-lock-ensure)))
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (if highlight-query
                         (let* ((cur-beg (line-beginning-position))
                                (cur-end (line-end-position))
                                (mbquery (condition-case nil
                                             (with-current-buffer
                                                 (window-buffer (active-minibuffer-window))
                                               (minibuffer-contents-no-properties))
                                           (error nil)))
                                (arg (car (consult--command-split mbquery))))
                           (git-branch-off--search-apply-highlights
                            arg cur-beg cur-end case-sensitive))
                       (setq line-ov
                             (make-overlay (line-beginning-position)
                                           (min (1+ (line-end-position)) (point-max))))
                       (overlay-put line-ov 'face 'consult-preview-line)
                       (overlay-put line-ov 'priority 2))
                     (setq buffer-read-only t))))
               (let ((prev-buf (window-buffer win))
                     (prev-pt  (window-point win)))
                 (setq restore-fn
                       (lambda ()
                         (when (window-live-p win)
                           (set-window-buffer win prev-buf)
                           (set-window-point win prev-pt))))
                 (set-window-buffer win pbuf)
                 (set-window-point win (with-current-buffer pbuf (point))))))))
        ('return
         (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when (buffer-live-p pbuf) (kill-buffer pbuf))
         (when cand (funcall on-return cand)))
        ('exit
         (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when (buffer-live-p pbuf) (kill-buffer pbuf)))))))

(defun git-branch-off--search-state (&optional highlight-query case-sensitive)
  "State: preview git blobs; open blob in magit-find-file on return."
  (git-branch-off--search-make-state
   (lambda (cand)
     (let* ((hash (get-text-property 0 'git-branch-off-hash cand))
            (file (get-text-property 0 'git-branch-off-file cand))
            (line (get-text-property 0 'git-branch-off-line cand)))
       (when (and hash file line)
         (magit-find-file hash file)
         (goto-char (point-min))
         (forward-line (1- line))
         (recenter))))
   highlight-query case-sensitive))

(defun git-branch-off--search-filename-state ()
  "State: preview git blobs; open commit via magit-show-commit on return."
  (git-branch-off--search-make-state
   (lambda (cand)
     (when-let ((hash (get-text-property 0 'git-branch-off-hash cand)))
       (magit-show-commit hash)))))

(defun git-branch-off--search-grep-read (builder prompt &optional highlight-query case-sensitive)
  "Run an async git grep consult session using BUILDER and PROMPT."
  (git-branch-off--search-check-deps)
  (let* ((top   (or (magit-toplevel) (user-error "Not in a git repository")))
         (default-directory top)
         (cache (git-branch-off--search-commit-cache)))
    (consult--read
     (consult--process-collection builder
       :transform (consult--async-transform
                   (lambda (lines) (git-branch-off--search-format-lines lines cache)))
       :file-handler t)
     :prompt prompt
     :lookup #'consult--lookup-member
     :state (git-branch-off--search-state highlight-query case-sensitive)
     :add-history (thing-at-point 'symbol)
     :require-match t
     :category 'consult-grep
     :group #'consult--prefix-group
     :history '(:input consult--grep-history)
     :sort nil)))

(defun git-branch-off-search-all-grep ()
  "Search ALL committed blobs for a pattern (may show duplicates across commits)."
  (interactive)
  (git-branch-off--search-grep-read #'git-branch-off--search-all-grep-builder
                                     "All-commits grep: " t))

(defun git-branch-off-search-pickaxe-s ()
  "Pickaxe -S: search commits where the literal count of a string changed."
  (interactive)
  (git-branch-off--search-grep-read #'git-branch-off--search-pickaxe-s-builder
                                     "Pickaxe -S (count changed): " t t))

(defun git-branch-off-search-pickaxe-g ()
  "Pickaxe -G: search commits where a line matching a regex changed."
  (interactive)
  (git-branch-off--search-grep-read #'git-branch-off--search-pickaxe-g-builder
                                     "Pickaxe -G (regex changed): " t t))

(defun git-branch-off-search-filename-history ()
  "Show all commits where a file was Added or Deleted; filter by filename."
  (interactive)
  (git-branch-off--search-check-deps)
  (let* ((top   (or (magit-toplevel) (user-error "Not in a git repository")))
         (default-directory top)
         (cache (git-branch-off--search-commit-cache))
         (cands (git-branch-off--search-filename-collect cache)))
    (if (null cands)
        (message "No file add/remove events found in git history")
      (consult--read
       cands
       :prompt "File history (add/remove): "
       :lookup #'consult--lookup-member
       :state (git-branch-off--search-filename-state)
       :require-match t
       :category 'consult-grep
       :group #'consult--prefix-group
       :history '(:input git-branch-off--search-filename-history)
       :sort nil))))

(provide 'git-branch-off-search)
;;; git-branch-off-search.el ends here

;;; git-branch-off-stage.el --- Hunk and line staging  -*- lexical-binding: t; -*-

;;; Stage-hunk helpers

(defun git-branch-off--selection-lines ()
  "Return (START-LINE . END-LINE) for the active region, or both = point's line.
Handles the evil/vim visual-mode case where region-end may sit at the
beginning of the line after the visual selection."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (end-line (save-excursion
                         (goto-char end)
                         (when (and (bolp) (> end beg))
                           (forward-line -1))
                         (line-number-at-pos))))
        (cons (line-number-at-pos beg) end-line))
    (cons (line-number-at-pos) (line-number-at-pos))))

(defun git-branch-off--stage-hunks-for-file-lines (file start-line end-line)
  "Stage every unstaged hunk in FILE that overlaps lines START-LINE..END-LINE.
Opens a minimal \\='-U1\\=' diff buffer for precise hunk detection, re-opening it
after each staging because magit refreshes the buffer in place.
Returns the number of hunks staged.  Signals `user-error' when the cursor
or selection misses all hunks, or when FILE has no unstaged changes."
  (let ((count 0)
        (first-pass t))
    (catch 'done
      (while t
        (let ((diff-buf
               (save-window-excursion
                 (magit-with-toplevel
                   (let ((magit-display-buffer-noselect t))
                     (magit-diff-setup-buffer
                      nil nil '("-U1") (list file) 'unstaged nil))))))
          (unwind-protect
              (with-current-buffer diff-buf
                (let* ((file-sec
                        (cl-find-if (lambda (s) (equal (oref s value) file))
                                    (oref magit-root-section children)))
                       (hunk-sec
                        (and file-sec
                             (cl-find-if
                              (lambda (h)
                                (when-let* ((r   (oref h to-range))
                                            (beg (car r))
                                            (len (cadr r)))
                                  (and (<= beg end-line)
                                       (>= (+ beg len) start-line))))
                              (oref file-sec children)))))
                  (when first-pass
                    (setq first-pass nil)
                    (unless file-sec
                      (user-error "No unstaged changes in %s" file))
                    (unless hunk-sec
                      (user-error "%s does not overlap any unstaged hunk in %s"
                                  (if (= start-line end-line)
                                      (format "Line %d" start-line)
                                    (format "Lines %d-%d" start-line end-line))
                                  file)))
                  (unless hunk-sec (throw 'done count))
                  (goto-char (oref hunk-sec start))
                  (magit-stage)
                  (cl-incf count)))
            (when (buffer-live-p diff-buf)
              (kill-buffer diff-buf))))))
    count))

(defun git-branch-off--stage-hunk-at-point ()
  "Stage the unstaged hunk(s) at point, or within the active visual selection.

With no active region: stages the single hunk whose range covers the current
line.  With an active region (evil visual mode): stages every unstaged hunk
that overlaps the selection.

Saves the buffer before staging.  Signals `user-error' if no hunk is found."
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (let* ((file  (or (magit-file-relative-name)
                    (user-error "File is not inside a git repository")))
         (range (git-branch-off--selection-lines)))
    (when (buffer-modified-p)
      (save-buffer))
    (git-branch-off--stage-hunks-for-file-lines file (car range) (cdr range))))

;;; Select-hunk command

(defun git-branch-off-select-hunk ()
  "Select the diff hunk at point in evil visual-line mode.
Parses `git diff -U0' to find the hunk whose new-file range contains
the current line, then activates an evil visual-line selection covering
those lines.  Signals `user-error' when point is not within any hunk."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (let* ((top (or (magit-toplevel) (user-error "Not in a git repository")))
         (rel (file-relative-name buffer-file-name top))
         (cur-line (line-number-at-pos))
         (default-directory top)
         (diff (with-temp-buffer
                 (call-process "git" nil t nil "diff" "-U0" "--" rel)
                 (buffer-string)))
         hunk-start hunk-end)
    (when (string-empty-p diff)
      (user-error "No unstaged changes in %s" rel))
    (with-temp-buffer
      (insert diff)
      (goto-char (point-min))
      (while (and (not hunk-start)
                  (re-search-forward
                   (rx bol "@@ -" (+ digit) (? "," (+ digit))
                       " +" (group (+ digit)) (? "," (group (+ digit)))
                       " @@")
                   nil t))
        (let* ((start (string-to-number (match-string 1)))
               (count (if (match-string 2)
                          (string-to-number (match-string 2))
                        1)))
          (when (and (> count 0)
                     (<= start cur-line)
                     (<= cur-line (+ start count -1)))
            (setq hunk-start start
                  hunk-end   (+ start count -1))))))
    (unless hunk-start
      (user-error "Point is not within a diff hunk (line %d)" cur-line))
    (goto-char (point-min))
    (forward-line (1- hunk-start))
    (let ((beg (line-beginning-position)))
      (forward-line (- hunk-end hunk-start))
      (let ((end-pos (line-end-position)))
        (if (and (bound-and-true-p evil-mode) (fboundp 'evil-visual-select))
            (evil-visual-select beg end-pos 'line)
          (push-mark beg nil t)
          (goto-char end-pos))))))

;;; Amend-hunk commands

(defun git-branch-off-amend-hunk ()
  "Stage the hunk(s) at point / visual selection and amend the previous commit.
Opens the commit message editor."
  (interactive)
  (git-branch-off--stage-hunk-at-point)
  (magit-commit-amend))

(defun git-branch-off-amend-hunk-no-edit ()
  "Stage the hunk(s) at point / visual selection and amend the previous commit.
Reuses the existing commit message without opening an editor."
  (interactive)
  (git-branch-off--stage-hunk-at-point)
  (magit-commit-extend))

;;; Stage-lines command (sub-hunk / line precision)

(defun git-branch-off--patch-from-diff (diff-text rel-path sel-start sel-end)
  "Build a patch from DIFF-TEXT staging all changes in new-file range [SEL-START..SEL-END].
REL-PATH is unused (the header is taken verbatim from DIFF-TEXT).
Handles additions, deletions, and modifications.
Returns (PATCH-STRING . CHANGE-COUNT) or nil when no changes fall in the range."
  (let (file-header output-hunks in-hunk
        context-anchor old-cursor new-cursor
        pending-del-start pending-del-lines pending-del-in-selection
        group-lines group-add-start group-index-start
        group-del-start group-del-lines group-ctx-anchor
        (staged-net-so-far 0))

    (cl-flet
        ((flush-adds ()
           (when group-lines
             (let ((add-count (length group-lines))
                   (del-count (length group-del-lines)))
               (push (list group-del-start group-del-lines
                           group-add-start (nreverse group-lines)
                           group-ctx-anchor group-index-start)
                     output-hunks)
               (cl-incf staged-net-so-far (- add-count del-count)))
             (setq group-lines       nil  group-add-start    nil
                   group-index-start nil  group-del-start    nil
                   group-del-lines   nil  group-ctx-anchor   nil)))
         (flush-dels ()
           (when (and pending-del-lines pending-del-in-selection)
             (let* ((del-count   (length pending-del-lines))
                    (del-ordered (nreverse (copy-sequence pending-del-lines)))
                    (idx-anchor  (+ context-anchor staged-net-so-far)))
               (push (list pending-del-start del-ordered nil nil
                           context-anchor idx-anchor)
                     output-hunks)
               (cl-decf staged-net-so-far del-count)))
           (setq pending-del-start       nil
                 pending-del-lines       nil
                 pending-del-in-selection nil)))

      (dolist (line (split-string diff-text "\n"))
        (cond
         ((string-match
           (rx bol "@@ -" (group (+ digit))
               (? "," (group (+ digit)))
               " +" (group (+ digit))
               (? "," (+ digit)) " @@")
           line)
          (flush-adds)
          (flush-dels)
          (let* ((old-s (string-to-number (match-string 1 line)))
                 (old-c (if (match-string 2 line)
                            (string-to-number (match-string 2 line))
                          1)))
            (setq in-hunk        t
                  old-cursor     old-s
                  new-cursor     (string-to-number (match-string 3 line))
                  context-anchor (if (= old-c 0) old-s (1- old-s)))))

         ((not in-hunk)
          (push line file-header))

         ((string-prefix-p "+" line)
          (if (and (>= new-cursor sel-start) (<= new-cursor sel-end))
              (progn
                (when (null group-lines)
                  (setq group-add-start   new-cursor
                        group-index-start (+ context-anchor 1 staged-net-so-far)
                        group-del-start   pending-del-start
                        group-del-lines   (nreverse (copy-sequence pending-del-lines))
                        group-ctx-anchor  context-anchor
                        pending-del-start        nil
                        pending-del-lines        nil
                        pending-del-in-selection nil))
                (push (substring line 1) group-lines))
            (flush-adds)
            (when pending-del-lines
              (setq context-anchor (1- old-cursor)))
            (setq pending-del-start        nil
                  pending-del-lines        nil
                  pending-del-in-selection nil))
          (setq new-cursor (1+ new-cursor)))

         ((string-prefix-p "-" line)
          (when group-lines (flush-adds))
          (when (null pending-del-start)
            (setq pending-del-start old-cursor
                  pending-del-in-selection (and (>= new-cursor sel-start)
                                                (<= new-cursor sel-end))))
          (push (substring line 1) pending-del-lines)
          (setq old-cursor (1+ old-cursor)))

         ((string-prefix-p " " line)
          (flush-adds)
          (flush-dels)
          (setq context-anchor old-cursor
                old-cursor      (1+ old-cursor)
                new-cursor      (1+ new-cursor)))))

      (flush-adds)
      (flush-dels))

    (when output-hunks
      (setq output-hunks (nreverse output-hunks))
      (let ((header (mapconcat #'identity (nreverse file-header) "\n"))
            body)
        (setq body
              (mapconcat
               (lambda (h)
                 (cl-destructuring-bind
                     (del-start del-lines add-start add-lines _ctx-anchor idx-start) h
                   (cond
                    ((and del-lines add-lines)
                     (concat (format "@@ -%d,%d +%d,%d @@\n"
                                     del-start (length del-lines)
                                     add-start (length add-lines))
                             (mapconcat (lambda (l) (concat "-" l "\n")) del-lines "")
                             (mapconcat (lambda (l) (concat "+" l "\n")) add-lines "")))
                    (del-lines
                     (concat (format "@@ -%d,%d +%d,0 @@\n"
                                     del-start (length del-lines) idx-start)
                             (mapconcat (lambda (l) (concat "-" l "\n")) del-lines "")))
                    (t
                     (concat (format "@@ -%d,0 +%d,%d @@\n"
                                     (1- idx-start) idx-start (length add-lines))
                             (mapconcat (lambda (l) (concat "+" l "\n")) add-lines ""))))))
               output-hunks ""))
        (cons (concat header "\n" body)
              (apply #'+ (mapcar (lambda (h)
                                   (+ (length (nth 1 h))
                                      (length (nth 3 h))))
                                 output-hunks)))))))

(provide 'git-branch-off-stage)
;;; git-branch-off-stage.el ends here

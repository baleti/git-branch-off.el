;;; +magit.el --- Custom Magit commands and configuration  -*- lexical-binding: t; -*-

;;; Diff face customisation


(after! magit
  (custom-set-faces!
    '(magit-diff-added
      :foreground "#98be65" :background "#1e2b1e")
    '(magit-diff-added-highlight
      :foreground "#b0d47a" :background "#263626" :weight bold)
    '(magit-diff-removed
      :foreground "#ff6c6b" :background "#2b1e1e")
    '(magit-diff-removed-highlight
      :foreground "#ff8080" :background "#3a2020" :weight bold)
    '(magit-diff-hunk-heading
      :foreground "#51afef" :background "#1e2535" :weight bold)
    '(magit-diff-hunk-heading-highlight
      :foreground "#7bc8f5" :background "#243050" :weight bold)
    '(magit-section-highlight
      :background "#3d4451" :extend t)))

;;; Stage-hunk helpers

(defun my/magit--selection-lines ()
  "Return (START-LINE . END-LINE) for the active region, or both = point's line.
Handles the evil/vim visual-mode case where region-end may sit at the
beginning of the line after the visual selection."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (end-line (save-excursion
                         (goto-char end)
                         ;; When end falls exactly on a line start (line-visual
                         ;; mode, or cursor parked after a newline), step back
                         ;; so we don't count that line as selected.
                         (when (and (bolp) (> end beg))
                           (forward-line -1))
                         (line-number-at-pos))))
        (cons (line-number-at-pos beg) end-line))
    (cons (line-number-at-pos) (line-number-at-pos))))

(defun my/magit--stage-hunks-for-file-lines (file start-line end-line)
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
                                  ;; overlap: [beg, beg+len] ∩ [start-line, end-line]
                                  (and (<= beg end-line)
                                       (>= (+ beg len) start-line))))
                              (oref file-sec children)))))
                  ;; On the very first pass, turn missing hunks into errors.
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
                  ;; No more overlapping hunks → done.
                  (unless hunk-sec (throw 'done count))
                  (goto-char (oref hunk-sec start))
                  (magit-stage)
                  (cl-incf count)))
            (when (buffer-live-p diff-buf)
              (kill-buffer diff-buf))))))
    count))

(defun my/magit--stage-hunk-at-point ()
  "Stage the unstaged hunk(s) at point, or within the active visual selection.

With no active region: stages the single hunk whose range covers the current
line.  With an active region (evil visual mode): stages every unstaged hunk
that overlaps the selection — so selecting across two hunks stages both.

Saves the buffer before staging.  Signals `user-error' if no hunk is found."
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (let* ((file  (or (magit-file-relative-name)
                    (user-error "File is not inside a git repository")))
         (range (my/magit--selection-lines)))
    (when (buffer-modified-p)
      (save-buffer))
    (my/magit--stage-hunks-for-file-lines file (car range) (cdr range))))

;;; Select-hunk command

(defun my/magit-select-hunk-at-point ()
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

;;; Stage-hunk commands


(defun my/magit-stage-hunk-and-amend ()
  "Stage the hunk(s) at point / visual selection and amend the previous commit.
Opens the commit message editor."
  (interactive)
  (my/magit--stage-hunk-at-point)
  (magit-commit-amend))

(defun my/magit-stage-hunk-and-amend-no-edit ()
  "Stage the hunk(s) at point / visual selection and amend the previous commit.
Reuses the existing commit message without opening an editor."
  (interactive)
  (my/magit--stage-hunk-at-point)
  (magit-commit-extend))

;;; Stage-lines command (sub-hunk / line precision)

(defun my/magit--patch-from-diff (diff-text rel-path sel-start sel-end)
  "Build a patch from DIFF-TEXT staging all changes in new-file range [SEL-START..SEL-END].
REL-PATH is unused (the header is taken verbatim from DIFF-TEXT).
Handles additions, deletions, and modifications:
- Selected +lines are staged; any immediately preceding -lines are included so
  git can anchor the replacement hunk at the right old-file position.
- Pure deletion hunks whose new-file position falls within the selection are
  staged as standalone deletion hunks (no paired addition required).
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
           ;; Emit a standalone deletion hunk if the pending deletions fall
           ;; within the selection.  Only called after a deletion run that was
           ;; NOT consumed by a paired addition.
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
         ;; ── hunk header ──────────────────────────────────────────────────────
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

         ;; ── file header (before first @@) ────────────────────────────────────
         ((not in-hunk)
          (push line file-header))

         ;; ── added line ───────────────────────────────────────────────────────
         ((string-prefix-p "+" line)
          (if (and (>= new-cursor sel-start) (<= new-cursor sel-end))
              (progn
                (when (null group-lines)
                  ;; Capture the deletion run (if any) that immediately precedes
                  ;; these selected lines so the patch is a true replacement hunk
                  ;; anchored at the right old-file position.
                  ;;
                  ;; group-index-start is the insertion position in the index
                  ;; after all previous output hunks have been applied.  It uses
                  ;; context-anchor (not new-cursor) so that non-selected +lines
                  ;; skipped earlier do not shift the anchor.  staged-net-so-far
                  ;; accounts for net lines added by previous output hunks.
                  (setq group-add-start   new-cursor
                        group-index-start (+ context-anchor 1 staged-net-so-far)
                        group-del-start   pending-del-start
                        group-del-lines   (nreverse (copy-sequence pending-del-lines))
                        group-ctx-anchor  context-anchor
                        ;; pending deletions consumed by this group
                        pending-del-start        nil
                        pending-del-lines        nil
                        pending-del-in-selection nil))
                (push (substring line 1) group-lines))
            ;; Non-selected +line: close any open addition group.  If pending
            ;; deletions were paired with this non-selected addition, discard
            ;; them without staging — do NOT call flush-dels here.
            (flush-adds)
            (when pending-del-lines
              ;; Paired deletion consumed by this non-selected addition;
              ;; advance context-anchor past the deleted old-file lines so
              ;; subsequent insertions land at the right index position.
              (setq context-anchor (1- old-cursor)))
            (setq pending-del-start        nil
                  pending-del-lines        nil
                  pending-del-in-selection nil))
          (setq new-cursor (1+ new-cursor)))

         ;; ── removed line ─────────────────────────────────────────────────────
         ((string-prefix-p "-" line)
          ;; If a selected group is already open, close it before accumulating
          ;; more deletions (handles interleaved +/- lines in unusual diffs).
          (when group-lines (flush-adds))
          (when (null pending-del-start)
            (setq pending-del-start old-cursor
                  ;; A deletion is "within the selection" when new-cursor (the
                  ;; new-file position at which the deletion occurs) is in range.
                  pending-del-in-selection (and (>= new-cursor sel-start)
                                                (<= new-cursor sel-end))))
          (push (substring line 1) pending-del-lines)
          (setq old-cursor (1+ old-cursor)))

         ;; ── context line ─────────────────────────────────────────────────────
         ((string-prefix-p " " line)
          (flush-adds)
          ;; A deletion run ending at a context line was not paired with any
          ;; addition — emit it as a standalone deletion hunk if selected.
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
                     ;; Replacement: preceded deletions tell git exactly which
                     ;; old-file lines to replace.
                     (concat (format "@@ -%d,%d +%d,%d @@\n"
                                     del-start (length del-lines)
                                     add-start (length add-lines))
                             (mapconcat (lambda (l) (concat "-" l "\n")) del-lines "")
                             (mapconcat (lambda (l) (concat "+" l "\n")) add-lines "")))
                    (del-lines
                     ;; Pure deletion: idx-start is the new-file position after
                     ;; which the old lines were present.
                     (concat (format "@@ -%d,%d +%d,0 @@\n"
                                     del-start (length del-lines) idx-start)
                             (mapconcat (lambda (l) (concat "-" l "\n")) del-lines "")))
                    (t
                     ;; Pure insertion: idx-start is the absolute index position
                     ;; (after previously applied output hunks) where the new
                     ;; lines are inserted.
                     (concat (format "@@ -%d,0 +%d,%d @@\n"
                                     (1- idx-start) idx-start (length add-lines))
                             (mapconcat (lambda (l) (concat "+" l "\n")) add-lines ""))))))
               output-hunks ""))
        (cons (concat header "\n" body)
              (apply #'+ (mapcar (lambda (h)
                                   (+ (length (nth 1 h))   ; del-lines
                                      (length (nth 3 h)))) ; add-lines
                                 output-hunks)))))))

(defun my/magit-stage-and-commit-selected-lines ()
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
         (range (my/magit--selection-lines)))
    (when (buffer-modified-p) (save-buffer))
    (let* ((default-directory top)
           (diff   (with-temp-buffer
                     (call-process "git" nil t nil "diff" "-U0" "--" rel)
                     (buffer-string)))
           (result (my/magit--patch-from-diff diff rel (car range) (cdr range))))
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
        (magit-commit-create)))))

(map! :after magit
      :leader
      "g c c" #'my/magit-stage-and-commit-selected-lines
      "g c o" #'my/magit-commit-and-branch-off
      "g l l" #'my/magit-log-all-flat
      (:prefix ("g a" . "amend hunk")
       "a" #'my/magit-stage-hunk-and-amend
       "n" #'my/magit-stage-hunk-and-amend-no-edit))

;;; Log / revision navigation

(after! magit
  (defvar my/magit-log-nav-overlay nil)

  (defun my/magit-revision-navigate (move-fn)
    (when-let* ((log-buf (magit-get-mode-buffer 'magit-log-mode)))
      (with-current-buffer log-buf
        (funcall move-fn)
        (mapc #'delete-overlay magit-section-highlight-overlays)
        (setq magit-section-highlight-overlays nil)
        (unless (overlayp my/magit-log-nav-overlay)
          (setq my/magit-log-nav-overlay (make-overlay 1 1))
          (overlay-put my/magit-log-nav-overlay 'face 'magit-section-highlight)
          (overlay-put my/magit-log-nav-overlay 'priority 200))
        (move-overlay my/magit-log-nav-overlay
                      (line-beginning-position)
                      (1+ (line-end-position)))
        (when-let ((commit (magit-section-value-if 'commit)))
          (let ((magit-display-buffer-noselect t))
            (magit-show-commit commit))))))

  (defun my/magit-revision-next ()
    (interactive)
    (my/magit-revision-navigate #'magit-section-forward))

  (defun my/magit-revision-prev ()
    (interactive)
    (my/magit-revision-navigate #'magit-section-backward))

  (add-hook 'magit-log-mode-hook
            (lambda ()
              (setq-local hl-line-sticky-flag t)
              (hl-line-mode 1)))

  (defvar-local my/magit-log-flat nil
    "Non-nil in log buffers opened with `my/magit-log-all-flat'.")

  (defvar my/magit-log-flat--pending nil
    "Dynamic flag set during `my/magit-log-all-flat' so the hook fires correctly.")

  (defun my/magit-log-all-flat ()
    "Log all refs without --graph; commits not ancestral to HEAD are marked with 2-space indent."
    (interactive)
    (let ((my/magit-log-flat--pending t))
      (magit-log-setup-buffer (list "--all") (list "--color" "--decorate" "--topo-order" "-n256") nil)))

  (defun my/magit-log-mark-archive-commits ()
    "Overlay 2-space indent on branched-off commits in flat log buffers.
A commit is considered branched off when it is reachable from any ref
but is not an ancestor of HEAD — regardless of which ref namespace holds it."
    (when (derived-mode-p 'magit-log-mode)
      (when my/magit-log-flat--pending
        (setq-local my/magit-log-flat t))
      (remove-overlays (point-min) (point-max) 'my/archive-marker t))
    (when (and (derived-mode-p 'magit-log-mode)
               (bound-and-true-p my/magit-log-flat))
      (let ((hashes (magit-git-lines "log" "--format=%H" "--all" "--not" "HEAD")))
        (when hashes
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (and (looking-at "^\\([0-9a-f]\\{7,40\\}\\)")
                         (let ((h (match-string 1)))
                           (cl-some (lambda (full) (string-prefix-p h full)) hashes)))
                (let ((ov (make-overlay (point) (point))))
                  (overlay-put ov 'before-string "  ")
                  (overlay-put ov 'my/archive-marker t)))
              (forward-line 1)))))))

  (add-hook 'magit-refresh-buffer-hook #'my/magit-log-mark-archive-commits)

  (defun my/magit-status-tab-dwim ()
    "On a commit section show it; otherwise toggle the section."
    (interactive)
    (if-let ((commit (magit-section-value-if 'commit)))
        (magit-show-commit commit)
      (call-interactively #'magit-section-toggle)))

  (defun my/magit-status-navigate (move-fn)
    (condition-case nil
        (progn
          (funcall move-fn)
          (when-let ((commit (magit-section-value-if 'commit)))
            (let ((magit-display-buffer-noselect t))
              (magit-show-commit commit))))
      (error nil)))

  (defun my/magit-status-next ()
    (interactive)
    (my/magit-status-navigate #'magit-section-forward))

  (defun my/magit-status-prev ()
    (interactive)
    (my/magit-status-navigate #'magit-section-backward))

  (map! :map magit-log-mode-map
        :n "TAB" #'magit-visit-thing)

  (map! :map magit-status-mode-map
        :n "TAB" #'my/magit-status-tab-dwim
        :m "n"   #'my/magit-status-next
        :m "p"   #'my/magit-status-prev)

  (map! :map magit-revision-mode-map
        :n "TAB" #'magit-diff-visit-file
        :n "n" #'my/magit-revision-next
        :n "p" #'my/magit-revision-prev))

;;; Commit reword

(defvar-local my/magit-reword--commit nil)
(defvar-local my/magit-reword--source-buffer nil)
(defvar-local my/magit-reword--source-line nil)
(defvar-local my/magit-reword--from-revision nil)

(defun my/magit-reword--fix-highlight ()
  "Reposition highlights to the current line after a programmatic cursor move."
  (mapc #'delete-overlay magit-section-highlight-overlays)
  (setq magit-section-highlight-overlays nil)
  (hl-line-highlight))

(defun my/magit-reword--refresh-log (line)
  "Refresh the magit-log buffer and restore point to LINE."
  (when-let ((log-buf (magit-get-mode-buffer 'magit-log-mode)))
    (with-current-buffer log-buf (magit-refresh))
    (when-let ((win (get-buffer-window log-buf)))
      (with-selected-window win
        (goto-char (point-min))
        (forward-line (1- line))
        (my/magit-reword--fix-highlight))
      (run-with-timer 0 nil
        (lambda ()
          (when-let ((w (get-buffer-window log-buf)))
            (with-selected-window w
              (forward-line 1)
              (forward-line -1))))))))

(defun my/magit-reword--parse-commit (hash)
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

(defun my/magit-reword--new-commit (info new-parent msg)
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

(defun my/magit-reword--cascade-branch-off (remap)
  "Rewrite all refs/branch-off/* whose parent changed according to REMAP.
Scans all branch-off refs; for each whose parent is a key in REMAP, creates
a new commit with the updated parent, replaces the ref, and adds the mapping
to REMAP.  Repeats until no further refs change, so chains of branch-off
commits are fully propagated.  Returns the augmented remap."
  (let ((bo-refs (split-string
                  (with-temp-buffer
                    (call-process "git" nil t nil "for-each-ref"
                                  "--format=%(refname)" "refs/branch-off/")
                    (buffer-string))
                  "\n" t))
        (changed nil))
    (dolist (ref bo-refs)
      (let* ((bo-hash   (magit-git-string "rev-parse" ref))
             (bo-info   (my/magit-reword--parse-commit bo-hash))
             (bo-parent (plist-get bo-info :parent))
             (new-par   (cdr (assoc bo-parent remap))))
        (when new-par
          (let ((new-bo (my/magit-reword--new-commit
                         bo-info new-par
                         (with-temp-buffer
                           (call-process "git" nil t nil "log" "-1" "--format=%B" bo-hash)
                           (buffer-string)))))
            (magit-call-git "update-ref" (format "refs/branch-off/%s" new-bo) new-bo)
            (magit-call-git "update-ref" "-d" ref)
            (push (cons bo-hash new-bo) remap)
            (setq changed t)))))
    (if changed
        (my/magit-reword--cascade-branch-off remap)
      remap)))

(defun my/magit-reword--apply (hash new-msg)
  "Reword HASH with NEW-MSG using git plumbing.
For branch-off refs, rewrites the commit and cascades through any chained
branch-off descendants.  For current-branch commits, rebases all descendants,
updates the branch ref, then cascades through all affected branch-off refs."
  (let* ((full-hash      (magit-git-string "rev-parse" hash))
         (branch-off-ref (format "refs/branch-off/%s" full-hash))
         (is-branch-off  (equal full-hash
                                (magit-git-string "rev-parse" "--verify" branch-off-ref))))
    (if is-branch-off
        (let* ((info     (my/magit-reword--parse-commit full-hash))
               (new-hash (my/magit-reword--new-commit info nil new-msg)))
          (unless new-hash (user-error "git commit-tree failed"))
          (magit-call-git "update-ref" (format "refs/branch-off/%s" new-hash) new-hash)
          (magit-call-git "update-ref" "-d" branch-off-ref)
          (my/magit-reword--cascade-branch-off (list (cons full-hash new-hash))))
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
               (target-info (my/magit-reword--parse-commit full-hash))
               (new-target  (my/magit-reword--new-commit target-info nil new-msg)))
          (unless new-target (user-error "git commit-tree failed"))
          (push (cons full-hash new-target) remap)
          (dolist (old-hash (cdr chain))
            (let* ((info       (my/magit-reword--parse-commit old-hash))
                   (old-parent (plist-get info :parent))
                   (new-parent (or (cdr (assoc old-parent remap)) old-parent))
                   (msg        (with-temp-buffer
                                 (call-process "git" nil t nil "log" "-1" "--format=%B" old-hash)
                                 (buffer-string)))
                   (new-hash   (my/magit-reword--new-commit info new-parent msg)))
              (push (cons old-hash new-hash) remap)))
          (magit-call-git "update-ref"
                          (format "refs/heads/%s" branch)
                          (cdr (assoc (car (last chain)) remap)))
          (my/magit-reword--cascade-branch-off remap))))))

(defun my/magit-reword--finish ()
  "Reword the target commit using git plumbing."
  (interactive)
  (let ((msg           (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (hash          my/magit-reword--commit)
        (dir           default-directory)
        (source        my/magit-reword--source-buffer)
        (line          my/magit-reword--source-line)
        (from-revision my/magit-reword--from-revision))
    (kill-buffer-and-window)
    (let ((default-directory dir))
      (my/magit-reword--apply hash msg)
      (when (and from-revision (buffer-live-p source))
        (kill-buffer source))
      (my/magit-reword--refresh-log line))))

(defun my/magit-reword--abort ()
  "Abort reword without applying changes."
  (interactive)
  (kill-buffer-and-window)
  (message "reword: aborted"))

(defun my/magit-branch-off-reword (commit)
  "Reword COMMIT message, editing in a dedicated buffer.
Pre-fills the buffer with the current message.  C-c C-c applies, C-c C-k aborts.
Works for both branch commits (rebases descendants and updates any branch-off refs
whose parents are in the rewritten chain) and refs/branch-off/ commits directly."
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
         (buf        (get-buffer-create
                      (format "*reword %s*" (substring commit 0 (min 7 (length commit)))))))
    (with-current-buffer buf
      (erase-buffer)
      (insert msg)
      (git-commit-mode)
      (setq-local default-directory dir)
      (setq-local my/magit-reword--commit commit)
      (setq-local my/magit-reword--source-buffer source-buf)
      (setq-local my/magit-reword--source-line source-line)
      (setq-local my/magit-reword--from-revision from-revision)
      (local-set-key (kbd "C-c C-c") #'my/magit-reword--finish)
      (local-set-key (kbd "C-c C-k") #'my/magit-reword--abort)
      (setq-local header-line-format
                  (list " "
                        (propertize "C-c C-c" 'face 'transient-key)
                        " apply  "
                        (propertize "C-c C-k" 'face 'transient-key)
                        " abort"))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun my/magit-branch-off-remove (commit)
  "Delete the refs/branch-off ref for COMMIT without touching history."
  (interactive (list (or (magit-commit-at-point)
                         (and (derived-mode-p 'magit-revision-mode)
                              magit-buffer-revision)
                         (magit-read-branch-or-commit "Remove branch-off ref for commit"))))
  (let* ((full-hash (magit-git-string "rev-parse" commit))
         (ref (format "refs/branch-off/%s" full-hash)))
    (unless (magit-git-string "rev-parse" "--verify" ref)
      (user-error "No branch-off ref for %s" (substring full-hash 0 8)))
    (magit-call-git "update-ref" "-d" ref)
    (magit-refresh)
    (message "Removed branch-off ref for %s" (substring full-hash 0 8))))

;;; Squash commits (branch-off suite)

(defvar-local my/magit-squash--chain nil)
(defvar-local my/magit-squash--parent nil)
(defvar-local my/magit-squash--tree nil)
(defvar-local my/magit-squash--bo-p nil)
(defvar-local my/magit-squash--branch nil)
(defvar-local my/magit-squash--source-line nil)
(defvar-local my/magit-squash--dir nil)
(defvar-local my/magit-squash--log-buf nil)

(defvar-local my/magit-squash--marks nil
  "Ordered list of commit hashes marked for squashing in this log buffer.")

(defvar-local my/magit-squash--overlays nil
  "Alist of (full-hash . overlay) for marked commits in this log buffer.")

(defface my/magit-squash-marked
  '((t :extend t))
  "Face applied to commit lines marked for squashing.")

(after! doom-themes
  (custom-set-faces!
    `(my/magit-squash-marked
      :background ,(doom-blend (doom-color 'orange) (doom-color 'bg) 0.25)
      :extend t)))

(defun my/magit-squash--clear-overlays ()
  "Delete all squash-mark overlays in the current buffer."
  (dolist (pair my/magit-squash--overlays)
    (delete-overlay (cdr pair)))
  (setq my/magit-squash--overlays nil))

(defun my/magit-squash-mark-commit ()
  "Toggle the squash mark on the commit at point.
The line is highlighted with `my/magit-squash-marked' face.
Marks are used by `my/magit-squash-commits' instead of the visual
selection whenever any are present."
  (interactive)
  (let ((hash (magit-section-value-if 'commit)))
    (unless hash
      (user-error "No commit at point"))
    (let ((full (magit-git-string "rev-parse" hash)))
      (if (member full my/magit-squash--marks)
          (progn
            (setq my/magit-squash--marks (delete full my/magit-squash--marks))
            (when-let ((ov (cdr (assoc full my/magit-squash--overlays))))
              (delete-overlay ov))
            (setq my/magit-squash--overlays
                  (cl-remove full my/magit-squash--overlays :key #'car :test #'equal))
            (message "Unmarked %s (%d remaining)"
                     (substring full 0 8) (length my/magit-squash--marks)))
        (setq my/magit-squash--marks (append my/magit-squash--marks (list full)))
        (let* ((bol (line-beginning-position))
               (eol (save-excursion (end-of-line) (point)))
               (ov  (make-overlay bol (1+ eol) nil t nil)))
          (overlay-put ov 'face 'my/magit-squash-marked)
          (push (cons full ov) my/magit-squash--overlays))
        (message "Marked %s (%d total)"
                 (substring full 0 8) (length my/magit-squash--marks))))))

(defun my/magit-squash--commits-in-region ()
  "Return commit hashes in the active region (display order), adjusted for evil visual-line."
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

(defun my/magit-squash--build-chain (full-hashes)
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

(defun my/magit-squash--try-chain (full-hashes)
  "Try `my/magit-squash--build-chain'; return nil instead of signaling on chain failure."
  (condition-case nil
      (my/magit-squash--build-chain full-hashes)
    (user-error nil)))

(defun my/magit-squash--sort-siblings (full-hashes)
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
select commits that chain (each parent is the previous) or that all branch from the same commit"))
    (cons sorted (car parents))))

(defun my/magit-squash--commit-tree (hash)
  "Return the tree SHA of commit HASH."
  (with-temp-buffer
    (call-process "git" nil t nil "rev-parse" (format "%s^{tree}" hash))
    (string-trim (buffer-string))))

;;; Conflict resolution via smerge (ediff available via C-c ^ e inside smerge)

(defvar-local my/magit-squash--conflict-done-fn nil
  "Continuation called with the resolved blob SHA from the conflict buffer.")

(defvar my/magit-squash--verbose nil
  "When non-nil, append the squash diff as comments to the message buffer.")

(defun my/magit-squash--finish-conflict ()
  "Confirm conflict resolution and resume the in-progress squash."
  (interactive)
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
    (user-error "Unresolved conflicts remain — use smerge (C-c ^ n/p) or ediff (C-c ^ e)"))
  (funcall my/magit-squash--conflict-done-fn))

(defun my/magit-squash--abort-conflict ()
  "Abort the squash from the conflict resolution buffer."
  (interactive)
  (kill-buffer-and-window)
  (message "Squash aborted"))

(defun my/magit-squash--open-conflict-buffer (path commit-hash conflicted-content mode done-fn)
  "Open a smerge buffer for CONFLICTED-CONTENT of PATH.
DONE-FN is called with the resolved blob SHA when the user confirms with C-c C-c."
  (let ((buf (get-buffer-create (format "*squash conflict: %s*" path))))
    (with-current-buffer buf
      (erase-buffer)
      (insert conflicted-content)
      (smerge-mode 1)
      (setq-local my/magit-squash--conflict-done-fn
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
      (local-set-key (kbd "C-c C-c") #'my/magit-squash--finish-conflict)
      (local-set-key (kbd "C-c C-k") #'my/magit-squash--abort-conflict)
      (setq-local header-line-format
                  (list (format " Conflict in %s (from %s) — " path (substring commit-hash 0 8))
                        (propertize "C-c C-c" 'face 'transient-key) " done  "
                        (propertize "C-c C-k" 'face 'transient-key) " abort  "
                        (propertize "C-c ^ e" 'face 'transient-key) " ediff"))
      (goto-char (point-min))
      (ignore-errors (smerge-next)))
    (pop-to-buffer buf)))

(defun my/magit-squash--resolve-path-list (paths by-path commit-hash penv all-resolved-fn)
  "Open smerge for each path in PATHS in turn; when all done call ALL-RESOLVED-FN.
Temp index (in PENV via GIT_INDEX_FILE) is updated after each resolution."
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
        ;; Produce conflict markers in ours-f
        (call-process "git" nil nil nil "merge-file" ours-f base-f thrs-f)
        (ignore-errors (delete-file base-f))
        (ignore-errors (delete-file thrs-f))
        (let ((conflicted (with-temp-buffer
                            (insert-file-contents ours-f)
                            (buffer-string))))
          (ignore-errors (delete-file ours-f))
          (my/magit-squash--open-conflict-buffer
           path commit-hash conflicted mode
           (lambda (resolved-sha)
             (let ((process-environment penv))
               (with-temp-buffer          ; remove all conflict stages
                 (insert (format "0 %s 0\t%s\n" (make-string 40 ?0) path))
                 (call-process-region (point-min) (point-max) "git" t nil nil
                                      "update-index" "--index-info"))
               (with-temp-buffer          ; add resolved blob at stage 0
                 (insert (format "%s %s 0\t%s\n" mode resolved-sha path))
                 (unless (= 0 (call-process-region (point-min) (point-max) "git" t t nil
                                                    "update-index" "--index-info"))
                   (user-error "git update-index failed for %s" path))))
             (my/magit-squash--resolve-path-list rest by-path commit-hash penv
                                                  all-resolved-fn))))))))

(defun my/magit-squash--merge-commits (remaining parent-tree current-tree temp-index penv done-fn)
  "Iteratively 3-way-merge REMAINING sibling commits into CURRENT-TREE.
PARENT-TREE is the constant base (shared ancestor of all siblings).
Calls DONE-FN with the final tree hash; may suspend for interactive conflict resolution."
  (if (null remaining)
      (funcall done-fn current-tree)
    (let* ((process-environment penv)
           (h        (car remaining))
           (rest     (cdr remaining))
           (bon-tree (my/magit-squash--commit-tree h)))
      (call-process "git" nil nil nil "read-tree" "-i" "-m" parent-tree current-tree bon-tree)
      (let ((unmerged (with-temp-buffer
                        (call-process "git" nil t nil "ls-files" "--unmerged")
                        (buffer-string))))
        (if (string-empty-p (string-trim unmerged))
            ;; Clean merge: write tree and continue synchronously
            (let ((new-tree (with-temp-buffer
                              (unless (= 0 (call-process "git" nil t nil "write-tree"))
                                (user-error "git write-tree failed merging %s" (substring h 0 8)))
                              (string-trim (buffer-string)))))
              (my/magit-squash--merge-commits rest parent-tree new-tree temp-index penv done-fn))
          ;; Conflict: parse unmerged entries, open smerge for each file, then resume
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
              (my/magit-squash--resolve-path-list
               paths by-path h penv
               (lambda ()
                 (let ((new-tree (let ((process-environment penv))
                                   (with-temp-buffer
                                     (unless (= 0 (call-process "git" nil t nil "write-tree"))
                                       (user-error "git write-tree failed after resolving %s"
                                                   (substring h 0 8)))
                                     (string-trim (buffer-string))))))
                   (my/magit-squash--merge-commits
                    rest parent-tree new-tree temp-index penv done-fn)))))))))))

(defun my/magit-squash--combined-tree (parent-hash sorted-commits done-fn)
  "Async: build a merged tree from sibling SORTED-COMMITS branched from PARENT-HASH.
Calls DONE-FN with the merged tree hash; may open smerge buffers for conflicts."
  (let* ((parent-tree (my/magit-squash--commit-tree parent-hash))
         (temp-index  (make-temp-file "git-squash" nil))
         (penv (append (list (format "GIT_INDEX_FILE=%s" temp-index)) process-environment)))
    (let ((process-environment penv))
      (with-temp-buffer
        (unless (= 0 (call-process "git" nil t nil "read-tree" parent-tree))
          (ignore-errors (delete-file temp-index))
          (user-error "git read-tree failed: %s" (buffer-string)))))
    (my/magit-squash--merge-commits
     sorted-commits parent-tree parent-tree temp-index penv
     (lambda (tree-hash)
       (ignore-errors (delete-file temp-index))
       (funcall done-fn tree-hash)))))

(defun my/magit-squash--make-info (tree-hash first-info)
  "Build a commit info plist: TREE-HASH as tree, author/committer identity from FIRST-INFO."
  (list :tree            tree-hash
        :author-name     (plist-get first-info :author-name)
        :author-email    (plist-get first-info :author-email)
        :author-date     (plist-get first-info :author-date)
        :committer-name  (plist-get first-info :committer-name)
        :committer-email (plist-get first-info :committer-email)
        :committer-date  (plist-get first-info :committer-date)))

(defun my/magit-squash--apply-branch-off (sorted-chain parent-of-first tree-hash new-msg)
  "Squash SORTED-CHAIN branch-off commits into one new branch-off commit."
  (let* ((first-info (my/magit-reword--parse-commit (car sorted-chain)))
         (info       (my/magit-squash--make-info tree-hash first-info))
         (new-hash   (my/magit-reword--new-commit info parent-of-first new-msg)))
    (unless new-hash (user-error "git commit-tree failed"))
    (magit-call-git "update-ref" (format "refs/branch-off/%s" new-hash) new-hash)
    (dolist (h sorted-chain)
      (magit-call-git "update-ref" "-d" (format "refs/branch-off/%s" h)))
    (my/magit-reword--cascade-branch-off
     (mapcar (lambda (h) (cons h new-hash)) sorted-chain))
    (magit-refresh)
    (message "Squashed %d branch-off commits → %s"
             (length sorted-chain) (substring new-hash 0 8))))

(defun my/magit-squash--apply-branch (sorted-chain parent-of-first tree-hash new-msg branch)
  "Squash SORTED-CHAIN branch commits into one, rebasing HEAD descendants."
  (let* ((first-info (my/magit-reword--parse-commit (car sorted-chain)))
         (info       (my/magit-squash--make-info tree-hash first-info))
         (new-squash (my/magit-reword--new-commit info parent-of-first new-msg)))
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
        (let* ((d-info   (my/magit-reword--parse-commit old-hash))
               (old-par  (plist-get d-info :parent))
               (new-par  (or (cdr (assoc old-par remap)) old-par))
               (msg      (with-temp-buffer
                           (call-process "git" nil t nil "log" "-1" "--format=%B" old-hash)
                           (buffer-string)))
               (new-hash (my/magit-reword--new-commit d-info new-par msg)))
          (push (cons old-hash new-hash) remap)))
      (let* ((head-hash (magit-git-string "rev-parse" "HEAD"))
             (new-head  (or (cdr (assoc head-hash remap)) new-squash)))
        (magit-call-git "update-ref" (format "refs/heads/%s" branch) new-head))
      (my/magit-reword--cascade-branch-off remap)
      (magit-refresh)
      (message "Squashed %d commits → %s"
               (length sorted-chain) (substring new-squash 0 8)))))

(defun my/magit-squash--finish ()
  "Apply the squash using the message in the current buffer."
  (interactive)
  (let* ((msg    (string-trim
                  (mapconcat #'identity
                             (seq-remove (lambda (l) (string-prefix-p "#" l))
                                         (split-string
                                          (buffer-substring-no-properties (point-min) (point-max))
                                          "\n"))
                             "\n")))
         (chain  my/magit-squash--chain)
         (parent my/magit-squash--parent)
         (tree   my/magit-squash--tree)
         (bo-p   my/magit-squash--bo-p)
         (branch my/magit-squash--branch)
         (line   my/magit-squash--source-line)
         (dir    my/magit-squash--dir))
    (when (string-empty-p msg)
      (user-error "Commit message cannot be empty"))
    (let ((log-buf my/magit-squash--log-buf))
      (kill-buffer-and-window)
      (when (buffer-live-p log-buf)
        (with-current-buffer log-buf
          (setq my/magit-squash--marks nil)
          (my/magit-squash--clear-overlays)))
      (let ((default-directory dir))
        (if bo-p
            (my/magit-squash--apply-branch-off chain parent tree msg)
          (my/magit-squash--apply-branch chain parent tree msg branch))
        (my/magit-reword--refresh-log line)))))

(defun my/magit-squash--abort ()
  "Abort the squash."
  (interactive)
  (kill-buffer-and-window)
  (message "squash: aborted"))

(defun my/magit-squash-commits ()
  "Squash visually selected commits in the magit-log buffer into one.
Requires a visual selection; signals an error if none is active.
Opens a pre-filled message buffer combining the selected commits' messages.
C-c C-c applies, C-c C-k aborts.

For branch-off commits: supports both chained commits (each parent is the
previous) and sibling commits (all branched from the same parent commit).
For regular branch commits: the selected range must be a contiguous chain.
Respects branch-off refs and rebases HEAD after squashing branch commits."
  (interactive)
  (let ((log-buf (if (derived-mode-p 'magit-log-mode)
                     (current-buffer)
                   (or (magit-get-mode-buffer 'magit-log-mode)
                       (user-error "No magit-log buffer found")))))
    (with-current-buffer log-buf
      (let* ((raw  (if my/magit-squash--marks
                       ;; Marks take priority; leave overlays visible until
                       ;; finish so abort restores the user's selection.
                       my/magit-squash--marks
                     (unless (use-region-p)
                       (user-error
                        "No commits selected — mark commits with %s or visually select with V"
                        (substitute-command-keys "\\[my/magit-squash-mark-commit]")))
                     (my/magit-squash--commits-in-region))))
        (when (< (length raw) 2)
          (user-error "Select at least 2 commits to squash (got %d)" (length raw)))
        (let* ((full  (mapcar (lambda (h) (magit-git-string "rev-parse" h)) raw))
               ;; Detect branch-off before sorting so we can pick the right strategy
               (bo-p  (cl-every
                       (lambda (h)
                         (equal h (magit-git-string "rev-parse" "--verify"
                                                    (format "refs/branch-off/%s" h))))
                       full))
               ;; For branch-off: try chain first, fall back to sibling grouping.
               ;; For branch commits: require a chain (signal on failure).
               (chain-try (and bo-p (my/magit-squash--try-chain full)))
               (sort-result
                (cond ((not bo-p) (my/magit-squash--build-chain full))
                      (chain-try  chain-try)
                      (t          (my/magit-squash--sort-siblings full))))
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
                       (when my/magit-squash--verbose
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
                       (setq-local my/magit-squash--chain chain)
                       (setq-local my/magit-squash--parent par)
                       (setq-local my/magit-squash--tree tree-hash)
                       (setq-local my/magit-squash--bo-p bo-p)
                       (setq-local my/magit-squash--branch branch)
                       (setq-local my/magit-squash--source-line source-line)
                       (setq-local my/magit-squash--dir dir)
                       (setq-local my/magit-squash--log-buf log-buf)
                       (local-set-key (kbd "C-c C-c") #'my/magit-squash--finish)
                       (local-set-key (kbd "C-c C-k") #'my/magit-squash--abort)
                       (setq-local header-line-format
                                   (list " "
                                         (propertize "C-c C-c" 'face 'transient-key)
                                         " squash  "
                                         (propertize "C-c C-k" 'face 'transient-key)
                                         " abort"))
                       (goto-char (point-min)))
                     (pop-to-buffer buf)))))
            (if (and bo-p (null chain-try))
                (my/magit-squash--combined-tree par chain open-buf-fn)
              (funcall open-buf-fn
                       (plist-get (my/magit-reword--parse-commit (car (last chain)))
                                  :tree)))))))))

(after! magit
  ;; Define here so transient is guaranteed loaded (magit requires it)
  (transient-define-suffix my/magit-squash-toggle-verbose ()
    "Toggle whether the branch-off edit buffer shows the diff as comments."
    :transient t
    :description (lambda ()
                   (concat "show diff in edit buffer "
                           (if my/magit-squash--verbose
                               (propertize "(on) " 'face 'success)
                             (propertize "(off)" 'face 'shadow))))
    (interactive)
    (setq my/magit-squash--verbose (not my/magit-squash--verbose)))
  ;; Rebase transient — Branch-off section
  (ignore-errors (transient-remove-suffix 'magit-rebase "W"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "K"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "S"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "v"))
  (transient-append-suffix 'magit-rebase '(2)
    ["Branch-off"
     [("W" "reword" my/magit-branch-off-reword)
      ("K" "remove" my/magit-branch-off-remove)
      ("S" "squash" my/magit-squash-commits)]
     [("v" my/magit-squash-toggle-verbose)]])
  ;; Merge transient — Marker section (appended after group 1 = "Actions")
  (ignore-errors (transient-remove-suffix 'magit-merge "M"))
  (transient-append-suffix 'magit-merge '(1)
    ["Marker"
     ("M" "mark for squash" my/magit-squash-mark-commit)]))

;;; Commit-and-branch-off

(defun my/magit--revert-committed-in-buffer (rel top &optional skip-del-contents)
  "Remove changes introduced by HEAD vs HEAD~1 from the buffer visiting REL.
Uses content-based matching so adjacent unstaged changes in the same file
don't cause conflicts.  Handles additions (delete), modifications (replace),
and deletions (re-insert after the preceding context line).
SKIP-DEL-CONTENTS is an optional list of line-content strings; any pure-deletion
hunk whose entire content is a subset of this list is skipped (the lines were
intentionally committed as ctx-line deletions and must stay gone from the
working tree).
Returns t on success, nil if any hunk could not be located."
  (let* ((diff   (with-temp-buffer
                   (call-process "git" nil t nil "diff" "HEAD~1" "HEAD" "--" rel)
                   (buffer-string)))
         (buf    (find-buffer-visiting (expand-file-name rel top)))
         (hunks  nil)
         ;; Parse diff into (ctx-before del-lines add-lines) triples per hunk
         (cur-ctx nil) (cur-del nil) (cur-add nil) (in-hunk nil))
    (dolist (line (split-string diff "\n"))
      (cond
       ((or (string-prefix-p "diff " line) (string-prefix-p "index " line)
            (string-prefix-p "--- "  line) (string-prefix-p "+++ "  line)) nil)
       ((string-match "^@@" line)
        (when in-hunk
          (push (list (nreverse cur-ctx) (nreverse cur-del) (nreverse cur-add)) hunks)
          (setq cur-ctx nil cur-del nil cur-add nil))
        (setq in-hunk t))
       ((not in-hunk) nil)
       ((string-prefix-p " " line)
        (when (null cur-del) (push (substring line 1) cur-ctx)))
       ((string-prefix-p "-" line) (push (substring line 1) cur-del))
       ((string-prefix-p "+" line) (push (substring line 1) cur-add))))
    (when in-hunk
      (push (list (nreverse cur-ctx) (nreverse cur-del) (nreverse cur-add)) hunks))
    (setq hunks (nreverse hunks))
    (when (and buf hunks)
      (with-current-buffer buf
        (let ((all-ok t))
          ;; Apply in reverse order so earlier hunks aren't shifted by later ones.
          (dolist (h (nreverse hunks))
            (let* ((ctx   (nth 0 h))
                   (del   (nth 1 h))
                   (add   (nth 2 h))
                   (ok
                    (cond
                     ;; Modification: replace the added content with the deleted content.
                     ((and del add)
                      (let ((search (mapconcat #'identity add "\n")))
                        (save-excursion
                          (goto-char (point-min))
                          (if (search-forward search nil t)
                              (progn (replace-match
                                      (mapconcat #'identity del "\n") t t)
                                     t)
                            nil))))
                     ;; Pure addition: delete the block from the working tree.
                     (add
                      (let ((search (concat (mapconcat #'identity add "\n") "\n")))
                        (save-excursion
                          (goto-char (point-min))
                          (if (search-forward search nil t)
                              (progn (delete-region (match-beginning 0) (match-end 0))
                                     t)
                            nil))))
                     ;; Pure deletion: re-insert after the last context line before it,
                     ;; unless every deleted line is a ctx-line we intentionally removed.
                     (del
                      (if (and skip-del-contents
                               (cl-every (lambda (l) (member l skip-del-contents)) del))
                          t  ; ctx-line committed as deletion — leave it gone
                        (let ((anchor (car (last ctx))))
                          (if (null anchor)
                              nil
                            (save-excursion
                              (goto-char (point-min))
                              (if (search-forward (concat anchor "\n") nil t)
                                  (progn
                                    (insert (mapconcat #'identity del "\n") "\n")
                                    t)
                                nil))))))
                     (t t))))
              (unless ok (setq all-ok nil))))
          (when all-ok (save-buffer) (revert-buffer t t))
          all-ok)))))

(defun my/magit--ctx-positions-in-selection (diff-text sel-start sel-end)
  "Return sorted list of working-tree line numbers in [SEL-START..SEL-END] not covered by DIFF-TEXT.
These are unchanged lines the user has selected."
  (let ((plus-positions nil)
        (new-cursor 0))
    (dolist (line (split-string diff-text "\n"))
      (cond
       ((string-match
         (rx bol "@@ " (+ nonl) " +" (group (+ digit)) (? "," (+ digit)) " @@")
         line)
        (setq new-cursor (string-to-number (match-string 1 line))))
       ((string-prefix-p "+" line)
        (when (and (>= new-cursor sel-start) (<= new-cursor sel-end))
          (push new-cursor plus-positions))
        (setq new-cursor (1+ new-cursor)))
       ((string-prefix-p " " line)
        (setq new-cursor (1+ new-cursor)))))
    (cl-loop for pos from sel-start to sel-end
             unless (memq pos plus-positions)
             collect pos)))

(defun my/magit--stage-ctx-deletions (rel ctx-lines)
  "Remove CTX-LINES from REL's current staged index entry.
Reads the index blob that the selection patch already staged, removes each
ctx-line by content-based search, and writes the result back to the index.
CTX-LINES is ((WT-POS . CONTENT) ...)."
  (let* (;; Read from the STAGED index, not the working-tree buffer.  The
         ;; staging-patch has already been applied --cached, so the index
         ;; contains exactly the selected diff changes; we must not replace
         ;; that with the full working-tree blob.
         (index-content
          (with-temp-buffer
            (unless (zerop (call-process "git" nil t nil
                                         "cat-file" "-p" (format ":%s" rel)))
              (error "git cat-file -p :%s failed: %s" rel (buffer-string)))
            (buffer-string)))
         (new-content
          (with-temp-buffer
            (insert index-content)
            (dolist (cl (sort (copy-sequence ctx-lines)
                              (lambda (a b) (> (car a) (car b)))))
              (let ((search (concat (cdr cl) "\n")))
                (goto-char (point-max))
                (unless (search-backward search nil t)
                  (error "Ctx-line %S not found in staged index for %s" (cdr cl) rel))
                (delete-region (match-beginning 0) (match-end 0))))
            (buffer-string)))
         (blob-sha
          (with-temp-buffer
            (insert new-content)
            (unless (zerop (call-process-region (point-min) (point-max)
                                                "git" t t nil
                                                "hash-object" "-w" "--stdin"))
              (error "git hash-object failed: %s" (buffer-string)))
            (string-trim (buffer-string))))
         (ls-out
          (with-temp-buffer
            (call-process "git" nil t nil "ls-files" "-s" "--" rel)
            (buffer-string)))
         (mode (if (string-match "\\`\\([0-9]+\\)" ls-out)
                   (match-string 1 ls-out)
                 "100644")))
    (with-temp-buffer
      (unless (zerop (call-process "git" nil t nil "update-index" "--cacheinfo"
                                   (format "%s,%s,%s" mode blob-sha rel)))
        (error "git update-index --cacheinfo failed: %s" (buffer-string))))))

(defun my/magit--adjust-ctx-positions (ctx-lines staging-patch)
  "Return CTX-LINES with positions adjusted for lines removed by reversing STAGING-PATCH.
The staging patch added certain lines to the working tree; reversing it removes
them, shifting ctx-lines that came after each removed block upward.
CTX-LINES is ((WT-POS . CONTENT) ...).  Returns ((BUF-POS . CONTENT) ...)."
  (let ((added-ranges
         (with-temp-buffer
           (insert-file-contents staging-patch)
           (let (ranges)
             (goto-char (point-min))
             (while (re-search-forward
                     (rx bol "@@ " (+ nonl) " +" (group (+ digit))
                         (? "," (group (+ digit))) " @@")
                     nil t)
               (let* ((start (string-to-number (match-string 1)))
                      (count (if (match-string 2)
                                 (string-to-number (match-string 2))
                               1)))
                 (when (> count 0)
                   (push (cons start count) ranges))))
             (sort (nreverse ranges) (lambda (a b) (< (car a) (car b))))))))
    (mapcar (lambda (cl)
              (let ((pos (car cl))
                    (shift 0))
                (dolist (r added-ranges)
                  ;; Lines in [rstart, rstart+rcount-1] will be removed.
                  ;; Any line at position >= rstart+rcount shifts up by rcount.
                  (when (<= (+ (car r) (cdr r)) pos)
                    (cl-incf shift (cdr r))))
                (cons (- pos shift) (cdr cl))))
            ctx-lines)))

(defun my/magit--delete-ctx-lines-from-worktree (rel ctx-lines top)
  "Delete CTX-LINES from the working-tree buffer of REL by exact line number.
CTX-LINES is ((BUF-LINE . CONTENT) ...) where BUF-LINE is the line number in
the current buffer state (already adjusted for any prior patch reversals).
Verifies the content matches before deleting; returns t on success."
  (let ((buf (find-buffer-visiting (expand-file-name rel top))))
    (if (null buf)
        nil
      (with-current-buffer buf
        (let* ((all-ok t)
               ;; Bottom-to-top so each deletion doesn't shift positions of
               ;; lines still to delete.
               (ctx-sorted (sort (copy-sequence ctx-lines)
                                 (lambda (a b) (> (car a) (car b))))))
          (dolist (cl ctx-sorted)
            (let* ((line-no  (car cl))
                   (expected (cdr cl)))
              (goto-char (point-min))
              (forward-line (1- line-no))
              (if (string= expected
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                  (delete-region (line-beginning-position)
                                 (min (1+ (line-end-position)) (point-max)))
                (setq all-ok nil))))
          (when all-ok (save-buffer))
          all-ok)))))

(defun my/magit-commit-and-branch-off ()
  "Stage the selected lines, commit, preserve under refs/branch-off/, then rewind.

Requires an active region.  Stages all lines within the selection at line
precision — additions, deletions, modifications, and unchanged lines (which
are committed as deletions and removed from the working tree).

The commit is preserved under refs/branch-off/<full-hash>.  HEAD and the
index are rewound with --mixed.  The branched-off changes are then removed
from the working tree so adjacent unstaged changes in the same file are
never disturbed."
  (interactive)
  (unless (use-region-p)
    (user-error "Select lines first"))
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (let* ((top (or (magit-toplevel) (user-error "Not in a git repository")))
         (default-directory top)
         (rel (file-relative-name buffer-file-name top))
         (commit-msg (read-string "Commit message: "))
         staging-patch
         ctx-lines)    ; ((wt-pos . content) ...) for unchanged selected lines
    (when (string-empty-p commit-msg)
      (user-error "Commit message cannot be empty"))
    (let* ((range (my/magit--selection-lines)))
      (when (buffer-modified-p) (save-buffer))
      (let* ((sel-start (car range))
             (sel-end   (cdr range))
             (diff      (with-temp-buffer
                          (call-process "git" nil t nil "diff" "-U0" "--" rel)
                          (buffer-string)))
             (result    (my/magit--patch-from-diff diff rel sel-start sel-end))
             (ctx-pos   (my/magit--ctx-positions-in-selection diff sel-start sel-end)))
        (setq ctx-lines
              (when ctx-pos
                (save-excursion
                  (mapcar (lambda (pos)
                            (goto-char (point-min))
                            (forward-line (1- pos))
                            (cons pos (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position))))
                          ctx-pos))))
        (unless (or result ctx-lines)
          (user-error "No changes in selection to stage"))
        ;; Stage diff changes (additions/deletions).
        (when result
          (setq staging-patch (make-temp-file "magit-lines" nil ".patch"))
          (write-region (car result) nil staging-patch nil 'silent)
          (with-temp-buffer
            (let ((exit (call-process "git" staging-patch (list t t) nil
                                      "apply" "--cached" "--unidiff-zero" "--")))
              (unless (and (integerp exit) (= exit 0))
                (ignore-errors (delete-file staging-patch))
                (setq staging-patch nil)
                (user-error "git apply --cached failed:\n%s" (buffer-string))))))
        ;; Stage unchanged-line deletions via index-blob update.
        (when ctx-lines
          (condition-case err
              (my/magit--stage-ctx-deletions rel ctx-lines)
            (error
             (when staging-patch
               (magit-call-git "reset" "HEAD" "--" rel))
             (user-error "Failed to stage unchanged lines: %s"
                         (error-message-string err)))))))
    (magit-call-git "commit" "-m" commit-msg)
    (let ((hash (magit-git-string "rev-parse" "HEAD")))
      (magit-call-git "update-ref" (format "refs/branch-off/%s" hash) hash)
      ;; Remove the branched-off changes from the working tree.
      ;; If there were diff changes, reverse the staging patch (falling back to
      ;; content-based reversal).  Then delete ctx-lines by exact position.
      (unwind-protect
          (let* ((diff-ok
                  (if staging-patch
                      (let ((exit (call-process "git" nil nil nil
                                                "apply" "-R" "--unidiff-zero"
                                                "--" staging-patch)))
                        (if (and (integerp exit) (= exit 0))
                            (progn (revert-buffer t t) t)
                          ;; Fallback: content-based reversal.  Pass ctx-line
                          ;; contents so the helper skips re-inserting them.
                          (my/magit--revert-committed-in-buffer
                           rel top (mapcar #'cdr ctx-lines))))
                    t))  ; ctx-only selection: nothing to reverse-apply
                 ;; Only attempt ctx-line deletion after diff changes are gone —
                 ;; otherwise ctx-lines would be removed from their original
                 ;; positions while additions still sit above them.
                 (adj-ctx
                  (when (and ctx-lines diff-ok)
                    (if staging-patch
                        (my/magit--adjust-ctx-positions ctx-lines staging-patch)
                      ctx-lines)))
                 (ctx-ok
                  (cond
                   ((null ctx-lines) t)
                   (diff-ok (my/magit--delete-ctx-lines-from-worktree
                             rel adj-ctx top))
                   (t nil)))
                 (ok (and diff-ok ctx-ok)))
            (unless ok
              (message "Warning: could not remove branched-off changes from working tree; \
check %s manually" (file-name-nondirectory buffer-file-name))))
        (when staging-patch (ignore-errors (delete-file staging-patch))))
      ;; Rewind HEAD and index; working tree is already correct.
      (condition-case err
          (magit-call-git "reset" "--mixed" "HEAD~1")
        (error
         (magit-refresh)
         (user-error "refs/branch-off/%s created but reset failed: %s"
                     (substring hash 0 8) (error-message-string err))))
      (magit-refresh)
      (message "Branched off %s — changes removed from working tree" (substring hash 0 8)))))

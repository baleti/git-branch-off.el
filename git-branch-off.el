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

(defun branch-off/magit--selection-lines ()
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

(defun branch-off/magit--stage-hunks-for-file-lines (file start-line end-line)
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

(defun branch-off/magit--stage-hunk-at-point ()
  "Stage the unstaged hunk(s) at point, or within the active visual selection.

With no active region: stages the single hunk whose range covers the current
line.  With an active region (evil visual mode): stages every unstaged hunk
that overlaps the selection — so selecting across two hunks stages both.

Saves the buffer before staging.  Signals `user-error' if no hunk is found."
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (let* ((file  (or (magit-file-relative-name)
                    (user-error "File is not inside a git repository")))
         (range (branch-off/magit--selection-lines)))
    (when (buffer-modified-p)
      (save-buffer))
    (branch-off/magit--stage-hunks-for-file-lines file (car range) (cdr range))))

;;; Select-hunk command

(defun branch-off/magit-select-hunk ()
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


(defun branch-off/magit-amend-hunk ()
  "Stage the hunk(s) at point / visual selection and amend the previous commit.
Opens the commit message editor."
  (interactive)
  (branch-off/magit--stage-hunk-at-point)
  (magit-commit-amend))

(defun branch-off/magit-amend-hunk-no-edit ()
  "Stage the hunk(s) at point / visual selection and amend the previous commit.
Reuses the existing commit message without opening an editor."
  (interactive)
  (branch-off/magit--stage-hunk-at-point)
  (magit-commit-extend))

;;; Stage-lines command (sub-hunk / line precision)

(defun branch-off/magit--patch-from-diff (diff-text rel-path sel-start sel-end)
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

(defun branch-off/worktree--current-branch-off ()
  "Return the nearest branch-off hash in HEAD's ancestry when inside a branch-off worktree.
A branch-off worktree has a detached HEAD and lives at .worktree/<40-hex-chars>.
Walks from HEAD back to the initial commit looking for refs/branch-off/<hash>.
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

(defun branch-off/magit-stage-and-commit ()
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
         (range (branch-off/magit--selection-lines)))
    (when (buffer-modified-p) (save-buffer))
    (let* ((default-directory top)
           (diff   (with-temp-buffer
                     (call-process "git" nil t nil "diff" "-U0" "--" rel)
                     (buffer-string)))
           (result (branch-off/magit--patch-from-diff diff rel (car range) (cdr range))))
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
        (when-let ((old-bo (branch-off/worktree--current-branch-off)))
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

(map! :after magit
      :leader
      "g c c" #'branch-off/magit-stage-and-commit
      "g c o" #'branch-off/magit-stage-and-commit-and-branch-off
      "g l l" #'branch-off/magit-log
      "g w" #'branch-off/create-worktree
      (:prefix ("g a" . "amend hunk")
       "a" #'branch-off/magit-amend-hunk
       "n" #'branch-off/magit-amend-hunk-no-edit))

;;; Log / revision navigation

(after! magit
  (defvar branch-off/magit-log-nav-overlay nil)

  (defun branch-off/magit-revision-navigate (move-fn)
    (when-let* ((log-buf (magit-get-mode-buffer 'magit-log-mode)))
      (with-current-buffer log-buf
        (funcall move-fn)
        (mapc #'delete-overlay magit-section-highlight-overlays)
        (setq magit-section-highlight-overlays nil)
        (unless (overlayp branch-off/magit-log-nav-overlay)
          (setq branch-off/magit-log-nav-overlay (make-overlay 1 1))
          (overlay-put branch-off/magit-log-nav-overlay 'face 'magit-section-highlight)
          (overlay-put branch-off/magit-log-nav-overlay 'priority 200))
        (move-overlay branch-off/magit-log-nav-overlay
                      (line-beginning-position)
                      (1+ (line-end-position)))
        (when-let ((commit (magit-section-value-if 'commit)))
          (let ((magit-display-buffer-noselect t))
            (magit-show-commit commit))))))

  (defun branch-off/magit-revision-next ()
    (interactive)
    (branch-off/magit-revision-navigate #'magit-section-forward))

  (defun branch-off/magit-revision-prev ()
    (interactive)
    (branch-off/magit-revision-navigate #'magit-section-backward))

  (add-hook 'magit-log-mode-hook
            (lambda ()
              (setq-local hl-line-sticky-flag t)
              (hl-line-mode 1)))

  (defvar-local branch-off/magit-log-flat nil
    "Non-nil in log buffers opened with `branch-off/magit-log'.")

  (defvar branch-off/magit-log-flat--pending nil
    "Dynamic flag set during `branch-off/magit-log' so the hook fires correctly.")

  (defun branch-off/magit-log ()
    "Log all refs without --graph; commits not ancestral to HEAD are marked with 2-space indent."
    (interactive)
    (let ((branch-off/magit-log-flat--pending t))
      (magit-log-setup-buffer (list "--all") (list "--color" "--decorate" "--topo-order" "-n256") nil)))

  (defun branch-off/magit-log--mark ()
    "Overlay depth-based indent on branch-off commits in flat log buffers.
Commits reachable from any ref but not from refs/heads/* are candidates;
this includes branch-off refs and detached worktree HEADs.  Depth is
computed from ancestry: depth 1 = directly off a branch head, crossing
a commit with a refs/branch-off/* ref adds one level (2 spaces).
Uses magit's section API for hash extraction."
    (when (derived-mode-p 'magit-log-mode)
      (when branch-off/magit-log-flat--pending
        (setq-local branch-off/magit-log-flat t))
      (remove-overlays (point-min) (point-max) 'branch-off/log-marker t)
      (remove-overlays (point-min) (point-max) 'branch-off/archive-marker t))
    (when (and (derived-mode-p 'magit-log-mode)
               (bound-and-true-p branch-off/magit-log-flat))
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
             (depth-cache (make-hash-table :test #'equal)))
        (dolist (line raw)
          (when (string-match
                 "^\\([0-9a-f]\\{40\\}\\)\\(?: \\([0-9a-f]\\{40\\}\\)\\)?" line)
            (let ((h (match-string 1 line))
                  (p (match-string 2 line)))
              (puthash h p parent-map)
              (push h all-hashes))))
        (when all-hashes
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
                (when-let* ((h    (magit-section-value-if 'commit))
                            (full (cl-find-if (lambda (f) (string-prefix-p h f))
                                              all-hashes)))
                  (let* ((d  (depth-of full))
                         (ov (make-overlay (line-beginning-position)
                                           (line-beginning-position))))
                    (overlay-put ov 'before-string (make-string (* d 2) ?\s))
                    (overlay-put ov 'branch-off/log-marker t)))
                (forward-line 1))))))))

  (add-hook 'magit-refresh-buffer-hook #'branch-off/magit-log--mark)

  (defun branch-off/magit-status-tab ()
    "On a commit section show it; otherwise toggle the section."
    (interactive)
    (if-let ((commit (magit-section-value-if 'commit)))
        (magit-show-commit commit)
      (call-interactively #'magit-section-toggle)))

  (defun branch-off/magit-status-navigate (move-fn)
    (condition-case nil
        (progn
          (funcall move-fn)
          (when-let ((commit (magit-section-value-if 'commit)))
            (let ((magit-display-buffer-noselect t))
              (magit-show-commit commit))))
      (error nil)))

  (defun branch-off/magit-status-next ()
    (interactive)
    (branch-off/magit-status-navigate #'magit-section-forward))

  (defun branch-off/magit-status-prev ()
    (interactive)
    (branch-off/magit-status-navigate #'magit-section-backward))

  (map! :map magit-log-mode-map
        :n "TAB" #'magit-visit-thing)

  (map! :map magit-status-mode-map
        :n "TAB" #'branch-off/magit-status-tab
        :m "n"   #'branch-off/magit-status-next
        :m "p"   #'branch-off/magit-status-prev)

  (map! :map magit-revision-mode-map
        :n "TAB" #'magit-diff-visit-file
        :n "n" #'branch-off/magit-revision-next
        :n "p" #'branch-off/magit-revision-prev))

;;; Create worktree

(after! magit
  (defvar-local branch-off/create-worktree--pick-source nil
    "Cons of (top . rel-path) for a pending worktree commit-pick in this log buffer.")

  (define-minor-mode branch-off/create-worktree--pick-mode
    "Transient: RET picks a commit at point to create a detached worktree."
    :lighter nil
    :keymap (let ((m (make-sparse-keymap)))
              (define-key m (kbd "RET") #'branch-off/create-worktree--pick-commit)
              (define-key m (kbd "C-g") #'branch-off/create-worktree--pick-abort)
              m)
    (when (bound-and-true-p evil-local-mode)
      (evil-normalize-keymaps)))

  (after! evil
    (evil-make-overriding-map branch-off/create-worktree--pick-mode-map))

  (defun branch-off/create-worktree--pick-abort ()
    "Abort the pending worktree commit-pick."
    (interactive)
    (setq-local branch-off/create-worktree--pick-source nil)
    (branch-off/create-worktree--pick-mode -1)
    (setq-local header-line-format nil)
    (message "create-worktree: aborted"))

  (defun branch-off/create-worktree--pick-commit ()
    "Pick the commit at point, create a detached worktree, and open the source file there."
    (interactive)
    (let ((commit (or (magit-section-value-if 'commit)
                      (user-error "No commit at point"))))
      (let ((source branch-off/create-worktree--pick-source))
        (setq-local branch-off/create-worktree--pick-source nil)
        (branch-off/create-worktree--pick-mode -1)
        (setq-local header-line-format nil)
        (branch-off/create-worktree--do commit (when source (cdr source))))))

  (defun branch-off/create-worktree--do (commit &optional rel-file)
    "Create a detached worktree for COMMIT at .worktree/<full-hash> under the repo root.
Opens REL-FILE (path relative to repo root) in the new worktree when given; otherwise
opens dired at the worktree root.  Silently reuses an existing worktree directory."
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

  (defun branch-off/create-worktree ()
    "Create a detached worktree at .worktree/<commit-hash> for a selected commit.

Context-sensitive behaviour:
- magit-log: uses the commit at point, then opens dired at the worktree root.
- magit-revision: uses the buffer's revision, then opens dired at the worktree root.
- magit-blob: uses the buffer's revision and opens the same file in the worktree.
- File buffer: opens `branch-off/magit-log' and enters pick mode — press RET on
  any commit to create the worktree and open that file at the same relative path."
    (interactive)
    (cond
     ((derived-mode-p 'magit-log-mode)
      (let ((commit (or (magit-section-value-if 'commit)
                        (user-error "No commit at point"))))
        (branch-off/create-worktree--do commit)))
     ((derived-mode-p 'magit-revision-mode)
      (let ((commit (or (and (bound-and-true-p magit-buffer-revision)
                             magit-buffer-revision)
                        (user-error "No revision in current buffer"))))
        (branch-off/create-worktree--do commit)))
     ((bound-and-true-p magit-blob-mode)
      (let* ((commit (or (and (bound-and-true-p magit-buffer-revision)
                              magit-buffer-revision)
                         (user-error "No revision in current buffer")))
             (top    (or (magit-toplevel) (user-error "Not in a git repository")))
             (rel    (file-relative-name (magit-buffer-file-name) top)))
        (branch-off/create-worktree--do commit rel)))
     (buffer-file-name
      (let* ((top (or (magit-toplevel) (user-error "Not in a git repository")))
             (rel (file-relative-name buffer-file-name top)))
        (branch-off/magit-log)
        (let ((log-buf (or (magit-get-mode-buffer 'magit-log-mode)
                           (user-error "Could not find magit-log buffer"))))
          (with-current-buffer log-buf
            (setq-local branch-off/create-worktree--pick-source (cons top rel))
            (branch-off/create-worktree--pick-mode 1)
            (setq-local header-line-format
                        (list (format " Worktree ← %s — " rel)
                              (propertize "RET" 'face 'transient-key)
                              " create  "
                              (propertize "C-g" 'face 'transient-key)
                              " abort"))))))
     (t
      (user-error "Invoke from magit-log, magit-revision, magit-blob, or a file buffer"))))

)

;;; Commit reword

(defvar-local branch-off/magit-reword--commit nil)
(defvar-local branch-off/magit-reword--source-buffer nil)
(defvar-local branch-off/magit-reword--source-line nil)
(defvar-local branch-off/magit-reword--from-revision nil)

(defun branch-off/magit-reword--fix-highlight ()
  "Reposition highlights to the current line after a programmatic cursor move."
  (mapc #'delete-overlay magit-section-highlight-overlays)
  (setq magit-section-highlight-overlays nil)
  (hl-line-highlight))

(defun branch-off/magit-reword--refresh-log (line)
  "Refresh the magit-log buffer and restore point to LINE."
  (when-let ((log-buf (magit-get-mode-buffer 'magit-log-mode)))
    (with-current-buffer log-buf (magit-refresh))
    (when-let ((win (get-buffer-window log-buf)))
      (with-selected-window win
        (goto-char (point-min))
        (forward-line (1- line))
        (branch-off/magit-reword--fix-highlight))
      (run-with-timer 0 nil
        (lambda ()
          (when-let ((w (get-buffer-window log-buf)))
            (with-selected-window w
              (forward-line 1)
              (forward-line -1))))))))

(defun branch-off/magit-reword--parse-commit (hash)
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

(defun branch-off/magit-reword--new-commit (info new-parent msg)
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

(defun branch-off/magit-reword--cascade-branch-off (remap)
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
             (bo-info   (branch-off/magit-reword--parse-commit bo-hash))
             (bo-parent (plist-get bo-info :parent))
             (new-par   (cdr (assoc bo-parent remap))))
        (when new-par
          (let ((new-bo (branch-off/magit-reword--new-commit
                         bo-info new-par
                         (with-temp-buffer
                           (call-process "git" nil t nil "log" "-1" "--format=%B" bo-hash)
                           (buffer-string)))))
            (magit-call-git "update-ref" (format "refs/branch-off/%s" new-bo) new-bo)
            (magit-call-git "update-ref" "-d" ref)
            (push (cons bo-hash new-bo) remap)
            (setq changed t)))))
    (if changed
        (branch-off/magit-reword--cascade-branch-off remap)
      remap)))

(defun branch-off/magit-reword--apply (hash new-msg)
  "Reword HASH with NEW-MSG using git plumbing.
For branch-off refs, rewrites the commit and cascades through any chained
branch-off descendants.  For current-branch commits, rebases all descendants,
updates the branch ref, then cascades through all affected branch-off refs."
  (let* ((full-hash      (magit-git-string "rev-parse" hash))
         (branch-off-ref (format "refs/branch-off/%s" full-hash))
         (is-branch-off  (equal full-hash
                                (magit-git-string "rev-parse" "--verify" branch-off-ref))))
    (if is-branch-off
        (let* ((info     (branch-off/magit-reword--parse-commit full-hash))
               (new-hash (branch-off/magit-reword--new-commit info nil new-msg)))
          (unless new-hash (user-error "git commit-tree failed"))
          (magit-call-git "update-ref" (format "refs/branch-off/%s" new-hash) new-hash)
          (magit-call-git "update-ref" "-d" branch-off-ref)
          (branch-off/magit-reword--cascade-branch-off (list (cons full-hash new-hash))))
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
               (target-info (branch-off/magit-reword--parse-commit full-hash))
               (new-target  (branch-off/magit-reword--new-commit target-info nil new-msg)))
          (unless new-target (user-error "git commit-tree failed"))
          (push (cons full-hash new-target) remap)
          (dolist (old-hash (cdr chain))
            (let* ((info       (branch-off/magit-reword--parse-commit old-hash))
                   (old-parent (plist-get info :parent))
                   (new-parent (or (cdr (assoc old-parent remap)) old-parent))
                   (msg        (with-temp-buffer
                                 (call-process "git" nil t nil "log" "-1" "--format=%B" old-hash)
                                 (buffer-string)))
                   (new-hash   (branch-off/magit-reword--new-commit info new-parent msg)))
              (push (cons old-hash new-hash) remap)))
          (magit-call-git "update-ref"
                          (format "refs/heads/%s" branch)
                          (cdr (assoc (car (last chain)) remap)))
          (branch-off/magit-reword--cascade-branch-off remap))))))

(defun branch-off/magit-reword--finish ()
  "Reword the target commit using git plumbing."
  (interactive)
  (let ((msg           (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (hash          branch-off/magit-reword--commit)
        (dir           default-directory)
        (source        branch-off/magit-reword--source-buffer)
        (line          branch-off/magit-reword--source-line)
        (from-revision branch-off/magit-reword--from-revision))
    (kill-buffer-and-window)
    (let ((default-directory dir))
      (branch-off/magit-reword--apply hash msg)
      (when (and from-revision (buffer-live-p source))
        (kill-buffer source))
      (branch-off/magit-reword--refresh-log line))))

(defun branch-off/magit-reword--abort ()
  "Abort reword without applying changes."
  (interactive)
  (kill-buffer-and-window)
  (message "reword: aborted"))

(defun branch-off/magit-commit-reword (commit)
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
      (setq-local branch-off/magit-reword--commit commit)
      (setq-local branch-off/magit-reword--source-buffer source-buf)
      (setq-local branch-off/magit-reword--source-line source-line)
      (setq-local branch-off/magit-reword--from-revision from-revision)
      (local-set-key (kbd "C-c C-c") #'branch-off/magit-reword--finish)
      (local-set-key (kbd "C-c C-k") #'branch-off/magit-reword--abort)
      (setq-local header-line-format
                  (list " "
                        (propertize "C-c C-c" 'face 'transient-key)
                        " apply  "
                        (propertize "C-c C-k" 'face 'transient-key)
                        " abort"))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun branch-off/magit-commit-remove (commit)
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

(defvar-local branch-off/magit-squash--chain nil)
(defvar-local branch-off/magit-squash--parent nil)
(defvar-local branch-off/magit-squash--tree nil)
(defvar-local branch-off/magit-squash--bo-p nil)
(defvar-local branch-off/magit-squash--branch nil)
(defvar-local branch-off/magit-squash--source-line nil)
(defvar-local branch-off/magit-squash--dir nil)
(defvar-local branch-off/magit-squash--log-buf nil)

(defvar-local branch-off/magit-squash--marks nil
  "Ordered list of commit hashes marked for squashing in this log buffer.")

(defvar-local branch-off/magit-squash--overlays nil
  "Alist of (full-hash . overlay) for marked commits in this log buffer.")

(defface branch-off/magit-squash-marked
  '((t :extend t))
  "Face applied to commit lines marked for squashing.")

(after! doom-themes
  (custom-set-faces!
    `(branch-off/magit-squash-marked
      :background ,(doom-blend (doom-color 'orange) (doom-color 'bg) 0.25)
      :extend t)))

(defun branch-off/magit-squash--clear-overlays ()
  "Delete all squash-mark overlays in the current buffer."
  (dolist (pair branch-off/magit-squash--overlays)
    (delete-overlay (cdr pair)))
  (setq branch-off/magit-squash--overlays nil))

(defun branch-off/magit-squash--mark-one (full bol)
  "Add FULL hash to marks with an overlay starting at BOL."
  (setq branch-off/magit-squash--marks (append branch-off/magit-squash--marks (list full)))
  (let* ((eol (save-excursion (goto-char bol) (end-of-line) (point)))
         (ov  (make-overlay bol (1+ eol) nil t nil)))
    (overlay-put ov 'face 'branch-off/magit-squash-marked)
    (push (cons full ov) branch-off/magit-squash--overlays)))

(defun branch-off/magit-squash--unmark-one (full)
  "Remove FULL hash from marks and delete its overlay."
  (setq branch-off/magit-squash--marks (delete full branch-off/magit-squash--marks))
  (when-let ((ov (cdr (assoc full branch-off/magit-squash--overlays))))
    (delete-overlay ov))
  (setq branch-off/magit-squash--overlays
        (cl-remove full branch-off/magit-squash--overlays :key #'car :test #'equal)))

(defun branch-off/magit-mark ()
  "Toggle squash mark on the commit at point, or on all commits in the visual selection.
With an active region (evil V), marks every commit in the selection — or
unmarks them all when every one is already marked.
Lines are highlighted with `branch-off/magit-squash-marked'.  Marks are used by
`branch-off/magit-squash' in preference to a live visual selection."
  (interactive)
  (if (not (use-region-p))
      ;; ── single commit at point ──────────────────────────────────────────────
      (let ((hash (magit-section-value-if 'commit)))
        (unless hash (user-error "No commit at point"))
        (let ((full (magit-git-string "rev-parse" hash)))
          (if (member full branch-off/magit-squash--marks)
              (progn
                (branch-off/magit-squash--unmark-one full)
                (message "Unmarked %s (%d remaining)"
                         (substring full 0 8) (length branch-off/magit-squash--marks)))
            (branch-off/magit-squash--mark-one full (line-beginning-position))
            (message "Marked %s (%d total)"
                     (substring full 0 8) (length branch-off/magit-squash--marks)))))
    ;; ── visual selection: mark/unmark all commits in region ─────────────────
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
      (if (cl-every (lambda (e) (member (car e) branch-off/magit-squash--marks)) entries)
          (progn
            (dolist (e entries) (branch-off/magit-squash--unmark-one (car e)))
            (message "Unmarked %d commit%s (%d remaining)"
                     (length entries) (if (= (length entries) 1) "" "s")
                     (length branch-off/magit-squash--marks)))
        (let ((newly 0))
          (dolist (e entries)
            (unless (member (car e) branch-off/magit-squash--marks)
              (branch-off/magit-squash--mark-one (car e) (cdr e))
              (cl-incf newly)))
          (message "Marked %d commit%s (%d total)"
                   newly (if (= newly 1) "" "s")
                   (length branch-off/magit-squash--marks)))))))

(defun branch-off/magit-squash--commits-in-region ()
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

(defun branch-off/magit-squash--build-chain (full-hashes)
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

(defun branch-off/magit-squash--try-chain (full-hashes)
  "Try `branch-off/magit-squash--build-chain'; return nil instead of signaling on chain failure."
  (condition-case nil
      (branch-off/magit-squash--build-chain full-hashes)
    (user-error nil)))

(defun branch-off/magit-squash--sort-siblings (full-hashes)
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

(defun branch-off/magit-squash--commit-tree (hash)
  "Return the tree SHA of commit HASH."
  (with-temp-buffer
    (call-process "git" nil t nil "rev-parse" (format "%s^{tree}" hash))
    (string-trim (buffer-string))))

;;; Conflict resolution via smerge (ediff available via C-c ^ e inside smerge)

(defvar-local branch-off/magit-squash--conflict-done-fn nil
  "Continuation called with the resolved blob SHA from the conflict buffer.")

(defvar branch-off/magit-squash--verbose nil
  "When non-nil, append the squash diff as comments to the message buffer.")

(defun branch-off/magit-squash--finish-conflict ()
  "Confirm conflict resolution and resume the in-progress squash."
  (interactive)
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
    (user-error "Unresolved conflicts remain — use smerge (C-c ^ n/p) or ediff (C-c ^ e)"))
  (funcall branch-off/magit-squash--conflict-done-fn))

(defun branch-off/magit-squash--abort-conflict ()
  "Abort the squash from the conflict resolution buffer."
  (interactive)
  (kill-buffer-and-window)
  (message "Squash aborted"))

(defun branch-off/magit-squash--open-conflict-buffer (path commit-hash conflicted-content mode done-fn)
  "Open a smerge buffer for CONFLICTED-CONTENT of PATH.
DONE-FN is called with the resolved blob SHA when the user confirms with C-c C-c."
  (let ((buf (get-buffer-create (format "*squash conflict: %s*" path))))
    (with-current-buffer buf
      (erase-buffer)
      (insert conflicted-content)
      (smerge-mode 1)
      (setq-local branch-off/magit-squash--conflict-done-fn
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
      (local-set-key (kbd "C-c C-c") #'branch-off/magit-squash--finish-conflict)
      (local-set-key (kbd "C-c C-k") #'branch-off/magit-squash--abort-conflict)
      (setq-local header-line-format
                  (list (format " Conflict in %s (from %s) — " path (substring commit-hash 0 8))
                        (propertize "C-c C-c" 'face 'transient-key) " done  "
                        (propertize "C-c C-k" 'face 'transient-key) " abort  "
                        (propertize "C-c ^ e" 'face 'transient-key) " ediff"))
      (goto-char (point-min))
      (ignore-errors (smerge-next)))
    (pop-to-buffer buf)))

(defun branch-off/magit-squash--resolve-path-list (paths by-path commit-hash penv all-resolved-fn)
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
          (branch-off/magit-squash--open-conflict-buffer
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
             (branch-off/magit-squash--resolve-path-list rest by-path commit-hash penv
                                                  all-resolved-fn))))))))

(defun branch-off/magit-squash--merge-commits (remaining parent-tree current-tree temp-index penv done-fn)
  "Iteratively 3-way-merge REMAINING sibling commits into CURRENT-TREE.
PARENT-TREE is the constant base (shared ancestor of all siblings).
Calls DONE-FN with the final tree hash; may suspend for interactive conflict resolution."
  (if (null remaining)
      (funcall done-fn current-tree)
    (let* ((process-environment penv)
           (h        (car remaining))
           (rest     (cdr remaining))
           (bon-tree (branch-off/magit-squash--commit-tree h)))
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
              (branch-off/magit-squash--merge-commits rest parent-tree new-tree temp-index penv done-fn))
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
              (branch-off/magit-squash--resolve-path-list
               paths by-path h penv
               (lambda ()
                 (let ((new-tree (let ((process-environment penv))
                                   (with-temp-buffer
                                     (unless (= 0 (call-process "git" nil t nil "write-tree"))
                                       (user-error "git write-tree failed after resolving %s"
                                                   (substring h 0 8)))
                                     (string-trim (buffer-string))))))
                   (branch-off/magit-squash--merge-commits
                    rest parent-tree new-tree temp-index penv done-fn)))))))))))

(defun branch-off/magit-squash--combined-tree (parent-hash sorted-commits done-fn)
  "Async: build a merged tree from sibling SORTED-COMMITS branched from PARENT-HASH.
Calls DONE-FN with the merged tree hash; may open smerge buffers for conflicts."
  (let* ((parent-tree (branch-off/magit-squash--commit-tree parent-hash))
         (temp-index  (make-temp-file "git-squash" nil))
         (penv (append (list (format "GIT_INDEX_FILE=%s" temp-index)) process-environment)))
    (let ((process-environment penv))
      (with-temp-buffer
        (unless (= 0 (call-process "git" nil t nil "read-tree" parent-tree))
          (ignore-errors (delete-file temp-index))
          (user-error "git read-tree failed: %s" (buffer-string)))))
    (branch-off/magit-squash--merge-commits
     sorted-commits parent-tree parent-tree temp-index penv
     (lambda (tree-hash)
       (ignore-errors (delete-file temp-index))
       (funcall done-fn tree-hash)))))

(defun branch-off/magit-squash--make-info (tree-hash first-info)
  "Build a commit info plist: TREE-HASH as tree, author/committer identity from FIRST-INFO."
  (list :tree            tree-hash
        :author-name     (plist-get first-info :author-name)
        :author-email    (plist-get first-info :author-email)
        :author-date     (plist-get first-info :author-date)
        :committer-name  (plist-get first-info :committer-name)
        :committer-email (plist-get first-info :committer-email)
        :committer-date  (plist-get first-info :committer-date)))

(defun branch-off/magit-squash--apply-branch-off (sorted-chain parent-of-first tree-hash new-msg)
  "Squash SORTED-CHAIN branch-off commits into one new branch-off commit."
  (let* ((first-info (branch-off/magit-reword--parse-commit (car sorted-chain)))
         (info       (branch-off/magit-squash--make-info tree-hash first-info))
         (new-hash   (branch-off/magit-reword--new-commit info parent-of-first new-msg)))
    (unless new-hash (user-error "git commit-tree failed"))
    (magit-call-git "update-ref" (format "refs/branch-off/%s" new-hash) new-hash)
    (dolist (h sorted-chain)
      (magit-call-git "update-ref" "-d" (format "refs/branch-off/%s" h)))
    (branch-off/magit-reword--cascade-branch-off
     (mapcar (lambda (h) (cons h new-hash)) sorted-chain))
    (magit-refresh)
    (message "Squashed %d branch-off commits → %s"
             (length sorted-chain) (substring new-hash 0 8))))

(defun branch-off/magit-squash--apply-branch (sorted-chain parent-of-first tree-hash new-msg branch)
  "Squash SORTED-CHAIN branch commits into one, rebasing HEAD descendants."
  (let* ((first-info (branch-off/magit-reword--parse-commit (car sorted-chain)))
         (info       (branch-off/magit-squash--make-info tree-hash first-info))
         (new-squash (branch-off/magit-reword--new-commit info parent-of-first new-msg)))
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
        (let* ((d-info   (branch-off/magit-reword--parse-commit old-hash))
               (old-par  (plist-get d-info :parent))
               (new-par  (or (cdr (assoc old-par remap)) old-par))
               (msg      (with-temp-buffer
                           (call-process "git" nil t nil "log" "-1" "--format=%B" old-hash)
                           (buffer-string)))
               (new-hash (branch-off/magit-reword--new-commit d-info new-par msg)))
          (push (cons old-hash new-hash) remap)))
      (let* ((head-hash (magit-git-string "rev-parse" "HEAD"))
             (new-head  (or (cdr (assoc head-hash remap)) new-squash)))
        (magit-call-git "update-ref" (format "refs/heads/%s" branch) new-head))
      (branch-off/magit-reword--cascade-branch-off remap)
      (magit-refresh)
      (message "Squashed %d commits → %s"
               (length sorted-chain) (substring new-squash 0 8)))))

(defun branch-off/magit-squash--finish ()
  "Apply the squash using the message in the current buffer."
  (interactive)
  (let* ((msg    (string-trim
                  (mapconcat #'identity
                             (seq-remove (lambda (l) (string-prefix-p "#" l))
                                         (split-string
                                          (buffer-substring-no-properties (point-min) (point-max))
                                          "\n"))
                             "\n")))
         (chain  branch-off/magit-squash--chain)
         (parent branch-off/magit-squash--parent)
         (tree   branch-off/magit-squash--tree)
         (bo-p   branch-off/magit-squash--bo-p)
         (branch branch-off/magit-squash--branch)
         (line   branch-off/magit-squash--source-line)
         (dir    branch-off/magit-squash--dir))
    (when (string-empty-p msg)
      (user-error "Commit message cannot be empty"))
    (let ((log-buf branch-off/magit-squash--log-buf))
      (kill-buffer-and-window)
      (when (buffer-live-p log-buf)
        (with-current-buffer log-buf
          (setq branch-off/magit-squash--marks nil)
          (branch-off/magit-squash--clear-overlays)))
      (let ((default-directory dir))
        (if bo-p
            (branch-off/magit-squash--apply-branch-off chain parent tree msg)
          (branch-off/magit-squash--apply-branch chain parent tree msg branch))
        (branch-off/magit-reword--refresh-log line)))))

(defun branch-off/magit-squash--abort ()
  "Abort the squash."
  (interactive)
  (kill-buffer-and-window)
  (message "squash: aborted"))

(defun branch-off/magit-squash ()
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
      (let* ((raw  (if branch-off/magit-squash--marks
                       ;; Marks take priority; leave overlays visible until
                       ;; finish so abort restores the user's selection.
                       branch-off/magit-squash--marks
                     (unless (use-region-p)
                       (user-error
                        "No commits selected — mark commits with %s or visually select with V"
                        (substitute-command-keys "\\[branch-off/magit-mark]")))
                     (branch-off/magit-squash--commits-in-region))))
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
               (chain-try (and bo-p (branch-off/magit-squash--try-chain full)))
               (sort-result
                (cond ((not bo-p) (branch-off/magit-squash--build-chain full))
                      (chain-try  chain-try)
                      (t          (branch-off/magit-squash--sort-siblings full))))
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
                       (when branch-off/magit-squash--verbose
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
                       (setq-local branch-off/magit-squash--chain chain)
                       (setq-local branch-off/magit-squash--parent par)
                       (setq-local branch-off/magit-squash--tree tree-hash)
                       (setq-local branch-off/magit-squash--bo-p bo-p)
                       (setq-local branch-off/magit-squash--branch branch)
                       (setq-local branch-off/magit-squash--source-line source-line)
                       (setq-local branch-off/magit-squash--dir dir)
                       (setq-local branch-off/magit-squash--log-buf log-buf)
                       (local-set-key (kbd "C-c C-c") #'branch-off/magit-squash--finish)
                       (local-set-key (kbd "C-c C-k") #'branch-off/magit-squash--abort)
                       (setq-local header-line-format
                                   (list " "
                                         (propertize "C-c C-c" 'face 'transient-key)
                                         " squash  "
                                         (propertize "C-c C-k" 'face 'transient-key)
                                         " abort"))
                       (goto-char (point-min)))
                     (pop-to-buffer buf)))))
            (if (and bo-p (null chain-try))
                (branch-off/magit-squash--combined-tree par chain open-buf-fn)
              (funcall open-buf-fn
                       (plist-get (branch-off/magit-reword--parse-commit (car (last chain)))
                                  :tree)))))))))

(after! magit
  ;; Define here so transient is guaranteed loaded (magit requires it)
  (transient-define-suffix branch-off/magit-squash-verbose ()
    "Toggle whether the branch-off edit buffer shows the diff as comments."
    :transient t
    :description (lambda ()
                   (concat "show diff in edit buffer "
                           (if branch-off/magit-squash--verbose
                               (propertize "(on) " 'face 'success)
                             (propertize "(off)" 'face 'shadow))))
    (interactive)
    (setq branch-off/magit-squash--verbose (not branch-off/magit-squash--verbose)))
  ;; Rebase transient — Branch-off section
  (ignore-errors (transient-remove-suffix 'magit-rebase "W"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "K"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "S"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "v"))
  (transient-append-suffix 'magit-rebase '(2)
    ["Branch-off"
     [("W" "reword" branch-off/magit-commit-reword)
      ("K" "remove" branch-off/magit-commit-remove)
      ("S" "squash" branch-off/magit-squash)]
     [("v" branch-off/magit-squash-verbose)]])
  ;; Merge transient — Branch Off section (appended after group 1 = "Actions")
  (ignore-errors (transient-remove-suffix 'magit-merge "M"))
  (transient-append-suffix 'magit-merge '(1)
    ["Branch Off"
     ("M" "toggle marker" branch-off/magit-mark)]))

;;; Commit-and-branch-off

(defun branch-off/magit--only-additions-in-selection-p (diff sel-start sel-end)
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

(defun branch-off/magit-stage-and-commit-and-branch-off ()
  "Stage the selected newly-added lines, commit, preserve under refs/branch-off/, then rewind.

Requires an active region containing only pure additions (lines added since
the last commit, not modifications or deletions).  Aborts with an explanation
if the selection contains anything else.

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
         (range     (branch-off/magit--selection-lines)))
    (when (buffer-modified-p) (save-buffer))
    (let* ((sel-start (car range))
           (sel-end   (cdr range))
           (diff      (with-temp-buffer
                        (call-process "git" nil t nil "diff" "-U0" "--" rel)
                        (buffer-string)))
           (err-msg   (branch-off/magit--only-additions-in-selection-p diff sel-start sel-end))
           (result    (unless err-msg
                        (branch-off/magit--patch-from-diff diff rel sel-start sel-end))))
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

;;; magit-blob navigation with branch-off awareness

(after! magit
  (defun branch-off/magit-blob--current-hash ()
    "Return the full 40-char hash if the current blob buffer is on a branch-off commit, else nil."
    (when (and (bound-and-true-p magit-blob-mode)
               (bound-and-true-p magit-buffer-revision))
      (let ((full (magit-git-string "rev-parse" "--verify" magit-buffer-revision)))
        (when (and full
                   (equal full (magit-git-string "rev-parse" "--verify"
                                                  (format "refs/branch-off/%s" full))))
          full))))

  (defun branch-off/magit-blob--parent-of (hash)
    "Return the full hash of HASH's parent commit, or nil for a root commit."
    (let ((p (magit-git-string "rev-parse" "--verify" (concat hash "^"))))
      (when (and p (not (string-empty-p p))) p)))

  (defun branch-off/magit-blob--branch-offs-by-date ()
    "Return all refs/branch-off/* hashes sorted by committer date ascending (oldest first)."
    (sort (magit-git-lines "for-each-ref" "--format=%(objectname)" "refs/branch-off/")
          (lambda (a b)
            (< (string-to-number
                (or (magit-git-string "log" "-1" "--format=%ct" a) "0"))
               (string-to-number
                (or (magit-git-string "log" "-1" "--format=%ct" b) "0"))))))

  (defun branch-off/magit-blob--touches-file-p (hash file-rel)
    "Return non-nil when HASH added or modified FILE-REL (path relative to repo root)."
    (magit-git-lines "log" "-1" "--format=%H" "--diff-filter=AM" hash "--" file-rel))

  (defun branch-off/magit-blob-next ()
    "Go to the next (more recent) blob revision, respecting the git DAG.
For branch-off commits: navigates to chain children first (branch-offs whose
parent is the current commit), then to same-parent siblings with a newer
committer date.  Falls through to `magit-blob-next' for regular commits."
    (interactive)
    (if-let ((full (branch-off/magit-blob--current-hash)))
        (let* ((file-abs      (magit-buffer-file-name))
               (file-rel      (file-relative-name file-abs (magit-toplevel)))
               (parent        (branch-off/magit-blob--parent-of full))
               (all-bo        (branch-off/magit-blob--branch-offs-by-date))   ; oldest -> newest
               (idx           (cl-position full all-bo :test #'equal))
               ;; chain children: branch-offs whose immediate parent is the current commit
               (chain-kids    (cl-remove-if-not
                               (lambda (h) (equal full (branch-off/magit-blob--parent-of h)))
                               all-bo))
               ;; newer siblings: same parent, appear after current in date-sorted order
               (newer-siblings (when idx
                                 (cl-remove-if-not
                                  (lambda (h) (equal parent (branch-off/magit-blob--parent-of h)))
                                  (nthcdr (1+ idx) all-bo))))
               (succ (or (cl-find-if (lambda (h) (branch-off/magit-blob--touches-file-p h file-rel))
                                     chain-kids)
                         (cl-find-if (lambda (h) (branch-off/magit-blob--touches-file-p h file-rel))
                                     newer-siblings))))
          (if succ
              (magit-blob-visit succ file-rel)
            (user-error "No next blob")))
      (call-interactively #'magit-blob-next)))

  (defun branch-off/magit-blob-prev ()
    "Go to the previous (older) blob revision, respecting the git DAG.
For branch-off commits: navigates to same-parent siblings with an older
committer date.  When no older same-parent sibling exists, falls through to
`magit-blob-previous' — which correctly follows git ancestry (including back
to a branch-off chain parent when the parent commit is itself branch-off).
Also falls through for regular (non-branch-off) commits."
    (interactive)
    (if-let ((full (branch-off/magit-blob--current-hash)))
        (let* ((file-abs       (magit-buffer-file-name))
               (file-rel       (file-relative-name file-abs (magit-toplevel)))
               (parent         (branch-off/magit-blob--parent-of full))
               (all-bo         (branch-off/magit-blob--branch-offs-by-date))  ; oldest -> newest
               (idx            (cl-position full all-bo :test #'equal))
               ;; older siblings: same parent, appear before current in date-sorted order
               (older-siblings (when idx
                                 (cl-remove-if-not
                                  (lambda (h) (equal parent (branch-off/magit-blob--parent-of h)))
                                  (reverse (seq-take all-bo idx)))))
               (pred           (cl-find-if
                                (lambda (h) (branch-off/magit-blob--touches-file-p h file-rel))
                                older-siblings)))
          (if pred
              (magit-blob-visit pred file-rel)
            ;; no older same-parent sibling: fall through to magit-blob-previous,
            ;; which follows git ancestry (handles chains and goes to parent commit)
            (call-interactively #'magit-blob-previous)))
      (call-interactively #'magit-blob-previous)))

  (map! :map magit-blob-mode-map
        :n "n" #'branch-off/magit-blob-next
        :n "p" #'branch-off/magit-blob-prev))

;;; Pickaxe — SPC s g
;; git grep across ALL committed blobs; consult preview via `git show sha:file';
;; selection opens the exact blob in magit-find-file (magit-blob-mode → n/p navigates history).

(defun my/magit-pickaxe--commit-cache ()
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

(defun my/magit-pickaxe--parse-line (line cache)
  "Parse one `git grep -n' history line into a propertized candidate, or nil.
Expected format from searching explicit commits: <40-sha>:<file>:<lineno>:<content>"
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
      (put-text-property 0 1 'my/hash  hash   cand)
      (put-text-property 0 1 'my/file  file   cand)
      (put-text-property 0 1 'my/line  lineno cand)
      ;; Group header shown in vertico: "abc12345  2026-05-26  author"
      (put-text-property 0 1 'consult--prefix-group
                         (concat short "  " info) cand)
      cand)))

(defun my/magit-pickaxe--format-lines (lines cache)
  "Filter and format a batch of git grep output LINES into candidates using CACHE."
  (delq nil (mapcar (lambda (l) (my/magit-pickaxe--parse-line l cache)) lines)))

(defun my/magit-pickaxe--git-builder (input)
  "Return (cmd . nil) running git grep across every committed blob for INPUT."
  (pcase-let ((`(,arg . ,_) (consult--command-split input)))
    (unless (string-blank-p arg)
      (cons (list "sh" "-c"
                  (format
                   "git --no-pager grep -In -e %s $(git rev-list --all 2>/dev/null) 2>/dev/null"
                   (shell-quote-argument arg)))
            nil))))

(defun my/magit-pickaxe--state ()
  "State: preview git blobs in-place via git-show; open as magit-find-file on return.
Consult calls us inside `with-selected-window' on the original (non-minibuffer) window,
so `selected-window' is the right target for showing the preview."
  (let ((pbuf (get-buffer-create " *pickaxe-preview*"))
        restore-fn
        line-ov)
    (lambda (action cand)
      (pcase action
        ('preview
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when cand
           (let* ((hash (get-text-property 0 'my/hash cand))
                  (file (get-text-property 0 'my/file cand))
                  (line (get-text-property 0 'my/line cand))
                  (win  (selected-window)))
             (when (and hash file line)
               (with-current-buffer pbuf
                 (let ((inhibit-read-only t))
                   (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
                   (erase-buffer)
                   (when (= 0 (call-process "git" nil t nil "show"
                                            (format "%s:%s" hash file)))
                     ;; Auto-detect mode from filename. delay-mode-hooks queues hooks;
                     ;; clearing delayed-mode-hooks after discards them so they never
                     ;; fire in a wrong context (e.g. vertico's minibuffer rendering).
                     (delay-mode-hooks
                       (let ((buffer-file-name file))
                         (set-auto-mode)))
                     (setq delayed-mode-hooks nil)
                     (font-lock-ensure)
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (setq line-ov
                           (make-overlay (line-beginning-position)
                                         (min (1+ (line-end-position)) (point-max))))
                     (overlay-put line-ov 'face 'consult-preview-line)
                     (overlay-put line-ov 'priority 2)
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
         ;; 'exit fires before 'return and already cleans up; guard defensively.
         (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when (buffer-live-p pbuf) (kill-buffer pbuf))
         (when cand
           (let* ((hash (get-text-property 0 'my/hash cand))
                  (file (get-text-property 0 'my/file cand))
                  (line (get-text-property 0 'my/line cand)))
             (when (and hash file line)
               (magit-find-file hash file)
               (goto-char (point-min))
               (forward-line (1- line))
               (recenter)))))
        ('exit
         (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when (buffer-live-p pbuf) (kill-buffer pbuf)))))))

(defun my/magit-pickaxe-ripgrep ()
  "Search ALL committed git blobs for a pattern; consult preview; open as magit-find-file."
  (interactive)
  (let* ((top   (or (magit-toplevel) (user-error "Not in a git repository")))
         (default-directory top)
         (cache (my/magit-pickaxe--commit-cache)))
    (consult--read
     (consult--process-collection #'my/magit-pickaxe--git-builder
       :transform (consult--async-transform
                   (lambda (lines) (my/magit-pickaxe--format-lines lines cache)))
       :file-handler t)
     :prompt "Pickaxe: "
     :lookup #'consult--lookup-member
     :state (my/magit-pickaxe--state)
     :add-history (thing-at-point 'symbol)
     :require-match t
     :category 'consult-grep
     :group #'consult--prefix-group
     :history '(:input consult--grep-history)
     :sort nil)))

(map! :leader
      :desc "Pickaxe (git history → magit-blob)" "s g" #'my/magit-pickaxe-ripgrep)

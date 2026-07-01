;;; git-branch-off-gitq.el --- GitQ: categorical query language for git  -*- lexical-binding: t; -*-

;; Provides `gitq': a pipeline query language for navigating git's object graph.
;; Syntax: source | step | step | terminal
;; Example: (gitq "commits | where .author contains \"alice\" | take 5 | show")

;;; Git execution layer

(defun gitq--git (&rest args)
  "Run git with ARGS; return output lines as a list of non-empty strings."
  (if (fboundp 'magit-git-lines)
      (apply #'magit-git-lines args)
    (let ((buf (generate-new-buffer " *gitq-git*")))
      (unwind-protect
          (progn
            (apply #'call-process "git" nil buf nil args)
            (with-current-buffer buf
              (split-string (buffer-string) "\n" t)))
        (kill-buffer buf)))))

(defun gitq--git-string (&rest args)
  "Run git with ARGS; return first line of output or nil."
  (car (apply #'gitq--git args)))

(defun gitq--toplevel ()
  "Return the git toplevel or signal an error."
  (or (if (fboundp 'magit-toplevel)
          (magit-toplevel)
        (let ((s (gitq--git-string "rev-parse" "--show-toplevel")))
          (when s (file-name-as-directory s))))
      (user-error "gitq: not in a git repository")))

;;; Tokenizer

(defun gitq--split-pipeline (str)
  "Split STR on | separators not inside quoted strings or /regex/ literals."
  (let (stages (cur "") (i 0) (len (length str)))
    (while (< i len)
      (let ((c (aref str i)))
        (cond
         ((eq c ?\")
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len) (not (eq (aref str i) ?\")))
              (when (eq (aref str i) ?\\) (setq i (1+ i)))
              (setq i (1+ i)))
            (setq i (1+ i) cur (concat cur (substring str s i)))))
         ((eq c ?/)
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len) (not (eq (aref str i) ?/)))
              (setq i (1+ i)))
            (setq i (1+ i) cur (concat cur (substring str s i)))))
         ((eq c ?|)
          (push (string-trim cur) stages)
          (setq cur "" i (1+ i)))
         (t (setq cur (concat cur (string c)) i (1+ i))))))
    (push (string-trim cur) stages)
    (nreverse (seq-remove #'string-empty-p stages))))

(defun gitq--tokenize (stage)
  "Tokenize a single pipeline STAGE string into a list of token strings."
  (let (tokens (i 0) (len (length stage)))
    (while (< i len)
      (let ((c (aref stage i)))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r)) (setq i (1+ i)))
         ((eq c ?\")
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len) (not (eq (aref stage i) ?\")))
              (when (eq (aref stage i) ?\\) (setq i (1+ i)))
              (setq i (1+ i)))
            (setq i (1+ i))
            (push (substring stage s i) tokens)))
         ((eq c ?/)
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len) (not (eq (aref stage i) ?/)))
              (setq i (1+ i)))
            (setq i (1+ i))
            (push (substring stage s i) tokens)))
         ((eq c ?,) (push "," tokens) (setq i (1+ i)))
         ((and (< (1+ i) len)
               (member (substring stage i (+ i 2)) '("==" "!=" ">=" "<=")))
          (push (substring stage i (+ i 2)) tokens) (setq i (+ i 2)))
         ((memq c '(?> ?<)) (push (string c) tokens) (setq i (1+ i)))
         ;; Negated dotted path: -.field (used in `sort -.date')
         ((and (eq c ?-) (< (1+ i) len) (eq (aref stage (1+ i)) ?.))
          (let ((s i))
            (setq i (+ i 2))            ; skip the leading -
            (while (and (< i len)
                        (let ((d (aref stage i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?. ?- ?_ ?\[ ?\] ?* ?+))
                              (eq d #x2020))))
              (setq i (1+ i)))
            (push (substring stage s i) tokens)))
         ((eq c ?.)
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len)
                        (let ((d (aref stage i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?. ?- ?_ ?\[ ?\] ?* ?+))
                              (eq d #x2020))))  ; U+2020 DAGGER †
              (setq i (1+ i)))
            (push (substring stage s i) tokens)))
         ((and (>= c ?0) (<= c ?9))
          (let ((s i))
            (while (and (< i len) (>= (aref stage i) ?0) (<= (aref stage i) ?9))
              (setq i (1+ i)))
            (push (substring stage s i) tokens)))
         ((or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)) (eq c ?_))
          (let ((s i))
            (while (and (< i len)
                        (let ((d (aref stage i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?- ?_ ?/ ?~ ?@ ?{ ?})))))
              (setq i (1+ i)))
            (push (substring stage s i) tokens)))
         (t (setq i (1+ i))))))
    (nreverse tokens)))

;;; Parser helpers

(defun gitq--unquote (str)
  "Strip surrounding double-quotes from STR."
  (if (and (> (length str) 1) (eq (aref str 0) ?\"))
      (substring str 1 (1- (length str)))
    str))

(defun gitq--unregex (str)
  "Extract the pattern from a /pattern/ token."
  (if (and (> (length str) 1) (eq (aref str 0) ?/))
      (substring str 1 (1- (length str)))
    str))

;;; Stage parsers

(defun gitq--parse-via (tokens)
  "Parse a `via' stage from TOKENS (the morphism path tokens)."
  (let ((path (car tokens)))
    (unless path (error "gitq: missing morphism after 'via'"))
    (cond
     ((equal path ".parent")          (list :type 'via :morphism 'parent))
     ((equal path ".parent*")         (list :type 'via :morphism 'parent :star t))
     ((equal path ".parent+")         (list :type 'via :morphism 'parent :plus t))
     ((string-match "^\\.parent\\[\\([0-9]+\\)\\]$" path)
      (list :type 'via :morphism 'parent :index (string-to-number (match-string 1 path))))
     ((or (equal path ".parent†") (equal path ".parent†"))
      (list :type 'via :morphism 'parent-adjoint))
     ((equal path ".tree")            (list :type 'via :morphism 'tree))
     ((string-match "^\\.tree\\.entries\\(?:\\[\\(Blob\\|Tree\\)\\]\\)?$" path)
      (list :type 'via :morphism 'tree-entries
            :filter (when (match-string 1 path)
                      (if (equal (match-string 1 path) "Blob") 'blob 'tree))))
     ((equal path ".tree.blobs")      (list :type 'via :morphism 'tree-entries :filter 'blob))
     ((equal path ".tree.subtrees")   (list :type 'via :morphism 'tree-entries :filter 'tree))
     ((equal path ".diff")            (list :type 'via :morphism 'diff
                                            :ref (when (and (cdr tokens)
                                                            (not (string-prefix-p "." (cadr tokens))))
                                                   (cadr tokens))))
     ((equal path ".diff.hunks")      (list :type 'via :morphism 'diff-hunks))
     ((equal path ".history")         (list :type 'via :morphism 'history))
     ((equal path ".commit")          (list :type 'via :morphism 'commit))
     (t (error "gitq: unknown morphism '%s'" path)))))

(defun gitq--parse-where (tokens)
  "Parse `where' conditions from TOKENS (list of token strings)."
  (let (conditions remaining)
    (setq remaining tokens)
    (while (and remaining (string-prefix-p "." (car remaining)))
      (let* ((field-tok (pop remaining))
             (field-str (substring field-tok 1))
             ;; .parents.count → parents-count
             (field (intern (replace-regexp-in-string "\\." "-" field-str))))
        (cond
         ;; Bare flag: .modified, .staged, .untracked (no op/value follows)
         ((or (null remaining) (equal (car remaining) ",")
              (string-prefix-p "." (car remaining)))
          (push (list :field field :op 'is :value t) conditions))
         ;; Operator + value
         (t
          (let* ((op-tok (pop remaining))
                 (op     (intern op-tok)))
            (if (or (null remaining) (equal (car remaining) ",")
                    (string-prefix-p "." (car remaining)))
                (push (list :field field :op op :value t) conditions)
              (let* ((val-tok (pop remaining))
                     (val     (cond
                               ((string-prefix-p "\"" val-tok) (gitq--unquote val-tok))
                               ((string-prefix-p "/" val-tok)  (gitq--unregex val-tok))
                               ((string-match-p "^[0-9]+$" val-tok)
                                (string-to-number val-tok))
                               (t val-tok))))
                (push (list :field field :op op :value val) conditions))))))
        (when (equal (car remaining) ",") (pop remaining))))
    (list :type 'where :conditions (nreverse conditions))))

(defun gitq--parse-pick (tokens)
  "Parse `pick' field list from TOKENS."
  (let (fields)
    (dolist (tok tokens)
      (when (and (not (equal tok ",")) (string-prefix-p "." tok))
        (push (intern (substring tok 1)) fields)))
    (list :type 'pick :fields (nreverse fields))))

(defun gitq--parse-grep (tokens)
  "Parse a `grep' stage from TOKENS."
  (let* ((pat-tok  (car tokens))
         (rest     (cdr tokens))
         (is-regex (string-prefix-p "/" pat-tok))
         (pattern  (if is-regex (gitq--unregex pat-tok) (gitq--unquote pat-tok)))
         (path-filter (when (equal (car rest) "path")
                        (gitq--unquote (cadr rest)))))
    (list :type 'grep :pattern pattern :regex is-regex :path-filter path-filter)))

(defun gitq--parse-pickaxe (tokens)
  "Parse a `pickaxe' stage from TOKENS."
  (let* ((pat-tok  (car tokens))
         (rest     (cdr tokens))
         (is-regex (or (string-prefix-p "/" pat-tok) (equal (car rest) "regex")))
         (pattern  (if (string-prefix-p "/" pat-tok)
                       (gitq--unregex pat-tok)
                     (gitq--unquote pat-tok))))
    (list :type 'pickaxe :pattern pattern :regex is-regex)))

(defconst gitq--terminal-keywords
  '("show" "copy" "insert" "count" "branch-off" "amend" "squash"
    "reword" "remove" "delete" "commit" "stage" "mark" "worktree")
  "Keywords that may appear as terminal pipeline operations.")

(defun gitq--parse-terminal (kw tokens)
  "Parse terminal operation KW with remaining TOKENS."
  (cond
   ((equal kw "show")      (list :type 'terminal :op 'show))
   ((equal kw "copy")      (list :type 'terminal :op 'copy))
   ((equal kw "insert")    (list :type 'terminal :op 'insert))
   ((equal kw "count")     (list :type 'terminal :op 'count))
   ((equal kw "remove")    (list :type 'terminal :op 'remove))
   ((equal kw "delete")    (list :type 'terminal :op 'delete))
   ((equal kw "stage")     (list :type 'terminal :op 'stage))
   ((equal kw "branch-off")
    (let* ((name (when (and tokens (string-prefix-p "\"" (car tokens)))
                   (gitq--unquote (pop tokens))))
           (wt   (when (equal (car tokens) "worktree")
                   (gitq--unquote (cadr tokens)))))
      (list :type 'terminal :op 'branch-off :name name :worktree wt)))
   ((equal kw "amend")
    (cond
     ((equal (car tokens) "no-edit")
      (list :type 'terminal :op 'amend :no-edit t :message nil))
     ((and tokens (string-prefix-p "\"" (car tokens)))
      (list :type 'terminal :op 'amend :no-edit nil
            :message (gitq--unquote (car tokens))))
     (t (list :type 'terminal :op 'amend :no-edit nil :message nil))))
   ((equal kw "squash")
    (list :type 'terminal :op 'squash
          :message (when (and tokens (string-prefix-p "\"" (car tokens)))
                     (gitq--unquote (car tokens)))))
   ((equal kw "reword")
    (list :type 'terminal :op 'reword
          :message (when (and tokens (string-prefix-p "\"" (car tokens)))
                     (gitq--unquote (car tokens)))))
   ((equal kw "commit")
    (list :type 'terminal :op 'commit
          :message (when (and tokens (string-prefix-p "\"" (car tokens)))
                     (gitq--unquote (car tokens)))))
   ((equal kw "mark")
    (list :type 'terminal :op 'mark
          :label (when tokens (gitq--unquote (car tokens)))))
   (t (error "gitq: unknown terminal operation '%s'" kw))))

(defun gitq--parse-source (tokens)
  "Parse the first (source) stage from TOKENS."
  (let ((kw   (car tokens))
        (rest (cdr tokens)))
    (cond
     ((member kw '("commits" "commit"))
      (if (equal (car rest) "in")
          ;; Join all remaining tokens to reconstruct "main..feature" etc.
          (list :type 'source :source 'commits
                :range (apply #'concat (cdr rest)))
        (list :type 'source :source 'commits :range nil)))
     ((equal kw "branches") (list :type 'source :source 'branches))
     ((equal kw "tags")     (list :type 'source :source 'tags))
     ((member kw '("worktrees" "worktree"))
      (list :type 'source :source 'worktree))
     ((equal kw "blobs")    (list :type 'source :source 'blobs))
     ((equal kw "refs")     (list :type 'source :source 'refs))
     (t (list :type 'source :source 'ref :ref kw)))))

(defun gitq--parse-stage (stage-str &optional is-source)
  "Parse a single pipeline stage string into an AST node plist."
  (let* ((tokens (gitq--tokenize stage-str))
         (kw     (car tokens))
         (rest   (cdr tokens)))
    (if is-source
        (gitq--parse-source tokens)
      (cond
       ((equal kw "via")     (gitq--parse-via rest))
       ((equal kw "where")   (gitq--parse-where rest))
       ((equal kw "grep")    (gitq--parse-grep rest))
       ((equal kw "pickaxe") (gitq--parse-pickaxe rest))
       ((equal kw "path")
        (list :type 'path :pattern (gitq--unquote (car rest))))
       ((equal kw "pick")    (gitq--parse-pick rest))
       ((equal kw "take")
        (list :type 'take :n (string-to-number (car rest))))
       ((equal kw "skip")
        (list :type 'skip :n (string-to-number (car rest))))
       ((equal kw "first")   (list :type 'first))
       ((equal kw "last")    (list :type 'last))
       ((equal kw "sort")
        (let* ((f   (car rest))
               (neg (string-prefix-p "-" f))
               (fn  (intern (substring (if neg (substring f 1) f) 1))))
          (list :type 'sort :field fn :desc neg)))
       ((member kw gitq--terminal-keywords)
        (gitq--parse-terminal kw rest))
       (t (error "gitq: unknown pipeline stage keyword '%s'" kw))))))

(defun gitq--parse (pipeline-str)
  "Parse a complete gitq pipeline string into a list of AST node plists."
  (let ((stages (gitq--split-pipeline pipeline-str)))
    (unless stages (error "gitq: empty pipeline"))
    (cons (gitq--parse-stage (car stages) t)
          (mapcar (lambda (s) (gitq--parse-stage s nil))
                  (cdr stages)))))

;;; Git data fetchers

(defconst gitq--log-format "%H%x00%ae%x00%an%x00%ai%x00%P%x00%T%x00%s"
  "NUL-delimited log format using git's %x00 escape (safe to pass as CLI arg).")

(defun gitq--parse-commit-line (line)
  "Parse a NUL-delimited commit log LINE into a frame plist, or nil."
  (let ((parts (split-string line "\x00")))
    (when (>= (length parts) 7)
      (let ((sha (nth 0 parts)))
        (unless (string-empty-p sha)
          (list :type 'commit
                :sha     sha
                :email   (nth 1 parts)
                :author  (nth 2 parts)
                :date    (nth 3 parts)
                :parents (split-string (nth 4 parts) " " t)
                :tree    (nth 5 parts)
                :message (nth 6 parts)))))))

(defun gitq--fetch-commits (&optional range)
  "Fetch commits reachable from HEAD (or within RANGE) as frame plists."
  (let* ((fmt  (format "--format=%s" gitq--log-format))
         (args (if range (list "log" fmt range) (list "log" fmt))))
    (delq nil (mapcar #'gitq--parse-commit-line (apply #'gitq--git args)))))

(defun gitq--fetch-commit (sha-or-ref)
  "Fetch a single commit by SHA-OR-REF, returning a frame plist or nil."
  (let ((sha (gitq--git-string "rev-parse" "--verify" sha-or-ref)))
    (when sha
      (car (delq nil
                 (mapcar #'gitq--parse-commit-line
                         (gitq--git "log" "--no-walk"
                                    (format "--format=%s" gitq--log-format)
                                    sha)))))))

(defun gitq--fetch-branches ()
  "Fetch all local branches as ref frame plists."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "^\\([0-9a-f]\\{40,\\}\\) \\(.+\\)$" line)
                    (list :type 'ref :reftype 'branch
                          :sha  (match-string 1 line)
                          :name (match-string 2 line))))
                (gitq--git "for-each-ref"
                           "--format=%(objectname) %(refname:short)"
                           "refs/heads/"))))

(defun gitq--fetch-tags ()
  "Fetch all tags as ref frame plists."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "^\\([0-9a-f]\\{40,\\}\\) \\(.+\\)$" line)
                    (list :type 'ref :reftype 'tag
                          :sha  (match-string 1 line)
                          :name (match-string 2 line))))
                (gitq--git "for-each-ref"
                           "--format=%(objectname) %(refname:short)"
                           "refs/tags/"))))

(defun gitq--fetch-refs ()
  "Fetch all refs as ref frame plists."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "^\\([0-9a-f]\\{40,\\}\\) \\(.+\\)$" line)
                    (list :type 'ref
                          :sha  (match-string 1 line)
                          :name (match-string 2 line))))
                (gitq--git "for-each-ref"
                           "--format=%(objectname) %(refname:short)"))))

(defun gitq--fetch-worktrees ()
  "Fetch all worktrees as worktree frame plists."
  (let (result entry)
    (dolist (line (gitq--git "worktree" "list" "--porcelain"))
      (cond
       ((string-prefix-p "worktree " line)
        (when entry (push entry result))
        (setq entry (list :type 'worktree :path (substring line 9))))
       ((string-prefix-p "HEAD " line)
        (setq entry (plist-put entry :sha (substring line 5))))
       ((string-prefix-p "branch " line)
        (setq entry (plist-put entry :branch
                               (string-remove-prefix "refs/heads/"
                                                     (substring line 7)))))
       ((string= (string-trim line) "detached")
        (setq entry (plist-put entry :detached t)))))
    (when entry (push entry result))
    (nreverse result)))

(defun gitq--fetch-blobs-at (tree-sha &optional path-filter type-filter)
  "Fetch blob/tree entries from TREE-SHA as frame plists."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match
                         "^\\([0-9]+\\) \\(blob\\|tree\\) \\([0-9a-f]+\\)\t\\(.+\\)$"
                         line)
                    (let* ((mode  (match-string 1 line))
                           (ftype (intern (match-string 2 line)))
                           (sha   (match-string 3 line))
                           (path  (match-string 4 line)))
                      (when (or (null type-filter) (eq ftype type-filter))
                        (when (or (null path-filter)
                                  (gitq--path-matches path path-filter))
                          (list :type ftype :sha sha :path path :mode mode))))))
                (gitq--git "ls-tree" "-r" tree-sha))))

(defun gitq--path-matches (path pattern)
  "Return non-nil if PATH matches the glob PATTERN."
  (or (string-match-p (wildcard-to-regexp pattern) path)
      (string-match-p (regexp-quote pattern) path)))

;;; Pipeline step executors

(defun gitq--exec-source (node)
  "Execute source node NODE and return the initial frame list."
  (pcase (plist-get node :source)
    ('commits
     (gitq--fetch-commits (plist-get node :range)))
    ('ref
     (let ((frame (gitq--fetch-commit (plist-get node :ref))))
       (if frame (list frame) nil)))
    ('branches (gitq--fetch-branches))
    ('tags     (gitq--fetch-tags))
    ('refs     (gitq--fetch-refs))
    ('worktree (gitq--fetch-worktrees))
    ('blobs
     (let ((tree (gitq--git-string "rev-parse" "HEAD^{tree}")))
       (when tree (gitq--fetch-blobs-at tree))))
    (src (error "gitq: unknown source '%s'" src))))

(defun gitq--traverse-parents-star (frames &optional plus)
  "Walk parent links from FRAMES, returning all reachable commits.
When PLUS is non-nil, exclude the start frames themselves (`.parent+')."
  (let (result (visited (make-hash-table :test 'equal)))
    (dolist (start frames)
      (let ((start-sha (plist-get start :sha))
            (queue     (list (plist-get start :sha))))
        (while queue
          (let* ((sha (pop queue))
                 (c   (gitq--fetch-commit sha)))
            (unless (gethash sha visited)
              (puthash sha t visited)
              (when c
                ;; .parent* includes start; .parent+ excludes start
                (unless (and plus (equal sha start-sha))
                  (push c result))
                (dolist (p (plist-get c :parents))
                  (unless (gethash p visited)
                    (push p queue)))))))))
    (nreverse result)))

(defun gitq--exec-via (frames node)
  "Traverse morphism in NODE from FRAMES, returning new frames."
  (let ((m (plist-get node :morphism)))
    (pcase m
      ('parent
       (cond
        ((plist-get node :star) (gitq--traverse-parents-star frames))
        ((plist-get node :plus) (gitq--traverse-parents-star frames t))
        ((numberp (plist-get node :index))
         (let ((idx (plist-get node :index)))
           (delq nil (mapcar (lambda (f)
                               (gitq--fetch-commit
                                (nth idx (plist-get f :parents))))
                             frames))))
        (t
         (delq nil
               (apply #'append
                      (mapcar (lambda (f)
                                (mapcar #'gitq--fetch-commit
                                        (plist-get f :parents)))
                              frames))))))
      ('parent-adjoint
       (let* ((target-shas (mapcar (lambda (f) (plist-get f :sha)) frames))
              (all (gitq--fetch-commits)))
         (seq-filter (lambda (c)
                       (seq-some (lambda (p) (member p target-shas))
                                 (plist-get c :parents)))
                     all)))
      ('tree
       (delq nil
             (mapcar (lambda (f)
                       (let ((tree (plist-get f :tree)))
                         (when tree (list :type 'tree :sha tree))))
                     frames)))
      ('tree-entries
       (let ((filter (plist-get node :filter)))
         (apply #'append
                (mapcar (lambda (f)
                          (let ((tree (or (and (eq (plist-get f :type) 'commit)
                                              (plist-get f :tree))
                                         (plist-get f :sha))))
                            (when tree (gitq--fetch-blobs-at tree nil filter))))
                        frames))))
      ('diff
       (let ((ref (plist-get node :ref)))
         (apply #'append
                (mapcar (lambda (f)
                          (let* ((sha   (plist-get f :sha))
                                 (other (or ref (format "%s^" sha)))
                                 (paths (gitq--git "diff-tree" "-r" "--name-only"
                                                   "--no-commit-id" other sha)))
                            (mapcar (lambda (p)
                                      (list :type 'diff :sha sha :path p
                                            :parent-sha other))
                                    paths)))
                        frames))))
      ('diff-hunks
       (apply #'append
              (mapcar (lambda (f)
                        (let* ((sha    (plist-get f :sha))
                               (parent (format "%s^" sha))
                               (text   (string-join
                                        (gitq--git "diff-tree" "-p" "--no-commit-id"
                                                   "-r" parent sha)
                                        "\n")))
                          (gitq--parse-diff-hunks text sha)))
                      frames)))
      ('history
       (apply #'append
              (mapcar (lambda (f)
                        (let* ((path (plist-get f :path))
                               (shas (gitq--git "log" "--follow" "--format=%H" "--" path)))
                          (delq nil
                                (mapcar (lambda (sha)
                                          (let ((c (gitq--fetch-commit sha)))
                                            (when c
                                              (append c (list :path path)))))
                                        shas))))
                      frames)))
      ('commit
       (delq nil
             (mapcar (lambda (f)
                       (gitq--fetch-commit (plist-get f :commit-sha)))
                     frames)))
      (_ (error "gitq: unknown morphism '%s'" m)))))

(defun gitq--parse-diff-hunks (diff-text commit-sha)
  "Parse DIFF-TEXT into a list of hunk frame plists for COMMIT-SHA."
  (let (hunks cur-path)
    (dolist (line (split-string diff-text "\n"))
      (cond
       ((string-match "^diff --git a/.+ b/\\(.+\\)$" line)
        (setq cur-path (match-string 1 line)))
       ((and cur-path
             (string-match
              "^@@ -[0-9,]+ \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line))
        (let* ((start (string-to-number (match-string 1 line)))
               (count (if (match-string 2 line)
                          (string-to-number (match-string 2 line))
                        1)))
          (push (list :type 'hunk :path cur-path
                      :start-line start :end-line (+ start (max 0 (1- count)))
                      :commit-sha commit-sha)
                hunks)))))
    (nreverse hunks)))

(defun gitq--exec-where (frames node)
  "Filter FRAMES by the conditions in NODE."
  (let ((conds (plist-get node :conditions)))
    (seq-filter (lambda (f)
                  (cl-every (lambda (c) (gitq--eval-condition f c)) conds))
                frames)))

(defun gitq--frame-field (frame field)
  "Extract FIELD (symbol) from FRAME plist."
  (pcase field
    ('sha            (plist-get frame :sha))
    ('message        (plist-get frame :message))
    ('author         (or (plist-get frame :author) (plist-get frame :name)))
    ('email          (plist-get frame :email))
    ('date           (plist-get frame :date))
    ('path           (plist-get frame :path))
    ('name           (plist-get frame :name))
    ('branch         (plist-get frame :branch))
    ('parents-count  (length (plist-get frame :parents)))
    ('modified       (plist-get frame :modified))
    ('staged         (plist-get frame :staged))
    ('untracked      (plist-get frame :untracked))
    (_ (plist-get frame (intern (format ":%s" field))))))

(defun gitq--eval-condition (frame cond)
  "Return non-nil if FRAME satisfies COND plist."
  (let* ((field  (plist-get cond :field))
         (op     (plist-get cond :op))
         (value  (plist-get cond :value))
         (actual (gitq--frame-field frame field)))
    (pcase op
      ('==       (equal actual value))
      ('!=       (not (equal actual value)))
      ('>        (and (numberp actual) (numberp value) (> actual value)))
      ('<        (and (numberp actual) (numberp value) (< actual value)))
      ('>=       (and (numberp actual) (numberp value) (>= actual value)))
      ('<=       (and (numberp actual) (numberp value) (<= actual value)))
      ('contains (and (stringp actual) (stringp value)
                      (string-match-p (regexp-quote value) actual)))
      ('matches  (and (stringp actual) (stringp value)
                      (string-match-p value actual)))
      ('after    (gitq--date-op actual value #'>))
      ('before   (gitq--date-op actual value #'<))
      ('within   (gitq--date-within actual value))
      ('is       (if (eq value t) (not (null actual)) (equal actual value)))
      (_         (error "gitq: unknown where operator '%s'" op)))))

(defun gitq--date-op (date-str ref-str cmp)
  "Compare DATE-STR and REF-STR using CMP function, or return nil on error."
  (ignore-errors
    (funcall cmp
             (float-time (date-to-time date-str))
             (float-time (date-to-time ref-str)))))

(defun gitq--date-within (date-str period-str)
  "Return non-nil if DATE-STR falls within PERIOD-STR of now."
  (when (string-match "^\\([0-9]+\\) +\\(day\\|week\\|month\\|year\\)s?\\b" period-str)
    (let* ((n    (string-to-number (match-string 1 period-str)))
           (unit (match-string 2 period-str))
           (secs (* n (pcase unit ("day" 86400) ("week" 604800)
                              ("month" 2592000) ("year" 31536000) (_ 0))))
           (cutoff (- (float-time) secs)))
      (ignore-errors (>= (float-time (date-to-time date-str)) cutoff)))))

(defun gitq--exec-grep (frames node)
  "Grep blob/commit FRAMES for pattern in NODE, returning line frames."
  (let* ((pattern     (plist-get node :pattern))
         (regex       (plist-get node :regex))
         (path-filter (plist-get node :path-filter)))
    (apply #'append
           (mapcar (lambda (f)
                     (let* ((sha  (plist-get f :sha))
                            (args (append (list "grep" "-n" "--no-color"
                                               (if regex "-E" "-F")
                                               pattern sha)
                                          (when path-filter (list "--" path-filter)))))
                       (delq nil
                             (mapcar (lambda (line)
                                       (when (string-match
                                              "^[^:]+:\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$"
                                              line)
                                         (list :type 'line :sha sha
                                               :path        (match-string 1 line)
                                               :line-number (string-to-number
                                                             (match-string 2 line))
                                               :content     (match-string 3 line)
                                               :commit-sha  sha)))
                                     (apply #'gitq--git args)))))
                   frames))))

(defun gitq--exec-pickaxe (frames node)
  "Filter commit FRAMES to those whose diffs match the pickaxe pattern in NODE."
  (let* ((pattern (plist-get node :pattern))
         (regex   (plist-get node :regex))
         (flag    (if regex "-G" "-S"))
         (shas    (delq nil (mapcar (lambda (f) (plist-get f :sha)) frames))))
    (when shas
      (let ((hits (apply #'gitq--git
                         (append (list "log" flag pattern "--format=%H" "--no-walk")
                                 shas))))
        (seq-filter (lambda (f) (member (plist-get f :sha) hits)) frames)))))

(defun gitq--exec-path (frames node)
  "Filter FRAMES to those whose :path matches the pattern in NODE."
  (let ((pattern (plist-get node :pattern)))
    (seq-filter (lambda (f)
                  (let ((p (plist-get f :path)))
                    (and p (gitq--path-matches p pattern))))
                frames)))

(defun gitq--exec-pick (frames node)
  "Project each frame in FRAMES to only the fields listed in NODE."
  (let ((fields (plist-get node :fields)))
    (mapcar (lambda (f)
              (let (proj)
                (dolist (field fields)
                  (setq proj (plist-put proj field (gitq--frame-field f field))))
                (cons :type (cons 'projection proj))))
            frames)))

(defun gitq--exec-sort (frames node)
  "Sort FRAMES by the field in NODE."
  (let ((field (plist-get node :field))
        (desc  (plist-get node :desc)))
    (sort (copy-sequence frames)
          (lambda (a b)
            (let ((va (or (gitq--frame-field a field) ""))
                  (vb (or (gitq--frame-field b field) "")))
              (if desc (string> va vb) (string< va vb)))))))

(defun gitq--exec-step (frames step)
  "Execute one pipeline STEP against FRAMES, returning new frame list."
  (let ((type (plist-get step :type)))
    (pcase type
      ('via     (gitq--exec-via frames step))
      ('where   (gitq--exec-where frames step))
      ('grep    (gitq--exec-grep frames step))
      ('pickaxe (gitq--exec-pickaxe frames step))
      ('path    (gitq--exec-path frames step))
      ('pick    (gitq--exec-pick frames step))
      ('take    (seq-take frames (plist-get step :n)))
      ('skip    (seq-drop frames (plist-get step :n)))
      ('first   (when frames (list (car frames))))
      ('last    (when frames (list (car (last frames)))))
      ('sort    (gitq--exec-sort frames step))
      (_        frames))))

;;; Terminal operations

(defun gitq--frame-commit-sha (frame)
  "Return the commit SHA for FRAME (direct or via :commit-sha)."
  (or (plist-get frame :commit-sha)
      (when (memq (plist-get frame :type) '(commit ref))
        (plist-get frame :sha))
      (plist-get frame :sha)))

;;; Results display

(defvar gitq-results-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'gitq-results-visit)
    (define-key m (kbd "b")   #'gitq-results-branch-off)
    (define-key m (kbd "c")   #'gitq-results-copy-sha)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for `gitq-results-mode'.")

(define-derived-mode gitq-results-mode special-mode "GitQ"
  "Major mode for displaying gitq pipeline results."
  (setq truncate-lines t))

(defun gitq--insert-frame (frame)
  "Insert a human-readable line for FRAME into the current buffer."
  (let ((type  (plist-get frame :type))
        (start (point)))
    (pcase type
      ('commit
       (let* ((sha   (plist-get frame :sha))
              (short (when sha (substring sha 0 (min 8 (length sha))))))
         (insert (propertize (or short "?") 'face 'magit-hash))
         (insert "  ")
         (let ((author (plist-get frame :author)))
           (when author
             (insert (propertize
                      (format "%-20s"
                              (substring author 0 (min 20 (length author))))
                      'face 'magit-log-author))))
         (let ((date (plist-get frame :date)))
           (when date
             (insert (propertize (substring date 0 (min 10 (length date)))
                                 'face 'magit-log-date))
             (insert "  ")))
         (insert (or (plist-get frame :message) ""))))
      ('blob
       (insert (propertize (or (plist-get frame :path) "?") 'face 'magit-filename)))
      ('ref
       (insert (propertize (or (plist-get frame :name) "?")
                           'face 'magit-branch-local))
       (when-let ((sha (plist-get frame :sha)))
         (insert "  ")
         (insert (propertize (substring sha 0 (min 8 (length sha))) 'face 'magit-hash))))
      ('worktree
       (insert (propertize (or (plist-get frame :path) "?") 'face 'magit-filename))
       (when-let ((b (plist-get frame :branch)))
         (insert "  ")
         (insert (propertize b 'face 'magit-branch-local))))
      ('line
       (insert (propertize (or (plist-get frame :path) "?") 'face 'magit-filename))
       (insert ":")
       (insert (propertize (number-to-string (or (plist-get frame :line-number) 0))
                           'face 'shadow))
       (insert ": ")
       (insert (or (plist-get frame :content) "")))
      ('hunk
       (insert (propertize (or (plist-get frame :path) "?") 'face 'magit-filename))
       (insert (format " lines %d-%d"
                       (or (plist-get frame :start-line) 0)
                       (or (plist-get frame :end-line) 0))))
      (_
       ;; projected or unknown — dump key:value pairs
       (let (first)
         (cl-loop for (k v) on frame by #'cddr
                  do (progn
                       (unless first (setq first t))
                       (insert (format "%s:%s " k v)))))))
    (put-text-property start (point) 'gitq-frame frame)
    (put-text-property start (point) 'gitq-sha (gitq--frame-commit-sha frame))
    (insert "\n")))

(defun gitq--display (frames pipeline-str)
  "Show FRAMES in the *gitq* results buffer."
  (with-current-buffer (get-buffer-create "*gitq*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (format "gitq: %s\n\n" pipeline-str)
                          'face 'font-lock-comment-face))
      (if frames
          (dolist (f frames) (gitq--insert-frame f))
        (insert "(no results)\n"))
      (gitq-results-mode)
      (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun gitq-results-visit ()
  "Visit the git object at point in the *gitq* buffer."
  (interactive)
  (let* ((frame (get-text-property (point) 'gitq-frame))
         (sha   (get-text-property (point) 'gitq-sha))
         (type  (plist-get frame :type)))
    (pcase type
      ('blob (when (and sha (fboundp 'magit-find-file))
               (magit-find-file sha (plist-get frame :path))))
      (_     (when (and sha (fboundp 'magit-show-commit))
               (magit-show-commit sha))))))

;;;###autoload
(defun gitq-results-branch-off ()
  "Create a branch from the commit at point in the *gitq* buffer."
  (interactive)
  (let ((sha (get-text-property (point) 'gitq-sha)))
    (unless sha (user-error "No commit at point"))
    (let ((name (read-string "Branch name: ")))
      (gitq--git "checkout" "-b" name sha)
      (when (fboundp 'magit-refresh) (magit-refresh))
      (message "gitq: created branch '%s'" name))))

;;;###autoload
(defun gitq-results-copy-sha ()
  "Copy the SHA at point to the kill ring."
  (interactive)
  (let ((sha (get-text-property (point) 'gitq-sha)))
    (if sha
        (progn (kill-new sha)
               (message "gitq: copied %s" (substring sha 0 (min 8 (length sha)))))
      (user-error "No SHA at point"))))

;;; Terminal dispatch

(defun gitq--apply-terminal (frames node pipeline-str)
  "Apply terminal operation from NODE to FRAMES."
  (pcase (plist-get node :op)
    ('show
     (gitq--display frames pipeline-str))
    ('copy
     (let ((sha (gitq--frame-commit-sha (car frames))))
       (if sha
           (progn (kill-new sha)
                  (message "gitq: copied %s" (substring sha 0 (min 8 (length sha)))))
         (user-error "gitq copy: no SHA in result"))))
    ('insert
     (let ((sha (gitq--frame-commit-sha (car frames))))
       (when sha (insert sha))))
    ('count
     (message "gitq: %d result(s)" (length frames)))
    ('branch-off
     (let* ((f    (car frames))
            (sha  (gitq--frame-commit-sha f))
            (name (or (plist-get node :name)
                      (read-string "Branch name: ")))
            (wt   (plist-get node :worktree)))
       (unless sha (user-error "gitq branch-off: no commit in result"))
       (if wt
           (gitq--git "worktree" "add" "-b" name wt sha)
         (gitq--git "checkout" "-b" name sha))
       (when (fboundp 'magit-refresh) (magit-refresh))
       (message "gitq: created branch '%s'" name)))
    ('amend
     (let ((no-edit  (plist-get node :no-edit))
           (msg      (plist-get node :message)))
       (cond
        (no-edit (gitq--git "commit" "--amend" "--no-edit"))
        (msg     (gitq--git "commit" "--amend" "-m" msg))
        (t (if (fboundp 'magit-commit-amend)
               (call-interactively #'magit-commit-amend)
             (gitq--git "commit" "--amend"))))
       (when (fboundp 'magit-refresh) (magit-refresh))))
    ('reword
     (let* ((f    (car frames))
            (sha  (gitq--frame-commit-sha f))
            (msg  (plist-get node :message)))
       (unless sha (user-error "gitq reword: no commit in result"))
       (if msg
           (when (fboundp 'git-branch-off--reword-apply)
             (git-branch-off--reword-apply sha msg))
         (when (fboundp 'git-branch-off-reword)
           (git-branch-off-reword sha)))))
    ('squash
     (let ((msg (plist-get node :message)))
       (message "gitq squash: %d commits%s — use git-branch-off-squash for full support"
                (length frames)
                (if msg (format " → \"%s\"" msg) ""))))
    ('remove
     (let* ((f   (car frames))
            (sha (gitq--frame-commit-sha f)))
       (unless sha (user-error "gitq remove: no commit in result"))
       (when (fboundp 'git-branch-off-remove)
         (git-branch-off-remove sha))))
    ('commit
     (let ((msg (plist-get node :message)))
       (if msg
           (progn
             (gitq--git "commit" "-m" msg)
             (when (fboundp 'magit-refresh) (magit-refresh)))
         (when (fboundp 'magit-commit-create)
           (call-interactively #'magit-commit-create)))))
    ('stage
     (when (fboundp 'magit-stage-modified)
       (magit-stage-modified)))
    ('mark
     (let* ((f     (car frames))
            (sha   (gitq--frame-commit-sha f))
            (label (plist-get node :label)))
       (when (and sha label)
         (gitq--git "notes" "add" "-m" label sha)
         (message "gitq: marked %s with '%s'"
                  (substring sha 0 (min 8 (length sha))) label))))
    (_
     (gitq--display frames pipeline-str))))

;;; Main entry points

(defvar gitq--history nil "Minibuffer history list for `gitq-interactive'.")

;;;###autoload
(defun gitq (pipeline)
  "Execute a GitQ PIPELINE string in the current git repository.

PIPELINE syntax:  source | step ... | terminal

Sources:   commits [in RANGE]  HEAD  BRANCH  branches  tags  refs  worktrees  blobs
Steps:     via MORPHISM  where COND[, COND...]  grep PATTERN  pickaxe PATTERN
           path GLOB  pick .FIELD[, ...]  take N  skip N  first  last  sort .FIELD
Terminals: show  copy  insert  count  branch-off [NAME]  amend [no-edit|MSG]
           squash [MSG]  reword [MSG]  remove  commit [MSG]

Examples:
  (gitq \"commits | where .author contains \\\"alice\\\" | take 10 | show\")
  (gitq \"HEAD | via .parent* | take 3 | squash \\\"consolidated\\\"\")
  (gitq \"commits | where .message contains \\\"fix\\\" | pick .sha, .date, .message\")"
  (interactive (list (gitq--read-pipeline "gitq (pipe syntax)> ")))
  (let* ((default-directory (gitq--toplevel))
         (nodes    (gitq--parse pipeline))
         (src-node (car nodes))
         (rest     (cdr nodes))
         (last     (car (last rest)))
         (is-term  (and last (eq (plist-get last :type) 'terminal)))
         (steps    (if is-term (butlast rest) rest))
         (terminal (when is-term last)))
    (let* ((frames (gitq--exec-source src-node))
           (result (cl-reduce #'gitq--exec-step steps :initial-value frames)))
      (if terminal
          (gitq--apply-terminal result terminal pipeline)
        (gitq--display result pipeline)))))

;;;###autoload
(defun gitq-interactive ()
  "Prompt for and execute a gitq pipeline with TAB completion."
  (interactive)
  (let ((p (gitq--read-pipeline "gitq> ")))
    (unless (string-empty-p (string-trim p))
      (gitq-flat p))))

;;; Flat-syntax pipeline parser (whitespace-separated stages, /terminal keywords)
;;
;; Grammar:
;;   pipeline ::= source step* terminal?
;;   source   ::= "commits" ["in" range-tokens] | "HEAD" | BRANCH
;;               | "branches" | "tags" | "refs" | "worktrees" | "blobs"
;;   step     ::= "via" MORPHISM | "where" conditions | "grep" PATTERN
;;               | "pickaxe" PATTERN ["regex"] | "path" GLOB
;;               | "pick" .FIELD[,...] | "take" N | "skip" N
;;               | "first" | "last" | "sort" ["-"].FIELD
;;   terminal ::= "/show" | "/copy" | "/insert" | "/count" | "/branch-off" [NAME]
;;               | "/amend" ["no-edit"|MSG] | "/squash" [MSG] | "/reword" [MSG]
;;               | "/remove" | "/delete" | "/commit" [MSG] | "/stage"
;;               | "/mark" [LABEL] | "/worktree"
;;   conditions ::= condition ("," condition)*
;;   condition  ::= "." FIELD [OP value]
;;   value      ::= QUOTED | /REGEX/ | NUMBER | BARE-WORD (not a step keyword)
;;
;; Disambiguation rules:
;;   1. Terminals start with / and have no closing /  (/show not /show/).
;;      /regex/ literals have a closing / and cannot be terminals.
;;   2. Step keywords (via where grep pickaxe path pick take skip first last sort)
;;      always start a new stage; they are reserved and cannot appear as unquoted
;;      values. Use quotes when searching for these literal strings:
;;        where .message contains "take"   (not: where .message contains take)
;;   3. Former terminal identifiers (commit show count remove stage mark …) are
;;      now plain identifiers and can appear freely as where-clause values.
;;   4. In "commits in RANGE", range tokens are consumed until a step keyword,
;;      /terminal, or end of input. Branch names that are step keywords must be
;;      quoted.

(defconst gitq--flat-step-keywords
  '("via" "where" "grep" "pickaxe" "path" "pick" "take" "skip" "first" "last" "sort")
  "Reserved step keywords in flat-syntax pipelines.
These always start a new stage; quote them when used as string values.")

(defun gitq--tokenize-flat (str)
  "Tokenize a flat pipeline STR.
Like `gitq--tokenize' but distinguishes /command terminal tokens from
/pattern/ regex literals by looking for a matching closing slash."
  (let (tokens (i 0) (len (length str)))
    (while (< i len)
      (let ((c (aref str i)))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r)) (setq i (1+ i)))
         ((eq c ?\")
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len) (not (eq (aref str i) ?\")))
              (when (eq (aref str i) ?\\) (setq i (1+ i)))
              (setq i (1+ i)))
            (setq i (1+ i))
            (push (substring str s i) tokens)))
         ((eq c ?/)
          ;; Scan forward to see if there is a matching closing /.
          ;; Found  → /pattern/ regex literal.
          ;; Absent → /command terminal token.
          (let ((j (1+ i)))
            (while (and (< j len) (not (eq (aref str j) ?/)))
              (setq j (1+ j)))
            (if (< j len)
                ;; Regex literal: consume up to and including closing /
                (progn (push (substring str i (1+ j)) tokens)
                       (setq i (1+ j)))
              ;; Command token: consume /alpha-chars
              (let ((s i))
                (setq i (1+ i))
                (while (and (< i len)
                            (let ((d (aref str i)))
                              (or (and (>= d ?a) (<= d ?z))
                                  (and (>= d ?A) (<= d ?Z))
                                  (and (>= d ?0) (<= d ?9))
                                  (memq d '(?- ?_)))))
                  (setq i (1+ i)))
                (push (substring str s i) tokens)))))
         ((eq c ?,) (push "," tokens) (setq i (1+ i)))
         ((and (< (1+ i) len)
               (member (substring str i (+ i 2)) '("==" "!=" ">=" "<=")))
          (push (substring str i (+ i 2)) tokens) (setq i (+ i 2)))
         ((memq c '(?> ?<)) (push (string c) tokens) (setq i (1+ i)))
         ((and (eq c ?-) (< (1+ i) len) (eq (aref str (1+ i)) ?.))
          (let ((s i))
            (setq i (+ i 2))
            (while (and (< i len)
                        (let ((d (aref str i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?. ?- ?_ ?\[ ?\] ?* ?+))
                              (eq d #x2020))))
              (setq i (1+ i)))
            (push (substring str s i) tokens)))
         ((eq c ?.)
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len)
                        (let ((d (aref str i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?. ?- ?_ ?\[ ?\] ?* ?+))
                              (eq d #x2020))))
              (setq i (1+ i)))
            (push (substring str s i) tokens)))
         ((and (>= c ?0) (<= c ?9))
          (let ((s i))
            (while (and (< i len) (>= (aref str i) ?0) (<= (aref str i) ?9))
              (setq i (1+ i)))
            (push (substring str s i) tokens)))
         ((or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)) (eq c ?_))
          (let ((s i))
            (while (and (< i len)
                        (let ((d (aref str i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?- ?_ ?/ ?~ ?@ ?{ ?})))))
              (setq i (1+ i)))
            (push (substring str s i) tokens)))
         (t (setq i (1+ i))))))
    (nreverse tokens)))

(defun gitq--flat-step-p (tok)
  "Return non-nil if TOK is a reserved step keyword in flat-syntax mode."
  (member tok gitq--flat-step-keywords))

(defun gitq--flat-terminal-p (tok)
  "Return non-nil if TOK is a /command terminal token (not a /regex/ literal)."
  (and (stringp tok)
       (> (length tok) 1)
       (eq (aref tok 0) ?/)
       ;; A /regex/ ends with /; a /command does not
       (not (eq (aref tok (1- (length tok))) ?/))))

(defun gitq--flat-boundary-p (tok)
  "Return non-nil if TOK is a stage boundary in flat-syntax mode."
  (or (null tok)
      (gitq--flat-step-p tok)
      (gitq--flat-terminal-p tok)))

(defun gitq--flat-parse-where (tokens)
  "Parse where-conditions from flat TOKENS, returning (node . remaining).
Step keywords and /terminals act as stage boundaries and are never consumed
as condition values."
  (let (conditions)
    (while (and tokens (string-prefix-p "." (car tokens)))
      (let* ((field-tok (pop tokens))
             (field     (intern (replace-regexp-in-string
                                 "\\." "-" (substring field-tok 1))))
             (next      (car tokens)))
        (cond
         ;; Bare flag: next token is a boundary, comma, or another .field
         ((or (null next) (equal next ",")
              (string-prefix-p "." next)
              (gitq--flat-boundary-p next))
          (push (list :field field :op 'is :value t) conditions))
         ;; Operator present
         (t
          (let* ((op-tok (pop tokens))
                 (op     (intern op-tok))
                 (next2  (car tokens)))
            (cond
             ;; Step keyword immediately after an operator that requires a value:
             ;; this is always an error — the keyword must be quoted.
             ((gitq--flat-step-p next2)
              (error
               "gitq: '%s' requires a value; step keyword '%s' must be quoted: \"%s\""
               op-tok next2 next2))
             ;; No value: nil, comma, dotted field, or /terminal after operator
             ((or (null next2) (equal next2 ",")
                  (string-prefix-p "." next2)
                  (gitq--flat-terminal-p next2))
              (push (list :field field :op op :value t) conditions))
             ;; Normal value
             (t
              (let* ((val-tok (pop tokens))
                     (val     (cond
                               ((string-prefix-p "\"" val-tok) (gitq--unquote val-tok))
                               ((string-prefix-p "/" val-tok)  (gitq--unregex val-tok))
                               ((string-match-p "^[0-9]+$" val-tok)
                                (string-to-number val-tok))
                               (t val-tok))))
                (push (list :field field :op op :value val) conditions))))))))
      (when (equal (car tokens) ",") (pop tokens)))
    (cons (list :type 'where :conditions (nreverse conditions)) tokens)))

(defun gitq--flat-parse-source (tokens)
  "Parse source node from flat TOKENS, returning (node . remaining)."
  (let ((kw (pop tokens)))
    (cond
     ((member kw '("commits" "commit"))
      (if (equal (car tokens) "in")
          (let (range-parts)
            (pop tokens)                  ; consume "in"
            (while (and tokens (not (gitq--flat-boundary-p (car tokens))))
              (push (pop tokens) range-parts))
            (cons (list :type 'source :source 'commits
                        :range (apply #'concat (nreverse range-parts)))
                  tokens))
        (cons (list :type 'source :source 'commits :range nil) tokens)))
     ((equal kw "branches") (cons (list :type 'source :source 'branches) tokens))
     ((equal kw "tags")     (cons (list :type 'source :source 'tags)     tokens))
     ((member kw '("worktrees" "worktree"))
      (cons (list :type 'source :source 'worktree) tokens))
     ((equal kw "blobs")    (cons (list :type 'source :source 'blobs)    tokens))
     ((equal kw "refs")     (cons (list :type 'source :source 'refs)     tokens))
     (t (cons (list :type 'source :source 'ref :ref kw) tokens)))))

(defun gitq--flat-parse-via (tokens)
  "Parse via-step morphism from flat TOKENS, returning (node . remaining).
Handles the optional REF argument of .diff without consuming step keywords."
  (let* ((path (pop tokens))
         (node (cond
                ((equal path ".parent")   (list :type 'via :morphism 'parent))
                ((equal path ".parent*")  (list :type 'via :morphism 'parent :star t))
                ((equal path ".parent+")  (list :type 'via :morphism 'parent :plus t))
                ((string-match "^\\.parent\\[\\([0-9]+\\)\\]$" path)
                 (list :type 'via :morphism 'parent
                       :index (string-to-number (match-string 1 path))))
                ((or (equal path ".parent†") (equal path ".parent†"))
                 (list :type 'via :morphism 'parent-adjoint))
                ((equal path ".tree")     (list :type 'via :morphism 'tree))
                ((string-match "^\\.tree\\.entries\\(?:\\[\\(Blob\\|Tree\\)\\]\\)?$" path)
                 (list :type 'via :morphism 'tree-entries
                       :filter (when (match-string 1 path)
                                 (if (equal (match-string 1 path) "Blob") 'blob 'tree))))
                ((equal path ".tree.blobs")    (list :type 'via :morphism 'tree-entries :filter 'blob))
                ((equal path ".tree.subtrees") (list :type 'via :morphism 'tree-entries :filter 'tree))
                ((equal path ".diff")
                 ;; Optional REF: consume only if not a step keyword or /terminal
                 (let ((ref (when (and tokens
                                       (not (gitq--flat-boundary-p (car tokens)))
                                       (not (string-prefix-p "." (car tokens))))
                              (pop tokens))))
                   (list :type 'via :morphism 'diff :ref ref)))
                ((equal path ".diff.hunks") (list :type 'via :morphism 'diff-hunks))
                ((equal path ".history")    (list :type 'via :morphism 'history))
                ((equal path ".commit")     (list :type 'via :morphism 'commit))
                (t (error "gitq: unknown morphism '%s'" path)))))
    (cons node tokens)))

(defun gitq--flat-parse-step (tokens)
  "Parse one step node from flat TOKENS (first token must be a step keyword).
Returns (node . remaining)."
  (let ((kw (pop tokens)))
    (pcase kw
      ("via" (gitq--flat-parse-via tokens))
      ("where" (gitq--flat-parse-where tokens))
      ("grep"
       (let* ((pat-tok (pop tokens))
              (regex   (string-prefix-p "/" pat-tok))
              (pattern (if regex (gitq--unregex pat-tok) (gitq--unquote pat-tok))))
         ;; Inline "path" qualifier removed in flat mode — use a separate path step.
         (cons (list :type 'grep :pattern pattern :regex regex :path-filter nil)
               tokens)))
      ("pickaxe"
       (let* ((pat-tok (pop tokens))
              (regex   (or (string-prefix-p "/" pat-tok)
                           (equal (car tokens) "regex")))
              (pattern (if (string-prefix-p "/" pat-tok)
                           (gitq--unregex pat-tok)
                         (gitq--unquote pat-tok))))
         (when (equal (car tokens) "regex") (pop tokens))
         (cons (list :type 'pickaxe :pattern pattern :regex regex) tokens)))
      ("path"
       (cons (list :type 'path :pattern (gitq--unquote (pop tokens))) tokens))
      ("pick"
       (let (fields)
         (while (and tokens (not (gitq--flat-boundary-p (car tokens))))
           (let ((tok (pop tokens)))
             (when (and (not (equal tok ",")) (string-prefix-p "." tok))
               (push (intern (substring tok 1)) fields))))
         (cons (list :type 'pick :fields (nreverse fields)) tokens)))
      ("take"
       (cons (list :type 'take :n (string-to-number (pop tokens))) tokens))
      ("skip"
       (cons (list :type 'skip :n (string-to-number (pop tokens))) tokens))
      ("first" (cons (list :type 'first) tokens))
      ("last"  (cons (list :type 'last)  tokens))
      ("sort"
       (let* ((f   (pop tokens))
              (neg (string-prefix-p "-" f))
              (fn  (intern (substring (if neg (substring f 1) f) 1))))
         (cons (list :type 'sort :field fn :desc neg) tokens)))
      (_ (error "gitq: unknown step keyword '%s'" kw)))))

(defun gitq--flat-parse-terminal (tok tokens)
  "Parse /terminal token TOK using TOKENS for optional arguments.
Returns (node . remaining)."
  (let ((op-str (substring tok 1)))  ; strip leading /
    (cons (gitq--parse-terminal op-str tokens)
          ;; Terminals consume 0-2 tokens from `tokens' internally via pop,
          ;; but gitq--parse-terminal uses its own local copy of the list.
          ;; Since terminals must be last, pass nil as remaining.
          nil)))

(defun gitq--parse-flat (pipeline-str)
  "Parse a flat pipeline PIPELINE-STR into a list of AST node plists.
Uses whitespace as the stage separator with /terminal syntax.
No pipe character required."
  (let* ((tokens (gitq--tokenize-flat (string-trim pipeline-str)))
         nodes)
    (unless tokens (error "gitq: empty pipeline"))
    ;; Parse source (first stage)
    (let* ((result (gitq--flat-parse-source tokens)))
      (push (car result) nodes)
      (setq tokens (cdr result)))
    ;; Parse steps and terminal
    (while tokens
      (let ((tok (car tokens)))
        (cond
         ((gitq--flat-terminal-p tok)
          (let* ((result (gitq--flat-parse-terminal tok (cdr tokens))))
            (push (car result) nodes)
            (setq tokens nil)))     ; terminal is always last
         ((gitq--flat-step-p tok)
          (let* ((result (gitq--flat-parse-step tokens)))
            (push (car result) nodes)
            (setq tokens (cdr result))))
         (t
          (error "gitq: expected step keyword or /terminal, got '%s'" tok)))))
    (nreverse nodes)))

;;; Completion

(defconst gitq--complete-source-keywords
  '("commits" "branches" "tags" "refs" "worktrees" "blobs" "HEAD")
  "Source keywords offered at the start of a pipeline.")

(defconst gitq--complete-morphisms
  '(".parent" ".parent*" ".parent+" ".tree" ".tree.blobs" ".tree.subtrees"
    ".tree.entries" ".tree.entries[Blob]" ".tree.entries[Tree]"
    ".diff" ".diff.hunks" ".history" ".commit")
  "Morphism paths offered after `via'.")

(defconst gitq--complete-field-names
  '(".sha" ".author" ".email" ".date" ".message" ".path" ".name"
    ".branch" ".parents-count" ".modified" ".staged" ".untracked")
  "Field names offered after `where', `sort', `pick', and comma separators.")

(defconst gitq--complete-where-operators
  '("==" "!=" ">" "<" ">=" "<=" "contains" "matches" "after" "before" "within" "is")
  "Operators offered after a field name in a where clause.")

(defconst gitq--complete-terminals
  '("/show" "/copy" "/insert" "/count" "/branch-off" "/amend"
    "/squash" "/reword" "/remove" "/delete" "/commit" "/stage" "/mark" "/worktree")
  "Terminal /command keywords.")

(defun gitq--complete-candidates (input)
  "Return a list of completion candidates for the pipeline string INPUT.
INPUT is everything typed so far; completions extend the last partial word."
  (let* ((trimmed    (string-trim-right input))
         (trailing   (not (equal trimmed input)))  ; trailing whitespace?
         (tokens     (gitq--tokenize-flat trimmed))
         ;; In-progress partial word (nil when trailing whitespace)
         (partial    (unless trailing (car (last tokens))))
         ;; Tokens that are fully typed
         (ctx        (if trailing tokens (butlast tokens)))
         (n          (length ctx))
         (last-ctx   (when (> n 0) (nth (1- n) ctx)))
         (prev-ctx   (when (> n 1) (nth (- n 2) ctx))))
    (cond
     ;; Start of pipeline → source keywords
     ((= n 0)
      gitq--complete-source-keywords)

     ;; After "commits" at position 1 → "in" or steps/terminals
     ((and (= n 1) (equal last-ctx "commits"))
      (cons "in" (append gitq--flat-step-keywords gitq--complete-terminals)))

     ;; After "commits in" → branch and tag names from git
     ((and (equal last-ctx "in") (equal prev-ctx "commits"))
      (ignore-errors
        (append (gitq--git "branch" "--format=%(refname:short)")
                (gitq--git "tag" "--list"))))

     ;; After "via" → morphisms
     ((equal last-ctx "via")
      gitq--complete-morphisms)

     ;; After "where" or "," (start of another condition) → field names
     ((or (equal last-ctx "where") (equal last-ctx ","))
      gitq--complete-field-names)

     ;; After a .field not preceded by "via" or "sort" → where operators
     ((and last-ctx (string-prefix-p "." last-ctx)
           (not (member prev-ctx '("via" "sort"))))
      gitq--complete-where-operators)

     ;; After "sort" → field names with optional "-" negation prefix
     ((equal last-ctx "sort")
      (append gitq--complete-field-names
              (mapcar (lambda (f) (concat "-" f)) gitq--complete-field-names)))

     ;; After "pick" or pick-comma → field names
     ((or (equal last-ctx "pick")
          (and (equal last-ctx ",") (member "pick" ctx)))
      gitq--complete-field-names)

     ;; After a where-operator → dynamic values (authors etc.)
     ((member last-ctx gitq--complete-where-operators)
      (let ((field (when (> n 1) (nth (- n 2) ctx))))
        (when (member field '(".author" ".email"))
          (ignore-errors
            (delete-dups (gitq--git "log" "--format=%an" "--all"))))))

     ;; Otherwise → step keywords + terminals
     (t (append gitq--flat-step-keywords gitq--complete-terminals)))))

(defun gitq--pipeline-completion-table (str pred action)
  "Completion table for a gitq pipeline string STR.
Returns full-pipeline candidates (prefix + token) so vertico/corfu can display
and filter them correctly without needing a separate CAPF layer."
  (if (eq action 'metadata)
      '(metadata (category . gitq-pipeline))
    (let* ((trimmed  (string-trim-right str))
           (trailing (not (equal trimmed str)))
           (tokens   (gitq--tokenize-flat trimmed))
           (partial  (if trailing "" (or (car (last tokens)) "")))
           (prefix   (substring str 0 (- (length str) (length partial))))
           (cands    (gitq--complete-candidates str))
           (full     (mapcar (lambda (c) (concat prefix c)) (or cands '()))))
      (complete-with-action action full str pred))))

(defun gitq-completion-at-point ()
  "CAPF for gitq pipeline strings in the minibuffer.
Fallback for environments that call `completion-at-point' directly
(e.g. vanilla Emacs, company-capf).  Vertico uses the completing-read
table instead and does not call this."
  (when (minibufferp)
    (let* ((start  (minibuffer-prompt-end))
           (input  (buffer-substring-no-properties start (point)))
           (trimmed   (string-trim-right input))
           (trailing  (not (equal trimmed input)))
           (tokens    (gitq--tokenize-flat trimmed))
           (partial   (if trailing "" (or (car (last tokens)) "")))
           (beg       (- (point) (length partial)))
           (end       (point))
           (candidates (gitq--complete-candidates input)))
      (when candidates
        (list beg end candidates :exclusive 'no)))))

(defun gitq--read-pipeline (prompt)
  "Read a gitq pipeline from the minibuffer with context-aware completion.
Uses `completing-read' so vertico, corfu, selectrum, and vanilla TAB all work.
The completion table returns full-pipeline strings so the framework's own
filtering logic sees each candidate as an extension of what was typed."
  (completing-read prompt #'gitq--pipeline-completion-table
                   nil nil nil 'gitq--history))

;;;###autoload
(defun gitq-flat (pipeline)
  "Execute a GitQ PIPELINE using flat syntax (whitespace-separated, /terminal).

PIPELINE syntax:  source [step...] [/terminal]

Sources:   commits [in RANGE]  HEAD  BRANCH  branches  tags  refs  worktrees  blobs
Steps:     via MORPHISM  where COND[,COND...]  grep PATTERN  pickaxe PATTERN
           path GLOB  pick .FIELD[,...]  take N  skip N  first  last  sort [-.] FIELD
Terminals: /show  /copy  /insert  /count  /branch-off [NAME]  /amend [no-edit|MSG]
           /squash [MSG]  /reword [MSG]  /remove  /delete  /commit [MSG]
           /stage  /mark [LABEL]

Step keywords are reserved: quote them when used as values.
  CORRECT:  commits where .message contains \"take\" take 5 /show
  WRONG:    commits where .message contains take take 5 /show  (error)

Press TAB for context-aware completion in the minibuffer.
Works with corfu, company, vertico, and vanilla `completion-at-point'.

Examples:
  (gitq-flat \"commits take 10 /show\")
  (gitq-flat \"commits where .author contains \\\"alice\\\" take 5 /count\")
  (gitq-flat \"HEAD via .parent* where .message contains \\\"fix\\\" /show\")
  (gitq-flat \"commits in main..HEAD sort -.date /show\")"
  (interactive (list (gitq--read-pipeline "gitq> ")))
  (let* ((default-directory (gitq--toplevel))
         (nodes    (gitq--parse-flat pipeline))
         (src-node (car nodes))
         (rest     (cdr nodes))
         (last     (car (last rest)))
         (is-term  (and last (eq (plist-get last :type) 'terminal)))
         (steps    (if is-term (butlast rest) rest))
         (terminal (when is-term last)))
    (let* ((frames (gitq--exec-source src-node))
           (result (cl-reduce #'gitq--exec-step steps :initial-value frames)))
      (if terminal
          (gitq--apply-terminal result terminal pipeline)
        (gitq--display result pipeline)))))

(provide 'git-branch-off-gitq)
;;; git-branch-off-gitq.el ends here

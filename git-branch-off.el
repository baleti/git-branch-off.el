;;; git-branch-off.el --- Git branch-off workflow toolkit  -*- lexical-binding: t; -*-

;; Author: baleti
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit "3.3.0"))
;; Keywords: git vc magit
;; URL: https://github.com/baleti/git-branch-off.el

;;; Commentary:

;; A toolkit for the branch-off workflow: stash commits under
;; refs/branch-off/<hash>, manage detached worktrees, and navigate
;; the resulting DAG through magit's log, revision, and blob views.
;;
;; Usage (doom emacs example):
;;
;;   ;; packages.el
;;   (package! git-branch-off
;;     :recipe (:host github :repo "baleti/git-branch-off.el"))
;;
;;   ;; config.el
;;   (use-package! git-branch-off
;;     :after magit
;;     :config
;;     (git-branch-off-setup)
;;     ;; personal leader bindings
;;     (map! :leader
;;           "g c c" #'git-branch-off-stage-and-commit
;;           "g c o" #'git-branch-off-stage-and-commit-branch-off
;;           "g l l" #'git-branch-off-log
;;           (:prefix ("g w" . "worktree")
;;            "c" #'git-branch-off-worktree-create
;;            "d" #'git-branch-off-worktree-delete)
;;           (:prefix ("g a" . "amend hunk")
;;            "a" #'git-branch-off-amend-hunk
;;            "n" #'git-branch-off-amend-hunk-no-edit)
;;           "s g f" #'git-branch-off-search-filename-history
;;           "s g g" #'git-branch-off-search-pickaxe-g
;;           "s g S" #'git-branch-off-search-pickaxe-s
;;           "s g a" #'git-branch-off-search-all-grep))

;;; Code:

(require 'git-branch-off-stage)
(require 'git-branch-off-commit)
(require 'git-branch-off-log)
(require 'git-branch-off-worktree)
(require 'git-branch-off-reword)
(require 'git-branch-off-squash)
(require 'git-branch-off-blob)
(require 'git-branch-off-search)
(require 'git-branch-off-gitq)

(defun git-branch-off--setup-transients ()
  "Add git-branch-off entries to magit's rebase and merge transients."
  (ignore-errors (transient-remove-suffix 'magit-rebase "W"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "K"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "S"))
  (ignore-errors (transient-remove-suffix 'magit-rebase "v"))
  (transient-append-suffix 'magit-rebase '(2)
    ["Branch-off"
     [("W" "reword" git-branch-off-reword)
      ("K" "remove" git-branch-off-remove)
      ("S" "squash" git-branch-off-squash)]
     [("v" "show diff in edit buffer" git-branch-off-squash-verbose)]])
  (ignore-errors (transient-remove-suffix 'magit-merge "M"))
  (transient-append-suffix 'magit-merge '(1)
    ["Branch Off"
     ("M" "toggle marker" git-branch-off-mark)]))

(defun git-branch-off-setup ()
  "Set up git-branch-off hooks, keybindings, and transient extensions.

Call this from your config after magit is loaded, e.g.:
  (use-package git-branch-off :after magit :config (git-branch-off-setup))"
  ;; Hooks
  (add-hook 'magit-log-mode-hook
            (lambda ()
              (setq-local hl-line-sticky-flag t)
              (hl-line-mode 1)))
  (add-hook 'magit-refresh-buffer-hook #'git-branch-off--log-mark)

  ;; Mode-map bindings — these override some magit defaults intentionally.
  ;; Wrapped in with-eval-after-load so the keymaps exist before we touch them.
  (with-eval-after-load 'magit-log
    (define-key magit-log-mode-map (kbd "m") #'git-branch-off-mark)
    (define-key magit-log-mode-map (kbd "M") #'git-branch-off-mark))
  (with-eval-after-load 'magit-status
    (define-key magit-status-mode-map (kbd "TAB") #'git-branch-off-status-tab))
  (with-eval-after-load 'magit-diff
    (define-key magit-revision-mode-map (kbd "TAB") #'magit-diff-visit-file))

  ;; Evil state bindings (override motion/normal state keys in magit maps).
  ;; Use evil-define-key* (function) rather than evil-define-key (macro) so
  ;; these compile correctly without evil on the byte-compiler's load-path.
  (with-eval-after-load 'evil
    (with-eval-after-load 'magit-log
      (evil-define-key* 'normal magit-log-mode-map
        (kbd "TAB") #'magit-visit-thing
        (kbd "m")   #'git-branch-off-mark
        (kbd "M")   #'git-branch-off-mark))
    (with-eval-after-load 'magit-status
      (evil-define-key* 'normal magit-status-mode-map
        (kbd "TAB") #'git-branch-off-status-tab)
      (evil-define-key* 'motion magit-status-mode-map
        (kbd "n") #'git-branch-off-status-next
        (kbd "p") #'git-branch-off-status-prev))
    (with-eval-after-load 'magit-diff
      (evil-define-key* 'normal magit-revision-mode-map
        (kbd "TAB") #'magit-diff-visit-file
        (kbd "n")   #'git-branch-off-revision-next
        (kbd "p")   #'git-branch-off-revision-prev))
    (with-eval-after-load 'magit-blob
      (evil-define-key* 'normal magit-blob-mode-map
        (kbd "n") #'git-branch-off-blob-next
        (kbd "p") #'git-branch-off-blob-prev)))

  ;; Transients
  (with-eval-after-load 'magit
    (git-branch-off--setup-transients)))

(provide 'git-branch-off)
;;; git-branch-off.el ends here


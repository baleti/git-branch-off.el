#!/usr/bin/env bash
# Run the git-branch-off ERT test suite with headless Emacs.
#
# Usage:
#   ./run-tests.sh              # auto-detect Emacs and load paths
#   EMACS=/path/to/emacs ./run-tests.sh
#   ./run-tests.sh --tags integration   # include :tags '(integration) tests only
#   ./run-tests.sh --no-integration     # skip integration tests (faster)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# ── Find Emacs ────────────────────────────────────────────────────────────────
EMACS="${EMACS:-emacs}"
if ! command -v "$EMACS" &>/dev/null; then
  echo "error: emacs not found (set EMACS= to override)" >&2
  exit 1
fi

# ── Build load-path from straight repos if available ──────────────────────────
# Supports both the doom emacs layout (~/.config/emacs) and a plain straight
# bootstrap under ~/.config/emacs or ~/straight.
STRAIGHT_REPOS=""
for candidate in \
    "$HOME/.config/emacs/.local/straight/repos" \
    "$HOME/.emacs.d/.local/straight/repos" \
    "$HOME/.straight/repos"; do
  if [[ -d "$candidate" ]]; then
    STRAIGHT_REPOS="$candidate"
    break
  fi
done

LOAD_PATHS=(-L "$REPO_DIR")
if [[ -n "$STRAIGHT_REPOS" ]]; then
  for pkg in magit magit-section transient with-editor llama cond-let compat seq; do
    if [[ -d "$STRAIGHT_REPOS/$pkg" ]]; then
      LOAD_PATHS+=(-L "$STRAIGHT_REPOS/$pkg/lisp" -L "$STRAIGHT_REPOS/$pkg")
    fi
  done
  for pkg in dash; do
    if [[ -d "$STRAIGHT_REPOS/$pkg" ]]; then
      LOAD_PATHS+=(-L "$STRAIGHT_REPOS/$pkg")
    fi
  done
fi

# ── Parse arguments ───────────────────────────────────────────────────────────
EXTRA_ARGS=()
SKIP_INTEGRATION=0
for arg in "$@"; do
  case "$arg" in
    --no-integration)   SKIP_INTEGRATION=1 ;;
    *)                  EXTRA_ARGS+=("$arg") ;;
  esac
done

# Build ERT selector expression
if [[ $SKIP_INTEGRATION -eq 1 ]]; then
  ERT_SELECTOR='(not (tag integration))'
else
  ERT_SELECTOR='t'
fi

# ── Run ───────────────────────────────────────────────────────────────────────
echo "Running git-branch-off tests..."
echo "  Emacs:     $EMACS"
echo "  Repo:      $REPO_DIR"
echo "  Selector:  $ERT_SELECTOR"
echo ""

"$EMACS" --batch \
  "${LOAD_PATHS[@]}" \
  --eval "(require 'ert)" \
  -l "$REPO_DIR/git-branch-off-stage.el" \
  -l "$SCRIPT_DIR/git-branch-off-test.el" \
  -l "$REPO_DIR/git-branch-off-gitq.el" \
  -l "$SCRIPT_DIR/git-branch-off-gitq-test.el" \
  --eval "(ert-run-tests-batch-and-exit '$ERT_SELECTOR)"

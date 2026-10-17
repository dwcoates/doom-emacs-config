#!/usr/bin/env bash
# run.sh — driver for the manage-agent-repl-runtime skill
# Usage:
#   run.sh --build [--force] [component...]      build what is stale (everything with --force)
#   run.sh --byte-compile                        byte-compile the elisp as a warning gate
#   run.sh --bounce                              rebuild, restart every backend; Emacs stays up
#   run.sh --hard-bounce                         rebuild, restart every backend AND Emacs.app
#   run.sh --deploy [-force]                     rolling deploy through the serving daemon
#   run.sh --hot-reload FILE.el...               load changed elisp into the running Emacs
#   run.sh --call METHOD [-workspace DIR] [JSON] one unary rpc to the serving daemon
#   run.sh --doctor [args...]                    read-only runtime health sweep
#   run.sh --readiness                           source vs deployed vs running report
#   run.sh --logs [args...]                      read the canonical logs
# Exit codes (every verb): 0=done  1=the operation failed or refused (its output says why)
#                          2=usage error

set -uo pipefail

SKILL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
RUNTIME="${MANAGE_RUNTIME_FRONT_DOOR:-$SKILL_DIR/../../bin/agent-repl-runtime}"

die() { echo "▶ ERROR: $*" >&2; exit 2; }
log() { echo "▶ $*"; }

[ -x "$RUNTIME" ] || die "the runtime front door is missing: $RUNTIME"

# run forwards to the front door and folds every failure into exit 1.
run() {
    "$RUNTIME" "$@"
    local code=$?
    [ "$code" -eq 0 ] && return 0
    log "the operation failed (exit $code); see its output above"
    exit 1
}

verb="${1:-}"
[ $# -gt 0 ] && shift

case "$verb" in
    --build)        run build "$@" ;;
    --byte-compile) [ $# -eq 0 ] || die "--byte-compile takes no arguments"; run byte-compile ;;
    --bounce)       [ $# -eq 0 ] || die "--bounce takes no arguments"; run bounce ;;
    --hard-bounce)  [ $# -eq 0 ] || die "--hard-bounce takes no arguments"; run bounce --hard ;;
    --deploy)
        [ $# -eq 0 ] || { [ $# -eq 1 ] && [ "$1" = "-force" ]; } || die "--deploy takes only -force"
        run deploy "$@"
        ;;
    --hot-reload)   [ $# -gt 0 ] || die "--hot-reload needs at least one .el file"; run hot-reload "$@" ;;
    --call)         [ $# -gt 0 ] || die "--call needs a method name"; run call "$@" ;;
    --doctor)       run doctor "$@" ;;
    --readiness)    [ $# -eq 0 ] || die "--readiness takes no arguments"; run readiness ;;
    --logs)         run logs "$@" ;;
    *)
        sed -n '2,/^$/p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//' >&2
        exit 2
        ;;
esac

#!/usr/bin/env bash
# run.sh — the arrange-sidebar skill's driver: reading and changing how the
# agent-repl sidebar's repository grouping is arranged.
#
# Verbs:
#   --read                Print the arrangement as JSON: every repository in
#                         order, its unsectioned families, and its subsections'
#                         families, with the refs a change names.
#   --arrange '<JSON>'    Apply ONE change, written as the change object of an
#                         ArrangeSidebar request (e.g.
#                         '{"createSubsection":{...}}'). Prints the answer as
#                         JSON.
#
# Exit codes:
#   0  printed what the verb answers
#   1  --arrange: the change was refused (the refusal is printed); nothing
#      changed
#   2  script/usage error, or the daemon could not be asked

set -uo pipefail

die() {
  printf 'arrange-sidebar: %s\n' "$*" >&2
  exit 2
}

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
runtime="${ARRANGE_SIDEBAR_RUNTIME:-$here/../../bin/agent-repl-runtime}"
[ -x "$runtime" ] || die "the agent-repl runtime door is missing at $runtime"
command -v jq >/dev/null 2>&1 || die "jq is not on PATH"

case "${1:-}" in
  --read)
    [ -z "${2:-}" ] || die "--read takes no argument, not $2"
    out="$("$runtime" call GetSidebarArrangement '{}' 2>&1)" || die "the arrangement could not be read: $out"
    printf '%s\n' "$out"
    ;;
  --arrange)
    [ -n "${2:-}" ] || die "--arrange needs a change"
    request="$(printf '%s' "$2" | jq -ce '.' 2>/dev/null)" || die "the change is not JSON: $2"
    out="$("$runtime" call ArrangeSidebar "$request" 2>&1)"
    status=$?
    if [ "$status" -eq 0 ]; then
      printf '%s\n' "$out"
      exit 0
    fi
    if printf '%s' "$out" | jq -e '.error' >/dev/null 2>&1; then
      printf '%s\n' "$out"
      exit 1
    fi
    die "the change could not be applied: $out"
    ;;
  *)
    printf "usage: run.sh --read | --arrange '<change JSON>'\n" >&2
    exit 1
    ;;
esac

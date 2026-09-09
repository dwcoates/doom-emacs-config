#!/usr/bin/env bash
# e2e-repeat.sh -- run the in-container Go e2e package N times, one run per
# artifacts directory, and NEVER delete a run's evidence.
#
# WHY THIS EXISTS.
#
# The handover/replay family of e2e failures is load-sensitive: each member
# fails roughly once in nine runs, so diagnosing one means running the package
# many times and reading the logs of the ONE run that went red. A previous
# investigation lost exactly that: it reused a single artifacts directory and
# cleaned it between runs, so the run that finally failed overwrote (and then
# removed) the only copy of its own daemon, shim, store and sidecar logs.
#
# The rule this script encodes is therefore structural rather than advisory:
#
#   * every run gets its OWN artifacts directory, named for the run index, so
#     no run can overwrite another's logs;
#   * nothing here removes anything, ever -- there is no cleanup path to
#     forget to guard. A red run's directory survives the whole session, and
#     the summary names it;
#   * each run's stdout is teed into its own artifacts directory as
#     `go-test.log`, so the transcript and the structured logs of one run are
#     found together.
#
# Usage:
#   bin/e2e-repeat.sh [--runs N] [--root DIR] [--stop-on-fail] [-- <go test args>]
#
# Defaults: 5 runs, root $TMPDIR/agent-repl-e2e-artifacts/<utc stamp>, and the
# package's own default arguments (-count=1 -parallel 8 -v .).
#
# Each run is taken through bin/suite-slot.sh, the host concurrency gate, so a
# repeat driver cannot overcommit the box the way concurrent agents did.
set -uo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
module_root=$(cd -- "$here/.." && pwd)

log() { printf 'e2e-repeat: %s\n' "$*" >&2; }
die() { log "$*"; exit 2; }

runs=5
stop_on_fail=0
root=""
test_args=(-count=1 -parallel 8 -v .)

while (( $# > 0 )); do
  case $1 in
    --runs) runs=${2:-} ; shift 2 || die "--runs needs a value" ;;
    --root) root=${2:-} ; shift 2 || die "--root needs a value" ;;
    --stop-on-fail) stop_on_fail=1 ; shift ;;
    --) shift ; test_args=("$@") ; break ;;
    *) die "unknown argument: $1" ;;
  esac
done

[[ $runs =~ ^[0-9]+$ ]] && (( runs > 0 )) || die "--runs must be a positive integer, got '$runs'"

if [[ -z $root ]]; then
  root="${TMPDIR:-/tmp}/agent-repl-e2e-artifacts/$(date -u +%Y%m%dT%H%M%SZ)"
fi
mkdir -p "$root" || die "cannot create artifacts root $root"
root=$(cd -- "$root" && pwd)

log "artifacts root: $root"
log "runs: $runs; go test args: ${test_args[*]}"

reds=()
for (( run = 1; run <= runs; run++ )); do
  run_dir="$root/run-$(printf '%03d' "$run")"
  mkdir -p "$run_dir" || die "cannot create $run_dir"
  log "run $run/$runs -> $run_dir"
  start=$(date -u +%s)
  AGENT_REPL_E2E_ARTIFACTS="$run_dir" \
    "$module_root/bin/suite-slot.sh" \
    "$module_root/e2e/sandbox/bin/e2e-sandbox.sh" run --dir e2e \
    go test "${test_args[@]}" 2>&1 | tee "$run_dir/go-test.log"
  status=${PIPESTATUS[0]}
  elapsed=$(( $(date -u +%s) - start ))
  printf '%s\n' "$status" > "$run_dir/exit-status"
  if (( status == 0 )); then
    log "run $run: PASS in ${elapsed}s"
  else
    reds+=("$run_dir")
    log "run $run: FAIL (exit $status) in ${elapsed}s -- evidence kept at $run_dir"
    grep -E '^\s*--- FAIL' "$run_dir/go-test.log" | sed 's/^/e2e-repeat:   /' >&2
    (( stop_on_fail == 1 )) && break
  fi
done

log "----"
if (( ${#reds[@]} == 0 )); then
  log "all runs passed; artifacts under $root"
  exit 0
fi
log "${#reds[@]} red run(s):"
for d in "${reds[@]}"; do log "  $d"; done
exit 1

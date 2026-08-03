#!/usr/bin/env bash
#
# test-all.sh — run every agent-repl test and coverage suite from one command.
#
# Usage:
#   bin/test-all.sh
#   bin/test-all.sh --record
#
# The default run is read-only with respect to the canonical timing history.
# --record atomically appends one successful timing row per suite to
# ../test_time.csv, then compares the run with recent entries on the same
# branch. Failed runs never update the timing history.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="$(cd "$THIS_DIR/.." && pwd)"
REPO_ROOT="$(cd "$MODULE_ROOT/../../.." && pwd)"
TIMING_CSV="$MODULE_ROOT/test_time.csv"
CSV_HEADER="run_id,recorded_at_utc,commit,branch,suite,duration_seconds"
RECORD=0
RUN_TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-test-all.XXXXXX")"
CSV_TMP=""
CSV_LOCK=""
SUITES=()
DURATIONS=()

cleanup() {
    [ -z "$CSV_TMP" ] || rm -f "$CSV_TMP"
    [ -z "$CSV_LOCK" ] || rmdir "$CSV_LOCK" 2>/dev/null || true
    rm -rf "$RUN_TMP"
}
trap cleanup EXIT

log() {
    printf '[agent-repl-tests] %s\n' "$*"
}

die() {
    printf '[agent-repl-tests] ERROR: %s\n' "$*" >&2
    exit 1
}

case "$#" in
    0) ;;
    1)
        [ "$1" = "--record" ] ||
            die "unknown argument '$1', expected --record"
        RECORD=1
        ;;
    *) die "expected no arguments or exactly --record" ;;
esac

validate_timing_csv() {
    local header

    [ -f "$TIMING_CSV" ] ||
        die "canonical timing file is missing: $TIMING_CSV"
    IFS= read -r header <"$TIMING_CSV" ||
        die "canonical timing file is unreadable: $TIMING_CSV"
    [ "$header" = "$CSV_HEADER" ] ||
        die "canonical timing header is invalid: $header"
}

require_executable() {
    [ -x "$1" ] || die "required test runner is not executable: $1"
}

run_timed() {
    local suite="$1"
    local timing_file="$RUN_TMP/$suite.time"
    local duration rc
    shift

    log "$suite: starting"
    TIMEFORMAT='%3R'
    set +e
    { time "$@" 2>&3; } 3>&2 2>"$timing_file"
    rc=$?
    set -e

    IFS= read -r duration <"$timing_file" ||
        die "$suite timing output is missing"
    [[ "$duration" =~ ^[0-9]+([.][0-9]+)?$ ]] ||
        die "$suite timing output is malformed: $duration"
    [ "$rc" -eq 0 ] ||
        die "$suite failed after ${duration}s with exit code $rc"

    SUITES+=("$suite")
    DURATIONS+=("$duration")
    log "$suite: passed in ${duration}s"
}

validate_csv_field() {
    local name="$1" value="$2"
    case "$value" in
        *','*|*$'\n'*) die "$name is not CSV-safe: $value" ;;
        '') die "$name is empty" ;;
    esac
}

report_regressions() {
    local run_id="$1" branch="$2"

    awk -F, -v current_run="$run_id" -v current_branch="$branch" '
        NR == 1 { next }
        $4 == current_branch && $1 != current_run {
            suite = $5
            slot = prior_count[suite] % 5
            prior[suite, slot] = $6 + 0
            prior_count[suite]++
        }
        $1 == current_run {
            current[$5] = $6 + 0
        }
        END {
            regressions = 0
            for (suite in current) {
                count = prior_count[suite]
                recent = count < 5 ? count : 5
                if (recent < 3) {
                    printf "[agent-repl-tests] %s: only %d prior %s timing entries, regression baseline needs 3\n",
                        suite, recent, current_branch
                    continue
                }
                total = 0
                for (i = 0; i < recent; i++) {
                    total += prior[suite, i]
                }
                average = total / recent
                delta = current[suite] - average
                percent = average > 0 ? sprintf("%.1f%%", (delta / average) * 100) : "infinite"
                if (delta >= 1.0 && current[suite] >= average * 1.25) {
                    printf "[agent-repl-tests] TIMING REGRESSION: %s %.3fs vs %.3fs recent average (+%s, +%.3fs)\n",
                        suite, current[suite], average, percent, delta
                    regressions++
                }
            }
            if (regressions == 0) {
                print "[agent-repl-tests] no big timing regressions detected"
            }
        }
    ' "$TIMING_CSV"
}

record_timings() {
    local start_branch="$1" start_commit="$2"
    local end_branch end_commit recorded_at run_id index suite duration

    end_branch="$(git -C "$REPO_ROOT" branch --show-current)" ||
        die "could not resolve the final git branch"
    end_commit="$(git -C "$REPO_ROOT" rev-parse HEAD)" ||
        die "could not resolve the final git commit"
    [ "$end_branch" = "$start_branch" ] ||
        die "git branch changed during tests: $start_branch -> $end_branch"
    [ "$end_commit" = "$start_commit" ] ||
        die "git commit changed during tests: $start_commit -> $end_commit"

    recorded_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    run_id="$(date -u +%Y%m%dT%H%M%SZ)-${start_commit:0:12}-$$"
    validate_csv_field "run id" "$run_id"
    validate_csv_field "recorded timestamp" "$recorded_at"
    validate_csv_field "commit" "$start_commit"
    validate_csv_field "branch" "$start_branch"

    CSV_LOCK="$TIMING_CSV.lock"
    mkdir "$CSV_LOCK" ||
        die "another timing writer holds $CSV_LOCK"
    CSV_TMP="$(mktemp "$MODULE_ROOT/.test_time.csv.XXXXXX")"
    cp "$TIMING_CSV" "$CSV_TMP"

    for index in "${!SUITES[@]}"; do
        suite="${SUITES[$index]}"
        duration="${DURATIONS[$index]}"
        validate_csv_field "suite" "$suite"
        printf '%s,%s,%s,%s,%s,%s\n' \
            "$run_id" "$recorded_at" "$start_commit" "$start_branch" \
            "$suite" "$duration" >>"$CSV_TMP"
    done

    mv "$CSV_TMP" "$TIMING_CSV"
    CSV_TMP=""
    rmdir "$CSV_LOCK"
    CSV_LOCK=""
    log "recorded ${#SUITES[@]} suite timings in $TIMING_CSV"
    report_regressions "$run_id" "$start_branch"
}

print_timing_summary() {
    local summary_file="$RUN_TMP/timing-summary.csv"
    local index suite duration

    for index in "${!SUITES[@]}"; do
        printf '%s,%s\n' "${SUITES[$index]}" "${DURATIONS[$index]}" \
            >>"$summary_file"
    done

    log "timing summary, slowest suite first"
    while IFS=, read -r suite duration; do
        log "timing: $suite ${duration}s"
    done < <(sort -t, -k2,2nr "$summary_file")
}

validate_timing_csv
require_executable "$THIS_DIR/test-test-all.sh"
require_executable "$THIS_DIR/test-report-nonlisp-coverage.sh"
require_executable "$THIS_DIR/test-report-logging-density.sh"
require_executable "$THIS_DIR/test-build-frontend.sh"
require_executable "$THIS_DIR/test-deploy-all.sh"
require_executable "$THIS_DIR/test-readiness-report.sh"
require_executable "$REPO_ROOT/modules/app/agent-repl/scripts/test-agent-shim-doctor.sh"
require_executable "$REPO_ROOT/.githooks/test-pre-commit.sh"
require_executable "$THIS_DIR/report-nonlisp-coverage.sh"
require_executable "$THIS_DIR/report-logging-density.sh"
require_executable "$REPO_ROOT/.claude/safe-test-run.sh"

START_BRANCH=""
START_COMMIT=""
if [ "$RECORD" -eq 1 ]; then
    START_BRANCH="$(git -C "$REPO_ROOT" branch --show-current)" ||
        die "could not resolve the initial git branch"
    START_COMMIT="$(git -C "$REPO_ROOT" rev-parse HEAD)" ||
        die "could not resolve the initial git commit"
    validate_csv_field "branch" "$START_BRANCH"
    validate_csv_field "commit" "$START_COMMIT"
fi

run_timed orchestrator-harness "$THIS_DIR/test-test-all.sh"
run_timed coverage-harness "$THIS_DIR/test-report-nonlisp-coverage.sh"
run_timed logging-density-harness "$THIS_DIR/test-report-logging-density.sh"
run_timed build-frontend-harness "$THIS_DIR/test-build-frontend.sh"
run_timed deploy-harness "$THIS_DIR/test-deploy-all.sh"
run_timed readiness-harness "$THIS_DIR/test-readiness-report.sh"
run_timed doctor-harness "$REPO_ROOT/modules/app/agent-repl/scripts/test-agent-shim-doctor.sh"
run_timed precommit-harness "$REPO_ROOT/.githooks/test-pre-commit.sh"
run_timed ert "$REPO_ROOT/.claude/safe-test-run.sh"

for component in daemon sidecar store wire logging webapp shim proto; do
    run_timed "$component" \
        "$THIS_DIR/report-nonlisp-coverage.sh" "$component"
done

run_timed logging-density "$THIS_DIR/report-logging-density.sh"

print_timing_summary

if [ "$RECORD" -eq 1 ]; then
    record_timings "$START_BRANCH" "$START_COMMIT"
else
    log "timings were not recorded, pass --record only for a canonical history run"
fi

log "all agent-repl tests and coverage suites passed"

#!/usr/bin/env bash

# Shared --list/--only protocol for splittable shell harnesses. A harness calls
# test_split_init before fixture setup, then test_split_run for each named group.
#
# Each group run prints `TESTRUN-ITEM <group> <seconds>`, the line the test
# runner (testrun) reads into the host's timing history. The clock is bash's
# own EPOCHREALTIME, so timing a group spawns no process.

# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-grep-in.sh"

test_split_init() {
    local script="$1" listed requested; shift
    # EPOCHREALTIME is bash 5's; an older bash expands it to nothing, which
    # would time every group as zero. Refuse rather than report a lie.
    [ -n "${EPOCHREALTIME:-}" ] || { echo "$script: lib-test-split.sh needs bash 5 or later (EPOCHREALTIME), running ${BASH_VERSION}" >&2; exit 2; }
    listed="$(awk '$1 == "test_split_run" { print $2 }' "$script")"
    [ -n "$listed" ] || { echo "$script: no test_split_run items" >&2; exit 2; }
    TEST_SPLIT_ONLY=""
    case "${1:-}" in
        --list)
            printf '%s\n' "$listed"
            exit 0
            ;;
        --only)
            [ "$#" -eq 2 ] || { echo "--only needs one comma-separated test list" >&2; exit 2; }
            [ -n "$2" ] || { echo "--only needs a nonempty test list" >&2; exit 2; }
            while IFS= read -r requested; do
                grep_in "$listed" -Fxq "$requested" || {
                    echo "unknown harness item: $requested" >&2
                    # WHAT THE SCRIPT LISTED, AND WHICH BYTES IT WAS READ
                    # FROM. A runner plans its chunks from an earlier --list
                    # of this same file, so an item it planned and this read
                    # does not list means the file changed between the two
                    # (seen once, 2026-10-03: build-frontend-harness#00 refused
                    # revision-staleness in 0.03s, not reproduced in 25,000
                    # runs). The listing and the content checksum say whether
                    # the script was rewritten under the run.
                    echo "$script lists: $(printf '%s\n' "$listed" | paste -sd, -); read as cksum $(cksum <"$script")" >&2
                    exit 2
                }
            done < <(printf '%s\n' "$2" | tr ',' '\n')
            TEST_SPLIT_ONLY=",$2,"
            ;;
        "") ;;
        *) echo "unknown harness argument: $1" >&2; exit 2 ;;
    esac
}

test_split_run() {
    local name="$1" fn="$2" start end us
    shift 2
    case "$TEST_SPLIT_ONLY" in
        ""|*",$name,"*) ;;
        *) return 0 ;;
    esac
    # EPOCHREALTIME is seconds.microseconds with the locale's radix; dropping
    # every non-digit leaves whole microseconds.
    start="${EPOCHREALTIME//[!0-9]/}"
    "$fn" "$@"
    end="${EPOCHREALTIME//[!0-9]/}"
    us=$((10#$end - 10#$start))
    printf 'TESTRUN-ITEM %s %d.%06d\n' "$name" "$((us / 1000000))" "$((us % 1000000))"
}

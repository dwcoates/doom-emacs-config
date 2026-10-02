#!/usr/bin/env bash

# Shared --list/--only protocol for splittable shell harnesses. A harness calls
# test_split_init before fixture setup, then test_split_run for each named group.

test_split_init() {
    local script="$1" listed requested; shift
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
                printf '%s\n' "$listed" | grep -Fxq "$requested" || {
                    echo "unknown harness item: $requested" >&2
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
    local name="$1" fn="$2" start end elapsed
    shift 2
    case "$TEST_SPLIT_ONLY" in
        ""|*",$name,"*) ;;
        *) return 0 ;;
    esac
    start="$(python3 -c 'import time; print(time.time_ns())')"
    "$fn" "$@"
    end="$(python3 -c 'import time; print(time.time_ns())')"
    elapsed="$(python3 -c 'import sys; print(f"{(int(sys.argv[2])-int(sys.argv[1]))/1e9:.6f}")' "$start" "$end")"
    printf 'TESTRUN-ITEM %s %s\n' "$name" "$elapsed"
}

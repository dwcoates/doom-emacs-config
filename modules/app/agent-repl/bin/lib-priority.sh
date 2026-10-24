#!/usr/bin/env bash
# lib-priority.sh -- run a command, and its whole process tree, at this host's
# background priority. Sourced by bin/background.sh (every test run, which it
# also marks) and bin/low-priority.sh (the live-editor realtests, unmarked).
# Why each mechanism is what it is: bin/background.sh's header.

priority_log() { printf 'background: %s\n' "$*" >&2; }

# EX_CONFIG: this host cannot run a test at background priority. Distinct from
# a failing suite (1), from a usage error (2) and from test-all's
# precondition decline (77), which would read as "skipped" rather than "wrong".
readonly PRIORITY_EXIT_REFUSED=78

priority_refuse() {
    priority_log "REFUSING TO RUN: $*"
    priority_log "  tests only ever run at background priority; see bin/background.sh"
    exit "$PRIORITY_EXIT_REFUSED"
}

# priority_read_niceness OS -- this process's niceness. macOS's nice(1) cannot
# print it (it demands a utility), so there it is getpriority(PRIO_PROCESS,
# self) from perl, which ships with macOS; the perl child inherits this
# process's value. Linux's coreutils nice prints it, and the sandbox image may
# lack perl.
priority_read_niceness() {
    case $1 in
        Darwin) perl -e 'print getpriority(0, 0)' ;;
        Linux) nice ;;
    esac
}

# priority_exec MARK COMMAND... -- exec COMMAND at the target niceness. MARK
# is `marker` to export AGENT_REPL_BACKGROUND_PRIORITY (a test run, which the
# unwrappable runners wait for) or `unmarked` to export nothing.
priority_exec() {
    local mark=$1
    shift
    local os niceness target increment
    os=$(uname -s) || priority_refuse "could not read the platform with uname"

    # THE TARGET NICENESS: 19, unless AGENT_REPL_TEST_NICENESS names another
    # from 1 to 19. Only the merge queue's gates name one (owner ruling
    # 2026-10-14): the queue's head runs at 10 and the merge behind it at 15,
    # so under contention the head's tests run before any other test run's,
    # and every one of them still yields to the owner's processes at 0.
    # Nothing here ever raises a priority: a run already at or past its target
    # runs as it is.
    target=${AGENT_REPL_TEST_NICENESS:-19}
    if ! [[ $target =~ ^[0-9]+$ ]] || (( target < 1 || target > 19 )); then
        priority_refuse "AGENT_REPL_TEST_NICENESS='$target' is not a niceness from 1 to 19"
    fi

    case $os in
        Darwin|Linux)
            niceness=$(priority_read_niceness "$os") || priority_refuse "could not read this process's niceness"
            [[ $niceness =~ ^-?[0-9]+$ ]] || priority_refuse "the niceness read back is not a number: '$niceness'"
            if (( niceness >= target )); then
                [[ $mark == marker ]] && export AGENT_REPL_BACKGROUND_PRIORITY=nice-$niceness
                exec "$@"
            fi
            [[ $mark == marker ]] && export AGENT_REPL_BACKGROUND_PRIORITY=nice-$target
            # TO EXACTLY THE TARGET, FROM WHEREVER IT STANDS. nice(1) adds its
            # increment to the current niceness, so `nice -n 19` from any
            # niceness above 0 overshoots to 20 (macOS's ceiling); the
            # increment is the distance to the target instead.
            increment=$(( target - niceness ))
            # DISK, TOO, ON macOS (owner ruling 2026-10-06): the CPU demotion
            # left a run's disk I/O at normal priority, and a full run's
            # hundreds of SQLite files starved the live store (a 111-page
            # checkpoint took 54 s, its writes 5-7 s). `taskpolicy -d throttle`
            # sets the I/O policy ONLY -- not `-b`, which also pins to the
            # efficiency cores and was rejected 2026-09-23 -- and children
            # inherit it with the niceness.
            if [[ $os == Darwin ]]; then
                exec taskpolicy -d throttle nice -n "$increment" "$@"
            fi
            exec nice -n "$increment" "$@"
            ;;
        *)
            priority_refuse "no background-priority mechanism is known for platform '$os'"
            ;;
    esac
}

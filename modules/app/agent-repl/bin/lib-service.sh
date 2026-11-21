#!/usr/bin/env bash

# lib-service.sh — the ONE driver of agent-repl's services under the platform's
# service manager: launchd's user domain on macOS, `systemd --user' on Linux.
#
# Every script that asks whether a service runs, stops, starts or (re)loads one
# does it through these functions, so a script never speaks launchctl or
# systemctl itself and every one of them works on both platforms
# (test-lib-service.sh fails a script that calls either tool directly). The
# daemon's own driver is daemon/internal/deploy's ServiceManager.
#
# A sourcing script sets SERVICE_PLATFORM (darwin or linux) before calling any
# function, from its own reading of the platform or from service_host_platform.
# Honored environment: AGENT_REPL_SERVICE_PLATFORM (service_host_platform's
# answer, for a harness), AGENT_REPL_LAUNCHCTL and AGENT_REPL_SYSTEMCTL (the tool
# driven, default the one on PATH), AGENT_REPL_LAUNCH_AGENTS_DIR and
# AGENT_REPL_SYSTEMD_USER_DIR (where definitions are installed).
#
# Each function answers the tool's own status: a caller decides whether a
# non-zero answer is its own error to report. A function of several steps
# (service_load, service_unload) leaves the step that failed in
# SERVICE_FAILED_STEP, spelled as the tool is ("systemctl --user enable ..."),
# for the caller's report to name.

SERVICE_LAUNCHCTL="${AGENT_REPL_LAUNCHCTL:-launchctl}"
SERVICE_SYSTEMCTL="${AGENT_REPL_SYSTEMCTL:-systemctl}"
SERVICE_LAUNCH_AGENTS_DIR="${AGENT_REPL_LAUNCH_AGENTS_DIR:-$HOME/Library/LaunchAgents}"
# SERVICE_FAILED_STEP is read by the sourcing script's report.
# shellcheck disable=SC2034
SERVICE_FAILED_STEP=""
SERVICE_SYSTEMD_USER_DIR="${AGENT_REPL_SYSTEMD_USER_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user}"

# service_host_platform: this host's platform, darwin or linux, or
# AGENT_REPL_SERVICE_PLATFORM when a hermetic harness names one; any other
# host is refused (non-zero, said on stderr).
service_host_platform() {
    if [ -n "${AGENT_REPL_SERVICE_PLATFORM:-}" ]; then
        printf '%s\n' "$AGENT_REPL_SERVICE_PLATFORM"
        return
    fi
    case "$(uname -s)" in
        Darwin) echo darwin ;;
        Linux) echo linux ;;
        *) echo "lib-service: $(uname -s) has no service manager agent-repl drives" >&2; return 1 ;;
    esac
}

# service_platform_set: refuse a call made before SERVICE_PLATFORM is set, or
# with a platform this library does not drive. An unset platform is a bug in
# the sourcing script, never a default to guess.
service_platform_set() {
    case "${SERVICE_PLATFORM:-}" in
        darwin|linux) return 0 ;;
        *) echo "lib-service: SERVICE_PLATFORM is '${SERVICE_PLATFORM:-}', not darwin or linux" >&2; return 2 ;;
    esac
}

# service_definition_path LABEL: where LABEL's definition is installed: its
# launchd plist, or its systemd user unit.
service_definition_path() {
    service_platform_set || return
    if [ "$SERVICE_PLATFORM" = darwin ]; then
        printf '%s/%s.plist\n' "$SERVICE_LAUNCH_AGENTS_DIR" "$1"
    else
        printf '%s/%s.service\n' "$SERVICE_SYSTEMD_USER_DIR" "$1"
    fi
}

# service_held LABEL: 0 while the service manager holds LABEL: loaded in
# launchd's user domain (which keeps it alive), or active under systemd.
service_held() {
    service_platform_set || return
    if [ "$SERVICE_PLATFORM" = darwin ]; then
        "$SERVICE_LAUNCHCTL" print "gui/$(id -u)/$1" >/dev/null 2>&1
    else
        "$SERVICE_SYSTEMCTL" --user is-active --quiet "$1.service" >/dev/null 2>&1
    fi
}

# service_pid LABEL: LABEL's running pid, or nothing when none runs. NOT
# RUNNING IS AN ANSWER, NOT A FAILURE: launchctl exits non-zero for a label it
# does not know, so its status is discarded and the absence of a pid is the
# whole signal. The output is read whole, never piped into a reader that exits
# at its match: under pipefail the writer then dies of SIGPIPE (2026-10-06).
service_pid() {
    service_platform_set || return
    local out pid
    if [ "$SERVICE_PLATFORM" = darwin ]; then
        out="$("$SERVICE_LAUNCHCTL" print "gui/$(id -u)/$1" 2>/dev/null || true)"
        pid="$(awk '/^[[:space:]]*pid = / && !found { gsub(/[^0-9]/, "", $3); print $3; found = 1 }' <<<"$out")"
    else
        pid="$("$SERVICE_SYSTEMCTL" --user show --property MainPID --value "$1.service" 2>/dev/null || true)"
        # systemd names a unit with no running process MainPID 0.
        [ "$pid" = 0 ] && pid=""
    fi
    printf '%s' "$pid"
}

# service_stop LABEL: stop LABEL so that it stays stopped until service_start.
# A launchd service is kept alive, so it only stops by leaving the domain: a
# kill is answered by a new pid at once, so the stop is `bootout'. systemd's
# stop is the unit's own, and it holds until a start.
service_stop() {
    service_platform_set || return
    if [ "$SERVICE_PLATFORM" = darwin ]; then
        "$SERVICE_LAUNCHCTL" bootout "gui/$(id -u)/$1" >/dev/null 2>&1
    else
        "$SERVICE_SYSTEMCTL" --user stop "$1.service" >/dev/null 2>&1
    fi
}

# service_start LABEL: start LABEL from its installed definition. A booted-out
# launchd service comes back by bootstrapping its plist (which runs it: every
# plist sets RunAtLoad).
service_start() {
    service_platform_set || return
    if [ "$SERVICE_PLATFORM" = darwin ]; then
        "$SERVICE_LAUNCHCTL" bootstrap "gui/$(id -u)" "$(service_definition_path "$1")" >/dev/null
    else
        "$SERVICE_SYSTEMCTL" --user start "$1.service" >/dev/null
    fi
}

# service_restart LABEL: restart a held LABEL in place onto what its program
# now is: launchd's `kickstart -k', systemd's restart. A service the manager
# does not hold is refused by the tool, and the caller decides whether that
# is its error.
service_restart() {
    service_platform_set || return
    if [ "$SERVICE_PLATFORM" = darwin ]; then
        "$SERVICE_LAUNCHCTL" kickstart -k "gui/$(id -u)/$1" >/dev/null
    else
        "$SERVICE_SYSTEMCTL" --user restart "$1.service" >/dev/null
    fi
}

# service_step TOOL-NAME TOOL ARGS...: run one step, and on failure leave it,
# spelled with TOOL-NAME, in SERVICE_FAILED_STEP.
service_step() {
    local name="$1" tool="$2"
    shift 2
    if ! "$tool" "$@"; then
        # shellcheck disable=SC2034 # read by the sourcing script
        SERVICE_FAILED_STEP="$name $*"
        return 1
    fi
}

# service_load LABEL: have the service manager read LABEL's definition just
# written, keep it at every login, and run it now from that definition,
# whether or not it was running.
service_load() {
    service_platform_set || return
    local definition uid
    definition="$(service_definition_path "$1")"
    if [ "$SERVICE_PLATFORM" = darwin ]; then
        uid="$(id -u)"
        if service_held "$1"; then
            service_step launchctl "$SERVICE_LAUNCHCTL" bootout "gui/$uid" "$definition" || return
        fi
        # THE BOOTSTRAP STARTS IT, and nothing follows: every service is
        # RunAtLoad. A `kickstart -k` here killed the instance the bootstrap
        # had just started, and launchd's ThrottleInterval held its respawn
        # for 10s, so every reload was a 10s outage (2026-11-18: the store
        # gone from 09:46:57 to 09:47:07; reproduced with a test job).
        service_step launchctl "$SERVICE_LAUNCHCTL" bootstrap "gui/$uid" "$definition"
    else
        service_step systemctl "$SERVICE_SYSTEMCTL" --user daemon-reload || return
        service_step systemctl "$SERVICE_SYSTEMCTL" --user enable "$1.service" || return
        service_step systemctl "$SERVICE_SYSTEMCTL" --user restart "$1.service"
    fi
}

# service_unload LABEL: stop LABEL and have the service manager forget it, so
# its definition can be removed. A service already unloaded is no failure.
service_unload() {
    service_platform_set || return
    if [ "$SERVICE_PLATFORM" = darwin ]; then
        if service_held "$1"; then
            service_step launchctl "$SERVICE_LAUNCHCTL" bootout "gui/$(id -u)" "$(service_definition_path "$1")"
        fi
    else
        service_step systemctl "$SERVICE_SYSTEMCTL" --user disable --now "$1.service"
    fi
}

# service_forget: have the service manager drop every definition removed
# since it last read them (systemd's daemon-reload; launchd reads none).
service_forget() {
    service_platform_set || return
    if [ "$SERVICE_PLATFORM" = linux ]; then
        service_step systemctl "$SERVICE_SYSTEMCTL" --user daemon-reload
    fi
}

#!/bin/bash
#
# requirements.sh — check and install everything agent-repl needs on a machine,
# as the requirement manifests declare it.
#
# Usage:
#   requirements.sh check     [--profile run|dev|all] [--json]
#   requirements.sh install   [--profile run|dev|all]
#   requirements.sh uninstall
#   requirements.sh list      [--profile run|dev|all]
#   requirements.sh purge-legacy-hooks
#   requirements.sh define-service LABEL
#   requirements.sh ensure-holder ID
#
# THE MANIFESTS ARE THE ONE RECORD. `requirements.json` at the module root
# names agent-repl's own requirements and `includes` each system's manifest
# (daemon/, webapp/, agent-shim/..., proto/, e2e/), which sits beside the code
# that needs it. Nothing here names a requirement: this script only knows the
# KINDS an entry may be, and checks and installs each kind one way. A new
# dependency is a manifest entry, never an edit here. The Requirements section
# of AGENTS.md is the contract for writing entries.
#
# Kinds:
#   program                  a command on PATH, optionally at a minimum version
#   emacs                    the emacs on PATH at a minimum version, with features;
#                            with `revision`, built from that commit of a source
#                            repository (`install.source`, with one build block
#                            per platform) with its configuration features
#                            (`configuration_features`, and the platform block's)
#   env                      an environment variable the user sets
#   group                    the user belongs to a group (`name`), which only an
#                            administrator grants
#   notification_permission  macOS lets a banner program notify
#   app_build                a macOS app built from the module (`build.module`, a
#                            script taking the app's path, which answers its
#                            sources' hash under `--source-hash`) into the cache
#                            (`app`), rebuilt only when that hash changed
#   location_permission      macOS lets a helper app (`app`, in the cache) name
#                            Wi-Fi networks: Location Services allowed
#   signing_identity         a self-signed code-signing identity (`name`) in the
#                            login keychain, made once, that a helper app is
#                            signed with so a rebuild keeps what macOS allowed it
#   sudo_grant               sudo runs each of a command's `invocations` without a
#                            password; the user installs the missing grant
#                            (`file`, under /etc/sudoers.d) with the printed
#                            command, since only they can give sudo a password
#   skill_link               a Claude Code skill linked into the skills directory of
#                            every Claude config root agent-repl runs sessions under
#                            (`"retired": true`: our link is removed instead)
#   command_link             a command of the repository linked into the user's
#                            bin dir (AGENT_REPL_BIN_DIR, ~/.local/bin by
#                            default), which must be on PATH
#   git_hook                 a managed git hook in the repository's hooks dir
#   service                  a service the platform's service manager keeps
#                            running (launchd on macOS, systemd --user on
#                            Linux), rendered from the entry: its program built
#                            here (`build_target` + `binary`) or an installed
#                            command (`command`), its `args`, `env` and `log`
#                            files, with any `files` copied from the module into
#                            the cache
#   socket_holder            listening sockets (`sockets`, each a `unix` path or a
#                            `tcp` host) the platform's service manager creates
#                            and keeps for a sockethold program (`label`), which
#                            hands each to the server that asks on its `control`
#                            socket. Its definition is rendered by its own
#                            template and never changes with the services'
#                            (docs/investigations/2026-11-18-stable-sockets-design.md)
#   download                 files unpacked from a zip at a URL into the cache,
#                            each pinned by its sha256
#
# Profiles: `run` entries are needed to use agent-repl, `dev` entries to build
# and test it. `--profile all` (the default) selects both.
#
# Statuses: ok, missing and outdated (install acts on these), needs-user (only
# the user can supply it: a value, or a file of theirs in the way, which is
# never overwritten), failed (a broken entry: fix the manifest). An entry marked `optional` never fails
# the run; it is reported so it is never silent.
#
# Exit: 0 when every selected required entry is ok, 1 when one is not, 2 on a
# usage error or a manifest that breaks the contract.
#
# Runs on the bash every Mac ships (3.2) and reads the manifests with jq, which
# macOS 15 and later ship at /usr/bin/jq: a fresh machine can run it before
# anything is installed.
#
# PACKAGES COME FROM THE PLATFORM'S OWN PACKAGE MANAGER: Homebrew on macOS, and
# on Linux the distribution's, the first of apt, dnf, pacman and zypper on PATH
# (package_manager). An entry names its package for each under `install`
# (`brew`, `apt`, `dnf`, `pacman`, `zypper`). A program no Linux package manager
# carries everywhere names a pinned `release` instead: an archive its
# publisher ships per machine, checked against its sha256 and installed into
# the user's bin dir. A Linux install runs as root
# through sudo, which asks for a password only when it has a terminal to ask
# on; without one it never prompts (`sudo -n`), and an install it cannot run
# says so with the command for the user to run.
#
# Test seams (bin/test-requirements.sh): AGENT_REPL_REQUIREMENTS_ROOT (the
# module dir), AGENT_REPL_REQUIREMENTS_PLATFORM, AGENT_REPL_REQUIREMENTS_ARCH,
# AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER, AGENT_REPL_REQUIREMENTS_BUILD
# (bin/build-frontend.sh), AGENT_REPL_LAUNCHCTL, AGENT_REPL_SYSTEMCTL,
# AGENT_REPL_LAUNCH_AGENTS_DIR and AGENT_REPL_SYSTEMD_USER_DIR (lib-service.sh),
# and HOME / PATH.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="${AGENT_REPL_REQUIREMENTS_ROOT:-$(cd "$THIS_DIR/.." && pwd -P)}"
ROOT_MANIFEST="$MODULE_DIR/requirements.json"
BUILD_FRONTEND="${AGENT_REPL_REQUIREMENTS_BUILD:-$MODULE_DIR/bin/build-frontend.sh}"
CACHE_ROOT="${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl"
# DEFAULT_CONFIG_ROOT is the Claude config root every session runs under unless
# its workspace is routed to the work account's (claude_config_roots).
DEFAULT_CONFIG_ROOT="$HOME/.claude"
# BIN_DIR is where a command_link puts its command: the user's own bin dir.
BIN_DIR="${AGENT_REPL_BIN_DIR:-$HOME/.local/bin}"

# claude_config_roots: every Claude config root a session runs under, one per
# line. A session runs under ~/.claude, or, for a workspace under
# MULTI_REPO_ROOT, under the work account's config root MULTI_REPO_CONFIG_DIR
# (the same value Emacs passes the daemon as --multi-repo-config-dir). Skills
# are linked into each root, and the sidecar watches each one
# (`{claude-config-roots}`, expand_placeholders).
claude_config_roots() {
  printf '%s\n' "$DEFAULT_CONFIG_ROOT"
  if [ -n "${MULTI_REPO_CONFIG_DIR:-}" ]; then
    printf '%s\n' "$MULTI_REPO_CONFIG_DIR"
  fi
}

# skills_dirs: every skills directory a skill link is kept in, one per line:
# each config root's. A skill linked into the first alone is missing from
# every session of the second.
skills_dirs() {
  local root
  while IFS= read -r root; do
    printf '%s/skills\n' "$root"
  done < <(claude_config_roots)
}

# ---------------------------------------------------------------------------
# Reporting. THE ONE OUTPUT PATH: every line this script prints goes through
# say (progress, to stdout) or err (an error, to stderr, always naming the
# operation and the entry), so the run's record reads from one place.
# ---------------------------------------------------------------------------

say() { printf '[requirements] %s\n' "$*"; }
err() { printf '[requirements] ERROR: %s\n' "$*" >&2; }

usage() {
  sed -n '5,13p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

die_usage() {
  err "$1"
  usage >&2
  exit 2
}

# ---------------------------------------------------------------------------
# Platform and repository.
# ---------------------------------------------------------------------------

platform() {
  if [ -n "${AGENT_REPL_REQUIREMENTS_PLATFORM:-}" ]; then
    printf '%s\n' "$AGENT_REPL_REQUIREMENTS_PLATFORM"
    return
  fi
  case "$(uname -s)" in
    Darwin) echo darwin ;;
    Linux) echo linux ;;
    *) err "platform: $(uname -s) is not a platform the manifests describe"; exit 2 ;;
  esac
}

# machine: this machine as a release names it, `<platform>-<arch>` with the
# arch spelled the way Go and most publishers do (arm64, amd64).
machine() {
  local arch="${AGENT_REPL_REQUIREMENTS_ARCH:-$(uname -m)}"
  case "$arch" in
    aarch64) arch=arm64 ;;
    x86_64) arch=amd64 ;;
  esac
  printf '%s-%s\n' "$(platform)" "$arch"
}

# find_program CMD: the path CMD runs from: PATH's, else the user's bin dir
# (BIN_DIR), where a source-built Emacs and a pinned release are installed.
# Whether that dir is on the user's PATH is one entry's to say (the
# command_link kind), not every program's.
find_program() { PATH="$PATH:$BIN_DIR" command -v "$1" 2>/dev/null; }

# The Linux package managers install knows, in the order package_manager
# prefers them.
LINUX_PACKAGE_MANAGERS="apt dnf pacman zypper"

# PACKAGE_MANAGER is the package manager an entry's packages come from, resolved
# once per run by resolve_package_manager, outside any subshell so a refused
# override ends the run: brew on macOS; on Linux
# AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER when it is set (one of
# LINUX_PACKAGE_MANAGERS, else a usage error), otherwise the first of them
# whose command is on PATH. Empty when a Linux machine has none of them.
PACKAGE_MANAGER=""

resolve_package_manager() {
  if [ "$(platform)" = darwin ]; then
    PACKAGE_MANAGER=brew
    return
  fi
  local pm
  if [ -n "${AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER:-}" ]; then
    case " $LINUX_PACKAGE_MANAGERS " in
      *" $AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER "*) PACKAGE_MANAGER="$AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER"; return ;;
    esac
    err "platform: AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER=$AGENT_REPL_REQUIREMENTS_PACKAGE_MANAGER is not one of: $LINUX_PACKAGE_MANAGERS"
    exit 2
  fi
  for pm in $LINUX_PACKAGE_MANAGERS; do
    if command -v "$(package_manager_command "$pm")" >/dev/null 2>&1; then
      PACKAGE_MANAGER="$pm"
      return
    fi
  done
}

# package_manager: this run's package manager (PACKAGE_MANAGER).
package_manager() { printf '%s\n' "$PACKAGE_MANAGER"; }

# package_manager_command PM: the command PM runs as.
package_manager_command() {
  case "$1" in
    apt) echo apt-get ;;
    *) echo "$1" ;;
  esac
}

# package_install_argv PM: the words that install packages through Linux
# package manager PM without asking anything, the packages following them.
package_install_argv() {
  case "$1" in
    # apt waits for dpkg's lock, which another apt (Ubuntu's own unattended
    # upgrades, most often) may hold, rather than failing on it at once.
    apt) echo "apt-get -o DPkg::Lock::Timeout=600 install -y" ;;
    dnf) echo "dnf install -y" ;;
    pacman) echo "pacman -S --needed --noconfirm" ;;
    zypper) echo "zypper --non-interactive install" ;;
  esac
}

# as_root ARGV...: ARGV run as root through sudo. With a terminal on stdin sudo
# may ask for the password there; without one it must not ask at all.
as_root() {
  if [ -t 0 ]; then
    sudo "$@"
  else
    sudo -n "$@"
  fi
}

# APT_UPDATED is set once this run has refreshed apt's package lists: a fresh
# machine's lists can be empty or stale, and every apt install after the first
# reuses the refresh.
APT_UPDATED=""

# os_install PM PACKAGE...: install PACKAGEs through Linux package manager PM,
# as root. A package already installed is left as it is.
os_install() {
  local pm="$1" argv
  shift
  if [ "$pm" = apt ] && [ -z "$APT_UPDATED" ]; then
    as_root apt-get update -q || { err "install: apt-get update failed"; return 1; }
    APT_UPDATED=1
  fi
  read -r -a argv <<<"$(package_install_argv "$pm")"
  as_root "${argv[@]}" "$@" || { err "install: sudo ${argv[*]} $* failed"; return 1; }
}

# packages ENTRY PATH PM: the packages the object at jq PATH of ENTRY names
# for PM (a string or a list), space-separated; empty when it names none.
packages() {
  printf '%s\n' "$1" | jq -r --arg pm "$3" "$2"' | .[$pm] // empty | if type == "array" then join(" ") else . end'
}

# shellcheck source=lib-worktrees.sh
. "$THIS_DIR/lib-worktrees.sh"
# shellcheck source=lib-service.sh
. "$THIS_DIR/lib-service.sh"
SERVICE_PLATFORM="$(platform)"

# The repository's MAIN worktree. A skill link must never point into a linked
# worktree, whose path dangles the moment it is pruned, so repo-relative skill
# sources always resolve here, whichever worktree runs the script.
#
#
# THE WORKTREE LIST IS READ ONCE PER RUN (resolve_worktrees, from
# prepare_entries): a run's worktrees cannot change under it, and every link
# check and every holder or service install asks for them, so reading git's
# list per question cost a git call each (27 per install of the install
# harness's synthetic repository, 2026-11-20).
WORKTREE_ROOTS=""
WORKTREES_READ=0
module_main_worktree() {
  worktrees_read_or_die
  printf '%s\n' "${WORKTREE_ROOTS%%$'\n'*}"
}

# module_linked_worktrees: every linked worktree of the module's repository, one
# a line, from the list read once.
module_linked_worktrees() {
  worktrees_read_or_die
  case "$WORKTREE_ROOTS" in
    *$'\n'*) printf '%s\n' "${WORKTREE_ROOTS#*$'\n'}" ;;
  esac
}

# worktrees_read_or_die: a question about the worktrees before the list was
# read is a bug in this script (resolve_worktrees reads it for every entry kind
# that asks), so it fails hard rather than reading the list again.
worktrees_read_or_die() {
  [ "$WORKTREES_READ" = 1 ] || { err "worktrees: asked before resolve_worktrees read the list; an entry kind that asks is missing from resolve_worktrees"; exit 2; }
}

# resolve_worktrees: read the module repository's worktree list into
# WORKTREE_ROOTS, in the script's own shell so every later subshell reads it,
# when any loaded entry asks about worktrees: a link (its source is resolved in
# the main worktree and refused inside a linked one), or a holder or service
# (installed from the main checkout only). A run that needs none never asks git.
resolve_worktrees() {
  local needed
  needed="$(printf '%s\n' "$ENTRIES" | jq -s 'any(.[]; (.kind | IN("skill_link", "command_link", "socket_holder", "service")) or ((.source | type) == "object" and .source.repo != null))')" || { err "worktrees: reading the loaded entries' kinds failed"; exit 2; }
  [ "$needed" = true ] || return 0
  WORKTREE_ROOTS="$(worktree_list "$MODULE_DIR")" || { err "worktrees: git worktree list failed for $MODULE_DIR"; exit 2; }
  [ -n "$WORKTREE_ROOTS" ] || { err "worktrees: git worktree list answered no worktree for $MODULE_DIR"; exit 2; }
  WORKTREES_READ=1
}

repo_top() { git -C "$MODULE_DIR" rev-parse --show-toplevel; }

# ---------------------------------------------------------------------------
# Loading. Every manifest is read once into ENTRIES: one compact JSON entry per
# line, each tagged with the system that declared it. An id declared by more
# than one system must be declared identically, so two systems that share a
# toolchain can never disagree about it.
# ---------------------------------------------------------------------------

ENTRIES=""

load_manifests() {
  if ! command -v jq >/dev/null 2>&1; then
    err "load: jq is not installed, and every manifest is read with it; macOS 15 and later ship /usr/bin/jq, else install it (brew install jq) and re-run"
    exit 2
  fi
  [ -f "$ROOT_MANIFEST" ] || { err "load: no root manifest at $ROOT_MANIFEST"; exit 2; }
  local files rel all
  files="$ROOT_MANIFEST"
  while IFS= read -r rel; do
    [ -n "$rel" ] || continue
    [ -f "$MODULE_DIR/$rel" ] || { err "load: $ROOT_MANIFEST includes $rel, which does not exist"; exit 2; }
    files="$files
$MODULE_DIR/$rel"
  done < <(jq -r '.includes // [] | .[]' "$ROOT_MANIFEST")
  all="$(while IFS= read -r f; do
    jq -c --arg file "$f" '.system as $s | .requirements[] | . + {_system: $s, _file: $file}' "$f"
  done <<<"$files")"
  validate_entries "$all"
  # Keep the first declaration of each id; validate_entries proved the rest equal.
  ENTRIES="$(printf '%s\n' "$all" | jq -sc 'reduce .[] as $e ({seen: {}, out: []};
      if .seen[$e.id] then . else .seen[$e.id] = true | .out += [$e] end) | .out[]')"
}

# prepare_entries: what every verb that checks entries reads once per run, after
# load_manifests: the worktree list and every entry's fields. list asks neither.
prepare_entries() {
  resolve_worktrees
  index_fields
}

# The manifest contract, checked before anything runs.
validate_entries() {
  local problems
  problems="$(printf '%s\n' "$1" | jq -rs '
    def need($e; $fields): $fields[] as $f | select($e[$f] == null) | "\($e._file): \($e.id // "?"): \($e.kind) needs \"\($f)\"";
    (.[] | . as $e |
      (["id", "kind", "why", "profile"][] as $f | select($e[$f] == null) | "\($e._file): \($e.id // "?"): every entry needs \"\($f)\""),
      (select(($e.profile != "run") and ($e.profile != "dev")) | "\($e._file): \($e.id): profile must be run or dev, not \($e.profile)"),
      (if $e.kind == "program" then need($e; ["command", "install", "platforms"]),
         (select($e.install.release != null) | $e.install.release
           | (select(.file == null) | "\($e._file): \($e.id): an install.release needs \"file\""),
             (to_entries[] | select(.key != "file") | .key as $m | .value
               | ("url", "sha256") as $f | select(.[$f] == null)
               | "\($e._file): \($e.id): install.release.\($m) needs \"\($f)\""))
       elif $e.kind == "emacs" then need($e; ["command", "min", "features", "install", "platforms"]),
         (select($e.install.source != null) | need($e; ["revision"]),
           (select($e.install.source.repo == null)
             | "\($e._file): \($e.id): an emacs install.source needs \"repo\""),
           ($e.platforms // [] | .[] as $p
             | (select($e.install.source[$p] == null)
                 | "\($e._file): \($e.id): an emacs install.source needs a \"\($p)\" build block"),
               (select($e.install.source[$p] != null and $e.install.source[$p].configure == null)
                 | "\($e._file): \($e.id): an emacs install.source.\($p) needs \"configure\""),
               (select($p == "darwin" and $e.install.source[$p] != null and $e.install.source[$p].app == null)
                 | "\($e._file): \($e.id): an emacs install.source.darwin needs \"app\"")))
       elif $e.kind == "env" then need($e; ["name", "doc"])
       elif $e.kind == "group" then need($e; ["name", "platforms"])
       elif $e.kind == "notification_permission" then need($e; ["command", "platforms"])
       elif $e.kind == "app_build" then need($e; ["app", "build", "platforms"]),
         (select($e.build != null and $e.build.module == null) | "\($e._file): \($e.id): an app_build build is a path in the module (\"module\")")
       elif $e.kind == "location_permission" then need($e; ["app", "platforms"])
       elif $e.kind == "signing_identity" then need($e; ["name", "platforms"])
       elif $e.kind == "sudo_grant" then need($e; ["command", "invocations", "file", "platforms"]),
         (select($e.command != null and ($e.command | test("^/[A-Za-z0-9_./-]+$") | not))
           | "\($e._file): \($e.id): a sudo_grant command is an absolute path of plain characters, not \($e.command)"),
         ($e.invocations // [] | .[] | select(test("^[A-Za-z0-9_./ -]+$") | not)
           | "\($e._file): \($e.id): a sudo_grant invocation is plain words, not \(.)"),
         (select($e.file != null and ($e.file | test("^[A-Za-z0-9_-]+$") | not))
           | "\($e._file): \($e.id): a sudo_grant file is a plain name sudo reads from /etc/sudoers.d, not \($e.file)")
       elif $e.kind == "skill_link" then need($e; ["name", "source"])
       elif $e.kind == "command_link" then need($e; ["name", "source"]),
         (select($e.source != null and $e.source.repo == null) | "\($e._file): \($e.id): a command_link source is a path in the repository (\"repo\")")
       elif $e.kind == "git_hook" then need($e; ["hook", "source", "markers"])
       elif $e.kind == "service" then need($e; ["label", "args", "log", "platforms"]),
         (("stdout", "stderr") as $f | select($e.log != null and $e.log[$f] == null)
           | "\($e._file): \($e.id): a service log needs \"\($f)\""),
         (select((($e.command != null) and (($e.build_target != null) or ($e.binary != null)))
                 or (($e.command == null) and (($e.build_target == null) or ($e.binary == null))))
           | "\($e._file): \($e.id): service needs either \"command\" or both \"build_target\" and \"binary\""),
         ($e.files // [] | .[] | select(.from == null or .to == null)
           | "\($e._file): \($e.id): every service file needs \"from\" and \"to\"")
       elif $e.kind == "socket_holder" then need($e; ["label", "control", "sockets", "log", "platforms"]),
         ($e.sockets // [] | .[] | select(.name == null or ((.unix == null) == (.tcp == null)))
           | "\($e._file): \($e.id): every held socket needs a \"name\" and exactly one of \"unix\" or \"tcp\"")
       elif $e.kind == "download" then need($e; ["url", "archive", "dest", "files"]),
         (select($e.archive != null and $e.archive != "zip") | "\($e._file): \($e.id): download archive must be zip, not \($e.archive)"),
         ($e.files // [] | .[] | select(.from == null or .to == null or .sha256 == null)
           | "\($e._file): \($e.id): every download file needs \"from\", \"to\" and \"sha256\"")
       else "\($e._file): \($e.id): unknown kind \($e.kind)" end)),
    (group_by(.id)[] | select((map(del(._system, ._file)) | unique | length) > 1)
      | "\(.[0].id): declared differently by \(map(._file) | join(" and ")); a shared requirement must be declared identically")')"
  if [ -n "$problems" ]; then
    printf '%s\n' "$problems" | while IFS= read -r line; do err "manifest: $line"; done
    exit 2
  fi
}

# field ENTRY JQ-PATH: one field of an entry, empty when absent.
#
# EVERY LOADED ENTRY'S FIELDS ARE DECODED ONCE PER RUN (index_fields, from
# prepare_entries), so a field of a loaded entry at a plain dotted path is
# answered without a jq process: an install of the install harness's synthetic
# repository spawned 114 jq processes, one per field asked (2026-11-20). Every
# other question (an entry not loaded, a bracketed path, a field holding an
# object or array or a line break) is jq's own, exactly as before, so the
# answer never differs from jq's.
field() {
  local path="$2" i flat="" rest line type
  if [[ $path =~ ^(\.[A-Za-z_][A-Za-z0-9_]*)+$ ]]; then
    for i in "${!FIELD_JSON[@]}"; do
      if [ "${FIELD_JSON[$i]}" = "$1" ]; then flat="${FIELD_FLAT[$i]}"; break; fi
    done
  fi
  if [ -n "$flat" ]; then
    case "$flat" in
      *$'\n'"$path"$'\t'*)
        rest="${flat#*$'\n'"$path"$'\t'}"
        line="${rest%%$'\n'*}"
        type="${line%%$'\t'*}"
        case "$type" in
          string|number|true) printf '%s\n' "${line#*$'\t'}"; return ;;
          false|null) return ;;
        esac ;;
      *)
        # Absent: jq answers empty when the nearest present ancestor is an
        # object, and decides for itself (an error, for one) otherwise.
        if field_absent_under_object "$flat" "$path"; then return; fi ;;
    esac
  fi
  printf '%s\n' "$1" | jq -r "$path // empty"
}

# FIELD_JSON and FIELD_FLAT: each loaded entry, and its fields one a line as
# "<dotted path>\t<type>\t<value>", led by a line break. A field whose value
# holds a tab or a line break is typed "text", which field leaves to jq.
FIELD_JSON=()
FIELD_FLAT=()

# index_fields: decode every loaded entry's fields once, with one jq process.
# jq writes each entry as an ASCII record separator (\036), its compact JSON,
# and its field lines; its output is read whole first, so a jq failure fails the
# run before anything is indexed.
index_fields() {
  local out rest rec
  FIELD_JSON=()
  FIELD_FLAT=()
  out="$(printf '%s\n' "$ENTRIES" | jq -j '
    def key: if type == "number" then "[\(.)]" elif test("^[A-Za-z_][A-Za-z0-9_]*$") then ".\(.)" else ".[\(tojson)]" end;
    . as $e | "\u001e" + tojson +
      ([paths as $p | ($e | getpath($p)) as $v
        | "\n" + ($p | map(key) | join("")) + "\t" +
          (if ($v | type) == "string" then (if ($v | test("[\t\n\u001e]")) then "text\t" else "string\t" + $v end)
           elif ($v | type) == "number" then "number\t" + ($v | tostring)
           elif $v == true then "true\ttrue"
           elif $v == false then "false\t"
           elif $v == null then "null\t"
           else ($v | type) + "\t" end)] | join(""))')" || { err "load: decoding the loaded entries' fields failed"; exit 2; }
  rest="${out#$'\036'}"
  while [ -n "$out" ]; do
    rec="${rest%%$'\036'*}"
    # Compact JSON holds no line break, and the field lines each begin with one.
    FIELD_JSON+=("${rec%%$'\n'*}")
    case "$rec" in
      *$'\n'*) FIELD_FLAT+=($'\n'"${rec#*$'\n'}") ;;
      *) FIELD_FLAT+=("") ;;
    esac
    case "$rest" in
      *$'\036'*) rest="${rest#*$'\036'}" ;;
      *) break ;;
    esac
  done
  local loaded=0 line
  while IFS= read -r line; do [ -z "$line" ] || loaded=$((loaded + 1)); done <<<"$ENTRIES"
  [ "${#FIELD_JSON[@]}" -eq "$loaded" ] || { err "load: decoded the fields of ${#FIELD_JSON[@]} entries, but $loaded were loaded"; exit 2; }
}

# field_absent_under_object FLAT PATH: 0 when PATH is absent from FLAT and its
# nearest present ancestor is an object (the entry itself at the top).
field_absent_under_object() {
  local flat="$1" path="$2" parent rest
  parent="${path%.*}"
  while [ -n "$parent" ]; do
    case "$flat" in
      *$'\n'"$parent"$'\t'*)
        rest="${flat#*$'\n'"$parent"$'\t'}"
        [ "${rest%%$'\t'*}" = object ]
        return ;;
    esac
    parent="${parent%.*}"
  done
  return 0
}

# ---------------------------------------------------------------------------
# Versions.
# ---------------------------------------------------------------------------

# first_version TEXT: the first dotted number in TEXT, empty when it has none.
# grep's no-match status (1) is that answer; any other failure stays fatal.
first_version() {
  { printf '%s\n' "$1" | grep -oE '[0-9]+(\.[0-9]+)*' || [ $? -eq 1 ]; } | sed -n 1p
}

# version_ge HAVE WANT: 0 when HAVE is at least WANT, part by part.
version_ge() {
  local have="$1" want="$2" h w
  while [ -n "$have$want" ]; do
    h="${have%%.*}"; w="${want%%.*}"
    h="${h:-0}"; w="${w:-0}"
    if [ "$h" -gt "$w" ]; then return 0; fi
    if [ "$h" -lt "$w" ]; then return 1; fi
    if [ "$have" = "${have#*.}" ]; then have=""; else have="${have#*.}"; fi
    if [ "$want" = "${want#*.}" ]; then want=""; else want="${want#*.}"; fi
  done
  return 0
}

# ---------------------------------------------------------------------------
# Checks. check_<kind> ENTRY sets STATUS and DETAIL, and FIX: what install
# does, or what the user must do, when STATUS is not ok.
# ---------------------------------------------------------------------------

STATUS=""
DETAIL=""
FIX=""

# install_text ENTRY: the install action an entry's `install` names for this
# machine's package manager, as text.
install_text() {
  local e="$1" how pm
  pm="$(package_manager)"
  if [ "$pm" != brew ]; then
    how="$(printf '%s\n' "$e" | jq -r --arg pm "$pm" --arg argv "$(package_install_argv "$pm")" --arg known "$LINUX_PACKAGE_MANAGERS" \
      --arg machine "$(machine)" --arg bin "$BIN_DIR" --arg command "$(field "$e" .command)" '.install |
      if $pm != "" and .[$pm] then "sudo \($argv) \(.[$pm] | if type == "array" then join(" ") else . end)"
      elif .go_install then "go install \(.go_install)"
      elif .release then (if .release[$machine] then "download the pinned \(.release[$machine].url) and install its \(.release.file) as \($bin)/\($command)"
                          else "no release is pinned for \($machine); add install.release.\($machine) to its manifest entry" end)
      elif .source then "build \(.source.repo) at the pinned revision and install it"
      elif .manual then .manual
      elif $pm == "" then "install it yourself: no package manager install knows (\($known)) is on PATH"
      else "no \($pm) package is declared for it; add install.\($pm) to its manifest entry" end')"
    printf '%s\n' "$how"
    return
  fi
  how="$(printf '%s\n' "$e" | jq -r '.install |
    if .brew then ((if .tap then "brew tap \(.tap) && " else "" end) + "brew install \(.brew)" + ((.options // []) | map(" " + .) | join("")))
    elif .brew_cask then "brew install --cask \(.brew_cask)"
    elif .go_install then "go install \(.go_install)"
    elif .source then "build \(.source.repo) at the pinned revision and install it as \(.source.darwin.app)"
    elif .manual then .manual
    else "no install step" end')"
  printf '%s\n' "$how"
}

check_program() {
  local e="$1" cmd path min args out have
  cmd="$(field "$e" .command)"
  FIX="$(install_text "$e")"
  if ! path="$(find_program "$cmd")"; then
    STATUS=missing; DETAIL="$cmd is not on PATH"; return
  fi
  min="$(field "$e" .version.min)"
  if [ -z "$min" ]; then
    STATUS=ok; DETAIL="$path"; return
  fi
  args=()
  while IFS= read -r a; do args+=("$a"); done < <(printf '%s\n' "$e" | jq -r '.version.args[]')
  if ! out="$("$path" "${args[@]}" 2>&1)"; then
    STATUS=failed; DETAIL="$path ${args[*]} failed: $out"; return
  fi
  have="$(first_version "$out")"
  if [ -z "$have" ]; then
    STATUS=failed; DETAIL="$path ${args[*]} answered no version: $out"; return
  fi
  if version_ge "$have" "$min"; then
    STATUS=ok; DETAIL="$path $have"
  else
    STATUS=outdated; DETAIL="$path is $have, older than $min (an older copy earlier on PATH shadows a newer one the same way)"
  fi
}

check_emacs() {
  local e="$1" cmd path min have lacking
  cmd="$(field "$e" .command)"
  FIX="$(install_text "$e")"
  if ! path="$(find_program "$cmd")"; then
    STATUS=missing; DETAIL="$cmd is not on PATH"; return
  fi
  min="$(field "$e" .min)"
  if ! have="$("$path" --batch --eval '(princ emacs-version)' 2>&1)"; then
    STATUS=failed; DETAIL="$path --batch could not report its version: $have"; return
  fi
  lacking="$("$path" --batch --eval "(princ (mapconcat #'symbol-name (seq-remove #'featurep '($(printf '%s\n' "$e" | jq -r '.features | join(" ")'))) \" \"))" 2>&1)"
  if ! version_ge "$(first_version "$have")" "$min"; then
    STATUS=outdated; DETAIL="$path is Emacs $have, older than $min"; return
  elif [ -n "$lacking" ]; then
    STATUS=outdated; DETAIL="$path (Emacs $have) lacks: $lacking"; return
  fi
  local revision built configured missing="" f
  revision="$(field "$e" .revision)"
  if [ -n "$revision" ]; then
    built="$("$path" --batch --eval '(princ (or emacs-repository-version ""))' 2>&1)"
    if [ "$built" != "$revision" ]; then
      STATUS=outdated; DETAIL="$path (Emacs $have) was built from ${built:-no recorded commit}, not the pinned $revision"; return
    fi
  fi
  configured=" $("$path" --batch --eval '(princ system-configuration-features)' 2>&1) "
  while IFS= read -r f; do
    [ -n "$f" ] || continue
    case "$configured" in *" $f "*) ;; *) missing="$missing $f" ;; esac
  done < <(printf '%s\n' "$e" | jq -r --arg p "$(platform)" '(.configuration_features // []) + (.install.source[$p].configuration_features // []) | .[]')
  if [ -n "$missing" ]; then
    STATUS=outdated; DETAIL="$path (Emacs $have) was configured without:$missing"
  else
    STATUS=ok; DETAIL="$path Emacs $have${revision:+ at $revision}"
  fi
}

check_env() {
  local e="$1" name
  name="$(field "$e" .name)"
  FIX="set $name in your shell profile, then run \`doom env\` (see $(field "$e" .doc))"
  if [ -n "${!name:-}" ]; then
    STATUS=ok; DETAIL="$name is set"
  else
    STATUS=needs-user; DETAIL="$name is not set"
  fi
}

# check_group: the user's groups, as the kernel holds them for this login
# (`id -nG`). A group granted since takes a new login, which the fix says.
check_group() {
  local e="$1" name
  name="$(field "$e" .name)"
  FIX="sudo usermod -aG $name \"\$USER\", then log out and back in"
  if grep -qx "$name" <<<"$(id -nG | tr ' ' '\n')"; then
    STATUS=ok; DETAIL="this user is in the $name group"
  else
    STATUS=needs-user; DETAIL="this user is not in the $name group"
  fi
}

check_notification_permission() {
  local e="$1" cmd out
  cmd="$(field "$e" .command)"
  FIX="install posts one banner so macOS asks; then allow $cmd in System Settings → Notifications"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    STATUS=missing; DETAIL="$cmd is not on PATH"; return
  fi
  if out="$("$cmd" -diagnose 2>&1)"; then
    STATUS=ok; DETAIL="$cmd may notify"
  else
    STATUS=missing; DETAIL="$cmd may not notify: $(printf '%s\n' "$out" | grep -iE 'authoriz|problem' | tr '\n' ' ')"
  fi
}

# app_path ENTRY: where an app_build or location_permission entry's app is
# installed, in the cache.
app_path() { printf '%s/%s\n' "$CACHE_ROOT" "$(field "$1" .app)"; }

# app_build_script ENTRY: the app_build entry's build script, in the module.
app_build_script() { printf '%s/%s\n' "$MODULE_DIR" "$(field "$1" .build.module)"; }

check_app_build() {
  local e="$1" app script want have
  app="$(app_path "$e")"
  script="$(app_build_script "$e")"
  FIX="install builds it: $script $app"
  if [ ! -d "$app" ]; then
    STATUS=missing; DETAIL="$app is not built"; return
  fi
  if ! want="$("$script" --source-hash </dev/null 2>&1)"; then
    STATUS=failed; DETAIL="$script --source-hash failed: $want"; return
  fi
  have="$(cat "$app/Contents/Resources/source-sha256" 2>/dev/null || true)"
  if [ "$have" = "$want" ]; then
    STATUS=ok; DETAIL="$app"
  else
    STATUS=outdated; DETAIL="$app was built from other sources than the checkout's"
  fi
}

# check_location_permission: the helper's own scan names its standing
# (wifi-helper/main.swift); only "authorized" names networks.
check_location_permission() {
  local e="$1" app out standing
  app="$(app_path "$e")"
  FIX="install asks once; then allow \"agent-repl Wi-Fi\" in System Settings → Privacy & Security → Location Services"
  if [ ! -d "$app" ]; then
    STATUS=missing; DETAIL="$app is not built"; return
  fi
  out="$(mktemp "${TMPDIR:-/tmp}/agent-repl-wifi-check.XXXXXX")"
  # </dev/null: the install loop reads its entries from stdin.
  if ! open -W -n -g -a "$app" --args scan --out "$out" </dev/null >/dev/null 2>&1; then
    rm -f "$out"
    STATUS=failed; DETAIL="$app could not be run"; return
  fi
  standing="$(jq -r '.authorization // empty' "$out" 2>/dev/null || true)"
  rm -f "$out"
  case "$standing" in
    authorized) STATUS=ok; DETAIL="$app may name Wi-Fi networks" ;;
    "") STATUS=failed; DETAIL="$app answered no standing" ;;
    *) STATUS=missing; DETAIL="$app may not name Wi-Fi networks: $standing" ;;
  esac
}

# check_signing_identity: the login keychain holds a code-signing identity
# named NAME. It is self-signed, so macOS never calls it valid; a signature
# made with it still names its certificate, which is what keeps a rebuilt
# app's Location Services permission (wifi-helper/build.sh).
check_signing_identity() {
  local e="$1" name listed
  name="$(field "$e" .name)"
  FIX="install makes it: a self-signed code-signing certificate named \"$name\" in the login keychain"
  listed="$(security find-identity -p codesigning </dev/null 2>/dev/null || true)"
  if grep -qF "\"$name\"" <<<"$listed"; then
    STATUS=ok; DETAIL="the login keychain signs code as \"$name\""
  else
    STATUS=missing; DETAIL="the login keychain holds no code-signing identity named \"$name\""
  fi
}

# SUDOERS_DIR is where a sudo_grant's file is installed; sudo reads every file
# in it without a dot or a trailing ~ in its name.
SUDOERS_DIR=/etc/sudoers.d

# sudo_nopasswd_commands: every command `sudo -n -l` lists the user may run as
# root without a password, one per line ("ALL" for any command). A listing line
# is `(RUNAS) [TAG: ...]CMD, [TAG: ...]CMD, ...`; a tag holds for the commands
# after it on the line until another tag resets it, and a command runs as root
# only when RUNAS names root or ALL. Answers non-zero when sudo cannot list
# without a password, printing its answer.
sudo_nopasswd_commands() {
  local out
  out="$(sudo -n -l 2>&1)" || { printf '%s\n' "$out"; return 1; }
  printf '%s\n' "$out" | awk '
    /^[ \t]+\(/ {
      line = $0
      sub(/^[ \t]+\(/, "", line)
      runas = substr(line, 1, index(line, ")") - 1)
      rest = substr(line, index(line, ")") + 1)
      sub(/^[ \t]+/, "", rest)
      split(runas, users, ":")
      gsub(/[ \t]/, "", users[1])
      asroot = 0
      n = split(users[1], who, ",")
      for (i = 1; i <= n; i++) if (who[i] == "root" || who[i] == "ALL") asroot = 1
      nopasswd = 0
      n = split(rest, cmds, /, /)
      for (i = 1; i <= n; i++) {
        cmd = cmds[i]
        while (match(cmd, /^[A-Z_]+:[ \t]*/)) {
          tag = substr(cmd, 1, index(cmd, ":") - 1)
          if (tag == "NOPASSWD") nopasswd = 1
          if (tag == "PASSWD") nopasswd = 0
          cmd = substr(cmd, RLENGTH + 1)
        }
        if (asroot && nopasswd) print cmd
      }
    }'
}

# sudo_grant_lines ENTRY: each invocation of the entry, as `COMMAND ARGS`.
sudo_grant_lines() {
  printf '%s\n' "$1" | jq -r '.command as $c | .invocations[] | "\($c) \(.)"'
}

# sudo_grant_fix ENTRY: the one command that installs the entry's file. It
# writes every invocation, so it can be run again over the file it made; a line
# another file already grants is a harmless duplicate. visudo checks the file
# before it goes in, and no existing sudoers file is ever edited.
sudo_grant_fix() {
  local e="$1" line quoted=""
  while IFS= read -r line; do quoted="$quoted \"$line\""; done < <(sudo_grant_lines "$e")
  printf '%s' "(f=\"\$(mktemp \"\${TMPDIR:-/tmp}/agent-repl-sudoers.XXXXXX\")\" && trap 'rm -f \"\$f\"' EXIT && u=\"\$(id -un)\" && for c in$quoted; do printf '%s ALL=(root) NOPASSWD: %s\\n' \"\$u\" \"\$c\"; done >\"\$f\" && sudo visudo -c -f \"\$f\" && sudo install -m 0440 -o root -g wheel \"\$f\" $SUDOERS_DIR/$(field "$e" .file))"
}

check_sudo_grant() {
  local e="$1" granted line missing=""
  FIX="run this in a terminal; it asks for your password: $(sudo_grant_fix "$e")"
  if ! granted="$(sudo_nopasswd_commands)"; then
    STATUS=needs-user
    DETAIL="sudo -n -l could not list your grants ($(sed -n 1p <<<"$granted")), so none of $(sudo_grant_lines "$e" | paste -sd ',' - | sed 's/,/, /g') is granted"
    return
  fi
  while IFS= read -r line; do
    if ! grep -qxF -e "$line" -e "$(field "$e" .command)" -e ALL <<<"$granted"; then
      missing="${missing:+$missing, }$line"
    fi
  done < <(sudo_grant_lines "$e")
  if [ -z "$missing" ]; then
    STATUS=ok; DETAIL="sudo runs every invocation of $(field "$e" .command) without a password"
  else
    STATUS=needs-user; DETAIL="sudo asks for a password for $missing"
  fi
}

# link_source ENTRY: the canonical source path of a link entry (a skill link or
# a command link), or empty when its env root is unset.
link_source() {
  local e="$1" repo env path
  repo="$(field "$e" .source.repo)"
  if [ -n "$repo" ]; then
    printf '%s/%s\n' "$(module_main_worktree)" "$repo"
    return
  fi
  env="$(field "$e" .source.env)"
  path="$(field "$e" .source.path)"
  [ -n "${!env:-}" ] || return 0
  printf '%s/%s\n' "${!env}" "$path"
}

# physical PATH: PATH with its existing directory part spelled physically, as
# git spells worktree roots (macOS's /var is /private/var).
physical() {
  local dir="$1" rest=""
  while [ ! -d "$dir" ] && [ "$dir" != / ]; do
    rest="/$(basename "$dir")$rest"
    dir="$(dirname "$dir")"
  done
  printf '%s%s\n' "$(cd "$dir" && pwd -P)" "$rest"
}

# in_linked_worktree PATH: 0 when PATH lies inside a linked worktree.
in_linked_worktree() {
  local root path
  path="$(physical "$1")"
  while IFS= read -r root; do
    [ -n "$root" ] || continue
    root="$(physical "$root")"
    case "$path" in "$root"/*|"$root") return 0 ;; esac
  done < <(module_linked_worktrees)
  return 1
}

# check_retired_skill_link ENTRY: a RETIRED skill is met once no link of ours
# stands at its name; a file of the user's there is theirs, and not ours to
# report.
check_retired_skill_link() {
  local e="$1" dir="$2" dest src
  dest="$dir/$(field "$e" .name)"
  src="$(link_source "$e")"
  FIX="rm $dest"
  if [ -L "$dest" ] && { [ "$(readlink "$dest")" = "$src" ] || [ ! -e "$dest" ]; }; then
    STATUS=outdated; DETAIL="$dest still links the retired skill"
  else
    STATUS=ok; DETAIL="$(field "$e" .name) is retired and not linked"
  fi
}

# check_skill_link ENTRY: the link checked in every skills directory
# (skills_dirs); the first directory where it is not ok decides the status.
check_skill_link() {
  local e="$1" dir joined=""
  while IFS= read -r dir; do
    check_skill_link_in "$e" "$dir"
    if [ "$STATUS" != ok ]; then return; fi
    joined="${joined:+$joined; }$DETAIL"
  done < <(skills_dirs)
  DETAIL="$joined"
}

# check_skill_link_in ENTRY DIR: the link checked in the one skills directory DIR.
check_skill_link_in() {
  local e="$1" dir="$2" name src dest
  if [ "$(field "$e" .retired)" = true ]; then
    check_retired_skill_link "$e" "$dir"
    return
  fi
  name="$(field "$e" .name)"
  dest="$dir/$name"
  src="$(link_source "$e")"
  if [ -z "$src" ]; then
    STATUS=needs-user; DETAIL="$(field "$e" .source.env) is not set, so $name has no source"
    FIX="set $(field "$e" .source.env), then re-run install"; return
  fi
  check_link "$name" "$src" "$dest"
}

# check_link NAME SRC DEST: the one check of a link this script keeps, for
# every link kind. SRC must be a main-worktree path that exists; a file of the
# user's at DEST is theirs and never overwritten.
check_link() {
  local name="$1" src="$2" dest="$3"
  FIX="ln -sfn $src $dest"
  if in_linked_worktree "$src"; then
    STATUS=failed; DETAIL="$name's source $src is inside a linked worktree, whose path dangles once it is pruned"
    FIX="point the entry at the main worktree"; return
  fi
  if [ ! -e "$src" ]; then
    STATUS=failed; DETAIL="$name's source $src does not exist"; FIX="fix the entry's source"; return
  fi
  if [ -e "$dest" ] && [ ! -L "$dest" ]; then
    STATUS=needs-user; DETAIL="$dest is a file of yours, not a link; it is never overwritten"
    FIX="move $dest aside, then re-run install"; return
  fi
  if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$src" ]; then
    STATUS=ok; DETAIL="$dest -> $src"
  elif [ -L "$dest" ]; then
    STATUS=outdated; DETAIL="$dest -> $(readlink "$dest"), not $src"
  else
    STATUS=missing; DETAIL="$dest is not linked"
  fi
}

# check_command_link ENTRY: the command linked into BIN_DIR, and BIN_DIR on
# PATH ahead of any other command of that name, so a shell, an agent's tool
# call and a skill all reach it by its bare name.
check_command_link() {
  local e="$1" name src dest found
  name="$(field "$e" .name)"
  dest="$BIN_DIR/$name"
  src="$(link_source "$e")"
  check_link "$name" "$src" "$dest"
  [ "$STATUS" = ok ] || return 0
  case ":$PATH:" in
    *":$BIN_DIR:"*) ;;
    *)
      STATUS=needs-user; DETAIL="$dest is linked, but $BIN_DIR is not on PATH, so $name is not found by name"
      FIX="add export PATH=\"$BIN_DIR:\$PATH\" to your shell profile, then run doom env"; return ;;
  esac
  found="$(command -v "$name" 2>/dev/null || true)"
  if [ "$found" != "$dest" ]; then
    STATUS=needs-user; DETAIL="$name is found at $found ahead of $dest on PATH"
    FIX="move $BIN_DIR ahead of $(dirname "$found") on PATH in your shell profile, or remove $found"; return
  fi
  DETAIL="$dest -> $src, on PATH"
}

# hook_paths ENTRY: sets HOOK_SRC and HOOK_DEST for a git_hook entry.
#
# The repository's top and hooks directory are read once per run (HOOK_TOP,
# HOOK_DIR): neither changes under a run, and a hook's check, install and
# re-check each asked git again.
HOOK_SRC=""
HOOK_DEST=""
HOOK_TOP=""
HOOK_DIR=""
hook_paths() {
  local e="$1"
  if [ -z "$HOOK_TOP" ]; then
    HOOK_TOP="$(repo_top)"
    HOOK_DIR="$(git -C "$HOOK_TOP" rev-parse --git-path hooks)"
    case "$HOOK_DIR" in /*) ;; *) HOOK_DIR="$HOOK_TOP/$HOOK_DIR" ;; esac
  fi
  HOOK_SRC="$HOOK_TOP/$(field "$e" .source)"
  HOOK_DEST="$HOOK_DIR/$(field "$e" .hook)"
}

# is_managed_hook ENTRY FILE: 0 when FILE carries one of the entry's markers.
is_managed_hook() {
  local marker
  while IFS= read -r marker; do
    grep -qF -- "$marker" "$2" 2>/dev/null && return 0
  done < <(printf '%s\n' "$1" | jq -r '.markers[]')
  return 1
}

check_git_hook() {
  local e="$1"
  hook_paths "$e"
  FIX="cp $HOOK_SRC $HOOK_DEST"
  [ -f "$HOOK_SRC" ] || { STATUS=failed; DETAIL="the hook source $HOOK_SRC does not exist"; FIX="fix the entry's source"; return; }
  if [ -e "$HOOK_DEST" ] && [ "$HOOK_SRC" -ef "$HOOK_DEST" ]; then
    # core.hooksPath reads the managed source dir: the hook is live as checked in.
    STATUS=ok; DETAIL="$HOOK_DEST is the checked-in hook (core.hooksPath)"
  elif [ ! -f "$HOOK_DEST" ]; then
    STATUS=missing; DETAIL="$HOOK_DEST is not installed"
  elif ! is_managed_hook "$e" "$HOOK_DEST"; then
    STATUS=needs-user; DETAIL="$HOOK_DEST is a hook of yours; it is never overwritten"
    FIX="append the body of $HOOK_SRC to $HOOK_DEST"
  elif cmp -s "$HOOK_SRC" "$HOOK_DEST"; then
    STATUS=ok; DETAIL="$HOOK_DEST"
  else
    STATUS=outdated; DETAIL="$HOOK_DEST differs from $HOOK_SRC"
  fi
}

# The service's settings every platform renders alike: a service is started at
# login and kept running, restarted no faster than every SERVICE_RESTART_SECONDS
# when it crash-loops, and killed when a stop it was asked for is not done
# within SERVICE_STOP_SECONDS (each service bounds its own shutdown well inside
# that, so reaching it means the process is stuck).
SERVICE_RESTART_SECONDS=10
SERVICE_STOP_SECONDS=20

# command_service_path: the PATH a `command` service's program is found on. A
# service manager's own PATH holds neither Homebrew's prefixes nor the user's
# bin dir.
command_service_path() {
  if [ "$(platform)" = darwin ]; then
    echo "/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"
  else
    echo "$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin"
  fi
}

# service_definition ENTRY: where the platform's service manager reads the
# service's definition from (lib-service.sh): a launchd plist, or a systemd
# user unit.
service_definition() { service_definition_path "$(field "$1" .label)"; }

# rendered_service ENTRY: the service's definition for this platform, rendered
# from the entry, so a plist and a unit can never disagree about a service.
# Every argument's placeholders are expanded (expand_placeholders), and every
# `~/` becomes the absolute $HOME (no service manager expands ~).
#
# THE PROGRAM IS RUN THROUGH /usr/bin/env ON BOTH PLATFORMS. On macOS the
# background task manager names a login item by its program's code signature,
# and a binary built here is only ad-hoc signed, so every rebuild read as a new
# item and posted an "App Background Activity" notice; Apple-signed
# /usr/bin/env never changes. A `command` service needs env to find its
# program on PATH on either platform.
#
# A macOS SERVICE IS `ProcessType` `Interactive`: the live editor waits on
# these services, and the test service's runs keep normal work's CPU priority
# (owner ruling 2026-11-13). Measured 2026-11-17 on battery: a launchd agent and
# its children run at priority 20 (utility's clamp) under `Standard` or no
# ProcessType, 4 under `Adaptive` and darwinbg under `Background`, and 31
# (normal work's, as a shell's child) only under `Interactive`.
rendered_service() {
  local entry="$1" arg args=()
  while IFS= read -r arg; do
    args+=("$(expand_placeholders "$arg")") || return 1
  done < <(printf '%s\n' "$entry" | jq -r '(.args // [])[]')
  # The arguments go to jq as JSON, never as its own argv, where one opening
  # with a dash would read as a jq option.
  local json="[]"
  if [ "${#args[@]}" -gt 0 ]; then
    json="$(printf '%s\n' "${args[@]}" | jq -Rn '[inputs]')"
  fi
  entry="$(printf '%s\n' "$entry" | jq -c --argjson args "$json" '.args = $args')"
  printf '%s\n' "$entry" | jq -r --arg home "$HOME" --arg platform "$(platform)" --arg cache "$CACHE_ROOT" \
    --arg path "$(command_service_path)" --arg restart "$SERVICE_RESTART_SECONDS" --arg stop "$SERVICE_STOP_SECONDS" '
    def home: gsub("~/"; $home + "/");
    def xml: gsub("&"; "&amp;") | gsub("<"; "&lt;") | gsub(">"; "&gt;");
    def unit: gsub("%"; "%%");
    def word: "\"" + (gsub("\\\\"; "\\\\") | gsub("\""; "\\\"") | gsub("\\$"; "$$") | unit) + "\"";
    (if .command then .command else $cache + "/bin/" + .binary end) as $program
    | (["/usr/bin/env", $program] + .args | map(home)) as $argv
    | ((if .command then {PATH: $path} else {} end) + ((.env // {}) | map_values(home))) as $env
    | ($cache + "/log/" + .log.stdout) as $out | ($cache + "/log/" + .log.stderr) as $err
    | "Rendered by bin/requirements.sh from the \(.id) entry of its requirements.json; change that entry, never this file." as $note
    | if $platform == "darwin" then
        ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
         "<!-- \($note | xml) -->",
         "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">",
         "<plist version=\"1.0\">", "<dict>",
         "    <key>Label</key>", "    <string>\(.label | xml)</string>",
         "    <key>ProgramArguments</key>", "    <array>",
         ($argv[] | "        <string>\(xml)</string>"),
         "    </array>",
         (if ($env | length) > 0 then
            "    <key>EnvironmentVariables</key>", "    <dict>",
            ($env | to_entries[] | "        <key>\(.key | xml)</key>", "        <string>\(.value | xml)</string>"),
            "    </dict>"
          else empty end),
         "    <key>RunAtLoad</key>", "    <true/>",
         "    <key>KeepAlive</key>", "    <true/>",
         "    <key>ThrottleInterval</key>", "    <integer>\($restart)</integer>",
         "    <key>ExitTimeOut</key>", "    <integer>\($stop)</integer>",
         "    <key>ProcessType</key>", "    <string>Interactive</string>",
         "    <key>StandardOutPath</key>", "    <string>\($out | xml)</string>",
         "    <key>StandardErrorPath</key>", "    <string>\($err | xml)</string>",
         "</dict>", "</plist>"]
      else
        ["# \($note)",
         "[Unit]", "Description=agent-repl \(.label)",
         "", "[Service]",
         "ExecStart=\($argv | map(word) | join(" "))",
         ($env | to_entries[] | "Environment=\((.key + "=" + .value) | word)"),
         "Restart=always", "RestartSec=\($restart)", "TimeoutStopSec=\($stop)",
         "StandardOutput=append:\($out | unit)", "StandardError=append:\($err | unit)",
         "", "[Install]", "WantedBy=default.target"]
      end | .[]'
}

# THE HOLDER'S TEMPLATE IS ITS OWN, and every service-wide setting above is
# deliberately absent from it: a change to those re-renders every service and
# reloads it, and a reloaded holder closes its sockets, which is the failure
# the holder exists to remove (docs/investigations/
# 2026-11-18-stable-sockets-design.md). test-requirements.sh pins this
# rendering byte for byte.
#
# HOLDER_JQ is the jq every holder rendering shares, so launchd's job and
# systemd's service run the one argv: `home` expands a leading ~/ against
# $home, `holder_argv` is the sockethold command line of a holder ENTRY
# under $cache, and `holder_sockets` is every socket it holds, its control
# socket's first.
HOLDER_JQ='
    def home: gsub("~/"; $home + "/");
    def holder_argv:
      ["/usr/bin/env", $cache + "/bin/sockethold", "--control", "control"]
        + ([.sockets[] | "--hold", .name]) + ["--log", $cache + "/log/" + .log];
    def holder_sockets: [{name: "control", unix: .control}] + .sockets;
'

# holder_plist ENTRY: the holder's launchd plist. Its Sockets are created by
# launchd at load and kept while the job is loaded.
holder_plist() {
  printf '%s\n' "$1" | jq -r --arg home "$HOME" --arg cache "$CACHE_ROOT" "$HOLDER_JQ"'
    def xml: gsub("&"; "&amp;") | gsub("<"; "&lt;") | gsub(">"; "&gt;");
    holder_argv as $argv
    | holder_sockets as $socks
    | ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
       "<!-- Rendered by bin/requirements.sh holder_plist from the \(.id | xml) entry; it never changes with the services. -->",
       "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">",
       "<plist version=\"1.0\">", "<dict>",
       "    <key>Label</key>", "    <string>\(.label | xml)</string>",
       "    <key>ProgramArguments</key>", "    <array>",
       ($argv[] | "        <string>\(home | xml)</string>"),
       "    </array>",
       "    <key>Sockets</key>", "    <dict>",
       ($socks[] |
         "        <key>\(.name | xml)</key>", "        <dict>",
         (if .unix then
            "            <key>SockPathName</key>", "            <string>\(.unix | home | xml)</string>",
            "            <key>SockPathMode</key>", "            <integer>384</integer>"
          else
            "            <key>SockNodeName</key>", "            <string>\(.tcp | xml)</string>",
            "            <key>SockServiceName</key>", "            <string>0</string>",
            "            <key>SockFamily</key>", "            <string>IPv4</string>"
          end),
         "        </dict>"),
       "    </dict>",
       "    <key>RunAtLoad</key>", "    <true/>",
       "    <key>KeepAlive</key>", "    <true/>",
       "</dict>", "</plist>"] | .[]'
}

# holder_service_unit ENTRY: the holder's systemd service, which takes every
# socket unit of the holder (holder_socket_unit).
holder_service_unit() {
  printf '%s\n' "$1" | jq -r --arg home "$HOME" --arg cache "$CACHE_ROOT" "$HOLDER_JQ"'
    holder_argv as $argv
    | ["# Rendered by bin/requirements.sh holder_service_unit from the \(.id) entry; it never changes with the services.",
       "[Unit]", "Description=agent-repl socket holder \(.label)",
       "", "[Service]",
       "ExecStart=\($argv | map(home) | join(" "))",
       "Sockets=\(([("control"), (.sockets[] | .name)] | map("\($ENV.HOLDER_LABEL)-" + . + ".socket")) | join(" "))",
       "Restart=always",
       "", "[Install]", "WantedBy=default.target"] | .[]'
}

# holder_socket_unit ENTRY NAME: the systemd socket unit of the holder\'s socket
# NAME ("control" for its control socket).
holder_socket_unit() {
  printf '%s\n' "$1" | jq -r --arg home "$HOME" --arg cache "$CACHE_ROOT" --arg name "$2" "$HOLDER_JQ"'
    (holder_sockets | map(select(.name == $name)) | .[0]) as $s
    | ["# Rendered by bin/requirements.sh holder_socket_unit from the \(.id) entry; it never changes with the services.",
       "[Socket]",
       (if $s.unix then "ListenStream=\($s.unix | home)" else "ListenStream=\($s.tcp):0" end),
       "FileDescriptorName=\($s.name)",
       (if $s.unix then "SocketMode=0600" else empty end),
       "Service=\(.label).service",
       "", "[Install]", "WantedBy=sockets.target"] | .[]'
}

# holder_names ENTRY: the holder's socket names, its control socket's first.
holder_names() { printf '%s\n' "$1" | jq -r --arg home "$HOME" --arg cache "$CACHE_ROOT" "$HOLDER_JQ"' holder_sockets[] | .name'; }

# holder_definitions ENTRY: one "PATH<tab>NAME" line per definition file the
# holder renders; NAME is empty for the plist or the service unit.
holder_definitions() {
  local e="$1" label def dir name
  label="$(field "$e" .label)"
  def="$(service_definition_path "$label")"
  printf '%s\t\n' "$def"
  if [ "$(platform)" = linux ]; then
    dir="$(dirname "$def")"
    while IFS= read -r name; do printf '%s\t%s\n' "$dir/$label-$name.socket" "$name"; done < <(holder_names "$e")
  fi
}

# holder_render ENTRY NAME: the rendering of one holder definition file.
holder_render() {
  if [ "$(platform)" = darwin ]; then holder_plist "$1"
  elif [ -z "$2" ]; then HOLDER_LABEL="$(field "$1" .label)" holder_service_unit "$1"
  else holder_socket_unit "$1" "$2"; fi
}

check_socket_holder() {
  local e="$1" label path name stale=""
  label="$(field "$e" .label)"
  FIX="build sockethold with bin/build-frontend.sh, install the holder's definitions, and load $label"
  while IFS=$'\t' read -r path name; do
    if [ ! -f "$path" ] || ! holder_render "$e" "$name" | cmp -s - "$path"; then stale="$stale $path"; fi
  done < <(holder_definitions "$e")
  if [ ! -x "$CACHE_ROOT/bin/sockethold" ]; then
    STATUS=missing; DETAIL="$CACHE_ROOT/bin/sockethold is not built"
  elif [ -n "$stale" ]; then
    STATUS=outdated; DETAIL="the holder's definitions differ from the $(field "$e" .id) entry's rendering:$stale"
  elif ! service_held "$label"; then
    STATUS=missing; DETAIL="$label is not loaded under the service manager"
  else
    STATUS=ok; DETAIL="$label holds its sockets"
  fi
}

install_socket_holder() {
  local e="$1" label path name uid
  label="$(field "$e" .label)"
  if in_linked_worktree "$MODULE_DIR"; then
    err "install: $(field "$e" .id): not installed from the linked worktree $MODULE_DIR; the holders every workspace shares are installed from the main checkout ($(module_main_worktree)) only"
    return 1
  fi
  # launchd and systemd create the held sockets, never the directory they sit in.
  mkdir -p "$CACHE_ROOT/bin" "$CACHE_ROOT/log" "$CACHE_ROOT/sock"
  "$BUILD_FRONTEND" sockethold
  while IFS=$'\t' read -r path name; do
    mkdir -p "$(dirname "$path")"
    holder_render "$e" "$name" >"$path"
  done < <(holder_definitions "$e")
  if [ "$(platform)" = darwin ]; then
    # A loaded holder is reloaded only here, when its definition changed on
    # purpose: that is the one moment its sockets close.
    service_load "$label" || { err "install: $(field "$e" .id): $SERVICE_FAILED_STEP failed"; return 1; }
  else
    service_step systemctl "$SERVICE_SYSTEMCTL" --user daemon-reload || return 1
    while IFS= read -r name; do
      service_step systemctl "$SERVICE_SYSTEMCTL" --user enable --now "$label-$name.socket" || return 1
    done < <(holder_names "$e")
    service_step systemctl "$SERVICE_SYSTEMCTL" --user enable "$label.service" || return 1
    service_step systemctl "$SERVICE_SYSTEMCTL" --user restart "$label.service" || return 1
  fi
}

# service_files ENTRY: one "FROM<tab>TO" line per file a service copies from
# the module into the cache.
service_files() { printf '%s\n' "$1" | jq -r '(.files // [])[] | "\(.from)\t\(.to)"'; }

check_service() {
  local e="$1" label command binary dest from to stale=""
  label="$(field "$e" .label)"
  command="$(field "$e" .command)"
  dest="$(service_definition "$e")"
  if [ -n "$command" ]; then
    FIX="install $command, install $dest, and start $label"
  else
    binary="$CACHE_ROOT/bin/$(field "$e" .binary)"
    FIX="build $(field "$e" .build_target) with bin/build-frontend.sh, install $dest, and start $label"
  fi
  while IFS=$'\t' read -r from to; do
    [ -n "$from" ] || continue
    cmp -s "$MODULE_DIR/$from" "$CACHE_ROOT/$to" || stale="$stale $to"
  done < <(service_files "$e")
  if [ -n "$command" ] && ! find_program "$command" >/dev/null; then
    STATUS=missing; DETAIL="$command is not on PATH"
  elif [ -z "$command" ] && [ ! -x "$binary" ]; then
    STATUS=missing; DETAIL="$binary is not built"
  elif [ ! -f "$dest" ]; then
    STATUS=missing; DETAIL="$dest is not installed"
  elif ! rendered_service "$e" | cmp -s - "$dest"; then
    # THE DIFFERENCE IS NAMED, so an install it causes says why.
    local differs
    # diff answers 1 for files that differ, which is this branch's case.
    differs="$(rendered_service "$e" | diff "$dest" - | grep -E '^[<>]' | sed -n '1,2p' | tr '\n' ' ' || true)"
    STATUS=outdated; DETAIL="$dest differs from the $(field "$e" .id) entry's rendering: ${differs% }"
  elif [ -n "$stale" ]; then
    STATUS=outdated; DETAIL="$CACHE_ROOT lacks the module's copy of$stale"
  elif ! service_held "$label"; then
    STATUS=missing; DETAIL="$label is not running under the service manager"
  else
    STATUS=ok; DETAIL="$label running from $dest"
  fi
}

# download_dir ENTRY: the cache dir a download entry's files are unpacked into.
download_dir() { printf '%s/%s\n' "$CACHE_ROOT" "$(field "$1" .dest)"; }

# download_files ENTRY: one "FROM<tab>TO<tab>SHA256" line per pinned file.
download_files() { printf '%s\n' "$1" | jq -r '.files[] | "\(.from)\t\(.to)\t\(.sha256)"'; }

# sha256_of FILE: FILE's sha256, as the manifests pin it.
sha256_of() { shasum -a 256 "$1" | cut -d' ' -f1; }

check_download() {
  local e="$1" dir from to want missing="" stale=""
  dir="$(download_dir "$e")"
  FIX="download $(field "$e" .url), check each file's sha256, and unpack it into $dir"
  while IFS=$'\t' read -r from to want; do
    if [ ! -f "$dir/$to" ]; then
      missing="$missing $to"
    elif [ "$(sha256_of "$dir/$to")" != "$want" ]; then
      stale="$stale $to"
    fi
  done < <(download_files "$e")
  if [ -n "$missing" ]; then
    STATUS=missing; DETAIL="$dir lacks$missing"
  elif [ -n "$stale" ]; then
    STATUS=outdated; DETAIL="$dir holds another copy of$stale than the entry pins"
  else
    STATUS=ok; DETAIL="$dir holds every pinned file"
  fi
}

check_entry() {
  STATUS=""; DETAIL=""; FIX=""
  "check_$(field "$1" .kind)" "$1"
}

# ---------------------------------------------------------------------------
# Installs. install_<kind> ENTRY acts on an entry check_entry found not ok,
# and answers non-zero when nothing could be done (a manual step).
# ---------------------------------------------------------------------------

install_program() {
  local e="$1" brew tap cask goi pm pkgs
  pm="$(package_manager)"
  goi="$(field "$e" .install.go_install)"
  if [ "$pm" != brew ]; then
    pkgs="$(packages "$e" .install "$pm")"
    if [ -n "$pm" ] && [ -n "$pkgs" ]; then
      # shellcheck disable=SC2086 # the packages are one word each
      os_install "$pm" $pkgs
    elif [ -n "$goi" ]; then
      install_go "$goi"
    elif [ -n "$(field "$e" .install.release.file)" ]; then
      install_release "$e"
    else
      return 1
    fi
    return
  fi
  brew="$(field "$e" .install.brew)"
  tap="$(field "$e" .install.tap)"
  cask="$(field "$e" .install.brew_cask)"
  if [ -n "$brew" ]; then
    local options=()
    while IFS= read -r o; do [ -n "$o" ] && options+=("$o"); done < <(printf '%s\n' "$e" | jq -r '.install.options // [] | .[]')
    if [ -n "$tap" ]; then brew tap "$tap"; fi
    if brew list --formula "$brew" >/dev/null 2>&1; then
      brew upgrade "$brew"
    else
      brew install "$brew" ${options[@]+"${options[@]}"}
    fi
  elif [ -n "$cask" ]; then
    brew install --cask "$cask"
  elif [ -n "$goi" ]; then
    install_go "$goi"
  else
    return 1
  fi
}

# install_go MODULE@VERSION: go-install a program into the user's bin dir, the
# one find_program looks in beside PATH. Go's own default, ~/go/bin, is on no
# PATH a fresh Linux machine sets, so a program installed there read missing
# after its install (Lima VM, 2026-11-16).
install_go() {
  mkdir -p "$BIN_DIR"
  GOBIN="$BIN_DIR" go install "$1"
}

# install_release ENTRY: install the program from the release its entry pins
# for this machine: download the archive, check it against the pinned sha256,
# unpack it, and put its `file` into the user's bin dir under the entry's
# command name. Nothing reaches the bin dir unless the archive is the one
# pinned: a changed upstream copy is an entry to re-pin, never a file to run.
install_release() {
  local e="$1" id m url want have file cmd work
  id="$(field "$e" .id)"
  m="$(machine)"
  url="$(field "$e" ".install.release[\"$m\"].url")"
  want="$(field "$e" ".install.release[\"$m\"].sha256")"
  file="$(field "$e" .install.release.file)"
  cmd="$(field "$e" .command)"
  if [ -z "$url" ]; then
    err "install: $id: no release is pinned for $m; add install.release.$m to its manifest entry"; return 1
  fi
  work="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-release.XXXXXX")" || { err "install: $id: no temporary directory could be made"; return 1; }
  if ! curl -fsSL --retry 2 -o "$work/archive.tar.gz" "$url"; then
    err "install: $id: downloading $url failed"; rm -rf "$work"; return 1
  fi
  have="$(sha256_of "$work/archive.tar.gz")"
  if [ "$have" != "$want" ]; then
    err "install: $id: $url has sha256 $have, not the pinned $want; the upstream copy changed, so re-pin the entry"; rm -rf "$work"; return 1
  fi
  mkdir -p "$work/unpacked"
  if ! tar -xzf "$work/archive.tar.gz" -C "$work/unpacked"; then
    err "install: $id: $url is not a tar.gz tar can unpack"; rm -rf "$work"; return 1
  fi
  if [ ! -f "$work/unpacked/$file" ]; then
    err "install: $id: $url holds no $file; fix the entry's install.release.file"; rm -rf "$work"; return 1
  fi
  mkdir -p "$BIN_DIR"
  # Written beside its final name and renamed, so a running copy is never
  # truncated and a failed copy never leaves half a program.
  if ! cp "$work/unpacked/$file" "$BIN_DIR/.$cmd.new" || ! chmod 755 "$BIN_DIR/.$cmd.new" || ! mv -f "$BIN_DIR/.$cmd.new" "$BIN_DIR/$cmd"; then
    err "install: $id: installing $file as $BIN_DIR/$cmd failed"; rm -f "$BIN_DIR/.$cmd.new"; rm -rf "$work"; return 1
  fi
  rm -rf "$work"
  say "install: $id: installed $BIN_DIR/$cmd from $url"
}

install_emacs() {
  if [ -n "$(field "$1" .install.source.repo)" ]; then
    install_emacs_source "$1"
  else
    install_program "$1"
  fi
}

# expand_placeholders ARG: ARG with its placeholders replaced, so a manifest
# names a path or a version without naming a machine's: `{home}` is the home
# directory, `{gcc-major}` the major version of the gcc on PATH, `{brew}`
# Homebrew's prefix, `{brew:FORMULA}` that formula's, and
# `{claude-config-roots}` every Claude config root, comma-separated
# (claude_config_roots).
expand_placeholders() {
  local arg="$1" out="" name
  if [[ "$arg" == *"{claude-config-roots}"* ]]; then
    local roots
    roots="$(claude_config_roots | paste -sd, -)"
    arg="${arg//\{claude-config-roots\}/$roots}"
  fi
  if [[ "$arg" == *"{gcc-major}"* ]]; then
    local major
    major="$(gcc -dumpversion)" || { err "expand: $arg names {gcc-major}, and gcc -dumpversion failed"; return 1; }
    arg="${arg//\{gcc-major\}/${major%%.*}}"
  fi
  arg="${arg//\{home\}/$HOME}"
  while [[ "$arg" == *"{brew"* ]]; do
    out="$out${arg%%\{brew*}"
    arg="${arg#*\{brew}"
    if [ "${arg:0:1}" = ":" ]; then
      name="${arg:1}"; name="${name%%\}*}"
      out="$out$(brew --prefix "$name")"
      arg="${arg#*\}}"
    else
      out="$out$(brew --prefix)"
      arg="${arg#\}}"
    fi
  done
  printf '%s\n' "$out$arg"
}

# install_source_deps ENTRY ID: install the build dependencies the platform
# block names for this machine's package manager. On macOS each Homebrew
# formula not yet installed is installed; on Linux one install of them all
# runs, the dependencies naming `{gcc-major}` after the rest, since gcc is
# itself among the rest on a machine that has none yet.
install_source_deps() {
  local e="$1" id="$2" pm dep first=() later=()
  pm="$(package_manager)"
  if [ "$pm" = brew ]; then
    while IFS= read -r dep; do
      [ -n "$dep" ] || continue
      if ! brew list --formula "$dep" >/dev/null 2>&1; then
        brew install "$dep" || { err "install: $id: brew install $dep failed"; return 1; }
      fi
    done < <(printf '%s\n' "$e" | jq -r '.install.source.darwin.deps.brew // [] | .[]')
    return
  fi
  [ -n "$pm" ] || { err "install: $id: no package manager install knows ($LINUX_PACKAGE_MANAGERS) is on PATH for its build dependencies"; return 1; }
  if [ "$(printf '%s\n' "$e" | jq -r --arg pm "$pm" '.install.source.linux.deps[$pm] // empty | length')" = "" ]; then
    err "install: $id: no $pm build dependencies are declared for it; add install.source.linux.deps.$pm to its manifest entry"
    return 1
  fi
  while IFS= read -r dep; do
    [ -n "$dep" ] || continue
    case "$dep" in
      *"{gcc-major}"*) later+=("$dep") ;;
      *) first+=("$dep") ;;
    esac
  done < <(printf '%s\n' "$e" | jq -r --arg pm "$pm" '.install.source.linux.deps[$pm] // [] | .[]')
  if [ "${#first[@]}" -gt 0 ]; then
    os_install "$pm" "${first[@]}" || return 1
  fi
  if [ "${#later[@]}" -gt 0 ]; then
    local expanded=()
    local x
    for dep in "${later[@]}"; do
      x="$(expand_placeholders "$dep")" || return 1
      expanded+=("$x")
    done
    os_install "$pm" "${expanded[@]}" || return 1
  fi
}

# install_emacs_source ENTRY: build Emacs from the entry's source repository at
# its pinned revision, with the platform block's dependencies and configure
# arguments. Nothing installed is touched until the build has succeeded: on
# macOS the app it makes replaces install.source.darwin.app, the app it
# replaces moved aside, never deleted; on Linux `make install` installs into
# the configure prefix only once `make` has built everything.
install_emacs_source() {
  local e="$1" id repo revision dir built backup jobs plat app
  id="$(field "$e" .id)"
  plat="$(platform)"
  repo="$(field "$e" .install.source.repo)"
  revision="$(field "$e" .revision)"
  app="$(field "$e" ".install.source.$plat.app")"
  dir="$CACHE_ROOT/src/$(basename "$repo" .git)"
  install_source_deps "$e" "$id" || return 1
  local args=() a x
  while IFS= read -r a; do
    x="$(expand_placeholders "$a")" || return 1
    args+=("$x")
  done < <(printf '%s\n' "$e" | jq -r --arg p "$plat" '.install.source[$p].configure[]')
  mkdir -p "$dir"
  if [ ! -d "$dir/.git" ]; then
    git -C "$dir" init -q || { err "install: $id: git init in $dir failed"; return 1; }
  fi
  git -C "$dir" fetch -q --depth 1 "$repo" "$revision" || { err "install: $id: fetching $revision from $repo failed"; return 1; }
  git -C "$dir" checkout -q --force "$revision" || { err "install: $id: checking out $revision in $dir failed"; return 1; }
  jobs="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)"
  (cd "$dir" && ./autogen.sh) || { err "install: $id: autogen.sh in $dir failed"; return 1; }
  (cd "$dir" && ./configure ${args[@]+"${args[@]}"}) || { err "install: $id: configure in $dir failed; config.log there says why"; return 1; }
  (cd "$dir" && make -j"$jobs") || { err "install: $id: make in $dir failed"; return 1; }
  (cd "$dir" && make install) || { err "install: $id: make install in $dir failed"; return 1; }
  if [ "$plat" != darwin ]; then
    say "install: $id: installed Emacs built from $revision into its configure prefix; restart Emacs to run it"
    return 0
  fi
  built="$dir/nextstep/Emacs.app"
  [ -d "$built" ] || { err "install: $id: the build made no $built"; return 1; }
  if [ -e "$app" ]; then
    backup="${app%.app}-$(date +%Y%m%d-%H%M%S).app"
    mv "$app" "$backup" || { err "install: $id: moving $app aside to $backup failed"; return 1; }
    say "install: $id: moved the previous $app aside to $backup"
  fi
  mkdir -p "$(dirname "$app")"
  cp -R "$built" "$app" || { err "install: $id: copying $built to $app failed"; return 1; }
  say "install: $id: installed $app, built from $revision; restart Emacs to run it"
}

install_env() { return 1; }

install_group() { return 1; }

install_notification_permission() {
  local cmd
  cmd="$(field "$1" .command)"
  command -v "$cmd" >/dev/null 2>&1 || return 1
  # The first banner is what makes macOS ask; its failure is the not-yet-allowed
  # state the re-check reports.
  "$cmd" -title "agent-repl" -message "agent-repl can now post desktop banners" -group agent-repl:requirements >/dev/null 2>&1 \
    || say "install: $cmd's first banner was refused; macOS asks once, and System Settings → Notifications allows it"
  return 0
}

install_app_build() {
  local e="$1"
  # </dev/null: the install loop reads its entries from stdin, which a build
  # must never consume.
  "$(app_build_script "$e")" "$(app_path "$e")" </dev/null
}

install_location_permission() {
  local app
  app="$(app_path "$1")"
  [ -d "$app" ] || return 1
  # The helper's authorize verb is what makes macOS ask; it waits for the
  # owner's answer, so it is left running, and the re-check reports the
  # permission until it is given.
  open -n -g -a "$app" --args authorize --out "$(mktemp "${TMPDIR:-/tmp}/agent-repl-wifi-authorize.XXXXXX")" </dev/null >/dev/null 2>&1 \
    || say "install: $app could not ask for Location Services"
  return 0
}
# install_signing_identity: a self-signed certificate for code signing alone,
# with its key, imported into the login keychain with codesign allowed to use
# the key, so a build signs with it unprompted.
install_signing_identity() {
  local name dir rc=0
  name="$(field "$1" .name)"
  dir="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-signing.XXXXXX")"
  printf '[req]\ndistinguished_name=dn\nx509_extensions=ext\nprompt=no\n[dn]\nCN=%s\n[ext]\nbasicConstraints=critical,CA:false\nkeyUsage=critical,digitalSignature\nextendedKeyUsage=critical,codeSigning\n' "$name" >"$dir/cert.cnf"
  openssl req -x509 -newkey rsa:2048 -nodes -days 36500 -keyout "$dir/key.pem" -out "$dir/cert.pem" -config "$dir/cert.cnf" </dev/null >/dev/null 2>&1 \
    && openssl pkcs12 -export -legacy -name "$name" -inkey "$dir/key.pem" -in "$dir/cert.pem" -out "$dir/identity.p12" -passout pass:agent-repl </dev/null >/dev/null 2>&1 \
    && security import "$dir/identity.p12" -k "$(security login-keychain </dev/null | tr -d ' "')" -P agent-repl -T /usr/bin/codesign </dev/null >/dev/null 2>&1 \
    || { say "install: the signing identity \"$name\" could not be made"; rc=1; }
  # The key carries the identity's name as its label (-name above), which is
  # how the partition list finds it.
  # THE KEY'S PARTITION LIST names the tools that may use it unprompted. An
  # import's -T alone leaves it unset, so macOS asked for the login keychain's
  # password at every codesign (2026-11-18). Setting it needs that password
  # once, which security asks for itself.
  if [ "$rc" = 0 ]; then
    security set-key-partition-list -S apple-tool:,apple:,codesign: -s -l "$name" "$(security login-keychain </dev/null | tr -d ' "')" >/dev/null \
      || { say "install: the signing identity \"$name\" was made, but codesign could not be allowed to use its key unprompted"; rc=1; }
  fi
  rm -rf "$dir"
  return "$rc"
}

# A grant needs the user's password, which install never asks for.
install_sudo_grant() { return 1; }

install_skill_link() {
  local e="$1" src dir name
  name="$(field "$e" .name)"
  if [ "$(field "$e" .retired)" = true ]; then
    src="$(link_source "$e")"
    while IFS= read -r dir; do
      # Only a link of ours comes out; a file or a foreign link is the user's.
      if [ -L "$dir/$name" ] && { [ "$(readlink "$dir/$name")" = "$src" ] || [ ! -e "$dir/$name" ]; }; then
        rm -f "$dir/$name"
      fi
    done < <(skills_dirs)
    return
  fi
  src="$(link_source "$e")"
  [ -n "$src" ] && [ -e "$src" ] || return 1
  while IFS= read -r dir; do
    mkdir -p "$dir"
    ln -sfn "$src" "$dir/$name"
  done < <(skills_dirs)
}

install_command_link() {
  local e="$1" src
  src="$(link_source "$e")"
  [ -n "$src" ] && [ -e "$src" ] || return 1
  mkdir -p "$BIN_DIR"
  ln -sfn "$src" "$BIN_DIR/$(field "$e" .name)"
}

install_git_hook() {
  local e="$1"
  hook_paths "$e"
  if [ -f "$HOOK_DEST" ] && ! is_managed_hook "$e" "$HOOK_DEST"; then return 1; fi
  mkdir -p "$(dirname "$HOOK_DEST")"
  cp "$HOOK_SRC" "$HOOK_DEST"
  chmod +x "$HOOK_DEST"
}

# write_service_definition ENTRY: the directories the service runs in, the
# files the entry ships beside its program, and its rendered definition, which
# the service manager loads next. install's step, and the daemon's when a
# deploy finds a service unloaded with no definition (define-service).
write_service_definition() {
  local e="$1" dest from to
  dest="$(service_definition "$e")"
  # No service manager creates these, and the services bind sockets and open
  # their logs and databases in them.
  mkdir -p "$CACHE_ROOT/bin" "$CACHE_ROOT/log" "$CACHE_ROOT/sock" "$CACHE_ROOT/store" "$(dirname "$dest")"
  while IFS=$'\t' read -r from to; do
    [ -n "$from" ] || continue
    mkdir -p "$(dirname "$CACHE_ROOT/$to")"
    cp "$MODULE_DIR/$from" "$CACHE_ROOT/$to"
  done < <(service_files "$e")
  rendered_service "$e" >"$dest"
}

# cmd_define_service LABEL: write the definition of the service LABEL names,
# from its entry, without building or loading anything. The daemon's deploy
# runs it when the service manager holds no LABEL and no definition exists (a
# service added since this machine's install), then starts the service from it.
cmd_define_service() {
  local label="$1" e
  e="$(printf '%s\n' "$ENTRIES" | jq -c --arg l "$label" --arg p "$(platform)" '
    select(.kind == "service" and .label == $l)
    | select((.platforms == null) or (.platforms | index($p)))')"
  if [ -z "$e" ]; then
    err "define-service: no service entry on $(platform) is labelled $label"; return 1
  fi
  write_service_definition "$e"
  say "define-service: wrote $(service_definition "$e")"
}

# cmd_ensure_holder ID: install the socket holder entry ID, the daemon's step
# when a probe of the holder's control socket finds no holder serving it (a
# holder added since this machine's install). It is install_socket_holder, so
# a holder whose definitions match its rendering is never reloaded.
cmd_ensure_holder() {
  local id="$1" e
  e="$(printf '%s\n' "$ENTRIES" | jq -c --arg id "$id" --arg p "$(platform)" '
    select(.kind == "socket_holder" and .id == $id)
    | select((.platforms == null) or (.platforms | index($p)))')"
  if [ -z "$e" ]; then
    err "ensure-holder: no socket_holder entry on $(platform) has the id $id"; return 1
  fi
  # A HOLDER ALREADY UP TO DATE AND LOADED IS LEFT ALONE: a reload is the one
  # moment its sockets close, and its probe failing is then the holder's own
  # failure, which the caller's take reports.
  check_socket_holder "$e"
  if [ "$STATUS" = ok ]; then
    say "ensure-holder: $(field "$e" .label) is already loaded; nothing was reloaded"
    return 0
  fi
  install_socket_holder "$e" || return 1
  say "ensure-holder: $(field "$e" .label) is loaded"
}

install_service() {
  local e="$1" label dest
  label="$(field "$e" .label)"
  dest="$(service_definition "$e")"
  # THE SERVICES ARE EVERY WORKSPACE'S (owner report, 2026-11-14): an install
  # run from a linked worktree rebuilt and restarted the store from that
  # worktree's source, which severed every session at once. They are installed
  # from the main checkout only.
  if in_linked_worktree "$MODULE_DIR"; then
    err "install: $(field "$e" .id): not installed from the linked worktree $MODULE_DIR; the services every workspace shares are installed from the main checkout ($(module_main_worktree)) only"
    return 1
  fi
  if [ -n "$(field "$e" .command)" ]; then
    if ! find_program "$(field "$e" .command)" >/dev/null; then
      err "install: $(field "$e" .id): $(field "$e" .command) is not on PATH; install it first"; return 1
    fi
  else
    mkdir -p "$CACHE_ROOT/bin"
    "$BUILD_FRONTEND" "$(field "$e" .build_target)"
  fi
  write_service_definition "$e"
  # The service manager reads the definition just written, keeps the service
  # at every login, and runs it now from that definition (lib-service.sh).
  service_load "$label" || { err "install: $(field "$e" .id): $SERVICE_FAILED_STEP failed"; return 1; }
}

# fetch_download ENTRY WORK: download and unzip the entry's archive into WORK,
# and check every pinned file's sha256 there. Nothing reaches the cache unless
# every file is the one the entry pins: a changed upstream copy is an entry to
# re-pin, never a file to install.
fetch_download() {
  local e="$1" work="$2" id url from to want have
  id="$(field "$e" .id)"
  url="$(field "$e" .url)"
  if ! curl -fsSL --retry 2 -o "$work/archive.zip" "$url"; then
    err "install: $id: downloading $url failed"; return 1
  fi
  if ! unzip -q "$work/archive.zip" -d "$work/unpacked"; then
    err "install: $id: $url is not a zip unzip can unpack"; return 1
  fi
  while IFS=$'\t' read -r from to want; do
    if [ ! -f "$work/unpacked/$from" ]; then
      err "install: $id: $url holds no $from; fix the entry's files"; return 1
    fi
    have="$(sha256_of "$work/unpacked/$from")"
    if [ "$have" != "$want" ]; then
      err "install: $id: $from from $url has sha256 $have, not the pinned $want; the upstream copy changed, so re-pin the entry"; return 1
    fi
  done < <(download_files "$e")
}

install_download() {
  local e="$1" dir work from to want rc=0
  dir="$(download_dir "$e")"
  work="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-download.XXXXXX")"
  if fetch_download "$e" "$work"; then
    mkdir -p "$dir"
    while IFS=$'\t' read -r from to want; do
      mv -f "$work/unpacked/$from" "$dir/$to"
    done < <(download_files "$e")
  else
    rc=1
  fi
  rm -rf "$work"
  return "$rc"
}

# ---------------------------------------------------------------------------
# Uninstall: only what this script itself put in place (links, hooks,
# services, downloads). Shared programs are never removed.
# ---------------------------------------------------------------------------

uninstall_entry() {
  local e="$1" kind dest src label uid dir from to want
  kind="$(field "$e" .kind)"
  case "$kind" in
    skill_link)
      src="$(link_source "$e")"
      while IFS= read -r dir; do
        dest="$dir/$(field "$e" .name)"
        if [ -L "$dest" ] && [ -n "$src" ] && [ "$(readlink "$dest")" = "$src" ]; then
          rm -f "$dest"; say "uninstall: removed $dest"
        fi
      done < <(skills_dirs) ;;
    command_link)
      src="$(link_source "$e")"
      dest="$BIN_DIR/$(field "$e" .name)"
      if [ -L "$dest" ] && [ -n "$src" ] && [ "$(readlink "$dest")" = "$src" ]; then
        rm -f "$dest"; say "uninstall: removed $dest"
      fi ;;
    git_hook)
      hook_paths "$e"
      if [ -e "$HOOK_DEST" ] && [ "$HOOK_SRC" -ef "$HOOK_DEST" ]; then
        say "uninstall: $HOOK_DEST is the checked-in hook (core.hooksPath); left in place"
      elif [ -f "$HOOK_DEST" ] && is_managed_hook "$e" "$HOOK_DEST"; then
        rm -f "$HOOK_DEST"; say "uninstall: removed $HOOK_DEST"
      fi ;;
    service)
      label="$(field "$e" .label)"
      dest="$(service_definition "$e")"
      if [ -f "$dest" ] || service_held "$label"; then
        if service_unload "$label"; then say "uninstall: stopped and unloaded $label"
        else err "uninstall: $SERVICE_FAILED_STEP failed"; fi
      fi
      if [ -f "$dest" ]; then rm -f "$dest"; say "uninstall: removed $dest"; fi
      service_forget || err "uninstall: $SERVICE_FAILED_STEP failed"
      if [ -n "$(field "$e" .binary)" ] && [ -f "$CACHE_ROOT/bin/$(field "$e" .binary)" ]; then
        rm -f "$CACHE_ROOT/bin/$(field "$e" .binary)"; say "uninstall: removed $CACHE_ROOT/bin/$(field "$e" .binary)"
      fi
      while IFS=$'\t' read -r from to; do
        [ -n "$from" ] || continue
        if [ -f "$CACHE_ROOT/$to" ]; then rm -f "$CACHE_ROOT/$to"; say "uninstall: removed $CACHE_ROOT/$to"; fi
      done < <(service_files "$e") ;;
    sudo_grant)
      dest="$SUDOERS_DIR/$(field "$e" .file)"
      if [ -e "$dest" ]; then
        say "uninstall: $dest is root's; remove it with \`sudo rm $dest\`"
      fi ;;
    download)
      dir="$(download_dir "$e")"
      while IFS=$'\t' read -r from to want; do
        if [ -f "$dir/$to" ]; then rm -f "$dir/$to"; say "uninstall: removed $dir/$to"; fi
      done < <(download_files "$e")
      if [ -d "$dir" ]; then rmdir "$dir" 2>/dev/null || true; fi ;;
  esac
}

# ---------------------------------------------------------------------------
# Selection and the verbs.
# ---------------------------------------------------------------------------

PROFILE=all
JSON=0

# selected: every loaded entry for this platform and profile, one per line.
selected() {
  printf '%s\n' "$ENTRIES" | jq -c --arg p "$(platform)" --arg profile "$PROFILE" '
    select((.platforms == null) or (.platforms | index($p)))
    | select($profile == "all" or .profile == $profile)'
}

# report ENTRY: one line (or one JSON object) for the entry's checked status.
REPORTS=""
report() {
  local e="$1" id optional
  id="$(field "$e" .id)"
  optional="$(field "$e" .optional)"
  if [ "$JSON" -eq 1 ]; then
    REPORTS="$REPORTS$(printf '%s\n' "$e" | jq -c --arg s "$STATUS" --arg d "$DETAIL" --arg f "$FIX" \
      '{id, kind, system: ._system, profile, optional: (.optional // false), why, status: $s, detail: $d, fix: (if $s == "ok" then "" else $f end)}')
"
    return
  fi
  if [ "$STATUS" = ok ]; then
    say "ok          $id: $DETAIL"
  elif [ "$optional" = true ]; then
    say "optional    $id ($STATUS): $DETAIL; $(field "$e" .why). To add it: $FIX"
  else
    say "$(printf '%-11s' "$STATUS") $id: $DETAIL. $(field "$e" .why). Fix: $FIX"
  fi
}

# required_failed ENTRY: 0 when the entry is required and not ok.
required_failed() {
  [ "$STATUS" != ok ] && [ "$(field "$1" .optional)" != true ]
}

cmd_check() {
  local e unmet=0
  while IFS= read -r e; do
    [ -n "$e" ] || continue
    check_entry "$e"
    report "$e"
    if required_failed "$e"; then unmet=$((unmet + 1)); fi
  done < <(selected)
  if [ "$JSON" -eq 1 ]; then printf '%s' "$REPORTS" | jq -s .; fi
  [ "$unmet" -eq 0 ]
}

cmd_install() {
  local e unmet=0 kind
  while IFS= read -r e; do
    [ -n "$e" ] || continue
    check_entry "$e"
    # Only a missing or outdated entry is installed: needs-user is the user's
    # to supply (a value, a file of theirs in the way), and failed is a broken
    # entry, which install must never paper over.
    if [ "$STATUS" = missing ] || [ "$STATUS" = outdated ]; then
      kind="$(field "$e" .kind)"
      say "install: $(field "$e" .id) is $STATUS ($DETAIL); installing"
      if "install_$kind" "$e"; then
        check_entry "$e"
      else
        say "install: $(field "$e" .id) was not installed; it needs you: $FIX"
      fi
    fi
    report "$e"
    if required_failed "$e"; then
      unmet=$((unmet + 1))
      err "install: $(field "$e" .id) is $STATUS after install: $DETAIL"
    fi
  done < <(selected)
  if [ "$unmet" -gt 0 ]; then
    err "install: $unmet required entr(y/ies) still unmet; each line above says why"
    return 1
  fi
  say "install: every required entry is met"
}

cmd_uninstall() {
  local e
  while IFS= read -r e; do
    [ -n "$e" ] || continue
    uninstall_entry "$e"
  done < <(PROFILE=all selected)
  purge_legacy_hook_entries
  say "uninstall: done"
}

cmd_list() {
  selected | jq -r '"\(.id)\t\(._system)\t\(.profile)\t\(.kind)\t\(.why)"'
}

# ---------------------------------------------------------------------------
# purge-legacy-hooks: remove this module's pre-cutover Claude Code hook
# entries from every config dir's settings.json, and the orphaned scripts.
#
# Before the sentinel cutover this module registered one hook entry per
# notify script in the user's settings.json. The cutover deleted the scripts
# and the registrar, but not what they wrote, so the remover lives on. An
# entry is ours ONLY when its command's basename is in the table below AND
# its path runs through a `hooks/` dir; every other key round-trips untouched.
# ---------------------------------------------------------------------------

LEGACY_HOOK_SCRIPTS=(
  "permission-notify.sh"
  "permission-request-notify.sh"
  "prompt-submit-notify.sh"
  "session-start-notify.sh"
  "stop-failure-notify.sh"
  "stop-notify.sh"
  "subagent-start-notify.sh"
  "subagent-stop-notify.sh"
)

_legacy_hook_settings_files() {
  local dir
  for dir in "$HOME/.claude" "$HOME"/.claude-*; do
    [ -d "$dir" ] || continue
    [ -f "$dir/settings.json" ] || continue
    echo "$dir/settings.json"
  done
}

# shellcheck disable=SC2016 # a jq program: its $names is jq's, never the shell's
_LEGACY_HOOK_JQ_PRUNE='
def ours($names):
  (. // "") as $cmd
  | ($cmd | split("/") | last) as $base
  | (($names | index($base)) != null) and ($cmd | test("/hooks/"));
if has("hooks") and (.hooks | type) == "object" then
  .hooks |= (
    with_entries(
      .value |= (
        map(if has("hooks") and (.hooks | type) == "array"
            then .hooks |= map(select((.command | ours($names)) | not))
            else . end)
        | map(select(((.hooks // []) | length) > 0))
      )
    )
    | with_entries(select((.value | length) > 0))
  )
else . end'

# shellcheck disable=SC2016 # a jq program: its $names is jq's, never the shell's
_LEGACY_HOOK_JQ_LIST='
def ours($names):
  (. // "") as $cmd
  | ($cmd | split("/") | last) as $base
  | (($names | index($base)) != null) and ($cmd | test("/hooks/"));
(.hooks // {}) | to_entries[] | .key as $event
| .value[]? | .hooks[]?
| select(.command | ours($names))
| "\($event)\t\(.command)"'

purge_legacy_hook_entries() {
  say "purge-hooks: removing this module's pre-cutover settings.json hook entries"
  local names_json settings removed backup tmp total=0
  names_json="$(printf '%s\n' "${LEGACY_HOOK_SCRIPTS[@]}" | jq -R . | jq -s .)"
  while IFS= read -r settings; do
    [ -n "$settings" ] || continue
    if ! jq -e . "$settings" >/dev/null 2>&1; then
      err "purge-hooks: $settings is not valid JSON; left untouched"
      return 1
    fi
    removed="$(jq -r --argjson names "$names_json" "$_LEGACY_HOOK_JQ_LIST" "$settings")"
    if [ -z "$removed" ]; then
      say "purge-hooks: $settings: no legacy entries"
      continue
    fi
    backup="$settings.pre-purge-$(date +%Y%m%d-%H%M%S)"
    cp "$settings" "$backup"
    tmp="$settings.purge-tmp-$$"
    jq --indent 2 --argjson names "$names_json" "$_LEGACY_HOOK_JQ_PRUNE" "$settings" >"$tmp"
    mv "$tmp" "$settings"
    say "purge-hooks: $settings: backed up to $backup"
    printf '%s\n' "$removed" | while IFS= read -r line; do say "purge-hooks:   removed: $line"; done
    total=$((total + $(printf '%s\n' "$removed" | wc -l | tr -d ' ')))
  done < <(_legacy_hook_settings_files)
  # The scripts go only after the entries invoking them, so no registered hook
  # ever points at a deleted file.
  local dir script
  for dir in "$HOME/.claude" "$HOME"/.claude-*; do
    [ -d "$dir/hooks" ] || continue
    for script in "${LEGACY_HOOK_SCRIPTS[@]}"; do
      if [ -f "$dir/hooks/$script" ]; then
        rm -f "$dir/hooks/$script"
        say "purge-hooks: removed orphaned script $dir/hooks/$script"
      fi
    done
  done
  say "purge-hooks: done, $total entr(y/ies) removed"
}

# ---------------------------------------------------------------------------
# Dispatch.
# ---------------------------------------------------------------------------

main() {
  [ "$#" -ge 1 ] || die_usage "no verb given"
  local verb="$1" label=""
  shift
  if [ "$verb" = define-service ]; then
    [ "$#" -ge 1 ] || die_usage "define-service needs a service label"
    label="$1"
    shift
  fi
  if [ "$verb" = ensure-holder ]; then
    [ "$#" -ge 1 ] || die_usage "ensure-holder needs a socket holder's id"
    label="$1"
    shift
  fi
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --profile)
        [ "$#" -ge 2 ] || die_usage "--profile needs run, dev or all"
        case "$2" in run|dev|all) PROFILE="$2" ;; *) die_usage "--profile must be run, dev or all, not $2" ;; esac
        shift 2 ;;
      --json) JSON=1; shift ;;
      -h|--help) usage; exit 0 ;;
      *) die_usage "unknown argument: $1" ;;
    esac
  done
  case "$verb" in
    check|install|uninstall|list|define-service|ensure-holder) resolve_package_manager ;;
  esac
  case "$verb" in
    check) load_manifests; prepare_entries; cmd_check ;;
    install) [ "$JSON" -eq 0 ] || die_usage "--json is for check"; load_manifests; prepare_entries; cmd_install ;;
    uninstall) load_manifests; prepare_entries; cmd_uninstall ;;
    list) load_manifests; cmd_list ;;
    define-service) load_manifests; prepare_entries; cmd_define_service "$label" ;;
    ensure-holder) load_manifests; prepare_entries; cmd_ensure_holder "$label" ;;
    purge-legacy-hooks) command -v jq >/dev/null 2>&1 || { err "purge-hooks: jq is not installed; settings.json is never edited without it"; exit 2; }; purge_legacy_hook_entries ;;
    help|-h|--help) usage ;;
    *) die_usage "unknown verb: $verb" ;;
  esac
}

if [ "${REQUIREMENTS_SH_LIB:-}" != 1 ]; then
  main "$@"
fi

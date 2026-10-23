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
#   emacs                    the emacs on PATH at a minimum version, with features
#   env                      an environment variable the user sets
#   notification_permission  macOS lets a banner program notify
#   skill_link               a Claude Code skill linked into the skills directory of
#                            every Claude config root agent-repl runs sessions under
#                            (`"retired": true`: our link is removed instead)
#   git_hook                 a managed git hook in the repository's hooks dir
#   launchd_service          a launchd user agent installed and running: its
#                            program built here (`build_target` + `binary`) or
#                            an installed command (`command`), with any
#                            `files` copied from the module into the cache
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
# Test seams (bin/test-requirements.sh): AGENT_REPL_REQUIREMENTS_ROOT (the
# module dir), AGENT_REPL_REQUIREMENTS_PLATFORM, AGENT_REPL_REQUIREMENTS_BUILD
# (bin/build-frontend.sh), AGENT_REPL_LAUNCHCTL, and HOME / PATH.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="${AGENT_REPL_REQUIREMENTS_ROOT:-$(cd "$THIS_DIR/.." && pwd -P)}"
ROOT_MANIFEST="$MODULE_DIR/requirements.json"
BUILD_FRONTEND="${AGENT_REPL_REQUIREMENTS_BUILD:-$MODULE_DIR/bin/build-frontend.sh}"
LAUNCHCTL="${AGENT_REPL_LAUNCHCTL:-launchctl}"
CACHE_ROOT="${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl"
SKILLS_DIR="$HOME/.claude/skills"

# skills_dirs: every skills directory a skill link is kept in, one per line.
# A session runs under ~/.claude, or, for a workspace under MULTI_REPO_ROOT,
# under the work account's config root MULTI_REPO_CONFIG_DIR (the same value
# Emacs passes the daemon as --multi-repo-config-dir). A skill linked into the
# first alone is missing from every session of the second.
skills_dirs() {
  printf '%s\n' "$SKILLS_DIR"
  if [ -n "${MULTI_REPO_CONFIG_DIR:-}" ]; then
    printf '%s/skills\n' "$MULTI_REPO_CONFIG_DIR"
  fi
}
LAUNCH_AGENTS_DIR="$HOME/Library/LaunchAgents"

# ---------------------------------------------------------------------------
# Reporting. THE ONE OUTPUT PATH: every line this script prints goes through
# say (progress, to stdout) or err (an error, to stderr, always naming the
# operation and the entry), so the run's record reads from one place.
# ---------------------------------------------------------------------------

say() { printf '[requirements] %s\n' "$*"; }
err() { printf '[requirements] ERROR: %s\n' "$*" >&2; }

usage() {
  sed -n '5,11p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
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

# shellcheck source=lib-worktrees.sh
. "$THIS_DIR/lib-worktrees.sh"

# The repository's MAIN worktree. A skill link must never point into a linked
# worktree, whose path dangles the moment it is pruned, so repo-relative skill
# sources always resolve here, whichever worktree runs the script.
module_main_worktree() {
  main_worktree "$MODULE_DIR" || { err "main-worktree: git worktree list answered no worktree for $MODULE_DIR"; exit 2; }
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

# The manifest contract, checked before anything runs.
validate_entries() {
  local problems
  problems="$(printf '%s\n' "$1" | jq -rs '
    def need($e; $fields): $fields[] as $f | select($e[$f] == null) | "\($e._file): \($e.id // "?"): \($e.kind) needs \"\($f)\"";
    (.[] | . as $e |
      (["id", "kind", "why", "profile"][] as $f | select($e[$f] == null) | "\($e._file): \($e.id // "?"): every entry needs \"\($f)\""),
      (select(($e.profile != "run") and ($e.profile != "dev")) | "\($e._file): \($e.id): profile must be run or dev, not \($e.profile)"),
      (if $e.kind == "program" then need($e; ["command", "install", "platforms"])
       elif $e.kind == "emacs" then need($e; ["command", "min", "features", "install", "platforms"])
       elif $e.kind == "env" then need($e; ["name", "doc"])
       elif $e.kind == "notification_permission" then need($e; ["command", "platforms"])
       elif $e.kind == "skill_link" then need($e; ["name", "source"])
       elif $e.kind == "git_hook" then need($e; ["hook", "source", "markers"])
       elif $e.kind == "launchd_service" then need($e; ["label", "plist", "platforms"]),
         (select((($e.command != null) and (($e.build_target != null) or ($e.binary != null)))
                 or (($e.command == null) and (($e.build_target == null) or ($e.binary == null))))
           | "\($e._file): \($e.id): launchd_service needs either \"command\" or both \"build_target\" and \"binary\""),
         ($e.files // [] | .[] | select(.from == null or .to == null)
           | "\($e._file): \($e.id): every launchd_service file needs \"from\" and \"to\"")
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
field() { printf '%s\n' "$1" | jq -r "$2 // empty"; }

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

# install_text ENTRY: the install action an entry's `install` names, as text.
install_text() {
  local e="$1" how
  how="$(printf '%s\n' "$e" | jq -r '.install |
    if .brew then ((if .tap then "brew tap \(.tap) && " else "" end) + "brew install \(.brew)" + ((.options // []) | map(" " + .) | join("")))
    elif .brew_cask then "brew install --cask \(.brew_cask)"
    elif .go_install then "go install \(.go_install)"
    elif .manual then .manual
    else "no install step" end')"
  printf '%s\n' "$how"
}

check_program() {
  local e="$1" cmd path min args out have
  cmd="$(field "$e" .command)"
  FIX="$(install_text "$e")"
  if ! path="$(command -v "$cmd" 2>/dev/null)"; then
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
  if ! path="$(command -v "$cmd" 2>/dev/null)"; then
    STATUS=missing; DETAIL="$cmd is not on PATH"; return
  fi
  min="$(field "$e" .min)"
  if ! have="$("$path" --batch --eval '(princ emacs-version)' 2>&1)"; then
    STATUS=failed; DETAIL="$path --batch could not report its version: $have"; return
  fi
  lacking="$("$path" --batch --eval "(princ (mapconcat #'symbol-name (seq-remove #'featurep '($(printf '%s\n' "$e" | jq -r '.features | join(" ")'))) \" \"))" 2>&1)"
  if ! version_ge "$(first_version "$have")" "$min"; then
    STATUS=outdated; DETAIL="$path is Emacs $have, older than $min"
  elif [ -n "$lacking" ]; then
    STATUS=outdated; DETAIL="$path (Emacs $have) lacks: $lacking"
  else
    STATUS=ok; DETAIL="$path Emacs $have"
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

# skill_source ENTRY: the canonical source path of a skill link, or empty when
# its env root is unset.
skill_source() {
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
  done < <(linked_worktrees "$MODULE_DIR")
  return 1
}

# check_retired_skill_link ENTRY: a RETIRED skill is met once no link of ours
# stands at its name; a file of the user's there is theirs, and not ours to
# report.
check_retired_skill_link() {
  local e="$1" dir="$2" dest src
  dest="$dir/$(field "$e" .name)"
  src="$(skill_source "$e")"
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
  if [ "$(printf '%s\n' "$e" | jq -r '.retired // false')" = true ]; then
    check_retired_skill_link "$e" "$dir"
    return
  fi
  name="$(field "$e" .name)"
  dest="$dir/$name"
  src="$(skill_source "$e")"
  if [ -z "$src" ]; then
    STATUS=needs-user; DETAIL="$(field "$e" .source.env) is not set, so $name has no source"
    FIX="set $(field "$e" .source.env), then re-run install"; return
  fi
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

# hook_paths ENTRY: sets HOOK_SRC and HOOK_DEST for a git_hook entry.
HOOK_SRC=""
HOOK_DEST=""
hook_paths() {
  local e="$1" top hooks
  top="$(repo_top)"
  hooks="$(git -C "$top" rev-parse --git-path hooks)"
  case "$hooks" in /*) ;; *) hooks="$top/$hooks" ;; esac
  HOOK_SRC="$top/$(field "$e" .source)"
  HOOK_DEST="$hooks/$(field "$e" .hook)"
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

# rendered_plist ENTRY: the plist template with every leading ~/ made absolute
# (launchd does not expand ~).
rendered_plist() { sed "s|~/|$HOME/|g" "$MODULE_DIR/$(field "$1" .plist)"; }

# service_files ENTRY: one "FROM<tab>TO" line per file a service copies from
# the module into the cache.
service_files() { printf '%s\n' "$1" | jq -r '(.files // [])[] | "\(.from)\t\(.to)"'; }

check_launchd_service() {
  local e="$1" label command binary dest from to stale=""
  label="$(field "$e" .label)"
  command="$(field "$e" .command)"
  dest="$LAUNCH_AGENTS_DIR/$label.plist"
  if [ -n "$command" ]; then
    FIX="install $command, install $dest, and bootstrap $label"
  else
    binary="$CACHE_ROOT/bin/$(field "$e" .binary)"
    FIX="build $(field "$e" .build_target) with bin/build-frontend.sh, install $dest, and bootstrap $label"
  fi
  while IFS=$'\t' read -r from to; do
    [ -n "$from" ] || continue
    cmp -s "$MODULE_DIR/$from" "$CACHE_ROOT/$to" || stale="$stale $to"
  done < <(service_files "$e")
  if [ -n "$command" ] && ! command -v "$command" >/dev/null 2>&1; then
    STATUS=missing; DETAIL="$command is not on PATH"
  elif [ -z "$command" ] && [ ! -x "$binary" ]; then
    STATUS=missing; DETAIL="$binary is not built"
  elif [ ! -f "$dest" ]; then
    STATUS=missing; DETAIL="$dest is not installed"
  elif ! rendered_plist "$e" | cmp -s - "$dest"; then
    STATUS=outdated; DETAIL="$dest differs from $(field "$e" .plist)"
  elif [ -n "$stale" ]; then
    STATUS=outdated; DETAIL="$CACHE_ROOT lacks the module's copy of$stale"
  elif ! "$LAUNCHCTL" print "gui/$(id -u)/$label" >/dev/null 2>&1; then
    STATUS=missing; DETAIL="$label is not loaded"
  else
    STATUS=ok; DETAIL="$label loaded from $dest"
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
  local e="$1" brew tap cask goi
  brew="$(field "$e" .install.brew)"
  tap="$(field "$e" .install.tap)"
  cask="$(field "$e" .install.brew_cask)"
  goi="$(field "$e" .install.go_install)"
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
    go install "$goi"
  else
    return 1
  fi
}

install_emacs() { install_program "$1"; }

install_env() { return 1; }

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

install_skill_link() {
  local e="$1" src dir name
  name="$(field "$e" .name)"
  if [ "$(printf '%s\n' "$e" | jq -r '.retired // false')" = true ]; then
    src="$(skill_source "$e")"
    while IFS= read -r dir; do
      # Only a link of ours comes out; a file or a foreign link is the user's.
      if [ -L "$dir/$name" ] && { [ "$(readlink "$dir/$name")" = "$src" ] || [ ! -e "$dir/$name" ]; }; then
        rm -f "$dir/$name"
      fi
    done < <(skills_dirs)
    return
  fi
  src="$(skill_source "$e")"
  [ -n "$src" ] && [ -e "$src" ] || return 1
  while IFS= read -r dir; do
    mkdir -p "$dir"
    ln -sfn "$src" "$dir/$name"
  done < <(skills_dirs)
}

install_git_hook() {
  local e="$1"
  hook_paths "$e"
  if [ -f "$HOOK_DEST" ] && ! is_managed_hook "$e" "$HOOK_DEST"; then return 1; fi
  mkdir -p "$(dirname "$HOOK_DEST")"
  cp "$HOOK_SRC" "$HOOK_DEST"
  chmod +x "$HOOK_DEST"
}

install_launchd_service() {
  local e="$1" label dest uid
  label="$(field "$e" .label)"
  dest="$LAUNCH_AGENTS_DIR/$label.plist"
  uid="$(id -u)"
  # launchd creates none of these, and the services bind sockets and open
  # their logs and databases in them.
  mkdir -p "$CACHE_ROOT/bin" "$CACHE_ROOT/log" "$CACHE_ROOT/sock" "$CACHE_ROOT/store" "$LAUNCH_AGENTS_DIR"
  if [ -n "$(field "$e" .command)" ]; then
    if ! command -v "$(field "$e" .command)" >/dev/null 2>&1; then
      err "install: $(field "$e" .id): $(field "$e" .command) is not on PATH; install it first"; return 1
    fi
  else
    "$BUILD_FRONTEND" "$(field "$e" .build_target)"
  fi
  local from to
  while IFS=$'\t' read -r from to; do
    [ -n "$from" ] || continue
    mkdir -p "$(dirname "$CACHE_ROOT/$to")"
    cp "$MODULE_DIR/$from" "$CACHE_ROOT/$to"
  done < <(service_files "$e")
  rendered_plist "$e" >"$dest"
  if "$LAUNCHCTL" print "gui/$uid/$label" >/dev/null 2>&1; then
    "$LAUNCHCTL" bootout "gui/$uid" "$dest"
  fi
  "$LAUNCHCTL" bootstrap "gui/$uid" "$dest"
  "$LAUNCHCTL" kickstart -k "gui/$uid/$label"
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
      src="$(skill_source "$e")"
      while IFS= read -r dir; do
        dest="$dir/$(field "$e" .name)"
        if [ -L "$dest" ] && [ -n "$src" ] && [ "$(readlink "$dest")" = "$src" ]; then
          rm -f "$dest"; say "uninstall: removed $dest"
        fi
      done < <(skills_dirs) ;;
    git_hook)
      hook_paths "$e"
      if [ -e "$HOOK_DEST" ] && [ "$HOOK_SRC" -ef "$HOOK_DEST" ]; then
        say "uninstall: $HOOK_DEST is the checked-in hook (core.hooksPath); left in place"
      elif [ -f "$HOOK_DEST" ] && is_managed_hook "$e" "$HOOK_DEST"; then
        rm -f "$HOOK_DEST"; say "uninstall: removed $HOOK_DEST"
      fi ;;
    launchd_service)
      label="$(field "$e" .label)"
      dest="$LAUNCH_AGENTS_DIR/$label.plist"
      uid="$(id -u)"
      if "$LAUNCHCTL" print "gui/$uid/$label" >/dev/null 2>&1; then
        "$LAUNCHCTL" bootout "gui/$uid" "$dest"; say "uninstall: booted out $label"
      fi
      if [ -f "$dest" ]; then rm -f "$dest"; say "uninstall: removed $dest"; fi
      if [ -n "$(field "$e" .binary)" ] && [ -f "$CACHE_ROOT/bin/$(field "$e" .binary)" ]; then
        rm -f "$CACHE_ROOT/bin/$(field "$e" .binary)"; say "uninstall: removed $CACHE_ROOT/bin/$(field "$e" .binary)"
      fi
      while IFS=$'\t' read -r from to; do
        [ -n "$from" ] || continue
        if [ -f "$CACHE_ROOT/$to" ]; then rm -f "$CACHE_ROOT/$to"; say "uninstall: removed $CACHE_ROOT/$to"; fi
      done < <(service_files "$e") ;;
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
  optional="$(printf '%s\n' "$e" | jq -r '.optional // false')"
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
  [ "$STATUS" != ok ] && [ "$(printf '%s\n' "$1" | jq -r '.optional // false')" != true ]
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
        say "install: $(field "$e" .id) has no install step; it needs you: $FIX"
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
  local verb="$1"
  shift
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
    check) load_manifests; cmd_check ;;
    install) [ "$JSON" -eq 0 ] || die_usage "--json is for check"; load_manifests; cmd_install ;;
    uninstall) load_manifests; cmd_uninstall ;;
    list) load_manifests; cmd_list ;;
    purge-legacy-hooks) command -v jq >/dev/null 2>&1 || { err "purge-hooks: jq is not installed; settings.json is never edited without it"; exit 2; }; purge_legacy_hook_entries ;;
    help|-h|--help) usage ;;
    *) die_usage "unknown verb: $verb" ;;
  esac
}

if [ "${REQUIREMENTS_SH_LIB:-}" != 1 ]; then
  main "$@"
fi

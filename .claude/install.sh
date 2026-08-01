#!/usr/bin/env bash
# install.sh — install the workspace-* / local skills as symlinks under
# ~/.claude/skills and install the managed git pre-commit hook into the
# repo's git hooks dir.
#
# Subcommands:
#   install    (default) Symlink the managed skills into ~/.claude/skills
#              and install the git pre-commit hook.  Idempotent: safe to
#              run again.
#   uninstall  Remove the managed skill symlinks (only ours; foreign files
#              are left alone), the managed git pre-commit hook, and this
#              module's legacy settings.json hook entries.
#   reinstall  uninstall then install.
#   purge-legacy-hooks
#              Remove ONLY this module's pre-cutover Claude Code hook
#              entries (and the orphaned scripts they invoke) from every
#              config dir on the host.  See purge_legacy_hook_entries.
#   help       Show usage.
#
# Skills: each manifest-declared skill is symlinked into ~/.claude/skills
# straight to its CANONICAL impl path.  There is NO cache fallback — if a
# canonical impl is absent, install FAILS HARD (non-zero exit) rather than
# silently linking a stale copy.  A skill is always the live in-tree
# source, so edits go live with no reinstall.
#
# Git pre-commit hook: a managed external-boundary lint gate is installed
# into the repo's git hooks dir, identified by a marker so a foreign
# pre-commit hook is never trampled.  It does NOT run the test suite: that
# gate lives in the workspace-merge machinery, which tests every
# cherry-picked commit as it lands on the target.
#
# Opt-in agent-shim services (OFF by default):
#   --with-agent-shim-services  Also build + launchd-install the shim-store
#              and shim-claude-sidecar services.  Never runs in the default
#              install path; a not-yet-landed service is skipped loudly, not
#              failed.  See install_agent_shim_services below.
#
# Usage:
#   bash .claude/install.sh [install|uninstall|reinstall|help] \
#        [--with-agent-shim-services]
#
# Requires: bash 4+.  (--with-agent-shim-services also needs go + launchctl.)
set -euo pipefail

# --- Shared constants + helpers (needed by both the sandbox repair path
#     and the full install path below) ---
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SKILLS_DIR="$HOME/.claude/skills"

# Resolve a $SCRIPT_DIR-relative path to an absolute canonical form
# (no ".." segments).  Symlink targets that pass through such segments
# would be valid but visually noisy and would break equality checks
# against canonical paths in other tooling.
_canonpath() {
  local raw="$1"
  if [ -d "$raw" ]; then
    # -P resolves symlinks to the PHYSICAL path so comparisons against
    # `git worktree list --porcelain` output (which git emits in physical
    # form, e.g. /private/var/... on macOS rather than /var/...) match.
    ( cd "$raw" && pwd -P )
  else
    echo "$raw"
  fi
}

# --- Main-worktree resolution ---
# Skill symlinks must ALWAYS target the MAIN worktree, never a transient
# linked worktree (whose path dangles the moment the worktree is pruned).
# `git worktree list --porcelain` emits the main worktree as its FIRST
# `worktree` line; every entry after it is a linked (non-main) worktree.
_main_worktree_root() {
  git -C "$SCRIPT_DIR" worktree list --porcelain 2>/dev/null \
    | awk '/^worktree /{print $2; exit}'
}

# Roots of all NON-main worktrees (every entry after the first).
_nonmain_worktree_roots() {
  git -C "$SCRIPT_DIR" worktree list --porcelain 2>/dev/null \
    | awk '/^worktree /{print $2}' | tail -n +2
}

# Resolve the main worktree, falling back to the SCRIPT_DIR-relative repo
# root when git is unavailable (e.g. the dir is not a git checkout).
MAIN_WORKTREE="$(_main_worktree_root || true)"
if [ -z "$MAIN_WORKTREE" ]; then
  MAIN_WORKTREE="$(_canonpath "$SCRIPT_DIR/..")"
fi

# Return 0 when IMPL resolves inside a non-main worktree of this repo.
# Used to FORBID linking a skill to a transient-worktree path: such links
# dangle once the worktree is pruned, so we fail hard instead.
_impl_in_nonmain_worktree() {
  # Canonicalize IMPL to its physical form: the worktree roots from git are
  # physical, so a symlinked IMPL spelling (e.g. /var/... vs /private/var/...
  # on macOS) would otherwise slip past the prefix match.
  local impl root
  impl="$(_canonpath "$(dirname "$1")")/$(basename "$1")"
  while IFS= read -r root; do
    [ -n "$root" ] || continue
    case "$impl/" in
      "$root"/*) return 0 ;;
    esac
  done < <(_nonmain_worktree_roots)
  return 1
}

# Skills checked into THIS (doom) repo, under modules/app/agent-repl/skills/.
# Symlinked into $SKILLS_DIR straight to the MAIN worktree's in-tree source —
# NEVER the invoking (possibly linked) worktree, so the link survives a
# worktree prune.
LOCAL_SKILLS_SRC="$MAIN_WORKTREE/modules/app/agent-repl/skills"
# NOTE: debug-emacs-agent-repl is NOT here — it is a PROJECT-scoped skill,
# registered via the checked-in <repo>/.claude/skills/debug-emacs-agent-repl
# symlink rather than a ~/.claude/skills user-level link (it is doom-specific
# and should only be discoverable when working in this repo).
LOCAL_SKILLS=(
  "profile"
  "runtime-eval-code"
  "workspace-close"
  "emit-workspace-commands.sh"
)

# Manifest declaring each cached workspace-* skill as "name|canonical-impl".
# The manifest is the single source of truth for where each skill lives;
# install always links straight to that impl (no cache copy exists).
SKILLS_MANIFEST="$(_canonpath "$SCRIPT_DIR/../modules/app/agent-repl/skills-cache")/manifest.sh"

# Link cached skill NAME to its canonical IMPL under $SKILLS_DIR.
# NO fallback: a missing IMPL returns 1 so the caller can fail hard.  A
# real (non-symlink) file at the destination is left untouched so a user
# file is never trampled.  An existing symlink (ours or a stale one) is
# repaired to point at IMPL.  TAG prefixes log lines.
link_skill_to_impl() {
  local name="$1" impl="$2" tag="${3:-install}"
  local dest="$SKILLS_DIR/$name"
  # FORBID linking into a non-main worktree: the link would dangle once
  # that transient worktree is pruned.  Fail hard so the bad manifest
  # entry (or a worktree-relative source) surfaces loudly.
  if _impl_in_nonmain_worktree "$impl"; then
    echo "[$tag] ERROR: skill '$name' impl is inside a non-main worktree: $impl" >&2
    echo "[$tag]        symlinks must target the main worktree: $MAIN_WORKTREE" >&2
    return 1
  fi
  if [ ! -e "$impl" ] && [ ! -L "$impl" ]; then
    echo "[$tag] ERROR: canonical impl for '$name' not found at $impl" >&2
    return 1
  fi
  if [ -e "$dest" ] && [ ! -L "$dest" ]; then
    echo "[$tag] WARNING: $name has a non-symlink file at $dest (skipped, not trampling)"
    return 0
  fi
  ln -sfn "$impl" "$dest"
  echo "[$tag] Linked $name -> $impl"
  return 0
}

# --- Sandbox detection ---
# When executing inside the agent sandbox, the host's ~/.claude/ is
# bind-mounted but skill symlinks in ~/.claude/skills/ may point to
# absolute host paths that don't exist in the container.  Repair them by
# relinking to the SAME canonical impl the host uses.  There is NO cache
# fallback: if a canonical impl is absent we FAIL HARD so a broken
# environment surfaces loudly instead of silently serving stale code.
# Only skill symlinks are repaired on this path.
if { [ -f /.dockerenv ] || [ "${DOOM_SANDBOX:-}" = "1" ]; } \
   && [ "${INSTALL_SH_SKIP_SANDBOX_DETECT:-}" != "1" ] \
   && [ "${INSTALL_SH_LIB:-}" != "1" ]; then
  echo "[install.sh] Detected sandbox environment — running skill symlink repair only."
  mkdir -p "$SKILLS_DIR"
  missing=0

  if [ ! -f "$SKILLS_MANIFEST" ]; then
    echo "[install.sh/sandbox] ERROR: skills manifest missing at $SKILLS_MANIFEST" >&2
    exit 1
  fi
  # shellcheck source=../modules/app/agent-repl/skills-cache/manifest.sh
  source "$SKILLS_MANIFEST"
  for entry in "${CACHED_SKILLS[@]}"; do
    IFS='|' read -r name impl <<< "$entry"
    link_skill_to_impl "$name" "$impl" "install.sh/sandbox" || missing=$((missing + 1))
  done
  for name in "${LOCAL_SKILLS[@]}"; do
    link_skill_to_impl "$name" "$LOCAL_SKILLS_SRC/$name" "install.sh/sandbox" || missing=$((missing + 1))
  done

  if [ "$missing" -gt 0 ]; then
    echo "[install.sh/sandbox] FAILED: $missing canonical impl path(s) missing — no fallback, refusing to leave stale links." >&2
    exit 1
  fi
  echo "[install.sh] Sandbox skill repair complete."
  exit 0
fi

# --- Constants (full install path) ---

# Pre-commit hook (external-boundary lint gate) installed into the repo's
# git hooks dir by do_install below.
GITHOOKS_DIR="$(_canonpath "$SCRIPT_DIR/../.githooks")"

# Marker identifying our managed pre-commit hook so install/uninstall can
# refresh or remove it without touching a foreign pre-commit hook.
PRECOMMIT_MARKER="AGENT_REPL_MANAGED_HOOK: agent-repl-precommit"
# Legacy marker from before the claude-repl -> agent-repl rename; installed
# copies in existing repos still carry it, so refresh/uninstall must keep
# recognizing it as managed rather than warning "foreign".  (The blanket
# rename had rewritten this constant to the CURRENT spelling, which made it
# a duplicate: every pre-rename installed hook was then misreported as
# foreign and never refreshed, silently freezing the lint gate at whatever
# copy predated the rename.)
PRECOMMIT_MARKER_LEGACY="CLAUDE_REPL_MANAGED_HOOK: claude-repl-precommit"

# Return 0 iff FILE carries either the current or the legacy managed marker.
_is_managed_precommit() {
  grep -q -e "$PRECOMMIT_MARKER" -e "$PRECOMMIT_MARKER_LEGACY" "$1" 2>/dev/null
}

# --- Helpers ---

show_help() {
  cat <<USAGE
Usage: bash $0 [install|uninstall|reinstall|purge-legacy-hooks|help] \\
       [--with-agent-shim-services]

  install    (default) Symlink the managed skills into ~/.claude/skills
             and install the git pre-commit hook.  Idempotent.
  uninstall  Remove the managed skill symlinks and the git pre-commit hook,
             and purge this module's legacy settings.json hook entries.
  reinstall  uninstall then install.
  purge-legacy-hooks
             Remove ONLY this module's pre-cutover Claude Code hook entries
             from every discovered settings.json (and the orphaned scripts
             they point at).  Idempotent; foreign entries are never touched.
  help       Show this message.

  --with-agent-shim-services
             OPT-IN, OFF BY DEFAULT.  Also build the shim-store and
             shim-claude-sidecar Go binaries into ~/.cache/agent-repl/bin/,
             install their launchd user-agent plists, and bootstrap/kickstart
             them (or, with uninstall, boot them out and remove them).  A
             service whose code has not landed yet is skipped with a loud
             notice instead of failing the install.
USAGE
}

# --- Install ---

do_install() {
  # Cached workspace-* skills.  Each is symlinked straight to its
  # canonical impl from the manifest.  NO fallback: a missing impl is a
  # hard error (fail loudly rather than serve a stale copy).  A real file
  # already at the destination is left untouched.
  mkdir -p "$SKILLS_DIR"
  if [ ! -f "$SKILLS_MANIFEST" ]; then
    echo "[install] ERROR: skills manifest missing at $SKILLS_MANIFEST" >&2
    exit 1
  fi
  # shellcheck source=../modules/app/agent-repl/skills-cache/manifest.sh
  source "$SKILLS_MANIFEST"
  missing=0
  for entry in "${CACHED_SKILLS[@]}"; do
    IFS='|' read -r name impl <<< "$entry"
    link_skill_to_impl "$name" "$impl" "install" || missing=$((missing + 1))
  done

  # Repo-local managed skills (under modules/app/agent-repl/skills/),
  # symlinked straight to the in-tree source so SKILL.md edits go live.
  if [ ! -d "$LOCAL_SKILLS_SRC" ]; then
    echo "[install] ERROR: local skills source dir not found: $LOCAL_SKILLS_SRC" >&2
    exit 1
  fi
  for name in "${LOCAL_SKILLS[@]}"; do
    link_skill_to_impl "$name" "$LOCAL_SKILLS_SRC/$name" "install" || missing=$((missing + 1))
  done

  # Install the pre-commit hook (external-boundary lint gate) into the
  # repo's current git hooks dir (wherever core.hooksPath / git config
  # say it is).  Idempotency:
  #   - No pre-commit exists  → copy ours in.
  #   - Pre-commit has our marker → refresh (the user re-ran install).
  #   - Foreign pre-commit exists → warn, skip, don't trample.
  if [ -d "$GITHOOKS_DIR" ] && command -v git >/dev/null 2>&1; then
    repo_top="$(git -C "$GITHOOKS_DIR" rev-parse --show-toplevel 2>/dev/null || true)"
    if [ -n "$repo_top" ]; then
      hooks_path="$(git -C "$repo_top" rev-parse --git-path hooks 2>/dev/null || true)"
      if [ -n "$hooks_path" ]; then
        if [[ "$hooks_path" != /* ]]; then
          hooks_path="$repo_top/$hooks_path"
        fi
        mkdir -p "$hooks_path"
        src_hook="$GITHOOKS_DIR/pre-commit"
        dest_hook="$hooks_path/pre-commit"
        if [ ! -f "$dest_hook" ]; then
          cp "$src_hook" "$dest_hook"
          chmod +x "$dest_hook"
          echo "[install] Installed pre-commit hook -> $dest_hook"
        elif _is_managed_precommit "$dest_hook"; then
          cp "$src_hook" "$dest_hook"
          chmod +x "$dest_hook"
          echo "[install] Refreshed managed pre-commit hook -> $dest_hook"
        else
          echo "[install] WARNING: foreign pre-commit hook at $dest_hook (skipped)"
          echo "[install] To enable the agent-repl boundary lint gate, append the body of $src_hook to it."
        fi
      fi
    fi
  fi

  # Fail hard at the END (after doing all possible work) when any skill
  # impl was missing, so a broken manifest entry surfaces loudly while the
  # valid skills and the hook still get installed.  No fallback.
  if [ "$missing" -gt 0 ]; then
    echo "[install] FAILED: $missing skill impl path(s) missing — no fallback, fix the manifest impl path(s) above." >&2
    exit 1
  fi

  echo "[install] Done."
}

# --- Uninstall ---

do_uninstall() {
  # Remove cached-skill symlinks — only ours (pointing at the canonical
  # impl declared in the manifest).  Foreign files are left alone.
  if [ -f "$SKILLS_MANIFEST" ]; then
    # shellcheck source=../modules/app/agent-repl/skills-cache/manifest.sh
    source "$SKILLS_MANIFEST"
    for entry in "${CACHED_SKILLS[@]}"; do
      IFS='|' read -r name impl <<< "$entry"
      dest="$SKILLS_DIR/$name"
      if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$impl" ]; then
        rm -f "$dest"
        echo "[uninstall] Removed cached-skill link: $dest"
      fi
    done
  fi

  # Remove repo-local skill symlinks (only ours, pointing at LOCAL_SKILLS_SRC).
  for name in "${LOCAL_SKILLS[@]}"; do
    dest="$SKILLS_DIR/$name"
    expected="$LOCAL_SKILLS_SRC/$name"
    if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$expected" ]; then
      rm -f "$dest"
      echo "[uninstall] Removed local skill link: $dest"
    fi
  done

  # Remove the managed pre-commit hook only if it carries our marker.
  if command -v git >/dev/null 2>&1 && [ -d "$GITHOOKS_DIR" ]; then
    repo_top="$(git -C "$GITHOOKS_DIR" rev-parse --show-toplevel 2>/dev/null || true)"
    if [ -n "$repo_top" ]; then
      hooks_path="$(git -C "$repo_top" rev-parse --git-path hooks 2>/dev/null || true)"
      if [ -n "$hooks_path" ]; then
        if [[ "$hooks_path" != /* ]]; then
          hooks_path="$repo_top/$hooks_path"
        fi
        dest_hook="$hooks_path/pre-commit"
        if [ -f "$dest_hook" ] && _is_managed_precommit "$dest_hook"; then
          rm -f "$dest_hook"
          echo "[uninstall] Removed managed pre-commit hook: $dest_hook"
        fi
      fi
    fi
  fi

  # Anything this module ever wrote into the user's Claude Code settings
  # comes out here too — see purge_legacy_hook_entries.
  purge_legacy_hook_entries

  echo "[uninstall] Done."
}

# --- legacy Claude Code hook entries (pre-cutover) ---
#
# Before the sentinel cutover this module registered hook entries in the
# user's Claude Code settings.json — one per notify script it had copied
# into <config-dir>/hooks/ — so the shim could signal Emacs by writing
# sentinel FILES.  The cutover replaced that whole plane with pushed
# protobuf state, and deleted the scripts, the registrar, and the remover
# along with it.
#
# Deleting the remover did not delete what it had written: on any host
# that ran a pre-cutover install, the ENTRIES survive in settings.json and
# keep firing on every prompt, stop, and permission request — invoking
# scripts that either no longer exist or write sentinels nothing consumes.
# Reinstating a settings.json *writer* is not wanted; reinstating the
# *remover* is, because removing what this module installed is precisely
# an uninstaller's job.  This is that remover, and it only ever subtracts.
#
# Scope discipline: an entry is ours ONLY when its command's basename is
# in the table below AND the command path runs through a `hooks/` dir.
# Foreign entries that happen to share an event (e.g. the gns hook.py on
# Stop/UserPromptSubmit) are structurally unmatchable and never touched.
# Every other settings.json key — auth, permissions, model, statusLine —
# is passed through untouched by a whole-document jq round-trip.

# Basenames of the notify scripts the pre-cutover installer registered.
# NOTE: prepare-commit-msg-emoji.sh is deliberately ABSENT — it is a live
# git hook, still in the repo, and was never a settings.json entry.
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

# Print every Claude Code config dir on this host that carries a
# settings.json: the default ~/.claude plus any sibling ~/.claude-* dir
# (alternate accounts get their own CLAUDE_CONFIG_DIR, and the pre-cutover
# installer provisioned hooks into each one).  No hardcoded account names.
_legacy_hook_settings_files() {
  local dir
  for dir in "$HOME/.claude" "$HOME"/.claude-*; do
    [ -d "$dir" ] || continue
    [ -f "$dir/settings.json" ] || continue
    echo "$dir/settings.json"
  done
}

# jq program: drop hook objects whose command is one of ours, then prune
# matcher groups and events left empty by the removal.  Applied to the
# whole document so every unrelated key round-trips untouched.
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

# Same predicate, but listing what WOULD be removed (for the log).
_LEGACY_HOOK_JQ_LIST='
def ours($names):
  (. // "") as $cmd
  | ($cmd | split("/") | last) as $base
  | (($names | index($base)) != null) and ($cmd | test("/hooks/"));
(.hooks // {}) | to_entries[] | .key as $event
| .value[]? | .hooks[]?
| select(.command | ours($names))
| "\($event)\t\(.command)"'

# Remove this module's legacy hook entries from every discovered
# settings.json, then delete the orphaned scripts they pointed at.
# Idempotent: with nothing to remove it reports so and rewrites nothing.
purge_legacy_hook_entries() {
  echo "[purge-hooks] Removing this module's pre-cutover settings.json hook entries."

  if ! command -v jq >/dev/null 2>&1; then
    # settings.json is shared, user-owned state; editing it with anything
    # less precise than a real JSON tool is not an option, so a missing jq
    # is a hard failure rather than a skip.
    echo "[purge-hooks] ERROR: 'jq' not found — refusing to edit settings.json without it." >&2
    return 1
  fi

  local names_json
  names_json="$(printf '%s\n' "${LEGACY_HOOK_SCRIPTS[@]}" | jq -R . | jq -s .)"

  local settings removed backup tmp total=0
  while IFS= read -r settings; do
    [ -n "$settings" ] || continue

    if ! jq -e . "$settings" >/dev/null 2>&1; then
      echo "[purge-hooks] ERROR: $settings is not valid JSON — leaving it untouched." >&2
      return 1
    fi

    removed="$(jq -r --argjson names "$names_json" "$_LEGACY_HOOK_JQ_LIST" "$settings")"
    if [ -z "$removed" ]; then
      echo "[purge-hooks] $settings: no legacy entries (nothing to do)."
      continue
    fi

    backup="$settings.pre-purge-$(date +%Y%m%d-%H%M%S)"
    cp "$settings" "$backup"
    tmp="$settings.purge-tmp-$$"
    jq --indent 2 --argjson names "$names_json" "$_LEGACY_HOOK_JQ_PRUNE" "$settings" > "$tmp"
    mv "$tmp" "$settings"

    echo "[purge-hooks] $settings: backed up to $backup"
    printf '%s\n' "$removed" | sed 's/^/[purge-hooks]   removed: /'
    total=$((total + $(printf '%s\n' "$removed" | wc -l | tr -d ' ')))
  done < <(_legacy_hook_settings_files)

  # The scripts themselves: installed copies under <config-dir>/hooks/ that
  # no longer have a source in this repo.  Removed only after the entries
  # that invoked them are gone, so no window exists where a registered hook
  # points at a deleted file.
  local dir script
  for dir in "$HOME/.claude" "$HOME"/.claude-*; do
    [ -d "$dir/hooks" ] || continue
    for script in "${LEGACY_HOOK_SCRIPTS[@]}"; do
      if [ -f "$dir/hooks/$script" ]; then
        rm -f "$dir/hooks/$script"
        echo "[purge-hooks] Removed orphaned script $dir/hooks/$script"
      fi
    done
  done

  echo "[purge-hooks] Done: $total entr(y/ies) removed."
  return 0
}

# --- agent-shim supervision services (OPT-IN, OFF BY DEFAULT) ---
#
# The shim ecosystem's two OS-managed services (shim-store,
# shim-claude-sidecar; §6.1/§7.1/§13 of design-agent-shim-architecture.md).
# This step is NEVER part of the default install path: it runs ONLY when an
# action is invoked WITH the explicit `--with-agent-shim-services` flag.
#
# It is idempotent (safe to re-run): it rebuilds the binaries, refreshes the
# plists, and re-bootstraps the launchd user agents.  Each service is
# INDEPENDENTLY skipped-with-loud-notice when its module dir has no buildable
# `main` package yet (the two services are landed by parallel workstreams
# G2/G3).  A skip never fails the install and never pretends success — it
# prints exactly what was skipped and why.

# Where the shim-ecosystem runtime state lives (matches agent-shim-doctor.sh
# and the plist templates).
AGENT_SHIM_CACHE_ROOT="${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl"
LAUNCH_AGENTS_DIR="$HOME/Library/LaunchAgents"
LAUNCHD_SRC_DIR="$SCRIPT_DIR/../modules/app/agent-repl/launchd"
AGENT_SHIM_SRC_DIR="$SCRIPT_DIR/../modules/app/agent-repl/agent-shim"

# Each entry: MODULE_SUBDIR|BINARY_NAME|LAUNCHD_LABEL
AGENT_SHIM_SERVICES=(
  "shim-store|shim-store|com.agentrepl.shim-store"
  "claude/shim-sidecar|shim-claude-sidecar|com.agentrepl.shim-claude-sidecar"
)

# Return 0 iff SRC_DIR is a Go module carrying a buildable `package main`.
# The two service dirs currently hold only an AGENTS.md charter; until a
# parallel workstream lands their code, this returns non-zero and the caller
# skips the service loudly rather than failing the whole install.
_service_buildable() {
  local src="$1"
  [ -f "$src/go.mod" ] || return 1
  local f
  for f in "$src"/*.go; do
    [ -f "$f" ] || continue
    # A `main` package = a top-level .go file whose package clause is `main`.
    if grep -Eq '^package main([[:space:]]|$)' "$f"; then
      return 0
    fi
  done
  return 1
}

# Install one service's launchd plist: rewrite the ~-relative template paths
# to absolute $HOME paths (launchd does NOT expand `~`) and write it into
# ~/Library/LaunchAgents/.  '|' is a safe sed delimiter here: $HOME never
# contains '|'.
_install_shim_plist() {
  local plist_src="$1" plist_dest="$2"
  sed "s|~/|$HOME/|g" "$plist_src" > "$plist_dest"
}

install_agent_shim_services() {
  echo "[install/services] Opt-in agent-shim services requested (--with-agent-shim-services)."

  if ! command -v go >/dev/null 2>&1; then
    # The caller opted in, so a missing toolchain is a hard failure — not a
    # skip.  (Skips are reserved for not-yet-landed service code.)
    echo "[install/services] ERROR: 'go' toolchain not found — cannot build the shim services." >&2
    echo "[install/services]        Install Go, then re-run with --with-agent-shim-services." >&2
    return 1
  fi
  if ! command -v launchctl >/dev/null 2>&1; then
    echo "[install/services] ERROR: 'launchctl' not found — these services are macOS launchd user agents." >&2
    return 1
  fi

  local bin_dir="$AGENT_SHIM_CACHE_ROOT/bin"
  local log_dir="$AGENT_SHIM_CACHE_ROOT/log"
  local sock_dir="$AGENT_SHIM_CACHE_ROOT/sock"
  local store_dir="$AGENT_SHIM_CACHE_ROOT/store"
  # launchd will not create these; they must exist before the services bind
  # their sockets / open their logs / DBs.
  mkdir -p "$bin_dir" "$log_dir" "$sock_dir" "$store_dir" "$LAUNCH_AGENTS_DIR"

  local uid installed=0 skipped=0
  uid="$(id -u)"

  local entry subdir bin label src_dir plist_src plist_dest
  for entry in "${AGENT_SHIM_SERVICES[@]}"; do
    IFS='|' read -r subdir bin label <<< "$entry"
    src_dir="$AGENT_SHIM_SRC_DIR/$subdir"
    plist_src="$LAUNCHD_SRC_DIR/$label.plist"
    plist_dest="$LAUNCH_AGENTS_DIR/$label.plist"

    if [ ! -f "$plist_src" ]; then
      echo "[install/services] ERROR: launchd plist missing at $plist_src" >&2
      return 1
    fi

    if ! _service_buildable "$src_dir"; then
      echo "[install/services] SKIP: $label — no buildable 'main' package under $src_dir yet."
      echo "[install/services]       (Its code is landed by a parallel workstream; nothing was built,"
      echo "[install/services]        no plist was installed, and the service is NOT running.)"
      skipped=$((skipped + 1))
      continue
    fi

    echo "[install/services] Building $bin from $src_dir ..."
    ( cd "$src_dir" && go build -o "$bin_dir/$bin" . )
    echo "[install/services] Built -> $bin_dir/$bin"

    _install_shim_plist "$plist_src" "$plist_dest"
    echo "[install/services] Installed plist -> $plist_dest"

    # Idempotent (re)load: bootout any prior instance (ignore when absent),
    # then bootstrap the refreshed plist and kickstart it now.
    launchctl bootout "gui/$uid" "$plist_dest" 2>/dev/null || true
    launchctl bootstrap "gui/$uid" "$plist_dest"
    launchctl kickstart -k "gui/$uid/$label"
    echo "[install/services] Bootstrapped + kickstarted $label"
    installed=$((installed + 1))
  done

  echo "[install/services] Done: $installed installed, $skipped skipped."
  if [ "$skipped" -gt 0 ]; then
    echo "[install/services] NOTE: skipped service(s) are NOT running. Re-run with"
    echo "[install/services]       --with-agent-shim-services once their code lands."
  fi
  return 0
}

# Symmetric teardown: bootout the launchd agents and remove the installed
# plists + built binaries.  Only ever invoked WITH --with-agent-shim-services.
uninstall_agent_shim_services() {
  echo "[uninstall/services] Removing agent-shim services (--with-agent-shim-services)."
  local uid bin_dir label plist_dest
  uid="$(id -u)"
  bin_dir="$AGENT_SHIM_CACHE_ROOT/bin"
  local entry subdir bin
  for entry in "${AGENT_SHIM_SERVICES[@]}"; do
    IFS='|' read -r subdir bin label <<< "$entry"
    plist_dest="$LAUNCH_AGENTS_DIR/$label.plist"
    if command -v launchctl >/dev/null 2>&1 && [ -f "$plist_dest" ]; then
      launchctl bootout "gui/$uid" "$plist_dest" 2>/dev/null || true
      echo "[uninstall/services] Booted out $label"
    fi
    if [ -f "$plist_dest" ]; then
      rm -f "$plist_dest"
      echo "[uninstall/services] Removed $plist_dest"
    fi
    if [ -f "$bin_dir/$bin" ]; then
      rm -f "$bin_dir/$bin"
      echo "[uninstall/services] Removed $bin_dir/$bin"
    fi
  done
  echo "[uninstall/services] Done."
}

# --- Dispatch ---

if [ "${INSTALL_SH_LIB:-}" != "1" ]; then
  # Parse args: exactly one optional action word plus optional flags. The
  # --with-agent-shim-services flag is OFF unless explicitly passed, and NEVER
  # engages the services step on its own — it only augments install/reinstall/
  # uninstall.
  ACTION=""
  WITH_SERVICES=0
  for arg in "$@"; do
    case "$arg" in
      --with-agent-shim-services) WITH_SERVICES=1 ;;
      -h|--help|help)             show_help; exit 0 ;;
      install|uninstall|reinstall|purge-legacy-hooks)
        if [ -n "$ACTION" ]; then
          echo "[install.sh] Multiple actions given: '$ACTION' and '$arg'" >&2
          show_help
          exit 2
        fi
        ACTION="$arg"
        ;;
      *)
        echo "[install.sh] Unknown argument: $arg" >&2
        show_help
        exit 2
        ;;
    esac
  done
  ACTION="${ACTION:-install}"

  # NOTE: each services step is guarded by a full `if` rather than
  # `[ "$WITH_SERVICES" -eq 1 ] && step`.  The short-circuit form is the
  # LAST command in its case arm, so with the flag OFF the false test
  # became the script's exit status and a fully successful default
  # install exited 1.
  case "$ACTION" in
    install)
      do_install
      if [ "$WITH_SERVICES" -eq 1 ]; then
        install_agent_shim_services
      fi
      ;;
    uninstall)
      do_uninstall
      if [ "$WITH_SERVICES" -eq 1 ]; then
        uninstall_agent_shim_services
      fi
      ;;
    reinstall)
      do_uninstall
      if [ "$WITH_SERVICES" -eq 1 ]; then
        uninstall_agent_shim_services
      fi
      do_install
      if [ "$WITH_SERVICES" -eq 1 ]; then
        install_agent_shim_services
      fi
      ;;
    purge-legacy-hooks)
      purge_legacy_hook_entries
      ;;
  esac
fi

#!/usr/bin/env bash

# shellcheck shell=bash
# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# lib-node-store.sh — the shared node-dependency store's ONE repair path,
# sourced by build-frontend.sh (link_node_modules) and ensure-deps.sh. Sourced,
# never executed.
#
# THE STORE. $AGENT_REPL_NODE_STORE (default ~/.cache/agent-repl/node-store)
# holds one ENTRY per package and lockfile hash, `<name>-<key>/`, carrying that
# package's manifests and a node_modules every checkout with that lockfile links
# to (`<checkout>/node_modules -> <entry>/node_modules`).
#
# WHY A REPAIR PATH. An entry used to be installed only when
# `<entry>/node_modules` was ABSENT. On 2026-09-23 a worktree's `npm ci` emptied
# the webapp entry THROUGH its link, the entry still existed, and so every
# checkout kept linking an empty tree and nothing ever refilled it. An entry is
# therefore judged by whether it SATISFIES ITS OWN LOCKFILE (`npm ls --depth=0`
# inside the entry), never by whether a directory exists.
#
# HOW A REPAIR IS SAFE.
#   - EXCLUSIVE PER ENTRY. `mkdir <store>/.<entry>.lock` is the lock (atomic
#     everywhere; flock(1) does not exist on macOS), holding the holder's pid.
#     A second repairer WAITS for it, then re-checks: the entry the first one
#     repaired is healthy, and the second skips. A lock whose pid is dead is
#     broken, loudly. The wait is bounded
#     ($AGENT_REPL_NODE_STORE_REPAIR_WAIT_SECS, default 900) and a timeout
#     fails the repair rather than installing beside a live holder.
#   - NEVER A HALF TREE. The install runs in a fresh tree beside the live one,
#     `<entry>/.trees/<id>/`, and `<entry>/node_modules` becomes a symlink to
#     it by rename(2), which replaces a symlink atomically: a concurrent reader
#     of the link sees the old tree or the new one, never a partial install.
#     The one non-atomic moment is retiring a LEGACY entry whose node_modules
#     is still a real directory, and that only ever happens to a tree already
#     judged broken.
#   - NOTHING RUNS `npm ci` THROUGH A SYMLINK. The install's cwd is the fresh
#     tree, whose node_modules does not exist yet.
#   - The trees live INSIDE the entry, so the store's gc removes them with it.
#
# Every repair line goes to stderr with the `[node-store]` prefix, and a repair
# is always announced: it means a tree other checkouts were running on was
# broken.

# node_store_root — the store's root directory.
node_store_root() {
    printf '%s\n' "${AGENT_REPL_NODE_STORE:-${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl/node-store}"
}

# node_store_key DIR — a short content hash of DIR's dependency manifest, so a
# lockfile (or, absent one, package.json) change keys a different entry. Fails
# when DIR has neither.
node_store_key() {
    local manifest="$1/package-lock.json"
    [ -f "$manifest" ] || manifest="$1/package.json"
    [ -f "$manifest" ] || return 1
    { shasum -a 256 "$manifest" 2>/dev/null || sha256sum "$manifest"; } | cut -c1-16
}

# node_store_say MESSAGE... — one loud repair line on stderr.
node_store_say() {
    printf '[node-store] %s\n' "$*" >&2
}

# node_store_entry_healthy ENTRY — succeed when ENTRY's node_modules satisfies
# ENTRY's own manifests. An entry with no package.json or no node_modules is
# not healthy: there is nothing to satisfy, or nothing satisfying it.
node_store_entry_healthy() {
    local entry="$1"
    [ -f "$entry/package.json" ] && [ -d "$entry/node_modules" ] || return 1
    ( cd "$entry" && npm ls --depth=0 >/dev/null 2>&1 )
}

# node_store_entry_of_link LINK — echo the store entry LINK points into, or
# fail when LINK is not a symlink to `<store>/<entry>/node_modules`. Only an
# entry that is a direct child of the store root is ever repaired: a link
# pointing anywhere else is somebody's deliberate choice.
node_store_entry_of_link() {
    local link="$1" target entry parent root
    [ -L "$link" ] || return 1
    target="$(readlink "$link")" || return 1
    [ "$(basename "$target")" = node_modules ] || return 1
    entry="$(dirname "$target")"
    parent="$(cd -P "$(dirname "$entry")" 2>/dev/null && pwd)" || return 1
    root="$(cd -P "$(node_store_root)" 2>/dev/null && pwd)" || return 1
    [ "$parent" = "$root" ] || return 1
    printf '%s\n' "$entry"
}

# node_store_manifest_source ENTRY CANDIDATE — echo the directory whose
# manifests ENTRY is installed from: CANDIDATE when its manifest keys to
# ENTRY's own name (the checkout asking IS on this lockfile), otherwise the
# copies ENTRY keeps of its own. Fails when neither is available, because
# installing an entry from a lockfile it is not keyed by would corrupt it for
# every checkout that is.
node_store_manifest_source() {
    local entry="$1" candidate="$2" key
    key="${entry##*-}"
    if [ -n "$candidate" ] && [ "$(node_store_key "$candidate" 2>/dev/null)" = "$key" ]; then
        printf '%s\n' "$candidate"
        return 0
    fi
    if [ -f "$entry/package.json" ]; then
        printf '%s\n' "$entry"
        return 0
    fi
    return 1
}

# node_store_rename_over SRC DST — rename SRC onto DST, replacing a DST that is
# a symlink as the link itself (rename(2), atomic) rather than moving SRC into
# the directory it names.
node_store_rename_over() {
    if [ "$(uname)" = Darwin ]; then
        mv -h -f "$1" "$2"
    else
        mv -T -f "$1" "$2"
    fi
}

# node_store_lock ENTRY — take ENTRY's exclusive repair lock, waiting for a
# live holder and breaking a dead one's. Fails, loudly, when the wait times out.
node_store_lock() {
    local entry="$1" lock holder waited=0 announced=0
    local limit="${AGENT_REPL_NODE_STORE_REPAIR_WAIT_SECS:-900}"
    lock="$(dirname "$entry")/.$(basename "$entry").lock"
    mkdir -p "$(dirname "$entry")"
    while ! mkdir "$lock" 2>/dev/null; do
        holder="$(cat "$lock/pid" 2>/dev/null || true)"
        if [ -n "$holder" ] && ! kill -0 "$holder" 2>/dev/null; then
            node_store_say "breaking $lock: its holder (pid $holder) is gone"
            rm -rf "$lock"
            continue
        fi
        if [ "$announced" -eq 0 ]; then
            node_store_say "waiting for the repair of $entry that pid ${holder:-unknown} holds ($lock)"
            announced=1
        fi
        if [ "$waited" -ge "$((limit * 5))" ]; then
            node_store_say "gave up waiting ${limit}s for the repair lock $lock (held by pid ${holder:-unknown})"
            return 1
        fi
        sleep 0.2
        waited=$((waited + 1))
    done
    echo "$$" > "$lock/pid"
}

# node_store_unlock ENTRY — release ENTRY's repair lock.
node_store_unlock() {
    rm -rf "$(dirname "$1")/.$(basename "$1").lock"
}

# node_store_install_tree ENTRY SOURCE — install SOURCE's manifests into a fresh
# tree under ENTRY and swap ENTRY/node_modules onto it. The caller holds the
# lock. Every step is checked explicitly, because a caller testing this in an
# `if` has `set -e` suspended: a failure before the swap removes the fresh tree
# and leaves the live node_modules untouched.
node_store_install_tree() {
    local entry="$1" source="$2" id tree swap old=""
    id="$(date +%Y%m%d%H%M%S)-$$"
    tree="$entry/.trees/$id"
    mkdir -p "$tree" || return 1
    if ! node_store_copy_manifests "$source" "$tree" ||
            { [ "$source" != "$entry" ] && ! node_store_copy_manifests "$source" "$entry"; }; then
        node_store_say "could not copy $source's manifests into $entry"
        rm -rf "$tree"
        return 1
    fi
    local verb=install
    [ -f "$tree/package-lock.json" ] && verb=ci
    if ! ( cd "$tree" && npm "$verb" --no-audit --no-fund >&2 ); then
        rm -rf "$tree"
        return 1
    fi

    if [ -L "$entry/node_modules" ]; then
        old="$(dirname "$entry/$(readlink "$entry/node_modules")")"
    elif [ -e "$entry/node_modules" ]; then
        # A LEGACY real directory cannot be replaced by rename; it is moved
        # aside first. It is already judged broken, so nobody loses a tree.
        old="$entry/.trees/retired-$id"
        if ! mv "$entry/node_modules" "$old"; then
            node_store_say "could not move the broken $entry/node_modules aside"
            rm -rf "$tree"
            return 1
        fi
    fi
    swap="$entry/.node_modules.swap.$$"
    if ! ln -s ".trees/$id/node_modules" "$swap" ||
            ! node_store_rename_over "$swap" "$entry/node_modules"; then
        node_store_say "could not swap $entry/node_modules onto the fresh tree"
        rm -f "$swap"
        rm -rf "$tree"
        return 1
    fi
    if [ -n "$old" ] && [ "$old" != "$tree" ] && ! rm -rf "$old"; then
        node_store_say "the retired tree $old could not be removed; it is unused and gc takes it with the entry"
    fi
    return 0
}

# node_store_copy_manifests FROM TO — copy FROM's package.json and, when it has
# one, its package-lock.json into TO.
node_store_copy_manifests() {
    cp "$1/package.json" "$2/package.json" || return 1
    [ -f "$1/package-lock.json" ] || return 0
    cp "$1/package-lock.json" "$2/package-lock.json"
}

# node_store_repair ENTRY [CANDIDATE] — make ENTRY satisfy its lockfile,
# installing it when absent and repairing it in place when broken. A healthy
# entry is left exactly as it is. CANDIDATE is the checkout asking, whose
# manifests are used when they key to ENTRY. Succeeds only with ENTRY healthy.
node_store_repair() {
    local entry="$1" candidate="${2:-}" source absent=0
    node_store_entry_healthy "$entry" && return 0
    [ -d "$entry/node_modules" ] || [ -L "$entry/node_modules" ] || absent=1

    node_store_lock "$entry" || return 1
    # THE RE-CHECK UNDER THE LOCK. A repairer that waited finds the entry the
    # holder just repaired, and must not install it a second time.
    if node_store_entry_healthy "$entry"; then
        node_store_say "$entry was repaired concurrently; skipping"
        node_store_unlock "$entry"
        return 0
    fi
    if ! source="$(node_store_manifest_source "$entry" "$candidate")"; then
        node_store_say "cannot repair $entry: it keeps no manifests and ${candidate:-no checkout} is not on its lockfile"
        node_store_unlock "$entry"
        return 1
    fi
    if [ "$absent" -eq 1 ]; then
        node_store_say "populating $entry from $source"
    else
        node_store_say "REPAIRING $entry: it exists but does not satisfy its lockfile (npm ls failed); reinstalling from $source into a fresh tree"
    fi
    if ! node_store_install_tree "$entry" "$source"; then
        node_store_say "the install for $entry FAILED; the entry is left as it was"
        node_store_unlock "$entry"
        return 1
    fi
    if ! node_store_entry_healthy "$entry"; then
        node_store_say "$entry still does not satisfy its lockfile after reinstalling"
        node_store_unlock "$entry"
        return 1
    fi
    [ "$absent" -eq 1 ] || node_store_say "repaired $entry"
    node_store_unlock "$entry"
    return 0
}

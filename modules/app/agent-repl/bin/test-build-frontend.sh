#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh. This harness is clean at the real default bar
# (plus `-S style`) with no suppressions at all.
# test-build-frontend.sh — hermetic tests for build-frontend.sh staleness logic.
#
# Builds a throwaway project tree around a copy of build-frontend.sh and stubs
# `npm` and `go` on PATH so no real toolchain runs: each stub just `touch`es the
# artifact it would have produced and records that it fired. Tests then assert
# WHICH artifacts the script decided to (re)build under each staleness scenario.
#
# The fixture is a scratch repository, because staleness IS a git question
# now: the script compares the source revision an artifact was built from
# against the revision standing in the checkout. A "fresh" fixture is therefore
# built artifacts PLUS the `.source-tree` stamps that say what they came from,
# which is what make_fresh_artifacts writes.
#
# NO REAL GIT RUNS HERE (owner rule). The repository is bin/fake-git.sh's model
# (history, index and working tree as files under <root>/.fakegit), installed as
# the only `git` on PATH for the harness AND every script it runs; the harness
# refuses to start if any other `git` would answer.
#
# Run with:   bash bin/test-build-frontend.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

# A pre-commit hook exports its live index to children. The fake git ignores
# them, but nothing here should carry a binding to the caller's repository.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/build-frontend.sh"

# The fake git, first on PATH for the whole run. Every invocation's argv is
# recorded in FAKE_GIT_LOG.
FAKE_GIT_BIN="$(mktemp -d)"
cp "$THIS_DIR/fake-git.sh" "$FAKE_GIT_BIN/git"
chmod +x "$FAKE_GIT_BIN/git"
export PATH="$FAKE_GIT_BIN:$PATH"
export FAKE_GIT_LOG="$FAKE_GIT_BIN/argv.log"
trap 'rm -rf "$FAKE_GIT_BIN"' EXIT
if [ "$(command -v git)" != "$FAKE_GIT_BIN/git" ]; then
    echo "test-build-frontend.sh: the fake git is not the git on PATH; refusing to run real git" >&2
    exit 2
fi

# The fixtures stamp themselves with the SAME functions the script under test
# stamps with. Reimplementing the id here would let the harness agree with a
# broken script, which is the one thing a staleness harness must not do.
# shellcheck source=lib-deploy-stamp.sh
. "$THIS_DIR/lib-deploy-stamp.sh"

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; }

# --- build a fake project tree in a temp dir -------------------------------
# Layout mirrors what build-frontend.sh expects relative to its parent dir.
#
# `make_tree ROOT nogit` skips the commit, for the one case whose subject is a
# checkout that is not a checkout at all.
make_tree() {
    local root="$1" nogit="${2:-}"
    mkdir -p "$root/bin" \
             "$root/agent-shim/claude/shim/src" \
             "$root/agent-shim/claude/shim/dist" \
             "$root/agent-shim/claude/shim-sidecar" \
             "$root/agent-shim/shim-store" \
             "$root/agent-shim/shim-lock" \
             "$root/agent-shim/logging/go" \
             "$root/proto/gen/go" \
             "$root/webapp/src" "$root/webapp/dist" \
             "$root/daemon/cmd/claude-repld" "$root/daemon/bin"
    cp "$SCRIPT_UNDER_TEST" "$root/bin/build-frontend.sh"
    cp "$THIS_DIR/lib-deploy-stamp.sh" "$root/bin/lib-deploy-stamp.sh"

    # Sources.
    echo "export const x = 1" > "$root/agent-shim/claude/shim/src/main.ts"
    echo "export const y = 1" > "$root/webapp/src/main.ts"
    echo "package main" > "$root/daemon/cmd/claude-repld/main.go"
    echo "package main" > "$root/agent-shim/shim-store/main.go"
    echo "package main" > "$root/agent-shim/claude/shim-sidecar/main.go"
    echo "package main" > "$root/agent-shim/shim-lock/main.go"
    echo "package logging" > "$root/agent-shim/logging/go/timestamp.go"
    echo "package proto" > "$root/proto/gen/go/proto.go"
    echo '{"scripts":{"build":"true"}}' > "$root/agent-shim/claude/shim/package.json"
    echo '{"scripts":{"build":"true"}}' > "$root/webapp/package.json"
    echo "module x" > "$root/daemon/go.mod"
    echo "module store" > "$root/agent-shim/shim-store/go.mod"
    echo "module sidecar" > "$root/agent-shim/claude/shim-sidecar/go.mod"
    echo "module lock" > "$root/agent-shim/shim-lock/go.mod"
    echo "module logging" > "$root/agent-shim/logging/go/go.mod"
    echo "module proto" > "$root/proto/gen/go/go.mod"

    # Sources the old mtime-scanned set never looked at, and which the twelve
    # merges of 2026-09-09 landed in: the shim's tests, and the shared logging
    # module. They are real members of each system's pathspec.
    mkdir -p "$root/agent-shim/claude/shim/test"
    echo "test('x', () => {})" > "$root/agent-shim/claude/shim/test/main.test.ts"

    # node_modules present so the stubs are never asked to `npm install`.
    mkdir -p "$root/agent-shim/claude/shim/node_modules" \
             "$root/webapp/node_modules"

    [ "$nogit" = "nogit" ] || commit_tree "$root"
}

# commit_tree ROOT — turn the fixture into a (fake-git) checkout. Staleness is
# read out of git, so there is nothing to assert without one.
commit_tree() {
    local root="$1"
    # Mirror the real repo, where every build output is ignored
    # (daemon/.gitignore, webapp/.gitignore, shim/.gitignore). Without this the
    # artifact a build just produced would itself make the tree "dirty" and
    # every build would read as stale forever.
    printf 'bin/\ndist/\nstore/\nstubs/\nhome/\nnode_modules/\nstaging/\n*.log\nout\nerr\n' > "$root/.gitignore"
    git -C "$root" init -q
    git -C "$root" add -A
    git -C "$root" commit -qm seed
}

# commit_change ROOT PATH CONTENT MESSAGE — land a committed source change, so
# the checkout moves to a new source revision with a CLEAN working tree. This is
# the shape a merge has, and the shape the old mtime rule kept missing.
commit_change() {
    local root="$1" rel="$2" content="$3" msg="$4"
    mkdir -p "$(dirname "$root/$rel")"
    printf '%s\n' "$content" >> "$root/$rel"
    git -C "$root" add -A
    git -C "$root" commit -qm "$msg"
}

# tree_stamp_path ROOT NAME — where NAME's `.source-tree` stamp lives in a
# fixture (HOME is redirected to $root/home, so the cache bin is in-fixture).
tree_stamp_path() {
    case "$2" in
        shim)                printf '%s' "$1/agent-shim/claude/shim/dist/.source-tree" ;;
        webapp)              printf '%s' "$1/webapp/dist/.source-tree" ;;
        daemon)              printf '%s' "$1/daemon/bin/.source-tree" ;;
        *)                   printf '%s' "$1/home/.cache/agent-repl/bin/.$2.source-tree" ;;
    esac
}

# stamp_fresh_source_trees ROOT — record, for every system, that its artifact
# was built from exactly the revision standing in the fixture.
stamp_fresh_source_trees() {
    local root="$1" name paths
    for name in shim webapp daemon shim-store shim-claude-sidecar shim-lock; do
        paths="$(deploy_stamp_system_paths "$name" "")"
        # shellcheck disable=SC2086
        write_source_tree "$(tree_stamp_path "$root" "$name")" \
                          "$(source_tree_id "$root" $paths || true)"
    done
}

# write_webapp_index PATH [HASH] — write an index.html shaped like the one Vite
# actually emits.
#
# THE SHAPE IS LOAD-BEARING, not decoration. build-frontend.sh takes the webapp
# BUILD ID from the content hash Vite fingerprints the entry bundle with, by
# reading the `src="/assets/index-<hash>.js"` reference straight out of this
# file, and it FAILS THE BUILD when the reference is absent. A fixture that
# wrote a bare marker here therefore did not model a built webapp at all: it
# modelled a corrupt one, and every fixture that reached the webapp path died on
# it. Every place this harness fakes a built webapp goes through here so the
# shape can never drift in one of them again.
write_webapp_index() {
    local path="$1" hash="${2:-BuiltHash0}"
    printf '<!doctype html><html><head><script type="module" crossorigin src="/assets/index-%s.js"></script></head><body><div id="root"></div></body></html>\n' \
           "$hash" > "$path"
}

# Fresh artifacts: built outputs PLUS the source-tree stamps saying they came
# from the revision the fixture is standing at, which is what "fresh" means now.
make_fresh_artifacts() {
    make_fresh_artifacts_files "$1"
    stamp_fresh_source_trees "$1"
}

# make_fresh_artifacts_files ROOT — the built outputs alone, in their live
# locations, without the stamps that say what they were built from.
make_fresh_artifacts_files() {
    local root="$1"
    echo built > "$root/agent-shim/claude/shim/dist/main.js"
    write_webapp_index "$root/webapp/dist/index.html"
    echo built > "$root/daemon/bin/claude-repld"
    mkdir -p "$root/home/.cache/agent-repl/bin"
    echo built > "$root/home/.cache/agent-repl/bin/shim-store"
    echo built > "$root/home/.cache/agent-repl/bin/shim-claude-sidecar"
    echo built > "$root/home/.cache/agent-repl/bin/shim-lock"
}

# PATH stubs for npm/go that log to $STUB_LOG and touch their artifact.
make_stubs() {
    local bindir="$1"
    mkdir -p "$bindir"
    cat > "$bindir/npm" <<'EOF'
#!/usr/bin/env bash
echo "npm $*" >> "$STUB_LOG"
# `npm ci` / `npm install` populate node_modules in the cwd (the store entry).
case "${1:-}" in
    ci|install) mkdir -p node_modules; echo installed > node_modules/.stamp ;;
esac
# Emulate `npm run build` producing dist output for whichever project we're in.
if [ -d src ] && [ -f package.json ]; then
    if [ -d dist ] || mkdir -p dist; then :; fi
    if grep -q webapp <<<"$PWD" 2>/dev/null || [ -f webapp.marker ]; then :; fi
fi
# Touch the conventional artifact for the cwd project. The webapp's index.html
# carries a Vite-shaped entry-bundle reference because build-frontend.sh reads
# the build id out of it and fails the build when it is missing.
#
# The outputs follow the same redirections the real builds honor, so a staging
# run lands where build-frontend.sh pointed it: the shim's SHIM_BUILD_OUTFILE
# (build.mjs's own override) and Vite's `--outDir`.
outdir=dist prev=""
for arg in "$@"; do
    [ "$prev" = "--outDir" ] && outdir="$arg"
    prev="$arg"
done
case "$PWD" in
    *shim*)
        out="${SHIM_BUILD_OUTFILE:-dist/main.js}"
        mkdir -p "$(dirname "$out")"; echo built > "$out"
        ;;
    *webapp*)
        mkdir -p "$outdir"
        printf '<!doctype html><script type="module" crossorigin src="/assets/index-%s.js"></script>\n' \
               "${WEBAPP_ENTRY_HASH:-BuiltHash0}" > "$outdir/index.html"
        ;;
esac
exit 0
EOF
    cat > "$bindir/go" <<'EOF'
#!/usr/bin/env bash
echo "go $*" >> "$STUB_LOG"
# `go build -o <out> ./cmd/...` — grab the -o target and touch it.
out=""
while [ $# -gt 0 ]; do
    [ "$1" = "-o" ] && { out="$2"; shift 2; continue; }
    shift
done
[ -n "$out" ] && { mkdir -p "$(dirname "$out")"; echo built > "$out"; }
exit 0
EOF
    chmod +x "$bindir/npm" "$bindir/go"
}

run_script() {
    # run_script ROOT [args...] — invoke the copied script with stubs on PATH and
    # the shared dep store pointed at STORE (default: ROOT's own throwaway store).
    local root="$1"; shift
    STUB_LOG="$root/stub.log" \
        AGENT_REPL_NODE_STORE="${STORE:-$root/store}" \
        AGENT_REPL_WORKTREE_ROOTS="${WORKTREE_ROOTS:-}" \
        AGENT_REPL_NODE_STORE_GRACE_MINS="${GRACE_MINS:-60}" \
        HOME="$root/home" \
        PATH="$root/stubs:$PATH" \
        bash "$root/bin/build-frontend.sh" "$@"
}

# --- gc helpers -------------------------------------------------------------

# store_key_of MANIFEST — mirror build-frontend.sh's store_key so a test can
# name the entry a given lockfile keys.
store_key_of() {
    { shasum -a 256 "$1" 2>/dev/null || sha256sum "$1"; } | cut -c1-16
}

# seed_entry STORE NAME [KB] — create a populated store entry, optionally with
# bulk so the size reporting has something to report.
seed_entry() {
    local store="$1" name="$2" kb="${3:-0}"
    mkdir -p "$store/$name/node_modules"
    if [ "$kb" -gt 0 ]; then
        dd if=/dev/zero of="$store/$name/node_modules/blob" bs=1024 count="$kb" 2>/dev/null
    fi
    return 0
}

# age_entries STORE — backdate every entry past any plausible grace window, so
# a test exercises the REFERENCE rules rather than the age rule.
age_entries() {
    local e
    for e in "$1"/*; do
        [ -d "$e" ] && touch -t 202001010000 "$e"
    done
    return 0
}

# --- Test 1: all fresh -> nothing rebuilt ----------------------------------
t_all_fresh() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    run_script "$root" >/dev/null
    if [ ! -s "$root/stub.log" ]; then
        pass "all-fresh: no build tool invoked"
    else
        fail "all-fresh: no build tool invoked" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# --- Test 2: one stale source -> only that artifact rebuilt -----------------
t_one_stale() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    commit_change "$root" agent-shim/claude/shim/src/main.ts "export const b = 2" "edit shim"
    # Shim is now stale; the other artifacts remain fresh.
    run_script "$root" >/dev/null
    local log; log="$(cat "$root/stub.log")"
    if grep -q "npm" <<<"$log" && ! grep -q "^go " <<<"$log"; then
        pass "one-stale: only shim (npm) rebuilt, daemon (go) skipped"
    else
        fail "one-stale: only shim rebuilt" "stub.log: $log"
    fi
    rm -rf "$root"
}

# --- Test 3: missing artifact -> rebuilt ------------------------------------
t_missing_artifact() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    rm -f "$root/daemon/bin/claude-repld"   # daemon artifact gone
    run_script "$root" daemon >/dev/null
    if grep -q "^go build" "$root/stub.log"; then
        pass "missing-artifact: absent daemon binary triggers go build"
    else
        fail "missing-artifact: daemon rebuilt" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# --- Test 4: --force -> rebuild despite fresh -------------------------------
t_force() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    run_script "$root" --force daemon >/dev/null
    if grep -q "^go build" "$root/stub.log"; then
        pass "force: fresh daemon still rebuilt under --force"
    else
        fail "force: fresh daemon rebuilt" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# --- Test 5: absent node_modules -> store populated once, symlinked in --------
t_deps_linked_from_store() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    rm -rf "$root/webapp/node_modules"
    run_script "$root" deps >/dev/null
    if [ -L "$root/webapp/node_modules" ] && [ -d "$root/webapp/node_modules" ] &&
           grep -q "npm install" "$root/stub.log"; then
        pass "deps: absent node_modules is installed once in the store and symlinked"
    else
        fail "deps: node_modules symlinked at store" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# --- Test 6: a second worktree reuses the store without reinstalling ----------
t_deps_store_shared_across_worktrees() {
    local store; store="$(mktemp -d)/store"
    local first; first="$(mktemp -d)"
    local second; second="$(mktemp -d)"
    make_tree "$first"; make_stubs "$first/stubs"; make_fresh_artifacts "$first"
    make_tree "$second"; make_stubs "$second/stubs"; make_fresh_artifacts "$second"
    rm -rf "$first/webapp/node_modules" "$second/webapp/node_modules"
    : > "$first/stub.log"; : > "$second/stub.log"
    STORE="$store" run_script "$first" deps >/dev/null
    STORE="$store" run_script "$second" deps >/dev/null
    if [ -d "$second/webapp/node_modules" ] && ! grep -q "npm" "$second/stub.log"; then
        pass "deps: second worktree links the populated store, no reinstall"
    else
        fail "deps: second worktree reuses store" "stub.log: $(cat "$second/stub.log")"
    fi
    rm -rf "$first" "$second" "$store"
}

# --- Test 7: a lockfile change keys a fresh store entry -----------------------
t_deps_lockfile_change_rekeys_store() {
    local store; store="$(mktemp -d)/store"
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    rm -rf "$root/webapp/node_modules"
    echo '{"lockfileVersion":3,"packages":{}}' > "$root/webapp/package-lock.json"
    STORE="$store" run_script "$root" deps >/dev/null
    rm -f "$root/webapp/node_modules"
    echo '{"lockfileVersion":3,"packages":{"x":{}}}' > "$root/webapp/package-lock.json"
    : > "$root/stub.log"
    STORE="$store" run_script "$root" deps >/dev/null
    if [ "$(find "$store" -maxdepth 1 -name 'webapp-*' | wc -l)" -eq 2 ] &&
           grep -q "npm ci" "$root/stub.log"; then
        pass "deps: changed lockfile keys a second store entry"
    else
        fail "deps: lockfile change rekeys store" "entries: $(find "$store" -maxdepth 1 -name 'webapp-*')"
    fi
    rm -rf "$root" "$store"
}

t_all_fresh
t_one_stale
t_missing_artifact
t_force
t_deps_linked_from_store
t_deps_store_shared_across_worktrees

# --- Test 8: gc collects a stranded entry, keeps the referenced one ----------
t_gc_collects_stranded_keeps_referenced() {
    local root; root="$(mktemp -d)"
    local store="$root/store"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    echo '{"lockfileVersion":3,"packages":{"live":{}}}' > "$root/webapp/package-lock.json"
    local live
    live="webapp-$(store_key_of "$root/webapp/package-lock.json")"
    seed_entry "$store" "$live" 64
    seed_entry "$store" "webapp-deadbeefdeadbeef" 512
    age_entries "$store"

    WORKTREE_ROOTS="$root" run_script "$root" gc >"$root/gc.log" 2>&1

    if [ -d "$store/$live" ] && [ ! -d "$store/webapp-deadbeefdeadbeef" ]; then
        pass "gc: stranded entry collected, referenced entry kept"
    else
        fail "gc: stranded collected / referenced kept" "$(cat "$root/gc.log")"
    fi
    rm -rf "$root"
}

# --- Test 9: an entry referenced only by a SYMLINK survives -------------------
# The safety-critical case: a worktree whose lockfile changed but which has not
# rebuilt is still POINTING at the old entry. Hashing the current lockfile alone
# would delete the deps out from under it.
t_gc_protects_symlink_target() {
    local root; root="$(mktemp -d)"
    local store="$root/store"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    # The entry the worktree is actually USING.
    seed_entry "$store" "webapp-0000000000000000" 64
    rm -rf "$root/webapp/node_modules"
    ln -sfn "$store/webapp-0000000000000000/node_modules" "$root/webapp/node_modules"
    # Its lockfile has since moved on to a different (not-yet-built) key.
    echo '{"lockfileVersion":3,"packages":{"moved-on":{}}}' > "$root/webapp/package-lock.json"
    age_entries "$store"

    WORKTREE_ROOTS="$root" run_script "$root" gc >"$root/gc.log" 2>&1

    if [ -d "$store/webapp-0000000000000000" ] && [ -d "$root/webapp/node_modules" ]; then
        pass "gc: entry referenced only by a node_modules symlink is protected"
    else
        fail "gc: symlink target protected" "$(cat "$root/gc.log")"
    fi
    rm -rf "$root"
}

# --- Test 10: another worktree's lockfile protects an entry -------------------
t_gc_protects_other_worktrees() {
    local store; store="$(mktemp -d)/store"
    local first; first="$(mktemp -d)"
    local second; second="$(mktemp -d)"
    make_tree "$first"; make_stubs "$first/stubs"; make_fresh_artifacts "$first"
    make_tree "$second"; make_stubs "$second/stubs"; make_fresh_artifacts "$second"
    echo '{"lockfileVersion":3,"packages":{"a":{}}}' > "$first/webapp/package-lock.json"
    echo '{"lockfileVersion":3,"packages":{"b":{}}}' > "$second/webapp/package-lock.json"
    local key_a key_b
    key_a="webapp-$(store_key_of "$first/webapp/package-lock.json")"
    key_b="webapp-$(store_key_of "$second/webapp/package-lock.json")"
    seed_entry "$store" "$key_a" 64
    seed_entry "$store" "$key_b" 64
    age_entries "$store"

    # Sweep from the FIRST worktree only; the second's entry must survive.
    STORE="$store" WORKTREE_ROOTS="$(printf '%s\n%s' "$first" "$second")" \
        run_script "$first" gc >"$first/gc.log" 2>&1

    if [ -d "$store/$key_a" ] && [ -d "$store/$key_b" ]; then
        pass "gc: an entry referenced by ANOTHER worktree is protected"
    else
        fail "gc: other worktree protected" "$(cat "$first/gc.log")"
    fi
    rm -rf "$first" "$second" "$store"
}

# --- Test 11: a held lock makes the sweep skip (concurrency guard) ------------
t_gc_skips_when_lock_held() {
    local root; root="$(mktemp -d)"
    local store="$root/store"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    seed_entry "$store" "webapp-deadbeefdeadbeef" 64
    age_entries "$store"
    # Stand in for a sweep already running in another worktree. Fresh, so the
    # stale-lock breaker must not fire.
    mkdir -p "$store/.gc.lock"

    WORKTREE_ROOTS="$root" run_script "$root" gc >"$root/gc.log" 2>&1

    if [ -d "$store/webapp-deadbeefdeadbeef" ] && grep -q "lock, skipping" "$root/gc.log"; then
        pass "gc: a held lock skips the sweep and collects nothing"
    else
        fail "gc: held lock skips sweep" "$(cat "$root/gc.log")"
    fi
    rm -rf "$root"
}

# --- Test 12: the lock is released so a later sweep can run ------------------
t_gc_releases_lock() {
    local root; root="$(mktemp -d)"
    local store="$root/store"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    seed_entry "$store" "webapp-deadbeefdeadbeef" 64
    seed_entry "$store" "webapp-cafecafecafecafe" 64
    age_entries "$store"

    WORKTREE_ROOTS="$root" run_script "$root" gc >/dev/null 2>&1
    # A second sweep must not find the lock still held.
    WORKTREE_ROOTS="$root" run_script "$root" gc >"$root/gc2.log" 2>&1

    if [ ! -d "$store/.gc.lock" ] && ! grep -q "lock, skipping" "$root/gc2.log"; then
        pass "gc: the sweep releases its lock"
    else
        fail "gc: lock released" "$(cat "$root/gc2.log")"
    fi
    rm -rf "$root"
}

# --- Test 13: --dry-run reports but deletes nothing ---------------------------
t_gc_dry_run_deletes_nothing() {
    local root; root="$(mktemp -d)"
    local store="$root/store"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    seed_entry "$store" "webapp-deadbeefdeadbeef" 512
    age_entries "$store"

    WORKTREE_ROOTS="$root" run_script "$root" gc --dry-run >"$root/gc.log" 2>&1

    if [ -d "$store/webapp-deadbeefdeadbeef" ] &&
           grep -q "WOULD collect webapp-deadbeefdeadbeef" "$root/gc.log"; then
        pass "gc: --dry-run reports the collection without performing it"
    else
        fail "gc: dry-run deletes nothing" "$(cat "$root/gc.log")"
    fi
    rm -rf "$root"
}

# --- Test 14: an entry inside the grace window survives -----------------------
# Guards a build in another worktree that is populating this entry right now.
t_gc_respects_grace_window() {
    local root; root="$(mktemp -d)"
    local store="$root/store"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    seed_entry "$store" "webapp-deadbeefdeadbeef" 64   # freshly created

    WORKTREE_ROOTS="$root" run_script "$root" gc -v >"$root/gc.log" 2>&1

    if [ -d "$store/webapp-deadbeefdeadbeef" ] && grep -q "grace" "$root/gc.log"; then
        pass "gc: an entry younger than the grace window is protected"
    else
        fail "gc: grace window respected" "$(cat "$root/gc.log")"
    fi
    rm -rf "$root"
}

# --- Test 15: minting a new entry triggers a sweep ---------------------------
t_gc_runs_after_minting_an_entry() {
    local root; root="$(mktemp -d)"
    local store="$root/store"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    rm -rf "$root/webapp/node_modules"
    echo '{"lockfileVersion":3,"packages":{"fresh":{}}}' > "$root/webapp/package-lock.json"
    seed_entry "$store" "webapp-deadbeefdeadbeef" 64
    age_entries "$store"

    # `deps` mints the webapp entry, which should pull the sweep in behind it.
    WORKTREE_ROOTS="$root" run_script "$root" deps >"$root/gc.log" 2>&1

    if [ ! -d "$store/webapp-deadbeefdeadbeef" ] && grep -q "gc: collecting" "$root/gc.log"; then
        pass "gc: a run that mints an entry sweeps afterwards"
    else
        fail "gc: sweep on mint" "$(cat "$root/gc.log")"
    fi
    rm -rf "$root"
}

# --- Test 16: a run that mints nothing does not sweep ------------------------
t_gc_not_run_when_nothing_minted() {
    local root; root="$(mktemp -d)"
    local store="$root/store"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    seed_entry "$store" "webapp-deadbeefdeadbeef" 64
    age_entries "$store"

    # Everything fresh and node_modules already present: nothing is minted, so
    # the (multi-second) enumeration must not run at all.
    WORKTREE_ROOTS="$root" run_script "$root" >"$root/gc.log" 2>&1

    if [ -d "$store/webapp-deadbeefdeadbeef" ] && ! grep -q "\[build-frontend\] gc:" "$root/gc.log"; then
        pass "gc: a run that mints nothing does not sweep"
    else
        fail "gc: no sweep without a mint" "$(cat "$root/gc.log")"
    fi
    rm -rf "$root"
}

# --- built-sha stamps -------------------------------------------------------

t_stamp_written_on_build() {
    local root sha; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"
    sha="$(git -C "$root" rev-parse HEAD)"
    run_script "$root" daemon >/dev/null
    if [ "$(cat "$root/daemon/bin/.built-sha" 2>/dev/null)" = "$sha" ]; then
        pass "stamp: a build records the source revision beside its artifact"
    else
        fail "stamp: a build records the source revision beside its artifact" \
             "want=$sha got=$(cat "$root/daemon/bin/.built-sha" 2>/dev/null)"
    fi
    rm -rf "$root"
}

t_stamp_marks_a_dirty_tree() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"
    echo "package main // edited" > "$root/daemon/cmd/claude-repld/main.go"
    run_script "$root" daemon >/dev/null
    if grep -q -- '-dirty$' "$root/daemon/bin/.built-sha"; then
        pass "stamp: a build off a dirty tree carries the -dirty marker"
    else
        fail "stamp: a build off a dirty tree carries the -dirty marker" \
             "stamp=$(cat "$root/daemon/bin/.built-sha" 2>/dev/null)"
    fi
    rm -rf "$root"
}

t_stamp_untouched_by_a_skipped_build() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    printf 'a-previous-revision\n' > "$root/daemon/bin/.built-sha"
    run_script "$root" daemon >/dev/null
    if [ "$(cat "$root/daemon/bin/.built-sha")" = "a-previous-revision" ]; then
        pass "stamp: a skipped (fresh) build leaves the existing stamp untouched"
    else
        fail "stamp: a skipped (fresh) build leaves the existing stamp untouched" \
             "stamp=$(cat "$root/daemon/bin/.built-sha")"
    fi
    rm -rf "$root"
}

t_no_git_leaves_no_stamp() {
    local root; root="$(mktemp -d)"
    make_tree "$root" nogit; make_stubs "$root/stubs"
    printf 'a-stale-guess\n' > "$root/daemon/bin/.built-sha"
    run_script "$root" daemon >/dev/null
    if [ ! -e "$root/daemon/bin/.built-sha" ]; then
        pass "stamp: a build outside a checkout drops the stamp rather than leaving a guess"
    else
        fail "stamp: a build outside a checkout drops the stamp rather than leaving a guess" \
             "stamp=$(cat "$root/daemon/bin/.built-sha")"
    fi
    rm -rf "$root"
}

t_deps_lockfile_change_rekeys_store
t_gc_collects_stranded_keeps_referenced
t_gc_protects_symlink_target
t_gc_protects_other_worktrees
t_gc_skips_when_lock_held
t_gc_releases_lock
t_gc_dry_run_deletes_nothing
t_gc_respects_grace_window
t_gc_runs_after_minting_an_entry

# --- awkward source filenames still reach the staleness check ---------------
# The pathspec list is passed to git unquoted (it is a space-separated table),
# so these pin that a FILENAME's own spaces and glob metacharacters are never
# taken as list separators or expanded against the cwd on the way there.

t_stale_source_with_space_in_name() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    # A source whose name contains a space.
    printf 'export const z = 1' > "$root/webapp/src/my component.ts"
    run_script "$root" webapp >/dev/null
    if grep -q npm "$root/stub.log"; then
        pass "staleness: a source filename containing a space triggers a rebuild"
    else
        fail "staleness: space in source filename" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_stale_source_with_glob_chars() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    # Glob metacharacters in a source name must be taken literally, never
    # expanded against the cwd on their way to the staleness check.
    printf 'export const z = 1' > "$root/webapp/src/a[1]*.ts"
    run_script "$root" webapp >/dev/null
    if grep -q npm "$root/stub.log"; then
        pass "staleness: a source filename with glob metacharacters triggers a rebuild"
    else
        fail "staleness: glob chars in source filename" "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# An UNDETERMINABLE source set is stale, never fresh. Outside a checkout there
# is no revision to compare against, and answering "fresh" there is how a build
# talks itself out of work it owes — the artifact and its stamp could have come
# from anywhere.
t_undeterminable_source_set_rebuilds() {
    local root; root="$(mktemp -d)"
    make_tree "$root" nogit; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    if run_script "$root" webapp >/dev/null 2>&1; then
        if grep -q "npm run build" "$root/stub.log"; then
            pass "staleness: a source set that cannot be determined rebuilds rather than skipping"
        else
            fail "staleness: undeterminable source set rebuilds" \
                 "stub.log: $(cat "$root/stub.log")"
        fi
    else
        fail "staleness: undeterminable source set rebuilds" "script exited non-zero"
    fi
    rm -rf "$root"
}

t_services_fresh_then_shared_dependency_stales_both() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    run_script "$root" store sidecar >/dev/null
    if [ -s "$root/stub.log" ]; then
        fail "services: fresh installed binaries skip go build" \
             "stub.log: $(cat "$root/stub.log")"
        rm -rf "$root"
        return
    fi
    commit_change "$root" proto/gen/go/proto.go "// regenerated" "regen proto"
    run_script "$root" store sidecar >/dev/null
    if [ "$(grep -c '^go build' "$root/stub.log")" -eq 2 ]; then
        pass "services: shared proto edit rebuilds store and sidecar"
    else
        fail "services: shared proto edit rebuilds both" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_services_shared_logging_edit_rebuilds_both() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    run_script "$root" store sidecar >/dev/null
    if [ -s "$root/stub.log" ]; then
        fail "services: fresh installed binaries skip go build (logging case)" \
             "stub.log: $(cat "$root/stub.log")"
        rm -rf "$root"
        return
    fi
    commit_change "$root" agent-shim/logging/go/timestamp.go "// rotated" "edit logging"
    run_script "$root" store sidecar >/dev/null
    if [ "$(grep -c '^go build' "$root/stub.log")" -eq 2 ]; then
        pass "services: shared logging edit rebuilds store and sidecar"
    else
        fail "services: shared logging edit rebuilds both" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_services_missing_shared_source_fails_loudly() {
    local root rc; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    rm -rf "$root/proto/gen/go"
    set +e
    run_script "$root" store >"$root/out" 2>"$root/err"
    rc=$?
    set -e
    if [ "$rc" -ne 0 ] \
       && grep -q "required Go source directory missing" "$root/err"; then
        pass "services: missing shared source directory fails loudly"
    else
        fail "services: missing shared source directory fails loudly" \
             "rc=$rc stderr: $(cat "$root/err")"
    fi
    rm -rf "$root"
}

t_gc_not_run_when_nothing_minted
t_stamp_written_on_build
t_stamp_marks_a_dirty_tree
t_stamp_untouched_by_a_skipped_build
t_no_git_leaves_no_stamp
t_stale_source_with_space_in_name
t_stale_source_with_glob_chars
t_undeterminable_source_set_rebuilds
# The daemon compiles against the generated proto module, so a regenerated
# binding must stale daemon/bin/claude-repld even though it lives outside
# daemon/.
t_daemon_shared_proto_edit_rebuilds() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    run_script "$root" daemon >/dev/null
    if [ -s "$root/stub.log" ]; then
        fail "daemon: a fresh binary skips go build" \
             "stub.log: $(cat "$root/stub.log")"
        rm -rf "$root"
        return
    fi
    commit_change "$root" proto/gen/go/proto.go "// regenerated" "regen proto"
    run_script "$root" daemon >/dev/null
    if [ "$(grep -c '^go build' "$root/stub.log")" -eq 1 ]; then
        pass "daemon: a regenerated proto binding rebuilds the daemon"
    else
        fail "daemon: a regenerated proto binding rebuilds the daemon" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_services_fresh_then_shared_dependency_stales_both
t_services_shared_logging_edit_rebuilds_both
t_services_missing_shared_source_fails_loudly
t_daemon_shared_proto_edit_rebuilds

# --- the shim bundle carries the SAME revision its stamp records ------------
# The daemon's stale-shim refresh compares the two, so a build that baked one
# value and stamped another would bounce healthy shims forever.
t_shim_bundle_and_stamp_share_one_revision() {
    local root sha baked stamped; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"
    # An npm stub that records the revision the build handed the bundler.
    cat > "$root/stubs/npm" <<'EOF'
#!/usr/bin/env bash
echo "npm $*" >> "$STUB_LOG"
case "${1:-}" in
    ci|install) mkdir -p node_modules; echo installed > node_modules/.stamp ;;
esac
case "$PWD" in
    *shim*)   mkdir -p dist; printf '%s' "${SHIM_BUILD_SHA:-unset}" > dist/main.js ;;
    *webapp*)
        mkdir -p dist
        printf '<!doctype html><script type="module" crossorigin src="/assets/index-%s.js"></script>\n' \
               "${WEBAPP_ENTRY_HASH:-BuiltHash0}" > dist/index.html
        ;;
esac
exit 0
EOF
    chmod +x "$root/stubs/npm"
    sha="$(git -C "$root" rev-parse HEAD)"
    run_script "$root" shim >/dev/null
    baked="$(cat "$root/agent-shim/claude/shim/dist/main.js" 2>/dev/null || echo MISSING)"
    stamped="$(cat "$root/agent-shim/claude/shim/dist/.built-sha" 2>/dev/null || echo MISSING)"
    if [ "$baked" = "$sha" ] && [ "$stamped" = "$sha" ]; then
        pass "shim: the bundle is baked with the same revision the stamp records"
    else
        fail "shim: the bundle is baked with the same revision the stamp records" \
             "want=$sha baked=$baked stamped=$stamped"
    fi
    rm -rf "$root"
}
t_shim_bundle_and_stamp_share_one_revision

# --- editing build.mjs stales the shim --------------------------------------
# build.mjs is the build DEFINITION — it is what injects the build identity —
# so a change to it must rebuild the bundle even though it lives outside src/.
t_build_mjs_stales_the_shim() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    commit_change "$root" agent-shim/claude/shim/build.mjs "// bundler tweak" "edit build.mjs"
    run_script "$root" shim >/dev/null
    if grep -q "npm run build" "$root/stub.log"; then
        pass "shim: a build.mjs edit stales the bundle"
    else
        fail "shim: a build.mjs edit stales the bundle" "no rebuild was triggered"
    fi
    rm -rf "$root"
}
t_build_mjs_stales_the_shim

# --- the webapp build id is the entry bundle's own content hash --------------
# The webview's URL carries this value, so a build that did not record it leaves
# the artifact standing beside it unaddressable.
t_webapp_build_id_is_the_entry_hash() {
    local root got; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"
    WEBAPP_ENTRY_HASH=CafeBabe01 run_script "$root" webapp >/dev/null
    got="$(cat "$root/webapp/dist/.build-id" 2>/dev/null || echo MISSING)"
    if [ "$got" = "CafeBabe01" ]; then
        pass "webapp: the build id is the entry bundle's content hash"
    else
        fail "webapp: the build id is the entry bundle's content hash" \
             "want=CafeBabe01 got=$got"
    fi
    rm -rf "$root"
}
t_webapp_build_id_is_the_entry_hash

# A SKIPPED build still owes the id: the artifact standing there is the one the
# webview must address, and a stamp missing beside it leaves that address
# unbuildable.
t_webapp_build_id_written_by_a_skipped_build() {
    local root got; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    write_webapp_index "$root/webapp/dist/index.html" FreshHash7
    : > "$root/stub.log"
    run_script "$root" webapp >/dev/null
    got="$(cat "$root/webapp/dist/.build-id" 2>/dev/null || echo MISSING)"
    if [ "$got" = "FreshHash7" ] && ! grep -q "npm run build" "$root/stub.log"; then
        pass "webapp: a skipped build still stamps the build id"
    else
        fail "webapp: a skipped build still stamps the build id" \
             "want=FreshHash7 got=$got stubs=$(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}
t_webapp_build_id_written_by_a_skipped_build

# An index.html with no entry reference is a CORRUPT artifact, not a buildable
# one. Continuing past it would deploy a webapp whose url addresses nothing.
t_webapp_build_id_missing_entry_fails_loudly() {
    local root out rc; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    echo "no entry bundle here" > "$root/webapp/dist/index.html"
    set +e
    out="$(run_script "$root" webapp 2>&1)"
    rc=$?
    set -e
    if [ "$rc" -ne 0 ] && grep -q "FAILED to read the entry bundle hash" <<<"$out"; then
        pass "webapp: an index.html with no entry bundle fails the build loudly"
    else
        fail "webapp: an index.html with no entry bundle fails the build loudly" \
             "rc=$rc out=$out"
    fi
    rm -rf "$root"
}
t_webapp_build_id_missing_entry_fails_loudly

# --- shim-lock -------------------------------------------------------------
# The shim spawns shim-lock for every kernel claim and refuses to start a
# session without it, which is why `lock` is in the DEFAULT target set while the
# two launchd services are not. Both halves are asserted: that a default run
# builds it, and that it installs beside shim-store.
t_lock_is_in_the_default_target_set() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    commit_change "$root" agent-shim/shim-lock/main.go "// lock tweak" "edit shim-lock"
    run_script "$root" >/dev/null
    if grep -q "shim-lock" "$root/stub.log"; then
        pass "lock: a default run rebuilds a stale shim-lock"
    else
        fail "lock: a default run rebuilds a stale shim-lock" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}
t_lock_is_in_the_default_target_set

t_lock_installs_beside_shim_store() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"
    run_script "$root" lock >/dev/null
    if [ -f "$root/home/.cache/agent-repl/bin/shim-lock" ]; then
        pass "lock: installs into ~/.cache/agent-repl/bin, beside shim-store"
    else
        fail "lock: installs into ~/.cache/agent-repl/bin" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}
t_lock_installs_beside_shim_store

# --- staleness is a SOURCE REVISION question, not an mtime question ---------
#
# These four are the 2026-09-09 deploy defect, pinned from both ends. Twelve
# merges landed into master; build-frontend judged the shim bundle and the
# webapp dist fresh and skipped both, and the readiness gate then reported the
# shim three commits behind and the webapp four — a state only `--force` could
# clear. Two independent causes, one per pair below.

# CAUSE 1: the source set was narrower than the one the gate measures. The shim
# scanned `shim/src` plus a few manifests; the merges touched `shim/test/`.
t_committed_change_outside_src_stales_the_shim() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    commit_change "$root" agent-shim/claude/shim/test/main.test.ts \
                  "test('y', () => {})" "test(shim): another case"
    run_script "$root" shim >/dev/null
    if grep -q "npm run build" "$root/stub.log"; then
        pass "staleness: a committed change under shim/test stales the shim bundle"
    else
        fail "staleness: a committed change under shim/test stales the shim bundle" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# The webapp's half of the same cause: the merges touched `webapp/test/` and the
# shared logging module, neither of which the old scan looked at.
t_committed_change_in_shared_logging_stales_the_webapp() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    commit_change "$root" agent-shim/logging/go/timestamp.go \
                  "// size-capped with N generations" "feat(logging): rotate"
    run_script "$root" webapp >/dev/null
    if grep -q "npm run build" "$root/stub.log"; then
        pass "staleness: a committed change to shared logging stales the webapp"
    else
        fail "staleness: a committed change to shared logging stales the webapp" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# CAUSE 2: an mtime is wall-clock metadata, not content. A checkout, a rebase,
# or a copy can hand a genuinely changed file an mtime OLDER than the artifact,
# and the prerequisite-newer-than-target rule then calls it fresh forever.
t_older_mtime_source_change_still_rebuilds() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    commit_change "$root" daemon/cmd/claude-repld/main.go \
                  "// changed" "fix(daemon): a real change"
    # Backdate the changed source far behind the artifact. Under the old rule
    # this made the daemon read as fresh; under a revision comparison the
    # timestamp is not consulted at all.
    touch -t 200001010000 "$root/daemon/cmd/claude-repld/main.go"
    run_script "$root" daemon >/dev/null
    if grep -q "^go build" "$root/stub.log"; then
        pass "staleness: a source change with an OLDER mtime than the artifact still rebuilds"
    else
        fail "staleness: a source change with an OLDER mtime than the artifact still rebuilds" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# The converse, and the reason the rule can be trusted to skip: a tree that has
# not moved is not rebuilt, however the artifact's timestamps look.
t_unchanged_tree_skips_even_with_an_older_artifact() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    # An artifact older than every source: stale under the mtime rule, fresh
    # under a revision comparison, and fresh is the honest answer — nothing
    # about the source has changed.
    touch -t 200001010000 "$root/daemon/bin/claude-repld"
    run_script "$root" daemon >/dev/null
    if [ ! -s "$root/stub.log" ]; then
        pass "staleness: an unchanged source revision skips the build"
    else
        fail "staleness: an unchanged source revision skips the build" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# A dirty working tree always rebuilds. A source-tree id is anchored to
# committed content and cannot describe an uncommitted edit, so the only honest
# answer while one is outstanding is to build.
t_dirty_tree_always_rebuilds() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    printf '// uncommitted\n' >> "$root/daemon/cmd/claude-repld/main.go"
    run_script "$root" daemon >/dev/null
    if grep -q "^go build" "$root/stub.log"; then
        pass "staleness: an uncommitted edit under the system's sources rebuilds"
    else
        fail "staleness: an uncommitted edit under the system's sources rebuilds" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# A committed change OUTSIDE the system's pathspec leaves it alone. Without
# this, a shared staleness rule would just rebuild everything on every commit
# and the skip would be worthless.
t_change_outside_the_pathspec_leaves_the_system_fresh() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"; make_fresh_artifacts "$root"
    : > "$root/stub.log"
    commit_change "$root" webapp/src/main.ts "export const w = 2" "feat(webapp): edit"
    run_script "$root" daemon >/dev/null
    if [ ! -s "$root/stub.log" ]; then
        pass "staleness: a commit outside the system's pathspec leaves it fresh"
    else
        fail "staleness: a commit outside the system's pathspec leaves it fresh" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# The stamp is what makes the skip possible, so a build must leave one behind.
t_build_records_the_source_tree_stamp() {
    local root recorded expected paths; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"
    run_script "$root" daemon >/dev/null
    recorded="$(cat "$root/daemon/bin/.source-tree" 2>/dev/null || echo MISSING)"
    paths="$(deploy_stamp_system_paths daemon "")"
    # shellcheck disable=SC2086
    expected="$(source_tree_id "$root" $paths)"
    if [ "$recorded" = "$expected" ]; then
        pass "stamp: a build records the source-tree id its artifact came from"
    else
        fail "stamp: a build records the source-tree id its artifact came from" \
             "want=$expected got=$recorded"
    fi
    rm -rf "$root"
}

t_committed_change_outside_src_stales_the_shim
t_committed_change_in_shared_logging_stales_the_webapp
t_older_mtime_source_change_still_rebuilds
t_unchanged_tree_skips_even_with_an_older_artifact
t_dirty_tree_always_rebuilds
t_change_outside_the_pathspec_leaves_the_system_fresh
t_build_records_the_source_tree_stamp

# --- -buildvcs=false on every go build ---------------------------------------
# The deploy decides staleness by each binary's CONTENT HASH, and Go's default
# VCS stamping embeds the commit and dirty flag, so without the flag every
# commit would change every binary and bounce every service for nothing.

# go_builds_all_pass_buildvcs_false LOG — true when LOG records at least one go
# build and every one of them carried -buildvcs=false.
go_builds_all_pass_buildvcs_false() {
    local log="$1"
    grep -q '^go build' "$log" || return 1
    ! grep '^go build' "$log" | grep -qv -- ' -buildvcs=false '
}

t_in_place_go_builds_pass_buildvcs_false() {
    local root; root="$(mktemp -d)"
    make_tree "$root"; make_stubs "$root/stubs"
    : > "$root/stub.log"
    run_script "$root" daemon store sidecar lock >/dev/null
    if [ "$(grep -c '^go build' "$root/stub.log")" -eq 4 ] &&
           go_builds_all_pass_buildvcs_false "$root/stub.log"; then
        pass "buildvcs: every in-place go build passes -buildvcs=false"
    else
        fail "buildvcs: every in-place go build passes -buildvcs=false" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}
t_in_place_go_builds_pass_buildvcs_false

# --- staging mode (--out DIR) ------------------------------------------------
#
# THE STAGED LAYOUT IS A CONTRACT the daemon's deploy (daemon/internal/deploy)
# reads, so every slot is pinned by path here.
#
# Staging mode never consults staleness, so the fixture's (fake) history feeds
# only the stamps, and their expected content is read back off the same fake.

# new_staging_root — a committed fixture with no live artifacts. Echoes the root.
new_staging_root() {
    local root; root="$(mktemp -d)"
    make_tree "$root"
    make_stubs "$root/stubs"
    : > "$root/stub.log"
    printf '%s' "$root"
}

# staging_head ROOT — the revision the fixture stands at.
staging_head() { git -C "$1" rev-parse HEAD; }

# fake_source_tree NAME — the source-tree id of NAME in STAGING_ROOT, computed
# by the SAME library function the script stamps with.
fake_source_tree() {
    local paths
    paths="$(deploy_stamp_system_paths "$1" "")"
    # shellcheck disable=SC2086
    source_tree_id "$STAGING_ROOT" $paths
}

# live_files ROOT — every file standing in the fixture's LIVE artifact
# locations, one per line.
live_files() {
    local root="$1" dir
    for dir in "$root/agent-shim/claude/shim/dist" "$root/webapp/dist" \
               "$root/daemon/bin" "$root/home/.cache/agent-repl/bin"; do
        [ -d "$dir" ] && find "$dir" -type f
    done
    return 0
}

t_out_stages_the_shim() {
    local root out dist; root="$(new_staging_root)"; STAGING_ROOT="$root"
    out="$root/staging"; dist="$out/agent-shim/claude/shim/dist"
    run_script "$root" --out "$out" shim >/dev/null
    if [ "$(cat "$dist/main.js" 2>/dev/null)" = built ] &&
           [ "$(cat "$dist/.built-sha" 2>/dev/null)" = "$(staging_head "$root")" ] &&
           [ "$(cat "$dist/.source-tree" 2>/dev/null)" = "$(fake_source_tree shim)" ]; then
        pass "staging: shim -> DIR/agent-shim/claude/shim/dist/main.js with its stamps"
    else
        fail "staging: shim -> DIR/agent-shim/claude/shim/dist/main.js with its stamps" \
             "staged: $(find "$out" -type f 2>/dev/null)"
    fi
    rm -rf "$root"
}

t_out_stages_the_webapp() {
    local root out dist; root="$(new_staging_root)"; STAGING_ROOT="$root"
    out="$root/staging"; dist="$out/webapp/dist"
    WEBAPP_ENTRY_HASH=StagedHash1 run_script "$root" --out "$out" webapp >/dev/null
    if [ -f "$dist/index.html" ] &&
           [ "$(cat "$dist/.built-sha" 2>/dev/null)" = "$(staging_head "$root")" ] &&
           [ "$(cat "$dist/.source-tree" 2>/dev/null)" = "$(fake_source_tree webapp)" ] &&
           [ "$(cat "$dist/.build-id" 2>/dev/null)" = StagedHash1 ] &&
           grep -qxF "npm run build -- --outDir $dist --emptyOutDir" "$root/stub.log"; then
        pass "staging: webapp -> the whole dist in DIR/webapp/dist with its stamps"
    else
        fail "staging: webapp -> the whole dist in DIR/webapp/dist with its stamps" \
             "staged: $(find "$out" -type f 2>/dev/null) stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_out_stages_the_daemon() {
    local root out bin; root="$(new_staging_root)"; STAGING_ROOT="$root"
    out="$root/staging"; bin="$out/daemon/bin"
    run_script "$root" --out "$out" daemon >/dev/null
    if [ -f "$bin/claude-repld" ] &&
           [ "$(cat "$bin/.built-sha" 2>/dev/null)" = "$(staging_head "$root")" ] &&
           [ "$(cat "$bin/.source-tree" 2>/dev/null)" = "$(fake_source_tree daemon)" ]; then
        pass "staging: daemon -> DIR/daemon/bin/claude-repld with its stamps"
    else
        fail "staging: daemon -> DIR/daemon/bin/claude-repld with its stamps" \
             "staged: $(find "$out" -type f 2>/dev/null)"
    fi
    rm -rf "$root"
}

# t_out_stages_a_cache_bin_service TARGET NAME — one staged cache-bin binary.
t_out_stages_a_cache_bin_service() {
    local target="$1" name="$2" root out bin; root="$(new_staging_root)"; STAGING_ROOT="$root"
    out="$root/staging"; bin="$out/cache-bin"
    run_script "$root" --out "$out" "$target" >/dev/null
    if [ -f "$bin/$name" ] &&
           [ "$(cat "$bin/.$name.built-sha" 2>/dev/null)" = "$(staging_head "$root")" ] &&
           [ "$(cat "$bin/.$name.source-tree" 2>/dev/null)" = "$(fake_source_tree "$name")" ]; then
        pass "staging: $target -> DIR/cache-bin/$name with its stamps"
    else
        fail "staging: $target -> DIR/cache-bin/$name with its stamps" \
             "staged: $(find "$out" -type f 2>/dev/null)"
    fi
    rm -rf "$root"
}

t_out_writes_nothing_live() {
    local root live; root="$(new_staging_root)"
    run_script "$root" --out "$root/staging" shim webapp daemon store sidecar lock >/dev/null
    live="$(live_files "$root")"
    if [ -z "$live" ] && [ "$(grep -c 'build' "$root/stub.log")" -eq 6 ]; then
        pass "staging: a staging run writes nothing into the live artifact locations"
    else
        fail "staging: a staging run writes nothing into the live artifact locations" \
             "live: $live stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# In place, a fixture whose stamps match the source revision is fresh and
# nothing builds, services included. This is the control for the staging case
# below it: the same fixture under --out must build every target.
t_in_place_fresh_fixture_builds_nothing() {
    local root; root="$(new_staging_root)"
    make_fresh_artifacts "$root"
    run_script "$root" shim webapp daemon store sidecar lock >/dev/null
    if [ ! -s "$root/stub.log" ]; then
        pass "staleness: in place, fresh stamps still skip every build"
    else
        fail "staleness: in place, fresh stamps still skip every build" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

# Fresh by every stamp, live AND staged: the staging dir already holds a
# complete build of this exact revision from a previous run. Staging never
# consults a stamp, so every target builds again anyway.
t_out_builds_fresh_targets_unconditionally() {
    local root; root="$(new_staging_root)"
    make_fresh_artifacts "$root"
    run_script "$root" --out "$root/staging" shim webapp daemon store sidecar lock >/dev/null
    : > "$root/stub.log"
    run_script "$root" --out "$root/staging" shim webapp daemon store sidecar lock >/dev/null
    if [ "$(grep -c '^npm run build' "$root/stub.log")" -eq 2 ] &&
           [ "$(grep -c '^go build' "$root/stub.log")" -eq 4 ]; then
        pass "staging: every selected target builds even when the live and staged ones are fresh"
    else
        fail "staging: every selected target builds even when the live and staged ones are fresh" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_out_go_builds_pass_buildvcs_false() {
    local root; root="$(new_staging_root)"
    run_script "$root" --out "$root/staging" daemon store sidecar lock >/dev/null
    if [ "$(grep -c '^go build' "$root/stub.log")" -eq 4 ] &&
           go_builds_all_pass_buildvcs_false "$root/stub.log"; then
        pass "buildvcs: every staged go build passes -buildvcs=false"
    else
        fail "buildvcs: every staged go build passes -buildvcs=false" \
             "stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_out_refuses_a_relative_dir() {
    local root rc out; root="$(new_staging_root)"
    set +e
    out="$(run_script "$root" --out relative/staging daemon 2>&1)"
    rc=$?
    set -e
    if [ "$rc" -eq 1 ] && grep -q "must be an absolute directory" <<<"$out" &&
           [ ! -s "$root/stub.log" ]; then
        pass "staging: a relative --out dir is refused with exit 1 before any build"
    else
        fail "staging: a relative --out dir is refused with exit 1 before any build" \
             "rc=$rc out=$out stub.log: $(cat "$root/stub.log")"
    fi
    rm -rf "$root"
}

t_out_creates_an_absent_dir() {
    local root; root="$(new_staging_root)"
    run_script "$root" --out "$root/not/yet/there" deps >/dev/null
    if [ -d "$root/not/yet/there" ]; then
        pass "staging: an absent --out dir is created"
    else
        fail "staging: an absent --out dir is created" "no $root/not/yet/there"
    fi
    rm -rf "$root"
}

t_out_stages_the_shim
t_out_stages_the_webapp
t_out_stages_the_daemon
t_out_stages_a_cache_bin_service store shim-store
t_out_stages_a_cache_bin_service sidecar shim-claude-sidecar
t_out_stages_a_cache_bin_service lock shim-lock
t_out_writes_nothing_live
t_in_place_fresh_fixture_builds_nothing
t_out_builds_fresh_targets_unconditionally
t_out_go_builds_pass_buildvcs_false
t_out_refuses_a_relative_dir
t_out_creates_an_absent_dir

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]

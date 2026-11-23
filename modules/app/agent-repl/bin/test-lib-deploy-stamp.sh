#!/usr/bin/env bash
# Hermetic tests for the source sets in bin/lib-deploy-stamp.sh: a target's
# source-tree id changes if and only if a file its build reads changed.
#
# The build tools run for real over small fixtures that use the real layout
# (`go list` for the Go targets, esbuild through bin/source-files.mjs for the
# shim and the webapp, bin/source-files.mjs's SwiftPM reader over a stubbed
# `swift package describe` for the launcher). The checkout is bin/fake-git.sh's
# model: NO REAL GIT RUNS HERE (owner rule).

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="$(cd "$THIS_DIR/.." && pwd -P)"

# shellcheck source=lib-fake-git-fixture.sh
. "$THIS_DIR/lib-fake-git-fixture.sh"
clear_live_git_bindings
# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"
# shellcheck source=lib-deploy-stamp.sh
. "$THIS_DIR/lib-deploy-stamp.sh"

TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin"
"$THIS_DIR/fake-git.sh" --install "$TMP/bin/git"
export PATH="$TMP/bin:$PATH"
if [ "$(command -v git)" != "$TMP/bin/git" ]; then
    echo "test-lib-deploy-stamp.sh: the fake git is not the git on PATH; refusing to run real git" >&2
    exit 2
fi
# The fixtures' Go modules need nothing from the network or a newer toolchain.
export GOFLAGS=-mod=mod GOPROXY=off GOTOOLCHAIN=local GOWORK=off

# esbuild comes from the shim's installed dependencies, which the fixtures'
# packages link to as the real ones link into the shared store.
"$THIS_DIR/ensure-deps.sh" "$MODULE_DIR/agent-shim/claude/shim" >/dev/null
NODE_MODULES="$MODULE_DIR/agent-shim/claude/shim/node_modules"

# write ROOT PATH CONTENT — write a fixture file, making its directory.
write() {
    mkdir -p "$(dirname "$1/$2")"
    printf '%s\n' "$3" > "$1/$2"
}

# commit ROOT MESSAGE — commit everything in the fixture.
commit() {
    git -C "$1" add -A
    git -C "$1" commit -qm "$2"
}

# edit ROOT PATH — change a fixture file's content and commit it.
edit() {
    printf '// edit\n' >> "$1/$2"
    commit "$1" "edit $2"
}

# system_id ROOT NAME — NAME's source-tree id in the fixture, as the build and
# the readiness report compute it, with its stderr shown on failure.
system_id() {
    deploy_stamp_source_id "$2" "$1" "" || echo "NO-ID"
}

# assert_id_kept / assert_id_changes DESCRIPTION ROOT NAME PATH — edit PATH
# in the fixture and compare NAME's id before and after.
assert_edit_effect() {
    local want="$1" what="$2" root="$3" name="$4" path="$5" before after
    before="$(system_id "$root" "$name")"
    edit "$root" "$path"
    after="$(system_id "$root" "$name")"
    case "$before" in NO-*) fail "$what" "no id before the edit: $before"; return ;; esac
    if [ "$want" = keeps ] && [ "$before" = "$after" ]; then
        pass "$what"
    elif [ "$want" = changes ] && [ "$before" != "$after" ] && [ "$after" != "NO-ID" ]; then
        pass "$what"
    else
        fail "$what" "edited $path: id before=$before after=$after, wanted it to be $want"
    fi
}

# --- Go targets -------------------------------------------------------------

# go_fixture ROOT — the real layout, small: the generated proto module with
# two packages, the shared logging module, and three services.
#   shim-store    imports proto/a and logging, embeds data.txt
#   zettel        imports proto/b and logging
#   shim-lock     imports logging only
go_fixture() {
    local root="$1"
    mkdir -p "$root"
    write "$root" agent-shim/logging/go/go.mod 'module agentrepl/logging'
    write "$root" agent-shim/logging/go/log.go 'package logging

func Log() string { return "log" }'
    write "$root" agent-shim/logging/go/unused/unused.go 'package unused'
    write "$root" proto/gen/go/go.mod 'module agentrepl/proto'
    write "$root" proto/gen/go/a/a.go 'package a

const A = "a"'
    write "$root" proto/gen/go/b/b.go 'package b

const B = "b"'
    write "$root" agent-shim/shim-store/go.mod 'module agentrepl/shim-store

go 1.24.0

require (
	agentrepl/logging v0.0.0
	agentrepl/proto v0.0.0
)

replace agentrepl/proto => ../../proto/gen/go

replace agentrepl/logging => ../logging/go'
    write "$root" agent-shim/shim-store/go.sum ''
    write "$root" agent-shim/shim-store/data.txt 'embedded'
    write "$root" agent-shim/shim-store/main.go 'package main

import (
	_ "embed"

	"agentrepl/logging"
	"agentrepl/proto/a"
)

//go:embed data.txt
var data string

func main() { _ = logging.Log() + a.A + data }'
    write "$root" agent-shim/shim-store/main_test.go 'package main

import "testing"

func TestMain(t *testing.T) {}'
    write "$root" zettel/go.mod 'module agentrepl/zettel

go 1.24.0

require (
	agentrepl/logging v0.0.0
	agentrepl/proto v0.0.0
)

replace agentrepl/proto => ../proto/gen/go

replace agentrepl/logging => ../agent-shim/logging/go'
    write "$root" zettel/main.go 'package main

import (
	"agentrepl/logging"
	"agentrepl/proto/b"
)

func main() { _ = logging.Log() + b.B }'
    write "$root" agent-shim/shim-lock/go.mod 'module agentrepl/shim-lock

go 1.24.0

require agentrepl/logging v0.0.0

replace agentrepl/logging => ../logging/go'
    write "$root" agent-shim/shim-lock/main.go 'package main

import "agentrepl/logging"

func main() { _ = logging.Log() }'
    printf '.fakegit\n' > "$root/.gitignore"
    git -C "$root" init -q
    commit "$root" seed
}

new_go_fixture() {
    local root; root="$(mktemp -d "$TMP/go.XXXXXX")"
    go_fixture "$root"
    printf '%s' "$root"
}

test_go_files_are_the_packages_the_build_compiles() {
    local root files want
    root="$(new_go_fixture)"
    files="$(deploy_stamp_system_files shim-store "$root" "" | tr '\n' ' ')"
    want="agent-shim/logging/go/go.mod agent-shim/logging/go/log.go agent-shim/shim-store/data.txt agent-shim/shim-store/go.mod agent-shim/shim-store/go.sum agent-shim/shim-store/main.go proto/gen/go/a/a.go proto/gen/go/go.mod "
    if [ "$files" = "$want" ]; then
        pass "go: the files are the imported packages' sources, the embedded file, the modules' go.mod and the main go.sum"
    else
        fail "go: the files are the imported packages' sources, the embedded file, the modules' go.mod and the main go.sum" \
             "want: $want" "got:  $files"
    fi
}

test_go_change_in_a_package_the_target_does_not_import_keeps_the_id() {
    local root; root="$(new_go_fixture)"
    assert_edit_effect keeps "go: a change in a generated package the target does not import keeps its id" \
        "$root" zettel proto/gen/go/a/a.go
    assert_edit_effect keeps "go: a change in an unimported package of an imported module keeps the id" \
        "$root" zettel agent-shim/logging/go/unused/unused.go
    assert_edit_effect keeps "go: a change in a service the target does not build keeps the id" \
        "$root" zettel agent-shim/shim-lock/main.go
}

test_go_change_in_a_test_file_keeps_the_id() {
    local root; root="$(new_go_fixture)"
    assert_edit_effect keeps "go: a change in a test file keeps the id" "$root" shim-store agent-shim/shim-store/main_test.go
}

test_go_change_in_an_imported_package_changes_the_id() {
    local root; root="$(new_go_fixture)"
    assert_edit_effect changes "go: a change in the target's own source changes the id" \
        "$root" shim-store agent-shim/shim-store/main.go
    assert_edit_effect changes "go: a change in a generated package the target imports changes the id" \
        "$root" shim-store proto/gen/go/a/a.go
    assert_edit_effect changes "go: a change in the shared logging package changes the id" \
        "$root" shim-lock agent-shim/logging/go/log.go
}

test_go_change_in_an_embedded_file_or_the_module_files_changes_the_id() {
    local root; root="$(new_go_fixture)"
    assert_edit_effect changes "go: a change in an embedded file changes the id" \
        "$root" shim-store agent-shim/shim-store/data.txt
    assert_edit_effect changes "go: a change in the main module's go.mod changes the id" \
        "$root" shim-store agent-shim/shim-store/go.mod
    assert_edit_effect changes "go: a change in the main module's go.sum changes the id" \
        "$root" shim-store agent-shim/shim-store/go.sum
    assert_edit_effect changes "go: a change in an imported module's go.mod changes the id" \
        "$root" shim-store proto/gen/go/go.mod
}

test_go_target_whose_files_cannot_be_listed_has_no_source_set() {
    local root; root="$(new_go_fixture)"
    printf 'package main\n\nimport "agentrepl/does/not/exist"\n\nfunc main() { exist.X() }\n' > "$root/zettel/main.go"
    if deploy_stamp_system_files zettel "$root" "" >/dev/null 2>&1; then
        fail "go: a package that does not build has no source set (the caller treats it as stale)"
    else
        pass "go: a package that does not build has no source set (the caller treats it as stale)"
    fi
}

# --- the shim and the webapp ------------------------------------------------

# js_fixture ROOT — the real layout, small: the shim's package, the webapp's,
# and the generated TypeScript two imports of which are the only ones read.
js_fixture() {
    local root="$1" pkg
    mkdir -p "$root"
    write "$root" proto/gen/ts/used_pb.ts 'export const used = 1;'
    write "$root" proto/gen/ts/unused_pb.ts 'export const unused = 1;'
    for pkg in agent-shim/claude/shim webapp; do
        write "$root" "$pkg/package.json" '{"name":"fixture","type":"module"}'
        write "$root" "$pkg/package-lock.json" '{"lockfileVersion":3}'
        write "$root" "$pkg/tsconfig.json" '{"compilerOptions":{}}'
        write "$root" "$pkg/src/dep.ts" 'export const dep = 1;'
        write "$root" "$pkg/src/orphan.ts" 'export const orphan = 1;'
        write "$root" "$pkg/test/dep.test.ts" 'import { dep } from "../src/dep"; export const t = dep;'
        mkdir -p "$root/$pkg"
        ln -s "$NODE_MODULES" "$root/$pkg/node_modules"
    done
    write "$root" agent-shim/claude/shim/build.mjs 'export const built = true;'
    write "$root" agent-shim/claude/shim/src/main.ts 'import { dep } from "./dep"; import { used } from "../../../../proto/gen/ts/used_pb"; export const m = dep + used;'
    write "$root" webapp/index.html '<!doctype html><script type="module" src="/src/main.ts"></script>'
    write "$root" webapp/vite.config.ts 'export default {};'
    write "$root" webapp/vitest.config.ts 'export default {};'
    write "$root" webapp/src/main.ts 'import { dep } from "./dep"; import { used } from "../../proto/gen/ts/used_pb"; export const m = dep + used;'
    printf '.fakegit\nnode_modules\n' > "$root/.gitignore"
    git -C "$root" init -q
    commit "$root" seed
}

new_js_fixture() {
    local root; root="$(mktemp -d "$TMP/js.XXXXXX")"
    js_fixture "$root"
    printf '%s' "$root"
}

test_shim_change_outside_the_bundle_graph_keeps_the_id() {
    local root; root="$(new_js_fixture)"
    assert_edit_effect keeps "shim: a change in a source file the bundle does not import keeps the id" \
        "$root" shim agent-shim/claude/shim/src/orphan.ts
    assert_edit_effect keeps "shim: a change in the shim's tests keeps the id" \
        "$root" shim agent-shim/claude/shim/test/dep.test.ts
    assert_edit_effect keeps "shim: a change in a generated binding the bundle does not import keeps the id" \
        "$root" shim proto/gen/ts/unused_pb.ts
    assert_edit_effect keeps "shim: a change in the webapp keeps the id" \
        "$root" shim webapp/src/main.ts
}

test_shim_change_inside_the_bundle_graph_changes_the_id() {
    local root; root="$(new_js_fixture)"
    assert_edit_effect changes "shim: a change in an imported source file changes the id" \
        "$root" shim agent-shim/claude/shim/src/dep.ts
    assert_edit_effect changes "shim: a change in a generated binding the bundle imports changes the id" \
        "$root" shim proto/gen/ts/used_pb.ts
    assert_edit_effect changes "shim: a change in the lockfile changes the id" \
        "$root" shim agent-shim/claude/shim/package-lock.json
    assert_edit_effect changes "shim: a change in the build script changes the id" \
        "$root" shim agent-shim/claude/shim/build.mjs
}

test_webapp_change_outside_its_graph_keeps_the_id() {
    local root; root="$(new_js_fixture)"
    assert_edit_effect keeps "webapp: a change in a generated binding it does not import keeps the id" \
        "$root" webapp proto/gen/ts/unused_pb.ts
    assert_edit_effect keeps "webapp: a change in the vitest config keeps the id" \
        "$root" webapp webapp/vitest.config.ts
    assert_edit_effect keeps "webapp: a change in the shim keeps the id" \
        "$root" webapp agent-shim/claude/shim/src/main.ts
}

test_webapp_change_inside_its_graph_changes_the_id() {
    local root; root="$(new_js_fixture)"
    assert_edit_effect changes "webapp: a change in an imported source file changes the id" \
        "$root" webapp webapp/src/dep.ts
    assert_edit_effect changes "webapp: a change in a source file nothing imports changes the id (the build typechecks it)" \
        "$root" webapp webapp/src/orphan.ts
    assert_edit_effect changes "webapp: a change in a test changes the id (the build typechecks it)" \
        "$root" webapp webapp/test/dep.test.ts
    assert_edit_effect changes "webapp: a change in a generated binding it imports changes the id" \
        "$root" webapp proto/gen/ts/used_pb.ts
    assert_edit_effect changes "webapp: a change in the lockfile changes the id" \
        "$root" webapp webapp/package-lock.json
    assert_edit_effect changes "webapp: a change in the Vite config changes the id" \
        "$root" webapp webapp/vite.config.ts
    assert_edit_effect changes "webapp: a change in index.html changes the id" \
        "$root" webapp webapp/index.html
}

test_js_import_that_does_not_resolve_has_no_source_set() {
    local root; root="$(new_js_fixture)"
    printf 'import { gone } from "./gone"; export const m = gone;\n' > "$root/agent-shim/claude/shim/src/main.ts"
    if deploy_stamp_system_files shim "$root" "" >/dev/null 2>&1; then
        fail "shim: an import that does not resolve has no source set (the caller treats it as stale)"
    else
        pass "shim: an import that does not resolve has no source set (the caller treats it as stale)"
    fi
}

# --- the launcher -----------------------------------------------------------

# launcher_fixture ROOT — a SwiftPM package whose ZettelProto target is the
# symlink into the generated bindings, as the real one is.
launcher_fixture() {
    local root="$1"
    mkdir -p "$root"
    write "$root" launcher/Package.swift '// package'
    write "$root" launcher/Package.resolved '{}'
    write "$root" launcher/Bundle/Info.plist '<plist/>'
    write "$root" launcher/Sources/Zettel/main.swift 'print("z")'
    write "$root" launcher/Tests/ZettelTests/ClicksTests.swift '// test'
    write "$root" launcher/README.md 'readme'
    write "$root" proto/gen/swift/launcher/v1/click.pb.swift '// binding'
    write "$root" proto/gen/go/proto.go 'package proto'
    write "$root" bin/build-launcher.sh '#!/bin/sh'
    ln -s ../../proto/gen/swift "$root/launcher/Sources/ZettelProto"
    mkdir -p "$root/stubs"
    cat > "$root/stubs/swift" <<'SWIFT'
#!/usr/bin/env bash
# `swift package describe --type json` of the fixture package.
printf '{"targets":[{"name":"Zettel","type":"executable","path":"Sources/Zettel","sources":["main.swift"]},{"name":"ZettelProto","type":"library","path":"Sources/ZettelProto","sources":["launcher/v1/click.pb.swift"]},{"name":"ZettelTests","type":"test","path":"Tests/ZettelTests","sources":["ClicksTests.swift"]}]}\n'
SWIFT
    chmod +x "$root/stubs/swift"
    printf '.fakegit\nstubs\n' > "$root/.gitignore"
    git -C "$root" init -q
    commit "$root" seed
}

launcher_id() {
    PATH="$1/stubs:$PATH" system_id "$1" launcher
}

assert_launcher_effect() {
    local want="$1" what="$2" root="$3" path="$4" before after
    before="$(launcher_id "$root")"
    edit "$root" "$path"
    after="$(launcher_id "$root")"
    if [ "$want" = keeps ] && [ "$before" = "$after" ] && [ "$before" != NO-ID ]; then
        pass "$what"
    elif [ "$want" = changes ] && [ "$before" != "$after" ] && [ "$after" != NO-ID ]; then
        pass "$what"
    else
        fail "$what" "edited $path: id before=$before after=$after, wanted it to be $want"
    fi
}

test_launcher_change_outside_the_swiftpm_sources_keeps_the_id() {
    local root; root="$(mktemp -d "$TMP/swift.XXXXXX")"
    launcher_fixture "$root"
    assert_launcher_effect keeps "launcher: a change in the package's tests keeps the id" \
        "$root" launcher/Tests/ZettelTests/ClicksTests.swift
    assert_launcher_effect keeps "launcher: a change in a file no target compiles keeps the id" \
        "$root" launcher/README.md
    assert_launcher_effect keeps "launcher: a change in the generated Go bindings keeps the id" \
        "$root" proto/gen/go/proto.go
}

test_launcher_change_inside_the_swiftpm_sources_changes_the_id() {
    local root; root="$(mktemp -d "$TMP/swift.XXXXXX")"
    launcher_fixture "$root"
    assert_launcher_effect changes "launcher: a change in a target's source changes the id" \
        "$root" launcher/Sources/Zettel/main.swift
    assert_launcher_effect changes "launcher: a change in the Swift bindings it links through the symlink changes the id" \
        "$root" proto/gen/swift/launcher/v1/click.pb.swift
    assert_launcher_effect changes "launcher: a change in Package.resolved changes the id" \
        "$root" launcher/Package.resolved
    assert_launcher_effect changes "launcher: a change in the bundle's Info.plist changes the id" \
        "$root" launcher/Bundle/Info.plist
    assert_launcher_effect changes "launcher: a change in its build script changes the id" \
        "$root" bin/build-launcher.sh
}

# --- proto and the table ----------------------------------------------------

test_proto_change_in_a_review_artifact_keeps_the_id_and_a_schema_changes_it() {
    local root; root="$(mktemp -d "$TMP/proto.XXXXXX")"
    write "$root" proto/src/a/v1/a.proto 'syntax = "proto3";'
    write "$root" proto/figma-idl-draft/draft.md 'draft'
    write "$root" proto/SKETCH-figma-idl.md 'sketch'
    printf '.fakegit\n' > "$root/.gitignore"
    git -C "$root" init -q
    commit "$root" seed
    assert_edit_effect keeps "proto: a change in the review artifacts keeps the id" "$root" proto proto/figma-idl-draft/draft.md
    assert_edit_effect changes "proto: a change in a schema changes the id" "$root" proto proto/src/a/v1/a.proto
}

test_unknown_system_has_no_source_set() {
    if deploy_stamp_load_pathspec no-such-system "$TMP" "" >/dev/null 2>&1; then
        fail "table: a system the table does not name has no source set"
    else
        pass "table: a system the table does not name has no source set"
    fi
}

test_every_go_system_is_listed_by_the_one_table() {
    local root name missing=""
    root="$(new_go_fixture)"
    for name in shim-store zettel shim-lock; do
        deploy_stamp_system_files "$name" "$root" "" >/dev/null 2>&1 || missing="$missing $name"
    done
    if [ -z "$missing" ]; then
        pass "table: the Go systems the fixture lays out each get a source set from the one table"
    else
        fail "table: the Go systems the fixture lays out each get a source set from the one table" "no set for:$missing"
    fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
    "$t"
done

harness_finish

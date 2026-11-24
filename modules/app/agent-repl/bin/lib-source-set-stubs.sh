#!/usr/bin/env bash

# shellcheck shell=bash
# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# lib-source-set-stubs.sh — the build tools' answers to "which files does this
# build read", stubbed for the harnesses whose fixtures are not real Go modules
# or JS packages (test-build-frontend.sh, test-readiness-report.sh). Sourced,
# never executed. TEST SUPPORT ONLY.
#
# lib-deploy-stamp.sh asks three tools: `go list -deps`, `node source-files.mjs`
# and `swift package describe`. Each stub here models the SAME question over a
# fixture's directories: the real tools' answers are pinned against real
# modules and packages by test-lib-deploy-stamp.sh, and these stubs let the
# other harnesses vary a fixture's source set with plain files.
#
#   go list     the files of the module in the working directory, plus those of
#               every module its go.mod `replace`s with a local path; test
#               files are not part of a build and are left out.
#   node        source-files.mjs esbuild answers with the entries, every file
#               under each entry directory, and the extras that exist; any
#               other script runs under the real node.
#   swift       `package describe` answers with one library target per
#               directory under Sources/, listing its .swift files.

# install_source_set_stubs BINDIR — write the `go list` helper, `node` and
# `swift` stubs into BINDIR. A harness's own `go` stub hands `list` to
# go-list-stub; stub_go_list_main writes a `go` that answers nothing else.
install_source_set_stubs() {
    local bindir="$1" real_node
    mkdir -p "$bindir"
    real_node="$(command -v node)"
    cat > "$bindir/go-list-stub" <<'GOLIST'
#!/usr/bin/env bash
# go-list-stub — `go list -deps ...` over a fixture: the module in $PWD plus
# every locally replaced module.
module_files() {
    find "$1" -type f \( -name '*.go' -o -name go.mod -o -name go.sum \) ! -name '*_test.go' \
        ! -path '*/bin/*' ! -path '*/dist/*' ! -path '*/node_modules/*' ! -path '*/.fakegit/*'
}
module_files "$PWD"
sed -n 's/^replace .* => \(\.\.*\/.*\)$/\1/p' "$PWD/go.mod" 2>/dev/null | while IFS= read -r rel; do
    target="$(cd "$PWD" && cd "$rel" 2>/dev/null && pwd)" || continue
    module_files "$target"
done
GOLIST
    cat > "$bindir/node" <<NODE
#!/usr/bin/env bash
case "\${1:-}" in
    */source-files.mjs)
        # The swift mode only reshapes the stubbed `swift` answer: real node.
        [ "\${2:-}" != swift ] || exec "$real_node" "\$@"
        shift 2
        repo="" package=""; entries=() dirs=() extras=()
        while [ \$# -gt 0 ]; do
            case "\$1" in
                --repo) repo="\$2" ;;
                --package) package="\$2" ;;
                --entry) entries+=("\$2") ;;
                --entry-dir) dirs+=("\$2") ;;
                --extra) extras+=("\$2") ;;
            esac
            shift 2
        done
        {
            for f in "\${entries[@]}" "\${extras[@]}"; do [ ! -f "\$package/\$f" ] || printf '%s\n' "\$package/\$f"; done
            for d in "\${dirs[@]}"; do find "\$package/\$d" -type f; done
        } | sed "s|^\$repo/||" | sort -u
        ;;
    *) exec "$real_node" "\$@" ;;
esac
NODE
    cat > "$bindir/swift" <<'SWIFT'
#!/usr/bin/env bash
# `swift package describe --type json`: one library target per Sources/ dir.
[ "${1:-}" = package ] && [ "${2:-}" = describe ] || exit 1
first=1
printf '{"targets":['
for d in Sources/*/; do
    [ -d "$d" ] || continue
    name="$(basename "$d")"
    [ "$first" -eq 1 ] || printf ','
    first=0
    printf '{"name":"%s","type":"library","path":"Sources/%s","sources":[' "$name" "$name"
    s=1
    while IFS= read -r f; do
        [ "$s" -eq 1 ] || printf ','
        s=0
        printf '"%s"' "${f#"Sources/$name/"}"
    done < <(find "Sources/$name/" -type f -name '*.swift' | sort)
    printf ']}'
done
printf ']}\n'
SWIFT
    chmod +x "$bindir/go-list-stub" "$bindir/node" "$bindir/swift"
}

# stub_go_list_main BINDIR — install the stubs plus a `go` that answers only
# `list`, for a fixture whose other go calls are never made.
stub_go_list_main() {
    local bindir="$1"
    install_source_set_stubs "$bindir"
    cat > "$bindir/go" <<'GOMAIN'
#!/usr/bin/env bash
if [ "${1:-}" = list ]; then exec "$(dirname "$0")/go-list-stub" "$@"; fi
echo "stub go: unexpected call: $*" >&2
exit 1
GOMAIN
    chmod +x "$bindir/go"
}

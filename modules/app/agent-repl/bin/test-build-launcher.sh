#!/usr/bin/env bash
# Hermetic tests for bin/build-launcher.sh: the bundle it assembles carries the
# Zettel executable its own SwiftPM run built, never one left by an earlier
# build. swift is a stub that, like the real one, builds only the LAST
# --product it is given (and every product when given none); iconutil, codesign,
# security and lsregister are stubs too.

# shellcheck disable=SC2329,SC2016
# SC2016: the stubs are written single-quoted so they expand when they run.
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="$(cd "$THIS_DIR/.." && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"

# case_root NAME — a module tree for one case: the script under test, its
# libraries, a launcher package with a bundle plist, and a signing manifest.
case_root() {
  local root="$TMP/$1"
  mkdir -p "$root/bin" "$root/launcher/Bundle" "$root/stubs" "$root/home"
  cp "$THIS_DIR/build-launcher.sh" "$THIS_DIR/lib-signing.sh" "$THIS_DIR/lib-service.sh" "$root/bin/"
  printf '<plist/>\n' >"$root/launcher/Bundle/Info.plist"
  printf '{"requirements": [{"id": "local-signing", "kind": "signing_identity", "name": "case signing"}]}\n' >"$root/requirements.json"
  cat >"$root/stubs/swift" <<'STUB'
#!/usr/bin/env bash
# build -c release [--product P]... | build -c release --show-bin-path
bin="$PWD/.build/release"
[ "$1" = build ] || exit 64
shift
products=() show=0
while [ $# -gt 0 ]; do
  case "$1" in
    --show-bin-path) show=1; shift ;;
    --product) products=("$2"); shift 2 ;;  # the real one keeps the last
    *) shift ;;
  esac
done
if [ "$show" -eq 1 ]; then echo "$bin"; exit 0; fi
[ ${#products[@]} -gt 0 ] || products=(Zettel ZettelIcon)
mkdir -p "$bin"
for p in "${products[@]}"; do
  case "$p" in
    Zettel) echo "zettel built by run $(cat "$bin/../runs" 2>/dev/null || echo 0)" >"$bin/Zettel" ;;
    ZettelIcon) printf '#!/bin/sh\ncp /dev/null "$2"\n' >"$bin/ZettelIcon" ;;
  esac
  chmod +x "$bin/$p"
done
STUB
  printf '#!/bin/sh\nmkdir -p "$(dirname "$5")"; : >"$5"\n' >"$root/stubs/iconutil"
  printf '#!/bin/sh\nexit 0\n' >"$root/stubs/codesign"
  printf '#!/bin/sh\nexit 0\n' >"$root/stubs/lsregister"
  printf '#!/bin/sh\necho "  1) AAAA \\"case signing\\""\n' >"$root/stubs/security"
  chmod +x "$root"/stubs/*
  echo "$root"
}

# run_build ROOT — build-launcher.sh --no-install into ROOT/out; its output
# and exit status are left in ROOT/log and ROOT/rc.
run_build() {
  local root="$1" rc=0
  PATH="$root/stubs:$PATH" HOME="$root/home" bash "$root/bin/build-launcher.sh" --out "$root/out/Zettel.app" --no-install >"$root/log" 2>&1 || rc=$?
  echo "$rc" >"$root/rc"
}

test_the_bundle_carries_the_zettel_this_run_built() {
  local root
  root="$(case_root fresh)"
  run_build "$root"
  if [ "$(cat "$root/rc")" = 0 ] && [ -s "$root/out/Zettel.app/Contents/MacOS/Zettel" ]; then
    pass "a build with no earlier Zettel bundles the Zettel its own run produced"
  else fail "fresh build" "$(cat "$root/log")"; fi
}

test_a_zettel_left_by_an_earlier_build_is_never_bundled() {
  local root
  root="$(case_root stale)"
  mkdir -p "$root/launcher/.build/release"
  echo "stale zettel from an earlier build" >"$root/launcher/.build/release/Zettel"
  chmod +x "$root/launcher/.build/release/Zettel"
  run_build "$root"
  if [ "$(cat "$root/rc")" = 0 ] && grep -q 'built by run' "$root/out/Zettel.app/Contents/MacOS/Zettel"; then
    pass "a Zettel left in .build/release by an earlier build is replaced by this run's"
  else fail "stale Zettel bundled" "$(cat "$root/log")" "$(cat "$root/out/Zettel.app/Contents/MacOS/Zettel" 2>&1)"; fi
}

test_a_run_that_does_not_produce_a_product_fails_loudly() {
  local root
  root="$(case_root missing)"
  # A swift that builds ZettelIcon only, as one --product-restricted run did.
  sed -i.bak 's/products=(Zettel ZettelIcon)/products=(ZettelIcon)/' "$root/stubs/swift"
  run_build "$root"
  if [ "$(cat "$root/rc")" != 0 ] && grep -q 'did not produce .*/Zettel$' "$root/log" && [ ! -e "$root/out/Zettel.app" ]; then
    pass "a run that leaves a product unbuilt fails naming it, with no bundle written"
  else fail "missing product" "rc=$(cat "$root/rc")" "$(cat "$root/log")"; fi
}

test_the_script_asks_swift_for_no_single_product() {
  if grep -vE '^[[:space:]]*#' "$MODULE_DIR/bin/build-launcher.sh" | grep -q -- '--product'; then
    fail "build-launcher.sh passes --product to swift build, which builds only the last one named"
  else pass "build-launcher.sh runs one plain release build, naming no --product"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

harness_finish

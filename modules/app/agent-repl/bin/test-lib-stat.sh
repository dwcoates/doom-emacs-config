#!/usr/bin/env bash
# Hermetic tests for bin/lib-stat.sh, the one reading of a file's stat fields.
# stat is a stub that records its argv and answers as the spelling it was
# asked in would.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="$(cd "$THIS_DIR/.." && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin"
# The stub answers 42 for any field of an existing file and fails otherwise.
cat >"$TMP/bin/stat" <<EOF
#!/usr/bin/env bash
printf 'stat %s\n' "\$*" >>"$TMP/transcript"
[ -e "\$3" ] || exit 1
echo 42
EOF
chmod +x "$TMP/bin/stat"
export PATH="$TMP/bin:$PATH"

# shellcheck source=lib-stat.sh
. "$THIS_DIR/lib-stat.sh"
# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"

FILE="$TMP/file"
: >"$FILE"

test_each_platform_asks_in_its_own_spelling() {
  local mac linux
  : >"$TMP/transcript"
  AGENT_REPL_STAT_PLATFORM=Darwin stat_mtime "$FILE" >/dev/null
  AGENT_REPL_STAT_PLATFORM=Darwin stat_size "$FILE" >/dev/null
  mac="$(cat "$TMP/transcript")"
  : >"$TMP/transcript"
  AGENT_REPL_STAT_PLATFORM=Linux stat_mtime "$FILE" >/dev/null
  AGENT_REPL_STAT_PLATFORM=Linux stat_size "$FILE" >/dev/null
  linux="$(cat "$TMP/transcript")"
  if [ "$mac" = "stat -f %m $FILE"$'\n'"stat -f %z $FILE" ] && [ "$linux" = "stat -c %Y $FILE"$'\n'"stat -c %s $FILE" ]; then
    pass "macOS asks BSD stat's -f spelling and Linux GNU stat's -c, one call each"
  else fail "spellings" "$mac" "$linux"; fi
}

test_a_failed_stat_is_answered_never_retried() {
  local rc=0
  : >"$TMP/transcript"
  AGENT_REPL_STAT_PLATFORM=Linux stat_size "$TMP/absent" >/dev/null || rc=$?
  if [ "$rc" -ne 0 ] && [ "$(wc -l <"$TMP/transcript")" -eq 1 ]; then
    pass "a failed stat answers its failure after one call, never the other spelling"
  else fail "failed stat" "rc=$rc" "$(cat "$TMP/transcript")"; fi
}

test_the_field_is_answered() {
  local got
  got="$(AGENT_REPL_STAT_PLATFORM=Linux stat_mtime "$FILE")"
  if [ "$got" = 42 ]; then pass "the field stat printed is the answer"; else fail "field" "$got"; fi
}

# The call sites share the reader: no script but the library spells stat's
# platform flags itself.
test_no_script_spells_stat_itself() {
  local f rel real=""
  while IFS= read -r f; do
    rel="${f#"$MODULE_DIR"/}"
    case "$rel" in bin/lib-stat.sh|*/test-*.sh) continue ;; esac
    if grep -qE '(^|[^[:alnum:]_-])stat -[fc] ' <<<"$(grep -vE '^[[:space:]]*#' "$f")"; then real="$real $rel"; fi
  done < <(find "$MODULE_DIR/bin" "$MODULE_DIR/scripts" -name '*.sh' -type f)
  if [ -z "$real" ]; then pass "no script spells stat -f or stat -c outside lib-stat.sh"; else fail "these scripts spell stat themselves; source lib-stat.sh instead:$real"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

harness_finish

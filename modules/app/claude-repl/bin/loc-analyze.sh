#!/usr/bin/env bash
# loc-analyze.sh — analyze elisp line counts in the claude-repl module.
#
# Usage:
#   modules/app/claude-repl/bin/loc-analyze.sh [--no-color]
#
# Outputs:
#   - per-file table (src / test split, total / code / comment / blank)
#   - totals row for src and test
#   - top-10 largest src + test files
#   - src:test ratio
#
# Heuristics (line-oriented, fast over perfect):
#   - blank line     : whitespace-only
#   - comment line   : starts with `;` after trimming leading whitespace
#   - code line      : everything else
#   Inline trailing comments still count as code.

set -euo pipefail

MODULE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

color=1
for arg in "$@"; do
  case "$arg" in
    --no-color) color=0 ;;
    -h|--help)
      sed -n '2,/^$/p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
  esac
done

if [[ "$color" -eq 1 && -t 1 ]]; then
  BOLD=$'\033[1m'; DIM=$'\033[2m'; CYAN=$'\033[36m'; GREEN=$'\033[32m'
  YELLOW=$'\033[33m'; MAGENTA=$'\033[35m'; RESET=$'\033[0m'
else
  BOLD=""; DIM=""; CYAN=""; GREEN=""; YELLOW=""; MAGENTA=""; RESET=""
fi

# Per-file classifier: prints "TOTAL CODE COMMENT BLANK" for one file.
classify_file() {
  awk '
    {
      total++
      # strip leading whitespace for the classification check
      stripped = $0
      sub(/^[ \t]+/, "", stripped)
      if (stripped == "") { blank++; next }
      if (substr(stripped, 1, 1) == ";") { comment++; next }
      code++
    }
    END {
      printf "%d %d %d %d\n", total+0, code+0, comment+0, blank+0
    }
  ' "$1"
}

# Collect files: anything ending in .el under the module dir.
mapfile -t all_files < <(find "$MODULE_DIR" -type f -name '*.el' | sort)

# Bucket into src / test.
src_files=(); test_files=()
for f in "${all_files[@]}"; do
  base="$(basename "$f")"
  case "$base" in
    test-*.el) test_files+=("$f") ;;
    *) src_files+=("$f") ;;
  esac
done

print_section() {
  local label="$1"; shift
  local files=("$@")
  local tot_total=0 tot_code=0 tot_comment=0 tot_blank=0
  printf '\n%s%s%s (%d files)\n' "$BOLD$CYAN" "$label" "$RESET" "${#files[@]}"
  printf '%s%-50s %7s %7s %7s %7s%s\n' "$DIM" "file" "total" "code" "comment" "blank" "$RESET"
  for f in "${files[@]}"; do
    read -r total code comment blank < <(classify_file "$f")
    rel="${f#$MODULE_DIR/}"
    printf '%-50s %7d %7d %7d %7d\n' "$rel" "$total" "$code" "$comment" "$blank"
    tot_total=$((tot_total + total))
    tot_code=$((tot_code + code))
    tot_comment=$((tot_comment + comment))
    tot_blank=$((tot_blank + blank))
  done
  printf '%s%-50s %7d %7d %7d %7d%s\n' "$BOLD" "TOTAL" "$tot_total" "$tot_code" "$tot_comment" "$tot_blank" "$RESET"
  # Side channel for cross-section totals.
  case "$label" in
    src*)  src_total=$tot_total; src_code=$tot_code ;;
    test*) test_total=$tot_total; test_code=$tot_code ;;
  esac
}

print_top() {
  local label="$1"; shift
  local files=("$@")
  printf '\n%s%s%s\n' "$BOLD$YELLOW" "Top 10 by line count — $label" "$RESET"
  for f in "${files[@]}"; do
    read -r total _ _ _ < <(classify_file "$f")
    rel="${f#$MODULE_DIR/}"
    printf '%d %s\n' "$total" "$rel"
  done | sort -rn | head -10 | awk '{ printf "  %6d  %s\n", $1, $2 }'
}

print_section "src" "${src_files[@]}"
print_section "tests" "${test_files[@]}"

print_top "src" "${src_files[@]}"
print_top "tests" "${test_files[@]}"

# Summary ratio.
if [[ -n "${src_total:-}" && -n "${test_total:-}" && "$src_total" -gt 0 ]]; then
  ratio=$(awk -v t="$test_total" -v s="$src_total" 'BEGIN { printf "%.2f", t/s }')
  code_ratio=$(awk -v t="$test_code" -v s="$src_code" 'BEGIN { printf "%.2f", t/s }')
  printf '\n%s%sRatios%s\n' "$BOLD" "$MAGENTA" "$RESET"
  printf '  test-total / src-total = %s%s%s\n' "$GREEN" "$ratio" "$RESET"
  printf '  test-code  / src-code  = %s%s%s\n' "$GREEN" "$code_ratio" "$RESET"
fi

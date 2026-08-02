#!/usr/bin/env bash
#
# run.sh — driver for the structural-invariants skill.
#
# VERBS
#   --scan --scope <branch|commit|uncommitted|all> [--path <path>]...
#       Resolve the audit scope to a concrete file list, harvest evidence of
#       probabilistic (non-structural) invariant enforcement from those files,
#       write both to an evidence file, and print that file's path to stdout.
#       EXIT 0  Evidence file written; path printed.
#       EXIT 1  The scope resolved to no auditable files.
#       EXIT 2  Usage or script error.
#
#   --format-report --verdict <pass|fail> --findings <file> [--iteration]
#       Wrap the caller-supplied findings in a report, append the iteration
#       signal when --iteration is passed, and print the report path to stdout.
#       EXIT 0  Report written; path printed.
#       EXIT 2  Usage or script error (including a `fail` verdict whose findings
#               file carries no `## Remediation Plan` section).
#
#   --self-test
#       Exercise every verb against a throwaway git repository.
#       EXIT 0  All checks passed.
#       EXIT 1  At least one check failed.
#       EXIT 2  Usage or script error.
#
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
STATE_PREFIX="${STRUCTURAL_INVARIANTS_STATE_PREFIX:-/tmp/structural-invariants}"

die() {
  echo "[structural-invariants] ERROR: $*" >&2
  exit 2
}

log() {
  echo "[structural-invariants] $*" >&2
}

usage() {
  sed -n '3,30p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//' >&2
  exit 1
}

# ---------------------------------------------------------------------------
# Evidence patterns: each row is "<group label>|<extended regex>".
#
# These are lexical smells only. They never decide a verdict; they narrow the
# reader's attention to the lines most likely to encode a probabilistic
# guarantee where a structural one belongs.
# ---------------------------------------------------------------------------
evidence_groups() {
  cat <<'PATTERNS'
timing-based sequencing|(time\.Sleep|Thread\.sleep|[^a-zA-Z_-](sleep|usleep|nanosleep)\(|sleep-for|setTimeout\(|asyncio\.sleep|sit-for)
retry and backoff|(retry|Retry|RETRY|backoff|Backoff|max_?[Aa]ttempts|num_?tries|give ?up after)
polling and readiness waits|(poll[A-Za-z_]*\(|Poll[A-Za-z_]*\(|wait_?[Ff]or|waitUntil|until.*ready|ping until|spin ?wait)
fallback and degraded modes|(fallback|Fallback|FALLBACK|best[- ]effort|degraded|limp|graceful(ly)? (degrade|handle)|or_else|orElse|getOrDefault)
swallowed or defaulted errors|(except[^:]*:[[:space:]]*pass|catch[^{]*\{[[:space:]]*\}|_ = err|ignore(d)? (the )?error|rescue nil|\?\?[[:space:]]*(nil|null|0|""))
duplicated contracts|(keep .*in sync|kept in sync|must match|mirrors? the|duplicate of|copy of the|same as in)
fail-fast assertions|(assert|panic\(|throw new|raise [A-Z]|cl-assert|user-error|error\(|fatal|MUST NOT)
PATTERNS
}

# ---------------------------------------------------------------------------
# Scope resolution
# ---------------------------------------------------------------------------
# Picks the trunk candidate whose merge-base with HEAD is NEAREST, measured in
# commits between that merge-base and HEAD. A stale remote tracking ref then
# cannot widen the scope to the whole tree, which a fixed preference order
# (origin/HEAD first) would otherwise do whenever the remote lags local trunk.
default_base_ref() {
  local -a candidates=()
  local ref
  ref="$(git symbolic-ref --quiet refs/remotes/origin/HEAD 2>/dev/null)"
  [[ -n "$ref" ]] && candidates+=("${ref#refs/remotes/}")
  candidates+=(origin/master origin/main master main)

  local best="" best_distance="" candidate merge_base distance
  for candidate in "${candidates[@]}"; do
    git rev-parse --verify --quiet "$candidate" >/dev/null 2>&1 || continue
    merge_base="$(git merge-base HEAD "$candidate" 2>/dev/null)" || continue
    distance="$(git rev-list --count "$merge_base..HEAD" 2>/dev/null)" || continue
    if [[ -z "$best_distance" || "$distance" -lt "$best_distance" ]]; then
      best="$candidate"
      best_distance="$distance"
    fi
  done

  [[ -n "$best" ]] || return 1
  echo "$best"
}

resolve_scope_files() {
  local scope="$1"
  shift
  local -a paths=("$@")

  case "$scope" in
    branch)
      local base merge_base
      base="$(default_base_ref)" || die "cannot resolve a base ref for --scope branch"
      merge_base="$(git merge-base HEAD "$base" 2>/dev/null)" \
        || die "cannot resolve merge-base of HEAD and $base"
      {
        git diff --name-only "$merge_base" HEAD
        git diff --name-only HEAD
        git ls-files --others --exclude-standard
      } 2>/dev/null
      ;;
    commit)
      git show --name-only --pretty=format: HEAD 2>/dev/null
      ;;
    uncommitted)
      {
        git diff --name-only HEAD
        git ls-files --others --exclude-standard
      } 2>/dev/null
      ;;
    all)
      git ls-files 2>/dev/null
      ;;
    *)
      die "unknown scope '$scope' (expected branch, commit, uncommitted, or all)"
      ;;
  esac | {
    if [[ ${#paths[@]} -gt 0 ]]; then
      # grep's exit 1 means "no path matched", which is an empty scope rather
      # than a failure; only its exit 2 (a real grep error) propagates.
      grep -F -f <(printf '%s\n' "${paths[@]}") || [[ $? -eq 1 ]]
    else
      cat
    fi
  } | sed '/^$/d' | sort -u
}

auditable_file() {
  local f="$1"
  [[ -f "$f" ]] || return 1
  case "$f" in
    */node_modules/*|node_modules/*) return 1 ;;
    */vendor/*|vendor/*) return 1 ;;
    *.min.js|*.lock|*.sum|*.png|*.jpg|*.jpeg|*.gif|*.pdf|*.zip|*.gz) return 1 ;;
  esac
  grep -Iq . "$f" 2>/dev/null || return 1
  return 0
}

cmd_scan() {
  local scope="" ; local -a paths=()
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --scope) scope="${2:-}"; [[ -n "$scope" ]] || die "--scope requires a value"; shift 2 ;;
      --path)  [[ -n "${2:-}" ]] || die "--path requires a value"; paths+=("$2"); shift 2 ;;
      *) die "unknown argument to --scan: $1" ;;
    esac
  done
  [[ -n "$scope" ]] || die "--scan requires --scope"
  case "$scope" in
    branch|commit|uncommitted|all) ;;
    *) die "unknown scope '$scope' (expected branch, commit, uncommitted, or all)" ;;
  esac

  local toplevel
  toplevel="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git repository"

  # State written inside the audited tree would be picked up as an untracked
  # file and then scanned as evidence, so the scan would read its own output.
  # Refusing outright makes that feedback loop unrepresentable.
  local state_dir
  state_dir="$(cd "$(dirname "$STATE_PREFIX")" 2>/dev/null && pwd -P)" \
    || die "state directory does not exist: $(dirname "$STATE_PREFIX")"
  case "$state_dir/" in
    "$(cd "$toplevel" && pwd -P)"/*)
      die "state prefix '$STATE_PREFIX' resolves inside the audited repository" ;;
  esac

  # Resolution runs into a file rather than a process substitution so a failure
  # inside it surfaces as this call's exit status instead of an empty scope.
  local resolved="${STATE_PREFIX}-resolved-${scope}.txt"
  resolve_scope_files "$scope" "${paths[@]+"${paths[@]}"}" >"$resolved" \
    || die "failed resolving scope '$scope'"

  local -a files=()
  local f
  while IFS= read -r f; do
    auditable_file "$f" && files+=("$f")
  done <"$resolved"

  if [[ ${#files[@]} -eq 0 ]]; then
    log "scope '$scope' resolved to no auditable files"
    exit 1
  fi

  local out="${STATE_PREFIX}-scan-${scope}.txt"
  {
    echo "# structural-invariants scan"
    echo "scope: $scope"
    echo "files: ${#files[@]}"
    echo
    echo "## Files in scope"
    printf '%s\n' "${files[@]}"
    echo
    echo "## Evidence"
    echo "Lexical smells only. Each hit is a place to check against the rubric, never a verdict."
    local group pattern hits
    while IFS='|' read -r group pattern; do
      [[ -n "$group" ]] || continue
      hits="$(grep -nHE "$pattern" "${files[@]}" 2>/dev/null)"
      echo
      echo "### $group"
      if [[ -z "$hits" ]]; then
        echo "(no hits)"
      else
        printf '%s\n' "$hits"
      fi
    done < <(evidence_groups)
  } >"$out" || die "failed writing evidence file $out"

  echo "$out"
  exit 0
}

cmd_format_report() {
  local verdict="" findings="" iteration=0
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --verdict)   verdict="${2:-}"; [[ -n "$verdict" ]] || die "--verdict requires a value"; shift 2 ;;
      --findings)  findings="${2:-}"; [[ -n "$findings" ]] || die "--findings requires a value"; shift 2 ;;
      --iteration) iteration=1; shift ;;
      *) die "unknown argument to --format-report: $1" ;;
    esac
  done
  [[ -n "$verdict" ]]  || die "--format-report requires --verdict"
  [[ -n "$findings" ]] || die "--format-report requires --findings"
  [[ -f "$findings" ]] || die "findings file not found: $findings"

  case "$verdict" in
    pass|fail) ;;
    *) die "unknown verdict '$verdict' (expected pass or fail)" ;;
  esac

  if [[ "$verdict" == "fail" ]] && ! grep -q '^## Remediation Plan' "$findings"; then
    die "a 'fail' verdict requires a '## Remediation Plan' section in the findings file"
  fi
  if [[ "$verdict" == "pass" ]] && grep -q '^## Remediation Plan' "$findings"; then
    die "a 'pass' verdict must not carry a '## Remediation Plan' section"
  fi

  local out="${STATE_PREFIX}-report.md"
  {
    if [[ "$verdict" == "pass" ]]; then
      echo "# Structural Invariants — STRUCTURALLY GUARANTEED"
    else
      echo "# Structural Invariants — NOT STRUCTURALLY GUARANTEED"
    fi
    echo
    cat "$findings"
    if [[ "$iteration" -eq 1 ]]; then
      echo
      if [[ "$verdict" == "pass" ]]; then
        echo "ITERATE_SIGNAL: TERMINATE ITERATION SUCCESS"
      else
        echo "ITERATE_SIGNAL: CONTINUE ITERATION"
      fi
    fi
  } >"$out" || die "failed writing report file $out"

  echo "$out"
  exit 0
}

# ---------------------------------------------------------------------------
# Self test
# ---------------------------------------------------------------------------
FAILURES=0
check() {
  local label="$1" expected="$2" actual="$3"
  if [[ "$expected" == "$actual" ]]; then
    echo "ok   - $label"
  else
    echo "FAIL - $label (expected '$expected', got '$actual')"
    FAILURES=$((FAILURES + 1))
  fi
}

check_contains() {
  local label="$1" needle="$2" haystack_file="$3"
  if grep -q -- "$needle" "$haystack_file" 2>/dev/null; then
    echo "ok   - $label"
  else
    echo "FAIL - $label (missing '$needle' in $haystack_file)"
    FAILURES=$((FAILURES + 1))
  fi
}

check_absent() {
  local label="$1" needle="$2" haystack_file="$3"
  if grep -q -- "$needle" "$haystack_file" 2>/dev/null; then
    echo "FAIL - $label (unexpected '$needle' in $haystack_file)"
    FAILURES=$((FAILURES + 1))
  else
    echo "ok   - $label"
  fi
}

cmd_self_test() {
  local sandbox
  sandbox="$(mktemp -d "${TMPDIR:-/tmp}/structural-invariants-selftest.XXXXXX")" \
    || die "cannot create sandbox"
  # shellcheck disable=SC2064
  trap "rm -rf '$sandbox'" EXIT

  # State lives outside the sandbox repository, mirroring the invariant --scan
  # enforces: scan output never re-enters the audited tree.
  mkdir -p "$sandbox/state" "$sandbox/repo" || die "cannot create sandbox layout"
  export STRUCTURAL_INVARIANTS_STATE_PREFIX="$sandbox/state/si"
  sandbox="$sandbox/repo"

  (
    cd "$sandbox" || exit 2
    git init --quiet -b master .
    git config user.email selftest@example.com
    git config user.name selftest
    printf 'package main\n\nfunc boot() {\n\ttime.Sleep(2 * time.Second) // wait for the daemon\n}\n' >boot.go
    git add boot.go
    git commit --quiet -m "base"
    printf 'package main\n\nfunc join() {\n\t<-ready // structural rendezvous\n\tpanic("unreachable")\n}\n' >join.go
    git add join.go
    git commit --quiet -m "second"
  ) || die "sandbox setup failed"

  local out rc

  # --scan over the whole tree finds files and classifies evidence.
  out="$(cd "$sandbox" && bash "$SCRIPT_DIR/run.sh" --scan --scope all 2>/dev/null)"
  rc=$?
  check "--scan --scope all exits 0" "0" "$rc"
  check_contains "--scan lists boot.go in scope" "boot.go" "$out"
  check_contains "--scan flags time.Sleep as timing-based sequencing" "time.Sleep" "$out"
  check_contains "--scan flags panic as a fail-fast assertion" "panic(" "$out"

  # --scan over the latest commit narrows the file list.
  out="$(cd "$sandbox" && bash "$SCRIPT_DIR/run.sh" --scan --scope commit 2>/dev/null)"
  rc=$?
  check "--scan --scope commit exits 0" "0" "$rc"
  check_contains "--scan --scope commit includes join.go" "join.go" "$out"
  check_absent "--scan --scope commit excludes boot.go" "boot.go" "$out"

  # --scope branch measures against local trunk when the remote tracking ref is
  # stale, rather than dragging the whole history into scope.
  (
    cd "$sandbox" || exit 2
    git update-ref refs/remotes/origin/master "$(git rev-list --max-parents=0 HEAD)"
    git symbolic-ref refs/remotes/origin/HEAD refs/remotes/origin/master
    git checkout --quiet -b feature
    printf 'package main\n\nfunc later() {}\n' >later.go
    git add later.go
    git commit --quiet -m "third"
  ) || die "stale-remote sandbox setup failed"
  out="$(cd "$sandbox" && bash "$SCRIPT_DIR/run.sh" --scan --scope branch 2>/dev/null)"
  rc=$?
  check "--scan --scope branch exits 0 with a stale remote ref" "0" "$rc"
  check_contains "--scope branch includes the branch's own change" "later.go" "$out"
  check_absent "--scope branch excludes commits already on local trunk" "boot.go" "$out"

  # --scan with a --path filter that matches nothing reports an empty scope.
  (cd "$sandbox" && bash "$SCRIPT_DIR/run.sh" --scan --scope all --path no/such/dir >/dev/null 2>&1)
  check "--scan with an empty scope exits 1" "1" "$?"

  # --scan rejects an unknown scope.
  (cd "$sandbox" && bash "$SCRIPT_DIR/run.sh" --scan --scope bogus >/dev/null 2>&1)
  check "--scan with an unknown scope exits 2" "2" "$?"

  # --scan outside a git repository is a script error, never a silent empty scan.
  (cd "$sandbox/.." && bash "$SCRIPT_DIR/run.sh" --scan --scope all >/dev/null 2>&1)
  local outside_rc=$?
  if [[ "$outside_rc" == "0" ]]; then
    echo "FAIL - --scan outside a repository must not exit 0"
    FAILURES=$((FAILURES + 1))
  else
    echo "ok   - --scan outside a repository fails loudly (exit $outside_rc)"
  fi

  # A state prefix inside the audited tree is refused rather than silently
  # scanning the scan's own output.
  (cd "$sandbox" && STRUCTURAL_INVARIANTS_STATE_PREFIX="$sandbox/si" \
     bash "$SCRIPT_DIR/run.sh" --scan --scope all >/dev/null 2>&1)
  check "--scan refuses a state prefix inside the repository" "2" "$?"

  # --format-report pass path.
  printf '## Verdict\nAll rubric rows are structural.\n' >"$sandbox/pass-findings.md"
  out="$(bash "$SCRIPT_DIR/run.sh" --format-report --verdict pass --findings "$sandbox/pass-findings.md" --iteration 2>/dev/null)"
  rc=$?
  check "--format-report pass exits 0" "0" "$rc"
  check_contains "pass report announces the structural verdict" "STRUCTURALLY GUARANTEED" "$out"
  check_contains "pass report terminates the iteration" "ITERATE_SIGNAL: TERMINATE ITERATION SUCCESS" "$out"

  # --format-report fail path.
  printf '## Verdict\nRow 1 is probabilistic.\n\n## Remediation Plan\nReplace the sleep with a latch.\n' >"$sandbox/fail-findings.md"
  out="$(bash "$SCRIPT_DIR/run.sh" --format-report --verdict fail --findings "$sandbox/fail-findings.md" --iteration 2>/dev/null)"
  rc=$?
  check "--format-report fail exits 0" "0" "$rc"
  check_contains "fail report announces the non-structural verdict" "NOT STRUCTURALLY GUARANTEED" "$out"
  check_contains "fail report continues the iteration" "ITERATE_SIGNAL: CONTINUE ITERATION" "$out"

  # --format-report omits the signal when --iteration is absent.
  out="$(bash "$SCRIPT_DIR/run.sh" --format-report --verdict fail --findings "$sandbox/fail-findings.md" 2>/dev/null)"
  check_absent "report omits the signal without --iteration" "ITERATE_SIGNAL" "$out"

  # A fail verdict without a remediation plan is a hard error, never a silent report.
  bash "$SCRIPT_DIR/run.sh" --format-report --verdict fail --findings "$sandbox/pass-findings.md" >/dev/null 2>&1
  check "fail verdict without a remediation plan exits 2" "2" "$?"

  # A pass verdict carrying a remediation plan is a hard error too.
  bash "$SCRIPT_DIR/run.sh" --format-report --verdict pass --findings "$sandbox/fail-findings.md" >/dev/null 2>&1
  check "pass verdict with a remediation plan exits 2" "2" "$?"

  # Unknown verdict and missing findings file are usage errors.
  bash "$SCRIPT_DIR/run.sh" --format-report --verdict maybe --findings "$sandbox/pass-findings.md" >/dev/null 2>&1
  check "unknown verdict exits 2" "2" "$?"
  bash "$SCRIPT_DIR/run.sh" --format-report --verdict pass --findings "$sandbox/absent.md" >/dev/null 2>&1
  check "missing findings file exits 2" "2" "$?"
  bash "$SCRIPT_DIR/run.sh" --format-report --verdict pass >/dev/null 2>&1
  check "--format-report without --findings exits 2" "2" "$?"
  bash "$SCRIPT_DIR/run.sh" --scan >/dev/null 2>&1
  check "--scan without --scope exits 2" "2" "$?"

  echo
  if [[ "$FAILURES" -eq 0 ]]; then
    echo "all checks passed"
    exit 0
  fi
  echo "$FAILURES check(s) failed"
  exit 1
}

case "${1:-}" in
  --scan)          shift; cmd_scan "$@" ;;
  --format-report) shift; cmd_format_report "$@" ;;
  --self-test)     shift; cmd_self_test "$@" ;;
  *) usage ;;
esac

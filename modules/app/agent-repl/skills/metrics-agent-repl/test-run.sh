#!/usr/bin/env bash
# Hermetic tests for skills/metrics-agent-repl/run.sh: the metrics store is a
# curl stub answering canned bodies; nothing reaches a real store.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F).

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
RUN="$THIS_DIR/run.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

PASS=0
FAIL=0
pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; shift; [ $# -eq 0 ] || printf '%s\n' "$@" | sed 's/^/        /' >&2; FAIL=$((FAIL + 1)); }

# new_case: a stub dir whose curl answers from $CASE/answers/<key>, the key
# being the request's path with / as _ (a POST query also by its query).
CASE=""
new_case() {
  CASE="$(mktemp -d "$TMP/case.XXXXXX")"
  mkdir -p "$CASE/bin" "$CASE/answers" "$CASE/root/targets"
  cat >"$CASE/bin/curl" <<'STUB'
#!/usr/bin/env bash
url=""; query=""
while [ $# -gt 0 ]; do
  case "$1" in
    --data-urlencode) case "$2" in query=*) query="${2#query=}" ;; esac; shift 2 ;;
    --max-time) shift 2 ;;
    -*) shift ;;
    *) url="$1"; shift ;;
  esac
done
printf '%s\t%s\n' "$url" "$query" >>"$CASE_DIR/requests"
path="${url#*://*/}"
key="$(printf '%s' "$path" | tr '/' '_')"
case "$query" in
  *agentrepl_process_cpu*) key="${key}_cpu" ;;
  *agentrepl_process_footprint*) key="${key}_mem" ;;
  *agentrepl_restart_seconds_count*) key="${key}_counts" ;;
  *agentrepl_restart_seconds_sum*) key="${key}_sums" ;;
  *"histogram_quantile(0.5,"*agentrepl_restart_seconds*) key="${key}_p50" ;;
  *"histogram_quantile(0.9,"*agentrepl_restart_seconds*) key="${key}_p90" ;;
  *"histogram_quantile(0.99,"*agentrepl_restart_seconds*) key="${key}_p99" ;;
  *"histogram_quantile(0.95,"*agentrepl_restart_seconds*) key="${key}_p95" ;;
esac
if [ -f "$CASE_DIR/answers/$key" ]; then cat "$CASE_DIR/answers/$key"; exit 0; fi
exit 7
STUB
  chmod +x "$CASE/bin/curl"
}

answer() { printf '%s' "$2" >"$CASE/answers/$1"; }

RC=0
OUT=""
run() {
  set +e
  OUT="$(env CASE_DIR="$CASE" PATH="$CASE/bin:/usr/bin:/bin" HOME="$CASE" AGENT_REPL_METRICS_URL=http://store.test:8428 \
    AGENT_REPL_METRICS_ROOT="$CASE/root" bash "$RUN" "$@" 2>&1)"
  RC=$?
  set -e
}

has() { grep -qF -- "$1" <<<"$OUT"; }

TARGETS_UP='{"status":"success","data":{"activeTargets":[{"labels":{"instance":"daemon"},"health":"up","lastScrape":"t","lastError":""}]}}'
TARGETS_DOWN='{"status":"success","data":{"activeTargets":[{"labels":{"instance":"daemon"},"health":"up","lastScrape":"t","lastError":""},{"labels":{"instance":"store"},"health":"down","lastScrape":"t","lastError":"refused"}]}}'
TARGETS_NONE='{"status":"success","data":{"activeTargets":[]}}'

# Two versions, the second slower: hourly p95 samples by version.
BY_VERSION='{"status":"success","data":{"result":[{"metric":{"version":"0.128.0"},"values":[[1760000000,"0.04"],[1760003600,"0.05"]]},{"metric":{"version":"0.129.0"},"values":[[1760007200,"0.3"],[1760010800,"NaN"]]}]}}'

test_status_reports_every_runtime_up() {
  new_case; answer health "OK"; answer api_v1_targets "$TARGETS_UP"
  run --status
  if [ "$RC" -eq 0 ] && has "daemon	up"; then pass "status lists the scraped runtimes, all up"; else fail "status up" "rc=$RC" "$OUT"; fi
}

test_status_names_a_runtime_down() {
  new_case; answer health "OK"; answer api_v1_targets "$TARGETS_DOWN"
  run --status
  if [ "$RC" -eq 3 ] && has "down: store"; then pass "status names a runtime that is down"; else fail "status down" "rc=$RC" "$OUT"; fi
}

test_status_says_when_nothing_is_scraped() {
  new_case; answer health "OK"; answer api_v1_targets "$TARGETS_NONE"
  run --status
  if [ "$RC" -eq 3 ] && has "scrapes no runtime"; then pass "status says when the store scrapes nothing"; else fail "status none" "rc=$RC" "$OUT"; fi
}

test_status_says_when_the_store_does_not_answer() {
  new_case
  run --status
  if [ "$RC" -eq 1 ] && has "does not answer" && has "bin/requirements.sh install"; then pass "status says the store does not answer, and how to set it up"; else fail "status absent" "rc=$RC" "$OUT"; fi
}

test_latency_by_version_lists_versions_oldest_first() {
  new_case; answer api_v1_query_range "$BY_VERSION"
  run --latency-by-version switch_repeat 2
  if [ "$RC" -eq 0 ] && grep -q $'^0.128.0\t.*\t0.05\t2$' <<<"$OUT" && grep -q $'^0.129.0\t.*\t0.3\t1$' <<<"$OUT" \
     && [ "$(grep -n '0.128.0' <<<"$OUT" | cut -d: -f1)" -lt "$(grep -n '0.129.0' <<<"$OUT" | cut -d: -f1)" ]; then
    pass "latency-by-version lists each version's p95, oldest first, skipping empty hours"
  else fail "latency by version" "rc=$RC" "$OUT"; fi
}

test_first_version_over_uses_the_vocabulary_threshold() {
  new_case; answer api_v1_query_range "$BY_VERSION"
  run --first-version-over switch_repeat
  if [ "$RC" -eq 0 ] && grep -q $'^0.129.0\t' <<<"$OUT" && ! has "0.128.0"; then
    pass "first-version-over names the first version past the latency's own threshold"
  else fail "first version over" "rc=$RC" "$OUT"; fi
}

test_first_version_over_says_when_none_went_over() {
  new_case; answer api_v1_query_range "$BY_VERSION"
  run --first-version-over switch_repeat 5
  if [ "$RC" -eq 1 ] && has "went over 5s" && has "0.128.0"; then pass "first-version-over says none went over, and prints the versions seen"; else fail "none over" "rc=$RC" "$OUT"; fi
}

test_first_version_over_refuses_an_unknown_latency() {
  new_case; answer api_v1_query_range "$BY_VERSION"
  run --first-version-over warp_drive
  if [ "$RC" -eq 2 ] && has "names no latency warp_drive"; then pass "first-version-over refuses a latency the vocabulary does not name"; else fail "unknown latency" "rc=$RC" "$OUT"; fi
}

test_a_refused_query_is_a_usage_error() {
  new_case; answer api_v1_query '{"status":"error","error":"parse error at char 3"}'
  run --query 'rate(('
  if [ "$RC" -eq 2 ] && has "refused the query: parse error at char 3"; then pass "a query the store refuses surfaces its error"; else fail "refused query" "rc=$RC" "$OUT"; fi
}

test_a_query_with_no_series_says_so() {
  new_case; answer api_v1_query '{"status":"success","data":{"result":[]}}'
  run --query 'agentrepl_build_info'
  if [ "$RC" -eq 1 ]; then pass "a query matching no series exits 1"; else fail "empty query" "rc=$RC" "$OUT"; fi
}

test_a_query_prints_labels_and_value() {
  new_case; answer api_v1_query '{"status":"success","data":{"result":[{"metric":{"program":"daemon","build":"ab"},"value":[1760000000,"1"]}]}}'
  run --query 'agentrepl_build_info'
  if [ "$RC" -eq 0 ] && has $'program=daemon,build=ab\t1'; then pass "a query prints each series' labels and value"; else fail "query" "rc=$RC" "$OUT"; fi
}

test_process_trend_joins_cpu_and_memory_by_day() {
  new_case
  answer api_v1_query_range_cpu '{"status":"success","data":{"result":[{"metric":{},"values":[[1760054400,"0.25"]]}]}}'
  answer api_v1_query_range_mem '{"status":"success","data":{"result":[{"metric":{},"values":[[1760054400,"734003200"]]}]}}'
  run --process-trend daemon 1
  if [ "$RC" -eq 0 ] && has $'2025-10-10\t0.25\t700'; then pass "process-trend prints each day's CPU cores and footprint in MB"; else fail "trend" "rc=$RC" "$OUT"; fi
}

test_catalog_lists_each_runtime_s_metrics() {
  new_case
  printf '[{"targets":["127.0.0.1:1"],"labels":{"instance":"daemon"}}]' >"$CASE/root/targets/daemon.json"
  answer metrics $'# HELP agentrepl_build_info Always 1.\n# TYPE agentrepl_build_info gauge\n# HELP go_goroutines n\nagentrepl_build_info 1\n'
  run --catalog
  if [ "$RC" -eq 0 ] && has "## daemon" && has $'agentrepl_build_info\tAlways 1.' && ! has "go_goroutines"; then
    pass "catalog lists each runtime's agentrepl metrics with their help"
  else fail "catalog" "rc=$RC" "$OUT"; fi
}

test_catalog_says_when_nothing_advertises() {
  new_case
  run --catalog
  if [ "$RC" -eq 1 ] && has "no runtime advertises"; then pass "catalog says when no runtime advertises a surface"; else fail "catalog empty" "rc=$RC" "$OUT"; fi
}

test_restarts_summarizes_each_restart_phase() {
  new_case
  answer api_v1_query_counts '{"status":"success","data":{"result":[{"metric":{"restart":"deploy","phase":"total","outcome":"ok"},"value":[1,"4"]},{"metric":{"restart":"deploy","phase":"total","outcome":"failed"},"value":[1,"1"]},{"metric":{"restart":"workspace_reconnect","phase":"web","outcome":"ok"},"value":[1,"12.0001"]}]}}'
  answer api_v1_query_sums '{"status":"success","data":{"result":[{"metric":{"restart":"deploy","phase":"total"},"value":[1,"120"]}]}}'
  answer api_v1_query_p50 '{"status":"success","data":{"result":[{"metric":{"restart":"deploy","phase":"total"},"value":[1,"28.123"]},{"metric":{"restart":"workspace_reconnect","phase":"web"},"value":[1,"NaN"]}]}}'
  answer api_v1_query_p90 '{"status":"success","data":{"result":[{"metric":{"restart":"deploy","phase":"total"},"value":[1,"40"]}]}}'
  answer api_v1_query_p99 '{"status":"success","data":{"result":[{"metric":{"restart":"deploy","phase":"total"},"value":[1,"52.5"]}]}}'
  run --restarts 7
  if [ "$RC" -eq 0 ] && has $'deploy\ttotal\t4\t1\t30\t28.12\t40\t52.5' && has $'workspace_reconnect\tweb\t12\t0\t-\t-\t-\t-'; then
    pass "restarts prints each restart phase's ok and failed counts, mean, p50, p90 and p99"
  else fail "restarts" "rc=$RC" "$OUT"; fi
}

test_restarts_with_none_recorded_says_so() {
  new_case
  for k in counts sums p50 p90 p99; do answer "api_v1_query_$k" '{"status":"success","data":{"result":[]}}'; done
  run --restarts
  if [ "$RC" -eq 1 ]; then pass "restarts exits 1 when no restart was recorded"; else fail "restarts empty" "rc=$RC" "$OUT"; fi
}

test_restart_by_version_reads_the_phase_s_ok_series() {
  new_case; answer api_v1_query_range_p95 "$BY_VERSION"
  run --restart-by-version shim_restart run 2
  if [ "$RC" -eq 0 ] && grep -q $'^0.129.0\t.*\t0.3\t1$' <<<"$OUT" \
     && grep -qF 'agentrepl_restart_seconds_bucket{restart="shim_restart",phase="run",outcome="ok"}' "$CASE/requests"; then
    pass "restart-by-version lists each version's p95 of the phase's ok restarts"
  else fail "restart by version" "rc=$RC" "$OUT" "$(cat "$CASE/requests")"; fi
}

test_restart_by_version_needs_a_restart_and_a_phase() {
  new_case
  run --restart-by-version shim_restart
  if [ "$RC" -eq 2 ] && has "needs a restart and a phase"; then pass "restart-by-version refuses a missing phase"; else fail "restart by version usage" "rc=$RC" "$OUT"; fi
}

test_every_counter_query_counts_a_series_first_sample() {
  new_case
  answer api_v1_query_range "$BY_VERSION"
  answer api_v1_query_range_p95 "$BY_VERSION"
  answer api_v1_query_range_cpu '{"status":"success","data":{"result":[]}}'
  answer api_v1_query_range_mem '{"status":"success","data":{"result":[]}}'
  for k in counts sums p50 p90 p99; do answer "api_v1_query_$k" '{"status":"success","data":{"result":[]}}'; done
  run --latency-by-version switch_repeat 1
  run --restart-by-version deploy total 1
  run --restarts 1
  run --process-trend daemon 1
  if [ -s "$CASE/requests" ] && ! grep -qE '(^|[^_])(rate|increase)\(' "$CASE/requests" && grep -q 'increase_pure(' "$CASE/requests"; then
    pass "every counter query reads its counter with increase_pure, never rate or increase"
  else fail "counter functions" "$(cat "$CASE/requests")"; fi
}

test_a_version_is_joined_on_the_series_own_run() {
  new_case; answer api_v1_query_range "$BY_VERSION"
  run --latency-by-version switch_repeat 1
  if grep -qF 'on(run) group_left(version) agentrepl_deployed_version_info' "$CASE/requests"; then
    pass "a latency is joined to the version of its own process run"
  else fail "version join" "$(cat "$CASE/requests")"; fi
}

test_process_trend_sums_the_group_before_averaging_the_day() {
  new_case
  answer api_v1_query_range_cpu '{"status":"success","data":{"result":[]}}'
  answer api_v1_query_range_mem '{"status":"success","data":{"result":[]}}'
  run --process-trend daemon 1
  if grep -qF 'avg_over_time(sum(agentrepl_process_footprint_bytes{process="daemon"})[1d:5m])' "$CASE/requests" \
     && grep -qF 'avg_over_time((sum(increase_pure(agentrepl_process_cpu_seconds_total{process="daemon"}[5m])) / 300)[1d:5m])' "$CASE/requests"; then
    pass "process-trend sums the group at each instant before averaging the day"
  else fail "trend queries" "$(cat "$CASE/requests")"; fi
}

test_an_unknown_verb_prints_usage() {
  new_case
  run --teleport
  if [ "$RC" -eq 1 ] && has "usage:"; then pass "an unknown verb prints usage"; else fail "usage" "rc=$RC" "$OUT"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done
printf '%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]

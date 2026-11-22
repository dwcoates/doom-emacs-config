#!/usr/bin/env bash
# run.sh — the agent-repl-metrics skill's driver: reading agent-repl's
# always-on runtime metrics.
#
# Verbs:
#   --status                         Say whether the metrics store answers and
#                                    which runtimes it scrapes, up or down.
#   --catalog                        Print every agentrepl_* metric each
#                                    running runtime exposes, with its help.
#   --query <promql>                 Evaluate PromQL now; print one line per
#                                    series: labels, then value.
#   --range <promql> <hours> <step>  Evaluate PromQL over the last <hours>
#                                    every <step> (e.g. 1h); print one line per
#                                    sample: labels, time, value.
#   --latency-by-version <latency> [days]
#                                    The latency's p95 per deployed version
#                                    over the last [days] (default 30): one
#                                    line per version, oldest first, with the
#                                    first time it was deployed.
#   --first-version-over <latency> [seconds] [days]
#                                    The first deployed version whose p95 rose
#                                    over [seconds] (default: the latency's
#                                    threshold) within [days] (default 30).
#   --process-trend <process> [days] One line per day: the process group's
#                                    mean CPU (cores) and mean footprint.
#   --restarts [window]              One line per restart, phase and cause
#                                    over the last [window] (whole days, or a
#                                    duration such as 30m, 6h or 7d; default
#                                    7 days): how many ended
#                                    ok and failed, and the mean, p50, p90 and
#                                    p99 of those that ended ok.
#   --restart-by-version <restart> <phase> [days]
#                                    The restart phase's p95 per deployed
#                                    version over the last [days] (default
#                                    30), as --latency-by-version prints.
#   --spend [window] [by] [selector] Vendor spend over the last [window]
#                                    (as --restarts reads it; default 1d),
#                                    one line per value of [by] (default
#                                    workspace; also model, account, cause,
#                                    agent, subagent_type, cache_outcome,
#                                    cold_reason, kind, context), the most
#                                    weighted first: price-weighted spend
#                                    (spend-weights.json), its share, calls,
#                                    tokens, and for a workspace its peak
#                                    context bucket. [selector] narrows the
#                                    series (PromQL matchers, such as
#                                    cache_outcome="cold_rewrite").
#   --spend-over <hours> <step>      Price-weighted spend per <step> (1h, 1d)
#                                    over the last <hours>, oldest first.
#   --keepalive-runs [window]        Keep-alive runs that ended in [window]
#                                    (default 1d), by how each ended: runs,
#                                    mean keep-alives per run, and their
#                                    price-weighted spend.
#
# Exit codes:
#   0  printed what the verb answers
#   1  --status: the metrics store does not answer
#      --catalog: no runtime advertises a metrics surface
#      --query / --range / --latency-by-version / --process-trend /
#      --restarts / --restart-by-version / --spend / --spend-over /
#      --keepalive-runs: no series
#      --first-version-over: no version went over (the versions seen are
#      printed)
#   2  script/usage error, or the metrics store refused the query
#   3  --status: the store answers but scrapes no runtime, or a runtime it
#      scrapes is down (printed)

set -uo pipefail

# EVERY COUNTER IS READ WITH increase_pure, NEVER rate OR increase. Every
# agentrepl_* counter starts from zero when its process starts, often with
# its first counts already in it by the first scrape (a boot's restarts, a
# takeover's workspace starts); rate and increase skip a series' first
# sample, and read those counts as nothing.

SELF_DIR="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" && pwd)"
STORE="${AGENT_REPL_METRICS_URL:-http://127.0.0.1:8428}"
ROOT="${AGENT_REPL_METRICS_ROOT:-$HOME/.cache/agent-repl/metrics}"
VOCAB="$SELF_DIR/../../proto/vocab/latencies.json"
WEIGHTS="$SELF_DIR/spend-weights.json"

die() {
  printf 'agent-repl-metrics: %s\n' "$*" >&2
  exit 2
}

log() {
  printf 'agent-repl-metrics: %s\n' "$*"
}

command -v curl >/dev/null 2>&1 || die "curl is not on PATH"
command -v jq >/dev/null 2>&1 || die "jq is not on PATH"

# get PATH [curl args...] — GET a store endpoint; prints the body.
get() {
  local path="$1"
  shift
  curl -fsS --max-time 20 "$@" "$STORE$path"
}

# promql_data ENDPOINT PROMQL [extra --data-urlencode args...] — the result
# array of a successful query, dying on a refusal.
promql_data() {
  local endpoint="$1" promql="$2" body
  shift 2
  body="$(curl -sS --max-time 60 "$STORE/api/v1/$endpoint" --data-urlencode "query=$promql" "$@" 2>&1)" ||
    die "the metrics store at $STORE did not answer: $body"
  [ "$(printf '%s' "$body" | jq -r '.status // empty' 2>/dev/null)" = "success" ] ||
    die "the metrics store refused the query: $(printf '%s' "$body" | jq -r '.error // .' 2>/dev/null)"
  printf '%s' "$body" | jq -c '.data.result'
}

# range_args HOURS STEP — set RANGE_ARGS to the start/end/step of a range
# ending now.
RANGE_ARGS=()
range_args() {
  local now start
  now="$(date +%s)"
  start=$((now - $1 * 3600))
  RANGE_ARGS=(--data-urlencode "start=$start" --data-urlencode "end=$now" --data-urlencode "step=$2")
}

# p95_by_version HISTOGRAM SELECTOR DAYS — the hourly p95 of the histogram's
# series matching SELECTOR, each sample labeled with the version deployed then.
# A series is joined to the version of its own process run (the `run` target
# label): two daemons overlap during a handover, each with its own version.
p95_by_version() {
  local histogram="$1" selector="$2" days="$3" promql
  promql="histogram_quantile(0.95, sum by (le, version) (increase_pure(${histogram}_bucket{$selector}[1h]) * on(run) group_left(version) agentrepl_deployed_version_info))"
  range_args $((days * 24)) 1h
  promql_data query_range "$promql" "${RANGE_ARGS[@]}"
}

# print_per_version RESULT — print the per-version table of a p95_by_version
# result, exiting 1 when it holds no sample.
print_per_version() {
  local lines
  lines="$(printf '%s' "$1" | per_version)" || die "could not read the samples"
  [ -n "$lines" ] || exit 1
  printf 'version\tfirst_deployed\tp95_seconds\thours\n%s\n' "$lines"
}

# per_version RESULT — versions oldest first: version, first seen, p95 of the
# hourly p95s (seconds), hours sampled.
per_version() {
  jq -r '
    [ .[] | .metric.version as $v | .values[] | select(.[1] != "NaN") | {v: $v, t: .[0], p: (.[1] | tonumber)} ]
    | group_by(.v)
    | map({v: .[0].v, first: (map(.t) | min), ps: (map(.p) | sort)})
    | sort_by(.first)
    | .[]
    | (.ps | length) as $n
    | (($n * 0.95 | floor) as $i | if $i >= $n then $n - 1 else $i end) as $j
    | [.v, (.first | floor | todate), .ps[$j], $n]
    | @tsv'
}

# window_of ARG DEFAULT — set WINDOW to ARG (or DEFAULT) as a PromQL range:
# a bare number is whole days, as every other mode reads it; 30m, 6h and 7d
# read the last half hour, six hours and week.
WINDOW=""
window_of() {
  local window="${1:-$2}"
  if [[ "$window" =~ ^[0-9]+$ ]]; then
    window="${window}d"
  fi
  [[ "$window" =~ ^[1-9][0-9]*[mhd]$ ]] || die "the window must be whole days or a duration such as 30m, 6h or 7d, not ${1}"
  WINDOW="$window"
}

# WEIGH is the jq definition every spend verb weighs tokens with: weight(kind;
# model) from spend-weights.json, the one home of the weights.
WEIGH='def weight($k; $m): ($w.kinds[$k] // error("spend-weights.json weighs no kind \($k)")) * ($w.models[$m] // $w.default_model);'

case "${1:-}" in
  --status)
    [ -z "${2:-}" ] || die "--status takes no argument, not $2"
    if ! get /health >/dev/null 2>&1; then
      log "the metrics store at $STORE does not answer; bin/requirements.sh install sets it up"
      exit 1
    fi
    targets="$(get /api/v1/targets 2>&1)" || die "the metrics store did not list its targets: $targets"
    printf '%s' "$targets" | jq -r '.data.activeTargets[] | [.labels.instance, .health, .lastScrape, .lastError] | @tsv' ||
      die "could not read the targets: $targets"
    if [ "$(printf '%s' "$targets" | jq '.data.activeTargets | length')" -eq 0 ]; then
      log "the metrics store scrapes no runtime: none has advertised a target under $ROOT/targets"
      exit 3
    fi
    down="$(printf '%s' "$targets" | jq -r '[.data.activeTargets[] | select(.health != "up") | .labels.instance] | join(" ")')"
    if [ -n "$down" ]; then
      log "down: $down"
      exit 3
    fi
    ;;
  --catalog)
    [ -z "${2:-}" ] || die "--catalog takes no argument, not $2"
    shopt -s nullglob
    files=("$ROOT"/targets/*.json)
    [ "${#files[@]}" -gt 0 ] || {
      log "no runtime advertises a metrics surface under $ROOT/targets"
      exit 1
    }
    for f in "${files[@]}"; do
      program="$(jq -r '.[0].labels.instance' "$f")" || die "could not read $f"
      address="$(jq -r '.[0].targets[0]' "$f")" || die "could not read $f"
      printf '## %s\n' "$program"
      if ! body="$(curl -fsS --max-time 10 "http://$address/metrics" 2>&1)"; then
        log "$program is down (its target names $address)"
        continue
      fi
      printf '%s\n' "$body" | sed -n 's/^# HELP \(agentrepl_[^ ]*\) /\1\t/p' | sort -u
    done
    ;;
  --query)
    [ -n "${2:-}" ] || die "--query needs a PromQL expression"
    result="$(promql_data query "$2")" || exit 2
    [ "$(printf '%s' "$result" | jq 'length')" -gt 0 ] || exit 1
    printf '%s' "$result" | jq -r '.[] | [(.metric | to_entries | map("\(.key)=\(.value)") | join(",")), .value[1]] | @tsv'
    ;;
  --range)
    [ -n "${2:-}" ] && [ -n "${3:-}" ] && [ -n "${4:-}" ] || die "--range needs a PromQL expression, hours and a step"
    [[ "$3" =~ ^[0-9]+$ ]] || die "--range hours must be a whole number, not $3"
    range_args "$3" "$4"
    result="$(promql_data query_range "$2" "${RANGE_ARGS[@]}")" || exit 2
    [ "$(printf '%s' "$result" | jq 'length')" -gt 0 ] || exit 1
    printf '%s' "$result" | jq -r '.[] | (.metric | to_entries | map("\(.key)=\(.value)") | join(",")) as $l | .values[] | [$l, (.[0] | todate), .[1]] | @tsv'
    ;;
  --latency-by-version)
    [ -n "${2:-}" ] || die "--latency-by-version needs a latency name"
    days="${3:-30}"
    [[ "$days" =~ ^[0-9]+$ ]] || die "days must be a whole number, not $days"
    result="$(p95_by_version agentrepl_latency_seconds "latency=\"$2\"" "$days")" || exit 2
    print_per_version "$result"
    ;;
  --first-version-over)
    [ -n "${2:-}" ] || die "--first-version-over needs a latency name"
    latency="$2"
    threshold="${3:-}"
    days="${4:-30}"
    if [ -z "$threshold" ]; then
      [ -f "$VOCAB" ] || die "no latency vocabulary at $VOCAB to read $latency's threshold from"
      threshold="$(jq -r --arg n "$latency" '.latencies[] | select(.name == $n) | .threshold_ms / 1000' "$VOCAB")" ||
        die "could not read $VOCAB"
      [ -n "$threshold" ] || die "the latency vocabulary names no latency $latency"
    fi
    [[ "$days" =~ ^[0-9]+$ ]] || die "days must be a whole number, not $days"
    result="$(p95_by_version agentrepl_latency_seconds "latency=\"$latency\"" "$days")" || exit 2
    lines="$(printf '%s' "$result" | per_version)" || die "could not read the latency's samples"
    over="$(printf '%s\n' "$lines" | awk -F'\t' -v t="$threshold" '$3 != "" && $3 + 0 > t + 0 { print; exit }')"
    if [ -z "$over" ]; then
      log "no version's p95 of $latency went over ${threshold}s in the last $days days"
      [ -z "$lines" ] || printf 'version\tfirst_deployed\tp95_seconds\thours\n%s\n' "$lines"
      exit 1
    fi
    printf 'version\tfirst_deployed\tp95_seconds\thours\n%s\n' "$over"
    ;;
  --process-trend)
    [ -n "${2:-}" ] || die "--process-trend needs a process group (daemon, shim, emacs, webkit_webcontent, ...)"
    days="${3:-30}"
    [[ "$days" =~ ^[0-9]+$ ]] || die "days must be a whole number, not $days"
    range_args $((days * 24)) 1d
    # THE GROUP IS SUMMED AT EACH INSTANT, THEN AVERAGED OVER THE DAY: summing
    # per-series daily averages would add every daemon run's own average
    # (each run is its own series), and dividing a day's increase by a whole
    # day would understate a day the store saw only part of.
    cpu="$(promql_data query_range "avg_over_time((sum(increase_pure(agentrepl_process_cpu_seconds_total{process=\"$2\"}[5m])) / 300)[1d:5m])" "${RANGE_ARGS[@]}")" || exit 2
    mem="$(promql_data query_range "avg_over_time(sum(agentrepl_process_footprint_bytes{process=\"$2\"})[1d:5m])" "${RANGE_ARGS[@]}")" || exit 2
    lines="$(jq -rn --argjson cpu "$cpu" --argjson mem "$mem" '
      ([$cpu[0].values // [] | .[] | {key: (.[0] | tostring), value: .[1]}] | from_entries) as $c
      | ([$mem[0].values // [] | .[] | {key: (.[0] | tostring), value: .[1]}] | from_entries) as $m
      | ($c + $m | keys | map(tonumber) | sort)[]
      | tostring as $k
      | [(tonumber | todate | .[0:10]), ($c[$k] // "-"), (if $m[$k] then ($m[$k] | tonumber / 1048576 | floor | tostring) else "-" end)]
      | @tsv')" || die "could not read the trend"
    [ -n "$lines" ] || exit 1
    printf 'day\tcpu_cores\tfootprint_mb\n%s\n' "$lines"
    ;;
  --restarts)
    # THE WINDOW IS WHOLE DAYS OR A DURATION: a bare number is days, as every
    # other mode reads it; 30m, 6h and 7d read the last half hour, six hours
    # and week, so a check right after a landing reads only its own restarts.
    window_of "${2:-}" 7
    window="$WINDOW"
    counts="$(promql_data query "sum by (restart, phase, cause, outcome) (increase_pure(agentrepl_restart_seconds_count[$window]))")" || exit 2
    sums="$(promql_data query "sum by (restart, phase, cause) (increase_pure(agentrepl_restart_seconds_sum{outcome=\"ok\"}[$window]))")" || exit 2
    quantile() {
      promql_data query "histogram_quantile($1, sum by (restart, phase, cause, le) (increase_pure(agentrepl_restart_seconds_bucket{outcome=\"ok\"}[$window])))"
    }
    p50="$(quantile 0.5)" || exit 2
    p90="$(quantile 0.9)" || exit 2
    p99="$(quantile 0.99)" || exit 2
    lines="$(jq -rn --argjson c "$counts" --argjson s "$sums" --argjson p50 "$p50" --argjson p90 "$p90" --argjson p99 "$p99" '
      def key: "\(.metric.restart)|\(.metric.phase)|\(.metric.cause // "" | if . == "" then "-" else . end)";
      def round2: tonumber * 100 | round / 100 | tostring;
      def num: (.value[1] | if . == "NaN" then "-" else round2 end);
      def table($r): [$r[] | {key: key, value: num}] | from_entries;
      ([$c[] | {k: key, o: .metric.outcome, n: (.value[1] | tonumber | round)}]) as $counts
      | ([$s[] | {key: key, value: (.value[1] | tonumber)}] | from_entries) as $sum
      | table($p50) as $q50 | table($p90) as $q90 | table($p99) as $q99
      | ($counts | map(.k) | unique)[] as $k
      | ([$counts[] | select(.k == $k and .o == "ok") | .n] | add // 0) as $ok
      | ([$counts[] | select(.k == $k and .o == "failed") | .n] | add // 0) as $failed
      # A phase that recorded nothing in the window (its series is older than
      # the window) is not a restart of the window: it gets no row.
      | select($ok + $failed > 0)
      | [($k | split("|")[]),
         $ok,
         $failed,
         (if $ok > 0 and $sum[$k] != null then ($sum[$k] / $ok | round2) else "-" end),
         ($q50[$k] // "-"), ($q90[$k] // "-"), ($q99[$k] // "-")]
      | @tsv')" || die "could not read the restarts"
    [ -n "$lines" ] || exit 1
    printf 'restart\tphase\tcause\tok\tfailed\tmean_seconds\tp50_seconds\tp90_seconds\tp99_seconds\n%s\n' "$lines"
    ;;
  --restart-by-version)
    [ -n "${2:-}" ] && [ -n "${3:-}" ] || die "--restart-by-version needs a restart and a phase (see --restarts)"
    days="${4:-30}"
    [[ "$days" =~ ^[0-9]+$ ]] || die "days must be a whole number, not $days"
    result="$(p95_by_version agentrepl_restart_seconds "restart=\"$2\",phase=\"$3\",outcome=\"ok\"" "$days")" || exit 2
    print_per_version "$result"
    ;;
  --spend)
    window_of "${2:-}" 1
    by="${3:-workspace}"
    sel="${4:-}"
    case "$by" in
      workspace | model | account | cause | agent | subagent_type | cache_outcome | cold_reason | kind | context) ;;
      *) die "--spend cannot break spend down by $by" ;;
    esac
    case "$by" in
      context) tokens="$(promql_data query "sum by (context, kind, model) (increase_pure(agentrepl_vendor_context_tokens_total{$sel}[$WINDOW]))")" || exit 2 ;;
      kind) tokens="$(promql_data query "sum by (kind, model) (increase_pure(agentrepl_vendor_tokens_total{$sel}[$WINDOW]))")" || exit 2 ;;
      *) tokens="$(promql_data query "sum by ($by, kind, model) (increase_pure(agentrepl_vendor_tokens_total{$sel}[$WINDOW]))")" || exit 2 ;;
    esac
    calls='[]'
    [ "$by" = kind ] || calls="$(promql_data query "sum by ($by) (increase_pure(agentrepl_vendor_calls_total{$sel}[$WINDOW]))")" || exit 2
    peaks='[]'
    [ "$by" != workspace ] || peaks="$(promql_data query "sum by (workspace, context) (increase_pure(agentrepl_vendor_calls_total{$sel}[$WINDOW])) > 0")" || exit 2
    lines="$(jq -rn --argjson w "$(cat "$WEIGHTS")" --argjson t "$tokens" --argjson c "$calls" --argjson p "$peaks" --arg by "$by" "$WEIGH"'
      def key: .metric[$by] // "";
      def num: .value[1] | tonumber;
      ([$t[] | {k: key, n: num, x: (num * weight(.metric.kind; .metric.model))}] | group_by(.k)
        | map({k: .[0].k, tokens: (map(.n) | add), weighted: (map(.x) | add)})) as $rows
      | ($rows | map(.weighted) | add // 0) as $total
      | ([$c[] | {key: key, value: num}] | from_entries) as $calls
      | ([$p[] | {k: .metric.workspace, b: (.metric.context | if . == "+Inf" then infinite else tonumber end)}]
          | group_by(.k) | map({key: .[0].k, value: (map(.b) | max)}) | from_entries) as $peak
      | def short: if . >= 1e6 then "\(. / 1e6 * 10 | round / 10)M" elif . >= 1e3 then "\(. / 1e3 | round)k" else "\(round)" end;
      ($rows | sort_by(-.weighted))[]
      | [(if .k == "" then "-" else .k end), (.weighted | short),
         (if $total > 0 then "\(.weighted / $total * 1000 | round / 10)%" else "-" end),
         ($calls[.k] // null | if . == null then "-" else round end), (.tokens | short)]
        + (if $by == "workspace" then [($peak[.k] // null | if . == null then "-" elif . == infinite then ">1M" else (. | short) end)] else [] end)
      | @tsv')" || die "could not read the spend"
    [ -n "$lines" ] || exit 1
    header="$by	weighted	share	calls	tokens"
    [ "$by" != workspace ] || header="$header	peak_context"
    printf '%s\n%s\n' "$header" "$lines"
    ;;
  --spend-over)
    [ -n "${2:-}" ] && [ -n "${3:-}" ] || die "--spend-over needs hours and a step (such as 1h or 1d)"
    [[ "$2" =~ ^[0-9]+$ ]] || die "--spend-over hours must be a whole number, not $2"
    [[ "$3" =~ ^[1-9][0-9]*[mhd]$ ]] || die "--spend-over step must be a duration such as 1h or 1d, not $3"
    range_args "$2" "$3"
    result="$(promql_data query_range "sum by (kind, model) (increase_pure(agentrepl_vendor_tokens_total[$3]))" "${RANGE_ARGS[@]}")" || exit 2
    lines="$(jq -rn --argjson w "$(cat "$WEIGHTS")" --argjson r "$result" "$WEIGH"'
      [$r[] | .metric as $m | .values[] | {t: .[0], x: ((.[1] | tonumber) * weight($m.kind; $m.model))}]
      | group_by(.t) | map({t: .[0].t, x: (map(.x) | add)}) | sort_by(.t)[]
      | [(.t | todate), (.x | round)] | @tsv')" || die "could not read the spend"
    [ -n "$lines" ] || exit 1
    printf 'until	weighted
%s
' "$lines"
    ;;
  --keepalive-runs)
    window_of "${2:-}" 1
    counts="$(promql_data query "sum by (end) (increase_pure(agentrepl_keepalive_run_beats_count[$WINDOW]))")" || exit 2
    beats="$(promql_data query "sum by (end) (increase_pure(agentrepl_keepalive_run_beats_sum[$WINDOW]))")" || exit 2
    spent="$(promql_data query "sum by (end, kind) (increase_pure(agentrepl_keepalive_run_tokens_total[$WINDOW]))")" || exit 2
    lines="$(jq -rn --argjson w "$(cat "$WEIGHTS")" --argjson c "$counts" --argjson b "$beats" --argjson s "$spent" "$WEIGH"'
      ([$b[] | {key: .metric.end, value: (.value[1] | tonumber)}] | from_entries) as $beats
      | ([$s[] | {k: .metric.end, x: ((.value[1] | tonumber) * weight(.metric.kind; ""))}] | group_by(.k)
          | map({key: .[0].k, value: (map(.x) | add)}) | from_entries) as $spent
      | $c[] | (.value[1] | tonumber | round) as $n | select($n > 0)
      | [.metric.end, $n, (($beats[.metric.end] // 0) / $n * 10 | round / 10), (($spent[.metric.end] // 0) | round)]
      | @tsv')" || die "could not read the keep-alive runs"
    [ -n "$lines" ] || exit 1
    printf 'end	runs	mean_keepalives	weighted
%s
' "$lines"
    ;;
  *)
    printf 'usage: run.sh --status | --catalog | --query <promql> | --range <promql> <hours> <step> | --latency-by-version <latency> [days] | --first-version-over <latency> [seconds] [days] | --process-trend <process> [days] | --restarts [window] | --restart-by-version <restart> <phase> [days] | --spend [window] [by] [selector] | --spend-over <hours> <step> | --keepalive-runs [window]\n' >&2
    exit 1
    ;;
esac

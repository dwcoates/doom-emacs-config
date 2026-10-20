#!/usr/bin/env bash
# run.sh — the metrics-agent-repl skill's driver: reading agent-repl's
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
#
# Exit codes:
#   0  printed what the verb answers
#   1  --status: the metrics store does not answer
#      --catalog: no runtime advertises a metrics surface
#      --query / --range / --latency-by-version / --process-trend /
#      --restarts / --restart-by-version: no series
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

die() {
  printf 'metrics-agent-repl: %s\n' "$*" >&2
  exit 2
}

log() {
  printf 'metrics-agent-repl: %s\n' "$*"
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
    window="${2:-7}"
    if [[ "$window" =~ ^[0-9]+$ ]]; then
      window="${window}d"
    fi
    [[ "$window" =~ ^[1-9][0-9]*[mhd]$ ]] || die "the window must be whole days or a duration such as 30m, 6h or 7d, not ${2}"
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
      | [($k | split("|")[]),
         $ok,
         ([$counts[] | select(.k == $k and .o == "failed") | .n] | add // 0),
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
  *)
    printf 'usage: run.sh --status | --catalog | --query <promql> | --range <promql> <hours> <step> | --latency-by-version <latency> [days] | --first-version-over <latency> [seconds] [days] | --process-trend <process> [days] | --restarts [window] | --restart-by-version <restart> <phase> [days]\n' >&2
    exit 1
    ;;
esac

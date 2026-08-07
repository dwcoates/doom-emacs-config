#!/usr/bin/env bash
# agent-repl-log-discovery.sh -- resolve and query agent-repl JSONL logs.
#
# This is deliberately read-only.  Workspace runtime logs are canonical
# symlinks under <workspace>/.claude/emacs; global logs are only the few
# runtime streams which are conceptually workspace-free.

set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  agent-repl-log-discovery.sh --workspace DIR [SELECTORS] [VERB]
  agent-repl-log-discovery.sh --global        [SELECTORS] [VERB]

Selectors (compose with every verb):
  --runtime RUNTIME   narrow to one runtime's canonical log
  --session ID        keep records whose agent_repl_session_id or
                      claude_session_id equals ID
  --pid PID           keep records whose pid equals PID
  --tail LINES        read only the last LINES lines of each selected log
                      before any JSON handling, so it suits active, large logs

Verbs (choose at most one output verb):
  (none)              without --session/--pid, print canonical log location(s);
                      with either, print matching JSONL records
  --spans PREFIX      pair start/completion records into latency spans
  --latency-by FIELD  aggregate spans per key; FIELD is one of
                      request_id, operation, workspace_id
  --gaps MS           report inter-record gaps strictly greater than MS

--spans and --latency-by combine: --spans supplies the operation prefix that
--latency-by aggregates over.  Without --spans, --latency-by spans every
selected record.  --gaps combines with neither.

Workspace runtimes: emacs, daemon, shim, webapp, sidecar
Global runtimes:    emacs, daemon, store, sidecar

The global Emacs default follows TMPDIR.  If agent-repl-log-file-name is
customized, set AGENT_REPL_EMACS_GLOBAL_LOG to that exact live path.

Performance output format
-------------------------
Every performance verb writes headerless, tab-separated rows on stdout; log
resolution evidence and unmatched-span evidence go to stderr.  An unknown or
inapplicable value is the literal "-".  Millisecond columns carry three
decimal places.  Tabs, carriage returns and newlines inside a field are
replaced with spaces so a row is always exactly one line.

  --spans       start_ts  end_ts  duration_ms  operation  key  status
                status is matched, unmatched-start or unmatched-end.  An
                unmatched endpoint is a finding, so it is emitted, never
                dropped; its missing side and duration_ms are "-".
                operation is the start record's operation (the end record's
                operation for an unmatched-end row).

  --latency-by  key  count  p50_ms  p95_ms  max_ms
                Rows are sorted by p95_ms descending, then key ascending.
                Percentiles are nearest-rank over the key's matched-span
                durations.  Only matched spans are aggregated; every excluded
                unmatched endpoint is reported on stderr.

  --gaps        gap_ms  before_ts  after_ts  before_operation  after_operation
                before_message  after_message
                Rows follow the selected stream in chronological order.

Span pairing
------------
Records are ordered by timestamp, ties by input order.  A record's phase comes
from context.phase or context.outcome when present, otherwise from the last
dot-segment of its operation.  A record whose phase is a completion word ends
the innermost open span; a start word opens one; an unclassified record
alternates open and close (the adjacency rule).  Pairing happens within one
correlation key and one operation family, where the family is the operation
with a trailing phase segment removed, and the key is request_id when the
record carries one, else pid:PID:FAMILY (conn:CONNECTION_ID:FAMILY for the
browser webapp, nokey:FAMILY when neither identifies the emitter).
EOF
}

fail() {
  printf 'agent-repl-log-discovery: %s\n' "$*" >&2
  exit 2
}

require_value() {
  [ "$#" -ge 2 ] || fail "$1 requires a value"
}

WORKSPACE=""
SCOPE=""
RUNTIME=""
SESSION=""
PID=""
TAIL_LINES=""
SPANS_PREFIX=""
SPANS_REQUESTED=0
LATENCY_FIELD=""
GAPS_MS=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --workspace)
      require_value "$@"
      [ -z "$SCOPE" ] || fail 'choose exactly one of --workspace and --global'
      SCOPE=workspace
      WORKSPACE="$2"
      shift 2
      ;;
    --global)
      [ -z "$SCOPE" ] || fail 'choose exactly one of --workspace and --global'
      SCOPE=global
      shift
      ;;
    --runtime)
      require_value "$@"
      [ -z "$RUNTIME" ] || fail '--runtime may be specified once'
      RUNTIME="$2"
      shift 2
      ;;
    --session)
      require_value "$@"
      [ -z "$SESSION" ] || fail '--session may be specified once'
      SESSION="$2"
      shift 2
      ;;
    --pid)
      require_value "$@"
      [ -z "$PID" ] || fail '--pid may be specified once'
      PID="$2"
      shift 2
      ;;
    --tail)
      require_value "$@"
      [ -z "$TAIL_LINES" ] || fail '--tail may be specified once'
      TAIL_LINES="$2"
      shift 2
      ;;
    --spans)
      require_value "$@"
      [ "$SPANS_REQUESTED" -eq 0 ] || fail '--spans may be specified once'
      SPANS_REQUESTED=1
      SPANS_PREFIX="$2"
      shift 2
      ;;
    --latency-by)
      require_value "$@"
      [ -z "$LATENCY_FIELD" ] || fail '--latency-by may be specified once'
      LATENCY_FIELD="$2"
      shift 2
      ;;
    --gaps)
      require_value "$@"
      [ -z "$GAPS_MS" ] || fail '--gaps may be specified once'
      GAPS_MS="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *) fail "unknown argument: $1" ;;
  esac
done

[ -n "$SCOPE" ] || fail 'choose --workspace DIR or --global'
[ -z "$PID" ] || [[ "$PID" =~ ^[1-9][0-9]*$ ]] || fail '--pid must be a positive integer'
[ -z "$TAIL_LINES" ] || [[ "$TAIL_LINES" =~ ^[1-9][0-9]*$ ]] || fail '--tail must be a positive integer'

if [ -n "$LATENCY_FIELD" ]; then
  case "$LATENCY_FIELD" in
    request_id|operation|workspace_id) ;;
    *) fail "--latency-by field must be request_id, operation or workspace_id, not $LATENCY_FIELD" ;;
  esac
fi

if [ -n "$GAPS_MS" ]; then
  [[ "$GAPS_MS" =~ ^(0|[1-9][0-9]*)$ ]] || fail '--gaps must be a non-negative integer of milliseconds'
  [ -z "$LATENCY_FIELD" ] || fail '--gaps cannot be combined with --latency-by'
  [ "$SPANS_REQUESTED" -eq 0 ] || fail '--gaps cannot be combined with --spans'
fi

MODE=records
if [ -n "$GAPS_MS" ]; then
  MODE=gaps
elif [ -n "$LATENCY_FIELD" ]; then
  MODE=latency
elif [ "$SPANS_REQUESTED" -eq 1 ]; then
  MODE=spans
fi

case "$SCOPE" in
  workspace)
    [ -d "$WORKSPACE" ] || fail "workspace is not a directory: $WORKSPACE"
    WORKSPACE="$(cd "$WORKSPACE" && pwd -P)"
    allowed_runtimes=(emacs daemon shim webapp sidecar)
    ;;
  global)
    allowed_runtimes=(emacs daemon store sidecar)
    ;;
esac

if [ -n "$RUNTIME" ]; then
  valid=0
  for allowed in "${allowed_runtimes[@]}"; do
    [ "$RUNTIME" = "$allowed" ] && valid=1
  done
  [ "$valid" -eq 1 ] || fail "runtime $RUNTIME is not available in $SCOPE scope"
  runtimes=("$RUNTIME")
else
  runtimes=("${allowed_runtimes[@]}")
fi

state_root="${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}"
cache_root="${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl"
emacs_global_log="${AGENT_REPL_EMACS_GLOBAL_LOG:-${TMPDIR:-/tmp}/doom-agent-repl-$(id -u)/doom-agent-repl.log}"

runtime_path() {
  local runtime="$1"
  if [ "$SCOPE" = workspace ]; then
    printf '%s/.claude/emacs/%s.log' "$WORKSPACE" "$runtime"
    return 0
  fi
  case "$runtime" in
    emacs) printf '%s' "$emacs_global_log" ;;
    daemon) printf '%s/claude-repld.log' "$state_root" ;;
    store) printf '%s/log/shim-store.log' "$cache_root" ;;
    sidecar) printf '%s/log/shim-claude-sidecar.log' "$cache_root" ;;
  esac
}

resolve_workspace_target() {
  local canonical="$1"
  python3 - "$canonical" <<'PY'
import os
import sys

path = sys.argv[1]
if not os.path.islink(path):
    raise SystemExit(1)
print(os.path.realpath(path))
PY
}

paths=()
location_fd=1
if [ -n "$SESSION" ] || [ -n "$PID" ] || [ "$MODE" != records ]; then
  # Query stdout is intentionally pure JSONL so it composes with jq and other
  # record consumers.  Resolution evidence remains visible on stderr.
  location_fd=2
fi
for runtime in "${runtimes[@]}"; do
  canonical="$(runtime_path "$runtime")"
  if [ "$SCOPE" = workspace ]; then
    if [ -L "$canonical" ]; then
      target="$(resolve_workspace_target "$canonical")" || fail "cannot resolve canonical symlink: $canonical"
      printf 'workspace runtime=%s canonical=%s target=%s\n' "$runtime" "$canonical" "$target" >&"$location_fd"
      paths+=("$canonical")
    else
      printf 'workspace runtime=%s canonical=%s status=absent\n' "$runtime" "$canonical" >&"$location_fd"
    fi
  elif [ -f "$canonical" ]; then
    printf 'global runtime=%s path=%s\n' "$runtime" "$canonical" >&"$location_fd"
    paths+=("$canonical")
  else
    printf 'global runtime=%s path=%s status=absent\n' "$runtime" "$canonical" >&"$location_fd"
  fi
done

if [ -z "$SESSION" ] && [ -z "$PID" ] && [ "$MODE" = records ]; then
  exit 0
fi

[ "${#paths[@]}" -gt 0 ] || fail 'none of the selected canonical log files exists'

if [ -n "$TAIL_LINES" ]; then
  input_for_query() {
    local path
    for path in "${paths[@]}"; do
      tail -n "$TAIL_LINES" "$path"
    done
  }
else
  input_for_query() {
    local path
    for path in "${paths[@]}"; do
      cat "$path"
    done
  }
fi

input_for_query | python3 -c '
import json
import math
import sys
from datetime import datetime

mode, session, pid, spans_prefix, latency_field, gaps_ms = sys.argv[1:7]

START_PHASES = {
    "start", "started", "begin", "began", "request", "requested", "dispatch",
    "dispatched", "spawn", "spawned", "attempt", "attempted", "open", "opened",
    "enter", "entered", "send", "sent", "submit", "submitted", "issue", "issued",
}
END_PHASES = {
    "complete", "completed", "done", "finish", "finished", "end", "ended",
    "result", "response", "ready", "success", "succeeded", "ok", "fail",
    "failed", "failure", "error", "fatal", "timeout", "timedout", "close",
    "closed", "exit", "exited", "receipt", "received", "reply", "ack",
}


def die(message):
    raise SystemExit("agent-repl-log-discovery: " + message)


def classify(token):
    normalized = str(token).strip().lower().replace("_", "-")
    if not normalized:
        return None
    for candidate in (normalized, normalized.rsplit("-", 1)[-1]):
        if candidate in START_PHASES:
            return "start"
        if candidate in END_PHASES:
            return "end"
    return None


def phase_and_family(record):
    operation = record.get("operation")
    operation = operation if isinstance(operation, str) else ""
    context = record.get("context")
    if isinstance(context, dict):
        for field in ("phase", "outcome"):
            value = context.get(field)
            if isinstance(value, str):
                phase = classify(value)
                if phase:
                    return phase, operation
    segments = operation.split(".")
    phase = classify(segments[-1]) if segments else None
    if phase and len(segments) > 1:
        return phase, ".".join(segments[:-1])
    return phase, operation


def correlation_key(record, family):
    request_id = record.get("request_id")
    if isinstance(request_id, (str, int)) and str(request_id):
        return str(request_id)
    process_id = record.get("pid")
    if process_id is not None:
        return "pid:{0}:{1}".format(process_id, family)
    connection_id = record.get("connection_id")
    if connection_id is not None:
        return "conn:{0}:{1}".format(connection_id, family)
    return "nokey:" + family


def parse_timestamp(record, number):
    value = record.get("timestamp")
    if not isinstance(value, str) or not value:
        die("selected input line {0} has no usable timestamp".format(number))
    text = value[:-1] + "+00:00" if value.endswith("Z") else value
    try:
        parsed = datetime.fromisoformat(text)
    except ValueError as exc:
        die("selected input line {0} has an unparsable timestamp {1}: {2}".format(
            number, value, exc))
    return parsed.astimezone() if parsed.tzinfo is None else parsed


def clean(value):
    if value is None or value == "":
        return "-"
    text = str(value)
    for bad in ("\t", "\r", "\n"):
        text = text.replace(bad, " ")
    return text or "-"


def millis(value):
    return "{0:.3f}".format(value)


def emit(*columns):
    sys.stdout.write("\t".join(columns) + "\n")


def percentile(sorted_durations, fraction):
    index = max(0, math.ceil(fraction * len(sorted_durations)) - 1)
    return sorted_durations[index]


entries = []
for number, raw in enumerate(sys.stdin, 1):
    if not raw.strip():
        continue
    try:
        record = json.loads(raw)
    except json.JSONDecodeError as exc:
        die("invalid JSONL record at selected input line {0}: {1}".format(number, exc))
    if not isinstance(record, dict):
        die("selected input line {0} is not a JSON object".format(number))
    session_match = bool(session) and session in (
        record.get("agent_repl_session_id"), record.get("claude_session_id"),
    )
    pid_match = bool(pid) and record.get("pid") == int(pid)
    if (session or pid) and not (session_match or pid_match):
        continue
    entries.append((number, raw, record))

if mode == "records":
    for _number, raw, _record in entries:
        sys.stdout.write(raw)
    raise SystemExit(0)

ordered = []
for number, raw, record in entries:
    ordered.append((parse_timestamp(record, number), number, record))
ordered.sort(key=lambda item: (item[0], item[1]))

if mode == "gaps":
    threshold = float(gaps_ms)
    previous = None
    for moment, _number, record in ordered:
        if previous is not None:
            gap = (moment - previous[0]).total_seconds() * 1000.0
            if gap > threshold:
                emit(
                    millis(gap),
                    clean(previous[1].get("timestamp")),
                    clean(record.get("timestamp")),
                    clean(previous[1].get("operation")),
                    clean(record.get("operation")),
                    clean(previous[1].get("message")),
                    clean(record.get("message")),
                )
        previous = (moment, record)
    raise SystemExit(0)

pending = {}
spans = []
for moment, number, record in ordered:
    operation = record.get("operation")
    operation = operation if isinstance(operation, str) else ""
    if not operation.startswith(spans_prefix):
        continue
    phase, family = phase_and_family(record)
    slot = (correlation_key(record, family), family)
    if phase == "start":
        pending.setdefault(slot, []).append((moment, number, record))
        continue
    open_starts = pending.get(slot)
    if open_starts:
        start_moment, start_number, start_record = open_starts.pop()
        spans.append({
            "sort": (start_moment, start_number),
            "start_ts": start_record.get("timestamp"),
            "end_ts": record.get("timestamp"),
            "duration_ms": (moment - start_moment).total_seconds() * 1000.0,
            "operation": start_record.get("operation"),
            "key": slot[0],
            "status": "matched",
            "record": start_record,
        })
    elif phase == "end":
        spans.append({
            "sort": (moment, number),
            "start_ts": None,
            "end_ts": record.get("timestamp"),
            "duration_ms": None,
            "operation": operation,
            "key": slot[0],
            "status": "unmatched-end",
            "record": record,
        })
    else:
        pending.setdefault(slot, []).append((moment, number, record))

for slot, open_starts in pending.items():
    for moment, number, record in open_starts:
        spans.append({
            "sort": (moment, number),
            "start_ts": record.get("timestamp"),
            "end_ts": None,
            "duration_ms": None,
            "operation": record.get("operation"),
            "key": slot[0],
            "status": "unmatched-start",
            "record": record,
        })

spans.sort(key=lambda span: span["sort"])

if mode == "spans":
    for span in spans:
        emit(
            clean(span["start_ts"]),
            clean(span["end_ts"]),
            "-" if span["duration_ms"] is None else millis(span["duration_ms"]),
            clean(span["operation"]),
            clean(span["key"]),
            span["status"],
        )
    raise SystemExit(0)

buckets = {}
for span in spans:
    if span["status"] != "matched":
        sys.stderr.write(
            "agent-repl-log-discovery: excluded {0} operation={1} key={2}\n".format(
                span["status"], clean(span["operation"]), clean(span["key"])))
        continue
    if latency_field == "operation":
        key = clean(span["operation"])
    else:
        key = clean(span["record"].get(latency_field))
    buckets.setdefault(key, []).append(span["duration_ms"])

rows = []
for key, durations in buckets.items():
    durations.sort()
    rows.append((percentile(durations, 0.95), key, len(durations),
                 percentile(durations, 0.50), max(durations)))
rows.sort(key=lambda row: (-row[0], row[1]))
for p95, key, count, p50, worst in rows:
    emit(key, str(count), millis(p50), millis(p95), millis(worst))
' "$MODE" "$SESSION" "$PID" "$SPANS_PREFIX" "$LATENCY_FIELD" "$GAPS_MS"

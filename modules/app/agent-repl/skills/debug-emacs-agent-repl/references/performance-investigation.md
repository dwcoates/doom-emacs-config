# Performance investigation

Use this playbook to answer "where is the time going?" — latency, stalls, and
hitching — across every agent-repl runtime. The normative record shape is
`../../../logging-contract.md`. The resolver script owns its accepted
arguments. `/profile` owns live Emacs sampling. This runbook owns only the
operator workflow that chooses and composes them.

## Choose a strategy before collecting evidence

Two strategies answer different questions. Pick deliberately; do not sample a
live process to explain something that already happened.

| Strategy | Use when | Entry point |
|---|---|---|
| Historical latency | The slow event already happened, is intermittent, or spans runtimes | Structured logs, via the resolver's extraction verbs |
| Fresh sampling | The slowness reproduces on demand inside live Emacs | `/profile`, paired with `/runtime-eval-code` |

Every persisted record already carries `timestamp` and, where the owning
runtime knows it, `request_id`. Historical latency is therefore available
without new instrumentation for any path that logs a start and a completion.

Prefer historical first. It is read-only, covers all five runtimes, and needs
no reproduction. Escalate to fresh sampling only when the log-derived answer
stops inside Emacs and cannot name the function.

## Historical latency from structured logs

Select the workspace, runtime, session, or process first, exactly as in
`structured-logs.md`. The extraction verbs below compose with those selectors.

### Boundary spans

Pair start and completion records for one operation family and report each
pair's `duration_ms`:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --spans daemon.command
```

- The argument is an operation PREFIX, so one call covers a whole family.
- An unmatched start is emitted marked rather than dropped.
- Treat a marked unmatched start as a hang, a lost completion signal, or a
  process that died mid-boundary — never as a zero-cost call.

### Latency distribution

Aggregate count, p50, p95, and max, sorted by p95 descending:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --latency-by operation
```

- Accepted fields are `request_id`, `operation`, and `workspace_id`.
- Group by `operation` to find which boundary is slow.
- Group by `request_id` to find which single command was slow end to end.
- Group by `workspace_id` to separate a system-wide regression from one
  pathological workspace.
- Read p95 against max: a p95 near the median with a distant max is a rare
  stall, not a uniform slowdown.

### Timeline gaps

Report inter-record gaps above a threshold, with the records bracketing each
gap:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --runtime emacs \
  --gaps 500
```

- The threshold is in milliseconds.
- Use gaps when nothing logs a duration, which is the common case for a stall.
- The bracketing records name the last thing that ran and the first thing that
  resumed. That pair, not the gap length, is the finding.
- A gap is silence, not proof of work. An idle process, a filtered verbosity
  rung, and a blocked main thread all look identical here — rule out the first
  two through `structured-logs.md` before concluding the third.

## Per-plane entry points and current limits

State the limit honestly whenever a plane cannot be measured directly.

| Plane | Entry point | Current limit |
|---|---|---|
| Emacs | `/profile` for sampled profiles; `/runtime-eval-code` to measure one operation | None for sampling; log-derived gaps still needed for cross-runtime context |
| Daemon | Log-derived spans and gaps only | No pprof endpoint exists yet |
| Store | Log-derived spans and gaps only | No pprof endpoint, and no per-query timing is emitted |
| Sidecar | Log-derived spans and gaps only | No pprof endpoint exists yet |
| Webapp | Forwarded browser records in `webapp.log` | No performance marks or navigation timings are emitted |

The absent Go profiling endpoints and the absent store query timing are
telemetry gaps, not dead ends. When either blocks a diagnosis, report it
through `observability-gaps.md` in the standard form rather than substituting
a guess about where a Go process spent its time.

## Known baselines

Compare measurements against these before calling a number slow:

- Workspace load targets one second or less per workspace.
- A frontend command must be acknowledged within the ack deadline of ten
  seconds, after which the command is treated as lost.

A duration inside its baseline is not a finding. A duration outside it is a
finding only once the identity spine ties it to the affected workspace,
session, and request.

## Traps

- A slow p95 for an operation whose records were written by stale deployed
  code measures the old artifact. Confirm freshness through
  `health-and-readiness.md` first.
- Verbose records are absent by default, so a hot path can look instantaneous
  purely because its intermediate records were never persisted.
- Wall-clock gaps include time the machine spent elsewhere. One slow workspace
  among many concurrently active ones is contention, not that workspace's bug.
- Never derive a duration by subtracting timestamps across runtimes without
  first confirming both records belong to the same `request_id`.

## Companion runbooks

- `structured-logs.md` for the record shape, sinks, and verbosity rungs that
  determine which records exist to measure.
- `identity-correlation.md` for the keys — `request_id`, `workspace_id`,
  session identifiers — that every aggregation groups by.
- `observability-gaps.md` because missing timing telemetry is itself a
  reportable finding.

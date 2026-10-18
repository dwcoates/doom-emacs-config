# Plan: always-on runtime performance metrics

Status: agreed with the owner 2026-10-17, unbuilt. Branch `metrics-o11y`.

## Goal

Every agent-repl runtime records its performance all the time, so an agent can
answer questions like "in which version did workspace switching start to slow"
or "when did the daemon's CPU start to climb" from stored history alone, with
no profile session open and no reproduction.

Dashboards are out of scope for now. The stored history and the skill that
reads it are the product; a page that draws it can come later on top of the
same data.

## Settled decisions

| Decision | Ruling |
|---|---|
| Scope | Performance first: CPU, memory, network, end-user latencies |
| Traces | Not wanted |
| Dashboards | Not now |
| History store | VictoriaMetrics single-node (faster and lighter than Prometheus; same PromQL and scrape config) |
| Retention | 30 days (`-retentionPeriod=30d`), revisit as needed |
| Scrape interval | 15 seconds, revisit as needed |
| Metrics vs profiling | Separate. Metrics are always on; profiling stays opt-in |
| Agent access | A `metrics-agent-repl` skill |

## Metrics are not profiling

- Metrics are a small set of running numbers (counters, gauges, histograms),
  updated in nanoseconds, safe to expose always. They answer what regressed
  and when.
- Profiling samples call stacks while a profile is being taken, costs a few
  percent of CPU while it runs, and exposes memory contents and command
  lines. It answers which code is responsible.
- So metrics get their own always-on, loopback-only listener that serves
  `/metrics` and nothing else. The existing opt-in profiling servers
  (`daemon/internal/pprofsurface`, `shim-store/internal/pprofsurface`,
  `shim-sidecar/internal/pprofsurface`) and the `SIGWINCH` profile session
  (`proto/vocab/profile-session.json`) are unchanged.
- Merging the three near-identical `pprofsurface` copies is a separate,
  behavior-preserving cleanup that this work does not depend on.

## Architecture

```text
 daemon ──┐                                  ┌── metrics-agent-repl skill
 store  ──┼── /metrics (loopback) ◀── scrape ─┤   (PromQL over HTTP)
 sidecar ─┘          ▲          VictoriaMetrics
                     │          (launchd, 15s, 30d)
 shims, Emacs, webapp ── report over existing links ──▶ daemon
```

- The daemon, store and sidecar each expose `/metrics` through one shared Go
  package (a new module beside `agent-shim/logging/go`), each with its own
  `client_golang` registry carrying the Go runtime collector.
- The shims, Emacs and the webapp cannot be scraped. They report observations
  to the daemon over the links they already have, and the daemon exposes the
  resulting series. This is a contract change and goes through the
  `create-or-update-protobufs` skill.
  - Metric names on that path are a closed proto enum, so a runtime cannot
    report a metric the daemon does not know.
  - A series reported for a page, shim or workspace is deleted when that page,
    shim or workspace goes away, so closed workspaces do not leave series
    behind.
- The metrics listeners bind fixed loopback TCP ports (VictoriaMetrics cannot
  scrape a unix socket). The ports are defined once and read by both the
  programs and the scrape config.
- VictoriaMetrics runs under `launchd` beside the store and sidecar plists,
  with its config in the repo. `install-agent-repl` and the deploy flow
  install and keep it running.

## Tracing a number to a version

- Every runtime exposes `agentrepl_build_info{program, version, build}` with
  value 1. `version` is the semver the merge queue tags each landing on
  master with (`daemon/internal/semver`,
  `docs/protobuf-design/merge-version-increment.md`); `build` is the content
  hash from `daemon/internal/buildid`.
- The version is `git describe --tags --match '[0-9]*.[0-9]*.[0-9]*'` at
  build time. A build of a landing reads exactly `0.109.0`; a build between
  landings (a feature branch, a worktree) reads `0.109.0-3-gabc1234`, so it
  is never mistaken for the tagged release. The match pattern excludes the
  `agent-repl-test-checkpoint-*` tags.
- `buildid` records no version today, so the build stamps it in
  (`-ldflags -X` for Go, a build-time constant for the shim and webapp, the
  elisp build for Emacs).
- The version rides on this one series only. Latency series never carry it,
  because landings happen many times a day and a version label on every
  series would mint a whole new set of series per landing. Queries join
  `agentrepl_build_info` to group by version.

## What is measured

### End-user latencies (histograms, seconds)

- The definitions already exist in `bin/profile-report.go`: `prompt_echo`,
  `switch_repeat`, `switch_first`, `workspace_open`, each with a threshold and
  built from Emacs and webapp log edges.
- They move into one shared vocabulary file, `proto/vocab/latencies.json`,
  naming each latency, its start and end edges, its owning runtime and its
  threshold. The runtime histograms and `profile-report.go` both read it, so
  both measure between exactly the same edges.
- Added: page load (page request to conversation drawn) and time to first
  token (prompt forwarded to the first vendor token).
- Open design point for the proto pass: a latency whose two edges are seen by
  different runtimes (e.g. `switch_first` starts in Emacs and ends in the
  webapp) either carries a correlation id from its start edge or is owned by
  the runtime that sees both. Both runtimes share the host's clock, so this is
  about pairing the edges, not about clock skew.
- Labels: `workspace` (by id; `agentrepl_workspace_info{workspace, name}`
  maps it to the current name) and `outcome` where meaningful.

### CPU and memory per process

- A process collector inside the daemon reads `proc_pid_rusage` and
  `proc_pidinfo` for every process agent-repl owns: the daemon, store,
  sidecar, every shim, Emacs, and the WebKit processes.
- Per process: CPU seconds, RSS, file descriptors, threads, disk I/O, and the
  energy counters macOS keeps per process.
- It is computed when scraped, so it runs no timer of its own.
- It replaces the 2-second `ps` sampler in `bin/profile-agent-repl.sh`, so the
  profile report and the stored history read the same numbers.

### Network

- Only traffic that leaves the machine is counted; the store, sidecar,
  daemon and webapp talk over loopback, which costs CPU (already measured)
  but no network.
- The shim counts the bytes it sends to and receives from the vendor API, in
  memory, per workspace. That is nearly all of agent-repl's external traffic.
- The daemon's remaining external callers are left uncounted unless a
  question needs them: the news digest's HTTP fetches
  (`daemon/internal/newsdigest/fetch.go`), the headless model calls, which
  run in a `claude` subprocess (`daemon/internal/headless/client.go`), and
  git fetches.
- Never through `com.apple.network.statistics` kernel-control sockets: the
  removed vendor-traffic sampler leaked those sockets and is the inferred
  cause of the kernel mbuf exhaustion recorded in
  `docs/protobuf-design/drop-vendor-traffic.md`.

### Host

- Host CPU, memory and disk free, read by the same process collector, so no
  `node_exporter` runs.

### Cardinality

- Allowed labels: `program`, `workspace`, `method`, `outcome`, `model`.
- Never labels: prompt text, paths, session ids, versions (see above).
- Expected size: a few thousand active series, mostly latency histograms.

## Collection never runs on a hot path

- Recording an observation on a hot path is a `client_golang` atomic
  update (or the shim's equivalent) and nothing more: no lock shared with
  the daemon's request handling, no I/O, no allocation per call.
- The process collector runs only inside the `/metrics` handler, on that
  listener's own goroutine, when VictoriaMetrics scrapes. It reads the set of
  processes to sample from a published snapshot (`daemon/internal/publish`),
  never by taking a workspace lock, so a slow `proc_pid_rusage` cannot stall
  a prompt or a switch.
- Reports from the shims, Emacs and the webapp are handled on their RPC's
  own goroutine and only update series.
- In the webapp and Emacs, the report is batched and sent off the
  interaction it measures (after paint, on an idle timer), so measuring a
  switch never lengthens it.
- A test drives a scrape whose process reader blocks and asserts a prompt
  and a switch still complete.

## Proto design ownership

The owner delegated the proto design (step 5) entirely: the reporting
messages and the edge-pairing design point are decided and landed without
owner input, through the `create-or-update-protobufs` skill, and reported
after.

## The `metrics-agent-repl` skill

Authored through the `create-or-update-skill` skill. It covers:

- Checking metrics: the VictoriaMetrics query endpoint, the metric catalog,
  and ready-made queries:
  - p95 of each latency, grouped by version;
  - the first version where a latency's p95 crossed its threshold;
  - CPU and memory trend changes per process.
- Adding a metric: where to register it, naming, allowed labels, adding a
  latency to `proto/vocab/latencies.json`, and the tests every new metric
  needs.
- When to offer adding a metric: whenever the user is investigating how fast
  a feature or UI element is and no metric covers it.
- When to mention metrics: in any performance investigation, check the stored
  series before profiling.
- `profile-agent-repl` and `debug-emacs-agent-repl` point to it.

## Order of work

1. Shared metrics package and the always-on loopback `/metrics` listener in
   the daemon, store and sidecar, with the Go runtime collector.
2. `agentrepl_build_info` in every runtime, with the version stamped at build.
3. The daemon's process collector (per-process and host figures), replacing
   the `ps` sampler in `bin/profile-agent-repl.sh`.
4. VictoriaMetrics under `launchd`, its scrape config, the fixed ports, and
   install/deploy coverage.
5. Proto pass: the reporting path from shims, Emacs and the webapp to the
   daemon, and the edge-pairing design point above.
6. `proto/vocab/latencies.json` and the end-user latency histograms, with
   `profile-report.go` reading the same file.
7. Vendor API byte counters in the shim.
8. The `metrics-agent-repl` skill, and the pointers from the other skills.

## Testing

- Unit tests for the shared package, the process collector (behind an
  injectable reader of process figures), series deletion, and every error
  path, including the canonical log record each error writes.
- A test that each of the three Go programs serves `/metrics` through the
  shared package rather than its own copy.
- e2e reads `/metrics` directly. VictoriaMetrics never runs in tests.

# Plan: always-on runtime performance metrics

Status: agreed with the owner 2026-10-17; built 2026-10-18 on branch
`metrics-o11y`. Where the build departed from this plan, the reason is in
`docs/METRICS-JUDGEMENT-CALLS.md`, and the reporting contract's record is
`docs/protobuf-design/runtime-metrics.md`. The built catalog is the last
section below.

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

- Allowed labels: `program`, `workspace`, `method`, `outcome`, `model`, and a series's own closed vocabulary (`cause`, `reason`, `choice`, `phase`, `restart`).
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

## Built catalog

Every series below is queryable through `/metrics-agent-repl`. Every series also
carries the target labels `instance` (the program) and `run` (its process
run, judgement call 12).

| Series | Type | Labels | Source |
|---|---|---|---|
| `agentrepl_build_info` | gauge, 1 | `program`, `build` | every Go runtime (`agentrepl/logging/metrics`) |
| `go_*`, `process_*` (Go runtime) | various | `instance` | every Go runtime |
| `agentrepl_deployed_version_info` | gauge, 1 | `version` | daemon (`runtimemetrics`), at start and after each deploy |
| `agentrepl_latency_seconds` | histogram | `latency`, `workspace` | daemon: `switch_repeat`, `switch_first`, `page_boot`, `prompt_echo`, `page_stall` and `reload_blank` reported by pages; `editor_stall`, `elisp_reload` and `editor_input_wait` reported by Emacs under an empty `workspace`; `vendor_first_output` and `shim_event_loop_delay` folded from shims |
| `agentrepl_shim_heap_bytes` | gauge | `workspace`, `kind` | daemon, from each shim's last measurements frame: `used`, `total`, `external` |
| `agentrepl_process_cpu_seconds_total` | counter | `process`, `workspace` | daemon (`procmetrics`), per group, monotonic across churn |
| `agentrepl_process_disk_read_bytes_total`, `..._written_bytes_total` | counter | `process`, `workspace` | daemon (`procmetrics`) |
| `agentrepl_process_energy_joules_total` | counter | `process`, `workspace` | daemon (`procmetrics`) |
| `agentrepl_process_resident_bytes`, `..._footprint_bytes` | gauge | `process`, `workspace` | daemon (`procmetrics`) |
| `agentrepl_process_threads`, `..._open_fds`, `..._count` | gauge | `process`, `workspace` | daemon (`procmetrics`) |
| `agentrepl_host_cpu_seconds_total` | counter | `mode` | daemon (`procmetrics`), since the daemon started like every counter here, so `increase_pure` reads it right |
| `agentrepl_host_memory_bytes`, `..._memory_total_bytes`, `..._memory_pressure` | gauge | `state` | daemon (`procmetrics`) |
| `agentrepl_host_disk_free_bytes`, `..._disk_total_bytes` | gauge | | daemon (`procmetrics`) |
| `agentrepl_host_on_battery` | gauge, 0 or 1 | | daemon (`procmetrics`) |
| `agentrepl_vendor_bytes_sent_total`, `..._received_total` | counter | `workspace` | daemon, folded from shims; published by no shim yet (judgement call 6) |
| `agentrepl_restart_seconds` | histogram | `restart`, `phase`, `cause`, `outcome` | daemon (`restarttiming`), timed where each restart starts and ends (below) |
| `agentrepl_workspace_restart_last_seconds` | gauge | `restart`, `phase`, `workspace` | daemon: each workspace's latest duration of a per-workspace restart phase |
| `agentrepl_rpc_seconds` | histogram | `rpc`, `outcome` | daemon: every unary rpc from its arrival to its handler's return; `outcome` is `ok` or the Connect code |
| `agentrepl_log_records_total` | counter | `runtime`, `level`, `operation` | daemon: every Warn and Error record it writes, its own and Emacs's and the webapp's (`dlog.Surfaces.BindRecordCounter`) |
| `agentrepl_db_bytes` | gauge | `db`, `file` | daemon (`wsm`) and store (`events`): each SQLite file (`db`, `wal`, `shm`) |
| `agentrepl_log_bytes`, `agentrepl_log_files` | gauge | `runtime` | daemon: the state root's logs directory, by the runtime each file's name carries |
| `agentrepl_merge_step_seconds` | histogram | `step` | daemon (`merge`): how long a merge spent on each step before it left it |
| `agentrepl_merge_seconds` | histogram | `span`, `outcome` | daemon (`merge`): each merge's `pick`, `queued`, `running` and `total` spans, `landed`, `already_on`, `failed` (or `ok` for a pick) |
| `agentrepl_keepalive_lateness_seconds` | histogram | `outcome`, `workspace` | daemon (`cachelapse`): how long after it came due each asked-for keep-alive went out; `outcome` is `sent`, `main_agent_busy`, `detached_work_live`, `cache_lapsed`, `submit_failed`, `shim_unreachable`, `no_session`, `unnamed_judgment`, `session_parked` or `not_served`; the count per outcome is how many ended that way |
| `agentrepl_cache_lapses_total` | counter | `outcome`, `workspace` | daemon (`cachelapse`): recorded cache lapses coming due, `stood_down`, `shown_first`, `merge_queued` or `moved_later` |
| `agentrepl_cold_gate_idle_seconds` | histogram | `outcome`, `workspace` | daemon (`cachelapse`): how long a cache sat idle before a cold gate was raised over it; `outcome` is `lapsed_5m`, `lapsed_1h`, `lapsed_other_ttl`, `model_switch` or `unnamed_reason`, and the count is how many gates a shim judged: a gate raised again from its record, after a failed answer or carried over a handover is not counted again |
| `agentrepl_shim_start_seconds` | histogram | `cause`, `outcome`, `workspace` | daemon (`workspace.Fleet`): how long a shim start took, from the fleet taking it to the session up or the start failing; `cause` is why it started (`internal/shimlife`: `prompt`, `relaunch`, `keepalive`, `cold_gate_answer`, `boot`, `takeover`, `restart`, `create`, `fork`, `thread`, `merge`, `rebind`, or `unstated` for a caller that stated none) and `outcome` `ok` or `failed` |
| `agentrepl_shim_stand_downs_total` | counter | `reason`, `workspace` | daemon (`workspace.Fleet`): shims stood down on purpose, by `reason` (`keepalive_cycle`, `hibernate`, `cache_lapse`, `cold_gate`, `kill`, `rebind`, `handover`, `shutdown`, `replaced`, or `unstated`) |
| `agentrepl_cold_gate_answers_total` | counter | `choice`, `outcome`, `workspace` | daemon (workspace verbs): cold gates answered, by `choice` (`pay`, `clear`, `compact`, `compact_and_resume`) and `outcome` (`resumed`, `failed`, `abandoned`, `refused`) |
| `agentrepl_test_seconds` | gauge, one sample per run | `suite`, `test`, `outcome`, `trigger` | testrun, pushed when a merge queue gate's run ends: every individual test (each go test and subtest, ERT test, vitest test, harness group); `trigger` is `merge_queue` or `merge_queue_speculative` |
| `agentrepl_test_suite_seconds` | gauge, one sample per run | `suite`, `outcome`, `trigger` | testrun, pushed with the tests: each suite's slot time |
| `agentrepl_test_unit_wall_seconds`, `..._cpu_seconds`, `..._max_rss_bytes`, `..._block_reads`, `..._block_writes` | gauge, one sample per run | `suite`, `unit`, `outcome`, `trigger` | testrun, pushed with the tests: each unit's process (a chunk of tests, a build, a harness group), from the kernel's wait accounting, the finest grain resources are accounted at |
| `agentrepl_workspaces` | gauge | `repository`, `state` | daemon: the registry's workspaces, `open`, `hidden` or `closed`; live sessions are `agentrepl_process_count{process="shim"}` |

Process groups (`process`): `daemon`, `daemon_tools`, `store`, `sidecar`,
`shim` and `shim_tools` (by `workspace`), `emacs`, `webkit_webcontent`,
`webkit_networking`, `webkit_gpu`.

Restarts (`restart`, then its `phase`; added 2026-10-19 on the owner's ask
for metrics that show where restart time goes and when it regressed):

| `restart` | `phase` | `cause` | Timed by |
|---|---|---|---|
| `deploy` | `build`, `install`, `restart_services`, `decide`, `total` | | the deploying daemon |
| `deploy` | `handover`, `total` of a deploy that moved the daemon | | the successor, from the instants the intent manifest carries (judgement call 10) |
| `daemon_boot` | `claim`, `reconciled`, `serving`, `sessions_up`, each from the process start | `cold`, `successor` | the booting daemon |
| `service_restart` | `store` (its sidecar with it), `sidecar` | | `deploy.Restarter`, for a deploy and a boot alike |
| `workspace_start` | `start`, `wake` | `boot`, `takeover` | the bring-up, per workspace |
| `session_start` | `shim`, `slot_wait`, `vendor`, `watch`, `total`, each its own duration (`slot_wait` and `vendor` once per attempt) | `fresh`, `resume`, `wake`, `adopted`, `relaunch`, `cold_answer` | the session fleet, per workspace, whoever asked for the start |
| `editor_start` | `init`, `daemon_linked`, `first_tab`, `all_tabs`, each from the Emacs process's start | `spawned`, `running` | Emacs, reported through `ReportLatencies` once its startup's open ends |
| `shim_restart` | `wait`, `prelaunch`, `stand_down`, `resume`, `run` | the relaunch reason (`build_stale`, `restart_verb`, `query_died`, `shim_log_hard_ceiling`) | the rollout's bounce engine, per workspace |
| `handover_transfer` | `wait`, `run` | | the outgoing daemon, per workspace |
| `workspace_reconnect` | `host`, `web` | `cold`, `successor`, `webapp_reload` | the daemon, per workspace: from a boot's start, a handover's manifest or a webview reload's push until the workspace's Emacs host or webview stream opens on it |

Not built, each recorded with its reason:

- `prompt_first_output` stays profile-only. `prompt_echo` is reported by
  the webapp (`docs/protobuf-design/runtime-metrics.md`, entry 8) and
  `workspace_open` is measured by the daemon (judgement call 16).
- Vendor bytes: no shim counts them (judgement call 6).
- An elisp reload is not timed: Emacs keeps its streams across it, so
  there is nothing to come back (judgement call 9). Emacs's own launch is
  timed as `editor_start` (judgement call 17).

---
name: metrics-agent-repl
description: Read agent-repl's always-on runtime metrics — user-facing latencies (workspace switches, page boots, vendor first output), how long restarts take (deploys and their phases, daemon boots, store and sidecar restarts, each workspace's session start, shim replacements, handover transfers), CPU, memory, disk, energy and descriptors per process group, host load, and the deployed version — from the 30 days of history the metrics store keeps; find the version where a latency or a restart regressed or the day a process started to climb; and add a new metric the right way. Use when the user investigates performance or asks how fast, heavy or hot something is, when a latency or resource question needs history rather than a live profile, when the user wants a feature or UI element measured, or invokes /metrics-agent-repl.
allowed-tools: Bash(<skill_base_dir>/run.sh:*)
---

## What This Skill Does

Answers performance questions about agent-repl from stored history: what a latency or resource figure is now, how it moved, and which deployed version or day it changed on. Also says how to add a metric, when to offer one, and when to bring the metrics up unasked.

## Arguments

| Argument | Behaviour |
|---|---|
| `status` | Say whether the metrics store answers and which runtimes it scrapes. |
| `catalog` | List every metric the running runtimes expose, with its meaning. |
| `latency <name> [days]` | The latency's p95 per deployed version over the last `days` (default 30). |
| `regression <name> [seconds] [days]` | The first deployed version whose p95 of the latency went over `seconds` (default: the latency's own threshold). |
| `trend <process> [days]` | A process group's daily mean CPU and memory. |
| `restarts [days]` | Every restart phase's ok and failed counts, mean, p50, p90 and p99 over the last `days` (default 7). |
| `restart <restart> <phase> [days]` | The restart phase's p95 per deployed version over the last `days` (default 30). |
| `query <promql>` | Evaluate PromQL now. |
| `add <what>` | Add a metric for `what`, per step 4. |
| (none) | Run `status`, then answer the user's question with the steps below. |

## Steps

0. Check the metrics store.
  - Call `<skill_base_dir>/run.sh --status`.
    - `EXIT CODE 0:` the store answers and every runtime it scrapes is up. Continue to step 1.
    - `EXIT CODE 1:` the store does not answer. Tell the user `bin/requirements.sh install` (or `/install-agent-repl`) sets it up, and STOP.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 3:` a runtime is down or none is scraped. Relay the printed line, then continue to step 1: history up to the outage still answers.
  - **Why this lives here**: a query against a store that is not running, or that scrapes nothing, answers "no series", which reads as "nothing happened" when the truth is "nothing was measured".

1. Pick the verb for the question.
  - a. A latency, by name, per version: call `<skill_base_dir>/run.sh --latency-by-version <latency> [days]`.
  - b. "When did it get slow", "which change made it slow": call `<skill_base_dir>/run.sh --first-version-over <latency> [seconds] [days]`.
  - c. A process group's CPU or memory over time: call `<skill_base_dir>/run.sh --process-trend <process> [days]`.
  - d. How long restarts take, or which phase of a restart is slow: call `<skill_base_dir>/run.sh --restarts [days]`.
  - e. When a restart phase got slow: call `<skill_base_dir>/run.sh --restart-by-version <restart> <phase> [days]`.
  - f. Anything else: call `<skill_base_dir>/run.sh --catalog` to find the metric, then `--query <promql>`, or `--range <promql> <hours> <step>` for a series over time.
  - React to every verb above:
    - `EXIT CODE 0:` the answer is printed as a table. Continue to step 2.
    - `EXIT CODE 1:` nothing matched: no series, no runtime advertising a surface, or no version over the threshold (the versions seen are printed). Say so plainly, then continue to step 3.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
  - *NOTE*: latency names are the `latency` label: `switch_repeat`, `switch_first`, `page_boot`, `prompt_echo`, `workspace_open`, `vendor_first_output`, `shim_event_loop_delay`. Process groups are the `process` label: `daemon`, `daemon_tools`, `store`, `sidecar`, `shim`, `shim_tools`, `emacs`, `webkit_webcontent`, `webkit_networking`, `webkit_gpu`.
  - *NOTE*: restarts are the `restart` label, each with its `phase` label: `deploy` (`build`, `install`, `restart_services`, `decide`, `handover`, `total`), `daemon_boot` (`claim`, `reconciled`, `serving`, `sessions_up`, each from the process start), `service_restart` (`store`, `sidecar`), `workspace_start` (`start`, `wake`), `session_start` (`shim`, `slot_wait`, `vendor`, `watch`, `total`, each its own duration; cause `fresh`, `resume`, `wake`, `adopted`, `relaunch` or `cold_answer`), `shim_restart` (`wait`, `prelaunch`, `stand_down`, `resume`, `run`), `handover_transfer` (`wait`, `run`) and `workspace_reconnect` (`host`, `web`; cause `cold`, `successor` or `webapp_reload`), each workspace's Emacs and webview streams coming back after a restart. The slowest workspaces of the latest restart are `topk(5, agentrepl_workspace_restart_last_seconds{restart="workspace_reconnect"})` through `--query`, and the time every workspace took to come back is the largest of them.
  - *NOTE*: in a `--query` or `--range` of an `agentrepl_*` counter or histogram, ALWAYS use `increase_pure(...)`, NEVER `rate(...)` or `increase(...)`: those skip a series' first sample, so counts taken before the first scrape (a boot's, a takeover's) read as nothing. For a per-second figure, divide `increase_pure` by the window's seconds.
  - *NOTE*: a version reads `0.129.0` for a landing and `0.129.0+<commit>` for a build between landings; `git log` between two versions names the changes to read.

2. Report.
  - Relay the table, then state in one line what it shows: the version or day the figure changed, by how much, against which threshold.
  - NEVER report a latency without its threshold when it has one; `regression` without `seconds` already uses it.

3. Offer what is missing.
  - a. If the question was about the speed of a feature or UI element and no metric covers it, OFFER to add one (step 4). NEVER add one unasked.
  - b. If the question needs which code is responsible rather than when it changed, point the user at `/profile-agent-repl`: metrics say when and where, a profile says why.

4. Add a metric (only when the user asked for one).
  - a. A user-facing latency:
    - i. Name it in `proto/vocab/latencies.json` with its start, end, measuring runtime, threshold and buckets.
    - ii. The runtime that sees the interaction END measures it, from a start instant handed to it through the channel that already carries the interaction. NEVER pair a start from one runtime with an end from another anywhere else.
    - iii. Report it: a page or Emacs through `ReportLatencies` (a new `ReportedLatencyKind`), the shim through its measurements frame (a new `ShimLatencyKind` and histogram). A new kind is a contract change: follow `/create-or-update-protobufs`.
    - iv. Add the kind to the daemon's kind table; its vocabulary test fails until the two agree.
  - a2. A restart or a restart phase: add it to the daemon's restart vocabulary, and time it in the code that sees it start and end, in the one process that runs it.
  - b. A daemon-side counter, gauge or histogram: register it on the daemon's metrics registry in the package that owns the measured thing. Recording on a hot path MUST be a single atomic update, never a lock request handling takes, never I/O.
  - c. A store or sidecar metric: register it on that service's own registry.
  - d. Labels: only `program`, `process`, `workspace` (by id), `latency`, `method`, `outcome`, `model`. NEVER a prompt, a path, a session id or a version.
  - e. Tests: one for the recorded value, one per error path asserting the record the runtime's canonical logger writes, and the vocabulary test for a latency.
  - f. Record the addition in `docs/PLAN-runtime-metrics.md`'s catalog.

## Notes

- **CRITICAL NOTE: Check the stored series before reaching for a profile.** In any performance investigation, step 1 comes first: it answers when and where in seconds, and a profile session costs CPU on a battery.
- **IMPORTANT NOTE: Offer a metric, never add one unasked.** A missing metric is an offer at step 3; adding one is step 4 only on the user's word.
- **IMPORTANT NOTE: No series is not no problem.** Say "not measured" when the store is down or a runtime is not scraped (step 0), never "nothing happened".
- **IMPORTANT NOTE: Vendor bytes are not measured yet.** The traffic is made by the agent SDK's own child process; say so if asked rather than querying for it.
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.

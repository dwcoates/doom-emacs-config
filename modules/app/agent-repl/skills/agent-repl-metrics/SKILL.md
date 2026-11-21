---
name: agent-repl-metrics
description: Read agent-repl's always-on runtime metrics — user-facing latencies (workspace switches, page boots, vendor first output), how long restarts take (deploys and their phases, daemon boots, store and sidecar restarts, each workspace's session start, shim replacements, handover transfers), CPU, memory, disk, energy and descriptors per process group, host load, the deployed version, the merge queue (merges landed and failed, each step and round, repairs per merge, queue changes) and its deploy checks (how each check ended, how each remediation ended, remediations deployed in a row), and vendor token spending (every vendor call's tokens by workspace, account, model, agent, cause, cache outcome and context size, price-weighted, and keep-alive runs and their waste) — from the 30 days of history the metrics store keeps; find the version where a latency or a restart regressed or the day a process started to climb; and add a new metric the right way. Its CATALOG.md documents every agent-repl metric, and every change that adds or changes a metric updates that catalog in the same change. Use when the user investigates performance or asks how fast, heavy or hot something is, asks how often merges land or fail, how long they or their steps take, or how the deploy checks are doing, asks where the tokens or the money went, which workspace, model or cause spent the most, how warm the prompt cache is, or what keep-alives cost, when a latency or resource question needs history rather than a live profile, when the user wants a feature or UI element measured, or invokes /agent-repl-metrics.
allowed-tools: Bash(<skill_base_dir>/run.sh:*)
---

## What This Skill Does

Answers performance questions about agent-repl from stored history: what a latency or resource figure is now, how it moved, and which deployed version or day it changed on. Also says how to add a metric, when to offer one, and when to bring the metrics up unasked. `<skill_base_dir>/CATALOG.md` documents every agent-repl metric: its series, type, labels and source.

## Arguments

| Argument | Behaviour |
|---|---|
| `status` | Say whether the metrics store answers and which runtimes it scrapes. |
| `catalog` | List every metric the running runtimes expose, with its meaning. |
| `latency <name> [days]` | The latency's p95 per deployed version over the last `days` (default 30). |
| `regression <name> [seconds] [days]` | The first deployed version whose p95 of the latency went over `seconds` (default: the latency's own threshold). |
| `trend <process> [days]` | A process group's daily mean CPU and memory. |
| `restarts [window]` | Every restart phase's ok and failed counts, mean, p50, p90 and p99 by cause, over the last `window`: whole days, or a duration such as `30m`, `6h` or `7d` (default 7 days). A phase that recorded nothing in the window gets no row. |
| `restart <restart> <phase> [days]` | The restart phase's p95 per deployed version over the last `days` (default 30). |
| `spend [window] [by] [selector]` | Price-weighted vendor spend over the last `window` (as `restarts` reads it; default 1 day), one line per value of `by` (default `workspace`; also `model`, `account`, `cause`, `agent`, `subagent_type`, `cache_outcome`, `cold_reason`, `kind`, `context`), most first, with its share, calls, tokens and, for a workspace, its peak context bucket. `selector` narrows the series (`cache_outcome="cold_rewrite"`). |
| `spend-over <hours> <step>` | Price-weighted spend per `step` (`1h`, `1d`) over the last `hours`. |
| `keepalive-runs [window]` | Keep-alive runs that ended in `window`, by how each ended: runs, mean keep-alives, weighted spend. |
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
  - d. How long restarts take, or which phase of a restart is slow: call `<skill_base_dir>/run.sh --restarts [window]`; right after a landing, a window such as `30m` reads only its own restarts.
  - e. When a restart phase got slow: call `<skill_base_dir>/run.sh --restart-by-version <restart> <phase> [days]`.
  - f. The merge queue or its deploy checks: call `<skill_base_dir>/run.sh --query <promql>` with the matching recipe below, or `--range <promql> <hours> <step>` for it over time.
  - f2. Vendor token spending: call `<skill_base_dir>/run.sh --spend [window] [by] [selector]`, `--spend-over <hours> <step>` or `--keepalive-runs [window]` with the recipe below.
  - g. Anything else: read `<skill_base_dir>/CATALOG.md` (or call `<skill_base_dir>/run.sh --catalog` for what the running runtimes expose) to find the metric, then `--query <promql>`, or `--range <promql> <hours> <step>` for a series over time.
  - React to every verb above:
    - `EXIT CODE 0:` the answer is printed as a table. Continue to step 2.
    - `EXIT CODE 1:` nothing matched: no series, no runtime advertising a surface, or no version over the threshold (the versions seen are printed). Say so plainly, then continue to step 3.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
  - *NOTE*: latency names are the `latency` label: `switch_repeat`, `switch_first`, `page_boot`, `prompt_echo`, `workspace_open`, `vendor_first_output`, `shim_event_loop_delay`. Process groups are the `process` label: `daemon`, `daemon_tools`, `store`, `sidecar`, `shim`, `shim_tools`, `emacs`, `webkit_webcontent`, `webkit_networking`, `webkit_gpu`.
  - *NOTE*: restarts are the `restart` label, each with its `phase` label: `deploy` (`build`, `install`, `restart_services`, `decide`, `handover`, `total`), `daemon_boot` (`claim`, `reconciled`, `serving`, `sessions_up`, each from the process start), `service_restart` (`store`, `sidecar`), `workspace_start` (`start`, `wake`), `session_start` (`shim`, `slot_wait`, `vendor`, `watch`, `total`, each its own duration; cause `fresh`, `resume`, `wake`, `adopted`, `relaunch` or `cold_answer`), `editor_start` (`init`, `daemon_linked`, `first_tab`, `all_tabs`, each from the Emacs process's start; cause `spawned` or `running`), `shim_restart` (`wait`, `prelaunch`, `stand_down`, `resume`, `run`), `handover_transfer` (`wait`, `run`) and `workspace_reconnect` (`host`, `web`; cause `cold`, `successor` or `webapp_reload`), each workspace's Emacs and webview streams coming back after a restart. The slowest workspaces of the latest restart are `topk(5, agentrepl_workspace_restart_last_seconds{restart="workspace_reconnect"})` through `--query`, and the time every workspace took to come back is the largest of them.
  - *NOTE*: in a `--query` or `--range` of an `agentrepl_*` counter or histogram, ALWAYS use `increase_pure(...)`, NEVER `rate(...)` or `increase(...)`: those skip a series' first sample, so counts taken before the first scrape (a boot's, a takeover's) read as nothing. For a per-second figure, divide `increase_pure` by the window's seconds.
  - *NOTE*: merge queue and deploy check recipes, each over a window `W` such as `7d`:
    - Merges landed and failed: `sum by (change) (increase_pure(agentrepl_merge_queue_changes_total{change=~"landed|failed"}[W]))`. It is the queue's own rate, merges into a parent's branch included; which merges landed on a main branch, through the queue or otherwise, is `merge-queue landings` (`/agent-repl-merge-queue`).
    - Why merges left the queue without landing: `sum by (cause) (increase_pure(agentrepl_merge_queue_changes_total{change=~"failed|dequeued"}[W]))`.
    - How long merges take end to end: `histogram_quantile(0.9, sum by (le, outcome) (increase_pure(agentrepl_merge_seconds_bucket{span="total"}[W])))`.
    - Which step is slow: `histogram_quantile(0.9, sum by (le, round) (increase_pure(agentrepl_merge_round_seconds_bucket[W])))`.
    - How often a step fails: `sum by (round, outcome) (increase_pure(agentrepl_merge_round_seconds_count[W]))`.
    - Repairs per merge: `sum by (round, outcome) (increase_pure(agentrepl_merge_rounds_sum[W])) / sum by (round, outcome) (increase_pure(agentrepl_merge_rounds_count[W]))`.
    - How the intake's checks ended: `sum by (outcome) (increase_pure(agentrepl_deploy_mediation_seconds_count{span="total"}[W]))`; `approved` found nothing the deploy caused, `dispatched` handed its problems to a remediation workspace, `escalated` handed the deploy to the user.
    - How long checks take: `histogram_quantile(0.9, sum by (le, span, outcome) (increase_pure(agentrepl_deploy_mediation_seconds_bucket[W])))`; `rollout` is the hot reload, `assessment` the intake's work.
    - How each remediation ended and how long it took: `histogram_quantile(0.9, sum by (le, outcome) (increase_pure(agentrepl_deploy_remediation_seconds_bucket[W])))`; `landed`, `escalated` or `redispatched`.
    - Failed gates per remediation finish: `histogram_quantile(0.9, sum by (le, outcome) (increase_pure(agentrepl_deploy_remediation_gate_rounds_bucket[W])))`.
    - Remediations deployed in a row before the queue advanced: `histogram_quantile(0.9, sum by (le) (increase_pure(agentrepl_deploy_remediation_chain_redeploys_bucket[W])))`, and right now: `agentrepl_deploy_remediations_in_a_row`.
  - *NOTE*: messages between workspaces, each over a window `W` such as `7d`:
    - Prompts agents sent to other workspaces (`agentrepld workspace prompt`): `sum(increase_pure(agentrepl_prompts_submitted_total{origin="PROMPT_ORIGIN_AGENT_SENT"}[W]))`; `sum by (origin) (...)` without the matcher is every submission by origin.
    - Claude Code's own cross-session messages (SendMessage between sessions): `agentrepl_peer_messages{end="sender"} - agentrepl_peer_messages{end="sender"} offset W` through `--query`, or `--range 'agentrepl_peer_messages{end="sender"}' <hours> <step>` for its climb over time. It is a gauge read from the store, so never `increase_pure` it.
    - Daemon notices and agent-sent prompts, how many by type, `sum by (notice) (increase_pure(agentrepl_workspace_notice_bytes_count[W]))`; their sizes, `histogram_quantile(0.9, sum by (le, notice) (increase_pure(agentrepl_workspace_notice_bytes_bucket[W])))`; notices switched off, `sum by (notice) (increase_pure(agentrepl_workspace_notices_suppressed_total[W]))`.
  - *NOTE*: vendor spend recipes, each over a window `W` such as `1d` or `6h` (the verbs take it as `30m`, `6h`, `7d` or whole days). Every weighted figure is derived by `run.sh` from `<skill_base_dir>/spend-weights.json`, the one home of the price weights (input 1, 5-minute cache write 1.25, 1-hour cache write 2, cache read 0.1, output 5, and a per-model base price where the vendor prices a model differently); NEVER weigh tokens by hand in a query, and NEVER state another weight:
    - Total spend and spend per day or hour: `--spend-over <hours> 1d` or `--spend-over <hours> 1h`; the window's total is any `--spend W <by>` summed, such as `--spend W account`.
    - Spend share by token kind: `--spend W kind`. This is the share of WEIGHTED SPEND each kind took (cache reads are usually most of it).
    - Cache hit rate, TOKEN-WEIGHTED, which is a DIFFERENT number from the spend share above: `--query 'sum(increase_pure(agentrepl_vendor_tokens_total{kind="cache_read"}[W])) / sum(increase_pure(agentrepl_vendor_tokens_total{kind=~"cache_read|cache_write_5m|cache_write_1h|uncached_input"}[W]))'`, the share of input TOKENS the cache served. Always name which of the two a figure is.
    - Per-workspace ranking, with calls and peak context: `--spend W workspace`. Workspaces are labeled by id; resolve each to its name (`agent-repld workspace status <id>`) before reporting it.
    - By model, by account (`default` is the default account root, `multi_repo` the multi-repo root's): `--spend W model`, `--spend W account`.
    - By cause (user prompt, keep-alive, cold-gate answer, merge, automated, workspace notice, compaction, vendor-started, network resume, vendor error retry, workspace task, prompt summary, headless question, response revision, or `unstated`, which the producer recorded at ERROR): `--spend W cause`.
    - Keep-alive spend: the `keepalive` row of `--spend W cause`; keep-alive runs and their waste: `--keepalive-runs W`, whose `run_cap` and `closed` rows are runs that ended without the workspace working again (unresumed waste) and `resumed` the ones that paid off.
    - Subagent spend: `--spend W agent` (`main`, `subagent`, `side_query`, `headless`), and by subagent type: `--spend W subagent_type 'agent="subagent"'`.
    - Cold rewrites by reason and by cause: `--spend W cold_reason 'cache_outcome="cold_rewrite"'` and `--spend W cause 'cache_outcome="cold_rewrite"'`; every cache outcome at once (`warm`, `partial`, `cold_rewrite`, `uncached_no_write`, the last a context sent uncached with nothing written): `--spend W cache_outcome`.
    - Spend by context bucket: `--spend W context` (a bucket is labeled by its upper bound in tokens, `+Inf` above 1M). Calls over 300k: `--query 'sum(increase_pure(agentrepl_vendor_call_context_tokens_count[W])) - sum(increase_pure(agentrepl_vendor_call_context_tokens_bucket{le="300000"}[W]))'`.
    - A window that predates the spend series reads as no series: say "not measured before the spend metrics landed", never "nothing was spent".
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
  - d. Labels: only `program`, `process`, `workspace` (by id), `latency`, `method`, `outcome`, `model`, or a closed vocabulary the code declares (a protobuf enum's spelling such as `origin`, a fixed set such as `end`). NEVER a prompt, a path, a session id or a version.
  - e. Tests: one for the recorded value, one per error path asserting the record the runtime's canonical logger writes, and the vocabulary test for a latency.
  - f. Add or change the metric's row in `<skill_base_dir>/CATALOG.md` in the same change: its series, type, labels with their values, and source. A change to a label, a label value, a bucket set, a restart or a phase updates its row too.

## Notes

- **CRITICAL NOTE: Check the stored series before reaching for a profile.** In any performance investigation, step 1 comes first: it answers when and where in seconds, and a profile session costs CPU on a battery.
- **CRITICAL NOTE: `CATALOG.md` is the one home of every metric.** Every change that adds or changes a metric updates it in the same change; NEVER land a metric without its row.
- **IMPORTANT NOTE: Offer a metric, never add one unasked.** A missing metric is an offer at step 3; adding one is step 4 only on the user's word.
- **IMPORTANT NOTE: No series is not no problem.** Say "not measured" when the store is down or a runtime is not scraped (step 0), never "nothing happened".
- **IMPORTANT NOTE: Vendor bytes are not measured yet.** The traffic is made by the agent SDK's own child process; say so if asked rather than querying for it.
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.

# agent-repl metrics catalog

THE ONE HOME of every agent-repl metric. Every change that adds or changes
a metric (a series, a label, a label value, a bucket set, a restart or phase)
updates this file in the same change.

Every series below is queryable through `/agent-repl-metrics`. Every series also
carries the target labels `instance` (the program) and `run` (its process
run, judgement call 12).

| Series | Type | Labels | Source |
|---|---|---|---|
| `agentrepl_build_info` | gauge, 1 | `program`, `build` | every Go runtime (`agentrepl/logging/metrics`) |
| `go_*`, `process_*` (Go runtime) | various | `instance` | every Go runtime |
| `agentrepl_deployed_version_info` | gauge, 1 | `version` | daemon (`runtimemetrics`), at start and after each deploy |
| `agentrepl_latency_seconds` | histogram | `latency`, `workspace` | daemon: `switch_repeat`, `switch_first`, `page_boot`, `prompt_echo`, `page_stall`, `reload_blank` and `switch_sidebar` reported by pages; `editor_stall`, `elisp_reload` and `editor_input_wait` reported by Emacs under an empty `workspace`; `vendor_first_output` and `shim_event_loop_delay` folded from shims |
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
| `agentrepl_deploy_mediation_seconds` | histogram | `span`, `outcome` | daemon (`merge`): each deploy check's `rollout` span (deploy start to the chain's intake told, outcome `told`; the sidebar's "HOT-LOADING ZETTEL"), its `assessment` (told to the intake's decision; "ASSESSING ZETTEL HEALTH") and its `total` span, by how it ended: `approved` (nothing this deploy caused), `escalated` (put before the user), `dispatched` (the deploy's problems handed to a remediation workspace; its `total` ends when the fix's deploy replaces the check), `undecided` (the intake's turn decided nothing), `superseded` (replaced before anything was dispatched), `unmediated` (the repository checks its deploys no more), `unchanged` (the deploy changed nothing, every component up to date, so it was not checked). The `total` span's count per outcome is how often each happened |
| `agentrepl_deploy_remediation_seconds` | histogram | `outcome` | daemon (`merge`): each deploy remediation, from the intake's dispatch to the check that ended it: `landed` (its fix's deploy was approved), `escalated` (the chain was escalated or ended undecided after it), `redispatched` (its fix's deploy was still broken and the intake dispatched again) |
| `agentrepl_deploy_remediation_gate_rounds` | histogram | `outcome` | daemon (`merge`): per remediation finish, how many of its full gates failed and resumed the workspace before it `landed` directly or was `given_up` past the fixing bound (the chain then escalated) |
| `agentrepl_deploy_remediation_chain_redeploys` | histogram | `outcome` | daemon (`merge`): per chain of mediations the queue advanced past, how many remediations were deployed in a row (`dispatched` checks) before it did, by how the chain's last mediation ended (`approved`, `escalated`, `undecided`, `unmediated`). The chain is kept in the store, so it counts across the daemon restarts each remediation makes. Its count per outcome is how many chains ended so |
| `agentrepl_deploy_remediation_chain_seconds` | histogram | `outcome` | daemon (`merge`): how long each chain held the queue, from its first deploy to the queue advancing |
| `agentrepl_deploy_remediations_in_a_row` | gauge | | daemon (`merge`): the chain owed now, remediations deployed in a row since the queue last advanced; `0` when none is owed. Set by every intake round, and on the release |
| `agentrepl_merge_round_seconds` | histogram | `round`, `outcome` | daemon (`merge`): each round of a merge's tab, by kind (`queue`, `pick`, `rebasing`, `conflicts`, `tests`, `fixes`, `committing`, and a deploy stage's kind) and ledger outcome (`succeeded`, `failed`, `restarted`, `escalated`, `abandoned`) |
| `agentrepl_merge_rounds` | histogram | `round`, `outcome` | daemon (`merge`): per concluded merge, how many `rebasing`, `conflicts`, `tests` and `fixes` rounds it took (`0` for a kind it never opened), by how the merge ended (`landed`, `already_on`, `failed`) |
| `agentrepl_merge_queue_changes_total` | counter | `change`, `cause` | daemon (`merge`): every change to a queue's line, by kind (`enqueued`, `requeued`, `started`, `landed`, `failed`, `dequeued`, `reordered`) and, for a merge leaving, the cause its removal recorded (`evicted`, `failed`, `restart failed`, ...); empty otherwise |
| `agentrepl_workspace_notice_bytes` | histogram | `notice` | daemon: every message sent to a workspace, its text's size in UTF-8 bytes, by type: each daemon notice by the brief it was composed from (`derived-workspace-notice`, `workspace-sibling-notice`, `workspace-child-notice`, merge-queue, gather, pull-request, subscription and main-sync notices; briefprompt.Composer), and `agent_sent` for a prompt one workspace's agent sent another. Images add nothing. Its count per type is how many were sent |
| `agentrepl_workspace_notices_suppressed_total` | counter | `notice` | daemon (`workspacenotice`): notices of a type the daemon is set not to send, by type: family notices (`workspace-sibling-notice`, `workspace-child-notice`) while `AGENT_REPL_SIBLING_NOTICES` is not `on` (the default) |
| `agentrepl_keepalive_lateness_seconds` | histogram | `outcome`, `workspace` | daemon (`cachelapse`): how long after it came due each asked-for keep-alive went out; `outcome` is `sent`, `main_agent_busy`, `detached_work_live`, `cache_lapsed`, `submit_failed`, `shim_unreachable`, `no_session`, `unnamed_judgment`, `session_parked` or `not_served`; the count per outcome is how many ended that way |
| `agentrepl_cache_lapses_total` | counter | `outcome`, `workspace` | daemon (`cachelapse`): recorded cache lapses coming due, `stood_down`, `shown_first`, `merge_queued` or `moved_later` |
| `agentrepl_cold_gate_idle_seconds` | histogram | `outcome`, `workspace` | daemon (`cachelapse`): how long a cache sat idle before a cold gate was raised over it; `outcome` is `lapsed_5m`, `lapsed_1h`, `lapsed_other_ttl`, `model_switch` or `unnamed_reason`, and the count is how many gates a shim judged: a gate raised again from its record, after a failed answer or carried over a handover is not counted again |
| `agentrepl_shim_start_seconds` | histogram | `cause`, `outcome`, `workspace` | daemon (`workspace.Fleet`): how long a shim start took, from the fleet taking it to the session up or the start failing; `cause` is why it started (`internal/shimlife`: `prompt`, `relaunch`, `keepalive`, `cold_gate_answer`, `boot`, `takeover`, `restart`, `create`, `fork`, `thread`, `merge`, `rebind`, `settings`, or `unstated` for a caller that stated none) and `outcome` `ok` or `failed` |
| `agentrepl_shim_stand_downs_total` | counter | `reason`, `workspace` | daemon (`workspace.Fleet`): shims stood down on purpose, by `reason` (`keepalive_cycle`, `hibernate`, `cache_lapse`, `cold_gate`, `kill`, `rebind`, `handover`, `shutdown`, `replaced`, `settings_visit`, `cache_cold`, or `unstated`) |
| `agentrepl_cold_gate_answers_total` | counter | `choice`, `outcome`, `workspace` | daemon (workspace verbs): cold gates answered, by `choice` (`pay`, `clear`, `compact`, `compact_and_resume`) and `outcome` (`resumed`, `failed`, `abandoned`, `refused`) |
| `agentrepl_test_seconds` | gauge, one sample per run | `suite`, `test`, `outcome`, `trigger` | testrun, pushed when a merge queue gate's run ends: every individual test (each go test and subtest, ERT test, vitest test, harness group); `trigger` is `merge_queue` or `merge_queue_speculative` |
| `agentrepl_test_suite_seconds` | gauge, one sample per run | `suite`, `outcome`, `trigger`, `batching` | testrun, pushed with the tests: each suite's slot time; `batching` (from 2026-11-12) names the vitest and ERT file batch sizes in force, `max<BatchMax>-min<BatchMin>` (`suites.BatchingLabel`) |
| `agentrepl_test_run_seconds`, `agentrepl_test_run_predicted_seconds` | gauge, one sample per run | `outcome`, `trigger`, `batching` | testrun, pushed with the tests (added 2026-11-12): the whole run's wall time from its first unit's start to its last unit's end, and its plan's predicted makespan; `outcome` is `fail` when any suite failed |
| `agentrepl_test_suite_units` | gauge, one sample per run for each `state` | `suite`, `state`, `trigger`, `batching` | testrun, pushed with the tests (added 2026-11-12): how many of a suite's reuse units ran (`run`, each a process) and how many a pass record let it skip (`reused`), the two sides batch size trades |
| `agentrepl_test_unit_wall_seconds`, `..._cpu_seconds`, `..._max_rss_bytes`, `..._block_reads`, `..._block_writes` | gauge, one sample per run | `suite`, `unit`, `outcome`, `trigger` | testrun, pushed with the tests: each unit's process (a chunk of tests, a build, a harness group), from the kernel's wait accounting, the finest grain resources are accounted at |
| `agentrepl_workspaces` | gauge | `repository`, `state` | daemon: the registry's workspaces, `open`, `hidden` or `closed`; live sessions are `agentrepl_process_count{process="shim"}` |
| `agentrepl_lid_quiet` | gauge, 0 or 1 | | daemon, from persistent wifi's quiet state: 1 while persistent wifi's lid-closed quiet state stands |
| `agentrepl_app_freezes_total`, `agentrepl_app_thaws_total` | counter | `app` | daemon, from persistent wifi's quiet state: each application the quiet state froze, and thawed |
| `agentrepl_view_renders_skipped_total` | counter | `view` | daemon: the view renders and publications the quiet state skipped, by `view` (`sidebar`, `footer`, `topbar`, `feed`) |
| `agentrepl_prompts_submitted_total` | counter | `origin` | daemon (`promptqueue`, added 2026-11-15): every submission the prompt queue took, delivered or held, counted once when it arrives (a folded prompt the queue resubmits is not counted again); a refused one (no or an undeclared origin, a sender disagreeing with its origin, the merge lease, a cold gate, no session) is not counted. `origin` is the `conversation.v1.PromptOrigin` spelling, every declared one served from 0: `PROMPT_ORIGIN_AGENT_SENT` is a prompt another workspace sent (`agentrepld workspace prompt`), beside the user's prompts and the daemon's notices |
| `agentrepl_peer_messages` | gauge | `end` | store (`metrics.go`, added 2026-11-15): Claude Code's own cross-session messages (SendMessage between sessions), the live page lines the store's delivery index (`peer_delivery`) holds for each `end`, `sender` or `recipient`, read at scrape time, so exact and durable across restarts; its difference over a window is the messages sent or received in it. A count that fails or runs out its 650ms bound omits only this series and is recorded once per cause (`store.metrics.peer-messages`) |

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

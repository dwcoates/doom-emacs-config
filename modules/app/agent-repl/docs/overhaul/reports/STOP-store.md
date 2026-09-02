# Stopping point — store + sidecar teamlead (second pause, 2026-09-01)

## Branch and tip
- Branch `overhaul/store`, worktree /Users/dodgecoates/.config/doom-overhaul/store, tip: the commit that carries this file (parent 6e2795771, verified green).
- Carries merges of `overhaul/shim` 731aa5f00 and `overhaul/integration` (landing 6: protos d46e601e7, bindings 8a98e4fca).
- Agent worktrees under ~/.config/doom-overhaul/store-agents/: NONE. No agents running. The lead is parked, resident.

## Verified green at 6e2795771 (both with -race, AGENT_REPL_FORBID_VENDOR_CALLS=1)
- Store (agent-shim/shim-store): build, vet, gofmt, staticcheck U1000 zero, `go test -race -count=1 ./...` 6/6 ok.
- Sidecar (agent-shim/claude/shim-sidecar): same gate, 9/9 ok (integration ~230s with node; mock scenarios included).

## The recorded queue is COMPLETE
1. Store audit 2 (C1–C17, rule-violating logging subjects, assertNoDatabaseTouch guard, invalid_request.field = full envelope paths rooted at WriteBatchRequest as db-owned vocabulary in shim-store/AGENTS.md, `refusal_kind` beside `refusal_site` on store refusal records, detached_work-is-a-page-line ruling recorded). Store `--socket` flag beats env (subject socketflag_test.go).
2. Sidecar audit 1 remainder, all subjects (3–9, 11–18, 19–25, 7/24 integration subjects) + the warn/error census (every remaining record subject-provoked) + the outage ladder (first refusal error, later attempts warn with attempt/backoff_ms, exactly one info on recovery).
3. Programmatic dead-code pass (staticcheck U1000 + coverprofile), see lists below.
4. Sidecar adversarial loop: audits 2 and 3 dispatched and remediated; the lead closed the loop after audit 3 (each pass narrower; no further auditor queued).

## Production fixes landed this session (each with unit tests)
- TaskStop result `task_type:"local_agent"` settles the agent task as cancelled (corpus fixture task_stop.jsonl states local_agent; previously fell through to the shell branch).
- Recovered cursors indexed by file_id (rename-proof); watch() asks GetSidecarCursors{file_id} for a path with no cursor in the cycle snapshot before building a tailer; rewindOnce keyed per identity.
- Backgrounded-spawn observation: the observer was never set, so a backgrounded subagent's spool frames named the main agent as top_level. Then ownerIndex.backgroundedCalls keyed by the spawning call's activity id + sidecar.refreshSpawnFacts() re-read every rescan (discovery order is not causal order), so spool and sidechain planes of one backgrounded agent agree on top_level.
- AgentTranscriptHandler.LostTerminal: an a* spool concluded LOST upserts `activity:<toolUseId>` in the parent's book with AgentSubagentFailure.cause.lost{file_vanished|went_silent|swept_up}.
- Workflow per-agent transcripts (wf_*/agent-*.jsonl) land as vendor_specific{kind:"workflow/agent_transcript"} residue, never `unknown` (workflow stays kicked: residue only, no meta hold).
- Sidecar logs `refusal_site` beside `refusal_kind` on every refusal record.

## Dead-code pass (ruling "Dead code is hunted programmatically")
Deleted (no caller, confirmed by staticcheck U1000 and grep): shim-sidecar internal/discover/watcher.go (fsnotify Watcher, never wired) and Discoverer.SpoolRoot; convert.WorkflowSpool; convert.wholeStdout; ShellOutputHandler.Lost; AgentTranscriptHandler/SessionTranscriptHandler.SetObserver; storeclient Client.Socket; bootstrapError.Unwrap (sidecar and store); shim-store detached_work `cause*` constants; test-only helpers detachedSubagentFrame, toolUseIDOf, containsSubstring, phase, four requireNoneIn duplicates, harness.started field.
Kept with reason and pinned by unit test: store db.queryError (error path the suites do not drive); sidecar convert dispatch-table success converters (12: write/grep/sendMessage/webFetch/webSearch/wakeup/artifact/planMode/findings/worktree/cron/push — table-referenced, no corpus fixture), imageBlock, settleUnmodeled, taskActSettled/taskState/taskIDs, settleQuestion/questionAnswers, seamObserver.TaskStopped, jitterBackoff/bootTimeMillis, oversizeCarryError.Error; main/run/runWithLogger/logProcessExit/cycle.Run reached only through the exec'd binaries (integration suites run the built binary; the coverprofile cannot see them) — covered by the integration suites; integration fake-store hook FailWritesWithoutAKind (caller landed in audit 2 remediation).
Not deleted: agent-shim/wire — still imported by the daemon; its deletion belongs to the daemon rewrite (store.md ruling).

## Open items for the next wave (implementation-detail, no ruling needed to resume)
- discover.Target.TaskID is overloaded: for a sidechain transcript it carries the `agent-<id>` locator, not a harness task id (worked around in ownerIndex.backgroundedFor; a rename to VendorAgentID-only is the clean end state).
- Bounded-hold subjects widen poll intervals by construction (audit 2 minor 12): load-sensitive but loud-fail, accepted.

## Grounding gaps (project lead's capture checklist; subjects marked synthetic)
- Real context-budget attachment spelling (`context_tip` is ruled NOT it); the fixture stays synthetic, the audit-2 proposal to force context_tip to convert was rejected.
- Compaction-summary line (cheap-model captures never reached the threshold).
- api_error taxonomy: table holds exactly one grounded row (connection/StreamSuspended).
- Real /clear shape (from the identity-rotation-clear golden): old transcript stops with no closing record; new file under the second system:init's session_id; lineage is the shim's link file only. Sidecar subjects assume no closing record.

## Cross-team relays outstanding
- C4 to the shim lead: reconciliation must tolerate a refused WatchBashRun open (CodeNotFound) for an announced run with no rows yet (retry after the first row or synthesize from the announcement).

## Cross-team facts recorded for integration (unchanged)
- Pins: connectrpc.com/connect v1.17.0 + golang.org/x/net v0.43.0, go 1.23.x everywhere.
- Private test socket: env AGENT_REPL_STORE_SOCKET; a flag beats it (store --socket, sidecar --store-socket).
- Connect standing streams: consumers cancel the stream context; acceptance is silent, refusal is CodeNotFound on first Receive; servers flush headers on accepting a stream.
- Correlation keys: producer, agent_id, vendor_session_id, book_agent_id, write_id, upsert_key, position, write_seq, watch_token_hash, rpc, refusal_site, refusal_kind, statement, file_id, path, offset, task_id, activity_id, turn_id, reason, attempt, backoff_ms; request_id via X-Agent-Repl-Request-Id (store only; the sidecar serves no inbound rpc).

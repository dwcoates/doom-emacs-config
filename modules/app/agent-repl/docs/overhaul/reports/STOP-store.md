# Stopping point — store + sidecar teamlead (wind-down, 2026-08-31)

## Branch and tip
- Branch `overhaul/store`, worktree /Users/dodgecoates/.config/doom-overhaul/store, tip `3fea72b14`.
- The branch deliberately carries a merge of `overhaul/shim` 731aa5f00 (the mock-generated fixture work needed the mocked vendor); the project lead expects that merge at integration time.
- Agent worktrees under ~/.config/doom-overhaul/store-agents/: NONE (all merged and removed). No agents running.

## Merged and green at the tip (verified 2026-08-31)
- Store (agent-shim/shim-store): `go build && go vet && go test -race -count=1 ./...` — 6/6 packages ok, integration suite included.
- Sidecar (agent-shim/claude/shim-sidecar): same gate — 9/9 packages ok, integration suite included (~112s; the 133 mock scenarios build the shim via npm and skip loudly without node).
- Landings 1–5 merged (last: 081dbbba8, AgentBashOutput.not_observed). Landed work: ops (doctor Connect probes + harness + fake-store fixture, plists, wire/agent-shim AGENTS notes); store (Connect over UDS with h2c + header flush, four tables + write_ledger schema v3+, position/write_seq orderings, single-use watch tokens, WatchBashRun replay-follow-end, failure `kind` arms with `field` at every refusal site, socket exclusivity by dial-before-unlink, nuke rules incl. garbage-db, pprof-before-db with failed-boot hold); sidecar (Connect client, cursor-first cycle + store-unreachable invariant, boot rewind to the last real user prompt, bounded hold, multi-root discovery + spool prefixes, LOST policy with wire-carried DetachedLost arms + reason context key, per-row bash keys, residue keys residue:<uuid>|residue:file:<path>:<offset>, write_id digests file_id (R-S1), refusal-kind handling (invalid_request parks the file; storage_failure suspends), subagent AgentId = meta.toolUseId, sole-producer converters (context_injected, diagnostics adjacency, context_budget_warning), TaskStop cancel through the spool handler with real output, not_observed for never-read spools, stale/unowned-window flags); both integration suites; the mock-generated scenario subjects (131 pass / 2 skipped with stated reasons).
- Audit state: store audit 1 fully remediated. Sidecar audit 1 remediated in part (see "Staged: sidecar remainder"). Store audit 2 delivered and STAGED below (not remediated — wind-down). Sidecar audit 2 never dispatched.

## Staged: store audit 2 (17 critiques; remediation NOT dispatched) — teamlead rulings attached
Critiques verbatim-condensed; C-n = auditor's rank. All are test-side; no production defect claimed.
- C1 DetachedWorkId==AgentActivityId convergence subject (TestACoincidentHandleAndUnitIdIsOneObligation). RULING: add as specified; also align critique-17's fixture keys to `detached:<work id>`.
- C2 WatchBashRun edges: terminal re-upsert against an ended stream and a fresh replay; a delta first-inserted after the terminal. RULING (staged): replay serves every stored row in first-insert order; the natural end fires after the last stored row once a terminal row has been sent; a terminal re-upsert to an ended stream is absorbed silently (no re-send to past watchers, exactly-once in a fresh replay). Pin both subjects to that.
- C3 interleaved two-plane writers of one run (TestInterleavedPlanesReplayInFirstInsertOrder). RULING: add.
- C4 announced-but-never-written run: WatchBashRun refused (CodeNotFound) while live_detached lists it. RULING: pin exactly that; relay to the shim lead that reconciliation must tolerate a refused open for an obligation with no rows yet (retry after the first row or synthesize from the announcement).
- C5 cursor-only batch over the wire (TestACursorOnlyBatchIsDurablySuccessful). RULING: add.
- C6 cursor latest-wins/multi-file/file_id filter over the wire (TestCursorsPerFileLatestWins). RULING: add.
- C7 directory-at---db refusal (left intact) and stale -wal sibling removal as process subjects. RULING: add both.
- C8 non-socket regular file at the listen path (refuse, untouched). RULING: add.
- C9 bash-registry overflow black-box (TestASlowBashWatcherIsEndedWithResourceExhausted). RULING: add.
- C10 live_detached across restart. RULING: add.
- C11 producer_empty/batch_missing/cursor-advance field spellings on the wire. RULING: add to the malformed-batch table, each with assertNoDatabaseTouch.
- C12 concurrent upserts of ONE key (TestConcurrentUpsertsOfOneKeyLeaveOneRow). RULING: add.
- C13 pprof wildcard refusal + healthy-boot enabled record subjects. RULING: add.
- C14 SIGTERM during a wedged boot: bounded subject. RULING: add, bounded, accept nondeterminism margins.
- C15 empty catch-up page's boundary arm. RULING: floor (the caller is current; nothing older is owed below its mark) — extend the restart-reopen subject to assert floor.
- C16 env-vs-flag precedence subject for --socket. RULING: add.
- C17 announcement-key fixture drift. RULING: folded into C1.
Rule-violating subjects to fix: workflow-warning subject (operation-scoped exactly-once record; assert GetNotImplemented() arm, never detail substring; prove durability by restart or write_id replay); unreadable-db nuke record (operation-scoped with error context); unknown-token refusal (assertExactlyOneNormalRecord + refusal_site + watch_token_hash; add exactly-once logging subjects for consumed-token, post-restart token, and both refused bash-run opens).
Helper: wire assertNoDatabaseTouch's dead sawAnyStatement per-window guard or delete it with a comment pointing at the global positive control. Also queued earlier: normalize invalid_request.field to FULL envelope paths (db-owned vocabulary, documented in AGENTS.md, asserted per refusal); record in AGENTS.md the ruling that AgentFrame.detached_work is a page line for EVERY DetachableWork kind incl. workflow (live_detached still excludes workflow).

## Staged: sidecar audit 1 remainder (production halves landed; these SUBJECTS are unwritten)
From SIDECAR-AUDIT-1.md (scratchpad copy now superseded by this file): critiques 3 (store bounce mid-ingest; integration/helpers' startRealStoreAt still has no caller), 4 (bash read-back through the REAL store's WatchBashRun, single stream, follow phase), 5 (transcript carry across three polls + seeded carry on boot), 6 (SIGTERM with an in-flight batch), 8 (never-SessionUpdate), 9 (split EXIT marker), 11 (invalid_request parking subject against the real store), 12 (multi-file interleaving), 13 (subagent join + meta edge cases; note: a meta parsing but naming no toolUseId logs at ERROR deliberately — the audit brief said WARNING and the code is right), 14 (usage on a tool_use first block), 15 (api_error table — GROUNDING LIMIT: the corpus holds exactly ONE api_error fixture, a connection/StreamSuspended record; rate_limit_error/overloaded_error/authentication_error/numeric-only/unknown-type have no fixture, so the table cannot be grounded past one row without a new capture), 16 (TaskStop agent-task + unlaunched), 17 (diagnostics adjacency across a poll + injected skills), 18 (hold-at-cursor restart + keep-alive across compaction), 19–21 (brittle subjects rewrites), 22 (logging field set + exactly-once + operation convergence), 23 (flags/env), 25 (residue key literals), plus critique 7's two rename INTEGRATION subjects and critique 24's a*/w* integration subjects. Spawn fixtures for 13/14 exist: tool-inputs/agent.jsonl, tool-results/agent_async_launch.jsonl, sidechain/agent-aef975b7bc3422d4b.jsonl (meta toolUseId toolu_019w534yMVsDAc3KqJYLGhP8). Also owed: the sidecar warn/error census on a green run at this tip (last census was pre-fix2: every record subject-provoked).

## Capture-run checklist items contributed by this team
- The context-budget warning's REAL attachment spelling (corpus sample is SYNTHETIC; the mock writes attachment/context_tip, which is probably not the carrier — needs a ruling from a real capture).
- A real compaction-summary line (the isCompactSummary shape is synthesized in one helper; every compaction subject proves a believed shape).
- The api_error taxonomy fixtures (see grounding limit above).

## Cross-team facts recorded for integration
- Pins: connectrpc.com/connect v1.17.0 + golang.org/x/net v0.43.0, go 1.23.x everywhere.
- Private test socket: env AGENT_REPL_STORE_SOCKET; a flag beats it (store --socket, sidecar --store-socket).
- Connect standing streams: consumers must CANCEL the stream context (Close alone drains a standing stream forever); acceptance is silent, refusal is CodeNotFound on first Receive; servers flush response headers on accepting a stream.
- The mock's !cancel-all writes EXIT= into agent spools (shim-side bug, relayed); !compact-failed writes nothing to disk (ContextCut.compaction_failed has no file-plane producer); !subagent-failed's transcript holds only a user record, withheld by R15.
- A db-refused batch yields two log records (db error-context at verbose + server warn with rpc) — accepted this wave; the single-record end state threads a request-scoped logger into db.

## Correlation keys (logging contract vocabulary for these two services)
producer, agent_id, vendor_session_id, book_agent_id, write_id, upsert_key, position, write_seq, watch_token_hash, rpc, refusal_site, statement, file_id, path, offset, task_id, activity_id, turn_id, reason, attempt, backoff_ms; top-level request_id via the X-Agent-Repl-Request-Id header.

# STOPPING POINT — shim (pause 2026-09-02, recreated fable-low shim lead)

Supersedes the 2026-08-31 STOP file. Everything below is the state at the
final tip named in "Where things stand"; the LIVE LEDGER in
docs/overhaul/shim-fanout.md carries the per-dispatch history.

## Where things stand

- Branch `overhaul/shim`, worktree `~/.config/doom-overhaul/shim`. FINAL
  CODE TIP `032a9f0ab` (the dead-code pass); this file's commit is the tip.
- Verified at the tip: `npm run typecheck` clean; `npm test` 3806 tests /
  98 files green; `npm run build` green; `npm run smoke` green (fake store + a
  no-store exit-1 step); `npm run test:integration` GREEN — 302 passed /
  0 failed / 3 todo in ~30 s (was 99 failed / 90 passed / 4 todo in ~30 min
  at the 2026-08-31 pause). Every test environment exports
  `AGENT_REPL_FORBID_VENDOR_CALLS=1`; the vendor was never called by any
  test; no real git in any test.
- No shim agent worktrees or branches remain (shim-agents/ empty).
- Landing 6 merged (c9280627f); no shim.v1 / conversation.v1 change.

## What landed since the 2026-08-31 pause (merge order)

1. Capture-harness fixes (48ed642eb and below): `on_control` trigger kind;
   sweep-end late reclaim; multi-turn drain held across turns
   (`drainToResult`); transport-closed control_failed quarantines;
   `permission-undecidable-parked` expects_error_subtypes; denied-by-user
   prompt rewrite. The project lead re-captured 13 scenarios; 69 goldens.
2. Goldens (eb7930399): the 69 real captures under
   agent-shim/claude/shim/testdata/captures/ (+MANIFEST with an
   evidence-gaps section) and the converter golden suites
   test/convert/goldens/*; two converter defects fixed (task kind only from
   task_started; per-block assistant line BEFORE content_block_stop).
3. Remediation #2 engine (062aaeda5) and fakes (1768697c7): the 12 failure
   buckets closed; R15 applied; store relays; rotation = post-clear init id;
   Persistence.onFault/onDegradedWindow subscribed; typed store failure
   arms; SHIM_BUILD_SHA from the spawn env; reader-side subagent join.
4. Mock rebuild from the goldens (3dc64a89b): golden-conformance suite (59
   mapped rows; 27 shape-exact, 32 pinned with reason); AGENTS.md marks
   capture-grounded vs declared-only rows; denied tool → failure(no
   content); observed /clear shape; detached causes; SourceCoordinates
   unified; StartTurn reads its page BEFORE the submit.
5. Remediation #3 (5721237ec): integration green; routes.ts boundary maps
   every handler; stoppedBashTerminal; interim unknown-target refusal; held
   cuts; teardown snapshot; knowsAgent.
6. Remediation #4 = audit #2 (faef4c312, e0addd384, 3a60a11b5): 61 audit
   items; standing grants validated against the OFFER; UpdateAgent subagent
   target by wire id; non-zero bash exit = completed; workspace lock inside
   StartSession; signal handlers before "serving"; failed StartSession undoes
   itself; response compression off (early-head ruling); h1 multi-stream.
7. Dead-code pass (sonnet-medium, 032a9f0ab): knip 0 unused files/exports/
   dependencies in src (only the 22 sdk/types.ts vendor-canary aliases
   remain flagged, intentional); tsc noUnusedLocals/Parameters 1 finding
   (`_QueryStillSatisfiesQueryLike`, compile-time vendor-drift assertion);
   zero-hit src functions 73 → 11, all live and named below; knip.json
   added (config only).
   DELETED: src/subscription-usage.ts (+test); convert/permission.ts
   crossCheckDenials; store/reader.ts READER_COMPONENT; writer.ts dead
   bashUpsertKey re-export; terminals.ts `empty`; fake/index.ts barrel
   re-exports (FAKE_DEFAULT_MODEL, SCENARIOS, selectScenario, Scenario,
   ScenarioContext) and the four unused ScenarioContext accessors
   (sessionUuid, gate, interrupted, liveTasks); TurnEngine.turnLive() and
   .workId() (zero callers); unused imports (PersistEntry, ToolOutcome x2,
   bashUpsertKey, StoreClient, SdkUserMessage, readJsonl); ~245 needless
   `export` keywords across src. Ruled-dead surfaces verified absent:
   src/uds/ (framing.ts both halves), src/protocol.ts, src/session.ts,
   runUdsMode, legacy flags, the three-surface AGENTS.md story.
   KEPT WITH REASON (each pinned by a unit test unless noted): sdk/types.ts
   22 Sdk* type aliases (compile-time upgrade canary; verified by tsc, no
   test possible); sdk/real-query.ts createRealQuery (vendor chokepoint,
   throws under FORBID; exercised by the dist smoke) and main.ts main()
   (process entrypoint; dist smoke); store/persistence.ts
   unavailablePersistence; the engine dispatch arrows; FAILURE_ARMS;
   InvalidModeledUsageError; REAL_SCHEDULER; PermissionGate
   noteVendorDenial/deniedCall; pushes return() path + default clock;
   reader transportFailure/concludeThrough/awaitFirstRow;
   stoppedBashTerminal; writer clearProducer/liveWork/default sleep; main
   logCorrelation/queryFactory; server listen bind-failure; AsyncQueue
   .return; vendor-files vendorSessionId/survivingShellRuns; scenarios
   query-eof-mid-ask, perm-allow-standing-mode, perm-no-standing,
   perm-hold, bash-hold, subagent-detached-live; 19 engine facade functions
   pinned through test/engine/*. Pinned by the INTEGRATION suite only
   (out-of-process; named tests): session.ts bookHead, withBudget,
   watcherOpened, knowsAgent, bashWatcherOpened (session.test "a forced
   kill concludes the detached stream with an interrupted arm FIRST");
   pendingAsk, deniedCall (gate.test deny/undecidable cases); compact,
   writeContextCut (session.test Hibernate + remediation.compact);
   liveTask (detached/session subagent suites).

## Rulings received this wave (all recorded in shim.md / the ledger)

- R15 wins: StartTurn's page holds exactly the prompt row.
- Store relays: WatchBashRun CodeNotFound before the first row (tolerated);
  post-terminal deltas served then end (fake store still to model it —
  resume queue).
- Denied tool: starts never deferred; the unit settles `failure` with
  content UNSET; drawn denied via the permission unit's shared id; no proto.
- WatchAgent serves ONE book; fan-wide cancel = KillTurn/KillSession{force}.
- Interim unknown-target refusal at the shim; `unknown_agent` on
  OpenAgentSessionFailure proposed for landing 7.
- Workspace lock taken inside StartSession; inert shim holds neither lock.
- /clear rotates to the post-clear system:init.session_id.
- Compaction helper stays synthetic (no longer-history capture approved).
- api_error page line is the sidecar's (no stream-plane producer).
- Busy-subagent UpdateAgent refusal is the ONE producer (daemon's
  turn_already_open retired).

## Capture-checklist answers (read from the goldens, not guessed)

- Budget warning: NO context_tip and NO budget-warning attachment in any
  capture; nearest carrier observed once: attachment `total_tokens_reminder`.
  The spelling is an OPEN EVIDENCE GAP; the sidecar producer stays ungrounded.
- Failed subagent: none in any capture.
- Compaction summary line: none; /compact answered "Not enough messages to
  compact" under the cheap model. Gap open pending a longer-history capture.
- /clear rotation record: observed — see shim.md "ROTATION, OBSERVED".
- Declared-only (not capture-grounded) mock rows are listed in AGENTS.md;
  the MANIFEST evidence-gaps section lists: glob, grep, artifact,
  schedule_wakeup, worktree, context_injected, failed subagent,
  compact_boundary, `!fail-execution`/`!fail-stop-hook`/
  `!fail-structured-output` failure terminals, api-error classes, refusals,
  query death, cold resume.

## Resume queue (in order)

1. Fake store models relay (b) — post-terminal deltas served then end — and
   the detached test re-asserts against it.
2. Retry-buffer overflow: survivors'-order half of the test (needs a drain
   hook or an observable ledger; deferred this wave).
3. `KillSession.query_refused_to_end` and `StartTurn.vendor_refused` stay
   todos (no producer; fatalizing a refused vendor interrupt is a contract
   change) — decide or leave.
4. Landing 7 candidates for the project lead: store.v1
   `OpenAgentSessionFailure.unknown_agent`; a `denied` marker only if the
   daemon lead finds the permission-id join awkward.
5. When a longer-history capture is approved: land compaction-directed's
   real compact_boundary, rebuild `!compact*` and the compaction helper from
   it, retire the "synthetic" label.
6. Next fresh-context audit (#3) only if the lead wants another loop; #2's
   61 items are all closed or dispositioned in
   docs/overhaul/reports/shim-audit-2.md.

## Where everything is written down

- docs/overhaul/shim-fanout.md — architecture + LIVE LEDGER (every
  dispatch, merge, ruling, override).
- docs/overhaul/shim.md — contract context; mock section; R9 + ROTATION
  OBSERVED; the gate ruling; capture-run negatives.
- docs/overhaul/reports/shim-audit-1.md, shim-audit-2.md — the audits,
  triaged.
- agent-shim/claude/shim/AGENTS.md — module map, process shell, the
  prompt→scenario table with capture-grounded marks.
- agent-shim/claude/shim/testdata/captures/MANIFEST.md — the 69 goldens
  and the evidence gaps.
- The project lead is reached by SendMessage to `main`.

# Integration test tier audit (Option B, integration half)

Workspace `test-tier-audit-integration-nzx`, child of `agent-repl-test-slimming`, 2026-10-14.

This is an investigation. It deletes, moves and merges no tests. Every verdict below is
SEMANTICALLY JUDGED AND UNCONFIRMED: no mutation run backs it yet. When
test-redundancy-measure's tool lands on master, each removal must be checked with it,
showing that the remaining tests kill every mutant the removed test kills.

## Question

Rule each integration test as one of:

- `KEEP`: it needs the integration tier, and the table names the boundary it needs.
- `MOVE-TO-UNIT`: a unit test of the same system can prove it.
  - `(covered)` means a named unit test already asserts the same thing, so the move is a pure removal.
  - `(new)` means a unit test must be written first.

Also flag tests that run a real peer system. The 2026-10-14 rule "A system's integration
tests run that system alone" forbids that. For such a test, `SWAP-TO-FAKE` (swap the peer
for its existing fake) is preferred over `MOVE-TO-E2E`. Finally, flag `E2E-DUP` where an
e2e test already proves the same behavior.

## Scope

| Suite | Cases audited | Table |
|---|---|---|
| daemon `integration` (`-tags integration`) | 702 Test funcs | `daemon-d1.tsv` … `daemon-d5.tsv` |
| store `integration` | 193 Test funcs | `store.tsv` |
| sidecar `integration` | 219 Test funcs (132 mock-scenario rows) | `sidecar.tsv` |
| shim `test/integration` | 411 cases | `shim-webapp.tsv` |
| webapp `test/integration` | 2,144 cases | `shim-webapp.tsv` |

- The tables sit in the directory next to this file and have one row per test, or per
  `describe` block for Vitest.
- Out of scope: `daemon/integration/{harness,fakegit,fakeshim}`. These are untagged unit
  tests of the test fixtures themselves, about 22 history-s in total.

## Cost basis

The seconds in test history (`~/.cache/agent-repl/test-history*.json`) are wall time under
the load of a full run. They are not cost. test-shared-setup measured daemon integration
tests at a median of 0.17-0.19s solo, against about 2.9s in history.

So this report gives cost first as PROCESS BOOTS REMOVED, then as CPU-seconds at a
measured per-boot rate. History seconds appear only as a secondary column.

One targeted run per system, `-test.parallel 1`, `/usr/bin/time -p`, at a host load
average of 16-37 (the host was not idle):

- **daemon:** 15 sample tests (median-time picks per verdict) performed 32 daemon boots for
  20.2 CPU-s (user 11.9, sys 8.3).
  - That is at most 0.63 CPU-s per boot. This is an upper bound, because it includes the
    harness's one-time setup.
- **sidecar:** `TestMockScenariosShard0` (17 rows) plus 5 non-mock tests used 6.8 CPU-s.
  - Solo, the mock rows took 0.25-0.44s of wall time. History shows about 3.4s per row.
  - The first row of the shard took 13.2s. This is a one-time warmup, handed to
    test-shared-setup (`sidecar-solo-sample.log`).
- **store:** not measured. The whole package is 26.8 history-s.

## Results

### Totals per suite

| Suite | KEEP | MOVE-TO-UNIT (covered) | MOVE-TO-UNIT (new) | PARTIAL | SWAP-TO-FAKE | MOVE-TO-E2E |
|---|---|---|---|---|---|---|
| daemon (tests / boots) | 234 / 314 | 429 / 472 | 34 / 46 | 5 / 30 (about 20 boots movable) | 0 | 0 |
| store (tests) | 34 | 120 + 6 duplicated within the suite | 29 | 4 | 0 | 0 |
| sidecar (tests) | 45 | 132 | 34 | 8 (the mock shards) | the shards' real shim | 0 |
| shim (cases) | 54 | 310 | 45 | n/a | 2, plus a suite-wide lock-helper swap | 0 |
| webapp (cases) | 451 | 1,571 | 122 | n/a | 0 | 0 |

Across all five suites, about 3,600 integration cases were audited. About 2,600 of them are
`MOVE-TO-UNIT(covered)` or duplicates within their suite. No test needs to move up to e2e.

### Savings estimate

The figures assume every proposal is confirmed by mutation.

| Suite | Boots removed | CPU-s (measured rate) | History-s (secondary) |
|---|---|---|---|
| daemon | about 540 of 862 (63%) | about 340 at most | about 1,400 of 2,270 |
| sidecar | about 310 sidecar and node-shim boots | about 90 at most (0.3 CPU-s per row, measured) | about 510 of 521 |
| shim | 2 lock-helper spawns per remaining case, plus the cases removed (about 355 node spawns) | not measured | about 136 of 154 |
| store | about 190 store boots | small (cheap boots) | about 20 of 26.8 |
| webapp | 0 (no spawns; jsdom plus an in-process fake daemon) | small | about 97 of 125 |

The daemon suite is where the cost is. Its boots are the dominant per-case cost, and
about 63% of them sit in tests whose assertion a unit test already makes.

## Patterns behind the verdicts

1. **The test boots a process to check one row, one refusal or one projection.** The
   cheaper tier already pins that output by name.
   - daemon: 67 of 84 tests in `feed_test.go` re-check `internal/resolve/feed`, which has
     about 1,190 unit tests.
   - daemon: 41 of 52 tests in `footer_topbar_test.go` re-check `resolve/footer` and
     `resolve/topbar`.
   - daemon: about 40 typed refusals reached at the first call are pinned in `workspace/`,
     `server/` and `merge/` (write-tests pitfall 9).
   - daemon: 19 of 23 commandfile tests restate `commandfile/ingress_test.go`.
   - sidecar: about 75% of the non-mock tests check converter, handler or cycle logic that
     `main.newHarness` (`support_test.go:292`) and `internal/handler/golden_test.go`
     already run in-process.
   - webapp: `it.each` over every schema arm, sent through the wire, while the unit suite
     already loops over the same arms.
2. **One wiring test per family is enough.** Every MOVE row names, in its notes, the KEEP
   test or e2e test that still proves the process reaches the moved logic ("wired by").
   Removing that KEEP test would leave the rows that cite it unproven.
3. **Restart variants that prove only a database round trip.** Store reads are plain SQL,
   and daemon `wsm` reopen tests already pin persistence. A restart test is kept only
   where boot wiring has no unit test.
4. **The sidecar mock scenarios start a real peer.** Every one of the 132 rows starts the
   real node shim in `--fake` mode, then the real sidecar.
   - About 41 rows give the sidecar file-identical input and can be removed outright.
   - The rest are `SWAP-TO-FAKE`: check the fake vendor's generated trees in as static
     goldens, with the shim's suite asserting the goldens are fresh.
   - Then they become `MOVE-TO-UNIT(new)`, driven through `internal/handler` and
     `main.newHarness`.
   - The row-level drop list is test-cross-product-audit's 45 rows with fingerprinted
     on-disk shapes. It supersedes this audit's estimate of about 41 rows, and the two lists
     diff cleanly.
   - The `!perm-*` and `!ask-*` rows write different files depending on how the gate is
     answered. Generate their goldens from the integration drive's answers, never from a
     default allow.
5. **The shim suite starts a real peer in every case.** Each `StartSession` spawns the
   real `shim-lock` helper twice.
   - Fix: point `AGENT_REPL_SHIM_LOCK_BIN` at the stand-in holder that
     `test/locks.test.ts` already installs, for the whole suite.
   - Only 3 lock-contention cases in `process.test.ts` need the real helper.
6. **Tests of the harness or a fake, not the system.** Examples:
   - the paging of the shim's fake store;
   - the webapp `client-log` wiring, which the harness copies from `main.ts`;
   - `TestAFeedTextScaleFrameIsNotARowOnTheFeedTail`;
   - `TestAnUnknownRunIsARefusedOpen` in the sidecar, which never starts the sidecar.

### What the integration tier genuinely holds (KEEP)

- Successor daemons and handover.
- Process deaths by SIGKILL and SIGTERM, and stray reaping.
- State a successor process must read.
- The fakeshim protocol: fork point, cold resume, history paging.
- Real spawns: vendor guard, spawn failure, chime, banner, CLI.
- Flags, env and exit status.
- Log file shape.
- Store and sidecar socket reclaim, redial and bounce.
- Real kernel lock contention.
- The HTTP/1.1 and h2c transports.
- Page-mux sharing.
- One wiring test per family.

## Unit tests that must exist before a removal

The `MOVE-TO-UNIT(new)` rows list every one. These gaps were found while checking
"covered" claims:

- **daemon:**
  - `merge.orchestrator.pauseScope` (`merge/queue.go:92`).
  - The `setPaused` no-op refusals (`merge/queue.go:47`).
  - The interjection's unforced `KillTurn`. `promptqueue/helpers_test.go:681`
    `killedForces()` exists but nothing calls it.
  - `server.validateRegisterRepositoryRequest` (`validate.go:500`).
  - The sessionwatcher `bash_terminal` retirement.
  - `NotificationAgentAddressed` (`sessionwatcher/route.go:1080`).
  - A table of the transport-closed refusals across the 5 per-workspace Watch handlers.
  - `server.holdParticipant` (`streams.go:723`).
  - `ejectCold` (`cmd/agent-repld/graph.go:1056`).
- **sidecar:**
  - `cycle.go:996`, a held subagent transcript.
  - `seam.go`'s workflow-journal dispatch.
  - Task-update dispatch.
  - The cursor at SIGTERM with an unacknowledged batch.
  - `file_id` on residue-drop records.
  - The bash tail after truncation.
  - Keep-alive beside a compaction.
- **store:**
  - Exact "one log record" counts on server-side refusals. The server tests use
    `findRecord` and never count.

## E2E-DUP

About 114 daemon rows and about 20 sidecar rows name an e2e twin. Most of these tests are
already MOVE verdicts, so the e2e twin only strengthens the case for removal.

The full duplicates of KEEP tests are:

- `TestAHeadlessWorkspaceTransfersWithoutAnyAdoptCall`
  - Its twins are `TestHandoverTransfersAtFreeness` and
    `TestADeployThatFindsTheDaemonStaleHandsOverAFreeWorkspace`.
- `TestARelaunchedDaemonRehydratesAnAnnouncedWorkspacesConversation`
  - Its twin is `TestARelaunchedDaemonKeepsTheConversationItStoodDown`.
- `TestRegisterWorkspaceAfterARestartIsIdempotent`
  - Its twin is `TestEmacsReRegisterIsIdempotentByDir`.
- `TestACompletedTurnRaisesTheCompletedBanner`
  - Its twin is `TestEmacsUnfocusedTurnEndRaisesTheDaemonsBanner`.

Removing a KEEP test in favor of its e2e twin is allowed only if mutation shows the e2e
test kills the same mutants. Many e2e twins check less: for example, the footer but not
the roster.

## Defects and oddities found along the way (not fixed here)

- `TestBootRefusesACorruptCreationJobOfAnAdmittedMerge` is documented as expected-red.
  - The defect is in `internal/merge/recover.go`.
- `TestInterruptTurnWithATransportFailureAnswersShimRefused` has a stale "EXPECTED RED"
  comment.
  - `workspace.TestInterruptTurnAnswersShimRefusedForATransportFailure` now pins that
    behavior.
- `TestAKilledDaemonLeavesNoProcessNamingItsStateDir` polls `StrayPIDs` in a loop with no
  ticker.
  - It can burn up to 5 CPU-s.
- Sidecar `unparsedOf(entries)` and `requireNoResidueStored` on stored entries can never
  fail, because residue is never stored.
- Sidecar `bash_readback_test.go` claims to use the real store but calls `startFakeStore`.
- The webapp refusals site table lists `SetModel`, `SetPermissionMode` and
  `RestartWorkspace` twice each.
- Some history entries are stale, for example the unsharded sidecar `TestMockScenarios`
  and webapp `tray.integration.test.ts`.

## AGENTS.md rule (DRAFT)

The wording was agreed with test-tier-audit-e2e and lands as ONE combined section,
"A test lives at the cheapest tier that can prove it".

- It is commit `d06bf0f99e` on branch `test-tier-audit-e2e-edj`.
- It sits in `modules/app/agent-repl/AGENTS.md`, right after "A system's integration tests
  run that system alone".
- It has three parts:
  - the general principle;
  - "An integration test proves what needs the system's own process", written from this
    audit;
  - "An e2e test proves a seam between real systems".
- It is held on that one branch so the two drafts cannot conflict. It is pending the
  owner's ruling.
- A clause making tests with the same arrange share one boot was dropped.
  - It collides with the one-edge-case-per-test rule and write-tests' refusal of shared
    mutable state.
  - That question is test-shared-setup's to raise with the owner.

## Method and caveats

- Eight read-only auditors (opus-medium) each read their files, the matching unit
  packages and `e2e/`. They wrote one row per test into the tables here.
- Most "covered" claims rest on a named unit test plus a reading of its body. Some rest on
  the test's name alone, as the notes say. Spot-check the named unit test before any
  removal; the mutation tool settles it.
- Where an auditor was unsure, it chose KEEP and said why in the notes.
- `daemon_starts` and `process_boots` are static counts, with subtests multiplied.
- Shim and webapp seconds are file totals split across cases by estimate, scaled to each
  file's measured total.

# Shared expensive setup (Option C): which tests can pay for their setup once

Workspace `test-shared-setup-lzb`, child of `agent-repl-test-slimming`, 2026-10-14.

This is an INVESTIGATION. No test was deleted, moved or merged. Every proposal below is
**semantically judged and unconfirmed**. It becomes confirmed only once test-redundancy-measure's
mutation tool shows that the proposed shape kills every mutant the current tests kill. That tool was
not on master when this report was written.

## The answer in brief

- **Shared setup is expensive in exactly one place: the e2e `World`.**
  - In steady state an e2e test pays about 3.4s solo, and about 2.7s of that is building and tearing down its `World`.
  - A fresh workspace inside an already-running `World` costs 0.30-0.41s, its first turn included.
- **Everywhere else, setup is cheap.** Solo measurements:

  | fixture | solo cost |
  |---|---|
  | daemon integration `newOpened` | 0.14s wall, ~0.17 CPU-s |
  | daemon merge fixtures | 0.7-1.5s |
  | one sidecar mock-scenario row | 0.3s |
  | whole shim integration `turn.test.ts` (119 cases) | 2.7s |
  | webapp `feed-kinds` + `refusals` integration (782 cases) | 30s |

- **The history file's per-test seconds mostly measure the full run's load, not cost.**
  - Daemon integration tests that take 2.7-3.0s in history take 0.17-0.73s solo, about 16x less.
  - Shim `turn.test.ts` takes 102s in history and 2.7s solo, about 37x less.
  - Every saving below is therefore stated in solo seconds and in CPU-seconds, never in history seconds.
- **The recommended shape is one `World` per cluster, with a fresh workspace per case.**
  - Each case keeps its own session, shim, feed and per-workspace logs.
  - Sharing the session as well would save only about 0.05s more per case and would give up that isolation.
- **The headline is clusters K1 and K1b, on top of test-tier-audit-e2e's KEEP set:** 17 default-world KEEP rows can share one `World`.
  - That saves about 43s solo wall and at least about 5 CPU-s per run.
  - Contingent only: the scenario matrix was ruled (a) on 2026-10-14, so the MOVE rows of clusters C-M1 and C-M2 mostly leave e2e. They are kept below in case some stay.
- **This needs one rule change,** drafted in the "Proposed rule change" section, plus one harness change: warning declarations scoped to a workspace.

## Method

- **Timing history.** `~/.cache/agent-repl/test-history.ramdisk-io-default.json` was used only to rank tests.
  - Its `item:` values are each test's wall time inside a full run.
- **Solo costs.** Throwaway `zz_probe_scratch_test.go` files, never committed and deleted after each run.
  - Each was run once, through `bin/background.sh`, at targeted granularity.
  - They timed each setup phase with `time.Now()` and with `getrusage` (`RUSAGE_SELF` + `RUSAGE_CHILDREN`).
- **CPU figures are lower bounds.** `RUSAGE_CHILDREN` sees only reaped descendants.
  - A shim the daemon leaves to exit after the daemon itself has exited is reparented, so its CPU is not counted.
  - test-tier-audit-e2e apportions the e2e suite's measured 430 CPU-s at about 1.4 CPU-s per test. A `World` takes about 80% of a test's solo wall, which would put the upper bound near 1.1 CPU-s per `World`.
- **Clustering.** Each e2e Test func was read for:
  - its `WorldOpts`;
  - its destructive acts (stop, kill, restart, deploy, relaunch, store restart);
  - the views it watches across the whole daemon (roster, sidebar, daemon stream, account usage);
  - its warning declarations.
- **Reconciliation.** Rows were then joined with test-tier-audit-e2e's rulings, from its report at commit `48ed55c35a`.

## Measured setup costs

### e2e (`e2e/`, host suite)

Each case drives a `fail-max-turns` turn. Solo, steady state, after a warm-up `World`:

| shape | wall | CPU (lower bound) |
|---|---|---|
| 3 separate worlds, one turn each | 7.80s | 1.33 CPU-s |
| 1 world, 3 fresh workspaces, one turn each | 2.36s | 0.72 CPU-s |
| 1 world, 1 workspace, 3 turns | 2.43s | 0.57 CPU-s |

- Phases of one steady-state test:
  - `NewWorld`: 0.6s;
  - register: 0.26-0.39s;
  - first turn, which spawns the node shim: 1.9s;
  - each later turn: 0.07-0.10s;
  - teardown: about 0.5s.
- **Each `World` a case stops building saves about 2.7s solo wall and at least 0.31 CPU-s.**
- An errored `fail-max-turns` turn leaves the session usable, because the next scenario ran normally.
- The first `World` in a process costs about 13s, for one-time builds. Under testrun this is the prebuilt step that `sharePrebuilt` shares across chunks.

### daemon integration (`daemon/integration/`)

| fixture | solo wall | CPU |
|---|---|---|
| `newOpened` (daemon, repo, register, open, fake shim, host and web streams, teardown) | 0.14s | about 0.17 CPU-s |
| `mergeCleanRepo` | 0.88s | 0.38 CPU-s |
| `newSourcedRepo` | 0.71s | 0.32 CPU-s |
| `mergeBlockedQueueFixture` | 1.53s | 0.88 CPU-s |

Ten of the costliest tests and eight random tests with a median history time, run solo:

- **Median tests:** 0.17-0.73s solo against 2.75-2.99s in history. This is contention only.
- **Table tests:** solo subtests take 0.1-0.5s each. For example, `TestSubscriptionInvariantAcrossWatchKinds` has 4 subtests totalling 0.72s.
- **Production-bound waits, a separate lever already routed to the owner:**
  - `TestSecondDaemonOnTheSameStateRootRefusesToBoot`: 12.8s solo.
    - It waits the whole `daemonaddr.ClaimWaitBound` (12.32s).
    - `harness.ClaimRefusalTimeout` is defined at `~/.config/doom-worktrees/test-shared-setup-lzb/modules/app/agent-repl/daemon/integration/harness/daemon.go:122`.
  - One subtest of `TestAColdStartedDaemonHandsOverTheSessionsItBroughtUp`: 15.8s solo.
- **No fixed poll interval dominated any sampled test.**

### sidecar integration (`agent-shim/claude/shim-sidecar/integration/`)

- **Cost is concentrated in one table.** `TestMockScenariosShard0`..`7` take 482s of the package's 543 history seconds: 133 rows, each with its own node mocked vendor, fake store, proxy and sidecar.
- **A row costs 0.28-0.34s solo** and 0.22-0.25 CPU-s, of which:
  - the node generator is about 0.19-0.21 CPU-s;
  - ingest is about 0.01 CPU-s.
- **The first row in a test process costs 13-20s.**
  - `buildMockShim` runs under `mockShimOnce`: `npm run ensure-deps`, the esbuild bundle and the fake-store build.
  - That cost is paid once per testrun chunk process that runs a shard, and history books it to that chunk's first row.
  - `overhead:sidecar:integration` is recorded as 0, so the planner cannot see the cost.
  - Side finding: the e2e package's `sharePrebuilt` (`~/.config/doom-worktrees/test-shared-setup-lzb/modules/app/agent-repl/testrun/internal/suites/gopkg.go`) is the existing fix shape.
    - It builds once in the build unit and points every chunk at the result.
    - It loses no coverage.
    - Contingent: the shards are slated to become unit-level golden checks (test-tier-audit-integration), so this matters only if some survive.

### Suites with no expensive shared setup

- **shim integration** (`agent-shim/claude/shim/test/integration/turn.test.ts`): 119 cases in 2.7s solo, median 23ms per case. Its history figure of 102s is load.
- **webapp integration:** `feed-kinds` has 418 cases in 14.5s solo, and `refusals` has 364 cases in 15.1s. Each case costs 35-40ms. There is no cluster worth its loss of isolation.
- **ERT:** the integration files already share ONE fake daemon per suite run, through the AGENTS.md section "ONE fake daemon serves a whole suite run". The unit files have no expensive fixture.
- **e2e-emacs:** 66 `TestEmacs*` funcs.
  - Each scenario costs about 2.6 slot-seconds (78s on 2 slots, `~/.config/doom-worktrees/test-shared-setup-lzb/modules/app/agent-repl/e2e/EMACS-LAYER-SPEC.md`).
  - None of the 6 KEEP rows share an act, because the launcher-argv test uses `coldStartWorld` and proof of life uses `NewEmacsWorld`.
  - See conditional cluster C-E1 below.

## Proposed clusters

Common to every e2e cluster:

- **Shape:** one `World` per cluster, one fresh workspace per case, and each case is its own `t.Run`.
  - Cases run in parallel inside the cluster, because many workspaces in one daemon is production's own shape.
  - A case may use `t.Fatalf`, which ends only its own subtest.
- **Diagnosis stays precise.**
  - A red run names the case's subtest.
  - The case's own per-workspace logs (`WorkspaceLog(dir, "daemon"|"shim")`) hold only its records.
  - The failure-artifact dump runs per subtest.
- **What an early failure hides:** nothing, because no case starts from another's end state.
  - Only the death of the `World` itself fails every case in the cluster, and that is reported once, as the `World`'s own failure.

### Cluster K1: KEEP wiring rows on a default `World`, unconditional

- **Members (14):**
  - `TestTurnStartToCompletion`
  - `TestQueryEofEndsTheTurnAsQueryDied`. A query death ends that case's own session only.
  - `TestResidueAttachmentsDrawNoRow`
  - `TestBashForegroundCompleted`
  - `TestSubagentSyncNestedActivity`
  - `TestSubagentDetached`
  - `TestSubagentDetachedSurvivesAnInterjection`
  - `TestPermissionAskAnsweredArms/AllowOnce`
  - `TestSkillInvocation`
  - `TestCompactionDirected`
  - `TestInterruptAfterTextDelta`
  - `TestClearRotatesIdentity`
  - `TestPlanModeCoalescesOntoOneBubble`, with `TestPlanModeSettlesPlanned` folded in as the audit proposes
  - `TestFanWideCancel`. Its interrupt and footer are scoped to its own workspace.
- **Shared setup:** `NewWorld(t, WorldOpts{})`, i.e. the real store, sidecar and daemon.
- **Saving:** 13 worlds removed, about 35s solo wall and at least 4.0 CPU-s per run.
- **To check when the proposal is built:** `TestQueryEofEndsTheTurnAsQueryDied` declares no warning in its body. If the `World`'s sweep relies on a declaration that some helper makes for the whole daemon, that declaration moves to per-workspace scope first.

### Cluster K1b: KEEP rows that declare warnings, conditional on per-workspace warning scope

- **Members (3):**
  - `TestConverterRecoveryClosesTheDegradedWindow`, which declares `daemon.health.open_fault`;
  - `TestBubbleRefusedNotDeliverable`, which declares `daemon.promptqueue.deliver`;
  - `TestApiRateLimitedCarriesTheVendorWait`, which declares `faApiErrorWarnings`.
- **Why a condition:** `harness.Daemon.ExpectWarnings` (`~/.config/doom-worktrees/test-shared-setup-lzb/modules/app/agent-repl/daemon/integration/harness/logs.go:387`) widens the sweep for the WHOLE daemon.
  - In a shared `World`, one case's declared warning would license the same warning from every other case. That is a coverage loss.
  - They join K1 only once the harness has `ExpectWarningsFor(ws, ...)`, which matches the operation AND the record's workspace.
  - Ruling, 2026-10-14: `ExpectWarningsFor(ws, ...)` is approved in principle, as a structural improvement on its own.
  - A record that carries no workspace keeps the case in its own `World`.
- **Saving:** 3 more worlds removed, about 8s solo wall and at least 0.9 CPU-s.

### KEEP rows that keep their own `World`

- **They read state shared across the daemon or the account:**
  - `TestAccountUsage`;
  - `TestRateLimitSevenDayWindowFeedsTheWeeklyAllowance`.
  - Allowance and usage belong to the account, so another case's session could move the figure they assert.
- **They need different world options:**
  - `TestDetachedLostWentSilent` (sidecar staleness);
  - `TestBashDetachedStartAndComplete`, `TestStartSessionVendorStartFailedRecovers` and `TestKeepAliveStoresNothingOnEitherPlane` (`ExtraEnv`);
  - `TestRevivalAfterHibernate` (`Timeout`).
  - Two rows may share only when their options are identical. No two of these are.
- **They change the fixture itself:**
  - `TestColdBootReadsReplayFromStore`;
  - `TestColdGate/Pay` and `TestColdGate/Clear`;
  - the three `daemonstop` rows;
  - `TestRecoveryAfterStoreRestart`;
  - both `deploy` rows;
  - `TestDisplacedTurnCapturedEndedThenResubmittedExactlyOnce`;
  - `TestARelaunchedDaemonKeepsTheConversationItStoodDown`;
  - `TestWebappLayerClientLog` and `TestWebappLayerRestartHandover`;
  - the 6 Emacs rows;
  - `TestPerfWebappLayer`, which is perf-tagged and outside the suite.
- **Already counted by the tier audit, so not counted here:**
  - `TestStopReplayedByEveryRewindNeverLoops`, six identical worlds cut to one with read-only `t.Run`s. The parent ruled this through test-cross-product-audit, and the rule below is its home.
  - The stop-after-turn and no-watch-token fold.

### Contingent cluster C-M: MOVE rows, kept only for the rows that end up staying in e2e

- **The condition:** test-tier-audit-e2e's question 1, which was ruled (a) on 2026-10-14. These rows therefore mostly leave e2e for their lower tier, with no `World` at all. The cluster applies only to rows that stay.
- **C-M1, MOVE rows on a default `World`, unconditional (40 funcs, 49 worlds):**
  - `TestTerminalReasonTurnFailedArms` builds 10 worlds alone.
  - The rows, grouped by file:
    - context usage;
    - two compaction rows;
    - three `detachedbash` rows;
    - three `failurearms` rows;
    - `filetools`, `footeractivity` and two hooks rows;
    - the second rotation;
    - the bash timeout;
    - two keep-alive rewind rows;
    - two monitor rows;
    - two permission rows;
    - three `producerfaults` rows;
    - eight `remainder` rows;
    - two `replay` rows;
    - the MCP catalog;
    - three subagent rows;
    - max tokens;
    - the web fetch redirect.
  - The `replay` rows hold up because each case opens its own fresh workspace, and "first page once" is about that workspace.
  - Saving: 48 worlds removed, about 130s solo wall and at least 15 CPU-s.
- **C-M2, MOVE rows that declare warnings (8):**
  - `TestCompactionFailed`;
  - `TestBashDetachExplicitPoll`;
  - `TestBashDetachedLiveNeverSettles`;
  - `TestApiUnmodeledKeepsTheVendorClassByName`;
  - `TestHookFailed`;
  - `TestFailMarkerFailsABeforeActionRunAndRidesAnAfterActionTerminal`;
  - `TestBubbleRefusedAgentBusy`;
  - `TestUnknownAgentOnUpdateAgent`.
  - Same condition as K1b. Saving: about 22s solo wall and at least 2.5 CPU-s.
- **C-W, webapp-layer MOVE areas that stay (7):**
  - The areas already serialize through one shared vitest server.
  - Each area still builds its own world, about 2.7s by the spec's own figure.
  - Merging the six default-option areas into one world, with one workspace per area, saves about 13.5s solo wall.
    - This is the tier audit's question 3, and it contradicts `WEBAPP-LAYER-SPEC.md`'s "one world per area" ruling. The owner rules on that.
  - `TestWebappLayerMergeTabs` keeps its own world, because it needs a repository and a child workspace.
- **Excluded from C-M, and keeping their own world:**
  - They change the fixture or need different options:
    - `TestColdBringUpEarnsNoStoreRefusal` and `TestColdBringUpServesTheMainBookWithoutAsking` (bring-up is their subject);
    - `TestColdGate/Compact`;
    - `TestAWorkspaceWhoseMergeFailedTakesTheNextPromptAsAnOrdinaryTurn` (merge world);
    - the four hibernation rows (options);
    - `TestMergeBubbleCoalescesIntoOneFeedRow` and `TestSubagentBubbleFromAReplayIsStillAddressable` (options).
  - They read a view shared across the daemon or the account:
    - `TestSampledAllowanceResetsAfterItWasRead` and `TestAccountUsageOpusAbsentDrawsTheSessionAndWeeklyAllowances` (account);
    - `TestAnOpenQuestionIsNamedAndGreenOnTheStripAndTheRail` (roster).
  - They are destructive: the four rows that deploy, restart or kill.
- **C-E1, conditional e2e-emacs panels walk:** the 7 `emacs_panels` MOVE rows stay only while no Doom-booted ERT tier exists, which is the audit's question 2.
  - They could share one Emacs and one world.
  - Each step changes window layout, so this would be a sequential walk, not independent cases. An early failure hides the later steps.
  - Saving: about 6 x 2.6 = 16 slot-seconds.
  - I do not recommend it unless those rows end up staying for good.

### Daemon integration: small clusters

Reconcile these with test-tier-audit-integration before acting on them, because its report was not committed when this one was written.

- **D1, one act with several observers (`~/.config/doom-worktrees/test-shared-setup-lzb/modules/app/agent-repl/daemon/integration/merge_test.go`):**
  - `TestTheTestGatePassingSettlesTheTestsTab`, `TestALandedMergeProducesSuccessFooterRosterAndRemovesTheWorktree` and `TestALandedMergeRaisesOnlyItsQueuedAndLandedBanners` perform the byte-identical act:
    - `mergeCleanRepo`;
    - the script exits 0 with `daemon: passed in 1s`;
    - `CommitWork`;
    - a one-shot `MergeWorkspace`;
    - the landing deploys.
  - Each asserts a different view: the tests tab; the footer, roster and removed worktree; the banners.
  - **Shape:** one test, one act, three observation `t.Run`s using `t.Errorf`. Every observation reports even when another fails, so nothing is hidden.
  - **Saving:** 2 fixtures and 2 merge runs, about 2.6s solo and 1.2 CPU-s.
  - **A fourth, judged member:** `TestTheTestsTabPaintsANSISpansFromTheScriptedOutput` could join by switching the shared act's stdout to its ANSI line, which still contains `daemon: passed in 1s`. It also commits a different path, and the merged act must keep that path.
- **D2, sequential walk over `mergeBlockedQueueFixture` (10 tests, 1.53s and 0.88 CPU-s per fixture):**
  - The members:
    - pause and resume;
    - evict;
    - move;
    - the interrupt question;
    - the answers evict, cancel and prioritize;
    - kill;
    - the cold-gate park;
    - pause with no repository.
  - Each mutates the queue, so sharing means a walk: pause/resume, then interrupt with cancel (the merge stays queued), then move, then prioritize, then evict, then kill.
  - **What an early failure hides:** every later step. This is the trade the parent asked to weigh.
  - **Saving:** about 7 fixtures, about 10s solo and 6 CPU-s.
  - **Recommendation:** do not do it. Each step is its own edge case of a queue operation, and 10s solo is small next to the diagnosis cost.
- **Not pursued:** no other daemon integration cluster is worth its isolation cost, since the saving is at most about 0.14s solo per removed `newOpened`.
  - `TestRosterAndFooterAgreeOnBlocked` went to test-cross-product-audit as an Option D candidate. It proposes cutting 15 rows to 6.

## An alternative that cuts cost without collapsing cases

The saving in every e2e cluster comes from building fewer `World`s, not from having fewer test functions. A package-wide shared `World` would give the same saving with no test restructured:

- built lazily on first use in each test process, like ERT's ONE fake daemon;
- keyed by `WorldOpts`, so only default-option tests share it;
- torn down at the end of the process.

- Each existing Test func would take a fresh workspace from it and keep its own name, artifacts and failure.
- Under testrun's 13 e2e chunks that is at most 13 default worlds per run, instead of the 74 that K1, K1b, C-M1 and C-M2 build today.
- The isolation conditions are the same as in the rule below.
- I recommend this shape over collapsing funcs into scenario tests, because it keeps every red-run name exactly as it is today. The owner chooses between them.
- On "fewer test cases": collapsing funcs into `t.Run` cases does not reduce the number of assertions, and must not, since coverage stays. In Option C the saving is runtime and CPU, and the count of top-level funcs is cosmetic.

## Totals, on top of test-tier-audit-e2e

| set | worlds removed | solo wall saved | CPU-s saved (lower bound) |
|---|---|---|---|
| K1 (KEEP, unconditional) | 13 | about 35s | 4.0 |
| K1b (KEEP, needs scoped warnings) | 3 | about 8s | 0.9 |
| C-M1 (contingent: ruled (a), rows mostly leave) | 48 | about 130s | 15 |
| C-M2 (contingent: ruled (a), rows mostly leave) | 8 | about 22s | 2.5 |
| C-W (only if webapp areas stay and the owner overrides one-world-per-area) | 5 | about 13.5s | 1.5 |
| D1 (daemon integration) | n/a | about 2.6s | 1.2 |

- Solo wall is summed per-test wall, and with 8 tests overlapping per chunk the run's wall drops by much less.
- CPU-s is the lower bound described under Method.
- None of these rows double-counts the tier audit: K rows are its KEEP rows, and C rows apply only where its MOVE rows stay.

## Proposed rule change (owner approval needed)

### Where the rules conflict today

- **`modules/app/agent-repl/AGENTS.md`, via write-tests:** "one edge case per test".
  - The same rule is in `~/.config/doom-worktrees/test-shared-setup-lzb/CLAUDE.md:112` ("One test per edge case -- no test should cover more than one edge case"), `daemon/AGENTS.md:2225` and `lisp/AGENTS.md:249`.
- **`skills/write-tests/SKILL.md`, "Changes that LOOK faster and are refused":** "Sharing mutable state across tests to save setup: hides order-dependent failures; isolation is the contract."

### Proposed wording

**1.** Replace the edge-case sentence, in all four places, with:

> One test case per edge case. A case is the smallest unit the runner names on failure: a `t.Run` subtest, a vitest `it`, an `ert-deftest`. A scenario test whose steps or observations are separate named cases, each asserting one edge case, satisfies this rule.

**2.** Replace the write-tests refusal bullet with:

> - Sharing state that more than one case READS or CHANGES: hides order-dependent failures. Sharing an expensive fixture in which every case owns its own slice is allowed, under AGENTS.md "A case may share an expensive fixture, never a slice another case reads".

**3.** Add a new section to `modules/app/agent-repl/AGENTS.md`, after "ONE fake daemon serves a whole suite run":

> ### A case may share an expensive fixture, never a slice another case reads
>
> A fixture that costs more than the cases it serves (an e2e `World`, a merge-queue fixture, a Doom boot) may be built once and shared by several cases, on these conditions:
>
> - **Each case owns its slice.** It opens its own workspace (so its own session, shim, feed and per-workspace logs), fresh, and asserts only on that slice.
> - **Fixture-wide views keep their own fixture.** A case that asserts on a view spanning the fixture (roster, sidebar, daemon stream, account usage or allowances, counts across workspaces, a log not scoped to its workspace) keeps its own fixture.
> - **Warning declarations are scoped to the case's workspace** (`ExpectWarningsFor(ws, ...)`), never fixture-wide. A record with no workspace is declared only by a case that owns its own fixture.
> - **A case that changes the fixture keeps its own.** That means stopping, restarting, deploying or relaunching the daemon, store or sidecar, or needing different fixture options or env. Two cases share only a fixture built with identical options.
> - **Each case is its own named case**, run in parallel with its siblings where the product allows. `t.Fatalf` ends only that case. The shared fixture's own failure fails every case that shares it, and that is reported once, as the fixture's.
> - **One act, several observers.** Tests that perform the byte-identical act and each assert a different view of its result are one test. Its observations are separate named cases using `t.Errorf`, so each one reports even when another fails.
> - **Sequential walks are the exception.** A case may start from another's end state only when the sequence is itself the subject. The walk's head states which later steps an early fatal failure hides.
> - **Mutation evidence first.** Merging existing tests under this section lands only with test-redundancy-measure's evidence that no killed mutant is lost.

## Open questions (sent to the parent)

1. **Which shape?** Approve the shared-fixture rule, and choose between per-cluster scenario tests and a package-wide shared default `World` that keeps every Test func.
   - Still with the owner, because the rule rewords the "one edge case per test" sentence in four files. The parent has recommended the package-wide `World`.

Ruled 2026-10-14:

- The scenario matrix was ruled (a), so C-M1 and C-M2 are contingent.
- `ExpectWarningsFor(ws, ...)` is approved in principle. K1b depends on it.
- Sidecar `sharePrebuilt` is contingent on the mock shards surviving, and they are slated to become unit-level golden checks.

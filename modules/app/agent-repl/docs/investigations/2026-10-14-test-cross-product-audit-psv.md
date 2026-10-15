# Needless cross-product tests: an audit of every agent-repl suite (Option D)

Date: 2026-10-14. Workspace `test-cross-product-audit-psv`, dispatched by
`agent-repl-test-slimming`. INVESTIGATION ONLY: no test was deleted, moved or
merged. Every proposal below is **semantically judged, unconfirmed**: the
production code was read (and, for two tables, probed), but no mutation run has
confirmed that a dropped row loses no killed mutant. Each proposal is to be
confirmed with test-redundancy-measure's tool once it lands on master.

## What was audited, and how

- Candidate tables: a scan for nested `range` / `for…of` / `dolist` loops
  driving `t.Run` / `it` / `ert-deftest`, nested `describe.each` / `it.each`,
  programmatic products (`flatMap`, generated rows), and single-axis tables
  that enumerate every arm of an enum through an expensive layer.
- Cost: `~/.cache/agent-repl/test-history.ramdisk-io-default.json` (per
  top-level test / per vitest or ERT file EWMA, unit-wall seconds). Subtests
  are not recorded there, so a row's cost is the top-level seconds over its
  rows, or one solo arrangement of the same shape. Rows that each start a
  process (a daemon, a shim, a World, a page) were ranked far ahead of
  in-process tables.
- Judgment, per table: which production branch, mapping entry or wire field
  each row reaches; whether a cheaper tier already pins the per-arm mapping;
  what the minimal row set is that still reaches every distinct branch the
  table reaches.
- Two scratch probes (run once, deleted, never committed) replaced reading
  with measurement where the table was too large to judge by eye:
  - the sidebar walk, enumerating the distinct (footer status shape, roster
    arm) outcomes of all 5040 subtests;
  - the mocked vendor, driven in-process for every row of the sidecar's
    scenario table, fingerprinting every file each scenario writes (record
    types, subtypes, keys and their value types; ids and free text masked).

## Headline: one axis replayed through every layer

The expensive redundancy is not the classic "every flag times every flag"
table. It is ONE axis (every vendor scenario, every failure arm, every status
arm) replayed through EVERY layer: shim integration, sidecar integration,
daemon integration, e2e, webapp integration. Each layer's own per-arm mapping
is already pinned by a millisecond unit table at that layer, cited per row
below. What the expensive tier adds is the WIRING of one generic path, and one
or two representatives prove that. Every further row re-proves a mapping the
unit tier owns, at 100-1000x the price.

Three shapes of it, measured:

- **The arm never reaches the layer.** All 12 `!api-*` scenarios write one
  `system:api_error` line with no type and no status, so the sidecar's kind
  switch lands every one of them on `Unmodeled{"unclassified"}`. 15 `!fail-*`
  scenarios write a byte-for-byte identical transcript, since the terminal
  reason rides only the stream. The sidecar table runs them as 27 rows of four
  processes each.
- **The arm meets the layer in one shared function.** The footer's 53 status x
  substatus rows, the 27 refusal sites x 4 cross-cutting arms, and 5 watch
  RPCs x one mismatch cause each pass through one arm-agnostic function, so
  the dimensions do not interact.
- **The product outlived the design that needed it.** The sidebar's 5040-row
  agreement walk was written when the footer and the roster decided status
  independently. The roster now derives its arm from the footer's status, so
  the walk reaches 44 distinct outcomes, and never reaches 8 of the roster's
  arms at all (section 5). Enumeration bought breadth in the wrong
  dimensions.

Where real multi-dimension products exist, a covering set suffices for the
same reason as the second shape. Hand-picked subsets with a stated reason
(`test-integration-roster.el:1171`, `test-commands.el:141`) show the rule
drafted below already being followed in places.

## Savings, net of the two tier audits

The tier audits (`test-tier-audit-e2e-edj`, `test-tier-audit-integration-nzx`)
DELETE or MOVE most rows of the expensive tables this audit also trims. Agreed
with both (2026-10-14):

- Where a tier audit deletes or moves a table, its proposal supersedes this
  one. This audit's per-row pins are the evidence its moves rely on.
- Where a tier audit keeps a table at its tier, this audit's row cuts apply.
- The sidecar drop list below (45 rows, fingerprinted) supersedes the
  integration audit's own ~41-row list, which agrees with it
  (`2026-10-14-test-tier-audit-integration-nzx/sidecar.tsv`).

Primary column: solo seconds (one targeted run on this host) or CPU seconds.
History seconds are the runner's loaded unit-wall EWMA, where rows overlap 8
per chunk.

### Unique to this audit (no tier audit touches these rows)

| Table | Rows | Solo / CPU saved | History seconds saved |
|---|---|---|---|
| daemon unit `TestTheFooterAndTheRosterAlwaysMakeTheSameCoarseClaim` (section 5); the cut ADDS rows reaching the 8 roster arms the walk never reaches | 5040 -> ~60-90 | 4.2 s solo wall, 5.6 CPU-s, down to ~0.1 s | ~19.7 of 20.0 |
| daemon integration tables the tier audit keeps at integration: `adopted_turns_test.go` 6 -> 4, `stateroot_loss_test.go` 4 -> 2, `exit_streams_test.go` 8 -> 4, `drain_rollout_test.go:842` 6 workspaces -> 2 | 10 daemon starts | ~0.14 s solo per bare daemon start (test-shared-setup's figure), more with shims | ~10 |
| webapp unit `main.test.ts` gate rows (needs 2 `vocab.test.ts` rows first) | 12 -> 2 boots | ~195 ms per warm boot, ~2 s | ~2 |
| small in-process tables (section 7) | ~9 rows | negligible | negligible |

Net unique saving: about 30 history-seconds and 10 daemon starts per full
run. The unit walk is the one large item.

### Superseded by a tier audit (this audit's row cut is the fallback)

These apply only if the owner rejects the tier audit's move for that table.

| Table | This audit's cut | Superseded by |
|---|---|---|
| sidecar `TestMockScenariosShard0..7` | 133 -> 88 rows, ~145 slot-s | integration audit: static goldens, unit drives (447 s -> ~2-5 s) |
| e2e `TestApiRequestFailedArms`, `TestTerminalReasonTurnFailedArms`, `TestFailMarker…`, `TestPushNotificationNotSent`, `TestAccountUsageUnreadArms…` | ~25 Worlds, ~70 slot-s | e2e audit: DELETE or MOVE; scenario-matrix ruling (a) made them unconditional |
| e2e `TestStopReplayedByEveryRewindNeverLoops` | 6 Worlds -> 1 (owner ruling: one finished run, independent `t.Run` with `t.Errorf`) | e2e audit: the same single World, cut to its ~3 store and feed checks |
| e2e `TestPermissionAskAnsweredArms` | 3 -> 2 | e2e audit: keep `AllowStanding` alone. Agreed: its standing-offer echo is the one cross-system byte equality, and `AllowOnce` proves nothing it does not |
| daemon integration `TestRosterAndFooterAgreeOnBlocked`, mismatched-dir watches, `…AcrossWatchKinds`, `MoveSwaps…`, `/help`, `/compact`, `AsyncMergeIsRefused…` | ~15 daemon starts | integration audit: PARTIAL or MOVE |
| shim `turn.test.ts` STOP_TAXONOMY, `session.test.ts` usage and fast rows | 30 -> 12 shim processes | integration audit: MOVE-TO-UNIT |
| webapp integration footer, refusals, feed-kinds, sidebar, lifecycle, held | ~590 page boots | integration audit: MOVE-TO-UNIT on most rows |

### Every number before netting

| # | Table | Rows now | Rows proposed | Saving (history / slot seconds) |
|---|---|---|---|---|
| 1 | sidecar `TestMockScenariosShard0..7` | 133 (4 processes each) | ~88 | ~145 slot-s |
| 2 | e2e arm tables (6 tables) | ~37 Worlds | ~11 | ~85-90 slot-s |
| 3 | daemon integration tables (11) | ~70 daemon starts | ~45 | ~30-60 s |
| 4 | webapp integration (footer, refusals, feed-kinds, sidebar, …) | ~1100 page boots | ~510 | ~35-45 s |
| 5 | daemon unit status walk | 5040 | ~60-90 | ~19.7 s (5.6 CPU-s solo) |
| 6 | shim integration `turn.test.ts` + `session.test.ts` | 40 shim processes | 12 | ~10-19 s |
| 7 | webapp unit `main.test.ts` gate rows | 12 boots | 2 | ~2 s |

Solo seconds for the sidecar and e2e rows could not be isolated. A one-row
sidecar run took 9-28 s wall on a loaded host with 2-3 CPU-seconds in the test
process (its node, store and sidecar children are reaped separately), so those
rows are wait-dominated, not CPU-bound. The e2e audit's apportioned CPU
(~1.3 CPU-s per World) is the better figure for them.

ERT, e2e-emacs, testrun, store and the remaining unit suites hold needless
products only in ms-scale pure tables; nothing there is worth the churn
(section 8).

## 1. Sidecar: `TestMockScenariosShard0..7`

`agent-shim/claude/shim-sidecar/integration/mock_scenarios_test.go:229`. 133
rows, each one the mocked vendor (node), the real sidecar and the real store
over its own trees: ~430 unit-wall seconds across the eight shards, the
costliest table in the repository (~3.2 slot-s a row).

The sidecar reads only the FILE plane. The probe drove 132 of the rows'
scenarios in-process (`!vendor-backgrounded`, `!hold`, `!interrupt` park and
were not fingerprinted) and found 79 distinct on-disk shapes among 129. Groups
of identical shape (same files, same record types and subtypes, same keys and
value types; only free text and numbers differ):

| Group | Rows | Why the sidecar cannot tell them apart | Keep |
|---|---|---|---|
| prose turn | `!md` `!context-usage-drift` `!fast-on` `!fast-off` `!fast-cooldown` `!mcp-all` `!mcp-healthy` `!usage-available` `!usage-full` `!usage-opus-absent` `!usage-service-unavailable` `!usage-window-unavailable` `!usage-utilization-unavailable` `!usage-sampling-failure` `!rate-limit` `!compact-failed` `!fault-converter` `!fault-recover` `!keepalive` (19) | each differs only on the STREAM plane (account usage, fast mode, MCP status, a converter fault); on disk it is one assistant line, the prompt and `turn_duration` | `!md` |
| stop scenarios | 15 `!fail-*` (all but `-stop-hook`, `-continuation-prevented`) | the subtype and `terminal_reason` ride the stream `result` only (`shim/src/fake/index.ts:1139`); the transcript gets the same `turn_duration` line (`:1202`) | `!fail-execution` |
| non-retried api errors | `!api-401` `-403` `-400` `-413` `-404` `-billing` `-oauth-org` `-max-output` `-unmodeled` (9) | the `system:api_error` line carries no type and no status (`failures.ts` `apiErrorScenario`), so `apiErrorKind` (`convert/system.go:98`) reaches `Unmodeled{"unclassified"}` for every one; the kind switch is pinned by `convert/system_test.go` | `!api-401` |
| retried api errors | `!api-529` `!api-500` | same, with retry fields | `!api-529` |
| push not sent | `!push-config-off` `-user-present` `-no-transport` | `pushSuccess` switches on the reason (`convert/results.go:897`), but no row of this table asserts the reason; `results_test.go` pins each case | `!push-config-off` |
| web fetch | `!web-fetch` `!web-fetch-redirect` | `webFetchSuccess` copies code/text with no branch (`convert/results.go:590`) | `!web-fetch` |
| query death | `!query-eof` `!query-fail` | the death is a stream-plane fact | `!query-eof` |

Kept as distinct although same-shaped: `!compact` / `!compact-auto`
(`convert/contextcut.go:100` branches on the trigger value),
`!bash-detach` / `!bash-detach-fail` (the exit code is the row's own
`ExitCode` assertion). Not judged: `!perm-*` and `!ask-*`, whose on-disk shape
depends on how the drive answers the gate, which the probe did not reproduce.

Proposal: drop 45 rows (18 + 14 + 8 + 1 + 2 + 1 + 1), ~145 slot-seconds.
Every dropped row still runs at the unit tier and, for its STREAM-plane
meaning, in the shim's own suites.

## 2. e2e arm tables (`modules/app/agent-repl/e2e`)

A World (daemon, shim, store, sidecar) per row, ~2.7-3.1 slot-s each.

| Table | Rows | Keep | What already pins each dropped arm |
|---|---|---|---|
| `TestApiRequestFailedArms` `failurearms_e2e_test.go:197` | 12 | `OauthOrgNotAllowed` only, beside the existing standalones `TestApiRateLimitedCarriesTheVendorWait` (:340) and `TestApiUnmodeledKeepsTheVendorClassByName` (:377), which already drive `api-429` and `api-unmodeled` | shim `test/convert/terminals.test.ts:121,140-163`; `fold.test.ts:1812-1935`; daemon `resolve/feed/turnended_test.go:415` (all 12), `:532`; `resolve/turnfault/words_test.go:145` |
| `TestTerminalReasonTurnFailedArms` `failurearms_e2e_test.go:437` | 10 | `ModelError` (its own `producerErrorArm` case, `turnended.go:480`), one default-branch row | `terminals.test.ts:71`; `turnended_test.go:604,710,759`; `words_test.go:58` |
| `TestStopReplayedByEveryRewindNeverLoops` `keepalive_e2e_test.go:288` | 6 byte-identical Arrange/Act | 1 run, the 6 assertions as subtests over it | not a product: one edge case paid six times. RULED (2026-10-14, via the parent): one finished run may serve read-only assertions, each its own `t.Run` using `t.Errorf`, under test-shared-setup's rule "A case may share an expensive fixture, never a slice another case reads" |
| `TestPermissionAskAnsweredArms` `permission_e2e_test.go:180` | 3 (serial) | `AllowStanding`, `DeniedByUser` | `daemon/integration/prompt_test.go:1674`; shim `test/integration/gate.test.ts:200`; `resolve/feed/permission_test.go:213` |
| `TestFailMarkerFails…` `mergequeue_e2e_test.go:1005` | 3 | before/self, after/non-self | after/self is pinned by `internal/merge/process_test.go:114` and is the costliest row |
| `TestPushNotificationNotSent` `remainder_e2e_test.go:742` | 3 on one World | 1 | no assertion depends on the reason; `push-notification.test.ts:160-196` |
| `TestAccountUsageUnreadArmsLeaveTheReadFiguresStanding` `sessionfacts_e2e_test.go:421` | 4 | all 4 TODAY | `window_unavailable` and `utilization_unavailable` (`footer/resolver.go:1301-1304`) have no unit pin. With a ms daemon unit table over `unavailableReason`, 3 rows go |

Not products: `TestWebappLayer*` (one World per area, distinct drawn
families), `TestColdGate`, `TestMergeBubbleCoalescesIntoOneFeedRow`,
`TestEmacsPrefixAndPostfixSendVariants`.

Saving: ~30 Worlds, ~85-90 slot-seconds; the six top-level tests ~108 s -> ~20 s.

## 3. Daemon integration (`daemon/integration`, a real daemon per row)

| Table | Rows | Keep | Why the rest go |
|---|---|---|---|
| `TestRosterAndFooterAgreeOnBlocked` `blocked_agreement_test.go:30` (39.4 s) | 15 | `authentication_failed`, `blocking_limit`, `api_overloaded`, `max_turns`, `stop_hook_prevented`, `query_died`: one per clause of `ladder.ClassifyFailure` (`resolve/ladder/failure.go:73`) | both surfaces read only the class; every arm's class is pinned at `ladder/failure_test.go:9`, `footer/status_test.go:658`, `sidebar/status_test.go:365` |
| `TestPerWorkspaceWatchOpensWithAMismatchedDirAreTransportClosed` `subscriptions_test.go:570` | 5 | WatchFooter | all five call the shared `resolveStreamRef` / `refuseStream`; each rpc's refusal wiring is pinned by the sibling at `:545` |
| `TestSubscriptionInvariantAcrossWatchKinds` `subscriptions_test.go:182` | 4 live | Footer, Daemon | Topbar and Roster share `serveTopicWith` (`server/streams.go:113`), pinned by `publish/topic_test.go:39,71`, `server/streams_test.go:61,87` |
| `TestEveryStandingStreamEndsWithItsEndFrameWhenTheDaemonExits` `exit_streams_test.go:119` | 2 transports x 4 streams | a covering set of 4 (each stream once, each transport twice) | the per-stream ending is `endStandingStream` (`server/streams.go:178`), pinned at `streams_test.go:593`; the transport is stream-independent |
| adoption tables `adopted_turns_test.go:98,205,237` | 3 paths x 2 outcomes | still-runs on all 3 paths, finished-unwatched on takeover | the reconciliation is unit-pinned (`sessionwatcher/watcher_test.go:2630-2671`, `promptqueue/lifecycle_test.go:351-399`) |
| `TestADaemonStandsDownWhenWhatItOwnsOnDiskVanishes` `stateroot_loss_test.go:56` | 4 | under a served workspace, `daemon.lock` | the watch does not depend on the file; `daemonaddr/claim_test.go:567` pins each file |
| `TestUpdateMergeQueueMoveSwapsTwoWaitingMerges` `merge_test.go:192` | 2 | move_up | move_down is pinned at `server/admin_test.go:193-201`, `wsm/mergequeue_test.go:422`, `merge/queue_test.go:63` |
| `TestRegistryReadsAcrossTheSuccessorsPromotionAreServed` `drain_rollout_test.go:842` | 6 workspaces | 2 | the count only changes race odds; the reproductions are `wsm/open_test.go:126`, `server/adopt_test.go:147` |
| `prompt_test.go:1098`, `:1143` | 2 + 2 | `/agents`, `/clear` | recognition pins both pairs (`prompthandler/recognition_test.go:25,124`, `promptqueue/acts_test.go:375`) |
| `TestAnAsyncMergeIsRefusedWhatItCannotRunAsAsked` `merge_sources_test.go:180` | 4 | 3 (one per wire arm) | both branches to `UnknownOneShotWorkspace` are pinned at `merge/asynccheck_test.go:31-32` |
| `TestWatchHostWorkspaceFlushesHeaders…` `subscriptions_test.go:410` | 1 | 0 | an exact duplicate of the WatchHostWorkspace row of `TestFlushOnAcceptAcrossWatchKinds` (`:254`) |

Every row distinct: `TestFlushOnAcceptAcrossWatchKinds`, AnswerPermission
(`prompt_test.go:1674`), `TestTheBootAdoptionStandsTheGateTheSurvivorHoldsOpen`,
`feed_test.go:2104`, `cold_start_handover_test.go:62`, `boot_test.go:719`.
Not a table: `TestSecondDaemonOnTheSameStateRootRefusesToBoot` (14.2 s is the
production `daemonaddr.ClaimWaitBound`, 12.32 s, hard-wired at `run.go:98`;
a different finding, outside this option).

Saving: ~25 daemon starts, ~30 s at history means, ~50-60 s at full-run load.

## 4. Webapp integration (`webapp/test/integration`, a harness boot per row, ~40-160 ms)

| Table | Rows | Keep | Pins |
|---|---|---|---|
| footer status x substatus `footer.integration.test.ts:161` | 53 | 1 (`merging/rebasing`) | `drawFooterSubStatus` is arm-agnostic (`src/footer/strip.ts:455`); `strip.test.ts:195,200` cover every pair |
| footer status x salient `:300` | 68 | 19 (one per salient kind, one under a waiting/permission/question status) | `activityTier` (`src/footer/activity.ts:192-217`); `activity.test.ts:965` |
| footer status arms `:117`, `:124` | 36 | 2 | `strip.ts:263-264`; `strip.test.ts:126`, `footer/tones.test.ts:65` |
| refusals `REFUSAL_SITES` x `CROSS_CUTTING_ARMS` `refusals.integration.test.ts:777` | 27 sites x 4 + more | `unknownWorkspace` at each site; per-rpc arms at `:818` kept | the cross-cutting sentence is shared (`src/rpc/refuse.ts:136-169`, `refusal.ts:64-80`); `refusal.test.ts:37-61`, `refuse.test.ts:72,105,133` |
| refusals duplicated sites | 4 (`RestartWorkspace`, `SetModel`, `SetPermissionMode`, `SelectWorkspace` each listed twice, `:643/656`, `:621/670`, `:629/679`, `:650/663`) | 0 | identical rows left by 13220b41c1 after the force verb went; vitest also reports two tests by one name |
| feed-kinds (15 tables, `feed-kinds.integration.test.ts:340-1732`) | 86 droppable | see the per-line table in the sibling report below | each dropped check is arm-agnostic in the drawer; per-arm unit pins in `tab-strip.test.ts`, `skill.test.ts`, `artifact.test.ts`, `plan.test.ts`, `subagent.test.ts`, `shell.test.ts`, `permission.test.ts` |
| sidebar arms `sidebar.integration.test.ts:374,392` | 50 | 3 | `row.ts:194-196`; `tones.test.ts:41,52,124-136`, `row.test.ts:115-130` |
| lifecycle drain reasons `lifecycle.integration.test.ts:270` | 3 | 0 | `lifecycle.ts:492-495`; `lifecycle.test.ts:250,254`; `deploy` asserted at `:221-231` |
| held `HOLD_ARMS` `held.integration.test.ts:149` | 6 | 1 | `held-prompt.ts:288`; `held-prompt.test.ts:492-496` |

feed-kinds detail (rows to drop): `:1547` merge tab kinds x {live, settled}
22 -> 2; `:1574` waiting kinds 5 -> 1; `:340/:396` response states: drop the
6 state-agnostic unit-hook/usage/notice rows; `:461` output forms: drop 12
state/clock rows; `:608` input forms: drop 4; `:729` skill 6; `:745` hook 1;
`:761` artifact 4; `:777` plan 2; `:915` subagent 4 (keep `restarted`, no
unit pin); `:940` shell 4; `:1103` separation 6; `:1279` cold gate 2;
`:1366` permission 8; `:1732` command panel 3.

Saving: ~590 rows, ~35-45 s.

Webapp unit: `main.test.ts:595,606,622` gate rows, 12 warm boots -> 2
(`:595` open, `:622 agentReplFault` closed; `:595 merging` duplicates
`:622 merging`), ~2 s, CONDITIONAL on adding `merging` and `idle` rows to
`vocab.test.ts:418-428` first (the per-arm decision is `composerClosedFor`,
`vocab.ts:134`). `topbar.webkit.test.ts:89`: the dpr x offset centering rows
are all distinct (the defect showed only at 2x); five layout rows do not
depend on dpr but the browser launches per dpr anyway, ~0.1-0.3 s.
`expanded-ceiling.webkit.test.ts:72`: 4 items x 2 heights -> 5 rows, ~0.1 s.
`verbs.test.ts:433`: partially needless, 163 ms total, leave it.

## 5. Daemon unit: `TestTheFooterAndTheRosterAlwaysMakeTheSameCoarseClaim`

`daemon/internal/resolve/sidebar/onestatus_test.go:339`. merge (5) x link (14)
x turn (18) x parked (2) x read (2) = 5040 subtests, 20 s unit-wall: the
costliest unit test in the daemon.

- The walk was written (owner ruling 2026-09-28) when the two resolvers
  decided independently. They no longer do: both walk the ONE shared
  `ladder.Resolve` (`resolve/ladder/ladder.go:196`), and above the idle rung
  the roster DERIVES its arm from the footer's status (`statusArm` /
  `projectArm`, `resolve/sidebar/status.go:47,98`). Agreement above idle is
  therefore structural, and a mutant in the shared ladder moves both surfaces
  together, invisible to this test by construction.
- `read` cannot change a claim: `SetViewed` only moves the roster between
  idle-rung arms (`done`/`interrupted`/`turn_failed` vs `ready`), all
  `ladder.Idle` (`resolve/ladder/projection.go:13`). 2520 rows assert nothing
  the other 2520 do not.
- Probe: the 5040 subtests reach 44 distinct (footer status shape, roster arm)
  outcomes. `merge=queued` and `merge=merging` alone are 2016 rows reaching 4
  outcomes, because `Merging` heads `ladder.Order`.
- What the walk still catches: a `projectArm` or `idleArm` mapping off its
  claim, and a footer rung drawing off the claim it resolved (the
  `status_claim` error records it checks). Both are reached by one row per
  outcome.
- COVERAGE GAP, and the cut must close it. The walk never reaches eight
  roster arms: `waiting`, `question`, `closing`, `cold_gate`,
  `daemon_impaired`, `idle_async`, `clearing` and `compacting`. Their
  projections are pinned only elsewhere, so the agreement this test exists to
  hold is unchecked for them. 5040 rows cover 44 outcomes and miss 8 of the
  ladder's arms: the product bought breadth in the wrong dimensions.
- Proposal: choose the reduced set to CLOSE that gap, not merely to shrink
  the walk:
  - one row per distinct outcome the walk reaches today (44);
  - a few rows reaching one outcome by a different fact (a fault through the
    link vs through the turn);
  - NEW rows, by new link or turn cases, reaching each of the eight missing
    arms;
  - ~60-90 rows in all; ~19 s saved.
  Better still, a direct table over `projectArm` per footer arm and sub-arm
  plus `idleArm` per branch, which the walk only reaches indirectly.
- Side finding, not a cross product: `newSurfaces` reloads the vocabulary from
  disk per subtest (`onestatus_test.go:50`), an Option C (shared setup)
  candidate. The probe ran all 5040 combinations in 5.2 s without per-subtest
  machinery.

## 6. Shim integration (a real shim process per row, ~0.35-0.7 s)

- `test/integration/turn.test.ts:431` STOP_TAXONOMY, 34 rows -> 10:
  `!fail-execution` (subtype-only path), `!fail-blocking-limit` (reason path),
  `!api-429` (kind from status), `!api-billing` (class remembered across
  records), `!api-unmodeled` and `!fail-aborted-tools` (no unit pin),
  `!context-window`, `!refusal-fallback`, `!refusal-no-fallback`,
  `!interrupt`. The prompt -> reason mapping is pinned at
  `test/fake/scenarios/failures.test.ts:15-60,196,322`; reason/status/class ->
  arm at `test/convert/terminals.test.ts:71,120,140-160,395`. ~8.5-17 s.
- `test/integration/session.test.ts:740` usage-unavailable 3 -> 1 and `:765`
  fast mode 3 -> 1 (pinned at `engine/session.test.ts:8002-8119,6760`,
  `fake/scenarios/session.test.ts:182`); `:1030` query-died 2 rows are
  distinct. ~1 s.

## 7. Small in-process tables (mentioned only because clearly needless)

- `testrun/internal/suites/suites_test.go:808` `TestEveryItemChunkReadsTheSharedItemLine`: 3 cases x 2 chunk kinds; both kinds' `Items` call the one `ParseItemLines` (`ert.go:63`, `script.go:39`). 6 -> 4. µs.
- `daemon/internal/server/admin_test.go:808`: 3 arms x 3 cases; move_down equals move_up after `mergeMoveOf`. 9 -> 6. 0.04 s.
- `daemon/internal/rollout/relaunch_test.go:1222`: one scenario run 6 times, once per assertion. ms.

## 8. Where needless products are rare

- ERT (`lisp/test-*.el`, ~89 s for the whole suite): the per-arm decoder
  tables in `test-integration-host/roster/link.el` each run inside ONE fake
  daemon and each row is an independently declared literal mapping pair; every
  row distinct. `test-commands.el:141` and `test-integration-roster.el:1171`
  are models of the rule below (a cheap symbol check instead of 35 tests; 5
  hand-picked pairs of a 20-pair product with the reason in the docstring).
  The only large product, `test-status.el:3469` (240 pure checks), costs
  nothing.
- e2e-emacs: no table drives the sandboxed Emacs more than once per distinct
  scenario (`TestEmacsRosterArmsPaintTheTabInOrder` is one walk, audited).
- store, sidecar unit, testrun, `health/faultclose_test.go:68` (each cell a
  distinct data mutant), `durationknob_test.go:72`: every row distinct or
  free.

## Owner decisions this report needs

1. Accept the unique proposals table by table, each pending mutation
   confirmation. The superseded ones stand as the fallback for any tier move
   the owner rejects.
2. Ruled: `TestStopReplayedByEveryRewindNeverLoops` may serve its read-only
   assertions from one finished run.
3. Items whose drop needs a ms unit test first (account-usage reasons in
   `footer/enduring_test.go`; `vocab.test.ts` merging/idle rows): add the unit
   row, then drop the e2e/integration rows.
4. The drafted AGENTS.md rule ("A table's cases are chosen, never enumerated"),
   on this branch.

## Confirmation plan (once test-redundancy-measure lands)

For each table: run the full table and the proposed subset under the tool;
a row goes only if the subset's killed-mutant set equals the table's over the
production packages the table reaches. The sidecar and daemon-walk proposals
come with probes that already show the outcome sets; the rest are reading
only.

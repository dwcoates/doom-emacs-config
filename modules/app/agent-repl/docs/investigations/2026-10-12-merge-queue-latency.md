# Why merge-queue members take so long (2026-10-12, 12:00-15:10)

Read-only diagnosis. Sources: `~/.claude-emacs/wsm.db` (`merge_queue`,
`merge_progress`, `merge_ledger`, `merge_tab_intervals`, read-only), the
daemon's run log and per-workspace daemon logs, the gate archives in
`~/.claude-emacs/merge-logs/<lease>-tests-<round>.log`, and master's history.
"Confirmed" means a record shows it; "inferred" means it does not.

## Headline

- **The queue landed nothing from 12:30 to 15:10.** Every one of master's
  ~60 commits in that window is a direct cherry-pick by the lead session, not
  a queue landing (confirmed: `git log`).
- In the window, 17 gate runs failed, 2 were cut by restarts, and 1 passed. The
  one pass was thrown away because master moved under it.
- The time is mostly NOT the merges' own code. In order of time lost:
  1. a red master that every gate inherited,
  2. the shared single suite slot plus host load from ~53 concurrent full
     runs,
  3. the lead session's direct pushes to master moving the target under
     passed gates,
  4. restarts,
  5. fix attempts burned instantly on a vendor auth failure.

## Per-entry timelines (minutes; rounds from `merge_tab_intervals`)

| Merge (lease) | Span | Gate runs | Fix rounds | Outcome | Where the time went |
|---|---|---|---|---|---|
| tab-bar-sibling-order-merge (4ce0642d) | 13:06-15:09, 123 | 7 (66 min, 25 of it slot wait) | 3: 0s (auth), 24 min, 9 min | failed 15:09 after 3 fixes | master red r2,r4; restart cut r3 (13:39 bounce); idle 13:40-13:55 behind the head; **r5 PASSED 14:38 and was discarded** ("the target moved since the rebase began", master moved by the lead's 14:27-14:33 picks); re-rebase hit a conflict; r6/r7 failed on load-sensitive suites |
| sidebar-oneshot-indicator-merge (b2008858) | 13:06-13:55, 48 | 4 (41 min, 7 slot wait) | 3: 3 min, **0s, 0s** (auth) | failed 13:55 | r1 master red (background-harness, ERT pin); r2-r4 master red + load storms (r4: 48 failing tests across 6 suites); 2 of 3 fix attempts were instant auth failures |
| minibuffer-commission-history-persist-merge (a9421435) | 12:48-13:21, 32 | 4 (21 min, 6 slot wait) | 3 real (6, 0.5, 5 min) | failed 13:21 | r1 master red; r2 one testrun test; r3/r4 load |
| minibuffer-...-merge-2 (9494be0e) | 13:55-15:09, 74 | 1 (cut at 14:28) | 0 | failed 15:09 | idle behind the head 13:55-14:27; suspended at the 14:27 bounce; **recovered but never resumed** (third in-progress merge, window now 2); requester re-queued `-merge-3` at 14:34; failed at 15:09 "no queue entry of 71e48120d88549d4 stands" |
| worktree-merged-cleanup-merge-2 (b4eb9c28) | 13:21-14:35, 74 | 2, both cut by restarts | 0 | evicted 14:35 | speculative gate waited 600s for the suite slot then was cut by the 13:39 bounce; idle 13:40-13:55 and 13:55-14:21; second gate cut by the 14:27 bounce; evicted by the operator and re-queued `-merge-3` |
| sidebar-oneshot-indicator-merge-2 (576f8e00) | 14:35-, 35+ | 2 (13 min) | 1 (6 min) | running | r1 failed on load-sensitive suites + background-harness niceness checks |
| doom (339ccf0a), fork-cache-miss-oiu (35d8ff24) | 12:12-12:16 | 1 / 0 | - | failed 12:16 / 12:13 | ended within minutes, but their ledger rounds stayed open (see Pathologies) |

Queue now (15:09): head `tab-bar-sibling-order-merge` just failed; 8 entries
waiting. Three of them are re-requests of merges that died to restarts or
eviction (`-merge-2`/`-merge-3` suffixes).

## Aggregate attribution (the six live/recent merges above, real rounds only)

| Bucket | Minutes | Share | Evidence |
|---|---|---|---|
| Gate execution that failed on master-red or load-only tests | ~120 | ~35% | 17 failed gate runs, all carrying recurring or master-red tests (below) |
| Fix agents working on failures that were not the merge's | ~55 | ~15% | fix rounds after master-red/load failures, e.g. a 24 min fix round for the head after r4 |
| Waiting for the shared suite slot | ~56 | ~15% | `suite-slot: still waiting ... (Ns)` in archives; max 600s; 15 of the waits were behind full runs from this session's forks (test-speed x3, master-reds, rollout-hold, hidden-page-coalesce) and other workspaces' own runs |
| Idle behind the head (window/chain) | ~60 | ~17% | gaps between rounds; e.g. 13:40-13:55, 13:55-14:21/14:27 |
| Work thrown away by a moved target | ~30 | ~8% | head's passed r5 discarded at 14:38 (lead's direct picks), then r6+fix+r7 |
| Work thrown away or stranded by restarts | ~25 (+74 for a merge stranded entirely) | ~7% | gates cut at 13:39 and 14:27; 9494be0e recovered but never resumed |
| Rebases and conflicts | ~2 | <1% | |

Shares are approximate (overlapping causes on one run are counted once, under
the first-listed cause).

## Why so many fix -> rebase -> failed-test cycles (classification of the 19 cut or failed gate runs)

| Class | Gate runs | Notes |
|---|---|---|
| (a) master itself red at that tip | 6 runs (+ the fix rounds that followed them) | `background-harness` scan, red 12:29 (8ade38a7a, another session's landing) to 14:33 (6c8b232db, lead's direct pick); ERT `merge-queue-action-arms-pinned`, red 12:13 (66dd2cff9, another session's landing) to 14:46 (lead's direct pick). Both were red on master for 2+ hours, and every gate in that window failed on them. |
| (b) load-only or recurring flakes | 11 runs | host load average 25-95 all afternoon (~53 full-suite runs started 12:00-15:59, 637 min of summed wall); tests failing in several unrelated merges: `TestWebappLayerFeedFamilies` (5 runs), `TestWebappLayer{Surfaces,Subfeeds,Refusals,ClientLog}` (2 each), shim `main.test.ts` boot cases (2 each), `TestHandoverTransfersAtFreeness`, `TestAMergeRunningThroughAHandoverResumesInTheSuccessorAndLands`, `TestAMergeStoppedInItsTestGateResumesAfterARestartAndLands`, background-harness "Darwin (real) ... niceness 19" (2 each) |
| (c) genuine conflict with what landed ahead | 1 cycle | the head's re-rebase at 14:38 conflicted with the lead's direct picks; resolved in 32s |
| (d) the fix did not fix it | 2 of 9 fix rounds were instant auth failures (the fixer never ran); the real fixes were followed by failures in different, load-sensitive tests | fix turns 2331464d (13:31), e780b40b (13:37), 69c480fe (13:46): "the vendor does not serve the session", `authentication_failed`, `vendor_block: auth` ("the credential was rejected - sign in again"); each still counted as a used fix attempt and the gate re-ran unchanged |
| (e) a restart cut the gate | 2 runs, plus 2 merges stranded | 13:39 and 14:27 bounces |

Runs whose failing tests were plausibly the merge's own (one-off names not seen
elsewhere) exist (e.g. `TestNoSuiteScriptPipesIntoAnEarlyExitingReader`,
`TestEmacsRestartKeepsTheSelectedWorkspace`, `footer/refusals.integration.test.ts`
cases). All of them occurred in runs with mass load failures, so whether they
are the merge's fault is INFERRED undecidable from these logs.

## Top causes, ranked by time lost

1. **Master was red for 2+ hours** (the ERT arm pin and the background-harness
   scan). Every gate in the window failed on them, and fix agents spent rounds
   on them. Raw inefficiency, not restarts.
2. **One shared suite slot plus a saturated host.** Merge gates queue behind
   every session's full runs (this session's forks included). Those runs then
   execute at load 25-95, which turns timing-sensitive tests red. Raw
   inefficiency.
3. **Direct pushes to master outside the queue.** The lead's batches at 13:37,
   14:27-14:33 and 14:46-14:47 moved the target under the queue. The only
   passing gate of the afternoon (head r5, 14:38) was discarded, causing a
   conflict, a re-test, a fix and another re-test (~30 min).
4. **Restart handling** (~25 min of cut gates, plus whole merges stranded):
   - A gate cut by a bounce re-runs from its start, and its slot wait starts
     over.
   - The window change (3 -> 2) at the 14:27 bounce left a third in-progress
     merge recovered but never resumed. It failed 41 minutes later on a queue
     entry its requester had replaced.
   - Two merges were evicted or re-requested after restarts, duplicating
     entries.
5. **Vendor auth failure burned fix attempts**: 2 of 3 attempts for
   sidebar-oneshot-indicator-merge were instant `authentication_failed` turns.
   The merge failed without a real second or third fix.

Split: roughly **75-80% raw inefficiency** (master red, slot plus load, idle
behind the head, direct pushes moving the target) and **15-20% restart
handling** (cut gates, a stranded merge, re-queued duplicates), plus the
auth-burned fix attempts.

## Pathologies found

- **Leaked open ledger rounds:** 24 tab rounds across 18 leases have no
  `ended_at`, including merges that failed at 12:13/12:16. A speculative round
  superseded by a restack (the head's `tests r1` from 13:10) also never
  closes, so anything reading the ledger for duration (bubbles, this analysis)
  over-counts.
- **Recovered-but-never-resumed merge:** 9494be0e was recovered at 14:28:38
  beyond the new window of 2. It sat with an open `rebasing` round for 41
  minutes, then failed on a missing queue entry.
- **Fix attempts counted when the fixer never ran:** a vendor-refused repair
  turn closes the fix round as "succeeded" in 0s and consumes an attempt.
- **Requeued duplicates:** `-merge-2` and `-merge-3` entries for the same work
  after restarts and evictions.

## Observability gaps

- The gate archive does not record its own start time. Slot wait and execution
  can only be separated from the `still waiting` lines.
- No record says why a merge sat idle behind the head: chain wait, governor
  deferral, or waiting for promotion. Idle time is inferred from gaps between
  rounds. Governor decisions since noon: 17 speculative gates deferred
  (load_in_or_above_band), 3 deferred (no_sample), 9 admitted, 16 head gates
  ungoverned.
- A fix round's outcome "succeeded" does not distinguish "the fixer committed"
  from "the fixer's turn failed instantly".
- Nothing marks a commit on the target as having bypassed the queue, so a
  "target moved" record cannot say who moved it.

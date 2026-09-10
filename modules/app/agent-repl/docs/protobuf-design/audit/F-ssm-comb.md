# F — SSM comb against the WSM categorization (2026-08-24)

The user's proposed purpose: SSM manages WORKSPACE state (what's open, what's
merging, merge queue) and should be renamed WSM. This comb classifies every
responsibility the implementation actually holds. SSM: `daemon/internal/ssm`
(~40 non-test files; ssm.go 1845 lines, connectivity.go 1395, turnclaims.go
1217). `daemon/internal/statedb` is NOT SSM-owned (shared SQLite package);
sessioncontroller is a peer reached via injected interfaces.

## SSM-owned persisted tables (ssm/db.go:60-235)

workspace_state, conversation_reader_position, turn_lifecycle_claim,
turn_interruption, session_connectivity, session_fault, merge_lease,
workspace_merged, compaction_gate.

## (a) Workspace state — stays in a pure WSM

- Workspace lifecycle axis + rank resolution (resolve.go:259-315, 420-571) — workspace_state.
- Merge axis & phases (ssm.go:1045, mergephase.go:54) — workspace_state.
- Merge exclusivity lease ledger (mergelease.go:304,457,475) — merge_lease.
- Merged-at set-once + merged teardown (merged.go, db.go:218) — workspace_merged.
- Merge-failed clear / merged-reopen (mergefailedclear.go:57, mergereopen.go).
- Merge queue binding + dequeue offer (ssm.go:245-250, mergedequeue.go) — in-memory, deliberate.
- Hibernation lease ⟂ controller registration exclusion (hibernationlease.go:25,67) — in-memory.
- Boot-sweep verdict (bootsweepverdict.go:80); last-activity (activity.go:22);
  fence (fence.go); snapshot/subscribe fan-out (ssm.go:1471,1577,1684).

## (b) NOT workspace state

- TURN LEDGER: turnclaims.go (187,270,438,516,1129,1155), turnliveness.go,
  turnboundary.go:51, turninterruption.go, turnorigin.go:101, shimstop.go:46,125,
  compactionclaim.go — tables turn_lifecycle_claim, turn_interruption.
- PROMPT/QUERY LIFECYCLE: promptstate.go (56,284,370,487,511),
  mergepromptgate.go, merge_lease's displaced_prompt columns.
- PERMISSION tracking: permission.go:35 — workspace_state.
- CONVERSATION CONTENT: readerposition.go (+conversation_reader_position),
  compactiongate.go (+compaction_gate), contextcut.go clearing/compacting axes
  + watchdog timers.
- LIVE TASK TRACKING: livetasks.go:67,123, reconcile.go:60, orphan.go.
- RENDER-STATE RESOLUTION & PUSH DIFFING: the rank table (resolve.go:420-571),
  last*/pushedAtMs/stampedComposite/publishEpoch caches (ssm.go:153-186).
- CONNECTIVITY/GENERATION/FAULTS: connectivity.go (118,222,677), wired.go:93,
  ApplyConnectionDegraded/ApplyBackfillState/ApplySessionRotated
  (ssm.go:811,979,1009) — session_connectivity, session_fault.

## (c) Borderline

- MergeStatus retention (mergestatus.go; ssm.go:157-172): content is (a),
  mechanism is a render-relay (b).
- Hibernation lease doubles as turn admission (b).
- merge_lease displaced_prompt/permission_mode: lease window (a), a held
  prompt (b) parked in a workspace table.
- session_fault: degrades workspace operability (a-ish), scoped to a
  controller generation (b).
- turn-origin closes for merge-resume: merge lifecycle via the turn ledger.

## Verdict

To become a pure WSM, SSM sheds: (1) the whole turn ledger → turn-scoped
component / shim store; (2) prompt/permission lifecycle → prompt-hold
component / store; (3) conversation-content state → shim store;
(4) live-task tracking → the shim's activity plane (GetLiveWork);
(5) render-state resolution → frontend resolvers; (6) connectivity/faults →
a connection-supervision component. Remains: workspace lifecycle, merge axis
+ lease + queue + merged fact, hibernation/registration exclusion, boot
sweep, activity, fence, fan-out.

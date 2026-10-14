# Paused merge queue in the sidebar

When a repository's merge queue is paused (`run.sh --pause-queue <repo-dir>`),
the sidebar still shows that repository's merges as purple `merge_queued` and
breathing `merging` rows, and its async-merge marks still breathe purple. A
paused queue therefore looks like it is still making progress. The owner's
ruling (2026-10-14), implemented exactly:

1. While a repository's merge queue is paused, every merge indicator in the
   sidebar for that repository's workspaces turns grey. The indicators are the
   async-merge row marker and the `merge_queued` / `merging` / async-merge
   status inks.
2. While paused, a workspace name that breathes because it is actively merging
   stops breathing, but keeps its color.
3. When the queue is resumed, the sidebar goes back to the normal indicators
   and animations at once, with no reload.

## Scope

- Objective: make a paused merge queue visible in the sidebar.
- Frontend component: yes, the sidebar's roster row (figma→idl). There is no
  new drawn box. The fact changes how two existing elements are drawn: the
  status mark's ink and the name's breath.
- Systems: the protobuf contract (`frontend.v1`), the shared color vocabulary
  (`proto/vocab/render-colors.json`), the daemon (merge orchestrator, roster
  resolver, vocab loader) and the webapp sidebar. The Emacs tab bar is outside
  the ruling and draws nothing for the fact.
- Non-additive changes: the daemon's `sidebar.Resolver.SetAsyncMerges` is
  replaced by `SetMergeQueue`, which states a repository queue's whole roster
  view (whether it is paused, the workspaces on it, and the async requesters)
  in one call.

## Process note

Contract changes were pre-approved by the owner for this task, so no design
dialogue was run. The shape below was decided by the orchestrator and is
recorded here.

## Landed changes

### 1. `frontend.v1.RosterRow.merge_paused` (tag 68, `frontend.v1.RosterRowMergePaused`, empty)

- WHAT: a new optional marker on the roster row. It is present exactly while
  the row draws a merge indicator AND the merge queue of the repository that
  merge is on is paused. A row draws a merge indicator when its status is
  `merging` or `merge_queued`, or when it carries `async_merging`.
- WHY: the paused state is per repository queue. But which rows a pause
  affects is a join that only the daemon can make: which queue each row's
  merge is on, and whether that row draws a merge indicator at all. Under
  figma→idl the client never derives, so the daemon resolves the join per row.
  A per-repository-section flag was rejected for three reasons. Rows also
  appear in the task, maturity and open groupings, where no repository section
  surrounds them. The webapp would have to work out which rows' marks it
  governs. And an async merge's requester row could sit under a different
  repository section than the queue its branch merges on.
- WHY NOT A STATUS ARM: the row's status arm and its name's color stay as
  they are (ruling 2). A paused merge does not change the workspace's state,
  only how its merge indicator is drawn.
- Rendering, per the vocabulary's new `paused_merge_mark` section
  (`{"ink": "grey", "motion": "still"}`, with `grey` added to
  `glyph_only_colors`):
  - The merge glyph of a `merging` / `merge_queued` row is inked grey.
  - The async-merge mark (the merge glyph that replaces a plain dot) is
    inked grey.
  - The breath the merge contributed stops. A `merging` row's name holds
    still. A row whose motion came only from its async-merge mark holds
    still. A row whose own status arm breathes (a turn in flight under an
    async merge) keeps breathing, because that breath belongs to the turn,
    not to the merge.
  - The row's tone (its name's color) is untouched.
- CONSEQUENCES:
  - Daemon: `merge.asyncMergesDB` wraps `SetMergeQueuePaused` as well, so
    every pause or resume republishes that repository's queue to the roster.
    No call site can forget to. Boot and adoption already republish every
    queue (`republishQueue`), so a restarted daemon draws a paused queue
    correctly.
  - Daemon: `sidebar.rosterState.asyncMerges` becomes `queues`, one
    `mergeQueue` per repository (paused, members, async requesters), each
    replaced whole on every publish.
  - Webapp: `tones.rosterRowMark` takes the marker as a third `RowMarkers`
    flag. `styles.css` gains `#ws-sidebar .st.ink-grey` (the inactive dot's
    `--muted`).
  - The footer and the Emacs tab bar are unchanged.

## Daemon log records fixed alongside

Records seen at 2026-10-14T15:32:37 when a queue was paused under a running
merge's test gate:

- `daemon.merge.tests` said the gate was cut by "the daemon's exit, a restart
  or a transfer". It now names the suspension that actually cut it (the
  queue's pause), and says the gate runs again once the queue is resumed.
- `daemon.merge.suspend` gave the step's own error (`scriptrunner: run env:
  context canceled`) as the cause and said the merge stopped "for the
  daemon's exit or its workspace's transfer". A run now records WHICH
  suspension took it (`run.suspendedFor`), set at every point where a run
  starts suspending. The record names that suspension, and keeps the step's
  error as a separate `step_error` field.
- `daemon.merge.recover` said a merge "resumes at the step it recorded" at the
  pause's own take-back, while the paused queue keeps it suspended. On a
  paused queue the record now says the merge is taken back and stays
  suspended until the queue is resumed.

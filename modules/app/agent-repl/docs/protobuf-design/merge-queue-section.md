# The merge queue section

The owner asked (2026-10-26) to merge the sidebar's merge queue view, which
the purple merge glyph on the OPEN header line toggled the repository
sections into, with the "RECENTLY MERGED" section: the section reads "MERGE
QUEUE", draws the merge queues at its top and the recently merged workspaces
as an area below them, and the toggle goes. Its header count is the merges in
the queue, where it was "(K today)". While a queue is paused the header goes
from the merge purple to the paused amber, as the retired toggle's glyph did.

## Landed changes

### `frontend.v1.RosterMergedSection.queues` / `frontend.v1.RosterMergeQueueRepoGroup`

- WHAT: one group per repository whose queue has a head or merges waiting,
  in the repository sections' order: the repository's key, its label header
  (`frontend.v1.RosterMergedRepoGroupHeader`, shared with the merged groups)
  and its `frontend.v1.RosterRepoMergeQueue`.
- WHY: the queues are drawn in the section, so the section carries them.
- A queue group follows no fold: a merge in flight is drawn whichever way its
  repository is folded.
- A PAUSED QUEUE HOLDING NOTHING IS NOT LISTED. The daemon places a queue under
  a repository by its merges' workspaces (queues are keyed by git common dir,
  which only the merge orchestrator resolves), so an empty queue has no
  repository to be drawn under. The pause reads from the header instead.

### `frontend.v1.RosterMergedSection.merged_label`

- WHAT: the "Recently Merged" heading over the merged groups, always set.
- WHY: below a queue group's repository label, a merged group's repository
  label alone would not say which area it heads.

### `frontend.v1.RosterMergedSection.merge_queue_paused` / `frontend.v1.RosterMergedSectionMergeQueuePaused`

- WHAT: the marker the retired `frontend.v1.RosterOpenSection.merge_queue_paused`
  was, moved to the section whose header it now colors.
- WHY: the header is drawn amber while any queue is paused, folded or not.

### `frontend.v1.RosterMergedSection.header.count`

- WHAT: now the merges in line over every listed queue (a failed head is in no
  line). It was the shown merged rows.
- WHY: the owner asked for the queue's count in place of "(K today)". The
  sidebar draws it folded or not.

### Retired

- `frontend.v1.RosterOpenSection` fields 4, 5 and 6 (`shows`,
  `merge_queue_paused`) and their messages.
- `frontend.v1.RosterRepoSection` fields 10 and 11 (`shows`) and
  `frontend.v1.RosterRepoShowsWorkspaces`: a repository section's body is
  always its entries.
- `agentrepl.v1.UpdateSidebarViewRequest.show_open` (field 3) and its
  messages, with the daemon verb behind it. The state store keeps the
  `sidebar_view.open_shows_merge_queues` column, unread: dropping it is a
  breaking migration, left for its own deploy.

## The follow-up (owner requests, 2026-10-27)

The owner saw the header amber while only another repository's queue was
paused, and asked for the pause, the count and the stopped work to be drawn on
each repository's own queue, for a waiting merge to be dragged to a place, and
for a repository to leave the section once its line empties.

### `frontend.v1.RosterRepoMergeQueue.paused` / `frontend.v1.RosterMergeQueuePaused`

- WHAT: the pause now carries its record: `paused_at_ms`, `by`
  (`frontend.v1.RosterMergeQueuePauser`: the user, or a workspace by name) and
  `hard`. The two are unset for a pause recorded before the daemon kept them
  (the state store's layout 83 added the columns).
- WHY: the label's hover says who paused the queue and when, and the head's
  clock stops at the pause.

### `frontend.v1.RosterMergeQueueHeadMerging.suspended`

- WHAT: present while the queue is paused and no turn of the merge's step
  runs: nothing of the head's work runs.
- WHY: a pause suspends a merge's own steps but leaves a step's agent turn
  running until a hard stop interrupts it, so only the daemon knows whether the
  head's live stage still runs. The sidebar stills the stage's dot when it does
  not.

### `frontend.v1.RosterMergeQueueRepoGroup.count`

- WHAT: the merges in the group's line. The section's
  `frontend.v1.RosterMergedSection.header` now carries no count.
- WHY: the owner asked for the count per repository.

### `agentrepl.v1.UpdateMergeQueueRequest.move_to` / `agentrepl.v1.UpdateMergeQueueMoveTo`

- WHAT: move a waiting merge directly ahead of or behind another waiting merge
  on its queue, taking that merge's front mark.
- WHY: the sidebar's drag drops a merge at any place, which one-place moves
  could only reach by several requests that another change could interleave.

### Retired

- `frontend.v1.RosterMergedSection.merge_queue_paused` (field 8) and its
  message: a pause is drawn on its own repository.
- `frontend.v1.RosterMergeQueueHead.failed` (field 5) and
  `frontend.v1.RosterMergeQueueHeadFailed`: a queue whose line is empty is not
  listed, so a failed merge no longer stands at its head. The merge's failure
  stays on its bubble and its workspace's footer.

## Follow-up: the Deploys section (owner request, 2026-10-27)

The section is renamed and moved: "Merge Queue" is now "Deploys", its merged
groups' heading "Recently Deployed", and it is drawn directly under the Open
section, above Hidden. The message keeps its name
(`frontend.v1.RosterMergedSection`); only the labels the daemon composes change.

### `frontend.v1.RosterMergedSection.queue_label`

> RETIRED 2026-10-31 (owner request): tag 9 is reserved, and the queues stand
> directly under the section's header with no heading of their own.

- WHAT: the heading over the queues, "Deploy Queue". Always set, and drawn
  even while no queue is listed, so the section always draws.
- WHY: the owner asked for an empty queue to read as empty rather than
  absent. The heading has one author, the daemon, as every other drawn string.

### The band fit

- The heading over no queue is a line of its own, so it claims a row of the
  section's share.
- A rail too short for the bands' share shrinks Hidden first, down to no rows,
  and only then Deploys. Spare share and room the Open section leaves go to
  Deploys' queue and today's merges first, then Hidden, then older merges.
- Hidden is now the lowest section, so it is the one stretched to the rail's
  foot.

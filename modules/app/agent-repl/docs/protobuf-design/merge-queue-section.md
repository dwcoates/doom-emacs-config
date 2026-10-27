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

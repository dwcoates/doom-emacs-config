# Sidebar roster streams

Every roster push was the whole `frontend.v1.WorkspaceRoster`: 1,154 rows per
render, 657 of them landed workspaces in Recently Merged, pushed about 30 times
a minute between deploys and up to 140 a minute during one. The sidebar page
took a median 41ms from push to next frame (p90 98ms, max 779ms) across about
800 `.row` elements, so the rail lagged under load (measured 2026-11-16). The
owner settled the fix the same day: split the roster into independent snapshot
streams, never diffs; take merged workspaces out of the roster entirely and
page them on demand; and make the merge queue one stream whose waiting members
carry only identity and order.

Design ran autonomously (no co-design was asked for).

## Core design principles

### Never diffs: every message is a complete snapshot of its stream

- **Principle (owner, 2026-11-16).** Every message on every stream is a
  complete snapshot of that stream, replacing the client's copy whole. A
  client must never be able to drift from the daemon. Each stream reconnects
  to a full snapshot.
- **Consequences.** `frontend.v1.RosterPush` carries each stream as an
  optional whole snapshot; an absent stream means unchanged, never empty. The
  first push of every subscription carries every stream. There is no row-level
  add, remove or patch message anywhere.
- **Reopens.** `frontend.v1.WorkspaceRoster` as the one whole-roster message;
  `agentrepl.v1.WatchWorkspaceRosterResponse.roster`.
- **Does not claim.** It does not forbid the webapp keeping DOM for a section
  whose snapshot is equal to the one it drew (`keepOrDraw`): that is drawing,
  not wire.

### Merged workspaces leave the roster

- **Principle (owner).** Merged workspaces are finished and conceptually no
  longer in the roster. The roster keeps only metadata about them.
- **Consequences.** `frontend.v1.RosterMergedRepoGroup.merged_rows` is
  retired; the group carries `frontend.v1.RosterMergedRepoGroupSummary`. Rows
  come from `agentrepl.v1.ListMergedWorkspaces`. Every daemon and editor reader
  of merged rows moves to that request or to the registry.
- **Does not claim.** Closed workspaces that never merged are not "finished"
  in this sense: they stay on a stream (the closed stream), because the editor
  reopens and routes them.

### A workspace is never drawn in two places, nor in none, across a transition

- **Principle (owner).** The owner requires this to be impossible, not merely
  unlikely.
- **Consequences.** See "The cross-stream invariant" below: impossible by
  construction for the four streams, and impossible by construction for the
  merged pages given the draw rule the contract states on
  `frontend.v1.RosterMergedSection.merged_revision`.

## Landed changes

### ITERATION-SEQUENCE

The default sequence, walked alone: transport (unchanged: the existing
`WatchWorkspaceRoster` server stream and the page mux
`agentrepl.v1.PageFrame`, which wraps the response unchanged) → endpoint
inventory (the stream's response, one new unary rpc) → conventions (the
package's `oneof result { success; error; }`, kept) → shapes, the stream
container first, then the merged section, then the new endpoint.

### Assumptions (stated, not asked)

- Objective: one small change no longer resends and redraws every workspace's
  row.
- Frontend: yes; figma→idl governs. The drawn layout of the rail does not
  change, so no new layout drawing is needed: the Recently Merged section is
  drawn as before, its rows coming from pages instead of the stream.
- Systems: daemon (sidebar resolver, stream server, rollout mirror, composer
  mentions, workspace find, agent CLI), webapp (sidebar), Emacs (wire decode,
  roster, switcher, verbs, log retention, attention sync).
- Non-additive: `WorkspaceRoster.recently_merged`, `.merge_queue`, `.closed`
  and `.revision` leave the message; `RosterMergedRepoGroup.merged_rows`
  leaves; `WatchWorkspaceRosterResponse.roster` leaves.

### 1. The roster becomes four snapshot streams on one push

**What.** New `frontend.v1.RosterPush { revision, optional workspaces,
optional merge_queue, optional recently_merged, optional closed }`.
`agentrepl.v1.WatchWorkspaceRosterResponse.push` gains `streams` (tag 3) and
retires `roster` (tag 1). `frontend.v1.WorkspaceRoster` keeps the open
repository view, backlog, open-section heading, selection, version, app
switcher, threads and cold gates, and retires `recently_merged` (3),
`merge_queue` (19), `closed` (14) and `revision` (11).

**Why.** The measured push drivers were the merge queue moving
(`daemon.sidebar.set_merge`, 74 in three minutes), a row's status
(`footer_status_changed`, 29), a repository's divergence (20) and the
selection (12). Each of those now resends one stream. The queue's moves never
touch the workspaces stream; a status edge never resends the merge queue, the
merged summary or the closed rows.

**The closed stream is this design's own addition.** The owner named the
queue and the merged band; closed, never-merged rows were in every push too,
undrawn by the webapp but decoded by it and by Emacs on every one. They change
only when a workspace closes or reopens, so they are a stream of their own.

**Why the revision moved to the push.** Verbs answer a revision
(`agentrepl.v1.OpenTabsOpenedReady.roster_revision`,
`SelectWorkspaceSuccess`, `SetWorkspaceNameSuccess`, ...) meaning "every
roster at or above it carries the change". A per-stream revision would break
that: a push at R that leaves `workspaces` out (unchanged) would leave a
client's workspaces revision below R forever. The push revision is the one a
client compares; a client holding every stream of push R holds the whole
roster as of R.

**Consequences.**

- The topic must carry the WHOLE state, not partial pushes: `publish.Topic`
  hands a new subscriber the latest value first, so a partial latest value
  would hand a late subscriber an incomplete roster. The resolver publishes a
  full `RosterPush` (every stream set), reusing the SAME message pointer for
  every stream whose snapshot is `proto.Equal` to the last one. The stream
  server (`daemon/internal/server/streams.go` `watchWorkspaceRoster`) keeps,
  per subscription, the pointer of each stream it last sent and leaves a
  stream out of the wire push when the pointer is unchanged. So filtering is
  per subscription, and a coalesced or late subscriber is still sent every
  stream it has not seen.
- The revision rises only when some stream differs (as today, when the whole
  roster differed).
- `sidebar.Resolver.Row(ws)` and `Revision()` answer the push revision.

### 2. The cross-stream invariant

**Between streams: impossible by construction.** Every stream is resolved by
one render under the resolver's one lock and published as one value, so a
fact that moves a workspace between streams changes all of them in one push.
The client applies every snapshot a push carries before drawing any. There is
no order of arrival in which a client holds one side of a transition and not
the other, because both sides travel in one frame on one ordered stream.

- A queued workspace is legitimately in two streams at once: its open row
  (workspaces) and its queue entry (merge_queue) mean different things, and
  both change in the same push when it joins or leaves the queue.

**Between the merged summary and the merged pages: impossible by construction,
by the draw rule.** Pages arrive over a separate request, so co-delivery
cannot cover them. Instead:

- `frontend.v1.RosterMergedSection.merged_revision` is a
  `frontend.v1.RosterMergedRevision`: the push revision in which the SET of
  merged workspaces last changed. Every page answer carries the same token
  (`agentrepl.v1.ListMergedWorkspacesSuccess.revision`).
- The set changes only in a push that also moves the workspace out of (merge
  landing) or into (merged workspace reopened) the workspaces stream.
- The page draws its loaded pages only when every one carries the summary's
  token. When a push raises the token while pages are loaded, the page holds
  that push undrawn (every stream of it), fetches its loaded pages again, and
  draws the push and the pages together once they agree. A page whose token
  is newer than the summary's waits for the push that raises it (the push is
  published in the same resolve that changed the set).
- So the drawn state is always (every stream at push P) plus (pages resolved
  from the same merged set as P): a workspace is in exactly one of open and
  merged in every drawn frame. While the section is folded no page is loaded,
  so no hold happens: the workspace leaves the open section and the count
  rises in one push.
- THE HOLD'S END IS STRUCTURAL, so it carries no timer of its own. A page
  token newer than the summary's can only come from a set the resolver
  changed under its lock, and the push carrying that set was enqueued on the
  topic under the same lock before any page could be resolved from it; the
  push therefore reaches every live subscription. A subscription that dies
  first resets the page's copy of every stream. The refetch is a separate
  wait and carries its own deadline, sized from the measured page latency
  (`webapp/src/sidebar/merged-pages.ts`). CORRECTION (2026-11-16): an earlier
  revision of this paragraph said the webapp's request client bounds the
  refetch; it does not (`webapp/src/rpc/unary.ts` `callUnary` sets no
  deadline). The claim was asserted without checking that function. `sidebar.merged-pages.hold-released`
  records `held_ms`, the measurement a later bound would be sized from.
- A failed refetch is logged through the page's canonical logger and the
  held push is then drawn with the merged pages unloaded, the section showing
  its count (the "partly loaded" state the contract names). That is the
  honest state, never a stale page.

**Rejected: per-row diffs on the merged band.** Owner rejected diffs.
**Rejected: pinning pages to a push revision on the daemon.** The daemon
would have to keep historical merged sets; the token identifies a set, and
equality is all the client needs.

### 3. The Recently Merged stream is a summary

**What.** `RosterMergedRepoGroup.merged_rows` (6) retired; added
`RosterMergedRepoGroup.summary` (9): `RosterMergedRepoGroupSummary { count,
latest, repeated recent }`, with `RosterMergedRepoGroupCount { merges }`,
`RosterMergedRepoGroupLatest { merged_at_ms }`,
`RosterMergedRepoGroupRecentMerge { merged_at_ms, optional commits }`.
`RosterMergedSection.header.count` is now SET: every merged workspace of every
group. Added `RosterMergedSection.merged_revision` (12; tags 10 and 11 are the section's `height` oneof) and
`RosterMergedRevision`.

**Why this shape.**

- The owner's metadata list: the section's counter (shown folded or partly
  loaded), the merged-today count, whether each group has older merges. The
  count answers "older merges" (count above what is loaded) and the page
  answer's `extent` says whether to fetch on.
- "Today" is the page's LOCAL date and ticks at midnight with no push (the
  existing `mergedTodayCountText` and `mergedTodayCommitsText` behavior, and
  the green names). A daemon-resolved integer would go stale at midnight. So
  the summary carries every merge of the 24 hours before it was resolved:
  today's merges at any later instant are always within it. Each carries its
  commits so the group hover's "N commits merged today" still sums exactly
  the merges the count names.
- The group header's "last D ago" needs the latest merge even when none
  landed in the last day: `latest`.

**Consequences.**

- `webapp/src/sidebar/roster.ts` `mergedGroupFacts`, `drawMergedTodayCount`,
  `tickGroupHover` read the summary, not rows.
- `webapp/src/sidebar/band-fit.ts` `mergedTodayLines` keeps reading the
  loaded rows' `data-merged-at`: the band fit sizes what is drawn.
- `daemon/internal/resolve/sidebar/sections.go` `mergedSection` stops
  building rows; `merged_window.go` `recentMerges` serves the pages and the
  summary.

### 4. The merge queue is its own stream; position lives in it

**What.** `RosterMergeQueueSection` is the merge queue stream's snapshot,
unchanged in shape. Its comments now state the split.

**Why no shape change.** The section was already split the way the owner
asked: the active members (`RosterMergeQueueHead`,
`RosterMergeQueueFixingTests`, `RosterMergeQueueResolvingConflicts`) carry
their full detail, and `RosterMergeQueueWaiting` carries the subject (who the
merge is) and the instant it joined the line; its order is its place in the
list. No row carries a queue position: `RosterRowStatusMergeQueued` is empty,
and nothing in `daemon/internal/resolve/sidebar` reads `MergeFacts.QueuePlace`
(verified by grep, 2026-11-16). The 28 re-stamps per queue move were full
re-renders of the whole roster that the topic then pushed whole; with the
queue its own stream and per-stream equality, a re-stamp that changes nothing
pushes nothing, and a move pushes the queue stream alone.

**Agreed with `merge-queue-active-clock` (2026-11-16, owner instruction to align).** That workspace owns every merge-queue clock field, and it rebases onto this branch, which lands first (the owner's front-of-queue instruction): `frontend.v1.RosterMergeQueueActiveClock`, `RosterMergeQueueWaitingClock` and `RosterMergeQueueHeadClock`, carried on the head, the waiting, fixing-tests, resolving-conflicts and awaiting members, retiring `queued_at_ms`, `since_ms` and `head_since_ms`. This branch adds no merge-queue field; that workspace carries its clock fields and their drawing into this branch's merge queue stream. A waiting member's clock is fixed at its last transition, so it stays identity and order plus facts that a move never changes. The paragraph below is superseded by that agreement and kept for the record.

**`queued_at_ms` is kept on waiting members.** It is the queue entry's
durable instant, which no move changes, and the waiting line's clock ticks
from it. Removing it would remove a drawn clock, a look-and-feel change the
owner has not asked for. If the owner reads "identity and order only" as
excluding it, that is a one-field removal plus the clock's removal from
`webapp/src/sidebar/merge-queue-view.ts` `drawRosterMergeQueueWaiting`.

### 5. `agentrepl.v1.ListMergedWorkspaces`

**What.** New `endpoint_list_merged_workspaces.proto`; service entry in the
SIDEBAR section. Request: `optional repository` (unset = every repository),
`order` (`most_recently_merged` | `most_recently_created`), `page` (0-based,
ten per page). Response: `success { revision, oneof extent { last { rows },
more { rows, omitted } } }` | `error { unknown_repository | not_ready }`.

**Why.**

- Ten per page, fixed by the daemon, as the owner set.
- Unset repository serves the editor's switcher, which wants the most
  recently merged across every repository.
- The completeness oneof per the counts rule: the partial arm states what is
  left (`omitted`).
- `not_ready` is a real runtime condition (before the first registry is
  installed the resolver holds no merged set); it is an error, not an empty
  page, because an empty page would claim "no merges".
- A page past the end is a `last` with no rows: an answer, not a failure.

### 6. Readers re-sourced

| Reader | Was | Now |
|---|---|---|
| `daemon/internal/workspacefind/roster.go` `Placements` | `rosterwalk.MergedRows` | merged placements from the registry's merged set, status from the row a page resolves |
| `daemon/internal/workspacefind/locate.go` `Locate` | `rosterwalk.AllRows` (drawn = open, backlogged, merged, closed) | open and backlogged from the workspaces stream, closed from the closed stream, merged from the registry (closed with `MergedAt`, not a thread, not merge-hidden) |
| `daemon/internal/resolve/sidebar/rowof.go` `findRow` / `Row` | `rosterwalk.AllRows` of the last roster | the last push's streams, and a merged workspace's row resolved on demand |
| `daemon/internal/composermentions/roster.go` `RosterNames` | open + backlogged of the roster | the same, off the workspaces stream (merged were never read) |
| `daemon/internal/rostermirror/mirror.go` | every row of each roster | holds the latest snapshot of each stream it is sent and reads open, backlogged and closed; merged workspaces are never served, so never mirrored |
| `daemon/internal/server/workspace_mentions.go`, `daemon/cmd/agent-repld/agentcli.go` | `rosterwalk.OpenRows(roster)` | the workspaces stream |
| `proto/helpers/go/rosterwalk` `MergedRows`, `AllRows` | merged rows of the roster | `MergedRows` removed; `AllRows` walks a push's streams |
| Emacs `lisp/roster.el` `agent-repl-roster-walk` | merged band rows walked | not walked; the closed stream walked |
| Emacs switcher `lisp/commands.el` (`SPC p p`, `SPC p P`) | merged rows off the roster | `ListMergedWorkspaces`, every repository, most recently merged: the first three pages for the 25 most recent, every page for `SPC p P` |
| Emacs verbs name lookups (`lisp/verbs.el` `agent-repl-verbs--all-rows`) | every walked row, merged included | streams first, then the merged pages |
| Emacs log retention (`lisp/core.el` `agent-repl--roster-workspace-dirs`) | every walked row's dir, merged included | streams' dirs plus every merged page's dirs |

### 7. The webapp's merged band loads and unloads for real

Replaces `webapp/src/sidebar/deploys-more.ts`'s page-local hide/show:

- Folded section: no page is loaded; every loaded page is released at the
  fold.
- Unfolded: each group's page 0 is fetched.
- A group's end scrolled into view: its next page is fetched (while the last
  answer was `more`).
- A loaded page lying wholly below the band's viewport: released, the last
  first.
- A group folded on the page: its pages are released; unfolded, page 0 again.
- A summary token change with pages loaded: the hold-and-refetch of section 2.

Per-stream draw coalescing: the page keeps the latest snapshot of each stream
and draws once per frame, drawing only the streams whose snapshot changed
since its last draw.

## Consequences recap

- One push carries only changed streams; the first carries all.
- The topic carries whole state; filtering is per subscription in the server.
- The revision is the push's; every verb's revision compares against it.
- Merged rows are on no stream; a page request serves them, ten at a time.
- The page draws merged pages only when their token equals the summary's;
  a token change holds the push until the pages agree.
- The closed rows are a stream of their own (added beyond the owner's list).
- Waiting queue members keep their join instant (owner may rule otherwise).
- Every former reader of merged rows moves to the request or the registry.

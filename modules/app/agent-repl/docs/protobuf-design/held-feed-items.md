# Held feed items

The feed's bottom moves while the feed updates. The clearest case is a live
merge bubble: growing at the tail, it slides the feed's bottom under the
footer, because the scroll owner refuses to chase a merge bubble's growth
(`TailFollow.holdForExempt`, webapp/src/scroll.ts), and the next unrelated
update snaps the view back. Held prompts already avoid this, because they
draw in their own region at the scroll content's tail (`#hold-tray`), where
their growth is ordinary content growth that a standing follow keeps in view.

The owner's direction (2026-10-10): items that are live and not yet settled
into the conversation's history are HELD. They sit in ONE section of the feed
at its tail, drawn exactly like the newest rows, and the daemon decides what
is held. A held item growing pushes the feed up, and never scrolls it. When an
item settles (a held prompt is delivered, a merge lands), it takes its feed
key at that moment, which is the bottom, so moving into the feed changes
nothing the reader sees. The daemon folding a merge bubble when its merge
lands remains the only automatic open-to-folded change. Held prompts become
held feed items in the same section, rather than a separate tray on a separate
stream.

## Process

- 2026-10-10, owner: the owner reviews the ASCII layouts of the UI only. The
  protobuf shapes are designed and landed by the orchestrator afterwards,
  without putting them to the owner, checked box-for-box against the agreed
  layouts and against this skill's design principles. A question that only a
  protobuf could raise is put to the owner as a question about the UI or the
  behavior, never as a sketch.

## Landed changes

### 2026-10-10 — the layout (owner agreed layout A)

The owner agreed this layout. The held section has no heading and no divider,
so it reads as the feed's newest rows:

```text
┌─ #feed-scroll ───────────────────────────────────────────────────┐
│ ┌─ settled rows ───────────────────────────────────────────────┐ │
│ │  [prompt]    "land it"                                       │ │
│ │  [response]  "Queued the merge…"                             │ │
│ └──────────────────────────────────────────────────────────────┘ │
│ ┌─ held section ───────────────────────────────────────────────┐ │
│ │ ┌─ merge bubble (open) ────────────────────────────────────┐ │ │
│ │ │ ● merging flaky-feed-jitter                    3m 12s    │ │ │
│ │ │ [queue ✓][rebasing ✓][tests ●]                           │ │ │
│ │ └──────────────────────────────────────────────────────────┘ │ │
│ │ ┌─ held prompt ────────────────────────────────────────────┐ │ │
│ │ │ "also fix the footer"     ⏳ held · merge running        │ │ │
│ │ └──────────────────────────────────────────────────────────┘ │ │
│ └──────────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────────┘
┌─ #footer ────────────────────────────────────────────────────────┐
```

When the merge lands it folds and takes its feed key at that moment, which is
the bottom of the settled rows, so on screen only the fold is seen. Every
decision below was made by the orchestrator under the Process rule, and the
owner was not asked about protobuf shapes.

### D1 — the held section rides the ROOT feed's own connection

- WHAT: `agentrepl.v1.OpenFeedSuccess` gains the held section (set for the root
  feed only), and `agentrepl.v1.WatchFeedResponse` gains a held-section frame.
  `WatchDaemonHolds`, its two `WatchPage` multiplex arms, `DaemonHoldTray` and
  `DaemonHoldItem` are retired.
- WHY: the owner wants one section of the FEED for held items, and a settle
  (an item leaving the section and becoming a row) must reach the page as ONE
  change. On two streams the page can paint a frame with the item in both
  places or in neither; on the root feed's publication log, under the same
  watch-token pin as the rows, that frame is unrepresentable.
- CONSEQUENCES: the feed resolver's publication log carries two kinds of
  publication (a row, or a held-section change), and a tail replays both in
  order. The holds resolver's state and publication move into the feed
  resolver; its rendering of a held prompt and a held offer stays as it is.

### D2 — what is held: merges, prompts, offers

- A merge bubble is held while its merge is live (queued or running), decided
  by the feed resolver from the merge head row's own result arm. A terminal
  head (success or error) settles it.
- A held prompt is held from the moment the prompt queue holds it until its
  acceptance mirror is drawn (the `mirrorAccepted` call in
  daemon/internal/promptqueue/deliver.go and join.go), which settles it. A
  mirror taken back down (a delivery refused for lack of a session) un-settles
  it in the same publication that removes the row.
- A held offer is held while it is posed. It never settles: answering or
  withdrawing it removes it. The orchestrator placed offers here because the
  tray already drew them at the feed's tail, so nothing about where an offer
  appears changes (the owner did not rule on offers).

### D3 — a settle is one publication

- The held-section frame carries the section whole AND every row that settled
  out of it in that change. A settled row rides only inside that frame, never
  as a separate row frame, and is keyed at the moment it settles, which is the
  bottom of the feed.
- A prompt whose turn draws at a non-root address (a merge's own turn draws in
  its tab) settles out of the root's section in a frame with no settled row,
  and its row is published on the feed it draws in.

### D4 — a held item's identity is the FeedId its settled row will carry

- A merge's held item carries the merge head row's FeedId, a prompt's carries
  the FeedId its prompt row will carry, and an offer's is minted from the offer
  kind. The webapp therefore keeps one element per item across the settle:
  an open merge bubble keeps its sub-feed and its state when it moves into the
  rows.

### D5 — order within the section

- Merges first, then prompts in queue order, then the offer. A prompt held
  while a merge runs is delivered after the merge, so the item that settles
  first sits nearest the settled rows.

### D6 — the webapp

- The section is drawn after the rows inside the scroll content, with no
  visual boundary. Items are reconciled by id in place: an unchanged item keeps
  its element, a merge updates its bubble in place, and a settled item's element
  MOVES into the rows. The `TailFollow` exemption for merge bubbles is deleted:
  a held item growing is ordinary content growth, which a standing follow keeps
  in view (owner ruling 2026-10-10, superseding 2026-10-08: a merge bubble's
  growth pushes the feed up and never scrolls it).

### P1 — the protobufs (landed)

- `frontend/v1/daemon_hold.proto` is renamed `frontend/v1/feed_held.proto`.
  `DaemonHoldTray` and `DaemonHoldItem` are replaced by `FeedHeld` (the
  section, whole) and `FeedHeldItem` (a FeedId plus a `merge` | `prompt` |
  `offer` arm). `HeldPrompt`, `HeldOffer` and everything under them keep their
  shapes; only their comments now say "held section" for "tray".
- The merge arm is `frontend.v1.FeedMerge`, the same message the settled row's
  `activity.merge` carries, so one renderer draws both. It is not a whole
  `FeedRow`, because a held item has no order key yet and `FeedRow.order` is
  required on every row.
- `agentrepl.v1.WatchFeedResponse.held` (tag 4) carries
  `agentrepl.v1.WatchFeedHeldChange { FeedHeld held; repeated FeedRow rows; }`,
  declared in the endpoint file because only WatchFeed carries it.
- `agentrepl.v1.OpenFeedSuccess.held` (tag 3) is set for the root feed only.
- `WatchDaemonHolds`, `endpoint_watch_daemon_holds.proto`, and the page
  multiplex arms `PageFrame.holds` (7) and `SubscribePageRequest.holds` (8)
  are retired; the tags are reserved.
- CONSEQUENCES: every consumer of `daemon_hold_pb` / `DaemonHoldTray` breaks:
  the daemon's holds resolver, server streams and page multiplex, the
  integration harness's `WatchHolds`, the Emacs fake daemon's refusing
  handler, and the webapp's tray. Obviated-declaration sweep: nothing else
  imports the deleted file, and no surviving message references
  `DaemonHoldTray` or `DaemonHoldItem`.

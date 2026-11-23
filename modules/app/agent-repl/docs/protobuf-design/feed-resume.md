# A handover costs the viewed page one attach, and every other page nothing

At the handover deploy of 2026-11-23 15:35, 45 web pages redrew their feeds
(1,746 `feed.draw-*` records, 57 `feed.open-asked`), the one web process's
main thread was blocked 15.6s in 41s, and the viewed workspace was unusable
for 22.6s. Twelve pages attached and opened their feed twice: once when the
page adopted its workspace (`rpc.context-rehome`, relayed through the
incumbent) and again when the incumbent exited. Every reattach replaced the
feed whole, and pages nobody was looking at paid the same as the shown one.

Design mode: autonomous. Landing: one landing.

## Assumptions

- Objective: a handover moves the viewed page once, at the promotion, with no
  redraw when nothing changed; a page that is not shown does nothing at all.
- Systems: the daemon (`daemon/internal/resolve/feed` stamps revisions,
  `daemon/internal/server` answers a resumed open and ends page streams at the
  yield, `daemon/internal/rollout` calls it), the webapp (`webapp/src/rpc`,
  `webapp/src/lifecycle`, `webapp/src/feed`), the test fakes.
- Frontend: no drawn element changes. A row is drawn as before; only how many
  times it is drawn changes.
- Non-additive: none in the contract (every field is new and optional). In
  behavior, the page no longer re-homes its streams when it adopts its
  workspace, and a page stream on the incumbent ends at the yield rather than
  at the incumbent's exit.
- The landing deploy's own handover runs the previous build's pages and
  daemon, which carry none of this; the first handover that runs it is the
  deploy after.

## Core design principles

- A page moves to the successor once, at the promotion.
  - Forces: the page keeps its incumbent streams through the adoption; the
    incumbent ends every page stream with its planned ending once the
    successor accepts on the shared address (after a successful yield), so the
    one reattach lands on the successor; subscriptions ended by a planned
    page ending reopen as planned, filing nothing.
  - Does not claim: that the incumbent's streams carry live data between the
    workspace's adoption and the yield. They go quiet for that span (the
    viewed workspace of 2026-11-23 was adopted 0.3s before the yield); the
    resume at the reattach catches the page up. Emacs's host streams make the
    same trade (`elisp.host.stream-move-deferred`).
- A reattach resumes, never redraws.
  - Forces: every row carries the daemon's revision; a reopened feed names the
    rows it holds at their revisions; the daemon sends only rows that differ;
    the reader upserts them, and its upsert draws nothing for a row equal to
    the one it holds.
- A page that is not shown holds nothing after its stream ends, and reattaches
  when it is shown.

## Decision: hidden pages are quiet after their stream ends, not always

The page re-attaches only while it is seen (`page-seen.ts`). A page attached
when it is hidden stays attached; once its stream ends (a handover, a restart,
a crash) it does not reopen it, nor open its feed, until a reader sees it.
Its first attach at boot is unconditional, so a preloaded tab still loads.

Why not "always quiet" (detach on every hide):

- The measured cost was the reattach burst, which the stream-end rule removes
  entirely: at a handover no hidden page attaches, subscribes or opens a feed.
- Detaching on every hide would make every workspace switch pay seven
  unsubscribes, an attach, seven subscribes and a resumed open on the daemon,
  and would leave a page that was hidden for a second showing stale state
  until its catch-up lands. Switches are the most frequent act in the editor.
- An attached hidden page costs a stream that is idle unless its workspace
  changes; the steady state was never the problem the evidence shows.

What the user sees: after a deploy, a page they switch to shows what it held
at once, then any rows that arrived meanwhile appear. Nothing else changes.

## Landed changes

### `frontend.v1.FeedRow.revision` (`frontend.v1.FeedRowRevision`)

- What: every published row carries a digest of its other fields, minted by
  the daemon at its one write path (`resolver.upsert`).
- Why: a reader must be able to say which publication of a row it holds, so a
  resumed open can leave out rows that did not change. A typed echo token,
  because the reader hands it back and never builds one.
- Consequences: the digest is a deterministic marshal of the row with the
  revision cleared, hashed. Two daemons publish equal revisions for equal
  rows; a row whose order key a successor recomputes differently carries a
  different revision and is sent again, and the reader re-places it at the new
  key (`feed.resume-row-rekeyed`) rather than recording an order error.
- Why "the reader compares rows itself" lost on its own: it keeps the redraw
  impossible but still ships and decodes the whole page on the main thread for
  every reattach. The reader's equality check stays as the second guarantee.

### `agentrepl.v1.OpenFeedRequest.resume` and `agentrepl.v1.OpenFeedSuccess.resumed`

- What: a reopen names the rows it holds (`FeedResume`); the answer's `page`
  then holds only new or changed rows, and `resumed.standing` names every row
  of the newest page so the reader can drop held rows that left.
- Why "tell the daemon the newest row held" lost: rows change in place (an
  interim response settles, a fold changes), so "newer than row X" misses
  changes to rows the reader already holds.
- Consequences: the reader keeps rows older than the newest page as it holds
  them; a held row older than that window that changed while the page was
  quiet stays as drawn until it is pushed again. Accepted: such changes are
  rare, and a replace lost those rows altogether.

### The page stream ends at the yield (no contract change)

- What: after a successful `daemonaddr.claim.Yield()`, the incumbent ends
  every page stream it holds as its planned exit does: each subscription's
  last frame, then the page's `ending` (`server.EndPageStreams`, called from
  `rollout.controller.yieldToSuccessor`).
- Why: the incumbent may live on long after the yield serving workspaces still
  busy, and a page held there would stay on quiet streams until it exited.
  The yield is the promotion: from then on a reopen can only reach the
  successor, because the incumbent accepts nothing on the shared address.
- Reverses a dropped item of `page-attachment-handover.md` ("Ending every page
  stream at the claim yield"), which lost because it would strand pages if the
  yield failed. It runs only after a yield that succeeded.
- `DaemonStreamEnding` is not given the successor's name: the page can dial
  only its own origin, so the name would change nothing it does.

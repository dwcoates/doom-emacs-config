# Client-anchored feed paging, for unshown pages that free their rows

Every workspace page lives in one WebKit process, and a page the host does not
show kept every row it ever drew, with each pushed row appended to it. The owner
ruled on 2026-10-15 that two properties hold instead:

- A page the host stops showing frees every row older than its newest daemon
  page (the store's page of 50 entries), whatever the reader's scroll position;
  a switch back lands at the tail (the 2026-10-02 switch ruling), so the freed
  history is never on screen on return. Each switch away trims again; there is
  no re-trim while a page stays hidden, and a long-hidden busy workspace grows
  until it is next visited and left (accepted).
- A hidden page keeps its stream open and draws every update as it arrives, so
  a switch back finds everything already there.

The trim runs on the hidden page after the switch, never blocking it. Keeping a
scrolled-up reader's place across a switch was considered and dropped: it
contradicts the switch ruling and adds restore state for little benefit.

Freeing rows breaks the daemon's walk, which held its own position: a `next`
after a free would continue below rows the reader no longer holds, leaving a
hole. This change moves the walk's position to the reader.

## Terms

- A SCREEN is one visible section of the feed.
- A DAEMON PAGE is one store page of feed content, 50 entries.

## Core design principles

### The reader's oldest held row is the walk's position

- In the owner's terms: the page asks for "the page older than this row", so the
  older walk is anchored to a row the page names.
- Consequences for the contract: an older page is asked for by the order of the
  reader's oldest held row; the daemon keeps no walk position and no walk
  identity; LoadFeedThrough starts from the same anchor.
- Reopens: `agentrepl.v1.GetFeedPageNext`, `frontend.v1.FeedWalkId`,
  `frontend.v1.FeedPageHasMore.walk`, `agentrepl.v1.GetFeedPageError.no_walk_standing`,
  `agentrepl.v1.LoadFeedThroughRequest.walk`.
- Does not claim: anything about the tail (WatchFeed and its token are
  unchanged), or about which rows a page keeps (that is the webapp's rule above).

## Landed changes

### 1. GetFeedPage asks for the page older than the reader's oldest held row (2026-10-15)

- WHAT: `agentrepl.v1.GetFeedPageRequest.page` gains `older_than`
  (`GetFeedPageOlderThan`, carrying the oldest held row's `frontend.v1.FeedRowOrder`).
  `next` (4) and `GetFeedPageNext` are deleted and reserved;
  `GetFeedPageError.no_walk_standing` (5) and its message are deleted and
  reserved.
- WHY: an unshown page frees its older rows, and only an anchor the reader
  names can be right after a free. A daemon-held position can only be made
  unlikely to drift; a reader-named one cannot drift at all.
- CONSEQUENCES: the daemon's per-reader walk table (`readers`,
  `maxStandingWalks`, `CloseReader`, the `ErrNoWalk` refusal) has no reader left
  and goes. The daemon decides an older page by key order alone
  (`daemon/internal/resolve/feed/pages.go`, `walk.end` and `olderThan` already
  work from a key), so the named row need not be held by the daemon. A
  `no_source` answer serves nothing older while claiming more, and the reader
  asks again with the same anchor.

### 2. FeedWalkId and FeedPageHasMore.walk are deleted (2026-10-15)

- WHAT: `frontend.v1.FeedWalkId` is deleted; `FeedPageHasMore.walk` (1) is
  reserved, so `FeedPageHasMore` is an empty edge arm.
- WHY: with the reader's row as the position, no walk identity has a use.
- CONSEQUENCES: the webapp stops holding a walk per bubble
  (`webapp/src/feed/feed-view.ts`), and asks with its oldest held row instead.

### 3. LoadFeedThrough starts from the reader's oldest held row (2026-10-15)

- WHAT: `agentrepl.v1.LoadFeedThroughRequest.walk` (3) is reserved and replaced
  by `optional frontend.v1.FeedRowOrder oldest_held = 4`; unset means the reader
  holds no row and the walk begins at the newest page.
- WHY: LoadFeedThrough advanced the same daemon-held walk, so a freed page
  would have streamed from below what it holds. It now begins exactly where
  `older_than` would.
- CONSEQUENCES: `daemon/internal/resolve/feed/loadthrough.go` builds its walk
  from the named key, and the webapp passes its oldest held row.

## Implementation consequences

- Webapp: on losing the host's show (`webapp/src/presence.ts`,
  `Presence.shown()`), a feed frees rows per the rule above. Finding the newest
  daemon page's boundary takes a `GetFeedPage` `first` in the background, since
  pushed rows carry no page. Pages older pages loaded through `older_than`
  already record their first key (`feed.page-placed`).
- Background pages keep their watch streams: closing them would save about
  330 ms of main thread a minute across all background pages, against a stale
  screen of about 1.3 s on every switch (measured 2026-10-15 from
  `page.stream-cost` and `feed.open-answered`). The owner agreed this is not
  worth it.

# Feed paging on demand

## Problem

The daemon replays up to 200 history entries into memory every time it opens
a session watch (`sessionwatcher.openingPageSize`), and then keeps every live
entry since, whether or not any webapp shows them. The webapp shows the
newest page (50 rows) and walks older one page per click, but the daemon can
only serve what it already holds: it never asks the shim for older history
(`ReadHistory` exists and is never called), and a walk past the replay ends
in the "history replay stopped" refusal. The owner wants the shim, the store
and the daemon to hold and pull only the pages a webapp asked for.

## Settled with the owner (2026-10-02), before the design round opens

1. ONE page size across the whole stack: 50 STORE ENTRIES. Nothing else
   (watch opening, daemon feed, webapp) picks its own budget. A page may draw
   fewer than 50 rows, because entries coalesce into rows.
2. Opening a session watch replays NO history: the watch carries only what is
   written after the newest entry the daemon already holds (`known_through`).
3. The daemon loads history only on a webapp's request: opening a feed loads
   its newest page; "older" loads the next page back. A page already in
   daemon memory (e.g. from an earlier webapp instance) is served from
   memory; anything older is fetched daemon → shim → store.
4. The daemon keeps its in-memory feed (it owns daemon-only rows: cold gate,
   merge rows, faults), but holds only requested pages, the live tail, and
   daemon-only rows. A fully stateless pass-through was considered and set
   aside because of the daemon-only rows.
5. A row whose starting entry is on a page not yet loaded is not drawn until
   that page is loaded (a tool result whose call is older, a prompt's answers
   whose prompt is older).
6. The "history replay stopped" refusal goes away: older history is always
   reachable down to the conversation's start.
7. Daemon features that read held rows work over loaded pages only:
   response/prompt stepping loads the next older page when it runs past the
   oldest loaded row; rollback offers prompts from loaded pages.
8. Clicking an expanded-footer item (detached subagent, shell, monitor) that
   is not in the loaded feed uses its OWN RPC: the daemon locates the entry
   (asking the shim/store), loads EVERY intermediate page from the oldest
   loaded page down to the one holding it, and streams those pages to the
   webapp itself. If the entry cannot be found, the RPC answers a typed
   not-found, which the footer's activity line surfaces as an error.

## Carried in from vendor-start resilience (open)

- After a process-group kill with no teardown, the next shim's reconciliation
  re-announces spool-backed shells as live. Proposed: the daemon states the
  predecessor's fate in StartSession, and a "killed with its process" lost
  cause. Open: whether Claude Code background shells share the shim's process
  group; whether a restart's kill should draw as `cancelled` rather than lost.

## Landed changes

### 1. One page size, owned by the store (store.v1, shim.v1)

- WHAT: every caller-supplied page budget is RETIRED — `page_size` on
  store `OpenAgentSessionRequest` (2) and `ReadAgentPageRequest` (2), and on
  shim `WatchAgentRequest` (2), `ReadHistoryRequest` (2) and
  `StartTurnRequest` (4). A page is the store's page: 50 entries, a single
  store-owned constant.
- WHY: owner ruling 1 ("ONE single definition of what constitutes one page …
  the daemon is simply asking for a page").
- Consequences: the store's validation of `page_size` and the shim's and
  daemon's budget constants (`openingPageSize = 200`, `turnPageSize = 1`,
  `shimclient.DefaultPageSize`) go away; the feed resolver's row-count
  `DefaultPageSize` stops defining pages — a feed page is what one store page
  resolves to.

### 2. Tail-only openings (store.v1, shim.v1)

- WHAT: store `OpenAgentSessionRequest`, shim `WatchAgentRequest` and shim
  `StartTurnRequest` each gain `oneof opening { known_through; tail_only; }`
  (`known_through` moved into the oneof on its existing tag;
  `AgentSessionTailOnly` = 5, `WatchAgentTailOnly` = 4, `StartTurnTailOnly`
  = 8). UNSET stays "repaint: the newest page".
- WHY: owner ruling 2 — opening a watch replays no history.
- Consequences: the daemon's session watches open `tail_only` when it holds
  nothing of the agent and `known_through` otherwise, never a repaint; an
  entry received both on the tail and in a later page read is absorbed by
  identity.

### 3. LoadFeedThrough (agentrepl.v1, endpoint_load_feed_through.proto)

- WHAT: new server-streaming rpc `LoadFeedThrough(workspace, target FeedId)`
  → zero or more `page` frames (older root-feed pages, prepended like a
  `next` page) then exactly one terminal `reached{target}` or `error`
  (unknown_workspace, workspace_ref_mismatch, transferring_away,
  not_yet_adopted, target_undecodable, not_found, history_unavailable{detail}).
  After it ends, `GetFeedPage next` continues from the oldest page it
  delivered.
- WHY: owner ruling 8 — selecting an expanded-footer item not in the loaded
  feed loads every intermediate page, via its own rpc, and a failure surfaces
  in the footer activity.
- Mechanism (orchestrator judgement): the daemon walks older store pages
  until the target row is drawn or the conversation's start is reached
  (`not_found`), so no shim "locate" rpc is needed — the walk IS the
  every-intermediate-page requirement. Target-scoped errors are ALSO
  published as a transient footer fault line (existing
  `FooterStatusActivityFault`, kind e.g. `feed_entry_not_found`), so no
  footer proto change.

### 4. GetFeedPage semantics (comment only)

- A page is the store's page; the daemon fetches a `next` past what it holds
  on the spot; a row whose starting entry is on an unloaded page is not drawn
  until that page is.

### Obviation candidates (owner to rule; not removed)

- `frontend.v1.FeedPageError.history_replay_truncated` /
  `FailureHistoryReplayTruncated`: with on-demand paging a walk can always
  reach the conversation's start, so the daemon stops emitting it. Still
  referenced by the webapp's page-error rendering.

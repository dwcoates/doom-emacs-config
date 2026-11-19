# Page attachment at a daemon handover

At a deploy the old daemon and its successor run side by side, and a page's one
standing stream (`WatchPage`) lives in one of them while its unary
`SubscribePage` calls are routed separately. At five handovers between
2026-11-18 and 2026-11-19 a subscribe reached the daemon that did not hold the
page's stream and was refused `page_not_attached`; every page filed an outage.
The page's stream and its subscriptions are tied to one open socket, so they
cannot move to shared storage. The contract instead makes every page call name
the daemon that holds the page, and has that daemon fetch a workspace's data
from whichever daemon serves the workspace.

Design mode: autonomous. Landing: ONE landing, with no compatibility path
(owner ruling, 2026-11-19).

## Assumptions

- Objective: a page's subscriptions are only ever evaluated by the daemon that
  holds the page's stream, and a workspace's data reaches the page from the
  daemon that serves the workspace, whichever daemons those are.
- Systems: the daemon (`daemon/internal/server`, `daemon/cmd/agent-repld`,
  `daemon/internal/resolve/feed`), the webapp (`webapp/src/rpc`), the test
  fakes (`webapp/test`, `lisp/testsupport/fakedaemon`, `daemon/integration`).
- Frontend: no drawn element changes. The page files nothing for the new
  planned outcomes.
- Non-additive: `SubscribePageResponse` gains an outcome oneof, so an answer
  with no arm set is no longer "subscribed". A page call with no attachment is
  refused as malformed.
- The landing deploy's own handover still runs the previous build's daemon and
  pages, which carry none of this. Its check will still show the old records.

## Core design principles

- A page call is served only by the daemon that issued the page's attachment.
  - Forces: every page call carries the attachment; routing reads it before any
    workspace rule; a daemon answers `page_gone` for an attachment it cannot
    serve.
  - Does not claim: that a page's data comes from that daemon. Data follows the
    workspace's serving row.
- A workspace's facts (its views, its page-connected and page-drawn facts) live
  with the daemon that serves the workspace.
  - Forces: a subscription body for a workspace served elsewhere is relayed
    from that daemon, and the relay registers the page there.
- Nothing is bridged by a timer. Where the daemon can know a fact (which daemon
  issued an attachment, which daemon minted a feed token) the message carries
  it.

## Iteration sequence

1. The attachment type and where it is echoed (`endpoint_watch_page.proto`).
2. The outcome of `SubscribePage` and `UnsubscribePage`.
3. The subscription ending for a relayed body whose serving daemon ended it.
4. The feed watch token's minter (`feed_token.proto`).

## Landed changes

### The page attachment (`agentrepl.v1.PageAttachment`)

- What: `PageAttached` carries a `PageAttachment` (the issuing daemon's
  instance id and a sequence it never reissues). `SubscribePageRequest` and
  `UnsubscribePageRequest` echo it. `WatchPageRequest.page` documents that a
  second stream for one page id replaces the first.
- Why: the page's stream lives in one daemon and a unary call can be accepted
  by another, so the call has to say which daemon holds its stream.
- Consequences: `daemon/internal/server/forward.go` routes a page call by its
  attachment's instance BEFORE the workspace rule, and never by an arm's
  workspace (today `requestWorkspaceID` descends into the `request` oneof and
  forwards the subscribe to a daemon that does not hold the page). A call with
  no attachment is Connect InvalidArgument. Accepted cost: every client of
  these verbs (webapp, Go integration harness, webapp fake daemon, elisp fake)
  changes in the same landing.
- `ReportPageDrawn` is deliberately unchanged: it is keyed by page but its
  fact belongs with the workspace's serving daemon, which learns of a relayed
  page from the relay itself (the `Agent-Repl-Relayed-Page` request header).

### The outcome of `SubscribePage`

- What: `SubscribePageResponse` gains `result { success }`, and
  `SubscribePageSuccess` gains `outcome { subscribed | page_gone }`.
- Why: "the attachment is over" is an answer the page acts on, not a failure,
  so it sits in the success arm. Refusals stay Connect errors, as for a
  refused `Watch*` open, so there is no error arm.
- Why `page_not_attached` lost: a Connect error is filed by the page as an
  outage, and this outcome is planned.
- `UnsubscribePageResponse` stays empty: an attachment that is over is the
  state an unsubscribe asks for.

### The `moved` ending (`agentrepl.v1.PageSubscriptionMoved`)

- What: `PageSubscriptionEnded.how` gains `moved`.
- Why: a relayed body whose serving daemon ended it cleanly (it stood down)
  used to read as `source_ended`, which the page files as a failure. The page
  subscribes again at once and files nothing.
- Not used for a handed-away workspace: that still ends `failed` with
  `handed_away`, and the page raises its move notice as before.

### The feed token's minter (`agentrepl.v1.FeedWatchToken.instance`)

- What: the token names the daemon instance that minted it.
- Why: the pin is in that daemon's memory, the token value is a per-daemon
  counter (`ft-N`), and two daemons mint the same values; a token honored by
  the wrong daemon could tail another workspace's feed. A tail is opened at
  the minter; a minter that no longer runs is `unknown_token`, and the feed
  reopens (it already re-mints on every reopen).
- Why "feed arm names workspace and feed, no token" lost: the token is also
  the pin that makes the page/tail seam gapless, which a bare address cannot
  carry.

## Implementation state

Kept current so a compaction loses nothing. Order of work:

1. Protos and regenerated bindings.
2. Daemon: attachment mint and check, routing by instance, body relay with
   acceptance, relay ending, page registration at the serving daemon, feed
   token instance, adopt routing by serving row, newer attach replaces older,
   failed yield resumes accepting, accept-gap metric.
3. Webapp: stamp the attachment, `page_gone`, the `moved` ending.
4. Fakes and tests: Go unit, webapp vitest, daemon integration, elisp fake.
5. Changelog line, metrics catalog, finish (one landing).

Dropped, with reasons:

- One WebSocket per page: it guarantees only that a subscribe reaches the
  page's daemon, the handover would still fail on the workspace's side, and it
  leaves Connect (no CORS, hijacked connections the exit does not see, lost
  backpressure, the request log and counters bypassed).
- Subscriptions sent with the attach: the daemon would start them before the
  page learned whether the daemon serves its webapp build.
- Ending every page stream at the claim yield: not needed once page calls are
  routed by attachment, and it strands pages if the yield fails.
- Holding an early subscribe on the successor until the page attaches: a timer
  in place of a fact the attachment states.

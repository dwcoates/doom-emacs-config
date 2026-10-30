# Retire the account-usage contract

As of master 81207676c4 (2026-10-30) no shim produces
`conversation.v1.SessionUpdate.account_usage`: the shim stopped sampling the
account's usage (the SDK's experimental usage verb cost each idle session's
CLI about 18 CPU-seconds every five minutes), and the daemon's footer draws
each login's weekly allowances from `conversation.v1.SessionUpdate.rate_limit_status`
events alone. The owner also ruled the five-hour window gone: it is no longer
drawn or restored. What remains is contract with no producer, and code that
still reads it. This change retires both.

Design ran autonomously (no co-design was asked for).

## Assumptions (step 1, derived from the request and the code)

- Objective: remove every message, field and code path that exists only for
  the sampled account usage, across every system, leaving the rate-limit
  event as the footer's one source of allowance figures.
- What is built: nothing new. The change is removal only.
- Frontend: yes. The footer's enduring line (`webapp/src/footer`) draws
  `frontend.v1.FooterActivityEnduring`; figma→idl governs, and the drawn
  boxes that lose their producer leave the contract with them.
- Systems: proto (`conversation.v1`, `frontend.v1`), the daemon (footer
  resolver, sessionwatcher, topbar, sidebar, feed sink, the state store's
  `account_usage` table), the shim (its owned-arm list and tests), the webapp
  (the enduring line's drawing and tests), and the docs that state the shapes.
- Non-additive changes: every change here is a removal. The state store's
  `account_usage` table loses the columns nothing produces through a new
  BREAKING layout step; no earlier step's text changes.

## Iteration sequence

Walked top-down by containment, leaf package first:

1. `conversation.v1` (the leaf every other package imports): the session
   stream's `account_usage` arm and the messages only it uses.
2. `frontend.v1` footer: the enduring line's arms and the usage element's
   fields that only a sample could produce.

## Landed changes

### 1. `conversation.v1.SessionUpdate.account_usage` retired

- WHAT: the `account_usage` arm (tag 6) is removed and its tag and name
  reserved. Removed with it, each verified (by grep over `proto/src`) to be
  referenced by nothing outside this group: `SessionAccountUsage`,
  `SessionSubscriptionType` and its four plan arms,
  `SessionAccountUsageSeatSpend`, `SessionMoney`,
  `SessionAccountUsageAvailable`, `SessionModelUsageWindow`,
  `SessionUsageWindow`, `SessionAccountUsageUnavailable` and its four reason
  messages. `SessionModelUsageWindow` was not named in the request; it is
  referenced only by `SessionAccountUsageAvailable.model_scoped`, so it goes
  with it.
- WHY: nothing produces the arm. A consumer arm with no producer reads as a
  live fact and keeps dead code alive in every consumer.
- CONSEQUENCES:
  - `conversation.v1.SessionRateLimitStatus` is now the only source of a
    window's figure as well as its verdict. It carries no instant, so the
    "newest sample wins" ordering the footer kept per window
    (`allowanceWindow.sampledAtMs`) has nothing left to order; the last event
    to arrive is drawn.
  - The per-seat billing mode had no source but a sample, so it leaves the
    daemon entirely (see change 2).

### 2. `frontend.v1` footer: the sample-only shapes retired

- WHAT:
  - `FooterActivityEnduring.seat_spend` (tag 5) removed, tag and name
    reserved; `FooterActivityEnduringSeatSpend` and `FooterMoney` removed
    (nothing else references `FooterMoney`).
  - `FooterActivityEnduringUsage.session` (tag 1) removed, tag and name
    reserved.
  - `FooterActivityEnduringUsage.sample` (tag 3) removed, tag and name
    reserved, with `FooterAllowanceSample` and its five arm messages. Not
    named in the request: the daemon already never sets it (the sample path
    was its only producer), so it is the same dead shape.
  - Comments on `FooterActivityEnduring`, `FooterActivityEnduringUsage` and
    `FooterAllowance.status` restated so they no longer describe a billing
    mode choice, a five-hour window, or sampled figures.
- WHY: each has no producer once the account is never sampled and the
  five-hour window is never drawn.
- CONSEQUENCES:
  - The enduring line is now one of two arms: `usage` (some weekly or overage
    figure is on hand) or `unobserved`.
  - The webapp drops its seat-spend drawing, its session-allowance segment and
    any sample handling (`webapp/src/footer/activity.ts`).
  - ACCEPTED COST: an enterprise per-seat account that the vendor never sends
    a rate-limit event for draws "unobserved" forever. That is the honest
    state of what is known: nothing measures the seat's spend any more.

### 3. State store: `account_usage` loses its dead columns (layout 91)

- Not proto text, recorded because it is forced by the schema change and is
  not obvious from it.
- WHAT: a new BREAKING layout step drops the `session_*` window (five
  columns), the four `seat_*` columns, every window's `*_sampled_at_ms`
  stamp, and the long-retired `no_allowance`. The fresh-file DDL declares the
  final shape directly; the layout-24 step's text is kept as shipped under its
  own name so older files still migrate through it, following the
  `agent_repl_session` precedent (layouts 21 and 26), and a test pins that a
  migrated table matches a fresh one.
- WHY BREAKING, not two deploys: `no_allowance` is `NOT NULL` with no default
  and each window's figure columns decode only when set together, so a build
  that stops writing them breaks the build before it either way. The one
  breaking step is the honest form; the deploy restarts across it.

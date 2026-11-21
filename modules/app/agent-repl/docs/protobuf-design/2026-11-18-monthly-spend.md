# The account's monthly spend on the usage line

The footer's enduring usage line draws a subscription account's weekly
allowances, but an account billed by its spend (the work account, an
Enterprise seat billed against a monthly limit) has no weekly window at all,
so its line reads "usage not yet seen for this account" forever. The vendor
does report that account's month-to-date spend against its monthly limit; the
shim drops it. This change carries that spend from the vendor's usage answer to
the footer, so an API-billed account's line reads "$1,993.96 of $12,000 this
month" the way a subscription's reads "weekly 40% · resets in 3d".

## Assumptions (autonomous design)

- **Objective**: the usage line draws an API-billed account's monthly spend,
  as it draws a subscription account's weekly allowances.
- **Frontend**: yes, the footer's enduring line and the tokens sheet that
  draws the same allowance list (figma→idl governs `frontend.v1`).
- **Systems**: the shim (reads the vendor's answer), the daemon's footer
  resolver and its state store (files, persists and projects the figure), the
  webapp (draws it).
- **Non-additive changes**: none on the wire. The plan-usage arm's consumer
  contract widens from "the whole truth about the three weekly windows" to
  "the whole truth about the windows and the spend it carries".

## Evidence

- Observed (strongest tier): the work account's vendor usage cache
  (`cachedUsageUtilization` in its config root's `.claude.json`) carries every
  window null and `extra_usage {is_enabled: true, monthly_limit: 1200000,
  used_credits: 199396, utilization: 16.6, currency: "USD", decimal_places: 2}`.
  The amounts are minor units (16.6% = 199396 / 1200000; $12,000 is the
  seat's known limit).
- Observed: the shim's plan-usage records for that account state
  `subscription_type: enterprise, rate_limits_available: true`.
- Declared type: the SDK's `SDKControlGetUsageResponse.rate_limits.extra_usage
  {is_enabled, monthly_limit, used_credits, utilization, currency?}` — no
  `decimal_places`, so the exponent is the currency's ISO 4217 one, which
  matches the observed `decimal_places: 2` for USD.
- Unverified: that the SDK's runtime answer carries `extra_usage` for this
  account (registered in the vetting register).

## Iteration sequence

Autonomous; the default sequence collapses to two increments, leaf package
first: (1) `conversation.v1.SessionPlanUsage` gains the spend, (2)
`frontend.v1.FooterActivityEnduringUsage` gains its drawn element.

## Landed changes

### 1. The plan-usage read carries the month-to-date spend

- **What**: `conversation.v1.SessionPlanUsage.monthly_spend` (tag 4,
  optional `SessionPlanUsageMonthlySpend {spent, oneof limit {capped,
  uncapped}}`), with `SessionMoney {amount_minor, currency}`.
- **Why**: the spend is the only usage figure an API-billed account has.
- **Shape reasoning**:
  - The limit is a two-arm oneof rather than an optional sibling: the
    vendor's `monthly_limit` is nullable while extra usage is enabled, which
    is an uncapped spend, a different line ("$1,993.96 this month") from a
    capped one. An unset optional would have meant both "no cap" and "not
    reported".
  - Money is minor units plus an ISO 4217 code, the vendor's own unit; the
    exponent is not carried, because the code determines it and the vendor
    does not state one in the SDK's declared answer.
  - The vendor's own `utilization` is not carried: it is spent / limit,
    which the daemon resolves from the two amounts, and a third figure that
    could disagree with them is a second authority.
- **Consequences**:
  - The shim states the spend only when the vendor states extra usage
    enabled, a spend, and a currency; any of them null leaves it unset with
    an INFO record naming which.
  - The read stays the whole truth: the daemon retires a stored spend when
    a read leaves it unset, exactly as it retires a weekly window.
  - A subscription account with extra usage enabled draws its spend too:
    every figure the vendor reports is drawn.

### 2. The usage line draws the spend as its own element

- **What**: `frontend.v1.FooterActivityEnduringUsage.monthly_spend` (tag 7,
  optional `FooterMonthlySpend {spent, oneof limit {capped {limit,
  utilization}, uncapped}}`), with `FooterMoney {amount_minor, currency}`.
- **Why**: the line's allowances are its elements; the spend is one more.
- **Layout**:

  ```
  +-FooterActivityEnduringUsage------------------------------------------+
  | [weekly 40% · resets in 3d] | [overage 4% · …] | [$1,993.96 of $12,000 this month] |
  +----------------------------------------------------------------------+
  ```

  The spend is drawn last, after weekly and overage, in the line's one
  fixed order.
- **Shape reasoning**:
  - It is an element of the usage arm, not a third arm of the line's oneof
    (the retired tag-5 `seat_spend` was one): an account can report weekly
    windows and a spend at once, and the line draws every figure reported.
  - `utilization` (0..1, spent / limit) rides on the capped arm only,
    resolved by the daemon, so the client paints the spend's gradient
    without dividing money.
  - `FooterMoney` is a frontend element type re-spelling
    `conversation.v1.SessionMoney`, licensed by the backend→frontend
    precedence: the daemon's resolver is its one author.
- **Consequences**:
  - The daemon's state store keeps the spend per account root (additive
    layout), so a restarted daemon draws it before any session speaks.
  - The enduring line's `usage` arm is set when any window or the spend is
    figured.

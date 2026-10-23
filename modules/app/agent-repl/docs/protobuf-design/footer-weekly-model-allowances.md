# The footer's weekly element carries the per-model weekly windows

The vendor meters several seven-day windows: the account-wide one, and one per
model family (Opus, Sonnet). The daemon folded every seven-day rate-limit
verdict onto the one weekly figure while drawing that figure only from the
account-wide window, so "Opus weekly rejected" could stand beside "weekly
40%", and an exhausted Opus window was a refusal with no visible cause. The
owner asked (2026-10-23) for the usage line to read
"weekly <percent> (overall) (<percent> opus <percent> sonnet)".

## Core design principles

- **Each vendor allowance window is its own fact.** Its figure, its verdict
  and its rejection are filed under that window alone. Consequence: the
  footer's weekly element carries one `FooterAllowance` per seven-day window,
  and no verdict on one window is drawn on, or lifts a block raised by,
  another. It does NOT claim the footer must draw every window the vendor
  names: `seven_day_overage_included` and `seven_day_oauth_apps` have no drawn
  allowance.

## Landed changes

### `frontend.v1.FooterActivityEnduringUsage.weekly` becomes the weekly element

- WHAT: tag 2 (`weekly`, a bare `FooterAllowance`) is retired by number; tag 6
  `weekly` is a `FooterWeeklyAllowance` holding an optional `overall`
  allowance and optional `models` (`FooterWeeklyModelAllowances`: optional
  `opus`, optional `sonnet`). `FooterAllowance.status` documents that a
  verdict about a window instance that has since reset is not shipped.
- WHY: the drawn element is one box, "weekly 40% (overall) (12% opus 30%
  sonnet) · resets in 3d", whose parenthesized group is a box of its own; the
  messages nest as the boxes do. A flat `weekly_opus`/`weekly_sonnet` beside
  `weekly` was rejected because it splits one drawn element across siblings.
- Consequences:
  - Retiring by number only (not by name) is what lets tag 6 keep the name
    `weekly`. An older client reading tag 6 sees no weekly figure; daemon and
    webapp ship together, so this is not a breaking change under
    `proto/AGENTS.md`.
  - The element's countdown is the overall window's; the per-model figures
    draw no countdown of their own and lapse independently ("opus reset").
    When the overall window lapses the whole element lapses.
  - With no overall figure the element reads "weekly (12% opus 30% sonnet)";
    a weekly element with neither, or a models element with no model, is a
    malformed view the webapp refuses.
  - The persistence model (`wsm.AccountUsage`, `account_usage` layout 72)
    gains the two model windows and a per-window verdict reset; the footer
    resolver owns the mapping between it and the protocol model.

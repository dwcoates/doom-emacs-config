# API retry resolves the blocked rung

## Problem

While the vendor retries a failing API call mid-turn (DNS failure, a revoked
token, anything the vendor answers with `attempt N of M, next attempt at T`),
the workspace stays on the `thinking` rung and draws red on the footer, the
sidebar and the tab bar. The workspace is not usable in that state: nothing
advances until the API answers. The owner wants it blue (unusable) while the
API fails, red for the moment a new prompt is registered, and blue again as
soon as the API fails once more.

The footer already carries the retry as a `working` salient line
(`FooterStatusActivityRetrying`), but the `blocked` arm's salient oneof has no
retrying line, so moving the claim to `blocked` today would lose the attempt
count and the countdown to the next attempt.

## Core design principles

### The footer's color is the sidebar's and the tab bar's color, always

- **Principle (owner's terms):** if the footer is blue, the sidebar and the
  tab bar are blue, always — for every color, not just blue.
- **Consequences for the contract:** every footer status arm that claims a
  rung has a roster arm on the same rung, so a fact the footer draws as a
  color is never a fact the roster lacks. The API-retry block therefore
  needs a roster arm as well as a footer substatus.
- **Reopens:** nothing landed in this change; it also governs the existing
  "facts only one resolver observes" list in `daemon/internal/resolve/ladder`
  (each such fact is a standing violation to close by feeding the other
  resolver). To be recorded in `modules/app/agent-repl/AGENTS.md`.
- **Does not claim:** that the footer and roster draw the same detail. Only
  the coarse color must agree; arm names, sub-statuses and activity lines
  stay per-surface.

## Settled behavior

- While the vendor retries a failing API call mid-turn, the workspace is
  BLUE on the footer, sidebar and tab bar (`blocked` rung), with the retry
  line (attempt N of M, countdown to the next attempt) kept in the footer.
- The reason is its own substatus, "API retrying" — not `vendor_error`,
  whose meaning is "the vendor is refusing requests"; an unreachable network
  is not a refusal.
- A prompt sent during the retry is registered at once and the workspace is
  RED because it IS working: the footer status is `working` with its step
  set accordingly, and the retry activity line is removed for that period —
  the whole footer reflects the working state, not only the color.
- If the API then fails again the workspace goes BLUE again; the first
  successful response returns it to red.

## Landed changes

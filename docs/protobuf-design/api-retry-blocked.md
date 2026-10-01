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

### 1. The API-retry block on the footer and the roster

- **What:** `FooterStatusBlocked.substatus` gains `api_retrying`
  (`FooterSubStatusBlockedApiRetrying`, tag 8); `FooterStatusBlockedSalient.kind`
  gains `retrying` (the existing `FooterStatusActivityRetrying`, tag 9);
  `RosterRow.status` gains `api_retrying` (`RosterRowStatusApiRetrying`, tag 39).
- **Why:** a turn whose API call is failing is unusable, so it is blue on every
  surface, and the footer keeps the attempt count and countdown it showed while
  red. Its own substatus because `vendor_error` means the vendor refusing
  requests, and an unreachable network is not a refusal.
- **Consequences:**
  - The daemon's ladder gains a fact on the `blocked` rung for both resolvers:
    the footer resolver's standing retry state, and a new retry fact in the
    sidebar resolver (`daemon/internal/resolve/sidebar`), fed the same api-error
    and cleared by the same predicate as the footer's `clearRetry`
    (`daemon/internal/resolve/footer/resolver.go`). That predicate becomes one
    shared helper so the two cannot drift.
  - `FooterStatusWorkingSalient.retrying` is no longer emitted: whenever the
    retry stands the status is `blocked`. It is kept on the wire (removing an
    arm is breaking); a new turn opening clears the retry, so the red period
    shows `working` with no retry line, per the settled behavior.
  - Emacs and the webapp map `api_retrying` to blue in their color tables.

### 2. Judgment call: the composer stays open under `blocked · api_retrying`

- **What:** `render-colors.json` gains `composer_open_substatuses`, the declared
  exceptions to the composer invariant, holding `blocked: [api_retrying]`.
  The Go vocab loader validates it (the status must close the composer; every
  substatus must be a real arm of that status's substatus oneof), and the
  webapp's `composerClosedFor(arm, substatus)` honors it.
- **Why:** blue closes the composer by the 2026-09-28 ruling, but the owner's
  settled behavior needs a prompt sent during the retry to go through and cut
  the retry short. The owner was asked to choose between this, turquoise, and
  a closed composer, and asked instead for all changes to proceed; the
  recommended option was taken. Reversible by deleting the one entry.
- **Consequences:** the composer gate is no longer a pure function of the
  status color; it reads the substatus too. Emacs is unaffected: its composer
  is gated by the daemon's host composer arm, which already takes prompts while
  the workspace is unusable.

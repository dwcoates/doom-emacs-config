# Streaming reveal window

The webapp's type-out of an arriving response stops and starts: each push's
new text is revealed at a rate proportional to the backlog (fast, then
slowing, then stopped until the next push), and the settling push snaps the
leftover text in at once. The owner wants the reveal paced from how often
updates actually arrive: the daemon keeps a rolling, persisted record of the
recent gaps between streaming updates, per model, and tells the webapp with
each update how long to spread the not-yet-shown text over.

## Core design principles

- **The daemon measures and keeps the pacing state; the webapp stays
  stateless.** In the owner's terms: "the daemon knows how long it takes
  between streaming updates, and is generally the thing that manages state."
  - Contract consequence: the pacing figure rides the feed row the daemon
    already pushes; no webapp-side history, no webapp storage.
  - Reopens nothing.
  - Does not claim the webapp holds no per-bubble bookkeeping: it still owns
    how much of a bubble is on screen (`data-revealed`), which only it can
    know.

## Landed changes

### 1. `frontend.v1.FeedResponseRevealWindow` on the update and success arms

- **What.** `frontend.v1.FeedResponseUpdate.reveal_window` and
  `frontend.v1.FeedResponseSuccess.reveal_window`, both `optional`, both the
  new element message `frontend.v1.FeedResponseRevealWindow { uint32
  expected_gap_ms }`. `frontend.v1.FeedResponseError` gets none: a bubble
  the turn's death cut short draws its partial prose at once, as it does
  today.
- **Why a duration, not a time per character.** The webapp spreads
  EVERYTHING it has not yet shown across the window. A per-character rate
  computed by the daemon from this push's new characters would assume the
  previous push had finished revealing, so an early push would leave a
  backlog that never catches up. Only the webapp knows what is on screen.
  The owner agreed ("reveal window is a good solution").
- **Why optional.** The owner: "we need to be sure to handle in the webapp
  the situation where the daemon sends a null reveal window value ... to do
  it as it now does (new model comes out, for example, we have to just wait
  to accumulate the wait information, and that's fine)." Unset means the
  webapp paces with its existing `SmoothReveal` default, and an unset
  window on `success` keeps the current draw-at-once behavior.
- **Why per model AND per kind.** The owner ruled thinking and prose keep
  separate records, erring on the side of caution about their latencies
  differing.
- **Element message, not a bare scalar.** figma→idl: every element on a
  view is its own message.

## Daemon-side model (not contract, recorded for implementers)

- **Window.** The 25 most recent gaps per (model, kind), oldest dropped
  first. A window is reported only once FULL (25 samples); until then the
  field is unset. The owner: the first window "will be accumulated on the
  very first execution, and then persisted to disk (along with all
  subsequent updates in rolling fashion)", so no seed file ships.
- **Weighting.** Recency-weighted: the newest gap has weight 1 and each
  older one is multiplied by a fixed decay (0.85), per the owner's "the most
  recent result counts more toward the average than the 25th most recent".
- **What is a sample.** Only the gap between two consecutive text-bearing
  fragments of the SAME block (prose or thinking), measured at the daemon's
  receipt, drawn on the LIVE plane, on the ROOT feed. Excluded: the wait
  before a block's first fragment (time to first token), pauses for tool
  calls (they fall between blocks), history replays (their frames arrive in
  one burst), and subagent blocks (the daemon knows only the session's
  model, `wsState.model`, so it cannot key a subagent's gaps).
  - Accepted cost: a subagent's bubble never carries a window and keeps the
    default pacing.
- **Window served.** Only on the live plane and the root feed, for the
  same reason samples are only taken there. A replayed draw carries no
  window (it is drawn whole anyway).
- **Persistence.** `wsm` table `reveal_gaps` (layout 24, additive). The
  pacer holds the windows in memory, loaded at boot, and writes a
  (model, kind) window when a live block of it settles: one write per block
  rather than per fragment, under the feed lock. Accepted cost: samples of a
  block still open when the daemon dies are lost.
- **Gap measured at the daemon, not the webapp.** The daemon-to-webapp hop
  is local, so its delay is small next to the vendor's gaps.

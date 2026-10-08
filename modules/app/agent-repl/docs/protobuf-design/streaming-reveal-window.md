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

### 2. `expected_gap_ms` is the time left until the next BURST (2026-10-08)

- **What.** No shape change. `FeedResponseRevealWindow.expected_gap_ms` now
  means the expected time from THIS push until the bubble's next burst of
  fragments, measured from the start of the burst the push belongs to; the
  proto comments say so.
- **Why.** The owner saw no effect from change 1 ("i really dont see it
  having any effect"): the vendor's fragments arrive in clumps, and a
  per-fragment average (~50ms) revealed each clump at once and then stood
  still for most of the ~530ms until the next one. See "What is a burst"
  below.
- **Why the daemon states the remainder rather than the webapp keeping the
  deadline.** Only the daemon knows where a burst started; the webapp stays
  stateless and keeps spreading each push across the window it is given.

## Daemon-side model (not contract, recorded for implementers)

- **What is a burst (2026-10-08 revision).** The vendor's deltas reach the
  daemon in clumps: in workspace 096682deb5314ad3 on 2026-10-08 the shim
  received ~11 deltas within 1ms, then nothing for ~450ms, on a steady
  ~530ms period. Measured per fragment, ten 2ms gaps and one 500ms gap
  averaged to ~50ms, so each clump was revealed in a frame or two and the
  type-out then stood still: no visible pacing. A fragment within 20ms
  (`revealpace.BurstThreshold`) of the previous one now joins its burst; a
  sample is the gap from one burst's START to the next's.
  - Why 20ms: inside a clump the daemon folds fragments 1-4ms apart (the first
    after the store's write batch up to ~8ms, the slowest observed 14-19ms);
    the vendor's own unclumped cadence was 40-60ms. It is also just over one
    60fps frame, within which two arrivals cannot be revealed apart anyway.
  - Accepted cost: a stream whose every gap is under 20ms is one long burst
    and samples nothing; its pushes are then a frame apart, which needs no
    pacing.
- **Window per push.** `expected_gap_ms` is what is LEFT of the expected gap,
  measured from the start of the burst the push belongs to (never below 1),
  so every push of one clump names the same deadline and the webapp spreads
  the whole clump toward it. A push not inside a burst (the settled whole
  landing more than 20ms after the last fragment) states the full expected
  gap. Both are the daemon's: the webapp still spreads from each push's draw
  across the stated window and keeps no clock of its own.
- **Window.** The 25 most recent gaps per (model, kind), oldest dropped
  first. A window is reported only once FULL (25 samples); until then the
  field is unset. The owner: the first window "will be accumulated on the
  very first execution, and then persisted to disk (along with all
  subsequent updates in rolling fashion)", so no seed file ships.
- **Weighting.** Recency-weighted: the newest gap has weight 1 and each
  older one is multiplied by a fixed decay (0.85), per the owner's "the most
  recent result counts more toward the average than the 25th most recent".
  Each gap is ALSO weighted by its own length (sum w·g² / sum w·g, 2026-10-08):
  burst gaps are bimodal (a clump, a few lone fragments ~50ms apart, a pause
  of several hundred ms), and a window too short for the gap that follows
  stops the type-out while one too long only slows it. Replayed against the
  probe's arrivals, the reveal stood still 22% of the streaming time this way
  against 42% with the plain average and 60% per fragment.
- **What is a sample.** Only the gap between two consecutive bursts of
  text-bearing fragments of the SAME block (prose or thinking), measured at the daemon's
  receipt, drawn on the LIVE plane, of the MAIN agent. Excluded: the wait
  before a block's first fragment (time to first token), pauses for tool
  calls (they fall between blocks), history replays (their frames arrive in
  one burst), and subagent blocks (the daemon knows only the session's
  model, `wsState.model`, so it cannot key a subagent's gaps).
  - Accepted cost: a subagent's bubble never carries a window and keeps the
    default pacing.
- **Window served.** Only on the live plane and for the main agent, for the
  same reason samples are only taken there. A replayed draw carries no
  window (it is drawn whole anyway).
- **Persistence.** `wsm` table `reveal_burst_gaps` (layout 30, additive).
  It replaced `reveal_gaps` (layout 27), whose per-fragment gaps would drag a
  burst-gap average back to the figure that paced nothing; a new table rather
  than a reset, so the build handing over (which still writes `reveal_gaps`)
  can never write into the table the new build reads. `reveal_gaps` is
  retired and is dropped by a later breaking step. The
  pacer holds the windows in memory, loaded at boot, and writes a
  (model, kind) window when a live block of it settles: one write per block
  rather than per fragment, under the feed lock. Accepted cost: samples of a
  block still open when the daemon dies are lost.
- **Gap measured at the daemon, not the webapp.** The daemon-to-webapp hop
  is local, so its delay is small next to the vendor's gaps.

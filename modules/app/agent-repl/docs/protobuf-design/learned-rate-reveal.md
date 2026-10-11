# Learned-rate reveal

Every response bubble's type-out is wrong today. Thinking types out at about
25 chars/sec while it arrives at about 65, because one long pause in the
daemon's burst-gap record (a 6538 ms gap, length-weighted) stretched its
reveal window to about 5.8 s. Prose types each burst out in about 150 ms and
then waits for the next, so it reads as chunks. A settled `success` with no
window snaps its unshown tail in at once, and subagents and models not yet
measured fall back to a 200 chars/sec chase. The owner wants one reveal for
every response bubble (prose, interim and thinking): it follows the speed the
text arrives at, held between a learned minimum and maximum chars/sec,
banking text that arrives faster than the maximum and spending the bank when
text arrives slower than the minimum, so the type-out finishes as little after
the stream as realistically possible and stalls only during genuinely long
pauses.

Supersedes the pacing model of `streaming-reveal-window.md`.

## Context (step 1)

- **Objective (owner, 2026-10-10).** "The reveal should be as slow as the
  text arrives on average", erring on finishing a little after the stream
  (as little as realistically possible) rather than catching up and stalling.
- **What is being built.**
  - A per-bubble reveal in the webapp whose speed follows the arrival speed,
    clamped to [min, max] chars/sec.
    - Text arriving faster than max is banked and typed at max.
    - When text arrives slower than min, banked text is typed at min.
    - An empty bank stalls; the owner accepts stalls during rare long pauses.
    - The bank is limited to 1.5 s of typing at max (owner, 2026-10-10);
      beyond it the reveal may exceed max to catch up.
    - A block that ends with text banked keeps typing at max to the end; no
      final speed-up (owner, 2026-10-10).
  - The daemon learns min and max per model and per kind from the arrival
    speed of whole settled blocks (chars over the block's streaming time),
    replacing the burst-gap record.
    - min is about the 25th percentile and max about the 38th percentile of
      per-block speeds; the owner set 45 to 55 chars/sec for thinking, which
      those percentiles reproduce on the 2026-10-10 data (thinking: 45 is
      p24, 55 is p38).
- **Frontend component.** Yes: `frontend.v1.FeedResponse` (the response
  bubble card). No layout changes; the change is to the reveal's pacing
  element only.
- **Systems involved.**
  - `proto/src/frontend/v1/feed.proto` (`frontend.v1.FeedResponseUpdate`,
    `frontend.v1.FeedResponseSuccess`, `frontend.v1.FeedResponseRevealWindow`).
  - Daemon: `daemon/internal/resolve/feed` (`revealpace.go`, `response.go`,
    `thinking.go`), `daemon/internal/revealpace`, `daemon/internal/wsm`
    (persistence).
  - Webapp: `webapp/src/feed/cards/response.ts`, `webapp/src/smooth.ts`.
- **Non-additive changes.**
  - `frontend.v1.FeedResponseRevealWindow` (`expected_gap_ms`) is replaced.
  - The daemon's burst-gap measurement (`revealpace.Burst`,
    `weightedAverage`, `reveal_burst_gaps`) is replaced by a per-block speed
    record.
  - The webapp's windowed reveal (`windowedReveal`) and the 200 chars/sec
    `SmoothReveal` fallback for live bubbles are replaced.
  - A folded bubble whose visible lines are full drops its bank as well as
    stopping its type-out.

## Core design principles

- **One reveal for every response bubble.** In the owner's terms: "this
  should be true for all response bubbles (prose, interim, thinking). we're
  also consolidating how streaming works across all response bubbles."
  - Contract consequence: the pacing element rides
    `frontend.v1.FeedResponseUpdate` and `frontend.v1.FeedResponseSuccess`
    for every response bubble, thinking or not, main agent or subagent.
  - Reopens: `streaming-reveal-window.md`'s "only the main agent's live
    frames carry a window" (subagents get a pace too).
  - Does not claim the bubbles look alike: thinking stays purple and
    one-line, interim stays one-line, the final answer stays unfolded.
- **Hidden text never prolongs the reveal.** In the owner's terms: "there's
  no point in risking prolonging the stream with this banking logic if the
  user can't see what's being streamed." A folded bubble whose visible lines
  are full stops typing and drops its bank, and resumes only once unfolded.
  - Contract consequence: none; the webapp alone knows what is visible.
  - Reopens nothing.
  - Does not claim the final green answer folds: it is never folded (owner,
    2026-09-27, restated 2026-10-10), so it always types out.
- **The daemon measures and keeps the pacing state; the webapp stays
  stateless** (carried from `streaming-reveal-window.md`).
  - Contract consequence: min, max and the bank limit come from the daemon on
    each push; the webapp keeps only per-bubble on-screen bookkeeping.

## Landed changes

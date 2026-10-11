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

### 1. `frontend.v1.FeedResponseRevealPace` replaces `frontend.v1.FeedResponseRevealWindow` (2026-10-10)

- **What.** `frontend.v1.FeedResponseUpdate.reveal_pace` and
  `frontend.v1.FeedResponseSuccess.reveal_pace` (tag 3, `optional`), both the
  new element `frontend.v1.FeedResponseRevealPace` with
  `min_chars_per_second`, `max_chars_per_second` and `bank_limit_ms`.
  `frontend.v1.FeedResponseRevealWindow` is deleted; tag 2 and the name
  `reveal_window` are reserved on both arms. `frontend.v1.FeedResponseError`
  still carries no pace: a bubble the turn's death cut short is drawn whole.
- **Why bounds and a bank rather than a window.** A window predicts the time
  to the next burst afresh for every burst, so its errors stay in the total;
  one 6538 ms pause stretched thinking's window to about 5.8 s, and prose's
  ~150 ms window typed each burst out at once. The owner's design: follow the
  arrival speed, clamped to learned bounds, bank the overflow, accept stalls
  during rare long pauses, and finish a little after the stream rather than
  catching up and stalling.
- **Why the arrival speed and the bank stay off the wire.** Only the webapp
  sees when each push reached it and how much of the bubble is on screen;
  this is the per-bubble bookkeeping the carried principle already allows.
- **Why the bank limit rides the wire.** It is a pacing figure the owner set
  (1.5 s), and pacing state is the daemon's per the carried principle; the
  webapp holds no tuning of its own.
- **Owner-delegated decisions (owner: "just pick your own best recommended
  decisions", 2026-10-10).**
  - A model or kind with too few measured blocks gets a built-in default pace
    (seeded from the 2026-10-10 store measurement), so every live draw is
    paced and the field is unset only on replays.
  - A subagent's bubble gets the bounds learned for its kind across every
    model. Why: `conversation.v1.AgentActivity` names no model, and the
    daemon's feed resolver knows only the session's model
    (`daemon/internal/resolve/feed/resolver.go:223`, code tier). Passing the
    subagent's model through would need a `conversation.v1` change across the
    shim; the kind-wide record works with what the daemon already has.
    - Gap routed around: activities carry no producing model. When an
      activity names its model, subagent bubbles should switch to their own
      (model, kind) record and the kind-wide record goes away.
- **Consequences.**
  - Subagent bubbles type out at a learned pace instead of the 200 chars/sec
    chase.
  - Every live `success` carries a pace, so a block's tail always types out.
  - The daemon's burst-gap machinery (`revealpace.Burst`, `weightedAverage`,
    `reveal_burst_gaps`) has no remaining reader; the table is retired with a
    later breaking step, per the wsm two-deploy rule.

## Daemon-side model (not contract, recorded for implementers)

- **What a sample is.** One settled block of the MAIN agent, on the LIVE
  plane, with the session's model known: the characters (runes) of its
  settled whole minus those its first text-bearing fragment brought, over the
  time from that fragment's receipt to the settle's
  (`daemon/internal/resolve/feed/revealpace.go`). The first fragment's text is
  left out because it arrived at the start instant.
  - Left out: blocks under 40 characters or under 200 ms (they say nothing
    about streaming speed), a block that never streamed (a settled whole with
    no fragment before it), a block the turn's death cut short (the failure
    arm), replays, subagents.
  - Accepted cost: the transcript plane's settle can land before the stream
    plane's last fragments (`response.go`, "THE SAME BLOCK'S SETTLED WHOLE"),
    which overstates that block's speed. The bounds are low percentiles, so a
    few fast outliers barely move them.
- **Window.** The 50 most recent block speeds per (model, kind), oldest
  dropped first (`revealpace.WindowSize`). A key with fewer than 20
  (`revealpace.MinBlocks`) serves its kind's default.
- **Bounds.** Nearest-rank 25th and 38th percentiles of the window, rounded to
  whole chars/sec, never below 1, minimum never above maximum.
- **Defaults.** Thinking 45-55, prose 49-90 chars/sec: the same percentiles
  measured off the store on 2026-10-10 (2149 thinking and 845 prose blocks of
  the last 7 days, each block's characters over its row's first-insert to
  last-write time, blocks over 60 s dropped as rewrites).
- **Kind-wide bounds** (subagents, a main agent whose model is not known
  yet): the percentiles of every model's window of that kind pooled, once the
  pool holds 20 speeds; the default before that. No separate record is kept.
- **Persistence.** `wsm` table `reveal_block_speeds` (layout 31, additive). A
  window is written when a measured block settles. `reveal_burst_gaps`
  (layout 30) and `reveal_gaps` (layout 27) are retired and dropped by a later
  breaking step.

## Webapp-side model (not contract, recorded for implementers)

- **One speed rule** (`webapp/src/reveal-pace.ts`). With `bank` = rendered
  characters arrived but not shown and `limit` = `max × bank_limit`, the speed
  is `clamp(bank / 0.3 s, min, max)` while `bank ≤ limit`, and
  `max + (bank − limit) / 0.3 s` past it; zero for an empty bank. A steady
  arrival speed keeps a steady bank, and `bank / 0.3 s` is the speed that keeps
  it steady, so the reveal follows the arrivals about 0.3 s behind within the
  bounds; a stream faster than the maximum settles just past the limit.
  - Why the excess is caught up on 0.3 s and not on the bank limit: a whole
    block landing in one push (a settle with no streamed fragments before it;
    the e2e `!md` scenario drops ~3.2k characters at once) took ~7 s to type
    out when the excess drained on a 1.5 s time constant; on 0.3 s it takes
    under 4 s, with no jump.
- **Across pushes** only the shown length is carried (`data-revealed`); the
  windowed reveal's carried speed (`data-reveal-speed`) is gone.
- **A draw with no pace** (a replay) and the error arm are drawn whole; the
  200 chars/sec `SmoothReveal` fallback is gone with `smooth.ts`.

# The footer's clock stands, frozen, until the next submission

The owner's request (2026-10-08): the duration cell behaves exactly as the
tokens cell does (`footer-last-turn-tokens.md`). Its figure stands until the
next turn starts and is restated across a relaunch, and it stops increasing
once the turn is over: it is the duration of the prompt, not a time-ago.

## Landed changes

### 1. `FooterClock.turn_ended_at_ms` (new, tag 2, additive)

- **What changed:** `optional int64 turn_ended_at_ms = 2` on `FooterClock`.
  It is set only with no turn in flight, together with `turn_started_at_ms`,
  and the client draws the difference once without ticking or a stop control.
- **Why a field:** the clock's one instant could only say "running since", and
  its absence said idle. A frozen duration needs its end instant. Per the clock
  convention an instant is shipped and a duration is drawn, as
  `FeedShellSettled.ended_at_ms` already does for a settled shell.
- **The end edge:** the first edge that ended the turn (`wsState.endTurn`):
  the main agent's terminal, the prompt queue's close, a dead shim, a dead
  query, or a compaction's cut. This is the same set of edges after which the
  tokens figure stops moving.
- **The one owner of the clear:** the turn reset at `SetTurn`, the edge that
  also clears the tokens figure. A refused submission (`SetTurn(nil)`) leaves
  no clock, as it leaves the tokens cell at `--`.

### 2. A relaunched daemon restates the clock

- **How:** read off the same first main-agent page the tokens figure is
  restated from, under the same guards (no turn opened on this daemon). The
  start is the place of the newest turn's user prompt entry, and the end is
  the place of that turn's main-agent terminal frame.
- **When nothing is restated:** the page lacks either entry (a turn straddling
  the page's boundary, a turn still running, a turn whose shim died without a
  terminal). The cell is then idle rather than showing a duration measured
  from a guessed edge.

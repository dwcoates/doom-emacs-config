/**
 * Smooth text reveal: interpolate a streaming block's growth so the reader
 * sees a continuous flow of characters rather than the raw API chunk bursts.
 *
 * The daemon relays the SDK's `text-delta`/`thinking-delta` frames verbatim,
 * and those land in whatever sizes the model's provider flushes them — a
 * word here, a whole paragraph there. The store appends each delta to the
 * owning block's `text` and every append repaints the bubble, so the feed
 * lurches forward one API chunk at a time. This layer decouples what the
 * reader SEES from what has ARRIVED: it hands the renderer a prefix of each
 * still-streaming block that grows a few characters per animation frame,
 * catching up to the arrived frontier over a short window rather than in one
 * jump. A burst therefore reads as a fast-but-continuous type-out, and a
 * steady trickle as a gentle one.
 *
 * Nothing here touches the DOM. The clock is injected, which is what lets the
 * pacing be tested: the webapp's test dependencies carry no browser clock.
 * The caller drives the animation — each `reveal()` reports whether any block
 * is still catching up, and the caller schedules the next frame while so.
 */

/**
 * The ONLY fields this module reads off a feed item. The conversation model
 * that used to define them (`store.ts`) went with the hand-decoded transport,
 * and the reveal never needed more than this: a block's identity, its arrived
 * text, and whether the producer closed it. Declaring the shape here keeps the
 * pacing logic independent of whichever view type the caller is animating.
 */
export interface RevealBlock {
  kind: "text" | "thinking";
  /**
   * The block's PLACE in the feed: the reveal cursor is tracked by it, so it
   * must be opened once and never moved, or the type-out restarts under the
   * reader.
   */
  blockId: string;
  text: string;
  done: boolean;
}

/** Any other feed item: opaque to the reveal, passed through untouched. */
export interface RevealOpaqueItem {
  kind: string;
}

export type RevealItem = RevealBlock | RevealOpaqueItem;

/** The feed state the reveal paces: an ordered item list, and nothing else. */
export interface RevealState {
  items: RevealItem[];
}

/**
 * Whether an item is one the reveal animates. Kind alone decides, exactly as
 * the discriminated union it replaces did.
 */
function isRevealBlock(item: RevealItem): item is RevealBlock {
  return item.kind === "text" || item.kind === "thinking";
}

/** Monotonic millisecond clock the reveal paces against (`performance.now`). */
export interface RevealClock {
  now(): number;
}

/** Tuning for how fast the shown prefix catches up to the arrived frontier. */
export interface RevealOptions {
  /**
   * Floor reveal rate (characters/second) applied once the shown prefix is
   * near the frontier — the gentle type-out speed a steady trickle settles
   * to, below which the reveal never slows.
   */
  minCps: number;
  /**
   * Time constant (seconds) the reveal chases the frontier on. The rate is
   * `backlog / catchupSeconds`, so the reveal accelerates with the backlog:
   * a big burst drains its bulk within roughly this window and a steady
   * stream settles about this far behind the frontier, rather than lagging
   * unboundedly behind. The final characters settle at `minCps` once the
   * backlog is small, so the last sliver trails the bulk by a beat.
   */
  catchupSeconds: number;
}

/**
 * Defaults tuned for a fluid feel: a burst's bulk drains on a ~0.3s time
 * constant, and the frontier itself types at a floor of 200 chars/sec (~3
 * characters per 60fps frame) so even a slow trickle reads as continuous
 * motion.
 */
export const DEFAULT_REVEAL_OPTIONS: RevealOptions = {
  minCps: 200,
  catchupSeconds: 0.3,
};

/** Per-block reveal progress: how much is shown, and when it last advanced. */
interface Track {
  /** Characters revealed so far (fractional — accumulates across frames). */
  revealed: number;
  /** Clock reading at the last advance, for the next frame's delta. */
  last: number;
}

/**
 * Slice `text` to its first `count` characters without splitting a UTF-16
 * surrogate pair: a cut landing between a pair's high and low half would
 * leave a lone high surrogate that renders as a replacement glyph for the one
 * frame before the low half is revealed. When the last kept unit is a high
 * surrogate, its low half is the next (excluded) unit, so the high half is
 * dropped too and the pair reveals atomically on the following frame.
 */
export function revealSlice(text: string, count: number): string {
  const n = Math.floor(count);
  if (n <= 0) return "";
  if (n >= text.length) return text;
  const last = text.charCodeAt(n - 1);
  if (last >= 0xd800 && last <= 0xdbff) return text.slice(0, n - 1);
  return text.slice(0, n);
}

/**
 * Paces the visible growth of every still-streaming text and thinking block.
 * A single instance backs one feed: it keeps a reveal cursor per block id,
 * advances each on `reveal()`, and forgets a block once it leaves the feed
 * (a `/clear`) or the session is swapped (`reset()`).
 */
export class SmoothReveal {
  private readonly tracks = new Map<string, Track>();

  constructor(
    private readonly clock: RevealClock,
    private readonly options: RevealOptions = DEFAULT_REVEAL_OPTIONS,
  ) {}

  /**
   * Return a view of `state` whose LIVE TAIL text/thinking block is truncated
   * to the length revealed so far, plus whether it is still behind its
   * frontier (so the caller schedules another frame).
   *
   * Only the most recent bubble — the last item, still being produced — types
   * out. Every earlier block is fully received (the SDK closes a content block
   * before opening the next), as is any block that first arrives here already
   * `done`, so both render whole at once: a replay, a reconnect gap-fill, or a
   * hidden workspace's drained backlog must not re-type prose the agent
   * finished long ago. A truncated block reads as still-streaming (its `done`
   * is held false) so no final-response chip or breath lands before the text
   * has finished arriving on screen. A block that is caught up (or never
   * animated) passes through untouched — real `done` and all — so a settled
   * feed renders from the genuine store state, not a copy.
   */
  reveal(state: RevealState): { state: RevealState; pending: boolean } {
    const now = this.clock.now();
    const live = new Set<string>();
    let pending = false;
    let changed = false;
    const lastIndex = state.items.length - 1;
    const items = state.items.map((item, index) => {
      if (!isRevealBlock(item)) return item;
      const id = item.blockId;
      live.add(id);
      const full = item.text.length;
      // A block with any later item is fully received — the SDK closes a
      // content block before the next one opens — so only the last item can
      // still be streaming. `superseded` marks everything else: a replayed,
      // gap-filled, or backlog-drained block whose text arrived complete
      // while this webview was not painting, which must show at once rather
      // than re-typing prose the agent already finished.
      const superseded = index !== lastIndex;
      let track = this.tracks.get(id);
      if (!track) {
        // First sight. Type out ONLY the genuinely-live tail (the last item,
        // still open); seed everything else fully shown so it renders whole
        // immediately. `markShown` pre-marks a restore, so restored prose
        // never reaches this seed either.
        track = { revealed: superseded || item.done ? full : 0, last: now };
        this.tracks.set(id, track);
      } else if (superseded && track.revealed < full) {
        // A block that WAS the animating tail but got superseded while this
        // webview was hidden is now fully received: snap it to full rather
        // than resuming its half-finished type-out on switch-back.
        track.revealed = full;
      }
      const revealed = this.step(track, full, now);
      track.revealed = revealed;
      track.last = now;
      if (revealed < full) {
        pending = true;
        changed = true;
        return { ...item, text: revealSlice(item.text, revealed), done: false };
      }
      return item;
    });
    // Forget blocks that have left the feed (a clear or compaction truncated
    // them away, an evicted
    // replay) so the cursor map cannot grow without bound over a session.
    for (const id of this.tracks.keys()) {
      if (!live.has(id)) this.tracks.delete(id);
    }
    if (!changed) return { state, pending };
    return { state: { ...state, items }, pending };
  }

  /**
   * Mark every current text/thinking block fully shown WITHOUT animating it.
   * Called right after a history replay's restored render so reconnected or
   * rehydrated prose is not re-typed from scratch the next time a live delta
   * grows one of those blocks — only growth beyond what is already on screen
   * then animates.
   */
  markShown(state: RevealState): void {
    const now = this.clock.now();
    for (const item of state.items) {
      if (isRevealBlock(item)) {
        this.tracks.set(item.blockId, { revealed: item.text.length, last: now });
      }
    }
  }

  /** Drop all progress: a session rebind starts a fresh feed. */
  reset(): void {
    this.tracks.clear();
  }

  /**
   * Advance one block's shown length toward `full`. The rate is the greater
   * of the floor and the whole current backlog spread over `catchupSeconds`,
   * so a large burst accelerates the reveal while a near-caught-up block
   * settles to the gentle floor.
   */
  private step(track: Track, full: number, now: number): number {
    if (full <= track.revealed) return full;
    const dt = Math.max(0, now - track.last) / 1000;
    const backlog = full - track.revealed;
    const cps = Math.max(this.options.minCps, backlog / this.options.catchupSeconds);
    return Math.min(full, track.revealed + cps * dt);
  }
}

/** Where a daemon-paced reveal stands at one instant. */
export interface WindowedRevealPoint {
  /** Characters shown, fractional, for `revealSlice` to floor. */
  at: number;
  /** How fast the reveal is moving there, in characters per millisecond. */
  speed: number;
}

/**
 * The largest start speed a window takes, as a multiple of its own average
 * speed. Within it the eased curve below never runs backwards (the
 * Fritsch–Carlson bound for a cubic Hermite segment whose end slope is its
 * average is a start slope of at most √8 ≈ 2.83 times that average).
 */
export const EASE_MAX_START_RATIO = 2.8;

/**
 * Where a reveal the daemon has paced (frontend.v1.FeedResponseRevealWindow)
 * stands `elapsedMs` into its window: everything from `from` to `to` is
 * revealed across `windowMs`, the time the daemon expects to pass before the
 * next fragment arrives, so a steady stream reads as one continuous type-out
 * rather than a burst and a stall per push.
 *
 * `from` is what was already on screen when the push was drawn, so text still
 * unrevealed from the previous push is spread along with the new text rather
 * than left behind.
 *
 * THE SPEED EASES ACROSS WINDOWS. A window starts at `startSpeed`, the speed
 * the previous window was moving at when this push replaced it, and eases to
 * its own average speed by its end (a cubic Hermite curve), so a big chunk
 * followed by a small one slows down gradually instead of switching speed in
 * one frame. With no start speed (the first paced push, or one following an
 * unpaced reveal) the window runs at its average speed throughout. The start
 * speed is capped at `EASE_MAX_START_RATIO` times the average, which keeps the
 * curve from overshooting and running backwards. The window still ends
 * exactly at `to` when `windowMs` has passed.
 */
export function windowedReveal(
  from: number,
  to: number,
  elapsedMs: number,
  windowMs: number,
  startSpeed?: number,
): WindowedRevealPoint {
  const distance = to - from;
  const average = distance / windowMs;
  if (distance <= 0) return { at: to, speed: 0 };
  if (elapsedMs >= windowMs) return { at: to, speed: average };
  const start = Math.min(startSpeed ?? average, EASE_MAX_START_RATIO * average);
  const s = Math.max(0, elapsedMs) / windowMs;
  // Hermite basis on s in [0, 1], with the start tangent m0 = start * window
  // and the end tangent m1 = distance (the average speed).
  const m0 = start * windowMs;
  const at = from + (s ** 3 - 2 * s ** 2 + s) * m0 + (-2 * s ** 3 + 3 * s ** 2) * distance + (s ** 3 - s ** 2) * distance;
  const slope = (3 * s ** 2 - 4 * s + 1) * m0 + (-6 * s ** 2 + 6 * s) * distance + (3 * s ** 2 - 2 * s) * distance;
  return { at, speed: slope / windowMs };
}

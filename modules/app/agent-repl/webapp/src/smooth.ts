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

import type { StoreState } from "./store.js";

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
  reveal(state: StoreState): { state: StoreState; pending: boolean } {
    const now = this.clock.now();
    const live = new Set<string>();
    let pending = false;
    let changed = false;
    const lastIndex = state.items.length - 1;
    const items = state.items.map((item, index) => {
      if (item.kind !== "text" && item.kind !== "thinking") return item;
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
  markShown(state: StoreState): void {
    const now = this.clock.now();
    for (const item of state.items) {
      if (item.kind === "text" || item.kind === "thinking") {
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

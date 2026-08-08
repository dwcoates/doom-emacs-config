/**
 * breathing — the progress footer's liveness signal, replacing the rotating arc.
 *
 * Two independent channels, and keeping them independent is the whole point:
 *
 *   SIZE says "still alive". The phase word oscillates gently between a little
 *   smaller and a little larger, forever, on wall-clock time. It is deliberately
 *   NOT driven by daemon traffic — a footer that restarts its animation on every
 *   arriving frame reads as a stutter rather than a breath, and the footer
 *   rewrites its whole `innerHTML` per chrome frame, so a plain CSS animation on
 *   a freshly built element would restart on every one of them. The fix is an
 *   EPOCH: the ticker stamps one start time and never moves it, and each render
 *   emits a NEGATIVE `animation-delay` of the elapsed time, which seeks the
 *   fresh element straight to where the cycle already was. The word therefore
 *   continues on its already-planned next size, exactly as if the element had
 *   never been replaced.
 *
 *   COLOR says "something just arrived". Each daemon-resolved ProgressView steps
 *   the word to the next shade along a closed 20-stop ramp. Because the ramp is
 *   walked one stop per arrival, the shade shifting IS the tick — a reader sees
 *   traffic land without anything jumping or resetting.
 *
 * The ramp runs green → purple the COOL way round (through teal and blue), so it
 * never passes through yellow, orange, or red — those hues are spoken for
 * elsewhere in the footer (the rate-limit rungs, the failure row) and a breathing
 * word wandering into them would read as an alarm rather than as progress.
 */

/** Stops on the green → purple ramp. */
export const BREATH_SHADES = 20;

/**
 * One full inhale-and-exhale of a resting PROMPT bubble. Must match the
 * `bubble-breath` keyframes' duration in `styles.css` (`.bubble.user`): the
 * negative delay computed here only seeks to the point the cycle had already
 * reached if both sides agree on how long the cycle is, and a period that
 * drifted from the stylesheet would land every rebuilt bubble at the wrong
 * phase instead of no phase at all — a subtler bug than the snap-back it
 * replaced. Reduced motion STOPS the bubble breath outright rather than
 * slowing it (see the `prefers-reduced-motion` block), so this is the one
 * period in both motion modes and the delay never has to ask which mode is
 * live.
 */
export const BUBBLE_BREATH_PERIOD_MS = 5200;

/** One full inhale-and-exhale. Must match the `pfooter-breath` keyframes. */
export const BREATH_PERIOD_MS = 2600;

/** Ramp endpoints in HSL hue degrees: green, and purple the cool way round. */
const HUE_GREEN = 140;
const HUE_PURPLE = 285;

/** What one render needs to paint the breathing word. */
export interface BreathState {
  /** Ramp stop, `0 .. BREATH_SHADES - 1`. */
  shade: number;
  /** Time since the never-reset epoch, for the negative animation delay. */
  elapsedMs: number;
}

/**
 * The shade at one ramp stop as an HSL color.
 *
 * Stops are evenly spaced across the closed interval, so stop 0 is exactly green
 * and the last stop is exactly purple. Out-of-range indices wrap rather than
 * clamp: the ramp is a cycle the tick walks forever, not a scale that runs out.
 */
export function breathColor(shade: number): string {
  const stop = ((shade % BREATH_SHADES) + BREATH_SHADES) % BREATH_SHADES;
  const hue = HUE_GREEN + (stop * (HUE_PURPLE - HUE_GREEN)) / (BREATH_SHADES - 1);
  return `hsl(${Math.round(hue)} 62% 58%)`;
}

/**
 * The footer's breathing bookkeeping: one ramp position and one fixed epoch.
 *
 * Arrival is detected by OBJECT IDENTITY of the resolved progress view, not by
 * comparing its fields. The store adopts each `ProgressView` wholesale as a
 * fresh object (`Store.adoptProgress`), so a new reference is precisely "the
 * daemon sent another one" — including a re-send whose numbers happen to be
 * identical, which is still a tick worth showing.
 */
export class BreathingTicker {
  private shade = 0;
  private epochMs: number | null = null;
  /** The last progress view seen, by reference. `undefined` = none yet. */
  private seen: unknown = undefined;

  /**
   * Note the progress view this render carries, stepping the ramp when it is a
   * new one. A null view (nothing resolved yet) is not a tick.
   */
  observe(progress: unknown): void {
    if (progress === null || progress === undefined) return;
    if (progress === this.seen) return;
    // First view ever: adopt the ramp's own starting stop rather than stepping
    // off it, so the first breath the user sees is the green end.
    if (this.seen !== undefined) this.shade = (this.shade + 1) % BREATH_SHADES;
    this.seen = progress;
  }

  /**
   * The current shade and cycle offset. The epoch is stamped on first read and
   * never moved again — that immovability is what keeps the breath continuous
   * across every rewrite of the footer.
   */
  state(nowMs: number): BreathState {
    if (this.epochMs === null) this.epochMs = nowMs;
    return { shade: this.shade, elapsedMs: Math.max(0, nowMs - this.epochMs) };
  }
}

/**
 * The prompt bubble's breath: the SIZE channel of the pattern above, with no
 * color channel at all — a resting prompt is ambience, not a signal.
 *
 * The feed rebuilds bubble nodes wholesale (a re-render, a resync, a lazy item
 * upgrading out of its placeholder), and a fresh element starts its CSS
 * animation at 0% — the deflated end of the swing. Mid-cycle that reads as a
 * visible snap backwards, which is the opposite of a continuous breath. So the
 * same EPOCH the footer uses: one start time, stamped on first read and never
 * moved, and every render emits a negative `animation-delay` of how far into
 * the cycle that epoch says we are. A rebuilt bubble seeks straight to where
 * the cycle already was, making the rebuild indistinguishable from a node that
 * was never touched.
 *
 * The delay is reduced modulo the period here (unlike the footer's raw elapsed
 * time, which browsers accept as-is) purely to keep the emitted attribute a
 * small number; either is equivalent to the animation.
 *
 * ONE epoch serves the whole page, so every prompt bubble breathes in unison.
 * That is deliberate: per-bubble epochs would make the feed shimmer as a dozen
 * unrelated phases drifted past each other.
 */
export class BubbleBreath {
  private epochMs: number | null = null;

  /** How far into the current cycle, in `[0, BUBBLE_BREATH_PERIOD_MS)`. */
  delayMs(nowMs: number): number {
    if (this.epochMs === null) this.epochMs = nowMs;
    // A clock that goes backwards would otherwise emit a POSITIVE delay, which
    // stalls the animation at its start until the clock catches up.
    const elapsed = Math.max(0, nowMs - this.epochMs);
    return elapsed % BUBBLE_BREATH_PERIOD_MS;
  }
}

/** The page-global bubble breath every prompt bubble renders against. */
export const bubbleBreath = new BubbleBreath();

/**
 * The inline delay one prompt bubble renders with, as the whole style value.
 * Every construction site of a `.bubble.user` must carry it — a bubble built
 * without it is the snap-back this module exists to remove.
 */
export function bubbleBreathStyle(nowMs: number = Date.now()): string {
  return `animation-delay:-${Math.round(bubbleBreath.delayMs(nowMs))}ms`;
}

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
 *
 * The status word's letter wave lives here too (`StatusWave`), on the same
 * epoch mechanic (`AnimationEpoch`) for the same reason: the footer rebuilds
 * the nodes it paints out from under the animation.
 */

/** Stops on the green → purple ramp. */
export const BREATH_SHADES = 20;

/** One full inhale-and-exhale. Must match the `pfooter-breath` keyframes. */
export const BREATH_PERIOD_MS = 2600;

/**
 * ONE immovable start time, shared by every animation in this module that has
 * to survive its element being rebuilt.
 *
 * Every animation here runs on wall-clock time and is painted onto nodes the
 * footer replaces wholesale, so each needs the same thing: a start time
 * stamped once and never moved, against which each render measures how far
 * into the cycle the page already is. That measurement is what a negative
 * `animation-delay` seeks the fresh element to, and it is the whole reason a
 * rebuild is invisible rather than a snap back to 0%.
 *
 * The clamp is not decoration: a clock that went backwards would otherwise
 * yield a negative elapsed time, which becomes a POSITIVE `animation-delay`
 * and stalls the animation at its start until the clock catches up.
 */
export class AnimationEpoch {
  private epochMs: number | null = null;

  /** Time since the epoch, stamping it on first read. Never negative. */
  elapsedMs(nowMs: number): number {
    this.epochMs ??= nowMs;
    return Math.max(0, nowMs - this.epochMs);
  }
}

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
  private epoch = new AnimationEpoch();
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
    return { shade: this.shade, elapsedMs: this.epoch.elapsedMs(nowMs) };
  }
}

/**
 * The footer status word's LETTER WAVE: a bulge that travels along the word
 * while the session is making PROGRESS, forwards and then backwards, forever.
 *
 * Owner's idea, 2026-09-14: the statuses that mean progress rather than
 * stagnation should say so in the word itself, with each letter slightly
 * enlarged in sequence. It is ADDITIONAL to the colour breath above, which is
 * untouched — the colour still steps per arrival and the word still carries its
 * arm's tone. Nothing here paints a colour.
 *
 * `transform: scale`, never `font-size`, for the same reason the breath uses it:
 * a font-size oscillation re-measures the word sixty times a second and shoves
 * the whole strip sideways. A transform is painted without re-layout, so the
 * word's width is exactly what it was at rest.
 *
 * PHASE CONTINUITY is the point. The footer rewrites its whole subtree on every
 * push, and a fresh element starts its CSS animation at 0% — mid-wave that reads
 * as a stutter on every arriving frame, which is precisely the defect the
 * footer's breath already solved. So the same
 * {@link AnimationEpoch}: one start time, stamped on first read and never moved,
 * and every render emits each letter's own negative `animation-delay`.
 *
 * THE MODULUS IS TWO PERIODS, not one. The rule runs
 * `animation-direction: alternate`, so the timeline's EVEN iterations run
 * forwards and its ODD ones backwards; reducing the delay modulo one period
 * would land every redraw on an even iteration and flip a receding bulge into an
 * advancing one. Two periods is the smallest interval over which the animation
 * actually repeats, so that is the modulus.
 */
export const STATUS_WAVE_PERIOD_MS = BREATH_PERIOD_MS;

/**
 * The smallest interval the waving letter repeats over: one forward pass and
 * one backward pass. Must stay `2 x` the `pfooter-status-wave` duration.
 */
export const STATUS_WAVE_CYCLE_MS = STATUS_WAVE_PERIOD_MS * 2;

/**
 * How far one letter lags the letter before it, which is the ONLY thing that
 * makes the bulge travel rather than the whole word breathing in unison.
 *
 * A tenth of the period. Across the longest waving words (`working`,
 * `loading`, `merging`, `closing`, seven letters each) that spreads the word
 * over 1560ms of the 5200ms cycle — far enough apart to read as a bulge moving
 * along the letters, near enough that the word never comes apart into seven
 * unrelated animations.
 */
export const STATUS_WAVE_LETTER_OFFSET_MS = STATUS_WAVE_PERIOD_MS / 10;

/**
 * The status word's COLOUR SWEEP period: 40% slower than the letter wave
 * (owner ruling, 2026-10-18), so the hues drift past more calmly than the
 * bulge travels. Must stay in step with the `pfooter-status-color` duration.
 *
 * It is its own period on the SAME epoch, so the sweep still continues across a
 * push; only the modulus differs from the bulge's.
 */
export const STATUS_COLOR_PERIOD_MS = (STATUS_WAVE_PERIOD_MS * 14) / 10;

/**
 * The smallest interval the colour sweep repeats over, two periods for the same
 * `alternate` reason as {@link STATUS_WAVE_CYCLE_MS}.
 */
export const STATUS_COLOR_CYCLE_MS = STATUS_COLOR_PERIOD_MS * 2;

/** The page-global wave every footer status word renders against. */
export class StatusWave {
  private epoch = new AnimationEpoch();

  /** Letter INDEX's offset into the cycle, in `[0, STATUS_WAVE_CYCLE_MS)`. */
  delayMs(nowMs: number, index: number): number {
    const elapsed = this.epoch.elapsedMs(nowMs) + index * STATUS_WAVE_LETTER_OFFSET_MS;
    return elapsed % STATUS_WAVE_CYCLE_MS;
  }

  /** The word's colour sweep offset, in `[0, STATUS_COLOR_CYCLE_MS)`. */
  colorDelayMs(nowMs: number): number {
    return this.epoch.elapsedMs(nowMs) % STATUS_COLOR_CYCLE_MS;
  }
}

/** The one wave the footer draws against, so a redraw continues it. */
export const statusWave = new StatusWave();

/** The class one waving letter wears; the `pfooter-status-wave` rule keys on it. */
export const STATUS_WAVE_LETTER_CLASS = "pfooter-wave-letter";

/** The span that holds the split letters, so they stay ONE flex item. */
export const STATUS_WAVE_WORD_CLASS = "pfooter-status-word";

/** The marker a waving status cell wears, and the one value it takes. */
export const STATUS_WAVE_ATTRIBUTE = "data-status-wave";
export const STATUS_WAVE_PROGRESS = "progress";

/**
 * The `FooterStatus` arms that mean the session is GETTING SOMEWHERE.
 *
 * `working` is the turn itself and carries every one of its steps —
 * `submitting`, the main agent's own steps (`thinking`, `executing`,
 * `reading`, …), and the two context cuts `clearing` and `compacting`. `loading` is a context item being taken on, `merging` a merge
 * running, `closing` a teardown running: all three are an operation underway.
 *
 * Everything else stands still because the session does: `waiting` is waiting on
 * a human or the vendor, `blocked` is blocked, `disconnected` is severed, `idle`
 * and `interrupted` are settled turns, and `background` is this session holding
 * nothing while detached work continues elsewhere.
 */
export const WAVING_STATUS_ARMS: ReadonlySet<string> = new Set([
  "working",
  "loading",
  "merging",
  "closing",
]);

/** Whether ARM's status word waves. */
export function statusWordWaves(armCase: string): boolean {
  return WAVING_STATUS_ARMS.has(armCase);
}

/**
 * The inline style one waving letter renders with, as the whole style value.
 *
 * Emitted on EVERY letter of a waving word, at every draw. Without it the letter
 * seeks to 0% and the wave restarts at the word's head on each push.
 */
export function statusWaveStyle(index: number, nowMs: number = Date.now()): string {
  return animationDelayStyle(statusWave.delayMs(nowMs, index));
}

/**
 * The inline style the status word's holder renders with: its colour sweep's
 * own negative delay, on the slower colour period, emitted at every draw so the
 * sweep continues across a push.
 */
export function statusColorStyle(nowMs: number = Date.now()): string {
  return animationDelayStyle(statusWave.colorDelayMs(nowMs));
}

/** A negative `animation-delay` of DELAY ms, as a whole inline style value. */
function animationDelayStyle(delayMs: number): string {
  return `animation-delay:-${Math.round(delayMs)}ms`;
}

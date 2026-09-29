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
 * The prompt bubble's own liveness signal lives here too (`BubbleWave`). It is
 * NOT a breath: the bubble and its text hold one static size and a shadow band
 * crosses the fill behind them instead. Both signals share the same epoch
 * mechanic (`AnimationEpoch`) for the same reason — the nodes they paint are
 * rebuilt out from under the animation.
 */

/** Stops on the green → purple ramp. */
export const BREATH_SHADES = 20;

/**
 * One full left-to-right pass of a WORKING prompt bubble's thinking wave. Must
 * match the `bubble-wave` keyframes' duration in `styles.css`:
 * the negative delay computed here only seeks to the point the pass had already
 * reached if both sides agree on how long the pass is, and a period that
 * drifted from the stylesheet would land every rebuilt bubble at the wrong
 * phase instead of no phase at all — a subtler bug than the jump-back it
 * replaced. Reduced motion STOPS the wave outright rather than slowing it (see
 * the `prefers-reduced-motion` block), so this is the one period in both motion
 * modes and the delay never has to ask which mode is live.
 */
export const BUBBLE_WAVE_PERIOD_MS = 3200;

/** One full inhale-and-exhale. Must match the `pfooter-breath` keyframes. */
export const BREATH_PERIOD_MS = 2600;

/**
 * ONE immovable start time, shared by every animation in this module that has
 * to survive its element being rebuilt.
 *
 * Both animations here run on wall-clock time and both are painted onto nodes
 * the feed replaces wholesale, so both need the same thing: a start time
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
 * The prompt bubble's thinking WAVE: a soft shadow band that crosses the
 * bubble's own background from left to right, over and over, while the turn
 * runs. Nothing about the bubble's geometry moves — the bubble and its text
 * hold one static size, and only the fill underneath them changes — so the
 * wave reads as progress running through the answer rather than as the box
 * itself inflating and deflating.
 *
 * The feed rebuilds bubble nodes wholesale (a re-render, a resync, a lazy item
 * upgrading out of its placeholder), and a fresh element starts its CSS
 * animation at 0% — the band back at the left edge. Mid-pass that reads as the
 * wave jumping backwards. So the same EPOCH the footer uses: one start time,
 * stamped on first read and never moved, and every render emits a negative
 * `animation-delay` of how far into the pass that epoch says we are. A rebuilt
 * bubble seeks straight to where the wave already was, making the rebuild
 * indistinguishable from a node that was never touched.
 *
 * The delay is reduced modulo the period here (unlike the footer's raw elapsed
 * time, which browsers accept as-is) purely to keep the emitted attribute a
 * small number; either is equivalent to the animation.
 *
 * ONE epoch serves the whole page, so every prompt bubble's wave crosses in
 * unison. That is deliberate: per-bubble epochs would make the feed shimmer as
 * a dozen unrelated phases drifted past each other.
 *
 * WHICH bubbles wave is not this class's business: the epoch is a phase, and
 * the feed decides who is in flight (`PROMPT_WAVE_ATTRIBUTE`).
 *
 * THE SAME PHASE PAINTS TWO WAYS, both reading THIS epoch, so they never
 * disagree:
 *  - VISIBLE, the compositor runs the `bubble-wave` CSS animation and
 *    `delayMs` seeks a rebuilt node to the right frame (a negative
 *    `animation-delay`);
 *  - HIDDEN, WebKit SUSPENDS that compositor animation (see
 *    `prompt-wave-driver.ts`), so a JS timer paints `background-position-x`
 *    itself, and `positionX` is where along the pass to paint it.
 */
export class BubbleWave {
  private epoch = new AnimationEpoch();

  /** How far into the current pass, in `[0, BUBBLE_WAVE_PERIOD_MS)`. */
  delayMs(nowMs: number): number {
    return this.epoch.elapsedMs(nowMs) % BUBBLE_WAVE_PERIOD_MS;
  }

  /**
   * The `background-position-x` percentage the band sits at RIGHT NOW, for the
   * JS driver that paints the wave while the compositor animation is suspended.
   *
   * It is the exact position the `bubble-wave` keyframes hold at this phase, so
   * the compositor pass and the hand-painted pass are one wave seen two ways: a
   * linear walk from {@link BUBBLE_WAVE_FROM_PCT} at the pass's start to
   * {@link BUBBLE_WAVE_TO_PCT} at its end, over one {@link BUBBLE_WAVE_PERIOD_MS}.
   * Reading the SAME epoch as `delayMs` is what keeps the hand-off between the
   * two — hiding, and coming back — free of any jump.
   */
  positionX(nowMs: number): number {
    const fraction = this.delayMs(nowMs) / BUBBLE_WAVE_PERIOD_MS;
    return BUBBLE_WAVE_FROM_PCT + (BUBBLE_WAVE_TO_PCT - BUBBLE_WAVE_FROM_PCT) * fraction;
  }
}

/**
 * The `background-position-x` the pass starts and ends at, DUPLICATED from the
 * `bubble-wave` keyframes in `styles.css` (`from`/`to`) for the same reason the
 * period is: the JS driver's hand-painted pass and the compositor's keyframe
 * pass must trace the identical travel or the wave would jump at the moment one
 * takes over from the other. The larger percentage is the LEFTMOST position
 * (the band enters off the left), so the pass runs from 200% DOWN to -100% —
 * see the keyframes' own note.
 */
export const BUBBLE_WAVE_FROM_PCT = 200;
export const BUBBLE_WAVE_TO_PCT = -100;

/** The page-global wave every prompt bubble renders against. */
export const bubbleWave = new BubbleWave();

/**
 * The attribute a prompt bubble wears WHILE ITS TURN IS IN FLIGHT, and the ONE
 * thing the `bubble-wave` rule keys on.
 *
 * WHETHER A TURN IS IN FLIGHT IS THE DAEMON'S FACT, NEVER THIS PAGE'S. The
 * prompt row carries it (`FeedUserPrompt.working`, `FeedAgentPrompt.working`):
 * set from the row's first draw until the turn's terminal, whichever way the
 * turn ended, and re-pushed unset on that edge. The bubble wears this
 * attribute exactly while its row says so — drawn from the flag at
 * construction ({@link armPromptWave}) and moved by it on a re-push
 * ({@link setPromptWave}) — and from nothing else: no final-answer mark, no
 * `turn_ended` row, no inference over the feed's other rows.
 *
 * A stylesheet that ran the band on `.bubble.user` outright animated every
 * prompt in the scrollback forever: a settled conversation of thirty prompts
 * painting thirty shadow bands over and over, which says "thirty turns are
 * working" when none of them is, and which no screenshot of the page can ever
 * catch at rest. The attribute is what separates those thirty from the one.
 *
 * It is an ATTRIBUTE ON THE BUBBLE rather than a fact rendered into the
 * bubble's body, because the transition out of flight must not redraw a word
 * of the prompt: a re-push that changes only the flag moves it on the element
 * already on screen, so the wave stops without the text under it moving.
 */
export const PROMPT_WAVE_ATTRIBUTE = "data-wave";

/** The one value {@link PROMPT_WAVE_ATTRIBUTE} takes: the turn is in flight. */
export const PROMPT_WAVE_WORKING = "working";

/**
 * The inline delay one prompt bubble renders with, as the whole style value.
 * Every construction site of a `.bubble.user` must carry it — a bubble built
 * without it is the jump-back this module exists to remove.
 *
 * STAMPED ON EVERY PROMPT BUBBLE, waving or not. The delay is only a phase,
 * and it is inert on a bubble with no animation to seek; a bubble that carried
 * it only while working would need it re-stamped at the moment the feed marks
 * it, which is one more thing to get wrong for no gain.
 */
export function bubbleWaveStyle(nowMs: number = Date.now()): string {
  return `animation-delay:-${Math.round(bubbleWave.delayMs(nowMs))}ms`;
}

/**
 * ARM A FRESH PROMPT BUBBLE: the wave's phase, and the wave iff WORKING.
 *
 * THE ONE CALL EVERY `.bubble.user` CONSTRUCTION SITE MAKES, handed its row's
 * `working` flag verbatim, so a new prompt-bubble site cannot carry the phase
 * and forget the mark, or decide the mark on some ground of its own.
 */
export function armPromptWave(
  bubble: HTMLElement,
  working: boolean,
  nowMs: number = Date.now(),
): void {
  bubble.setAttribute("style", bubbleWaveStyle(nowMs));
  setPromptWave(bubble, working);
}

/**
 * Put the wave on BUBBLE iff WORKING, and take it off otherwise. The phase is
 * left as it stands, so a bubble whose flag flips mid-pass never jumps.
 */
export function setPromptWave(bubble: HTMLElement, working: boolean): void {
  if (working) {
    bubble.setAttribute(PROMPT_WAVE_ATTRIBUTE, PROMPT_WAVE_WORKING);
    return;
  }
  bubble.removeAttribute(PROMPT_WAVE_ATTRIBUTE);
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
 * bubble's wave and the footer's breath already solved. So the same
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

/** The page-global wave every footer status word renders against. */
export class StatusWave {
  private epoch = new AnimationEpoch();

  /** Letter INDEX's offset into the cycle, in `[0, STATUS_WAVE_CYCLE_MS)`. */
  delayMs(nowMs: number, index: number): number {
    const elapsed = this.epoch.elapsedMs(nowMs) + index * STATUS_WAVE_LETTER_OFFSET_MS;
    return elapsed % STATUS_WAVE_CYCLE_MS;
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
  return `animation-delay:-${Math.round(statusWave.delayMs(nowMs, index))}ms`;
}

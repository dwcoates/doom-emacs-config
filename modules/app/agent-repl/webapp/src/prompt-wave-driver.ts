/**
 * prompt-wave-driver — keeps the prompt bubble's thinking wave RUNNING while
 * the webview reports itself hidden.
 *
 * REGRESSION WATCH (prompt glimmer, 2026-09-14). The wave was reported as
 * intermittently freezing — stopping even while the reader was looking straight
 * at it. It was never a source regression: the `bubble-wave` rule (styles.css)
 * and its phase (breathing.ts) were byte-unchanged since they were added. The
 * cause is the PLATFORM. The wave is a compositor CSS animation, and WebKit —
 * which Emacs's xwidget embeds (WKWebView) — SUSPENDS compositor animations
 * whenever the page's `document.visibilityState === "hidden"`. The xwidget
 * reports "hidden" whenever Emacs.app is not the frontmost application, so the
 * band froze every time the editor lost OS focus, even though its window was
 * still on screen. (A live probe in the running webview found `visibilityState`
 * "hidden" with `prefers-reduced-motion` false at the moment it stopped.)
 *
 * THE FIX, and it is a WATCH FLAG rather than a lock. While the page is hidden
 * this driver stamps {@link PAGE_HIDDEN_ATTRIBUTE} on the root, which the
 * stylesheet uses to turn the (now-suspended, so useless) compositor animation
 * OFF, and a JS timer paints `background-position-x` on every waving bubble
 * itself — JS timers keep firing when the page is hidden, unlike the compositor.
 * Each paint reads the SAME page-global epoch the compositor pass reads
 * (`bubbleWave` in breathing.ts), so a coarse or throttled hidden tick still
 * lands the band at the correct phase rather than drifting a second clock. When
 * the page returns to visible the driver hands the pass back to the compositor,
 * re-seeked to that same epoch so the resumed animation starts where the band
 * already is. If the glimmer freezes again, first re-probe `visibilityState`
 * in the live webview: a NEW way for the platform to suspend animation surfaces
 * here, not in the unchanged wave source.
 *
 * ONLY BUBBLES ALREADY MARKED IN FLIGHT WAVE. This driver never decides who is
 * working — it paints exactly the elements the feed marked
 * `[${PROMPT_WAVE_ATTRIBUTE}="${PROMPT_WAVE_WORKING}"]` and no others, so a
 * settled or scrollback prompt is untouched. And it honours the owner's
 * reduced-motion ruling: under `prefers-reduced-motion` it paints nothing, and
 * the stylesheet drops the band's fill, so the wave is off exactly as before.
 */
import {
  PROMPT_WAVE_ATTRIBUTE,
  PROMPT_WAVE_WORKING,
  bubbleWave,
} from "./breathing.js";

/**
 * The flag the driver stamps on the document root WHILE THE PAGE IS HIDDEN, and
 * the one thing the stylesheet's `animation: none` override keys on. It is what
 * lets the JS-painted `background-position-x` win: a running (or suspended)
 * compositor animation outranks an inline style, so the animation must be off
 * for the driver's paint to show.
 */
export const PAGE_HIDDEN_ATTRIBUTE = "data-page-hidden";

/**
 * How often the driver repaints the band WHILE HIDDEN. Fine enough to read as
 * motion on a hidden-but-visible window; a platform that throttles hidden-page
 * timers (WebKit may clamp toward 1s) still advances the band — every paint
 * self-seeks from the epoch, so a slow tick is a coarser wave, never a frozen
 * or a drifting one. The compositor carries the visible case, so this rate
 * costs nothing when the reader is actually looking with focus.
 */
export const HIDDEN_TICK_MS = 100;

/** How the driver reads whether reduced motion is asked for, injectable for tests. */
export type ReducedMotionQuery = () => boolean;

/** What one driver needs; every field defaults to the live page. */
export interface PromptWaveDriverOptions {
  /** Where waving bubbles are queried from. Defaults to the document. */
  root?: ParentNode;
  /** The document whose visibility and root the driver watches. Defaults to `document`. */
  doc?: Document;
  /** The clock, injectable so tests can pin the phase. Defaults to `Date.now`. */
  now?: () => number;
  /** Whether reduced motion is asked for. Defaults to the media query. */
  reducedMotion?: ReducedMotionQuery;
}

/** A running driver: started once at boot, stoppable for teardown and tests. */
export interface PromptWaveDriver {
  start(): void;
  stop(): void;
}

/** The default reduced-motion read: the media query, absent in a test host. */
function mediaReducedMotion(doc: Document): boolean {
  const view = doc.defaultView;
  if (view === null || typeof view.matchMedia !== "function") return false;
  return view.matchMedia("(prefers-reduced-motion: reduce)").matches;
}

/**
 * Build the driver. It is inert until {@link PromptWaveDriver.start}, and it
 * synchronises to the CURRENT visibility on start — a page that booted already
 * hidden begins hand-painting immediately rather than waiting for the first
 * `visibilitychange`.
 */
export function createPromptWaveDriver(options: PromptWaveDriverOptions = {}): PromptWaveDriver {
  const doc = options.doc ?? document;
  const root: ParentNode = options.root ?? doc;
  const now = options.now ?? ((): number => Date.now());
  const reducedMotion = options.reducedMotion ?? ((): boolean => mediaReducedMotion(doc));
  const html = doc.documentElement;

  let hiddenTimer: ReturnType<typeof setInterval> | null = null;

  function workingBubbles(): HTMLElement[] {
    return [
      ...root.querySelectorAll<HTMLElement>(
        `.bubble.user[${PROMPT_WAVE_ATTRIBUTE}="${PROMPT_WAVE_WORKING}"]`,
      ),
    ];
  }

  /** Paint the current epoch phase onto every waving bubble's own fill. */
  function paint(): void {
    // Reduced motion has no wave at all — the stylesheet drops the fill, so a
    // painted position would have nothing to move; painting none keeps the two
    // ends saying the same thing.
    if (reducedMotion()) return;
    const positionX = `${bubbleWave.positionX(now())}%`;
    for (const bubble of workingBubbles()) bubble.style.backgroundPositionX = positionX;
  }

  /**
   * Hand the pass back to the compositor: clear the JS-painted position so the
   * animation owns `background-position-x` again, and re-seek each bubble's
   * negative `animation-delay` to the epoch so the animation resumes where the
   * band already is rather than at the phase it was stamped with when drawn (a
   * hidden spell may have been long). Removing the root flag then restarts the
   * animation, which reads that fresh delay.
   */
  function handBackToCompositor(): void {
    const delay = reducedMotion() ? null : `-${Math.round(bubbleWave.delayMs(now()))}ms`;
    for (const bubble of workingBubbles()) {
      bubble.style.backgroundPositionX = "";
      if (delay !== null) bubble.style.animationDelay = delay;
    }
  }

  function enterHidden(): void {
    if (hiddenTimer !== null) return;
    html.setAttribute(PAGE_HIDDEN_ATTRIBUTE, "");
    paint();
    hiddenTimer = setInterval(paint, HIDDEN_TICK_MS);
  }

  function enterVisible(): void {
    if (hiddenTimer !== null) {
      clearInterval(hiddenTimer);
      hiddenTimer = null;
    }
    handBackToCompositor();
    html.removeAttribute(PAGE_HIDDEN_ATTRIBUTE);
  }

  function sync(): void {
    if (doc.visibilityState === "hidden") enterHidden();
    else enterVisible();
  }

  return {
    start(): void {
      doc.addEventListener("visibilitychange", sync);
      sync();
    },
    stop(): void {
      doc.removeEventListener("visibilitychange", sync);
      if (hiddenTimer !== null) {
        clearInterval(hiddenTimer);
        hiddenTimer = null;
      }
      html.removeAttribute(PAGE_HIDDEN_ATTRIBUTE);
    },
  };
}

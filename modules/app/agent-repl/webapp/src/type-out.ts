/**
 * type-out — THE ONE frame loop that types arrived text out at a reveal pace.
 *
 * Every typed-out surface (a response bubble's prose, a tool card's title)
 * runs this loop, and only says what is particular to it: how long its text
 * is, how to show a prefix of it, how to show all of it, and whether its box
 * is a collapsed one already full. So the rules below hold for every surface
 * alike rather than once per copy.
 *
 * EACH FRAME STEPS AT THE SPEED THE BANK CALLS FOR (`advanceReveal`,
 * reveal-pace.ts): the text not yet shown is the bank, and the reveal follows
 * the speed it fills at within the pace.
 *
 * THE FRAME IS AN ANIMATION FRAME, not the app ticker: the shared ticker steps
 * once a second, which is the right cadence for a clock and useless for a
 * type-out. The loop is self-limiting: it stops when the whole text is shown,
 * and on the first frame that finds its element replaced (it was in the
 * document and no longer is) or its content owned by a later draw. Text not
 * yet rendered has no length to pace, so the loop waits for its render rather
 * than spinning. A host with no `requestAnimationFrame` shows the text whole
 * rather than not at all.
 *
 * A COLLAPSED BOX STOPS TYPING ONCE IT IS FULL (owner request, 2026-10-10).
 * The type-out is for a reader watching the text arrive, and a collapsed box
 * shows nothing past its cap, so banking text nobody can see would only make
 * it land later. The first frame on which the shown text already runs past the
 * cap shows the rest whole.
 *
 * AN UNSEEN PAGE DOES NOT TYPE (owner request, 2026-10-12). Nobody watches a
 * workspace that is not on screen (page-seen.ts), and WebKit suspends a hidden
 * page's frames, so a reveal begun there would still be typing when the
 * reader switched in. A loop started on an unseen page shows its text whole at
 * once, and a page turning unseen mid-reveal shows the rest whole.
 */
import type { Ticker } from "./clock.js";
import { log } from "./log.js";
import type { PageSeen } from "./page-seen.js";
import { advanceReveal, type RevealPace } from "./reveal-pace.js";

/** One surface's part in the loop. */
export interface TypeOutSurface {
  /** The page's seen state and clock. */
  readonly ctx: { readonly seen: PageSeen; readonly ticker: Ticker };
  /** The bounds the loop types within. */
  readonly pace: RevealPace;
  /** How many characters an earlier draw already showed. */
  readonly resumed: number;
  /**
   * The loop's log records are `${operation}.no-frames`,
   * `${operation}.collapsed-full` and `${operation}.unseen`.
   */
  readonly operation: string;
  /** Identifies the surface on every record (the row, say). */
  readonly logContext: Readonly<Record<string, string | number>>;
  /** The element whose replacement ends the loop. */
  readonly element: HTMLElement;
  /** Whether a later draw now owns the text this loop types. */
  readonly reowned: () => boolean;
  /** The text's whole length, or undefined while it is not yet rendered. */
  readonly total: () => number | undefined;
  /** Run FN once the text has rendered. */
  readonly whenRendered: (fn: () => void) => void;
  /** Show the first SHOWN characters and hide the rest. */
  readonly reveal: (shown: number) => void;
  /** Show the whole text. */
  readonly showWhole: () => void;
  /** Whether the shown text already runs past a collapsed box's cap. */
  readonly full: () => boolean;
}

/** Type SURFACE's text out from its resumed length to the whole. */
export function typeOut(surface: TypeOutSurface): void {
  const { ctx, pace, operation, logContext, element } = surface;
  const frame = globalThis.requestAnimationFrame?.bind(globalThis);
  if (frame === undefined) {
    log.debug("no animation frame host; drawing the arriving text whole", {
      operation: `${operation}.no-frames`,
      context: { ...logContext, resumed: surface.resumed },
    });
    surface.showWhole();
    return;
  }
  if (!ctx.seen.isSeen()) {
    log.debug("the page is unseen; drawing the arriving text whole", {
      operation: `${operation}.unseen`,
      context: { ...logContext, resumed: surface.resumed },
    });
    surface.showWhole();
    return;
  }

  // The first frame's step is measured from the draw.
  let last = ctx.ticker.now();
  let shown = surface.resumed;
  // An element that WAS in the document and no longer is has been replaced by
  // a later draw, and its loop is over. One never mounted (a test, a draw into
  // a detached fragment) keeps typing: absence of a document is not the same
  // fact as removal from one.
  let mounted = element.isConnected;
  let over = false;
  const unsubscribe = ctx.seen.onChange((nowSeen) => {
    if (nowSeen || over) return;
    end();
    if (superseded()) return;
    log.debug("the page turned unseen mid-reveal; drawing the rest of the arriving text whole", {
      operation: `${operation}.unseen`,
      context: { ...logContext, shown: Math.floor(shown) },
    });
    surface.showWhole();
  });
  /** This loop is over: no further frame, and no unseen turn, acts on it. */
  function end(): void {
    over = true;
    unsubscribe();
  }
  /** Whether a later draw owns the text, or the element was replaced. */
  function superseded(): boolean {
    if (surface.reowned()) return true;
    if (element.isConnected) mounted = true;
    return !element.isConnected && mounted;
  }
  const step = (): void => {
    if (over) return;
    if (superseded()) {
      end();
      return;
    }
    const total = surface.total();
    if (total === undefined) {
      surface.whenRendered(() => frame(step));
      return;
    }
    const now = ctx.ticker.now();
    shown = advanceReveal(Math.min(shown, total), total, now - last, pace);
    last = now;
    if (shown >= total) {
      end();
      surface.showWhole();
      return;
    }
    surface.reveal(Math.floor(shown));
    if (surface.full()) {
      log.debug("a collapsed box is full; drawing the rest of the arriving text whole", {
        operation: `${operation}.collapsed-full`,
        context: { ...logContext, shown, total },
      });
      end();
      surface.showWhole();
      return;
    }
    frame(step);
  };
  frame(step);
}

/**
 * collapse-motion — A BUBBLE THAT TURNS INTERIM EASES INTO ITS NEW LOOK.
 *
 * A response is drawn whole while it is its turn's latest row, and the daemon
 * proves it interim once a later row lands (`isInterimResponse`). At that
 * moment its bubble changes everything at once: it collapses to one line,
 * loses its fill and shadow, and takes the tighter interim padding. The cap
 * mode changes, so the bubble is a FRESH element (`reusableParts`), and no CSS
 * transition can run across the swap. So the old bubble's look is read before
 * it is replaced, and the new one is animated FROM it: its height, its fill and
 * its shadow ease into the new look over `COLLAPSE_MS` (owner request,
 * 2026-10-13: no snap). The one line of text is the new bubble's from the
 * first frame; the box closes around it.
 *
 * Under `prefers-reduced-motion` the change is instant, as it was.
 */
import { log } from "../log.js";
import { prefersReducedMotion } from "../reduced-motion.js";
import { nextFrame } from "../frame.js";

/** How long the bubble takes to ease into its interim look. */
export const COLLAPSE_MS = 220;

/** What a bubble looked like: what the animation eases from, or to. */
export interface BubbleLook {
  /** The bubble's border-box height, px. */
  readonly height: number;
  /** Its computed fill. */
  readonly background: string;
  /** Its computed shadow. */
  readonly boxShadow: string;
}

/** The seams the easing reaches through, so a test can drive them. */
export interface CollapseMotionDeps {
  /** Run FN at the next frame, before it paints. */
  readonly nextFrame: (fn: () => void) => void;
  /** Whether the reader asked for reduced motion. */
  readonly reducedMotion: () => boolean;
}

/** The page's own frame clock and reduced-motion setting. */
function pageDeps(bubble: HTMLElement): CollapseMotionDeps {
  return {
    nextFrame,
    reducedMotion: () => prefersReducedMotion(bubble.ownerDocument),
  };
}

/** EL's look as it is drawn now. Reads layout. */
export function readBubbleLook(el: HTMLElement): BubbleLook {
  const style = getComputedStyle(el);
  return {
    height: el.getBoundingClientRect().height,
    background: style.backgroundColor,
    boxShadow: style.boxShadow,
  };
}

/**
 * Ease BUBBLE, just drawn in place of a bubble that looked like FROM, into its
 * own look. Runs at the next frame, once BUBBLE is laid out where the old one
 * stood; a bubble gone by then, or a reader who asked for reduced motion, gets
 * the instant change.
 */
export function easeIntoLook(bubble: HTMLElement, from: BubbleLook, deps: CollapseMotionDeps = pageDeps(bubble)): void {
  deps.nextFrame(() => {
    if (!bubble.isConnected) {
      log.debug("a collapsing bubble left the feed before its first frame; nothing to ease", {
        operation: "feed.bubble.collapse-skipped",
        context: { why: "detached" },
      });
      return;
    }
    if (deps.reducedMotion()) {
      log.debug("a bubble collapsed at once: the reader asked for reduced motion", {
        operation: "feed.bubble.collapse-skipped",
        context: { why: "reduced-motion" },
      });
      return;
    }
    const to = readBubbleLook(bubble);
    log.debug("a bubble eases into its interim look", {
      operation: "feed.bubble.collapse-eased",
      context: { from_height: from.height, to_height: to.height, duration_ms: COLLAPSE_MS },
    });
    bubble.animate(
      [
        { boxSizing: "border-box", overflow: "hidden", height: `${String(from.height)}px`, backgroundColor: from.background, boxShadow: from.boxShadow },
        { boxSizing: "border-box", overflow: "hidden", height: `${String(to.height)}px`, backgroundColor: to.background, boxShadow: to.boxShadow },
      ],
      { duration: COLLAPSE_MS, easing: "ease-out" },
    );
  });
}

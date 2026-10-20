/**
 * send-glide — A PROMPT THE PERSON JUST SENT GLIDES UP INTO PLACE.
 *
 * When a prompt is sent (from the Emacs input window or the composer), its
 * bubble rises from the bottom of the feed, out from behind the footer dock,
 * into its resting spot over `SEND_GLIDE_MS` (owner request, 2026-10-20). Only
 * a SEND glides: the caller is the feed's sent-prompt park, which a history
 * page, a prepend, a redraw, or a terminal re-push never reaches
 * (`isSentPrompt` in feed-view.ts).
 *
 * THE ROW CLIPS ITS OWN BOTTOM WHILE THE BUBBLE GLIDES. A sent prompt is
 * parked at the tail, so the row's bottom edge is the bottom of the feed's
 * readable area, one `--feed-bottom-gap` above the dock: the bubble starts one
 * row-height below that edge, hidden, and rises out from it. The clip is
 * `overflow-y: clip`, never a paint-only clip, because a translated box
 * otherwise counts toward the scroll box's `scrollHeight`: every follow or
 * park during the glide would land at a tail inflated by the bubble's height,
 * then jump back when it ends. A clipped box adds nothing, so the scroll
 * owner never sees the glide at all.
 *
 * Under `prefers-reduced-motion` the bubble simply appears, as before.
 */
import { log } from "../log.js";
import { prefersReducedMotion } from "../reduced-motion.js";

/** How long the bubble takes to glide into place. Quick: it is a send, not a show. */
export const SEND_GLIDE_MS = 180;

/** A fast start that settles softly into the resting spot. */
export const SEND_GLIDE_EASING = "cubic-bezier(0.2, 0.8, 0.2, 1)";

/** The flag the row carries while its bubble glides; the stylesheet clips on it. */
export const SEND_GLIDE_ATTRIBUTE = "data-send-glide";

/** The seam the glide reaches through, so a test can drive it. */
export interface SendGlideDeps {
  /** Whether the reader asked for reduced motion. */
  readonly reducedMotion: () => boolean;
}

/** The page's own reduced-motion setting. */
function pageDeps(row: HTMLElement): SendGlideDeps {
  return { reducedMotion: () => prefersReducedMotion(row.ownerDocument) };
}

/**
 * Glide BUBBLE, just drawn and parked inside ROW at the feed's tail, up from
 * below ROW's bottom edge into place. Runs at once, in the task that drew it,
 * so the first painted frame already has the bubble below the edge.
 */
export function glideSentPrompt(
  row: HTMLElement,
  bubble: HTMLElement,
  deps: SendGlideDeps = pageDeps(row),
): void {
  if (deps.reducedMotion()) {
    log.debug("a sent prompt appeared at once: the reader asked for reduced motion", {
      operation: "feed.send-glide-skipped",
      context: { why: "reduced-motion" },
    });
    return;
  }
  const rise = row.getBoundingClientRect().height;
  row.setAttribute(SEND_GLIDE_ATTRIBUTE, "");
  log.debug("a sent prompt glides up into place", {
    operation: "feed.send-glide",
    context: { rise_px: rise, duration_ms: SEND_GLIDE_MS },
  });
  const glide = bubble.animate(
    [{ transform: `translateY(${String(rise)}px)` }, { transform: "none" }],
    { duration: SEND_GLIDE_MS, easing: SEND_GLIDE_EASING },
  );
  // A glide cut short (the bubble replaced or the page torn down) ends the
  // clip exactly as a finished one does; neither outcome is a failure.
  const unclip = (): void => row.removeAttribute(SEND_GLIDE_ATTRIBUTE);
  glide.finished.then(unclip, unclip);
}

/**
 * THE PAINTED-FRAME CLOCK every WebKit page that measures the feed reads its
 * frames by (anchoring-page.ts, pinned-tail-page.ts, click-hold-page.ts).
 *
 * WHAT IT MEASURES IS WHAT IS PAINTED. A frame's sample is taken in a
 * ResizeObserver created AFTER the feed's own, whose target (a hidden fixed
 * sentinel) is resized on every animation frame: the rendering update runs
 * the scroll events, the animation frames, layout, then the resize observers
 * in creation order, and only then paints. So the sample sees the layout the
 * feed's own corrections left, which is the frame the reader sees. Reading
 * layout anywhere else -- a task between two frames -- sees a change no
 * rendering update has corrected yet, a state that is never painted.
 *
 * Build it AFTER the feed's tail owner is observing its box, so its observer
 * runs after the feed's.
 */

/** Resolves with READ's answer, taken in the next rendering update after the feed's own. */
export type PaintedFrame = <T>(read: () => T) => Promise<T>;

/** A painted-frame clock over DOC's body. */
export function paintedFrames(doc: Document = document): PaintedFrame {
  const sentinel = doc.createElement("div");
  sentinel.style.cssText = "position:fixed;left:0;top:0;height:1px;width:1px;visibility:hidden";
  doc.body.append(sentinel);
  let waiting: (() => void) | null = null;
  const observer = new ResizeObserver(() => {
    const next = waiting;
    waiting = null;
    next?.();
  });
  observer.observe(sentinel);
  let wide = false;
  return <T>(read: () => T) =>
    new Promise<T>((resolve) => {
      requestAnimationFrame(() => {
        wide = !wide;
        sentinel.style.width = wide ? "2px" : "1px";
        waiting = () => resolve(read());
      });
    });
}

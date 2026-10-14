/**
 * frame — THE ONE WAY A PAGE MODULE WAITS FOR THE NEXT ANIMATION FRAME.
 *
 * Every module that takes a frame scheduler as a seam (so a test can run the
 * frame by hand) defaults it to this: FN runs at the next frame, before it
 * paints, and is handed nothing. The frame's timestamp is dropped on purpose,
 * so no caller comes to depend on an argument its test double never passes.
 *
 * A module drawing into a view that may have no frames at all (an iframe's
 * window, a test host) reads `requestAnimationFrame` off that view instead,
 * and owns the case where it is missing (type-out.ts, bubble/body.ts).
 */

/** Run FN at the next animation frame. */
export function nextFrame(fn: () => void): void {
  requestAnimationFrame(() => fn());
}

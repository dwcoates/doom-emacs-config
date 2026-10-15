/**
 * `Range.getClientRects` FOR JSDOM — an environment substitution, because
 * jsdom lacks the capability rather than because the app has a seam here.
 *
 * jsdom lays nothing out, so the truthful answer to "where does this text
 * sit" is nowhere: no rects. The ellipsis trim (src/feed/ellipsis-trim.ts)
 * walks a clamp's characters through it whenever a title or ellipsis bubble
 * overflows, which a suite says by giving the box a taller `scrollHeight`; a
 * suite that asserts on the trim injects its own `ClampGeometry`.
 */

/**
 * Install the stub on `Range.prototype` unless the environment already has
 * one. A node-environment file has no DOM at all, so there is nothing to
 * install on; a later jsdom file in the same worker installs it then.
 */
export function installRangeClientRects(): void {
  if (typeof Range === "undefined") return;
  if (typeof Range.prototype.getClientRects === "function") return;
  Range.prototype.getClientRects = function nowhere(): DOMRectList {
    return [] as unknown as DOMRectList;
  };
}

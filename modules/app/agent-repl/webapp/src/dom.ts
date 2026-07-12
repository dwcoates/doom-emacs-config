/**
 * The one ancestor walk the feed's pointer logic needs.
 *
 * Both pointer gestures over the feed answer the same question — which
 * section, if any, does the pointer sit inside? — and differ only in what
 * makes a node a section: a wheel wants the innermost SCROLL BOX
 * (scroll.ts), a click wants the innermost CAPPED SECTION (expand.ts).
 * The walk itself is shared, so it lives here once.
 *
 * Generic over the node shape (`parentElement` is all it touches), so it
 * runs against a real DOM and against a plain object in a test alike.
 */

/**
 * Innermost node at or above `start` that satisfies `match`, stopping
 * below `stop` — `stop` itself is never a candidate, and neither is
 * anything above it. Null when nothing between `start` and `stop`
 * matches.
 */
export function ancestorMatching<T extends { parentElement: T | null }>(
  start: T | null,
  stop: T,
  match: (node: T) => boolean,
): T | null {
  for (let node = start; node && node !== stop; node = node.parentElement) {
    if (match(node)) return node;
  }
  return null;
}

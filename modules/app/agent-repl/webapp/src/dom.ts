/**
 * DOM helpers shared across the feed: the one ancestor walk the feed's pointer
 * logic needs, and the one way a redraw puts elements in order.
 *
 * THE ANCESTOR WALK.
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

/**
 * Make PARENT's children exactly DESIRED, in order, MOVING ONLY WHAT IS OUT OF
 * PLACE. Answers how many elements it had to insert or move.
 *
 * WHY NOT `replaceChildren`. Removing an element from the document and putting
 * it back is not a no-op to a browser: its layout box is torn down and rebuilt,
 * which RESETS the scroll position of every scroll box inside it and restarts
 * `content-visibility` skipping around it. A feed that re-laid every row with
 * `replaceChildren` on each live push therefore snapped every expanded bubble
 * the reader was scrolled inside back to its top, several times a second
 * (owner rule, 2026-09-23: the user owns the scroll). So an element already in
 * its place is never touched: a push that appends a row inserts that row and
 * nothing else, and a push that only redrew a row inside its chrome moves
 * nothing at all. Only a genuine reorder or reparent moves an element.
 */
export function placeChildren(parent: Element, desired: readonly Element[]): number {
  const wanted = new Set<Node>(desired);
  for (const child of [...parent.childNodes]) {
    if (!wanted.has(child)) child.remove();
  }
  let placed = 0;
  let cursor: ChildNode | null = parent.firstChild;
  for (const el of desired) {
    if (cursor === el) {
      cursor = cursor.nextSibling;
      continue;
    }
    parent.insertBefore(el, cursor);
    placed += 1;
  }
  return placed;
}

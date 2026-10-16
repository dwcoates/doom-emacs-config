/**
 * children — placing a box's children without disturbing the ones already
 * in place.
 *
 * A NODE MOVED IS A NODE RESTYLED. `replaceChildren` takes every child out
 * and puts it back even when nothing moved, and WebKit then recomputes the
 * style and layout of every subtree it re-inserted: the rail re-inserted its
 * whole roster on every push, kept sections and all (2026-10-15).
 */

/** Make CHILDREN exactly PARENT's children, in order, touching nothing already in place. */
export function placeChildren(parent: Element, children: readonly Node[]): void {
  const current = parent.childNodes;
  if (current.length === children.length && children.every((child, i) => current[i] === child)) return;
  parent.replaceChildren(...children);
}

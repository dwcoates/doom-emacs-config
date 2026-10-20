/**
 * drag — THE ONE drag-to-reorder gesture the rail has, shared by every kind of
 * item it moves: a workspace row among its repository's subsections, a
 * subsection among its repository's, a repository among the repositories.
 *
 * THE ITEM ITSELF MOVES. Pressing an item's handle and moving past a small
 * threshold lifts it: a translucent copy follows the pointer, and the item
 * stays in the list as a dimmed slot that travels to wherever the drop would
 * land, so the items around it shift out of the way as the pointer passes
 * their middles. Releasing drops it where the slot stands; Escape (or a lost
 * pointer) puts it back where it started. Near the top or bottom edge of the
 * region the item scrolls in (`DragSpec.scroller`) the region follows the
 * pointer.
 *
 * THE PAGE'S DRAWING IS HELD FROM THE MOMENT THE ITEM LIFTS
 * (`arrange.ts`'s `ArrangeHold`), because a roster push redraws a section whose
 * wire changed, and the slot with it. A drop that changes nothing, or a
 * cancelled drag, lets the hold go; a drop that changes something hands the
 * hold to the request it sends.
 *
 * A LIFTED BOX IS NEVER KEPT BY THE NEXT DRAW. The drag moves elements inside
 * a drawn section, so that section's DOM no longer matches the wire it was
 * drawn from. Lifting marks the box (`REARRANGED_ATTRIBUTE`) and the roster
 * drawer redraws a marked box from its wire whatever the push says, so a
 * refused or cancelled drag can never leave a rearranged section on screen.
 *
 * A DRAG IS NOT A CLICK. The click a browser fires after the release would
 * select the row or fold the section under the pointer, so the first click
 * after a drag is swallowed. It is forgotten at the next press, so a release
 * that fires no click cannot swallow a later, real one. The guard belongs to
 * the WINDOW the drag happens in and stands only until it has done its one
 * job (`swallowTheNextClick`).
 */
import { queryAll, windowOf } from "../dom.js";
import { nextFrame } from "../frame.js";
import { log } from "../log.js";
import { dragEdgeScroll } from "../scroll.js";
import type { ArrangeContext } from "./context.js";

/** How far the pointer moves, in px, before a press becomes a drag. */
export const DRAG_THRESHOLD_PX = 4;

/** How close to the scroller's edge, in px, the pointer scrolls it. */
export const DRAG_SCROLL_EDGE_PX = 28;

/** The class the lifted item's slot wears while it travels. */
export const DRAG_SLOT_CLASS = "sb-drag-slot";

/** The class of the copy that follows the pointer. */
export const DRAG_GHOST_CLASS = "sb-drag-ghost";

/** The class the page's root wears while a drag is in progress. */
export const DRAGGING_CLASS = "sb-dragging";

/** The attribute a drawn box wears once a drag has rearranged it; see the file comment. */
export const REARRANGED_ATTRIBUTE = "data-rearranged";

/** One kind of draggable item, and what a drop of it asks for. */
export interface DragSpec {
  /** Which kind of item this is, for the records. */
  kind: string;
  /** The element that moves. */
  item: HTMLElement;
  /** The drawn box the drag rearranges, which the next draw must not keep. */
  box: HTMLElement;
  /** The region the item scrolls in, which follows the pointer near its edges. */
  scroller: HTMLElement;
  /** The lists the item may be dropped into, read when it lifts. */
  lists: () => HTMLElement[];
  /** LIST's items, in order: the elements the slot moves among. */
  itemsOf: (list: HTMLElement) => HTMLElement[];
  /**
   * The item was dropped into LIST just before BEFORE (null: after every
   * item there), somewhere other than where it started.
   */
  dropped: (list: HTMLElement, before: HTMLElement | null) => void;
}

/** Where an item stood in its list: its list and the item after it. */
interface Spot {
  list: HTMLElement;
  next: Node | null;
}

/**
 * Swallow the next click in WIN, the one a finished drag leaves behind,
 * unless a press comes first; see the file comment. THE GUARD STANDS ONLY
 * WHILE A CLICK IS OWED: the swallowed click or the next press takes it
 * down, so nothing is installed for the page's life that could outlive the
 * listeners it relies on.
 */
export function swallowTheNextClick(win: Window): void {
  const forget = (): void => {
    win.removeEventListener("click", swallow, true);
    win.removeEventListener("pointerdown", forget, true);
  };
  const swallow = (event: Event): void => {
    forget();
    event.preventDefault();
    event.stopPropagation();
  };
  win.addEventListener("click", swallow, true);
  win.addEventListener("pointerdown", forget, true);
}

/**
 * A press on SPEC's handle: watch the pointer until it lifts the item past
 * the threshold, or lets go first and the press stays an ordinary click.
 */
export function pressToDrag(arrange: ArrangeContext, spec: DragSpec, down: PointerEvent): void {
  const win = windowOf(spec.item);
  const startX = down.clientX;
  const startY = down.clientY;
  let lifted: Lifted | null = null;

  const move = (event: PointerEvent): void => {
    if (event.pointerId !== down.pointerId) return;
    if (lifted === null) {
      if (Math.hypot(event.clientX - startX, event.clientY - startY) < DRAG_THRESHOLD_PX) return;
      lifted = lift(arrange, spec, down);
    }
    event.preventDefault();
    lifted.follow(event.clientX, event.clientY);
  };
  const end = (event: PointerEvent): void => {
    if (event.pointerId !== down.pointerId) return;
    stop();
    if (lifted === null) return;
    if (event.type === "pointerup") lifted.drop();
    else lifted.cancel("the pointer was lost");
  };
  // A DRAG SELECTS NO TEXT: the browser would otherwise sweep a selection
  // across every row the pointer passes.
  const noSelect = (event: Event): void => {
    if (lifted !== null) event.preventDefault();
  };
  const key = (event: KeyboardEvent): void => {
    if (event.key !== "Escape" || lifted === null) return;
    event.preventDefault();
    stop();
    lifted.cancel("Escape");
  };
  const stop = (): void => {
    win.removeEventListener("pointermove", move);
    win.removeEventListener("pointerup", end);
    win.removeEventListener("pointercancel", end);
    win.removeEventListener("keydown", key, true);
    document.removeEventListener("selectstart", noSelect, true);
  };
  win.addEventListener("pointermove", move);
  win.addEventListener("pointerup", end);
  win.addEventListener("pointercancel", end);
  win.addEventListener("keydown", key, true);
  document.addEventListener("selectstart", noSelect, true);
}

/** A lifted item: it follows the pointer until it is dropped or cancelled. */
interface Lifted {
  follow(x: number, y: number): void;
  drop(): void;
  cancel(why: string): void;
}

/** Lift SPEC's item: hold the drawing, mark its box, draw the copy, turn the item into its slot. */
function lift(arrange: ArrangeContext, spec: DragSpec, down: PointerEvent): Lifted {
  const { item } = spec;
  const parent = item.parentElement;
  if (parent === null) throw new Error(`invariant: a ${spec.kind} item is lifted from a list`);
  const origin: Spot = { list: parent, next: item.nextSibling };
  const lists = spec.lists();
  if (!lists.includes(parent)) throw new Error(`invariant: a ${spec.kind} item's own list is one it may be dropped into`);
  arrange.hold.drag();
  spec.box.setAttribute(REARRANGED_ATTRIBUTE, "true");
  item.ownerDocument.getSelection()?.removeAllRanges();
  log.debug("lifted a sidebar item to drag it", { operation: "sidebar.drag.lift", context: { kind: spec.kind } });

  const rect = item.getBoundingClientRect();
  const ghost = item.cloneNode(true) as HTMLElement;
  ghost.classList.add(DRAG_GHOST_CLASS);
  // THE COPY IS NO ROW: whatever finds rows by their hook finds the item once.
  for (const hooked of [ghost, ...queryAll(ghost, "[data-roster-row]")]) hooked.removeAttribute("data-roster-row");
  ghost.style.width = `${rect.width}px`;
  const grabX = down.clientX - rect.left;
  const grabY = down.clientY - rect.top;
  arrange.layer.appendChild(ghost);
  item.classList.add(DRAG_SLOT_CLASS);
  arrange.layer.ownerDocument.documentElement.classList.add(DRAGGING_CLASS);

  const { scroller } = spec;
  let pointerY = down.clientY;
  /** Whether a frame is already asked for the edge scroll, and whether the drag still stands. */
  let scrolling = false;
  let lifting = true;
  const scroll = (): void => {
    scrolling = false;
    if (!lifting) return;
    const box = scroller.getBoundingClientRect();
    const step =
      pointerY < box.top + DRAG_SCROLL_EDGE_PX ? -8 : pointerY > box.bottom - DRAG_SCROLL_EDGE_PX ? 8 : 0;
    if (step === 0) return;
    dragEdgeScroll(scroller, step);
    place(pointerY);
    scrolling = true;
    nextFrame(scroll);
  };

  const place = (y: number): void => {
    const list = nearestList(lists, y);
    const items = spec.itemsOf(list).filter((other) => other !== item);
    const before = items.find((other) => {
      const box = other.getBoundingClientRect();
      return y < box.top + box.height / 2;
    });
    const reference = before ?? null;
    if (item.parentElement === list && nextItem(spec, list, item) === reference) return;
    list.insertBefore(item, reference);
  };

  const finish = (): void => {
    lifting = false;
    ghost.remove();
    item.classList.remove(DRAG_SLOT_CLASS);
    arrange.layer.ownerDocument.documentElement.classList.remove(DRAGGING_CLASS);
    swallowTheNextClick(windowOf(item));
  };

  const follow = (x: number, y: number): void => {
    pointerY = y;
    ghost.style.left = `${x - grabX}px`;
    ghost.style.top = `${y - grabY}px`;
    place(y);
    if (!scrolling) {
      scrolling = true;
      nextFrame(scroll);
    }
  };
  follow(down.clientX, down.clientY);

  return {
    follow,
    drop(): void {
      finish();
      const list = item.parentElement;
      if (list === null) throw new Error(`invariant: a dropped ${spec.kind} item is still in a list`);
      const before = nextItem(spec, list, item);
      if (list === origin.list && before === nextItemFrom(spec, list, origin.next)) {
        log.debug("a sidebar item was dropped where it started", { operation: "sidebar.drag.unmoved", context: { kind: spec.kind } });
        arrange.hold.cancel();
        return;
      }
      log.info("dropped a sidebar item in a new place", { operation: "sidebar.drag.drop", context: { kind: spec.kind } });
      spec.dropped(list, before);
    },
    cancel(why: string): void {
      finish();
      origin.list.insertBefore(item, origin.next);
      log.info("a sidebar drag was cancelled; the item is back where it started", {
        operation: "sidebar.drag.cancel",
        context: { kind: spec.kind, why },
      });
      arrange.hold.cancel();
    },
  };
}

/** The list whose box holds Y, or else the one nearest it. */
function nearestList(lists: HTMLElement[], y: number): HTMLElement {
  let best = lists[0];
  let bestDistance = Infinity;
  for (const list of lists) {
    const box = list.getBoundingClientRect();
    const distance = y < box.top ? box.top - y : y > box.bottom ? y - box.bottom : 0;
    if (distance < bestDistance) {
      best = list;
      bestDistance = distance;
    }
  }
  if (best === undefined) throw new Error("invariant: a drag has at least its own list to land in");
  return best;
}

/** The item after ITEM in LIST, null when it is the last. */
function nextItem(spec: DragSpec, list: HTMLElement, item: HTMLElement): HTMLElement | null {
  const items = spec.itemsOf(list);
  return items[items.indexOf(item) + 1] ?? null;
}

/** The first item of LIST at or after NODE, null when there is none. */
function nextItemFrom(spec: DragSpec, list: HTMLElement, node: Node | null): HTMLElement | null {
  const items = spec.itemsOf(list);
  for (let at: Node | null = node; at !== null; at = at.nextSibling) {
    if (items.includes(at as HTMLElement)) return at as HTMLElement;
  }
  return null;
}

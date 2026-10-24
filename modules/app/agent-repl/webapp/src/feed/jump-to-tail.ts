/**
 * THE JUMP-TO-TAIL CONTROL (owner request, 2026-10-24): a small down arrow
 * over the feed's bottom edge, shown while the reader stands more than one
 * viewport above the feed's tail (`fartherThanAPage`). Pressing it parks the
 * feed at its tail and follows (`TailFollow.jumpToTail`, the `tailJumped`
 * scroll cause), so new content keeps the tail in view again.
 *
 * It is drawn in its own slot OUTSIDE the scroll box (`#feed-jump`, between
 * the box and the docked footer), so it never scrolls with the content and a
 * press on it is no click in the feed: the feed's click hold
 * (`holdViewOnClicks`) never sees it.
 *
 * Whether it shows is decided from the box's own geometry each time the tail
 * owner reports the view changed (`TailFollow.onViewChanged`), which it does
 * after every scroll event and every resize pass.
 */
import { createControl } from "../control.js";
import { log } from "../log.js";
import type { ScrollPosition, TailFollow } from "../scroll.js";

/** The class the control wears. */
export const JUMP_TO_TAIL_CLASS = "feed-jump-to-tail";

/** The attribute the control wears while it shows. */
export const JUMP_TO_TAIL_SHOWN_ATTRIBUTE = "data-shown";

/** A downward chevron. */
const ARROW_ICON =
  `<svg class="feed-jump-to-tail-icon" viewBox="0 0 16 16" aria-hidden="true">` +
  `<path d="M4 6.25l4 4l4-4"/>` +
  `</svg>`;

const LABEL = "Jump to the latest";

/**
 * Whether BOX stands more than one viewport above its tail: the content below
 * the view is taller than the view itself.
 */
export function fartherThanAPage(box: ScrollPosition): boolean {
  return box.scrollHeight - box.scrollTop - box.clientHeight > box.clientHeight;
}

/**
 * Draw the jump-to-tail control into SLOT, shown and hidden by TAIL's view
 * reports, and pressing it jumps TAIL to the tail. Answers the uninstall,
 * which removes the control.
 */
export function mountJumpToTail(slot: HTMLElement, tail: TailFollow): () => void {
  const control = createControl(slot.ownerDocument);
  control.className = JUMP_TO_TAIL_CLASS;
  control.title = LABEL;
  control.setAttribute("aria-label", LABEL);
  control.innerHTML = ARROW_ICON;
  let shown = false;
  const setShown = (next: boolean, box: ScrollPosition): void => {
    if (next === shown) return;
    shown = next;
    control.toggleAttribute(JUMP_TO_TAIL_SHOWN_ATTRIBUTE, next);
    // Hidden, it is out of the tab order and of the accessibility tree too.
    control.tabIndex = next ? 0 : -1;
    control.setAttribute("aria-hidden", next ? "false" : "true");
    log.debug(next ? "the jump-to-tail control shows" : "the jump-to-tail control hides", {
      operation: "feed.jump-to-tail-shown",
      context: {
        shown: next,
        below_view: box.scrollHeight - box.scrollTop - box.clientHeight,
        client_height: box.clientHeight,
      },
    });
  };
  control.tabIndex = -1;
  control.setAttribute("aria-hidden", "true");
  control.addEventListener("click", (event) => {
    event.preventDefault();
    log.info("the reader jumped to the feed's tail", {
      operation: "feed.jump-to-tail",
      context: { shown },
    });
    tail.jumpToTail();
  });
  tail.onViewChanged((box) => setShown(fartherThanAPage(box), box));
  slot.append(control);
  return () => {
    control.remove();
  };
}

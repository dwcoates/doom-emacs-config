/**
 * selection-scroll — THE SELECTED ROW IS SCROLLED INTO ITS SECTION'S VIEW.
 *
 * WHY IT EXISTS (owner request, 2026-10-20). The rail never scrolls; its
 * sections do (`band-fit.ts`), so a selected workspace can sit below the
 * Open section's viewport, highlighted where nobody sees it. Once a workspace
 * becomes the highlighted one, its section's viewport scrolls until its line
 * shows.
 *
 * ONLY AS FAR AS THE LINE NEEDS (owner request, 2026-10-26). A line already in
 * view moves nothing. A line above the view is brought to its top edge, and one
 * below it to its bottom edge, never centered, so a switch (`M-}` or any other)
 * moves the rail no more than showing the line takes. The view is the
 * viewport less its scroll padding: the stylesheet pads a faded foot's strip
 * (`--band-foot-fade`) out of it in the same rule that paints the fade, so a
 * line is never left under the fade.
 *
 * ONCE PER SELECTION. The rail is redrawn on every push, and the reader may
 * scroll the section away from the selected row; a push that leaves the same
 * workspace highlighted moves nothing. Only a row that is drawn with a box
 * counts as followed: one in a folded section, a collapsed repository or
 * narrowed away by the search is followed on the first draw that lays it out.
 */
import { computedPx, queryAll } from "../dom.js";
import { log } from "../log.js";
import { selectionRowScroll } from "../scroll.js";
import { REGION_SELECTOR } from "./band-fit.js";

/** What became of following a selection into view. */
export type FollowOutcome =
  /** No row is highlighted. */
  | "none"
  /** The highlighted row was already followed. */
  | "followed"
  /** The highlighted row draws no box, so it is left for a later draw. */
  | "unlaid"
  /** The highlighted row's line already shows whole. */
  | "visible"
  /** The section scrolled to show the highlighted row's line. */
  | "scrolled";

/** The rail's follower of the highlighted row. */
export interface SelectionScroll {
  /** Scroll ROOT's highlighted row into its section's view, once per selection. */
  follow(root: HTMLElement): FollowOutcome;
}

/** The highlighted row's own line: the `.row` of the one `[data-roster-row][data-current]`. */
function highlightedLine(root: HTMLElement): { id: string; line: HTMLElement } | null {
  const rows = queryAll<HTMLElement>(root, "[data-roster-row][data-current]");
  if (rows.length === 0) return null;
  const ids = rows.map((row) => row.getAttribute("data-roster-row"));
  if (rows.length > 1) {
    log.error("the rail highlights more than one row", {
      operation: "sidebar.selection-scroll.ambiguous",
      context: { workspaces: ids },
    });
    throw new Error(`sidebar selection scroll: ${rows.length} rows are highlighted`);
  }
  const line = rows[0].querySelector<HTMLElement>(":scope > .row");
  if (line === null) {
    log.error("the highlighted rail row is drawn without its line", {
      operation: "sidebar.selection-scroll.malformed",
      context: { workspace: ids[0] },
    });
    throw new Error(`sidebar selection scroll: row ${ids[0] ?? "?"} has no line`);
  }
  return { id: ids[0] ?? "", line };
}

/** Where a section's viewport was scrolled from and to. */
export interface BandScroll {
  from: number;
  to: number;
}

/** The class of the rail's body, which the Chat app's thread list scrolls in. */
export const RAIL_BODY_CLASS = "sb-scroll";

/**
 * The element LINE scrolls in: a band row's section rows region, or, for a
 * chat thread's row, the rail's body, as the Chat app's list is no band.
 */
function scrollerOf(line: HTMLElement): HTMLElement {
  const thread = line.closest("[data-thread-section]") !== null;
  const scroller = line.closest<HTMLElement>(thread ? `.${RAIL_BODY_CLASS}` : REGION_SELECTOR);
  if (scroller === null) {
    log.error(thread ? "a chat thread's row hangs outside the rail's body" : "a rail row hangs in no section's rows region", {
      operation: "sidebar.selection-scroll.orphan-row",
      context: { workspace: line.parentElement?.getAttribute("data-roster-row") ?? null, thread },
    });
    throw new Error(`sidebar selection scroll: a row hangs in no ${thread ? "rail body" : "section's rows region"}`);
  }
  return scroller;
}

/** The scroll padding that insets a viewport's edges. */
type ScrollPaddingEdge = "scrollPaddingTop" | "scrollPaddingBottom";

/**
 * SCROLLER's computed scroll padding on EDGE, in px: `auto`, the initial value
 * a viewport the stylesheet pads nothing out of computes to, is none.
 */
function scrollPadding(scroller: HTMLElement, edge: ScrollPaddingEdge): number {
  const value = window.getComputedStyle(scroller)[edge];
  if (value === "auto") return 0;
  const px = computedPx(value);
  if (px === null) {
    log.error("the selected row cannot be scrolled into view: its viewport's scroll padding is not in px", {
      operation: "sidebar.selection-scroll.unmeasurable",
      context: { edge, value, scroller: scroller.className },
    });
    throw new RangeError(`sidebar selection scroll: computed ${edge} '${value}' is not in px`);
  }
  return px;
}

/**
 * Scroll LINE's section viewport (the rail's body, for a chat thread's row)
 * only as far as LINE needs to show whole: its top to the view's top when it
 * lies above, its bottom to the view's bottom when it lies below. Answers how
 * it moved, or null when LINE already showed whole.
 */
export function scrollLineIntoBand(line: HTMLElement): BandScroll | null {
  const region = scrollerOf(line);
  const box = region.getBoundingClientRect();
  const viewportTop = box.top + region.clientTop;
  const viewTop = viewportTop + scrollPadding(region, "scrollPaddingTop");
  const viewBottom = viewportTop + region.clientHeight - scrollPadding(region, "scrollPaddingBottom");
  const edges = line.getBoundingClientRect();
  if (edges.top >= viewTop && edges.bottom <= viewBottom) return null;
  const from = region.scrollTop;
  selectionRowScroll(region, edges.top < viewTop ? edges.top - viewTop : edges.bottom - viewBottom);
  return { from, to: region.scrollTop };
}

/** The rail's follower: it remembers the last workspace it followed. */
export function createSelectionScroll(): SelectionScroll {
  let followed: string | null = null;
  return {
    follow(root: HTMLElement): FollowOutcome {
      const highlighted = highlightedLine(root);
      if (highlighted === null) return "none";
      const { id, line } = highlighted;
      if (id === followed) return "followed";
      if (line.getBoundingClientRect().height <= 0) return "unlaid";
      followed = id;
      const scrolled = scrollLineIntoBand(line);
      log.debug(scrolled === null ? "the selected row already shows" : "scrolled the selected row into its section's view", {
        operation: "sidebar.selection-scroll",
        context: { workspace: id, ...(scrolled ?? {}) },
      });
      return scrolled === null ? "visible" : "scrolled";
    },
  };
}

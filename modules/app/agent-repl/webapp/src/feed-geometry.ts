/**
 * feed-geometry — where the root feed's scroll box stands against what it
 * holds, as the page census states it.
 *
 * WHY IT EXISTS (2026-11-23). Pages kept on screen were seen blank: the
 * topbar and the footer drawn, the feed area empty, the census counting rows
 * all the while. A page is a cross-origin frame of the webhost, so nothing
 * outside it can read its layout; the page states it itself. The census
 * carries the geometry every interval, and a feed that holds rows while none
 * of them meets its viewport is a WARN (`feed.blank-view`).
 *
 * Positions are in the scroll box's content coordinates (0 is the top of what
 * it scrolls), so they compare directly with `scroll_top`.
 */
import { FEED_ROW_ATTRIBUTE } from "./feed/row-id.js";

/** How many of the feed's last root rows the geometry describes. */
export const GEOMETRY_TAIL_ROWS = 3;

/** One box, as the geometry describes it. */
export interface BoxGeometry {
  /** Its tag, id and classes. */
  readonly what: string;
  /** Its top, in the scroll box's content coordinates. */
  readonly top: number;
  readonly height: number;
  readonly opacity: string;
  readonly visibility: string;
  readonly display: string;
}

/** The root feed's scroll box and what stands in it. */
export interface FeedGeometry {
  readonly scroll_top: number;
  readonly scroll_height: number;
  readonly client_height: number;
  /** The root rows of `#feed`. */
  readonly root_rows: number;
  /** How many of them meet the viewport. */
  readonly rows_in_view: number;
  /** Every child of the scroll box, `#feed` and the trays included. */
  readonly children: readonly BoxGeometry[];
  /** The last GEOMETRY_TAIL_ROWS root rows. */
  readonly tail: readonly BoxGeometry[];
}

/** The root feed's geometry in DOC, or null when the page draws no root feed. */
export function feedGeometry(doc: Document): FeedGeometry | null {
  const box = doc.getElementById("feed-scroll");
  const feed = doc.getElementById("feed");
  const view = doc.defaultView;
  if (box === null || feed === null || view === null) return null;
  const boxTop = box.getBoundingClientRect().top;
  const scrollTop = box.scrollTop;
  const describe = (el: Element): BoxGeometry => {
    const rect = el.getBoundingClientRect();
    const style = view.getComputedStyle(el);
    return {
      what: label(el),
      top: Math.round(rect.top - boxTop + scrollTop),
      height: Math.round(rect.height),
      opacity: style.opacity,
      visibility: style.visibility,
      display: style.display,
    };
  };
  const rows = [...feed.children].filter((el) => el.hasAttribute(FEED_ROW_ATTRIBUTE));
  const viewTop = boxTop;
  const viewBottom = boxTop + box.clientHeight;
  const inView = rows.filter((row) => {
    const rect = row.getBoundingClientRect();
    return rect.height > 0 && rect.bottom > viewTop && rect.top < viewBottom;
  }).length;
  return {
    scroll_top: Math.round(scrollTop),
    scroll_height: box.scrollHeight,
    client_height: box.clientHeight,
    root_rows: rows.length,
    rows_in_view: inView,
    children: [...box.children].map(describe),
    tail: rows.slice(-GEOMETRY_TAIL_ROWS).map(describe),
  };
}

/** Whether GEOMETRY is a feed holding rows while none of them meets its viewport. */
export function blankView(geometry: FeedGeometry): boolean {
  return geometry.root_rows > 0 && geometry.client_height > 0 && geometry.rows_in_view === 0;
}

/** EL's tag, id and classes, as a selector reads them. */
function label(el: Element): string {
  const id = el.id === "" ? "" : `#${el.id}`;
  const classes = [...el.classList].map((c) => `.${c}`).join("");
  return `${el.tagName.toLowerCase()}${id}${classes}`;
}

/**
 * capTopbarTitle — the SECOND layer of the topbar's centering.
 *
 * THE GRID CENTERS THE TRACK; THIS CENTERS THE TEXT. `.topbar-row`'s three
 * tracks (styles.css) put the middle track's midpoint on the strip's midpoint
 * at every width, and that is all a grid can do without knowing what the two
 * flank groups actually measure. A long title fills its whole track, so with a
 * narrow left group and a wide right one the visible text ends one gap from
 * the right-hand chips while clear room is left on the left — a centered
 * TRACK, which reads as an off-center title. Measured live: row 1090px wide,
 * tracks 292.5 / 472.9 / 292.5, left group 160px (132px of its track empty),
 * right group filling its 292.5.
 *
 * So after every draw the title's width is capped from the MEASURED flanks:
 *
 *   titleMax = rowWidth − 2 · edgePadding − 2 · max(leftWidth, rightWidth)
 *              − 2 · gap
 *
 * which is the widest box that keeps the same clear space on both sides of the
 * text whichever flank is the wider one. EVERY TERM THE ROW SPENDS IS IN IT:
 * the row's border box carries the strip's own edge inset at each end, then a
 * flank group, then a track gap, before the title's box begins — so a cap that
 * left the padding out would be one inset too wide on each side and the
 * symmetry would be approximate rather than exact. The inset and the gap are
 * the same token by declaration (`padding: 0 var(--topbar-cell-gap)`), and
 * both are read from the row rather than assumed equal here. It lands as an inline `max-width` on
 * the title element; `text-align: center`, the ellipsis and the tooltip are
 * the stylesheet's and stay exactly as they were, so a page with no JS running
 * still gets today's layout rather than a broken one.
 *
 * THE ARITHMETIC IS PURE and the measuring is injected, because jsdom reports
 * every rect as zero and a "caps correctly" test cannot be written against a
 * layout engine that has none.
 */
import { log } from "../log.js";

/**
 * The narrowest cap applied, in px.
 *
 * A strip whose flanks together leave less than this has no honest symmetric
 * room left; the title takes the floor and the ellipsis does the rest, which
 * is the same concession the stylesheet's own clamp makes.
 */
export const TITLE_CAP_FLOOR_PX = 48;

/** What the cap needs measured off the drawn row. */
export interface TitleMetrics {
  /** The border box width of an element, in px. */
  width(el: Element): number;
  /** The row's computed `--topbar-cell-gap`, in px. */
  gap(row: HTMLElement): number;
  /** The row's computed edge inset — its `padding-left` — in px. */
  edgePadding(row: HTMLElement): number;
}

/** The real measurement: the browser's own boxes. */
export const LIVE_METRICS: TitleMetrics = {
  width: (el) => el.getBoundingClientRect().width,
  gap: (row) => {
    const style = getComputedStyle(row);
    // `columnGap` IS the token resolved to px — the row sets `gap:
    // var(--topbar-cell-gap)` — so reading it costs no second parse of a rem
    // value. The custom property itself is the fallback for a computed style
    // that reports the keyword `normal`.
    const resolved = Number.parseFloat(style.columnGap);
    if (Number.isFinite(resolved)) return resolved;
    return Number.parseFloat(style.getPropertyValue("--topbar-cell-gap"));
  },
  edgePadding: (row) => {
    // The row is symmetric by declaration, so one side names the inset.
    const resolved = Number.parseFloat(getComputedStyle(row).paddingLeft);
    return Number.isFinite(resolved) ? resolved : 0;
  },
};

/**
 * The cap for a row of these measurements, or `null` when there is nothing to
 * measure (a hidden panel: every box is zero and any cap computed from it
 * would be a lie applied to the next real layout).
 */
export function titleCapPx(
  rowWidth: number,
  leftWidth: number,
  rightWidth: number,
  gap: number,
  edgePadding: number,
): number | null {
  if (!(rowWidth > 0)) return null;
  const flank = Math.max(leftWidth, rightWidth);
  const room = rowWidth - 2 * edgePadding - 2 * flank - 2 * gap;
  return Math.max(TITLE_CAP_FLOOR_PX, room);
}

/**
 * Cap the title inside ROW, which must already be in the document — the
 * measurement is of laid-out boxes, so a row measured before insertion reports
 * zeros and is skipped.
 *
 * A row without all three groups is not a topbar row; it is left alone.
 */
export function capTopbarTitle(row: HTMLElement, metrics: TitleMetrics = LIVE_METRICS): void {
  const left = row.querySelector<HTMLElement>(".topbar-left");
  const title = row.querySelector<HTMLElement>(".topbar-title");
  const right = row.querySelector<HTMLElement>(".topbar-right");
  if (left === null || title === null || right === null) {
    log.debug("no topbar row to cap the title in", { operation: "topbar.titleCap" });
    return;
  }

  const rowWidth = metrics.width(row);
  const cap = titleCapPx(
    rowWidth,
    metrics.width(left),
    metrics.width(right),
    metrics.gap(row),
    metrics.edgePadding(row),
  );
  if (cap === null) {
    // NOT A FAILURE AND NOT A WARNING. A workspace whose panel is not shown
    // draws its topbar all the same, and every box in it is zero until the
    // panel appears — at which point the next draw or the resize caps it.
    log.debug("the topbar row has no width yet; leaving the title uncapped", {
      operation: "topbar.titleCap",
    });
    return;
  }

  title.style.maxWidth = `${cap}px`;
}

/** A standing resize cap, removed by its own answer. */
export interface TitleCapHandle {
  dispose(): void;
}

/**
 * Re-cap the CURRENT row of STRIP on every window resize, at most once per
 * frame.
 *
 * The row is read from the strip at each firing rather than captured, because
 * a push replaces it: a captured row would be a detached element whose boxes
 * are all zero, and the live one would keep the cap from whatever width the
 * window last had.
 */
export function watchTitleCap(strip: HTMLElement, metrics: TitleMetrics = LIVE_METRICS): TitleCapHandle {
  let frame: number | null = null;

  const onResize = (): void => {
    if (frame !== null) return;
    frame = requestAnimationFrame(() => {
      frame = null;
      const row = strip.querySelector<HTMLElement>(".topbar-row");
      if (row !== null) capTopbarTitle(row, metrics);
    });
  };

  window.addEventListener("resize", onResize);

  return {
    dispose(): void {
      window.removeEventListener("resize", onResize);
      if (frame !== null) cancelAnimationFrame(frame);
      frame = null;
    },
  };
}

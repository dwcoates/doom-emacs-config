/**
 * merged-fit — how many Recently Merged entries the rail shows, and the count
 * the folded band says.
 *
 * NO FIXED CAP, AND THE COUNT IS WHAT IS SHOWN (owner request, 2026-10-08).
 * The daemon sends EVERY merged workspace, most recently merged first. The
 * band shows as many of them, from the top, as FIT in the rail without
 * scrolling, and the "(N)" its folded header draws is that same N: the number
 * of entries visible when it is unfolded. Only this page knows its own height,
 * so "fits" is decided here, and it is decided ONCE: `fitMergedSection`
 * computes N and, in the same pass, hides every row past N and writes N into
 * the count. Nothing else writes either, so the two cannot drift.
 *
 * N DOES NOT DEPEND ON THE FOLD. The room is the rail's visible height less
 * everything the pane draws OTHER than the band's rows region, and a folded
 * band's rows region takes no height, so the folded and the unfolded band
 * compute the same N from the same layout.
 *
 * AN UNLAID BAND KEEPS ITS DRAWN STATE. A pane that is not shown (the other
 * grouping), a rail not yet on the page, or a row unit that measures nothing
 * cannot be fitted; such a band is left as drawn — every row, under the
 * daemon's count of every row — which is itself consistent, and it is fitted
 * the moment it lays out (the rail observes its scroller and its panes).
 */
import { log } from "../log.js";

/** The class of the band's section, of its rows region, and of the row-height probe. */
export const MERGED_SECTION_CLASS = "merged-section";
export const MERGED_ROW_PROBE_CLASS = "merged-row-probe";

/** The attribute the band states how many entries it shows on. */
export const MERGED_SHOWN_ATTRIBUTE = "data-merged-shown";

/**
 * How many entries fit: the whole rows that AVAILABLE pixels hold at
 * ROW_HEIGHT each, never fewer than none nor more than TOTAL.
 */
export function mergedRowsThatFit(available: number, rowHeight: number, total: number): number {
  if (!(rowHeight > 0)) throw new RangeError(`merged rows cannot be fitted at row height ${rowHeight}`);
  // A hair of tolerance, so a room of exactly N rows in sub-pixel layout is N.
  const whole = Math.floor(available / rowHeight + 1e-6);
  return Math.max(0, Math.min(total, whole));
}

/** A box's declared vertical padding, in pixels; an undeclared one is zero. */
function verticalPadding(el: HTMLElement): number {
  const style = getComputedStyle(el);
  const px = (value: string): number => (value === "" ? 0 : Number.parseFloat(value));
  return px(style.paddingTop) + px(style.paddingBottom);
}

/**
 * Fit the band in PANE, which hangs in the rail's SCROLLER: show its first N
 * rows, hide the rest, and say N in its count. Returns N, or null when the
 * band is absent, empty, or not laid out (left as drawn; see the module note).
 */
export function fitMergedSection(scroller: HTMLElement, pane: HTMLElement): number | null {
  const section = pane.querySelector<HTMLElement>(`:scope > .${MERGED_SECTION_CLASS}`);
  if (section === null || section.hidden) return null;
  const region = section.querySelector<HTMLElement>(":scope > .rows");
  const probe = section.querySelector<HTMLElement>(`:scope > .${MERGED_ROW_PROBE_CLASS}`);
  const count = section.querySelector<HTMLElement>(":scope > .repo-head [data-section-count]");
  if (region === null || probe === null || count === null) {
    log.error("the Recently Merged band cannot be fitted: it is missing a part it is drawn with", {
      operation: "sidebar.merged-fit.malformed",
      context: { rows: region !== null, probe: probe !== null, count: count !== null },
    });
    throw new Error("merged fit: the band is missing its rows region, row probe or count");
  }
  const rows = [...region.children].filter((el): el is HTMLElement => el instanceof HTMLElement);
  const rowHeight = probe.offsetHeight;
  const paneHeight = pane.offsetHeight;
  if (rowHeight <= 0 || paneHeight <= 0 || scroller.clientHeight <= 0) {
    log.debug("left the Recently Merged band as drawn: it is not laid out", {
      operation: "sidebar.merged-fit.unlaid",
      context: { row_height: rowHeight, pane_height: paneHeight, scroller_height: scroller.clientHeight },
    });
    return null;
  }
  const others = paneHeight - region.offsetHeight;
  const available = scroller.clientHeight - verticalPadding(scroller) - others;
  const shown = mergedRowsThatFit(available, rowHeight, rows.length);
  rows.forEach((row, index) => {
    row.hidden = index >= shown;
  });
  count.textContent = `(${shown})`;
  section.setAttribute(MERGED_SHOWN_ATTRIBUTE, String(shown));
  log.debug("fitted the Recently Merged band", {
    operation: "sidebar.merged-fit",
    context: { total: rows.length, shown, available, row_height: rowHeight },
  });
  return shown;
}

/** Fit the band of every pane in SCROLLER (the shown one lays out; the other is left). */
export function fitMergedSections(scroller: HTMLElement): void {
  for (const pane of scroller.querySelectorAll<HTMLElement>(".sb-pane")) fitMergedSection(scroller, pane);
}

/** The rail's re-fit: on every draw, and whenever the scroller or a pane changes size. */
export interface MergedFit {
  /** Fit after a draw, and watch the freshly drawn panes. */
  refit(): void;
  dispose(): void;
}

/**
 * Keep SCROLLER's bands fitted: a resize of the rail (the window, the editor
 * pane) or of a pane (a section folded or unfolded, the grouping switched)
 * re-fits, because N is a function of exactly that layout.
 */
export function installMergedFit(scroller: HTMLElement): MergedFit {
  const observer = new ResizeObserver(() => fitMergedSections(scroller));
  observer.observe(scroller);
  let panes: HTMLElement[] = [];
  return {
    refit(): void {
      for (const pane of panes) observer.unobserve(pane);
      panes = [...scroller.querySelectorAll<HTMLElement>(".sb-pane")];
      for (const pane of panes) observer.observe(pane);
      fitMergedSections(scroller);
    },
    dispose(): void {
      observer.disconnect();
      panes = [];
    },
  };
}

/**
 * band-fit — how many lines of the Hidden Workspaces and Recently Merged bands
 * the rail shows, each band scrolling through the rest.
 *
 * THE BANDS SHARE THE RAIL'S LEFTOVER ROOM (owner request, 2026-10-11). The
 * daemon sends every row of both bands (Recently Merged holds a week of
 * merges). Only this page knows its own height, so the split is decided here:
 *
 *   - When every line of both bands fits below the rest of the pane, every
 *     line shows and neither band scrolls.
 *   - Otherwise the rail has no more vertical space, and each band is a
 *     scrolling viewport of at most `BAND_CAP` lines.
 *   - Room for both bands' capped viewports and more grows them, one line at
 *     a time, Hidden Workspaces first.
 *   - Room one line short of both capped viewports goes to Hidden
 *     Workspaces: Recently Merged gives up its last line, so long as it still
 *     shows `MERGED_FLOOR`.
 *   - Less room than that keeps both capped viewports, and the rail scrolls.
 *
 * A LINE IS ONE `.row`, a nested family member included, and a viewport of K
 * lines is exactly as tall as the first K lines are drawn, a Recently Merged
 * repository group's header riding with the line under it, so no line is
 * ever cut through. A folded band shows no lines and takes no room.
 *
 * THE COUNT IS THE DAEMON'S. Every row is reachable by scrolling, so the
 * folded "(N)" stays the number of rows the band holds.
 *
 * AN UNLAID PANE KEEPS ITS DRAWN STATE. A pane that is not shown (the other
 * grouping) or a rail not yet on the page cannot be fitted; its bands are left
 * as drawn, every line shown, and fitted the moment they lay out (the rail
 * observes its scroller and its panes).
 */
import { log } from "../log.js";

/** The classes the two bands' sections are drawn with. */
export const HIDDEN_SECTION_CLASS = "hidden-section";
export const MERGED_SECTION_CLASS = "merged-section";

/** The attribute a fitted band states how many lines its viewport shows on. */
export const BAND_LINES_ATTRIBUTE = "data-band-lines";

/** The most lines either band's viewport shows once the rail is out of room. */
export const BAND_CAP = 6;

/** The fewest lines Recently Merged gives up its capped viewport down to, for Hidden Workspaces. */
export const MERGED_FLOOR = 5;

/** How many lines each band's viewport shows. */
export interface BandLines {
  hidden: number;
  merged: number;
}

/**
 * A band's STOPS: `stops[k]` is the height, in pixels, of its rows region
 * showing its first K lines. `stops[0]` is 0, and the last is the whole
 * region, so a band of N lines has N + 1 stops.
 */
export type BandStops = readonly number[];

/** Refuse stops that do not describe a band (see `BandStops`). */
function assertStops(stops: BandStops, band: string): void {
  if (stops.length === 0 || stops[0] !== 0 || stops.some((h, i) => i > 0 && h < (stops[i - 1] as number))) {
    throw new RangeError(`band fit: ${band} stops ${JSON.stringify(stops)} do not start at 0 and grow`);
  }
}

/**
 * Split ROOM pixels between the two bands, as the module note says. Returns
 * the number of lines each band's viewport shows.
 */
export function allocateBandLines(room: number, hidden: BandStops, merged: BandStops): BandLines {
  assertStops(hidden, "hidden");
  assertStops(merged, "merged");
  const totalH = hidden.length - 1;
  const totalM = merged.length - 1;
  const height = (h: number, m: number): number => (hidden[h] as number) + (merged[m] as number);
  if (height(totalH, totalM) <= room) return { hidden: totalH, merged: totalM };
  let h = Math.min(totalH, BAND_CAP);
  let m = Math.min(totalM, BAND_CAP);
  if (height(h, m) <= room) {
    for (let grew = true; grew; ) {
      grew = false;
      if (h < totalH && height(h + 1, m) <= room) {
        h += 1;
        grew = true;
      }
      if (m < totalM && height(h, m + 1) <= room) {
        m += 1;
        grew = true;
      }
    }
  } else if (m === BAND_CAP && height(h, MERGED_FLOOR) <= room) {
    m = MERGED_FLOOR;
  }
  return { hidden: h, merged: m };
}

/** A box's declared vertical padding, in pixels; an undeclared one is zero. */
function verticalPadding(el: HTMLElement): number {
  const style = getComputedStyle(el);
  const px = (value: string): number => (value === "" ? 0 : Number.parseFloat(value));
  return px(style.paddingTop) + px(style.paddingBottom);
}

/**
 * The stops of a laid-out rows REGION: one per drawn `.row` line, at that
 * line's BOTTOM, so whatever is drawn above a line (a Recently Merged
 * repository group's header) is shown together with it, never dangling at the
 * viewport's foot. Measured against the region's scrolled content, so a
 * scrolled or capped region measures the same as an uncapped one. A line that
 * draws no box (a collapsed repository's group) is no line.
 */
export function regionStops(region: HTMLElement): number[] {
  const origin = region.getBoundingClientRect().top + region.clientTop - region.scrollTop;
  const bottoms = [...region.querySelectorAll<HTMLElement>(".row")]
    .map((line) => line.getBoundingClientRect())
    .filter((rect) => rect.height > 0)
    .map((rect) => rect.bottom - origin);
  if (bottoms.length === 0) return [0];
  return [0, ...bottoms.slice(0, -1), region.scrollHeight];
}

/** A band the fit sizes: its section and its rows region. */
interface Band {
  section: HTMLElement;
  region: HTMLElement;
}

/** PANE's band of CLASS, or null when it is absent, not drawn, or folded. */
function openBand(pane: HTMLElement, className: string): Band | null {
  const section = pane.querySelector<HTMLElement>(`:scope > .${className}`);
  if (section === null || section.hidden || section.classList.contains("folded")) return null;
  const region = section.querySelector<HTMLElement>(":scope > .rows");
  if (region === null) {
    log.error("a sidebar band cannot be fitted: it is drawn without its rows region", {
      operation: "sidebar.band-fit.malformed",
      context: { band: className },
    });
    throw new Error(`band fit: the ${className} band is missing its rows region`);
  }
  return { section, region };
}

/** Size BAND's viewport to LINES of its STOPS, and state LINES on it. */
function sizeBand(band: Band, stops: BandStops, lines: number): void {
  band.region.style.maxHeight = lines < stops.length - 1 ? `${stops[lines]}px` : "";
  band.section.setAttribute(BAND_LINES_ATTRIBUTE, String(lines));
}

/**
 * Fit the bands in PANE, which hangs in the rail's SCROLLER. Returns the lines
 * each shows, or null when neither is open or the pane is not laid out (left
 * as drawn; see the module note).
 */
export function fitBands(scroller: HTMLElement, pane: HTMLElement): BandLines | null {
  const hidden = openBand(pane, HIDDEN_SECTION_CLASS);
  const merged = openBand(pane, MERGED_SECTION_CLASS);
  if (hidden === null && merged === null) return null;
  const paneHeight = pane.offsetHeight;
  if (paneHeight <= 0 || scroller.clientHeight <= 0) {
    log.debug("left the sidebar bands as drawn: the pane is not laid out", {
      operation: "sidebar.band-fit.unlaid",
      context: { pane_height: paneHeight, scroller_height: scroller.clientHeight },
    });
    return null;
  }
  const bands = [hidden, merged].filter((band): band is Band => band !== null);
  const others = paneHeight - bands.reduce((sum, band) => sum + band.region.offsetHeight, 0);
  const room = scroller.clientHeight - verticalPadding(scroller) - others;
  const hiddenStops = hidden === null ? [0] : regionStops(hidden.region);
  const mergedStops = merged === null ? [0] : regionStops(merged.region);
  const lines = allocateBandLines(room, hiddenStops, mergedStops);
  if (hidden !== null) sizeBand(hidden, hiddenStops, lines.hidden);
  if (merged !== null) sizeBand(merged, mergedStops, lines.merged);
  log.debug("fitted the sidebar bands", {
    operation: "sidebar.band-fit",
    context: {
      room,
      hidden_total: hiddenStops.length - 1,
      hidden_shown: lines.hidden,
      merged_total: mergedStops.length - 1,
      merged_shown: lines.merged,
    },
  });
  return lines;
}

/** Fit the bands of every pane in SCROLLER (the shown one lays out; the other is left). */
export function fitAllBands(scroller: HTMLElement): void {
  for (const pane of scroller.querySelectorAll<HTMLElement>(".sb-pane")) fitBands(scroller, pane);
}

/** The rail's re-fit: on every draw, and whenever the scroller or a pane changes size. */
export interface BandFit {
  /** Fit after a draw, and watch the freshly drawn panes. */
  refit(): void;
  dispose(): void;
}

/**
 * Keep SCROLLER's bands fitted: a resize of the rail (the window, the editor
 * pane) or of a pane (a section folded or unfolded, the grouping switched)
 * re-fits, because the split is a function of exactly that layout. A fit that
 * settles resizes the pane to the size it fitted for, so the re-fit it
 * provokes changes nothing.
 */
export function installBandFit(scroller: HTMLElement): BandFit {
  const observer = new ResizeObserver(() => fitAllBands(scroller));
  observer.observe(scroller);
  let panes: HTMLElement[] = [];
  return {
    refit(): void {
      for (const pane of panes) observer.unobserve(pane);
      panes = [...scroller.querySelectorAll<HTMLElement>(".sb-pane")];
      for (const pane of panes) observer.observe(pane);
      fitAllBands(scroller);
    },
    dispose(): void {
      observer.disconnect();
      panes = [];
    },
  };
}

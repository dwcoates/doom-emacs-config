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
 * THE FIT NEVER MOVES ITS OWN ROOM. The room is measured from layout the
 * fit does not size (the rail's height under its head, the pane's content
 * other than the bands' rows regions), in the engine's fractional pixels
 * rather than rounded whole ones, so fitting again finds the same room and
 * draws the same lines. A content-sized rail scroller once broke this: each
 * fit moved the room by a pixel and the observer re-fitted between two
 * answers many times a second (2026-10-11). A fit that moves its room is an
 * error and refuses.
 *
 * AN UNLAID PANE KEEPS ITS DRAWN STATE. A rail not yet on the page cannot be
 * fitted; its bands are left
 * as drawn, every line shown, and fitted the moment they lay out (the rail
 * observes its scroller and its panes).
 */
import { computedPx } from "../dom.js";
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
  if (stops.length === 0 || stops[0] !== 0 || stops.some((h, i) => i > 0 && h < stops[i - 1])) {
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
  const height = (h: number, m: number): number => hidden[h] + merged[m];
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

/** A computed length, which a laid-out element always reports in px. */
function computedLength(style: CSSStyleDeclaration, property: FrameLength): number {
  const value = style[property];
  const px = computedPx(value);
  if (px === null) {
    log.error("a sidebar band cannot be fitted: the rail's computed box is not in px", {
      operation: "sidebar.band-fit.unmeasurable",
      context: { property, value },
    });
    throw new RangeError(`band fit: computed ${property} '${value}' is not in px`);
  }
  return px;
}

/** The computed lengths that frame a box vertically. */
type FrameLength = "borderTopWidth" | "borderBottomWidth" | "paddingTop" | "paddingBottom";

/**
 * A box's content height, in fractional pixels: its drawn height less its
 * vertical borders and padding.
 */
function contentHeight(el: HTMLElement): number {
  const style = getComputedStyle(el);
  const frame = (["borderTopWidth", "borderBottomWidth", "paddingTop", "paddingBottom"] as const).reduce(
    (sum, property) => sum + computedLength(style, property),
    0,
  );
  return el.getBoundingClientRect().height - frame;
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

/**
 * The pixels SCROLLER's PANE leaves its open BANDS' rows regions: the
 * scroller's content height less everything else the pane draws. Measured in
 * fractional pixels, so no rounding moves it between two fits.
 */
function measureRoom(scroller: HTMLElement, pane: HTMLElement, bands: readonly Band[]): number {
  const regions = bands.reduce((sum, band) => sum + band.region.getBoundingClientRect().height, 0);
  return contentHeight(scroller) - (pane.getBoundingClientRect().height - regions);
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
  const paneHeight = pane.getBoundingClientRect().height;
  const scrollerHeight = scroller.getBoundingClientRect().height;
  if (paneHeight <= 0 || scrollerHeight <= 0) {
    log.debug("left the sidebar bands as drawn: the pane is not laid out", {
      operation: "sidebar.band-fit.unlaid",
      context: { pane_height: paneHeight, scroller_height: scrollerHeight },
    });
    return null;
  }
  const bands = [hidden, merged].filter((band): band is Band => band !== null);
  const room = measureRoom(scroller, pane, bands);
  const hiddenStops = hidden === null ? [0] : regionStops(hidden.region);
  const mergedStops = merged === null ? [0] : regionStops(merged.region);
  const lines = allocateBandLines(room, hiddenStops, mergedStops);
  if (hidden !== null) sizeBand(hidden, hiddenStops, lines.hidden);
  if (merged !== null) sizeBand(merged, mergedStops, lines.merged);
  const fittedRoom = measureRoom(scroller, pane, bands);
  if (fittedRoom !== room) {
    log.error("the sidebar band fit moved its own room: fitting again would draw other lines", {
      operation: "sidebar.band-fit.unsettled",
      context: {
        room,
        fitted_room: fittedRoom,
        hidden_total: hiddenStops.length - 1,
        hidden_shown: lines.hidden,
        merged_total: mergedStops.length - 1,
        merged_shown: lines.merged,
      },
    });
    throw new Error(`band fit: fitting moved the room from ${room}px to ${fittedRoom}px`);
  }
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
 * pane) or of a pane (a section folded or unfolded)
 * re-fits, because the split is a function of exactly that layout. The fit
 * never moves its own room (see the module note), so the re-fit its own
 * resize of the pane provokes draws the same lines and stops there.
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

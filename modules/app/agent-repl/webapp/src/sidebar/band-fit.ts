/**
 * band-fit — how many lines of the Open, Hidden Workspaces and Recently Merged
 * sections the rail shows, each section scrolling through the rest.
 *
 * THE RAIL NEVER SCROLLS; ITS SECTIONS DO (owner request, 2026-10-18). The
 * daemon sends every row of every section (Recently Merged holds a week of
 * merges). Only this page knows its own height, so the split is decided here:
 *
 *   - When every line of every section fits, every line shows and nothing
 *     scrolls.
 *   - Otherwise the Open section dominates: it lengthens until the two bands
 *     below it are left `BAND_FLOOR` lines each, and scrolls past that.
 *   - The bands SHARE those `2 * BAND_FLOOR` lines (six, owner request,
 *     2026-10-19): a band that needs fewer than `BAND_FLOOR` gives the rest
 *     to the other, so a Recently Merged of one merge today leaves Hidden
 *     Workspaces five.
 *   - Recently Merged claims its share only for the lines that reach TODAY's
 *     merges. An older merge has no claim against the Open section: it shows
 *     only in room the Open section cannot use.
 *   - NO ROOM IS LEFT UNUSED (owner request, 2026-10-19). Room the Open
 *     section leaves, whole or stopped short by a line taller than the room
 *     (a repository's header riding with its first row), fills Hidden
 *     Workspaces as far as its lines fit, then Recently Merged.
 *   - A rail too short for the bands' share shrinks the bands, the longer one
 *     first, Recently Merged on a tie, and shows the Open section no lines.
 *   - THE LOWEST OPEN SECTION REACHES THE RAIL'S FOOT (owner request,
 *     2026-10-19). Whatever room is left once every section shows its whole
 *     lines, less than one more line of any section, is added to the
 *     viewport of the lowest section not folded (Recently Merged, unless it
 *     is folded), so no strip of the rail is ever left under it. That
 *     viewport may cut its last line through, scrolling it into view; the
 *     section's rows region stands that tall even when it holds fewer rows.
 *   - A LINE CUT AT THE FOOT FADES OUT (owner request, 2026-10-19). While the
 *     stretched viewport has more below its edge, the section is marked
 *     `BAND_MORE_ATTRIBUTE` and the stylesheet fades the viewport's last
 *     strip; scrolled to its end, the mark drops and the last row shows whole.
 *
 * A LINE IS ONE `.row`, a nested family member included, a folded
 * repository's header, or the header of a subsection that is folded or empty
 * (a subsection with no rows to show draws its header alone, so it is a line
 * as a folded repository's header is). While the repository sections show
 * their merge queues, a queue's head card and each waiting merge is a line,
 * and so is the header of a repository whose queue holds nothing, which draws
 * its header alone. A viewport of K lines is exactly as tall as the
 * first K lines are drawn, the header above a line (a repository section's,
 * a subsection's, a Recently Merged repository group's) riding with it, so no
 * line is cut through but the lowest section's last. A folded section shows no
 * lines and takes no room.
 *
 * THE COUNT IS THE DAEMON'S. Every row is reachable by scrolling, so the
 * folded "(N)" stays the number of rows the section holds.
 *
 * TODAY IS THE LOCAL DATE, as the band's "(K today)" counts it
 * (`mergedToday`). The rail re-fits when the date turns over, so a merge stops
 * claiming room at midnight on the same tick its row stops reading as today's.
 *
 * THE FIT NEVER MOVES ITS OWN ROOM. The room is measured from layout the
 * fit does not size (the rail's height under its head, the pane's content
 * other than the sections' rows regions), in the engine's fractional pixels
 * rather than rounded whole ones, so fitting again finds the same room and
 * draws the same lines. A content-sized rail scroller once broke this: each
 * fit moved the room by a pixel and the observer re-fitted between two
 * answers many times a second (2026-10-11). A fit that moves its room is an
 * error and refuses.
 *
 * AN UNLAID PANE KEEPS ITS DRAWN STATE. A rail not yet on the page cannot be
 * fitted; its sections are left as drawn, every line shown, and fitted the
 * moment they lay out (the rail observes its scroller, its panes and the
 * sections inside its rows regions).
 */
import { sameLocalDate, type Ticker } from "../clock.js";
import { computedPx, queryAll } from "../dom.js";
import { log } from "../log.js";
import { MERGE_QUEUE_EMPTY_CLASS, MERGE_QUEUE_HEAD_CLASS, MERGE_QUEUE_WAITING_CLASS } from "./merge-queue-view.js";
import { mergedToday } from "./row.js";

/** The classes the three sections are drawn with. */
export const OPEN_SECTION_CLASS = "open-section";
export const HIDDEN_SECTION_CLASS = "hidden-section";
export const MERGED_SECTION_CLASS = "merged-section";

/** The class of the Open section's rows region: the box its repository sections are placed in. */
export const OPEN_REGION_CLASS = "sb-sections";

/** The attribute a fitted section states how many lines its viewport shows on. */
export const BAND_LINES_ATTRIBUTE = "data-band-lines";

/** The attribute marking the section whose viewport reaches the rail's foot. */
export const BAND_STRETCHED_ATTRIBUTE = "data-band-stretched";

/** The attribute marking a section whose viewport has rows below its edge. */
export const BAND_MORE_ATTRIBUTE = "data-band-more";

/** The attribute a merged row states its merged instant (epoch ms) on. */
export const MERGED_AT_ATTRIBUTE = "data-merged-at";

/** The lines each band keeps once the Open section has filled the rail. */
export const BAND_FLOOR = 3;

/** Each section's class and its rows region's, top to bottom. */
const SECTION_REGIONS = [
  [OPEN_SECTION_CLASS, OPEN_REGION_CLASS],
  [HIDDEN_SECTION_CLASS, "rows"],
  [MERGED_SECTION_CLASS, "rows"],
] as const;

/**
 * The attribute an EMPTY subsection (`subsections.ts`) wears: it draws its
 * header alone, so its header is a line as a folded subsection's is.
 */
export const EMPTY_SUBSECTION_ATTRIBUTE = "data-empty";


/**
 * What one line is: a row, a folded repository section's header, or the
 * header of a subsection that is folded or empty.
 */
const LINE_SELECTOR = [
  ".row",
  ".repo.folded > .repo-head",
  ".sb-subsection.folded > .sb-subsection-head",
  `.sb-subsection[${EMPTY_SUBSECTION_ATTRIBUTE}] > .sb-subsection-head`,
  `.${MERGE_QUEUE_HEAD_CLASS}`,
  `.${MERGE_QUEUE_WAITING_CLASS}`,
  `.repo.${MERGE_QUEUE_EMPTY_CLASS} > .repo-head`,
].join(", ");

/** How many lines each section's viewport shows. */
export interface SectionLines {
  open: number;
  hidden: number;
  merged: number;
}

/**
 * A section's STOPS: `stops[k]` is the height, in pixels, of its rows region
 * showing its first K lines. `stops[0]` is 0, and the last is the whole
 * region, so a section of N lines has N + 1 stops.
 */
export type BandStops = readonly number[];

/** What the fit is handed of the three sections. */
export interface SectionStops {
  open: BandStops;
  hidden: BandStops;
  merged: BandStops;
  /** How many of Recently Merged's first lines it takes to show every merge of today. */
  mergedToday: number;
}

/** Refuse stops that do not describe a section (see `BandStops`). */
function assertStops(stops: BandStops, section: string): void {
  if (stops.length === 0 || stops[0] !== 0 || stops.some((h, i) => i > 0 && h < stops[i - 1])) {
    throw new RangeError(`band fit: ${section} stops ${JSON.stringify(stops)} do not start at 0 and grow`);
  }
}

/**
 * The bands' share of `2 * BAND_FLOOR` lines: `BAND_FLOOR` each, a band that
 * needs fewer giving the rest to the other. HIDDEN and TODAY are the lines
 * each band claims.
 */
export function bandShare(hidden: number, today: number): { hidden: number; merged: number } {
  const h = Math.min(hidden, BAND_FLOOR);
  const m = Math.min(today, BAND_FLOOR);
  const spare = 2 * BAND_FLOOR - h - m;
  const hGrown = h + Math.min(hidden - h, spare);
  return { hidden: hGrown, merged: m + Math.min(today - m, spare - (hGrown - h)) };
}

/**
 * Split ROOM pixels between the three sections, as the module note says.
 * Returns the number of lines each section's viewport shows.
 */
export function allocateSectionLines(room: number, stops: SectionStops): SectionLines {
  const { open, hidden, merged, mergedToday: today } = stops;
  assertStops(open, "open");
  assertStops(hidden, "hidden");
  assertStops(merged, "merged");
  const totalO = open.length - 1;
  const totalH = hidden.length - 1;
  const totalM = merged.length - 1;
  if (!Number.isInteger(today) || today < 0 || today > totalM) {
    throw new RangeError(`band fit: ${today} lines of today's merges in a band of ${totalM}`);
  }
  const fits = (o: number, h: number, m: number): boolean => open[o] + hidden[h] + merged[m] <= room;
  if (fits(totalO, totalH, totalM)) return { open: totalO, hidden: totalH, merged: totalM };
  let { hidden: h, merged: m } = bandShare(totalH, today);
  while (!fits(0, h, m) && h + m > 0) {
    if (m >= h) m -= 1;
    else h -= 1;
  }
  let o = 0;
  while (o < totalO && fits(o + 1, h, m)) o += 1;
  while (h < totalH && fits(o, h + 1, m)) h += 1;
  while (m < totalM && fits(o, h, m + 1)) m += 1;
  return { open: o, hidden: h, merged: m };
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
 * The drawn LINES of a laid-out rows REGION, in order: every `.row` and every
 * folded repository section's header that draws a box. A line that draws no
 * box (a collapsed repository's group, a row the search narrowed away) is no
 * line.
 */
export function regionLines(region: HTMLElement): HTMLElement[] {
  return [...queryAll<HTMLElement>(region, LINE_SELECTOR)].filter((line) => line.getBoundingClientRect().height > 0);
}

/**
 * The stops of a laid-out rows REGION drawing LINES: one per line, at that
 * line's BOTTOM, so whatever is drawn above a line (a repository section's
 * header, a Recently Merged repository group's) is shown together with it,
 * never dangling at the viewport's foot. Measured against the region's
 * scrolled content, so a scrolled or capped region measures the same as an
 * uncapped one.
 */
export function regionStops(region: HTMLElement, lines: readonly HTMLElement[] = regionLines(region)): number[] {
  const origin = region.getBoundingClientRect().top + region.clientTop - region.scrollTop;
  const bottoms = lines.map((line) => line.getBoundingClientRect().bottom - origin);
  if (bottoms.length === 0) return [0];
  return [0, ...bottoms.slice(0, -1), region.scrollHeight];
}

/**
 * How many of Recently Merged's first LINES it takes to show every merge that
 * landed on NOW_MS's local date: through the last line whose row carries a
 * merged instant of today. A line outside any stamped row is malformed.
 */
export function mergedTodayLines(lines: readonly HTMLElement[], nowMs: number): number {
  let through = 0;
  lines.forEach((line, i) => {
    const stamped = line.closest<HTMLElement>(`[${MERGED_AT_ATTRIBUTE}]`);
    const atMs = stamped === null ? Number.NaN : Number(stamped.getAttribute(MERGED_AT_ATTRIBUTE));
    if (!Number.isFinite(atMs)) {
      log.error("a sidebar band cannot be fitted: a Recently Merged line carries no merged instant", {
        operation: "sidebar.band-fit.unstamped",
        context: { line: i, merged_at: stamped?.getAttribute(MERGED_AT_ATTRIBUTE) ?? null },
      });
      throw new Error(`band fit: Recently Merged line ${i} carries no merged instant`);
    }
    if (mergedToday(atMs, nowMs)) through = i + 1;
  });
  return through;
}

/** A section the fit sizes: its box and its rows region. */
interface Band {
  section: HTMLElement;
  region: HTMLElement;
}

/** PANE's section of CLASS, or null when it is absent, not drawn, or folded. */
function openBand(pane: HTMLElement, className: string, regionClass: string): Band | null {
  const section = pane.querySelector<HTMLElement>(`:scope > .${className}`);
  if (section === null || section.hidden || section.classList.contains("folded")) return null;
  const region = section.querySelector<HTMLElement>(`:scope > .${regionClass}`);
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
 * The pixels SCROLLER's PANE leaves its open sections' rows REGIONS: the
 * scroller's content height less everything else the pane draws. Measured in
 * fractional pixels, so no rounding moves it between two fits.
 */
function measureRoom(scroller: HTMLElement, pane: HTMLElement, bands: readonly Band[]): number {
  const regions = bands.reduce((sum, band) => sum + band.region.getBoundingClientRect().height, 0);
  return contentHeight(scroller) - (pane.getBoundingClientRect().height - regions);
}

/**
 * Size BAND's viewport to LINES of its STOPS, and state LINES on it. A band
 * handed a STRETCH (the lowest; see the module note) stands exactly that
 * much taller than its LINES, cutting through the next line; any other is
 * capped at its LINES.
 */
function sizeBand(band: Band, stops: BandStops, lines: number, stretch: number | null): void {
  if (stretch === null) {
    band.region.style.height = "";
    band.region.style.maxHeight = lines < stops.length - 1 ? `${stops[lines]}px` : "";
  } else {
    band.region.style.maxHeight = "";
    band.region.style.height = `${stops[lines] + stretch}px`;
  }
  band.section.setAttribute(BAND_LINES_ATTRIBUTE, String(lines));
  band.section.toggleAttribute(BAND_STRETCHED_ATTRIBUTE, stretch !== null);
  paintBandMore(band.region);
}

/**
 * Mark REGION's section `BAND_MORE_ATTRIBUTE` while rows lie below its
 * viewport's edge (half a pixel of rounding is no row), and clear it once
 * the viewport reaches its end.
 */
export function paintBandMore(region: HTMLElement): void {
  const section = region.parentElement;
  if (section === null) {
    log.error("a sidebar band's fade cannot be painted: its rows region hangs in no section", {
      operation: "sidebar.band-fit.orphan-region",
      context: { region: region.className },
    });
    throw new Error("band fit: a rows region hangs in no section");
  }
  section.toggleAttribute(BAND_MORE_ATTRIBUTE, region.scrollTop + region.clientHeight < region.scrollHeight - 0.5);
}

/** The selector every section's rows region matches. */
export const REGION_SELECTOR = SECTION_REGIONS.map(([className, regionClass]) => `.${className} > .${regionClass}`).join(", ");

/** One open section's band, its lines, and its stops. */
interface Measured {
  band: Band;
  lines: HTMLElement[];
  stops: number[];
}

/** Measure BAND, or nothing for a folded one. */
function measure(band: Band | null): Measured | null {
  if (band === null) return null;
  const lines = regionLines(band.region);
  return { band, lines, stops: regionStops(band.region, lines) };
}

/**
 * Fit the sections in PANE, which hangs in the rail's SCROLLER, NOW_MS
 * deciding which merges are today's. Returns the lines each shows, or null
 * when none is open or the pane is not laid out (left as drawn; see the
 * module note).
 */
export function fitBands(scroller: HTMLElement, pane: HTMLElement, nowMs: number): SectionLines | null {
  const [openSection, hiddenSection, mergedSection] = SECTION_REGIONS.map(([className, regionClass]) =>
    openBand(pane, className, regionClass),
  );
  if (openSection === null && hiddenSection === null && mergedSection === null) return null;
  const paneHeight = pane.getBoundingClientRect().height;
  const scrollerHeight = scroller.getBoundingClientRect().height;
  if (paneHeight <= 0 || scrollerHeight <= 0) {
    log.debug("left the sidebar bands as drawn: the pane is not laid out", {
      operation: "sidebar.band-fit.unlaid",
      context: { pane_height: paneHeight, scroller_height: scrollerHeight },
    });
    return null;
  }
  const bands = [openSection, hiddenSection, mergedSection].filter((band): band is Band => band !== null);
  // A stretched viewport stands taller than its rows, so it is let go before
  // the rows are measured: the stops are the rows' own, never the stretch's.
  for (const band of bands) band.region.style.height = "";
  const room = measureRoom(scroller, pane, bands);
  const open = measure(openSection);
  const hidden = measure(hiddenSection);
  const merged = measure(mergedSection);
  const today = merged === null ? 0 : mergedTodayLines(merged.lines, nowMs);
  const lines = allocateSectionLines(room, {
    open: open?.stops ?? [0],
    hidden: hidden?.stops ?? [0],
    merged: merged?.stops ?? [0],
    mergedToday: today,
  });
  const sized = [
    [open, lines.open],
    [hidden, lines.hidden],
    [merged, lines.merged],
  ].filter((entry): entry is [Measured, number] => entry[0] !== null);
  const used = sized.reduce((sum, [m, n]) => sum + m.stops[n], 0);
  // A rail shorter than the sections' headers leaves no room to stretch into.
  const stretch = Math.max(0, room - used);
  sized.forEach(([m, n], i) => sizeBand(m.band, m.stops, n, i === sized.length - 1 ? stretch : null));
  const context = {
    room,
    open_total: (open?.stops.length ?? 1) - 1,
    open_shown: lines.open,
    hidden_total: (hidden?.stops.length ?? 1) - 1,
    hidden_shown: lines.hidden,
    merged_total: (merged?.stops.length ?? 1) - 1,
    merged_today: today,
    merged_shown: lines.merged,
    stretch,
  };
  const fittedRoom = measureRoom(scroller, pane, bands);
  if (fittedRoom !== room) {
    log.error("the sidebar band fit moved its own room: fitting again would draw other lines", {
      operation: "sidebar.band-fit.unsettled",
      context: { ...context, fitted_room: fittedRoom },
    });
    throw new Error(`band fit: fitting moved the room from ${room}px to ${fittedRoom}px`);
  }
  log.debug("fitted the sidebar bands", { operation: "sidebar.band-fit", context });
  return lines;
}

/** Fit the sections of every pane in SCROLLER at NOW_MS (the shown one lays out; the other is left). */
export function fitAllBands(scroller: HTMLElement, nowMs: number): void {
  for (const pane of queryAll<HTMLElement>(scroller, ".sb-pane")) fitBands(scroller, pane, nowMs);
}

/** The rail's re-fit: on every draw, whenever the scroller or a pane changes size, and at midnight. */
export interface BandFit {
  /** Fit after a draw, and watch the freshly drawn panes. */
  refit(): void;
  dispose(): void;
}

/**
 * The boxes whose size the fit is a function of inside PANE: the pane, and
 * every child of each section's rows region, whose size a capped region
 * hides from the pane (a repository folded inside the Open section, a
 * Recently Merged group collapsed).
 */
function watchedBoxes(pane: HTMLElement): HTMLElement[] {
  const regions = SECTION_REGIONS.map(([className, regionClass]) =>
    pane.querySelector<HTMLElement>(`:scope > .${className} > .${regionClass}`),
  ).filter((region): region is HTMLElement => region !== null);
  return [pane, ...regions.flatMap((region) => [...region.children] as HTMLElement[])];
}

/**
 * Keep SCROLLER's sections fitted: a resize of the rail (the window, the
 * editor pane), of a pane, or of a box inside a rows region (a section or a
 * repository folded or unfolded) re-fits, because the split is a function of
 * exactly that layout, and so does TICKER's first tick of a new local date,
 * because which merges are today's is part of it. The fit never moves its
 * own room (see the module note), so the re-fit its own resize of the pane
 * provokes draws the same lines and stops there.
 */
export function installBandFit(scroller: HTMLElement, ticker: Ticker): BandFit {
  let fittedAtMs = ticker.now();
  const fitAt = (nowMs: number): void => {
    fittedAtMs = nowMs;
    fitAllBands(scroller, nowMs);
  };
  const fitNow = (): void => fitAt(ticker.now());
  const observer = new ResizeObserver(fitNow);
  observer.observe(scroller);
  // A scroll does not bubble, so the rail captures every section's scroll to
  // repaint whether rows still lie below its edge.
  const onScroll = (event: Event): void => {
    const target = event.target;
    if (target instanceof HTMLElement && target.matches(REGION_SELECTOR)) paintBandMore(target);
  };
  scroller.addEventListener("scroll", onScroll, true);
  let watched: HTMLElement[] = [];
  const unsubscribe = ticker.subscribe((nowMs) => {
    if (!sameLocalDate(fittedAtMs, nowMs)) fitAt(nowMs);
  });
  return {
    refit(): void {
      for (const box of watched) observer.unobserve(box);
      watched = queryAll<HTMLElement>(scroller, ".sb-pane").flatMap(watchedBoxes);
      for (const box of watched) observer.observe(box);
      fitNow();
    },
    dispose(): void {
      observer.disconnect();
      scroller.removeEventListener("scroll", onScroll, true);
      unsubscribe();
      watched = [];
    },
  };
}

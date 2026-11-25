/**
 * section-resize — THE RULES BETWEEN THE RAIL'S SECTIONS ARE DRAGGED (owner
 * request, 2026-11-11): pressing the grey rule above an open band section
 * (Merge Queue, Recently Merged, Backlog) and dragging it moves the
 * room between that section and the open section above it. Dragging the rule
 * between the Open section and Merge Queue upward makes the Open section
 * shorter and the Merge Queue section taller.
 *
 * A HEIGHT IS COUNTED IN LINES (band-fit.ts): the drag snaps to the stops the
 * last fit measured, so no line is cut through, and a height means the same
 * on every page whatever its pixels.
 *
 * THE HEIGHT IS THE DAEMON'S, like a fold (`view.ts`): the drag paints and
 * holds it on this page as the pointer moves, and on release asks the daemon
 * for every section it sized in ONE UpdateSidebarView (`changeViews`), whose
 * roster push draws it on every page. The Open section is never sized: it
 * fills what the bands leave, so a drag against it sizes the band below
 * alone, and a drag between two bands sizes both. EITHER WAY THE DRAG MOVES
 * ITS OWN RULE ALONE (owner report, 2026-11-21): the band below is given
 * exactly the room the section above it gives up, at the section above's
 * stops, so a drag against an Open section that shows every line cannot hand
 * the band's room on to the band below it. THE ROOM BETWEEN THE TWO IS
 * CONSERVED (owner report, 2026-11-21, a Merge Queue of nine lines dragged to
 * none against a Recently Merged of one): the section above gives up only the
 * room the section below can take, and the reverse. A drag sizes only the
 * sections it changed: one it could not change keeps the height it had,
 * fitted or sized, so a drag never pins a section the owner did not move.
 * Escape, or a pointer lost
 * mid-drag, puts back what the wire says. A double click on a rule fits the
 * sections beside it by their share again.
 */
import { FOLDED_CLASS } from "./folded-class.js";
import type { MessageInitShape } from "@bufbuild/protobuf";
import type { SidebarViewSectionSizeSchema, UpdateSidebarViewRequestSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { DRAGGING_CLASS } from "../drag.js";
import { createLogger } from "../log.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { BAND_SIZED_ATTRIBUTE, bandSize, fittedSection, type FittedSection } from "./band-fit.js";
import { BACKLOG_SECTION_CLASS, MERGED_SECTION_CLASS, MERGE_QUEUE_SECTION_CLASS, OPEN_SECTION_CLASS } from "./section-classes.js";
import { BACKLOG_FOLD_KEY, MERGED_FOLD_KEY, MERGE_QUEUE_FOLD_KEY, type SidebarContext } from "./context.js";
import { changeViews, type ViewPiece } from "./view-change.js";
import { heightViewKey } from "./view.js";
import { decorativeElement } from "../dom.js";

const log = createLogger("webapp.sidebar.section-resize");

/** The class of the grip drawn over a band section's rule. */
export const RESIZE_HANDLE_CLASS = "sb-band-resize";

/** The class the page's root wears while a rule is dragged, so the cursor stays the resize one. */
export const RESIZING_CLASS = "sb-resizing";

/**
 * A band section's two-arm height oneof, as each of the three spells it: the
 * arm IS the height.
 */
export type RosterSectionHeight =
  | { case: "fitted"; value: unknown }
  | { case: "sized"; value: { lines: number } }
  | { case: undefined; value?: undefined };

/** The lines a band section is sized to, or null while it is fitted. EVERY arm is named; unset is malformed. */
export function drawRosterSectionHeight(height: RosterSectionHeight, path: string): number | null {
  const arm = requireCase(height, path);
  switch (arm.case) {
    case "fitted":
      return null;
    case "sized":
      return arm.value.lines;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(path, other.case);
    }
  }
}

/** State on SECTION the lines it is sized to (`BAND_SIZED_ATTRIBUTE`), or that it is fitted. */
export function paintSectionHeight(section: HTMLElement, lines: number | null): void {
  if (lines === null) section.removeAttribute(BAND_SIZED_ATTRIBUTE);
  else section.setAttribute(BAND_SIZED_ATTRIBUTE, String(lines));
}

/**
 * Draw SECTION at the height the wire says (WIRE), unless this page's own
 * drag or ask under FOLD_KEY still holds another, and add the grip over its
 * rule.
 */
export function drawSectionHeight(section: HTMLElement, foldKey: string, wire: number | null, sc: SidebarContext): void {
  paintSectionHeight(section, sc.view.track(heightViewKey(foldKey), wire, (lines) => paintSectionHeight(section, lines)));
  section.prepend(decorativeElement("div", RESIZE_HANDLE_CLASS));
}

/** The four sections top to bottom, as the pane draws them. */
const SECTION_ORDER = [OPEN_SECTION_CLASS, MERGE_QUEUE_SECTION_CLASS, MERGED_SECTION_CLASS, BACKLOG_SECTION_CLASS] as const;

/** Whether SECTION is drawn open in its pane. */
const isOpen = (section: Element): boolean => !(section as HTMLElement).hidden && !section.classList.contains(FOLDED_CLASS);

/** One rule's drag: the open section above it and the band below it. */
export interface ResizePair {
  /** The section above the rule; the Open section is never sized. */
  upper: HTMLElement;
  /** The band section whose rule it is. */
  lower: HTMLElement;
}

/**
 * The sections the rule over LOWER moves room between: LOWER, and the
 * nearest section above it drawn open (a folded one's header sits between
 * them unmoved). Null when LOWER is folded or nothing above it is open.
 */
export function resizePair(lower: HTMLElement): ResizePair | null {
  if (!isOpen(lower)) return null;
  const pane = lower.parentElement;
  if (pane === null) return null;
  const sections = SECTION_ORDER.map((cls) => pane.querySelector<HTMLElement>(`:scope > .${cls}`));
  const at = sections.indexOf(lower);
  const upper = sections.slice(0, Math.max(at, 0)).reverse().find((section) => section !== null && isOpen(section)) ?? null;
  return upper === null ? null : { upper, lower };
}

/** The index of the stop in STOPS nearest PX. */
export function nearestStop(stops: readonly number[], px: number): number {
  let best = 0;
  stops.forEach((stop, i) => {
    if (Math.abs(stop - px) < Math.abs(stops[best] - px)) best = i;
  });
  return best;
}

/**
 * The lines a drag DY px down (up when negative) gives the sections beside
 * a rule, from what the last fit measured of them. The two stood in
 * COMBINED px together, and the rule moves that room between them, none of it
 * out: the upper's new height snaps to its nearest stop, but never shorter than
 * the last stop that still leaves the lower room to take every line it holds
 * (past it the lower could take no more, and the room would go to the Open
 * section or the band below), and never taller than the two stood in
 * together (past it the upper would take the band below the lower's room). The lower
 * keeps the lines that fit in what the upper leaves. The Open section as
 * UPPER is measured the same, though only the band below it is sized.
 */
export function draggedLines(upper: FittedSection, lower: FittedSection, dy: number): { upper: number; lower: number } {
  const lowerPx = lower.stops[lower.lines];
  const upperPx = upper.stops[upper.lines];
  const combined = upperPx + lowerPx;
  const lowerWhole = lower.stops[lower.stops.length - 1];
  let floor = 0;
  let ceiling = 0;
  upper.stops.forEach((stop, i) => {
    if (stop + lowerWhole <= combined + 0.5) floor = i;
    if (stop <= combined + 0.5) ceiling = i;
  });
  const upperLines = Math.min(ceiling, Math.max(floor, nearestStop(upper.stops, upperPx + dy)));
  const left = combined - upper.stops[upperLines];
  let lowerLines = 0;
  while (lowerLines + 1 < lower.stops.length && lower.stops[lowerLines + 1] <= left + 0.5) lowerLines += 1;
  return { upper: upperLines, lower: lowerLines };
}

/** The UpdateSidebarView section arm a band is sized under, by its fold key. */
function sizeSection(foldKey: string): MessageInitShape<typeof SidebarViewSectionSizeSchema>["section"] {
  switch (foldKey) {
    case MERGE_QUEUE_FOLD_KEY:
      return { case: "mergeQueue", value: {} };
    case MERGED_FOLD_KEY:
      return { case: "recentlyMerged", value: {} };
    case BACKLOG_FOLD_KEY:
      return { case: "backlog", value: {} };
    default:
      break;
  }
  log.error("a sidebar rule cannot be dragged: its section is no band", {
    operation: "sidebar.section-resize.not-a-band",
    context: { section: foldKey },
  });
  throw new Error(`section resize: '${foldKey}' names no band section`);
}

/** The fold key a drawn section is identified by (`data-section`). */
function foldKeyOf(section: HTMLElement): string {
  const key = section.getAttribute("data-section");
  if (key === null) {
    log.error("a sidebar rule cannot be dragged: its section names no key", {
      operation: "sidebar.section-resize.unkeyed",
      context: { section: section.className },
    });
    throw new Error("section resize: a section is drawn without its data-section key");
  }
  return key;
}

/**
 * What FIT_OF says the last fit measured of SECTION, which a drag needs: a
 * pressed rule's sections are open and laid out.
 */
export function measuredSection(section: HTMLElement, fitOf: (section: HTMLElement) => FittedSection | null): FittedSection {
  const fit = fitOf(section);
  if (fit === null) {
    log.error("a sidebar rule cannot be dragged: its section was never fitted", {
      operation: "sidebar.section-resize.unfitted",
      context: { section: section.className },
    });
    throw new Error(`section resize: the ${section.className} section was never fitted`);
  }
  return fit;
}

/** One section a rule's drag sizes, at LINES (null: fitted). */
interface SizedSection {
  section: HTMLElement;
  lines: number | null;
}

/** The view pieces, and the request's sizes, for ENTRIES. */
function sizedPieces(entries: readonly SizedSection[]) {
  const pieces: ViewPiece[] = entries.map((e) => ({ key: heightViewKey(foldKeyOf(e.section)), value: e.lines }));
  const sizes = entries.map(
    (e): MessageInitShape<typeof SidebarViewSectionSizeSchema> => ({
      section: sizeSection(foldKeyOf(e.section)),
      height: e.lines === null ? { case: "fitted", value: {} } : { case: "lines", value: { lines: e.lines } },
    }),
  );
  const change: MessageInitShape<typeof UpdateSidebarViewRequestSchema>["change"] = { case: "sizeSections", value: { sizes } };
  return { pieces, change };
}

/** The grip EVENT landed on and the sections its rule moves room between, or null when it moves none. */
function pressedRule(event: Event): { grip: HTMLElement; pair: ResizePair } | null {
  const grip = event.target instanceof HTMLElement ? event.target.closest<HTMLElement>(`.${RESIZE_HANDLE_CLASS}`) : null;
  const lower = grip?.parentElement ?? null;
  const pair = lower === null ? null : resizePair(lower);
  return grip === null || pair === null ? null : { grip, pair };
}

/**
 * Make every band rule under HOST draggable, the view asked through SC, and
 * REFIT re-fitting the rail after every height painted. FIT_OF reads what the
 * last fit measured of a section (band-fit.ts's own record unless a test
 * states it). Answers the uninstall.
 */
export function installSectionResize(
  host: HTMLElement,
  sc: SidebarContext,
  refit: () => void,
  fitOf: (section: HTMLElement) => FittedSection | null = fittedSection,
): () => void {
  const doc = host.ownerDocument;
  let dragging: (() => void) | null = null;

  const press = (event: PointerEvent): void => {
    if (event.button !== 0 || dragging !== null) return;
    const pressed = pressedRule(event);
    if (pressed === null) return;
    const { grip, pair } = pressed;
    event.preventDefault();
    const sizing = pair.upper.classList.contains(OPEN_SECTION_CLASS) ? [pair.lower] : [pair.upper, pair.lower];
    const upperFit = measuredSection(pair.upper, fitOf);
    const lowerFit = measuredSection(pair.lower, fitOf);
    const startY = event.clientY;
    const linesOf = (lines: { upper: number; lower: number }): number[] => (sizing.length === 2 ? [lines.upper, lines.lower] : [lines.lower]);
    const startLines = { upper: upperFit.lines, lower: lowerFit.lines };
    const start = linesOf(startLines);
    // What the wire says of each section, which one the drag leaves unchanged keeps.
    const startSizes = sizing.map((section) => bandSize(section));
    let shownLines = startLines;
    let shown = start;
    let pointerPx = 0;
    const held: Array<{ key: string; token: number }> = [];
    grip.setPointerCapture(event.pointerId);
    doc.documentElement.classList.add(DRAGGING_CLASS, RESIZING_CLASS);

    const move = (e: PointerEvent): void => {
      pointerPx = e.clientY - startY;
      const nextLines = draggedLines(upperFit, lowerFit, pointerPx);
      const next = linesOf(nextLines);
      if (next.every((lines, i) => lines === shown[i])) return;
      shownLines = nextLines;
      shown = next;
      const asked = sizing.map((section, i) => ({ section, lines: next[i] === start[i] ? startSizes[i] : next[i] }));
      for (const piece of sizedPieces(asked).pieces) held.push({ key: piece.key, token: sc.view.ask(piece.key, piece.value) });
      refit();
    };
    const end = (commit: boolean): void => {
      doc.removeEventListener("pointermove", move);
      doc.removeEventListener("pointerup", up);
      doc.removeEventListener("pointercancel", lost);
      grip.removeEventListener("lostpointercapture", lost);
      doc.removeEventListener("keydown", key, true);
      doc.documentElement.classList.remove(DRAGGING_CLASS, RESIZING_CLASS);
      dragging = null;
      // ONLY A SECTION THE DRAG CHANGED IS SIZED: one it left as it stood keeps its height.
      const changed = sizing.map((section, i) => ({ section, lines: shown[i] })).filter((_, i) => shown[i] !== start[i]);
      if (!commit || changed.length === 0) {
        for (const { key: k, token } of held) sc.view.abandon(k, token);
        refit();
        log.debug(commit ? "a sidebar rule was pressed and let go where it stood" : "a sidebar rule's drag was cancelled", {
          operation: "sidebar.section-resize.cancelled",
          context: { upper: pair.upper.className, lower: pair.lower.className },
        });
        return;
      }
      const { pieces, change } = sizedPieces(changed);
      log.info("a sidebar rule was dragged: asking the daemon for the sections' heights", {
        operation: "sidebar.section-resize",
        context: {
          upper: pair.upper.className,
          lower: pair.lower.className,
          from: `${startLines.upper}/${startLines.lower}`,
          to: `${shownLines.upper}/${shownLines.lower}`,
          upper_total: upperFit.stops.length - 1,
          lower_total: lowerFit.stops.length - 1,
          pointer_px: pointerPx,
          moved_px: upperFit.stops[shownLines.upper] - upperFit.stops[startLines.upper],
          sized: changed.map((c) => foldKeyOf(c.section)).join(" "),
        },
      });
      void changeViews(sc, { pieces, change, control: grip }).then((took) => {
        if (!took) refit();
      });
    };
    const up = (): void => end(true);
    const lost = (): void => end(false);
    const key = (e: KeyboardEvent): void => {
      if (e.key !== "Escape") return;
      e.preventDefault();
      end(false);
    };
    // On the document, not the grip: a re-render that detaches the grip mid-drag
    // would strand listeners on it and leave RESIZING_CLASS's cursor stuck.
    doc.addEventListener("pointermove", move);
    doc.addEventListener("pointerup", up);
    doc.addEventListener("pointercancel", lost);
    grip.addEventListener("lostpointercapture", lost);
    doc.addEventListener("keydown", key, true);
    dragging = () => end(false);
  };

  const reset = (event: MouseEvent): void => {
    const pressed = pressedRule(event);
    if (pressed === null) return;
    const { grip, pair } = pressed;
    event.preventDefault();
    const bands = pair.upper.classList.contains(OPEN_SECTION_CLASS) ? [pair.lower] : [pair.upper, pair.lower];
    const { pieces, change } = sizedPieces(bands.map((section) => ({ section, lines: null })));
    log.info("a sidebar rule was double clicked: asking the daemon to fit the sections beside it", {
      operation: "sidebar.section-resize.fit",
      context: { upper: pair.upper.className, lower: pair.lower.className },
    });
    const asked = changeViews(sc, { pieces, change, control: grip });
    refit();
    void asked.then((took) => {
      if (!took) refit();
    });
  };

  host.addEventListener("pointerdown", press);
  host.addEventListener("dblclick", reset);
  return () => {
    dragging?.();
    host.removeEventListener("pointerdown", press);
    host.removeEventListener("dblclick", reset);
  };
}

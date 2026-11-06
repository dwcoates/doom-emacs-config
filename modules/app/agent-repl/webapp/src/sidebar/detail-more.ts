/**
 * detail-more — a row panel's "details" entry and the child popup it opens
 * (owner request, 2026-11-01).
 *
 * THE PANEL STATES WHAT THE READER WORKS WITH; HOW AGENT-REPL RUNS IT IS ONE
 * HOVER FURTHER. Every fact a row panel can state has one home
 * (`FACT_HOMES`): the panel itself (branch, from, commits, the summary, the
 * children), or the details popup (what the workspace's agent process and web
 * page hold in memory, and whether the page is loaded and when it unloads).
 * The details popup's facts are reached through a single "details" entry at
 * the panel's bottom, drawn only while the popup has at least one fact.
 *
 * THE CHILD POPUP STANDS TO THE PANEL AS THE PANEL STANDS TO ITS ROW: beside
 * it, meeting its right edge with no gap, its top on the entry's line, placed
 * by the same `clampBeside`. It is the entry's own descendant, so the pointer
 * travelling from the panel into it never leaves the panel (no `mouseleave`
 * fires on an ancestor for a move into its descendant), and leaving both
 * leaves the panel, which closes both.
 */
import { log } from "../log.js";
import { placeBeside } from "../placement.js";
import { clampBeside, type Rect, type RevealPlacement, type Viewport } from "../topbar/clamp.js";

/** The class of a row panel's "details" entry. */
export const DETAILS_ENTRY_CLASS = "detail-more";

/** The class of the child popup inside the entry. */
export const DETAILS_PANEL_CLASS = "detail-sub";

/** The entry's own words. */
export const DETAILS_LABEL = "details";

/** Where a row panel's fact is stated. */
export type FactHome = "panel" | "details";

/**
 * EVERY FACT A ROW PANEL CAN STATE, AND ITS HOME. A label missing here is a
 * fact nobody classified, which `factHome` refuses rather than guess.
 */
export const FACT_HOMES: Readonly<Record<string, FactHome>> = {
  branch: "panel",
  from: "panel",
  commits: "panel",
  summary: "panel",
  children: "panel",
  agent: "details",
  page: "details",
};

/** LABEL's home. Throws for a label `FACT_HOMES` does not classify. */
export function factHome(label: string): FactHome {
  const home = FACT_HOMES[label];
  if (home === undefined) {
    log.error("a row panel was handed a fact with no home", {
      operation: "sidebar.row.detail-fact-unclassified",
      context: { label },
    });
    throw new Error(`agent-repl: the row panel fact '${label}' has no home in FACT_HOMES`);
  }
  return home;
}

/** Refuse LABEL unless its home is HOME: the drawing about to state it there would misplace it. */
export function requireFactHome(label: string, home: FactHome): void {
  const actual = factHome(label);
  if (actual === home) return;
  log.error("a row panel fact was drawn away from its home", {
    operation: "sidebar.row.detail-fact-misplaced",
    context: { label, home: actual, drawn_in: home },
  });
  throw new Error(`agent-repl: the row panel fact '${label}' belongs in the ${actual}, not the ${home}`);
}

/** DETAIL's "details" entry, or null while it has none. */
export function detailsEntry(detail: HTMLElement): HTMLElement | null {
  return detail.querySelector<HTMLElement>(`:scope > .${DETAILS_ENTRY_CLASS}`);
}

/** The child popup inside ENTRY. Throws when the entry has none: every entry is drawn with one. */
function entryPanel(entry: HTMLElement): HTMLElement {
  const sub = entry.querySelector<HTMLElement>(`:scope > .${DETAILS_PANEL_CLASS}`);
  if (sub === null) {
    log.error("a row panel's details entry has no popup", {
      operation: "sidebar.row.details-no-popup",
      context: { html: entry.outerHTML.slice(0, 200) },
    });
    throw new Error("agent-repl: a row panel's details entry has no popup");
  }
  return sub;
}

/**
 * DETAIL's details popup, its entry drawn at the panel's bottom the first time
 * it is asked for. The entry is hidden until `syncDetailsEntry` finds a fact
 * in the popup.
 */
export function detailsPanel(detail: HTMLElement): HTMLElement {
  const existing = detailsEntry(detail);
  if (existing !== null) return entryPanel(existing);
  const entry = document.createElement("div");
  entry.className = DETAILS_ENTRY_CLASS;
  entry.tabIndex = 0;
  entry.hidden = true;
  const label = document.createElement("span");
  label.className = "detail-more-label";
  label.textContent = DETAILS_LABEL;
  const sub = document.createElement("div");
  sub.className = DETAILS_PANEL_CLASS;
  entry.append(label, sub);
  detail.appendChild(entry);
  return sub;
}

/** Draw DETAIL's details entry exactly while its popup states at least one fact. */
export function syncDetailsEntry(detail: HTMLElement): void {
  const entry = detailsEntry(detail);
  if (entry === null) return;
  const empty = (entryPanel(entry).textContent ?? "").trim() === "";
  entry.hidden = empty;
  if (empty) entry.classList.remove("open");
}

/**
 * Where the details popup lands beside PANEL, its top on ENTRY's line: the
 * row panel's own placement (`clampBeside`) with the panel's right edge as
 * the anchor's.
 */
export function detailsPlacement(panel: Rect, entry: Rect, sub: Rect, viewport: Viewport): RevealPlacement {
  return clampBeside(detailsAnchor(panel, entry), sub, viewport);
}

/** What the details popup stands beside: PANEL's right edge, on ENTRY's line. */
export function detailsAnchor(panel: Rect, entry: Rect): Rect {
  return {
    left: panel.left,
    right: panel.right,
    width: panel.width,
    top: entry.top,
    bottom: entry.bottom,
    height: entry.bottom - entry.top,
  };
}

/** Place DETAIL's open details popup beside it, measuring with MEASURE inside VIEWPORT. Nothing while it is shut. */
export function placeDetailsPanel(detail: HTMLElement, measure: (el: Element) => Rect, viewport: Viewport): void {
  const entry = detailsEntry(detail);
  if (entry === null || !entry.classList.contains("open")) return;
  placeBeside(entryPanel(entry), detailsAnchor(measure(detail), measure(entry)), viewport, measure);
}

/** Open DETAIL's details popup (a drawn entry only) and place it with PLACE. */
export function openDetails(detail: HTMLElement, place: () => void): void {
  const entry = detailsEntry(detail);
  if (entry === null || entry.hidden || entry.classList.contains("open")) return;
  entry.classList.add("open");
  log.debug("opening a row panel's details", {
    operation: "sidebar.row.details-open",
    context: { workspace: detail.closest("[data-roster-row]")?.getAttribute("data-roster-row") ?? null },
  });
  place();
}

/** Shut DETAIL's details popup. */
export function closeDetails(detail: HTMLElement): void {
  detailsEntry(detail)?.classList.remove("open");
}

/**
 * Open DETAIL's details popup as the pointer or the focus reaches its entry
 * or the entry is activated, and shut it as both leave the entry and its
 * popup. Delegated on the panel, so an entry drawn after this (the memory
 * block's, on the panel's first open) is wired too. PLACE places the popup.
 */
export function wireDetailsEntry(detail: HTMLElement, place: () => void): void {
  const inEntry = (node: EventTarget | null): boolean => {
    const entry = detailsEntry(detail);
    return entry !== null && node instanceof Node && entry.contains(node);
  };
  const open = (event: Event): void => {
    if (inEntry(event.target)) openDetails(detail, place);
  };
  const shut = (event: Event): void => {
    const next = (event as MouseEvent | FocusEvent).relatedTarget;
    if (inEntry(event.target) && !inEntry(next)) closeDetails(detail);
  };
  detail.addEventListener("mouseover", open);
  detail.addEventListener("focusin", open);
  detail.addEventListener("click", open);
  detail.addEventListener("keydown", (event) => {
    if (event.key === "Enter" || event.key === " ") open(event);
  });
  detail.addEventListener("mouseout", shut);
  detail.addEventListener("focusout", shut);
}

/**
 * search — the rail's workspace search: a magnifier left of the title, and the
 * search bar it slides open under the header.
 *
 * THE BAR OVERLAPS THE ROWS, it never moves them. It hangs absolutely under the
 * header and its height animates open, its bottom rule sliding down from where
 * the header's divider sits, over the top of the sections. The scroller keeps
 * its height, so the band fit (`band-fit.ts`) is never re-measured for it.
 *
 * THE QUERY NARROWS EVERY SECTION: the repository sections, Hidden
 * Workspaces and Recently Merged alike. A row stays
 * when its name matches or any row nested under it does, so a matching child
 * keeps its family's line above it; a section or merged group left with no
 * row is hidden with its header. Folds and the daemon's counts are untouched:
 * the search is this page's own, transient, and selects among drawn rows.
 *
 * THE MATCH: the query's words, in order, each found in the name as typed
 * (case aside), with anything between them. Whitespace is the wildcard, so
 * "fix side" matches "fix-the-sidebar". A blank query matches everything.
 *
 * Closing the bar clears the query, so no row is ever hidden by a search the
 * page no longer shows.
 */
import { createControl, type Control } from "../control.js";
import { childrenMatching, queryAll, SVG_NS } from "../dom.js";
import { log } from "../log.js";

/** The class a row the query does not match wears. */
export const SEARCH_MISS_CLASS = "sb-search-miss";

/** The class a section or merged group the query leaves no row in wears. */
export const SEARCH_EMPTY_CLASS = "sb-search-empty";

/** The class the bar wears while it is open. */
export const SEARCH_OPEN_CLASS = "open";

/**
 * The pattern QUERY matches names with, or null when the query is blank (every
 * name matches). Each whitespace-separated word is literal; the gaps between
 * them match anything.
 */
export function searchPattern(query: string): RegExp | null {
  const words = query.trim().split(/\s+/).filter((word) => word !== "");
  if (words.length === 0) return null;
  return new RegExp(words.map(escapeRegExp).join(".*"), "i");
}

/** Whether NAME matches QUERY (see `searchPattern`). */
export function matchesSearch(name: string, query: string): boolean {
  return searchPattern(query)?.test(name) ?? true;
}

function escapeRegExp(word: string): string {
  return word.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/** The name a drawn roster row shows: its own line's `.name`. */
function rowName(ws: HTMLElement): string {
  const label = ws.querySelector<HTMLElement>(":scope > .row > .name");
  if (label === null) {
    log.error("a sidebar row cannot be searched: it is drawn without its name", {
      operation: "sidebar.search.malformed",
      context: { workspace: ws.getAttribute("data-roster-row") },
    });
    throw new Error(`sidebar search: row ${ws.getAttribute("data-roster-row") ?? "?"} has no name`);
  }
  return label.textContent ?? "";
}

/**
 * Mark WS and every row nested under it hit or miss against PATTERN. Returns
 * whether WS stays: its own name matches, or a nested row's does.
 */
function markRow(ws: HTMLElement, pattern: RegExp | null): boolean {
  let kept = pattern === null || pattern.test(rowName(ws));
  for (const kid of childrenMatching(ws, ".kids").flatMap((kids) => childrenMatching<HTMLElement>(kids, "[data-roster-row]"))) {
    if (markRow(kid, pattern)) kept = true;
  }
  ws.classList.toggle(SEARCH_MISS_CLASS, !kept);
  return kept;
}

/** What one search pass left drawn. */
export interface SearchOutcome {
  /** Rows kept, nested rows included, across every pane. */
  kept: number;
  /** Rows the query hid. */
  missed: number;
}

/**
 * Narrow every drawn row under ROOT to those QUERY matches, and hide every
 * section and merged group it leaves empty. A blank query shows everything.
 */
export function applySearch(root: ParentNode, query: string): SearchOutcome {
  const pattern = searchPattern(query);
  for (const region of queryAll<HTMLElement>(root, ".rows")) {
    for (const ws of childrenMatching<HTMLElement>(region, "[data-roster-row]")) markRow(ws, pattern);
  }
  for (const box of queryAll<HTMLElement>(root, ".repo, .merged-group")) {
    const empty =
      pattern !== null && box.querySelector(`[data-roster-row]:not(.${SEARCH_MISS_CLASS})`) === null;
    box.classList.toggle(SEARCH_EMPTY_CLASS, empty);
  }
  const rows = queryAll(root, "[data-roster-row]").length;
  const missed = queryAll(root, `[data-roster-row].${SEARCH_MISS_CLASS}`).length;
  return { kept: rows - missed, missed };
}

/** The magnifier glyph the toggle draws. */
function drawMagnifier(): SVGSVGElement {
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("viewBox", "0 0 16 16");
  svg.setAttribute("aria-hidden", "true");
  svg.setAttribute("class", "sb-search-glyph");
  const lens = document.createElementNS(SVG_NS, "circle");
  lens.setAttribute("cx", "6.5");
  lens.setAttribute("cy", "6.5");
  lens.setAttribute("r", "4.5");
  const handle = document.createElementNS(SVG_NS, "path");
  handle.setAttribute("d", "M10 10 L14.5 14.5");
  svg.append(lens, handle);
  return svg;
}

/** The rail's search: the header's toggle, the bar under it, and its query. */
export interface RailSearch {
  /** The magnifier control the header draws left of its title. */
  toggle: Control;
  /** The bar, hung under the header. */
  bar: HTMLElement;
  /** The query as typed; blank while the bar is closed. */
  query(): string;
  /** Whether the bar is open. */
  isOpen(): boolean;
}

/**
 * Build the rail's search. CHANGED is told the query every time it changes:
 * typed, or cleared by the bar closing.
 */
export function createRailSearch(changed: (query: string) => void): RailSearch {
  const toggle = createControl();
  toggle.className = "sb-search-btn";
  toggle.setAttribute("data-search-toggle", "");
  toggle.title = "search workspaces";
  toggle.appendChild(drawMagnifier());

  const bar = document.createElement("div");
  bar.className = "sb-search";
  bar.setAttribute("data-search-bar", "");
  const input = document.createElement("input");
  input.type = "text";
  input.className = "sb-search-input";
  input.setAttribute("data-search-input", "");
  input.placeholder = "Search workspaces";
  input.spellcheck = false;
  input.tabIndex = -1;
  bar.appendChild(input);

  const isOpen = (): boolean => bar.classList.contains(SEARCH_OPEN_CLASS);
  const setOpen = (open: boolean): void => {
    bar.classList.toggle(SEARCH_OPEN_CLASS, open);
    toggle.classList.toggle("active", open);
    input.tabIndex = open ? 0 : -1;
    log.debug(open ? "opened the sidebar search" : "closed the sidebar search", {
      operation: "sidebar.search.toggle",
      context: { open },
    });
    if (open) {
      input.focus();
      return;
    }
    if (input.value === "") return;
    input.value = "";
    changed("");
  };

  toggle.addEventListener("click", (event) => {
    event.preventDefault();
    setOpen(!isOpen());
  });
  input.addEventListener("input", () => changed(input.value));
  input.addEventListener("keydown", (event) => {
    if (event.key === "Escape") setOpen(false);
  });

  return {
    toggle,
    bar,
    query: () => (isOpen() ? input.value : ""),
    isOpen,
  };
}

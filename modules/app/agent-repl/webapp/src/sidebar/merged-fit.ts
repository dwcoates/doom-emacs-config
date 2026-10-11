/**
 * merged-fit — how many Recently Merged entries the rail shows, and the count
 * the folded band says.
 *
 * NO FIXED CAP, AND THE COUNT IS WHAT IS SHOWN (owner request, 2026-10-08).
 * The daemon sends EVERY merged workspace, in one group per repository, each
 * group most recently merged first. The band shows as many of them as FIT in
 * the rail without scrolling, the MOST RECENT across every shown group first,
 * and the "(N)" its folded header draws is that same N: the number of entries
 * visible when it is unfolded. Only this page knows its own height, so "fits"
 * is decided here, and it is decided ONCE: `fitMergedSection` computes N and,
 * in the same pass, hides every row past it (and every group left with no
 * row) and writes N into the count. Nothing else writes either, so the two
 * cannot drift.
 *
 * A GROUP COSTS ITS HEADER. The first row a group shows also spends one group
 * header's height, measured off its own probe, so N rows across three groups
 * take three headers' room more than N rows in one.
 *
 * A COLLAPSED REPOSITORY'S GROUP IS NOT FITTED. It is not drawn
 * (`data-repository-collapsed`, painted by the roster off the repository's
 * fold), so it neither spends room nor counts.
 *
 * N DOES NOT DEPEND ON THE FOLD. The room is the rail's visible height less
 * everything the pane draws OTHER than the band's rows region, and a folded
 * band's rows region takes no height, so the folded and the unfolded band
 * compute the same N from the same layout.
 *
 * AN UNLAID BAND KEEPS ITS DRAWN STATE. A pane that is not shown (the other
 * grouping), a rail not yet on the page, or a unit that measures nothing
 * cannot be fitted; such a band is left as drawn — every row, under the
 * daemon's count of every shown row — which is itself consistent, and it is
 * fitted the moment it lays out (the rail observes its scroller and its panes).
 */
import { log } from "../log.js";

/** The class of the band's section, and of the probes its units are measured off. */
export const MERGED_SECTION_CLASS = "merged-section";
export const MERGED_ROW_PROBE_CLASS = "merged-row-probe";
export const MERGED_GROUP_HEAD_PROBE_CLASS = "merged-group-head-probe";

/** The class of one repository's group inside the band, and of its header. */
export const MERGED_GROUP_CLASS = "merged-group";
export const MERGED_GROUP_HEAD_CLASS = "merged-group-head";

/** The attribute a merged row states its merged instant (epoch ms) on. */
export const MERGED_AT_ATTRIBUTE = "data-merged-at";

/** The attribute a group states whether its repository is collapsed on. */
export const MERGED_REPOSITORY_COLLAPSED_ATTRIBUTE = "data-repository-collapsed";

/** The attribute the band states how many entries it shows on. */
export const MERGED_SHOWN_ATTRIBUTE = "data-merged-shown";

/**
 * How many rows of each group fit: AVAILABLE pixels spent on the most recent
 * rows across GROUPS first (each group's merged instants, most recent first),
 * ROW_HEIGHT per row and HEAD_HEIGHT more for a group's first shown row. The
 * spending stops at the first row that does not fit, so what is shown is
 * always the N most recent merges. A tie goes to the earlier group.
 */
export function mergedRowsThatFit(available: number, rowHeight: number, headHeight: number, groups: number[][]): number[] {
  if (!(rowHeight > 0)) throw new RangeError(`merged rows cannot be fitted at row height ${rowHeight}`);
  if (!(headHeight >= 0)) throw new RangeError(`merged groups cannot be fitted at header height ${headHeight}`);
  const shown = groups.map(() => 0);
  // A hair of tolerance, so a room of exactly N rows in sub-pixel layout is N.
  let room = available + 1e-6;
  for (;;) {
    let next = -1;
    groups.forEach((atMs, g) => {
      const candidate = atMs[shown[g]];
      if (candidate === undefined) return;
      if (next === -1 || candidate > groups[next][shown[next]]) next = g;
    });
    if (next === -1) return shown;
    const cost = rowHeight + (shown[next] === 0 ? headHeight : 0);
    if (cost > room) return shown;
    room -= cost;
    shown[next] += 1;
  }
}

/** A box's declared vertical padding, in pixels; an undeclared one is zero. */
function verticalPadding(el: HTMLElement): number {
  const style = getComputedStyle(el);
  const px = (value: string): number => (value === "" ? 0 : Number.parseFloat(value));
  return px(style.paddingTop) + px(style.paddingBottom);
}

/** The band's parts the fit reads, or a malformed band refused loudly. */
function bandParts(section: HTMLElement): { region: HTMLElement; rowProbe: HTMLElement; headProbe: HTMLElement; count: HTMLElement } {
  const region = section.querySelector<HTMLElement>(":scope > .rows");
  const rowProbe = section.querySelector<HTMLElement>(`:scope > .${MERGED_ROW_PROBE_CLASS}`);
  const headProbe = section.querySelector<HTMLElement>(`:scope > .${MERGED_GROUP_HEAD_PROBE_CLASS}`);
  const count = section.querySelector<HTMLElement>(":scope > .repo-head [data-section-count]");
  if (region === null || rowProbe === null || headProbe === null || count === null) {
    log.error("the Recently Merged band cannot be fitted: it is missing a part it is drawn with", {
      operation: "sidebar.merged-fit.malformed",
      context: { rows: region !== null, row_probe: rowProbe !== null, head_probe: headProbe !== null, count: count !== null },
    });
    throw new Error("merged fit: the band is missing its rows region, row probe, group header probe or count");
  }
  return { region, rowProbe, headProbe, count };
}

/** A shown group's rows and their merged instants; a row without one is malformed. */
function groupRows(group: HTMLElement): { rows: HTMLElement[]; mergedAt: number[] } {
  const rows = [...(group.querySelector<HTMLElement>(":scope > .rows")?.children ?? [])].filter(
    (el): el is HTMLElement => el instanceof HTMLElement,
  );
  const mergedAt = rows.map((row) => Number(row.getAttribute(MERGED_AT_ATTRIBUTE)));
  if (mergedAt.some((atMs, i) => !Number.isFinite(atMs) || rows[i].getAttribute(MERGED_AT_ATTRIBUTE) === null)) {
    log.error("the Recently Merged band cannot be fitted: a merged row states no merged instant", {
      operation: "sidebar.merged-fit.malformed",
      context: { repository: group.getAttribute("data-repository"), rows: rows.length },
    });
    throw new Error("merged fit: a merged row states no merged instant");
  }
  return { rows, mergedAt };
}

/**
 * Fit the band in PANE, which hangs in the rail's SCROLLER: show the N most
 * recent rows across its shown groups, hide the rest (and every group left
 * empty), and say N in its count. Returns N, or null when the band is absent,
 * empty, or not laid out (left as drawn; see the module note).
 */
export function fitMergedSection(scroller: HTMLElement, pane: HTMLElement): number | null {
  const section = pane.querySelector<HTMLElement>(`:scope > .${MERGED_SECTION_CLASS}`);
  if (section === null || section.hidden) return null;
  const { region, rowProbe, headProbe, count } = bandParts(section);
  const groups = [...region.querySelectorAll<HTMLElement>(`:scope > .${MERGED_GROUP_CLASS}`)]
    .filter((group) => group.getAttribute(MERGED_REPOSITORY_COLLAPSED_ATTRIBUTE) !== "true")
    .map((group) => ({ group, ...groupRows(group) }));
  const rowHeight = rowProbe.offsetHeight;
  const headHeight = headProbe.offsetHeight;
  const paneHeight = pane.offsetHeight;
  if (rowHeight <= 0 || headHeight <= 0 || paneHeight <= 0 || scroller.clientHeight <= 0) {
    log.debug("left the Recently Merged band as drawn: it is not laid out", {
      operation: "sidebar.merged-fit.unlaid",
      context: { row_height: rowHeight, head_height: headHeight, pane_height: paneHeight, scroller_height: scroller.clientHeight },
    });
    return null;
  }
  const others = paneHeight - region.offsetHeight;
  const available = scroller.clientHeight - verticalPadding(scroller) - others;
  const perGroup = mergedRowsThatFit(available, rowHeight, headHeight, groups.map((g) => g.mergedAt));
  groups.forEach(({ group, rows }, g) => {
    group.hidden = perGroup[g] === 0;
    rows.forEach((row, index) => {
      row.hidden = index >= perGroup[g];
    });
  });
  const shown = perGroup.reduce((sum, n) => sum + n, 0);
  count.textContent = `(${shown})`;
  section.setAttribute(MERGED_SHOWN_ATTRIBUTE, String(shown));
  log.debug("fitted the Recently Merged band", {
    operation: "sidebar.merged-fit",
    context: {
      groups: groups.length,
      total: groups.reduce((sum, g) => sum + g.rows.length, 0),
      shown,
      available,
      row_height: rowHeight,
      head_height: headHeight,
    },
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

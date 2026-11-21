/**
 * roster — the rail: the open workspaces by repository, the Merge Queue
 * section, the Recently Merged section, and the hidden workspaces, one copy
 * of each (owner ruling, 2026-10-15). Nothing is re-grouped, re-sorted or
 * counted here.
 *
 * THE ROSTER IS FOUR SNAPSHOT STREAMS (`roster-streams.ts`): the workspaces
 * (the open sections and the hidden band), the merge queue, the Recently
 * Merged summary and the closed rows, which no section draws. A stream a push
 * did not replace is the very snapshot already drawn, so its sections are
 * kept without even being compared; a replaced one is compared section by
 * section, and only what changed is drawn anew.
 *
 * THE MERGE QUEUE AND RECENTLY MERGED SECTIONS (owner request, 2026-11-01,
 * splitting the Deploys section in two) have no key of their own (the roster
 * carries exactly one of each), so their fold identities are fixed. Each
 * draws one group per repository under the same header: the Merge Queue
 * section its active queue (`merge-queue-view.ts`), the Recently Merged
 * section its merged workspaces, each group folding on this page by hand
 * (`deploys-fold.ts`).
 *
 * THE VIEW IS THE DAEMON'S. Every section's fold is drawn from the push,
 * painted at once on a gesture and asked of the daemon (`view-change.ts`), so
 * the sidebar looks the same in every workspace's page.
 *
 * `WorkspaceRoster.current` IS NOT COMPARED HERE. The row states its own
 * highlight through `RosterRowCurrent`, deliberately, so no client can compute
 * it differently. The field is validated and logged, and that is all.
 */
import { drawRosterSectionHeight, drawSectionHeight, type RosterSectionHeight } from "./section-resize.js";
import { FOLDED_CLASS } from "./folded-class.js";
import {
  HIDDEN_SECTION_CLASS,
  MERGED_AT_ATTRIBUTE,
  MERGED_SECTION_CLASS,
  MERGE_QUEUE_SECTION_CLASS,
  OPEN_REGION_CLASS,
  OPEN_SECTION_CLASS,
} from "./band-fit.js";
import {
  RosterHiddenSectionSchema,
  RosterMergedSectionSchema,
  RosterMergeQueueSectionSchema,
  RosterOpenSectionSchema,
  RosterRepoSectionSchema,
  type RosterCurrentWorkspace,
  type RosterHiddenSection,
  type RosterLabel,
  type RosterMergedRepoGroup,
  type RosterMergedRepoGroupHeader,
  type RosterMergedSection,
  type RosterMergeQueueRepoGroup,
  type RosterMergeQueueSection,
  type RosterMergedRepoGroupSummary,
  type RosterOpenCount,
  type RosterOpenSection,
  type RosterRepoDivergence,
  type RosterRepoKey,
  type RosterRepoSection,
  type RosterOpenRows,
  type RosterSectionCount,
  type RosterSectionHeader,
  type RosterOpenHeader,
  type WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { changedStreams, type RosterStreams } from "./roster-streams.js";
import type { RepositoryRef, WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { clone, equals } from "@bufbuild/protobuf";
import { placeChildren } from "./children.js";
import { decorativeElement, queryAll } from "../dom.js";
import { disposeKeyedRows, drawKeyedEntries, drawKeyedRows, patchKeyedRegions, remarkKeyedRows, type KeyedRows } from "./keyed-rows.js";
import { repoEntries, repoSectionRegions, sectionOpenRows, sectionSubsections, unsectionedRows, type PathedOpenRow } from "./entries.js";
import { log } from "../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { MalformedView } from "../rpc/malformed.js";
import {
  HIDDEN_FOLD_KEY,
  MERGED_FOLD_KEY,
  MERGE_QUEUE_FOLD_KEY,
  OPEN_FOLD_KEY,
  repoFoldKey,
  type RailContext,
  type SidebarContext,
} from "./context.js";
import { REARRANGED_ATTRIBUTE } from "../drag.js";
import {
  drawCreateSubsectionControl,
  drawRosterSubsection,
  installArrangeDrag,
  paintSubsectionWork,
  REPOSITORY_ATTRIBUTE,
  requireDrawnBucket,
  stampBucket,
  SUBSECTION_CLASS,
  SUBSECTION_HEAD_CLASS,
  SUBSECTION_LABEL_CLASS,
  subsectionBucket,
  type ArrangeRefs,
} from "./subsections.js";
import {
  foldOnClick,
  drawRosterHoldsCurrent,
  drawRosterSectionFold,
  paintHoldsCurrent,
  sectionFold,
  trackDaemonFold,
  type FoldGesture,
  type FoldTarget,
  type RosterSectionFold,
} from "./section-fold.js";
import { openScope, type DrawScope } from "./scope.js";
import {
  DEPLOYS_GROUP_CLASS,
  DEPLOYS_LINE_ATTRIBUTE,
  DEPLOYS_REPOSITORY_ATTRIBUTE,
  DEPLOYS_SECTION_ATTRIBUTE,
  type DeploysGroupSection,
} from "./deploys-fold.js";
import { MERGED_PAGE_CLASS, drawMergedMoreSpinner, mergedRowAtMs, type MergedGroupPages, type MergedPages } from "./merged-pages.js";
import { drawEscalationRelease, drawQueuedGroupHead, drawQueueMediationLabel, drawRosterMergeQueue, queueCount, queueHoverText, queueLine } from "./merge-queue-view.js";
import { coldGatedOf, sameColdGated, type ColdGated } from "./cold-gate-name.js";
import {
  drawRosterFamily,
  type RowFamily,
  pathedFamilies,
  mergedToday,
  tickPaint,
  type RowPlace,
} from "./row.js";
import { countNoun } from "../count-noun.js";
import { formatAge } from "../duration.js";
import { paintRecencyGutters, recencyGutters } from "./recency-gutters.js";
import { paintTabNumbers, tabNumbers } from "./tab-numbers.js";
import { drawRosterOpenCount, openCountText } from "./open-count.js";

/**
 * Every roster stream as of one push, drawn through RAIL's one roster drawer
 * (`createRosterDrawer`): a section the push leaves unchanged is the one
 * already drawn.
 */
export function drawWorkspaceRoster(u: RosterStreams, rail: RailContext): HTMLElement {
  let drawer = drawers.get(rail);
  if (drawer === undefined) {
    drawer = createRosterDrawer(rail);
    drawers.set(rail, drawer);
  }
  return drawer.draw(u);
}

/** Tear down everything RAIL's roster drawer drew. */
export function disposeWorkspaceRoster(rail: RailContext): void {
  drawers.get(rail)?.dispose();
  drawers.delete(rail);
}

/** Each rail's one roster drawer. */
const drawers = new WeakMap<RailContext, RosterDrawer>();

/** One rail's roster, drawn push after push. */
export interface RosterDrawer {
  /** Draw U, keeping every section it leaves unchanged; answers the rail's one roster root. */
  draw(u: RosterStreams): HTMLElement;
  /** Tear down every section's registrations. */
  dispose(): void;
}

/** What drawing a section made: its element, and whatever else the drawer reaches into. */
interface Drawn {
  element: HTMLElement;
}

/** A drawn section, the wire it was drawn from, and what it registered. */
interface DrawnSection<W, D extends Drawn = Drawn> {
  wire: W;
  drawn: D;
  scope: DrawScope;
}

/**
 * The rail's roster drawer.
 *
 * A PUSH REDRAWS ONLY WHAT IT CHANGED (2026-10-15). Every push used to throw
 * the whole rail away and rebuild it: 45ms and more of script for a push that
 * changed one row, and pushes come in bursts. A section — a repository's, the
 * hidden band, the Merge Queue and Recently Merged sections, the open
 * section's heading — whose
 * wire is equal to the one it was drawn from is kept, DOM, view copies, clocks
 * and all (`scope.ts`); a changed one is drawn anew and the old one's scope
 * disposed. Equality is the wire's own, so nothing here decides what changed.
 *
 * A KEPT SECTION WAS VALIDATED when it was drawn: its wire is equal to one
 * that was, so a malformed push still fails at the section it breaks.
 */
export function createRosterDrawer(rail: RailContext): RosterDrawer {
  const roster = document.createElement("div");
  roster.className = "sb-roster";
  // ONE COPY OF EVERY SECTION (owner ruling, 2026-10-15): the open workspaces
  // by repository, then the Merge Queue section, then the Recently Merged
  // section, then the hidden workspaces (owner requests, 2026-10-27: the
  // queues and merges above Hidden; 2026-11-01: split in two).
  // The pane is the box band-fit.ts fits the four sections' viewports in.
  const pane = document.createElement("div");
  pane.className = "sb-pane";
  roster.appendChild(pane);
  let repos = new Map<string, DrawnSection<RosterRepoSection, DrawnRepoSection>>();
  // THE GROUPING'S DRAGS (subsections.ts) are found from the press, under one
  // listener for the drawer's life, and name the refs of the roster drawn.
  const dragScope = openScope(rail);
  const removeDrag = installArrangeDrag(dragScope.context, roster, drawnRefs(() => repos.values()));
  /** The open section, and the box its repository sections are placed in. */
  let open: DrawnSection<{ open: RosterOpenSection; count: string }, Drawn & { body: HTMLElement }> | null = null;
  let hidden: DrawnSection<RosterHiddenSection> | null = null;
  let merged: DrawnSection<{ summary: RosterMergedSection; pages: number }> | null = null;
  let mergeQueue: DrawnSection<{ queue: RosterMergeQueueSection; coldGated: ColdGated }> | null = null;
  /** The streams last drawn, null before the first draw. */
  let drawnStreams: RosterStreams | null = null;

  /** SECTION kept when WIRE equals what it was drawn from, else drawn anew by DRAW. */
  const keepOrDraw = <W, D extends Drawn>(
    section: DrawnSection<W, D> | null,
    wire: W,
    same: (a: W, b: W) => boolean,
    draw: (scoped: SidebarContext) => D,
    discarded: Array<DrawnSection<unknown>>,
  ): DrawnSection<W, D> => {
    // A BOX A DRAG REARRANGED IS DRAWN ANEW (drag.ts), whatever its wire.
    if (section !== null && !section.drawn.element.hasAttribute(REARRANGED_ATTRIBUTE) && same(section.wire, wire)) {
      section.scope.remark(rail.attention);
      return section;
    }
    if (section !== null) discarded.push(section);
    const scope = openScope(rail);
    return { wire, drawn: draw(scope.context), scope };
  };

  /** Whether no drag rearranged SECTION's box, so a wire it was drawn from still says what it shows. */
  const untouched = (section: DrawnSection<unknown>): boolean => !section.drawn.element.hasAttribute(REARRANGED_ATTRIBUTE);

  /** Keep SECTION as drawn: its attention markers are marked again for this pass. */
  const keep = <W, D extends Drawn>(section: DrawnSection<W, D>): DrawnSection<W, D> => {
    section.scope.remark(rail.attention);
    return section;
  };

  /**
   * Draw the WORKSPACES stream's sections (the repositories, the open
   * heading, the hidden band) from U, into DISCARDED what they replace;
   * answers how many rows were patched in place.
   */
  const drawWorkspaces = (u: RosterStreams["workspaces"], discarded: Array<DrawnSection<unknown>>): number => {
    const path = "WorkspaceRoster";
    const repository = requireMessage(u.repository, `${path}.repository`);
    const hiddenWire = requireMessage(u.hidden, `${path}.hidden`);
    const openWire = requireMessage(u.open, `${path}.open`);
    const openCount = openCountText(
      drawRosterOpenCount(
        requireMessage(repository.openCount, `${path}.repository.open_count`),
        `${path}.repository.open_count`,
      ),
    );

    const nextRepos = new Map<string, DrawnSection<RosterRepoSection, DrawnRepoSection>>();
    let patchedRows = 0;
    for (const [index, section] of repository.sections.entries()) {
      const sectionPath = `${path}.repository.sections[${index}]`;
      const key = drawRosterRepoKey(requireMessage(section.key, `${sectionPath}.key`), `${sectionPath}.key`).id;
      if (nextRepos.has(key)) {
        throw new MalformedView(`${sectionPath}.key`, `repository ${key} has two sections`);
      }
      let kept = repos.get(key) ?? null;
      repos.delete(key);
      // A SECTION A DRAG REARRANGED IS DRAWN ANEW (drag.ts): its DOM no
      // longer matches the wire it was drawn from.
      if (kept !== null && kept.drawn.element.hasAttribute(REARRANGED_ATTRIBUTE)) {
        discarded.push(kept);
        kept = null;
      }
      // A SECTION WHOSE ROWS ALONE CHANGED KEEPS ITS BOX and redraws only
      // the rows that changed (`keyed-rows.ts`): a switch changes the two
      // rows it moves the highlight between, never their sections.
      if (kept !== null && !equals(RosterRepoSectionSchema, kept.wire, section) && sameButRows(kept.wire, section)) {
        const regions = kept.drawn.regions;
        const before = regions.map((rows) => rows.entries.map((entry) => entry.element));
        const wires = repoSectionRegions(section, sectionPath);
        if (patchKeyedRegions(regions.map((rows, r) => ({ kept: rows, rows: wires[r] })), rail, "grouping")) {
          regions.forEach((rows, r) => {
            patchedRows += rows.entries.filter((entry, i) => entry.element !== before[r][i]).length;
            remarkKeyedRows(rows, rail.attention);
          });
          kept.wire = section;
          paintRepoSection(kept.drawn.element, section, sectionPath);
          kept.scope.remark(rail.attention);
          nextRepos.set(key, kept);
          continue;
        }
      }
      const drawnSection = keepOrDraw(
        kept,
        section,
        (a, b) => equals(RosterRepoSectionSchema, a, b),
        (scoped) => {
          // The section's rows in no subsection, then each subsection's.
          const regions: KeyedRows[] = repoSectionRegions(section, sectionPath).map(() => ({ entries: [] }));
          // The rows' scopes go down with the section's.
          scoped.onDispose(() => regions.forEach(disposeKeyedRows));
          const element = drawRosterRepoSection(section, scoped, sectionPath, {
            own: (rows) => drawKeyedEntries(rows, rail, "grouping", regions[0]),
            subsection: (rows, p, region) => drawKeyedRows(rows, rail, p, "grouping", regions[region]),
          });
          return { element, regions };
        },
        discarded,
      );
      if (drawnSection === kept) kept.drawn.regions.forEach((rows) => remarkKeyedRows(rows, rail.attention));
      nextRepos.set(key, drawnSection);
    }
    for (const gone of repos.values()) discarded.push(gone);
    repos = nextRepos;

    // THE OPEN SECTION'S COUNT IS PATCHED IN PLACE (owner report, 2026-11-06):
    // a workspace opened or closed changes only the heading's count, and
    // drawing the section anew brings a new rows region that starts at its
    // top, so the rail scrolled to the top and back (`scrollKept`) and
    // flashed its bar. The section keeps its box and its scroll position.
    if (
      open !== null &&
      open.wire.count !== openCount &&
      !open.drawn.element.hasAttribute(REARRANGED_ATTRIBUTE) &&
      equals(RosterOpenSectionSchema, open.wire.open, openWire)
    ) {
      paintOpenCount(open.drawn.element, openCount);
      open.wire = { open: openWire, count: openCount };
    }
    open = keepOrDraw(
      open,
      { open: openWire, count: openCount },
      (a, b) => a.count === b.count && equals(RosterOpenSectionSchema, a.open, b.open),
      (scoped) => {
        const body = document.createElement("div");
        body.className = OPEN_REGION_CLASS;
        return { element: drawRosterOpenSection(openWire, { body, openCount }, scoped, `${path}.open`), body };
      },
      discarded,
    );
    placeChildren(open.drawn.body, [...repos.values()].map((section) => section.drawn.element));
    // EVERY PUSH RENUMBERS EVERY OPEN ROW, kept sections too: a tab opened
    // anywhere shifts the numbers of rows whose own wire did not change.
    paintTabNumbers(open.drawn.body, tabNumbers(repository, `${path}.repository`));
    // Every switch moves every reach, so every push repaints every gutter too.
    paintRecencyGutters(open.drawn.body, recencyGutters(repository, `${path}.repository`));

    hidden = keepOrDraw(
      hidden,
      hiddenWire,
      (a, b) => equals(RosterHiddenSectionSchema, a, b),
      (scoped) => ({ element: drawRosterHiddenSection(hiddenWire, scoped, `${path}.hidden`) }),
      discarded,
    );
    return patchedRows;
  };

  /** Whether a section of the workspaces stream was rearranged by a drag, so its unchanged snapshot is drawn anew anyway. */
  const workspacesRearranged = (): boolean =>
    [...repos.values(), open, hidden].some((section) => section !== null && !untouched(section));

  return {
    draw(u: RosterStreams): HTMLElement {
      const changed = changedStreams(drawnStreams, u);
      const discarded: Array<DrawnSection<unknown>> = [];
      let patchedRows = 0;
      // A STREAM THE PUSH DID NOT REPLACE IS THE SNAPSHOT ALREADY DRAWN: its
      // sections are kept as they stand, uncompared.
      if (open === null || hidden === null || changed.includes("workspaces") || workspacesRearranged()) {
        patchedRows = drawWorkspaces(u.workspaces, discarded);
      } else {
        for (const section of repos.values()) {
          keep(section).drawn.regions.forEach((rows) => remarkKeyedRows(rows, rail.attention));
        }
        keep(open);
        keep(hidden);
      }
      const openDrawn = requireDrawn(open, "the open section");
      const hiddenDrawn = requireDrawn(hidden, "the hidden section");
      // THE RECENTLY MERGED SECTION is the summary and the pages loaded beside
      // it (`merged-pages.ts`), drawn anew when either moved.
      const mergedWire = u.recentlyMerged;
      const pages = rail.mergedPages.version();
      merged =
        merged !== null && untouched(merged) && merged.wire.summary === mergedWire && merged.wire.pages === pages
          ? keep(merged)
          : keepOrDraw(
              merged,
              { summary: mergedWire, pages },
              (a, b) => a.pages === b.pages && equals(RosterMergedSectionSchema, a.summary, b.summary),
              (scoped) => ({ element: drawRosterMergedSection(mergedWire, rail.mergedPages, scoped, "RosterPush.recently_merged") }),
              discarded,
            );
      // THE QUEUE'S LINES READ THE STANDING COLD GATES (cold-gate-name.ts),
      // which ride the workspaces stream, so a gate raised or answered draws
      // the section anew though the queue's own stream is unchanged.
      const mergeQueueWire = u.mergeQueue;
      const coldGated = coldGatedOf(u.workspaces.coldGates, "WorkspaceRoster.cold_gates");
      mergeQueue =
        mergeQueue !== null &&
        untouched(mergeQueue) &&
        mergeQueue.wire.queue === mergeQueueWire &&
        sameColdGated(mergeQueue.wire.coldGated, coldGated)
          ? keep(mergeQueue)
          : keepOrDraw(
              mergeQueue,
              { queue: mergeQueueWire, coldGated },
              (a, b) => equals(RosterMergeQueueSectionSchema, a.queue, b.queue) && sameColdGated(a.coldGated, b.coldGated),
              (scoped) => ({
                element: drawRosterMergeQueueSection(mergeQueueWire, scoped, "RosterPush.merge_queue", coldGated),
              }),
              discarded,
            );
      const workspaces = u.workspaces;
      const current =
        workspaces.current === undefined
          ? null
          : drawRosterCurrentWorkspace(workspaces.current, "WorkspaceRoster.current");
      // A SECTION LEFT OUT OF THE RAIL (`HeadedBoxShape.leftOut`) is not
      // placed, so each section placed stands beside the one drawn next to
      // it, which the stylesheet's rules between sections read.
      placeChildren(
        pane,
        [openDrawn.drawn.element, mergeQueue.drawn.element, merged.drawn.element, hiddenDrawn.drawn.element].filter(
          (element) => !element.hidden,
        ),
      );

      // Disposed only once every kept section has moved into the new drawing,
      // so a discarded box's teardown never reaches a kept section's rows.
      for (const section of discarded) section.scope.dispose(section.drawn.element);
      drawnStreams = u;
      log.debug("drew the workspace roster", {
        operation: "sidebar.roster",
        context: {
          revision: String(u.revision),
          changed_streams: changed,
          repo_sections: repos.size,
          merged_groups: mergedWire.groups.length,
          merge_queue_groups: mergeQueueWire.groups.length,
          hidden_rows: hiddenDrawn.wire.hiddenRows?.rows.length ?? 0,
          redrawn_sections: discarded.length,
          redrawn_rows: patchedRows,
          current,
        },
      });
      return roster;
    },
    dispose(): void {
      removeDrag();
      dragScope.dispose(roster);
      const drawn: Array<DrawnSection<unknown>> = [...repos.values()];
      for (const section of [open, hidden, merged, mergeQueue]) if (section !== null) drawn.push(section);
      for (const section of drawn) section.scope.dispose(section.drawn.element);
      repos = new Map();
      open = hidden = merged = mergeQueue = null;
      drawnStreams = null;
      pane.replaceChildren();
    },
  };
}

/** SECTION, which every draw has drawn by the time it is read; WHAT names it for the invariant. */
function requireDrawn<T>(section: T | null, what: string): T {
  if (section === null) throw new Error(`invariant: ${what} is drawn before it is placed`);
  return section;
}

/** A drawn repository section: its element, and its rows, its own first, then each subsection's. */
interface DrawnRepoSection extends Drawn {
  regions: KeyedRows[];
}

/**
 * Whether two repository sections differ in their rows at most: the same entries in the same order (each row the same
 * workspace, each subsection the same one under the same header), only the
 * rows' drawings changed.
 */
function sameButRows(a: RosterRepoSection, b: RosterRepoSection): boolean {
  const shape = (section: RosterRepoSection): string[] =>
    repoEntries(section, "RosterRepoSection").map((entry) =>
      entry.case === "row" ? `row:${entry.row.row?.workspace?.workspace?.id ?? ""}` : "subsection",
    );
  const left = clone(RosterRepoSectionSchema, a);
  const right = clone(RosterRepoSectionSchema, b);
  for (const section of [left, right]) {
    // WHETHER A SECTION HOLDS THE CURRENT WORKSPACE moves with every switch
    // between repositories, and the patch repaints it
    // (`paintRepoSection`), so it is no reason to redraw the section.
    if (section.fold.case === "expanded") section.fold.value.collapse = { case: undefined };
    section.entries = section.entries.filter((entry) => entry.entry.case === "subsection");
    for (const entry of section.entries) {
      if (entry.entry.case !== "subsection") continue;
      const sub = entry.entry.value;
      sub.openRows = undefined;
      if (sub.fold.case === "expanded") sub.fold.value.collapse = { case: undefined };
    }
  }
  const sameShape = shape(a).join("\n") === shape(b).join("\n");
  return sameShape && equals(RosterRepoSectionSchema, left, right);
}

/**
 * Paint on a drawn repository section ELEMENT what the wire U changes on the
 * boxes a row patch keeps: on the section and each of its subsections,
 * whether it holds the current workspace, and on each subsection's name the
 * breath its working rows give it (`paintSubsectionWork`). A drawn section
 * whose subsections differ from U's is a broken invariant: a section whose
 * subsections changed is drawn anew, never patched.
 */
export function paintRepoSection(element: HTMLElement, u: RosterRepoSection, path: string): void {
  paintHoldsCurrent(element, drawRosterHoldsCurrent(u.fold, `${path}.fold`));
  const drawn = queryAll<HTMLElement>(element, `.${SUBSECTION_CLASS}`);
  const wire = sectionSubsections(u, path);
  if (drawn.length !== wire.length) {
    throw new Error(`${path}: ${drawn.length} subsections drawn for ${wire.length} on the wire`);
  }
  wire.forEach(({ subsection: sub, path: subPath }, index) => {
    requireDrawnBucket(drawn[index], subsectionBucket(sub, subPath), subPath);
    paintHoldsCurrent(drawn[index], drawRosterHoldsCurrent(sub.fold, `${subPath}.fold`));
    const label = drawn[index].querySelector<HTMLElement>(`:scope > .${SUBSECTION_HEAD_CLASS} > .${SUBSECTION_LABEL_CLASS}`);
    if (label === null) throw new Error(`invariant: ${subPath}: a drawn subsection has its name`);
    paintSubsectionWork(label, sub, subPath);
  });
}

/**
 * The refs a drag names, read from SECTIONS, the repository sections drawn.
 * A drag holds the drawing (`arrange.ts`), so the sections drawn are the ones
 * the press found its rows in.
 */
function drawnRefs(sections: () => Iterable<DrawnSection<RosterRepoSection, DrawnRepoSection>>): ArrangeRefs {
  return {
    workspace(id: string): WorkspaceRef {
      for (const { wire } of sections()) {
        for (const open of sectionOpenRows(wire, "WorkspaceRoster.repository.sections[]")) {
          const workspace = open.row.row?.workspace?.workspace;
          if (workspace?.id === id) return workspace;
        }
      }
      throw new Error(`invariant: a dragged row's workspace ${id} is in the roster drawn`);
    },
    repository(id: string): RepositoryRef {
      for (const { wire } of sections()) {
        const repository = wire.key?.repository;
        if (repository?.id === id) return repository;
      }
      throw new Error(`invariant: a dragged section's repository ${id} is in the roster drawn`);
    },
  };
}

/** The open workspaces' drawn repository sections and the count of the rows they list, as drawn (`openCountText`). */
export interface DrawnGroupingView {
  body: HTMLElement;
  openCount: string;
}

/**
 * The selected workspace, by identity.
 *
 * Validated and returned for the log record; the rail never joins on it,
 * because the row already carries its own highlight.
 */
export function drawRosterCurrentWorkspace(u: RosterCurrentWorkspace, path: string): string {
  return requireMessage(u.workspace, `${path}.workspace`).id;
}

/** The class of the open-workspaces section (`band-fit.ts` fits it), and of its count. */
export { OPEN_SECTION_CLASS };
export const OPEN_COUNT_CLASS = "sb-open-count";

/**
 * The open-workspaces section: the "Open" heading over the repository
 * sections, drawn like the Hidden, Merge Queue and Recently Merged headings,
 * with "(U/N)" beside the label whether the section is folded or not (owner
 * requests, 2026-10-13 and 2026-11-19). N is the daemon's sum of the
 * sections' counts, so it is what a person counting the section's entries
 * arrives at, a folded section's entries included, and U the sum of the
 * rows no fold hides (`open-count.ts`).
 *
 * Its fold is the daemon's, like every section's.
 */
export function drawRosterOpenSection(
  u: RosterOpenSection,
  view: DrawnGroupingView,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const section = daemonFoldedBox(OPEN_FOLD_KEY, drawRosterSectionFold(u.fold, `${path}.fold`), sc);
  section.classList.add(OPEN_SECTION_CLASS);
  const header = drawFoldHeader(
    drawSectionLabel(requireMessage(u.label, `${path}.label`), `${path}.label`),
    section,
    sectionFold(sc, OPEN_FOLD_KEY, { case: "open", value: {} }),
  );
  const count = document.createElement("span");
  count.className = OPEN_COUNT_CLASS;
  paintOpenCountText(count, view.openCount);
  header.appendChild(count);
  section.appendChild(header);
  section.appendChild(view.body);
  return section;
}

function paintOpenCountText(count: HTMLElement, openCount: string): void {
  count.textContent = openCount;
}

/** Repaint the open SECTION's heading count to OPEN_COUNT, as drawn, in place. */
export function paintOpenCount(section: HTMLElement, openCount: string): void {
  const count = section.querySelector<HTMLElement>(`.${OPEN_COUNT_CLASS}`);
  if (count === null) throw new Error("sidebar roster: the open section draws no heading count");
  paintOpenCountText(count, openCount);
}

/**
 * How a repository section's rows are drawn: OWN draws its rows in no
 * subsection (region 0) as elements, in order, for the section to stand its
 * subsections among; SUBSECTION draws one subsection's rows box (REGION is 1
 * + the subsection's index).
 */
export interface DrawRepoRows {
  own: (rows: readonly PathedOpenRow[]) => HTMLElement[];
  subsection: (rows: RosterOpenRows, path: string, region: number) => HTMLElement;
}

/**
 * One repository's section: its key, its header, and its body, ONE LIST of
 * its entries in drawn order (`entries.ts`): its rows in no subsection and
 * its subsections (`subsections.ts`), interleaved as arranged, each rows
 * region drawn by DRAW_ROWS (`drawRosterRows` under the section's scope
 * unless said).
 */
export function drawRosterRepoSection(
  u: RosterRepoSection,
  sc: SidebarContext,
  path: string,
  drawRows: DrawRepoRows = {
    own: (rows) => rows.map((open) => drawRosterFamily(open.row, sc, open.path, "grouping")),
    subsection: (r, p) => drawRosterRows(r, sc, p, "grouping"),
  },
): HTMLElement {
  const key = drawRosterRepoKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  const divergence = drawRosterRepoDivergence(
    requireMessage(u.divergence, `${path}.divergence`),
    `${path}.divergence`,
  );
  // THE FOLD IS THE DAEMON'S: the Emacs tab bar hides a collapsed
  // repository's workspaces off the same pushed arm, so the two can never
  // disagree.
  const section = drawHeadedBox(
    u,
    sc,
    path,
    {
      foldKey: repoFoldKey(key.id),
      className: "repo-section",
      target: { case: "repository", value: key },
      decorateHeader: (header) => {
        headerLabel(header).title = divergence;
        header.appendChild(drawCreateSubsectionControl(key, sc));
      },
    },
    () => {
      const body = document.createElement("div");
      body.className = "rows";
      stampBucket(body, { case: "none" });
      const own = drawRows.own(unsectionedRows(u, path));
      let rows = 0;
      let subsections = 0;
      for (const entry of repoEntries(u, path)) {
        if (entry.case === "row") {
          body.appendChild(own[rows++]);
          continue;
        }
        const region = 1 + subsections++;
        body.appendChild(
          drawRosterSubsection(entry.subsection, sc, entry.path, (r, p) => drawRows.subsection(r, p, region)).element,
        );
      }
      return body;
    },
  );
  section.setAttribute(REPOSITORY_ATTRIBUTE, key.id);
  paintRepoSection(section, u, path);
  return section;
}

/**
 * The repository label's hover: its primary branch against its upstream, as
 * the daemon last measured it. EVERY arm is named; unset is malformed.
 */
export function drawRosterRepoDivergence(u: RosterRepoDivergence, path: string): string {
  const arm = requireCase(u.state, `${path}.state`);
  switch (arm.case) {
    case "pending":
      return "ahead/behind not measured yet";
    case "counted":
      return `${arm.value.branch}: ${arm.value.ahead} ahead, ${arm.value.behind} behind ${arm.value.upstream}`;
    case "noUpstream":
      return `${arm.value.branch} tracks no upstream`;
    case "failed":
      return `ahead/behind unknown: ${arm.value.reason}`;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.state`, other.case);
    }
  }
}

/** The label a section header draws (`drawRosterSectionHeader`). */
function headerLabel(header: HTMLElement): HTMLElement {
  const label = header.querySelector<HTMLElement>(`:scope > .${SECTION_LABEL_CLASS}`);
  if (label === null) {
    throw new Error("a section header was drawn without its label");
  }
  return label;
}

/** What every headed section shares on the wire: its header and its fold. */
/**
 * A headed section's header: a section's outside the Open section, or a
 * repository section's inside it, whose count is an Open-section count.
 */
export type SectionHeader = RosterSectionHeader | RosterOpenHeader;

export interface HeadedBoxWire<H extends SectionHeader = SectionHeader> {
  header?: H;
  fold: RosterSectionFold;
  /** A band section's height (section-resize.ts); only the bands carry one. */
  height?: RosterSectionHeight;
}

/** How one kind of headed section is identified and decorated. */
export interface HeadedBoxShape<H extends SectionHeader = SectionHeader> {
  /** The page key its fold is held under (`view.ts`). */
  foldKey: string;
  /** The class naming the section's kind. */
  className: string;
  /** The section UpdateSidebarView folds. */
  target: FoldTarget;
  /** Anything the kind adds to its header, after the folded count. */
  decorateHeader?: (header: HTMLElement, section: HTMLElement) => void;
  /**
   * The header carries NO count, and one on the wire is malformed: the Merge
   * Queue section's, whose queue groups count their own lines.
   */
  countless?: true;
  /**
   * A section the rail leaves out while it draws nothing: WHEN says so from the
   * wire, and ATTRIBUTE marks the undrawn box. Absent, the section always draws
   * (a repository with no rows still draws, so the grouping does not flicker).
   */
  leftOut?: { when: (header: H, path: string) => boolean; attribute: string };
  /**
   * A band section below the Open section, whose height the daemon holds and
   * a drag of its rule sets (section-resize.ts): its wire carries `height`.
   */
  resizable?: true;
}

/** How a rows-bodied headed section is identified, decorated and drawn. */
export interface HeadedSectionShape<H extends SectionHeader = SectionHeader> extends HeadedBoxShape<H> {
  /** Which kind of section the rows sit in (`RowPlace`); a grouping's unless said. */
  place?: RowPlace;
}

/**
 * A section under the shared `RosterSectionHeader`: the box folded as the
 * daemon says, the label-and-count header whose gesture asks for the other
 * fold, and ROWS (at ROWS_PATH) below it, each with its family.
 *
 * THE ONE SHAPE every folding section but the merged band
 * draws — repository, hidden workspaces — through
 * `drawHeadedBox`, which the merged band shares, so none can disagree with
 * another on how a header folds.
 */
export function drawHeadedSection<H extends SectionHeader>(
  u: HeadedBoxWire<H>,
  sc: SidebarContext,
  path: string,
  shape: HeadedSectionShape<H>,
  rows: { rows: readonly RowFamily[] },
  rowsPath: string,
): HTMLElement {
  const place = shape.place ?? "grouping";
  return drawHeadedBox(u, sc, path, shape, () => drawRosterRows(rows, sc, rowsPath, place));
}

/**
 * A section box under the shared `RosterSectionHeader`, its body drawn by
 * BODY below the header: folded as the daemon says, with the header's gesture
 * asking for the other fold.
 *
 * A LEFT-OUT SECTION IS VALIDATED WHOLE, then left undrawn: a malformed empty
 * section is still a contract breach, and skipping the checks would hide it
 * until the section next drew.
 */
export function drawHeadedBox<H extends SectionHeader>(
  u: HeadedBoxWire<H>,
  sc: SidebarContext,
  path: string,
  shape: HeadedBoxShape<H>,
  body: () => HTMLElement,
): HTMLElement {
  const section = daemonFoldedBox(shape.foldKey, drawRosterSectionFold(u.fold, `${path}.fold`), sc);
  section.classList.add(shape.className);
  const header = requireMessage(u.header, `${path}.header`);
  const head = drawRosterSectionHeader(
    header,
    section,
    sectionFold(sc, shape.foldKey, shape.target),
    `${path}.header`,
    shape.countless === true,
  );
  const drawnBody = body();
  if (shape.resizable === true) {
    if (u.height === undefined) throw new Error(`${path}: a band section's wire carries no height`);
    drawSectionHeight(section, shape.foldKey, drawRosterSectionHeight(u.height, `${path}.height`), sc);
  }
  if (shape.leftOut?.when(header, `${path}.header`) === true) {
    section.hidden = true;
    section.setAttribute(shape.leftOut.attribute, "true");
    return section;
  }
  shape.decorateHeader?.(head, section);
  section.appendChild(head);
  section.appendChild(drawnBody);
  return section;
}

/** A repository's stable identity — the fold key and the create echo token. */
export function drawRosterRepoKey(u: RosterRepoKey, path: string): RepositoryRef {
  return requireMessage(u.repository, `${path}.repository`);
}

/**
 * The Recently Merged section (owner request, 2026-11-01; it was the merges
 * of the Deploys section), drawn directly under the Merge Queue section and
 * above Hidden: ONE GROUP PER REPOSITORY with a merge, from the band's
 * SUMMARY (U) and the pages of rows this page has loaded beside it (PAGES,
 * `merged-pages.ts`): the stream carries no merged row.
 *
 * ITS HEADER'S COUNT, every merged workspace of every group, is drawn while
 * the section is folded or holds a group whose merges are not all loaded
 * (`MERGED_PARTLY_LOADED_ATTRIBUTE`), since the count is then the only
 * account of the merges not drawn. Each group's header carries its
 * repository's "(K today, last D ago)", with the commits today's merges
 * brought in on its hover, and the section's header carries the same
 * "(K today, last D ago)" over every group (owner request, 2026-11-06), all
 * from the summary, so they count the merges no page has loaded too. Its
 * label is always the purple of a merge in progress.
 *
 * A SECTION WITH NO GROUPS IS NOT DRAWN (frontend.v1.RosterMergedSection).
 *
 * Once the section is laid out, `fitBands` (band-fit.ts) sizes its scrolling
 * viewport against the other sections', the loaded merges of today claiming
 * room first.
 */
export function drawRosterMergedSection(
  u: RosterMergedSection,
  pages: MergedPages,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const facts = u.groups.map((group, index) => mergedGroupFacts(group, `${path}.groups[${index}]`));
  const loaded = u.groups.map((group, index) => pages.loaded(group, `${path}.groups[${index}]`));
  const groups = u.groups.map((group, index) =>
    drawRosterMergedRepoGroup(group, facts[index], loaded[index], sc, `${path}.groups[${index}]`),
  );
  const partly = facts.some((group, index) => loaded[index].rows < group.count);
  return drawGroupedSection(
    u,
    sc,
    path,
    {
      foldKey: MERGED_FOLD_KEY,
      className: MERGED_SECTION_CLASS,
      target: { case: "recentlyMerged", value: {} },
      decorateHeader: (header, section) => {
        header.appendChild(drawMergedTodayCount(mergedSectionFacts(facts), sc));
        section.toggleAttribute(MERGED_PARTLY_LOADED_ATTRIBUTE, partly);
      },
      resizable: true,
    },
    groups,
    true,
  );
}

/**
 * The attribute the Recently Merged section wears while a group's merges are
 * not all loaded: its header's count is then drawn though it is unfolded.
 */
export const MERGED_PARTLY_LOADED_ATTRIBUTE = "data-partly-loaded";

/** The attribute a section with no groups wears while the rail leaves it out. */
export const GROUPLESS_ATTRIBUTE = "data-groupless";

/**
 * A section of repository GROUPS under the shared header: the one shape the
 * Merge Queue and Recently Merged sections share, so the two draw alike.
 * ITS HEADER CARRIES A WIRE COUNT ONLY WHEN COUNTED (the Recently Merged
 * section's; DECORATE_HEADER may add the kind's own), and A SECTION WITH NO
 * GROUPS IS LEFT OUT of the rail (`GROUPLESS_ATTRIBUTE`).
 */
export function drawGroupedSection(
  u: HeadedBoxWire,
  sc: SidebarContext,
  path: string,
  shape: Pick<HeadedBoxShape, "foldKey" | "className" | "target" | "decorateHeader" | "resizable">,
  groups: HTMLElement[],
  counted = false,
): HTMLElement {
  return drawHeadedBox(
    u,
    sc,
    path,
    {
      ...shape,
      ...(counted ? {} : { countless: true as const }),
      leftOut: { when: () => groups.length === 0, attribute: GROUPLESS_ATTRIBUTE },
    },
    () => {
      const region = document.createElement("div");
      region.className = "rows";
      region.append(...groups);
      return region;
    },
  );
}

/** The class of the Merge Queue section. */
export { MERGE_QUEUE_SECTION_CLASS };

/**
 * The Merge Queue section (owner request, 2026-11-01; it was the queues of
 * the Deploys section), drawn directly under the Open section: ONE GROUP PER
 * REPOSITORY whose queue is active, each its queue (`merge-queue-view.ts`).
 *
 * ITS HEADER COUNTS THE MERGES in every repository's queue, "(N)" like the
 * Open section's (owner request, 2026-11-08), and its label is always the purple of a merge
 * in progress; a paused queue is drawn on its own group.
 *
 * A SECTION WITH NO GROUPS IS NOT DRAWN (frontend.v1.RosterMergeQueueSection).
 */
export function drawRosterMergeQueueSection(
  u: RosterMergeQueueSection,
  sc: SidebarContext,
  path: string,
  coldGated: ColdGated,
): HTMLElement {
  const groups = u.groups.map((group, index) => drawRosterMergeQueueRepoGroup(group, sc, `${path}.groups[${index}]`, coldGated));
  const total = u.groups.reduce(
    (sum, group, index) => sum + queueCount(requireMessage(group.queue, `${path}.groups[${index}].queue`)),
    0,
  );
  return drawGroupedSection(
    u,
    sc,
    path,
    {
      foldKey: MERGE_QUEUE_FOLD_KEY,
      className: MERGE_QUEUE_SECTION_CLASS,
      target: { case: "mergeQueue", value: {} },
      decorateHeader: (header) => header.appendChild(drawQueueCount(total)),
      resizable: true,
    },
    groups,
  );
}

/** The class of one repository's group inside either section, and of its header. */
export const MERGED_GROUP_CLASS = DEPLOYS_GROUP_CLASS;
export const MERGED_GROUP_HEAD_CLASS = "merged-group-head";

/** The class of a group's body: its queue or its merged rows, which a folded group hides. */
export const MERGED_GROUP_BODY_CLASS = "rows";

/** The attribute a merged row states its merged instant (epoch ms) on (`band-fit.ts` reads it). */
export { MERGED_AT_ATTRIBUTE };

/** One recent merge's facts a header counts: a summary's `RosterMergedRepoGroupRecentMerge`. */
export interface MergedRowFacts {
  /** When the merge landed (epoch ms). */
  atMs: number;
  /** The commits the merge brought in; undefined when the daemon has no count. */
  commits: number | undefined;
}

/** What a header says about merges: the last day's, and the latest's instant. */
export interface MergedSummaryFacts {
  /** Every merge of the day before the summary was resolved, most recent first. */
  recent: MergedRowFacts[];
  /** When the latest merge landed (epoch ms), or null when there is none. */
  latestMs: number | null;
}

/** A Recently Merged group's summary facts, with how many merges it holds. */
export interface MergedGroupFacts extends MergedSummaryFacts {
  /** Every merged workspace of the group, loaded or not. */
  count: number;
  latestMs: number;
}

/**
 * A repository's group box in SECTION, keyed by KEY, under HEAD, with BODY
 * below it: the one shape the Merge Queue and Recently Merged sections'
 * groups share, so the two draw their repositories alike. A click on the
 * header folds or opens the group (`deploys-fold.ts`).
 */
export function drawRepositoryGroup(
  section: DeploysGroupSection,
  key: RepositoryRef,
  head: HTMLElement,
  body: HTMLElement,
  sc: SidebarContext,
): HTMLElement {
  const group = document.createElement("div");
  group.className = MERGED_GROUP_CLASS;
  group.setAttribute(DEPLOYS_SECTION_ATTRIBUTE, section);
  group.setAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE, key.id);
  body.classList.add(MERGED_GROUP_BODY_CLASS);
  group.append(head, body);
  head.addEventListener("click", (event) => {
    // A HEADER DRAWING A DEPLOY CHECK STATUS IS CLICKABLE ONLY ON ITS TEXTS
    // (owner request, 2026-11-16): a click on the bare header, which is its
    // rules and gaps, folds nothing.
    if (head.classList.contains(QUEUE_STATUS_HEAD_CLASS) && event.target === head) return;
    sc.deploysFold.toggle(group);
  });
  return group;
}

/** The class a queue group's header wears while it draws a deploy check status. */
export const QUEUE_STATUS_HEAD_CLASS = "sb-mq-status-head";

/** The class of the short rule after a queue group's deploy check status. */
export const QUEUE_STATUS_TAIL_RULE_CLASS = "sb-mq-status-tail";

/** The short rule a queue group's header draws after its deploy check status. */
function drawQueueStatusTailRule(): HTMLElement {
  return decorativeElement("span", QUEUE_STATUS_TAIL_RULE_CLASS);
}

/** A Merge Queue heading's "(N)": the merges queued, drawn as the Open section's count is. */
export function drawQueueCount(count: number): HTMLElement {
  const span = document.createElement("span");
  span.className = OPEN_COUNT_CLASS;
  span.textContent = `(${count})`;
  return span;
}

/**
 * One repository's group inside the Merge Queue section: its header, then
 * its queue. The group states its queue's line (`DEPLOYS_LINE_ATTRIBUTE`),
 * so a change to the line drops the hand's fold. A double click on the
 * header, or a click on its queue's merge glyph, pauses a running queue or
 * resumes a paused one (`merge-queue-view.ts`).
 */
export function drawRosterMergeQueueRepoGroup(u: RosterMergeQueueRepoGroup, sc: SidebarContext, path: string, coldGated: ColdGated): HTMLElement {
  const key = drawRosterRepoKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  const queuePath = `${path}.queue`;
  const queue = requireMessage(u.queue, queuePath);
  const header = requireMessage(u.header, `${path}.header`);
  const head = drawRosterMergedRepoGroupHeader(header, `${path}.header`);
  const name = requireMessage(header.label, `${path}.header.label`).text;
  const group = drawRepositoryGroup("merge-queue", key, head, drawRosterMergeQueue(queue, { key, name }, sc, queuePath, coldGated), sc);
  group.setAttribute(DEPLOYS_LINE_ATTRIBUTE, queueLine(queue));
  head.appendChild(drawQueueCount(queueCount(queue)));
  // THE DEPLOY CHECK'S STATUS STANDS AGAINST THE RIGHT MARGIN (owner request,
  // 2026-11-16): `-- <repo> (count) ------ <status> --`, after the long rule
  // and before a short one of its own.
  if (queue.mediation !== undefined) {
    head.classList.add(QUEUE_STATUS_HEAD_CLASS);
    head.appendChild(drawQueueMediationLabel(queue.mediation, sc, `${queuePath}.mediation`));
    if (queue.mediation.standing.case === "escalated") head.appendChild(drawEscalationRelease(key, sc));
    head.appendChild(drawQueueStatusTailRule());
  }
  drawQueuedGroupHead(group, head, queue, key, sc, queuePath);
  tickPaint(sc, (nowMs) => {
    head.title = queueHoverText(queue, nowMs, queuePath);
  });
  sc.deploysFold.paint(group);
  return group;
}

/**
 * What a Recently Merged group U at PATH says about ALL its merges, loaded or
 * not: its summary's count, latest instant and the last day's merges.
 */
export function mergedGroupFacts(u: RosterMergedRepoGroup, path: string): MergedGroupFacts {
  const summaryPath = `${path}.summary`;
  const summary: RosterMergedRepoGroupSummary = requireMessage(u.summary, summaryPath);
  return {
    count: requireMessage(summary.count, `${summaryPath}.count`).merges,
    latestMs: msOf(requireMessage(summary.latest, `${summaryPath}.latest`).mergedAtMs, `${summaryPath}.latest.merged_at_ms`),
    recent: summary.recent.map((merge, i) => ({
      atMs: msOf(merge.mergedAtMs, `${summaryPath}.recent[${i}].merged_at_ms`),
      commits: merge.commits,
    })),
  };
}

/** The section header's facts: every group's last day of merges together, and the latest of their latest. */
export function mergedSectionFacts(groups: readonly MergedGroupFacts[]): MergedSummaryFacts {
  return {
    recent: groups.flatMap((group) => group.recent),
    latestMs: groups.length === 0 ? null : Math.max(...groups.map((group) => group.latestMs)),
  };
}

/**
 * One repository's group inside the Recently Merged section: its header,
 * from its summary FACTS, then the rows of its LOADED pages, most recently
 * merged first: page 0's in the group's body, each later page in its own box
 * (`MERGED_PAGE_CLASS`) so it can be let go as a whole once it lies below
 * the viewport, and the loading spinner at the end while more pages exist
 * (`merged-pages.ts`).
 *
 * Each row carries its merged instant (`data-merged-at`), the one the row
 * already draws.
 */
export function drawRosterMergedRepoGroup(
  u: RosterMergedRepoGroup,
  facts: MergedGroupFacts,
  loaded: MergedGroupPages,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const key = drawRosterRepoKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  const head = drawRosterMergedRepoGroupHeader(requireMessage(u.header, `${path}.header`), `${path}.header`);
  head.appendChild(drawMergedTodayCount(facts, sc));
  tickGroupHover(head, facts.recent, sc);
  const body = document.createElement("div");
  const group = drawRepositoryGroup("recently-merged", key, head, body, sc);
  loaded.pages.forEach((rows, page) => {
    const into = page === 0 ? body : document.createElement("div");
    if (page > 0) {
      into.className = MERGED_PAGE_CLASS;
      body.appendChild(into);
    }
    rows.forEach((merged, i) => {
      const rowPath = `${path}.pages[${page}].rows[${i}]`;
      const drawn = drawRosterFamily(merged, sc, rowPath, "merged");
      drawn.setAttribute(MERGED_AT_ATTRIBUTE, String(mergedRowAtMs(merged, rowPath)));
      into.appendChild(drawn);
    });
  });
  if (loaded.more) body.appendChild(drawMergedMoreSpinner());
  sc.deploysFold.paint(group);
  return group;
}

/** A group's header: the repository's label, which a click folds or opens. */
export function drawRosterMergedRepoGroupHeader(u: RosterMergedRepoGroupHeader, path: string): HTMLElement {
  const head = document.createElement("div");
  head.className = MERGED_GROUP_HEAD_CLASS;
  head.appendChild(drawSectionLabel(requireMessage(u.label, `${path}.label`), `${path}.label`));
  return head;
}

/** The class of a group header's today count. */
export const MERGED_TODAY_COUNT_CLASS = "sb-today-count";

/**
 * A group header's, or the Recently Merged section header's (every group's
 * merges together), "(K today, last D ago)": how many of its merges landed
 * TODAY (local date), counted over the summary's last day of merges, loaded
 * or not, and how long ago the latest landed (`mergedTodayCountText`). The
 * shared ticker repaints it, so the age counts up and the count drops at
 * midnight together with the green names, with no push.
 */
export function drawMergedTodayCount(facts: MergedSummaryFacts, sc: SidebarContext): HTMLElement {
  const count = document.createElement("span");
  count.className = `sb-count ${MERGED_TODAY_COUNT_CLASS}`;
  tickPaint(sc, (nowMs) => {
    count.textContent = mergedTodayCountText(facts, nowMs);
  });
  return count;
}

/**
 * The today count's words for FACTS at NOW_MS: "(K today, last D ago)",
 * K counting the recent merges of NOW_MS's local date and D the latest
 * merge's age in at most two units (`formatAge`: `1h 5m`, `5m 30s`; owner
 * request, 2026-10-27), or "(K today)" while there is no merge at all.
 */
export function mergedTodayCountText(facts: MergedSummaryFacts, nowMs: number): string {
  const today = todaysMerges(facts.recent, nowMs).length;
  if (facts.latestMs === null) return `(${today} today)`;
  return `(${today} today, last ${formatAge(nowMs - facts.latestMs)} ago)`;
}

/**
 * The merges among MERGES that landed today (local date): the ONE filter the
 * heading's "(K today)" and its commit hover both count over, so the hover
 * always sums exactly the merges the count names.
 */
export function todaysMerges(merges: MergedRowFacts[], nowMs: number): MergedRowFacts[] {
  return merges.filter((m) => mergedToday(m.atMs, nowMs));
}

/**
 * Paint a Recently Merged group HEADER's hover now and on every tick: the
 * commits today's merges brought in, summed (owner request, 2026-10-14). The
 * ticker drops a merge from the sum at midnight.
 */
function tickGroupHover(header: HTMLElement, merges: MergedRowFacts[], sc: SidebarContext): void {
  tickPaint(sc, (nowMs) => {
    header.title = mergedTodayCommitsText(todaysMerges(merges, nowMs));
  });
}

/**
 * The hover's words for TODAY's merges: their commits summed. A merge the
 * daemon sent no count for is named as uncounted rather than summed as zero,
 * so the total never claims more certainty than the wire carries.
 */
export function mergedTodayCommitsText(today: MergedRowFacts[]): string {
  let commits = 0;
  let uncounted = 0;
  for (const m of today) {
    if (m.commits === undefined) uncounted += 1;
    else commits += m.commits;
  }
  const summed = `${countNoun(commits, "commit")} merged today`;
  if (uncounted === 0) return summed;
  return `${summed} (${countNoun(uncounted, "merge")} not counted)`;
}

/**
 * The hidden-workspaces section: the workspaces the user hid, hoisted out of
 * every grouping. They keep their sessions and their editor tabs; the Emacs
 * tab bar does not draw them.
 *
 * A SECTION THAT DRAWS NO ROW IS NOT DRAWN. The wire always carries it, so
 * the emptiness is answered here, off the daemon's own count of the rows it
 * shows (closed rows ride the wire but are never drawn): a heading over
 * nothing is a fold toggle onto nothing.
 *
 * ITS ROWS SCROLL once the rail is out of room: `fitBands` (band-fit.ts)
 * sizes its viewport against the Open section's and the Recently Merged
 * section's.
 *
 * CLICKING A ROW SHOWS THE WORKSPACE AGAIN (`SetWorkspaceHidden`), rather
 * than selecting it: the push that follows moves it back into its groupings
 * and onto the tab bar.
 */
export function drawRosterHiddenSection(
  u: RosterHiddenSection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  return drawHeadedSection(
    u,
    sc,
    path,
    {
      foldKey: HIDDEN_FOLD_KEY,
      className: HIDDEN_SECTION_CLASS,
      target: { case: "hidden", value: {} },
      place: "hidden",
      resizable: true,
      leftOut: {
        when: (header, headerPath) =>
          drawRosterSectionCount(requireMessage(header.count, `${headerPath}.count`), `${headerPath}.count`) === 0,
        attribute: "data-hidden-empty",
      },
    },
    requireMessage(u.hiddenRows, `${path}.hidden_rows`),
    `${path}.hidden_rows`,
  );
}

/** The header repos and the Recently Merged section share: a label, the header itself folding. */
export function drawRosterSectionHeader(
  u: SectionHeader,
  section: HTMLElement,
  gesture: FoldGesture,
  path: string,
  countless = false,
): HTMLElement {
  const header = drawFoldHeader(
    drawSectionLabel(requireMessage(u.label, `${path}.label`), `${path}.label`),
    section,
    gesture,
  );
  if (countless) {
    if (u.count !== undefined) throw new MalformedView(`${path}.count`, "this section's header carries no count");
    return header;
  }
  // THE FOLDED COUNT sits between the label and the add control the repo
  // section appends after this header. The daemon resolves it (the rows the
  // unfolded section shows, nested family rows included); the stylesheet
  // shows the count under the section's `.folded` only, and nothing here
  // counts or hides it.
  const count = document.createElement("span");
  count.className = "sb-count";
  count.setAttribute("data-section-count", "");
  count.textContent = sectionCountText(u, path);
  header.appendChild(count);
  return header;
}

/**
 * THE ONE `repo-head` every folding section box draws: LABEL, the header
 * itself folding SECTION through GESTURE on a click (`foldOnClick`; owner
 * request, 2026-10-31: no fold triangle anywhere in the sidebar). A
 * repository's header is also the grip it is dragged by, and a drag is no
 * click. Whatever a kind of section adds (a count, a control) is appended
 * after.
 */
export function drawFoldHeader(label: HTMLElement, section: HTMLElement, gesture: FoldGesture): HTMLElement {
  const header = document.createElement("div");
  header.className = "repo-head";
  header.appendChild(label);
  foldOnClick(header, section, gesture);
  return header;
}

/**
 * The count U, the header at PATH, draws: an Open-section count as every
 * count in the Open section is spelled, "(U/N)" (`open-count.ts`), and any
 * other section's as "(N)".
 */
function sectionCountText(u: SectionHeader, path: string): string {
  if (u.$typeName === "frontend.v1.RosterOpenHeader") {
    return openCountText(drawRosterOpenCount(requireMessage(u.count, `${path}.count`), `${path}.count`));
  }
  return `(${drawRosterSectionCount(requireMessage(u.count, `${path}.count`), `${path}.count`)})`;
}

/** The section's workspace count, exactly as the daemon resolved it. */
export function drawRosterSectionCount(u: RosterSectionCount, path: string): number {
  void path;
  return u.workspaces;
}

/** The class every section header's label is drawn with. */
export const SECTION_LABEL_CLASS = "sb-label";

/**
 * A section header's label box: the label's text in the `sb-label` span every
 * section header (and every Recently Merged group header) draws it in.
 */
export function drawSectionLabel(u: RosterLabel, path: string): HTMLElement {
  const label = document.createElement("span");
  label.className = SECTION_LABEL_CLASS;
  label.textContent = drawRosterLabel(u, path);
  return label;
}

/** A section's display label. DISPLAY ONLY — the key beside it is identity. */
export function drawRosterLabel(u: RosterLabel, path: string): string {
  void path;
  return u.text;
}

/**
 * The rows region of a section at PATH, in the resolver's render order, each
 * row with its family. The daemon lists each state on its own (open, hidden,
 * merged), so every row it sends is drawn: a closed workspace is in none of
 * these lists, and an open workspace whose parent is not open already stands
 * in its parent's place. `place` is which kind of section the rows sit in
 * (`RowPlace`).
 */
export function drawRosterRows(
  u: { rows: readonly RowFamily[] },
  sc: SidebarContext,
  path: string,
  place: RowPlace = "grouping",
): HTMLElement {
  const rows = document.createElement("div");
  rows.className = "rows";
  for (const entry of pathedFamilies(u.rows, `${path}.rows`)) {
    rows.appendChild(drawRosterFamily(entry.family, sc, entry.path, place));
  }
  return rows;
}

/**
 * The box a section is drawn in: folded as the wire says, unless this page's
 * own ask is still in flight (`view.ts`).
 */
function daemonFoldedBox(foldKey: string, wireFolded: boolean, sc: SidebarContext): HTMLElement {
  const section = foldedBox(foldKey, false);
  trackDaemonFold(section, foldKey, wireFolded, sc);
  return section;
}

/** The box a section is drawn in, folded as FOLDED says. */
function foldedBox(foldKey: string, folded: boolean): HTMLElement {
  const section = document.createElement("div");
  section.className = "repo";
  section.setAttribute("data-section", foldKey);
  if (folded) section.classList.add(FOLDED_CLASS);
  return section;
}



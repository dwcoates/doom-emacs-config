/**
 * roster — the rail: the open workspaces by repository, the Deploys
 * section, and the hidden workspaces, one copy of each (owner ruling,
 * 2026-10-15). Nothing is re-grouped, re-sorted or counted here.
 *
 * THE DEPLOYS SECTION has no key of its own (the roster carries exactly
 * one), so its fold identity is fixed. It draws every repository's merge
 * queue (`merge-queue-view.ts`), and below them its recently merged
 * workspaces in one group per repository, a merged group following its
 * REPOSITORY'S fold: a collapsed repository's group is not drawn here.
 *
 * THE VIEW IS THE DAEMON'S. Every section's fold is drawn from the push,
 * painted at once on a gesture and asked of the daemon (`view-change.ts`), so
 * the sidebar looks the same in every workspace's page.
 *
 * `WorkspaceRoster.current` IS NOT COMPARED HERE. The row states its own
 * highlight through `RosterRowCurrent`, deliberately, so no client can compute
 * it differently. The field is validated and logged, and that is all.
 */
import {
  BAND_FLOOR,
  HIDDEN_SECTION_CLASS,
  MERGED_AT_ATTRIBUTE,
  MERGED_SECTION_CLASS,
  OPEN_REGION_CLASS,
  OPEN_SECTION_CLASS,
} from "./band-fit.js";
import {
  RosterHiddenSectionSchema,
  RosterMergedSectionSchema,
  RosterOpenSectionSchema,
  RosterRepoSectionSchema,
  type RosterCurrentWorkspace,
  type RosterHiddenSection,
  type RosterLabel,
  type RosterMergedRepoGroup,
  type RosterMergedRepoGroupHeader,
  type RosterMergedSection,
  type RosterOpenCount,
  type RosterOpenSection,
  type RosterRepoDivergence,
  type RosterRepoKey,
  type RosterRepoSection,
  type RosterOpenRows,
  type RosterSectionCount,
  type RosterSectionHeader,
  type WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { RepositoryRef, WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { clone, equals } from "@bufbuild/protobuf";
import { placeChildren } from "./children.js";
import { queryAll } from "../dom.js";
import { disposeKeyedRows, drawKeyedEntries, drawKeyedRows, patchKeyedRegions, remarkKeyedRows, type KeyedRows } from "./keyed-rows.js";
import { repoEntries, repoSectionRegions, sectionOpenRows, sectionSubsections, unsectionedRows, type PathedOpenRow } from "./entries.js";
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { MalformedView } from "../rpc/malformed.js";
import {
  HIDDEN_FOLD_KEY,
  MERGED_FOLD_KEY,
  OPEN_FOLD_KEY,
  repoFoldKey,
  type RailContext,
  type SidebarContext,
} from "./context.js";
import { REARRANGED_ATTRIBUTE } from "./drag.js";
import {
  drawCreateSubsectionControl,
  drawRosterSubsection,
  installArrangeDrag,
  REPOSITORY_ATTRIBUTE,
  requireDrawnBucket,
  stampBucket,
  SUBSECTION_CLASS,
  subsectionBucket,
  type ArrangeRefs,
} from "./subsections.js";
import {
  drawFoldToggle,
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
import { RECENTLY_DEPLOYED_LABEL_CLASS } from "./recently-deployed.js";
import { drawRosterMergeQueueRepoGroup, MERGE_QUEUES_CLASS } from "./merge-queue-view.js";
import { foldViewKey } from "./view.js";
import {
  drawRosterRowDetailCommits,
  drawRosterRowWhenMerged,
  drawRosterRowWhenShown,
  drawRosterFamily,
  type RowFamily,
  pathedFamilies,
  mergedToday,
  tickPaint,
  type RowPlace,
} from "./row.js";
import { countNoun } from "../count-noun.js";
import { formatAge } from "../duration.js";
import { paintTabNumbers, tabNumbers } from "./tab-numbers.js";

/**
 * The whole roster, drawn through RAIL's one roster drawer (`createRosterDrawer`):
 * a section the push leaves unchanged is the one already drawn.
 */
export function drawWorkspaceRoster(u: WorkspaceRoster, rail: RailContext): HTMLElement {
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
  draw(u: WorkspaceRoster): HTMLElement;
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
 * hidden band, the Deploys section, the open section's heading — whose
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
  // by repository, then the Deploys section, then the hidden workspaces
  // (owner request, 2026-10-27: Deploys moved above Hidden).
  // The pane is the box band-fit.ts fits the three sections' viewports in.
  const pane = document.createElement("div");
  pane.className = "sb-pane";
  roster.appendChild(pane);
  let repos = new Map<string, DrawnSection<RosterRepoSection, DrawnRepoSection>>();
  // THE GROUPING'S DRAGS (subsections.ts) are found from the press, under one
  // listener for the drawer's life, and name the refs of the roster drawn.
  const dragScope = openScope(rail);
  const removeDrag = installArrangeDrag(dragScope.context, roster, drawnRefs(() => repos.values()));
  /** The open section, and the box its repository sections are placed in. */
  let open: DrawnSection<{ open: RosterOpenSection; count: number }, Drawn & { body: HTMLElement }> | null = null;
  let hidden: DrawnSection<RosterHiddenSection> | null = null;
  let merged: DrawnSection<RosterMergedSection> | null = null;

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

  return {
    draw(u: WorkspaceRoster): HTMLElement {
      const path = "WorkspaceRoster";
      const repository = requireMessage(u.repository, `${path}.repository`);
      const mergedWire = requireMessage(u.recentlyMerged, `${path}.recently_merged`);
      const hiddenWire = requireMessage(u.hidden, `${path}.hidden`);
      const openWire = requireMessage(u.open, `${path}.open`);
      const current =
        u.current === undefined
          ? null
          : drawRosterCurrentWorkspace(u.current, `${path}.current`);
      const openCount = drawRosterOpenCount(
        requireMessage(repository.openCount, `${path}.repository.open_count`),
        `${path}.repository.open_count`,
      );
      const discarded: Array<DrawnSection<unknown>> = [];

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
            paintRepoHoldsCurrent(kept.drawn.element, section, sectionPath);
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

      hidden = keepOrDraw(
        hidden,
        hiddenWire,
        (a, b) => equals(RosterHiddenSectionSchema, a, b),
        (scoped) => ({ element: drawRosterHiddenSection(hiddenWire, scoped, `${path}.hidden`) }),
        discarded,
      );
      merged = keepOrDraw(
        merged,
        mergedWire,
        (a, b) => equals(RosterMergedSectionSchema, a, b),
        (scoped) => ({ element: drawRosterMergedSection(mergedWire, scoped, `${path}.recently_merged`) }),
        discarded,
      );
      placeChildren(pane, [open.drawn.element, merged.drawn.element, hidden.drawn.element]);

      // Disposed only once every kept section has moved into the new drawing,
      // so a discarded box's teardown never reaches a kept section's rows.
      for (const section of discarded) section.scope.dispose(section.drawn.element);
      log.debug("drew the workspace roster", {
        operation: "sidebar.roster",
        context: {
          repo_sections: repository.sections.length,
          merged_groups: mergedWire.groups.length,
          hidden_rows: hiddenWire.hiddenRows?.rows.length ?? 0,
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
      for (const section of [open, hidden, merged]) if (section !== null) drawn.push(section);
      for (const section of drawn) section.scope.dispose(section.drawn.element);
      repos = new Map();
      open = hidden = merged = null;
      pane.replaceChildren();
    },
  };
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
    // (`paintRepoHoldsCurrent`), so it is no reason to redraw the section.
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
 * Say on a drawn repository section ELEMENT, and on each of its subsections,
 * whether the wire U says it holds the current workspace. A drawn section
 * whose subsections differ from U's is a broken invariant: a section whose
 * subsections changed is drawn anew, never patched.
 */
export function paintRepoHoldsCurrent(element: HTMLElement, u: RosterRepoSection, path: string): void {
  paintHoldsCurrent(element, drawRosterHoldsCurrent(u.fold, `${path}.fold`));
  const drawn = queryAll<HTMLElement>(element, `.${SUBSECTION_CLASS}`);
  const wire = sectionSubsections(u, path);
  if (drawn.length !== wire.length) {
    throw new Error(`${path}: ${drawn.length} subsections drawn for ${wire.length} on the wire`);
  }
  wire.forEach(({ subsection: sub, path: subPath }, index) => {
    requireDrawnBucket(drawn[index], subsectionBucket(sub, subPath), subPath);
    paintHoldsCurrent(drawn[index], drawRosterHoldsCurrent(sub.fold, `${subPath}.fold`));
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

/** The open workspaces' drawn repository sections and the count of the rows they list. */
export interface DrawnGroupingView {
  body: HTMLElement;
  openCount: number;
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

/** How many rows the open-workspaces section lists, exactly as the daemon resolved it. */
export function drawRosterOpenCount(u: RosterOpenCount, path: string): number {
  void path;
  return u.workspaces;
}

/** The class of the open-workspaces section (`band-fit.ts` fits it), and of its count. */
export { OPEN_SECTION_CLASS };
export const OPEN_COUNT_CLASS = "sb-open-count";

/**
 * The open-workspaces section: the "Open" heading over the repository
 * sections, drawn like the Hidden and Deploys headings, with "(N)"
 * beside the label whether the section is folded or not (owner request,
 * 2026-10-13). N is the daemon's sum of the sections' counts, so it is what a
 * person counting the section's entries arrives at, a folded section's
 * entries included.
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
    "header",
  );
  const count = document.createElement("span");
  count.className = OPEN_COUNT_CLASS;
  count.textContent = `(${view.openCount})`;
  header.appendChild(count);
  section.appendChild(header);
  section.appendChild(view.body);
  return section;
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
      foldBy: "triangle",
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
  paintRepoHoldsCurrent(section, u, path);
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
export interface HeadedBoxWire {
  header?: RosterSectionHeader;
  fold: RosterSectionFold;
}

/** How one kind of headed section is identified and decorated. */
export interface HeadedBoxShape {
  /** The page key its fold is held under (`view.ts`). */
  foldKey: string;
  /** The class naming the section's kind. */
  className: string;
  /** The section UpdateSidebarView folds. */
  target: FoldTarget;
  /** Where a click on its header asks for the other fold. */
  foldBy: FoldHitArea;
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
  leftOut?: { when: (header: RosterSectionHeader, path: string) => boolean; attribute: string };
}

/** How a rows-bodied headed section is identified, decorated and drawn. */
export interface HeadedSectionShape extends HeadedBoxShape {
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
export function drawHeadedSection(
  u: HeadedBoxWire,
  sc: SidebarContext,
  path: string,
  shape: HeadedSectionShape,
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
export function drawHeadedBox(
  u: HeadedBoxWire,
  sc: SidebarContext,
  path: string,
  shape: HeadedBoxShape,
  body: () => HTMLElement,
): HTMLElement {
  const section = daemonFoldedBox(shape.foldKey, drawRosterSectionFold(u.fold, `${path}.fold`), sc);
  section.classList.add(shape.className);
  const header = requireMessage(u.header, `${path}.header`);
  const head = drawRosterSectionHeader(
    header,
    section,
    sectionFold(sc, shape.foldKey, shape.target),
    shape.foldBy,
    `${path}.header`,
    shape.countless === true,
  );
  const drawnBody = body();
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
 * The Deploys section (owner request, 2026-10-26; renamed from Merge Queue
 * 2026-10-27), drawn directly under the Open section and above Hidden:
 * every repository's merge queue (`merge-queue-view.ts`), then, under a
 * "Recently Deployed" heading, the settled merges hoisted out of every
 * grouping, in one group per repository.
 *
 * ITS HEADER COUNTS NOTHING (owner request, 2026-10-27): each queue group
 * counts the merges in its own line, and the "Recently Deployed" heading
 * counts today's merges, "(K today)", with the commits they brought in on its
 * hover. Its label is always the purple of a merge in progress; a paused
 * queue is drawn on its own group (`merge-queue-view.ts`).
 *
 * THE SECTION ALWAYS DRAWS. Its queues stand directly under its header,
 * with no heading of their own (owner request, 2026-10-31; the "Deploy
 * Queue" heading is retired): with no deploy going on, "Recently Deployed"
 * stands first.
 *
 * EVERY MERGE THE DAEMON SENDS IS DRAWN (a week of them); once the section is
 * laid out, `fitBands` (band-fit.ts) sizes its scrolling viewport against the
 * Open section's and the Hidden Workspaces band's, its queues and its merges
 * of today claiming room first.
 */
export function drawRosterMergedSection(
  u: RosterMergedSection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const queues = u.queues.map((group, index) =>
    drawRosterMergeQueueRepoGroup(group, sc, `${path}.queues[${index}]`, (headerPath) =>
      drawRosterMergedRepoGroupHeader(requireMessage(group.header, headerPath), headerPath),
    ),
  );
  const mergedLabel = drawSectionLabel(requireMessage(u.mergedLabel, `${path}.merged_label`), `${path}.merged_label`);
  const drawnNow = { left: MERGED_DRAWN_AT_ONCE };
  const groups = u.groups.map((group, index) =>
    drawRosterMergedRepoGroup(group, sc, `${path}.groups[${index}]`, drawnNow),
  );
  const section = drawHeadedBox(
    u,
    sc,
    path,
    {
      foldKey: MERGED_FOLD_KEY,
      className: MERGED_SECTION_CLASS,
      target: { case: "recentlyMerged", value: {} },
      foldBy: "header",
      countless: true,
    },
    () => {
      const region = document.createElement("div");
      region.className = "rows";
      const box = document.createElement("div");
      box.className = MERGE_QUEUES_CLASS;
      box.append(...queues);
      region.appendChild(box);
      if (groups.length > 0) {
        const merges = groups.flatMap((g) => g.shownMerges);
        const heading = drawDeploysHeading(MERGED_LABEL_CLASS, mergedLabel, drawMergedTodayCount(merges, sc));
        tickMergedTodayCommits(heading, merges, sc);
        // THE ROWS ARE SHOWN ONLY WHILE ASKED FOR (recently-deployed.ts): a
        // click on the heading, which draws no chevron, asks the other.
        heading.addEventListener("click", () => sc.recentlyDeployed.toggle());
        sc.recentlyDeployed.paint(heading);
        region.appendChild(heading);
      }
      for (const group of groups) region.appendChild(group.element);
      return region;
    },
  );
  return section;
}

/**
 * The Deploys section's "Recently Deployed" heading: a CLASS_NAME box
 * holding its drawn LABEL and whatever follows it (its "(K today)").
 */
export function drawDeploysHeading(className: string, label: HTMLElement, ...after: HTMLElement[]): HTMLElement {
  const heading = document.createElement("div");
  heading.className = className;
  heading.append(label, ...after);
  return heading;
}

/**
 * The class of the "Recently Deployed" heading over the merged groups, below
 * the queues. The stylesheet hides it while every group under it is left out
 * (its repository collapsed).
 */
export const MERGED_LABEL_CLASS = RECENTLY_DEPLOYED_LABEL_CLASS;

/**
 * How many of the band's rows are drawn with the band; every later one is a
 * stand-in until it comes into view (`reveal.ts`, owner ruling 2026-10-15:
 * a row scrolled out of the band's viewport is not drawn). Twice the bands'
 * whole share of the rail, so a band opened or fitted shows drawn rows at once.
 */
export const MERGED_DRAWN_AT_ONCE = BAND_FLOOR * 4;

/** The class of one repository's group inside the band, and of its header. */
export const MERGED_GROUP_CLASS = "merged-group";
export const MERGED_GROUP_HEAD_CLASS = "merged-group-head";

/** The attribute a merged row states its merged instant (epoch ms) on (`band-fit.ts` reads it). */
export { MERGED_AT_ATTRIBUTE };

/** The attribute a group states whether its repository is collapsed on. */
export const MERGED_REPOSITORY_COLLAPSED_ATTRIBUTE = "data-repository-collapsed";

/** One merged row's facts the band's header counts. */
export interface MergedRowFacts {
  /** When the merge landed (epoch ms). */
  atMs: number;
  /** The commits the merge brought in; undefined when the daemon has no count. */
  commits: number | undefined;
}

/** One drawn merged group, and the facts of its rows while it is shown. */
export interface DrawnMergedRepoGroup {
  element: HTMLElement;
  /** Every row's facts when the wire shows the group; none when it does not. */
  shownMerges: MergedRowFacts[];
}

/**
 * One repository's merged workspaces inside the band: its label header and its
 * rows, most recently merged first.
 *
 * THE GROUP HAS NO FOLD OF ITS OWN. It is drawn while its repository is
 * expanded and left out while it is collapsed, and it is tracked under the
 * REPOSITORY SECTION'S fold key, so a fold gesture on the repository section
 * moves the group on the same paint, not a push later.
 *
 * Each row carries its merged instant (`data-merged-at`), the one the row
 * already draws. A merged row is always on the `when.merged` arm; one that is
 * not is malformed.
 */
export function drawRosterMergedRepoGroup(
  u: RosterMergedRepoGroup,
  sc: SidebarContext,
  path: string,
  drawnNow: { left: number },
): DrawnMergedRepoGroup {
  const key = drawRosterRepoKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  const rows = requireMessage(u.mergedRows, `${path}.merged_rows`);
  const merges = rows.rows.map((merged, i): MergedRowFacts => {
    const rowPath = `${path}.merged_rows.rows[${i}].row`;
    const row = requireMessage(merged.row, rowPath);
    const when = requireMessage(row.when, `${rowPath}.when`);
    const arm = requireCase(when.shown, `${rowPath}.when.shown`);
    if (arm.case !== "merged") return unreachableArm(`${rowPath}.when.shown`, arm.case);
    const commits = requireMessage(row.detail, `${rowPath}.detail`).commits;
    return {
      atMs: drawRosterRowWhenMerged(arm.value, `${rowPath}.when.merged`),
      commits: commits === undefined ? undefined : drawRosterRowDetailCommits(commits, `${rowPath}.detail.commits`),
    };
  });
  const wireCollapsed = drawRosterMergedRepoGroupDrawn(u.drawn, `${path}.drawn`);
  const group = document.createElement("div");
  group.className = MERGED_GROUP_CLASS;
  group.setAttribute("data-repository", key.id);
  group.appendChild(drawRosterMergedRepoGroupHeader(requireMessage(u.header, `${path}.header`), `${path}.header`));
  const region = document.createElement("div");
  region.className = "rows";
  rows.rows.forEach((row, i) => {
    const rowPath = `${path}.merged_rows.rows[${i}]`;
    const stamp = (drawn: HTMLElement): void => drawn.setAttribute(MERGED_AT_ATTRIBUTE, String(merges[i].atMs));
    if (drawnNow.left > 0) {
      drawnNow.left -= 1;
      const drawn = drawRosterFamily(row, sc, rowPath, "merged");
      stamp(drawn);
      region.appendChild(drawn);
      return;
    }
    drawRosterRowWhenShown(region, row, sc, rowPath, "merged", stamp);
  });
  group.appendChild(region);
  const collapsed = sc.view.track(foldViewKey(repoFoldKey(key.id)), wireCollapsed, (folded) =>
    paintMergedRepoGroup(group, folded),
  );
  paintMergedRepoGroup(group, collapsed);
  return { element: group, shownMerges: wireCollapsed ? [] : merges };
}

/** Whether a merged group's repository is collapsed. EVERY arm is named; unset is malformed. */
export function drawRosterMergedRepoGroupDrawn(drawn: RosterMergedRepoGroup["drawn"], path: string): boolean {
  const arm = requireCase(drawn, path);
  switch (arm.case) {
    case "shown":
      return false;
    case "repositoryCollapsed":
      return true;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(path, other.case);
    }
  }
}

/** A merged group's header: the repository's label, and nothing to toggle. */
export function drawRosterMergedRepoGroupHeader(u: RosterMergedRepoGroupHeader, path: string): HTMLElement {
  const head = document.createElement("div");
  head.className = MERGED_GROUP_HEAD_CLASS;
  head.appendChild(drawSectionLabel(requireMessage(u.label, `${path}.label`), `${path}.label`));
  return head;
}

/** Say on a merged group whether its repository is collapsed: a collapsed one is not drawn. */
export function paintMergedRepoGroup(group: HTMLElement, collapsed: boolean): void {
  group.hidden = collapsed;
  group.setAttribute(MERGED_REPOSITORY_COLLAPSED_ATTRIBUTE, collapsed ? "true" : "false");
}

/** The class of the "Recently Deployed" heading's today count. */
export const MERGED_TODAY_COUNT_CLASS = "sb-today-count";

/**
 * The "Recently Deployed" heading's "(K today)": how many of its merges landed
 * TODAY (local date), counted over every row of its SHOWN groups the daemon
 * sent, including those scrolled out of view (owner requests, 2026-10-11 and
 * 2026-10-27), and how long ago the latest of them landed
 * (`mergedTodayCountText`). The shared ticker repaints it, so the age counts
 * up and the count drops at midnight together with the green names.
 */
export function drawMergedTodayCount(merges: MergedRowFacts[], sc: SidebarContext): HTMLElement {
  const count = document.createElement("span");
  count.className = `sb-count ${MERGED_TODAY_COUNT_CLASS}`;
  tickPaint(sc, (nowMs) => {
    count.textContent = mergedTodayCountText(todaysMerges(merges, nowMs), nowMs);
  });
  return count;
}

/**
 * The today count's words for TODAY's merges at NOW_MS: "(K today)", and, once
 * any landed, "(K today, last D ago)" with D the latest one's age in at most
 * two units (`formatAge`: `1h 5m`, `5m 30s`; owner request, 2026-10-27).
 */
export function mergedTodayCountText(today: MergedRowFacts[], nowMs: number): string {
  if (today.length === 0) return "(0 today)";
  const last = Math.max(...today.map((m) => m.atMs));
  return `(${today.length} today, last ${formatAge(nowMs - last)} ago)`;
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
 * Paint the "Recently Deployed" heading's hover now and on every tick: the
 * commits today's merges brought in, summed (owner request, 2026-10-14). The
 * ticker drops a merge from the sum at midnight.
 */
function tickMergedTodayCommits(header: HTMLElement, merges: MergedRowFacts[], sc: SidebarContext): void {
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
 * sizes its viewport against the Open section's and the Deploys
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
      foldBy: "header",
      place: "hidden",
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

/** The header repos and the Deploys section share: a label and a fold toggle. */
export function drawRosterSectionHeader(
  u: RosterSectionHeader,
  section: HTMLElement,
  gesture: FoldGesture,
  hit: FoldHitArea,
  path: string,
  countless = false,
): HTMLElement {
  const header = drawFoldHeader(
    drawSectionLabel(requireMessage(u.label, `${path}.label`), `${path}.label`),
    section,
    gesture,
    hit,
  );
  if (countless) {
    if (u.count !== undefined) throw new MalformedView(`${path}.count`, "this section's header carries no count");
    return header;
  }
  // THE FOLDED COUNT, "(N)", sits between the label and the add control the
  // repo section appends after this header. The daemon resolves N (the rows
  // the unfolded section shows, nested family rows included); the stylesheet
  // shows the count under the section's `.folded` only, and nothing here
  // counts or hides it.
  const count = document.createElement("span");
  count.className = "sb-count";
  count.setAttribute("data-section-count", "");
  count.textContent = `(${drawRosterSectionCount(requireMessage(u.count, `${path}.count`), `${path}.count`)})`;
  header.appendChild(count);
  return header;
}

/**
 * Where a section header's click asks for the other fold: anywhere on the
 * header, or on its fold triangle alone.
 *
 * A REPOSITORY FOLDS FROM ITS TRIANGLE ALONE (owner request, 2026-10-20), as
 * its subsections do (`subsections.ts`): the rest of its line is the grip it
 * is dragged by, and a click there asks nothing.
 */
export type FoldHitArea = "header" | "triangle";

/**
 * THE ONE `repo-head` every folding section box draws: its fold triangle,
 * then LABEL, with a click on HIT asking GESTURE for the other fold.
 * Whatever a kind of section adds (a count, a control) is appended after.
 */
export function drawFoldHeader(label: HTMLElement, section: HTMLElement, gesture: FoldGesture, hit: FoldHitArea): HTMLElement {
  const header = document.createElement("div");
  header.className = "repo-head";
  header.setAttribute(FOLD_HIT_ATTRIBUTE, hit);
  header.appendChild(drawFoldToggle(section, gesture));
  header.appendChild(label);
  switch (hit) {
    case "header":
      header.addEventListener("click", () => gesture(section, header));
      break;
    case "triangle":
      break;
    default: {
      const other: string = hit;
      throw new Error(`drawFoldHeader: unknown fold hit area ${other}`);
    }
  }
  return header;
}

/** The attribute a section header carries naming its `FoldHitArea`, which the stylesheet's cursor reads. */
export const FOLD_HIT_ATTRIBUTE = "data-fold-hit";

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
  if (folded) section.classList.add("folded");
  return section;
}



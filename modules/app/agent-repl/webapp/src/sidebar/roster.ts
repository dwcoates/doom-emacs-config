/**
 * roster — the rail's groupings, their sections, and the recently-merged band
 * that sits under every one.
 *
 * EVERY GROUPING IS DRAWN, ONE IS SHOWN. The daemon resolves the repository,
 * task and maturity views as siblings, so choosing between them is a
 * SELECTION — exactly as folding hides rows — and never a derivation: this
 * file draws every pane and hides all but one. Nothing is re-grouped,
 * re-sorted or counted here: which stage a workspace is at is the daemon's
 * resolution, never this file's.
 *
 * RECENTLY MERGED APPEARS UNDER EVERY GROUPING, because the interesting fact
 * about those workspaces is that they are done, which is true in any grouping
 * (under the maturity grouping it IS the done stage). It has no key of its own
 * (the roster carries exactly one), so its fold identity is fixed. Its merges
 * sit in one group per repository, and a group follows its REPOSITORY'S fold:
 * the repository grouping is the one place a repository's workspaces are
 * collapsed, and a collapsed repository's group is not drawn here.
 *
 * THE VIEW IS THE DAEMON'S. Every section's fold and which grouping is shown
 * are drawn from the push, painted at once on a gesture and asked of the
 * daemon (`view-change.ts`), so the sidebar looks the same in every
 * workspace's page.
 *
 * `WorkspaceRoster.current` IS NOT COMPARED HERE. The row states its own
 * highlight through `RosterRowCurrent`, deliberately, so no client can compute
 * it differently. The field is validated and logged, and that is all.
 */
import { HIDDEN_SECTION_CLASS, MERGED_SECTION_CLASS } from "./band-fit.js";
import { createControl, type Control } from "../control.js";
import type {
  RosterCurrentWorkspace,
  RosterHiddenSection,
  RosterLabel,
  RosterMaturityKey,
  RosterMaturitySection,
  RosterMaturityView,
  RosterMergedRepoGroup,
  RosterMergedRepoGroupHeader,
  RosterMergedSection,
  RosterOpenCount,
  RosterOpenSection,
  RosterRepoDivergence,
  RosterRepoKey,
  RosterRepoSection,
  RosterRepositoryView,
  RosterRows,
  RosterSectionCount,
  RosterSectionHeader,
  RosterTaskDone,
  RosterTaskKey,
  RosterTaskSection,
  RosterTaskSectionHeader,
  RosterTaskView,
  WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  MaturityStageSchema,
  type MaturityStage,
  type RepositoryRef,
} from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { UpdateTaskResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_task_pb";
import { TaskRefSchema } from "../../../proto/gen/ts/agentrepl/v1/task_pb";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import type { SidebarViewFoldSectionSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import {
  HIDDEN_FOLD_KEY,
  MERGED_FOLD_KEY,
  OPEN_FOLD_KEY,
  maturityFoldKey,
  repoFoldKey,
  taskFoldKey,
  type Grouping,
  type MaturityStageName,
  type SidebarContext,
} from "./context.js";
import { drawCreateWorkspaceControl } from "./create.js";
import { toggleHiddenDropdown } from "./dropdowns.js";
import { changeView } from "./view-change.js";
import { foldViewKey, GROUPING_VIEW_KEY, paintSectionFold, paintTriangle } from "./view.js";
import {
  drawRosterRow,
  drawRosterRowDetailCommits,
  drawRosterRowWhenMerged,
  expandVisibleRows,
  mergedToday,
  tickPaint,
  type RowPlace,
} from "./row.js";
import {
  buildUpdateTaskRequest,
  drawCreateTaskControl,
  updateTaskRefusal,
  type TaskChange,
} from "./tasks.js";
import { fireVerb } from "./verbs.js";

/**
 * The whole roster.
 *
 * The task choices the assign menu offers are refreshed here, at the top of
 * the draw, from the task view's own sections — the one place the rail learns
 * which tasks exist.
 */
export function drawWorkspaceRoster(u: WorkspaceRoster, sc: SidebarContext): HTMLElement {
  const path = "WorkspaceRoster";
  const repository = requireMessage(u.repository, `${path}.repository`);
  const task = requireMessage(u.task, `${path}.task`);
  const maturity = requireMessage(u.maturity, `${path}.maturity`);
  const merged = requireMessage(u.recentlyMerged, `${path}.recently_merged`);
  const hidden = requireMessage(u.hidden, `${path}.hidden`);
  const open = requireMessage(u.open, `${path}.open`);
  const current =
    u.current === undefined
      ? null
      : drawRosterCurrentWorkspace(u.current, `${path}.current`);
  log.debug("drawing the workspace roster", {
    operation: "sidebar.roster",
    context: {
      repo_sections: repository.sections.length,
      task_sections: task.sections.length,
      maturity_sections: maturity.sections.length,
      merged_groups: merged.groups.length,
      hidden_rows: hidden.rows?.rows.length ?? 0,
      current,
      grouping: drawRosterShown(u.shown, `${path}.shown`),
    },
  });

  // The copies drawn from the last roster are about to be thrown away.
  sc.view.beginDraw();
  sc.dropdowns.beginDraw();
  sc.rowHover.beginDraw();
  sc.tasks.length = 0;
  for (const [index, section] of task.sections.entries()) {
    sc.tasks.push({
      id: requireMessage(section.key, `${path}.task.sections[${index}].key`).taskId,
      label: requireMessage(
        requireMessage(section.header, `${path}.task.sections[${index}].header`).label,
        `${path}.task.sections[${index}].header.label`,
      ).text,
    });
  }

  const roster = document.createElement("div");
  roster.className = "sb-roster";
  const bands: PaneBands = { open, hidden, merged, path };
  roster.appendChild(pane("repository", drawRosterRepositoryView(repository, sc, `${path}.repository`), sc, bands));
  roster.appendChild(pane("task", drawRosterTaskView(task, sc, `${path}.task`), sc, bands));
  roster.appendChild(pane("maturity", drawRosterMaturityView(maturity, sc, `${path}.maturity`), sc, bands));
  const shown = sc.view.track(GROUPING_VIEW_KEY, drawRosterShown(u.shown, `${path}.shown`), (grouping) =>
    showPane(roster, grouping),
  );
  showPane(roster, shown);
  return roster;
}

/** Which grouping the daemon says every page shows. EVERY arm is named; unset is malformed. */
export function drawRosterShown(shown: WorkspaceRoster["shown"], path: string): Grouping {
  const arm = requireCase(shown, path);
  switch (arm.case) {
    case "shownRepository":
      return "repository";
    case "shownTask":
      return "task";
    case "shownMaturity":
      return "maturity";
    default: {
      const other: { case: string } = arm;
      return unreachableArm(path, other.case);
    }
  }
}

/** Show GROUPING's pane under ROSTER and hide the others. */
export function showPane(roster: ParentNode, grouping: string): void {
  for (const pane of roster.querySelectorAll<HTMLElement>(":scope > [data-grouping]")) {
    pane.hidden = pane.getAttribute("data-grouping") !== grouping;
  }
}

/** The sections drawn around EVERY grouping, and the roster path they sit at. */
interface PaneBands {
  open: RosterOpenSection;
  hidden: RosterHiddenSection;
  merged: RosterMergedSection;
  path: string;
}

/** One grouping's drawn sections and the count of the rows they list. */
export interface DrawnGroupingView {
  body: HTMLElement;
  openCount: number;
}

/**
 * One grouping's pane: its sections inside its own copy of the open-workspaces
 * section, then its own copies of the sections every grouping draws below:
 * the hidden workspaces, then the recently-merged band.
 */
function pane(grouping: Grouping, view: DrawnGroupingView, sc: SidebarContext, bands: PaneBands): HTMLElement {
  const element = document.createElement("div");
  element.className = "sb-pane";
  element.setAttribute("data-grouping", grouping);
  element.appendChild(drawRosterOpenSection(bands.open, view, sc, `${bands.path}.open`));
  element.appendChild(drawRosterHiddenSection(bands.hidden, sc, `${bands.path}.hidden`));
  element.appendChild(drawRosterMergedSection(bands.merged, sc, `${bands.path}.recently_merged`));
  return element;
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

/** The repository grouping: one section per repository, in the resolver's order. */
export function drawRosterRepositoryView(
  u: RosterRepositoryView,
  sc: SidebarContext,
  path: string,
): DrawnGroupingView {
  const view = document.createElement("div");
  view.className = "sb-sections";
  for (const [index, section] of u.sections.entries()) {
    view.appendChild(drawRosterRepoSection(section, sc, `${path}.sections[${index}]`));
  }
  return { body: view, openCount: drawRosterOpenCount(requireMessage(u.openCount, `${path}.open_count`), `${path}.open_count`) };
}

/** The task grouping: one section per task, plus the new-task control. */
export function drawRosterTaskView(
  u: RosterTaskView,
  sc: SidebarContext,
  path: string,
): DrawnGroupingView {
  const view = document.createElement("div");
  view.className = "sb-sections";
  view.appendChild(drawCreateTaskControl(sc));
  for (const [index, section] of u.sections.entries()) {
    view.appendChild(drawRosterTaskSection(section, sc, `${path}.sections[${index}]`));
  }
  return { body: view, openCount: drawRosterOpenCount(requireMessage(u.openCount, `${path}.open_count`), `${path}.open_count`) };
}

/** The maturity grouping: one section per stage, in the order work moves through them. */
export function drawRosterMaturityView(
  u: RosterMaturityView,
  sc: SidebarContext,
  path: string,
): DrawnGroupingView {
  const view = document.createElement("div");
  view.className = "sb-sections";
  for (const [index, section] of u.sections.entries()) {
    view.appendChild(drawRosterMaturitySection(section, sc, `${path}.sections[${index}]`));
  }
  return { body: view, openCount: drawRosterOpenCount(requireMessage(u.openCount, `${path}.open_count`), `${path}.open_count`) };
}

/** How many rows the open-workspaces section lists under a grouping, exactly as the daemon resolved it. */
export function drawRosterOpenCount(u: RosterOpenCount, path: string): number {
  void path;
  return u.workspaces;
}

/** The class of the open-workspaces section, and of its count. */
export const OPEN_SECTION_CLASS = "open-section";
export const OPEN_COUNT_CLASS = "sb-open-count";

/**
 * The open-workspaces section: the "Open" heading over one grouping's
 * sections, drawn like the Hidden and Recently Merged headings, with "(N)"
 * beside the label whether the section is folded or not (owner request,
 * 2026-10-13). N is the grouping's own count, the daemon's sum of its
 * sections' counts, so it is what a person counting the section's entries
 * arrives at, a folded section's entries included.
 *
 * Its fold is the daemon's, like every section's, and a fold gesture paints
 * every pane's copy at once.
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
  count.textContent = `(${view.openCount})`;
  header.appendChild(count);
  section.appendChild(header);
  section.appendChild(view.body);
  return section;
}

/** One maturity stage's section: its key, its header, its rows. */
export function drawRosterMaturitySection(
  u: RosterMaturitySection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const stage = drawRosterMaturityKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  const section = drawHeadedSection(u, sc, path, {
    foldKey: maturityFoldKey(stage),
    className: "maturity-section",
    target: { case: "maturity", value: maturityStageRef(stage) },
  });
  section.setAttribute("data-maturity-stage", stage);
  return section;
}

/** A stage's stable identity: the stage arm. EVERY arm is named; unset is malformed. */
export function drawRosterMaturityKey(u: RosterMaturityKey, path: string): MaturityStageName {
  const arm = requireCase(requireMessage(u.stage, `${path}.stage`).stage, `${path}.stage.stage`);
  switch (arm.case) {
    case "backlog":
    case "inProgress":
    case "merging":
      return arm.case;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.stage.stage`, other.case);
    }
  }
}

/** The stage's wire identity, echoed back on a fold. */
function maturityStageRef(stage: MaturityStageName): MaturityStage {
  return create(MaturityStageSchema, { stage: { case: stage, value: {} } });
}

/** One repository's section: its key, its header, its rows. */
export function drawRosterRepoSection(
  u: RosterRepoSection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const key = drawRosterRepoKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  const divergence = drawRosterRepoDivergence(
    requireMessage(u.divergence, `${path}.divergence`),
    `${path}.divergence`,
  );
  // THE FOLD IS THE DAEMON'S: the Emacs tab bar hides a collapsed
  // repository's workspaces off the same pushed arm, so the two can never
  // disagree.
  return drawHeadedSection(u, sc, path, {
    foldKey: repoFoldKey(key.id),
    className: "repo-section",
    target: { case: "repository", value: key },
    decorateHeader: (header, section) => {
      headerLabel(header).title = divergence;
      header.appendChild(drawCreateWorkspaceControl(key, section, sc));
    },
  });
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

/** What a live grouping's section shares with the others: header, rows, fold. */
export interface HeadedSectionWire extends HeadedBoxWire {
  rows?: RosterRows;
}

/** How one kind of headed section is identified and decorated. */
export interface HeadedBoxShape {
  /** The page key its fold is held under (`view.ts`). */
  foldKey: string;
  /** The class naming the section's kind. */
  className: string;
  /** The section UpdateSidebarView folds. */
  target: MessageInitShape<typeof SidebarViewFoldSectionSchema>["section"];
  /** Anything the kind adds to its header, after the folded count. */
  decorateHeader?: (header: HTMLElement, section: HTMLElement) => void;
  /**
   * A section the rail leaves out while it draws nothing: WHEN says so from the
   * wire, and ATTRIBUTE marks the undrawn box. Absent, the section always draws
   * (a repository with no rows still draws, so the grouping does not flicker).
   */
  leftOut?: { when: (header: RosterSectionHeader, path: string) => boolean; attribute: string };
}

/** How a rows-bodied headed section is identified, decorated and drawn. */
export interface HeadedSectionShape extends HeadedBoxShape {
  /**
   * Whether closed rows are dropped (`drawRosterRows`); they are unless said.
   */
  hideClosed?: boolean;
  /** Which kind of section the rows sit in (`RowPlace`); a grouping's unless said. */
  place?: RowPlace;
}

/**
 * A live grouping's section under the shared `RosterSectionHeader`: the box
 * folded as the daemon says, the label-and-count header whose gesture asks
 * for the other fold, and the rows with closed workspaces dropped.
 *
 * THE ONE SHAPE every folding section but a task's and the merged band's
 * draws — repository, maturity stage, hidden workspaces — through
 * `drawHeadedBox`, which the merged band shares, so none can disagree with
 * another on how a header folds.
 */
export function drawHeadedSection(
  u: HeadedSectionWire,
  sc: SidebarContext,
  path: string,
  shape: HeadedSectionShape,
): HTMLElement {
  const rows = requireMessage(u.rows, `${path}.rows`);
  const hideClosed = shape.hideClosed ?? true;
  const place = shape.place ?? "grouping";
  return drawHeadedBox(u, sc, path, shape, () => drawRosterRows(rows, sc, `${path}.rows`, hideClosed, place));
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
  const head = drawRosterSectionHeader(header, section, sectionFold(sc, shape.foldKey, shape.target), `${path}.header`);
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

/**
 * A section's two-arm fold oneof, as every folding section spells it: the arm
 * IS the fold, each a dedicated empty message.
 */
export type RosterSectionFold =
  | { case: "expanded"; value: unknown }
  | { case: "collapsed"; value: unknown }
  | { case: undefined; value?: undefined };

/** Whether a section is collapsed. EVERY arm is named; unset is malformed. */
export function drawRosterSectionFold(fold: RosterSectionFold, path: string): boolean {
  const arm = requireCase(fold, path);
  switch (arm.case) {
    case "expanded":
      return false;
    case "collapsed":
      return true;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(path, other.case);
    }
  }
}

/** A repository's stable identity — the fold key and the create echo token. */
export function drawRosterRepoKey(u: RosterRepoKey, path: string): RepositoryRef {
  return requireMessage(u.repository, `${path}.repository`);
}

/** One task's section: its key, its header (with the done check), its rows. */
export function drawRosterTaskSection(
  u: RosterTaskSection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const taskId = drawRosterTaskKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  const section = daemonFoldedBox(taskFoldKey(taskId), drawRosterSectionFold(u.fold, `${path}.fold`), sc);
  section.classList.add("task-section");
  section.appendChild(
    drawRosterTaskSectionHeader(
      requireMessage(u.header, `${path}.header`),
      taskId,
      section,
      sc,
      `${path}.header`,
    ),
  );
  section.appendChild(drawRosterRows(requireMessage(u.rows, `${path}.rows`), sc, `${path}.rows`, true));
  return section;
}

/** A task's stable identity: the ID, never the title. */
export function drawRosterTaskKey(u: RosterTaskKey, path: string): string {
  void path;
  return u.taskId;
}

/**
 * The recently-merged band: settled merges, hoisted out of every grouping, in
 * one group per repository.
 *
 * A BAND THAT SHOWS NO MERGE IS NOT DRAWN. The wire always carries the
 * section -- the roster's `recently_merged` is a plain field with a
 * resolver-composed heading, and its absence would be a contract breach, not
 * an empty band -- so the emptiness is answered HERE, off the daemon's count
 * of the rows its SHOWN groups carry: a heading over no rows is a fold toggle
 * onto nothing, and it says a workspace merged when none has (or none of an
 * expanded repository has). A REPOSITORY section still draws with no rows,
 * because the grouping would flicker as its last workspace merged; the merged
 * band has no place to keep, so it goes away entire until a merge shows.
 *
 * EVERY MERGE THE DAEMON SENDS IS DRAWN (a week of them), under the daemon's
 * count of the shown groups' rows; once the band is laid out, `fitBands`
 * (band-fit.ts) sizes its scrolling viewport against the Hidden Workspaces
 * band's.
 */
export function drawRosterMergedSection(
  u: RosterMergedSection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const groups = u.groups.map((group, index) => drawRosterMergedRepoGroup(group, sc, `${path}.groups[${index}]`));
  const section = drawHeadedBox(
    u,
    sc,
    path,
    {
      foldKey: MERGED_FOLD_KEY,
      className: MERGED_SECTION_CLASS,
      target: { case: "recentlyMerged", value: {} },
      leftOut: {
        when: (header, headerPath) =>
          drawRosterSectionCount(requireMessage(header.count, `${headerPath}.count`), `${headerPath}.count`) === 0,
        attribute: "data-merged-empty",
      },
      decorateHeader: (header) => {
        const merges = groups.flatMap((g) => g.shownMerges);
        header.appendChild(drawMergedTodayCount(merges, sc));
        tickMergedTodayCommits(header, merges, sc);
      },
    },
    () => {
      const region = document.createElement("div");
      region.className = "rows";
      for (const group of groups) region.appendChild(group.element);
      return region;
    },
  );
  return section;
}

/** The class of one repository's group inside the band, and of its header. */
export const MERGED_GROUP_CLASS = "merged-group";
export const MERGED_GROUP_HEAD_CLASS = "merged-group-head";

/** The attribute a merged row states its merged instant (epoch ms) on. */
export const MERGED_AT_ATTRIBUTE = "data-merged-at";

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
): DrawnMergedRepoGroup {
  const key = drawRosterRepoKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  const rows = requireMessage(u.rows, `${path}.rows`);
  const merges = rows.rows.map((row, i): MergedRowFacts => {
    const when = requireMessage(row.when, `${path}.rows.rows[${i}].when`);
    const arm = requireCase(when.shown, `${path}.rows.rows[${i}].when.shown`);
    if (arm.case !== "merged") return unreachableArm(`${path}.rows.rows[${i}].when.shown`, arm.case);
    const commits = requireMessage(row.detail, `${path}.rows.rows[${i}].detail`).commits;
    return {
      atMs: drawRosterRowWhenMerged(arm.value, `${path}.rows.rows[${i}].when.merged`),
      commits: commits === undefined ? undefined : drawRosterRowDetailCommits(commits, `${path}.rows.rows[${i}].detail.commits`),
    };
  });
  const wireCollapsed = drawRosterMergedRepoGroupDrawn(u.drawn, `${path}.drawn`);
  const group = document.createElement("div");
  group.className = MERGED_GROUP_CLASS;
  group.setAttribute("data-repository", key.id);
  group.appendChild(drawRosterMergedRepoGroupHeader(requireMessage(u.header, `${path}.header`), `${path}.header`));
  const region = drawRosterRows(rows, sc, `${path}.rows`, false, "merged");
  [...region.children].forEach((row, i) => row.setAttribute(MERGED_AT_ATTRIBUTE, String(merges[i].atMs)));
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

/** The class of the open band's merged-today count. */
export const MERGED_TODAY_COUNT_CLASS = "sb-today-count";

/**
 * The open band's "(K today)": how many of its merges landed TODAY (local date),
 * counted over every row of its SHOWN groups the daemon sent, including those
 * scrolled out of view (owner request, 2026-10-11). The stylesheet shows it
 * only while the band is unfolded, where the folded count is not, and the
 * shared ticker recounts it, so it drops at midnight together with the green
 * names.
 */
export function drawMergedTodayCount(merges: MergedRowFacts[], sc: SidebarContext): HTMLElement {
  const count = document.createElement("span");
  count.className = MERGED_TODAY_COUNT_CLASS;
  tickPaint(sc, (nowMs) => {
    count.textContent = `(${todaysMerges(merges, nowMs).length} today)`;
  });
  return count;
}

/**
 * The merges among MERGES that landed today (local date): the ONE filter the
 * header's "(K today)" and its commit hover both count over, so the hover
 * always sums exactly the merges the count names.
 */
export function todaysMerges(merges: MergedRowFacts[], nowMs: number): MergedRowFacts[] {
  return merges.filter((m) => mergedToday(m.atMs, nowMs));
}

/**
 * Paint the band header's hover now and on every tick: the commits today's
 * merges brought in, summed (owner request, 2026-10-14). The ticker drops a
 * merge at midnight from the sum on the same paint as from "(K today)".
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
  const summed = `${commits} ${commits === 1 ? "commit" : "commits"} merged today`;
  if (uncounted === 0) return summed;
  return `${summed} (${uncounted} ${uncounted === 1 ? "merge" : "merges"} not counted)`;
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
 * sizes its viewport against the Recently Merged band's.
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
  return drawHeadedSection(u, sc, path, {
    foldKey: HIDDEN_FOLD_KEY,
    className: HIDDEN_SECTION_CLASS,
    target: { case: "hidden", value: {} },
    place: "hidden",
    leftOut: {
      when: (header, headerPath) =>
        drawRosterSectionCount(requireMessage(header.count, `${headerPath}.count`), `${headerPath}.count`) === 0,
      attribute: "data-hidden-empty",
    },
  });
}

/** The header repos and the merged band share: a label and a fold toggle. */
export function drawRosterSectionHeader(
  u: RosterSectionHeader,
  section: HTMLElement,
  gesture: FoldGesture,
  path: string,
): HTMLElement {
  const header = drawFoldHeader(
    drawSectionLabel(requireMessage(u.label, `${path}.label`), `${path}.label`),
    section,
    gesture,
  );
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
 * THE ONE `repo-head` every folding section box draws: its fold triangle,
 * then LABEL, with a click anywhere on it asking GESTURE for the other fold.
 * Whatever a kind of section adds (a count, a control) is appended after.
 */
export function drawFoldHeader(label: HTMLElement, section: HTMLElement, gesture: FoldGesture): HTMLElement {
  const header = document.createElement("div");
  header.className = "repo-head";
  header.appendChild(drawFoldToggle(section, gesture));
  header.appendChild(label);
  header.addEventListener("click", () => gesture(section, header));
  return header;
}

/** The section's workspace count, exactly as the daemon resolved it. */
export function drawRosterSectionCount(u: RosterSectionCount, path: string): number {
  void path;
  return u.workspaces;
}

/**
 * What a section header's fold gesture does, handed the section and the
 * control the gesture landed on.
 */
export type FoldGesture = (section: HTMLElement, control: HTMLElement) => void;

/**
 * A section's fold gesture: the other fold, painted on every copy of the
 * section at once and asked of the daemon, whose push carries it to every
 * page (`view-change.ts`).
 */
function sectionFold(sc: SidebarContext, foldKey: string, target: MessageInitShape<typeof SidebarViewFoldSectionSchema>["section"]): FoldGesture {
  return (section, control) => {
    const collapse = !section.classList.contains("folded");
    void changeView(sc, {
      key: foldViewKey(foldKey),
      value: collapse,
      control,
      change: {
        case: "foldSection",
        value: {
          section: target,
          fold: collapse ? { case: "collapse", value: {} } : { case: "expand", value: {} },
        },
      },
    });
  };
}

/**
 * A task section's header: label, fold, done check, rename.
 *
 * ITS OWN MESSAGE, because the done axis exists only for tasks — and its own
 * function here for the same reason. The check is a CONTROL as well as a fact:
 * clicking it is `UpdateTask`, with the arm chosen by what the check currently
 * says, so a done task reopens and an open one completes.
 */
export function drawRosterTaskSectionHeader(
  u: RosterTaskSectionHeader,
  taskId: string,
  section: HTMLElement,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const done = drawRosterTaskDone(requireMessage(u.done, `${path}.done`), `${path}.done`);
  const title = drawRosterLabel(requireMessage(u.label, `${path}.label`), `${path}.label`);
  log.debug("drawing a task section header", {
    operation: "sidebar.roster.task-header",
    context: { path, task: taskId, done },
  });

  const header = document.createElement("div");
  header.className = "task-head";
  if (done) header.classList.add("done");
  const fold = sectionFold(sc, taskFoldKey(taskId), {
    case: "task",
    value: create(TaskRefSchema, { id: taskId }),
  });
  header.appendChild(drawFoldToggle(section, fold));
  header.appendChild(drawTaskDoneCheck(taskId, done, sc));

  const label = document.createElement("span");
  label.className = "task-label";
  label.textContent = title;
  header.appendChild(label);

  // THE TASK'S OWN VERBS, drawn with the header and revealed by the "⋯"
  // control beside it — the same shape the roster row's menu has, for the same
  // reason: what a task IS and what can be DONE to one are two surfaces.
  const menu = drawTaskMenu(taskId, title, sc);
  const more = createControl();
  more.className = "sb-more";
  more.textContent = "⋯";
  more.title = "task actions";
  more.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    toggleHiddenDropdown(sc.dropdowns, "task-menu", menu, [more]);
  });
  header.appendChild(more);
  header.appendChild(menu);
  header.addEventListener("click", () => fold(section, header));
  return header;
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

/** A task's done check, as the daemon resolved it. */
export function drawRosterTaskDone(u: RosterTaskDone, path: string): boolean {
  void path;
  return u.done;
}

/**
 * The rows region of a section, in the resolver's render order.
 *
 * `hideClosed` drops closed, killed and nuked workspaces from a live grouping
 * (owner ruling, 2026-09-14) and hoists their live descendants up in their
 * place. The recently-merged band leaves it OFF: its rows are `closed = true`
 * by design and are exactly what that band exists to show. `place` is which
 * kind of section the rows sit in (`RowPlace`).
 */
export function drawRosterRows(
  u: RosterRows,
  sc: SidebarContext,
  path: string,
  hideClosed = false,
  place: RowPlace = "grouping",
): HTMLElement {
  const rows = document.createElement("div");
  rows.className = "rows";
  const drawn = hideClosed
    ? expandVisibleRows(u.rows, `${path}.rows`)
    : u.rows.map((row, index) => ({ row, path: `${path}.rows[${index}]` }));
  for (const entry of drawn) {
    rows.appendChild(drawRosterRow(entry.row, sc, entry.path, place));
  }
  return rows;
}

/**
 * The box a section is drawn in: folded as the wire says, unless this page's
 * own ask is still in flight (`view.ts`).
 */
function daemonFoldedBox(foldKey: string, wireFolded: boolean, sc: SidebarContext): HTMLElement {
  const section = foldedBox(foldKey, false);
  const folded = sc.view.track(foldViewKey(foldKey), wireFolded, (value) => paintSectionFold(section, value));
  section.classList.toggle("folded", folded);
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

/** The ▸/▾ fold triangle, drawn the way its section stands. */
function drawFoldToggle(section: HTMLElement, gesture: FoldGesture): HTMLElement {
  const triangle = document.createElement("span");
  triangle.className = "tri";
  triangle.setAttribute("data-section-fold", "");
  triangle.addEventListener("click", (event) => {
    event.stopPropagation();
    gesture(section, triangle);
  });
  paintTriangle(triangle, section.classList.contains("folded"));
  return triangle;
}

/** The done check: a fact from the wire and the control that flips it. */
function drawTaskDoneCheck(taskId: string, done: boolean, sc: SidebarContext): HTMLElement {
  const check = createControl();
  check.className = done ? "task-check done" : "task-check";
  check.setAttribute("data-task-status", done ? "done" : "open");
  check.setAttribute("data-task-change", done ? "setOpen" : "setDone");
  check.textContent = done ? "✓" : "";
  check.title = done ? "reopen this task" : "mark this task done";
  check.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    void fireVerb(check, {
      sc,
      rpc: "UpdateTask",
      call: (client) =>
        client.updateTask(
          buildUpdateTaskRequest(taskId, done ? { case: "setOpen" } : { case: "setDone" }),
        ),
      schema: UpdateTaskResponseSchema,
      refusalText: (cause) => updateTaskRefusal(cause as never),
    });
  });
  return check;
}

/**
 * A task's menu: rename, complete, reopen.
 *
 * BOTH done arms are offered, always. `UpdateTask` has three changes and the
 * daemon answers `no_change` when one is a no-op, so the rail draws all three
 * controls and lets the refusal say so at the control — rather than hiding an
 * arm and deciding on the daemon's behalf what a task's state permits.
 */
function drawTaskMenu(taskId: string, title: string, sc: SidebarContext): HTMLElement {
  const menu = document.createElement("div");
  menu.className = "sb-menu list-rows";
  menu.hidden = true;
  menu.addEventListener("click", (event) => event.stopPropagation());

  const input = document.createElement("input");
  input.type = "text";
  input.className = "task-rename";
  input.setAttribute("data-task-rename", taskId);
  input.value = title;
  menu.appendChild(input);

  const rename = taskChangeButton(
    "setTitle",
    "Rename",
    () => {
      const next = input.value.trim();
      return next === "" ? null : { case: "setTitle", title: next };
    },
    taskId,
    sc,
  );
  menu.appendChild(rename);
  input.addEventListener("keydown", (event) => {
    if (event.key === "Enter") rename.click();
  });

  menu.appendChild(
    taskChangeButton("setDone", "Mark done", () => ({ case: "setDone" }), taskId, sc),
  );
  menu.appendChild(
    taskChangeButton("setOpen", "Reopen", () => ({ case: "setOpen" }), taskId, sc),
  );
  return menu;
}

/** One `UpdateTask` control, with the change it composes when clicked. */
function taskChangeButton(
  arm: TaskChange["case"],
  label: string,
  compose: () => TaskChange | null,
  taskId: string,
  sc: SidebarContext,
): Control {
  const button = createControl();
  button.className = "sb-menu-item";
  button.setAttribute("data-task-change", arm);
  button.textContent = label;
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    const change = compose();
    // A blank title is not SENT: the contract says the title is non-blank, so
    // the refusal is avoided rather than provoked.
    if (change === null) return;
    void fireVerb(button, {
      sc,
      rpc: "UpdateTask",
      call: (client) => client.updateTask(buildUpdateTaskRequest(taskId, change)),
      schema: UpdateTaskResponseSchema,
      refusalText: (cause) => updateTaskRefusal(cause as never),
    });
  });
  return button;
}

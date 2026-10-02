/**
 * roster — the rail's two groupings, their sections, and the recently-merged
 * band that sits under both.
 *
 * BOTH GROUPINGS ARE DRAWN, ONE IS SHOWN. The daemon resolves the repository
 * view and the task view as siblings, so choosing between them is a SELECTION
 * — exactly as folding hides rows — and never a derivation: this file draws
 * both panes and hides one. Nothing is re-grouped, re-sorted or counted here.
 *
 * RECENTLY MERGED APPEARS UNDER BOTH, because the interesting fact about those
 * workspaces is that they are done, which is true in either grouping. It has no
 * key of its own (the roster carries exactly one), so its fold identity is
 * fixed, and it starts folded: settled history should not spend rail height
 * until it is asked for.
 *
 * `WorkspaceRoster.current` IS NOT COMPARED HERE. The row states its own
 * highlight through `RosterRowCurrent`, deliberately, so no client can compute
 * it differently. The field is validated and logged, and that is all.
 */
import { createControl, type Control } from "../control.js";
import type {
  RosterCurrentWorkspace,
  RosterLabel,
  RosterMergedSection,
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
import type { RepositoryRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { UpdateTaskResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_task_pb";
import { create } from "@bufbuild/protobuf";
import {
  FoldRepositoryRequestSchema,
  FoldRepositoryResponseSchema,
  type FoldRepositoryError,
  type FoldRepositoryRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_repository_pb";
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import {
  MERGED_FOLD_KEY,
  repoFoldKey,
  taskFoldKey,
  type Grouping,
  type SidebarContext,
} from "./context.js";
import { drawCreateWorkspaceControl } from "./create.js";
import { drawRosterRow, expandVisibleRows } from "./row.js";
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
  const merged = requireMessage(u.recentlyMerged, `${path}.recently_merged`);
  const current =
    u.current === undefined
      ? null
      : drawRosterCurrentWorkspace(u.current, `${path}.current`);
  log.debug("drawing the workspace roster", {
    operation: "sidebar.roster",
    context: {
      repo_sections: repository.sections.length,
      task_sections: task.sections.length,
      merged_rows: merged.rows?.rows.length ?? 0,
      current,
      grouping: sc.prefs.grouping(),
    },
  });

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
  const shown = sc.prefs.grouping();
  roster.appendChild(
    pane("repository", shown, drawRosterRepositoryView(repository, sc, `${path}.repository`), sc, merged, `${path}.recently_merged`),
  );
  roster.appendChild(
    pane("task", shown, drawRosterTaskView(task, sc, `${path}.task`), sc, merged, `${path}.recently_merged`),
  );
  return roster;
}

/** One grouping's pane, with its own copy of the recently-merged band. */
function pane(
  grouping: Grouping,
  shown: Grouping,
  body: HTMLElement,
  sc: SidebarContext,
  merged: RosterMergedSection,
  mergedPath: string,
): HTMLElement {
  const element = document.createElement("div");
  element.className = "sb-pane";
  element.setAttribute("data-grouping", grouping);
  element.hidden = grouping !== shown;
  element.appendChild(body);
  element.appendChild(drawRosterMergedSection(merged, sc, mergedPath));
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
): HTMLElement {
  const view = document.createElement("div");
  view.className = "sb-sections";
  for (const [index, section] of u.sections.entries()) {
    view.appendChild(drawRosterRepoSection(section, sc, `${path}.sections[${index}]`));
  }
  return view;
}

/** The task grouping: one section per task, plus the new-task control. */
export function drawRosterTaskView(
  u: RosterTaskView,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const view = document.createElement("div");
  view.className = "sb-sections";
  view.appendChild(drawCreateTaskControl(sc));
  for (const [index, section] of u.sections.entries()) {
    view.appendChild(drawRosterTaskSection(section, sc, `${path}.sections[${index}]`));
  }
  return view;
}

/** One repository's section: its key, its header, its rows. */
export function drawRosterRepoSection(
  u: RosterRepoSection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const key = drawRosterRepoKey(requireMessage(u.key, `${path}.key`), `${path}.key`);
  // THE FOLD IS THE DAEMON'S (FoldRepository): the Emacs tab bar hides a
  // collapsed repository's workspaces off the same pushed arm, so the two can
  // never disagree.
  const section = foldedBox(repoFoldKey(key.id), drawRosterRepoSectionFold(u.fold, `${path}.fold`));
  section.classList.add("repo-section");
  const header = drawRosterSectionHeader(
    requireMessage(u.header, `${path}.header`),
    section,
    repositoryFold(sc, key),
    `${path}.header`,
  );
  header.appendChild(drawCreateWorkspaceControl(key, section, sc));
  section.appendChild(header);
  section.appendChild(drawRosterRows(requireMessage(u.rows, `${path}.rows`), sc, `${path}.rows`, true));
  return section;
}

/** Whether a repository section is collapsed. EVERY arm is named; unset is malformed. */
export function drawRosterRepoSectionFold(fold: RosterRepoSection["fold"], path: string): boolean {
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

/** FoldRepository: the arm is the fold asked for. */
export function buildFoldRepositoryRequest(repository: RepositoryRef, collapse: boolean): FoldRepositoryRequest {
  return create(FoldRepositoryRequestSchema, {
    repository,
    fold: collapse ? { case: "collapse", value: {} } : { case: "expand", value: {} },
  });
}

/** FoldRepository's one arm of its own. */
export function foldRepositoryRefusal(cause: NonNullable<FoldRepositoryError["cause"]> & { case: string }): string {
  switch (cause.case) {
    case "unknownRepository":
      return "the daemon no longer has that repository";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("FoldRepositoryError.cause", other.case);
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
  const section = sectionBox(taskFoldKey(taskId), sc, false);
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
 * The recently-merged band: settled merges, hoisted out of both groupings.
 *
 * A BAND WITH NO MERGES IS NOT DRAWN. The wire always carries the section --
 * the roster's `recently_merged` is a plain field with a resolver-composed
 * heading, and its absence would be a contract breach, not an empty band -- so
 * the emptiness is answered HERE, where the box is made: a heading over no
 * rows is a fold toggle onto nothing, and it says a workspace merged when none
 * has. A REPOSITORY section still draws with no rows, because the grouping
 * would flicker as its last workspace merged; the merged band has no place to
 * keep, so it goes away entire until a merge lands.
 */
export function drawRosterMergedSection(
  u: RosterMergedSection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const section = sectionBox(MERGED_FOLD_KEY, sc, true);
  section.classList.add("merged-section");
  const rows = requireMessage(u.rows, `${path}.rows`);
  const header = requireMessage(u.header, `${path}.header`);
  if (rows.rows.length === 0) {
    // Validated whole first, then left undrawn: a malformed empty section is
    // still a contract breach, and skipping the checks would hide it until the
    // first merge landed.
    drawRosterLabel(requireMessage(header.label, `${path}.header.label`), `${path}.header.label`);
    section.hidden = true;
    section.setAttribute("data-merged-empty", "true");
    return section;
  }
  section.appendChild(
    drawRosterSectionHeader(header, section, localFold(sc, MERGED_FOLD_KEY), `${path}.header`),
  );
  section.appendChild(drawRosterRows(rows, sc, `${path}.rows`, false, true));
  return section;
}

/** The header repos and the merged band share: a label and a fold toggle. */
export function drawRosterSectionHeader(
  u: RosterSectionHeader,
  section: HTMLElement,
  gesture: FoldGesture,
  path: string,
): HTMLElement {
  const header = document.createElement("div");
  header.className = "repo-head";
  header.appendChild(drawFoldToggle(section, gesture));
  const label = document.createElement("span");
  label.className = "sb-label";
  label.textContent = drawRosterLabel(requireMessage(u.label, `${path}.label`), `${path}.label`);
  header.appendChild(label);
  // THE FOLDED COUNT, "(N)", sits between the label and the add control the
  // repo section appends after this header. The daemon resolves N (nested
  // family rows included); fold state is this page's, so the stylesheet shows
  // the count under `.folded` only and nothing here counts or hides it.
  const count = document.createElement("span");
  count.className = "sb-count";
  count.setAttribute("data-section-count", "");
  count.textContent = `(${drawRosterSectionCount(requireMessage(u.count, `${path}.count`), `${path}.count`)})`;
  header.appendChild(count);
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

/** A webview-local fold: the task and merged sections' own preference. */
function localFold(sc: SidebarContext, foldKey: string): FoldGesture {
  return (section) => toggleFold(section, sc, foldKey);
}

/**
 * A repository's fold: asked of the daemon, and redrawn by the roster push
 * that answers it -- the section never folds itself, so it can never show a
 * fold the daemon (and the Emacs tab bar) does not hold.
 */
function repositoryFold(sc: SidebarContext, repository: RepositoryRef): FoldGesture {
  return (section, control) => {
    const collapse = !section.classList.contains("folded");
    log.debug("asking the daemon to fold a repository section", {
      operation: "sidebar.roster.fold-repository",
      context: { repository: repository.id, collapse },
    });
    void fireVerb(control, {
      sc,
      rpc: "FoldRepository",
      call: (client) => client.foldRepository(buildFoldRepositoryRequest(repository, collapse)),
      schema: FoldRepositoryResponseSchema,
      refusalText: (cause) => foldRepositoryRefusal(cause as never),
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
  header.appendChild(drawFoldToggle(section, localFold(sc, taskFoldKey(taskId))));
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
    menu.hidden = !menu.hidden;
  });
  header.appendChild(more);
  header.appendChild(menu);
  header.addEventListener("click", () => toggleFold(section, sc, taskFoldKey(taskId)));
  return header;
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
 * by design and are exactly what that band exists to show.
 */
export function drawRosterRows(
  u: RosterRows,
  sc: SidebarContext,
  path: string,
  hideClosed = false,
  merged = false,
): HTMLElement {
  const rows = document.createElement("div");
  rows.className = "rows";
  const drawn = hideClosed
    ? expandVisibleRows(u.rows, `${path}.rows`)
    : u.rows.map((row, index) => ({ row, path: `${path}.rows[${index}]` }));
  for (const entry of drawn) {
    rows.appendChild(drawRosterRow(entry.row, sc, entry.path, merged));
  }
  return rows;
}

/** The box a section is drawn in, folded per the local preference. */
function sectionBox(foldKey: string, sc: SidebarContext, defaultFolded: boolean): HTMLElement {
  return foldedBox(foldKey, sc.prefs.isFolded(foldKey, defaultFolded));
}

/** The box a section is drawn in, folded as FOLDED says. */
function foldedBox(foldKey: string, folded: boolean): HTMLElement {
  const section = document.createElement("div");
  section.className = "repo";
  section.setAttribute("data-section", foldKey);
  if (folded) section.classList.add("folded");
  return section;
}

/** The ▸/▾ fold triangle. The state is webview-local and no wire element. */
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

function toggleFold(section: HTMLElement, sc: SidebarContext, foldKey: string): void {
  const folded = !section.classList.contains("folded");
  section.classList.toggle("folded", folded);
  paintFold(section, folded);
  sc.prefs.setFolded(foldKey, folded);
}

/**
 * Say on the toggle itself which way the section stands.
 *
 * The fold is webview-local, so the element that carries the gesture is also
 * the element that reports it — `[data-section-fold][data-folded]` — and the
 * triangle's direction follows from the same one fact.
 */
function paintFold(section: HTMLElement, folded: boolean): void {
  for (const triangle of section.querySelectorAll<HTMLElement>("[data-section-fold]")) {
    paintTriangle(triangle, folded);
  }
}

/** One triangle, told which way its section stands. */
function paintTriangle(triangle: HTMLElement, folded: boolean): void {
  triangle.setAttribute("data-folded", folded ? "true" : "false");
  triangle.textContent = folded ? "▸" : "▾";
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

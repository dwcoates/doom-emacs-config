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
import type {
  RosterCurrentWorkspace,
  RosterLabel,
  RosterMergedSection,
  RosterRepoKey,
  RosterRepoSection,
  RosterRepositoryView,
  RosterRows,
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
import { log } from "../log.js";
import { requireMessage } from "../rpc/strict.js";
import {
  MERGED_FOLD_KEY,
  repoFoldKey,
  taskFoldKey,
  type Grouping,
  type SidebarContext,
} from "./context.js";
import { drawCreateWorkspaceControl } from "./create.js";
import { drawRosterRow } from "./row.js";
import { buildUpdateTaskRequest, drawCreateTaskControl } from "./tasks.js";
import { runVerb } from "./verbs.js";

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
  log("debug", "drawing the workspace roster", {
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
  const section = sectionBox(repoFoldKey(key.id), sc, false);
  const header = drawRosterSectionHeader(
    requireMessage(u.header, `${path}.header`),
    section,
    sc,
    repoFoldKey(key.id),
    `${path}.header`,
  );
  header.appendChild(drawCreateWorkspaceControl(key, section, sc));
  section.appendChild(header);
  section.appendChild(drawRosterRows(requireMessage(u.rows, `${path}.rows`), sc, `${path}.rows`));
  return section;
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
  section.appendChild(drawRosterRows(requireMessage(u.rows, `${path}.rows`), sc, `${path}.rows`));
  return section;
}

/** A task's stable identity: the ID, never the title. */
export function drawRosterTaskKey(u: RosterTaskKey, path: string): string {
  void path;
  return u.taskId;
}

/** The recently-merged band: settled merges, hoisted out of both groupings. */
export function drawRosterMergedSection(
  u: RosterMergedSection,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const section = sectionBox(MERGED_FOLD_KEY, sc, true);
  section.classList.add("merged-section");
  section.appendChild(
    drawRosterSectionHeader(
      requireMessage(u.header, `${path}.header`),
      section,
      sc,
      MERGED_FOLD_KEY,
      `${path}.header`,
    ),
  );
  section.appendChild(drawRosterRows(requireMessage(u.rows, `${path}.rows`), sc, `${path}.rows`));
  return section;
}

/** The header repos and the merged band share: a label and a fold toggle. */
export function drawRosterSectionHeader(
  u: RosterSectionHeader,
  section: HTMLElement,
  sc: SidebarContext,
  foldKey: string,
  path: string,
): HTMLElement {
  const header = document.createElement("div");
  header.className = "repo-head";
  header.appendChild(drawFoldToggle(section, sc, foldKey));
  const label = document.createElement("span");
  label.className = "sb-label";
  label.textContent = drawRosterLabel(requireMessage(u.label, `${path}.label`), `${path}.label`);
  header.appendChild(label);
  header.addEventListener("click", () => toggleFold(section, sc, foldKey));
  return header;
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
  log("debug", "drawing a task section header", {
    operation: "sidebar.roster.task-header",
    context: { path, task: taskId, done },
  });

  const header = document.createElement("div");
  header.className = "task-head";
  if (done) header.classList.add("done");
  header.appendChild(drawFoldToggle(section, sc, taskFoldKey(taskId)));
  header.appendChild(drawTaskDoneCheck(taskId, done, sc));

  const label = document.createElement("span");
  label.className = "task-label";
  label.setAttribute("data-task-rename", taskId);
  label.textContent = title;
  label.title = "rename";
  label.addEventListener("click", (event) => {
    event.stopPropagation();
    openRename(label, taskId, title, sc);
  });
  header.appendChild(label);
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

/** The rows region of a section, in the resolver's render order. */
export function drawRosterRows(
  u: RosterRows,
  sc: SidebarContext,
  path: string,
): HTMLElement {
  const rows = document.createElement("div");
  rows.className = "rows";
  for (const [index, row] of u.rows.entries()) {
    rows.appendChild(drawRosterRow(row, sc, `${path}.rows[${index}]`));
  }
  return rows;
}

/** The box a section is drawn in, folded per the local preference. */
function sectionBox(foldKey: string, sc: SidebarContext, defaultFolded: boolean): HTMLElement {
  const section = document.createElement("div");
  section.className = "repo";
  section.setAttribute("data-section", foldKey);
  if (sc.prefs.isFolded(foldKey, defaultFolded)) section.classList.add("folded");
  return section;
}

/** The ▸/▾ fold triangle. The state is webview-local and no wire element. */
function drawFoldToggle(
  section: HTMLElement,
  sc: SidebarContext,
  foldKey: string,
): HTMLElement {
  const triangle = document.createElement("span");
  triangle.className = "tri";
  triangle.setAttribute("data-section-fold", "");
  triangle.textContent = "▾";
  triangle.addEventListener("click", (event) => {
    event.stopPropagation();
    toggleFold(section, sc, foldKey);
  });
  return triangle;
}

function toggleFold(section: HTMLElement, sc: SidebarContext, foldKey: string): void {
  const folded = !section.classList.contains("folded");
  section.classList.toggle("folded", folded);
  sc.prefs.setFolded(foldKey, folded);
}

/** The done check: a fact from the wire and the control that flips it. */
function drawTaskDoneCheck(taskId: string, done: boolean, sc: SidebarContext): HTMLElement {
  const check = document.createElement("button");
  check.type = "button";
  check.className = done ? "task-check done" : "task-check";
  check.setAttribute("data-task-status", done ? "done" : "open");
  check.setAttribute("data-task-change", done ? "setOpen" : "setDone");
  check.textContent = done ? "✓" : "";
  check.title = done ? "reopen this task" : "mark this task done";
  check.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    void runVerb(check, {
      sc,
      rpc: "UpdateTask",
      call: (client) =>
        client.updateTask(
          buildUpdateTaskRequest(taskId, done ? { case: "setOpen" } : { case: "setDone" }),
        ),
      schema: UpdateTaskResponseSchema,
    });
  });
  return check;
}

/** Rename in place: the label becomes an input, and Enter is the request. */
function openRename(
  label: HTMLElement,
  taskId: string,
  title: string,
  sc: SidebarContext,
): void {
  if (label.hidden) return;
  const input = document.createElement("input");
  input.type = "text";
  input.className = "task-rename";
  input.setAttribute("data-task-title", "");
  input.value = title;
  const submit = document.createElement("button");
  submit.type = "button";
  submit.className = "task-rename-go";
  submit.setAttribute("data-task-change", "setTitle");
  submit.textContent = "Rename";
  const send = (): void => {
    const next = input.value.trim();
    if (next === "") return;
    void runVerb(submit, {
      sc,
      rpc: "UpdateTask",
      call: (client) =>
        client.updateTask(buildUpdateTaskRequest(taskId, { case: "setTitle", title: next })),
      schema: UpdateTaskResponseSchema,
    });
  };
  submit.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    send();
  });
  input.addEventListener("click", (event) => event.stopPropagation());
  input.addEventListener("keydown", (event) => {
    if (event.key === "Enter") send();
  });
  label.hidden = true;
  label.after(input, submit);
}

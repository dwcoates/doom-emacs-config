/**
 * context — what every part of the rail is handed while it draws.
 *
 * The rail is a stateless renderer of the roster like every other component.
 * Its VIEW STATE — folds and the grouping shown — is the daemon's, carried on
 * the roster push so the sidebar looks the same in every workspace's page
 * (owner rulings, 2026-10-06); `view.ts` holds only this page's asks still in
 * flight. A dropdown open mid-gesture (a row's menu or detail popover) is NOT
 * view state: it is transient to the page that opened it.
 */
import type { DrawnStatusLog } from "../drawn-status.js";
import type { DaemonContext } from "../rpc/context.js";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { AttentionRegistry } from "./attention.js";
import type { Dropdowns } from "./dropdowns.js";
import type { RowHover } from "./row-hover.js";
import type { SidebarView } from "./view.js";

/** The resolved groupings; which one is drawn is the daemon's view state. */
export type Grouping = "repository" | "task" | "maturity";

/** Every grouping, in the order the rail's picker offers them. */
export const GROUPINGS: readonly Grouping[] = ["repository", "task", "maturity"];

/** A maturity stage, as `workspace.v1.MaturityStage`'s arm spells it. */
export type MaturityStageName = "backlog" | "inProgress" | "merging";

/** A task the assign menu can offer, taken from the task view's sections. */
export interface TaskChoice {
  /** `RosterTaskKey.task_id`, echoed back to AssignWorkspaceTask verbatim. */
  id: string;
  /** The section header's label, drawn as the menu entry. */
  label: string;
}

/** What a draw of the rail carries with it. */
export interface SidebarContext {
  /** What talking to the daemon takes: client, stream, ticker, failures. */
  ctx: DaemonContext;
  /**
   * The workspace a new workspace created "as a child" is a child of: the
   * page's own workspace when the sidebar is drawn in a page on its own, and
   * the roster's `current` (the workspace Emacs stands in) when it is drawn in
   * the webhost. Null when there is none to name.
   */
  parentWorkspace(): WorkspaceRef | null;
  /** The daemon-held view state, with this page's asks still in flight. */
  view: SidebarView;
  /**
   * The rows whose detail popover is open on THIS page, by `WorkspaceRef.id`.
   * A popover is a dropdown, transient to its page and never the daemon's
   * view state; it is kept here only so a redraw does not snap it shut.
   */
  openDetails: Set<string>;
  /** What each row drew last, stated on every change (`drawn-status.ts`). */
  drawnStatus: DrawnStatusLog;
  /** The one dismiss rule every dropdown in the rail shares. */
  dropdowns: Dropdowns;
  /**
   * The page's one owner of the detail panels' hover and focus gesture,
   * which outlives every draw so a redraw never strands a timer on a
   * thrown-away row (`row-hover.ts`).
   */
  rowHover: RowHover;
  /** The blink registry the attention markers are driven from. */
  attention: AttentionRegistry;
  /**
   * The tasks the assign menu offers, refreshed from the task view at the top
   * of every draw. A live array rather than a snapshot: a menu opened later
   * reads whatever the last push resolved.
   */
  tasks: TaskChoice[];
  /** Register a teardown the NEXT push (or dispose) runs. */
  onDispose(fn: () => void): void;
}

/** The key a repository section is identified by on this page. */
export function repoFoldKey(repositoryId: string): string {
  return `repo:${repositoryId}`;
}

/** The key a task section is identified by on this page (`view.ts`). */
export function taskFoldKey(taskId: string): string {
  return `task:${taskId}`;
}

/** The key a maturity stage's section is identified by on this page (`view.ts`). */
export function maturityFoldKey(stage: MaturityStageName): string {
  return `maturity:${stage}`;
}

/**
 * The recently-merged section's fold key.
 *
 * FIXED, because the roster carries exactly one such section and the message
 * has no key of its own — its identity is that there is only ever one.
 */
export const MERGED_FOLD_KEY = "merged";

/**
 * The hidden-workspaces section's fold key. FIXED for the same reason as
 * `MERGED_FOLD_KEY`: the roster carries exactly one such section.
 */
export const HIDDEN_FOLD_KEY = "hidden";

/**
 * The open-workspaces section's fold key. FIXED for the same reason as
 * `MERGED_FOLD_KEY`: the roster carries exactly one such section.
 */
export const OPEN_FOLD_KEY = "open";

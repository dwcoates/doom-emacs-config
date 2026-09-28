/**
 * context — what every part of the rail is handed while it draws.
 *
 * The rail is a stateless renderer of the roster like every other component,
 * so the only state here is the WEBVIEW-LOCAL preference set the contract
 * explicitly leaves to the client (R14): which grouping is shown, which
 * sections are folded, which rows have their detail panel open. None of it is
 * wire state, and none of it is ever inferred from the roster.
 */
import type { AppContext } from "../rpc/context.js";
import type { AttentionRegistry } from "./attention.js";

/** The two resolved groupings; which one is drawn is local preference. */
export type Grouping = "repository" | "task";

/** A task the assign menu can offer, taken from the task view's sections. */
export interface TaskChoice {
  /** `RosterTaskKey.task_id`, echoed back to AssignWorkspaceTask verbatim. */
  id: string;
  /** The section header's label, drawn as the menu entry. */
  label: string;
}

/**
 * The webview-local preferences, persisted behind try/catch.
 *
 * Reading and writing `localStorage` throws outright in some embeddings (a
 * private window, a webview with site data disabled), and a rail that cannot
 * remember a fold must still draw, so every access here is guarded and a
 * failure costs the preference and nothing else.
 */
export interface SidebarPrefs {
  grouping(): Grouping;
  setGrouping(grouping: Grouping): void;
  /**
   * Whether the section under KEY is folded.
   *
   * DEFAULTFOLDED is what an unremembered section does — open for the live
   * groupings, folded for recently-merged, which is settled history the rail
   * should not spend height on until it is asked for.
   */
  isFolded(key: string, defaultFolded?: boolean): boolean;
  setFolded(key: string, folded: boolean): void;
  /** Whether the row for `WorkspaceRef.id` has its detail panel open. */
  isExpanded(id: string): boolean;
  setExpanded(id: string, expanded: boolean): void;
}

/** What a draw of the rail carries with it. */
export interface SidebarContext {
  /** The app-wide capabilities: client, workspace, ticker, failures. */
  ctx: AppContext;
  /** The webview-local preference set. */
  prefs: SidebarPrefs;
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

/** The fold key a repository section is remembered under. */
export function repoFoldKey(repositoryId: string): string {
  return `repo:${repositoryId}`;
}

/** The fold key a task section is remembered under. */
export function taskFoldKey(taskId: string): string {
  return `task:${taskId}`;
}

/**
 * The recently-merged section's fold key.
 *
 * FIXED, because the roster carries exactly one such section and the message
 * has no key of its own — its identity is that there is only ever one.
 */
export const MERGED_FOLD_KEY = "merged";

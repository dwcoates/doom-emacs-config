/**
 * context — what every part of the rail is handed while it draws.
 *
 * The rail is a stateless renderer of the roster like every other component.
 * Its VIEW STATE — the folds — is the daemon's, carried on
 * the roster push so the sidebar looks the same in every workspace's page
 * (owner rulings, 2026-10-06); `view.ts` holds only this page's asks still in
 * flight. A dropdown open mid-gesture (a row's menu or detail popover) is NOT
 * view state: it is transient to the page that opened it.
 */
import type { DrawnStatusLog } from "../drawn-status.js";
import type { DaemonContext } from "../rpc/context.js";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Dropdowns } from "./dropdowns.js";
import type { RowHover } from "./row-hover.js";
import type { RowReveal } from "./reveal.js";
import type { SelectionFade } from "./selection-fade.js";
import type { DrawnView, SidebarView } from "./view.js";

/** The one thing a drawn row asks of the attention registry. */
export interface AttentionMarks {
  /** ELEMENT carries the attention marker for workspace ID. */
  mark(id: string, element: HTMLElement): void;
}

/**
 * What the rail holds for the life of its mount.
 *
 * IT REGISTERS NOTHING. Every copy of the view and every teardown belongs to
 * the one drawn section that made it (`scope.ts`), so the rail's own context
 * has no way to make one: a section is drawn through `SidebarContext`, which
 * only a scope hands out.
 */
export interface RailContext {
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
  /** Where a drawn row's attention marker is registered (`attention.ts`). */
  attention: AttentionMarks;
  /** Draws a row the first time it comes into view (`reveal.ts`). */
  reveal: RowReveal;
  /** The highlighted row's number fade, across redraws (`selection-fade.ts`). */
  selectionFade: SelectionFade;
  /**
   * The workspace whose page the webhost shows, while the roster has not yet
   * named it current (`markShown`, sidebar.ts), or null. While it stands, it
   * and not `RosterRowCurrent.current` decides which row is highlighted.
   */
  shownAhead(): string | null;
}

/** What one drawn section of the rail is handed: the rail's context, bound to the section's scope. */
export interface SidebarContext extends Omit<RailContext, "view"> {
  /** The view, its copies the section's own. */
  view: DrawnView;
  /** Register a teardown that runs when the section's drawing is replaced or the rail disposed. */
  onDispose(fn: () => void): void;
}

/** The key a repository section is identified by on this page. */
export function repoFoldKey(repositoryId: string): string {
  return `repo:${repositoryId}`;
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

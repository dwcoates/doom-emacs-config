/**
 * context — what every part of the rail is handed while it draws.
 *
 * The rail is a stateless renderer of the roster like every other component.
 * Its VIEW STATE — the folds — is the daemon's, carried on
 * the roster push so the sidebar looks the same in every workspace's page
 * (owner rulings, 2026-10-06); `view.ts` holds only this page's asks still in
 * flight. A dropdown open mid-gesture (a row's menu or detail popover) is NOT
 * view state: it is transient to the page that opened it.
 *
 * IT DECLARES THE SERVICES IT HANDS OUT whose makers draw rows (the row hover,
 * the fold fades, the arrange hold), so every drawer imports this module and
 * this module imports none of them: no import cycle runs through the context
 * (test/import-cycles.test.ts).
 */
import type { DeploysFold } from "./deploys-fold.js";
import type { DrawnStatusLog } from "../drawn-status.js";
import type { MergedPages } from "./merged-pages.js";
import type { DaemonContext } from "../rpc/context.js";
import type { Dropdowns } from "./dropdowns.js";
import type { RowMenus } from "./row-menu.js";
import type { SelectionFade } from "./selection-fade.js";
import type { DrawnView, SidebarView } from "./view.js";
import type { DrawingHold } from "../hold.js";
import type { RosterStreams } from "./roster-streams.js";

/** The one thing a drawn row asks of the attention registry. */
export interface AttentionMarks {
  /** ELEMENT carries the attention marker for workspace ID. */
  mark(id: string, element: HTMLElement): void;
}

/** The page's hold on its own roster drawing while an arrangement is dragged or asked for (`hold.ts`). */
export type ArrangeHold = DrawingHold<RosterStreams>;
/** The rail's fold fades. */
export interface FoldFade {
  /** ROW was just drawn for workspace ID, SURFACED or not: start, join or end its fade. */
  apply(id: string, row: HTMLElement, surfaced: boolean): void;
}
/** One drawn copy of a row, as the current draw registered it. */
export interface DrawnRow {
  /** The row's box, which carries the `.open` class. */
  ws: HTMLElement;
  /** The row's line: the pointer's trigger, and keyboard focus inside it holds the panel open. */
  line: HTMLElement;
  /** The detail panel. */
  detail: HTMLElement;
}
/** Which part of a row the pointer is over. */
export type HoverPart = "line" | "duration" | "more" | "panel";
/** The rail's row hover: which row's detail panel is open, from the pointer and the focus (row-hover.ts). */
export interface RowHover {
  /** The drawing under ROOT was thrown away: its row copies are gone, every other stays. */
  forgetWithin(root: Node): void;
  /** The current draw drew a copy of workspace ID's row. */
  drawn(id: string, row: DrawnRow): void;
  /** The pointer arrived on PART of workspace ID's row. */
  enter(id: string, part: HoverPart): void;
  /** The pointer left PART of workspace ID's row. */
  leave(id: string, part: HoverPart): void;
  /**
   * Workspace ID's row opened its verb menu where its panel stands
   * (row-menu.ts): the panel the pointer resting on the line was about to
   * open stays shut, until the pointer arrives on the row again.
   */
  cancelOpening(id: string): void;
  /** The keyboard focus arrived in workspace ID's row line. */
  focusIn(id: string): void;
  /** The focus left workspace ID's row line for NEXT. */
  focusOut(id: string, next: EventTarget | null): void;
  /** The pointer is over TARGET in the rail, or (null) has left it. */
  pointerAt(target: EventTarget | null): void;
  /** The rail lost the user, for WHY: close every open panel, and open none until `resume`. */
  dismiss(why: string): void;
  /** The user is back in the rail: open the panel of the row the pointer rests on, at once on its duration or "⋯", after the delay elsewhere on its line. */
  resume(): void;
  /**
   * Hand every open panel's live copy to the popup mirror again (a redraw
   * replaced its row, a scroll or a resize moved its line). Nothing without a
   * mirror.
   */
  mirrorOpen(): void;
  /** Cancel every timer: the rail is gone. */
  dispose(): void;
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
  /** The page's one owner of the open row's "⋯" verb menu (`row-menu.ts`). */
  rowMenus: RowMenus;
  /** Where a drawn row's attention marker is registered (`attention.ts`). */
  attention: AttentionMarks;
  /** The highlighted row's number fade, across redraws (`selection-fade.ts`). */
  selectionFade: SelectionFade;
  /** The fade of a row a fold stops drawing, across redraws (`fold-fade.ts`). */
  foldFade: FoldFade;
  /**
   * The workspace whose page the webhost shows, while the roster has not yet
   * named it current (`markShown`, sidebar.ts), or null. While it stands, it
   * and not `RosterRowCurrent.current` decides which row is highlighted.
   */
  shownAhead(): string | null;
  /** What arranging the repository grouping needs (`arrange.ts`). */
  arrange: ArrangeContext;
  /** Whether each Merge Queue and Recently Merged group is folded, this page's own (`deploys-fold.ts`). */
  deploysFold: DeploysFold;
  /** The Recently Merged groups' pages of rows this page has loaded (`merged-pages.ts`). */
  mergedPages: MergedPages;
}

/**
 * What arranging the repository grouping needs, all of it OUTSIDE the drawing
 * a push replaces, so a drag, a name being typed or a refusal outlives every
 * push.
 */
export interface ArrangeContext {
  /** The page's hold on its own drawing while an arrangement is dragged or asked. */
  hold: ArrangeHold;
  /** Where a drag's floating copy and a subsection's name popup are drawn. */
  layer: HTMLElement;
  /** The element an arrangement's refusal is drawn beside. */
  status: HTMLElement;
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

/** The key a subsection is identified by on this page. */
export function subsectionFoldKey(subsectionId: string): string {
  return `subsection:${subsectionId}`;
}

/** The key one workspace's children fold is identified by on this page. */
export function workspaceChildrenFoldKey(workspaceId: string): string {
  return `children:${workspaceId}`;
}

/**
 * The recently-merged section's fold key.
 *
 * FIXED, because the roster carries exactly one such section and the message
 * has no key of its own — its identity is that there is only ever one.
 */
export const MERGED_FOLD_KEY = "merged";

/**
 * The Merge Queue section's fold key. FIXED for the same reason as
 * `MERGED_FOLD_KEY`: the roster carries exactly one such section.
 */
export const MERGE_QUEUE_FOLD_KEY = "merge-queue";

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

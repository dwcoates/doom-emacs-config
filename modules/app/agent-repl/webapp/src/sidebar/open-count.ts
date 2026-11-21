/**
 * open-count — THE ONE SPELLING of every workspace count in the Open section
 * (owner request, 2026-11-19): "(unfolded/total)", where the total is every
 * workspace the part lists, folded or not, and unfolded is how many of them
 * no fold hides.
 *
 * The Open heading's count and every repository section's are spelled
 * `openCountText`. A SUBSECTION'S COUNT AND A ROW'S CHILD COUNT (owner
 * request, 2026-11-20) are the one parenthetical `showWorkspaceCountParenthetical`
 * draws, "(working/merging/total)", so the two can never differ in format,
 * size or ink.
 */
import type { RosterOpenCount, RosterOpenRowTab } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { RosterRow } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage } from "../rpc/strict.js";
import { tabNumber } from "./tab-numbers.js";
import { armWorks, rosterArmMark } from "./tones.js";

/** An Open-section count: every workspace a part lists, and the ones no fold hides. */
export interface OpenCount {
  workspaces: number;
  unfolded: number;
}

/** COUNT as drawn: "(unfolded/workspaces)". */
export function openCountText(count: OpenCount): string {
  return `(${count.unfolded}/${count.workspaces})`;
}

/**
 * The daemon's count U at PATH. More unfolded workspaces than workspaces is
 * malformed.
 */
export function drawRosterOpenCount(u: RosterOpenCount, path: string): OpenCount {
  if (u.unfolded > u.workspaces) {
    throw new MalformedView(`${path}.unfolded`, `${u.unfolded} unfolded of ${u.workspaces} workspaces`);
  }
  return { workspaces: u.workspaces, unfolded: u.unfolded };
}

/**
 * Whether an open row's TAB (at PATH) is numbered (`tabNumber`): no fold
 * hides the row, or one does and the row is surfaced.
 */
export function tabUnfolded(tab: RosterOpenRowTab | undefined, path: string): boolean {
  return tabNumber(tab, path) !== undefined;
}

/** How many of a part's workspaces work, and how many are merging synchronously. */
export interface StatusCount {
  working: number;
  syncMerging: number;
}

/** The status arm that is a workspace's own merge running: a synchronous merge. */
const SYNC_MERGE_ARM = "merging";

/** The arm whose tone is the canonical working red (every working arm paints it). */
const WORKING_TONE_ARM = "thinking";

/** The class of the one workspace-count parenthetical, in a subsection's header and on a row alike. */
export const WORKSPACE_COUNT_CLASS = "sb-count";

/** The class of the working number inside the parenthetical. */
export const WORKING_COUNT_CLASS = "sb-count-working";

/** The class of the synchronously-merging number inside the parenthetical. */
export const MERGING_COUNT_CLASS = "sb-count-merging";

/**
 * How many of ROWS work (`armWorks`) and how many merge synchronously (the
 * `merging` arm). Each entry is a `frontend.v1.RosterRow` with its message
 * path; a row with no status is malformed.
 */
export function statusCountOf(rows: Iterable<{ row: RosterRow | undefined; path: string }>): StatusCount {
  const count: StatusCount = { working: 0, syncMerging: 0 };
  for (const { row, path } of rows) {
    const arm = requireCase(requireMessage(row, path).status, `${path}.status`).case;
    if (armWorks(arm)) count.working++;
    else if (arm === SYNC_MERGE_ARM) count.syncMerging++;
  }
  return count;
}

/**
 * THE ONE WORKSPACE-COUNT PARENTHETICAL, drawn beside a subsection's name
 * (its member workspaces) and a workspace's name (its children): the
 * working workspaces in the working red, the synchronously merging ones in
 * the merge purple, and the TOTAL, with the parentheses and slashes,
 * inheriting the host's ink, so a fold greys them as it greys the name.
 */
export function showWorkspaceCountParenthetical(total: number, status: StatusCount): HTMLElement {
  const element = document.createElement("span");
  element.className = WORKSPACE_COUNT_CLASS;
  const working = document.createElement("span");
  working.className = `${WORKING_COUNT_CLASS} ${rosterArmMark(WORKING_TONE_ARM).toneClass}`;
  working.textContent = String(status.working);
  const merging = document.createElement("span");
  merging.className = `${MERGING_COUNT_CLASS} ${rosterArmMark(SYNC_MERGE_ARM).toneClass}`;
  merging.textContent = String(status.syncMerging);
  element.append("(", working, "/", merging, `/${total})`);
  return element;
}

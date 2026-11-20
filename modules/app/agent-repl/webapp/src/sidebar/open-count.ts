/**
 * open-count — THE ONE SPELLING of every workspace count in the Open section
 * (owner request, 2026-11-19): "(unfolded/total)", where the total is every
 * workspace the part lists, folded or not, and unfolded is how many of them
 * no fold hides.
 *
 * The Open heading's count, every repository section's and every
 * subsection's are the daemon's (`frontend.v1.RosterOpenCount`); a row's
 * child count (`child-count.ts`) reads its children's tabs. All of them are
 * spelled here, so no two counts can disagree on the format.
 */
import type { RosterOpenCount, RosterOpenRowTab } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../rpc/malformed.js";
import { tabNumber } from "./tab-numbers.js";

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

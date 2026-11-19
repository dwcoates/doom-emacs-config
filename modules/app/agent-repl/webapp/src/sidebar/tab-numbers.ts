/**
 * tab-numbers — each open row's Emacs tab number, drawn left of its mark.
 *
 * THE SAME NUMERALS AS THE EMACS TAB BAR (owner request, 2026-10-17). The
 * daemon numbers every open row's tab (`frontend.v1.RosterOpenRow.tab`) in the
 * Open section's walk, which IS the tab bar, so `M-1`… reach exactly the rows
 * these numbers sit beside, and the numbers read 1, 2, 3… down the section. A
 * folded repository's rows take no number.
 *
 * PAINTED AFTER EVERY DRAW, NOT DRAWN WITH THE ROW. A number belongs to the
 * whole bar, not to one row's drawing: a tab opened in another repository
 * shifts every later number while this row's section is kept unchanged
 * (`roster.ts`, "A PUSH REDRAWS ONLY WHAT IT CHANGED"). So a row draws an
 * empty slot, and the roster drawer fills every slot from the push once it is
 * drawn, kept sections included.
 */
import type { RosterOpenRowTab, RosterRepositoryView } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { openRowWorkspaces } from "./entries.js";
import { decorativeElement, queryAll } from "../dom.js";
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";

/** The class of a row's number slot, which `paintTabNumbers` fills. */
export const TAB_NUMBER_CLASS = "sb-num";

/** The attribute a filled slot carries its number in, for the hook contract and tests. */
export const TAB_NUMBER_ATTRIBUTE = "data-tab-number";

/**
 * Each numbered tab's number, by `WorkspaceRef.id`, read off the Open
 * section's open rows (U) exactly as the daemon numbered them. A workspace
 * with two rows, or a row with no tab, is malformed.
 */
export function tabNumbers(u: RosterRepositoryView, path: string): Map<string, number> {
  const numbers = new Map<string, number>();
  for (const { id, row, path: rowPath } of openRowWorkspaces(u, path)) {
    const number = tabNumber(row.tab, `${rowPath}.tab`);
    if (number !== undefined) numbers.set(id, number);
  }
  return numbers;
}

/**
 * An open row's TAB (at PATH) as the daemon resolved it: its number, or
 * undefined when a fold hides the row. A missing tab or arm is malformed.
 */
export function tabNumber(tab: RosterOpenRowTab | undefined, path: string): number | undefined {
  const arm = requireCase(requireMessage(tab, path).tab, `${path}.tab`);
  switch (arm.case) {
    case "numbered":
      return arm.value.number;
    case "folded":
      return undefined;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.tab`, other.case);
    }
  }
}

/** The empty slot a row draws its number into. */
export function drawTabNumberSlot(): HTMLElement {
  return decorativeElement("span", TAB_NUMBER_CLASS);
}

/**
 * Fill every number slot under ROOT from NUMBERS, by the slot's row. A row
 * whose workspace has no drawn tab (one still being opened) leaves its slot
 * empty, holding its width so the names stay aligned.
 */
export function paintTabNumbers(root: ParentNode, numbers: ReadonlyMap<string, number>): void {
  for (const slot of queryAll<HTMLElement>(root, `.${TAB_NUMBER_CLASS}`)) {
    const id = slot.closest("[data-roster-row]")?.getAttribute("data-roster-row");
    if (id === undefined || id === null) {
      log.error("a tab number slot sits outside any roster row", {
        operation: "sidebar.tab-numbers.orphan-slot",
        context: { slots: numbers.size },
      });
      throw new Error("sidebar tab numbers: a number slot outside any roster row");
    }
    const number = numbers.get(id);
    const text = number === undefined ? "" : String(number);
    if (slot.textContent === text) continue;
    slot.textContent = text;
    if (number === undefined) slot.removeAttribute(TAB_NUMBER_ATTRIBUTE);
    else slot.setAttribute(TAB_NUMBER_ATTRIBUTE, text);
  }
}

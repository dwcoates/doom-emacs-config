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
import type { RosterOpenRow, RosterRepositoryView } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { MalformedView } from "../rpc/malformed.js";
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
  const seen = new Set<string>();
  const visit = (row: RosterOpenRow, rowPath: string): void => {
    const id = requireMessage(requireMessage(row.row, `${rowPath}.row`).workspace, `${rowPath}.row.workspace`).workspace?.id;
    if (id === undefined) throw new MalformedView(`${rowPath}.row.workspace.workspace`, "an open row names no workspace");
    if (seen.has(id)) throw new MalformedView(`${rowPath}.row.workspace`, `workspace ${id} has two open rows`);
    seen.add(id);
    const tab = requireCase(requireMessage(row.tab, `${rowPath}.tab`).tab, `${rowPath}.tab.tab`);
    switch (tab.case) {
      case "numbered":
        numbers.set(id, tab.value.number);
        break;
      case "folded":
        break;
      default: {
        const other: { case: string } = tab;
        unreachableArm(`${rowPath}.tab.tab`, other.case);
      }
    }
    row.children.forEach((child, index) => visit(child, `${rowPath}.children[${index}]`));
  };
  u.sections.forEach((section, s) => {
    const sectionPath = `${path}.sections[${s}]`;
    requireMessage(section.openRows, `${sectionPath}.open_rows`).rows.forEach((row, i) => visit(row, `${sectionPath}.open_rows.rows[${i}]`));
    section.subsections.forEach((sub, b) => {
      const subPath = `${sectionPath}.subsections[${b}]`;
      requireMessage(sub.openRows, `${subPath}.open_rows`).rows.forEach((row, i) => visit(row, `${subPath}.open_rows.rows[${i}]`));
    });
  });
  return numbers;
}

/** The empty slot a row draws its number into. */
export function drawTabNumberSlot(): HTMLElement {
  const slot = document.createElement("span");
  slot.className = TAB_NUMBER_CLASS;
  slot.setAttribute("aria-hidden", "true");
  return slot;
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

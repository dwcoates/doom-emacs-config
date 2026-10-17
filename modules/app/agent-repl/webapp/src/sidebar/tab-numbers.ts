/**
 * tab-numbers — each open row's Emacs tab number, drawn left of its mark.
 *
 * THE SAME NUMERALS AS THE EMACS TAB BAR (owner request, 2026-10-17). The bar
 * numbers its DRAWN tabs from 1 in `WorkspaceRoster.tab_bar` order
 * (`frontend.v1.RosterTabDrawn`: "numbered by its position among drawn tabs"),
 * and `M-1`… reach exactly those slots. A withheld tab (its repository
 * collapsed, or the workspace hidden) takes no number and does not count.
 *
 * PAINTED AFTER EVERY DRAW, NOT DRAWN WITH THE ROW. A number belongs to the
 * whole bar, not to one row's wire: a tab opened in another repository shifts
 * every later number while this row's section is kept unchanged
 * (`roster.ts`, "A PUSH REDRAWS ONLY WHAT IT CHANGED"). So a row draws an
 * empty slot, and the roster drawer fills every slot from the push's tab bar
 * once the push is drawn, kept sections included.
 */
import type { RosterTabBar } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { MalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";

/** The class of a row's number slot, which `paintTabNumbers` fills. */
export const TAB_NUMBER_CLASS = "sb-num";

/** The attribute a filled slot carries its number in, for the hook contract and tests. */
export const TAB_NUMBER_ATTRIBUTE = "data-tab-number";

/** Each drawn tab's number, by `WorkspaceRef.id`, counting drawn tabs from 1. */
export function tabNumbers(u: RosterTabBar, path: string): Map<string, number> {
  const numbers = new Map<string, number>();
  const seen = new Set<string>();
  let next = 1;
  u.tabs.forEach((tab, index) => {
    const tabPath = `${path}.tabs[${index}]`;
    const id = requireMessage(tab.workspace, `${tabPath}.workspace`).id;
    if (seen.has(id)) throw new MalformedView(`${tabPath}.workspace`, `workspace ${id} has two tabs`);
    seen.add(id);
    const visibility = requireCase(tab.visibility, `${tabPath}.visibility`);
    switch (visibility.case) {
      case "drawn":
        numbers.set(id, next);
        next += 1;
        return;
      case "withheldCollapsed":
      case "withheldHidden":
        return;
      default: {
        const other: { case: string } = visibility;
        unreachableArm(`${tabPath}.visibility`, other.case);
      }
    }
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

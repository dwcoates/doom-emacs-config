/**
 * recency-gutters — the bar left of an open row's tab number that says how few
 * presses of Emacs's recency walk reach its tab (owner request, 2026-11-19).
 *
 * The daemon carries each reach on the row
 * (`frontend.v1.RosterOpenRow.recency_gutter`), resolved by Emacs with the
 * walk's own stepping, so the bar never disagrees with the press it describes.
 * `s-[` (older) paints the canonical orange and `s-]` (newer) the canonical
 * purple, full at one press and more muted with each press after it; the
 * shade per press is the stylesheet's (`data-recency-presses`).
 *
 * PAINTED AFTER EVERY DRAW, NOT DRAWN WITH THE ROW, as the tab numbers are
 * (`tab-numbers.ts`): every switch moves every reach, so a row draws an empty
 * slot and the roster drawer fills every slot from the push.
 */
import type { RosterRepositoryView } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { openRowWorkspaces } from "./entries.js";
import { decorativeElement } from "../dom.js";
import { rowSlots } from "./row-slots.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";

/** The class of a row's gutter slot, which `paintRecencyGutters` fills. */
export const RECENCY_GUTTER_CLASS = "sb-gutter";

/** The attribute naming which chord reaches the row's tab: `older` or `newer`. */
export const RECENCY_ATTRIBUTE = "data-recency";

/** The attribute carrying how many presses reach it, from 1. */
export const RECENCY_PRESSES_ATTRIBUTE = "data-recency-presses";

/** One row's reach: which chord, and in how many presses. */
export interface RecencyGutter {
  direction: "older" | "newer";
  presses: number;
}

/**
 * Each reached row's gutter, by `WorkspaceRef.id`, read off the Open section's
 * open rows (U). A row with no gutter is absent; a gutter on no arm is malformed.
 */
export function recencyGutters(u: RosterRepositoryView, path: string): Map<string, RecencyGutter> {
  const gutters = new Map<string, RecencyGutter>();
  for (const { id, row, path: rowPath } of openRowWorkspaces(u, path)) {
    if (row.recencyGutter === undefined) continue;
    const reach = requireCase(row.recencyGutter.reach, `${rowPath}.recency_gutter.reach`);
    switch (reach.case) {
      case "older":
      case "newer":
        gutters.set(id, { direction: reach.case, presses: reach.value.presses });
        break;
      default: {
        const other: { case: string } = reach;
        unreachableArm(`${rowPath}.recency_gutter.reach`, other.case);
      }
    }
  }
  return gutters;
}

/** The empty slot a row draws its gutter into. */
export function drawRecencyGutterSlot(): HTMLElement {
  return decorativeElement("span", RECENCY_GUTTER_CLASS);
}

/**
 * Fill every gutter slot under ROOT from GUTTERS, by the slot's row. A row
 * with no reach leaves its slot bare, which draws nothing. Answers how many
 * slots it changed, so a draw that changed none is known to have drawn nothing.
 */
export function paintRecencyGutters(root: ParentNode, gutters: ReadonlyMap<string, RecencyGutter>): number {
  let painted = 0;
  for (const { slot, id } of rowSlots(root, RECENCY_GUTTER_CLASS, "sidebar.recency-gutters.orphan-slot", { gutters: gutters.size })) {
    const gutter = gutters.get(id);
    const direction = gutter?.direction ?? null;
    const presses = gutter === undefined ? null : String(gutter.presses);
    // A slot already standing as the gutter says is left untouched, and not counted.
    if (slot.getAttribute(RECENCY_ATTRIBUTE) === direction && slot.getAttribute(RECENCY_PRESSES_ATTRIBUTE) === presses) continue;
    painted += 1;
    if (gutter === undefined) {
      slot.removeAttribute(RECENCY_ATTRIBUTE);
      slot.removeAttribute(RECENCY_PRESSES_ATTRIBUTE);
      continue;
    }
    slot.setAttribute(RECENCY_ATTRIBUTE, gutter.direction);
    slot.setAttribute(RECENCY_PRESSES_ATTRIBUTE, String(gutter.presses));
  }
  return painted;
}

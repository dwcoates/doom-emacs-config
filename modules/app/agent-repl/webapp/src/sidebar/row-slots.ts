/**
 * row-slots — THE ONE READ of a roster row's painted slots: every slot a row
 * draws empty and the roster drawer fills from the push (the tab numbers, the
 * recency gutters) is found here, each with the workspace of the row it sits
 * in, so no painter can resolve a slot's row some other way.
 */
import { queryAll } from "../dom.js";
import { log } from "../log.js";

/** One painted slot and the `WorkspaceRef.id` of the row it sits in. */
export interface RowSlot {
  slot: HTMLElement;
  id: string;
}

/**
 * Every slot of CLASS_NAME under ROOT, each with its row's workspace id. A slot
 * outside any roster row is a drawing defect: it is recorded at error under
 * OPERATION, with CONTEXT, and thrown.
 */
export function rowSlots(root: ParentNode, className: string, operation: string, context: Record<string, unknown>): RowSlot[] {
  return queryAll<HTMLElement>(root, `.${className}`).map((slot) => {
    const id = slot.closest("[data-roster-row]")?.getAttribute("data-roster-row");
    if (id === undefined || id === null) {
      log.error(`a ${className} slot sits outside any roster row`, { operation, context });
      throw new Error(`sidebar ${className}: a slot outside any roster row`);
    }
    return { slot, id };
  });
}

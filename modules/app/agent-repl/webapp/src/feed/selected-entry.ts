/**
 * selected-entry — THE ONE MARK A SELECTED FEED ENTRY WEARS (owner ruling,
 * 2026-09-23), on the CARD itself rather than on the row's full-width
 * `.feed-item` wrapper.
 *
 * Two acts select an entry: a footer detached-work click lands on it (the
 * reveal, `data-revealed`, feed.ts) and the daemon's feed selection names it
 * (`data-selected-row`, feed-view.ts: a final response or a prompt). They are one
 * concept, so they share one class. Each act states ITS fact on the row's
 * `<article>`, which outlives every redraw of the card inside it, and then
 * calls {@link syncSelectedEntry}, which derives the class on the card from
 * those facts. The chrome mirror calls it again after every body draw, so a
 * card replaced by a push inherits the mark from its row.
 *
 * The card is the row's first element child: the drawn body (a bubble, a tool
 * card, a permission prompt) or a detached bubble's fold. A centered,
 * width-capped card is therefore marked where it is drawn, not at the feed's
 * far left edge.
 */
import { log } from "../log.js";

/** The class the selected entry's CARD wears; the stylesheet rings it. */
export const SELECTED_ENTRY_CLASS = "entry-selected";

/** The attribute a row wears while it is the one a jump landed on. */
export const REVEAL_ATTRIBUTE = "data-revealed";

/**
 * The attribute the row the daemon's feed selection names wears, valued with
 * the row's kind (`response` or `prompt`) — spelled once so a re-push naming a
 * different row can strip it from every other row.
 */
export const SELECTED_ROW_ATTRIBUTE = "data-selected-row";

/** The row facts that each select the entry. */
const SELECTING_ATTRIBUTES = [REVEAL_ATTRIBUTE, SELECTED_ROW_ATTRIBUTE] as const;

/** The card ROW draws: its first element child, or null for an empty row. */
export function cardOf(row: HTMLElement): HTMLElement | null {
  const card = row.firstElementChild;
  return card instanceof HTMLElement ? card : null;
}

/**
 * Put the selected-entry class on ROW's card iff one of the selecting facts
 * stands on ROW, and take it off otherwise.
 */
export function syncSelectedEntry(row: HTMLElement): void {
  const selected = SELECTING_ATTRIBUTES.some((attribute) => row.hasAttribute(attribute));
  const card = cardOf(row);
  if (card === null) {
    if (selected) {
      log.debug("a selected row draws no card; there is nothing to mark", {
        operation: "feed.selected-entry-no-card",
        context: { row: row.getAttribute("data-feed-row") ?? "unset" },
      });
    }
    return;
  }
  card.classList.toggle(SELECTED_ENTRY_CLASS, selected);
}

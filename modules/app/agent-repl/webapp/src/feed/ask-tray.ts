/**
 * ask-tray — AN OPEN QUESTION OR PERMISSION SITS BELOW THE HELD PROMPTS.
 *
 * An open ask is the choice the agent is waiting on now, and a held prompt is
 * what the reader already said and is waiting to send: the ask is the more
 * urgent of the two, so it is drawn last, at the very tail of the scroll zone,
 * in `#ask-tray` after the hold tray (owner request, 2026-10-25).
 *
 * THE CARD MOVES; THE ROW STAYS (card-dock.ts). Each docked card is drawn in a
 * row wrapper of its own (`.feed-item`), so it lays out exactly as it did in
 * the feed; once answered or expired the feed redraws the row, and the card
 * that redraw draws stays in the row, where the conversation's history is.
 *
 * Only a ROOT-feed ask docks: a sub-feed's asks belong to their bubble.
 */
import { log } from "../log.js";
import { installCardDock, type CardDock } from "./card-dock.js";

/** The class a docked ask card wears. */
export const DOCKED_ASK_CLASS = "ask-docked";
/** The class a docked ask's (hidden) home row wears. */
export const DOCKED_ASK_ROW_CLASS = "ask-docked-row";
/** The class of the row wrapper each docked ask is drawn in. */
export const ASK_SLOT_CLASS = "ask-tray-slot";

/** An open question or permission card (asks/question.ts, asks/permission.ts). */
const OPEN_ASK = '.ask-card[data-state="open"]';

/** The row wrapper a docked ask is drawn in. */
function askSlot(card: HTMLElement): HTMLElement {
  const slot = card.ownerDocument.createElement("div");
  slot.className = `feed-item ${ASK_SLOT_CLASS}`;
  return slot;
}

/** Record one ask's docking. */
function recordDocked(card: HTMLElement): void {
  log.info("an ask stands; its card is drawn below the held prompts", {
    operation: "feed.ask-tray.docked",
    context: { kind: card.classList.contains("permission") ? "permission" : "question" },
  });
}

/**
 * Draw the root FEED's open asks in TRAY, below the held prompts, while they
 * stand.
 */
export function installAskTray(feed: HTMLElement, tray: HTMLElement): CardDock {
  return installCardDock(feed, tray, {
    operation: "feed.ask-tray",
    noun: "ask's card",
    standing: OPEN_ASK,
    stateAttributes: ["data-state"],
    cardClass: DOCKED_ASK_CLASS,
    rowClass: DOCKED_ASK_ROW_CLASS,
    slot: askSlot,
    docked: recordDocked,
  });
}

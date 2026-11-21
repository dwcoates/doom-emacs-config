/**
 * gate-dock — THE STANDING GATE DOCKS AT THE PAGE'S VERY BOTTOM.
 *
 * A gate is a choice the user must answer before the conversation can go on,
 * and whose answer REPLACES the composer (owner, 2026-10-02). In Emacs the
 * webhost draws the shown workspace's gate (host/gate.ts) in the slot of the
 * input window Emacs hides, so a page in the webhost docks nothing. A page on
 * its own (a browser tab) docks its root feed's standing `FeedColdGate` row
 * here instead: `#gate-dock`, below the footer, its card's own height,
 * growing to fit a taller gate.
 *
 * THE CARD MOVES; THE ROW STAYS (card-dock.ts): the feed draws no inline
 * gate, and the card keeps every control and listener it was drawn with. The
 * row is retired when the gate is answered or retracted, and the dock hides.
 *
 * Only a ROOT-feed gate docks: a sub-feed's rows belong to their bubble.
 */
import { createLogger } from "../log.js";
import { installCardDock, type CardDock } from "./card-dock.js";

const log = createLogger("webapp.feed.gate-dock");

/**
 * The class a page's root wears while the webhost draws its gate
 * (host/gate.ts): the page draws no standing gate, in its feed or its dock.
 */
export const WEBHOST_GATE_CLASS = "webhost-gate";

/** The class the docked card wears. */
export const DOCKED_CARD_CLASS = "cold-gate-docked";
/** The class the docked card's (hidden) home row wears. */
export const DOCKED_ROW_CLASS = "gate-docked-row";
/** The custom property the webhost's gate takes the composer's told height from. */
export const DOCK_HEIGHT_PROPERTY = "--gate-dock-height";

/** What `installGateDock` hands back. */
export type GateDock = CardDock;

/** Record a gate's docking. A page on its own has no composer slot to take,
 * so the dock is its card's own height. */
function recordDocked(): void {
  log.info("a gate stands; its banner docks below the footer", {
    operation: "feed.gate-dock.docked",
  });
}

/**
 * Dock the root FEED's standing gate in DOCK while it stands. Watches both, so
 * every upsert, retirement and resolution is seen.
 */
export function installGateDock(feed: HTMLElement, dock: HTMLElement): GateDock {
  return installCardDock(feed, dock, {
    operation: "feed.gate-dock",
    noun: "gate's banner",
    standing: '.cold-gate[data-state="standing"]',
    stateAttributes: ["data-state"],
    cardClass: DOCKED_CARD_CLASS,
    rowClass: DOCKED_ROW_CLASS,
    docked: recordDocked,
  });
}

/**
 * The page's part in drawing its gate. A page IN THE WEBHOST (EMBEDDED) draws
 * no standing gate: the webhost draws it (host/gate.ts), and the page's root
 * wears `WEBHOST_GATE_CLASS`, which hides the standing row. A page on its own
 * docks its gate in DOCK. Answers the dock installed, or null in the webhost.
 */
export function installPageGate(doc: Document, embedded: boolean, feed: HTMLElement, dock: HTMLElement): GateDock | null {
  if (embedded) {
    doc.documentElement.classList.add(WEBHOST_GATE_CLASS);
    log.info("the page is in the webhost, which draws its gate; the page draws no standing gate", {
      operation: "feed.gate-dock.webhost",
    });
    return null;
  }
  return installGateDock(feed, dock);
}

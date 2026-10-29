/**
 * gate-dock — THE STANDING GATE DOCKS AT THE PAGE'S VERY BOTTOM.
 *
 * A gate is a choice the user must answer before the conversation can go on,
 * and whose answer REPLACES the composer (owner, 2026-10-02). While one
 * stands, Emacs hides the workspace's input window (it reads
 * `HostWorkspace.gate`) and this webview grows into that space; the gate's
 * banner is drawn in the exact slot the input had: `#gate-dock`, below the
 * footer, at least the input window's height and growing to fit a taller
 * gate. The daemon resolves both from the ONE cold-gate standing, which
 * this page reads as the root feed's standing `FeedColdGate` row, so the
 * hidden input and the docked banner move together.
 *
 * THE CARD MOVES; THE ROW STAYS (card-dock.ts): the feed draws no inline
 * gate, and the card keeps every control and listener it was drawn with. The
 * row is retired when the gate is answered or retracted, and the dock hides.
 *
 * Only a ROOT-feed gate docks: a sub-feed's rows belong to their bubble.
 */
import { log } from "../log.js";
import { installCardDock, type CardDock } from "./card-dock.js";

/**
 * The class a page's root wears while the webhost draws its gate
 * (host/gate.ts): the page draws no standing gate, in its feed or its dock.
 */
export const WEBHOST_GATE_CLASS = "webhost-gate";

/** The class the docked card wears. */
export const DOCKED_CARD_CLASS = "cold-gate-docked";
/** The class the docked card's (hidden) home row wears. */
export const DOCKED_ROW_CLASS = "gate-docked-row";
/** The root custom property Emacs sets to the input window's pixel height. */
export const DOCK_HEIGHT_PROPERTY = "--gate-dock-height";

/** What Emacs set PROPERTY to on the root, or null when it never told it. */
function toldRootProperty(doc: Document, property: string): string | null {
  const told = doc.documentElement.style.getPropertyValue(property).trim();
  return told === "" ? null : told;
}

/** The height the dock takes: what Emacs said, or null when it said nothing
 * and the stylesheet's fraction stands in. */
export function toldDockHeight(doc: Document): string | null {
  return toldRootProperty(doc, DOCK_HEIGHT_PROPERTY);
}


/** What `installGateDock` hands back. */
export type GateDock = CardDock;

/** Record a gate's docking: the height Emacs told the dock. */
function recordDocked(_card: HTMLElement, dock: HTMLElement): void {
  const height = toldDockHeight(dock.ownerDocument);
  if (height === null) {
    // Emacs tells the input's height before it hides the input; a page it
    // never told sizes the dock by the input-height fraction instead.
    log.info("a gate stands; Emacs never told the input's height, so the dock takes the input-height fraction", {
      operation: "feed.gate-dock.height-fallback",
    });
  }
  log.info("a gate stands; its banner docks below the footer", {
    operation: "feed.gate-dock.docked",
    context: { height: height ?? "fallback" },
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

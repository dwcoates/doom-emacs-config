/**
 * gate-dock — THE STANDING GATE DOCKS AT THE PAGE'S VERY BOTTOM.
 *
 * A gate is a choice the user must answer before the conversation can go on,
 * and whose answer REPLACES the composer (owner, 2026-10-02). While one
 * stands, Emacs hides the workspace's input window (it reads
 * `HostWorkspace.gate`) and this webview grows into that space; the gate's
 * banner is drawn in the exact slot the input had: `#gate-dock`, below the
 * footer, at the input window's height. The daemon resolves both from the ONE
 * cold-gate standing, which this page reads as the root feed's standing
 * `FeedColdGate` row, so the hidden input and the docked banner move together.
 *
 * THE CARD MOVES; THE ROW STAYS. The feed view owns the row element (upserts,
 * retirement, positional inserts), so the row stays in the feed, hidden, and
 * only its card is moved into the dock: the feed draws no inline gate, and
 * the card keeps every control and listener it was drawn with.
 *
 * Every edge is read from the DOM the feed view leaves:
 *   - a standing card appears in a root row: it is docked;
 *   - the feed redraws that row (a fresh standing card in it): the stale card
 *     leaves the dock and the fresh one takes its place;
 *   - the row is retired (answered or retracted): the card leaves the dock,
 *     the dock hides;
 *   - the card is resolved in place: it goes back to its row, which shows.
 *
 * Only a ROOT-feed gate docks: a sub-feed's rows belong to their bubble.
 */
import { log } from "../log.js";

/** The class the docked card wears. */
export const DOCKED_CARD_CLASS = "cold-gate-docked";
/** The class the docked card's (hidden) home row wears. */
export const DOCKED_ROW_CLASS = "gate-docked-row";
/** The root custom property Emacs sets to the input window's pixel height. */
export const DOCK_HEIGHT_PROPERTY = "--gate-dock-height";
/** The root custom property Emacs sets to the input window's background. */
export const DOCK_BACKGROUND_PROPERTY = "--input-bg";

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

/** The background the docked card takes: the input window's, as Emacs said,
 * or null when it said nothing and the card's own background stands in. */
export function toldDockBackground(doc: Document): string | null {
  return toldRootProperty(doc, DOCK_BACKGROUND_PROPERTY);
}

/** What `installGateDock` hands back. */
export interface GateDock {
  /** Stop watching, and undock whatever is docked. */
  dispose(): void;
}

/** A docked gate: the card, its home row, and where in the row it sat. */
interface Docked {
  card: HTMLElement;
  row: HTMLElement;
  parent: HTMLElement;
  next: Node | null;
}

const STANDING = '.cold-gate[data-state="standing"]';

/** Every feed row's own wrapper (feed-view.ts). */
const ROW = ".feed-item";

/**
 * The root feed's ROW holding CARD: its nearest row wrapper, null when CARD is
 * not on the root feed itself or sits in no row.
 *
 * THE ROW, NEVER AN ANCESTOR OF ROWS. The feed nests its rows inside a body
 * (`#feed > .feed-body > .feed-item`); walking up to FEED's direct child named
 * that body as the gate's row, and hiding it hid the whole conversation behind
 * every standing gate (owner's report, 2026-10-03).
 */
function rootRowOf(feed: HTMLElement, card: HTMLElement): HTMLElement | null {
  if (card.closest("[data-feed]") !== feed) return null;
  const row = card.closest<HTMLElement>(ROW);
  return row !== null && feed.contains(row) ? row : null;
}

/** A standing gate card on the root feed itself, with its row; null when none. */
function standingRootGate(feed: HTMLElement): { card: HTMLElement; row: HTMLElement } | null {
  for (const card of feed.querySelectorAll<HTMLElement>(STANDING)) {
    const row = rootRowOf(feed, card);
    if (row !== null) return { card, row };
  }
  return null;
}

/**
 * Dock the root FEED's standing gate in DOCK while it stands. Watches both, so
 * every upsert, retirement and resolution is seen.
 */
export function installGateDock(feed: HTMLElement, dock: HTMLElement): GateDock {
  let docked: Docked | null = null;

  /** Take the docked card out; RETURN puts it back in its row. */
  const undock = (why: string, returnCard: boolean): void => {
    if (docked === null) return;
    const { card, row, parent, next } = docked;
    docked = null;
    card.classList.remove(DOCKED_CARD_CLASS);
    row.classList.remove(DOCKED_ROW_CLASS);
    if (returnCard && parent.isConnected) {
      parent.insertBefore(card, next !== null && next.parentNode === parent ? next : null);
    } else {
      card.remove();
    }
    dock.hidden = true;
    log.info("the gate's banner undocks", { operation: "feed.gate-dock.undocked", context: { why } });
  };

  const dockCard = (card: HTMLElement, row: HTMLElement): void => {
    const parent = card.parentElement;
    if (parent === null) return;
    docked = { card, row, parent, next: card.nextSibling };
    row.classList.add(DOCKED_ROW_CLASS);
    card.classList.add(DOCKED_CARD_CLASS);
    dock.replaceChildren(card);
    dock.hidden = false;
    const height = toldDockHeight(dock.ownerDocument);
    if (height === null) {
      // Emacs tells the input's height before it hides the input; a page it
      // never told sizes the dock by the input-height fraction instead.
      log.info("a gate stands; Emacs never told the input's height, so the dock takes the input-height fraction", {
        operation: "feed.gate-dock.height-fallback",
      });
    }
    const background = toldDockBackground(dock.ownerDocument);
    if (background === null) {
      // Emacs tells the input's background with its height; a page it never
      // told keeps the card's own background instead.
      log.info("a gate stands; Emacs never told the input's background, so the docked card keeps its own", {
        operation: "feed.gate-dock.background-fallback",
      });
    }
    log.info("a gate stands; its banner docks below the footer", {
      operation: "feed.gate-dock.docked",
      context: { height: height ?? "fallback", background: background ?? "fallback" },
    });
  };

  const sync = (): void => {
    const fresh = standingRootGate(feed);
    if (fresh !== null) {
      undock("redrawn", false);
      dockCard(fresh.card, fresh.row);
      return;
    }
    if (docked === null) return;
    if (!docked.row.isConnected || !feed.contains(docked.row)) {
      undock("retired", false);
      return;
    }
    if (docked.card.getAttribute("data-state") !== "standing") undock("resolved", true);
  };

  const mutations = new MutationObserver(sync);
  const watch = {
    subtree: true,
    childList: true,
    attributes: true,
    attributeFilter: ["data-state"],
  };
  mutations.observe(feed, watch);
  mutations.observe(dock, watch);
  sync();

  return {
    dispose(): void {
      mutations.disconnect();
      undock("disposed", true);
    },
  };
}

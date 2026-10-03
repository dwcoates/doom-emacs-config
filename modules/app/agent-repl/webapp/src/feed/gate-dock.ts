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

/** The root row (FEED's direct child) holding CARD, null when CARD is not on
 * the root feed itself. */
function rootRowOf(feed: HTMLElement, card: HTMLElement): HTMLElement | null {
  if (card.closest("[data-feed]") !== feed) return null;
  let row: HTMLElement = card;
  while (row.parentElement !== null && row.parentElement !== feed) row = row.parentElement;
  return row.parentElement === feed ? row : null;
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
    log.info("a gate stands; its banner docks below the footer", { operation: "feed.gate-dock.docked" });
  };

  const sync = (): void => {
    const fresh = standingRootGate(feed);
    if (fresh !== null) {
      undock("redrawn", false);
      dockCard(fresh.card, fresh.row);
      return;
    }
    if (docked === null) return;
    if (docked.row.parentElement !== feed) {
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

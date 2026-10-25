/**
 * card-dock — A STANDING CARD ON THE ROOT FEED IS DRAWN OUTSIDE IT.
 *
 * Some cards stand for a choice the reader must make, and the page draws them
 * somewhere other than their row while they stand: the cold gate in the dock
 * below the footer (gate-dock.ts), and an open question or permission after
 * the held prompts (ask-tray.ts). This is the one mechanism both use.
 *
 * THE CARD MOVES; THE ROW STAYS. The feed view owns the row element (upserts,
 * retirement, positional inserts), so the row stays in the feed, hidden, and
 * only its card is moved into the dock: the card keeps every control and
 * listener it was drawn with, and the feed view still holds it by reference,
 * so a redraw removes it from the dock exactly as it would from the row.
 *
 * Every edge is read from the DOM the feed view leaves:
 *   - a standing card appears in a root row: it is docked;
 *   - the feed redraws that row (its docked card removed, a fresh card drawn
 *     in the row): the stale card is gone, and a fresh standing card docks;
 *   - the row is retired: the card leaves the dock;
 *   - the card is resolved in place: it goes back to its row, which shows.
 *
 * The dock holds its cards in their rows' feed order, and is hidden while it
 * holds none. Only a ROOT-feed card docks: a sub-feed's rows belong to their
 * bubble.
 */
import { log } from "../log.js";
import { pageMutationObserver, queryAll } from "../dom.js";

/** How one kind of standing card docks. */
export interface CardDockSpec {
  /** The log operation prefix of this dock (`<operation>.undocked`). */
  operation: string;
  /** What a card leaving this dock is, for the undock record's message. */
  noun: string;
  /** The selector a standing card matches; a card that stops matching is resolved. */
  standing: string;
  /** The attributes whose change can resolve a card in place. */
  stateAttributes: readonly string[];
  /** The class the docked card wears. */
  cardClass: string;
  /** The class the docked card's (hidden) home row wears. */
  rowClass: string;
  /**
   * The element the card is drawn in inside the dock: the card itself, or a
   * slot wrapping it. Absent, the card is the dock's own child.
   */
  slot?: (card: HTMLElement) => HTMLElement;
  /** Record (and finish) one card's docking. */
  docked: (card: HTMLElement, dock: HTMLElement) => void;
}

/** What `installCardDock` hands back. */
export interface CardDock {
  /** Stop watching, and return every docked card to its row. */
  dispose(): void;
}

/** A docked card: the card, the element it is drawn in, its home row, and where in the row it sat. */
interface Docked {
  card: HTMLElement;
  slot: HTMLElement;
  row: HTMLElement;
  parent: HTMLElement;
  next: Node | null;
}

/** Every feed row's own wrapper (feed-view.ts). */
const ROW = ".feed-item";

/**
 * The root feed's ROW holding CARD: its nearest row wrapper, null when CARD is
 * not on the root feed itself or sits in no row.
 *
 * THE ROW, NEVER AN ANCESTOR OF ROWS. The feed nests its rows inside a body
 * (`#feed > .feed-body > .feed-item`); walking up to FEED's direct child named
 * that body as the card's row, and hiding it hid the whole conversation behind
 * every standing gate (owner's report, 2026-10-03).
 */
export function rootRowOf(feed: HTMLElement, card: HTMLElement): HTMLElement | null {
  if (card.closest("[data-feed]") !== feed) return null;
  const row = card.closest<HTMLElement>(ROW);
  return row !== null && feed.contains(row) ? row : null;
}

/** Every standing card on the root FEED itself, with its row, in feed order. */
export function standingRootCards(
  feed: HTMLElement,
  standing: string,
): Array<{ card: HTMLElement; row: HTMLElement }> {
  const found: Array<{ card: HTMLElement; row: HTMLElement }> = [];
  for (const card of queryAll<HTMLElement>(feed, standing)) {
    const row = rootRowOf(feed, card);
    if (row !== null) found.push({ card, row });
  }
  return found;
}

/** Whether A's row comes before B's in the feed. */
function feedOrder(a: Docked, b: Docked): number {
  if (a.row === b.row) return 0;
  return a.row.compareDocumentPosition(b.row) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1;
}

/**
 * Dock the root FEED's standing cards in DOCK while they stand. Watches both,
 * so every upsert, retirement and resolution is seen.
 */
export function installCardDock(feed: HTMLElement, dock: HTMLElement, spec: CardDockSpec): CardDock {
  let docked: Docked[] = [];

  /** Take ENTRY's card out of the dock; RETURN puts it back in its row. */
  const undock = (entry: Docked, why: string, returnCard: boolean): void => {
    const { card, slot, row, parent, next } = entry;
    card.classList.remove(spec.cardClass);
    row.classList.remove(spec.rowClass);
    if (returnCard && parent.isConnected) {
      parent.insertBefore(card, next !== null && next.parentNode === parent ? next : null);
    } else {
      card.remove();
    }
    if (slot !== card) slot.remove();
    log.info(`the ${spec.noun} undocks`, { operation: `${spec.operation}.undocked`, context: { why } });
  };

  const dockCard = (card: HTMLElement, row: HTMLElement): Docked | null => {
    const parent = card.parentElement;
    if (parent === null) return null;
    const entry: Docked = { card, slot: card, row, parent, next: card.nextSibling };
    row.classList.add(spec.rowClass);
    card.classList.add(spec.cardClass);
    if (spec.slot !== undefined) {
      entry.slot = spec.slot(card);
      entry.slot.append(card);
    }
    return entry;
  };

  /** Draw the docked cards in DOCK in feed order, touching it only when it differs. */
  const arrange = (): void => {
    docked.sort(feedOrder);
    const want = docked.map((entry) => entry.slot);
    const have = [...dock.children];
    if (want.length !== have.length || want.some((slot, i) => slot !== have[i])) {
      dock.replaceChildren(...want);
    }
    const hidden = docked.length === 0;
    if (dock.hidden !== hidden) dock.hidden = hidden;
  };

  const sync = (): void => {
    docked = docked.filter((entry) => {
      if (entry.card.parentElement !== (entry.slot === entry.card ? dock : entry.slot)) {        // The feed redrew the row and removed the card it held by reference.
        undock(entry, "redrawn", false);
        return false;
      }
      if (!entry.row.isConnected || !feed.contains(entry.row)) {
        undock(entry, "retired", false);
        return false;
      }
      if (!entry.card.matches(spec.standing)) {
        undock(entry, "resolved", true);
        return false;
      }
      return true;
    });
    const fresh: Docked[] = [];
    for (const { card, row } of standingRootCards(feed, spec.standing)) {
      const entry = dockCard(card, row);
      if (entry !== null) fresh.push(entry);
    }
    docked.push(...fresh);
    arrange();
    for (const entry of fresh) spec.docked(entry.card, dock);
  };

  const mutations = pageMutationObserver(feed.ownerDocument, sync);
  const watch = {
    subtree: true,
    childList: true,
    attributes: true,
    attributeFilter: [...spec.stateAttributes],
  };
  mutations.observe(feed, watch);
  mutations.observe(dock, watch);
  sync();

  return {
    dispose(): void {
      mutations.disconnect();
      for (const entry of docked) undock(entry, "disposed", true);
      docked = [];
      arrange();
    },
  };
}

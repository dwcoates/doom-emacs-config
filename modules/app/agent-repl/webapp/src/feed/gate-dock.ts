/**
 * gate-dock — THE STANDING GATE DOCKS AT THE BOTTOM.
 *
 * A gate is a choice the user must answer before the conversation can go on,
 * and whose answer REPLACES the composer (owner, 2026-10-02). While one
 * stands, Emacs hides the workspace's input window (it reads
 * `HostWorkspace.gate`) and this webview, grown into that space, keeps the
 * gate's banner docked at the bottom, at least the input's height. The daemon
 * resolves both from the ONE cold-gate standing: this page reads it as the
 * root feed's standing `FeedColdGate` row, so the hidden input and the docked
 * banner move together.
 *
 * THE ROW STAYS THE FEED'S. The feed view owns the row's element (upserts,
 * retirement, positional inserts), so this module never moves it: it marks the
 * row `gate-docked-row` and the card `cold-gate-docked`, and the stylesheet
 * pins the row to the scroll zone's bottom edge. The moment the row is retired
 * or resolved the marks come off.
 *
 * Only a ROOT-feed gate docks: a sub-feed's rows belong to their bubble.
 */
import { log } from "../log.js";

/** The class the standing card wears while docked. */
export const DOCKED_CARD_CLASS = "cold-gate-docked";
/** The class the card's root row wears while docked. */
export const DOCKED_ROW_CLASS = "gate-docked-row";

/** What `installGateDock` hands back. */
export interface GateDock {
  /** Stop watching, and undock whatever is docked. */
  dispose(): void;
}

/** A docked gate: the card, and the root row holding it. */
interface Docked {
  card: HTMLElement;
  row: HTMLElement;
}

/** The root row (FEED's direct child) holding CARD, null when CARD is not on
 * the root feed itself. */
function rootRowOf(feed: HTMLElement, card: HTMLElement): HTMLElement | null {
  if (card.closest("[data-feed]") !== feed) return null;
  let row: HTMLElement = card;
  while (row.parentElement !== null && row.parentElement !== feed) row = row.parentElement;
  return row.parentElement === feed ? row : null;
}

/** The root feed's standing gate, null when none stands. */
function standingRootGate(feed: HTMLElement): Docked | null {
  for (const card of feed.querySelectorAll<HTMLElement>('.cold-gate[data-state="standing"]')) {
    const row = rootRowOf(feed, card);
    if (row !== null) return { card, row };
  }
  return null;
}

/**
 * Dock the root feed's standing gate at the bottom while it stands. Watches
 * FEED's subtree, so every upsert, retirement and resolution is seen.
 */
export function installGateDock(feed: HTMLElement): GateDock {
  let docked: Docked | null = null;

  const undock = (): void => {
    if (docked === null) return;
    docked.card.classList.remove(DOCKED_CARD_CLASS);
    docked.row.classList.remove(DOCKED_ROW_CLASS);
    docked = null;
    log.info("the gate is gone; the banner undocks", { operation: "feed.gate-dock.undocked" });
  };

  const sync = (): void => {
    const standing = standingRootGate(feed);
    if (standing?.card === docked?.card && standing?.row === docked?.row) return;
    undock();
    if (standing === null) return;
    docked = standing;
    standing.card.classList.add(DOCKED_CARD_CLASS);
    standing.row.classList.add(DOCKED_ROW_CLASS);
    log.info("a gate stands; its banner docks at the bottom", {
      operation: "feed.gate-dock.docked",
    });
  };

  const mutations = new MutationObserver(sync);
  mutations.observe(feed, {
    subtree: true,
    childList: true,
    attributes: true,
    attributeFilter: ["data-state"],
  });
  sync();

  return {
    dispose(): void {
      mutations.disconnect();
      undock();
    },
  };
}

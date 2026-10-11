/**
 * section — THE ROOT FEED'S HELD SECTION (frontend.v1 FeedHeld): what has not
 * yet settled into the conversation's history. A prompt the daemon holds for
 * delivery, a question waiting for an answer. (A running merge is not held: its
 * bubble is the row pinned to the feed's bottom, FeedRowOrder.pinned.)
 *
 * PART OF THE FEED, DRAWN AT ITS TAIL. The section rides the root feed's own
 * connection (OpenFeed's answer, WatchFeed's held-section frames), and the
 * feed hands each state of it to `apply`. Its host sits after the rows inside
 * the same scroll zone, with no heading and no divider, so a held item reads
 * exactly as the feed's newest row does, and its growth is ordinary growth at
 * the tail, which the feed's follow keeps in view.
 *
 * DRAWN IN PLACE, KEYED BY THE ITEM'S ID. Every push carries the section whole,
 * and an item that left is simply absent. Each item keeps its element across
 * pushes: a held prompt's card is updated in place over its previous drawing
 * (`drawHeldPrompt` over the old card), and nothing already in its place
 * moves (`placeChildren`); only what left is dropped, its clocks stopped. The
 * cards' queued-at ages subscribe through `HeldContext.onDispose`, cleared and
 * re-taken on every push.
 *
 * A HELD PROMPT'S FIRST DRAW PARKS THE FEED (owner ruling, 2026-09-23). When a
 * push draws a held prompt's card that the previous drawing did not hold — one
 * this client just sent that the daemon held included — the feed jumps to its
 * tail and follows, exactly as a sent prompt does (`promptHeld`, scroll.ts),
 * AFTER the card is placed so the tail includes it. A re-push of a card already
 * drawn, and a card's removal, move nothing.
 *
 * THE ONE TOGGLE. The section's host sits outside `#feed`, so the feed's click
 * handler never reaches it: the section arms the same one
 * (`installClickExpand`) on its own host, with the same has-more refresh after
 * each toggle.
 */
import type { FeedHeld, FeedHeldItem } from "../../../proto/gen/ts/frontend/v1/feed_held_pb";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { HeldContext } from "./context.js";
import { drawHeldOffer } from "./held-offer.js";
import { drawHeldPrompt } from "./held-prompt.js";
import { ClassifierUpdateForms } from "./classifier-update.js";
import { placeChildren } from "../dom.js";
import { installClickExpand } from "../expand.js";
import { refreshHasMore } from "../feed/bubble-more.js";
import { stopTicking } from "../feed/ticking.js";

/** What the section is handed by the feed that owns it. */
export interface HeldSectionDeps {
  /**
   * A held prompt's card was drawn for the first time, and is placed: park the
   * feed at its tail and follow (`promptHeld`, scroll.ts).
   */
  promptHeld(turn: string): void;
}

/** The root feed's held section, as the feed drives it. */
export interface HeldSection {
  /** Draw SECTION, whole, over the previous drawing. */
  apply(section: FeedHeld): void;
  dispose(): void;
}

/** The section region's class, and its list's. */
const SECTION_REGION_CLASS = "held-section";
const SECTION_LIST_CLASS = "held-section-items";

/** The attribute every held item's element carries: its FeedHeldItem id. */
export const HELD_ITEM_ATTRIBUTE = "data-held-item";

/**
 * Every held entry the section draws, in the daemon's order: the list's own
 * items. The section sits after the feed in the scroll zone, so its last item,
 * when there is one, is the feed column's latest entry (`latestEntry`, feed.ts).
 */
export const HELD_ENTRY_SELECTOR = `.${SECTION_LIST_CLASS} > *`;

/**
 * Mount the held section on HOST. It draws nothing until the feed applies a
 * section, and every push after that is the feed's too: the section has no
 * stream of its own.
 */
export function mountHeldSection(host: HTMLElement, ctx: AppContext, deps: HeldSectionDeps): HeldSection {
  log.debug("mounting the held section", { operation: "held.mount" });
  // The one click-to-expand every bubble opens by, and the has-more refresh a
  // toggle that moved no height needs (bubble-more.ts).
  const uninstallExpand = installClickExpand(host, undefined, (section) => refreshHasMore(section));

  // The classifier update forms OUTLIVE every push (see HeldContext).
  const classifierForms = new ClassifierUpdateForms(ctx);

  /** Teardowns the CURRENT drawing owns; replaced wholesale on every push. */
  let disposers: Array<() => void> = [];
  const clear = (): void => {
    for (const fn of disposers) fn();
    disposers = [];
  };

  const apply = (section: FeedHeld): void => {
    // The teardowns come down BEFORE the draw, so a card whose subscription
    // is about to be replaced cannot tick a node already detached.
    clear();
    const tc: HeldContext = {
      ctx,
      onDispose: (fn) => {
        disposers.push(fn);
      },
      classifierForms,
    };
    const before = heldTurns(host);
    const current = host.firstElementChild instanceof HTMLElement ? host.firstElementChild : null;
    const drawn = drawSection(section, tc, current);
    if (current !== null && current !== drawn) stopTicking(current);
    // AN EMPTY SECTION DRAWS NOTHING (owner ruling 3, 2026-09-13), so the host
    // is emptied rather than given a region: `#hold-tray:empty` is what
    // collapses the space, and it only matches a host with no children at all.
    placeChildren(host, drawn === null ? [] : [drawn]);
    // A form whose card left with this push goes with it.
    classifierForms.retain(heldTurns(host));
    // Only a FIRST draw parks, and only once it is placed; one park covers
    // every card the push landed, since each parks at the same tail.
    const landed = heldTurns(host).filter((turn) => !before.includes(turn));
    if (landed.length === 0) return;
    log.debug("a held prompt landed in the held section", {
      operation: "held.held-prompt-landed",
      context: { turns: landed },
    });
    deps.promptHeld(landed[landed.length - 1]);
  };

  return {
    apply,
    dispose(): void {
      log.debug("disposing the held section", { operation: "held.dispose" });
      clear();
      uninstallExpand();
      for (const child of host.children) stopTicking(child);
      host.replaceChildren();
    },
  };
}

/**
 * The section, whole: the held items, in the order the daemon served them —
 * or `null` when nothing is held.
 *
 * AN EMPTY SECTION DRAWS NOTHING (owner ruling 3, 2026-09-13). Not a "nothing
 * held" line, not an empty region: the answer to "what is the daemon holding
 * for you" when it is holding nothing is silence. `null` rather than an empty
 * element, because `#hold-tray:empty` collapses the region only while the host
 * has NO children, so an empty wrapper would still pay layout.
 */
export function drawSection(
  u: FeedHeld,
  tc: HeldContext,
  previous: HTMLElement | null,
): HTMLElement | null {
  const path = "FeedHeld";
  log.debug("drawing the held section", {
    operation: "held.draw",
    context: { items: u.items.length, in_place: previous !== null },
  });

  if (u.items.length === 0) return null;

  // THE PREVIOUS DRAWING IS REUSED, region and list alike, so an item updated
  // in place never leaves the document (see the file header).
  const reused = previous?.classList.contains(SECTION_REGION_CLASS) === true ? previous : null;
  const region = reused ?? document.createElement("div");
  region.className = SECTION_REGION_CLASS;
  let list = region.querySelector<HTMLElement>(`:scope > .${SECTION_LIST_CLASS}`);
  if (list === null) {
    list = document.createElement("div");
    region.appendChild(list);
  }
  list.className = SECTION_LIST_CLASS;

  const cards = new Map<string, HTMLElement>();
  for (const card of list.querySelectorAll<HTMLElement>(`:scope > [${HELD_ITEM_ATTRIBUTE}]`)) {
    cards.set(card.getAttribute(HELD_ITEM_ATTRIBUTE) ?? "", card);
  }
  const items = u.items.map((item, index) =>
    drawHeldItem(item, tc, `${path}.items[${index.toString()}]`, cards),
  );
  const kept = new Set<Element>(items);
  for (const child of list.children) {
    if (!kept.has(child)) stopTicking(child);
  }
  placeChildren(list, items);
  return region;
}

/**
 * The turns of every held prompt card HOST currently holds, in order. The
 * selector matches only cards carrying the attribute, so a card that answers
 * none is a broken DOM, thrown rather than read as some default turn.
 */
function heldTurns(host: HTMLElement): string[] {
  return [...host.querySelectorAll(`.${SECTION_LIST_CLASS} > [data-held-turn]`)].map((card) => {
    const turn = card.getAttribute("data-held-turn");
    if (turn === null) throw new Error("a held prompt card lost its data-held-turn while it was read");
    return turn;
  });
}

/**
 * One held item. THE ARM IS THE KIND.
 *
 * A prompt is released or dropped and an offer is answered, so the routing is
 * on the arm and never on anything inside it. CARDS are the previous drawing's
 * items by id.
 */
export function drawHeldItem(
  u: FeedHeldItem,
  tc: HeldContext,
  path: string,
  cards: ReadonlyMap<string, HTMLElement>,
): HTMLElement {
  const id = requireMessage(u.id, `${path}.id`);
  const item = requireCase(u.item, `${path}.item`);
  log.debug("drawing a held item", {
    operation: "held.item",
    context: { path, item: item.case, id: id.value },
  });
  let element: HTMLElement;
  switch (item.case) {
    case "prompt":
      // This item's card from the last drawing, updated in place.
      element = drawHeldPrompt(item.value, tc, cards.get(id.value));
      break;
    case "offer":
      element = drawHeldOffer(item.value, tc);
      break;
    default: {
      const other: { case: string } = item;
      return unreachableArm(`${path}.item`, other.case);
    }
  }
  element.setAttribute(HELD_ITEM_ATTRIBUTE, id.value);
  return element;
}


/**
 * tray — the daemon-hold tray: what the daemon is holding FOR the user.
 *
 * ITS OWN REGION AT THE FEED'S TAIL, inside the same scroll zone, so a reader
 * at the bottom of the conversation sees the held prompts sitting after the
 * last row rather than as chrome docked beneath it. Nothing here is a feed row
 * — a held prompt is daemon-owned pending intent the vendor never saw.
 *
 * WHOLE-LIST PUSHED, DRAWN IN PLACE. Every push carries the tray entire, and
 * an item that left is simply absent from the next push — deletion is row
 * omission, never an event. But a held prompt is a bubble a reader may have
 * opened and scrolled (owner rule, 2026-09-23: the user owns the scroll), so a
 * redraw updates each held prompt's card IN PLACE, matched by its echoed turn
 * (`drawBubble` over the previous card), and moves nothing already in its
 * place (`placeChildren`); only what left is dropped, its clocks stopped. The
 * cards' queued-at ages subscribe through `TrayContext.onDispose`, cleared and
 * re-taken on every push.
 *
 * A HELD PROMPT'S FIRST DRAW PARKS THE FEED (owner ruling, 2026-09-23). When a
 * push draws a held prompt's card that the previous drawing did not hold — one
 * this client just sent that the daemon held included — the feed jumps to its
 * tail and follows, exactly as a sent prompt does (`promptHeld`, scroll.ts),
 * AFTER the card is placed so the tail includes it. A re-push of a card already
 * drawn, and a card's removal, move nothing.
 *
 * THE ONE TOGGLE. The tray sits outside `#feed`, so the feed's click handler
 * never reaches it: the tray arms the same one (`installClickExpand`) on its
 * own host, with the same has-more refresh after each toggle.
 *
 * THERE IS NO HEADING (owner rulings 2 and 5, 2026-09-13). The cards say what
 * is held, so a "held (2)" counter over two visible cards is a second answer to
 * a question the cards already answer. The field is RETIRED from the proto, so
 * the tray neither draws nor requires one.
 */
import { WatchDaemonHoldsResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_holds_pb";
import type {
  DaemonHoldItem,
  DaemonHoldTray,
} from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { watchStream } from "../rpc/streams.js";
import type { TrayContext } from "./context.js";
import { drawHeldOffer } from "./held-offer.js";
import { drawHeldPrompt } from "./held-prompt.js";
import { ClassifierUpdateForms } from "./classifier-update.js";
import { placeChildren } from "../dom.js";
import { installClickExpand } from "../expand.js";
import { refreshHasMore } from "../feed/bubble-more.js";
import { stopTicking } from "../feed/ticking.js";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

/** What the tray is handed by the page that mounts it. */
export interface HoldTrayDeps {
  /**
   * A held prompt's card was drawn for the first time, and is placed: park the
   * feed at its tail and follow (`FeedHandle.promptHeld`).
   */
  promptHeld(turn: string): void;
}

/**
 * Mount the tray on HOST and keep it drawn.
 *
 * The stream is STANDING: it never concludes on its own, so `dispose()` is the
 * only thing that closes it (and closing it stops nothing daemon-side — the
 * holds are the daemon's).
 */
export function mountHoldTray(host: HTMLElement, ctx: AppContext, deps: HoldTrayDeps): Handle {
  log.debug("mounting the hold tray", { operation: "tray.mount" });
  // The one click-to-expand every bubble opens by, and the has-more refresh a
  // toggle that moved no height needs (bubble-more.ts).
  const uninstallExpand = installClickExpand(host, undefined, (section) => refreshHasMore(section));

  // The classifier update forms OUTLIVE every push (see TrayContext).
  const classifierForms = new ClassifierUpdateForms(ctx);

  /** Teardowns the CURRENT drawing owns; replaced wholesale on every push. */
  let disposers: Array<() => void> = [];
  const clear = (): void => {
    for (const fn of disposers) fn();
    disposers = [];
  };

  const stream = watchStream(ctx, {
    name: "WatchDaemonHolds",
    schema: WatchDaemonHoldsResponseSchema,
    open: (_client, signal) =>
      ctx.streams.watch("holds", { workspace: ctx.workspace }, signal),
    onPush: (response) => {
      const tray = requireMessage(response.tray, "WatchDaemonHoldsResponse.tray");
      // The teardowns come down BEFORE the draw, so a card whose subscription
      // is about to be replaced cannot tick a node already detached.
      clear();
      const tc: TrayContext = {
        ctx,
        onDispose: (fn) => {
          disposers.push(fn);
        },
        classifierForms,
      };
      // An EMPTY tray draws NOTHING, so the host is emptied rather than given
      // a region: `#hold-tray:empty` is what collapses the space, and it only
      // matches a host with no children at all.
      const current = host.firstElementChild instanceof HTMLElement ? host.firstElementChild : null;
      const before = heldTurns(host);
      const drawn = drawDaemonHoldTray(tray, tc, current);
      if (current !== null && current !== drawn) stopTicking(current);
      placeChildren(host, drawn === null ? [] : [drawn]);
      // A form whose card left with this push goes with it.
      classifierForms.retain(heldTurns(host));
      // Only a FIRST draw parks, and only once it is placed; one park covers
      // every card the push landed, since each parks at the same tail.
      const landed = heldTurns(host).filter((turn) => !before.includes(turn));
      if (landed.length === 0) return;
      log.debug("a held prompt landed in the tray", {
        operation: "tray.held-prompt-landed",
        context: { turns: landed },
      });
      deps.promptHeld(landed[landed.length - 1]);
    },
  });

  return {
    dispose(): void {
      log.debug("disposing the hold tray", { operation: "tray.dispose" });
      stream.cancel();
      clear();
      uninstallExpand();
      for (const child of host.children) stopTicking(child);
      host.replaceChildren();
    },
  };
}

/**
 * The tray, whole: the held things, in the order the daemon served them —
 * or `null` when nothing is held.
 *
 * AN EMPTY TRAY DRAWS NOTHING (owner ruling 3, 2026-09-13). Not a "nothing
 * held" line, not an empty region: the answer to "what is the daemon
 * holding for you" when it is holding nothing is silence, and the region only
 * appears when cards arrive. `null` rather than an empty element, because
 * `#hold-tray:empty` collapses the region only while the host has NO children,
 * so an empty wrapper would still pay layout.
 */
export function drawDaemonHoldTray(
  u: DaemonHoldTray,
  tc: TrayContext,
  previous: HTMLElement | null = null,
): HTMLElement | null {
  const path = "DaemonHoldTray";
  log.debug("drawing the hold tray", {
    operation: "tray.draw",
    context: { items: u.items.length, in_place: previous !== null },
  });

  if (u.items.length === 0) return null;

  // THE PREVIOUS DRAWING IS REUSED, region and list alike, so a held prompt's
  // card updated in place never leaves the document (see the file header).
  const reused = previous?.classList.contains(TRAY_REGION_CLASS) === true ? previous : null;
  const region = reused ?? document.createElement("div");
  region.className = TRAY_REGION_CLASS;
  let list = region.querySelector<HTMLElement>(`:scope > .${TRAY_LIST_CLASS}`);
  if (list === null) {
    list = document.createElement("div");
    region.appendChild(list);
  }
  // The shared delimiter class: the tray's rows are delimited exactly as a
  // topbar dropdown's and an expanded footer panel's are.
  list.className = `${TRAY_LIST_CLASS} list-rows`;

  const cards = new Map<string, HTMLElement>();
  for (const card of list.querySelectorAll<HTMLElement>(":scope > [data-held-turn]")) {
    cards.set(card.getAttribute("data-held-turn") ?? "", card);
  }
  const items = u.items.map((item, index) =>
    drawDaemonHoldItem(item, tc, `${path}.items[${index}]`, cards),
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
  return [...host.querySelectorAll(`.${TRAY_LIST_CLASS} > [data-held-turn]`)].map((card) => {
    const turn = card.getAttribute("data-held-turn");
    if (turn === null) throw new Error("a held prompt card lost its data-held-turn while it was read");
    return turn;
  });
}

/** The tray region's class, and its list's. */
const TRAY_REGION_CLASS = "hold-tray";
const TRAY_LIST_CLASS = "hold-tray-items";

/**
 * Every held entry the tray draws, in the daemon's order: the list's own cards.
 * The tray sits after the feed in the scroll zone, so its last card, when there
 * is one, is the feed column's latest entry (`latestEntry`, feed.ts).
 */
export const HELD_ENTRY_SELECTOR = `.${TRAY_LIST_CLASS} > *`;

/**
 * One held thing. THE ARM IS THE KIND.
 *
 * A prompt is released or dropped and an offer is answered, so the routing is
 * on the arm and never on anything inside it.
 */
export function drawDaemonHoldItem(
  u: DaemonHoldItem,
  tc: TrayContext,
  path: string,
  cards: ReadonlyMap<string, HTMLElement> = new Map(),
): HTMLElement {
  const item = requireCase(u.item, `${path}.item`);
  log.debug("drawing a held item", {
    operation: "tray.item",
    context: { path, item: item.case },
  });
  switch (item.case) {
    case "prompt":
      // This turn's card from the last drawing, updated in place.
      return drawHeldPrompt(item.value, tc, cards.get(item.value.turn?.value ?? ""));
    case "offer":
      return drawHeldOffer(item.value, tc);
    default: {
      const other: { case: string } = item;
      return unreachableArm(`${path}.item`, other.case);
    }
  }
}

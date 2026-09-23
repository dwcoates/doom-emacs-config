/**
 * tray — the daemon-hold tray: what the daemon is holding FOR the user.
 *
 * ITS OWN REGION AT THE FEED'S TAIL, inside the same scroll zone, so a reader
 * at the bottom of the conversation sees the held prompts sitting after the
 * last row rather than as chrome docked beneath it. Nothing here is a feed row
 * — a held prompt is daemon-owned pending intent the vendor never saw.
 *
 * WHOLE-LIST-REPLACED. Every push carries the tray entire, so a redraw throws
 * the previous DOM away instead of reconciling it, and an item that left is
 * simply absent from the next push — deletion is row omission, never an event.
 * The one thing that must survive that is the ticker subscriptions the cards
 * open for their queued-at ages, which is what `TrayContext.onDispose` is for.
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
import { drawHeldPrompt, openHeldTurns, reopenHeldFolds } from "./held-prompt.js";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

/**
 * Mount the tray on HOST and keep it drawn.
 *
 * The stream is STANDING: it never concludes on its own, so `dispose()` is the
 * only thing that closes it (and closing it stops nothing daemon-side — the
 * holds are the daemon's).
 */
export function mountHoldTray(host: HTMLElement, ctx: AppContext): Handle {
  log.debug("mounting the hold tray", { operation: "tray.mount" });

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
      };
      // An EMPTY tray draws NOTHING, so the host is emptied rather than given
      // a region: `#hold-tray:empty` is what collapses the space, and it only
      // matches a host with no children at all.
      const drawn = drawDaemonHoldTray(tray, tc);
      // The reader's open folds are view state the push knows nothing of, so
      // they are carried from the drawing being replaced onto its successor.
      const open = openHeldTurns(host);
      host.replaceChildren(...(drawn === null ? [] : [drawn]));
      reopenHeldFolds(host, open);
    },
  });

  return {
    dispose(): void {
      log.debug("disposing the hold tray", { operation: "tray.dispose" });
      stream.cancel();
      clear();
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
export function drawDaemonHoldTray(u: DaemonHoldTray, tc: TrayContext): HTMLElement | null {
  const path = "DaemonHoldTray";
  log.debug("drawing the hold tray", {
    operation: "tray.draw",
    context: { items: u.items.length },
  });

  if (u.items.length === 0) return null;

  const region = document.createElement("div");
  region.className = "hold-tray";

  const list = document.createElement("div");
  // The shared delimiter class: the tray's rows are delimited exactly as a
  // topbar dropdown's and an expanded footer panel's are.
  list.className = "hold-tray-items list-rows";
  for (const [index, item] of u.items.entries()) {
    list.appendChild(drawDaemonHoldItem(item, tc, `${path}.items[${index}]`));
  }
  region.appendChild(list);
  return region;
}

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
): HTMLElement {
  const item = requireCase(u.item, `${path}.item`);
  log.debug("drawing a held item", {
    operation: "tray.item",
    context: { path, item: item.case },
  });
  switch (item.case) {
    case "prompt":
      return drawHeldPrompt(item.value, tc);
    case "offer":
      return drawHeldOffer(item.value, tc);
    default: {
      const other: { case: string } = item;
      return unreachableArm(`${path}.item`, other.case);
    }
  }
}

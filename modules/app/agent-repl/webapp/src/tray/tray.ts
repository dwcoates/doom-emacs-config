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
 * AN EMPTY LIST IS A VALUE, not an absence: the heading still draws (the
 * daemon composed it, and it is the daemon's to compose) with a quiet empty
 * line beneath it. The host itself collapses to nothing only when the stream
 * has pushed nothing at all.
 */
import { WatchDaemonHoldsResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_holds_pb";
import type {
  DaemonHoldHeading,
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
  log("debug", "mounting the hold tray", { operation: "tray.mount" });

  /** Teardowns the CURRENT drawing owns; replaced wholesale on every push. */
  let disposers: Array<() => void> = [];
  const clear = (): void => {
    for (const fn of disposers) fn();
    disposers = [];
  };

  const stream = watchStream(ctx, {
    name: "WatchDaemonHolds",
    schema: WatchDaemonHoldsResponseSchema,
    open: (client, signal) =>
      client.watchDaemonHolds({ workspace: ctx.workspace }, { signal }),
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
      const drawn = drawDaemonHoldTray(tray, tc);
      host.replaceChildren(drawn);
    },
  });

  return {
    dispose(): void {
      log("debug", "disposing the hold tray", { operation: "tray.dispose" });
      stream.cancel();
      clear();
      host.replaceChildren();
    },
  };
}

/** The tray, whole: the daemon's heading over the held things in order. */
export function drawDaemonHoldTray(u: DaemonHoldTray, tc: TrayContext): HTMLElement {
  const path = "DaemonHoldTray";
  log("debug", "drawing the hold tray", {
    operation: "tray.draw",
    context: { items: u.items.length },
  });

  const region = document.createElement("div");
  region.className = "hold-tray";
  region.appendChild(
    drawDaemonHoldHeading(requireMessage(u.heading, `${path}.heading`), `${path}.heading`),
  );

  const list = document.createElement("div");
  // The shared delimiter class: the tray's rows are delimited exactly as a
  // topbar dropdown's and an expanded footer panel's are.
  list.className = "hold-tray-items list-rows";
  if (u.items.length === 0) {
    // AN EMPTY TRAY IS A MEANINGFUL VALUE. It draws collapsed but present, so
    // the reader can see that nothing is held rather than infer it from a gap.
    const empty = document.createElement("div");
    empty.className = "hold-tray-empty";
    empty.setAttribute("data-empty", "");
    empty.textContent = "nothing held";
    list.appendChild(empty);
  }
  for (const [index, item] of u.items.entries()) {
    list.appendChild(drawDaemonHoldItem(item, tc, `${path}.items[${index}]`));
  }
  region.appendChild(list);
  return region;
}

/** The heading, composed by the daemon, drawn verbatim. */
export function drawDaemonHoldHeading(u: DaemonHoldHeading, path: string): HTMLElement {
  log("debug", "drawing the hold tray heading", {
    operation: "tray.heading",
    context: { path },
  });
  const heading = document.createElement("div");
  heading.className = "hold-tray-heading";
  heading.textContent = u.text;
  return heading;
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
  log("debug", "drawing a held item", {
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

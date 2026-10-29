/**
 * gate — THE WEBHOST DRAWS THE SHOWN WORKSPACE'S COLD GATE (owner request,
 * 2026-10-29).
 *
 * A cold-gated workspace's page is unloaded off screen, and the gate must be
 * on screen the instant the workspace is: only the feed may load behind it.
 * So the gate is the webhost's, as the sidebar is: the roster carries every
 * open workspace's shown gate (`WorkspaceRoster.cold_gates`), and this draws
 * the shown page's in the composer's slot at the window's bottom, through the
 * feed's own card renderer. The page draws no standing gate of its own.
 *
 * THE SLOT IS THE COMPOSER'S EXACT HEIGHT (owner, 2026-10-29). Emacs hides the
 * input window while the gate stands and tells the slot's pixels
 * (`Host.setGateDock`); the gate takes at least that height, grows only to fit
 * a selection (the compact submenu) and shrinks back, and never scrolls. The
 * pages and the sidebar end above the band it measures, so a switch between a
 * gated and an ungated workspace moves the sidebar by nothing, and a gate
 * coming or going resizes no page.
 */
import { create, equals } from "@bufbuild/protobuf";
import {
  FeedColdGateSchema,
  FeedRowSchema,
  type FeedColdGate,
  type FeedId,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { RosterColdGate } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { drawFeedColdGate, GATE_ANSWERING_EVENT } from "../feed/asks/cold-gate.js";
import type { RowContext } from "../feed/cards/context.js";
import { DOCKED_CARD_CLASS, DOCK_HEIGHT_PROPERTY } from "../feed/gate-dock.js";
import { stopTicking } from "../feed/ticking.js";
import { log } from "../log.js";
import type { PageSeen } from "../page-seen.js";
import type { AppContext, DaemonContext } from "../rpc/context.js";

/** The id of the webhost's gate element. */
export const WEBHOST_GATE_ID = "webhost-gate";

/** The root custom property the pages and the sidebar end above: the gate's band, in pixels. */
export const GATE_BAND_PROPERTY = "--webhost-gate-band";

/** What the gate needs from its environment, injectable for tests. */
export interface WebhostGateEnv {
  readonly doc: Document;
  /** The element the gate is drawn in. */
  readonly el: HTMLElement;
  /** The webhost's own daemon context, which the card's answers go through. */
  readonly ctx: DaemonContext;
  /** An element's laid-out height; `getBoundingClientRect` when omitted. */
  readonly measure?: (el: Element) => number;
  /** Watch EL's size, calling FN on each change; a ResizeObserver when omitted. Answers the stop. */
  readonly watchSize?: (el: Element, fn: () => void) => () => void;
  /** Close the standing news digest of page ID, as answering a gate does. */
  readonly dismissDigest: (id: string) => void;
}

/** The webhost's gate. */
export interface WebhostGate {
  /** The roster's shown gates, whole. */
  setGates(gates: readonly RosterColdGate[]): void;
  /** The page now shown, or null for none. */
  setShown(id: string | null): void;
  /** The slot Emacs told page ID's gate takes: the input window's height. */
  setSlot(id: string, pixels: number): void;
  /** Stop, and draw nothing. */
  dispose(): void;
}

/** The gate drawn now: whose, which row, which card, and its element. */
interface Drawn {
  readonly workspace: string;
  readonly row: FeedId;
  readonly gate: FeedColdGate;
  readonly card: HTMLElement;
}

/** A page-seen that is always seen: the webhost's gate is drawn only on screen. */
const ALWAYS_SEEN: PageSeen = {
  isSeen: () => true,
  noteCurrent: () => undefined,
  onChange: () => () => undefined,
  dispose: () => undefined,
};

/**
 * The context a card drawn in the webhost answers through: the webhost's own
 * daemon context, addressed to WORKSPACE. The daemon context is the
 * prototype, so its live members (`client`, re-read per call) stay live.
 */
function gateContext(ctx: DaemonContext, workspace: WorkspaceRef): AppContext {
  return Object.create(ctx, {
    workspace: { value: workspace },
    seen: { value: ALWAYS_SEEN },
    composerEnabled: { value: false },
  }) as AppContext;
}

/** Create the webhost's gate over ENV. */
export function createWebhostGate(env: WebhostGateEnv): WebhostGate {
  const { doc, el } = env;
  const measure = env.measure ?? ((e: Element) => e.getBoundingClientRect().height);
  let gates = new Map<string, RosterColdGate>();
  const slots = new Map<string, number>();
  let shown: string | null = null;
  let drawn: Drawn | null = null;
  let band = -1;

  el.id = WEBHOST_GATE_ID;
  el.setAttribute("data-component", "gate-dock");
  el.hidden = true;

  /** End the pages and the sidebar above the gate's band as laid out now. */
  const placeBand = (): void => {
    // THE EXACT HEIGHT, never rounded: a gate grown to fit a selection lays out
    // at a fractional height, and a rounded band left a seam between it and the
    // page above (measured in WebKit, 2026-10-29).
    const pixels = el.hidden ? 0 : measure(el);
    if (pixels === band) return;
    band = pixels;
    doc.documentElement.style.setProperty(GATE_BAND_PROPERTY, `${pixels}px`);
  };
  const stopWatch = (
    env.watchSize ??
    ((target: Element, fn: () => void) => {
      const watch = new ResizeObserver(fn);
      watch.observe(target);
      return () => watch.disconnect();
    })
  )(el, placeBand);

  /** Take the gate drawn now off screen. */
  const clear = (): void => {
    if (drawn === null) return;
    stopTicking(drawn.card);
    drawn.card.remove();
    drawn = null;
  };

  /** Size the slot by what Emacs told for the shown page, or the stylesheet's fallback. */
  const sizeSlot = (): void => {
    const pixels = shown === null ? undefined : slots.get(shown);
    if (pixels === undefined) {
      el.style.removeProperty(DOCK_HEIGHT_PROPERTY);
      return;
    }
    el.style.setProperty(DOCK_HEIGHT_PROPERTY, `${pixels}px`);
  };

  /** Draw the shown page's gate, keeping the card drawn while its row and facts are unchanged. */
  const render = (): void => {
    const wanted = shown === null ? undefined : gates.get(shown);
    if (wanted === undefined || wanted.row === undefined || wanted.gate === undefined || wanted.workspace === undefined) {
      if (drawn !== null) {
        log.info("the shown workspace's gate is gone; the webhost's gate band hides", {
          operation: "webhost.gate.hidden",
          context: { workspace: drawn.workspace },
        });
      }
      clear();
      el.hidden = true;
      sizeSlot();
      placeBand();
      return;
    }
    const { row, gate, workspace } = wanted;
    if (
      drawn !== null &&
      drawn.workspace === workspace.id &&
      drawn.row.value === row.value &&
      equals(FeedColdGateSchema, drawn.gate, gate)
    ) {
      sizeSlot();
      placeBand();
      return;
    }
    clear();
    const rc: RowContext = {
      ctx: gateContext(env.ctx, workspace),
      feed: "root",
      row: create(FeedRowSchema, { id: row, row: { case: "coldGate", value: gate } }),
      arrival: "push",
      revealRow: () => Promise.resolve(false),
    };
    const card = drawFeedColdGate(gate, rc);
    card.classList.add(DOCKED_CARD_CLASS);
    el.append(card);
    drawn = { workspace: workspace.id, row, gate, card };
    el.hidden = false;
    sizeSlot();
    log.info("the shown workspace's gate stands; the webhost draws it in the composer's slot", {
      operation: "webhost.gate.drawn",
      context: { workspace: workspace.id, row: row.value, slot: slots.get(workspace.id) ?? "fallback" },
    });
    // THE BAND IS PLACED NOW, in the same turn as the draw, so no frame shows
    // the page running under the gate before the size watch reports.
    placeBand();
  };

  // ANSWERING THE GATE CLOSES THE SHOWN PAGE'S STANDING NEWS DIGEST, as
  // answering it inside the page did (owner, 2026-10-06).
  const onAnswering = (): void => {
    if (shown !== null) env.dismissDigest(shown);
  };
  el.addEventListener(GATE_ANSWERING_EVENT, onAnswering);

  return {
    setGates(next) {
      gates = new Map();
      for (const g of next) {
        if (g.workspace === undefined || g.row === undefined || g.gate === undefined) {
          log.error("the roster carried a cold gate missing its workspace, row or card; it is not drawn", {
            operation: "webhost.gate.malformed",
            context: { workspace: g.workspace?.id ?? null, row: g.row?.value ?? null, card: g.gate !== undefined },
          });
          continue;
        }
        gates.set(g.workspace.id, g);
      }
      render();
    },
    setShown(id) {
      shown = id;
      render();
    },
    setSlot(id, pixels) {
      slots.set(id, pixels);
      if (id === shown) {
        sizeSlot();
        placeBand();
      }
    },
    dispose() {
      el.removeEventListener(GATE_ANSWERING_EVENT, onAnswering);
      stopWatch();
      clear();
      el.hidden = true;
    },
  };
}

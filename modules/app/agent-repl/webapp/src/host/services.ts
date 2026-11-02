/**
 * The webhost's own services: what the host draws over its pages from its own
 * daemon connection.
 *
 * THE SIDEBAR IS NOT HERE. It runs in a web view of its own, in a window that
 * runs the frame's whole height (sidebar-page.ts), so the host draws only what
 * belongs over the pages: the shown workspace's cold gate (gate.ts), in the
 * composer's slot, and a held-back page's load button (load-button.ts).
 *
 * SO THE HOST STILL TALKS TO THE DAEMON (document.ts): the gate's answers go
 * through its context, it watches the roster for the gates it draws, and its
 * failures go to every page's warning chip (host.ts `setFailures`), the
 * webapp's one error surface over the pages.
 */
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { bootFailed } from "../failure/sink.js";
import { log } from "../log.js";
import type { DaemonContext } from "../rpc/context.js";
import { requireCase, requireMessage, unreachablePushArm } from "../rpc/strict.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { startWebhostDocument, type WebhostDocumentDeps, type WebhostRuntime } from "./document.js";
import type { Host } from "./host.js";
import { createWebhostGate, type WebhostGate } from "./gate.js";
import { createWebhostLoadButton, type WebhostLoadButton } from "./load-button.js";
import { errorCause } from "../error-cause.js";

/** What the start may have injected, for a suite that drives it. */
export interface WebhostServicesDeps extends WebhostDocumentDeps {
  /**
   * The element the shown workspace's cold gate is drawn in (host/gate.ts);
   * no gate is drawn when absent.
   */
  gate?: HTMLElement;
  /** Create the webhost's gate; the real one draws through the feed's card. */
  createGate?: typeof createWebhostGate;
  /** Watch the roster's cold gates; the real one opens the roster stream. */
  watchColdGates?: typeof watchColdGates;
  /**
   * The element a held-back page's load button is drawn in
   * (host/load-button.ts); no button is drawn when absent.
   */
  load?: HTMLElement;
  /** Create the webhost's load button; the real one asks the daemon. */
  createLoadButton?: typeof createWebhostLoadButton;
}

/**
 * Tell ON_GATES the roster's shown cold gates on every roster pushed over CTX.
 *
 * The gate is drawn from the roster (`WorkspaceRoster.cold_gates`), which the
 * sidebar's document watches too; each document watches its own, since the
 * two run in separate web views. The stream is STANDING, so the stop is the
 * only thing that closes it.
 */
export function watchColdGates(
  ctx: DaemonContext,
  onGates: (gates: WorkspaceRoster["coldGates"]) => void,
): StreamHandle {
  return watchStream(ctx, {
    name: "WatchWorkspaceRoster",
    schema: WatchWorkspaceRosterResponseSchema,
    open: (_client, signal) => ctx.streams.watch("roster", {}, signal),
    plannedEnding: (response) => response.push.case === "ending",
    onPush: (response) => {
      const push = requireCase(response.push, "WatchWorkspaceRosterResponse.push");
      if (push.case !== "roster") return unreachablePushArm("WatchWorkspaceRosterResponse.push", push.case);
      onGates(requireMessage(push.value, "WatchWorkspaceRosterResponse.roster").coldGates);
    },
  });
}

/**
 * Start the webhost's services for HOST, served from WIN's origin, on RUNTIME's connection.
 *
 * Services that cannot start leave the pages working: the failure is logged
 * and listed in every page's warning chip, never thrown past the webhost.
 * Answers the stop.
 */
export function startWebhostServices(
  doc: Document,
  win: Window,
  host: Host,
  runtime: WebhostRuntime,
  deps: WebhostServicesDeps = {},
): { dispose(): void } {
  const started = startWebhostDocument(runtime, doc, win, deps);
  const { ctx, failures } = started;
  // THE WEBHOST'S FAILURES ARE LISTED IN EVERY PAGE'S CHIP: the whole set,
  // every time it changes.
  failures.subscribe(() => {
    host.setFailures(failures.standing());
  });

  let stop = (): void => undefined;
  try {
    log.info("the webhost is starting its services", { operation: "webhost.services-start", context: {} });
    // THE SHOWN WORKSPACE'S COLD GATE follows the roster, the page shown and
    // the slot Emacs told, whether or not the page is loaded.
    const gateEl = deps.gate;
    const gate: WebhostGate | null =
      gateEl === undefined
        ? null
        : (deps.createGate ?? createWebhostGate)({
            doc,
            el: gateEl,
            ctx,
            dismissDigest: (id) => {
              host.dismissDigest(id);
            },
          });
    const coldGates = gate === null ? null : (deps.watchColdGates ?? watchColdGates)(ctx, (gates) => gate.setGates(gates));
    const stopSlots = gate === null ? () => undefined : host.onGateSlot((id, pixels) => gate.setSlot(id, pixels));
    const stopShown = gate === null ? () => undefined : host.onShown((id) => gate.setShown(id));
    // THE SHOWN HELD-BACK PAGE'S LOAD BUTTON follows the host's holds.
    const loadEl = deps.load;
    const loadButton: WebhostLoadButton | null =
      loadEl === undefined ? null : (deps.createLoadButton ?? createWebhostLoadButton)({ el: loadEl, ctx });
    const stopHeldBack =
      loadButton === null ? () => undefined : host.onHeldBackShown((shown) => loadButton.setShown(shown));
    stop = () => {
      stopShown();
      stopSlots();
      stopHeldBack();
      coldGates?.cancel();
      loadButton?.dispose();
      gate?.dispose();
    };
  } catch (err) {
    const cause = errorCause(err);
    log.error(`the webhost's services failed to start: ${cause}`, {
      operation: "webhost.services-start-failed",
      context: { cause },
    });
    failures.report(bootFailed(cause));
  }
  return {
    dispose: () => {
      stop();
      started.dispose();
    },
  };
}

/**
 * load-button — THE WEBHOST'S LOAD BUTTON FOR A HELD-BACK PAGE (owner request,
 * 2026-10-30).
 *
 * A cold-gated workspace's page is not loaded when the user switches to it:
 * the gate is the webhost's own (host/gate.ts), and loading a whole feed behind
 * it for a workspace the user is only passing costs the one web process a
 * page for nothing. Emacs holds the page back (`Host.holdBack`), and while it
 * is shown this draws a subtle button at the centre of where the page would
 * be. The click asks for the page: the daemon relays it to Emacs
 * (LoadPageInEditor), which owns which pages the host holds, and the page
 * Emacs then loads takes the button's place.
 *
 * A REFUSAL OR A FAILED CALL GIVES THE BUTTON BACK. The daemon has already
 * answered, or could not be reached; the record says which, and the user may
 * click again.
 */
import { LoadPageInEditorResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_load_page_in_editor_pb";
import { createControl } from "../control.js";
import { log } from "../log.js";
import { requireCase } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { DaemonContext } from "../rpc/context.js";
import type { HeldBackPage } from "./host.js";

/** The id of the webhost's load button's element. */
export const WEBHOST_LOAD_ID = "webhost-load";

/** The button's words. */
export const LOAD_BUTTON_LABEL = "load conversation";

/** What the load button needs from its environment. */
export interface WebhostLoadButtonEnv {
  /** The element the button is drawn in. */
  readonly el: HTMLElement;
  /** The webhost's own daemon context, which the click's call goes through. */
  readonly ctx: DaemonContext;
}

/** The webhost's load button. */
export interface WebhostLoadButton {
  /** The held-back page shown now, or null for none (`Host.onHeldBackShown`). */
  setShown(shown: HeldBackPage | null): void;
  /** Stop, and draw nothing. */
  dispose(): void;
}

/** Create the webhost's load button over ENV. */
export function createWebhostLoadButton(env: WebhostLoadButtonEnv): WebhostLoadButton {
  const { el, ctx } = env;
  const doc = el.ownerDocument;
  el.id = WEBHOST_LOAD_ID;
  el.setAttribute("data-component", "load-button");
  el.hidden = true;
  // THE ONE CONTROL (control.ts), as every clickable thing in the webapp is.
  const button = createControl(doc);
  button.className = "webhost-load-button";
  button.textContent = LOAD_BUTTON_LABEL;
  el.append(button);
  let shown: HeldBackPage | null = null;

  /** Ask Emacs, through the daemon, for PAGE. */
  const request = async (page: HeldBackPage): Promise<void> => {
    button.disabled = true;
    log.info("the user asked for a held-back page", {
      operation: "webhost.load-button.clicked",
      context: { workspace: page.id },
    });
    let outcome: string;
    try {
      const response = await callUnary(
        ctx,
        "LoadPageInEditor",
        (client) => client.loadPageInEditor({ workspace: { id: page.id, dir: page.dir } }),
        LoadPageInEditorResponseSchema,
      );
      const result = requireCase(response.result, "LoadPageInEditorResponse.result");
      if (result.case === "success") return;
      outcome = requireCase(result.value.cause, "LoadPageInEditorError.cause").case;
    } catch (err) {
      // `callUnary` logged the call's failure, as its owner; this records only
      // that the button is given back.
      outcome = err instanceof Error ? err.name : "thrown";
    }
    log.warn("the held-back page was not loaded; the load button is given back", {
      operation: "webhost.load-button.refused",
      context: { workspace: page.id, outcome },
    });
    if (shown?.id === page.id) button.disabled = false;
  };

  const onClick = (): void => {
    if (shown === null || button.disabled) return;
    void request(shown);
  };
  button.addEventListener("click", onClick);

  return {
    setShown(next) {
      if (next?.id === shown?.id && next?.dir === shown?.dir) return;
      shown = next;
      button.disabled = false;
      el.hidden = next === null;
      if (next !== null) {
        log.info("a held-back page is shown; the webhost draws its load button", {
          operation: "webhost.load-button.drawn",
          context: { workspace: next.id },
        });
      }
    },
    dispose() {
      button.removeEventListener("click", onClick);
      el.hidden = true;
      shown = null;
    },
  };
}

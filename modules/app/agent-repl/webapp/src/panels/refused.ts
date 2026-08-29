/**
 * refused — the card for a command the daemon RECOGNIZES and neither answers
 * nor forwards, and the offer to make it work.
 *
 * A GAP IN THE UI, NOT A VERDICT. `/agents` and `/help` reach this card today
 * because the daemon knows them and has no panel for them yet — the data
 * behind them exists. So the card carries the "engineer support for it" offer,
 * whose button asks the daemon to open an ordinary support workspace with a
 * brief IT composes. This end never composes the brief and never names the
 * repository: it echoes the command off the card it is drawn on, and nothing
 * else.
 *
 * THE OFFER IS PRESENCE-GATED. `add_support` is set iff the daemon offers it,
 * so an absent field draws no button — never a disabled one, which would
 * promise a door that is not there.
 *
 * SYNTHESIZED AND NON-DURABLE, like the panel rows: the card is minted when
 * the command was refused and never comes back in a paged history.
 */
import { RequestCommandSupportResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_request_command_support_pb";
import type {
  FeedCommandAddSupportOffer,
  FeedCommandRefused,
  FeedCommandRefusedCommand,
  FeedCommandRefusedReason,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { RowContext } from "../feed/cards/context.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, requireMessage } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";

/** The refusal card. */
export function drawFeedCommandRefused(u: FeedCommandRefused, rc: RowContext): HTMLElement {
  const path = "FeedCommandRefused";
  const command = requireMessage(u.command, `${path}.command`);
  const reason = requireMessage(u.reason, `${path}.reason`);
  log("debug", "drawing a command-refused card", {
    operation: "panels.command-refused",
    context: { command: command.text, offer: u.addSupport !== undefined },
  });

  const card = document.createElement("div");
  card.className = "command-refused";

  const head = document.createElement("div");
  head.className = "command-refused-head";
  head.appendChild(drawFeedCommandRefusedCommand(command, `${path}.command`));
  card.appendChild(head);
  card.appendChild(drawFeedCommandRefusedReason(reason, `${path}.reason`));

  if (u.addSupport !== undefined) {
    card.appendChild(
      drawFeedCommandAddSupportOffer(u.addSupport, rc.ctx, command.text, `${path}.add_support`),
    );
  }
  return card;
}

/** The command as typed, verbatim, in the app's monospace command face. */
export function drawFeedCommandRefusedCommand(
  u: FeedCommandRefusedCommand,
  path: string,
): HTMLElement {
  log("debug", "drawing a refused command", {
    operation: "panels.command-refused.command",
    context: { path, command: u.text },
  });
  const command = document.createElement("code");
  command.className = "daemon-intercepted-command-name";
  command.setAttribute("data-command", "");
  command.textContent = u.text;
  return command;
}

/** The daemon's composed sentence, verbatim. */
export function drawFeedCommandRefusedReason(
  u: FeedCommandRefusedReason,
  path: string,
): HTMLElement {
  log("debug", "drawing a refusal reason", {
    operation: "panels.command-refused.reason",
    context: { path },
  });
  const reason = document.createElement("div");
  reason.className = "command-refused-reason";
  reason.textContent = u.text;
  return reason;
}

/**
 * The offer: one button, and the note its success leaves.
 *
 * THE LABEL IS THE CLIENT'S — the marker is empty on the wire, because its
 * presence is the whole fact it carries.
 */
export function drawFeedCommandAddSupportOffer(
  _u: FeedCommandAddSupportOffer,
  ctx: AppContext,
  command: string,
  path: string,
): HTMLElement {
  log("debug", "drawing an add-support offer", {
    operation: "panels.command-refused.offer",
    context: { path, command },
  });
  const row = document.createElement("div");
  row.className = "command-refused-actions";

  const button = document.createElement("button");
  button.type = "button";
  button.className = "command-refused-support";
  button.setAttribute("data-add-support", "");
  button.textContent = "Engineer support for it";
  row.appendChild(button);

  button.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    void requestSupport(ctx, command, button, row);
  });
  return row;
}

/**
 * Ask for the support workspace.
 *
 * THE ANSWER'S WorkspaceRef IS NOT DRAWN. The roster is where a workspace
 * appears, and re-stating it here would be a second, immediately stale account
 * of it — so the card says only that the ask landed, which is the one thing
 * the roster cannot say at the place the user clicked.
 */
async function requestSupport(
  ctx: AppContext,
  command: string,
  button: HTMLButtonElement,
  row: HTMLElement,
): Promise<void> {
  clearNotes(row);
  button.disabled = true;
  try {
    const response = await callUnary(
      ctx,
      "RequestCommandSupport",
      (client) => client.requestCommandSupport({ workspace: ctx.workspace, command }),
      RequestCommandSupportResponseSchema,
    );
    const result = requireCase(response.result, "RequestCommandSupportResponse.result");
    if (result.case === "success") {
      const note = document.createElement("span");
      note.className = "command-refused-note";
      note.setAttribute("data-support-note", "");
      note.textContent = "support workspace created";
      row.appendChild(note);
      log("info", `RequestCommandSupport created a workspace for ${command}`, {
        operation: "panels.command-refused.support-created",
        context: { command },
      });
      // The button stays disabled: the ask landed, and a second one would open
      // a second workspace for the same gap.
      return;
    }
    drawRefusal(row, result.case, "the daemon could not open a support workspace");
    log("warn", `RequestCommandSupport refused ${command}`, {
      operation: "panels.command-refused.support-refused",
      context: { command, arm: result.case },
    });
    button.disabled = false;
  } catch (err) {
    drawRefusal(row, "error", "the daemon could not be reached");
    log("error", `RequestCommandSupport failed for ${command}: ${String(err)}`, {
      operation: "panels.command-refused.support-failed",
      context: { command, cause: err },
    });
    button.disabled = false;
  }
}

function drawRefusal(row: HTMLElement, arm: string, message: string): void {
  const refusal = document.createElement("span");
  refusal.className = "refusal command-refused-refusal";
  refusal.setAttribute("data-arm", arm);
  refusal.textContent = message;
  row.appendChild(refusal);
}

function clearNotes(row: HTMLElement): void {
  row
    .querySelectorAll(".command-refused-refusal, .command-refused-note")
    .forEach((node) => node.remove());
}

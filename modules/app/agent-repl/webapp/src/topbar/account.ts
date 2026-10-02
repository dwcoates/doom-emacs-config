/**
 * The account cell's dropdown: every root the daemon knows, and the pick that
 * makes this workspace's session spend as one of them.
 *
 * WHY IT EXISTS. Clicking the account cell used to open the session-line
 * reveal, which is a DIFFERENT fact and — for most sessions — an empty one, so
 * the cell read as a control that did nothing (owner ruling, 2026-09-13). It
 * opens the login options instead: the roots, each with its email or the words
 * "logged out", the current one marked.
 *
 * THE ROOT IS AN ECHO TOKEN. A pick sends the option's own `config_dir` back
 * unchanged; this end never composes a path and never offers a root the daemon
 * did not serve. The new account then arrives on the TOPBAR STREAM, not in the
 * response — `SelectAccountSuccess` carries only whether that root holds a
 * login, which is this end's cue to open the login flow next.
 *
 * A ONE-ROOT MACHINE STILL GETS A DROPDOWN, of one row. It states what the
 * choice is; a cell that silently did nothing because there was nothing else
 * to pick is the defect this replaces.
 */
import { createControl, type Control } from "../control.js";
import { SelectAccountResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_account_pb";
import type {
  TopbarAccount,
  TopbarAccountOption,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { whileInFlight } from "../feed/cards/controls.js";
import { log } from "../log.js";
import { guardMalformed } from "../rpc/guard.js";
import { isMalformedView } from "../rpc/malformed.js";
import {
  clearRefusals,
  drawTransportRefusal,
  drawTypedRefusal,
  drawUnreadableRefusal,
  type SentenceTable,
} from "../rpc/refuse.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { TopbarContext } from "./context.js";
import { asAnchor } from "./strip.js";

/** The reveal's name in the strip's one reveal layer. */
export const ACCOUNT_REVEAL = "account";

/** The hook one offered root carries: the echo token itself. */
export const ACCOUNT_OPTION_ATTRIBUTE = "data-account-option";

/** The hook marking the offered root the session already spends as. */
export const CURRENT_OPTION_ATTRIBUTE = "data-current";

/** What a root with no login says, in the cell and in the row alike. */
export const LOGGED_OUT_LABEL = "logged out";

/** The causes only SelectAccount can answer with. */
export const SELECT_ACCOUNT_CAUSES = {
  unknownAccount: () => "this daemon does not know that account root",
} as unknown as SentenceTable;

/**
 * Wire the cell's click to the options dropdown.
 *
 * Separate from `drawTopbarAccount` because the cell draws the account and the
 * reveal draws the CHOICE — the same split the model selector's button and its
 * option list keep.
 */
export function bindAccountReveal(
  button: HTMLElement,
  u: TopbarAccount,
  tc: TopbarContext,
): void {
  asAnchor(button, ACCOUNT_REVEAL);
  const body = (): HTMLElement => drawAccountOptions(u, tc, button);
  // Registered as it is drawn, so a push arriving while the reveal is open
  // re-opens it with THIS push's options rather than the previous push's.
  tc.reveals.register(ACCOUNT_REVEAL, ACCOUNT_REVEAL, body);
  button.addEventListener("click", () => {
    tc.reveals.toggle(ACCOUNT_REVEAL, ACCOUNT_REVEAL, body);
  });
}

/** The reveal: exactly the served roots, in the served order. */
export function drawAccountOptions(
  u: TopbarAccount,
  tc: TopbarContext,
  button: HTMLElement,
): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-account-options list-rows";
  log.debug("drawing the account options", {
    operation: "topbar.account-options",
    context: { options: u.options.length },
  });
  for (const option of u.options) {
    list.append(drawAccountOption(option, tc, button));
  }
  return list;
}

/** One offered root. */
export function drawAccountOption(
  option: TopbarAccountOption,
  tc: TopbarContext,
  button: HTMLElement,
): HTMLElement {
  const row = createControl();
  row.className = "topbar-account-option";
  row.setAttribute(ACCOUNT_OPTION_ATTRIBUTE, option.configDir);
  // The row that IS the current account, so the reveal shows where the reader
  // already is rather than offering it as a change.
  row.toggleAttribute(CURRENT_OPTION_ATTRIBUTE, option.current);

  const state = requireCase(option.state, "TopbarAccountOption.state");
  const label = document.createElement("span");
  label.className = "topbar-account-option-label";
  switch (state.case) {
    case "loggedIn":
      label.textContent = state.value.email;
      break;
    case "loggedOut":
      // THE LABEL IS THE WARNING here for the same reason it is on the cell: a
      // root with no login cannot run a turn, and a blank row would read as a
      // row that failed to load.
      label.classList.add("topbar-account-warn");
      label.textContent = LOGGED_OUT_LABEL;
      break;
    default: {
      const other: { case: string } = state;
      return unreachableArm("TopbarAccountOption.state", other.case);
    }
  }
  row.append(label);

  const dir = document.createElement("span");
  dir.className = "topbar-account-option-dir";
  dir.textContent = option.configDir;
  row.append(dir);

  row.addEventListener("click", () => {
    void pickAccount(option, tc, button, row);
  });
  return row;
}

/**
 * Send the pick, with the row and the cell inert while it is unanswered.
 *
 * The refusal draws at the CELL rather than inside the reveal, because the
 * reveal closes on the next click and a refusal the reader never sees is a
 * click that silently did nothing.
 */
export async function pickAccount(
  option: TopbarAccountOption,
  tc: TopbarContext,
  button: HTMLElement,
  row: Control,
): Promise<void> {
  log.info(`the reader chose the account root ${option.configDir}`, {
    operation: "topbar.account-picked",
    context: { config_dir: option.configDir, current: option.current },
  });
  // CLEARED BEFORE THE CALL, never after: a refusal from the previous pick
  // standing beside the control the reader just clicked again reads as the
  // answer to the NEW click.
  clearRefusals(button);
  const answered = await whileInFlight([row], () =>
    callUnary(
      tc.ctx,
      "SelectAccount",
      (client) =>
        client.selectAccount({ workspace: tc.ctx.workspace, configDir: option.configDir }),
      SelectAccountResponseSchema,
    ),
  );
  if ("failed" in answered) {
    // AN ANSWER THIS BUILD CANNOT READ IS MACHINERY, not the daemon refusing:
    // it is filed as `frame_undecodable` through the one click guard and
    // nothing is drawn at the cell, because there is no refusal to state.
    if (isMalformedView(answered.failed)) {
      await guardMalformed(tc.ctx, "topbar.account-pick", Promise.reject(answered.failed));
      return;
    }
    drawTransportRefusal(button);
    return;
  }
  try {
    const result = requireCase(answered.value.result, "SelectAccountResponse.result");
    switch (result.case) {
      case "success":
        // The switch happened; the new account arrives on the topbar stream.
        tc.reveals.close();
        if (!result.value.loggedIn) {
          // THE CHOICE IS HONORED AND THEN COMPLETED. A root with no login is
          // still the root this workspace now spends as, so the remedy is the
          // same login flow the logged-out cell's own click opens.
          log.info("the chosen account root holds no login; opening its login flow", {
            operation: "topbar.account-login-needed",
            context: { config_dir: option.configDir },
          });
          tc.openLogin(button);
        }
        return;
      case "error":
        drawTypedRefusal(
          button,
          "SelectAccountError.cause",
          "SelectAccount",
          result.value.cause,
          SELECT_ACCOUNT_CAUSES,
        );
        return;
      default: {
        const other: { case: string } = result;
        return unreachableArm("SelectAccountResponse.result", other.case);
      }
    }
  } catch (err) {
    if (!drawUnreadableRefusal(tc.ctx, button, "topbar.account-malformed-refusal", err)) throw err;
  }
}

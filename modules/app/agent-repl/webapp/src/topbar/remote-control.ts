/**
 * The remote-control chip: one phone glyph immediately left of the
 * persistent-wifi chip, stating whether THIS WORKSPACE'S session is reachable
 * from claude.ai (and its mobile app) through the vendor's Remote Control
 * bridge (docs/developer/remote-control.md).
 *
 * THE SAME LOOK AND FEEL AS THE PERSISTENT-WIFI CHIP, ANOTHER SYMBOL (owner
 * ruling, 2026-10-31). The glyph is always black and the mode arm paints the
 * disc: on → a green disc, off → a white one. The glyph and its disc are drawn
 * through the topbar's one disc drawing (src/topbar/disc-glyph.ts), and the
 * stylesheet's persistent-wifi rules name this chip's hooks beside the wifi
 * chip's, so the two cannot drift apart in size, stroke or paint. The arm rides
 * as a data attribute and the stylesheet does the painting.
 *
 * A TOGGLE IN FLIGHT PULSES (`data-settling`) until the daemon answers, as the
 * wifi chip's does: the answer is the one signal of "settled" this end has.
 *
 * A CONTROL AS WELL AS A STATUS, in every workspace's strip alike: clicking
 * the glyph turns the mode over through `agentrepl.v1.UpdateRemoteControlMode`'s
 * `toggle` arm, for THIS page's workspace, the same endpoint and arm
 * `agent-repl-remote-control-toggle` sends from Emacs. The DAEMON resolves the
 * toggle from the mode it reads at that moment, so this end decides nothing: it
 * sends the arm, states a refusal at the chip, and the chip redraws from the
 * push the change causes.
 */
import { createControl, type Control } from "../control.js";
import {
  UpdateRemoteControlModeResponseSchema,
  type UpdateRemoteControlModeResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_remote_control_mode_pb";
import type { TopbarRemoteControl } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
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
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { TopbarContext } from "./context.js";
import { drawDiscGlyph, type DiscGlyph } from "./disc-glyph.js";

/**
 * The phone glyph: a smartphone's outline and its home dot, stroked in the
 * current color on the same 24-unit grid as the wifi glyph. The body spans
 * x 5..19 and y 2..22, so it is symmetric about (12, 12), and its farthest
 * stroked corner is under 13 units from center, inside the disc.
 */
export const PHONE_GLYPH_PATHS: readonly string[] = [
  "M7 2h10a2 2 0 0 1 2 2v16a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2z",
  "M12 18h.01",
];

/** The phone glyph on its disc, drawn through the topbar's one disc drawing. */
const PHONE_GLYPH: DiscGlyph = {
  glyphClass: "topbar-remote-control-glyph",
  discClass: "topbar-remote-control-disc",
  paths: PHONE_GLYPH_PATHS,
};

/**
 * The mode arm's attribute value. The mode is REQUIRED (topbar.proto: every
 * workspace has a remote-control mode, on or off), so an unset arm is a
 * malformed view rather than an "unknown" disc.
 */
function modeArm(u: TopbarRemoteControl): string {
  const mode = requireCase(u.mode, "TopbarRemoteControl.mode");
  switch (mode.case) {
    case "on":
      return "on";
    case "off":
      return "off";
    default: {
      const other: { case: string } = mode;
      return unreachableArm("TopbarRemoteControl.mode", other.case);
    }
  }
}

/**
 * The causes only UpdateRemoteControlMode can answer with. The other four arms
 * (unknown workspace, ref mismatch, transferring away, not yet adopted) are the
 * cross-cutting four every per-workspace rpc shares, worded once in
 * src/rpc/refusal.ts.
 */
const UPDATE_REMOTE_CONTROL_MODE_CAUSES = {
  vendorRefused: (value: { detail: string }) => `the vendor refused remote control: ${value.detail}`,
} as unknown as SentenceTable;

/**
 * The chip, painted from its mode arm, with the daemon's tooltip, and its click
 * turning the mode over.
 *
 * THE WRAP CARRIES THE ARM AND THE BUTTON IS THE CONTROL, as the wifi chip's
 * do: a refusal is drawn inside the wrap beside the button, never inside the
 * button it answers.
 */
export function drawTopbarRemoteControl(u: TopbarRemoteControl, tc: TopbarContext): HTMLElement {
  const mode = modeArm(u);
  const chip = document.createElement("span");
  chip.className = "topbar-remote-control";
  chip.setAttribute("data-mode", mode);
  chip.title = requireMessage(u.tooltip, "TopbarRemoteControl.tooltip").text;

  const button = createControl();
  button.className = "topbar-remote-control-button";
  button.setAttribute("aria-label", "toggle remote control");
  button.append(drawDiscGlyph(PHONE_GLYPH));
  button.addEventListener("click", () => {
    void toggleRemoteControlMode(tc, chip, button, mode);
  });
  chip.append(button);

  log.debug("drawing the remote-control chip", {
    operation: "topbar.remote-control",
    context: { mode },
  });
  return chip;
}

/**
 * Send `UpdateRemoteControlMode{toggle}` for this page's workspace and state
 * what it answered.
 *
 * DRAWN is the mode the clicked chip showed, for the log record only: the
 * daemon turns the mode it reads, never the one this chip last drew.
 */
async function toggleRemoteControlMode(
  tc: TopbarContext,
  chip: HTMLElement,
  button: Control,
  drawn: string,
): Promise<void> {
  // AT INFO: a person's click that opens or closes the session to claude.ai
  // is exactly the edge someone asks about afterwards.
  log.info("the reader toggled remote control", {
    operation: "topbar.remote-control-toggle",
    context: { mode: drawn },
  });
  // CLEARED BEFORE THE CALL: a refusal standing beside the chip the reader just
  // clicked again reads as the answer to the NEW click.
  clearRefusals(chip);
  chip.setAttribute("data-settling", "");
  let answered: Awaited<ReturnType<typeof whileInFlight<UpdateRemoteControlModeResponse>>>;
  try {
    answered = await whileInFlight([button], () =>
      callUnary(
        tc.ctx,
        "UpdateRemoteControlMode",
        (client) =>
          client.updateRemoteControlMode({
            workspace: tc.ctx.workspace,
            action: { case: "toggle", value: {} },
          }),
        UpdateRemoteControlModeResponseSchema,
      ),
    );
  } finally {
    chip.removeAttribute("data-settling");
  }
  if ("failed" in answered) {
    if (isMalformedView(answered.failed)) {
      await guardMalformed(tc.ctx, "topbar.remote-control-toggle", Promise.reject(answered.failed));
      return;
    }
    drawTransportRefusal(chip, answered.failed);
    return;
  }
  // THE BUTTON COMES BACK ON EVERY ANSWER. A toggle stays a legitimate click
  // after it lands, and the redraw that would replace this chip is the push of
  // a CHANGED mode, which an answer does not promise.
  button.disabled = false;
  try {
    const result = requireCase(answered.value.result, "UpdateRemoteControlModeResponse.result");
    switch (result.case) {
      case "success":
        log.info("remote control was toggled", {
          operation: "topbar.remote-control-toggled",
          context: { on: result.value.on },
        });
        return;
      case "error":
        drawTypedRefusal(
          chip,
          "UpdateRemoteControlModeError.cause",
          "UpdateRemoteControlMode",
          result.value.cause,
          UPDATE_REMOTE_CONTROL_MODE_CAUSES,
        );
        return;
      default: {
        const other: { case: string } = result;
        return unreachableArm("UpdateRemoteControlModeResponse.result", other.case);
      }
    }
  } catch (err) {
    if (!drawUnreadableRefusal(tc.ctx, chip, "topbar.remote-control-malformed-answer", err)) {
      throw err;
    }
  }
}

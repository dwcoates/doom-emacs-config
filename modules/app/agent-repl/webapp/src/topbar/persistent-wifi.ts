/**
 * The persistent-wifi chip: one wifi glyph between the context chip and the
 * warning chip, stating two facts about the MACHINE the daemon runs on.
 *
 * THE GLYPH IS ALWAYS BLACK AND THE MODE ARM PAINTS THE DISC (owner request,
 * 2026-10-02): on → a green disc; off or unassigned → a white one. The wifi arm
 * rides as a data attribute but paints nothing; the daemon's tooltip says it.
 * The arms ride as data attributes and the stylesheet does the painting, so
 * this file decides nothing beyond naming the arm.
 *
 * A TOGGLE IN FLIGHT PULSES (`data-settling`) until the daemon answers. The
 * contract states no "still changing" fact, so the answer is the one signal
 * of "settled" this end has.
 *
 * A CONTROL AS WELL AS A STATUS (owner request, 2026-10-02): clicking the
 * glyph turns the mode over through `agentrepl.v1.UpdatePersistentWifiMode`'s
 * `toggle` arm, the same endpoint and arm `agent-repl-persistent-wifi-mode-toggle`
 * sends from Emacs. The DAEMON resolves the toggle from the mode it reads at
 * that moment and runs the whole change (hotspot, power settings, display), so
 * this end decides nothing: it sends the arm, states a refusal at the chip, and
 * the chip redraws from the push the change causes.
 */
import { createControl, type Control } from "../control.js";
import {
  UpdatePersistentWifiModeResponseSchema,
  type UpdatePersistentWifiModeResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_persistent_wifi_mode_pb";
import type { TopbarPersistentWifi } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
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

const SVG_NS = "http://www.w3.org/2000/svg";

/**
 * The wifi glyph: three arcs and a dot, stroked in the current color, drawn
 * on a 24-unit grid centered on (12, 12). Every arc is symmetric about x = 12,
 * so the glyph's horizontal center IS the grid's.
 */
const GLYPH_PATHS = [
  "M1.42 9a16 16 0 0 1 21.16 0",
  "M5 12.55a11 11 0 0 1 14.08 0",
  "M8.53 16.11a6 6 0 0 1 6.95 0",
  "M12 20h.01",
];

/**
 * THE DISC IS DRAWN IN THE GLYPH'S OWN SVG, centered on the same (12, 12).
 * It used to be the chip's CSS background with a smaller svg centered inside
 * it by flexbox; the two then landed on the pixel grid separately, and WebKit
 * snaps an inline svg to whole pixels while it paints a background where
 * layout put it, so the glyph sat up to a pixel right of the disc's center
 * (measured in test/webkit/topbar.webkit.test.ts). One svg has one placement,
 * so the glyph and the disc cannot drift apart. The radius keeps the glyph
 * just inscribed: its farthest stroked corner is under 15 units from center.
 */
export const DISC_RADIUS = 15;

/** The viewBox: the disc's square, so the svg's box is the disc's. */
const VIEW_BOX = `${12 - DISC_RADIUS} ${12 - DISC_RADIUS} ${2 * DISC_RADIUS} ${2 * DISC_RADIUS}`;

/** The glyph element: the disc, then the arcs over it. */
function drawGlyph(): SVGSVGElement {
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("viewBox", VIEW_BOX);
  svg.setAttribute("aria-hidden", "true");
  svg.setAttribute("class", "topbar-wifi-glyph");
  const disc = document.createElementNS(SVG_NS, "circle");
  disc.setAttribute("class", "topbar-wifi-disc");
  disc.setAttribute("cx", "12");
  disc.setAttribute("cy", "12");
  disc.setAttribute("r", String(DISC_RADIUS));
  svg.append(disc);
  for (const d of GLYPH_PATHS) {
    const path = document.createElementNS(SVG_NS, "path");
    path.setAttribute("d", d);
    svg.append(path);
  }
  return svg;
}

/** The wifi arm's attribute value; "unknown" is the unassigned oneof. */
function wifiArm(u: TopbarPersistentWifi): string {
  switch (u.wifi.case) {
    case "joined":
      return "joined";
    case "notJoined":
      return "not-joined";
    case undefined:
      return "unknown";
    default: {
      const other: { case: string } = u.wifi;
      return unreachableArm("TopbarPersistentWifi.wifi", other.case);
    }
  }
}

/** The mode arm's attribute value; "unknown" is the unassigned oneof. */
function modeArm(u: TopbarPersistentWifi): string {
  switch (u.mode.case) {
    case "on":
      return "on";
    case "off":
      return "off";
    case undefined:
      return "unknown";
    default: {
      const other: { case: string } = u.mode;
      return unreachableArm("TopbarPersistentWifi.mode", other.case);
    }
  }
}

/** The causes only UpdatePersistentWifiMode can answer with. */
export const UPDATE_PERSISTENT_WIFI_MODE_CAUSES = {
  powerSettingsRefused: (value: { detail: string }) =>
    `the power settings change was refused: ${value.detail}`,
  modeUnreadable: (value: { detail: string }) =>
    `the mode could not be read, so nothing was changed: ${value.detail}`,
} as unknown as SentenceTable;

/**
 * The chip, painted from its two arms, with the daemon's tooltip, and its click
 * turning the mode over.
 *
 * THE WRAP CARRIES THE ARMS AND THE BUTTON IS THE CONTROL, as the model and
 * mode cells do: a refusal is drawn inside the wrap beside the button, never
 * inside the button it answers.
 */
export function drawTopbarPersistentWifi(u: TopbarPersistentWifi, tc: TopbarContext): HTMLElement {
  const wifi = wifiArm(u);
  const mode = modeArm(u);
  const chip = document.createElement("span");
  chip.className = "topbar-wifi";
  chip.setAttribute("data-wifi", wifi);
  chip.setAttribute("data-mode", mode);
  chip.title = requireMessage(u.tooltip, "TopbarPersistentWifi.tooltip").text;

  const button = createControl();
  button.className = "topbar-wifi-button";
  button.setAttribute("aria-label", "toggle persistent wifi mode");
  button.append(drawGlyph());
  button.addEventListener("click", () => {
    void togglePersistentWifiMode(tc, chip, button, { wifi, mode });
  });
  chip.append(button);

  log.debug("drawing the persistent-wifi chip", {
    operation: "topbar.persistent-wifi",
    context: { wifi, mode },
  });
  return chip;
}

/**
 * Send `UpdatePersistentWifiMode{toggle}` and state what it answered.
 *
 * DRAWN is the standing the clicked chip showed, for the log record only: the
 * daemon turns the mode it reads, never the one this chip last drew.
 */
export async function togglePersistentWifiMode(
  tc: TopbarContext,
  chip: HTMLElement,
  button: Control,
  drawn: { wifi: string; mode: string },
): Promise<void> {
  // AT INFO: a person's click on the machine's power settings is exactly the
  // edge someone asks about afterwards.
  log.info("the reader toggled persistent wifi mode", {
    operation: "topbar.persistent-wifi-toggle",
    context: drawn,
  });
  // CLEARED BEFORE THE CALL: a refusal standing beside the chip the reader just
  // clicked again reads as the answer to the NEW click.
  clearRefusals(chip);
  chip.setAttribute("data-settling", "");
  let answered: Awaited<ReturnType<typeof whileInFlight<UpdatePersistentWifiModeResponse>>>;
  try {
    answered = await whileInFlight([button], () =>
      callUnary(
        tc.ctx,
        "UpdatePersistentWifiMode",
        (client) => client.updatePersistentWifiMode({ action: { case: "toggle", value: {} } }),
        UpdatePersistentWifiModeResponseSchema,
      ),
    );
  } finally {
    chip.removeAttribute("data-settling");
  }
  if ("failed" in answered) {
    if (isMalformedView(answered.failed)) {
      await guardMalformed(tc.ctx, "topbar.persistent-wifi-toggle", Promise.reject(answered.failed));
      return;
    }
    drawTransportRefusal(chip, answered.failed);
    return;
  }
  // THE BUTTON COMES BACK ON EVERY ANSWER. A toggle stays a legitimate click
  // after it lands, and the redraw that would replace this chip is the push of
  // a CHANGED standing, which an answer does not promise.
  button.disabled = false;
  try {
    const result = requireCase(answered.value.result, "UpdatePersistentWifiModeResponse.result");
    switch (result.case) {
      case "success": {
        const success = result.value;
        const state = requireMessage(success.state, "UpdatePersistentWifiModeSuccess.state");
        log.info("persistent wifi mode was toggled", {
          operation: "topbar.persistent-wifi-toggled",
          context: {
            mode: state.mode.case ?? "unknown",
            wifi: state.wifi.case ?? "unknown",
            hotspot: requireCase(
              requireMessage(success.hotspot, "UpdatePersistentWifiModeSuccess.hotspot").outcome,
              "UpdatePersistentWifiModeHotspot.outcome",
            ).case,
            display: requireCase(
              requireMessage(success.display, "UpdatePersistentWifiModeSuccess.display").outcome,
              "UpdatePersistentWifiModeDisplay.outcome",
            ).case,
          },
        });
        return;
      }
      case "error":
        drawTypedRefusal(
          chip,
          "UpdatePersistentWifiModeError.cause",
          "UpdatePersistentWifiMode",
          result.value.cause,
          UPDATE_PERSISTENT_WIFI_MODE_CAUSES,
        );
        return;
      default: {
        const other: { case: string } = result;
        return unreachableArm("UpdatePersistentWifiModeResponse.result", other.case);
      }
    }
  } catch (err) {
    if (!drawUnreadableRefusal(tc.ctx, chip, "topbar.persistent-wifi-malformed-answer", err)) {
      throw err;
    }
  }
}

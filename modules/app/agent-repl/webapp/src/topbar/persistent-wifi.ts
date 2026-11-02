/**
 * The persistent-wifi chip: one wifi glyph between the remote-control chip and
 * the warning chip, stating two facts about the MACHINE the daemon runs on.
 *
 * THE GLYPH IS ALWAYS BLACK AND THE MODE ARM PAINTS THE DISC (owner request,
 * 2026-10-02): on → a green disc; off or unassigned → a white one. The wifi arm
 * and the lid arm ride as data attributes but paint nothing; the daemon's
 * tooltip says them (the lid, and the lid-close dim in force).
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
 * that moment and runs the whole change (hotspot, power settings, energy mode,
 * display), so
 * this end decides nothing: it sends the arm, states a refusal at the chip, and
 * the chip redraws from the push the change causes.
 */
import { createControl, type Control } from "../control.js";
import { UpdatePersistentWifiModeResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_persistent_wifi_mode_pb";
import type { TopbarPersistentWifi } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { log } from "../log.js";
import type { SentenceTable } from "../rpc/refuse.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { sendChipToggle } from "./chip-toggle.js";
import type { TopbarContext } from "./context.js";
import { drawDiscGlyph, type DiscGlyph } from "./disc-glyph.js";

/**
 * The wifi glyph: three arcs and a dot, stroked in the current color, drawn
 * on a 24-unit grid centered on (12, 12) (src/topbar/disc-glyph.ts). Every
 * arc is symmetric about x = 12, so the glyph's horizontal center IS the
 * grid's.
 */
const GLYPH_PATHS = [
  "M1.42 9a16 16 0 0 1 21.16 0",
  "M5 12.55a11 11 0 0 1 14.08 0",
  "M8.53 16.11a6 6 0 0 1 6.95 0",
  "M12 20h.01",
];

/** The wifi glyph on its disc, drawn through the topbar's one disc drawing. */
const WIFI_GLYPH: DiscGlyph = {
  glyphClass: "topbar-wifi-glyph",
  discClass: "topbar-wifi-disc",
  paths: GLYPH_PATHS,
};

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

/** The lid arm's attribute value; "unheard" is the unassigned oneof. */
function lidArm(u: TopbarPersistentWifi): string {
  switch (u.lid.case) {
    case "lidOpen":
      return "open";
    case "lidClosed":
      return "closed";
    case "noLid":
      return "none";
    case "lidUnwatched":
      return "unwatched";
    case undefined:
      return "unheard";
    default: {
      const other: { case: string } = u.lid;
      return unreachableArm("TopbarPersistentWifi.lid", other.case);
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
  const lid = lidArm(u);
  const chip = document.createElement("span");
  chip.className = "topbar-wifi";
  chip.setAttribute("data-wifi", wifi);
  chip.setAttribute("data-mode", mode);
  chip.setAttribute("data-lid", lid);
  chip.title = requireMessage(u.tooltip, "TopbarPersistentWifi.tooltip").text;

  const button = createControl();
  button.className = "topbar-wifi-button";
  button.setAttribute("aria-label", "toggle persistent wifi mode");
  button.append(drawDiscGlyph(WIFI_GLYPH));
  button.addEventListener("click", () => {
    void togglePersistentWifiMode(tc, chip, button, { wifi, mode });
  });
  chip.append(button);

  log.debug("drawing the persistent-wifi chip", {
    operation: "topbar.persistent-wifi",
    context: { wifi, mode, lid },
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
  await sendChipToggle(tc, chip, button, {
    rpc: "UpdatePersistentWifiMode",
    operation: "topbar.persistent-wifi",
    send: (client) => client.updatePersistentWifiMode({ action: { case: "toggle", value: {} } }),
    schema: UpdatePersistentWifiModeResponseSchema,
    causes: UPDATE_PERSISTENT_WIFI_MODE_CAUSES,
    onSuccess: (success) => {
      const state = requireMessage(success.state, "UpdatePersistentWifiModeSuccess.state");
      log.info("persistent wifi mode was toggled", {
        operation: "topbar.persistent-wifi-toggled",
        context: {
          mode: state.mode.case ?? "unknown",
          wifi: state.wifi.case ?? "unknown",
          lid: state.lid.case ?? "unheard",
          hotspot: stepOutcomeArm(success.hotspot, "hotspot", "UpdatePersistentWifiModeHotspot"),
          energyMode: stepOutcomeArm(success.energyMode, "energyMode", "UpdatePersistentWifiModeEnergyMode"),
          display: stepOutcomeArm(success.display, "display", "UpdatePersistentWifiModeDisplay"),
        },
      });
    },
  });
}

/**
 * THE ONE READ of a success's step outcome: the required step `field` of
 * `agentrepl.v1.UpdatePersistentWifiModeSuccess`, whose `message` type holds
 * its outcome in a required `outcome` oneof. Answers the set arm's name, and
 * throws a malformed-view error naming the path when either is absent.
 */
export function stepOutcomeArm(
  step: { outcome: { case?: string | undefined } } | undefined,
  field: string,
  message: string,
): string {
  return requireCase(
    requireMessage(step, `UpdatePersistentWifiModeSuccess.${field}`).outcome,
    `${message}.outcome`,
  ).case;
}

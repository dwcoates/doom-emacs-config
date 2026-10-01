/**
 * The persistent-wifi chip: one wifi glyph between the context chip and the
 * warning chip, stating two facts about the MACHINE the daemon runs on.
 *
 * THE TWO ONEOFS ARE PAINTED INDEPENDENTLY, and each arm maps to exactly one
 * paint (topbar.proto, TopbarPersistentWifi):
 *   wifi  joined → green glyph; not_joined → red glyph; unassigned → muted.
 *   mode  on → the glyph sits on a blue disc it is just inscribed in; off or
 *         unassigned → no disc.
 * The arms ride as data attributes and the stylesheet does the painting, so
 * this file decides nothing beyond naming the arm.
 *
 * A STATUS, NOT A CONTROL: no click, no cursor. The mode is changed from the
 * editor, and the chip redraws from the next push.
 */
import type { TopbarPersistentWifi } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { log } from "../log.js";
import { requireMessage, unreachableArm } from "../rpc/strict.js";

const SVG_NS = "http://www.w3.org/2000/svg";

/**
 * The wifi glyph: three arcs and a dot, stroked in the current color. Its
 * stroked bounding box is about 23 by 17 viewBox units, so its diagonal is the
 * disc's diameter at the stylesheet's glyph-to-disc ratio.
 */
const GLYPH_PATHS = [
  "M1.42 9a16 16 0 0 1 21.16 0",
  "M5 12.55a11 11 0 0 1 14.08 0",
  "M8.53 16.11a6 6 0 0 1 6.95 0",
  "M12 20h.01",
];

/** The glyph element. */
function drawGlyph(): SVGSVGElement {
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("viewBox", "0 0 24 24");
  svg.setAttribute("aria-hidden", "true");
  svg.setAttribute("class", "topbar-wifi-glyph");
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

/** The chip, painted from its two arms, with the daemon's tooltip. */
export function drawTopbarPersistentWifi(u: TopbarPersistentWifi): HTMLElement {
  const wifi = wifiArm(u);
  const mode = modeArm(u);
  const chip = document.createElement("span");
  chip.className = "topbar-wifi";
  chip.setAttribute("data-wifi", wifi);
  chip.setAttribute("data-mode", mode);
  chip.title = requireMessage(u.tooltip, "TopbarPersistentWifi.tooltip").text;
  chip.append(drawGlyph());
  log.debug("drawing the persistent-wifi chip", {
    operation: "topbar.persistent-wifi",
    context: { wifi, mode },
  });
  return chip;
}

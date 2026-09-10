/**
 * The fast-mode cell — the permission-mode picker's read-only sibling.
 *
 * FAST MODE IS A STANDING SESSION STATE, like the permission mode beside it,
 * and it is the other one a reader has to know before sending. Unlike the
 * mode, nothing here can change it: there is no verb on the contract to set
 * fast mode, so this is a LABEL and never a control — a picker would offer a
 * pick that goes nowhere.
 *
 * THE ARM IS THE STATE, drawn by name. `cooldown` is deliberately not folded
 * into `off`: off is a setting somebody chose, cooldown is the vendor saying
 * "not right now, and it will come back on its own". Drawing them the same
 * would invite a reader to go looking for a switch that cannot take effect.
 *
 * NOTHING IS DRAWN WHEN THE VENDOR HAS SAID NOTHING. `TopbarView.fast_mode`
 * unset means no fast-mode statement has been made for this session, which is
 * not the same as `off`; a cell would claim a fact nobody stated.
 */
import type { TopbarFastMode } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { log } from "../log.js";
import { unreachableArm } from "../rpc/strict.js";

/** The label each state draws, and the tooltip behind it. */
const FAST_MODE_LABELS = {
  on: { label: "fast", title: "fast mode is on" },
  off: { label: "fast off", title: "fast mode is off" },
  cooldown: {
    label: "fast cooling",
    title: "fast mode is unavailable for now and returns on its own",
  },
} as const;

/**
 * The cell, or null when the vendor has stated no fast mode. A null is the
 * strip drawing nothing at all rather than a quiet placeholder.
 */
export function drawTopbarFastMode(
  u: TopbarFastMode | undefined,
): HTMLElement | null {
  const state = u?.state;
  if (state === undefined || state.case === undefined) return null;

  const cell = document.createElement("span");
  cell.className = "topbar-fast";
  cell.setAttribute("data-fast-mode", state.case);

  switch (state.case) {
    case "on":
    case "cooldown": {
      const drawn = FAST_MODE_LABELS[state.case];
      cell.textContent = drawn.label;
      cell.title = drawn.title;
      break;
    }
    case "off": {
      cell.textContent = FAST_MODE_LABELS.off.label;
      // THE VENDOR'S REASON IS THE TOOLTIP, verbatim when it gave one. It is
      // never composed into the label: the label is a fixed vocabulary the
      // strip's width is sized for, and the reason is free vendor text.
      cell.title =
        state.value.reason === ""
          ? FAST_MODE_LABELS.off.title
          : state.value.reason;
      break;
    }
    default: {
      const other: { case: string } = state;
      return unreachableArm("TopbarFastMode.state", other.case);
    }
  }

  log.debug("drawing the fast-mode cell", {
    operation: "topbar.fast-mode",
    context: { state: state.case },
  });
  return cell;
}

/**
 * The permission-mode picker — the model selector's sibling, and its mirror in
 * every respect that matters.
 *
 * THE DAEMON SERVES EXACTLY WHAT IT WILL ACCEPT: an ungated mode appears in
 * `options` only where the workspace's creation consented to it, so this end
 * offers what it was served and validates nothing. A pick echoes the option's
 * `mode` string UNCHANGED — the shim's own spelling, never a normalization of
 * it — and the new mode arrives on the pushed surfaces, not in the response.
 *
 * WHY THE PICKER AND NOT A BANNER. The old ungated-session standing banner is
 * dead: the mode is visible in the picker itself, which is both quieter and
 * more useful — the reader sees the mode in force AND can change it in the same
 * control.
 */
import { createControl, type Control } from "../control.js";
import {
  SetPermissionModeResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_permission_mode_pb";
import type {
  TopbarPermissionModeOption,
  TopbarPermissionModePicker,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { log } from "../log.js";
import { requireMessage } from "../rpc/strict.js";
import type { TopbarContext } from "./context.js";
import type { SentenceTable } from "../rpc/refuse.js";
import { drawNoSessionCell } from "./no-session.js";
import { sendPick } from "./pick.js";
import { asAnchor } from "./strip.js";

/** The causes only SetPermissionMode can answer with. */
export const SET_PERMISSION_MODE_CAUSES = {
  modeNotServed: () => "that mode is not among the ones this session offers",
  ungatedWithoutConsent: () => "an ungated mode needs the workspace's explicit consent",
  noSession: () => "this workspace has no session to set a mode on",
  vendorRefused: (value: { detail: string }) => `the vendor refused: ${value.detail}`,
} as unknown as SentenceTable;

/** The picker: the mode in force, and the modes it may become. */
export function drawTopbarPermissionModePicker(
  u: TopbarPermissionModePicker | undefined,
  tc: TopbarContext,
): HTMLElement {
  // ABSENT IS "NO SESSION HAS STATED A MODE", drawn as the dash in this cell's
  // own slot; see `no-session.ts`.
  if (u === undefined) return drawNoSessionCell("mode");
  const current = requireMessage(u.current, "TopbarPermissionModePicker.current");
  log.debug("drawing the permission-mode picker", {
    operation: "topbar.permission-mode",
    context: { mode: current.mode, options: u.options.length },
  });

  const wrap = document.createElement("div");
  wrap.className = "topbar-mode";

  const button = createControl();
  button.className = "topbar-mode-button";
  button.textContent = current.displayName;
  button.setAttribute("data-mode", current.mode);
  wrap.append(button);

  // The wrap is the control (`.topbar-mode` per the DOM contract), so it holds
  // the anchor and the click; see the model selector for the reasoning.
  asAnchor(wrap, "mode");
  const body = (): HTMLElement => drawPermissionModeOptions(u, tc, wrap, button);
  tc.reveals.register("mode", "mode", body);
  wrap.addEventListener("click", () => {
    tc.reveals.toggle("mode", "mode", body);
  });
  return wrap;
}

/** The reveal: exactly the served modes, in the served order. */
export function drawPermissionModeOptions(
  u: TopbarPermissionModePicker,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: Control,
): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-mode-options list-rows";
  for (const option of u.options) list.append(drawPermissionModeOption(option, tc, wrap, button));
  return list;
}

/** One offered mode. */
export function drawPermissionModeOption(
  option: TopbarPermissionModeOption,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: Control,
): HTMLElement {
  const row = createControl();
  row.className = "topbar-mode-option";
  row.setAttribute("data-mode-option", option.mode);
  row.textContent = option.displayName;
  row.addEventListener("click", () => {
    void pickPermissionMode(option, tc, wrap, button, row);
  });
  return row;
}

/** Send the pick, echoing the served spelling verbatim. */
export async function pickPermissionMode(
  option: TopbarPermissionModeOption,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: Control,
  row: Control,
): Promise<void> {
  log.info(`the reader picked the permission mode ${option.mode}`, {
    operation: "topbar.permission-mode-picked",
    context: { mode: option.mode },
  });
  await sendPick(
    {
      rpc: "SetPermissionMode",
      send: (client) => client.setPermissionMode({ workspace: tc.ctx.workspace, mode: option.mode }),
      schema: SetPermissionModeResponseSchema,
      causes: SET_PERMISSION_MODE_CAUSES,
      malformedOperation: "topbar.permission-mode-pick",
      unreadableOperation: "topbar.permission-mode-malformed-refusal",
    },
    tc,
    wrap,
    button,
    row,
  );
}

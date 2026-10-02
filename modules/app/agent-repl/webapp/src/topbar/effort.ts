/**
 * The effort selector — the reasoning effort this workspace's session runs at,
 * and the levels it may be switched to. Drawn between the model selector and
 * the permission-mode picker, in exactly their form: a button naming the level
 * in force, and a reveal listing the levels the selected model accepts.
 *
 * THE LEVEL IS AN ECHO TOKEN, exactly as the model is. A pick sends back the
 * option's own `level` unchanged, and the new level arrives on the TOPBAR
 * STREAM, never in the response: `SetEffortSuccess` is empty on purpose.
 *
 * NO FEED ROW. A pick never draws a held entry, a `/effort` prompt or any row
 * in the feed (owner ruling, 2026-10-01); the button moving is the whole of
 * its visible effect.
 *
 * THE ARM IS WHETHER THE MODEL TAKES A LEVEL. `unsupported` is a dash with no
 * dropdown; ABSENT is the no-session dash every session-scoped cell draws.
 */
import { SetEffortResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_effort_pb";
import type {
  TopbarEffortOption,
  TopbarEffortSelector,
  TopbarEffortSelectorSupported,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { AgentEffortLevel } from "../../../proto/gen/ts/conversation/v1/api_pb";
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
import { drawNoSessionCell, NO_SESSION_DASH } from "./no-session.js";
import { asAnchor } from "./strip.js";

/**
 * The selector's hover (owner, 2026-10-01; design record 2026-10-02 decision
 * 2). Client-owned static copy.
 */
export const EFFORT_TOOLTIP =
  "Changes the agent's reasoning effort in this workspace for the rest of the session. Will cause token cache misses.";

/** What the unsupported arm's dash says on hover. */
export const EFFORT_UNSUPPORTED_TOOLTIP = "this model takes no reasoning-effort level";

/** The hook naming the level in force, carried by the control itself. */
export const SELECTED_EFFORT_ATTRIBUTE = "data-effort";

/** The causes only SetEffort can answer with. */
export const SET_EFFORT_CAUSES = {
  noSession: () => "this workspace has no session to set an effort on",
  notSupported: () => "the session's model does not accept that effort level",
  vendorRefused: (value: { detail: string }) => `the vendor refused: ${value.detail}`,
} as unknown as SentenceTable;

/** The selector, or its dash. */
export function drawTopbarEffortSelector(
  u: TopbarEffortSelector | undefined,
  tc: TopbarContext,
): HTMLElement {
  if (u === undefined) return drawNoSessionCell("effort");
  const support = requireCase(u.support, "TopbarEffortSelector.support");
  switch (support.case) {
    case "unsupported":
      return drawEffortUnsupported();
    case "supported":
      return drawEffortSupported(support.value, tc);
    default: {
      const other: { case: string } = support;
      return unreachableArm("TopbarEffortSelector.support", other.case);
    }
  }
}

/** The unsupported arm: a dash, and no dropdown. */
export function drawEffortUnsupported(): HTMLElement {
  log.debug("drawing the effort selector for a model that takes no level", {
    operation: "topbar.effort-selector",
    context: { arm: "unsupported" },
  });
  const element = document.createElement("span");
  element.className = "topbar-effort";
  element.setAttribute("data-effort-unsupported", "");
  element.title = EFFORT_UNSUPPORTED_TOOLTIP;
  element.textContent = NO_SESSION_DASH;
  return element;
}

/** The supported arm: the level in force, opening the accepted levels. */
export function drawEffortSupported(u: TopbarEffortSelectorSupported, tc: TopbarContext): HTMLElement {
  const current = requireMessage(u.current, "TopbarEffortSelectorSupported.current");
  log.debug("drawing the effort selector", {
    operation: "topbar.effort-selector",
    context: { arm: "supported", level: AgentEffortLevel[current.level], options: u.options.length },
  });

  const wrap = document.createElement("div");
  wrap.className = "topbar-effort";
  wrap.title = EFFORT_TOOLTIP;
  wrap.setAttribute(SELECTED_EFFORT_ATTRIBUTE, effortToken(current));

  const button = document.createElement("button");
  button.type = "button";
  button.className = "topbar-effort-button";
  button.textContent = current.displayName;
  wrap.append(button);

  // The wrap is the control; see the model selector for the reasoning.
  asAnchor(wrap, "effort");
  const body = (): HTMLElement => drawEffortOptions(u, current, tc, wrap, button);
  tc.reveals.register("effort", "effort", body);
  wrap.addEventListener("click", () => {
    tc.reveals.toggle("effort", "effort", body);
  });
  return wrap;
}

/**
 * A level's echo token as a DOM hook: the enum's own name, so the selection
 * and the rows are named in one vocabulary. A level this build's enum does not
 * carry is a malformed push, never a nameless row.
 */
export function effortToken(option: TopbarEffortOption): string {
  const name = AgentEffortLevel[option.level] as string | undefined;
  if (name === undefined || option.level === AgentEffortLevel.UNSPECIFIED) {
    return unreachableArm("TopbarEffortOption.level", String(option.level));
  }
  return name;
}

/** The reveal: exactly the served levels, in the served order. */
export function drawEffortOptions(
  u: TopbarEffortSelectorSupported,
  current: TopbarEffortOption,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: HTMLButtonElement,
): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-effort-options list-rows";
  for (const option of u.options) {
    const row = drawEffortOption(option, tc, wrap, button);
    row.toggleAttribute("data-selected", option.level === current.level);
    list.append(row);
  }
  return list;
}

/** One offered level. */
export function drawEffortOption(
  option: TopbarEffortOption,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: HTMLButtonElement,
): HTMLButtonElement {
  const row = document.createElement("button");
  row.type = "button";
  row.className = "topbar-effort-option";
  row.setAttribute("data-effort-option", effortToken(option));
  row.textContent = option.displayName;
  row.addEventListener("click", () => {
    void pickEffort(option, tc, wrap, button, row);
  });
  return row;
}

/** Send the pick, echoing the served level verbatim. */
export async function pickEffort(
  option: TopbarEffortOption,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: HTMLButtonElement,
  row: HTMLButtonElement,
): Promise<void> {
  log.info(`the reader picked the effort level ${option.displayName}`, {
    operation: "topbar.effort-picked",
    context: { level: AgentEffortLevel[option.level] },
  });
  // CLEARED BEFORE THE CALL; see the permission-mode picker.
  clearRefusals(wrap);
  const answered = await whileInFlight([row, button], () =>
    callUnary(
      tc.ctx,
      "SetEffort",
      (client) => client.setEffort({ workspace: tc.ctx.workspace, effort: option.level }),
      SetEffortResponseSchema,
    ),
  );
  if ("failed" in answered) {
    if (isMalformedView(answered.failed)) {
      await guardMalformed(tc.ctx, "topbar.effort-pick", Promise.reject(answered.failed));
      return;
    }
    drawTransportRefusal(wrap);
    return;
  }
  try {
    const result = requireCase(answered.value.result, "SetEffortResponse.result");
    switch (result.case) {
      case "success":
        tc.reveals.close();
        return;
      case "error":
        drawTypedRefusal(wrap, "SetEffortError.cause", "SetEffort", result.value.cause, SET_EFFORT_CAUSES);
        button.disabled = false;
        return;
      default: {
        const other: { case: string } = result;
        return unreachableArm("SetEffortResponse.result", other.case);
      }
    }
  } catch (err) {
    button.disabled = false;
    if (!drawUnreadableRefusal(tc.ctx, wrap, "topbar.effort-malformed-refusal", err)) {
      throw err;
    }
  }
}

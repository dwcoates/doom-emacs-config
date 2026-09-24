/**
 * convert/tools/plan-mode.ts — the pair of calls around a plan document.
 *
 * # ONE CONVERTER, TWO TOOL NAMES
 *
 * `EnterPlanMode` and `ExitPlanMode` are two vendor calls of one unit kind, and
 * the act is read off the NAME rather than off any field: the enter call has an
 * empty input, so nothing else could tell them apart. Each call is its own unit
 * with its own identity, exactly as the vendor makes them — the one-bubble
 * treatment is the feed's coalescing, never this tier's.
 *
 * AN EXIT WITH NO ENTER IS LEGAL: a session started in the plan permission mode
 * never calls `EnterPlanMode` at all, so nothing here pairs the two.
 *
 * # The vendor's requestId is deliberately dropped
 *
 * `ExitPlanModeOutput.requestId` names a dialog in the VENDOR'S identity space.
 * Vendor identity spaces never cross this contract, so it is not carried — a
 * decision, not a gap.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { prose, startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, bool, failureOf, settle, str, strOr } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-plan-mode",
  operation: "shim.convert.tools.plan_mode",
});

/** The vendor's two spellings of this unit's two acts. */
const ENTER = "EnterPlanMode";
const EXIT = "ExitPlanMode";

/** Which act a call is, by the name the agent used. */
function actNameOf(call: PendingCall): "enter" | "exit" | undefined {
  if (call.toolName === ENTER) return "enter";
  if (call.toolName === EXIT) return "exit";
  LOGGER.debug(
    { tool: call.toolName, tool_use_id: call.toolUseId },
    "a plan-mode unit was built from a tool name that is neither the enter nor the exit call",
  );
  return undefined;
}

/** The one wrapper every frame of this unit shares. */
function item(state: conversationv1.AgentPlanMode["state"]): conversationv1.AgentActivity["item"] {
  return { case: "planMode", value: create(conversationv1.AgentPlanModeSchema, { state }) };
}

/** The exit's answer: the plan and the flags the vendor stated about it. */
function exited(output: Record<string, unknown> | undefined): conversationv1.AgentPlanModeExited {
  const plan = str(output, "plan");
  return create(conversationv1.AgentPlanModeExitedSchema, {
    // UNSET when the vendor stated a null plan — an exit with nothing to show.
    plan: plan === undefined ? undefined : prose(plan),
    planWasEdited: bool(output, "planWasEdited") ?? false,
    filePath: str(output, "filePath"),
    isAgent: bool(output, "isAgent") ?? false,
    hasTaskTool: bool(output, "hasTaskTool") ?? false,
    awaitingLeaderApproval: bool(output, "awaitingLeaderApproval") ?? false,
  });
}

export const planModeConverter: ToolConverter = {
  kind: "plan_mode",
  // AgentPlanMode declares no progress arm.
  carriesProgress: false,

  start(call) {
    const act = actNameOf(call);
    LOGGER.logVerbose({ tool_use_id: call.toolUseId, act }, "a plan-mode call was issued");
    return item({
      case: "start",
      value: create(conversationv1.AgentPlanModeStartSchema, {
        act:
          act === "enter"
            ? { case: "enter", value: create(conversationv1.AgentPlanModeEnterSchema, {}) }
            : act === "exit"
              ? { case: "exit", value: create(conversationv1.AgentPlanModeExitSchema, {}) }
              : { case: undefined },
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the plan-mode call never ran");
      return item({
        case: "failure",
        value: create(conversationv1.AgentPlanModeFailureSchema, { error: failureOf(call, outcome) }),
      });
    }
    const act = actNameOf(call);
    if (act === undefined) {
      // The act IS the fact this arm carries; a success with neither arm set
      // would say a plan-mode call returned without saying which one.
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a settled plan-mode call names no act; no success frame is produced",
      );
      return undefined;
    }
    const output = asRecord(outcome.structured);
    if (output === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId, act },
        "a settled plan-mode call carried no typed output; the act is recorded with nothing stated about it",
      );
    }
    LOGGER.logVerbose({ tool_use_id: call.toolUseId, act }, "the plan-mode call returned");
    return item({
      case: "success",
      value: create(conversationv1.AgentPlanModeSuccessSchema, {
        act:
          act === "enter"
            ? {
                case: "entered",
                value: create(conversationv1.AgentPlanModeEnteredSchema, {
                  message: strOr(output, "message"),
                }),
              }
            : { case: "exited", value: exited(output) },
        settledAt: settle(call, outcome),
      }),
    });
  },
};

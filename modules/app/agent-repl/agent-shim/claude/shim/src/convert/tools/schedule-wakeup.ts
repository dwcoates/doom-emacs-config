/**
 * convert/tools/schedule-wakeup.ts — the agent pacing its own next tick.
 *
 * # Two acts, and the vendor's own exclusivity rule
 *
 * `stop: true` makes every other input field ignored, so the arms are exclusive
 * at the vendor rather than by our choice. The answer is exclusive on the same
 * terms: `stopped: true` in the output is the stop's receipt, and everything
 * else is a pending wakeup.
 *
 * # wake_at_ms is THE fact
 *
 * The footer's countdown ticks from `scheduledFor` — an absolute instant, which
 * survives a reload and a clock the shim never observed. The clamped delay and
 * the clamped flag ride along for fidelity and nothing draws them.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, big, bool, failureOf, strOr, uint } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-schedule-wakeup",
  operation: "shim.convert.tools.schedule_wakeup",
});

/** What was asked, as the call spelled it. */
function actOf(call: PendingCall): conversationv1.AgentScheduleWakeupStart["act"] {
  if (bool(call.input, "stop") === true) {
    return { case: "stop", value: create(conversationv1.AgentScheduleWakeupStopSchema, {}) };
  }
  return {
    case: "schedule",
    value: create(conversationv1.AgentScheduleWakeupScheduleSchema, {
      delaySeconds: uint(call.input, "delaySeconds") ?? 0,
      reason: strOr(call.input, "reason"),
      prompt: strOr(call.input, "prompt"),
    }),
  };
}

/** The one wrapper every frame of this unit shares. */
function item(
  result: conversationv1.AgentScheduleWakeup["result"],
): conversationv1.AgentActivity["item"] {
  return {
    case: "scheduleWakeup",
    value: create(conversationv1.AgentScheduleWakeupSchema, { result }),
  };
}

export const scheduleWakeupConverter: ToolConverter = {
  kind: "schedule_wakeup",
  // AgentScheduleWakeup declares no progress arm.
  carriesProgress: false,

  start(call) {
    const act = actOf(call);
    LOGGER.logVerbose({ tool_use_id: call.toolUseId, act: act.case }, "a self-wakeup call was issued");
    return item({
      case: "start",
      value: create(conversationv1.AgentScheduleWakeupStartSchema, {
        act,
        startedAtMs: BigInt(Math.trunc(call.startedAtMs)),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the self-wakeup call never ran");
      return item({
        case: "failure",
        value: create(conversationv1.AgentScheduleWakeupFailureSchema, {
          failure: failureOf(call, outcome),
        }),
      });
    }
    const output = asRecord(outcome.structured);
    if (output === undefined) {
      // Both outcome arms are entirely made of the typed output's fields; a
      // scheduled arm without wake_at_ms would name an instant nothing stated.
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a settled self-wakeup carried no typed output; neither outcome arm can be built from nothing",
      );
      return undefined;
    }
    if (bool(output, "stopped") === true) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the self-pacing loop was ended");
      return item({
        case: "success",
        value: create(conversationv1.AgentScheduleWakeupSuccessSchema, {
          outcome: {
            case: "stopped",
            value: create(conversationv1.AgentScheduleWakeupStoppedSchema, {
              cancelledWakeups: uint(output, "cancelledWakeups") ?? 0,
            }),
          },
        }),
      });
    }
    const wakeAtMs = big(output, "scheduledFor");
    if (wakeAtMs === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a scheduled wakeup named no instant to fire at; the countdown has nothing to tick from",
      );
      return undefined;
    }
    LOGGER.logVerbose(
      { tool_use_id: call.toolUseId, wake_at_ms: Number(wakeAtMs) },
      "a wakeup is pending",
    );
    return item({
      case: "success",
      value: create(conversationv1.AgentScheduleWakeupSuccessSchema, {
        outcome: {
          case: "scheduled",
          value: create(conversationv1.AgentScheduleWakeupScheduledSchema, {
            wakeAtMs,
            clampedDelaySeconds: uint(output, "clampedDelaySeconds") ?? 0,
            wasClamped: bool(output, "wasClamped") ?? false,
          }),
        },
      }),
    });
  },
};

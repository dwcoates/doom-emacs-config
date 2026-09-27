/**
 * convert/tools/subagent-handback.ts — a subagent handing its final report back.
 *
 * Every background subagent ends by calling the vendor's `SubagentHandback`
 * tool with one input field, `message`: the report its parent receives as the
 * subagent's outcome. The call's own result is a bare acknowledgement
 * (`{"success":true,"message":"Report delivered to your caller."}`), so the
 * REPORT is read off the call's input on every arm, never off the result.
 *
 * # The settle stands alone
 *
 * Both settle arms RESTATE the report. The start and the settle upsert ONE
 * unit, so once the hand-back settles the store holds the settle alone, and a
 * replay drawing it with no start beside it must still carry the result.
 *
 * # The file plane agrees
 *
 * The sidecar builds the same arms from the transcript
 * (`shim-sidecar/internal/convert/activity.go` and `settled_items.go`); both
 * write this unit under one upsert key, so the two must say the same thing.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { failureOf, str } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-subagent-handback",
  operation: "shim.convert.subagent_handback",
});

/** One hand-back item, whatever arm it carries. */
function handbackItem(
  result: conversationv1.AgentSubagentHandback["result"],
): conversationv1.AgentActivity["item"] {
  return {
    case: "subagentHandback",
    value: create(conversationv1.AgentSubagentHandbackSchema, { result }),
  };
}

/**
 * The report, as the subagent wrote it. Read by the start AND by both settle
 * arms, so the restated report can never drift from the announced one.
 */
function reportOf(call: PendingCall): conversationv1.AgentSubagentHandbackReport {
  const text = str(call.input, "message");
  if (text === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a hand-back states no report text; the report is carried empty rather than the hand-back being lost",
    );
  }
  return create(conversationv1.AgentSubagentHandbackReportSchema, { text: text ?? "" });
}

/** The `SubagentHandback` tool: a subagent's final report to its parent. */
export const subagentHandbackConverter: ToolConverter = {
  kind: "subagent_handback",
  // `AgentSubagentHandback` declares the vendor's liveness beat as an arm of its own.
  carriesProgress: true,

  start(call) {
    return handbackItem({
      case: "start",
      value: create(conversationv1.AgentSubagentHandbackStartSchema, {
        report: reportOf(call),
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a hand-back failed");
      return handbackItem({
        case: "failure",
        value: create(conversationv1.AgentSubagentHandbackFailureSchema, {
          error: failureOf(call, outcome),
          // RESTATED so the settled frame stands alone: a refused hand-back is
          // still drawn with the report it tried to deliver.
          report: reportOf(call),
        }),
      });
    }
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a hand-back reached the parent");
    return handbackItem({
      case: "success",
      value: create(conversationv1.AgentSubagentHandbackSuccessSchema, {
        // RESTATED so the settled frame stands alone.
        report: reportOf(call),
        settledAt: settledAt(outcome.settledAtMs, call.startedAtMs),
      }),
    });
  },

  progress(beat) {
    return handbackItem({ case: "progress", value: beat });
  },
};

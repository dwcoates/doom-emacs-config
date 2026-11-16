/**
 * convert/tools/read-notifications.ts — the agent reading its queued notifications.
 *
 * # The call takes nothing, and its answer is prose
 *
 * `ReadNotifications` is called with `{}` and drains the notifications the
 * vendor holds for the agent (a background task finishing, a task
 * notification). It states the queue only as text — "No queued
 * notifications." when it is empty — so the returned text is carried verbatim
 * and never parsed: an empty queue is a successful answer, and a sentence the
 * vendor rewords must not turn into a different claim.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { failureOf, resultText, settle } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-read-notifications",
  operation: "shim.convert.tools.read_notifications",
});

/** The one wrapper every frame of this unit shares. */
function item(
  result: conversationv1.AgentReadNotifications["result"],
): conversationv1.AgentActivity["item"] {
  return {
    case: "readNotifications",
    value: create(conversationv1.AgentReadNotificationsSchema, { result }),
  };
}

export const readNotificationsConverter: ToolConverter = {
  kind: "read_notifications",
  // AgentReadNotifications declares no progress arm.
  carriesProgress: false,

  start(call) {
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the agent read its queued notifications");
    return item({
      case: "start",
      value: create(conversationv1.AgentReadNotificationsStartSchema, {
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "reading the queued notifications failed");
      return item({
        case: "failure",
        value: create(conversationv1.AgentReadNotificationsFailureSchema, {
          error: failureOf(call, outcome),
        }),
      });
    }
    const text = resultText(outcome.content);
    if (text === "") {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a notifications read returned no text; its answer is left unstated",
      );
    }
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the queued notifications were read");
    return item({
      case: "success",
      value: create(conversationv1.AgentReadNotificationsSuccessSchema, {
        returnedText: text === "" ? undefined : text,
        settledAt: settle(call, outcome),
      }),
    });
  },
};

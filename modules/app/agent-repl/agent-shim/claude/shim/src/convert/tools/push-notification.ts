/**
 * convert/tools/push-notification.ts — the agent reaching an absent user.
 *
 * # WHETHER IT WAS ACTUALLY DELIVERED IS THE POINT
 *
 * `PushNotificationOutput` reports a `disabledReason` when the vendor declined,
 * and its presence is what picks the `not_sent` arm — an agent that believes it
 * notified an absent user, and did not, left them waiting on nothing. The
 * reason vocabulary is the vendor's own closed set.
 *
 * # The input's `status` literal is deliberately not carried
 *
 * `status` is typed `"proactive"` and nothing else. A constant is not a fact,
 * so no field records it: a decision, not a gap.
 *
 * # `sentAt` is an ISO string on the wire and an instant here
 *
 * The vendor stamps it at tool execution on the emitting process, and resumed
 * sessions replay pre-`sentAt` outputs verbatim — so it is genuinely absent
 * sometimes, and the field stays UNSET when it is rather than falling back to
 * the settle instant, which is a different clock's reading of a different
 * moment.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, bool, failureOf, settle, str } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-push-notification",
  operation: "shim.convert.tools.push_notification",
});

/** The vendor's own reason vocabulary, mapped to this contract's arms. */
function reasonOf(
  disabledReason: string,
  toolUseId: string,
): conversationv1.AgentPushNotificationNotSent["reason"] {
  switch (disabledReason) {
    case "config_off":
      return {
        case: "configOff",
        value: create(conversationv1.AgentPushNotificationConfigOffSchema, {}),
      };
    case "user_present":
      return {
        case: "userPresent",
        value: create(conversationv1.AgentPushNotificationUserPresentSchema, {}),
      };
    case "no_transport":
      return {
        case: "noTransport",
        value: create(conversationv1.AgentPushNotificationNoTransportSchema, {}),
      };
    default:
      // The DELIVERY fact is still stated — it was not sent — and only the
      // vendor's reason for it is lost, which is what the log records.
      LOGGER.debug(
        { tool_use_id: toolUseId, disabled_reason: disabledReason },
        "a push notification was declined for a reason outside the vendor's declared set; the reason is left unstated",
      );
      return { case: undefined };
  }
}

/** The vendor's ISO send instant, as epoch millis. UNSET when it stated none. */
function sentAtMsOf(output: Record<string, unknown>, toolUseId: string): bigint | undefined {
  const sentAt = str(output, "sentAt");
  if (sentAt === undefined) return undefined;
  const parsed = Date.parse(sentAt);
  if (!Number.isFinite(parsed)) {
    LOGGER.debug(
      { tool_use_id: toolUseId, sent_at: sentAt },
      "a push notification stated a send instant that is not a parsable timestamp; it is left unstated",
    );
    return undefined;
  }
  return BigInt(Math.trunc(parsed));
}

/** The one wrapper every frame of this unit shares. */
function item(
  state: conversationv1.AgentPushNotification["state"],
): conversationv1.AgentActivity["item"] {
  return {
    case: "pushNotification",
    value: create(conversationv1.AgentPushNotificationSchema, { state }),
  };
}

export const pushNotificationConverter: ToolConverter = {
  kind: "push_notification",
  // AgentPushNotification declares no progress arm.
  carriesProgress: false,

  start(call) {
    const message = str(call.input, "message");
    if (message === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a push notification names no message; there is nothing to tell the user",
      );
    }
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a push notification was sent");
    return item({
      case: "start",
      value: create(conversationv1.AgentPushNotificationStartSchema, {
        message: message ?? "",
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the push notification never ran");
      return item({
        case: "failure",
        value: create(conversationv1.AgentPushNotificationFailureSchema, {
          error: failureOf(call, outcome),
        }),
      });
    }
    const output = asRecord(outcome.structured);
    if (output === undefined) {
      // Sent and not-sent are opposite claims, and only the typed output tells
      // them apart; neither can be asserted from nothing.
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a settled push notification carried no typed output; whether it was delivered cannot be told",
      );
      return undefined;
    }
    const disabledReason = str(output, "disabledReason");
    if (disabledReason !== undefined) {
      LOGGER.logVerbose(
        { tool_use_id: call.toolUseId, disabled_reason: disabledReason },
        "the vendor declined to deliver the push notification",
      );
      return item({
        case: "success",
        value: create(conversationv1.AgentPushNotificationSuccessSchema, {
          outcome: {
            case: "notSent",
            value: create(conversationv1.AgentPushNotificationNotSentSchema, {
              reason: reasonOf(disabledReason, call.toolUseId),
            }),
          },
          settledAt: settle(call, outcome),
        }),
      });
    }
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the vendor delivered the push notification");
    return item({
      case: "success",
      value: create(conversationv1.AgentPushNotificationSuccessSchema, {
        outcome: {
          case: "sent",
          value: create(conversationv1.AgentPushNotificationSentSchema, {
            pushSent: bool(output, "pushSent") ?? false,
            localSent: bool(output, "localSent") ?? false,
            sentAtMs: sentAtMsOf(output, call.toolUseId),
          }),
        },
        settledAt: settle(call, outcome),
      }),
    });
  },
};

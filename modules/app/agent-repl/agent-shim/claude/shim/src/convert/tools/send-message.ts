/**
 * convert/tools/send-message.ts — one agent addressing another.
 *
 * # Two identifier spaces, one send
 *
 * `addressed_to` is a PLAIN STRING and deliberately not a typed `AgentId`: at
 * the moment of the call the recipient is unresolved — the caller may have
 * written a human-readable name a spawn was given — and typing it as an identity
 * would claim a resolution nobody has performed. The resolved identity appears
 * only on the success arm, minted through {@link subagentId} from the vendor's
 * own agent id.
 *
 * # The one structured discriminator
 *
 * The vendor distinguishes a resumed recipient from a live one ONLY inside a
 * prose sentence written for the model — except for `resumedAgentId`, which it
 * sets exactly when it resumed a dormant agent. That field is therefore the
 * whole basis of the `delivery` arm: present means `resumed_recipient`, absent
 * means `queued_to_live`.
 *
 * # The settle stands alone
 *
 * Both settle arms RESTATE the address and the summary from the call's own
 * input. The start and the settle upsert ONE unit, so once the send settles the
 * store holds the settle alone, and a replay drawing it with no start beside it
 * drew the send with an empty body.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import { subagentId } from "../ids.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, failureOf, obj, str } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-send-message",
  operation: "shim.convert.send_message",
});

/** One send item, whatever arm it carries. */
function sendItem(
  result: conversationv1.AgentSendMessage["result"],
): conversationv1.AgentActivity["item"] {
  return {
    case: "sendMessage",
    value: create(conversationv1.AgentSendMessageSchema, { result }),
  };
}

/**
 * The message's full text.
 *
 * The vendor declares no `SendMessageInput`, so both spellings the tool is
 * called with are read; neither is invented, and the corpus carries no send's
 * input to prefer one over the other.
 */
function bodyOf(call: PendingCall): conversationv1.AgentSendMessageBody {
  const text = str(call.input, "message") ?? str(call.input, "body");
  if (text === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a send states no message text; the body is carried empty rather than the send being lost",
    );
  }
  return create(conversationv1.AgentSendMessageBodySchema, { text: text ?? "" });
}

/** The one-line preview a surface draws IN PLACE OF the body. */
function summaryOf(call: PendingCall): conversationv1.AgentSendMessageSummary | undefined {
  const text = str(call.input, "summary");
  return text === undefined
    ? undefined
    : create(conversationv1.AgentSendMessageSummarySchema, { text });
}

/**
 * WHO the caller addressed, exactly as written. Read by the start AND by both
 * settle arms: a settled frame restates it so it stands alone, because the start
 * and the settle upsert one unit and a store keeping the latest frame has no
 * start left to read it from.
 */
function addressedToOf(call: PendingCall): string {
  const addressedTo = str(call.input, "to");
  if (addressedTo === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a send addresses nobody; the recipient is carried empty",
    );
  }
  return addressedTo ?? "";
}

/**
 * WHICH AGENT the send actually reached.
 *
 * `resumedAgentId` first because it is the vendor's own statement of the
 * resolved recipient; the pin's id is the same value on a live delivery, where
 * no resume happened.
 */
function recipientOf(structured: Record<string, unknown>): string | undefined {
  const resumed = str(structured, "resumedAgentId");
  if (resumed !== undefined && resumed !== "") return resumed;
  const pinned = str(obj(structured, "pin"), "id");
  return pinned === undefined || pinned === "" ? undefined : pinned;
}

/** The `SendMessage` tool: the visible cause of another agent's renewed activity. */
export const sendMessageConverter: ToolConverter = {
  kind: "send_message",
  // `AgentSendMessage` declares the vendor's liveness beat as an arm of its own.
  carriesProgress: true,

  start(call) {
    return sendItem({
      case: "start",
      value: create(conversationv1.AgentSendMessageStartSchema, {
        addressedTo: addressedToOf(call),
        summary: summaryOf(call),
        body: bodyOf(call),
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a send failed");
      return sendItem({
        case: "failure",
        value: create(conversationv1.AgentSendMessageFailureSchema, {
          error: failureOf(outcome),
          // RESTATED so the settled frame stands alone: a refused send is
          // still drawn, and its start is gone once the settle upserts over it.
          addressedTo: addressedToOf(call),
          summary: summaryOf(call),
        }),
      });
    }
    const structured = asRecord(outcome.structured);
    const recipient = structured === undefined ? undefined : recipientOf(structured);
    if (recipient === undefined) {
      // NO SUCCESS FRAME. `recipient_agent_id` is not optional, and a send whose
      // recipient cannot be resolved would otherwise claim it reached an agent
      // nobody named.
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a send resolved no recipient identity; no success frame is produced",
      );
      return undefined;
    }
    const resumed = str(structured, "resumedAgentId") !== undefined;
    LOGGER.logVerbose(
      { tool_use_id: call.toolUseId, resumed },
      "a send reached its recipient",
    );
    return sendItem({
      case: "success",
      value: create(conversationv1.AgentSendMessageSuccessSchema, {
        recipientAgentId: subagentId(recipient),
        delivery: resumed
          ? {
              case: "resumedRecipient",
              value: create(conversationv1.AgentSendMessageResumedRecipientSchema, {}),
            }
          : {
              case: "queuedToLive",
              value: create(conversationv1.AgentSendMessageQueuedToLiveSchema, {}),
            },
        settledAt: settledAt(outcome.settledAtMs),
        // RESTATED so the settled frame stands alone: a replay of the unit's
        // latest frame draws the send from this alone.
        addressedTo: addressedToOf(call),
        summary: summaryOf(call),
      }),
    });
  },

  progress(beat) {
    return sendItem({ case: "progress", value: beat });
  },
};

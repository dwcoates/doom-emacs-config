/**
 * convert/tools/web-fetch.ts — one URL fetched, and what the server answered.
 *
 * # The target is the CALL'S, never the result's
 *
 * `WebFetchOutput` restates a `url`, but a redirect makes the two differ: the
 * corpus's one observed fetch asked for `api.slack.com/methods` and the result
 * describes a 302 to another host. The unit is "what the agent asked for", so
 * the target on EVERY frame comes from the input — the settled frame then
 * stands alone without claiming the agent asked for somewhere it did not.
 *
 * # An HTTP error is a SUCCESS
 *
 * A 302 and a 404 are served answers; the status carries them. The failure arm
 * means the fetch never ran at all, which is exactly what `is_error` reports.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, big, failureOf, obj, str, strOr, uint } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-web-fetch",
  operation: "shim.convert.tools.web_fetch",
});

/** What was asked for, from the CALL — carried on every frame of the unit. */
function targetOf(call: PendingCall): conversationv1.AgentWebFetchTarget {
  const url = str(call.input, "url");
  if (url === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a web fetch call names no url; the unit's target cannot restate one",
    );
  }
  return create(conversationv1.AgentWebFetchTargetSchema, { url: url ?? "" });
}

/** How the server answered. */
function statusOf(output: Record<string, unknown>): conversationv1.AgentWebFetchHttpStatus {
  return create(conversationv1.AgentWebFetchHttpStatusSchema, {
    code: uint(output, "code") ?? 0,
    text: strOr(output, "codeText"),
  });
}

/** The one wrapper every frame of this unit shares. */
function item(result: conversationv1.AgentWebFetch["result"]): conversationv1.AgentActivity["item"] {
  return {
    case: "webFetch",
    value: create(conversationv1.AgentWebFetchSchema, { result }),
  };
}

export const webFetchConverter: ToolConverter = {
  kind: "web_fetch",
  // AgentWebFetch declares an AgentToolCallProgress arm.
  carriesProgress: true,

  start(call) {
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a web fetch was issued");
    return item({
      case: "start",
      value: create(conversationv1.AgentWebFetchStartSchema, {
        target: targetOf(call),
        startedAtMs: BigInt(Math.trunc(call.startedAtMs)),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the web fetch never ran");
      return item({
        case: "failure",
        value: create(conversationv1.AgentWebFetchFailureSchema, {
          target: targetOf(call),
          failure: failureOf(call, outcome),
        }),
      });
    }
    const output = asRecord(outcome.structured);
    if (output === undefined) {
      // The status is a REQUIRED submessage of the success arm and only the
      // typed output states it; a message built without it would claim a code
      // the server never sent. No message, and the gap is recorded.
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a settled web fetch carried no typed output; no success frame can be built without its http status",
      );
      return undefined;
    }
    LOGGER.logVerbose(
      { tool_use_id: call.toolUseId, code: uint(output, "code") },
      "the web fetch was answered",
    );
    return item({
      case: "success",
      value: create(conversationv1.AgentWebFetchSuccessSchema, {
        target: targetOf(call),
        status: statusOf(output),
        result: strOr(output, "result"),
        bytes: big(output, "bytes") ?? 0n,
        durationMs: big(output, "durationMs") ?? 0n,
        // Presence IS the fact: the vendor states the artifact route by
        // supplying the descriptor, never by a boolean.
        artifactRead: obj(output, "artifactRead") !== undefined,
      }),
    });
  },

  progress(beat) {
    return item({ case: "progress", value: beat });
  },
};

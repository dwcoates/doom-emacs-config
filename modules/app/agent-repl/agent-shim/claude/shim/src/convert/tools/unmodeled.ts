/**
 * convert/tools/unmodeled.ts — a tool this contract does not model.
 *
 * NOT A FALLBACK FOR CONVENIENCE. This kind is for a tool whose schema
 * genuinely cannot be known to the producer — a built-in added after the
 * schema was written. A recognizable built-in arriving here is a PRODUCER
 * DEFECT, which is why the registry's table, not this file, decides who lands
 * here. An MCP server's tool is NOT unmodeled: it is an ordinary tool call
 * (mcp.ts).
 *
 * # The arguments are untyped BY NECESSITY
 *
 * The producer holds no schema for a tool it did not define, so the input is
 * carried as a `Struct`: structured enough that a generic view can list fields
 * without re-parsing, and NOTHING here branches on a key inside it.
 */
import { create, type JsonObject } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { returnedContent, untypedArguments } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-tools",
  operation: "shim.convert.tools.unmodeled",
});

/** What the tool returned, recorded when the vendor returned nothing. */
function content(outcome: ToolOutcome): conversationv1.ToolResultContent {
  const { content: returned, returned: answered } = returnedContent(outcome);
  if (!answered) LOGGER.logVerbose({}, "an unmodeled tool returned no content; an empty result is recorded");
  return returned;
}

/** The call's input as the untyped `Struct` the arm carries. */
function argumentsOf(call: PendingCall): JsonObject | undefined {
  const args = untypedArguments(call);
  if (args === undefined) {
    LOGGER.debug(
      { tool: call.toolName, tool_use_id: call.toolUseId },
      "an unmodeled tool's arguments could not be represented as a Struct; they are left unset",
    );
  }
  return args;
}

/** A tool this contract does not model. */
export const unmodeledConverter: ToolConverter = {
  kind: "unmodeled",
  carriesProgress: true,

  start(call) {
    return {
      case: "unmodeled",
      value: create(conversationv1.AgentUnmodeledSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentUnmodeledStartSchema, {
            toolName: call.toolName,
            arguments: argumentsOf(call),
            startedAt: startedAt(call.startedAtMs),
          }),
        },
      }),
    };
  },

  settle(call, outcome) {
    if (outcome.isError) {
      LOGGER.logVerbose(
        { tool: call.toolName, tool_use_id: call.toolUseId },
        "an unmodeled tool failed",
      );
      return {
        case: "unmodeled",
        value: create(conversationv1.AgentUnmodeledSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentUnmodeledFailureSchema, {
              toolName: call.toolName,
              content: content(outcome),
                settledAt: settledAt(outcome.settledAtMs, call.startedAtMs),
            }),
          },
        }),
      };
    }
    LOGGER.logVerbose(
      { tool: call.toolName, tool_use_id: call.toolUseId },
      "an unmodeled tool returned",
    );
    return {
      case: "unmodeled",
      value: create(conversationv1.AgentUnmodeledSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentUnmodeledSuccessSchema, {
            toolName: call.toolName,
            content: content(outcome),
            settledAt: settledAt(outcome.settledAtMs, call.startedAtMs),
          }),
        },
      }),
    };
  },

  progress(beat) {
    return {
      case: "unmodeled",
      value: create(conversationv1.AgentUnmodeledSchema, {
        result: { case: "progress", value: beat },
      }),
    };
  },
};

/**
 * convert/tools/unmodeled.ts — a tool this contract does not model.
 *
 * NOT A FALLBACK FOR CONVENIENCE. This kind is for a tool whose schema
 * genuinely cannot be known to the producer — an MCP server's tool registered
 * at runtime, or a built-in added after the schema was written. A recognizable
 * built-in arriving here is a PRODUCER DEFECT, which is why the registry's
 * table, not this file, decides who lands here.
 *
 * # The arguments are untyped BY NECESSITY
 *
 * The producer holds no schema for a tool it did not define, so the input is
 * carried as a `Struct`: structured enough that a generic view can list fields
 * without re-parsing, and NOTHING here branches on a key inside it.
 *
 * # `mcp_server` is STATED, never parsed
 *
 * The qualified tool name could be split — but the qualification grammar is the
 * vendor's, and a server whose own name contains the separator would be split
 * wrongly. The vendor states the server nowhere the fold can see it (neither
 * the `tool_use` block nor the tool's input carries such a field), so this
 * producer leaves it UNSET rather than deriving it from the name.
 */
import { create, isMessage, toJson, type JsonObject } from "@bufbuild/protobuf";
import { StructSchema } from "@bufbuild/protobuf/wkt";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import { rawStruct } from "../residue.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";

const LOGGER = bindLog({
  component: "shim-convert-tools",
  operation: "shim.convert.tools.unmodeled",
});

/**
 * What the tool returned.
 *
 * NON-OPTIONAL on both terminal arms, so when the vendor returned nothing at
 * all an EMPTY content is the honest value: the call did settle, and the frame
 * says the tool answered with no blocks rather than pretending it never
 * answered.
 */
function content(outcome: ToolOutcome): conversationv1.ToolResultContent {
  if (outcome.content !== undefined) return outcome.content;
  LOGGER.logVerbose({}, "an unmodeled tool returned no content; an empty result is recorded");
  return create(conversationv1.ToolResultContentSchema, {});
}

/**
 * The call's input as the untyped `Struct` the arm carries.
 *
 * `rawStruct` is the one place a vendor record is checked for JSON
 * representability, and its answer is normalized to the generated field's JSON
 * form here — a `Struct` and its JSON object are the same value in two
 * spellings, and this converter accepts whichever the helper hands back.
 */
function argumentsOf(call: PendingCall): JsonObject | undefined {
  const raw = rawStruct(call.input);
  if (raw === undefined) {
    LOGGER.log(
      { level: "warn", tool: call.toolName, tool_use_id: call.toolUseId },
      "an unmodeled tool's arguments could not be represented as a Struct; they are left unset",
    );
    return undefined;
  }
  return (isMessage(raw, StructSchema) ? toJson(StructSchema, raw) : raw) as JsonObject;
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
            // UNSET: no vendor field states the serving MCP server, and the
            // qualified name is never split to guess one.
            mcpServer: undefined,
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
              mcpServer: undefined,
              settledAt: settledAt(outcome.settledAtMs),
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
            mcpServer: undefined,
            settledAt: settledAt(outcome.settledAtMs),
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

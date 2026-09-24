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
 * # `mcp_server` is RESOLVED BY LOOKUP, never parsed
 *
 * No vendor field states which server served an `mcp__<server>__<tool>` call —
 * neither the corpus nor the SDK declares one — and the qualified name cannot
 * simply be split, because the qualification grammar is the vendor's and a
 * server whose own name contains the separator would be split wrongly.
 *
 * So the candidate segment is matched EXACTLY against the server names the
 * session actually knows (`system:init.mcp_servers`, `mcpServerStatus()`),
 * which arrive on the tool environment. A match is a resolution; anything else
 * leaves the field UNSET and is logged (shim lead's ruling, 2026-08-29).
 */
import { create, isMessage, toJson, type JsonObject } from "@bufbuild/protobuf";
import { StructSchema } from "@bufbuild/protobuf/wkt";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import { rawStruct } from "../residue.js";
import type { PendingCall, ToolConverter, ToolEnvironment, ToolOutcome } from "../tool-calls.js";

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
    LOGGER.debug(
      { tool: call.toolName, tool_use_id: call.toolUseId },
      "an unmodeled tool's arguments could not be represented as a Struct; they are left unset",
    );
    return undefined;
  }
  return (isMessage(raw, StructSchema) ? toJson(StructSchema, raw) : raw);
}

/** The vendor's MCP tool-name prefix. */
const MCP_PREFIX = "mcp__";

/** The separator between an MCP server's name and its tool's. */
const MCP_SEPARATOR = "__";

/**
 * Which MCP server served this call, when the session knows a name that matches.
 *
 * A LOOKUP, NOT A PARSE: the split is only ever a CANDIDATE, and it becomes an
 * answer solely by matching a server the session actually has. That is what
 * makes a server called `my__server` safe — its qualified names split wrongly,
 * every candidate misses, and the field stays unset instead of naming `my`.
 */
function resolveMcpServer(
  toolName: string,
  environment: ToolEnvironment | undefined,
): string | undefined {
  if (!toolName.startsWith(MCP_PREFIX)) return undefined;
  const known = environment?.mcpServerNames ?? [];
  const remainder = toolName.slice(MCP_PREFIX.length);
  for (const name of known) {
    if (remainder === name) continue;
    if (remainder.startsWith(`${name}${MCP_SEPARATOR}`)) return name;
  }
  LOGGER.debug(
    { tool: toolName, known_servers: known.length },
    "no MCP server this session knows matches this qualified tool name; the server is left unset",
  );
  return undefined;
}

/** A tool this contract does not model. */
export const unmodeledConverter: ToolConverter = {
  kind: "unmodeled",
  carriesProgress: true,

  start(call, environment) {
    return {
      case: "unmodeled",
      value: create(conversationv1.AgentUnmodeledSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentUnmodeledStartSchema, {
            toolName: call.toolName,
            arguments: argumentsOf(call),
            startedAt: startedAt(call.startedAtMs),
            mcpServer: resolveMcpServer(call.toolName, environment),
          }),
        },
      }),
    };
  },

  settle(call, outcome, environment) {
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
              mcpServer: resolveMcpServer(call.toolName, environment),
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
            mcpServer: resolveMcpServer(call.toolName, environment),
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

/**
 * convert/tools/mcp.ts — a tool an MCP server provides: an ORDINARY TOOL CALL.
 *
 * It starts, may beat while it runs, and settles with what it returned or how
 * it failed, exactly as a Read or a Bash does. Only the schema is different:
 * the server registers the tool at runtime, so the input rides untyped (a
 * `Struct`), and NOTHING here branches on a key inside it.
 *
 * # The address is RESOLVED BY LOOKUP, never parsed
 *
 * No vendor field states which server served an `mcp__<server>__<tool>` call,
 * and the qualified name cannot simply be split: the qualification grammar is
 * the vendor's, and a server whose own name contains the separator would be
 * split wrongly. So the candidate segment is matched EXACTLY against the server
 * names the session actually knows (`system:init.mcp_servers`,
 * `mcpServerStatus()`), which arrive on the tool environment. A match is a
 * resolution; anything else leaves the address UNSET and is logged (shim lead's
 * ruling, 2026-08-29).
 *
 * # The settle stands alone
 *
 * Both settled arms restate the tool and the arguments the start carried.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import { MCP_PREFIX, type PendingCall, type ToolConverter, type ToolEnvironment } from "../tool-calls.js";
import { failureOf, returnedContent, untypedArguments } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-tools",
  operation: "shim.convert.tools.mcp",
});

/** The separator between an MCP server's name and its tool's. */
const MCP_SEPARATOR = "__";

/**
 * Which server and which of its tools this call addressed, when the session
 * knows a server name that matches.
 *
 * A LOOKUP, NOT A PARSE: the split is only ever a CANDIDATE, and it becomes an
 * answer solely by matching a server the session actually has. That is what
 * makes a server called `my__server` safe — its qualified names split wrongly,
 * every candidate misses, and the address stays unset instead of naming `my`.
 */
export function resolveMcpAddress(
  toolName: string,
  environment: ToolEnvironment | undefined,
): conversationv1.AgentMcpToolAddress | undefined {
  const known = environment?.mcpServerNames ?? [];
  const remainder = toolName.slice(MCP_PREFIX.length);
  for (const server of known) {
    const prefix = `${server}${MCP_SEPARATOR}`;
    if (remainder.startsWith(prefix) && remainder.length > prefix.length) {
      return create(conversationv1.AgentMcpToolAddressSchema, {
        server,
        tool: remainder.slice(prefix.length),
      });
    }
  }
  LOGGER.debug(
    { tool: toolName, known_servers: known.length },
    "no MCP server this session knows matches this qualified tool name; the address is left unset",
  );
  return undefined;
}

/** Which tool, restated on every frame of the unit. */
function toolOf(call: PendingCall, environment: ToolEnvironment | undefined): conversationv1.AgentMcpTool {
  return create(conversationv1.AgentMcpToolSchema, {
    name: call.toolName,
    address: resolveMcpAddress(call.toolName, environment),
  });
}

/** The call's input as the untyped `Struct` the arm carries. */
function argumentsOf(call: PendingCall): ReturnType<typeof untypedArguments> {
  const args = untypedArguments(call);
  if (args === undefined) {
    LOGGER.debug(
      { tool: call.toolName, tool_use_id: call.toolUseId },
      "an MCP tool's arguments could not be represented as a Struct; they are left unset",
    );
  }
  return args;
}

/** The one wrapper every frame of this unit shares. */
function item(result: conversationv1.AgentMcpToolCall["result"]): conversationv1.AgentActivity["item"] {
  return { case: "mcpToolCall", value: create(conversationv1.AgentMcpToolCallSchema, { result }) };
}

/** An MCP server's tool, drawn as the ordinary tool card. */
export const mcpToolConverter: ToolConverter = {
  kind: "mcp_tool_call",
  carriesProgress: true,

  start(call, environment) {
    LOGGER.logVerbose({ tool: call.toolName, tool_use_id: call.toolUseId }, "an MCP tool was called");
    return item({
      case: "start",
      value: create(conversationv1.AgentMcpToolCallStartSchema, {
        tool: toolOf(call, environment),
        arguments: argumentsOf(call),
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call, outcome, environment) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool: call.toolName, tool_use_id: call.toolUseId }, "an MCP tool failed");
      return item({
        case: "failure",
        value: create(conversationv1.AgentMcpToolCallFailureSchema, {
          tool: toolOf(call, environment),
          arguments: argumentsOf(call),
          error: failureOf(call, outcome),
        }),
      });
    }
    const { content, returned } = returnedContent(outcome);
    LOGGER.logVerbose(
      { tool: call.toolName, tool_use_id: call.toolUseId, returned_content: returned },
      "an MCP tool returned",
    );
    return item({
      case: "success",
      value: create(conversationv1.AgentMcpToolCallSuccessSchema, {
        tool: toolOf(call, environment),
        arguments: argumentsOf(call),
        content,
        settledAt: settledAt(outcome.settledAtMs, call.startedAtMs),
      }),
    });
  },

  progress(beat) {
    return item({ case: "progress", value: beat });
  },
};

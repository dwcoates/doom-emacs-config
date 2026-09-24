package convert

// mcp.go — AN MCP SERVER'S TOOL IS AN ORDINARY TOOL CALL (AgentMcpToolCall),
// never AgentUnmodeled. Its schema is the server's, so its input rides untyped,
// and both settled arms restate the tool and the arguments the call carried.
//
// THE ADDRESS IS STATED, NEVER PARSED. The qualification grammar
// (`mcp__<server>__<tool>`) is the vendor's, and a server whose own name holds
// the separator would split wrongly. This reader knows no session's server
// list, so it states the address only when the vendor named the server on the
// block (an API-side `mcp_tool_use`), and leaves it unset otherwise.

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// mcpPrefix is the vendor's MCP tool-name prefix.
const mcpPrefix = "mcp__"

// mcpTool answers which MCP tool a call addressed, or nil when the call is no
// MCP server's tool: its name carries the vendor's MCP prefix, or its block is
// the API's `mcp_tool_use`.
func mcpTool(name string, block map[string]any) *conversationv1.AgentMcpTool {
	apiSide := str(block["type"]) == "mcp_tool_use"
	if !apiSide && !strings.HasPrefix(name, mcpPrefix) {
		return nil
	}
	tool := &conversationv1.AgentMcpTool{Name: name}
	if server := optionalString(pick(block, "server_name", "mcp_server")); server != nil && apiSide {
		// The API-side block names the server beside the tool's bare name.
		tool.Address = &conversationv1.AgentMcpToolAddress{Server: *server, Tool: name}
	}
	return tool
}

// mcpToolCall announces an MCP server's tool.
func (c *Converter) mcpToolCall(tool *conversationv1.AgentMcpTool, id string, index int, input map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	c.log.With(at.ctxFor("mcp-tool-call")).With(logging.Context{ActivityID: id, UpsertKey: ActivityKey(id)}).
		LogVerbose("MCP tool call name=%q addressed=%t announced", tool.GetName(), tool.GetAddress() != nil)
	return c.activityEntry(at, env, agent, id, index, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_McpToolCall{McpToolCall: &conversationv1.AgentMcpToolCall{
			Result: &conversationv1.AgentMcpToolCall_Start{Start: &conversationv1.AgentMcpToolCallStart{
				Tool:      tool,
				Arguments: rawStruct(input),
				StartedAt: startedAt(env.timestampMs),
			}},
		}},
	})
}

// settleMcpToolCall settles an MCP server's tool, restating the tool and the
// arguments the call carried.
func (c *Converter) settleMcpToolCall(call openCall, block map[string]any, failed bool, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	var activity *conversationv1.AgentActivity
	if failed {
		activity = item(&conversationv1.AgentActivity_McpToolCall{McpToolCall: &conversationv1.AgentMcpToolCall{
			Result: &conversationv1.AgentMcpToolCall_Failure{Failure: &conversationv1.AgentMcpToolCallFailure{
				Tool:      call.mcp,
				Arguments: rawStruct(call.input),
				Error:     toolFailure(block, env.timestampMs, call.startedAt),
			}},
		}})
	} else {
		content := resultContent(block["content"])
		if content == nil {
			// The call did settle: an empty answer is the honest value.
			content = &conversationv1.ToolResultContent{}
		}
		activity = item(&conversationv1.AgentActivity_McpToolCall{McpToolCall: &conversationv1.AgentMcpToolCall{
			Result: &conversationv1.AgentMcpToolCall_Success{Success: &conversationv1.AgentMcpToolCallSuccess{
				Tool:      call.mcp,
				Arguments: rawStruct(call.input),
				Content:   content,
				SettledAt: settledAt(env.timestampMs, call.startedAt),
			}},
		}})
	}
	activity.ActivityId = activityID(call.activityID)
	c.log.With(at.ctxFor("mcp-tool-return")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID)}).
		LogVerbose("MCP tool name=%q settled failed=%t", call.name, failed)
	return c.settledEntry(at, agent, call.activityID, activity)
}

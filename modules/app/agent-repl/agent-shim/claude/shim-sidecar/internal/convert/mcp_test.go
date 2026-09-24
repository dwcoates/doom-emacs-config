package convert

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// The shapes below are the claude-in-chrome server's, as the vendor wrote them
// in real transcripts: a qualified `mcp__claude-in-chrome__<tool>` name, a
// text-block result, and an `is_error` result whose content is a bare string.

func TestMcpToolClassifiesTheCall(t *testing.T) {
	// Arrange.
	tests := []struct {
		name        string
		toolName    string
		block       map[string]any
		wantMcp     bool
		wantAddress *conversationv1.AgentMcpToolAddress
	}{
		{name: "a qualified MCP name", toolName: "mcp__claude-in-chrome__navigate", block: map[string]any{"type": "tool_use"}, wantMcp: true},
		{name: "a built-in", toolName: "Read", block: map[string]any{"type": "tool_use"}, wantMcp: false},
		{name: "a genuinely unknown tool", toolName: "StructuredOutput", block: map[string]any{"type": "tool_use"}, wantMcp: false},
		{
			name: "an API-side block naming its server", toolName: "search",
			block:   map[string]any{"type": "mcp_tool_use", "server_name": "docs"},
			wantMcp: true, wantAddress: &conversationv1.AgentMcpToolAddress{Server: "docs", Tool: "search"},
		},
		{name: "an API-side block naming no server", toolName: "search", block: map[string]any{"type": "mcp_tool_use"}, wantMcp: true},
		{
			name: "a qualified name whose block names a server", toolName: "mcp__docs__search",
			block: map[string]any{"type": "tool_use", "server_name": "docs"}, wantMcp: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			tool := mcpTool(tt.toolName, tt.block)

			// Assert.
			if (tool != nil) != tt.wantMcp {
				t.Fatalf("mcpTool = %v, want an MCP tool = %t", tool, tt.wantMcp)
			}
			if tool == nil {
				return
			}
			if tool.GetName() != tt.toolName {
				t.Fatalf("name = %q, want %q", tool.GetName(), tt.toolName)
			}
			got, want := tool.GetAddress(), tt.wantAddress
			if (got == nil) != (want == nil) || got.GetServer() != want.GetServer() || got.GetTool() != want.GetTool() {
				t.Fatalf("address = %v, want %v", got, want)
			}
		})
	}
}

func TestAnMcpToolCallIsAnnouncedAsAnMcpToolCall(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	blocks := toolCall("toolu_m", "mcp__claude-in-chrome__tabs_context_mcp", `{"createIfEmpty":true}`)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, blocks))

	// Assert.
	start := activityOf(entries[0]).GetMcpToolCall().GetStart()
	if start == nil {
		t.Fatalf("activity = %v, want an AgentMcpToolCall start", activityOf(entries[0]))
	}
	if start.GetTool().GetName() != "mcp__claude-in-chrome__tabs_context_mcp" || start.GetArguments().GetFields()["createIfEmpty"].GetBoolValue() != true {
		t.Fatalf("start = %v, want the tool and its arguments", start)
	}
}

func TestAnMcpToolCallIsNeverUnmodeled(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	blocks := toolCall("toolu_m", "mcp__claude-in-chrome__navigate", `{"tabId":1,"url":"https://a.example"}`)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, blocks))

	// Assert.
	if activityOf(entries[0]).GetUnmodeled() != nil {
		t.Fatal("an MCP server's tool landed as AgentUnmodeled")
	}
}

func TestAnMcpToolsResultSettlesRestatingTheCall(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_m", "mcp__claude-in-chrome__navigate", `{"tabId":1}`))
	result := toolResultLine("u1", "toolu_m", ts2, `[{"type":"text","text":"Navigated to https://a.example"}]`, `[{"type":"text","text":"Navigated to https://a.example"}]`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	success := activityOf(lastEntryByKey(t, entries, ActivityKey("toolu_m"))).GetMcpToolCall().GetSuccess()
	if success == nil {
		t.Fatal("an MCP tool's result must settle AgentMcpToolCall_Success")
	}
	if success.GetTool().GetName() != "mcp__claude-in-chrome__navigate" || success.GetArguments().GetFields()["tabId"].GetNumberValue() != 1 {
		t.Fatalf("success = %v, want the tool and its arguments restated", success)
	}
	if got := success.GetContent().GetBlocks()[0].GetText().GetText(); got != "Navigated to https://a.example" {
		t.Fatalf("content = %q, want what the tool returned", got)
	}
	if success.GetSettledAt().GetStartedAt() == nil {
		t.Fatal("the settle instant must restate the start it closes")
	}
}

func TestAnMcpToolsErrorSettlesOnTheFailureArmRestatingTheCall(t *testing.T) {
	// Arrange: the vendor's is_error result, content a bare string.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_m", "mcp__claude-in-chrome__computer", `{"action":"screenshot"}`))
	result := toolResultLineWithError("u1", "toolu_m", ts2, `"Error: Couldn't determine which page this action targets."`, `"Error: Couldn't determine which page this action targets."`, true)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	failure := activityOf(lastEntryByKey(t, entries, ActivityKey("toolu_m"))).GetMcpToolCall().GetFailure()
	if failure == nil {
		t.Fatal("an is_error MCP result must settle on the failure arm")
	}
	if failure.GetTool().GetName() != "mcp__claude-in-chrome__computer" || failure.GetArguments().GetFields()["action"].GetStringValue() != "screenshot" {
		t.Fatalf("failure = %v, want the tool and its arguments restated", failure)
	}
	if got := failure.GetError().GetContent().GetBlocks()[0].GetText().GetText(); got != "Error: Couldn't determine which page this action targets." {
		t.Fatalf("error = %q, want the tool's own account", got)
	}
}

func TestAnMcpToolThatReturnedNothingSettlesAnEmptyContent(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_m", "mcp__claude-in-chrome__tabs_close_mcp", `{}`))
	result := toolResultLine("u1", "toolu_m", ts2, `null`, ``)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	success := activityOf(lastEntryByKey(t, entries, ActivityKey("toolu_m"))).GetMcpToolCall().GetSuccess()
	if success == nil || success.GetContent() == nil || len(success.GetContent().GetBlocks()) != 0 {
		t.Fatalf("success = %v, want an empty, present content", success)
	}
}

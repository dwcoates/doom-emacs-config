package footer

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// activityOf wraps one item as an activity frame.
func activityOf(item func(*conversationv1.AgentActivity)) *conversationv1.AgentActivity {
	act := &conversationv1.AgentActivity{ActivityId: &conversationv1.AgentActivityId{Value: "u-1"}}
	item(act)
	return act
}

func TestToolCallStartNamesTheToolAndComposesItsGist(t *testing.T) {
	tests := []struct {
		name        string
		act         *conversationv1.AgentActivity
		wantTool    string
		wantSummary string
	}{
		{"a read", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Start{
				Start: &conversationv1.AgentReadStart{Path: &conversationv1.ReadPath{Path: "a/b.go"}}}}}
		}), "Read", "a/b.go"},
		{"a write", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Start{
				Start: &conversationv1.AgentWriteStart{Path: &conversationv1.ReadPath{Path: "c.go"}}}}}
		}), "Write", "c.go"},
		{"an edit", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_Edit{Edit: &conversationv1.AgentEdit{Result: &conversationv1.AgentEdit_Start{
				Start: &conversationv1.AgentEditStart{Path: &conversationv1.ReadPath{Path: "d.go"}}}}}
		}), "Edit", "d.go"},
		{"a grep", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_Grep{Grep: &conversationv1.AgentGrep{Result: &conversationv1.AgentGrep_Start{
				Start: &conversationv1.AgentGrepStart{Query: &conversationv1.AgentGrepQuery{Pattern: "TODO"}}}}}
		}), "Grep", "TODO"},
		{"a glob", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_Glob{Glob: &conversationv1.AgentGlob{Result: &conversationv1.AgentGlob_Start{
				Start: &conversationv1.AgentGlobStart{Query: &conversationv1.AgentGlobQuery{Pattern: "**/*.go"}}}}}
		}), "Glob", "**/*.go"},
		{"a shell command, first line only", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
				Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: "go test ./...\necho done"}}}}}
		}), "Bash", "go test ./..."},
		{"a subagent", subagentStart("u-1", "agent-sub", "Explore", "find the call sites"), "Agent", "find the call sites"},
		{"a web search", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_WebSearch{WebSearch: &conversationv1.AgentWebSearch{Result: &conversationv1.AgentWebSearch_Start{
				Start: &conversationv1.AgentWebSearchStart{Query: &conversationv1.AgentWebSearchQuery{Terms: "protobuf oneof"}}}}}
		}), "WebSearch", "protobuf oneof"},
		{"a web fetch", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_WebFetch{WebFetch: &conversationv1.AgentWebFetch{Result: &conversationv1.AgentWebFetch_Start{
				Start: &conversationv1.AgentWebFetchStart{Target: &conversationv1.AgentWebFetchTarget{Url: "https://go.dev"}}}}}
		}), "WebFetch", "https://go.dev"},
		{"a monitor", monitorStart("m-1", "watch the build", false), "Monitor", "watch the build"},
		{"a plan-mode act with nothing worth a line", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Start{
				Start: &conversationv1.AgentPlanModeStart{}}}}
		}), "ExitPlanMode", ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			call, started := toolCallStart(tt.act)

			// Assert
			if !started {
				t.Fatalf("a %s start raised no tool-call line", tt.wantTool)
			}
			if call.GetTool() != tt.wantTool || call.GetSummary() != tt.wantSummary {
				t.Fatalf("tool call = (%q, %q), want (%q, %q)", call.GetTool(), call.GetSummary(), tt.wantTool, tt.wantSummary)
			}
			if tt.wantSummary == "" && call.Summary != nil {
				t.Fatalf("summary = %q, want UNSET when the input has nothing worth a line", call.GetSummary())
			}
		})
	}
}

// thinkingDelta is one streamed frame of visible reasoning.
func thinkingDelta(unit, text string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
			Result: &conversationv1.AgentThinking_Update{Update: &conversationv1.AgentThinkingUpdate{
				Reasoning: &conversationv1.AgentThinkingUpdate_Text{Text: &conversationv1.AgentThinkingTextDelta{NewText: text}}}}}},
	}
}

// responseDelta is one streamed frame of prose.
func responseDelta(unit, markdown string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Update{Update: &conversationv1.AgentResponseUpdate{NewMarkdown: markdown}}}},
	}
}

func TestToolCallStartIgnoresFramesThatAreNotAStart(t *testing.T) {
	tests := []struct {
		name string
		act  *conversationv1.AgentActivity
	}{
		{"a settled read", activityOf(func(a *conversationv1.AgentActivity) {
			a.Item = &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Success{
				Success: &conversationv1.AgentReadSuccess{}}}}
		})},
		{"a subagent's progress", subagentProgress("u-1", 10)},
		{"reasoning", thinkingDelta("th-1", "hmm\n")},
		{"prose", responseDelta("r-1", "hello\n")},
		{"a hook, which has its own kind", hookFrame("pre-commit", true)},
		{"a push notification, which has its own kind", notificationFrame("hi")},
		{"a task act, which has its own kind", taskAct("t-1", pendingTask(stated("x")))},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			_, started := toolCallStart(tt.act)

			// Assert
			if started {
				t.Fatalf("%s raised a tool-call line", tt.name)
			}
		})
	}
}

func TestAToolCallsGistIsCapped(t *testing.T) {
	// Arrange
	act := activityOf(func(a *conversationv1.AgentActivity) {
		a.Item = &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: strings.Repeat("x", 300)}}}}}
	})

	// Act
	call, _ := toolCallStart(act)

	// Assert
	if n := len([]rune(call.GetSummary())); n != DefaultWarningRowWidth {
		t.Fatalf("summary is %d runes, want capped at %d", n, DefaultWarningRowWidth)
	}
}

func TestAToolCallStartRaisesTheToolCallTransient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, activityOf(func(a *conversationv1.AgentActivity) {
		a.Item = &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Start{
			Start: &conversationv1.AgentReadStart{Path: &conversationv1.ReadPath{Path: "a/b.go"}}}}}
	}))

	// Assert
	call := transientOf(t, h).GetToolCall()
	if call.GetTool() != "Read" || call.GetSummary() != "a/b.go" {
		t.Fatalf("tool call = %+v, want Read a/b.go", call)
	}
}

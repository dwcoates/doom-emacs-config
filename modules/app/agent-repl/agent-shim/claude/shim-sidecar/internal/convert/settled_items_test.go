package convert

// settled_items_test.go — every failure arm RESTATES what its start carried.
// The start and the settle upsert one unit, so once the call settles the store
// holds the settle alone, and a replay drawing it with no start beside it must
// still name what the call acted on.

import (
	"testing"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestFailedSettleRestatesWhatTheStartCarried(t *testing.T) {
	tests := []struct {
		name  string
		kind  toolKind
		input map[string]any
		// restated reads the one field the failure arm restates.
		restated func(*conversationv1.AgentActivity) string
		want     string
	}{
		{
			name:     "read restates its path",
			kind:     kindRead,
			input:    map[string]any{"file_path": "/p/a.go"},
			restated: func(a *conversationv1.AgentActivity) string { return a.GetRead().GetFailure().GetPath().GetPath() },
			want:     "/p/a.go",
		},
		{
			name:     "write restates its path",
			kind:     kindWrite,
			input:    map[string]any{"file_path": "/p/b.go"},
			restated: func(a *conversationv1.AgentActivity) string { return a.GetWrite().GetFailure().GetPath().GetPath() },
			want:     "/p/b.go",
		},
		{
			name:     "edit restates its path",
			kind:     kindEdit,
			input:    map[string]any{"file_path": "/p/c.go"},
			restated: func(a *conversationv1.AgentActivity) string { return a.GetEdit().GetFailure().GetPath().GetPath() },
			want:     "/p/c.go",
		},
		{
			name:     "grep restates its pattern",
			kind:     kindGrep,
			input:    map[string]any{"pattern": "func main"},
			restated: func(a *conversationv1.AgentActivity) string { return a.GetGrep().GetFailure().GetQuery().GetPattern() },
			want:     "func main",
		},
		{
			name:     "glob restates its pattern",
			kind:     kindGlob,
			input:    map[string]any{"pattern": "**/*.go"},
			restated: func(a *conversationv1.AgentActivity) string { return a.GetGlob().GetFailure().GetQuery().GetPattern() },
			want:     "**/*.go",
		},
		{
			name:     "bash restates its command line",
			kind:     kindBash,
			input:    map[string]any{"command": "make lint"},
			restated: func(a *conversationv1.AgentActivity) string { return a.GetBash().GetFailure().GetCommand().GetLine() },
			want:     "make lint",
		},
		{
			name:     "skill restates the skill it invoked",
			kind:     kindSkill,
			input:    map[string]any{"skill": "absent-skill"},
			restated: func(a *conversationv1.AgentActivity) string { return a.GetSkillUse().GetFailure().GetSkill().GetName() },
			want:     "absent-skill",
		},
		{
			name:     "send restates its address",
			kind:     kindSendMessage,
			input:    map[string]any{"to": "vetter", "message": "go"},
			restated: func(a *conversationv1.AgentActivity) string { return a.GetSendMessage().GetFailure().GetAddressedTo() },
			want:     "vetter",
		},
		{
			name:  "send restates its summary",
			kind:  kindSendMessage,
			input: map[string]any{"to": "vetter", "message": "go", "summary": "resume the vetting"},
			restated: func(a *conversationv1.AgentActivity) string {
				return a.GetSendMessage().GetFailure().GetSummary().GetText()
			},
			want: "resume the vetting",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a call the producer refused, whose result is an error.
			c := newTestConverter(t)
			call := openCall{input: tt.input}
			block := map[string]any{"content": "Error: refused"}

			// Act
			got := c.settledItem(tt.kind, call, nil, block, true, 1000, Attribution{})

			// Assert
			if restated := tt.restated(got); restated != tt.want {
				t.Fatalf("restated = %q, want %q", restated, tt.want)
			}
		})
	}
}

func TestFailedSendLeavesTheSummaryUnsetWhenTheCallerGaveNone(t *testing.T) {
	// Arrange: no summary in the input, exactly as the start leaves it unset.
	c := newTestConverter(t)
	call := openCall{input: map[string]any{"to": "vetter", "message": "go"}}
	block := map[string]any{"content": "Error: refused"}

	// Act
	got := c.settledItem(kindSendMessage, call, nil, block, true, 1000, Attribution{})

	// Assert
	if summary := got.GetSendMessage().GetFailure().GetSummary(); summary != nil {
		t.Fatalf("Summary = %v, want unset: the caller supplied none", summary)
	}
}

// settleInstants collects every settle instant a message carries, at any depth.
func settleInstants(m proto.Message) []*conversationv1.AgentActivitySettledAt {
	var out []*conversationv1.AgentActivitySettledAt
	var walk func(protoreflect.Message)
	walk = func(msg protoreflect.Message) {
		if settled, ok := msg.Interface().(*conversationv1.AgentActivitySettledAt); ok {
			out = append(out, settled)
		}
		msg.Range(func(fd protoreflect.FieldDescriptor, v protoreflect.Value) bool {
			switch {
			case fd.IsList() && fd.Kind() == protoreflect.MessageKind:
				for i := 0; i < v.List().Len(); i++ {
					walk(v.List().Get(i).Message())
				}
			case fd.IsMap():
			case fd.Kind() == protoreflect.MessageKind:
				walk(v.Message())
			}
			return true
		})
	}
	walk(m.ProtoReflect())
	return out
}

func TestEverySettleInstantRestatesTheCallsStart(t *testing.T) {
	tests := []struct {
		name   string
		kind   toolKind
		input  map[string]any
		result map[string]any
		failed bool
	}{
		{name: "a failed read", kind: kindRead, input: map[string]any{"file_path": "/p"}, failed: true},
		{name: "a failed write", kind: kindWrite, input: map[string]any{"file_path": "/p"}, failed: true},
		{name: "a failed edit", kind: kindEdit, input: map[string]any{"file_path": "/p"}, failed: true},
		{name: "a failed grep", kind: kindGrep, input: map[string]any{"pattern": "x"}, failed: true},
		{name: "a failed glob", kind: kindGlob, input: map[string]any{"pattern": "x"}, failed: true},
		{name: "a failed shell call", kind: kindBash, input: map[string]any{"command": "x"}, failed: true},
		{name: "a failed spawn", kind: kindSubagent, input: map[string]any{"prompt": "x"}, failed: true},
		{name: "a failed skill", kind: kindSkill, input: map[string]any{"skill": "x"}, failed: true},
		{name: "a failed send", kind: kindSendMessage, input: map[string]any{"to": "x"}, failed: true},
		{name: "a failed fetch", kind: kindWebFetch, input: map[string]any{"url": "https://x"}, failed: true},
		{name: "a failed search", kind: kindWebSearch, input: map[string]any{"query": "x"}, failed: true},
		{name: "a failed artifact call", kind: kindArtifact, input: map[string]any{"file_path": "/p"}, failed: true},
		{name: "a failed plan-mode call", kind: kindPlanMode, failed: true},
		{name: "a failed findings report", kind: kindReportFindings, failed: true},
		{name: "a failed worktree call", kind: kindWorktree, failed: true},
		{name: "a failed cron call", kind: kindCron, failed: true},
		{name: "a failed push", kind: kindPushNotification, failed: true},
		{name: "a failed wakeup", kind: kindScheduleWakeup, failed: true},
		{name: "a failed monitor", kind: kindMonitor, failed: true},
		{
			name:   "a read that answered",
			kind:   kindRead,
			input:  map[string]any{"file_path": "/p"},
			result: map[string]any{"type": "text", "file": map[string]any{"filePath": "/p", "content": "c", "numLines": 1.0, "totalLines": 1.0}},
		},
		{
			name:   "a shell call that exited",
			kind:   kindBash,
			input:  map[string]any{"command": "true"},
			result: map[string]any{"stdout": "", "stderr": "", "exitCode": 0.0},
		},
		{
			name:   "a send that was delivered",
			kind:   kindSendMessage,
			input:  map[string]any{"to": "vetter", "message": "go"},
			result: map[string]any{"success": true},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a call announced at 1000 and settled at 4000.
			c := newTestConverter(t)
			call := openCall{input: tt.input, startedAt: 1000, activityID: "toolu_s"}
			block := map[string]any{"content": "Error: refused"}

			// Act
			got := c.settledItem(tt.kind, call, tt.result, block, tt.failed, 4000, Attribution{})

			// Assert
			if got == nil {
				t.Fatal("the result settled no unit")
			}
			instants := settleInstants(got)
			if len(instants) == 0 {
				t.Fatal("the settled arm carried no settle instant to check")
			}
			for _, instant := range instants {
				if instant.GetStartedAt().GetAtMs() != 1000 {
					t.Fatalf("settle instant %v restates start %v, want 1000", instant, instant.GetStartedAt())
				}
			}
		})
	}
}

func TestAFailedArtifactCallRestatesItsAct(t *testing.T) {
	tests := []struct {
		name     string
		input    map[string]any
		restated func(*conversationv1.AgentArtifactFailure) string
		want     string
	}{
		{
			name:     "a publish restates its file",
			input:    map[string]any{"file_path": "/p/page.html", "title": "The Page"},
			restated: func(f *conversationv1.AgentArtifactFailure) string { return f.GetPublish().GetFilePath() },
			want:     "/p/page.html",
		},
		{
			name:     "a publish restates its title",
			input:    map[string]any{"file_path": "/p/page.html", "title": "The Page"},
			restated: func(f *conversationv1.AgentArtifactFailure) string { return f.GetPublish().GetTitle() },
			want:     "The Page",
		},
		{
			name:  "a listing restates itself as a list",
			input: map[string]any{"action": "list", "scope": "mine"},
			restated: func(f *conversationv1.AgentArtifactFailure) string {
				if f.GetList() == nil {
					return "not a list"
				}
				return f.GetList().GetScope()
			},
			want: "mine",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: an artifact call the service refused.
			c := newTestConverter(t)
			call := openCall{input: tt.input, startedAt: 1000}
			block := map[string]any{"content": "Error: refused"}

			// Act
			got := c.settledItem(kindArtifact, call, nil, block, true, 4000, Attribution{})

			// Assert
			if restated := tt.restated(got.GetArtifact().GetFailure()); restated != tt.want {
				t.Fatalf("restated = %q, want %q", restated, tt.want)
			}
		})
	}
}

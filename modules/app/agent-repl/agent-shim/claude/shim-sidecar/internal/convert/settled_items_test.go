package convert

// settled_items_test.go — every failure arm RESTATES what its start carried.
// The start and the settle upsert one unit, so once the call settles the store
// holds the settle alone, and a replay drawing it with no start beside it must
// still name what the call acted on.

import (
	"testing"

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

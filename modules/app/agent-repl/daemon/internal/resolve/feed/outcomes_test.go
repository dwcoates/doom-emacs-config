package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// succeededHookEntry is a settled, succeeded hook: an entry that draws no row.
func succeededHookEntry(unit string) *conversationv1.HistoryEntry {
	return frameEntry(mainAgent(), &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_Succeeded{Succeeded: &conversationv1.AgentHookSucceeded{Command: "true"}},
		}},
	}}})
}

func TestEntryKind(t *testing.T) {
	tests := []struct {
		name  string
		entry *conversationv1.HistoryEntry
		want  string
	}{
		{name: "a prompt", entry: promptEntry("turn-1", "hi"), want: "user_prompt"},
		{name: "a succeeded hook names every arm to its outcome", entry: succeededHookEntry("hook-1"), want: "agent_frame.update.activity.hook.succeeded"},
		{name: "an entry with no arm", entry: &conversationv1.HistoryEntry{}, want: "unset"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			at := &conversationv1.HistoryEntryAt{Entry: tt.entry}

			// Act.
			got := entryKind(at)

			// Assert.
			if got != tt.want {
				t.Fatalf("entryKind = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestEntryOutcomesRenderSortedWithCounts(t *testing.T) {
	// Arrange.
	var outcomes entryOutcomes
	hook := &conversationv1.HistoryEntryAt{Entry: succeededHookEntry("hook-1")}
	prompt := &conversationv1.HistoryEntryAt{Entry: promptEntry("turn-1", "hi")}

	// Act.
	outcomes.note(prompt, outcomeDrew)
	outcomes.note(hook, outcomeNoRow)
	outcomes.note(hook, outcomeNoRow)

	// Assert.
	if got, want := outcomes.String(), "agent_frame.update.activity.hook.succeeded=no_row:2 user_prompt=drew:1"; got != want {
		t.Fatalf("outcomes = %q, want %q", got, want)
	}
}

func TestAnEmptyTallyRendersEmpty(t *testing.T) {
	// Arrange, Act.
	var outcomes entryOutcomes

	// Assert.
	if got := outcomes.String(); got != "" {
		t.Fatalf("outcomes = %q, want empty", got)
	}
}

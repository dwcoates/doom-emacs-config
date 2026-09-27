package contextcut

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestBounds(t *testing.T) {
	cases := []struct {
		name string
		cut  *conversationv1.ContextCut
		want bool
	}{
		{name: "a clear bounds", cut: &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}}}, want: true},
		{name: "a compaction bounds", cut: &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}}}, want: true},
		{name: "a failed compaction cut nothing", cut: &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_CompactionFailed{CompactionFailed: &conversationv1.ContextCompactionFailed{}}}, want: false},
		{name: "an unset arm bounds nothing", cut: &conversationv1.ContextCut{}, want: false},
		{name: "no cut bounds nothing", cut: nil, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert.
			if got := Bounds(tc.cut); got != tc.want {
				t.Fatalf("Bounds = %v, want %v", got, tc.want)
			}
		})
	}
}

// cutAt is a page entry carrying one cut.
func cutAt(cut *conversationv1.ContextCut) *conversationv1.HistoryEntryAt {
	return &conversationv1.HistoryEntryAt{Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{
		AgentFrame: &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Update{
			Update: &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: cut}},
		}},
	}}}
}

// promptAt is a page entry carrying a prompt.
func promptAt() *conversationv1.HistoryEntryAt {
	return &conversationv1.HistoryEntryAt{Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{
		UserPrompt: &conversationv1.AgentPrompt{},
	}}}
}

func TestNewestBoundOnPage(t *testing.T) {
	compacted := &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}}}
	failed := &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_CompactionFailed{CompactionFailed: &conversationv1.ContextCompactionFailed{}}}
	cases := []struct {
		name    string
		entries []*conversationv1.HistoryEntryAt
		want    int
	}{
		{name: "a page with no cut", entries: []*conversationv1.HistoryEntryAt{promptAt(), promptAt()}, want: -1},
		{name: "the newest of two cuts is the first met newest-first", entries: []*conversationv1.HistoryEntryAt{promptAt(), cutAt(compacted), promptAt(), cutAt(compacted)}, want: 1},
		{name: "a failed compaction is passed over", entries: []*conversationv1.HistoryEntryAt{cutAt(failed), promptAt(), cutAt(compacted)}, want: 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert.
			if got := NewestBoundOnPage(&conversationv1.HistoryPage{Entries: tc.entries}); got != tc.want {
				t.Fatalf("NewestBoundOnPage = %d, want %d", got, tc.want)
			}
		})
	}
}

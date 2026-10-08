package main

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/integration/harness"
)

// pushed files N prompts on the main agent's book.
func pushed(n int) *book {
	b := newBook()
	for range n {
		b.record(agentFrame{agent: MainAgentID, prompt: &conversationv1.AgentPrompt{}})
	}
	return b
}

func TestTheFakeStorePageSizeIsTheHarnesss(t *testing.T) {
	// Assert.
	if DefaultHistoryPageSize != harness.FeedPageSize {
		t.Fatalf("DefaultHistoryPageSize = %d, harness.FeedPageSize = %d; they must agree", DefaultHistoryPageSize, harness.FeedPageSize)
	}
}

func TestBookNewestPageIsTheNewestEntriesWithMore(t *testing.T) {
	// Arrange.
	b := pushed(5)

	// Act.
	page, ok := b.page("", nil, 2)

	// Assert.
	if !ok || len(page.GetEntries()) != 2 || page.GetEntries()[0].GetAt().GetValue() != pointerAt(MainAgentID, 5) {
		t.Fatalf("page = %v, want the two newest, newest first", page)
	}
	if got := page.GetMore().GetLastEntry().GetValue(); got != pointerAt(MainAgentID, 4) {
		t.Fatalf("more.last_entry = %q, want the page's oldest", got)
	}
}

func TestBookPageAfterAPointerReachesTheFloor(t *testing.T) {
	// Arrange.
	b := pushed(3)

	// Act.
	page, ok := b.page("", &conversationv1.HistoryPointer{Value: pointerAt(MainAgentID, 2)}, 5)

	// Assert.
	if !ok || len(page.GetEntries()) != 1 || page.GetFloor() == nil {
		t.Fatalf("page = %v, want the one older entry at the floor", page)
	}
}

func TestBookPageAfterAnUnknownPointerIsRefused(t *testing.T) {
	// Arrange.
	b := pushed(1)

	// Act.
	_, ok := b.page("", &conversationv1.HistoryPointer{Value: "nowhere"}, 5)

	// Assert.
	if ok {
		t.Fatal("a pointer the book never served was answered")
	}
}

func TestBookSinceIsTheCatchUpAfterAPointer(t *testing.T) {
	// Arrange.
	b := pushed(3)

	// Act.
	page := b.since("", &conversationv1.HistoryPointer{Value: pointerAt(MainAgentID, 1)})

	// Assert.
	if len(page.GetEntries()) != 2 {
		t.Fatalf("catch-up = %v, want the two entries after the pointer", page)
	}
}

func TestBookRetiredEntryIsNoLongerServed(t *testing.T) {
	// Arrange.
	b := pushed(2)

	// Act.
	b.record(agentFrame{agent: MainAgentID, retired: &conversationv1.HistoryEntryAt{
		At: &conversationv1.HistoryPointer{Value: pointerAt(MainAgentID, 2)},
	}})

	// Assert.
	if page, _ := b.page("", nil, 5); len(page.GetEntries()) != 1 {
		t.Fatalf("page = %v, want the retired entry gone", page)
	}
}

func TestBookSeedStampsEachEntrysTurn(t *testing.T) {
	// Arrange.
	b := newBook()
	entries := harness.EncodeHistory(t, &conversationv1.HistoryEntry{}, &conversationv1.HistoryEntry{})

	// Act: the newest entry names a turn, the oldest none.
	b.seed(entries, []string{"turn-1"})
	page, _ := b.page("", nil, 2)

	// Assert.
	got := []string{page.GetEntries()[0].GetTurn().GetValue(), page.GetEntries()[1].GetTurn().GetValue()}
	if got[0] != "turn-1" || got[1] != "" {
		t.Fatalf("turns = %q, want the newest stamped turn-1 and the oldest unstamped", got)
	}
}

// askFrame is an ask's frame on the main agent: a question batch when QUESTION,
// a consent ask otherwise, open when OPEN and settled otherwise.
func askFrame(id string, question, open bool) agentFrame {
	update := &conversationv1.AgentUpdate{}
	switch {
	case question && open:
		update.Update = &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: id}, Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{}}}}
	case question:
		update.Update = &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: id}, Result: &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{}}}}
	case open:
		update.Update = &conversationv1.AgentUpdate_Permission{Permission: &conversationv1.AgentPermission{
			Id: &conversationv1.AgentPermissionId{Value: id}, Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{}}}}
	default:
		update.Update = &conversationv1.AgentUpdate_Permission{Permission: &conversationv1.AgentPermission{
			Id: &conversationv1.AgentPermissionId{Value: id}, Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{}}}}
	}
	return agentFrame{agent: MainAgentID, frame: &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: MainAgentID},
		Result:  &conversationv1.AgentFrame_Update{Update: update},
	}}
}

func TestTheBookHoldsEveryAskOpenInTheOrderItOpened(t *testing.T) {
	tests := []struct {
		name   string
		frames []agentFrame
		want   []string
	}{
		{name: "two open asks", frames: []agentFrame{askFrame("p-1", false, true), askFrame("q-1", true, true)}, want: []string{"permission:p-1", "question:q-1"}},
		{name: "a settled permission", frames: []agentFrame{askFrame("p-1", false, true), askFrame("p-1", false, false)}, want: []string{}},
		{name: "a settled question", frames: []agentFrame{askFrame("q-1", true, true), askFrame("q-1", true, false)}, want: []string{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			b := newBook()
			for _, f := range tt.frames {
				b.record(f)
			}

			// Act.
			asks := b.openAsks()

			// Assert.
			got := []string{}
			for _, ask := range asks {
				got = append(got, askKey(ask))
			}
			if len(got) != len(tt.want) {
				t.Fatalf("open asks = %v, want %v", got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Fatalf("open asks = %v, want %v", got, tt.want)
				}
			}
		})
	}
}

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

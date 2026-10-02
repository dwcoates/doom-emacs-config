package sessionwatcher

import (
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// TestNoteHistoryLoadedHoldsTheNewestPointer covers which loaded page gives an
// agent the daemon held nothing of its catch-up pointer: only that agent's
// NEWEST page, and never one that would move a pointer already held.
func TestNoteHistoryLoadedHoldsTheNewestPointer(t *testing.T) {
	tests := []struct {
		name string
		// held is the main watch's pointer before the load, "" for none.
		held   string
		newest bool
		want   string
	}{
		{name: "a newest page with nothing held gives its newest entry", newest: true, want: "ptr-9"},
		{name: "an older page gives nothing", newest: false, want: ""},
		{name: "a newest page never moves a held pointer", held: "ptr-live", newest: true, want: "ptr-live"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			if tt.held != "" {
				h.routeNow(func(w *watcher) {
					w.routeAgentResponseLocked(w.main, entryFrameAt(frameUpdate("main-1", activityUpdate(readActivity("act-1"))), tt.held))
				})
			}
			page := &conversationv1.HistoryPage{Entries: []*conversationv1.HistoryEntryAt{
				frameEntryAt("ptr-9", frameUpdate("main-1", activityUpdate(readActivity("act-9")))),
				promptEntry("ptr-1", "turn-1", "main-1"),
			}}

			// Act.
			h.w.NoteHistoryLoaded(nil, page, tt.newest)

			// Assert.
			if got := h.w.MainKnownThrough().GetValue(); got != tt.want {
				t.Fatalf("MainKnownThrough() = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestNoteHistoryLoadedNamesTheMainAgent covers an adopted session whose main
// watch opened tail_only: the root's loaded page is what names the main agent
// for the views.
func TestNoteHistoryLoadedNamesTheMainAgent(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	page := &conversationv1.HistoryPage{Entries: []*conversationv1.HistoryEntryAt{
		promptEntry("ptr-1", "turn-1", "main-1"),
	}}

	// Act.
	h.w.NoteHistoryLoaded(nil, page, true)

	// Assert.
	if got := h.rec.mainNamings(); !slices.Contains(got, "feed:main-1") {
		t.Fatalf("main namings = %v, want the feed told main-1", got)
	}
}

// TestNoteHistoryLoadedTellsTheFooter covers the footer's reconciliation: a
// loaded page is the views' one statement that the conversation ran before.
func TestNoteHistoryLoadedTellsTheFooter(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	page := &conversationv1.HistoryPage{Entries: []*conversationv1.HistoryEntryAt{
		promptEntry("ptr-1", "turn-1", "main-1"),
	}}

	// Act.
	h.w.NoteHistoryLoaded(agentID("sub-1"), page, false)

	// Assert.
	got := h.rec.until(t, "footer.OnHistoryPage")
	if _, fed := find(got, "feed.OnHistoryPage"); fed {
		t.Fatal("a loaded page was handed to the feed by the watcher; the feed draws it itself")
	}
}

// TestAWatchOfAnAgentWhoseNewestPageWasLoadedCatchesUp covers the open after
// a reader's load: a re-open no longer opens tail_only past what was written
// between the load and the open.
func TestAWatchOfAnAgentWhoseNewestPageWasLoadedCatchesUp(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.w.NoteHistoryLoaded(nil, &conversationv1.HistoryPage{Entries: []*conversationv1.HistoryEntryAt{
		promptEntry("ptr-loaded", "turn-1", "main-1"),
	}}, true)

	// Act.
	req := h.relink(t)

	// Assert.
	if got := req.GetKnownThrough().GetValue(); got != "ptr-loaded" {
		t.Fatalf("re-opened with known_through %q, want ptr-loaded", got)
	}
}

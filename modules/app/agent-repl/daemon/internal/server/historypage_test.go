package server

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// The command handler's half of conversation-history.proto: it carries the
// READER to the reader, sends no fence at all, and refuses rather than serving
// nothing when no reader is wired behind it.

func TestFirstPageCarriesTheReaderToTheHistoryReader(t *testing.T) {
	// Arrange — the position the client cannot name is selected by this
	// identity, so it has to reach the authority that holds the position.
	pager := &recordingPager{historyPage: &frontendv1.ConversationHistoryPage{}}
	var logged []string
	h := newResyncHandler(t, pager, &logged)

	// Act.
	if _, err := h.FirstPage(context.Background(), "conn-9", "/w", "r1", &frontendv1.FirstPageCmd{}); err != nil {
		t.Fatalf("FirstPage: %v", err)
	}

	// Assert.
	if pager.gotReader != "conn-9" {
		t.Fatalf("history reader saw reader %q, want conn-9", pager.gotReader)
	}
}

func TestNextPageWithNoResyncerWiredIsRefused(t *testing.T) {
	// Arrange — the command exists, so something must answer it. A nil page
	// with a nil error would be indistinguishable from an empty conversation.
	var logged []string
	h := newResyncHandler(t, nil, &logged)

	// Act.
	_, err := h.NextPage(context.Background(), "conn-1", "/w", "r1", &frontendv1.NextPageCmd{})

	// Assert.
	if err == nil {
		t.Fatal("next_page with no history reader wired returned no error")
	}
}

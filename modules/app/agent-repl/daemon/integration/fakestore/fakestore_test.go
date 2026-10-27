package fakestore

import (
	"context"
	"path/filepath"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	storereadv1 "agentrepl/proto/storeread/v1"
)

func entry(at string, ms int64) *conversationv1.HistoryEntryAt {
	return &conversationv1.HistoryEntryAt{
		At:    &conversationv1.HistoryPointer{Value: at},
		Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{}}},
		Place: &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: &conversationv1.ConversationPlace{AtMs: ms}},
	}
}

func three() []*conversationv1.HistoryEntryAt {
	return []*conversationv1.HistoryEntryAt{entry("p-1", 10), entry("p-2", 20), entry("p-3", 30)}
}

func TestABookIsReadBackAsItWasWritten(t *testing.T) {
	// Arrange
	dir := t.TempDir()

	// Act
	if err := Write(dir, "main@ws-1", 2, three()); err != nil {
		t.Fatalf("Write: %v", err)
	}
	size, got, kept, err := Read(dir, "main@ws-1")

	// Assert
	if err != nil || !kept || size != 2 || len(got) != 3 || !proto.Equal(got[2], entry("p-3", 30)) {
		t.Fatalf("Read = %d, %v, %v, %v; want the three entries at page size 2", size, got, kept, err)
	}
}

func TestPage(t *testing.T) {
	tests := []struct {
		name      string
		after     *conversationv1.HistoryPointer
		wantFirst string
		wantN     int
		wantMore  string
		wantOK    bool
	}{
		{name: "the newest page", wantFirst: "p-3", wantN: 2, wantMore: "p-2", wantOK: true},
		{name: "the page below a pointer reaches the floor", after: &conversationv1.HistoryPointer{Value: "p-2"}, wantFirst: "p-1", wantN: 1, wantOK: true},
		{name: "a pointer naming no entry", after: &conversationv1.HistoryPointer{Value: "p-9"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			page, ok := Page(three(), tt.after, 2)

			// Assert
			if ok != tt.wantOK {
				t.Fatalf("ok = %v, want %v", ok, tt.wantOK)
			}
			if !ok {
				return
			}
			if len(page.GetEntries()) != tt.wantN || page.GetEntries()[0].GetAt().GetValue() != tt.wantFirst || page.GetMore().GetLastEntry().GetValue() != tt.wantMore {
				t.Fatalf("page = %v, want %d entries from %s, more %q", page, tt.wantN, tt.wantFirst, tt.wantMore)
			}
		})
	}
}

func TestPageThroughStopsAtTheInstant(t *testing.T) {
	// Act
	page := PageThrough(three(), 20, 5)

	// Assert
	if len(page.GetEntries()) != 2 || page.GetEntries()[0].GetAt().GetValue() != "p-2" {
		t.Fatalf("page = %v, want p-2 and p-1", page)
	}
}

func TestTheServerAnswersAsTheStoreDoes(t *testing.T) {
	stated := &storereadv1.ReadHistoryPageRequest_Newest{Newest: &storereadv1.ReadHistoryPageNewest{KnownAs: &storereadv1.ReadHistoryPageNewest_Stated{Stated: &storereadv1.ReadHistoryPageNewestStated{}}}}
	named := &storereadv1.ReadHistoryPageRequest_Newest{Newest: &storereadv1.ReadHistoryPageNewest{KnownAs: &storereadv1.ReadHistoryPageNewest_Named{Named: &storereadv1.ReadHistoryPageNewestNamed{}}}}
	tests := []struct {
		name    string
		book    string
		req     *storereadv1.ReadHistoryPageRequest
		wantArm string
		wantN   int
	}{
		{name: "a kept book's newest page", book: "main@ws-1", req: &storereadv1.ReadHistoryPageRequest{Position: named}, wantArm: "success", wantN: 3},
		{name: "a stated book nothing kept is the empty page", book: "main@ws-2", req: &storereadv1.ReadHistoryPageRequest{Position: stated}, wantArm: "success"},
		{name: "a named book nothing kept is unknown", book: "main@ws-2", req: &storereadv1.ReadHistoryPageRequest{Position: named}, wantArm: "unknown_agent"},
		{name: "a stale pointer", book: "main@ws-1", req: &storereadv1.ReadHistoryPageRequest{Position: &storereadv1.ReadHistoryPageRequest_After{After: &conversationv1.HistoryPointer{Value: "p-9"}}}, wantArm: "stale_pointer"},
		{name: "the book through an instant", book: "main@ws-1", req: &storereadv1.ReadHistoryPageRequest{Position: &storereadv1.ReadHistoryPageRequest_Through{Through: &conversationv1.ConversationThrough{AtMs: 20}}}, wantArm: "success", wantN: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := filepath.Join(t.TempDir(), "books")
			if err := Write(dir, "main@ws-1", 0, three()); err != nil {
				t.Fatalf("Write: %v", err)
			}
			s := &Server{dir: dir, defaultSize: 50}
			tt.req.Book = &conversationv1.AgentId{Value: tt.book}

			// Act
			resp, err := s.ReadHistoryPage(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("ReadHistoryPage: %v", err)
			}
			got := "success"
			switch {
			case resp.Msg.GetFailure().GetUnknownAgent() != nil:
				got = "unknown_agent"
			case resp.Msg.GetFailure().GetStalePointer() != nil:
				got = "stale_pointer"
			case resp.Msg.GetFailure() != nil:
				got = "other failure"
			}
			if got != tt.wantArm || len(resp.Msg.GetSuccess().GetPage().GetEntries()) != tt.wantN {
				t.Fatalf("answer = %v, want %s with %d entries", resp.Msg, tt.wantArm, tt.wantN)
			}
		})
	}
}

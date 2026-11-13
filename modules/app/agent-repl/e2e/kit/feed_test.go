package kit

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"context"
	"errors"
	"strings"
	"testing"

	"connectrpc.com/connect"
)

func TestCursorIngested(t *testing.T) {
	tests := []struct {
		name    string
		cursors []*storev1.CursorState
		want    bool
	}{
		{name: "a cursor at the file's size has ingested it",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 40}}, want: true},
		{name: "a cursor past the file's size has ingested it",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 41}}, want: true},
		{name: "a cursor short of the file's size has not",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 39}}},
		{name: "no cursor for the file has not",
			cursors: nil},
		{name: "another file's cursor at the size does not count",
			cursors: []*storev1.CursorState{{Path: "/p/main.jsonl", Offset: 40}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := CursorIngested(tt.cursors, "/p/agent-1.jsonl", 40)

			// Assert.
			if got != tt.want {
				t.Fatalf("CursorIngested = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestEndsTurn pins the one turn-ended match every feed wait and page scan in
// this suite goes through.
func TestEndsTurn(t *testing.T) {
	t.Parallel()
	turn := &conversationv1.TurnId{Value: "t1"}
	ended := &frontendv1.FeedRow_TurnEnded{TurnEnded: &frontendv1.FeedTurnEnded{}}
	cases := []struct {
		name string
		row  *frontendv1.FeedRow
		want bool
	}{
		{"the turn's own ended row", &frontendv1.FeedRow{Turn: turn, Row: ended}, true},
		{"another turn's ended row", &frontendv1.FeedRow{Turn: &conversationv1.TurnId{Value: "t2"}, Row: ended}, false},
		{"the turn's row that is not its end", &frontendv1.FeedRow{Turn: turn}, false},
		{"a row with no turn", &frontendv1.FeedRow{Row: ended}, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Act
			got := EndsTurn(turn)(tc.row)
			// Assert
			if got != tc.want {
				t.Fatalf("EndsTurn(t1)(%v) = %v, want %v", tc.row, got, tc.want)
			}
		})
	}
}

// TestHookRowsAnswersEveryHookCardInFeedOrder pins the one hook-card scan the
// hook tests' single-card and one-per-firing checks go through.
func TestHookRowsAnswersEveryHookCardInFeedOrder(t *testing.T) {
	t.Parallel()
	// Arrange
	hookCard := func(name string) *frontendv1.FeedRow {
		return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Hook{Hook: &frontendv1.FeedHook{Headline: &frontendv1.FeedHookHeadline{Text: name}}},
		}}}
	}
	rows := []*frontendv1.FeedRow{hookCard("first"), {Turn: &conversationv1.TurnId{Value: "t1"}}, hookCard("second")}

	// Act
	got := HookRows(rows)

	// Assert
	if len(got) != 2 || got[0].GetHeadline().GetText() != "first" || got[1].GetHeadline().GetText() != "second" {
		t.Fatalf("HookRows = %v, want the two hook cards in feed order", got)
	}
	if HookRow(rows) != got[0] {
		t.Fatalf("HookRow = %v, want the first of HookRows", HookRow(rows))
	}
}

// fakeBookStore answers OpenAgentSession with a scripted response or error;
// every other method is the embedded nil interface and panics if reached.
type fakeBookStore struct {
	storev1connect.ShimStoreClient
	resp *storev1.OpenAgentSessionResponse
	err  error
}

func (f fakeBookStore) OpenAgentSession(context.Context, *connect.Request[storev1.OpenAgentSessionRequest]) (*connect.Response[storev1.OpenAgentSessionResponse], error) {
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.resp), nil
}

func TestReadAgentBook(t *testing.T) {
	page := &storev1.AgentSessionPage{}
	tests := []struct {
		name     string
		store    fakeBookStore
		wantPage bool
		wantErr  string
	}{
		{
			name:     "a success answers its page",
			store:    fakeBookStore{resp: &storev1.OpenAgentSessionResponse{Result: &storev1.OpenAgentSessionResponse_Success{Success: &storev1.OpenAgentSessionSuccess{Page: page}}}},
			wantPage: true,
		},
		{
			name:    "a transport failure is an error naming the book",
			store:   fakeBookStore{err: errors.New("dial unix: no such file")},
			wantErr: "OpenAgentSession(agent-1): dial unix: no such file",
		},
		{
			name:    "a failure answer is an error",
			store:   fakeBookStore{resp: &storev1.OpenAgentSessionResponse{Result: &storev1.OpenAgentSessionResponse_Failure{Failure: &storev1.OpenAgentSessionFailure{}}}},
			wantErr: "want success",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := ReadAgentBook(context.Background(), tt.store, &conversationv1.AgentId{Value: "agent-1"})

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("ReadAgentBook error = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != page {
				t.Fatalf("ReadAgentBook = (%v, %v), want the success's page", got, err)
			}
		})
	}
}

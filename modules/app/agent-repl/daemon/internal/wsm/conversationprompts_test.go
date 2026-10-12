package wsm

import (
	"context"
	"slices"
	"testing"
	"time"
)

// putTurnAt records turn ID of WS, opened at epoch-ms AT.
func putTurnAt(t *testing.T, s *store, ws WorkspaceID, id TurnID, atMs int64) {
	t.Helper()
	if err := s.PutTurn(context.Background(), Turn{ID: id, Workspace: ws, Text: "say " + string(id), Origin: "user", StartedAt: time.UnixMilli(atMs)}); err != nil {
		t.Fatalf("PutTurn %s: %v", id, err)
	}
}

// promptTurns answers the turn ids of PROMPTS, in order.
func promptTurns(prompts []ConversationPrompt) []TurnID {
	out := make([]TurnID, 0, len(prompts))
	for _, p := range prompts {
		out = append(out, p.Turn)
	}
	return out
}

// forkedAt makes CHILD a fork of PARENT bounded at THROUGH, with UNINHERITED open.
func forkedAt(t *testing.T, s *store, parent, child WorkspaceID, through int64, uninherited ...TurnID) {
	t.Helper()
	ctx := context.Background()
	instant := time.Unix(1_700_000_000, 0)
	if err := s.PutSession(ctx, Session{Workspace: child, HostSessionID: "h-" + string(child), StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.PutPendingFork(ctx, PendingFork{Workspace: child, Parent: parent}); err != nil {
		t.Fatalf("PutPendingFork: %v", err)
	}
	if err := s.ResolveFork(ctx, "v-"+string(child), ForkLineage{Workspace: child, Parent: parent, ParentBook: "book", ThroughMs: through, Uninherited: uninherited}); err != nil {
		t.Fatalf("ResolveFork: %v", err)
	}
}

func TestRecentConversationPrompts(t *testing.T) {
	cases := []struct {
		name  string
		limit int
		want  []TurnID
	}{
		{name: "the newest own turns alone when they fill the limit", limit: 2, want: []TurnID{"c1", "c2"}},
		{name: "inherited turns within the bound fill the rest, oldest first", limit: 4, want: []TurnID{"p1", "p2", "c1", "c2"}},
		{name: "a parent turn after the bound or open at the fork is never inherited", limit: 10, want: []TurnID{"g1", "p1", "p2", "c1", "c2"}},
		{name: "a zero limit answers nothing", limit: 0, want: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: grandparent g -> parent p (forked at 150) -> child c (forked at 250).
			s, _ := testStore(t)
			g, p, c := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
			putTurnAt(t, s, g.ID, "g1", 100)
			putTurnAt(t, s, g.ID, "g2", 200) // after the parent's fork bound
			forkedAt(t, s, g.ID, p.ID, 150)
			putTurnAt(t, s, p.ID, "p1", 160)
			putTurnAt(t, s, p.ID, "p2", 170)
			putTurnAt(t, s, p.ID, "p3", 240) // open when the child forked
			putTurnAt(t, s, p.ID, "p4", 300) // after the child's fork bound
			forkedAt(t, s, p.ID, c.ID, 250, "p3")
			putTurnAt(t, s, c.ID, "c1", 400)
			putTurnAt(t, s, c.ID, "c2", 500)

			// Act
			got, err := s.RecentConversationPrompts(context.Background(), c.ID, tc.limit)

			// Assert
			if err != nil {
				t.Fatalf("RecentConversationPrompts: %v", err)
			}
			if !slices.Equal(promptTurns(got), tc.want) {
				t.Fatalf("turns = %v, want %v", promptTurns(got), tc.want)
			}
			for i, prompt := range got {
				if prompt.Ordinal != int64(i) {
					t.Fatalf("prompt %d has ordinal %d, want %d", i, prompt.Ordinal, i)
				}
			}
		})
	}
}

func TestRecentConversationPromptsNamesTheWorkspaceThatRecordedEachTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	p, c := testWorkspace(t, s), testWorkspace(t, s)
	putTurnAt(t, s, p.ID, "p1", 100)
	forkedAt(t, s, p.ID, c.ID, 150)
	putTurnAt(t, s, c.ID, "c1", 200)

	// Act
	got, err := s.RecentConversationPrompts(context.Background(), c.ID, 10)

	// Assert
	if err != nil || len(got) != 2 {
		t.Fatalf("RecentConversationPrompts = (%+v, %v), want two prompts", got, err)
	}
	if got[0].Workspace != p.ID || got[1].Workspace != c.ID || got[0].Text != "say p1" {
		t.Fatalf("prompts = %+v, want p1 recorded by the parent then c1 by the child", got)
	}
}

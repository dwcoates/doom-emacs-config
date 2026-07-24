package frontend

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func TestScopeMatchesSessionOrWorkspace(t *testing.T) {
	// Arrange
	sc := Scope{SessionID: "s1", Workspace: "/w"}
	// Act / Assert — a match on EITHER identity passes.
	cases := []struct {
		sid, ws string
		want    bool
	}{
		{"s1", "/other", true},   // session match
		{"other", "/w", true},    // workspace match
		{"other", "/x", false},   // neither
		{"", "", false},          // neither
	}
	for _, c := range cases {
		if got := sc.matches(c.sid, c.ws); got != c.want {
			t.Errorf("matches(%q,%q) = %v, want %v", c.sid, c.ws, got, c.want)
		}
	}
}

func TestScopeFrameDropsNonMatchingConversationDelta(t *testing.T) {
	// Arrange — a delta for a different session.
	sc := Scope{SessionID: "s1"}
	frame := ConversationDeltaFrame(&frontendv1.ConversationDelta{SessionId: "s2", Workspace: "/w2"})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert
	if keep {
		t.Fatal("a conversation delta for another session must be dropped")
	}
}

func TestScopeFramePassesGlobalDegradedNotice(t *testing.T) {
	// Arrange — a DegradedNotice carries no session/workspace identity.
	sc := Scope{SessionID: "s1"}
	frame := DegradedNoticeFrame(&frontendv1.DegradedNotice{Component: "shim-connection"})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert — connection-global frames always pass.
	if !keep {
		t.Fatal("a global degraded notice must pass a scoped connection")
	}
}

func TestScopeFrameFiltersSnapshotContents(t *testing.T) {
	// Arrange — a snapshot with two workspaces + two sessions, one matching.
	sc := Scope{SessionID: "s1", Workspace: "/w"}
	snap := &frontendv1.StateSnapshot{
		Workspaces: []*frontendv1.WorkspaceState{
			{Workspace: "/w", SessionId: "s1"},
			{Workspace: "/other", SessionId: "s2"},
		},
		Sessions: []*frontendv1.SessionView{
			{Workspace: "/w", SessionId: "s1"},
			{Workspace: "/other", SessionId: "s2"},
		},
	}
	// Act
	out, keep := scopeFrame(SnapshotFrame(snap), sc)
	// Assert — snapshot always kept, but filtered to the scoped workspace/session.
	if !keep {
		t.Fatal("snapshot must be kept (filtered)")
	}
	got := out.GetSnapshot()
	if len(got.GetWorkspaces()) != 1 || got.GetWorkspaces()[0].GetSessionId() != "s1" {
		t.Fatalf("workspaces = %v", got.GetWorkspaces())
	}
	if len(got.GetSessions()) != 1 || got.GetSessions()[0].GetSessionId() != "s1" {
		t.Fatalf("sessions = %v", got.GetSessions())
	}
}

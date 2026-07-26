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
		{"s1", "/other", true}, // session match
		{"other", "/w", true},  // workspace match
		{"other", "/x", false}, // neither
		{"", "", false},        // neither
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

func TestScopeFrameDropsNonMatchingHeartbeat(t *testing.T) {
	// Arrange — a heartbeat for a different session. Without its own scope
	// case a HeartbeatView would fall to the default and leak connection-wide.
	sc := Scope{SessionID: "s1"}
	frame := HeartbeatViewFrame(&frontendv1.HeartbeatView{SessionId: "s2", Workspace: "/w2"})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert
	if keep {
		t.Fatal("a heartbeat for another session must be dropped")
	}
}

func TestScopeFramePassesMatchingHeartbeat(t *testing.T) {
	// Arrange — a heartbeat for this connection's own session.
	sc := Scope{SessionID: "s1"}
	frame := HeartbeatViewFrame(&frontendv1.HeartbeatView{SessionId: "s1", Workspace: "/w1"})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert
	if !keep {
		t.Fatal("a heartbeat for this session must pass")
	}
}

func TestScopeFrameDropsNonMatchingQueue(t *testing.T) {
	// Arrange — another session's queue must not leak into this connection:
	// its entries are prompts the user typed somewhere else.
	sc := Scope{SessionID: "s1"}
	frame := QueueViewFrame(&frontendv1.QueueView{SessionId: "s2", Workspace: "/w2"})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert
	if keep {
		t.Fatal("a queue for another session must be dropped")
	}
}

func TestScopeFramePassesMatchingQueue(t *testing.T) {
	// Arrange
	sc := Scope{SessionID: "s1"}
	frame := QueueViewFrame(&frontendv1.QueueView{SessionId: "s1", Workspace: "/w1"})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert
	if !keep {
		t.Fatal("a queue for this session must pass")
	}
}

func TestFilterSnapshotKeepsOnlyThisScopesQueue(t *testing.T) {
	// Arrange — the connect snapshot carries every session's queue.
	sc := Scope{SessionID: "s1"}
	snap := &frontendv1.StateSnapshot{Queues: []*frontendv1.QueueView{
		{SessionId: "s1", Workspace: "/w1"},
		{SessionId: "s2", Workspace: "/w2"},
	}}
	// Act
	out := filterSnapshot(snap, sc)
	// Assert
	if len(out.GetQueues()) != 1 || out.GetQueues()[0].GetSessionId() != "s1" {
		t.Fatalf("filtered queues = %+v", out.GetQueues())
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

func TestScopeFrameDropsNonMatchingProgressView(t *testing.T) {
	// Arrange — a progress view for a different session. Without its own scope
	// case a ProgressView would fall to the default and leak connection-wide,
	// putting another workspace's footer on this connection.
	sc := Scope{SessionID: "s1"}
	frame := ProgressViewFrame(&frontendv1.ProgressView{SessionId: "s2", Workspace: "/w2"})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert
	if keep {
		t.Fatal("a progress view for another session must be dropped")
	}
}

func TestScopeFramePassesMatchingProgressView(t *testing.T) {
	// Arrange
	sc := Scope{SessionID: "s1"}
	frame := ProgressViewFrame(&frontendv1.ProgressView{SessionId: "s1", Workspace: "/w1"})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert
	if !keep {
		t.Fatal("a progress view for this connection's own session must pass")
	}
}

func TestFilterSnapshotKeepsOnlyTheScopedProgressView(t *testing.T) {
	// Arrange
	sc := Scope{Workspace: "/w1"}
	snap := &frontendv1.StateSnapshot{Progress: []*frontendv1.ProgressView{
		{Workspace: "/w1", SessionId: "s1"},
		{Workspace: "/w2", SessionId: "s2"},
	}}
	// Act
	got := filterSnapshot(snap, sc)
	// Assert
	if len(got.GetProgress()) != 1 || got.GetProgress()[0].GetWorkspace() != "/w1" {
		t.Fatalf("progress = %v, want only /w1", got.GetProgress())
	}
}

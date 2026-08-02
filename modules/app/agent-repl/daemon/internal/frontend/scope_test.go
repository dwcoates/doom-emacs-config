package frontend

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func TestScopeRequiresExactAgentSessionForControlState(t *testing.T) {
	// Arrange
	sc := Scope{SessionID: "s1", Workspace: "/w"}
	// Act / Assert — a nonempty frame session is authoritative. A shared
	// workspace cannot make a historical session match this connection.
	cases := []struct {
		sid, ws string
		want    bool
	}{
		{"s1", "/other", true}, // exact session match
		{"other", "/w", false}, // shared workspace cannot override session
		{"other", "/x", false}, // neither
		{"", "/w", true},       // sessionless state routes by workspace
		{"", "", false},        // neither
	}
	for _, c := range cases {
		if got := sc.matchesAgentSession(c.sid, c.ws); got != c.want {
			t.Errorf("matchesAgentSession(%q,%q) = %v, want %v", c.sid, c.ws, got, c.want)
		}
	}
}

func TestScopeFrameDropsHistoricalSameWorkspaceSessionView(t *testing.T) {
	// Arrange — terminal records remain in snapshots and legitimately share
	// the cwd with their successor. They must not rebind the successor's page.
	sc := Scope{SessionID: "s_current", Workspace: "/w"}
	frame := SessionViewFrame(&frontendv1.SessionView{SessionId: "s_retired", Workspace: "/w"})

	// Act.
	_, keep := scopeFrame(frame, sc)

	// Assert.
	if keep {
		t.Fatal("a historical same-workspace SessionView crossed a session-scoped connection")
	}
}

func TestScopeFrameDropsAgentSessionViewFromAnotherSameWorkspaceSession(t *testing.T) {
	// Arrange — workspace equality is deliberately irrelevant once a frame
	// names its owning session.
	sc := Scope{SessionID: "s_current", Workspace: "/w"}
	cases := map[string]*frontendv1.FrontendFrame{
		"workspace-state": WorkspaceStateFrame(&frontendv1.WorkspaceState{SessionId: "s_retired", Workspace: "/w"}),
		"typing-delta":    TypingDeltaFrame(&frontendv1.TypingDelta{SessionId: "s_retired", Workspace: "/w"}),
		"task-catalog":    TaskCatalogFrame(&frontendv1.TaskCatalog{SessionId: "s_retired", Workspace: "/w"}),
		"session-init":    SessionInitViewFrame(&frontendv1.SessionInitView{SessionId: "s_retired", Workspace: "/w"}),
		"heartbeat":       HeartbeatViewFrame(&frontendv1.HeartbeatView{SessionId: "s_retired", Workspace: "/w"}),
		"queue":           QueueViewFrame(&frontendv1.QueueView{SessionId: "s_retired", Workspace: "/w"}),
		"progress":        ProgressViewFrame(&frontendv1.ProgressView{SessionId: "s_retired", Workspace: "/w"}),
	}

	for name, frame := range cases {
		t.Run(name, func(t *testing.T) {
			if _, keep := scopeFrame(frame, sc); keep {
				t.Fatalf("%s crossed a connection scoped to another session", name)
			}
		})
	}
}

func TestScopeFrameRoutesRotatedConversationByWorkspace(t *testing.T) {
	// ConversationDelta.session_id is a vendor conversation id, not the stable
	// agent-repl session id in Scope. Vendor rotation must not sever the feed.
	sc := Scope{SessionID: "s_agent", Workspace: "/w"}
	frame := ConversationDeltaFrame(&frontendv1.ConversationDelta{
		SessionId: "550053ca-53a6-456b-97ff-0c73269ce253",
		Workspace: "/w",
	})

	if _, keep := scopeFrame(frame, sc); !keep {
		t.Fatal("a rotated vendor conversation delta for the scoped workspace was dropped")
	}
}

func TestScopeFramePassesSessionlessWorkspaceStateByWorkspace(t *testing.T) {
	// Arrange — merge-only workspace state can genuinely lack a session id.
	sc := Scope{SessionID: "s_current", Workspace: "/w"}
	frame := WorkspaceStateFrame(&frontendv1.WorkspaceState{Workspace: "/w"})

	// Act.
	_, keep := scopeFrame(frame, sc)

	// Assert.
	if !keep {
		t.Fatal("sessionless state for the scoped workspace was dropped")
	}
}

func TestScopeFrameDropsNonMatchingConversationDelta(t *testing.T) {
	// Arrange — a delta for a different workspace. Its vendor session id is not
	// comparable to the agent-repl session id in Scope.
	sc := Scope{SessionID: "s1", Workspace: "/w1"}
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

func TestScopeFramePassesGlobalCommandAck(t *testing.T) {
	// Arrange — a CommandAck carries no session/workspace identity.
	sc := Scope{SessionID: "s1"}
	frame := CommandAckFrame(&frontendv1.CommandAck{RequestId: "r1", Ok: true})
	// Act
	_, keep := scopeFrame(frame, sc)
	// Assert — connection-global frames always pass.
	if !keep {
		t.Fatal("a global command ack must pass a scoped connection")
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
			{Workspace: "/w", SessionId: "s_retired"},
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
	sc := Scope{SessionID: "s1", Workspace: "/w1"}
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

func TestFilterSnapshotKeepsOnlyTheScopedTaskCatalog(t *testing.T) {
	// Arrange.
	sc := Scope{SessionID: "s1", Workspace: "/w1"}
	snap := &frontendv1.StateSnapshot{Catalogs: []*frontendv1.TaskCatalog{
		{Workspace: "/w1", SessionId: "s1", Tasks: []*frontendv1.TaskEntry{{TaskId: "mine"}}},
		{Workspace: "/w2", SessionId: "s2", Tasks: []*frontendv1.TaskEntry{{TaskId: "theirs"}}},
	}}

	// Act.
	got := filterSnapshot(snap, sc)

	// Assert.
	if len(got.GetCatalogs()) != 1 || got.GetCatalogs()[0].GetTasks()[0].GetTaskId() != "mine" {
		t.Fatalf("catalogs = %v, want only s1's catalog", got.GetCatalogs())
	}
}

func TestSnapshotScopeSessionAuditNamesRetainedAndRejectedSessions(t *testing.T) {
	// Arrange — the exact shape from the recovered incident: several durable
	// records share one workspace but only one owns the scoped connection.
	scope := &Scope{SessionID: "s_current", Workspace: "/w"}
	snapshot := &frontendv1.StateSnapshot{Sessions: []*frontendv1.SessionView{
		{SessionId: "s_retired_a", Workspace: "/w"},
		{SessionId: "s_current", Workspace: "/w"},
		{SessionId: "s_retired_b", Workspace: "/w"},
	}}

	// Act.
	retained, rejected := snapshotScopeSessionAudit(snapshot, scope)

	// Assert.
	if retained != "s_current" || rejected != "s_retired_a,s_retired_b" {
		t.Fatalf("retained=%q rejected=%q", retained, rejected)
	}
}

func TestFilterSnapshotHandlesNilAndSessionlessRecoveryViews(t *testing.T) {
	// A reconnect can capture an empty snapshot while the state publisher is
	// between revisions.  Sessionless recovery state still belongs to the
	// workspace, while nil must produce a usable empty frame.
	sc := Scope{SessionID: "s_current", Workspace: "/w"}
	if got := filterSnapshot(nil, sc); got == nil || len(got.GetSessions()) != 0 {
		t.Fatalf("nil snapshot filtered to %+v, want a usable empty snapshot", got)
	}

	snapshot := &frontendv1.StateSnapshot{Sessions: []*frontendv1.SessionView{
		{Workspace: "/w"},
		{Workspace: "/other"},
	}}
	got := filterSnapshot(snapshot, sc)
	if len(got.GetSessions()) != 1 || got.GetSessions()[0].GetWorkspace() != "/w" {
		t.Fatalf("sessionless recovery views = %+v, want only /w", got.GetSessions())
	}
}

func TestFilterSnapshotPreservesDaemonIdentity(t *testing.T) {
	// Arrange — DaemonView is connection-global, not workspace-scoped.
	// Dropping it handed scoped webviews a snapshot with an empty boot id,
	// which their version-skew gate rejects on every adoption — aborting the
	// post-adoption conversation resync and leaving the pane empty.
	sc := Scope{SessionID: "s_current", Workspace: "/w"}
	snap := &frontendv1.StateSnapshot{
		Daemon:   &frontendv1.DaemonView{BootId: "boot-1"},
		Sessions: []*frontendv1.SessionView{{Workspace: "/w"}},
	}

	// Act
	got := filterSnapshot(snap, sc)

	// Assert
	if got.GetDaemon().GetBootId() != "boot-1" {
		t.Fatalf("filtered snapshot daemon = %+v, want boot id boot-1 preserved", got.GetDaemon())
	}
}

package frontend

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// Scope restricts a single frontend connection to the frames about ONE session
// (or its workspace). It is the mechanism behind the daemon's per-session
// GET /sessions/{id}/stream WebSocket: the same frontend.Server that fans
// unfiltered frames to /frontend serves a session-scoped view here, filtering
// per connection rather than re-deriving frames (design ruling: the webapp
// keeps its existing /stream URL and now parses frontend.v1 there).
//
// Durable/control views carry an agent-repl session id and therefore require
// an exact session match. ConversationDelta is the exception: its session_id
// is the vendor conversation id from the durable event, which can rotate while
// the agent-repl session and scoped WebSocket remain stable. It routes by the
// authoritative workspace instead.
type Scope struct {
	SessionID string
	Workspace string
}

func (s Scope) matchesAgentSession(sessionID, workspace string) bool {
	if sessionID != "" {
		return s.SessionID != "" && sessionID == s.SessionID
	}
	return s.Workspace != "" && workspace == s.Workspace
}

func (s Scope) matchesWorkspace(workspace string) bool {
	return s.Workspace != "" && workspace == s.Workspace
}

// scopeFrame decides whether frame reaches a client with this scope and returns
// the frame to actually send. A StateSnapshot is REPLACED with a scope-filtered
// copy so a scoped client's connect/resync snapshot carries only its own
// workspace and session. A frame that carries no session/workspace identity
// (CommandAck) is connection-global and always passes.
func scopeFrame(frame *frontendv1.FrontendFrame, sc Scope) (*frontendv1.FrontendFrame, bool) {
	switch f := frame.GetFrame().(type) {
	case *frontendv1.FrontendFrame_Snapshot:
		return SnapshotFrame(filterSnapshot(f.Snapshot, sc)), true
	case *frontendv1.FrontendFrame_WorkspaceState:
		return frame, sc.matchesAgentSession(f.WorkspaceState.GetSessionId(), f.WorkspaceState.GetWorkspace())
	case *frontendv1.FrontendFrame_SessionView:
		return frame, sc.matchesAgentSession(f.SessionView.GetSessionId(), f.SessionView.GetWorkspace())
	case *frontendv1.FrontendFrame_ConversationDelta:
		return frame, sc.matchesWorkspace(f.ConversationDelta.GetWorkspace())
	case *frontendv1.FrontendFrame_TypingDelta:
		return frame, sc.matchesAgentSession(f.TypingDelta.GetSessionId(), f.TypingDelta.GetWorkspace())
	case *frontendv1.FrontendFrame_TaskCatalog:
		return frame, sc.matchesAgentSession(f.TaskCatalog.GetSessionId(), f.TaskCatalog.GetWorkspace())
	case *frontendv1.FrontendFrame_SessionInit:
		return frame, sc.matchesAgentSession(f.SessionInit.GetSessionId(), f.SessionInit.GetWorkspace())
	case *frontendv1.FrontendFrame_Heartbeat:
		return frame, sc.matchesAgentSession(f.Heartbeat.GetSessionId(), f.Heartbeat.GetWorkspace())
	case *frontendv1.FrontendFrame_Queue:
		return frame, sc.matchesAgentSession(f.Queue.GetSessionId(), f.Queue.GetWorkspace())
	case *frontendv1.FrontendFrame_Progress:
		return frame, sc.matchesAgentSession(f.Progress.GetSessionId(), f.Progress.GetWorkspace())
	default:
		// CommandAck / unknown: connection-global, pass through.
		return frame, true
	}
}

type scopedView interface {
	GetSessionId() string
	GetWorkspace() string
}

// filterScopedViews retains each session-bearing view only for its exact
// session scope. A sessionless view may instead route by workspace. The protos
// themselves remain shared and read-only; only the slice is new.
func filterScopedViews[T scopedView](views []T, sc Scope) []T {
	var out []T
	for _, view := range views {
		if sc.matchesAgentSession(view.GetSessionId(), view.GetWorkspace()) {
			out = append(out, view)
		}
	}
	return out
}

// filterSnapshot returns a copy of snap carrying only the state-bearing views
// matching sc. The retained protos are shared (read-only downstream), only the
// slices are new.
func filterSnapshot(snap *frontendv1.StateSnapshot, sc Scope) *frontendv1.StateSnapshot {
	if snap == nil {
		return &frontendv1.StateSnapshot{}
	}
	return &frontendv1.StateSnapshot{
		Workspaces: filterScopedViews(snap.GetWorkspaces(), sc),
		Sessions:   filterScopedViews(snap.GetSessions(), sc),
		Catalogs:   filterScopedViews(snap.GetCatalogs(), sc),
		Inits:      filterScopedViews(snap.GetInits(), sc),
		Queues:     filterScopedViews(snap.GetQueues(), sc),
		Progress:   filterScopedViews(snap.GetProgress(), sc),
		// Daemon identity is connection-global, not workspace-scoped. Dropping
		// it here handed every scoped client a snapshot with an empty boot id,
		// which the webapp's version-skew gate rejects on EVERY adoption —
		// aborting its post-adoption resync and leaving the conversation pane
		// empty while freshness read "current".
		Daemon: snap.GetDaemon(),
		// Host-only data has no session routing key. Server strips it from every
		// non-host client after this scope pass; preserve it here so a future
		// host-scoped transport cannot accidentally erase durable work.
		WorkspaceAvailable: snap.GetWorkspaceAvailable(),
		HostActions:        snap.GetHostActions(),
	}
}

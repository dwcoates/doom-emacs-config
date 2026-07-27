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
// A match on EITHER the session id OR the workspace passes: a WorkspaceState
// resolved from a merge transition may carry the workspace but no session id,
// and it still belongs to this connection's view.
type Scope struct {
	SessionID string
	Workspace string
}

func (s Scope) matches(sessionID, workspace string) bool {
	if s.SessionID != "" && sessionID == s.SessionID {
		return true
	}
	if s.Workspace != "" && workspace == s.Workspace {
		return true
	}
	return false
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
		return frame, sc.matches(f.WorkspaceState.GetSessionId(), f.WorkspaceState.GetWorkspace())
	case *frontendv1.FrontendFrame_SessionView:
		return frame, sc.matches(f.SessionView.GetSessionId(), f.SessionView.GetWorkspace())
	case *frontendv1.FrontendFrame_ConversationDelta:
		return frame, sc.matches(f.ConversationDelta.GetSessionId(), f.ConversationDelta.GetWorkspace())
	case *frontendv1.FrontendFrame_TypingDelta:
		return frame, sc.matches(f.TypingDelta.GetSessionId(), f.TypingDelta.GetWorkspace())
	case *frontendv1.FrontendFrame_TaskCatalog:
		return frame, sc.matches(f.TaskCatalog.GetSessionId(), f.TaskCatalog.GetWorkspace())
	case *frontendv1.FrontendFrame_SessionInit:
		return frame, sc.matches(f.SessionInit.GetSessionId(), f.SessionInit.GetWorkspace())
	case *frontendv1.FrontendFrame_Heartbeat:
		return frame, sc.matches(f.Heartbeat.GetSessionId(), f.Heartbeat.GetWorkspace())
	case *frontendv1.FrontendFrame_Queue:
		return frame, sc.matches(f.Queue.GetSessionId(), f.Queue.GetWorkspace())
	case *frontendv1.FrontendFrame_Progress:
		return frame, sc.matches(f.Progress.GetSessionId(), f.Progress.GetWorkspace())
	default:
		// CommandAck / unknown: connection-global, pass through.
		return frame, true
	}
}

type scopedView interface {
	GetSessionId() string
	GetWorkspace() string
}

// filterScopedViews retains the views whose session or workspace belongs to
// sc. The protos themselves remain shared and read-only; only the slice is new.
func filterScopedViews[T scopedView](views []T, sc Scope) []T {
	var out []T
	for _, view := range views {
		if sc.matches(view.GetSessionId(), view.GetWorkspace()) {
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
		Inits:      filterScopedViews(snap.GetInits(), sc),
		Queues:     filterScopedViews(snap.GetQueues(), sc),
		Progress:   filterScopedViews(snap.GetProgress(), sc),
	}
}

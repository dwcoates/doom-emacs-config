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
// (DegradedNotice, CommandAck) is connection-global and always passes.
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
	default:
		// DegradedNotice / CommandAck / unknown: connection-global, pass through.
		return frame, true
	}
}

// filterSnapshot returns a copy of snap carrying only the workspaces and
// sessions matching sc. The retained protos are shared (read-only downstream),
// only the slices are new.
func filterSnapshot(snap *frontendv1.StateSnapshot, sc Scope) *frontendv1.StateSnapshot {
	if snap == nil {
		return &frontendv1.StateSnapshot{}
	}
	out := &frontendv1.StateSnapshot{}
	for _, w := range snap.GetWorkspaces() {
		if sc.matches(w.GetSessionId(), w.GetWorkspace()) {
			out.Workspaces = append(out.Workspaces, w)
		}
	}
	for _, v := range snap.GetSessions() {
		if sc.matches(v.GetSessionId(), v.GetWorkspace()) {
			out.Sessions = append(out.Sessions, v)
		}
	}
	return out
}

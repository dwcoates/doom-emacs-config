package frontend

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// Scope restricts a single frontend connection to the frames about ONE session
// or ONE workspace. It is the mechanism behind the daemon's scoped WebSockets:
// the same frontend.Server that fans unfiltered frames to /frontend serves a
// filtered view here, filtering per connection rather than re-deriving frames.
//
// A scope has one of two ADDRESSES, and which one it holds decides how every
// session-bearing frame routes:
//
//   - SESSION-ADDRESSED (SessionID set, GET /sessions/{id}/stream). Durable and
//     control views carry an agent-repl session id and require an exact match on
//     it, so a retired session sharing the workspace cannot rebind the page.
//     ConversationDelta is the exception: its session_id is the vendor
//     conversation id from the durable event, which rotates while the agent-repl
//     session and the scoped WebSocket both stand, so it routes by workspace.
//
//   - WORKSPACE-ADDRESSED (SessionID empty, Workspace set, GET
//     /workspace-stream?workspace=<dir>). The connection holds NO session
//     identity, so there is no id to match a frame's against and EVERY frame
//     routes by its workspace. This is what lets a viewer outlive the sessions
//     its workspace runs: which session the workspace owns is the daemon's
//     ruling, re-read from each arriving frame, not a key the viewer pinned at
//     connect and must be remounted to change.
type Scope struct {
	SessionID string
	Workspace string
}

// matchesAgentSession decides whether a frame bearing (sessionID, workspace)
// belongs to this scope. sessionID is the agent-repl session id; an empty one
// means the frame names no session (merge-only workspace state, say) and can
// only route by workspace.
func (s Scope) matchesAgentSession(sessionID, workspace string) bool {
	if s.SessionID == "" {
		// Workspace-addressed: the frame's session id is not a key this
		// connection holds a counterpart for, so the workspace decides alone.
		return s.matchesWorkspace(workspace)
	}
	if sessionID != "" {
		return sessionID == s.SessionID
	}
	return s.matchesWorkspace(workspace)
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
	// The FENCED pushes below carry no session id at all: the contract replaced
	// it with an opaque staleness token a renderer compares byte-wise and never
	// parses. There is therefore no id to match a scope's against, and every one
	// of them routes by workspace exactly as ConversationDelta already did.
	case *frontendv1.FrontendFrame_TypingDelta:
		return frame, sc.matchesWorkspace(f.TypingDelta.GetWorkspace())
	case *frontendv1.FrontendFrame_AsyncBubbleDelta:
		return frame, sc.matchesWorkspace(f.AsyncBubbleDelta.GetWorkspace())
	case *frontendv1.FrontendFrame_TaskCatalog:
		return frame, sc.matchesWorkspace(f.TaskCatalog.GetWorkspace())
	case *frontendv1.FrontendFrame_SessionInit:
		return frame, sc.matchesWorkspace(f.SessionInit.GetWorkspace())
	case *frontendv1.FrontendFrame_Heartbeat:
		return frame, sc.matchesWorkspace(f.Heartbeat.GetWorkspace())
	case *frontendv1.FrontendFrame_Queue:
		return frame, sc.matchesWorkspace(f.Queue.GetWorkspace())
	case *frontendv1.FrontendFrame_Progress:
		return frame, sc.matchesWorkspace(f.Progress.GetWorkspace())
	case *frontendv1.FrontendFrame_Topbar:
		return frame, sc.matchesWorkspace(f.Topbar.GetWorkspace())
	case *frontendv1.FrontendFrame_TokenBreakdown:
		return frame, sc.matchesWorkspace(f.TokenBreakdown.GetWorkspace())
	case *frontendv1.FrontendFrame_WorkspaceGate:
		return frame, sc.matchesWorkspace(f.WorkspaceGate.GetWorkspace())
	case *frontendv1.FrontendFrame_ShutdownSchedule:
		// DAEMON-GLOBAL, listed explicitly rather than left to the default arm.
		// There is exactly one drain lease for the whole daemon, it carries no
		// session or workspace key to filter on, and it blocks EVERY session —
		// so a session-scoped webview is entitled to the same view the host
		// gets, and filtering it out would leave that client's prompts parked
		// with nothing on screen explaining why.
		return frame, true
	case *frontendv1.FrontendFrame_RestartPending:
		// DAEMON-GLOBAL, listed explicitly rather than left to the default arm.
		// The whole daemon is going down, which takes every session with it, so
		// a session-scoped webview is exactly as entitled to the notice as the
		// host: it is the client that would otherwise paint the severed banner.
		return frame, true
	case *frontendv1.FrontendFrame_WorkspaceRoster:
		// EDITOR-GLOBAL, and listed explicitly rather than left to the default
		// arm. The roster is the whole sidebar — every workspace, not just this
		// connection's — and it carries no session or workspace routing key to
		// filter on. A session-scoped webview renders the same sidebar the
		// Emacs host does, so it passes to everyone.
		return frame, true
	default:
		// CommandAck / unknown: connection-global, pass through.
		return frame, true
	}
}

type scopedView interface {
	GetSessionId() string
	GetWorkspace() string
}

// workspaceView is a FENCED view: it carries a workspace and no session
// identity, so its workspace is the only routing key there is.
type workspaceView interface {
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

// filterWorkspaceViews retains each fenced view for its workspace alone.
func filterWorkspaceViews[T workspaceView](views []T, sc Scope) []T {
	var out []T
	for _, view := range views {
		if sc.matchesWorkspace(view.GetWorkspace()) {
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
		Catalogs:   filterWorkspaceViews(snap.GetCatalogs(), sc),
		Inits:      filterWorkspaceViews(snap.GetInits(), sc),
		Queues:     filterWorkspaceViews(snap.GetQueues(), sc),
		Progress:   filterWorkspaceViews(snap.GetProgress(), sc),
		// The three RESOLVED-VIEW families are fenced and workspace-keyed like
		// the four above them, so they filter by workspace and nothing else.
		Topbars:         filterWorkspaceViews(snap.GetTopbars(), sc),
		TokenBreakdowns: filterWorkspaceViews(snap.GetTokenBreakdowns(), sc),
		WorkspaceGates:  filterWorkspaceViews(snap.GetWorkspaceGates(), sc),
		// Async bubbles scope by workspace exactly as every other per-workspace
		// family does. AsyncBubble.workspace is the same key its delta carries
		// on the envelope, so a client's reconnect restatement and the pushes
		// it then receives are filtered by one rule rather than two.
		AsyncBubbles: filterWorkspaceViews(snap.GetAsyncBubbles(), sc),
		// Daemon identity is connection-global, not workspace-scoped. Dropping
		// it here handed every scoped client a snapshot with an empty boot id,
		// which the webapp's version-skew gate rejects on EVERY adoption —
		// aborting its post-adoption resync and leaving the conversation pane
		// empty while freshness read "current".
		Daemon: snap.GetDaemon(),
		// The drain lease is daemon-global for the same reason the roster is,
		// and a scoped client that lost it would see its own prompts parked
		// with no schedule to explain them.
		ShutdownSchedule: snap.GetShutdownSchedule(),
		// Host-only data has no session routing key. Server strips it from every
		// non-host client after this scope pass; preserve it here so a future
		// host-scoped transport cannot accidentally erase durable work.
		WorkspaceAvailable: snap.GetWorkspaceAvailable(),
		HostActions:        snap.GetHostActions(),
	}
}

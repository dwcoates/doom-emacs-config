// frames.go — the FrontendFrame oneof arms, one wrapper per frame kind.
//
// PACKAGING, NOT TRANSLATION, and that distinction is why this file exists apart
// from the curation layer it used to sit beside. A wrapper here takes a view the
// daemon has ALREADY resolved and puts it in the arm that names it: it reads no
// field, decides nothing, and cannot lose information. The layer that re-typed
// one neutral shape into another was translate.go, and it goes — the producers
// convert at the edge now, so there is no vendor shape left for a daemon layer
// to neutralize, and decoding a neutral shape only to re-encode it is ceremony.
//
// They are wrappers rather than inline composite literals so that no call site
// ever spells a generated oneof wrapper type. A frame arm added without one here
// is a frame arm every caller has to know the generated name of.

package frontend

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// SnapshotFrame wraps a StateSnapshot.
func SnapshotFrame(s *frontendv1.StateSnapshot) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Snapshot{Snapshot: s}}
}

// WorkspaceStateFrame wraps a WorkspaceState.
func WorkspaceStateFrame(w *frontendv1.WorkspaceState) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_WorkspaceState{WorkspaceState: w}}
}

// SessionViewFrame wraps a SessionView.
func SessionViewFrame(v *frontendv1.SessionView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_SessionView{SessionView: v}}
}

// ConversationDeltaFrame wraps a ConversationDelta.
func ConversationDeltaFrame(c *frontendv1.ConversationDelta) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_ConversationDelta{ConversationDelta: c}}
}

// ConversationPageFrame wraps a ConversationPage.
//
// A page is addressed to the ONE connection that asked for it — it echoes that
// request's id, and no other client is waiting on it — so it is enqueued to
// the requesting client as a command response rather than broadcast. That is
// the difference from a ConversationDelta, which is unsolicited state every
// client of the workspace needs.
func ConversationPageFrame(p *frontendv1.ConversationPage) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_ConversationPage{ConversationPage: p}}
}

// ConversationHistoryPageFrame wraps a ConversationHistoryPage.
//
// Like the older page frame, it is addressed to the ONE connection that asked:
// it echoes that request's id, and no other client is waiting on it. The echo
// is also what lets a client DISCARD a page it is no longer awaiting, which is
// how a page in flight across a generation change is handled — there is no
// fence on this surface, by design.
func ConversationHistoryPageFrame(p *frontendv1.ConversationHistoryPage) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_ConversationHistoryPage{ConversationHistoryPage: p}}
}

// TypingDeltaFrame wraps a TypingDelta.
func TypingDeltaFrame(t *frontendv1.TypingDelta) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_TypingDelta{TypingDelta: t}}
}

// TypingCutFrame wraps a TypingCut — the daemon's statement that a preview it
// opened will NEVER be completed, because the authoritative record that would
// have retired it can no longer arrive.
func TypingCutFrame(c *frontendv1.TypingCut) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_TypingCut{TypingCut: c}}
}

// TaskCatalogFrame wraps a TaskCatalog.
func TaskCatalogFrame(c *frontendv1.TaskCatalog) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_TaskCatalog{TaskCatalog: c}}
}

// CommandAckFrame wraps a CommandAck.
func CommandAckFrame(a *frontendv1.CommandAck) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_CommandAck{CommandAck: a}}
}

// SessionInitViewFrame wraps a SessionInitView (S9): the session's retained
// SystemInit (slash commands, tools, skills, model list).
func SessionInitViewFrame(v *frontendv1.SessionInitView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_SessionInit{SessionInit: v}}
}

// HeartbeatViewFrame wraps a HeartbeatView (E4): the ephemeral long-tool
// liveness relay.
func HeartbeatViewFrame(h *frontendv1.HeartbeatView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Heartbeat{Heartbeat: h}}
}

// QueueViewFrame wraps a QueueView (E4): the session's held-prompt queue.
func QueueViewFrame(q *frontendv1.QueueView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Queue{Queue: q}}
}

// ProgressViewFrame wraps a ProgressView (F1): the consolidated progress
// footer's whole input, resolved by internal/progress.
func ProgressViewFrame(p *frontendv1.ProgressView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Progress{Progress: p}}
}

// TopbarViewFrame wraps a TopbarView: one workspace's fully resolved topbar,
// pushed whenever any fact the topbar renders changes.
func TopbarViewFrame(v *frontendv1.TopbarView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Topbar{Topbar: v}}
}

// TokenBreakdownViewFrame wraps a TokenBreakdownView: the counter menu's
// resolved section/row tree.
func TokenBreakdownViewFrame(v *frontendv1.TokenBreakdownView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_TokenBreakdown{TokenBreakdown: v}}
}

// WorkspaceGateViewFrame wraps a WorkspaceGateView: whether prompts may be
// sent to the workspace, and the account behind a closed gate.
func WorkspaceGateViewFrame(v *frontendv1.WorkspaceGateView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_WorkspaceGate{WorkspaceGate: v}}
}

// WorkspaceAvailableFrame wraps the durable, host-only workspace lifecycle
// notification.  Server routes it only to ClientKindHost connections.
func WorkspaceAvailableFrame(v *frontendv1.WorkspaceAvailable) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_WorkspaceAvailable{WorkspaceAvailable: v}}
}

// HostActionFrame wraps one durable UI-only action from the daemon inbox.
// Server routes it only to ClientKindHost connections.
func HostActionFrame(v *frontendv1.HostAction) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_HostAction{HostAction: v}}
}

// DaemonHealthFrame wraps a daemon-global correlated health assertion.
func DaemonHealthFrame(v *frontendv1.DaemonHealthView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_DaemonHealth{DaemonHealth: v}}
}

// SessionHealthFrame wraps a session-specific correlated health assertion.
func SessionHealthFrame(v *frontendv1.SessionHealthView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_SessionHealth{SessionHealth: v}}
}

// ShutdownScheduleFrame wraps the daemon-global scheduled-shutdown drain
// lease. Like the roster it is DAEMON-GLOBAL and carries no routing key: there
// is exactly one lease for the whole daemon, and it blocks every session, so
// every client is entitled to see it.
func ShutdownScheduleFrame(v *frontendv1.ShutdownScheduleView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_ShutdownSchedule{ShutdownSchedule: v}}
}

// WorkspaceRosterFrame wraps the editor-global workspace roster.
func WorkspaceRosterFrame(r *frontendv1.WorkspaceRoster) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_WorkspaceRoster{WorkspaceRoster: r}}
}

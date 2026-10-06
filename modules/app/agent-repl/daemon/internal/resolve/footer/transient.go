package footer

import (
	"strings"

	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE TRANSIENT TIER (owner rulings, 2026-09-28 and 2026-09-30; footer.proto
// "The transient tier"; agent-repl AGENTS.md "Footer activity lines are
// salient, transient, or enduring").
//
// A transient is an EVENT: something the session just did or just learned. It
// ends when a newer transient replaces it or its expiry passes, and nothing
// else ends it. The resolver therefore holds exactly ONE slot per workspace,
// `wsState.transient`, and every source below fills it through raiseTransient,
// which stamps the event instant and the expiry (`at + window`). THE NEWEST
// WINS, whatever its kind.
//
// THE DAEMON RUNS NO EXPIRY TIMER. The expiry is shipped and the client's clock
// retires the line; when a transient lapses the daemon pushes nothing. A view
// the daemon composes AFTER the expiry simply omits the lapsed transient
// (liveTransient), which the contract allows and which keeps a later push from
// re-shipping a line the client has already retired.

// raiseTransient fills the workspace's one transient slot with t, replacing
// whatever stood there. It stamps the event instant and the expiry, and the
// subagent label when a subagent's work raised it (agent empty for the main
// agent). The caller builds only the kind.
func (r *resolver) raiseTransient(ws ids.WorkspaceID, s *wsState, agent string, t *frontendv1.FooterActivityTransient) {
	now := r.opts.clock.Now()
	t.At = stamp(now)
	t.Expiry = &frontendv1.FooterActivityTransientExpiry{ExpiresAtMs: epochMs(now.Add(r.opts.transientWindow))}
	if agent != "" {
		t.Agent = &frontendv1.FooterActivityTransientAgent{Label: agent}
	}
	s.transient = t
	r.logOf(ws, s).Debug("daemon.footer.transient_raised", "the footer raised a transient activity line",
		dlog.Context{"kind": transientKind(t), "agent": agent, "expires_at_ms": t.GetExpiry().GetExpiresAtMs()})
}

// liveTransient is the transient a view composed NOW carries: the slot's, or
// nil when none was ever raised or its expiry has already passed.
func (r *resolver) liveTransient(s *wsState) *frontendv1.FooterActivityTransient {
	if s.transient == nil {
		return nil
	}
	if epochMs(r.opts.clock.Now()) >= s.transient.GetExpiry().GetExpiresAtMs() {
		return nil
	}
	return s.transient
}

// unpinned is the activity cell when no salient line stands, under every
// status: the live transient, if any, over the always-set enduring line.
func (r *resolver) unpinned(s *wsState) *frontendv1.FooterActivityTransientOverEnduring {
	return &frontendv1.FooterActivityTransientOverEnduring{
		Transient: r.liveTransient(s),
		Enduring:  r.enduring(s),
	}
}

// transientKind names a transient's kind arm for the record, "unset" for one
// with none.
func transientKind(t *frontendv1.FooterActivityTransient) string {
	m := t.ProtoReflect()
	field := m.WhichOneof(m.Descriptor().Oneofs().ByName("kind"))
	if field == nil {
		return "unset"
	}
	return string(field.Name())
}

// agentLabel is the label a transient carries for work the agent `id` raised:
// the subagent's label as the agents panel names it when a subagent row claims
// the agent, and empty for the main agent (and for any agent no row claims).
func (s *wsState) agentLabel(id string) string {
	row := s.subagentRow(id)
	if row == nil {
		return ""
	}
	return rowAgentLabel(row)
}

// rowAgentLabel is a subagent row's label for a transient: its description,
// which is how the agents panel names the commission, or its type when the
// spawn carried none.
func rowAgentLabel(row *agentRow) string {
	if row.description != "" {
		return truncate(row.description, 48)
	}
	return row.label
}

// ---- the kinds each source raises ----------------------------------------

// stageName names a submission stage for the record, and reports false for a
// stage the footer does not declare.
func stageName(stage SubmissionStage) (string, bool) {
	switch stage {
	case StageHeld:
		return "held", true
	case StageClassifying:
		return "classifying", true
	case StageInterjecting:
		return "interjecting", true
	case StageCoalesced:
		return "coalesced", true
	case StageAfterToolCall:
		return "after_tool_call", true
	default:
		return "unknown", false
	}
}

// raiseHook raises the `hook` line for a hook that started.
func (r *resolver) raiseHook(ws ids.WorkspaceID, s *wsState, agent, name string) {
	r.raiseTransient(ws, s, agent, &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_Hook{Hook: &frontendv1.FooterActivityTransientHook{Name: name}},
	})
}

// raiseContextInjected raises the `context_injected` line for the item taken on.
func (r *resolver) raiseContextInjected(ws ids.WorkspaceID, s *wsState, agent, text string) {
	r.raiseTransient(ws, s, agent, &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_ContextInjected{
			ContextInjected: &frontendv1.FooterActivityTransientContextInjected{Text: text}},
	})
}

// raiseFault announces a NON-ESCALATING fault: the session is serving, so the
// fault is an event, never a line pinned over the session's live feedback.
func (r *resolver) raiseFault(ws ids.WorkspaceID, s *wsState, fault Fault) {
	r.raiseTransient(ws, s, "", &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_Fault{
			Fault: &frontendv1.FooterStatusActivityFault{Kind: fault.Kind, Detail: fault.Detail}},
	})
}

// raiseUpdated announces a finished deploy on this workspace, with what it left
// for later here.
func (r *resolver) raiseUpdated(ws ids.WorkspaceID, s *wsState, notes []*frontendv1.FooterStatusActivityUpdateNote) {
	r.raiseTransient(ws, s, "", &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_Updated{Updated: &frontendv1.FooterActivityTransientUpdated{Notes: notes}},
	})
}

// raiseSessionChange announces a changed session setting, composed.
func (r *resolver) raiseSessionChange(ws ids.WorkspaceID, s *wsState, text string) {
	r.raiseTransient(ws, s, "", &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_SessionChange{
			SessionChange: &frontendv1.FooterActivityTransientSessionChange{Text: text}},
	})
}

// raiseTask announces a task-tracker move: the moved task's subject and the
// tracker's counts after the move.
func (r *resolver) raiseTask(ws ids.WorkspaceID, s *wsState, agent, subject string) {
	done, total := s.taskCounts()
	r.raiseTransient(ws, s, agent, &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_Task{Task: &frontendv1.FooterActivityTransientTask{
			Subject: subject, Completed: done, Total: total}},
	})
}

// taskCounts is the tracker's completed and total task counts, the same
// figures the ☑ chip draws.
func (s *wsState) taskCounts() (done, total uint32) {
	for _, row := range s.tasks {
		if row.status == taskCompleted {
			done++
		}
	}
	return done, uint32(len(s.tasks))
}

// ---- session changes -------------------------------------------------------

// sessionChangeText composes the session_change line for a SessionUpdate arm
// that changes a session setting, and reports false for every other arm.
func sessionChangeText(update *conversationv1.SessionUpdate) (string, bool) {
	switch u := update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_ModelChanged:
		if name := u.ModelChanged.GetEffectiveModel().GetName(); name != "" {
			return "model → " + name, true
		}
		return "model changed", true
	case *conversationv1.SessionUpdate_PermissionModeChanged:
		return "permission mode → " + permissionModeName(u.PermissionModeChanged.GetPermissionMode()), true
	case *conversationv1.SessionUpdate_McpServer:
		return mcpServerText(u.McpServer), true
	default:
		return "", false
	}
}

// permissionModeName is the mode's arm name, rendered lowercase with spaces as
// every status cell renders an arm name. An unset mode reads "unstated".
func permissionModeName(mode *conversationv1.AgentPermissionMode) string {
	m := mode.ProtoReflect()
	field := m.WhichOneof(m.Descriptor().Oneofs().ByName("mode"))
	if field == nil {
		return "unstated"
	}
	return armWords(field.Name())
}

// mcpServerText composes an MCP server's health change.
func mcpServerText(server *conversationv1.SessionMcpServer) string {
	lead := "mcp " + server.GetName()
	switch health := server.GetHealth().(type) {
	case *conversationv1.SessionMcpServer_Connected:
		return lead + " connected"
	case *conversationv1.SessionMcpServer_Failed:
		if why := health.Failed.GetError(); why != "" {
			return truncate(lead+" failed — "+firstLine(why), DefaultWarningRowWidth)
		}
		return lead + " failed"
	case *conversationv1.SessionMcpServer_NeedsAuth:
		return lead + " needs auth"
	case *conversationv1.SessionMcpServer_Pending:
		return lead + " connecting"
	case *conversationv1.SessionMcpServer_Disabled:
		return lead + " disabled"
	default:
		return lead + " changed"
	}
}

// armWords renders an arm name lowercase with spaces, never underscores.
func armWords(name protoreflect.Name) string {
	return strings.ReplaceAll(string(name), "_", " ")
}

// firstLine is the first non-blank line of text, trimmed.
func firstLine(text string) string {
	for _, line := range strings.Split(text, "\n") {
		if trimmed := strings.TrimSpace(line); trimmed != "" {
			return trimmed
		}
	}
	return ""
}

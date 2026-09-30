package footer

import (
	"time"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE STATUS-INDEPENDENT SALIENT LINES (owner ruling, 2026-09-30; agent-repl
// AGENTS.md "Footer activity lines are salient, transient, quiet, or
// enduring"). Every status arm's salient oneof carries them under the same
// field names, ranked below the arm's own kinds and a fault:
//
//	update          a deploy's progress, until the deploy is done here
//	                (update.go);
//	rate_limit      a vendor rate-limit event that warned or refused, until a
//	                later event reports that allowance allowed;
//	notification    the agent's push notification, until the next prompt;
//	context_budget  the vendor's context-budget warning or a failed
//	                compaction, until a cut shrinks the context.
//
// Each ends on its own condition and never on a timer: a timer may end only a
// transient.

// budgetState is the standing context-budget line.
type budgetState struct {
	standing
	// agent is the agent whose context the warning is about: empty for a
	// failed compaction, which is always the main agent's.
	agent string
}

// rateEventState is the standing vendor rate-limit event.
type rateEventState struct {
	// line is the event as the strip draws it.
	line *frontendv1.FooterStatusActivityRateLimit
	// window names the allowance, "" when the vendor named none, so a later
	// event about the same allowance can end it.
	window string
	// at is when the event arrived.
	at time.Time
}

// standNotification stands the agent's push notification.
func (r *resolver) standNotification(ws ids.WorkspaceID, s *wsState, text string) {
	s.notification = &standing{text: truncate(text, DefaultWarningRowWidth), at: r.opts.clock.Now()}
	r.logOf(ws, s).Debug("daemon.footer.notification_stood", "the footer stood the agent's push notification",
		dlog.Context{"text": s.notification.text})
}

// endNotification ends the standing push notification at the next prompt.
func (r *resolver) endNotification(ws ids.WorkspaceID, s *wsState, cause string) {
	if s.notification == nil {
		return
	}
	r.logOf(ws, s).Debug("daemon.footer.notification_ended", "the next prompt ended the agent's push notification",
		dlog.Context{"text": s.notification.text, "cause": cause})
	s.notification = nil
}

// standContextBudget stands the context-budget line for AGENT's context (empty
// for the main agent's).
func (r *resolver) standContextBudget(ws ids.WorkspaceID, s *wsState, agent, text string) {
	s.contextBudget = &budgetState{standing: standing{text: text, at: r.opts.clock.Now()}, agent: agent}
	r.logOf(ws, s).Debug("daemon.footer.context_budget_stood", "the footer stood the context-budget line",
		dlog.Context{"text": text, "agent_id": agent})
}

// endContextBudget ends the standing context-budget line: a cut shrank the
// context, the session switched, or the subagent whose context it was ended.
func (r *resolver) endContextBudget(ws ids.WorkspaceID, s *wsState, cause string) {
	if s.contextBudget == nil {
		return
	}
	r.logOf(ws, s).Debug("daemon.footer.context_budget_ended", "the context-budget line ended with its condition",
		dlog.Context{"text": s.contextBudget.text, "agent_id": s.contextBudget.agent, "cause": cause})
	s.contextBudget = nil
}

// endSubagentBudget ends a context-budget line about AGENT's context when that
// agent's run ends: the context it warned about is gone.
func (r *resolver) endSubagentBudget(ws ids.WorkspaceID, s *wsState, agent string) {
	if s.contextBudget == nil || s.contextBudget.agent == "" || s.contextBudget.agent != agent {
		return
	}
	r.endContextBudget(ws, s, "daemon.footer.on_agent_terminal")
}

// observeRateLimitEvent stands or ends the rate-limit event line: a warning or
// a refusal stands it, and an event reporting the same allowance allowed ends
// it. A statusless event changes nothing.
func (r *resolver) observeRateLimitEvent(ws ids.WorkspaceID, s *wsState, status *conversationv1.SessionRateLimitStatus) {
	window, name := rateLimitWindow(status.GetRateLimitType())
	line := &frontendv1.FooterStatusActivityRateLimit{Window: window}
	switch status.GetStatus().(type) {
	case *conversationv1.SessionRateLimitStatus_Allowed:
		if s.rateEvent == nil || s.rateEvent.window != name {
			return
		}
		r.logOf(ws, s).Debug("daemon.footer.rate_limit_ended", "an event reported the allowance allowed; the rate-limit line ended",
			dlog.Context{"window": name})
		s.rateEvent = nil
		return
	case *conversationv1.SessionRateLimitStatus_AllowedWarning:
		line.Verdict = &frontendv1.FooterStatusActivityRateLimit_AllowedWarning{AllowedWarning: &frontendv1.FooterAllowanceAllowedWarning{}}
	case *conversationv1.SessionRateLimitStatus_Rejected:
		line.Verdict = &frontendv1.FooterStatusActivityRateLimit_Rejected{Rejected: &frontendv1.FooterAllowanceRejected{}}
	default:
		return
	}
	if status.UtilizationPercent != nil {
		utilization := status.GetUtilizationPercent() / 100
		line.Utilization = &utilization
	}
	if status.ResetsAtMs != nil {
		resets := status.GetResetsAtMs() / 1000
		line.ResetsAtS = &resets
	}
	s.rateEvent = &rateEventState{line: line, window: name, at: r.opts.clock.Now()}
	r.logOf(ws, s).Debug("daemon.footer.rate_limit_stood", "the footer stood the vendor's rate-limit event",
		dlog.Context{"window": name, "verdict": rateVerdictName(line)})
}

// rateLimitWindow maps the vendor's window onto the strip's, with its name for
// the record and for matching a later event; nil and "" when the vendor named
// none.
func rateLimitWindow(t *conversationv1.SessionRateLimitType) (*frontendv1.FooterStatusActivityRateLimitWindow, string) {
	w := &frontendv1.FooterStatusActivityRateLimitWindow{}
	switch t.GetWindow().(type) {
	case *conversationv1.SessionRateLimitType_FiveHour:
		w.Window = &frontendv1.FooterStatusActivityRateLimitWindow_Session{Session: &frontendv1.FooterStatusActivityRateLimitWindowSession{}}
		return w, "session"
	case *conversationv1.SessionRateLimitType_SevenDay:
		w.Window = &frontendv1.FooterStatusActivityRateLimitWindow_Weekly{Weekly: &frontendv1.FooterStatusActivityRateLimitWindowWeekly{}}
		return w, "weekly"
	case *conversationv1.SessionRateLimitType_SevenDayOpus:
		w.Window = &frontendv1.FooterStatusActivityRateLimitWindow_WeeklyOpus{WeeklyOpus: &frontendv1.FooterStatusActivityRateLimitWindowWeeklyOpus{}}
		return w, "weekly_opus"
	case *conversationv1.SessionRateLimitType_SevenDaySonnet:
		w.Window = &frontendv1.FooterStatusActivityRateLimitWindow_WeeklySonnet{WeeklySonnet: &frontendv1.FooterStatusActivityRateLimitWindowWeeklySonnet{}}
		return w, "weekly_sonnet"
	case *conversationv1.SessionRateLimitType_SevenDayOverageIncluded:
		w.Window = &frontendv1.FooterStatusActivityRateLimitWindow_WeeklyOverageIncluded{
			WeeklyOverageIncluded: &frontendv1.FooterStatusActivityRateLimitWindowWeeklyOverageIncluded{}}
		return w, "weekly_overage_included"
	case *conversationv1.SessionRateLimitType_Overage:
		w.Window = &frontendv1.FooterStatusActivityRateLimitWindow_Overage{Overage: &frontendv1.FooterStatusActivityRateLimitWindowOverage{}}
		return w, "overage"
	default:
		return nil, ""
	}
}

// rateVerdictName names a rate-limit line's verdict for the record.
func rateVerdictName(line *frontendv1.FooterStatusActivityRateLimit) string {
	if line.GetRejected() != nil {
		return "rejected"
	}
	return "allowed_warning"
}

// sharedLine is one status-independent salient line: the salient oneof's field
// that carries it, its message, and when it began standing.
type sharedLine struct {
	field protoreflect.Name
	value proto.Message
	at    time.Time
}

// sharedSalient answers the highest-ranked status-independent salient line
// standing: a deploy's progress, then a rate-limit event, then the push
// notification, then the context-budget line. ok is false when none stands.
func (r *resolver) sharedSalient(s *wsState) (sharedLine, bool) {
	if update, at := r.updateLine(s); update != nil {
		return sharedLine{field: "update", value: update, at: at}, true
	}
	if s.rateEvent != nil {
		return sharedLine{field: "rate_limit", value: s.rateEvent.line, at: s.rateEvent.at}, true
	}
	if s.notification != nil {
		return sharedLine{field: "notification", value: &frontendv1.FooterStatusActivityNotification{Text: s.notification.text}, at: s.notification.at}, true
	}
	if s.contextBudget != nil {
		return sharedLine{field: "context_budget", value: &frontendv1.FooterStatusActivityContextBudget{Text: s.contextBudget.text}, at: s.contextBudget.at}, true
	}
	return sharedLine{}, false
}

// fillShared stamps LINE into SALIENT, a status arm's salient message. Every
// arm's salient message carries `at` and the shared kinds under the same field
// names (TestEverySalientMessageCarriesTheSharedKinds pins it), so one filling
// serves every arm; a message missing one is a contract defect and panics.
func fillShared[M proto.Message](salient M, line sharedLine) M {
	m := salient.ProtoReflect()
	fields := m.Descriptor().Fields()
	at := fields.ByName("at")
	kind := fields.ByName(line.field)
	if at == nil || kind == nil {
		panic("footer: " + string(m.Descriptor().FullName()) + " lacks the shared salient field " + string(line.field))
	}
	m.Set(at, protoreflect.ValueOfMessage(stamp(line.at).ProtoReflect()))
	m.Set(kind, protoreflect.ValueOfMessage(line.value.ProtoReflect()))
	return salient
}

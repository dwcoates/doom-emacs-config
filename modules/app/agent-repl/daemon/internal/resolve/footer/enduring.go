package footer

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE ENDURING TIER: facts that are always true, drawn beneath every transient
// so the activity cell is never empty (footer.proto "The enduring tier"). It is
// the account's usage allowances and how full the main agent's context window
// is, each UNSET until its first figure has been observed.

// enduring composes the enduring line. It is always set; its parts are unset
// until observed.
func (r *resolver) enduring(s *wsState) *frontendv1.FooterActivityEnduring {
	return &frontendv1.FooterActivityEnduring{
		Usage:         r.enduringUsage(s),
		ContextWindow: enduringContextWindow(s),
	}
}

// enduringUsage is the account's usage allowances, or nil before any figure
// has been observed. EVERY FIGURE ON HAND IS DRAWN, WHETHER OR NOT ANY IS NEAR
// ITS LIMIT: the line is always true, so the reader always knows how close the
// account is. `newsworthy` only colors an allowance; it never decides whether
// the line is drawn.
//
// A window the vendor never reported (a weekly allowance the account does not
// have, an overage window most accounts never see) is drawn ABSENT rather than
// synthesized. An unreadable sample leaves the figures on hand standing and
// never opens the line by itself; its outcome stays visible in the logs
// (`logUnreadableSample`).
func (r *resolver) enduringUsage(s *wsState) *frontendv1.FooterActivityEnduringUsage {
	session := r.allowance(&s.rate.session)
	weekly := r.allowance(&s.rate.weekly)
	overage := r.allowance(&s.rate.overage)
	if session == nil && weekly == nil && overage == nil {
		return nil
	}
	usage := &frontendv1.FooterActivityEnduringUsage{
		Session: session,
		Weekly:  weekly,
		Overage: overage,
	}
	// The instant the figures were last READ off a sample, so the client can
	// tick the reading's age ("… 10m 30s ago"). Left UNSET when the figures
	// came from a rate-limit EVENT alone (no read instant), in which case the
	// client draws them with no age.
	if !s.rate.figuresReadAt.IsZero() {
		readAt := epochMs(s.rate.figuresReadAt)
		usage.FiguresReadAtMs = &readAt
	}
	return usage
}

// allowance projects one window's evidence onto the drawn allowance, or nil
// when no figure has been observed for it. The status arm is COPIED, arm for
// arm, from the window's last rate-limit event; no event yet, or an event the
// vendor left statusless, draws NO arm — nothing is defaulted to "allowed".
func (r *resolver) allowance(w *allowanceWindow) *frontendv1.FooterAllowance {
	if !w.figured {
		return nil
	}
	allowance := &frontendv1.FooterAllowance{
		Newsworthy:  w.utilization >= r.opts.rateNewsworth,
		ResetsAtS:   w.resetsAtS,
		Utilization: w.utilization,
	}
	switch w.verdict.GetStatus().(type) {
	case *conversationv1.SessionRateLimitStatus_Allowed:
		allowance.Status = &frontendv1.FooterAllowance_Allowed{Allowed: &frontendv1.FooterAllowanceAllowed{}}
	case *conversationv1.SessionRateLimitStatus_AllowedWarning:
		allowance.Status = &frontendv1.FooterAllowance_AllowedWarning{AllowedWarning: &frontendv1.FooterAllowanceAllowedWarning{}}
	case *conversationv1.SessionRateLimitStatus_Rejected:
		allowance.Status = &frontendv1.FooterAllowance_Rejected{Rejected: &frontendv1.FooterAllowanceRejected{}}
	}
	return allowance
}

// enduringContextWindow is how full the main agent's context window is, or nil
// before a readable context-usage report. The fill is resolved HERE so the
// client draws the percentage without arithmetic.
func enduringContextWindow(s *wsState) *frontendv1.FooterActivityEnduringContextWindow {
	w := s.contextWindow
	if w == nil {
		return nil
	}
	return &frontendv1.FooterActivityEnduringContextWindow{
		UsedTokens:   w.used,
		WindowTokens: w.window,
		Fill:         float64(w.used) / float64(w.window),
	}
}

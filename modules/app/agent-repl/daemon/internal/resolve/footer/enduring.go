package footer

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE ENDURING TIER: facts that are always true, drawn beneath every other
// tier so the activity cell is never empty (footer.proto "The enduring tier").
// It is ONE line: the account's usage allowances. Being always true, it
// carries no age (owner ruling, 2026-09-30).

// enduring composes the enduring line: the usage allowances, or the
// unobserved arm before any figure has been observed.
func (r *resolver) enduring(s *wsState) *frontendv1.FooterActivityEnduring {
	usage := r.enduringUsage(s)
	if usage == nil {
		return &frontendv1.FooterActivityEnduring{Line: &frontendv1.FooterActivityEnduring_Unobserved{
			Unobserved: &frontendv1.FooterActivityEnduringUnobserved{}}}
	}
	return &frontendv1.FooterActivityEnduring{Line: &frontendv1.FooterActivityEnduring_Usage{Usage: usage}}
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
	return &frontendv1.FooterActivityEnduringUsage{
		Session: session,
		Weekly:  weekly,
		Overage: overage,
	}
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

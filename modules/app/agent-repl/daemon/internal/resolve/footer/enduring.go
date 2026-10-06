package footer

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/wsm"
)

// THE ENDURING TIER: facts that are always true, drawn beneath every other
// tier so the activity cell is never empty (footer.proto "The enduring tier").
// It is ONE line: the account's usage allowances. Being always true, it
// carries no age (owner ruling, 2026-09-30).

// enduring composes the enduring line BY THE ACCOUNT'S BILLING MODE (owner
// ruling, 2026-10-06): a per-seat account's spend, a subscription's
// allowances, or the unobserved arm before anything has been observed.
//
// THE LINE IS THE ACCOUNT'S (account.go), and it is NEVER EMPTY: each arm is
// drawn in words.
func (r *resolver) enduring(s *wsState) *frontendv1.FooterActivityEnduring {
	if seat := s.usage.seat; seat != nil {
		return &frontendv1.FooterActivityEnduring{Line: &frontendv1.FooterActivityEnduring_SeatSpend{
			SeatSpend: enduringSeatSpend(seat)}}
	}
	if usage := r.enduringUsage(s); usage != nil {
		return &frontendv1.FooterActivityEnduring{Line: &frontendv1.FooterActivityEnduring_Usage{Usage: usage}}
	}
	return &frontendv1.FooterActivityEnduring{Line: &frontendv1.FooterActivityEnduring_Unobserved{
		Unobserved: &frontendv1.FooterActivityEnduringUnobserved{}}}
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
	session := r.allowance(&s.usage.rate.session)
	weekly := r.allowance(&s.usage.rate.weekly)
	overage := r.allowance(&s.usage.rate.overage)
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
	switch w.verdict {
	case wsm.VerdictAllowed:
		allowance.Status = &frontendv1.FooterAllowance_Allowed{Allowed: &frontendv1.FooterAllowanceAllowed{}}
	case wsm.VerdictAllowedWarning:
		allowance.Status = &frontendv1.FooterAllowance_AllowedWarning{AllowedWarning: &frontendv1.FooterAllowanceAllowedWarning{}}
	case wsm.VerdictRejected:
		allowance.Status = &frontendv1.FooterAllowance_Rejected{Rejected: &frontendv1.FooterAllowanceRejected{}}
	}
	return allowance
}

// enduringSeatSpend is a per-seat account's spend as the footer draws it. The
// utilization, which drives the drawn spend's percent gradient, is
// spent/allotment and is set exactly when the spend is. A zero allotment has
// no ratio: any spend against it is drawn full (1), and none drawn empty (0).
func enduringSeatSpend(seat *seatSpend) *frontendv1.FooterActivityEnduringSeatSpend {
	out := &frontendv1.FooterActivityEnduringSeatSpend{
		Allotment: &frontendv1.FooterMoney{AmountMinor: seat.allotmentMinor, Currency: seat.currency},
	}
	if seat.spentMinor == nil {
		return out
	}
	spent := *seat.spentMinor
	out.Spent = &frontendv1.FooterMoney{AmountMinor: spent, Currency: seat.currency}
	var utilization float64
	switch {
	case seat.allotmentMinor > 0:
		utilization = float64(spent) / float64(seat.allotmentMinor)
	case spent > 0:
		utilization = 1
	}
	out.Utilization = &utilization
	return out
}

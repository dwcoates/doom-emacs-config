package footer

import (
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE ACTIVITY LINE is the strip's finest cell, and exactly ONE stands per
// push. The precedence is the contract's, stated once here and applied by
// every status arm below:
//
//	notification  OUTRANKS everything.
//	status-bound  ranks next — the kinds only that status admits.
//	rate_limited  is SECOND-LOWEST.
//	context_budget is LOWEST — shown only when nothing else stands.
//
// Every line ships the instant it began standing; the client ticks the
// relative age from it.

// stamp renders an activity's standing instant.
func stamp(t time.Time) *frontendv1.FooterStatusActivityAt {
	return &frontendv1.FooterStatusActivityAt{AtMs: epochMs(t)}
}

// notificationLine is the standing notification, or nil.
func (r *resolver) notificationLine(s *wsState) *frontendv1.FooterStatusActivityNotification {
	if s.notification == nil {
		return nil
	}
	return &frontendv1.FooterStatusActivityNotification{Text: s.notification.text}
}

// rateLine is the standing rate-limit report, or nil. A report is drawn only
// once at least one allowance is newsworthy: an unremarkable allowance is not
// news and would crowd out the lines that are.
func (r *resolver) rateLine(s *wsState) *frontendv1.FooterStatusActivityRateLimited {
	if s.rate == nil {
		return nil
	}
	session := r.allowance(s.rate.session)
	weekly := r.allowance(s.rate.weekly)
	if !session.GetNewsworthy() && !weekly.GetNewsworthy() {
		return nil
	}
	return &frontendv1.FooterStatusActivityRateLimited{Session: session, Weekly: weekly}
}

// allowance projects one vendor usage window onto the drawn allowance. The
// vendor reports utilization as a percentage and a reset in millis; the
// contract carries a 0..1 fraction and epoch SECONDS, so the conversion is the
// daemon's and never the client's.
//
// `status` is deliberately empty: SessionUsageWindow carries no status word,
// and inventing one would state a vendor vocabulary nobody reported.
func (r *resolver) allowance(w interface {
	GetUtilizationPercent() float64
	GetResetsAtMs() int64
}) *frontendv1.FooterAllowance {
	utilization := w.GetUtilizationPercent() / 100
	return &frontendv1.FooterAllowance{
		Newsworthy:  utilization >= r.opts.rateNewsworth,
		ResetsAtS:   w.GetResetsAtMs() / 1000,
		Utilization: utilization,
	}
}

// budgetLine is the vendor's standing context-budget warning, or nil.
func (r *resolver) budgetLine(s *wsState) *frontendv1.FooterStatusActivityContextBudget {
	if s.contextBudget == nil {
		return nil
	}
	return &frontendv1.FooterStatusActivityContextBudget{Text: s.contextBudget.text}
}

// idleActivity resolves the line legal while idle: no status-bound kind
// exists, so it is notification, then rate, then budget.
func (r *resolver) idleActivity(s *wsState) *frontendv1.FooterStatusIdleActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusIdleActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusIdleActivity_Notification{Notification: line},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusIdleActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusIdleActivity_RateLimited{RateLimited: line},
		}
	}
	if line := r.budgetLine(s); line != nil {
		return &frontendv1.FooterStatusIdleActivity{
			At:   stamp(s.contextBudget.at),
			Kind: &frontendv1.FooterStatusIdleActivity_ContextBudget{ContextBudget: line},
		}
	}
	return nil
}

// thinkingActivity resolves the line legal while a turn runs.
func (r *resolver) thinkingActivity(s *wsState) *frontendv1.FooterStatusThinkingActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusThinkingActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusThinkingActivity_Notification{Notification: line},
		}
	}
	if s.hook != nil {
		return &frontendv1.FooterStatusThinkingActivity{
			At: stamp(s.hook.at),
			Kind: &frontendv1.FooterStatusThinkingActivity_Hook{
				Hook: &frontendv1.FooterStatusActivityHook{Name: s.hook.name}},
		}
	}
	if s.retrying != nil {
		return &frontendv1.FooterStatusThinkingActivity{
			At: stamp(s.retrying.at),
			Kind: &frontendv1.FooterStatusThinkingActivity_Retrying{
				Retrying: &frontendv1.FooterStatusActivityRetrying{
					Attempt: s.retrying.attempt,
					Status:  s.retrying.status,
				}},
		}
	}
	if s.injected != nil {
		return &frontendv1.FooterStatusThinkingActivity{
			At: stamp(s.injected.at),
			Kind: &frontendv1.FooterStatusThinkingActivity_ContextInjected{
				ContextInjected: &frontendv1.FooterStatusActivityContextInjected{Text: s.injected.text}},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusThinkingActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusThinkingActivity_RateLimited{RateLimited: line},
		}
	}
	if line := r.budgetLine(s); line != nil {
		return &frontendv1.FooterStatusThinkingActivity{
			At:   stamp(s.contextBudget.at),
			Kind: &frontendv1.FooterStatusThinkingActivity_ContextBudget{ContextBudget: line},
		}
	}
	return nil
}

// waitingActivity resolves the line legal while waiting. It is REQUIRED, so
// this never answers nil: every waiting state has a composable line by
// construction, and a producer that cannot compose one has a bug.
func (r *resolver) waitingActivity(s *wsState) *frontendv1.FooterStatusWaitingActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusWaitingActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusWaitingActivity_Notification{Notification: line},
		}
	}
	switch {
	case s.interrupting:
		return &frontendv1.FooterStatusWaitingActivity{
			At: stamp(r.opts.clock.Now()),
			Kind: &frontendv1.FooterStatusWaitingActivity_Interrupting{
				Interrupting: &frontendv1.FooterStatusActivityInterrupting{
					Text: "stopping the current turn…"}},
		}
	case len(s.permissionOrder) > 0:
		return &frontendv1.FooterStatusWaitingActivity{
			At: stamp(r.opts.clock.Now()),
			Kind: &frontendv1.FooterStatusWaitingActivity_GatedCall{
				GatedCall: &frontendv1.FooterStatusActivityGatedCall{
					Text: s.permissions[s.permissionOrder[0]]}},
		}
	case len(s.questionOrder) > 0:
		return &frontendv1.FooterStatusWaitingActivity{
			At: stamp(r.opts.clock.Now()),
			Kind: &frontendv1.FooterStatusWaitingActivity_QuestionLead{
				QuestionLead: &frontendv1.FooterStatusActivityQuestionLead{
					Text: s.questions[s.questionOrder[0]]}},
		}
	case s.coldGate.Standing:
		return &frontendv1.FooterStatusWaitingActivity{
			At: stamp(r.opts.clock.Now()),
			Kind: &frontendv1.FooterStatusWaitingActivity_ColdGateCost{
				ColdGateCost: &frontendv1.FooterStatusActivityColdGateCost{
					Text: s.coldGate.Detail}},
		}
	case s.wakeup != nil:
		wakeup := &frontendv1.FooterStatusActivityWakeup{WakeAtMs: epochMs(s.wakeup.wakeAt)}
		if s.wakeup.reason != "" {
			wakeup.Reason = &frontendv1.FooterStatusActivityWakeupReason{Text: s.wakeup.reason}
		}
		return &frontendv1.FooterStatusWaitingActivity{
			At:   stamp(s.wakeup.at),
			Kind: &frontendv1.FooterStatusWaitingActivity_Wakeup{Wakeup: wakeup},
		}
	case s.blockedOnUser != nil:
		return &frontendv1.FooterStatusWaitingActivity{
			At: stamp(s.blockedOnUser.at),
			Kind: &frontendv1.FooterStatusWaitingActivity_BlockedOnUser{
				BlockedOnUser: &frontendv1.FooterStatusActivityBlockedOnUser{
					Detail: s.blockedOnUser.text}},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusWaitingActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusWaitingActivity_RateLimited{RateLimited: line},
		}
	}
	return &frontendv1.FooterStatusWaitingActivity{
		At: stamp(r.opts.clock.Now()),
		Kind: &frontendv1.FooterStatusWaitingActivity_ContextBudget{
			ContextBudget: &frontendv1.FooterStatusActivityContextBudget{
				Text: budgetText(s)}},
	}
}

// budgetText answers the standing budget warning, or the sentence the waiting
// state's required line falls back to when the vendor has warned about nothing.
func budgetText(s *wsState) string {
	if s.contextBudget != nil {
		return s.contextBudget.text
	}
	return "the session is parked on you"
}

// interruptedActivity resolves the line legal while interrupted.
func (r *resolver) interruptedActivity(s *wsState) *frontendv1.FooterStatusInterruptedActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusInterruptedActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusInterruptedActivity_Notification{Notification: line},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusInterruptedActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusInterruptedActivity_RateLimited{RateLimited: line},
		}
	}
	if line := r.budgetLine(s); line != nil {
		return &frontendv1.FooterStatusInterruptedActivity{
			At:   stamp(s.contextBudget.at),
			Kind: &frontendv1.FooterStatusInterruptedActivity_ContextBudget{ContextBudget: line},
		}
	}
	return nil
}

// mergingActivity resolves the line legal while merging.
func (r *resolver) mergingActivity(s *wsState) *frontendv1.FooterStatusMergingActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusMergingActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusMergingActivity_Notification{Notification: line},
		}
	}
	if s.mergingCommit != nil {
		return &frontendv1.FooterStatusMergingActivity{
			At: stamp(s.mergingCommit.at),
			Kind: &frontendv1.FooterStatusMergingActivity_MergingCommit{
				MergingCommit: &frontendv1.FooterStatusActivityMergingCommit{
					Sha:     s.mergingCommit.sha,
					Subject: s.mergingCommit.subject,
				}},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusMergingActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusMergingActivity_RateLimited{RateLimited: line},
		}
	}
	if line := r.budgetLine(s); line != nil {
		return &frontendv1.FooterStatusMergingActivity{
			At:   stamp(s.contextBudget.at),
			Kind: &frontendv1.FooterStatusMergingActivity_ContextBudget{ContextBudget: line},
		}
	}
	return nil
}

// backgroundActivity resolves the line legal while detached work runs.
func (r *resolver) backgroundActivity(s *wsState) *frontendv1.FooterStatusBackgroundActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusBackgroundActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusBackgroundActivity_Notification{Notification: line},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusBackgroundActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusBackgroundActivity_RateLimited{RateLimited: line},
		}
	}
	if line := r.budgetLine(s); line != nil {
		return &frontendv1.FooterStatusBackgroundActivity{
			At:   stamp(s.contextBudget.at),
			Kind: &frontendv1.FooterStatusBackgroundActivity_ContextBudget{ContextBudget: line},
		}
	}
	return nil
}

// blockedActivity resolves the line legal while blocked.
func (r *resolver) blockedActivity(s *wsState) *frontendv1.FooterStatusBlockedActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusBlockedActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusBlockedActivity_Notification{Notification: line},
		}
	}
	if s.blocked != nil && s.blocked.kind == blockedAuth && s.authLine != nil {
		return &frontendv1.FooterStatusBlockedActivity{
			At: stamp(s.authLine.at),
			Kind: &frontendv1.FooterStatusBlockedActivity_Authenticating{
				Authenticating: &frontendv1.FooterStatusActivityAuthenticating{Line: s.authLine.text}},
		}
	}
	if s.blocked != nil && s.blocked.kind == blockedQueryDied {
		return &frontendv1.FooterStatusBlockedActivity{
			At: stamp(s.blocked.at),
			Kind: &frontendv1.FooterStatusBlockedActivity_QueryDied{
				QueryDied: &frontendv1.FooterStatusActivityQueryDied{Text: s.blocked.line}},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusBlockedActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusBlockedActivity_RateLimited{RateLimited: line},
		}
	}
	if line := r.budgetLine(s); line != nil {
		return &frontendv1.FooterStatusBlockedActivity{
			At:   stamp(s.contextBudget.at),
			Kind: &frontendv1.FooterStatusBlockedActivity_ContextBudget{ContextBudget: line},
		}
	}
	return nil
}

// disconnectedActivity resolves the line legal while the link is not serving.
func (r *resolver) disconnectedActivity(s *wsState) *frontendv1.FooterStatusDisconnectedActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusDisconnectedActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusDisconnectedActivity_Notification{Notification: line},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusDisconnectedActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusDisconnectedActivity_RateLimited{RateLimited: line},
		}
	}
	if line := r.budgetLine(s); line != nil {
		return &frontendv1.FooterStatusDisconnectedActivity{
			At:   stamp(s.contextBudget.at),
			Kind: &frontendv1.FooterStatusDisconnectedActivity_ContextBudget{ContextBudget: line},
		}
	}
	return nil
}

// closingActivity resolves the line legal while closing. The blocked step
// composes its reasons here.
func (r *resolver) closingActivity(s *wsState) *frontendv1.FooterStatusClosingActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusClosingActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusClosingActivity_Notification{Notification: line},
		}
	}
	if s.closing != nil {
		return &frontendv1.FooterStatusClosingActivity{
			At: stamp(r.opts.clock.Now()),
			Kind: &frontendv1.FooterStatusClosingActivity_CloseBlocked{
				CloseBlocked: &frontendv1.FooterStatusActivityCloseBlocked{Text: s.closing.Detail}},
		}
	}
	if line := r.rateLine(s); line != nil {
		return &frontendv1.FooterStatusClosingActivity{
			At:   stamp(s.rate.at),
			Kind: &frontendv1.FooterStatusClosingActivity_RateLimited{RateLimited: line},
		}
	}
	if line := r.budgetLine(s); line != nil {
		return &frontendv1.FooterStatusClosingActivity{
			At:   stamp(s.contextBudget.at),
			Kind: &frontendv1.FooterStatusClosingActivity_ContextBudget{ContextBudget: line},
		}
	}
	return nil
}

// loadingActivity resolves the line legal while context is being injected. It
// is REQUIRED: the injection IS the status, so the item line always exists.
func (r *resolver) loadingActivity(s *wsState) *frontendv1.FooterStatusLoadingActivity {
	if line := r.notificationLine(s); line != nil {
		return &frontendv1.FooterStatusLoadingActivity{
			At:   stamp(s.notification.at),
			Kind: &frontendv1.FooterStatusLoadingActivity_Notification{Notification: line},
		}
	}
	return &frontendv1.FooterStatusLoadingActivity{
		At: stamp(s.loading.at),
		Kind: &frontendv1.FooterStatusLoadingActivity_ContextInjected{
			ContextInjected: &frontendv1.FooterStatusActivityContextInjected{Text: s.loading.line}},
	}
}

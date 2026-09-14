package footer

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// THE ACTIVITY LINE is the strip's finest cell, and exactly ONE stands per
// push. The precedence is the contract's, stated once here and applied by
// every status arm below:
//
//	status-bound  ranks first where a kind EXPLAINS the standing substatus —
//	              `start_failed` under disconnected, the wakeup countdown
//	              under waiting, the injected item under loading. A cell that
//	              exists to explain its step cannot be crowded out of it.
//	fault         ranks next: a STANDING DAEMON FAULT outranks a notification,
//	              because a notification from before the fault would otherwise
//	              hide the condition the user has to be told about. It is the
//	              same exception `start_failed` already held, generalized to
//	              every fault kind by the owner's ruling of 2026-09-13.
//	notification  ranks next.
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

// rateLine is the standing rate-limit report, or nil. The line draws as soon
// as a USAGE SAMPLE exists — figures come from account_usage and are complete
// from the first sample — and at least one allowance is newsworthy: an
// unremarkable allowance is not news and would crowd out the lines that are.
// The verdict arm joins each allowance later, when a rate-limit event for
// that window arrives.
//
// A SAMPLE THAT READ NOTHING IS THE SECOND THING THAT MAKES THE LINE NEWS,
// whatever the figures say. An unremarkable allowance is not news, but an
// allowance nobody could read IS: the figures drawn beside it are whatever
// survived the failed read, and a reader who is never told cannot tell a
// fresh 41% from one the vendor stopped answering about an hour ago. The
// figures still stand — the outcome joins them, it never clears them.
//
// WHEN THE SAMPLE CARRIES NO SEVEN-DAY WINDOW (an account with no weekly
// allowance, or a vendor that reported none) the weekly allowance is drawn
// ABSENT rather than synthesized: FooterStatusActivityRateLimited.weekly is a
// message field, so an unset weekly is representable, and stating a figure
// nobody reported would be worse than stating none.
func (r *resolver) rateLine(s *wsState) *frontendv1.FooterStatusActivityRateLimited {
	session := r.allowance(&s.rate.session)
	weekly := r.allowance(&s.rate.weekly)
	unreadable := s.rate.sampleUnread
	if session == nil && weekly == nil && !unreadable {
		return nil
	}
	if !session.GetNewsworthy() && !weekly.GetNewsworthy() && !unreadable {
		return nil
	}
	return &frontendv1.FooterStatusActivityRateLimited{
		Session: session,
		Weekly:  weekly,
		Sample:  s.rate.sample,
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

// faultLine is the standing daemon fault's line, or nil when none stands, and
// the instant it began standing.
//
// ONE LINE FOR EVERY FAULT FAMILY BUT ONE. The kind names itself and the
// detail says what it says; the status and substatus cells beside it are
// already resolved, so the line carries no third copy of the classification.
// `shim_start_failed` keeps its own richer leaf, which counts the held prompts
// the failure dropped.
func (r *resolver) faultLine(s *wsState) (*frontendv1.FooterStatusActivityFault, time.Time) {
	fault := r.standingFault(s)
	if fault == nil {
		return nil, time.Time{}
	}
	return &frontendv1.FooterStatusActivityFault{Kind: fault.Kind, Detail: fault.Detail}, fault.At
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusIdleActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusIdleActivity_Fault{Fault: line},
		}
	}
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusThinkingActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusThinkingActivity_Fault{Fault: line},
		}
	}
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusWaitingActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusWaitingActivity_Fault{Fault: line},
		}
	}
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusInterruptedActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusInterruptedActivity_Fault{Fault: line},
		}
	}
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusMergingActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusMergingActivity_Fault{Fault: line},
		}
	}
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusBackgroundActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusBackgroundActivity_Fault{Fault: line},
		}
	}
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusBlockedActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusBlockedActivity_Fault{Fault: line},
		}
	}
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
	// THE DEAD-QUERY LINE OUTLIVES THE SUBSTATUS THE TERMINAL PICKS. The turn's
	// terminal arrives after the session's query_died update and respells the
	// block from the FAILURE alone, which is a line-less vendor error; reading
	// the line off the block therefore left the strip saying `blocked · vendor
	// error` and nothing else. The line is the session's fact, so it stands
	// under whichever blocked step the terminal chose.
	if s.queryDied != nil {
		return &frontendv1.FooterStatusBlockedActivity{
			At: stamp(s.queryDied.at),
			Kind: &frontendv1.FooterStatusBlockedActivity_QueryDied{
				QueryDied: &frontendv1.FooterStatusActivityQueryDied{Text: s.queryDied.text}},
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
//
// The bring-up failure OUTRANKS a notification, per footer.proto: it is the
// standing line the `start_failed` step exists to explain, and it can only
// stand while that step does — the successful link edge that would move the
// status off `start_failed` clears the failure in the same breath.
func (r *resolver) disconnectedActivity(s *wsState, log dlog.Logger) *frontendv1.FooterStatusDisconnectedActivity {
	if s.startFailed != nil {
		if !s.startFailed.announced {
			s.startFailed.announced = true
			log.Info("daemon.footer.start_failed_activity",
				"the footer composed the bring-up failure's standing line",
				dlog.Context{
					"workspace_dir":   s.dir,
					"dropped_prompts": s.startFailed.dropped,
					"detail":          s.startFailed.detail,
				})
		}
		return &frontendv1.FooterStatusDisconnectedActivity{
			At: stamp(s.startFailed.at),
			Kind: &frontendv1.FooterStatusDisconnectedActivity_StartFailed{
				StartFailed: &frontendv1.FooterStatusActivityStartFailed{
					Detail:         s.startFailed.detail,
					DroppedPrompts: s.startFailed.dropped,
				}},
		}
	}
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusDisconnectedActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusDisconnectedActivity_Fault{Fault: line},
		}
	}
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusClosingActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusClosingActivity_Fault{Fault: line},
		}
	}
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
	if line, at := r.faultLine(s); line != nil {
		return &frontendv1.FooterStatusLoadingActivity{
			At:   stamp(at),
			Kind: &frontendv1.FooterStatusLoadingActivity_Fault{Fault: line},
		}
	}
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

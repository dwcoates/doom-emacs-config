package footer

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/shimclient"
)

// status resolves the whole status family — the coarse status, its step and
// its activity line — as ONE tree, so an illegal pairing is unrepresentable
// rather than forbidden by comment.
//
// STATUS PRECEDENCE, strongest claim first:
//  1. disconnected — one of the three connectivity hops is not serving, so
//     nothing else the footer could say is knowable right now.
//  2. closing — a close was requested; its refusal manifests here.
//  3. interrupted — MOMENTARY, retired by the R1 dwell.
//  4. loading — MOMENTARY, retired by the R1 dwell.
//  5. blocked — the session cannot proceed until something outside it changes.
//  6. merging — the daemon holds this session for a merge.
//  7. waiting — interrupting, then permission, then question, then cold gate.
//  8. thinking — a turn is in flight.
//  9. background — detached work runs while the main thread is free.
//  10. waiting·wakeup — the fallback the contract admits ONLY where the footer
//     would otherwise read idle, which is why it ranks below background.
//  11. idle.
func (r *resolver) status(s *wsState) *frontendv1.FooterStatus {
	if arm := r.disconnected(s); arm != nil {
		return arm
	}
	if arm := r.closing(s); arm != nil {
		return arm
	}
	if arm := r.interrupted(s); arm != nil {
		return arm
	}
	if arm := r.loading(s); arm != nil {
		return arm
	}
	if arm := r.blocked(s); arm != nil {
		return arm
	}
	if arm := r.merging(s); arm != nil {
		return arm
	}
	if arm := r.waiting(s); arm != nil {
		return arm
	}
	if arm := r.thinking(s); arm != nil {
		return arm
	}
	if arm := r.background(s); arm != nil {
		return arm
	}
	if arm := r.wakeup(s); arm != nil {
		return arm
	}
	return r.idle(s)
}

// disconnected resolves the link's step, or nil while the link serves without
// degradation.
func (r *resolver) disconnected(s *wsState) *frontendv1.FooterStatus {
	if !s.linkSeen {
		return nil
	}
	arm := &frontendv1.FooterStatusDisconnected{}
	switch {
	case s.link == shimclient.LinkDialing:
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Starting{
			Starting: &frontendv1.FooterSubStatusDisconnectedStarting{}}
	case s.link == shimclient.LinkRedialing:
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Severed{
			Severed: &frontendv1.FooterSubStatusDisconnectedSevered{}}
	case s.link == shimclient.LinkDead && s.everConnected:
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Dead{
			Dead: &frontendv1.FooterSubStatusDisconnectedDead{}}
	case s.link == shimclient.LinkDead:
		arm.Substatus = &frontendv1.FooterStatusDisconnected_StartFailed{
			StartFailed: &frontendv1.FooterSubStatusDisconnectedStartFailed{}}
	case !s.hostStream || !s.webStream:
		// A HOP IS DOWN. The daemon-to-shim link serves, but one of the two
		// client streams does not, so the workspace is not connected
		// (daemon.md invariant 11) and the footer says so rather than drawing
		// a status nobody is receiving.
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Severed{
			Severed: &frontendv1.FooterSubStatusDisconnectedSevered{}}
	case s.degraded:
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Degraded{
			Degraded: &frontendv1.FooterSubStatusDisconnectedDegraded{}}
	default:
		return nil
	}
	arm.Activity = r.disconnectedActivity(s)
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Disconnected{Disconnected: arm}}
}

// closing resolves the close step, or nil when no close is blocked.
func (r *resolver) closing(s *wsState) *frontendv1.FooterStatus {
	if s.closing == nil {
		return nil
	}
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Closing{
			Closing: &frontendv1.FooterStatusClosing{
				Substatus: &frontendv1.FooterStatusClosing_Blocked{
					Blocked: &frontendv1.FooterSubStatusCloseBlocked{}},
				Activity: r.closingActivity(s),
			},
		},
	}
}

// interrupted resolves the momentary interrupted status.
func (r *resolver) interrupted(s *wsState) *frontendv1.FooterStatus {
	if s.interrupted == nil {
		return nil
	}
	arm := &frontendv1.FooterStatusInterrupted{Activity: r.interruptedActivity(s)}
	if s.interrupted.kind == interruptedByHostShutdown {
		arm.Substatus = &frontendv1.FooterStatusInterrupted_HostShutdown{
			HostShutdown: &frontendv1.FooterSubStatusInterruptedByHostShutdown{}}
	} else {
		arm.Substatus = &frontendv1.FooterStatusInterrupted_ByUser{
			ByUser: &frontendv1.FooterSubStatusInterruptedByUser{}}
	}
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Interrupted{Interrupted: arm}}
}

// loading resolves the momentary loading status. Its activity is REQUIRED: the
// injection IS the status, so an item line always exists.
func (r *resolver) loading(s *wsState) *frontendv1.FooterStatus {
	if s.loading == nil {
		return nil
	}
	arm := &frontendv1.FooterStatusLoading{Activity: r.loadingActivity(s)}
	switch s.loading.kind {
	case loadingMemory:
		arm.Substatus = &frontendv1.FooterStatusLoading_Memory{
			Memory: &frontendv1.FooterSubStatusLoadingMemory{}}
	case loadingInvoked:
		arm.Substatus = &frontendv1.FooterStatusLoading_Invoked{
			Invoked: &frontendv1.FooterSubStatusLoadingInvoked{}}
	case loadingDiscovered:
		arm.Substatus = &frontendv1.FooterStatusLoading_Discovered{
			Discovered: &frontendv1.FooterSubStatusLoadingDiscovered{}}
	case loadingListing:
		arm.Substatus = &frontendv1.FooterStatusLoading_Listing{
			Listing: &frontendv1.FooterSubStatusLoadingListing{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Loading{Loading: arm}}
}

// blocked resolves the block, or nil when nothing blocks the session.
func (r *resolver) blocked(s *wsState) *frontendv1.FooterStatus {
	if s.blocked == nil {
		return nil
	}
	arm := &frontendv1.FooterStatusBlocked{Activity: r.blockedActivity(s)}
	switch s.blocked.kind {
	case blockedAuth:
		arm.Substatus = &frontendv1.FooterStatusBlocked_Auth{
			Auth: &frontendv1.FooterSubStatusBlockedAuth{}}
	case blockedUsageLimit:
		arm.Substatus = &frontendv1.FooterStatusBlocked_UsageLimit{
			UsageLimit: &frontendv1.FooterSubStatusBlockedUsageLimit{}}
	case blockedVendorError:
		arm.Substatus = &frontendv1.FooterStatusBlocked_VendorError{
			VendorError: &frontendv1.FooterSubStatusBlockedVendorError{}}
	case blockedBilling:
		arm.Substatus = &frontendv1.FooterStatusBlocked_Billing{
			Billing: &frontendv1.FooterSubStatusBlockedBilling{}}
	case blockedQueryDied:
		arm.Substatus = &frontendv1.FooterStatusBlocked_QueryDied{
			QueryDied: &frontendv1.FooterSubStatusBlockedQueryDied{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Blocked{Blocked: arm}}
}

// merging projects the merge orchestrator's facts onto the merging phase.
func (r *resolver) merging(s *wsState) *frontendv1.FooterStatus {
	arm := &frontendv1.FooterStatusMerging{Activity: r.mergingActivity(s)}
	switch s.merge.State {
	case "", "none":
		return nil
	case "enqueuing":
		arm.Substatus = &frontendv1.FooterStatusMerging_Enqueuing{
			Enqueuing: &frontendv1.FooterSubStatusMergingEnqueuing{}}
	case "queued":
		arm.Substatus = &frontendv1.FooterStatusMerging_Queued{
			Queued: &frontendv1.FooterSubStatusMergingQueued{
				Position: int32(s.merge.QueuePosition),
				Depth:    int32(s.merge.QueueDepth),
			}}
	case "parked":
		arm.Substatus = &frontendv1.FooterStatusMerging_Parked{
			Parked: &frontendv1.FooterSubStatusMergingParked{Line: s.merge.ParkedLine}}
	case "conflict":
		arm.Substatus = &frontendv1.FooterStatusMerging_Conflicts{
			Conflicts: &frontendv1.FooterSubStatusMergingConflicts{}}
	case "failed":
		arm.Substatus = &frontendv1.FooterStatusMerging_Failed{
			Failed: &frontendv1.FooterSubStatusMergingFailed{}}
	case "merged":
		arm.Substatus = &frontendv1.FooterStatusMerging_Merged{
			Merged: &frontendv1.FooterSubStatusMergingMerged{}}
	case "merging":
		setMergingPhase(arm, s.merge.ActiveTab)
	default:
		arm.Substatus = &frontendv1.FooterStatusMerging_Merge{
			Merge: &frontendv1.FooterSubStatusMergingMerge{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Merging{Merging: arm}}
}

// mergingPhase maps the front entry's ACTIVE TAB onto the phase substatus. The
// tab is the orchestrator's own fact, so the footer never guesses a phase.
func setMergingPhase(arm *frontendv1.FooterStatusMerging, tab string) {
	switch tab {
	case "pre_prompt":
		arm.Substatus = &frontendv1.FooterStatusMerging_PrePrompt{
			PrePrompt: &frontendv1.FooterSubStatusMergingPrePrompt{}}
	case "testing":
		arm.Substatus = &frontendv1.FooterStatusMerging_Testing{
			Testing: &frontendv1.FooterSubStatusMergingTesting{}}
	case "fixes":
		arm.Substatus = &frontendv1.FooterStatusMerging_Fixes{
			Fixes: &frontendv1.FooterSubStatusMergingFixes{}}
	case "conflicts":
		arm.Substatus = &frontendv1.FooterStatusMerging_Conflicts{
			Conflicts: &frontendv1.FooterSubStatusMergingConflicts{}}
	case "post_prompt":
		arm.Substatus = &frontendv1.FooterStatusMerging_PostPrompt{
			PostPrompt: &frontendv1.FooterSubStatusMergingPostPrompt{}}
	default:
		arm.Substatus = &frontendv1.FooterStatusMerging_Merge{
			Merge: &frontendv1.FooterSubStatusMergingMerge{}}
	}
}

// waiting resolves the parked states other than the wakeup fallback. Its
// activity is REQUIRED: every waiting state has a composable line by
// construction.
func (r *resolver) waiting(s *wsState) *frontendv1.FooterStatus {
	arm := &frontendv1.FooterStatusWaiting{}
	switch {
	case s.interrupting:
		arm.Substatus = &frontendv1.FooterStatusWaiting_Interrupting{
			Interrupting: &frontendv1.FooterSubStatusWaitingInterrupting{}}
	case len(s.permissionOrder) > 0:
		arm.Substatus = &frontendv1.FooterStatusWaiting_Permission{
			Permission: &frontendv1.FooterSubStatusWaitingPermission{}}
	case len(s.questionOrder) > 0:
		arm.Substatus = &frontendv1.FooterStatusWaiting_Question{
			Question: &frontendv1.FooterSubStatusWaitingQuestion{}}
	case s.coldGate.Standing:
		arm.Substatus = &frontendv1.FooterStatusWaiting_ColdGate{
			ColdGate: &frontendv1.FooterSubStatusWaitingColdGate{}}
	default:
		return nil
	}
	arm.Activity = r.waitingActivity(s)
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Waiting{Waiting: arm}}
}

// wakeup resolves the self-scheduled wakeup fallback, which the contract
// admits only where the footer would otherwise read idle.
func (r *resolver) wakeup(s *wsState) *frontendv1.FooterStatus {
	if s.wakeup == nil {
		return nil
	}
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Waiting{
			Waiting: &frontendv1.FooterStatusWaiting{
				Substatus: &frontendv1.FooterStatusWaiting_Wakeup{
					Wakeup: &frontendv1.FooterSubStatusWaitingWakeup{}},
				Activity: r.waitingActivity(s),
			},
		},
	}
}

// thinking resolves the turn's step, or nil when no turn is in flight.
func (r *resolver) thinking(s *wsState) *frontendv1.FooterStatus {
	if s.turn == nil {
		return nil
	}
	arm := &frontendv1.FooterStatusThinking{Activity: r.thinkingActivity(s)}
	switch {
	case s.turn.Act == ActClear:
		arm.Substatus = &frontendv1.FooterStatusThinking_Clearing{
			Clearing: &frontendv1.FooterSubStatusThinkingClearing{}}
	case s.turn.Act == ActCompact || s.compacting:
		arm.Substatus = &frontendv1.FooterStatusThinking_Compacting{
			Compacting: &frontendv1.FooterSubStatusThinkingCompacting{}}
	case !s.sawActivity:
		arm.Substatus = &frontendv1.FooterStatusThinking_Submitting{
			Submitting: &frontendv1.FooterSubStatusThinkingSubmitting{}}
	default:
		arm.Substatus = &frontendv1.FooterStatusThinking_Thinking{
			Thinking: &frontendv1.FooterSubStatusThinkingThinking{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Thinking{Thinking: arm}}
}

// background resolves detached work running while the main thread is free.
func (r *resolver) background(s *wsState) *frontendv1.FooterStatus {
	if len(s.agents)+len(s.shells)+len(s.monitors) == 0 {
		return nil
	}
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Background{
			Background: &frontendv1.FooterStatusBackground{Activity: r.backgroundActivity(s)}}}
}

// idle is the bottom of the tree: nothing in flight.
func (r *resolver) idle(s *wsState) *frontendv1.FooterStatus {
	arm := &frontendv1.FooterStatusIdle{Activity: r.idleActivity(s)}
	if s.turnEverRan {
		arm.Substatus = &frontendv1.FooterStatusIdle_Done{Done: &frontendv1.FooterSubStatusIdleDone{}}
	} else {
		arm.Substatus = &frontendv1.FooterStatusIdle_Ready{Ready: &frontendv1.FooterSubStatusIdleReady{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Idle{Idle: arm}}
}

// statusName is the status arm's name, for logs and for the render-colors
// footer_status table.
func statusName(status *frontendv1.FooterStatus) string {
	switch status.GetStatus().(type) {
	case *frontendv1.FooterStatus_Idle:
		return "idle"
	case *frontendv1.FooterStatus_Thinking:
		return "thinking"
	case *frontendv1.FooterStatus_Waiting:
		return "waiting"
	case *frontendv1.FooterStatus_Interrupted:
		return "interrupted"
	case *frontendv1.FooterStatus_Merging:
		return "merging"
	case *frontendv1.FooterStatus_Background:
		return "background"
	case *frontendv1.FooterStatus_Blocked:
		return "blocked"
	case *frontendv1.FooterStatus_Disconnected:
		return "disconnected"
	case *frontendv1.FooterStatus_Closing:
		return "closing"
	case *frontendv1.FooterStatus_Loading:
		return "loading"
	default:
		return "unset"
	}
}

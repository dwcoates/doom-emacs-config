package footer

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/shimclient"
)

// The FooterStatus.status arms this resolver emits, in the precedence order
// documented below. render-colors.json's footer_status table is asserted
// against them at construction: an arm landing without a color would draw
// unpainted, and a table row no arm claims is a state the vocabulary paints and
// the resolver can never reach.
var statusArms = []string{
	"disconnected", "closing", "interrupted", "loading", "blocked", "merging",
	"merge_conflict", "merge_failed", "merged", "waiting", "thinking",
	"background", "idle",
}

// The FooterAllowance.status arms this resolver emits, asserted the same way
// against the footer_allowance table.
var allowanceArms = []string{"allowed", "allowed_warning", "rejected"}

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
//  7. thinking·<the answered remediation> — a standing cold gate's answer is
//     being SPENT. It outranks `waiting` because the gate it answers is not
//     lifted until the re-open lands, and drawing the question over the answer
//     is exactly what made a minute-long compaction look like a dead button
//     (owner's report, 2026-09-14).
//  8. waiting — interrupting, then permission, then question, then cold gate.
//  9. thinking — a turn is in flight.
//  9. background — detached work runs while the main thread is free.
//  10. waiting·wakeup — the fallback the contract admits ONLY where the footer
//     would otherwise read idle, which is why it ranks below background.
//  11. idle.
func (r *resolver) status(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if arm := r.disconnected(s, log); arm != nil {
		return arm
	}
	if arm := r.closing(s, log); arm != nil {
		return arm
	}
	if arm := r.interrupted(s, log); arm != nil {
		return arm
	}
	if arm := r.loading(s, log); arm != nil {
		return arm
	}
	if arm := r.blocked(s, log); arm != nil {
		return arm
	}
	if arm := r.merging(s, log); arm != nil {
		return arm
	}
	if arm := r.coldGateAnswer(s, log); arm != nil {
		return arm
	}
	if arm := r.waiting(s, log); arm != nil {
		return arm
	}
	if arm := r.thinking(s, log); arm != nil {
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
func (r *resolver) disconnected(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if !s.linkSeen {
		// NO LINK STATE YET IS NOT "SERVING". A standing fault that says the
		// session cannot be reached is evidence in its own right — a bring-up
		// that never produced a link edge at all is exactly the case row N1 1
		// of the footer topology audit was about.
		return r.disconnectedByFault(s, log)
	}
	arm := &frontendv1.FooterStatusDisconnected{}
	switch {
	case s.link == shimclient.LinkDialing:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkDialing"})
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Starting{
			Starting: &frontendv1.FooterSubStatusDisconnectedStarting{}}
	case s.link == shimclient.LinkRedialing:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkRedialing"})
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Severed{
			Severed: &frontendv1.FooterSubStatusDisconnectedSevered{}}
	case s.link == shimclient.LinkDead && s.parked:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkDead && s.parked"})
		// A PARKED SESSION IS IDLE, NOT BROKEN — the same ruling the roster
		// states at resolve/sidebar/status.go, whose `linkArm` promises to
		// mirror THIS step "fact for fact, so the dot and the strip cannot
		// disagree about the same link". The idle sweep put this route down
		// itself and a prompt brings it straight back, so there is no fault to
		// report and the status falls through to the idle family.
		//
		// The webapp makes that more than a wording question: its composer
		// gate IS this word (webapp/src/main.ts — a `disconnected` status
		// closes the composer), so `dead` here withholds the very prompt that
		// revives the session.
		return nil
	case s.link == shimclient.LinkDead && s.everConnected:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkDead && s.everConnected"})
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Dead{
			Dead: &frontendv1.FooterSubStatusDisconnectedDead{}}
	case s.link == shimclient.LinkDead:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkDead"})
		arm.Substatus = &frontendv1.FooterStatusDisconnected_StartFailed{
			StartFailed: &frontendv1.FooterSubStatusDisconnectedStartFailed{}}
	case !s.hostStream || !s.webStream:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case !s.hostStream || !s.webStream"})
		// A HOP IS DOWN. The daemon-to-shim link serves, but one of the two
		// client streams does not, so the workspace is not connected
		// (daemon.md invariant 11) and the footer says so rather than drawing
		// a status nobody is receiving.
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Severed{
			Severed: &frontendv1.FooterSubStatusDisconnectedSevered{}}
	case s.degraded:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.degraded"})
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Degraded{
			Degraded: &frontendv1.FooterSubStatusDisconnectedDegraded{}}
	default:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "default"})
		// THE LINK SERVES. Only a standing fault can still claim the status,
		// and only one that says the session cannot be reached.
		return r.disconnectedByFault(s, log)
	}
	arm.Activity = r.disconnectedActivity(s, log)
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Disconnected{Disconnected: arm}}
}

// disconnectedByFault resolves the disconnected step a STANDING FAULT claims,
// where the link state itself claimed none. The bucket is the health package's
// verdict (THE FAULT PARTITION, internal/health/footer.go), taken as given: no
// mapping is derived here.
//
// A fault whose bucket is not one of the three disconnected steps claims
// nothing: the four non-escalating kinds leave the status exactly as it stands
// and take the activity cell alone, and a `blocked` fault is the blocked
// arm's.
func (r *resolver) disconnectedByFault(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	fault := r.standingFault(s)
	if fault == nil || fault.Status != "disconnected" {
		return nil
	}
	arm := &frontendv1.FooterStatusDisconnected{}
	switch fault.SubStatus {
	case "start_failed":
		arm.Substatus = &frontendv1.FooterStatusDisconnected_StartFailed{
			StartFailed: &frontendv1.FooterSubStatusDisconnectedStartFailed{}}
	case "dead":
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Dead{
			Dead: &frontendv1.FooterSubStatusDisconnectedDead{}}
	case "severed":
		arm.Substatus = &frontendv1.FooterStatusDisconnected_Severed{
			Severed: &frontendv1.FooterSubStatusDisconnectedSevered{}}
	default:
		log.Warn("daemon.footer.fault_bucket_unknown",
			"a standing fault claims the disconnected status with a step this resolver cannot draw",
			dlog.Context{"kind": fault.Kind, "substatus": fault.SubStatus})
		return nil
	}
	log.Debug("daemon.footer.status_decision", "selected a footer status branch",
		dlog.Context{"function": "status", "branch": "disconnected by standing fault", "kind": fault.Kind})
	arm.Activity = r.disconnectedActivity(s, log)
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Disconnected{Disconnected: arm}}
}

// closing resolves the close step, or nil when no close is blocked.
func (r *resolver) closing(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
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
func (r *resolver) interrupted(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
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
func (r *resolver) loading(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if s.loading == nil {
		return nil
	}
	arm := &frontendv1.FooterStatusLoading{Activity: r.loadingActivity(s)}
	switch s.loading.kind {
	case loadingMemory:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case loadingMemory"})
		arm.Substatus = &frontendv1.FooterStatusLoading_Memory{
			Memory: &frontendv1.FooterSubStatusLoadingMemory{}}
	case loadingInvoked:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case loadingInvoked"})
		arm.Substatus = &frontendv1.FooterStatusLoading_Invoked{
			Invoked: &frontendv1.FooterSubStatusLoadingInvoked{}}
	case loadingDiscovered:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case loadingDiscovered"})
		arm.Substatus = &frontendv1.FooterStatusLoading_Discovered{
			Discovered: &frontendv1.FooterSubStatusLoadingDiscovered{}}
	case loadingListing:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case loadingListing"})
		arm.Substatus = &frontendv1.FooterStatusLoading_Listing{
			Listing: &frontendv1.FooterSubStatusLoadingListing{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Loading{Loading: arm}}
}

// blocked resolves the block, or nil when nothing blocks the session.
func (r *resolver) blocked(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if s.blocked == nil {
		// A DAEMON THAT CANNOT SERVE THIS SESSION BLOCKS IT. The vendor-side
		// blocks above are the session's own; this one is the daemon's, and
		// the shim may be perfectly healthy while it stands.
		return r.blockedByFault(s, log)
	}
	arm := &frontendv1.FooterStatusBlocked{Activity: r.blockedActivity(s)}
	switch s.blocked.kind {
	case blockedAuth:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedAuth"})
		arm.Substatus = &frontendv1.FooterStatusBlocked_Auth{
			Auth: &frontendv1.FooterSubStatusBlockedAuth{}}
	case blockedUsageLimit:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedUsageLimit"})
		arm.Substatus = &frontendv1.FooterStatusBlocked_UsageLimit{
			UsageLimit: &frontendv1.FooterSubStatusBlockedUsageLimit{}}
	case blockedVendorError:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedVendorError"})
		arm.Substatus = &frontendv1.FooterStatusBlocked_VendorError{
			VendorError: &frontendv1.FooterSubStatusBlockedVendorError{}}
	case blockedBilling:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedBilling"})
		arm.Substatus = &frontendv1.FooterStatusBlocked_Billing{
			Billing: &frontendv1.FooterSubStatusBlockedBilling{}}
	case blockedQueryDied:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedQueryDied"})
		arm.Substatus = &frontendv1.FooterStatusBlocked_QueryDied{
			QueryDied: &frontendv1.FooterSubStatusBlockedQueryDied{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Blocked{Blocked: arm}}
}

// blockedByFault resolves `blocked · daemon_impaired` when a standing fault
// says the daemon owes this session a service it cannot give — its prompts
// directory, its state client, its durable log sink, its own redeploy. The
// bucket is the health package's verdict, taken as given.
func (r *resolver) blockedByFault(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	fault := r.standingFault(s)
	if fault == nil || fault.Status != "blocked" {
		return nil
	}
	log.Debug("daemon.footer.status_decision", "selected a footer status branch",
		dlog.Context{"function": "status", "branch": "blocked by standing fault", "kind": fault.Kind})
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Blocked{
			Blocked: &frontendv1.FooterStatusBlocked{
				Substatus: &frontendv1.FooterStatusBlocked_DaemonImpaired{
					DaemonImpaired: &frontendv1.FooterSubStatusBlockedDaemonImpaired{}},
				Activity: r.blockedActivity(s),
			},
		},
	}
}

// merging projects the merge orchestrator's facts onto the merging phase.
func (r *resolver) merging(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	arm := &frontendv1.FooterStatusMerging{Activity: r.mergingActivity(s)}
	switch s.merge.State {
	case "", "none":
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case \"\", \"none\""})
		return nil
	case "enqueuing":
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case \"enqueuing\""})
		arm.Substatus = &frontendv1.FooterStatusMerging_Enqueuing{
			Enqueuing: &frontendv1.FooterSubStatusMergingEnqueuing{}}
	case "queued":
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case \"queued\""})
		arm.Substatus = &frontendv1.FooterStatusMerging_Queued{
			Queued: &frontendv1.FooterSubStatusMergingQueued{
				Position: int32(s.merge.QueuePosition),
				Depth:    int32(s.merge.QueueDepth),
			}}
	case "parked":
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case \"parked\""})
		arm.Substatus = &frontendv1.FooterStatusMerging_Parked{
			Parked: &frontendv1.FooterSubStatusMergingParked{Line: s.merge.ParkedLine}}
	case "conflict":
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case \"conflict\""})
		arm.Substatus = &frontendv1.FooterStatusMerging_Conflicts{
			Conflicts: &frontendv1.FooterSubStatusMergingConflicts{}}
	case "failed":
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case \"failed\""})
		arm.Substatus = &frontendv1.FooterStatusMerging_Failed{
			Failed: &frontendv1.FooterSubStatusMergingFailed{}}
	case "merged":
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case \"merged\""})
		arm.Substatus = &frontendv1.FooterStatusMerging_Merged{
			Merged: &frontendv1.FooterSubStatusMergingMerged{}}
	case "merging":
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case \"merging\""})
		setMergingPhase(arm, s.merge.ActiveTab)
	default:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "default"})
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
func (r *resolver) waiting(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	arm := &frontendv1.FooterStatusWaiting{}
	switch {
	case s.interrupting:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.interrupting"})
		arm.Substatus = &frontendv1.FooterStatusWaiting_Interrupting{
			Interrupting: &frontendv1.FooterSubStatusWaitingInterrupting{}}
	case len(s.permissionOrder) > 0:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case len(s.permissionOrder) > 0"})
		arm.Substatus = &frontendv1.FooterStatusWaiting_Permission{
			Permission: &frontendv1.FooterSubStatusWaitingPermission{}}
	case len(s.questionOrder) > 0:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case len(s.questionOrder) > 0"})
		arm.Substatus = &frontendv1.FooterStatusWaiting_Question{
			Question: &frontendv1.FooterSubStatusWaitingQuestion{}}
	case s.coldGate.Standing:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.coldGate.Standing"})
		arm.Substatus = &frontendv1.FooterStatusWaiting_ColdGate{
			ColdGate: &frontendv1.FooterSubStatusWaitingColdGate{}}
	default:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "default"})
		return nil
	}
	arm.Activity = r.waitingActivity(s)
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Waiting{Waiting: arm}}
}

// coldGateAnswer resolves a cold gate's answer while it is being spent, or nil
// when no answer is in flight.
//
// IT INVENTS NO VOCABULARY (owner ruling, 2026-09-14). The status is
// `thinking`; the step is the one the chosen remediation already has — the
// SAME `compacting` step the vendor's auto-compaction takes, so the two read
// alike — and the line is the daemon's own progress sentence.
func (r *resolver) coldGateAnswer(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if s.coldAnswer == nil {
		return nil
	}
	arm := &frontendv1.FooterStatusThinking{Activity: r.thinkingActivity(s)}
	switch s.coldAnswer.Choice {
	case ChoiceCompact:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "cold gate answered with compact"})
		arm.Substatus = &frontendv1.FooterStatusThinking_Compacting{
			Compacting: &frontendv1.FooterSubStatusThinkingCompacting{}}
	case ChoiceClear:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "cold gate answered with clear"})
		arm.Substatus = &frontendv1.FooterStatusThinking_Clearing{
			Clearing: &frontendv1.FooterSubStatusThinkingClearing{}}
	default:
		// A PAID RESUME SUBMITS THE CONVERSATION AND NOTHING ELSE, which is
		// what `submitting` already says; an unrecognized choice reads the
		// same rather than falling out of the tree unpainted.
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "cold gate answered with pay"})
		arm.Substatus = &frontendv1.FooterStatusThinking_Submitting{
			Submitting: &frontendv1.FooterSubStatusThinkingSubmitting{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Thinking{Thinking: arm}}
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
func (r *resolver) thinking(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if s.turn == nil {
		return nil
	}
	arm := &frontendv1.FooterStatusThinking{Activity: r.thinkingActivity(s)}
	switch {
	case s.turn.Act == ActClear:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.turn.Act == ActClear"})
		arm.Substatus = &frontendv1.FooterStatusThinking_Clearing{
			Clearing: &frontendv1.FooterSubStatusThinkingClearing{}}
	case s.turn.Act == ActCompact || s.compacting:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.turn.Act == ActCompact || s.compacting"})
		arm.Substatus = &frontendv1.FooterStatusThinking_Compacting{
			Compacting: &frontendv1.FooterSubStatusThinkingCompacting{}}
	case !s.sawActivity:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case !s.sawActivity"})
		arm.Substatus = &frontendv1.FooterStatusThinking_Submitting{
			Submitting: &frontendv1.FooterSubStatusThinkingSubmitting{}}
	default:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "default"})
		arm.Substatus = &frontendv1.FooterStatusThinking_Thinking{
			Thinking: &frontendv1.FooterSubStatusThinkingThinking{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Thinking{Thinking: arm}}
}

// background resolves detached work running while the main thread is free.
//
// IT ANSWERS FROM THE WATCHER'S LIVE-WORK SET, never from a count of the
// footer's own rows: the watcher reaps each detached item at its terminal and
// is the single authority for detached liveness on this surface and on the
// roster (see livework.go). An EMPTY set is never a background arm, whatever
// rows the footer still holds.
func (r *resolver) background(s *wsState) *frontendv1.FooterStatus {
	if !s.detachedLive() {
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
	case *frontendv1.FooterStatus_MergeConflict:
		return "merge_conflict"
	case *frontendv1.FooterStatus_MergeFailed:
		return "merge_failed"
	case *frontendv1.FooterStatus_Merged:
		return "merged"
	default:
		return "unset"
	}
}

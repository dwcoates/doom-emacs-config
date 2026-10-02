package footer

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/shimclient"
)

// The FooterStatus.status arms this resolver emits, in the precedence order
// documented below. render-colors.json's footer_status table is asserted
// against them at construction: an arm landing without a color would draw
// unpainted, and a table row no arm claims is a state the vocabulary paints and
// the resolver can never reach.
var statusArms = []string{
	"agent_repl_fault", "network_fault", "closing", "interrupted", "loading", "vendor_fault", "merging",
	"merge_failed", "merged", "degraded", "waiting",
	"working", "background", "turn_failed", "idle",
}

// status resolves the whole status family — the coarse status, its step and
// its activity line — as ONE tree, so an illegal pairing is unrepresentable
// rather than forbidden by comment.
//
// THE PRECEDENCE IS NOT STATED HERE. It is resolve/ladder's, the one ladder the
// roster row is projected from too, so the strip and the rail cannot make
// different coarse claims about one workspace (owner ruling, 2026-09-28). This
// resolver only draws each rung it can claim (`rung`) and the idle family at
// the bottom (`idleFamily`), and asserts that what it drew projects back onto
// the claim the ladder chose.
//
// WITHIN a rung the order is this surface's detail:
//   - thinking: a momentary `loading`, then a standing cold gate's answer
//     being SPENT (it outranks the whole waiting rung, because the gate it
//     answers is not lifted until the re-open lands, and drawing the question
//     over the answer is what made a minute-long compaction look like a dead
//     button — owner's report, 2026-09-14), then the turn itself;
//   - waiting: interrupting, then permission, then question, then cold gate;
//   - idle: the momentary `interrupted`, then background, then the wakeup
//     fallback the contract admits ONLY where the footer would otherwise read
//     idle, then idle itself.
func (r *resolver) status(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	claim, status := ladder.Resolve(s.merge.State, s.parked,
		func(claim ladder.Claim) (*frontendv1.FooterStatus, bool) {
			drawn := r.rung(claim, s, log)
			return drawn, drawn != nil
		},
		func() *frontendv1.FooterStatus { return r.idleFamily(s) })
	if drawn, ok := ladder.FooterClaim(status); !ok || drawn != claim {
		log.Error("daemon.footer.status_claim",
			"the footer drew a status that does not project onto the ladder claim it resolved",
			dlog.Context{
				"claim":               string(claim),
				"drawn":               string(drawn),
				"arm":                 statusName(status),
				"invariant_violation": "the footer and the roster must make the same coarse claim",
				"remediation":         "draw the rung's own arm, or place the arm in ladder.FooterClaim",
			})
	}
	return status
}

// rung draws one ladder rung, or nil when this workspace's facts make no claim
// there.
func (r *resolver) rung(claim ladder.Claim, s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	switch claim {
	case ladder.Merging:
		return r.merging(s, log)
	case ladder.AgentReplFault:
		return r.agentReplFault(s, log)
	case ladder.NetworkFault:
		return r.networkFault(s, log)
	case ladder.Closing:
		return r.closing(s, log)
	case ladder.MergeFailed:
		return r.mergeFailed(s, log)
	case ladder.Merged:
		return r.merged(s, log)
	case ladder.VendorFault:
		return r.vendorFault(s, log)
	case ladder.Degraded:
		return r.degraded(s, log)
	case ladder.Waiting:
		if s.coldAnswer != nil {
			// The answer being spent outranks the whole waiting rung; the
			// thinking rung draws it.
			return nil
		}
		return r.waiting(s, log)
	case ladder.Thinking:
		if arm := r.loading(s, log); arm != nil {
			return arm
		}
		if arm := r.coldGateAnswer(s, log); arm != nil {
			return arm
		}
		return r.thinking(s, log)
	default:
		log.Error("daemon.footer.status_rung", "the ladder asked the footer for a rung it has no drawing for",
			dlog.Context{
				"claim":               string(claim),
				"invariant_violation": "every ladder rung above idle has a footer drawing",
				"remediation":         "add the rung to resolver.rung",
			})
		return nil
	}
}

// idleFamily draws the bottom rung, which always answers.
func (r *resolver) idleFamily(s *wsState) *frontendv1.FooterStatus {
	if arm := r.interrupted(s); arm != nil {
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

// agentReplFault resolves the link's step, or a standing agent-repl fault, or
// nil while agent-repl's own services serve. A link that serves WITH
// DEGRADATION is not a fault: it is usable, and the degraded rung draws it
// (owner ruling, 2026-09-28).
//
// A PARKED SESSION NEVER REACHES HERE. The ladder skips the whole rung while
// the idle sweep's park stands (resolve/ladder): the sweep put the route down
// itself and a prompt brings it straight back, so there is no fault to report.
// The webapp makes that more than a wording question: its composer gate is
// this arm's color (render-colors.json, blue closes it), so `dead` for a
// parked session would withhold the very prompt that revives it.
func (r *resolver) agentReplFault(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if !s.linkSeen {
		// NO LINK STATE YET IS NOT "SERVING". A standing fault that says the
		// session cannot be reached is evidence in its own right — a bring-up
		// that never produced a link edge at all is exactly the case row N1 1
		// of the footer topology audit was about.
		if arm := r.agentReplByFault(s, log); arm != nil {
			return arm
		}
		if !ladder.AwaitingBringUp(s.linkSeen, s.turn != nil, s.sessionStarted) {
			return nil
		}
		// A TURN ACCEPTED, OR A SESSION ANNOUNCED, ON A ROUTE NEVER SEEN
		// awaits the bring-up, and the roster draws that window `init`
		// (resolve/ladder).
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "an accepted turn awaits the bring-up"})
		return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_AgentReplFault{
			AgentReplFault: &frontendv1.FooterStatusAgentReplFault{
				Substatus: &frontendv1.FooterStatusAgentReplFault_Starting{
					Starting: &frontendv1.FooterSubStatusAgentReplFaultStarting{}},
				Activity: r.agentReplFaultActivity(s, log),
			}}}
	}
	// A VENDOR THAT DID NOT START IS THE VENDOR'S FAULT, NOT THE LINK'S: a
	// spawned shim stopped after its vendor failed reads as a dead link that
	// never connected, which is the shim PROCESS's word, and the stop was this
	// daemon's own doing. So while a vendor-start fault stands, only a route
	// still being dialed or redialed, or a standing agent-repl fault, claims
	// this rung; the vendor rung draws the rest (owner ruling, 2026-10-02).
	if s.link != shimclient.LinkDialing && s.link != shimclient.LinkRedialing && r.standingFault(s, health.FaultStatusVendorFault) != nil {
		return r.agentReplByFault(s, log)
	}
	arm := &frontendv1.FooterStatusAgentReplFault{}
	switch {
	case s.link == shimclient.LinkDialing:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkDialing"})
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_Starting{
			Starting: &frontendv1.FooterSubStatusAgentReplFaultStarting{}}
	case s.link == shimclient.LinkRedialing:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkRedialing"})
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_Severed{
			Severed: &frontendv1.FooterSubStatusAgentReplFaultSevered{}}
	case s.link == shimclient.LinkDead && s.everConnected:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkDead && s.everConnected"})
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_Dead{
			Dead: &frontendv1.FooterSubStatusAgentReplFaultDead{}}
	case s.link == shimclient.LinkDead:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.link == shimclient.LinkDead"})
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_StartFailed{
			StartFailed: &frontendv1.FooterSubStatusAgentReplFaultStartFailed{}}
	case !s.hostStream || !s.webStream:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case !s.hostStream || !s.webStream"})
		// A HOP IS DOWN. The daemon-to-shim link serves, but one of the two
		// client streams does not, so the workspace is not connected
		// (daemon.md invariant 11) and the footer says so rather than drawing
		// a status nobody is receiving.
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_Severed{
			Severed: &frontendv1.FooterSubStatusAgentReplFaultSevered{}}
	default:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "default"})
		// THE LINK SERVES. Only a standing agent-repl fault can still claim
		// the rung.
		return r.agentReplByFault(s, log)
	}
	arm.Activity = r.agentReplFaultActivity(s, log)
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_AgentReplFault{AgentReplFault: arm}}
}

// agentReplByFault resolves the agent-repl step a STANDING FAULT claims, where
// the link state itself claimed none. The bucket is the health package's
// verdict (THE FAULT PARTITION, internal/health/footer.go), taken as given: no
// mapping is derived here.
func (r *resolver) agentReplByFault(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	fault := r.standingFault(s, health.FaultStatusAgentReplFault)
	if fault == nil {
		return nil
	}
	arm := &frontendv1.FooterStatusAgentReplFault{}
	switch fault.SubStatus {
	case health.FaultSubStatusStartFailed:
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_StartFailed{
			StartFailed: &frontendv1.FooterSubStatusAgentReplFaultStartFailed{}}
	case health.FaultSubStatusDead:
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_Dead{
			Dead: &frontendv1.FooterSubStatusAgentReplFaultDead{}}
	case health.FaultSubStatusSevered:
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_Severed{
			Severed: &frontendv1.FooterSubStatusAgentReplFaultSevered{}}
	case health.FaultSubStatusDaemonImpaired:
		arm.Substatus = &frontendv1.FooterStatusAgentReplFault_DaemonImpaired{
			DaemonImpaired: &frontendv1.FooterSubStatusAgentReplFaultDaemonImpaired{}}
	default:
		log.Error("daemon.footer.fault_bucket_unknown",
			"a standing fault claims the agent-repl fault status with a step this resolver cannot draw",
			dlog.Context{
				"kind": fault.Kind, "substatus": fault.SubStatus,
				"invariant_violation": "every agent-repl fault bucket has a drawing",
				"remediation":         "add the bucket to agentReplByFault",
			})
		return nil
	}
	log.Debug("daemon.footer.status_decision", "selected a footer status branch",
		dlog.Context{"function": "status", "branch": "agent-repl fault by standing fault", "kind": fault.Kind})
	arm.Activity = r.agentReplFaultActivity(s, log)
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_AgentReplFault{AgentReplFault: arm}}
}

// networkFault resolves the network rung: a standing fault that says this
// machine cannot reach the network, or nil.
func (r *resolver) networkFault(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	fault := r.standingFault(s, health.FaultStatusNetworkFault)
	if fault == nil {
		return nil
	}
	if fault.SubStatus != health.FaultSubStatusOffline {
		log.Error("daemon.footer.fault_bucket_unknown",
			"a standing fault claims the network fault status with a step this resolver cannot draw",
			dlog.Context{
				"kind": fault.Kind, "substatus": fault.SubStatus,
				"invariant_violation": "every network fault bucket has a drawing",
				"remediation":         "add the bucket to networkFault",
			})
		return nil
	}
	log.Debug("daemon.footer.status_decision", "selected a footer status branch",
		dlog.Context{"function": "status", "branch": "network fault by standing fault", "kind": fault.Kind})
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_NetworkFault{
		NetworkFault: &frontendv1.FooterStatusNetworkFault{
			Substatus: &frontendv1.FooterStatusNetworkFault_Offline{Offline: &frontendv1.FooterSubStatusNetworkFaultOffline{}},
			Activity:  r.networkFaultActivity(s, fault),
		}}}
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

// vendorFault resolves the vendor rung: a vendor that will not start (by its
// standing fault), else the vendor or account block, else a call the vendor
// is retrying, else nil. A VENDOR-START FAULT RANKS FIRST: it means no session
// exists, so whatever a previous session's turn said about the vendor is
// stale beside it.
func (r *resolver) vendorFault(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if fault := r.standingFault(s, health.FaultStatusVendorFault); fault != nil {
		arm := &frontendv1.FooterStatusVendorFault{}
		switch fault.SubStatus {
		case health.FaultSubStatusVendorRetry:
			arm.Substatus = &frontendv1.FooterStatusVendorFault_VendorRetry{
				VendorRetry: &frontendv1.FooterSubStatusVendorFaultVendorRetry{}}
		case health.FaultSubStatusVendorRejection:
			arm.Substatus = &frontendv1.FooterStatusVendorFault_VendorRejection{
				VendorRejection: &frontendv1.FooterSubStatusVendorFaultVendorRejection{}}
		case health.FaultSubStatusVendorFailed:
			arm.Substatus = &frontendv1.FooterStatusVendorFault_VendorFailed{
				VendorFailed: &frontendv1.FooterSubStatusVendorFaultVendorFailed{}}
		default:
			log.Error("daemon.footer.fault_bucket_unknown",
				"a standing fault claims the vendor fault status with a step this resolver cannot draw",
				dlog.Context{
					"kind": fault.Kind, "substatus": fault.SubStatus,
					"invariant_violation": "every vendor fault bucket has a drawing",
					"remediation":         "add the bucket to vendorFault",
				})
			return nil
		}
		log.Debug("daemon.footer.status_decision", "selected a footer status branch",
			dlog.Context{"function": "status", "branch": "vendor fault by standing fault", "kind": fault.Kind})
		arm.Activity = r.vendorFaultActivity(s)
		return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_VendorFault{VendorFault: arm}}
	}
	if s.blocked == nil {
		if !s.retryBlocks() {
			return nil
		}
		// A TURN WHOSE CALL THE VENDOR IS RETRYING CANNOT ADVANCE, so it is a
		// vendor fault until the retried agent is answered (ladder/retry.go).
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "the vendor is retrying the turn's call"})
		return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_VendorFault{VendorFault: &frontendv1.FooterStatusVendorFault{
			Substatus: &frontendv1.FooterStatusVendorFault_ApiRetrying{ApiRetrying: &frontendv1.FooterSubStatusVendorFaultApiRetrying{}},
			Activity:  r.vendorFaultActivity(s),
		}}}
	}
	arm := &frontendv1.FooterStatusVendorFault{Activity: r.vendorFaultActivity(s)}
	switch s.blocked.kind {
	case blockedAuth:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedAuth"})
		arm.Substatus = &frontendv1.FooterStatusVendorFault_Auth{
			Auth: &frontendv1.FooterSubStatusVendorFaultAuth{}}
	case blockedUsageLimit:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedUsageLimit"})
		arm.Substatus = &frontendv1.FooterStatusVendorFault_UsageLimit{
			UsageLimit: &frontendv1.FooterSubStatusVendorFaultUsageLimit{}}
	case blockedVendorError:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedVendorError"})
		arm.Substatus = &frontendv1.FooterStatusVendorFault_VendorError{
			VendorError: &frontendv1.FooterSubStatusVendorFaultVendorError{}}
	case blockedBilling:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedBilling"})
		arm.Substatus = &frontendv1.FooterStatusVendorFault_Billing{
			Billing: &frontendv1.FooterSubStatusVendorFaultBilling{}}
	case blockedQueryDied:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case blockedQueryDied"})
		arm.Substatus = &frontendv1.FooterStatusVendorFault_QueryDied{
			QueryDied: &frontendv1.FooterSubStatusVendorFaultQueryDied{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_VendorFault{VendorFault: arm}}
}

// merging projects a merge IN FLIGHT onto its step. The ladder calls it only
// when the merge state stands on the merging rung, so a concluded merge --
// failed or merged -- never reaches here: each is its own arm.
//
// THE STEP IS THE ORCHESTRATOR'S FACT; the footer never guesses one. A merge
// in flight whose facts name no step it knows is an orchestrator defect,
// recorded at ERROR and drawn with no substatus rather than a made-up one.
func (r *resolver) merging(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	arm := &frontendv1.FooterStatusMerging{Activity: r.mergingActivity(s)}
	m := s.merge
	switch m.Step {
	case StepEnqueued:
		arm.Substatus = &frontendv1.FooterStatusMerging_Enqueued{Enqueued: &frontendv1.FooterSubStatusMergingEnqueued{
			Place: uint32(m.QueuePlace), Waiting: uint32(m.QueueWaiting)}}
	case StepPreprocessing:
		arm.Substatus = &frontendv1.FooterStatusMerging_Preprocessing{Preprocessing: &frontendv1.FooterSubStatusMergingPreprocessing{}}
	case StepRebasing:
		arm.Substatus = &frontendv1.FooterStatusMerging_Rebasing{Rebasing: &frontendv1.FooterSubStatusMergingRebasing{
			Replayed: uint32(m.Replayed), Total: uint32(m.Total)}}
	case StepConflictResolution:
		arm.Substatus = &frontendv1.FooterStatusMerging_ConflictResolution{ConflictResolution: &frontendv1.FooterSubStatusMergingConflictResolution{}}
	case StepTesting:
		arm.Substatus = &frontendv1.FooterStatusMerging_Testing{Testing: &frontendv1.FooterSubStatusMergingTesting{}}
	case StepFixing:
		arm.Substatus = &frontendv1.FooterStatusMerging_Fixing{Fixing: &frontendv1.FooterSubStatusMergingFixing{
			Attempt: uint32(m.Attempt), MaxAttempts: uint32(m.MaxAttempts)}}
	case StepCommitting:
		arm.Substatus = &frontendv1.FooterStatusMerging_Committing{Committing: &frontendv1.FooterSubStatusMergingCommitting{}}
	case StepUpdatingMain:
		arm.Substatus = &frontendv1.FooterStatusMerging_UpdatingMain{UpdatingMain: &frontendv1.FooterSubStatusMergingUpdatingMain{}}
	case StepPostprocessing:
		arm.Substatus = &frontendv1.FooterStatusMerging_Postprocessing{Postprocessing: &frontendv1.FooterSubStatusMergingPostprocessing{}}
	default:
		log.Error("daemon.footer.merge_step", "a merge in flight names no step the footer draws; it is drawn with no substatus",
			dlog.Context{
				"state":               m.State,
				"step":                string(m.Step),
				"invariant_violation": "every merge in flight stands on a named step",
				"remediation":         "set MergeFacts.Step with every queued or merging state",
			})
	}
	log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "merging", "step": string(m.Step)})
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Merging{Merging: arm}}
}

// mergeFailed draws a failed merge, its substatus the AREA it failed in. A
// failed merge naming no area is an orchestrator defect, recorded at ERROR and
// drawn with no substatus rather than a made-up one.
func (r *resolver) mergeFailed(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	arm := &frontendv1.FooterStatusMergeFailed{Activity: r.mergingActivity(s)}
	switch s.merge.FailedArea {
	case FailedConflicts:
		arm.Substatus = &frontendv1.FooterStatusMergeFailed_Conflicts{Conflicts: &frontendv1.FooterSubStatusMergeFailedConflicts{}}
	case FailedTests:
		arm.Substatus = &frontendv1.FooterStatusMergeFailed_Tests{Tests: &frontendv1.FooterSubStatusMergeFailedTests{}}
	case FailedOther:
		arm.Substatus = &frontendv1.FooterStatusMergeFailed_Other{Other: &frontendv1.FooterSubStatusMergeFailedOther{}}
	default:
		log.Error("daemon.footer.merge_failed_area", "a failed merge names no area; it is drawn with no substatus",
			dlog.Context{
				"area":                string(s.merge.FailedArea),
				"invariant_violation": "every failed merge names the area it failed in",
				"remediation":         "set MergeFacts.FailedArea with every failed state",
			})
	}
	log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "merge failed", "area": string(s.merge.FailedArea)})
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_MergeFailed{MergeFailed: arm}}
}

// merged draws a landed merge.
func (r *resolver) merged(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "merged"})
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Merged{
		Merged: &frontendv1.FooterStatusMerged{Activity: r.mergingActivity(s)}}}
}

// waiting resolves the parked states other than the wakeup fallback. Its
// activity is REQUIRED: every waiting state has a composable line by
// construction.
func (r *resolver) waiting(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	arm := &frontendv1.FooterStatusWaiting{}
	// EACH STEP AND THE SALIENT LINE THAT EXPLAINS IT ARE ONE DECISION, so a
	// waiting step can never stand without its line.
	switch {
	case s.interrupting:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.interrupting"})
		arm.Substatus = &frontendv1.FooterStatusWaiting_Interrupting{
			Interrupting: &frontendv1.FooterSubStatusWaitingInterrupting{}}
		arm.Activity = waitingSalient(s.interruptingAt, func(w *frontendv1.FooterStatusWaitingSalient) {
			w.Kind = &frontendv1.FooterStatusWaitingSalient_Interrupting{
				Interrupting: &frontendv1.FooterStatusActivityInterrupting{Text: interruptingLine}}
		})
	case len(s.permissionOrder) > 0:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case len(s.permissionOrder) > 0"})
		arm.Substatus = &frontendv1.FooterStatusWaiting_Permission{
			Permission: &frontendv1.FooterSubStatusWaitingPermission{}}
		ask := s.permissions[s.permissionOrder[0]]
		arm.Activity = waitingSalient(ask.at, func(w *frontendv1.FooterStatusWaitingSalient) {
			w.Kind = &frontendv1.FooterStatusWaitingSalient_GatedCall{
				GatedCall: &frontendv1.FooterStatusActivityGatedCall{Text: ask.text}}
		})
	case len(s.questionOrder) > 0:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case len(s.questionOrder) > 0"})
		arm.Substatus = &frontendv1.FooterStatusWaiting_Question{
			Question: &frontendv1.FooterSubStatusWaitingQuestion{}}
		batch := s.questions[s.questionOrder[0]]
		arm.Activity = waitingSalient(batch.at, func(w *frontendv1.FooterStatusWaitingSalient) {
			w.Kind = &frontendv1.FooterStatusWaitingSalient_QuestionLead{
				QuestionLead: &frontendv1.FooterStatusActivityQuestionLead{Text: batch.text}}
		})
	case s.coldGate.Standing:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.coldGate.Standing"})
		arm.Substatus = &frontendv1.FooterStatusWaiting_ColdGate{
			ColdGate: &frontendv1.FooterSubStatusWaitingColdGate{}}
		arm.Activity = waitingSalient(s.coldGateAt, func(w *frontendv1.FooterStatusWaitingSalient) {
			w.Kind = &frontendv1.FooterStatusWaitingSalient_ColdGateCost{
				ColdGateCost: &frontendv1.FooterStatusActivityColdGateCost{Text: s.coldGate.Detail}}
		})
	default:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "default"})
		return nil
	}
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
	arm := &frontendv1.FooterStatusWorking{Activity: r.workingActivity(s)}
	switch s.coldAnswer.Choice {
	case ChoiceCompact:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "cold gate answered with compact"})
		arm.Substatus = &frontendv1.FooterStatusWorking_Compacting{
			Compacting: &frontendv1.FooterSubStatusWorkingCompacting{}}
	case ChoiceClear:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "cold gate answered with clear"})
		arm.Substatus = &frontendv1.FooterStatusWorking_Clearing{
			Clearing: &frontendv1.FooterSubStatusWorkingClearing{}}
	default:
		// A PAID RESUME SUBMITS THE CONVERSATION AND NOTHING ELSE, which is
		// what `submitting` already says; an unrecognized choice reads the
		// same rather than falling out of the tree unpainted.
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "cold gate answered with pay"})
		arm.Substatus = &frontendv1.FooterStatusWorking_Submitting{
			Submitting: &frontendv1.FooterSubStatusWorkingSubmitting{}}
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Working{Working: arm}}
}

// wakeup resolves the self-scheduled wakeup fallback, which the contract
// admits only where the footer would otherwise read idle.
func (r *resolver) wakeup(s *wsState) *frontendv1.FooterStatus {
	if s.wakeup == nil {
		return nil
	}
	wakeup := &frontendv1.FooterStatusActivityWakeup{WakeAtMs: epochMs(s.wakeup.wakeAt)}
	if s.wakeup.reason != "" {
		wakeup.Reason = &frontendv1.FooterStatusActivityWakeupReason{Text: s.wakeup.reason}
	}
	return &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_Waiting{
			Waiting: &frontendv1.FooterStatusWaiting{
				Substatus: &frontendv1.FooterStatusWaiting_Wakeup{
					Wakeup: &frontendv1.FooterSubStatusWaitingWakeup{}},
				Activity: waitingSalient(s.wakeup.at, func(w *frontendv1.FooterStatusWaitingSalient) {
					w.Kind = &frontendv1.FooterStatusWaitingSalient_Wakeup{Wakeup: wakeup}
				}),
			},
		},
	}
}

// interruptingLine is the waiting · interrupting step's line.
const interruptingLine = "stopping the current turn…"

// thinking resolves the turn's step, or nil when no turn is in flight.
//
// A VENDOR COMPACTION IS A TURN IN FLIGHT even before the turn-open edge it
// belongs to lands: `SessionUpdate.compacting` arrives first, and the roster
// draws `compacting` from that same fact the moment it does. Answering nil
// there left the strip reading idle beside a rail reading compacting.
func (r *resolver) thinking(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if s.turn == nil && !s.compacting {
		return nil
	}
	arm := &frontendv1.FooterStatusWorking{Activity: r.workingActivity(s)}
	switch {
	case s.turn == nil:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.turn == nil (vendor compaction)"})
		arm.Substatus = &frontendv1.FooterStatusWorking_Compacting{
			Compacting: &frontendv1.FooterSubStatusWorkingCompacting{}}
	case s.turn.Act == ActClear:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.turn.Act == ActClear"})
		arm.Substatus = &frontendv1.FooterStatusWorking_Clearing{
			Clearing: &frontendv1.FooterSubStatusWorkingClearing{}}
	case s.turn.Act == ActCompact || s.compacting:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.turn.Act == ActCompact || s.compacting"})
		arm.Substatus = &frontendv1.FooterStatusWorking_Compacting{
			Compacting: &frontendv1.FooterSubStatusWorkingCompacting{}}
	case !s.sawActivity:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case !s.sawActivity"})
		arm.Substatus = &frontendv1.FooterStatusWorking_Submitting{
			Submitting: &frontendv1.FooterSubStatusWorkingSubmitting{}}
	default:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "default"})
		arm.Substatus = workingStep(s).Substatus
	}
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Working{Working: arm}}
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

// degraded resolves the degraded rung: the session serves, but the daemon's
// view of it is compromised. It is nil when the view is whole.
//
// A STATE THE SHIM NEVER RE-REPORTED is named first: it is about the whole
// session (the daemon cannot see whether a turn is in flight), where an
// observation window is about holes in what is drawn.
func (r *resolver) degraded(s *wsState, log dlog.Logger) *frontendv1.FooterStatus {
	if !s.linkSeen {
		return nil
	}
	arm := &frontendv1.FooterStatusDegraded{}
	switch {
	case s.stateUnreported:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.stateUnreported"})
		arm.Substatus = &frontendv1.FooterStatusDegraded_StateUnreported{
			StateUnreported: &frontendv1.FooterSubStatusDegradedStateUnreported{}}
	case s.degraded:
		log.Debug("daemon.footer.status_decision", "selected a footer status branch", dlog.Context{"function": "status", "branch": "case s.degraded"})
		arm.Substatus = &frontendv1.FooterStatusDegraded_Observation{
			Observation: &frontendv1.FooterSubStatusDegradedObservation{}}
	default:
		return nil
	}
	arm.Activity = r.idleActivity(s)
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Degraded{Degraded: arm}}
}

// idle is the bottom of the tree: nothing in flight.
//
// A FAILED TURN END is its own arm, `turn_failed` — the same turn end the
// roster draws `turn_failed` (ladder.ClassifyFailure) — because it is
// turquoise where idle is green (owner ruling, 2026-09-28). It stands where
// `idle` would, and carries idle's activity kinds.
func (r *resolver) idle(s *wsState) *frontendv1.FooterStatus {
	if s.turnFailed {
		return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_TurnFailed{
			TurnFailed: &frontendv1.FooterStatusTurnFailed{Activity: r.idleActivity(s)}}}
	}
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
	case *frontendv1.FooterStatus_Working:
		return "working"
	case *frontendv1.FooterStatus_Waiting:
		return "waiting"
	case *frontendv1.FooterStatus_Interrupted:
		return "interrupted"
	case *frontendv1.FooterStatus_Merging:
		return "merging"
	case *frontendv1.FooterStatus_Background:
		return "background"
	case *frontendv1.FooterStatus_VendorFault:
		return "vendor_fault"
	case *frontendv1.FooterStatus_NetworkFault:
		return "network_fault"
	case *frontendv1.FooterStatus_Degraded:
		return "degraded"
	case *frontendv1.FooterStatus_TurnFailed:
		return "turn_failed"
	case *frontendv1.FooterStatus_AgentReplFault:
		return "agent_repl_fault"
	case *frontendv1.FooterStatus_Closing:
		return "closing"
	case *frontendv1.FooterStatus_Loading:
		return "loading"
	case *frontendv1.FooterStatus_MergeFailed:
		return "merge_failed"
	case *frontendv1.FooterStatus_Merged:
		return "merged"
	default:
		return "unset"
	}
}

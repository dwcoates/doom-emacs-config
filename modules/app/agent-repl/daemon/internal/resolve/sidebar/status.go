package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/wsm"
)

// statusArm resolves the row's status arm by PROJECTING THE FOOTER'S STATUS.
//
// THE ROSTER RESOLVES NO STATUS OF ITS OWN. The footer resolver is the one
// walker of resolve/ladder, over every fact the daemon holds about the
// workspace, and the row takes the claim the footer resolved and draws its own
// arm within it (owner ruling, 2026-10-08: the footer and the sidebar/tab-bar
// status are the same structurally, never by coincidence). A fact that reaches
// only one resolver therefore cannot split the surfaces: the roster has no
// claim to make that the footer did not make first.
//
// ONE ARM SITS ABOVE THE PROJECTION: `inactive`, a registered workspace with
// no open perspective and nothing live behind it. It dominates every session
// state, and no footer is ever drawn for it.
//
// WITHIN A CLAIM the roster draws the footer's step as its own arm
// (`projectArm`), except on the idle rung, whose detail is the roster's own
// ruling (`idleArm`): an unread turn end holds the row over detached work, and
// a read one is drawn partial.
func statusArm(s *wsState, rec wsm.Workspace, session *wsm.Session, status *frontendv1.FooterStatus, log dlog.Logger) string {
	if !s.live(session) && rec.Closed {
		return "inactive"
	}
	claim, ok := ladder.FooterClaim(status)
	if !ok {
		log.Error("daemon.sidebar.footer_status_unset",
			"the footer answered a status with no arm set, so the roster has no claim to project",
			dlog.Context{
				"invariant_violation": "the footer's status always resolves, bottoming out at idle",
				"remediation":         "find the footer path that published an unset status",
			})
		return "none"
	}
	arm := idleArm(s, session, log)
	if claim != ladder.Idle {
		arm = projectArm(status, log)
	}
	if drawn, ok := ladder.RosterArmClaim(arm); !ok || drawn != claim {
		log.Error("daemon.sidebar.status_claim",
			"the roster drew a status arm that does not project onto the claim the footer resolved",
			dlog.Context{
				"claim":               string(claim),
				"drawn":               string(drawn),
				"arm":                 arm,
				"footer_arm":          footerArmName(status),
				"invariant_violation": "the roster and the footer must make the same coarse claim",
				"remediation":         "project the footer's step in projectArm, or place the arm in ladder.RosterArmClaim",
			})
	}
	log.Debug("daemon.sidebar.status_decision", "the roster projected the footer's status",
		dlog.Context{"claim": string(claim), "footer_arm": footerArmName(status), "arm": arm})
	return arm
}

// projectArm names the roster arm that draws the footer's status on every rung
// above idle. Each footer step maps onto exactly one roster arm making the same
// claim; a step this function does not name answers empty, which statusArm
// reports as a broken projection.
func projectArm(status *frontendv1.FooterStatus, log dlog.Logger) string {
	switch arm := status.GetStatus().(type) {
	case *frontendv1.FooterStatus_Merging:
		if arm.Merging.GetEnqueued() != nil {
			return "merge_queued"
		}
		return "merging"
	case *frontendv1.FooterStatus_MergeFailed:
		return "merge_failed"
	case *frontendv1.FooterStatus_Merged:
		return "merged"
	case *frontendv1.FooterStatus_AgentReplFault:
		return agentReplFaultArm(arm.AgentReplFault)
	case *frontendv1.FooterStatus_NetworkFault:
		return "network_fault"
	case *frontendv1.FooterStatus_Closing:
		return "closing"
	case *frontendv1.FooterStatus_VendorFault:
		return vendorFaultArm(arm.VendorFault)
	case *frontendv1.FooterStatus_Degraded:
		return "degraded"
	case *frontendv1.FooterStatus_Permission:
		return "permission"
	case *frontendv1.FooterStatus_Question:
		return "question"
	case *frontendv1.FooterStatus_Waiting:
		return "waiting"
	case *frontendv1.FooterStatus_Working:
		switch {
		case arm.Working.GetSubmitting() != nil:
			return "submitting"
		case arm.Working.GetClearing() != nil:
			return "clearing"
		case arm.Working.GetCompacting() != nil:
			return "compacting"
		default:
			return "thinking"
		}
	case *frontendv1.FooterStatus_Loading:
		return "thinking"
	default:
		log.Error("daemon.sidebar.project_arm", "the footer drew a status the roster has no projection for",
			dlog.Context{
				"footer_arm":          footerArmName(status),
				"invariant_violation": "every footer status above idle has a roster arm",
				"remediation":         "add the footer arm to projectArm",
			})
		return ""
	}
}

// agentReplFaultArm projects the footer's agent-repl fault step.
func agentReplFaultArm(fault *frontendv1.FooterStatusAgentReplFault) string {
	switch fault.GetSubstatus().(type) {
	case *frontendv1.FooterStatusAgentReplFault_Starting:
		return "init"
	case *frontendv1.FooterStatusAgentReplFault_Severed:
		return "severed"
	case *frontendv1.FooterStatusAgentReplFault_Dead:
		return "dead"
	case *frontendv1.FooterStatusAgentReplFault_StartFailed:
		return "start_failed"
	case *frontendv1.FooterStatusAgentReplFault_TurnDied:
		return "turn_died"
	case *frontendv1.FooterStatusAgentReplFault_DaemonImpaired:
		return "daemon_impaired"
	default:
		return ""
	}
}

// vendorFaultArm projects the footer's vendor fault step: a vendor that will
// not START is `vendor_fault`, a call the vendor is retrying is
// `api_retrying`, and every vendor or account block is `vendor_blocked`.
func vendorFaultArm(fault *frontendv1.FooterStatusVendorFault) string {
	switch fault.GetSubstatus().(type) {
	case *frontendv1.FooterStatusVendorFault_VendorRetry,
		*frontendv1.FooterStatusVendorFault_VendorRejection,
		*frontendv1.FooterStatusVendorFault_VendorFailed:
		return "vendor_fault"
	case *frontendv1.FooterStatusVendorFault_ApiRetrying:
		return "api_retrying"
	default:
		return "vendor_blocked"
	}
}

// footerArmName names the footer status's arm for a record, "unset" for none.
func footerArmName(status *frontendv1.FooterStatus) string {
	if status.GetStatus() == nil {
		return "unset"
	}
	return string(status.ProtoReflect().WhichOneof(status.ProtoReflect().Descriptor().Oneofs().ByName("status")).Name())
}

// noSessionArm names `none` — registered, but no session was ever created —
// and is empty when a session exists or is coming up. It is an ASSERTION that
// the resolver looked and there is none, which is why it has an arm at all
// rather than leaving the oneof unset.
func noSessionArm(s *wsState, session *wsm.Session) string {
	if session != nil || s.started || s.linkSeen {
		return ""
	}
	return "none"
}

// terminalHibernated is wsm's spelling of the idle sweep's stand-down. It is a
// PARK: the workspace is recoverable by a prompt, and the session record keeps
// the mark only so the next boot knows why the shim is gone. It is spelled
// here rather than imported from drain, which sits above this resolver.
const terminalHibernated = "hibernated"

// parked reports whether a session record carries the idle sweep's park.
func parked(session *wsm.Session) bool {
	return session != nil && session.Terminal != nil && session.Terminal.Kind == terminalHibernated
}

// idleArm names the bottom rung's arm. It always answers: every path ends at
// `ready`.
func idleArm(s *wsState, session *wsm.Session, log dlog.Logger) string {
	switch {
	case noSessionArm(s, session) != "":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "no session was ever created"})
		return "none"
	case s.resultUnreadNow():
		arm := s.turnEndArm()
		if s.asyncLive() {
			log.Debug("daemon.sidebar.unread_outranks_async",
				"the last turn's result is unread, so the row stays on its turn-end arm over live detached work",
				dlog.Context{"status": arm})
		}
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.resultUnreadNow()"})
		return arm
	case s.asyncLive():
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.asyncLive()"})
		return "idle_async"
	case s.turnEverRan:
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.turnEverRan", "close": int(s.lastClose)})
		return s.turnEndArm()
	default:
		log.Debug("daemon.sidebar.status", "the session is live and idle", nil)
		return "ready"
	}
}

// setStatus installs the named arm on the row. It is the ONE place a name
// becomes an arm, so the name the record carries, the name the render-colors
// table is asserted against and the arm the wire carries are the same string
// resolved once.
//
// AN UNSET ONEOF IS A CONTRACT BREACH, so an unrecognized name is recorded
// loudly and the row takes `none` — the arm that says the resolver looked.
func setStatus(row *frontendv1.RosterRow, arm string, log dlog.Logger) {
	switch arm {
	case "submitting":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"submitting\""})
		row.Status = &frontendv1.RosterRow_Submitting{Submitting: &frontendv1.RosterRowStatusSubmitting{}}
	case "thinking":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"thinking\""})
		row.Status = &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}}
	case "clearing":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"clearing\""})
		row.Status = &frontendv1.RosterRow_Clearing{Clearing: &frontendv1.RosterRowStatusClearing{}}
	case "compacting":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"compacting\""})
		row.Status = &frontendv1.RosterRow_Compacting{Compacting: &frontendv1.RosterRowStatusCompacting{}}
	case "permission":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"permission\""})
		row.Status = &frontendv1.RosterRow_Permission{Permission: &frontendv1.RosterRowStatusPermission{}}
	case "question":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"question\""})
		row.Status = &frontendv1.RosterRow_Question{Question: &frontendv1.RosterRowStatusQuestion{}}
	case "done":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"done\""})
		row.Status = &frontendv1.RosterRow_Done{Done: &frontendv1.RosterRowStatusDone{}}
	case "interrupted":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"interrupted\""})
		row.Status = &frontendv1.RosterRow_Interrupted{Interrupted: &frontendv1.RosterRowStatusInterrupted{}}
	case "turn_failed":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"turn_failed\""})
		row.Status = &frontendv1.RosterRow_TurnFailed{TurnFailed: &frontendv1.RosterRowStatusTurnFailed{}}
	case "ready":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"ready\""})
		row.Status = &frontendv1.RosterRow_Ready{Ready: &frontendv1.RosterRowStatusReady{}}
	case "idle_async":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"idle_async\""})
		row.Status = &frontendv1.RosterRow_IdleAsync{IdleAsync: &frontendv1.RosterRowStatusIdleAsync{}}
	case "vendor_blocked":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"vendor_blocked\""})
		row.Status = &frontendv1.RosterRow_VendorBlocked{VendorBlocked: &frontendv1.RosterRowStatusVendorBlocked{}}
	case "vendor_fault":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"vendor_fault\""})
		row.Status = &frontendv1.RosterRow_VendorFault{VendorFault: &frontendv1.RosterRowStatusVendorFault{}}
	case "network_fault":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"network_fault\""})
		row.Status = &frontendv1.RosterRow_NetworkFault{NetworkFault: &frontendv1.RosterRowStatusNetworkFault{}}
	case "api_retrying":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"api_retrying\""})
		row.Status = &frontendv1.RosterRow_ApiRetrying{ApiRetrying: &frontendv1.RosterRowStatusApiRetrying{}}
	case "init":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"init\""})
		row.Status = &frontendv1.RosterRow_Init{Init: &frontendv1.RosterRowStatusInit{}}
	case "severed":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"severed\""})
		row.Status = &frontendv1.RosterRow_Severed{Severed: &frontendv1.RosterRowStatusSevered{}}
	case "start_failed":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"start_failed\""})
		row.Status = &frontendv1.RosterRow_StartFailed{StartFailed: &frontendv1.RosterRowStatusStartFailed{}}
	case "degraded":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"degraded\""})
		row.Status = &frontendv1.RosterRow_Degraded{Degraded: &frontendv1.RosterRowStatusDegraded{}}
	case "turn_died":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"turn_died\""})
		row.Status = &frontendv1.RosterRow_TurnDied{TurnDied: &frontendv1.RosterRowStatusTurnDied{}}
	case "dead":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"dead\""})
		row.Status = &frontendv1.RosterRow_Dead{Dead: &frontendv1.RosterRowStatusDead{}}
	case "merging":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merging\""})
		row.Status = &frontendv1.RosterRow_Merging{Merging: &frontendv1.RosterRowStatusMerging{}}
	case "merge_queued":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merge_queued\""})
		row.Status = &frontendv1.RosterRow_MergeQueued{MergeQueued: &frontendv1.RosterRowStatusMergeQueued{}}
	case "merge_failed":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merge_failed\""})
		row.Status = &frontendv1.RosterRow_MergeFailed{MergeFailed: &frontendv1.RosterRowStatusMergeFailed{}}
	case "merged":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merged\""})
		row.Status = &frontendv1.RosterRow_Merged{Merged: &frontendv1.RosterRowStatusMerged{}}
	case "none":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"none\""})
		row.Status = &frontendv1.RosterRow_None{None: &frontendv1.RosterRowStatusNone{}}
	case "closing":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"closing\""})
		row.Status = &frontendv1.RosterRow_Closing{Closing: &frontendv1.RosterRowStatusClosing{}}
	case "daemon_impaired":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"daemon_impaired\""})
		row.Status = &frontendv1.RosterRow_DaemonImpaired{DaemonImpaired: &frontendv1.RosterRowStatusDaemonImpaired{}}
	case "waiting":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"waiting\""})
		row.Status = &frontendv1.RosterRow_Waiting{Waiting: &frontendv1.RosterRowStatusWaiting{}}
	case "inactive":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"inactive\""})
		row.Status = &frontendv1.RosterRow_Inactive{Inactive: &frontendv1.RosterRowStatusInactive{}}
	default:
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "default"})
		log.Error("daemon.sidebar.set_status", "the roster resolved a status name with no arm behind it",
			dlog.Context{
				"arm":                 arm,
				"invariant_violation": "a row would carry an unset status oneof",
				"remediation":         "add the arm to setStatus and to statusArms",
			})
		row.Status = &frontendv1.RosterRow_None{None: &frontendv1.RosterRowStatusNone{}}
	}
}

package sidebar

import (
	"fmt"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/protoarm"
	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/wsm"
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
// ONE ARM SITS ABOVE THE PROJECTION: `inactive`, a workspace INCIDENTALLY
// inactive (owner ruling, 2026-10-13). It is drawn while an OpenTabs names the
// workspace and has not answered it `opened` yet — the row also carries its
// `opening` marker then, and takes no click, whatever its session or its
// registry record says — and for a registered workspace with no open
// perspective and nothing live behind it. Only an open in flight raises the
// marker, so a daemon that restarted under open tabs draws them as they are.
// It dominates every session state, and no footer is ever drawn for it.
//
// A SECOND ARM SITS ABOVE IT FOR THE SAME REASON: `cold_gate`, for a HIDDEN
// workspace whose cache has lapsed with a gated verdict (cachelapse.Keeper,
// 2026-10-13). Its session was stood down at the lapse, and showing it again
// will raise this gate, so its name is struck through the moment the lapse
// comes. No footer is drawn for a hidden workspace, and the gate it projects
// is raised by the start that shows it again, which then makes the claim the
// footer draws.
//
// WITHIN A CLAIM the roster draws the footer's step as its own arm
// (`projectArm`), except on the idle rung, whose detail is the roster's own
// ruling (`idleArm`): live detached work draws `idle_async` over an unread
// turn end, and a read turn end is drawn partial. An async merge of the
// workspace's own branch is NO ARM: it is the row's `async_merging` marker
// beside whatever arm this resolves (`asyncMergingMarker`).
func statusArm(s *wsState, rec wsm.Workspace, session *wsm.Session, status *frontendv1.FooterStatus, now time.Time, log dlog.Logger) string {
	if s.opening || !s.live(session) && rec.Closed {
		return "inactive"
	}
	if hiddenColdGate(rec, now) {
		log.Debug("daemon.sidebar.status_decision", "the hidden workspace's cache lapsed with a gated verdict", dlog.Context{
			"lapses_at": rec.CacheLapse.LapsesAt, "gated_tokens": *rec.CacheLapse.GatedTokens,
		})
		return "cold_gate"
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
	arm := idleArm(s, session, status, log)
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

// hiddenColdGate reports whether a hidden workspace's cache has lapsed with a
// gated verdict at NOW.
func hiddenColdGate(rec wsm.Workspace, now time.Time) bool {
	lapse := rec.CacheLapse
	return rec.InHiddenSection() && lapse != nil && lapse.GatedTokens != nil && lapse.Lapsed(now)
}

// projectArm names the roster arm that draws the footer's status on every rung
// above idle. Each footer step maps onto exactly one roster arm making the same
// claim; a step this function does not name answers empty, which statusArm
// reports as a broken projection.
func projectArm(status *frontendv1.FooterStatus, log dlog.Logger) string {
	switch arm := status.GetStatus().(type) {
	case *frontendv1.FooterStatus_Merging:
		// A MERGE THAT HAS NOT STARTED is `merge_queued`, whether it waits in
		// line or for the workspace to fall free (owner ruling, 2026-10-13).
		if arm.Merging.GetEnqueued() != nil || arm.Merging.GetAwaitingFree() != nil {
			return "merge_queued"
		}
		return "merging"
	case *frontendv1.FooterStatus_MergeFailed:
		return "merge_failed"
	case *frontendv1.FooterStatus_Merged:
		return "merged"
	case *frontendv1.FooterStatus_AgentReplFault:
		return agentReplFaultArm(arm.AgentReplFault)
	case *frontendv1.FooterStatus_Starting:
		return "init"
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
		if arm.Waiting.GetColdGate() != nil {
			return "cold_gate"
		}
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
	arm, ok := protoarm.Name(status.ProtoReflect(), "status")
	if !ok {
		return "unset"
	}
	return string(arm)
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

// parked reports whether a session record carries a park: the idle sweep's,
// or the keep-alive cycle's (wsm.Session.Parked).
func parked(session *wsm.Session) bool {
	return session != nil && session.Parked()
}

// idleArm names the bottom rung's arm. It always answers: every path ends at
// `ready`.
//
// `idle_async` IS THE BACKGROUND: detached work running, or correspondence
// awaited from another workspace, while no turn runs. Detached liveness is
// read from the watcher's live-work set, the one authority the footer reads
// too (see livework.go). Awaited correspondence is PROJECTED from the
// footer's background arm, so the roster holds no awaiting fact of its own
// and the two surfaces color an awaiting workspace alike by construction.
//
// THE BACKGROUND OUTRANKS AN UNREAD TURN END: a turn that ends with
// background work still running draws `idle_async` at once, without waiting
// for the user to read the result first. The result stays unread under it, so
// the row returns FULL on its turn-end arm when the background ends, unless
// the user read it meanwhile (SetViewed).
func idleArm(s *wsState, session *wsm.Session, status *frontendv1.FooterStatus, log dlog.Logger) string {
	awaited := status.GetBackground().GetAwaiting()
	switch {
	case noSessionArm(s, session) != "":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "no session was ever created"})
		return "none"
	case s.asyncLive() || awaited != nil:
		if s.resultUnreadNow() {
			log.Debug("daemon.sidebar.async_outranks_unread",
				"the workspace is in the background, so the row is idle_async at once though the last turn's result is unread",
				dlog.Context{"held": s.turnEndArm()})
		}
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{
			"function": "status", "branch": "the workspace is in the background",
			"async_live": s.asyncLive(), "awaiting": awaited.GetAwaitedName(),
		})
		return "idle_async"
	case s.resultUnreadNow():
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.resultUnreadNow()"})
		return s.turnEndArm()
	case s.turnEverRan && !s.endAnswered:
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.turnEverRan", "close": int(s.lastClose)})
		return s.turnEndArm()
	default:
		log.Debug("daemon.sidebar.status", "the session is live and idle", nil)
		return "ready"
	}
}

// asyncMergingMarker answers the row's `async_merging` marker: present
// exactly while the workspace's own branch has an async merge on a
// repository's merge queue, on ANY status arm (owner request, 2026-10-13).
// The arm keeps resolving as for any free workspace, so the row's name keeps
// its status color; the marker is the one extra fact the surfaces draw.
func asyncMergingMarker(asyncMerging bool, arm string, log dlog.Logger) *frontendv1.RosterRowAsyncMerging {
	if !asyncMerging {
		return nil
	}
	log.Debug("daemon.sidebar.async_merging_marker",
		"the workspace's own branch merges async, so the row carries the async_merging marker beside its status",
		dlog.Context{"status": arm})
	return &frontendv1.RosterRowAsyncMerging{}
}

// idleAsyncUnreadMarker answers the row's `idle_async_unread` marker: present
// exactly while the row is `idle_async` over a turn end whose result is unread
// (`frontend.v1.RosterRowIdleAsyncUnread`), naming that turn end by the arm it
// draws once the background ends (`turnEndArm`). The arm stays `idle_async`;
// the marker is the one extra fact the surfaces draw, inking the name in the
// turn end's color while the dot or `[N]` keeps the background's yellow.
func idleAsyncUnreadMarker(s *wsState, arm string, log dlog.Logger) *frontendv1.RosterRowIdleAsyncUnread {
	if arm != "idle_async" || !s.resultUnreadNow() {
		return nil
	}
	end := s.turnEndArm()
	marker := &frontendv1.RosterRowIdleAsyncUnread{}
	switch end {
	case armDone:
		marker.TurnEnd = &frontendv1.RosterRowIdleAsyncUnread_Done{Done: &frontendv1.RosterRowStatusDone{}}
	case armInterrupted:
		marker.TurnEnd = &frontendv1.RosterRowIdleAsyncUnread_Interrupted{Interrupted: &frontendv1.RosterRowStatusInterrupted{}}
	case armTurnFailed:
		marker.TurnEnd = &frontendv1.RosterRowIdleAsyncUnread_TurnFailed{TurnFailed: &frontendv1.RosterRowStatusTurnFailed{}}
	default:
		// turnEndArm answers one of the three turn-end arms by construction.
		log.Error("daemon.sidebar.idle_async_unread_marker",
			"the unread result under the idle_async row names no turn-end arm, which turnEndArm guarantees",
			dlog.Context{"turn_end": end})
		panic(fmt.Sprintf("sidebar: turnEndArm answered %q, which is not a turn-end arm", end))
	}
	log.Debug("daemon.sidebar.idle_async_unread_marker",
		"the row is idle_async over an unread turn end, so it carries the idle_async_unread marker naming it",
		dlog.Context{"turn_end": end})
	return marker
}

// mergePausedMarker answers the row's `merge_paused` marker: present exactly
// while a merge indicator the row draws -- its own `merging` or
// `merge_queued` arm, or its async_merging marker (ASYNC) -- is on a paused
// repository queue (owner ruling, 2026-10-14). The arm and its color stand;
// the marker changes only how the merge indicator is drawn.
func mergePausedMarker(state *rosterState, ws ids.WorkspaceID, arm string, async bool, log dlog.Logger) *frontendv1.RosterRowMergePaused {
	repo, paused := state.pausedMergeQueue(ws, arm, async)
	if !paused {
		return nil
	}
	log.Debug("daemon.sidebar.merge_paused_marker",
		"the merge the row draws is on a paused queue, so the row carries the merge_paused marker beside its merge indicator",
		dlog.Context{"status": arm, "async_merging": async, "repo": string(repo)})
	return &frontendv1.RosterRowMergePaused{}
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
	case "cold_gate":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"cold_gate\""})
		row.Status = &frontendv1.RosterRow_ColdGate{ColdGate: &frontendv1.RosterRowStatusColdGate{}}
	case "inactive":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"inactive\""})
		row.Status = &frontendv1.RosterRow_Inactive{Inactive: &frontendv1.RosterRowStatusInactive{}}
	default:
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "default"})
		log.Error("daemon.sidebar.set_status", "the roster resolved a status name with no arm behind it",
			dlog.Context{
				"arm":                 arm,
				"invariant_violation": "a row would carry an unset status oneof",
				"remediation":         "add the arm to setStatus",
			})
		row.Status = &frontendv1.RosterRow_None{None: &frontendv1.RosterRowStatusNone{}}
	}
}

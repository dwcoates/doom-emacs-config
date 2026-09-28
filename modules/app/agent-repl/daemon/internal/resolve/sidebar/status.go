package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// statusArm resolves the row's status arm.
//
// THE PRECEDENCE IS NOT STATED HERE. It is resolve/ladder's, the one ladder
// the footer strip is projected from too, so the rail, the tab bar and the
// strip cannot make different coarse claims about one workspace (owner
// ruling, 2026-09-28). This resolver only draws each rung it can claim
// (`rosterRung`) and the idle family at the bottom (`idleArm`), and asserts
// that what it drew projects back onto the claim the ladder chose.
//
// ONE ARM SITS ABOVE THE LADDER: `inactive`, a registered workspace with no
// open perspective and nothing live behind it. It dominates every session
// state, and no footer is ever drawn for it.
//
// WITHIN a rung the order is this surface's detail:
//   - merging: enqueuing, queued and merging are the one rung;
//   - disconnected: start_failed, dead, severed, init, degraded (linkArm);
//   - thinking: clearing, then compacting, then submitting, then thinking;
//   - idle: none (no session was ever created), then the last turn's end
//     while its result is UNREAD (done, interrupted or turn_failed — it
//     outranks idle_async: a result is waiting, and detached work running
//     must not hide that, until the editor reports the row viewed or a new
//     prompt starts a turn), then idle_async (detached work running NOW
//     outranks how an already-read turn ended), then the read turn end (drawn
//     PARTIAL), then ready.
func statusArm(s *wsState, rec wsm.Workspace, session *wsm.Session, log dlog.Logger) string {
	if !s.live(session) && rec.Closed {
		return "inactive"
	}
	// A PARKED SESSION IS IDLE, NOT BROKEN. The idle sweep stands the shim
	// down deliberately and a prompt brings it straight back, so the ladder
	// skips the link rung: nothing about a hibernation is visible to the user
	// beyond the wait for the revival, and drawing `severed` or `dead` would
	// report a fault where there is none.
	isParked := parked(session)
	if isParked {
		log.Debug("daemon.sidebar.status", "the session is parked by the idle sweep", nil)
	}
	claim, arm := ladder.Resolve(s.merge.State, isParked,
		func(claim ladder.Claim) (string, bool) {
			drawn := rosterRung(claim, s, session, log)
			return drawn, drawn != ""
		},
		func() string { return idleArm(s, session, log) })
	if drawn, ok := ladder.RosterArmClaim(arm); !ok || drawn != claim {
		log.Error("daemon.sidebar.status_claim",
			"the roster drew a status arm that does not project onto the ladder claim it resolved",
			dlog.Context{
				"claim":               string(claim),
				"drawn":               string(drawn),
				"arm":                 arm,
				"invariant_violation": "the roster and the footer must make the same coarse claim",
				"remediation":         "draw the rung's own arm, or place the arm in ladder.RosterArmClaim",
			})
	}
	return arm
}

// rosterRung draws one ladder rung, empty when this workspace's facts make no
// claim there.
func rosterRung(claim ladder.Claim, s *wsState, session *wsm.Session, log dlog.Logger) string {
	switch claim {
	case ladder.Merging:
		return mergeArm(s.merge)
	case ladder.MergeConflict:
		return "merge_conflict"
	case ladder.MergeFailed:
		return "merge_failed"
	case ladder.Merged:
		return "merged"
	case ladder.Disconnected:
		if ladder.AwaitingBringUp(s.linkSeen, s.turn != nil, s.started) {
			log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "an accepted turn awaits the bring-up"})
			return "init"
		}
		if noSessionArm(s, session) != "" {
			// No session was ever created, so there is no route to be down.
			return ""
		}
		return linkArm(s, session)
	case ladder.Closing:
		// The roster observes no close refusal; only the footer claims it.
		return ""
	case ladder.Blocked:
		if s.vendorBlocked {
			log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.vendorBlocked"})
			return "vendor_blocked"
		}
		return ""
	case ladder.Waiting:
		if len(s.permissions) > 0 {
			log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case len(s.permissions) > 0"})
			return "permission"
		}
		return ""
	case ladder.Thinking:
		return thinkingArm(s, log)
	default:
		log.Error("daemon.sidebar.status_rung", "the ladder asked the roster for a rung it has no drawing for",
			dlog.Context{
				"claim":               string(claim),
				"invariant_violation": "every ladder rung above idle has a roster drawing",
				"remediation":         "add the rung to rosterRung",
			})
		return ""
	}
}

// mergeArm names a merge IN FLIGHT's arm. The ladder calls it only when the
// merge state stands on the merging rung; a merge state this build does not
// name is one the orchestrator reports, so it draws `merging` (ladder.MergeClaim).
func mergeArm(facts footer.MergeFacts) string {
	switch facts.State {
	case "enqueuing":
		return "merge_enqueuing"
	case "queued":
		return "merge_queued"
	default:
		return "merging"
	}
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

// linkArm names the route's arm, empty when the route serves undegraded. It
// mirrors the footer's disconnected step, fact for fact, so the dot and the
// strip cannot disagree about the same link.
//
// A ROUTE NOBODY HAS SEEN IS NOT A ROUTE THAT SERVES. A workspace whose
// session record exists while no link state has yet been observed has proven
// nothing: the shim is being spawned and dialed, and that window is `init`.
// Reporting `ready` there published the leading `ready` the editor saw —
// none -> ready -> init -> ... — `ready` before the `init` it is supposed to
// follow, and `ready` for a workspace whose prompt the daemon had already
// accepted. `init` is what that pre-link state actually is, and saying so
// keeps the cold-start walk monotone with the workspace's own lifecycle.
//
// A CONNECTED route, on the other hand, is proven and is NOT a link fault, so
// once the link connects the row leaves the `init`/link band even before a
// SessionStarted lands — see the default arm in the switch. Blue is reserved
// for a compromised route, and a connected route is not one.
//
// A session that ENDED is the one case that still falls through: its route is
// not coming up, there is nothing to wait on, and the lifecycle arms below are
// what report how it ended.
func linkArm(s *wsState, session *wsm.Session) string {
	if !s.linkSeen {
		if session != nil && session.Terminal != nil {
			return ""
		}
		return "init"
	}
	switch {
	case s.link == shimclient.LinkDialing:
		return "init"
	case s.link == shimclient.LinkRedialing:
		return "severed"
	case s.link == shimclient.LinkDead && s.everConnected:
		return "dead"
	case s.link == shimclient.LinkDead:
		return "start_failed"
	case s.degraded:
		return "degraded"
	default:
		// A CONNECTED route is proven, so the row is NOT a link fault. `init`
		// belongs to a route still being established — dialing, or a session
		// record with no link seen yet — and once the link connects the
		// route is up whether or not a SessionStarted has landed on this
		// resolver's view.
		//
		// The `init` window used to extend past the connect, held open by
		// `!s.started`, on the theory that the route is not "proven usable"
		// until the shim announces its session. That reported `init` — a
		// BLUE, link-fault color — for a workspace whose route was up and
		// whose webapp was idle, and it never cleared on a RESUME: a daemon
		// reconnect replays the link (OnLink -> Connected) but not the
		// one-shot SessionStarted, so `s.started` stayed false forever and
		// the tab stayed blue on a healthy, idle session.
		//
		// Blue means the route is compromised, and a connected route is not.
		// A connected-but-not-yet-announced session falls through to
		// `sessionArm`, which reports the lifecycle it actually has
		// (`ready` for an idle one). The published cold-start walk stays
		// monotone because `init` is still what the pre-connect dialing
		// phase reports.
		return ""
	}
}

// thinkingArm names a turn in flight's arm, empty when none is.
func thinkingArm(s *wsState, log dlog.Logger) string {
	switch {
	case s.turn != nil && s.turn.Act == footer.ActClear:
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.turn != nil && s.turn.Act == footer.ActClear"})
		return "clearing"
	case s.compacting:
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.compacting"})
		return "compacting"
	case s.turn != nil && !s.sawActivity:
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.turn != nil && !s.sawActivity"})
		return "submitting"
	case s.turn != nil:
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case s.turn != nil"})
		return "thinking"
	default:
		return ""
	}
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
	case "dead":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"dead\""})
		row.Status = &frontendv1.RosterRow_Dead{Dead: &frontendv1.RosterRowStatusDead{}}
	case "merge_enqueuing":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merge_enqueuing\""})
		row.Status = &frontendv1.RosterRow_MergeEnqueuing{MergeEnqueuing: &frontendv1.RosterRowStatusMergeEnqueuing{}}
	case "merging":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merging\""})
		row.Status = &frontendv1.RosterRow_Merging{Merging: &frontendv1.RosterRowStatusMerging{}}
	case "merge_queued":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merge_queued\""})
		row.Status = &frontendv1.RosterRow_MergeQueued{MergeQueued: &frontendv1.RosterRowStatusMergeQueued{}}
	case "merge_conflict":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merge_conflict\""})
		row.Status = &frontendv1.RosterRow_MergeConflict{MergeConflict: &frontendv1.RosterRowStatusMergeConflict{}}
	case "merge_failed":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merge_failed\""})
		row.Status = &frontendv1.RosterRow_MergeFailed{MergeFailed: &frontendv1.RosterRowStatusMergeFailed{}}
	case "merged":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"merged\""})
		row.Status = &frontendv1.RosterRow_Merged{Merged: &frontendv1.RosterRowStatusMerged{}}
	case "none":
		log.Debug("daemon.sidebar.status_decision", "selected a roster status branch", dlog.Context{"function": "status", "branch": "case \"none\""})
		row.Status = &frontendv1.RosterRow_None{None: &frontendv1.RosterRowStatusNone{}}
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

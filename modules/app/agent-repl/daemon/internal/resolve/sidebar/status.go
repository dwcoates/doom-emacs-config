package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// THE STATUS PRECEDENCE, strongest claim first. Several facts hold at once
// routinely — a merging workspace still has a session, a severed link still
// has a last turn — so the order below is the contract's answer to which one
// the dot reports, and it is stated ONCE here.
//
//  1. inactive        no open perspective and nothing live behind it.
//     DOMINATES every session state: a perspective-less
//     workspace is inactive whatever its session once was.
//  2. merge_*         the merge pipeline owns the row while it runs
//     (enqueuing, queued, merging, conflict, failed, merged).
//  3. none            no session has ever existed, so there is no lifecycle
//     to report — an assertion, not an absent oneof.
//  4. link states     start_failed, dead, severed, init, degraded. The route
//     is what every session state below is reported OVER, so
//     a broken route outranks them all.
//  5. vendor_blocked  the block is the vendor's or the account's.
//  6. permission      a gated call is waiting on the user.
//  7. clearing        a context cut is running…
//  8. compacting      …either kind.
//  9. submitting      the turn is accepted and the shim has not acked it.
//  10. thinking        the turn is producing activity.
//  11. idle_async      detached work runs while the foreground is free. It
//     outranks the two turn terminals below because work
//     happening NOW outranks how the last turn ended.
//  12. interrupted     the last turn was stopped by the user.
//  13. done            the last turn finished and its response is unread.
//  14. ready           live, proven usable and idle.
func statusArm(s *wsState, rec wsm.Workspace, session *wsm.Session, log dlog.Logger) string {
	if !s.live(session) && rec.Closed {
		return "inactive"
	}
	// A PARKED SESSION IS IDLE, NOT BROKEN. The idle sweep stands the shim
	// down deliberately and a prompt brings it straight back, so the row keeps
	// an IDLE arm: nothing about a hibernation is visible to the user beyond
	// the wait for the revival, and drawing `severed` or `dead` would report a
	// fault where there is none. The link arm is skipped for exactly that
	// reason — the route is down because the daemon put it down.
	if parked(session) {
		log.Debug("daemon.sidebar.status", "the session is parked by the idle sweep", nil)
		return sessionArm(s, log)
	}
	if arm := mergeArm(s.merge); arm != "" {
		return arm
	}
	if arm := noSessionArm(s, session); arm != "" {
		return arm
	}
	if arm := linkArm(s, session); arm != "" {
		return arm
	}
	return sessionArm(s, log)
}

// mergeArm names the merge pipeline's arm, empty when no merge stands. "none"
// and the empty state both mean the orchestrator has said nothing.
func mergeArm(facts footer.MergeFacts) string {
	switch facts.State {
	case "enqueuing":
		return "merge_enqueuing"
	case "queued":
		return "merge_queued"
	case "merging":
		return "merging"
	case "conflict", "parked":
		// A PARKED merge is one stopped awaiting the user's resolution, which
		// is exactly what merge_conflict spells ("the merge stopped on a
		// conflict awaiting resolution"). The roster has no parked arm of its
		// own, and a parked merge must never fall through to the session's
		// status: the row would then read `ready` for a workspace whose merge
		// is holding its lease.
		return "merge_conflict"
	case "failed":
		return "merge_failed"
	case "merged":
		return "merged"
	default:
		return ""
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
// A ROUTE NOBODY HAS SEEN IS NOT A ROUTE THAT SERVES. `ready` means "live,
// PROVEN USABLE, and idle" (sidebar.proto), and a workspace whose session
// record exists while no link state has yet been observed has proven nothing:
// the shim is being spawned and dialed. Falling through to `sessionArm` there
// published `ready` for it, which is how the roster's arm walked
// none -> ready -> init -> submitting -> thinking — `ready` BEFORE the `init`
// it is supposed to follow, and `ready` for a workspace whose prompt the
// daemon had already accepted (measured at ~340ms after SubmitPrompt answered
// with its turn id). `init` — "starting up; the route is not yet proven" — is
// what that state actually is, and saying so makes the published walk monotone
// with the workspace's own lifecycle.
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
	case !s.started:
		// The route is up but the session has not announced itself, so it is
		// still coming up — which is exactly what `init` says.
		return "init"
	default:
		return ""
	}
}

// sessionArm names the lifecycle of a live, proven session. It is the last
// step, so it always answers: every path below ends at `ready`.
func sessionArm(s *wsState, log dlog.Logger) string {
	switch {
	case s.vendorBlocked:
		return "vendor_blocked"
	case len(s.permissions) > 0:
		return "permission"
	case s.turn != nil && s.turn.Act == footer.ActClear:
		return "clearing"
	case s.compacting:
		return "compacting"
	case s.turn != nil && !s.sawActivity:
		return "submitting"
	case s.turn != nil:
		return "thinking"
	case s.asyncLive():
		return "idle_async"
	case s.turnEverRan && s.lastClose == wsm.CloseKilled:
		return "interrupted"
	case s.turnEverRan:
		return "done"
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
		row.Status = &frontendv1.RosterRow_Submitting{Submitting: &frontendv1.RosterRowStatusSubmitting{}}
	case "thinking":
		row.Status = &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}}
	case "clearing":
		row.Status = &frontendv1.RosterRow_Clearing{Clearing: &frontendv1.RosterRowStatusClearing{}}
	case "compacting":
		row.Status = &frontendv1.RosterRow_Compacting{Compacting: &frontendv1.RosterRowStatusCompacting{}}
	case "permission":
		row.Status = &frontendv1.RosterRow_Permission{Permission: &frontendv1.RosterRowStatusPermission{}}
	case "done":
		row.Status = &frontendv1.RosterRow_Done{Done: &frontendv1.RosterRowStatusDone{}}
	case "interrupted":
		row.Status = &frontendv1.RosterRow_Interrupted{Interrupted: &frontendv1.RosterRowStatusInterrupted{}}
	case "ready":
		row.Status = &frontendv1.RosterRow_Ready{Ready: &frontendv1.RosterRowStatusReady{}}
	case "idle_async":
		row.Status = &frontendv1.RosterRow_IdleAsync{IdleAsync: &frontendv1.RosterRowStatusIdleAsync{}}
	case "vendor_blocked":
		row.Status = &frontendv1.RosterRow_VendorBlocked{VendorBlocked: &frontendv1.RosterRowStatusVendorBlocked{}}
	case "init":
		row.Status = &frontendv1.RosterRow_Init{Init: &frontendv1.RosterRowStatusInit{}}
	case "severed":
		row.Status = &frontendv1.RosterRow_Severed{Severed: &frontendv1.RosterRowStatusSevered{}}
	case "start_failed":
		row.Status = &frontendv1.RosterRow_StartFailed{StartFailed: &frontendv1.RosterRowStatusStartFailed{}}
	case "degraded":
		row.Status = &frontendv1.RosterRow_Degraded{Degraded: &frontendv1.RosterRowStatusDegraded{}}
	case "dead":
		row.Status = &frontendv1.RosterRow_Dead{Dead: &frontendv1.RosterRowStatusDead{}}
	case "merge_enqueuing":
		row.Status = &frontendv1.RosterRow_MergeEnqueuing{MergeEnqueuing: &frontendv1.RosterRowStatusMergeEnqueuing{}}
	case "merging":
		row.Status = &frontendv1.RosterRow_Merging{Merging: &frontendv1.RosterRowStatusMerging{}}
	case "merge_queued":
		row.Status = &frontendv1.RosterRow_MergeQueued{MergeQueued: &frontendv1.RosterRowStatusMergeQueued{}}
	case "merge_conflict":
		row.Status = &frontendv1.RosterRow_MergeConflict{MergeConflict: &frontendv1.RosterRowStatusMergeConflict{}}
	case "merge_failed":
		row.Status = &frontendv1.RosterRow_MergeFailed{MergeFailed: &frontendv1.RosterRowStatusMergeFailed{}}
	case "merged":
		row.Status = &frontendv1.RosterRow_Merged{Merged: &frontendv1.RosterRowStatusMerged{}}
	case "none":
		row.Status = &frontendv1.RosterRow_None{None: &frontendv1.RosterRowStatusNone{}}
	case "inactive":
		row.Status = &frontendv1.RosterRow_Inactive{Inactive: &frontendv1.RosterRowStatusInactive{}}
	default:
		log.Error("daemon.sidebar.set_status", "the roster resolved a status name with no arm behind it",
			dlog.Context{
				"arm":                 arm,
				"invariant_violation": "a row would carry an unset status oneof",
				"remediation":         "add the arm to setStatus and to statusArms",
			})
		row.Status = &frontendv1.RosterRow_None{None: &frontendv1.RosterRowStatusNone{}}
	}
}

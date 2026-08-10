package sessioncontroller

import (
	"fmt"

	"claude-repld/internal/shim"
)

// stopcause.go — THE CLOSED VOCABULARY OF SHIM STOPS.
//
// Every deliberate shim stop this daemon issues names ITSELF, and it names
// itself from here. A StopCause is minted only by the constructors below, so
// the set of answers to "why did this shim stop" is enumerable by reading this
// file rather than by grepping every teardown for a string literal it happened
// to pass.
//
// # One table, three renderings
//
// A cause renders THREE things and they all come from `stopCauseTable`:
//
//   - the FUNNEL PATH token, which travels to the SSM's stale-turn close and
//     into the `SHIM STOP ENTRY` line;
//   - the shim.Stop INITIATOR, the component the stop is attributed to; and
//   - the shim.Stop REASON, the actionable statement of why.
//
// Having them in one table is the point: a cause cannot log one story in the
// daemon log and hand a different one to the process it kills, because there is
// only one place either could come from.
//
// # The path tokens are a SEAM, not a label
//
// The tokens below are the exact strings these call sites passed before the
// vocabulary existed. Other code — the drain-lease provenance formatter and the
// SSM's stale-turn close among them — reads or records them, so a cause
// replacing an existing call site renders that call site's token unchanged. New
// causes may mint new tokens; existing ones may not drift.
//
// # The zero value is invalid ON PURPOSE
//
// `StopCause{}` is constructible outside this package (the fields are
// unexported, but an empty composite literal is not). It is REFUSED rather than
// defaulted: `shim.Stop.Validate` rejects the empty attribution it renders, the
// funnel refuses it before touching the spawner, and `ShimSpawner.StopShim`
// refuses it again at the boundary. An unattributed stop is exactly the failure
// this vocabulary exists to make impossible, so no layer papers over it.

// stopCauseID enumerates the closed vocabulary. It is unexported and carries no
// wire or durable meaning: the rendered token and attribution are the contract.
type stopCauseID int

const (
	// causeUnset is the zero value: NOT a cause, and never rendered.
	causeUnset stopCauseID = iota
	causeHibernateIdleSweep
	causeHibernateForced
	causeHibernateCacheExpired
	causeMergedTeardown
	causeHardRestartLive
	causeHardRestartOrphan
	causeDrainExecution
	causeDaemonShutdown
	causeSessionDeleted
	causeSessionSuperseded
	causeAccountSwitch
	causeBringUpFailed
	causeControllerExit
	// causeSupersededRecord is INTERNAL: a session-scoped stop that reached a
	// record a different live session has since replaced. It refines whichever
	// cause the caller supplied rather than replacing it, so the record still
	// names who asked (see StopCause.origin).
	causeSupersededRecord
)

// stopCauseRendering is one cause's complete rendering. Every field is
// required; a cause with a blank one cannot be constructed, because the table
// below is the only source and it is asserted at init.
type stopCauseRendering struct {
	// path is the funnel token, unchanged from the string the call site used
	// before the vocabulary existed (see the seam note above).
	path string
	// initiator is the shim.Stop.Initiator: the component that commanded it.
	initiator string
	// reason is the shim.Stop.Reason: why, in a form a log reader can act on.
	reason string
}

// stopCauseTable is THE table. Path token, initiator and reason for every cause
// live in one row so no two of them can drift.
var stopCauseTable = map[stopCauseID]stopCauseRendering{
	causeHibernateIdleSweep: {
		path:      "hibernate",
		initiator: "idle_sweep",
		reason:    "the workspace was quiet past the idle timeout, so the daemon hibernated its session",
	},
	causeHibernateForced: {
		path:      "hibernate",
		initiator: "user_hibernate",
		reason:    "the user forced hibernation, so the daemon stopped the session's shim and gated it behind a revival choice",
	},
	causeHibernateCacheExpired: {
		path:      "hibernate",
		initiator: "cache_expired",
		reason:    "the prompt cache went cold before a keep-alive ping could fire, so the daemon hibernated instead of paying a full re-ingest for nobody",
	},
	causeMergedTeardown: {
		path:      "hibernate",
		initiator: "merged_teardown",
		reason:    "the workspace merged, so its session was stood down and its shim's memory reclaimed",
	},
	causeHardRestartLive: {
		path:      "hibernate",
		initiator: "hard_restart",
		reason:    "an explicit session restart stopped the live shim before respawning the same conversation",
	},
	causeHardRestartOrphan: {
		path:      "restart_session_orphan",
		initiator: "hard_restart",
		reason:    "an explicit session restart stopped an orphaned shim no session controller was driving",
	},
	causeDrainExecution: {
		path:      "hibernate",
		initiator: "scheduled_shutdown",
		reason:    "a scheduled shutdown finished draining and stops every session shim on the way out",
	},
	causeDaemonShutdown: {
		path:      "hibernate",
		initiator: "daemon_shutdown",
		reason:    "the daemon is shutting down with stop_shims set, so the shim bundle can be replaced",
	},
	causeSessionDeleted: {
		path:      "hibernate_session",
		initiator: "session_delete",
		reason:    "the session record was deleted, so the shim serving it was stopped",
	},
	causeSessionSuperseded: {
		path:      "hibernate_session",
		initiator: "session_supersede",
		reason:    "a replacement session took the workspace, so the superseded record's shim was stopped",
	},
	causeAccountSwitch: {
		path:      "hibernate",
		initiator: "account_switch",
		reason:    "the session's account root changed, so its shim was stopped before relaunching under the new root",
	},
	causeBringUpFailed: {
		path:      "bringup_failed",
		initiator: "bringup_failure",
		reason:    "the bring-up never wired, so the shim it spawned was stopped rather than left racing the retry",
	},
	causeControllerExit: {
		path:      "session_controller_exit",
		initiator: "session_controller_exit",
		reason:    "the session controller's run loop ended unexpectedly, so the shim it owned was stopped",
	},
	causeSupersededRecord: {
		path:      "hibernate_session_superseded",
		initiator: "session_stop_superseded_record",
		reason:    "the stop named a record a different live session has since replaced, so only that record's shim was stopped",
	},
}

func init() {
	for id, rendering := range stopCauseTable {
		if rendering.path == "" || rendering.initiator == "" || rendering.reason == "" {
			panic(fmt.Sprintf("sessioncontroller: stop cause %d is incompletely rendered: %+v", id, rendering))
		}
	}
}

// StopCause names why a shim is being stopped. It is opaque by construction:
// the fields are unexported and the constructors below are the only way to mint
// a usable value, so the vocabulary cannot be extended from outside this
// package and a call site cannot invent an ad-hoc reason string.
type StopCause struct {
	id stopCauseID
	// origin retains the CALLER's cause when a branch refined it, so a refined
	// record still answers "who asked" as well as "what the daemon found".
	origin stopCauseID
}

// StopCauseHibernateIdleSweep — the idle sweeper reaped a quiet workspace.
func StopCauseHibernateIdleSweep() StopCause { return StopCause{id: causeHibernateIdleSweep} }

// StopCauseHibernateForced — the user forced hibernation (HibernateWorkspaceCmd).
func StopCauseHibernateForced() StopCause { return StopCause{id: causeHibernateForced} }

// StopCauseHibernateCacheExpired — the keep-alive loop discovered the prompt
// cache had already gone cold, so the discovery became the hibernation.
func StopCauseHibernateCacheExpired() StopCause { return StopCause{id: causeHibernateCacheExpired} }

// StopCauseMergedTeardown — the workspace merged and its session stood down.
func StopCauseMergedTeardown() StopCause { return StopCause{id: causeMergedTeardown} }

// StopCauseHardRestartLive — an explicit restart stopped the LIVE shim.
func StopCauseHardRestartLive() StopCause { return StopCause{id: causeHardRestartLive} }

// StopCauseHardRestartOrphan — an explicit restart stopped an ORPHANED shim no
// session controller was driving.
func StopCauseHardRestartOrphan() StopCause { return StopCause{id: causeHardRestartOrphan} }

// StopCauseDrainExecution — a scheduled shutdown's drain completed and the
// bounce it was taken for is executing.
func StopCauseDrainExecution() StopCause { return StopCause{id: causeDrainExecution} }

// StopCauseDaemonShutdown — an ordinary daemon shutdown in stop-shims mode.
func StopCauseDaemonShutdown() StopCause { return StopCause{id: causeDaemonShutdown} }

// StopCauseSessionDeleted — the session record was deleted.
func StopCauseSessionDeleted() StopCause { return StopCause{id: causeSessionDeleted} }

// StopCauseSessionSuperseded — a replacement session took the workspace.
func StopCauseSessionSuperseded() StopCause { return StopCause{id: causeSessionSuperseded} }

// StopCauseAccountSwitch — the session's account root changed under it.
func StopCauseAccountSwitch() StopCause { return StopCause{id: causeAccountSwitch} }

// StopCauseBringUpFailed — a bring-up that never wired is being torn down.
func StopCauseBringUpFailed() StopCause { return StopCause{id: causeBringUpFailed} }

// StopCauseControllerExit — a session controller's run loop ended on its own.
func StopCauseControllerExit() StopCause { return StopCause{id: causeControllerExit} }

// causesOwingTurnResumption is the set of stops after which an interrupted
// turn is still OWED.
//
// THE DISTINCTION IS WHETHER THE WORK WAS ABANDONED OR MERELY DISPLACED. Most
// stops end the work on purpose: an idle sweep reaped a workspace nobody has
// touched in an hour, a user forced hibernation, a merge stood the session
// down, a session was deleted or superseded. Re-driving a turn after any of
// those would restart work the system was right to stop.
//
// These three are different. The daemon is going away and taking the shim with
// it — a deploy whose bundle changed, a scheduled drain executing, an explicit
// hard restart — and the user asked for the turn that the bounce happened to
// land on top of. Nothing about their request stopped being true, so the
// successor daemon owes them the work.
//
// It is a property OF THE CAUSE, defined here beside the vocabulary, so no
// teardown re-derives "is this a bounce" from a path string.
var causesOwingTurnResumption = map[stopCauseID]bool{
	causeDaemonShutdown:  true,
	causeDrainExecution:  true,
	causeHardRestartLive: true,
}

// owesTurnResumption reports whether a turn this cause interrupts must be
// re-driven once the session is wired again.
//
// A REFINED CAUSE ANSWERS FOR WHAT THE DAEMON FOUND, not for what the caller
// asked. `supersededRecord` refines a bounce into "this record is not the one
// driving the workspace any more", and the replacement session's own turn is
// not this record's to resume — so the refinement's answer, not the origin's,
// is the one that counts.
func (c StopCause) owesTurnResumption() bool {
	return causesOwingTurnResumption[c.id]
}

// supersededRecord refines a caller's cause for the branch where the requested
// session is no longer the one driving the workspace. The caller's cause is
// retained as the origin, so the record names both the request and the finding.
func (c StopCause) supersededRecord() StopCause {
	return StopCause{id: causeSupersededRecord, origin: c.id}
}

// valid reports whether this cause was minted by a constructor.
func (c StopCause) valid() bool {
	_, ok := stopCauseTable[c.id]
	return ok
}

// rendering resolves the table row, or false for the invalid zero value.
func (c StopCause) rendering() (stopCauseRendering, bool) {
	r, ok := stopCauseTable[c.id]
	return r, ok
}

// path is the funnel token this cause travels under. An invalid cause renders
// a token that names the defect rather than a plausible path, so a stop that
// somehow reaches the SSM unattributed is identifiable in the log it lands in.
func (c StopCause) path() string {
	r, ok := c.rendering()
	if !ok {
		return "unattributed_stop"
	}
	return r.path
}

// stop renders the attribution handed to the shim. An invalid cause renders the
// ZERO shim.Stop, which shim.Stop.Validate refuses — the refusal is the point,
// and no layer substitutes a plausible-looking attribution for it.
func (c StopCause) stop() shim.Stop {
	r, ok := c.rendering()
	if !ok {
		return shim.Stop{}
	}
	reason := r.reason
	if origin, refined := stopCauseTable[c.origin]; refined {
		reason = fmt.Sprintf("%s; requested by %s (%s)", reason, origin.initiator, origin.reason)
	}
	return shim.Stop{Initiator: r.initiator, Reason: reason}
}

// String names the cause for a log line. An invalid one says so.
func (c StopCause) String() string {
	r, ok := c.rendering()
	if !ok {
		return "invalid_stop_cause"
	}
	if origin, refined := stopCauseTable[c.origin]; refined {
		return r.initiator + "<-" + origin.initiator
	}
	return r.initiator
}

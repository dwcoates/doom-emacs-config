package cachelapse

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A KEEPER THAT FAILS A WORKSPACE SAYS SO, LOUDLY (owner request,
// 2026-11-11). On 2026-11-11 the keeper sat in a nil-fleet boot loop for
// hours: next to no keep-alives went out (0 to 4 an hour, against 30 to 50 the
// day before), nothing surfaced, and the caches lapsed in silence. So every
// keep-alive the keeper owed and did not deliver is recorded at ERROR with
// the workspace and the cause, and stands as a `keepalive_undelivered` fault
// on the workspace's footer and health answers until a keep-alive goes out
// for it again (health.EdgeKeepaliveSent) or its next turn starts. A keeper
// whose loop stops stands as a daemon-wide `keepalive_drive_stopped` fault.
// NOTHING HERE RETRIES OR WORKS AROUND THE FAILURE: the retries the drive
// already makes are its design, and this file only reports.

// opDelivery is the operation the delivery faults are recorded under.
const opDelivery = "daemon.cachelapse.delivery"

// Faults is the slice of the state client the keeper records its delivery
// faults through: the observed client (health.ObserveFaults), so each one
// reaches the footer.
type Faults interface {
	health.FaultStore
	OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error)
}

// undelivered records a keep-alive WS was owed, due at DUE (the zero time when
// it could not be timed), that could not be armed or sent: MESSAGE at ERROR
// under OP with FIELDS and CAUSE, and its fault. A cause the daemon's exit made is no
// failure (leaving), and records neither.
func (k *Keeper) undelivered(ctx context.Context, log dlog.Logger, op string, ws ids.WorkspaceID, due time.Time, doing, message string, fields dlog.Context, cause error) {
	if leaving(ctx, log, op, doing, cause) {
		return
	}
	log.Error(op, message, dlog.WithCause(dlog.Merge(fields, dueField(due)), cause))
	k.raiseUndelivered(ctx, log, ws, due, message+": "+cause.Error())
}

// raiseUndelivered opens WS's `keepalive_undelivered` fault for a failure
// already recorded at ERROR, superseding the one standing, so a run of
// failures stands as its latest. It is THE ONE open of the fault
// (TestTheUndeliveredFaultHasOneOpen).
func (k *Keeper) raiseUndelivered(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, due time.Time, cause string) {
	now := k.deps.Clock.Now()
	// A read or close that fails is recorded at ERROR by CloseOnEdge, and the
	// new fault still opens beside the one left standing.
	_, _ = health.CloseOnEdge(ctx, k.deps.Faults, log, health.EdgeSuperseded, health.EdgeScope{
		Workspace: &ws,
		Match:     func(f wsm.Fault) bool { return f.Kind == health.KindKeepaliveUndelivered },
	}, now)
	if _, err := k.deps.Faults.OpenFault(ctx, wsm.Fault{
		Workspace: &ws,
		Kind:      health.KindKeepaliveUndelivered,
		Detail:    cause,
		Evidence:  health.KeepaliveUndeliveredEvidence(cause, due),
		OpenedAt:  now,
	}); err != nil && !leaving(ctx, log, opDelivery, "recording the undelivered keep-alive's fault", err) {
		log.Error(opDelivery, "could not record the undelivered keep-alive's fault; the failure stands on the record above alone",
			dlog.WithCause(dlog.Merge(dlog.Context{"undelivered": cause}, dueField(due)), err))
	}
}

// delivered closes WS's `keepalive_undelivered` fault: a keep-alive went out
// for it. A read or close that fails is recorded at ERROR by CloseOnEdge.
func (k *Keeper) delivered(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	_, _ = health.CloseOnEdge(ctx, k.deps.Faults, log, health.EdgeKeepaliveSent, health.EdgeScope{Workspace: &ws}, k.deps.Clock.Now())
}

// missed records a keep-alive due at DUE whose cache the shim judged lapsed
// at LAPSED, and answers whether the keeper missed it: a cache that lapsed
// at or after its keep-alive came due was still warm when the beat was owed,
// so the beat did not go out in time (the daemon or its keeper was not
// running, or the session was not brought back). A cache that lapsed before
// its beat came due (a tier the cadence cannot keep, a request that left the
// context uncached) was never the keeper's to keep.
func (k *Keeper) missed(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, due, lapsed time.Time) bool {
	if lapsed.Before(due) {
		return false
	}
	cause := fmt.Sprintf("the cache lapsed at %s, %s after its keep-alive came due at %s, before the keep-alive went out",
		lapsed.UTC().Format(time.RFC3339), lapsed.Sub(due).Round(time.Second), due.UTC().Format(time.RFC3339))
	log.Error(opDrive, "a keep-alive that came due was not sent before the cache lapsed; the keeper missed it", dlog.Merge(dueField(due), dlog.Context{
		"lapsed_at": lapsed, "late_by": lapsed.Sub(due).String(), "cause": cause,
	}))
	k.raiseUndelivered(ctx, log, ws, due, cause)
	return true
}

// driveStopped stands the keeper's loop ending before the daemon did, with
// CAUSE, as the daemon-wide `keepalive_drive_stopped` fault. The failure that
// stopped it was recorded at ERROR where it happened (restore).
func (k *Keeper) driveStopped(ctx context.Context, cause error) {
	log := k.deps.Log.Global()
	if _, err := k.deps.Faults.OpenFault(ctx, wsm.Fault{
		Kind:     health.KindKeepaliveDriveStopped,
		Detail:   cause.Error(),
		Evidence: map[string]string{health.EvidenceCause: cause.Error()},
		OpenedAt: k.deps.Clock.Now(),
	}); err != nil {
		log.Error(opDelivery, "could not record the stopped keeper's fault; the stop stands on the record above alone", dlog.WithCause(nil, err))
	}
}

// dueField names a keep-alive's due instant for a record, "untimed" when the
// keeper could not time it.
func dueField(due time.Time) dlog.Context {
	if due.IsZero() {
		return dlog.Context{"due": "untimed"}
	}
	return dlog.Context{"due": due}
}

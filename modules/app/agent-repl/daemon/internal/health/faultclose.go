package health

import (
	"context"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE ONE DOOR A STANDING FAULT LEAVES BY. Every recovery edge closes through
// CloseOnEdge, which closes every standing fault whose declared edge it is
// (lifetime.go); a site that already holds the one fault it retracts closes it
// through CloseFaultOn, which CloseOnEdge itself closes through. Either way the
// kind's declared lifetime is consulted, never a list at the call site, and
// the close is recorded once, at INFO, with the kind, the fault, the edge and
// how long it stood.

// opCloseOnEdge is the operation every edge close is recorded under.
const opCloseOnEdge = "daemon.health.close_on_edge"

// FaultCloser closes one recorded fault.
type FaultCloser interface {
	// CloseFault stamps a fault's resolved-at.
	CloseFault(ctx context.Context, id ids.FaultID, at time.Time) error
}

// FaultStore is the slice of the state client a recovery edge reads and
// closes through. The observed state client (ObserveFaults) is one, so every
// close reaches the footer.
type FaultStore interface {
	FaultCloser
	// OpenFaults lists the standing faults in scope.
	OpenFaults(ctx context.Context, scope wsm.FaultScope) ([]wsm.Fault, error)
}

// FaultRecorder is the slice of the state client a momentary record needs.
type FaultRecorder interface {
	FaultCloser
	// OpenFault records a fault and returns its id.
	OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error)
}

// EdgeScope narrows which standing faults one firing of an edge speaks for.
type EdgeScope struct {
	// Workspace limits the edge to one workspace's faults. Nil reads every
	// standing fault, daemon-scoped and workspace-scoped alike.
	Workspace *ids.WorkspaceID
	// DaemonOnly limits the edge to the daemon-scoped faults.
	DaemonOnly bool
	// Match narrows the edge to the faults it is about (a deploy's step, the
	// one fault a resolver tracks). Nil matches every fault in scope.
	Match func(wsm.Fault) bool
}

// CloseOnEdge closes every standing fault in scope whose kind declares edge,
// and answers how many it closed. A read or a close that fails is recorded
// at ERROR here and leaves the fault standing. The error is the READ's
// failure, already recorded: a caller that must not go on without knowing
// what stands checks it, and every other caller has nothing to add.
func CloseOnEdge(ctx context.Context, store FaultStore, log dlog.Logger, edge Edge, scope EdgeScope, at time.Time) (int, error) {
	fields := dlog.Context{"edge": string(edge)}
	if scope.Workspace != nil {
		fields["workspace"] = string(*scope.Workspace)
	}
	open, err := store.OpenFaults(ctx, wsm.FaultScope{Workspace: scope.Workspace})
	if err != nil {
		fields["cause"] = err.Error()
		log.Error(opCloseOnEdge, "could not read the standing faults a recovery edge closes; they stand", fields)
		return 0, fmt.Errorf("health: read the faults %s closes: %w", edge, err)
	}
	closed := 0
	for _, f := range open {
		if scope.DaemonOnly && f.Workspace != nil {
			continue
		}
		if !ClosesOn(f.Kind, edge) {
			continue
		}
		if scope.Match != nil && !scope.Match(f) {
			continue
		}
		if CloseFaultOn(ctx, store, log, edge, f, at) {
			closed++
		}
	}
	return closed, nil
}

// CloseFaultOn closes ONE standing fault on edge, answering whether it
// closed. An edge the fault's kind does not declare is REFUSED at ERROR: it is
// a call site that has drifted from the lifetime table, and the fault stands.
func CloseFaultOn(ctx context.Context, closer FaultCloser, log dlog.Logger, edge Edge, f wsm.Fault, at time.Time) bool {
	fields := dlog.Context{"kind": f.Kind, "fault": string(f.ID), "edge": string(edge)}
	if f.Workspace != nil {
		fields["workspace"] = string(*f.Workspace)
	}
	if !ClosesOn(f.Kind, edge) {
		log.Error(opCloseOnEdge, "refused to close a fault on an edge its kind does not declare; it stands", fields)
		return false
	}
	if err := closer.CloseFault(ctx, f.ID, at); err != nil {
		fields["cause"] = err.Error()
		log.Error(opCloseOnEdge, "could not close a standing fault on its recovery edge; it stands", fields)
		return false
	}
	if f.OpenedAt.IsZero() {
		fields["stood"] = "unknown: the record carries no open instant"
	} else {
		fields["stood"] = at.Sub(f.OpenedAt).String()
	}
	log.Info(opCloseOnEdge, "a recovery edge closed a standing fault", fields)
	return true
}

// RecordMomentary records a MOMENTARY fault and closes it at once, on
// EdgeRecorded, so it never stands. A standing kind is refused: it would be
// closed before any recovery edge proved its condition over.
func RecordMomentary(ctx context.Context, rec FaultRecorder, log dlog.Logger, f wsm.Fault, at time.Time) (ids.FaultID, error) {
	if !Momentary(f.Kind) {
		log.Error(opCloseOnEdge, "refused to record a standing kind as momentary", dlog.Context{"kind": f.Kind})
		return "", fmt.Errorf("health: %q is not a momentary fault kind", f.Kind)
	}
	if f.OpenedAt.IsZero() {
		f.OpenedAt = at
	}
	id, err := rec.OpenFault(ctx, f)
	if err != nil {
		return "", err
	}
	f.ID = id
	if !CloseFaultOn(ctx, rec, log, EdgeRecorded, f, at) {
		return id, fmt.Errorf("health: close the momentary fault %s", id)
	}
	return id, nil
}

// ReporterFaults is a Reporter as a FaultStore, for the sites that record
// faults through the reporter. The reporter stamps a close with its own
// clock, so the instant CloseOnEdge is given only measures how long the fault
// stood.
func ReporterFaults(r Reporter) FaultStore { return reporterFaults{r: r} }

// reporterFaults is ReporterFaults' adapter.
type reporterFaults struct{ r Reporter }

// OpenFaults implements FaultStore.
func (a reporterFaults) OpenFaults(ctx context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	return a.r.OpenFaults(ctx, scope)
}

// CloseFault implements FaultStore.
func (a reporterFaults) CloseFault(ctx context.Context, id ids.FaultID, _ time.Time) error {
	return a.r.CloseFault(ctx, id)
}

// Package handover is the intake and view halves of a blue-green handover.
//
// The rollout controller drives the handover but owns neither the prompt queue
// nor the resolvers, and both halves of the move need them: the OUTGOING
// daemon must hold every arrival for a workspace it is about to detach from,
// and the INCOMING one must let those arrivals through and repaint the
// workspace once it owns it. Those three steps live here, in one package, so
// the two halves of one move are written against the same facts rather than
// improvised at the composition root.
package handover

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/wsm"
)

// The operation names this package logs under.
const (
	opQuiesce = "daemon.handover.quiesce"
	opDrain   = "daemon.handover.drain_intake"
	opPublish = "daemon.handover.publish_views"
)

// QuiesceHolder is the lease holder a handover's hold is taken under. A
// handover is a daemon restart from the workspace's point of view — the shim
// keeps running, the daemon serving it changes — so it holds the intake under
// the same holder a shim relaunch does, and the queue's hold policy is the
// same one.
const QuiesceHolder = wsm.HolderRestart

// Intake holds and releases one workspace's prompt intake across a handover.
type Intake struct {
	db    wsm.DB
	queue promptqueue.Queue
	log   dlog.Logger
}

// NewIntake builds the intake half. Both collaborators are required: a hold
// with no queue to re-evaluate is a row nobody acts on, and a queue with no
// durable lease is a hold that does not survive the bounce it exists for.
func NewIntake(db wsm.DB, queue promptqueue.Queue, log dlog.Logger) (*Intake, error) {
	switch {
	case db == nil:
		return nil, fmt.Errorf("handover: the intake needs a state client")
	case queue == nil:
		return nil, fmt.Errorf("handover: the intake needs a prompt queue")
	case log == nil:
		return nil, fmt.Errorf("handover: the intake needs a logger")
	}
	return &Intake{db: db, queue: queue, log: log}, nil
}

// Quiesce holds ALL intake for one workspace. From the transfer notice on this
// daemon does no work for the workspace: every arrival is HELD rather than
// served, so nothing the successor is about to own is still moving under it.
//
// A lease already held is SUCCESS, not a refusal: the workspace is already
// quiet, which is the state the caller asked for, and a merge or a relaunch
// that got there first holds it for its own reason.
//
// IT ANSWERS THE LEASE IT TOOK, empty when another holder's lease already
// held the intake: the caller owns exactly what it took, and a transfer that
// fails or is reclaimed releases THAT lease and never another holder's.
func (i *Intake) Quiesce(ctx context.Context, ws ids.WorkspaceID) (wsm.LeaseID, error) {
	fields := dlog.Context{"workspace": string(ws)}
	if _, held, err := i.db.Lease(ctx, ws); err != nil {
		i.log.Error(opQuiesce, "could not read the workspace's lease", withCause(fields, err))
		return "", fmt.Errorf("handover: quiesce %q: read the lease: %w", ws, err)
	} else if held {
		i.log.Debug(opQuiesce, "a lease already holds this workspace's intake", fields)
		i.queue.OnLeaseChanged(ws)
		return "", nil
	}
	lease, err := i.db.AcquireLease(ctx, ws, QuiesceHolder, wsm.PolicyHold)
	if err != nil {
		i.log.Error(opQuiesce, "could not take the handover hold", withCause(fields, err))
		return "", fmt.Errorf("handover: quiesce %q: take the hold: %w", ws, err)
	}
	fields["lease"] = string(lease.ID)
	// THE QUEUE IS TOLD, not left to notice: the hold's effect on standing
	// submissions is a re-evaluation, and nothing else triggers one.
	i.queue.OnLeaseChanged(ws)
	i.log.Info(opQuiesce, "held the workspace's intake for the handover", fields)
	return lease.ID, nil
}

// DrainIntake releases the held intake IN ORDER once the successor owns the
// workspace. Releasing the hold is what drains it: the queue re-evaluates
// every standing hold against the lease that is no longer there.
//
// A workspace with NO lease is success: nothing was held, so nothing has to be
// let go, and refusing here would fail an adoption over a workspace that was
// already open.
func (i *Intake) DrainIntake(ctx context.Context, ws ids.WorkspaceID) error {
	fields := dlog.Context{"workspace": string(ws)}
	lease, held, err := i.db.Lease(ctx, ws)
	if err != nil {
		i.log.Error(opDrain, "could not read the workspace's lease", withCause(fields, err))
		return fmt.Errorf("handover: drain %q: read the lease: %w", ws, err)
	}
	if !held {
		i.log.Debug(opDrain, "nothing holds this workspace's intake", fields)
		i.queue.OnLeaseChanged(ws)
		return nil
	}
	fields["lease"] = string(lease.ID)
	fields["holder"] = lease.Holder.String()
	if lease.Holder != QuiesceHolder {
		// ANOTHER HOLDER'S LEASE IS NOT THIS ONE'S TO RELEASE. A merge that
		// was running when the handover began still owns the workspace on the
		// successor, and dropping its lease would let work in behind it.
		i.log.Warn(opDrain, "the workspace is held by another lease holder; leaving it in place", fields)
		i.queue.OnLeaseChanged(ws)
		return nil
	}
	if err := i.db.ReleaseLease(ctx, lease.ID); err != nil {
		i.log.Error(opDrain, "could not release the handover hold", withCause(fields, err))
		return fmt.Errorf("handover: drain %q: release the hold: %w", ws, err)
	}
	i.queue.OnLeaseChanged(ws)
	i.log.Info(opDrain, "released the handover hold; the held intake drains", fields)
	return nil
}

// Views republishes a workspace's whole views after adoption.
type Views struct {
	topbar  topbar.Resolver
	footer  footer.Resolver
	holds   holds.Resolver
	sidebar sidebar.Resolver
	log     dlog.Logger
}

// NewViews builds the view half from the four resolvers that hold a
// per-workspace or editor-global publication.
func NewViews(t topbar.Resolver, f footer.Resolver, h holds.Resolver, s sidebar.Resolver, log dlog.Logger) (*Views, error) {
	switch {
	case t == nil || f == nil || h == nil || s == nil:
		return nil, fmt.Errorf("handover: the view republisher needs all four view resolvers")
	case log == nil:
		return nil, fmt.Errorf("handover: the view republisher needs a logger")
	}
	return &Views{topbar: t, footer: f, holds: h, sidebar: s, log: log}, nil
}

// PublishViews republishes one workspace's whole views, so a client that
// re-attached to the successor is repainted from what the daemon holds rather
// than from what it happened to receive before the move.
//
// A view NOTHING HAS EVER PUBLISHED is skipped rather than published empty: an
// empty topbar drawn over a real one is worse than the client's own last
// paint, and the workspace's own frames will publish it as they arrive.
func (v *Views) PublishViews(_ context.Context, ws ids.WorkspaceID) error {
	fields := dlog.Context{"workspace": string(ws)}
	republished := 0
	if republish(v.topbar.Topic(ws)) {
		republished++
	}
	if republish(v.footer.Topic(ws)) {
		republished++
	}
	if republish(v.holds.Topic(ws)) {
		republished++
	}
	// The roster is EDITOR-GLOBAL: one publication for every workspace, so it
	// is republished with each adoption and the last one leaves it current.
	if republish(v.sidebar.Topic()) {
		republished++
	}
	fields["views"] = republished
	v.log.Debug(opPublish, "republished the workspace's views after adoption", fields)
	return nil
}

// republish hands a topic's latest value to its subscribers again, reporting
// whether there was one. It is generic because the four topics carry four view
// types and the step is identical for each.
func republish[T any](topic *publish.Topic[T]) bool {
	if topic == nil {
		return false
	}
	return topic.Republish()
}

// withCause adds an error to a log context without a nil check at every site.
func withCause(fields dlog.Context, err error) dlog.Context {
	out := dlog.Context{}
	for k, v := range fields {
		out[k] = v
	}
	if err != nil {
		out["cause"] = err.Error()
	}
	return out
}

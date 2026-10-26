// Package coldbacklog moves a workspace whose cold gate has stood for over a
// day into the sidebar's hidden section, drawn as the Backlog (owner request,
// 2026-10-26). WEEKENDS NEVER COUNT toward the day (owner ruling, 2026-10-26):
// a gate is stale once a whole day of Monday-to-Friday time has passed since
// its conversation went cold (StaleAt).
//
// A MOVE IS A HIDE IN EVERY RESPECT (workspace.Verbs.BacklogColdGate): the
// tab leaves the bar, and the cache keeper takes the gate's lapse and stands
// the parked session down. It is made ONCE PER GATE: the row records the gate
// it was moved for (wsm.Workspace.BackloggedGateAt), so a workspace the user
// shows again stays shown, across every restart that raises the same gate
// again, until a new conversation goes cold into a new gate.
//
// THE SWEEP IS PERIODIC (clock.Repeat): a gate's staleness is measured in a
// day, so a few minutes' lateness is nothing, and a workspace the user was
// looking at when its gate went stale is moved by the first sweep after they
// move off it. Each sweep reads only the workspaces whose gate stands
// (workspace.Fleet.StandingColdGates), never the whole registry.
package coldbacklog

import (
	"context"
	"errors"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/cachelapse"
	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opSweep is the operation this package records under.
const opSweep = "daemon.coldbacklog.sweep"

// DefaultEvery is the sweep's cadence.
const DefaultEvery = 5 * time.Minute

// staleSpan is how much weekday time a gate stands before it is stale.
const staleSpan = 24 * time.Hour

// DB is the slice of the state client the Sweeper reads.
type DB interface {
	Workspace(ctx context.Context, id wsm.WorkspaceID) (wsm.Workspace, error)
}

// Deps are the Sweeper's collaborators. Every one is REQUIRED.
type Deps struct {
	DB DB
	// Gates answers the cold facts of every standing cold gate this daemon
	// serves (workspace.Fleet.StandingColdGates).
	Gates func() map[ids.WorkspaceID]*conversationv1.SessionCold
	// Backlog moves a workspace into the backlog for the gate whose cache
	// lapsed at gateLapsedAt (workspace.Verbs.BacklogColdGate), which judges
	// again and records its own outcome.
	Backlog func(ctx context.Context, ws ids.WorkspaceID, gateLapsedAt time.Time) error
	Clock   clock.Clock
	// Location is where a weekend is judged: the user's own days.
	Location *time.Location
	Log      dlog.Surfaces
}

// Sweeper is the cold-gate backlog's periodic sweep.
type Sweeper struct {
	deps Deps
}

// New builds a Sweeper. A missing collaborator is a wiring defect.
func New(deps Deps) (*Sweeper, error) {
	switch {
	case deps.DB == nil:
		return nil, errors.New("coldbacklog: the sweeper needs a state client")
	case deps.Gates == nil:
		return nil, errors.New("coldbacklog: the sweeper needs the standing cold gates")
	case deps.Backlog == nil:
		return nil, errors.New("coldbacklog: the sweeper needs the backlog verb")
	case deps.Clock == nil:
		return nil, errors.New("coldbacklog: the sweeper needs a clock")
	case deps.Location == nil:
		return nil, errors.New("coldbacklog: the sweeper needs the location weekends are judged in")
	case deps.Log == nil:
		return nil, errors.New("coldbacklog: the sweeper needs log surfaces")
	}
	return &Sweeper{deps: deps}, nil
}

// Run sweeps every EVERY until ctx ends.
func (s *Sweeper) Run(ctx context.Context, every time.Duration) error {
	clock.Repeat(ctx, s.deps.Clock, every, every, s.Sweep)
	return nil
}

// Sweep moves every shown workspace whose standing cold gate is stale into
// the backlog.
func (s *Sweeper) Sweep(ctx context.Context) {
	now := s.deps.Clock.Now()
	gates := s.deps.Gates()
	stale := 0
	for ws, cold := range gates {
		lapsedAt := cachelapse.GateLapsedAt(cold)
		staleAt := StaleAt(lapsedAt, s.deps.Location)
		if !now.After(staleAt) {
			continue
		}
		stale++
		s.backlog(ctx, ws, lapsedAt, staleAt)
	}
	s.deps.Log.Global().Debug(opSweep, "swept the standing cold gates for stale ones", dlog.Context{"standing": len(gates), "stale": stale})
}

// backlog moves WS into the backlog for its stale gate, unless its row
// declines the move (wsm.Workspace.BacklogsColdGate), which skips the write
// the store would decline anyway.
func (s *Sweeper) backlog(ctx context.Context, ws ids.WorkspaceID, lapsedAt, staleAt time.Time) {
	log := s.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "gate_lapsed_at": lapsedAt, "stale_at": staleAt})
	record, err := s.deps.DB.Workspace(ctx, ws)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		log.Debug(opSweep, "the workspace behind a stale cold gate is no longer registered; nothing is moved", nil)
		return
	case err != nil && ctx.Err() != nil:
		log.Info(opSweep, "the daemon is leaving; the sweep ended with it", dlog.Context{"cause": err.Error()})
		return
	case err != nil:
		log.Error(opSweep, "could not read the workspace behind a stale cold gate; it is not moved this sweep", dlog.Context{"cause": err.Error()})
		return
	}
	if !record.BacklogsColdGate(lapsedAt) {
		log.Debug(opSweep, "the workspace's stale cold gate does not move it: it is not shown, the user is looking at it, or it was already moved for this gate", dlog.Context{
			"hidden": record.Hidden, "closed": record.Closed, "current": record.Current,
		})
		return
	}
	if err := s.deps.Backlog(ctx, ws, lapsedAt); err != nil {
		// The verb is the failure's owner and recorded it at its own level.
		log.Debug(opSweep, "the backlog verb did not move the workspace; it is tried again next sweep", dlog.Context{"cause": err.Error()})
	}
}

// StaleAt is when a cold gate whose conversation went cold at LAPSEDAT has
// stood for a whole day of weekday time in LOC: 24 hours that fall on Monday
// through Friday. Weekend hours never count, so a gate that went cold on a
// Friday afternoon is stale on Monday afternoon, and one that went cold on a
// Saturday is stale at the end of Monday.
func StaleAt(lapsedAt time.Time, loc *time.Location) time.Time {
	at := lapsedAt.In(loc)
	remaining := staleSpan
	for {
		y, m, d := at.Date()
		nextDay := time.Date(y, m, d+1, 0, 0, 0, 0, loc)
		if weekend(at.Weekday()) {
			at = nextDay
			continue
		}
		span := nextDay.Sub(at)
		if span >= remaining {
			return at.Add(remaining)
		}
		remaining -= span
		at = nextDay
	}
}

func weekend(day time.Weekday) bool {
	return day == time.Saturday || day == time.Sunday
}

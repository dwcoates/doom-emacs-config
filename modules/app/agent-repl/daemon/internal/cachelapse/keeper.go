// Package cachelapse keeps a HIDDEN workspace's prompt cache until it lapses
// (owner request, 2026-10-13; docs/protobuf-design/hidden-cache-lapse.md).
//
// A CLOSED WORKSPACE IS KEPT THE SAME WAY, after ONE LAST ROUND (owner
// request, 2026-10-17). Closing leaves the session up, so its shim is asked
// for its last keep-alive round (shim.v1.SetKeepaliveLastRound): the beat it
// had scheduled goes out, then the cadence holds and states the lapse, and the
// session is stood down when it comes, so reopening it afterwards meets the
// cold gate like any lapsed conversation. Every close and reopen the daemon
// records reaches this keeper (ObserveClosures).
//
// A HIDDEN MERGE WORKSPACE WHOSE MERGE HAS LEFT EVERY QUEUE is stood down the
// same way (wsm.Workspace.Runs): its merge failed or was taken off, nothing
// is carried, and its requester's bubble is how the user reaches it. Every
// removal from a merge queue reaches this keeper (ObserveMergeQueues), and so
// does every session coming up or parking, so a session a takeover or a boot
// adopts for one is stood down too.
//
// Hiding a workspace no longer stands its session down at once. The daemon
// sends it no more keep-alives, so the cache lapses on its own clock, and
// until then the user can show the workspace again with its cache still
// warm. Every shim states the projected lapse on its session stream
// (conversation.v1.SessionCacheLapse); the Keeper records it on the workspace's
// row, which the roster counts down to, and stands the session down when it
// comes.
//
// ONE LOOP OWNS EVERYTHING. Every edge (a projection stated, a visibility
// change, a session coming up) is queued to Run, which applies them in order
// and owns the one deadline timer, so no two edges for one workspace race and
// no deadline is armed twice.
//
// THE BEAT IS RECONCILED, NEVER TOLD ONCE. On a hide, a show, a close, a
// reopen, a selection moving onto or off the workspace, every session coming
// up and every session falling free, the loop reads the workspace's row afresh
// and arms its next beat to match (beatDue): none while hidden and not
// selected (wsm.Workspace.KeepaliveHeld), one last one while closed, every
// cycle otherwise. A hide that races a bring-up is caught either way, because
// the hide writes the row before it queues the edge and the bring-up installs
// its client before SessionUp queues its own, so whichever edge comes second
// sees both.
//
// THE DEADLINE OUTLIVES THE DAEMON AND THE SHIM. The projection lives on the
// row, and Run re-arms every recorded one when it starts, so a lapse that
// passes while the daemon is down is taken at once on the next start.
//
// THE DAEMON DRIVES EVERY KEEP-ALIVE (owner ruling, 2026-10-26; schedule.go).
// A shim runs no keep-alive timer of its own: the Keeper asks it for each beat
// (shim.v1.SendKeepalive), timed from the cache clock
// (wsm.Workspace.CacheClock): the later of the last turn's end and the last
// keep-alive the Keeper sent, which it records on the row, because nothing of
// a keep-alive turn is stored and the turns' own clock never moves for one.
// Once a beat has gone out a session nobody is using is stood down
// (drain.Controller.CycleDown) and restored a minute before its next beat, at
// most MaxRestores at once, so an idle workspace's ~350MB is free between
// beats. A hidden workspace beats no more and its cache lapses as above; a
// closed one gets the one beat it was owed, then lapses.
package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/bringup"
	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// The operations this package records under.
const (
	opProjection = "daemon.cachelapse.projection"
	opCadence    = "daemon.cachelapse.reconcile"
	opDrive      = "daemon.cachelapse.drive"
	opRestore    = "daemon.cachelapse.restore"
	opLapse      = "daemon.cachelapse.lapse"
	opMergeEnded = "daemon.cachelapse.merge_ended"
)

// DB is the slice of the state client the Keeper reads and writes. The
// repositories and merge queues it lists answer whether a hidden merge
// workspace still carries its merge (bringup.MergeQueued).
type DB interface {
	bringup.RepositoryLister
	ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error)
	Workspace(ctx context.Context, id wsm.WorkspaceID) (wsm.Workspace, error)
	SetCacheLapse(ctx context.Context, id wsm.WorkspaceID, lapse wsm.CacheLapse) (bool, error)
	// SetLastKeepalive records when a workspace's keep-alive was sent: the
	// beat's half of the cache clock (wsm.Workspace.CacheClock).
	SetLastKeepalive(ctx context.Context, id wsm.WorkspaceID, at time.Time) error
	// Session reads a workspace's session record: its engagement clock and
	// its park.
	Session(ctx context.Context, id wsm.WorkspaceID) (wsm.Session, bool, error)
}

// Keepalives is one serving shim's keep-alive.
type Keepalives interface {
	// Beat sends one keep-alive now and answers the shim's judgment of it.
	// ErrNoSession when the shim holds no session; a *BeatSubmitFailed when
	// the submission failed.
	Beat(ctx context.Context) (*shimv1.SendKeepaliveSuccess, error)
}

// ErrNoSession is a shim's answer that it holds no session, so there is no
// cache to keep. Not a fault: the session is not up yet, or already gone.
var ErrNoSession = errors.New("cachelapse: the shim holds no session")

// StandDowns stands a withheld workspace's session down as the idle sweep
// does, never under a turn (drain.Controller).
type StandDowns interface {
	StandDownWithheld(ws ids.WorkspaceID)
}

// Deps are the Keeper's collaborators. Every one is REQUIRED.
type Deps struct {
	DB DB
	// Shim answers the cadence of the shim serving WS, false when this daemon
	// serves no session for it.
	Shim func(ws ids.WorkspaceID) (Keepalives, bool)
	// ColdGate answers the cold facts of WS's STANDING cold gate, false when
	// none stands (workspace.Fleet.StandingColdGate). A session parked at the
	// gate holds no cadence to project a lapse from, and its conversation is
	// already cold, so the gate's facts are its lapse (gateLapse).
	ColdGate   func(ws ids.WorkspaceID) (*conversationv1.SessionCold, bool)
	StandDowns StandDowns
	// PublishRegistry republishes the roster's durable half, which is where
	// a recorded projection and a lapse reach the hidden section.
	PublishRegistry func(context.Context) error
	// IdleCutoff is the idle sweep's cutoff (drain): no beat is sent for a
	// workspace whose engagement is older than it by the time the beat comes
	// due, since the sweep puts that session to sleep. Zero disables the
	// sweep, and with it this limit.
	IdleCutoff time.Duration
	// Interval is how long after the cache clock a beat is due:
	// KeepaliveInterval, or a test run's compressed cadence (IntervalFromEnv).
	Interval time.Duration
	// CycleDown asks for WS's session to be stood down after the keep-alive
	// asked at BEAT (drain.Controller.CycleDown).
	CycleDown func(ws ids.WorkspaceID, beat time.Time)
	// Restore brings a session the keep-alive cycle parked back up, calling
	// DONE once with how its start ended. It returns at once.
	Restore func(ws ids.WorkspaceID, done func(error))
	Clock   clock.Clock
	Log     dlog.Surfaces
	// Telemetry receives every beat, lapse and cold gate (telemetry.go).
	Telemetry Telemetry
}

// edgeKind is what one queued event is.
type edgeKind int

const (
	// edgeReconcile re-reads the workspace and reconciles it (the default).
	edgeReconcile edgeKind = iota
	// edgeLapse records a projection the shim stated.
	edgeLapse
	// edgeFree is the session falling free: its cache clock may have moved,
	// and a beat that waited on its work may now go out.
	edgeFree
	// edgeRestored is a parked session's restore ending.
	edgeRestored
)

// edge is one queued event.
type edge struct {
	ws   ids.WorkspaceID
	kind edgeKind
	// lapse is a projection the shim stated (edgeLapse).
	lapse *conversationv1.SessionCacheLapse
	// err is how a restore ended (edgeRestored).
	err error
	// workEnded is a free edge's (edgeFree): work ended, rather than an
	// attach coming up free.
	workEnded bool
	// why names the edge for the records.
	why string
}

// Keeper is the hidden workspaces' cache keeper.
type Keeper struct {
	deps Deps

	mu    sync.Mutex
	queue []edge
	// wake is signalled when an edge is queued. Its one slot coalesces
	// signals: the loop drains the whole queue on each wake.
	wake chan struct{}

	// deadlines is every armed lapse, owned by Run alone.
	deadlines map[ids.WorkspaceID]time.Time
	// beats is every armed keep-alive, owned by Run alone (schedule.go).
	beats map[ids.WorkspaceID]time.Time
	// restores is every parked session's armed restore, owned by Run alone.
	restores map[ids.WorkspaceID]time.Time
	// queued are the restores waiting for a slot, earliest beat first, and
	// restoring the ones in flight; both owned by Run alone.
	queued    []queuedRestore
	restoring map[ids.WorkspaceID]bool
	// lapsedAt is the cache clock at which the shim answered that the cache
	// had lapsed: no beat goes out again until a turn moves the clock past it.
	lapsedAt map[ids.WorkspaceID]time.Time
	// sent is when this Keeper last sent each workspace's keep-alive: the
	// record's own copy (wsm.Workspace.LastKeepaliveAt) held in memory too,
	// so a write that failed cannot time the next beat from before it.
	sent map[ids.WorkspaceID]time.Time
	// freeSince is when each session last fell free (no turn, no live
	// detached work), as this Keeper saw it. A beat stands a session down
	// only once it has been free StandDownGrace (owner ruling, 2026-10-26).
	freeSince map[ids.WorkspaceID]time.Time
}

// New builds a Keeper. A missing collaborator is a wiring defect.
func New(deps Deps) (*Keeper, error) {
	switch {
	case deps.DB == nil:
		return nil, errors.New("cachelapse: the keeper needs a state client")
	case deps.Shim == nil:
		return nil, errors.New("cachelapse: the keeper needs the shim fleet")
	case deps.ColdGate == nil:
		return nil, errors.New("cachelapse: the keeper needs the standing cold gates")
	case deps.StandDowns == nil:
		return nil, errors.New("cachelapse: the keeper needs the withheld stand-downs")
	case deps.PublishRegistry == nil:
		return nil, errors.New("cachelapse: the keeper needs the roster republish")
	case deps.Interval <= 0:
		return nil, errors.New("cachelapse: the keeper needs a positive keep-alive interval")
	case deps.CycleDown == nil:
		return nil, errors.New("cachelapse: the keeper needs the keep-alive cycle's stand-down")
	case deps.Restore == nil:
		return nil, errors.New("cachelapse: the keeper needs the parked sessions' restore")
	case deps.Clock == nil:
		return nil, errors.New("cachelapse: the keeper needs a clock")
	case deps.Log == nil:
		return nil, errors.New("cachelapse: the keeper needs log surfaces")
	case deps.Telemetry == nil:
		return nil, errors.New("cachelapse: the keeper needs telemetry")
	}
	return &Keeper{
		deps: deps, wake: make(chan struct{}, 1),
		deadlines: map[ids.WorkspaceID]time.Time{},
		beats:     map[ids.WorkspaceID]time.Time{},
		restores:  map[ids.WorkspaceID]time.Time{},
		restoring: map[ids.WorkspaceID]bool{},
		lapsedAt:  map[ids.WorkspaceID]time.Time{},
		sent:      map[ids.WorkspaceID]time.Time{},
		freeSince: map[ids.WorkspaceID]time.Time{},
	}, nil
}

// OnCacheLapse takes a projection a held shim stated
// (sessionwatcher.CacheLapseSink). It never blocks.
func (k *Keeper) OnCacheLapse(ws ids.WorkspaceID, lapse *conversationv1.SessionCacheLapse) {
	k.enqueue(edge{ws: ws, kind: edgeLapse, lapse: lapse, why: "the shim stated its projected lapse"})
}

// SessionFree is WS's session falling free (no turn, no live detached work):
// its next beat is timed afresh from the cache clock its turns moved, and a
// beat that waited on the work goes out. WORKENDED restarts the hour of grace
// before a stand-down; an attach coming up free does not. It never blocks.
func (k *Keeper) SessionFree(ws ids.WorkspaceID, workEnded bool) {
	k.enqueue(edge{ws: ws, kind: edgeFree, workEnded: workEnded, why: "the session fell free"})
}

// TurnClosed is a turn's close on WS, already on the record
// (promptqueue.CacheClocks): the cache clock the beat is timed from moved,
// so its cadence is reconciled. The free edge alone cannot carry this, since
// the session watcher raises it before the close is written. It never
// blocks.
func (k *Keeper) TurnClosed(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "a turn's close was recorded"})
}

// VisibilityChanged is a hide or a show of WS, or the selection moving onto
// or off it, already recorded: its cadence is reconciled. It never blocks.
func (k *Keeper) VisibilityChanged(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "the workspace was hidden, shown, selected or deselected"})
}

// ClosureChanged is a close or a reopen of WS, already recorded: its cadence
// is reconciled. It never blocks.
func (k *Keeper) ClosureChanged(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "the workspace was closed or reopened"})
}

// ColdGateRaised is WS's session parking at its cold gate: a hidden or closed
// workspace takes the gate's lapse. It never blocks.
func (k *Keeper) ColdGateRaised(ws ids.WorkspaceID) {
	if cold, gated := k.deps.ColdGate(ws); gated {
		idle := k.deps.Clock.Now().Sub(time.UnixMilli(cold.GetLastRequestAtMs()))
		k.deps.Telemetry.ObserveColdGate(coldGateOutcome(cold), idle)
	}
	k.enqueue(edge{ws: ws, why: "the session parked at its cold gate"})
}

// MergeLeftQueue is WS's entry leaving its merge queue: a hidden merge
// workspace whose merge left every queue is stood down, and any other's
// cadence reconciled, so a hidden or closed one lets its cache lapse again.
// It never blocks.
func (k *Keeper) MergeLeftQueue(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "its merge left its queue"})
}

// MergeEnteredQueue is WS's merge moving into a queue: its cadence is
// reconciled, so a hidden or closed workspace beats again while the entry
// stands. It never blocks.
func (k *Keeper) MergeEnteredQueue(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "its merge entered a queue"})
}

// SessionUp is a session coming up on WS: its cadence is reconciled. It never
// blocks.
func (k *Keeper) SessionUp(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "a session came up"})
}

func (k *Keeper) enqueue(e edge) {
	k.mu.Lock()
	k.queue = append(k.queue, e)
	k.mu.Unlock()
	select {
	case k.wake <- struct{}{}:
	default:
	}
}

// Run arms every recorded lapse, then applies edges and takes lapses until
// ctx ends.
func (k *Keeper) Run(ctx context.Context) error {
	if err := k.restore(ctx); err != nil {
		return err
	}
	for {
		next, armed := k.earliest()
		// A DEADLINE ALREADY DUE IS TAKEN AT ONCE, never through a timer: a
		// beat a reconcile found overdue goes out now. Each due handler removes
		// what it took or re-arms it later, so this never spins.
		if armed && !next.After(k.deps.Clock.Now()) {
			if ctx.Err() != nil {
				return nil
			}
			k.lapseDue(ctx)
			k.beatsDue(ctx)
			k.restoresDue(ctx)
			continue
		}
		var due <-chan time.Time
		if armed {
			due = k.deps.Clock.After(next.Sub(k.deps.Clock.Now()))
		}
		select {
		case <-ctx.Done():
			return nil
		case <-k.wake:
			for _, e := range k.take() {
				k.apply(ctx, e)
			}
		case <-due:
			k.lapseDue(ctx)
			k.beatsDue(ctx)
			k.restoresDue(ctx)
		}
	}
}

// restore arms the lapse every hidden workspace's row records.
func (k *Keeper) restore(ctx context.Context) error {
	log := k.deps.Log.Global()
	workspaces, err := k.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		if leaving(ctx, log, opLapse, "listing the workspaces to re-arm their cache lapses", err) {
			return nil
		}
		log.Error(opLapse, "could not list the workspaces to re-arm their cache lapses", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("cachelapse: list the workspaces: %w", err)
	}
	for _, ws := range workspaces {
		if ws.CacheLapse != nil {
			k.deadlines[ws.ID] = ws.CacheLapse.LapsesAt
		}
	}
	log.Info(opLapse, "re-armed the hidden workspaces' recorded cache lapses", dlog.Context{"armed": len(k.deadlines)})
	for _, ws := range workspaces {
		k.schedule(ctx, ws.ID, "the daemon started")
	}
	log.Info(opDrive, "armed every workspace's next keep-alive", dlog.Context{"beats": len(k.beats), "restores": len(k.restores)})
	return nil
}

func (k *Keeper) take() []edge {
	k.mu.Lock()
	defer k.mu.Unlock()
	queued := k.queue
	k.queue = nil
	return queued
}

// earliest answers the first armed deadline: a lapse, a beat or a restore.
func (k *Keeper) earliest() (time.Time, bool) {
	var next time.Time
	armed := false
	for _, set := range []map[ids.WorkspaceID]time.Time{k.deadlines, k.beats, k.restores} {
		for _, at := range set {
			if !armed || at.Before(next) {
				next, armed = at, true
			}
		}
	}
	return next, armed
}

func (k *Keeper) apply(ctx context.Context, e edge) {
	switch e.kind {
	case edgeLapse:
		k.record(ctx, e.ws, e.lapse)
	case edgeFree:
		if e.workEnded {
			k.freeSince[e.ws] = k.deps.Clock.Now()
		}
		k.schedule(ctx, e.ws, e.why)
	case edgeRestored:
		k.restored(ctx, e.ws, e.err)
	default:
		k.reconcile(ctx, e.ws, e.why)
	}
}

// record stores a stated projection on a hidden or closed workspace's row and
// arms it. A workspace shown or reopened meanwhile takes none: the store
// refuses it in the statement that reads the visibility and the close.
func (k *Keeper) record(ctx context.Context, ws ids.WorkspaceID, stated *conversationv1.SessionCacheLapse) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	lapse := lapseOf(stated)
	if stated.GetAfterLapse() == nil {
		// A VERDICT THIS SCHEMA REVISION DOES NOT NAME is read as no gate:
		// the row is not struck through, and showing the workspace again
		// raises whatever gate the shim then judges.
		log.Info(opProjection, "the projected lapse states a verdict this daemon does not name; it is read as ungated", lapseFields(lapse))
	}
	k.store(ctx, log, ws, lapse, "the shim's projected cache lapse")
}

// store records LAPSE on a hidden or closed workspace's row and arms it,
// answering whether it was recorded. A workspace shown or reopened meanwhile
// takes none: the store refuses it in the statement that reads the visibility
// and the close. It is THE ONE write of a lapse in this package
// (TestLapseWritesHaveOneHome), whether the shim projected it or a standing
// cold gate stated it.
func (k *Keeper) store(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, lapse wsm.CacheLapse, what string) bool {
	fields := dlog.Merge(lapseFields(lapse), dlog.Context{"source": what})
	recorded, err := k.deps.DB.SetCacheLapse(ctx, ws, lapse)
	if err != nil {
		if leaving(ctx, log, opProjection, "recording the cache lapse", err) {
			return false
		}
		log.Error(opProjection, "could not record the workspace's cache lapse", dlog.WithCause(fields, err))
		return false
	}
	if !recorded {
		// DEBUG, NOT INFO: every shim states its projection on every anchor
		// change now that the daemon drives the beats, and only a hidden or
		// closed workspace's is kept.
		log.Debug(opProjection, "a cache lapse arrived for a workspace that is neither hidden nor closed; it is dropped", fields)
		delete(k.deadlines, ws)
		return false
	}
	k.deadlines[ws] = lapse.LapsesAt
	log.Info(opProjection, "recorded the workspace's cache lapse", fields)
	k.publish(ctx, log, opProjection)
	return true
}

// reconcile re-reads WS's row after an edge: a workspace whose cache is not
// left to lapse has no lapse deadline, a parked cold gate takes the gate's
// lapse, a hidden merge workspace whose merge has ended is stood down, and the
// next keep-alive is armed afresh (schedule).
func (k *Keeper) reconcile(ctx context.Context, ws ids.WorkspaceID, why string) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "edge": why})
	record, err := k.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			log.Info(opCadence, "the workspace is no longer registered; nothing is kept for it", nil)
			k.forget(ws)
			return
		}
		if leaving(ctx, log, opCadence, "reading the workspace's visibility", err) {
			return
		}
		log.Error(opCadence, "could not read the workspace's visibility; its keep-alives are left as they are", dlog.Context{"cause": err.Error()})
		return
	}
	if !record.LetsCacheLapse() {
		delete(k.deadlines, ws)
	}
	// A WORKSPACE PARKED AT ITS COLD GATE IS ALREADY COLD: its column reads
	// "cold cache" at once and its row keeps the gate, rather than counting
	// up from the hide for a lapse no shim will ever project (owner request,
	// 2026-10-17). The lapse comes due at once, so the parked session is
	// stood down as any lapsed one is, and showing it again raises the gate.
	if cold, gated := k.deps.ColdGate(ws); gated && record.LetsCacheLapse() {
		lapse := gateLapse(cold)
		if record.CacheLapse == nil || !sameLapse(*record.CacheLapse, lapse) {
			k.store(ctx, log, ws, lapse, "the standing cold gate")
		}
	}
	if k.mergeEnded(ctx, log, record) {
		delete(k.beats, ws)
		delete(k.restores, ws)
		return
	}
	k.schedule(ctx, ws, why)
}

// forget drops every deadline the Keeper holds for WS.
func (k *Keeper) forget(ws ids.WorkspaceID) {
	delete(k.deadlines, ws)
	delete(k.beats, ws)
	delete(k.restores, ws)
	delete(k.lapsedAt, ws)
	delete(k.sent, ws)
	delete(k.freeSince, ws)
}

// mergeEnded stands RECORD's session down when it is a hidden merge workspace
// the user is not looking at whose merge has left every queue, and answers
// whether it did (or could not tell, which is recorded), so no cadence is set
// on a session being stood down. The drain stands it down never under a turn,
// and finds nothing to do when no shim serves it.
func (k *Keeper) mergeEnded(ctx context.Context, log dlog.Logger, record wsm.Workspace) bool {
	if !record.MergeHidden() || record.Current {
		return false
	}
	queued, err := bringup.MergeQueued(ctx, k.deps.DB, record)
	if err != nil {
		if leaving(ctx, log, opMergeEnded, "reading the merge queues", err) {
			return true
		}
		log.Error(opMergeEnded, "could not tell whether the merge workspace still carries its merge; its session is left as it is", dlog.Context{"cause": err.Error()})
		return true
	}
	if queued {
		return false
	}
	log.Info(opMergeEnded, "the merge workspace's merge has left every queue; its session is stood down", nil)
	k.deps.StandDowns.StandDownWithheld(record.ID)
	return true
}

// lapseDue takes every lapse that has come: the session is stood down, and
// the roster republished so the hidden section shows the cold cache.
func (k *Keeper) lapseDue(ctx context.Context) {
	now := k.deps.Clock.Now()
	lapsed := false
	for ws, at := range k.deadlines {
		if now.Before(at) {
			continue
		}
		delete(k.deadlines, ws)
		if k.lapse(ctx, ws, now) {
			lapsed = true
		}
	}
	if lapsed {
		k.publish(ctx, k.deps.Log.Global(), opLapse)
	}
}

// lapse stands one workspace's session down at its lapse, read afresh from its
// row so a show or a later projection meanwhile is honored. It answers
// whether the lapse stood.
func (k *Keeper) lapse(ctx context.Context, ws ids.WorkspaceID, now time.Time) bool {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	record, err := k.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			log.Info(opLapse, "a workspace whose cache lapsed is no longer registered", nil)
			return false
		}
		if leaving(ctx, log, opLapse, "reading a workspace whose cache lapsed", err) {
			return false
		}
		log.Error(opLapse, "could not read a workspace whose cache lapsed; its session is not stood down", dlog.Context{"cause": err.Error()})
		return false
	}
	if !record.LetsCacheLapse() || record.CacheLapse == nil {
		k.deps.Telemetry.CountLapse(LapseShownFirst)
		log.Info(opLapse, "the workspace was shown, selected or reopened before its cache lapsed; it keeps running", nil)
		return false
	}
	queued, err := bringup.MergeQueued(ctx, k.deps.DB, record)
	if err != nil {
		if !leaving(ctx, log, opLapse, "reading whether a workspace whose cache lapsed has a merge queued", err) {
			log.Error(opLapse, "could not read whether the workspace has a merge queued; its session is left running", dlog.Context{"cause": err.Error()})
		}
		return false
	}
	if queued {
		k.deps.Telemetry.CountLapse(LapseMergeQueued)
		log.Info(opLapse, "the workspace has a merge queued, so its session and cache are kept until the merge leaves its queue", nil)
		return false
	}
	if !record.CacheLapse.Lapsed(now) {
		k.deadlines[ws] = record.CacheLapse.LapsesAt
		k.deps.Telemetry.CountLapse(LapseMovedLater)
		log.Debug(opLapse, "the recorded lapse moved later; it is re-armed", lapseFields(*record.CacheLapse))
		return false
	}
	k.deps.Telemetry.CountLapse(LapseStoodDown)
	log.Info(opLapse, "the workspace's cache lapsed; its session is stood down", dlog.Merge(lapseFields(*record.CacheLapse), dlog.Context{"hidden": record.InHiddenSection(), "closed": record.Closed}))
	k.deps.StandDowns.StandDownWithheld(ws)
	return true
}

func (k *Keeper) publish(ctx context.Context, log dlog.Logger, op string) {
	if err := k.deps.PublishRegistry(ctx); err != nil {
		if leaving(ctx, log, op, "republishing the roster", err) {
			return
		}
		log.Error(op, "could not republish the roster", dlog.Context{"cause": err.Error()})
	}
}

// shimFailure words one shim call's failure records.
type shimFailure struct {
	// doing names the call for the leaving record ("setting the shim's
	// keep-alive cadence").
	doing string
	// died is the INFO record of a shim that died under the call.
	died string
	// stoodDown is the INFO record of a shim this daemon was already standing
	// down (a kill marks the row closed before its teardown, so the close's
	// last round meets the shim the kill is stopping).
	stoodDown string
	// failed is the ERROR record of any other failure.
	failed string
}

// shimCallFailed records a shim call that failed for a reason other than the
// shim holding no session, which each caller answers in its own terms. A
// SHIM THAT DIED UNDER THE CALL is not this keeper's failure: the death is
// recorded once, by daemon.shimclient.exit, so it is INFO here. Nor is a SHIM
// THIS DAEMON WAS STANDING DOWN (shimclient.ErrStandDownOrdered): the shim did
// what it was told, and the stand-down is recorded where it was ordered. A call the
// daemon's exit cancelled is INFO (leaving). Anything else is an ERROR. It is
// THE ONE triage of a failed shim call in this package
// (TestShimCallFailureTriageHasOneHome).
func shimCallFailed(ctx context.Context, log dlog.Logger, op string, words shimFailure, fields dlog.Context, err error) {
	switch {
	case errors.Is(err, shimclient.ErrStandDownOrdered):
		log.Info(op, words.stoodDown, dlog.WithCause(fields, err))
	case errors.Is(err, shimclient.ErrShimDied):
		log.Info(op, words.died, dlog.WithCause(fields, err))
	case leaving(ctx, log, op, words.doing, err):
	default:
		log.Error(op, words.failed, dlog.WithCause(fields, err))
	}
}

// leaving reports whether ERR is the Run loop's own lifetime ending (the
// daemon is leaving), and records it at INFO when it is: an exit cancels the
// context under whatever was in flight, which is not a fault.
func leaving(ctx context.Context, log dlog.Logger, op, doing string, err error) bool {
	if ctx.Err() == nil {
		return false
	}
	log.Info(op, "the daemon is leaving; "+doing+" ended with it", dlog.Context{"cause": err.Error()})
	return true
}

// lapseOf is the stored shape of a stated projection.
func lapseOf(stated *conversationv1.SessionCacheLapse) wsm.CacheLapse {
	lapse := wsm.CacheLapse{LapsesAt: time.UnixMilli(stated.GetLapsesAtMs()).UTC()}
	if gated := stated.GetGated(); gated != nil {
		tokens := gated.GetContextTokens()
		lapse.GatedTokens = &tokens
	}
	return lapse
}

// gateLapse is the lapse a standing cold gate states: the conversation went
// cold when its cache's lifetime passed after the last request, or at the
// last request itself when the gate is a model switch (a cache is per model,
// so the requested model's never held it). A gate always asks, so the verdict
// is gated at the context the resume re-reads; a gate raised over no context
// is a broken shim.
func gateLapse(cold *conversationv1.SessionCold) wsm.CacheLapse {
	tokens := cold.GetContextTokens()
	if tokens == 0 {
		panic("cachelapse: a cold gate stands over no context; the shim's cold gate asks only above its floor")
	}
	at := cold.GetLastRequestAtMs()
	if lapsed := cold.GetLapsed(); lapsed != nil {
		at += lapsed.GetCacheTtlMs()
	}
	return wsm.CacheLapse{LapsesAt: time.UnixMilli(at).UTC(), GatedTokens: &tokens}
}

// sameLapse reports whether A and B record the same lapse.
func sameLapse(a, b wsm.CacheLapse) bool {
	if !a.LapsesAt.Equal(b.LapsesAt) || (a.GatedTokens == nil) != (b.GatedTokens == nil) {
		return false
	}
	return a.GatedTokens == nil || *a.GatedTokens == *b.GatedTokens
}

func lapseFields(lapse wsm.CacheLapse) dlog.Context {
	fields := dlog.Context{"lapses_at": lapse.LapsesAt, "gated": lapse.GatedTokens != nil}
	if lapse.GatedTokens != nil {
		fields["gated_tokens"] = *lapse.GatedTokens
	}
	return fields
}

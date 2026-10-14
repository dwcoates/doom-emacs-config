// Package cachelapse keeps a HIDDEN workspace's prompt cache until it lapses
// (owner request, 2026-10-13; docs/protobuf-design/hidden-cache-lapse.md).
//
// Hiding a workspace no longer stands its session down at once. Its shim's
// keep-alives are HELD (shim.v1.SetKeepalive), so the cache lapses on its own
// clock, and until then the user can show the workspace again with its cache
// still warm. A held shim states the projected lapse on its session stream
// (conversation.v1.SessionCacheLapse); the Keeper records it on the workspace's
// row, which the roster counts down to, and stands the session down when it
// comes.
//
// ONE LOOP OWNS EVERYTHING. Every edge (a projection stated, a visibility
// change, a session coming up) is queued to Run, which applies them in order
// and owns the one deadline timer, so no two edges for one workspace race and
// no deadline is armed twice.
//
// THE CADENCE IS RECONCILED, NEVER TOLD ONCE. On a hide, a show, and every
// session coming up, the loop reads the workspace's visibility afresh and sets
// the shim's cadence to match: held while hidden, running otherwise. A hide
// that races a bring-up is caught either way, because the hide writes the row
// before it queues the edge and the bring-up installs its client before
// SessionUp queues its own, so whichever edge comes second sees both.
//
// THE DEADLINE OUTLIVES THE DAEMON AND THE SHIM. The projection lives on the
// row, and Run re-arms every recorded one when it starts, so a lapse that
// passes while the daemon is down is taken at once on the next start.
package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// The operations this package records under.
const (
	opProjection = "daemon.cachelapse.projection"
	opCadence    = "daemon.cachelapse.cadence"
	opLapse      = "daemon.cachelapse.lapse"
)

// DB is the slice of the state client the Keeper reads and writes.
type DB interface {
	ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error)
	Workspace(ctx context.Context, id wsm.WorkspaceID) (wsm.Workspace, error)
	SetCacheLapse(ctx context.Context, id wsm.WorkspaceID, lapse wsm.CacheLapse) (bool, error)
}

// Keepalives is one serving shim's keep-alive cadence.
type Keepalives interface {
	// Hold sends no keep-alive from now on; the shim states the lapse.
	Hold(ctx context.Context) error
	// Run keeps the cache warm again.
	Run(ctx context.Context) error
}

// ErrNoSession is a shim's answer that it holds no session, so there is no
// cadence to set. Not a fault: the session is not up yet, or already gone.
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
	Shim       func(ws ids.WorkspaceID) (Keepalives, bool)
	StandDowns StandDowns
	// PublishRegistry republishes the roster's durable half, which is where
	// a recorded projection and a lapse reach the hidden section.
	PublishRegistry func(context.Context) error
	Clock           clock.Clock
	Log             dlog.Surfaces
}

// edge is one queued event.
type edge struct {
	ws ids.WorkspaceID
	// lapse is a projection the shim stated; nil for a reconcile.
	lapse *conversationv1.SessionCacheLapse
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
}

// New builds a Keeper. A missing collaborator is a wiring defect.
func New(deps Deps) (*Keeper, error) {
	switch {
	case deps.DB == nil:
		return nil, errors.New("cachelapse: the keeper needs a state client")
	case deps.Shim == nil:
		return nil, errors.New("cachelapse: the keeper needs the shim fleet")
	case deps.StandDowns == nil:
		return nil, errors.New("cachelapse: the keeper needs the withheld stand-downs")
	case deps.PublishRegistry == nil:
		return nil, errors.New("cachelapse: the keeper needs the roster republish")
	case deps.Clock == nil:
		return nil, errors.New("cachelapse: the keeper needs a clock")
	case deps.Log == nil:
		return nil, errors.New("cachelapse: the keeper needs log surfaces")
	}
	return &Keeper{deps: deps, wake: make(chan struct{}, 1), deadlines: map[ids.WorkspaceID]time.Time{}}, nil
}

// OnCacheLapse takes a projection a held shim stated
// (sessionwatcher.CacheLapseSink). It never blocks.
func (k *Keeper) OnCacheLapse(ws ids.WorkspaceID, lapse *conversationv1.SessionCacheLapse) {
	k.enqueue(edge{ws: ws, lapse: lapse, why: "the shim stated its projected lapse"})
}

// VisibilityChanged is a hide or a show of WS, already recorded: its cadence
// is reconciled. It never blocks.
func (k *Keeper) VisibilityChanged(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "the workspace was hidden or shown"})
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
		var due <-chan time.Time
		if armed {
			due = k.deps.Clock.After(max(0, next.Sub(k.deps.Clock.Now())))
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
	return nil
}

func (k *Keeper) take() []edge {
	k.mu.Lock()
	defer k.mu.Unlock()
	queued := k.queue
	k.queue = nil
	return queued
}

// earliest answers the first armed deadline.
func (k *Keeper) earliest() (time.Time, bool) {
	var next time.Time
	armed := false
	for _, at := range k.deadlines {
		if !armed || at.Before(next) {
			next, armed = at, true
		}
	}
	return next, armed
}

func (k *Keeper) apply(ctx context.Context, e edge) {
	if e.lapse != nil {
		k.record(ctx, e.ws, e.lapse)
		return
	}
	k.reconcile(ctx, e.ws, e.why)
}

// record stores a stated projection on a hidden workspace's row and arms it.
// A workspace shown again meanwhile takes none: the store refuses it in the
// statement that reads the visibility.
func (k *Keeper) record(ctx context.Context, ws ids.WorkspaceID, stated *conversationv1.SessionCacheLapse) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	lapse := lapseOf(stated)
	fields := lapseFields(lapse)
	if stated.GetAfterLapse() == nil {
		// A VERDICT THIS SCHEMA REVISION DOES NOT NAME is read as no gate:
		// the row is not struck through, and showing the workspace again
		// raises whatever gate the shim then judges.
		log.Info(opProjection, "the projected lapse states a verdict this daemon does not name; it is read as ungated", fields)
	}
	recorded, err := k.deps.DB.SetCacheLapse(ctx, ws, lapse)
	if err != nil {
		if leaving(ctx, log, opProjection, "recording the projected cache lapse", err) {
			return
		}
		log.Error(opProjection, "could not record the hidden workspace's projected cache lapse", withCause(fields, err))
		return
	}
	if !recorded {
		log.Info(opProjection, "a projected cache lapse arrived for a workspace that is not hidden; it is dropped", fields)
		delete(k.deadlines, ws)
		return
	}
	k.deadlines[ws] = lapse.LapsesAt
	log.Info(opProjection, "recorded the hidden workspace's projected cache lapse", fields)
	k.publish(ctx, log, opProjection)
}

// reconcile sets WS's cadence to match its visibility: held while hidden,
// running otherwise. A workspace not hidden has no deadline.
func (k *Keeper) reconcile(ctx context.Context, ws ids.WorkspaceID, why string) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "edge": why})
	record, err := k.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			log.Info(opCadence, "the workspace is no longer registered; no cadence is set", nil)
			delete(k.deadlines, ws)
			return
		}
		if leaving(ctx, log, opCadence, "reading the workspace's visibility", err) {
			return
		}
		log.Error(opCadence, "could not read the workspace's visibility; its keep-alive cadence is not set", dlog.Context{"cause": err.Error()})
		return
	}
	hidden := record.InHiddenSection()
	if !hidden {
		delete(k.deadlines, ws)
	}
	shim, ok := k.deps.Shim(ws)
	if !ok {
		log.Debug(opCadence, "no session is served for the workspace; no cadence is set", dlog.Context{"hidden": hidden})
		return
	}
	set, cadence := shim.Run, "run"
	if hidden {
		set, cadence = shim.Hold, "hold"
	}
	fields := dlog.Context{"cadence": cadence}
	if err := set(ctx); err != nil {
		if errors.Is(err, ErrNoSession) {
			log.Debug(opCadence, "the shim holds no session yet; the session's coming up sets its cadence", fields)
			return
		}
		if leaving(ctx, log, opCadence, "setting the shim's keep-alive cadence", err) {
			return
		}
		log.Error(opCadence, "could not set the shim's keep-alive cadence", withCause(fields, err))
		return
	}
	log.Info(opCadence, "set the shim's keep-alive cadence to match the workspace's visibility", fields)
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
	if !record.InHiddenSection() || record.CacheLapse == nil {
		log.Info(opLapse, "the workspace was shown again before its cache lapsed; it keeps running", nil)
		return false
	}
	if !record.CacheLapse.Lapsed(now) {
		k.deadlines[ws] = record.CacheLapse.LapsesAt
		log.Debug(opLapse, "the recorded lapse moved later; it is re-armed", lapseFields(*record.CacheLapse))
		return false
	}
	log.Info(opLapse, "the hidden workspace's cache lapsed; its session is stood down", lapseFields(*record.CacheLapse))
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

func lapseFields(lapse wsm.CacheLapse) dlog.Context {
	fields := dlog.Context{"lapses_at": lapse.LapsesAt, "gated": lapse.GatedTokens != nil}
	if lapse.GatedTokens != nil {
		fields["gated_tokens"] = *lapse.GatedTokens
	}
	return fields
}

func withCause(fields dlog.Context, err error) dlog.Context {
	out := dlog.Context{"cause": err.Error()}
	for key, value := range fields {
		out[key] = value
	}
	return out
}

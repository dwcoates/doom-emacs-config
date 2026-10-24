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
// THE CADENCE IS RECONCILED, NEVER TOLD ONCE. On a hide, a show, a close, a
// reopen, a selection moving onto or off the workspace, and every session
// coming up, the loop reads the workspace's row afresh and sets the shim's
// cadence to match (cadenceFor): held while hidden and not selected
// (wsm.Workspace.KeepaliveHeld), a last round while closed, running
// otherwise. A hide
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
	opCadence    = "daemon.cachelapse.cadence"
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
}

// Keepalives is one serving shim's keep-alive cadence.
type Keepalives interface {
	// Hold sends no keep-alive from now on; the shim states the lapse.
	Hold(ctx context.Context) error
	// Run keeps the cache warm again.
	Run(ctx context.Context) error
	// LastRound sends the one keep-alive already scheduled, then holds; the
	// shim states the lapse once the round is spent.
	LastRound(ctx context.Context) error
	// Beat sends one keep-alive now, leaving the cadence as it is, and
	// answers the shim's judgment of it. ErrNoSession when the shim holds no
	// session; a *BeatSubmitFailed when the submission failed.
	Beat(ctx context.Context) (*shimv1.SendKeepaliveSuccess, error)
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
	case deps.ColdGate == nil:
		return nil, errors.New("cachelapse: the keeper needs the standing cold gates")
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
	k.enqueue(edge{ws: ws, why: "the session parked at its cold gate"})
}

// MergeLeftQueue is WS's entry leaving its merge queue: a hidden merge
// workspace whose merge left every queue is stood down. It never blocks.
func (k *Keeper) MergeLeftQueue(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "its merge left its queue"})
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
		log.Info(opProjection, "a cache lapse arrived for a workspace that is neither hidden nor closed; it is dropped", fields)
		delete(k.deadlines, ws)
		return false
	}
	k.deadlines[ws] = lapse.LapsesAt
	log.Info(opProjection, "recorded the workspace's cache lapse", fields)
	k.publish(ctx, log, opProjection)
	return true
}

// cadence is the keep-alive cadence a workspace's row asks of its shim.
type cadence string

const (
	cadenceHold      cadence = "hold"
	cadenceLastRound cadence = "last_round"
	cadenceRun       cadence = "run"
)

// cadenceFor answers the cadence RECORD asks for. THE HIDE WINS over the
// close: a hidden workspace never beats, closed or not, so closing it grants
// no last round. A hidden workspace the user is looking at runs
// (wsm.Workspace.KeepaliveHeld).
func cadenceFor(record wsm.Workspace) cadence {
	switch {
	case record.KeepaliveHeld():
		return cadenceHold
	case record.Closed:
		return cadenceLastRound
	default:
		return cadenceRun
	}
}

// set is the shim call that asks for C.
func (c cadence) set(shim Keepalives) func(context.Context) error {
	switch c {
	case cadenceHold:
		return shim.Hold
	case cadenceLastRound:
		return shim.LastRound
	case cadenceRun:
		return shim.Run
	default:
		panic(fmt.Sprintf("cachelapse: cadence %q names no shim call", string(c)))
	}
}

// reconcile sets WS's cadence to match its row (cadenceFor). A workspace
// whose cache is not left to lapse has no deadline.
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
		return
	}
	want := cadenceFor(record)
	fields := dlog.Context{"cadence": string(want), "hidden": record.InHiddenSection(), "selected": record.Current, "closed": record.Closed}
	shim, ok := k.deps.Shim(ws)
	if !ok {
		log.Debug(opCadence, "no session is served for the workspace; no cadence is set", fields)
		return
	}
	if err := want.set(shim)(ctx); err != nil {
		if errors.Is(err, ErrNoSession) {
			log.Debug(opCadence, "the shim holds no session yet; the session's coming up sets its cadence", fields)
			return
		}
		// The next session's coming up sets the cadence again.
		shimCallFailed(ctx, log, opCadence, shimFailure{
			doing:     "setting the shim's keep-alive cadence",
			died:      "the shim died under the cadence call; the death is recorded by daemon.shimclient.exit, and the next session's coming up sets its cadence",
			stoodDown: "the shim was being stood down by this daemon under the cadence call; the next session's coming up sets its cadence",
			failed:    "could not set the shim's keep-alive cadence",
		}, fields, err)
		return
	}
	log.Info(opCadence, "set the shim's keep-alive cadence to match the workspace's row", fields)
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
		log.Info(opLapse, "the workspace was shown, selected or reopened before its cache lapsed; it keeps running", nil)
		return false
	}
	if !record.CacheLapse.Lapsed(now) {
		k.deadlines[ws] = record.CacheLapse.LapsesAt
		log.Debug(opLapse, "the recorded lapse moved later; it is re-armed", lapseFields(*record.CacheLapse))
		return false
	}
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

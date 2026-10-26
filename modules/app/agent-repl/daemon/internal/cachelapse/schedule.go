package cachelapse

import (
	"context"
	"errors"
	"slices"
	"time"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is the daemon's keep-alive drive (owner ruling, 2026-10-26). A
// shim runs no keep-alive timer: the Keeper times every beat from the cache
// clock the workspace's turns keep durably, asks the shim for it, stands an
// idle session down once it has gone out, and restores a parked one a minute
// before its next beat.
//
// THE SHIM STAYS THE JUDGE OF THE BEAT. Whether a keep-alive may go out -- a
// turn holding the main agent, live detached work its rewind would kill, a
// cache already lapsed -- is the shim's answer (shim.v1.SendKeepalive), and
// the daemon only acts on it: a busy or working session is asked again when
// it falls free, and a lapsed cache is beaten no more until a turn moves its
// clock.

// The keep-alive drive's timings.
const (
	// KeepaliveInterval is how long after the cache clock a beat is due: 52
	// minutes (ruled 2026-09-17), inside the subscription's one-hour cache
	// window with an eight-minute margin against a slow turn.
	KeepaliveInterval = 52 * time.Minute
	// RestoreLead is how long before its beat a parked session is restored.
	// A shim that takes longer than this to start is a defect of its own
	// (owner ruling, 2026-10-26).
	RestoreLead = time.Minute
	// BeatRetry is how long a beat whose submission failed, or whose shim
	// could not be asked, waits before it is asked again.
	BeatRetry = 30 * time.Second
	// MaxRestores is how many parked sessions are brought back at once. The
	// beats are staggered by the workspaces' own histories; this bounds the
	// rest, so a burst never starts dozens of vendor children together.
	MaxRestores = 3
)

// queuedRestore is one parked session waiting for a restore slot.
type queuedRestore struct {
	ws ids.WorkspaceID
	// due is the beat the restore is for, or the instant it was asked for out
	// of turn: the queue is served earliest first.
	due time.Time
}

// schedule arms WS's next beat, and its restore when its session is parked,
// from the workspace's row and session read afresh. A workspace that owes no
// beat has neither armed.
func (k *Keeper) schedule(ctx context.Context, ws ids.WorkspaceID, why string) {
	delete(k.beats, ws)
	delete(k.restores, ws)
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "edge": why})
	due, parked, ok := k.beatDue(ctx, log, ws)
	if !ok {
		return
	}
	k.beats[ws] = due
	if parked {
		k.restores[ws] = due.Add(-RestoreLead)
	}
	log.Debug(opDrive, "armed the workspace's next keep-alive", dlog.Context{"due": due, "parked": parked})
}

// beatDue answers when WS's next keep-alive is due and whether its session is
// parked by the keep-alive cycle, false when it owes none. Every reason it
// owes none is recorded at DEBUG: they are the ordinary states of a fleet.
func (k *Keeper) beatDue(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) (time.Time, bool, bool) {
	record, err := k.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			k.forget(ws)
			return time.Time{}, false, false
		}
		if !leaving(ctx, log, opDrive, "reading the workspace to time its keep-alive", err) {
			log.Error(opDrive, "could not read the workspace; its keep-alive is not armed", dlog.Context{"cause": err.Error()})
		}
		return time.Time{}, false, false
	}
	none := func(why string) (time.Time, bool, bool) {
		log.Debug(opDrive, "the workspace owes no keep-alive", dlog.Context{"why": why})
		return time.Time{}, false, false
	}
	if record.KeepaliveHeld() {
		return none("it is hidden and nobody is looking at it; its cache lapses on its own clock")
	}
	if record.LastTurnEndedAt == nil {
		return none("no turn has ended, so there is no cache to keep")
	}
	ended := *record.LastTurnEndedAt
	// THE CACHE CLOCK, NOT THE TURNS': a keep-alive writes the cache as a
	// turn does, and timed from the turns alone the beat after one was already
	// overdue (2026-10-26).
	clock := *record.CacheClock()
	if sent, ok := k.sent[ws]; ok && sent.After(clock) {
		clock = sent
	}
	if record.Closed && (record.Closure == nil || !clock.Before(record.Closure.At)) {
		return none("it is closed and its last keep-alive is spent")
	}
	if lapsed, ok := k.lapsedAt[ws]; ok && !ended.After(lapsed) {
		return none("its cache lapsed and no turn has moved its clock since; the cold gate owns the next request")
	}
	if _, gated := k.deps.ColdGate(ws); gated {
		return none("its session stands at the cold gate")
	}
	runs, _, err := bringup.OnlyVisible(ctx, k.deps.DB, []wsm.Workspace{record})
	if err != nil {
		if !leaving(ctx, log, opDrive, "reading whether the workspace runs", err) {
			log.Error(opDrive, "could not read whether the workspace runs; its keep-alive is not armed", dlog.Context{"cause": err.Error()})
		}
		return time.Time{}, false, false
	}
	if len(runs) == 0 {
		return none("it is withheld, so it runs no session")
	}
	session, found, err := k.deps.DB.Session(ctx, ws)
	if err != nil {
		if !leaving(ctx, log, opDrive, "reading the workspace's session to time its keep-alive", err) {
			log.Error(opDrive, "could not read the workspace's session; its keep-alive is not armed", dlog.Context{"cause": err.Error()})
		}
		return time.Time{}, false, false
	}
	if !found {
		return none("it has no session")
	}
	if session.Terminal != nil && !session.KeepaliveCycled() {
		return none("its session is not live: " + session.Terminal.Kind)
	}
	due := clock.Add(KeepaliveInterval)
	engaged := session.LastEngagementAt
	if engaged.IsZero() {
		engaged = session.StartedAt
	}
	if k.deps.IdleCutoff > 0 && engaged.Add(k.deps.IdleCutoff).Before(due) {
		return none("it will be idle past the cutoff before the beat is due, and the idle sweep puts it to sleep")
	}
	return due, session.KeepaliveCycled(), true
}

// beatsDue sends every keep-alive that has come due.
func (k *Keeper) beatsDue(ctx context.Context) {
	now := k.deps.Clock.Now()
	for ws, at := range k.beats {
		if now.Before(at) {
			continue
		}
		delete(k.beats, ws)
		k.sendBeat(ctx, ws, now)
	}
}

// sendBeat sends WS's keep-alive, re-read first so a turn that moved the clock in
// the meantime re-arms it instead.
func (k *Keeper) sendBeat(ctx context.Context, ws ids.WorkspaceID, now time.Time) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	due, parked, ok := k.beatDue(ctx, log, ws)
	if !ok {
		delete(k.restores, ws)
		return
	}
	if due.After(now) {
		k.beats[ws] = due
		if parked {
			k.restores[ws] = due.Add(-RestoreLead)
		}
		log.Debug(opDrive, "a turn moved the cache clock; the keep-alive is re-armed", dlog.Context{"due": due})
		return
	}
	fields := dlog.Context{"due": due}
	shim, served := k.deps.Shim(ws)
	if !served {
		if parked {
			// THE RESTORE IS LATE OR STILL RUNNING: the beat goes out when its
			// session comes up (SessionUp schedules it, already due).
			log.Info(opDrive, "the keep-alive is due and its session is still parked; it goes out once the session is restored", fields)
			k.requestRestore(ctx, ws, due)
			return
		}
		log.Debug(opDrive, "the keep-alive is due and no session is served; the session's coming up sends it", fields)
		return
	}
	answer, err := shim.Beat(ctx)
	if err != nil {
		var failed *BeatSubmitFailed
		switch {
		case errors.Is(err, ErrNoSession):
			log.Debug(opDrive, "the shim holds no session; the session's coming up sends the keep-alive", fields)
			return
		case errors.As(err, &failed):
			log.Error(opDrive, "the keep-alive's submission failed; it is asked again shortly",
				dlog.WithCause(dlog.Merge(fields, dlog.Context{"retry_in": BeatRetry.String()}), err))
		default:
			shimCallFailed(ctx, log, opDrive, shimFailure{
				doing:     "asking the shim for its keep-alive",
				died:      "the shim died under the keep-alive; the death is recorded by daemon.shimclient.exit, and the next session's coming up sends it",
				stoodDown: "the shim was being stood down by this daemon under the keep-alive; the next session's coming up sends it",
				failed:    "could not ask the shim for its keep-alive; it is asked again shortly",
			}, fields, err)
		}
		k.beats[ws] = now.Add(BeatRetry)
		return
	}
	switch {
	case answer.GetSent() != nil:
		log.Info(opDrive, "sent the workspace's keep-alive; its session is stood down until the next one once it is free", fields)
		// THE BEAT MOVES THE CACHE CLOCK, and it is recorded before the
		// stand-down, whose edges re-time the next beat from that clock.
		k.sent[ws] = now
		if err := k.deps.DB.SetLastKeepalive(ctx, ws, now); err != nil && !leaving(ctx, log, opDrive, "recording the keep-alive that was sent", err) {
			log.Error(opDrive, "could not record the keep-alive that was sent; this daemon times the next beat from it, a successor from the turn before it",
				dlog.WithCause(fields, err))
		}
		// AN HOUR'S GRACE AFTER THE WORK, NOT ONLY AFTER THE TURN (owner
		// ruling, 2026-10-26): the user prompts with no shim start for about
		// an interval after the session last fell free. A beat that comes
		// sooner, because detached work ended after the turn, goes out and
		// leaves the shim up; the next one stands it down.
		if free, ok := k.freeSince[ws]; ok && now.Sub(free) < KeepaliveInterval {
			log.Info(opDrive, "the session fell free less than an interval ago; the shim stays up until the next keep-alive",
				dlog.Merge(fields, dlog.Context{"free_since": free}))
		} else {
			k.deps.CycleDown(ws, now)
		}
		k.beats[ws] = now.Add(KeepaliveInterval)
	case answer.GetSkipped().GetMainAgentBusy() != nil:
		log.Info(opDrive, "the main agent is busy, which keeps the cache warm itself; the next beat is timed when it falls free",
			dlog.Merge(fields, dlog.Context{"detail": answer.GetSkipped().GetMainAgentBusy().GetDetail()}))
	case answer.GetSkipped().GetDetachedWorkLive() != nil:
		log.Info(opDrive, "detached work is live, and the rewind a keep-alive owes would kill it; the keep-alive goes out when it ends",
			dlog.Merge(fields, dlog.Context{"live_items": answer.GetSkipped().GetDetachedWorkLive().GetLiveItems()}))
	case answer.GetSkipped().GetCacheLapsed() != nil:
		record, err := k.deps.DB.Workspace(ctx, ws)
		if err == nil && record.LastTurnEndedAt != nil {
			k.lapsedAt[ws] = *record.LastTurnEndedAt
		}
		log.Info(opDrive, "the cache had already lapsed; no keep-alive is sent until a turn moves its clock",
			dlog.Merge(fields, dlog.Context{"lapsed_at_ms": answer.GetSkipped().GetCacheLapsed().GetLapsedAtMs()}))
	default:
		log.Error(opDrive, "the shim answered the keep-alive with a judgment this daemon does not name", dlog.Merge(fields, dlog.Context{
			"invariant_violation": "every SendKeepaliveSuccess arm is handled",
		}))
	}
}

// restoresDue queues every parked session whose restore has come due, ALL
// of them before any slot is filled, so the earliest beats take the slots
// whatever order the armed restores are walked in.
func (k *Keeper) restoresDue(ctx context.Context) {
	now := k.deps.Clock.Now()
	due := false
	for ws, at := range k.restores {
		if now.Before(at) {
			continue
		}
		delete(k.restores, ws)
		beat := at.Add(RestoreLead)
		if armed, ok := k.beats[ws]; ok {
			beat = armed
		}
		if k.enqueueRestore(ws, beat) {
			due = true
		}
	}
	if due {
		k.pump(ctx)
	}
}

// restoreAsked queues WS's restore out of turn, now, when its session is
// parked by the keep-alive cycle.
func (k *Keeper) restoreAsked(ctx context.Context, ws ids.WorkspaceID) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	session, found, err := k.deps.DB.Session(ctx, ws)
	if err != nil {
		if !leaving(ctx, log, opRestore, "reading the session a reader asked to restore", err) {
			log.Error(opRestore, "could not read the session a reader asked to restore; it is not restored", dlog.Context{"cause": err.Error()})
		}
		return
	}
	if !found || !session.KeepaliveCycled() {
		log.Debug(opRestore, "a restore was asked for a session the keep-alive cycle has not parked; nothing to do", nil)
		return
	}
	k.requestRestore(ctx, ws, k.deps.Clock.Now())
}

// requestRestore puts WS in line for a restore slot and fills the free
// slots.
func (k *Keeper) requestRestore(ctx context.Context, ws ids.WorkspaceID, due time.Time) {
	if k.enqueueRestore(ws, due) {
		k.pump(ctx)
	}
}

// enqueueRestore puts WS in line for a restore slot, earliest beat first,
// unless it is in line or in flight already. It answers whether it did.
func (k *Keeper) enqueueRestore(ws ids.WorkspaceID, due time.Time) bool {
	if k.restoring[ws] || slices.ContainsFunc(k.queued, func(q queuedRestore) bool { return q.ws == ws }) {
		return false
	}
	k.queued = append(k.queued, queuedRestore{ws: ws, due: due})
	slices.SortStableFunc(k.queued, func(a, b queuedRestore) int { return a.due.Compare(b.due) })
	return true
}

// pump starts the earliest queued restores while a slot is free. A workspace
// whose session is no longer parked -- revived by a look or a prompt in the
// meantime -- is dropped from the line.
func (k *Keeper) pump(ctx context.Context) {
	for len(k.restoring) < MaxRestores && len(k.queued) > 0 {
		next := k.queued[0]
		k.queued = k.queued[1:]
		log := k.deps.Log.Global().With(dlog.Context{"workspace": string(next.ws)})
		if _, served := k.deps.Shim(next.ws); served {
			log.Debug(opRestore, "the parked session is already served again; it is not restored", nil)
			continue
		}
		session, found, err := k.deps.DB.Session(ctx, next.ws)
		if err != nil {
			if !leaving(ctx, log, opRestore, "reading the parked session to restore it", err) {
				log.Error(opRestore, "could not read the parked session; it is not restored", dlog.Context{"cause": err.Error()})
			}
			continue
		}
		if !found || !session.KeepaliveCycled() {
			log.Debug(opRestore, "the session is no longer parked by the keep-alive cycle; it is not restored", nil)
			continue
		}
		k.restoring[next.ws] = true
		ws := next.ws
		log.Info(opRestore, "restoring the parked session ahead of its keep-alive", dlog.Context{
			"due": next.due, "in_flight": len(k.restoring), "waiting": len(k.queued),
		})
		k.deps.Restore(ws, func(err error) {
			k.enqueue(edge{ws: ws, kind: edgeRestored, err: err, why: "the parked session's restore ended"})
		})
	}
}

// restored takes a restore's end: its slot is freed for the next in line. A
// restore that failed is asked again shortly, while its beat still stands;
// one that came up is scheduled by its own SessionUp.
func (k *Keeper) restored(ctx context.Context, ws ids.WorkspaceID, err error) {
	delete(k.restoring, ws)
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	switch {
	case err == nil:
		log.Info(opRestore, "the parked session is back up for its keep-alive", nil)
	case leaving(ctx, log, opRestore, "restoring the parked session", err):
	default:
		log.Error(opRestore, "the parked session could not be restored; it is asked again shortly",
			dlog.Context{"cause": err.Error(), "retry_in": BeatRetry.String()})
		if _, armed := k.beats[ws]; armed {
			k.restores[ws] = k.deps.Clock.Now().Add(BeatRetry)
		}
	}
	k.pump(ctx)
}

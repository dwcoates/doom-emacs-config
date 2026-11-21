package cachelapse

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// EVERY LIVE SESSION HAS EXACTLY ONE STAND-DOWN OWNER (owner request,
// 2026-11-20). An idle shim is not kept between keep-alives (owner ruling,
// 2026-10-26, drain/cycle.go), but each stand-down used to be asked on one
// edge only: the cycle after a beat, the cold park the moment a mark was
// written, the visit on a switch away the UI saw. A session that came up
// with its cache already known cold -- a settings pick's revival, a
// handover's adoption -- passed none of those edges, owed no beat, and
// stayed up for hours: at 18:18 on 2026-11-20, 24 of 44 live shims were
// idle, among them agent-update-no-toolcalls, whose keeper said "owes no
// keep-alive" at each of three handovers and asked nothing.
//
// So the owner is decided on EVERY RECONCILE, from the row read afresh
// (beatPlanFor calls own): every edge the keeper takes, every session coming
// up (a handover's adoption included), and every daemon start. Nothing of it
// lives only in memory, so a successor decides the same owner. The owners:
//
//   - the settings visit, while a pick's visit stands (wsm.Workspace.
//     SettingsVisitAt) and no turn has run since it: stood down at once
//     unless in view, so a pick through the agent API, which no switch away
//     ever ends, ends by the same rule;
//   - the keep-alive cycle, while a beat is owed: stood down after it;
//   - the cache's lapse, while a hidden or closed workspace's lapse is
//     armed;
//   - the idle sweep, when the session will be idle past its cutoff first;
//   - the cold park, when no beat is owed for any other reason: stood down
//     at once.
//
// A session is LEFT UP, and owned by none, while the user is looking at it
// or its remote control is on; the edge that ends either reconciles it
// again. A busy session is the drain's to wait on: it never stands one down
// under a turn (drain's TEARDOWN NEVER INTERRUPTS ruling), and asks again as
// the session falls free.

const (
	opOwner = "daemon.cachelapse.stand_down_owner"
	opVisit = "daemon.cachelapse.settings_visit"
)

// The stand-down owners, and the reasons a session is left up.
const (
	ownerVisit = "settings_visit"
	ownerCycle = "keepalive_cycle"
	ownerLapse = "cache_lapse"
	ownerSweep = "idle_sweep"
	ownerCold  = "cold_park"

	upInView        = "in_view"
	upRemoteControl = "remote_control"
)

// noBeat is the kind of reason a workspace owes no keep-alive, which names
// the owner of its session's stand-down (own).
type noBeat int

const (
	// noBeatCold is a cache with nothing warm to keep: the cold park's.
	noBeatCold noBeat = iota
	// noBeatSweep is a session the idle sweep puts to sleep first.
	noBeatSweep
	// noBeatNoSession is a workspace with no live session on record.
	noBeatNoSession
)

// standing is the stand-down owner a session was last given, or why it was
// left up, as this keeper last recorded it.
type standing struct {
	owner string
	up    bool
}

// own gives RECORD's session its one stand-down owner and asks the owner's
// stand-down when the owner is the visit or the cold park, whose stand-down
// is taken at once. OWED is a beat owed; NB and WHY are why none is owed
// otherwise. QUEUED is a merge standing in a queue.
//
// THE ASK IS MADE ON EVERY RECONCILE, not only when the owner changes: the
// drain settles an ask it finds in view without parking, and the next edge
// must ask again. The drain coalesces repeated asks.
func (k *Keeper) own(ctx context.Context, log dlog.Logger, record wsm.Workspace, queued, owed bool, nb noBeat, why string) {
	ws := record.ID
	if _, served := k.deps.Shim(ws); !served {
		k.endStoodDownVisit(ctx, log, record)
		k.disown(log, ws)
		return
	}
	if record.Current {
		k.setStanding(log, ws, standing{owner: upInView, up: true}, "the user is looking at it")
		return
	}
	remote, err := k.deps.DB.RemoteControl(ctx, ws)
	if err != nil {
		if !leaving(ctx, log, opOwner, "reading the workspace's remote-control mode", err) {
			log.Error(opOwner, "could not read the workspace's remote-control mode; its stand-down owner is left as it was", dlog.Context{"cause": err.Error()})
		}
		return
	}
	if remote.On {
		k.setStanding(log, ws, standing{owner: upRemoteControl, up: true}, "remote control is on; claude.ai reaches the session only through its shim")
		return
	}
	if since := record.SettingsVisitAt; since != nil {
		ended, turned := record.TurnedSince(*since)
		if !turned {
			k.setStanding(log, ws, standing{owner: ownerVisit}, "a settings pick started its shim and no turn has run since")
			k.deps.VisitDown(ws, *since)
			return
		}
		k.endVisit(ctx, log, ws, *since, "a real turn ran since the settings pick", dlog.Context{"turn_ended_at": ended})
	}
	_, lapseArmed := k.deadlines[ws]
	switch {
	case owed:
		k.setStanding(log, ws, standing{owner: ownerCycle}, "it owes keep-alives; it is stood down after the next one")
	case record.LetsCacheLapse() && !queued && lapseArmed:
		k.setStanding(log, ws, standing{owner: ownerLapse}, "its cache is left to lapse, and the lapse stands it down")
	case nb == noBeatSweep:
		k.setStanding(log, ws, standing{owner: ownerSweep}, why)
	case nb == noBeatNoSession:
		k.disown(log, ws)
	default:
		k.setStanding(log, ws, standing{owner: ownerCold}, why)
		k.parkCold(ws, k.deps.Clock.Now(), why)
	}
}

// setStanding records WS's stand-down owner, or why it is left up, at INFO
// when it changed and at DEBUG while it stands.
func (k *Keeper) setStanding(log dlog.Logger, ws ids.WorkspaceID, next standing, why string) {
	was, had := k.owners[ws]
	fields := dlog.Context{"why": why}
	if had {
		fields["was"] = was.owner
	}
	if had && was == next {
		log.Debug(opOwner, "the session's stand-down owner stands", dlog.Merge(fields, dlog.Context{"owner": next.owner}))
		return
	}
	k.owners[ws] = next
	if next.up {
		log.Info(opOwner, "the session is left up, owned by no stand-down", dlog.Merge(fields, dlog.Context{"left_up": next.owner}))
		return
	}
	log.Info(opOwner, "the session's stand-down owner changed", dlog.Merge(fields, dlog.Context{"owner": next.owner}))
}

// disown drops WS's stand-down owner: no session of it is live here.
func (k *Keeper) disown(log dlog.Logger, ws ids.WorkspaceID) {
	was, had := k.owners[ws]
	if !had {
		return
	}
	delete(k.owners, ws)
	log.Info(opOwner, "the session is no longer live here; it needs no stand-down owner", dlog.Context{"was": was.owner})
}

// endStoodDownVisit ends RECORD's settings visit once the session the visit
// started has been stood down: its park is later than the pick. The park the
// pick revived is earlier, so a visit noted before its own bring-up stands.
func (k *Keeper) endStoodDownVisit(ctx context.Context, log dlog.Logger, record wsm.Workspace) {
	since := record.SettingsVisitAt
	if since == nil {
		return
	}
	session, found, err := k.deps.DB.Session(ctx, record.ID)
	if err != nil {
		if !leaving(ctx, log, opVisit, "reading the session a settings visit started", err) {
			log.Error(opVisit, "could not read the session a settings visit started; the visit stands", dlog.Context{"since": *since, "cause": err.Error()})
		}
		return
	}
	if !found || session.Terminal == nil || !session.Terminal.At.After(*since) {
		return
	}
	k.endVisit(ctx, log, record.ID, *since, "the session the settings pick started was stood down", dlog.Context{"terminal": session.Terminal.Kind})
}

// endVisit ends WS's settings visit begun at SINCE, for WHY. A visit a later
// pick began stands.
func (k *Keeper) endVisit(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, since time.Time, why string, fields dlog.Context) {
	fields = dlog.Merge(fields, dlog.Context{"since": since, "why": why})
	ended, err := k.deps.DB.EndSettingsVisit(ctx, ws, since)
	if err != nil {
		if !leaving(ctx, log, opVisit, "ending the settings visit", err) {
			log.Error(opVisit, "could not end the settings visit; it still owns the session's stand-down", dlog.WithCause(fields, err))
		}
		return
	}
	if !ended {
		log.Debug(opVisit, "a later settings pick's visit stands", fields)
		return
	}
	log.Info(opVisit, "the settings visit ended", fields)
}

// SettingsVisitStarted is a settings pick starting WS's shim at AT
// (promptqueue.Deps.SettingsStarted), told before the bring-up starts: the
// visit is written on the row, so the session's coming up finds it, whichever
// daemon serves it then. A visit that cannot be written is recorded at
// ERROR, and its session is owned as any other session is.
func (k *Keeper) SettingsVisitStarted(ws ids.WorkspaceID, at time.Time) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	if err := k.deps.DB.SetSettingsVisit(context.Background(), ws, at); err != nil {
		log.Error(opVisit, "could not record the settings visit; its session's stand-down is owned as though no pick had started it", dlog.Context{"since": at, "cause": err.Error()})
		return
	}
	log.Info(opVisit, "a settings pick started the workspace's shim; the visit owns its stand-down until it ends", dlog.Context{"since": at})
	k.enqueue(edge{ws: ws, why: "a settings pick started its shim"})
}

// RemoteControlChanged is WS's remote-control mode changed, already
// recorded: its session's owner is decided again. It never blocks.
func (k *Keeper) RemoteControlChanged(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "its remote-control mode changed"})
}

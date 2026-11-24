package cachelapse

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwhy"
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
//     once the workspace's shim hold ends (ownCold).
//
// WHICHEVER OWNS IT, AN IDLE SESSION IS PARKED ONCE ITS SHIM HOLD ENDS (owner
// rulings, 2026-11-24; Deps.Hold, wsm.ShimHold): the roster's workspace
// resource allocation says until when an idle shim stays up (the selection
// and work for as long as they stand, an unread result for fifteen minutes
// after its turn, a workspace the user left for as long as its page), and the
// cycle, the lapse and the sweep park at it too (ownIdle, parkIdle), so no
// session waits for its cache's lapse or the sweep's cutoff of hours.
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
	ownerVisit = string(sessionwhy.OwnerVisit)
	ownerCycle = string(sessionwhy.OwnerCycle)
	ownerLapse = string(sessionwhy.OwnerLapse)
	ownerSweep = string(sessionwhy.OwnerSweep)
	ownerCold  = string(sessionwhy.OwnerCold)

	upInView        = string(sessionwhy.OwnerInView)
	upRemoteControl = string(sessionwhy.OwnerRemoteControl)
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
// stand-down when the owner is the visit, or the cold park or the idle park
// once its grace is out. BEAT is the next keep-alive owed, zero when none is;
// NB and WHY are why none is owed otherwise. QUEUED is a merge standing in a
// queue.
//
// THE ASK IS MADE ON EVERY RECONCILE, not only when the owner changes: the
// drain settles an ask it finds in view without parking, and the next edge
// must ask again. The drain coalesces repeated asks.
func (k *Keeper) own(ctx context.Context, log dlog.Logger, record wsm.Workspace, queued bool, beat time.Time, nb noBeat, why string) {
	ws := record.ID
	// AN IDLE PARK'S INSTANT IS ARMED ONLY WHILE ITS OWNER OWNS THE SESSION:
	// ownCold and ownIdle re-arm it from the row.
	armed, wasArmed := k.idleDue[ws]
	delete(k.idleDue, ws)
	k.deps.Why.ClearParkDue(ws)
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
		worked, did := record.WorkedSince(*since)
		if !did {
			k.setStanding(log, ws, standing{owner: ownerVisit}, "a settings pick started its shim and no turn has run since")
			k.deps.VisitDown(ws, *since)
			return
		}
		k.endVisit(ctx, log, ws, *since, "a real turn ran since the settings pick", dlog.Context{"worked_at": worked})
	}
	_, lapseArmed := k.deadlines[ws]
	switch {
	case !beat.IsZero():
		k.setStanding(log, ws, standing{owner: ownerCycle}, "it owes keep-alives; it is parked once idle, and restored before each beat")
		k.ownIdle(ctx, log, record, beat, armed, wasArmed)
	case record.LetsCacheLapse() && !queued && lapseArmed:
		k.setStanding(log, ws, standing{owner: ownerLapse}, "its cache is left to lapse, and the lapse stands it down")
		k.parkIdle(ctx, log, record, armed, wasArmed)
	case nb == noBeatSweep:
		k.setStanding(log, ws, standing{owner: ownerSweep}, why)
		k.parkIdle(ctx, log, record, armed, wasArmed)
	case nb == noBeatNoSession:
		k.disown(log, ws)
	default:
		k.ownCold(ctx, log, record, why, armed, wasArmed)
	}
}

// ownCold gives RECORD's session to the cold park, which takes it once the
// workspace's shim hold ends (parkWaits), and arms that instant until then
// (idleParksDue).
//
// THE SAME HOLD AS THE IDLE PARK'S (owner rulings, 2026-11-24): the shim
// stays up for as long as the workspace's resource allocation holds it. A session that came up cold for a reason -- a new workspace's
// first prompt on its way, a cold gate's answer re-opening the session the
// user asked for -- is not parked under it, since that prompt's delivery is
// work; one adopted or started after hours idle is parked at once. The
// instant is on the row (wsm.Workspace.LastWorkAt), so a successor arms the
// same one. The parks the keeper takes the moment it
// learns the cache cold (a gate raised, a beat judged lapsed) are not this
// one, and take no grace. ARMED is the instant armed before this decision,
// when WASARMED, so its wait is recorded at INFO only when it moves.
func (k *Keeper) ownCold(ctx context.Context, log dlog.Logger, record wsm.Workspace, why string, armed time.Time, wasArmed bool) {
	since, ok := k.idleSince(ctx, log, record)
	if !ok {
		return
	}
	k.setStanding(log, record.ID, standing{owner: ownerCold}, why)
	if k.parkWaits(log, record.ID, since, armed, wasArmed) {
		return
	}
	k.parkCold(record.ID, k.deps.Clock.Now(), why, false)
}

// ownIdle parks RECORD's session, which owes the keep-alive due at BEAT,
// once its shim hold ends (parkWaits), and arms that instant until then
// (idleParksDue). The keeper restores the
// session a minute before the beat (RestoreLead), and the beat's own
// stand-down parks it again once the vendor answers (settleAnswer), so:
//
//   - a session whose grace runs out only inside the beat's restore lead is
//     left to the beat's stand-down, never parked a minute before its own
//     restore;
//   - a session the keeper restored for its beat, or whose keep-alive's answer
//     is awaited, is never parked under that beat.
//
// ARMED and WASARMED are ownCold's.
func (k *Keeper) ownIdle(ctx context.Context, log dlog.Logger, record wsm.Workspace, beat time.Time, armed time.Time, wasArmed bool) {
	ws := record.ID
	restoreAt := beat.Add(-RestoreLead)
	if _, awaiting := k.answering[ws]; awaiting {
		return
	}
	since, ok := k.idleSince(ctx, log, record)
	if !ok {
		return
	}
	// A HOLD THAT ENDS ONLY INSIDE THE BEAT'S RESTORE LEAD IS LEFT TO THE
	// BEAT'S STAND-DOWN, and one that holds while its cause stands is never
	// parked under it.
	if hold := k.deps.Hold(ws); hold.Indefinite || !hold.Until.Before(restoreAt) {
		return
	}
	if k.parkWaits(log, ws, since, armed, wasArmed) {
		return
	}
	if !k.deps.Clock.Now().Before(restoreAt) {
		return
	}
	log.Debug(opOwner, "the session has been idle past its grace; it is parked until a minute before its next keep-alive", dlog.Context{"idle_since": since, "beat": beat})
	k.deps.IdleDown(ws, since)
}

// parkIdle parks RECORD's session, owed no keep-alive, once its shim hold
// ends, and arms that instant until then (idleParksDue): the lapse's and the
// sweep's sessions are parked at the same hold as every other's.
// ARMED and WASARMED are ownCold's.
func (k *Keeper) parkIdle(ctx context.Context, log dlog.Logger, record wsm.Workspace, armed time.Time, wasArmed bool) {
	since, ok := k.idleSince(ctx, log, record)
	if !ok || k.parkWaits(log, record.ID, since, armed, wasArmed) {
		return
	}
	log.Debug(opOwner, "the session has been idle past its grace and owes no keep-alive; it is parked", dlog.Context{"idle_since": since})
	k.deps.IdleDown(record.ID, since)
}

// idleSince answers when RECORD's workspace last did real work
// (wsm.Workspace.IdleSince): the one durable instant every park measures its
// grace from, which no session start, keep-alive or bounce moves, so a
// successor reads the same one. False when the session is gone (disowned) or
// cannot be read (recorded at ERROR).
func (k *Keeper) idleSince(ctx context.Context, log dlog.Logger, record wsm.Workspace) (time.Time, bool) {
	ws := record.ID
	_, found, err := k.deps.DB.Session(ctx, ws)
	if err != nil {
		if !leaving(ctx, log, opOwner, "reading the workspace's session", err) {
			log.Error(opOwner, "could not read the workspace's session; its idle park is not armed", dlog.Context{"cause": err.Error()})
		}
		return time.Time{}, false
	}
	if !found {
		k.disown(log, ws)
		return time.Time{}, false
	}
	return record.IdleSince(), true
}

// parkWaits arms WS's idle park at the instant its resource allocation stops
// holding its idle shim up (Deps.Hold), when that is still to come, answering
// true; a hold for as long as what decided it stands arms nothing, since the
// roster tells the keeper when it ends (ShimHoldChanged). SINCE is when the
// session last did real work, for the record. The wait is recorded at INFO
// only when the armed instant moved from ARMED (WASARMED).
func (k *Keeper) parkWaits(log dlog.Logger, ws ids.WorkspaceID, since, armed time.Time, wasArmed bool) bool {
	hold := k.deps.Hold(ws)
	now := k.deps.Clock.Now()
	if hold.Indefinite {
		log.Debug(opOwner, "the session's resource allocation holds it up while what decided it stands; its park waits", dlog.Context{"idle_since": since})
		return true
	}
	due := hold.Until
	if !now.Before(due) {
		return false
	}
	if !wasArmed || !armed.Equal(due) {
		log.Info(opOwner, "the session's resource allocation holds it up until its hold ends; its park waits", dlog.Context{"idle_since": since, "due": due})
	}
	k.idleDue[ws] = due
	k.deps.Why.SetParkDue(ws, due)
	return true
}

// idleParksDue decides again every session whose cold or idle park has come
// due: a turn or an engagement since may have moved it.
func (k *Keeper) idleParksDue(ctx context.Context) {
	now := k.deps.Clock.Now()
	for ws, at := range k.idleDue {
		if now.Before(at) {
			continue
		}
		delete(k.idleDue, ws)
		k.deps.Why.ClearParkDue(ws)
		k.schedule(ctx, ws, "its idle park came due")
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
	k.deps.Why.SetOwner(ws, sessionwhy.Owner(next.owner))
	if next.up {
		log.Info(opOwner, "the session is left up, owned by no stand-down", dlog.Merge(fields, dlog.Context{"left_up": next.owner}))
		return
	}
	log.Info(opOwner, "the session's stand-down owner changed", dlog.Merge(fields, dlog.Context{"owner": next.owner}))
}

// disown drops WS's stand-down owner: no session of it is live here.
func (k *Keeper) disown(log dlog.Logger, ws ids.WorkspaceID) {
	k.deps.Why.Forget(ws)
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

// ShimHoldChanged is the roster's word that WS's shim hold moved
// (sidebar.ShimHoldSink): the selection left it, it was read, its work ended.
// Its stand-down is reconciled against the new hold.
func (k *Keeper) ShimHoldChanged(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "its shim hold moved"})
}

// RemoteControlChanged is WS's remote-control mode changed, already
// recorded: its session's owner is decided again. It never blocks.
func (k *Keeper) RemoteControlChanged(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, why: "its remote-control mode changed"})
}

package cachelapse

import (
	"context"
	"errors"
	"io/fs"
	"slices"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// This file is the daemon's keep-alive drive (owner ruling, 2026-10-26). A
// shim runs no keep-alive timer: the Keeper times every beat from the cache
// clock the workspace's turns keep durably, asks the shim for it, stands an
// idle session down once it has gone out, and restores a parked one a minute
// before its next beat.
//
// THE SHIM STAYS THE JUDGE OF THE BEAT. Whether a keep-alive may go out -- a
// turn holding the main agent, live detached work its rewind would kill, a
// cache already lapsed, which includes one whose last request left its
// context uncached (shim.engine.cold.leftContextCached) -- is the shim's answer (shim.v1.SendKeepalive), and
// the daemon only acts on it: a busy or working session is asked again when
// it falls free (a busy one an interval on at the latest, since not every
// busy slot is one the daemon sees end), and a lapsed cache is beaten no more
// until a turn moves its clock.

// The keep-alive drive's timings.
const (
	// KeepaliveInterval is how long after the cache clock a beat is due: 52
	// minutes (ruled 2026-09-17), inside the subscription's one-hour cache
	// window with an eight-minute margin against a slow turn. Deps.Interval
	// carries it, or a test run's compressed cadence (IntervalFromEnv).
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

// withinGraceRule is why a beat sent while the workspace's resource
// allocation holds its shim up leaves the shim up: the beat's record and a
// keep-live run's answer both state it.
const withinGraceRule = "the workspace's resource allocation holds its shim up; the shim stays up until its hold ends"

// beatPlan is when a workspace's next beat is due, and what it beats.
type beatPlan struct {
	// due is the earlier of the two conversations' next beats.
	due time.Time
	// sessionParked reports the workspace's session parked, so its shim is
	// restored before the beat.
	sessionParked bool
	// bound reports that the conversation the session runs owes beats.
	bound bool
	// parked reports that the workspace's parked conversation owes beats,
	// and none of its keep-alives is out (parked.go).
	parked bool
	// bringUp is what the session's own bring-up holds (wsm.Session
	// .BringUpHold), as the plan was made.
	bringUp wsm.ShimHold
	// idleSince is when the workspace last did real work, as the record the
	// plan was read from says (wsm.Workspace.IdleSince).
	idleSince time.Time
}

// beatDue answers when WS's next keep-alive is due and whether its session is
// parked, false when it owes none (beatPlanFor).
func (k *Keeper) beatDue(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) (time.Time, bool, bool) {
	plan, ok := k.beatPlanFor(ctx, log, ws)
	return plan.due, plan.sessionParked, ok
}

// beatPlanFor answers WS's next beat, false when it owes none. ONE PARK,
// WHATEVER STOOD IT DOWN: a session the idle sweep parked owes none by the
// cutoff below, and a withheld one by its withholding, so no reader here asks
// which stand-down it was. A run at its break-even owes none (runcap.go).
//
// THE DECISION IS RECORDED AT INFO WHEN IT CHANGES, at DEBUG while it stands
// (owesNone): every reason it owes none is an ordinary state of a fleet, but
// at DEBUG alone it never reached the disk, and 76 sessions marked cold on
// 2026-11-14 left no record of why the keeper stopped beating them.
//
// BOTH CONVERSATIONS ARE KEPT WARM IN TANDEM (owner request, 2026-11-10). A
// workspace that took the queue bypass mode parks the conversation it does
// not run, with its own cache clock (wsm.QueueBypass.ParkedClockAt); the
// beat is due at the earlier of the two conversations' next beats, and each
// beat beats both. Everything that holds a workspace's beats (a hide, a
// close, a cold gate, a withholding, the idle cutoff) holds both.
func (k *Keeper) beatPlanFor(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) (beatPlan, bool) {
	record, err := k.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			k.forget(ws)
			delete(k.noBeat, ws)
			return beatPlan{}, false
		}
		k.undelivered(ctx, log, opDrive, ws, time.Time{}, "reading the workspace to time its keep-alive", logsite.Msg("could not read the workspace; its keep-alive is not armed"), nil, err)
		return beatPlan{}, false
	}
	// queued is read below; a reason found before it reads none.
	queued := false
	none := func(nb noBeat, why string) (beatPlan, bool) {
		// A BEAT THAT IS NOT OWED IS NOT OWED EARLY EITHER.
		delete(k.early, ws)
		k.owesNone(ctx, log, record, why)
		k.own(ctx, log, record, queued, time.Time{}, nb, why)
		return beatPlan{}, false
	}
	// A WORKSPACE WHOSE DIRECTORY IS GONE OWES NO KEEP-ALIVE: no session can
	// ever run there again, so a parked one can never be restored for its
	// beat. Armed, its restore failed on every daemon boot with an ERROR in
	// the keeper, the feed resolver and the footer each (31 landed
	// workspaces whose worktrees were reaped with their landing, at the
	// 2026-11-13 14:42 deploy, one last beat owed each since 2026-10-31).
	// Only a stat that says "not exist" is gone; any other failure is said.
	if _, err := k.deps.Stat(record.Dir); err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			k.forget(ws)
			return none(noBeatCold, "its directory is gone, so no session can ever run for it")
		}
		log.Error(opDrive, "could not tell whether the workspace's directory exists; its keep-alive is not armed",
			dlog.Context{"dir": record.Dir, "cause": err.Error()})
		return beatPlan{}, false
	}
	// A WORKSPACE IN A MERGE QUEUE IS KEPT WARM (owner rulings, 2026-10-26
	// and 2026-11-11): its entry can wait for hours, and the merge's prompts
	// run on its session, so neither a hide nor a close lets its cache lapse
	// while the entry stands, for QueuedIdleFactor idle cutoffs
	// (bringup.IdleCutoff, read below). Warm is the cache, not the shim: a
	// sync merge's workspace is parked the moment its entry is in line
	// (drain.Controller.MergeQueuedDown), and its beats go on through the
	// restore before each.
	queuedAt, inQueue, err := bringup.MergeQueuedAt(ctx, k.deps.DB, record)
	queued = inQueue
	if err != nil {
		k.undelivered(ctx, log, opDrive, ws, time.Time{}, "reading whether the workspace has a merge queued", logsite.Msg("could not read whether the workspace has a merge queued; its keep-alive is not armed"), nil, err)
		return beatPlan{}, false
	}
	if _, gated := k.deps.ColdGate(ws); gated {
		return none(noBeatCold, "its session stands at the cold gate")
	}
	if why, capped := k.runCapped(ctx, log, ws, record, queued); capped {
		return none(noBeatCold, why)
	}
	// A COLLAPSED SECTION WITHHOLDS NO KEEP-ALIVE (wsm.Workspace.KeptWarm).
	if !record.KeptWarm(queued) {
		return none(noBeatCold, "it is hidden, so it runs no session")
	}
	session, found, err := k.deps.DB.Session(ctx, ws)
	if err != nil {
		k.undelivered(ctx, log, opDrive, ws, time.Time{}, "reading the workspace's session to time its keep-alive", logsite.Msg("could not read the workspace's session; its keep-alive is not armed"), nil, err)
		return beatPlan{}, false
	}
	if !found {
		return none(noBeatNoSession, "it has no session")
	}
	if session.Terminal != nil && !session.Parked() {
		return none(noBeatNoSession, "its session is not live: "+session.Terminal.Kind)
	}
	// A CLOSED OR BACKLOGGED WORKSPACE OWES EACH CONVERSATION ONE LAST BEAT
	// (owner rulings, 2026-10-17 and 2026-11-24): none once that
	// conversation's clock has passed the close, or the backlog while nobody
	// is looking at it (wsm.Workspace.KeepaliveHeld). Its cache then lapses on
	// its own clock.
	spent := func(clock time.Time) bool {
		switch {
		case queued:
			return false
		case record.Closed:
			return record.Closure == nil || !clock.Before(record.Closure.At)
		case record.KeepaliveHeld():
			return record.BackloggedAt == nil || !clock.Before(*record.BackloggedAt)
		default:
			return false
		}
	}
	plan := beatPlan{sessionParked: session.Parked()}
	// THE BOUND CONVERSATION, timed from THE CACHE CLOCK, NOT THE TURNS': a
	// keep-alive writes the cache as a turn does, and timed from the turns
	// alone the beat after one was already overdue (2026-10-26). A CACHE
	// KNOWN COLD IS NEVER BEATEN (owner ruling, 2026-10-29): its cold gate was
	// raised or answered, or its shim judged it lapsed, and no turn has
	// closed since. Passing the gate makes no request of its own, so the
	// first keep-alive is owed an interval after the next real turn's end.
	if clock := record.CacheClock(); clock != nil && !record.CacheKnownCold() {
		at := *clock
		if sent, ok := k.sent[ws]; ok && sent.After(at) {
			at = sent
		}
		if !spent(at) {
			plan.bound, plan.due = true, at.Add(k.deps.Interval)
		}
	}
	if due, ok := k.parkedDue(ctx, log, ws, spent); ok {
		plan.parked = true
		if !plan.bound || due.Before(plan.due) {
			plan.due = due
		}
	}
	if !plan.bound && !plan.parked {
		return none(noBeatCold, "neither of its conversations has a warm cache to keep, or each one's last keep-alive is spent")
	}
	plan.due = k.pulledForward(ws, plan.due)
	// A KEEP-LIVE RUN MAKES THE BEAT DUE NOW (keeplive.go): the same plan,
	// asked for at once, so every reason it owes none still holds.
	if k.forcing(ws) {
		plan.due = k.deps.Clock.Now()
	}
	plan.idleSince = record.IdleSince()
	plan.bringUp = session.BringUpHold(record.LastTurnEndedAt, k.deps.Clock.Now())
	if k.deps.IdleCutoff > 0 && bringup.IdleHorizon(k.deps.IdleCutoff, plan.idleSince, queuedAt, queued).Before(plan.due) {
		return none(noBeatSweep, "it will be idle past the cutoff before the beat is due, and the idle sweep puts it to sleep")
	}
	k.owesAgain(ctx, log, record)
	k.own(ctx, log, record, queued, plan.due, noBeatCold, "")
	return plan, true
}

// owesNone records that RECORD's workspace owes no keep-alive for WHY: at
// INFO when the decision changed since the last one, at DEBUG when it stands
// as it was. The decision goes on the row when the row does not already
// carry it (wsm.SetKeepaliveOwing), so a beat due while it stands is no miss
// to any daemon (missed).
func (k *Keeper) owesNone(ctx context.Context, log dlog.Logger, record wsm.Workspace, why string) {
	owesNone := logsite.Msg("the workspace owes no keep-alive")
	ws := record.ID
	fields := dlog.Context{"why": why}
	if record.KeepaliveOwedNoneAt == nil {
		k.recordOwing(ctx, log, ws, false, fields)
	}
	if was, ok := k.noBeat[ws]; ok && was == why {
		log.Log(dlog.LevelDebug, opDrive, owesNone, fields)
		return
	}
	k.noBeat[ws] = why
	log.Log(dlog.LevelInfo, opDrive, owesNone, fields)
}

// owesAgain records at INFO that RECORD's workspace owes keep-alives again
// after a decision that it owed none, this daemon's or a predecessor's on the
// row; a workspace that owed them all along is no change.
func (k *Keeper) owesAgain(ctx context.Context, log dlog.Logger, record wsm.Workspace) {
	ws := record.ID
	was, inMemory := k.noBeat[ws]
	delete(k.noBeat, ws)
	if record.KeepaliveOwedNoneAt == nil && !inMemory {
		return
	}
	fields := dlog.Context{}
	if inMemory {
		fields["owed_none_because"] = was
	}
	if record.KeepaliveOwedNoneAt != nil {
		fields["owed_none_since"] = *record.KeepaliveOwedNoneAt
		k.recordOwing(ctx, log, ws, true, fields)
	}
	log.Info(opDrive, "the workspace owes keep-alives again", fields)
}

// recordOwing writes whether WS owes keep-alives (OWING) to its row, at the
// clock's now. A write that fails is recorded at ERROR: a lapse judged
// against the row may then call a beat that was never owed a miss.
//
// A JOINING SUCCESSOR WRITES NOTHING: its handle is read-only until its
// promotion, and the incumbent, still the sole writer, records its own
// decisions. The workspace is withheld instead, and the loop reconciles it
// the moment the handle writes (promotion.go), which takes the decision
// afresh from the row and writes it then.
func (k *Keeper) recordOwing(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, owing bool, fields dlog.Context) {
	if k.withheldWhileJoining(log, "whether the workspace owes keep-alives", dlog.Merge(fields, dlog.Context{"owing": owing}), func() { k.withholdOwing(ws) }) {
		return
	}
	err := k.deps.DB.SetKeepaliveOwing(ctx, ws, owing, k.deps.Clock.Now())
	// A ROW THAT IS GONE OWES NOTHING: the workspace was closed or forgotten
	// between the edge that drove this decision and its write, so there is no
	// lapse left to judge against it.
	if errors.Is(err, wsm.ErrNotFound) {
		log.Info(opDrive, "the workspace's row is gone, so whether it owes keep-alives is recorded nowhere",
			dlog.WithCause(dlog.Merge(fields, dlog.Context{"owing": owing}), err))
		return
	}
	if err != nil && !leaving(ctx, log, opDrive, "recording whether the workspace owes keep-alives", err) {
		log.Error(opDrive, "could not record whether the workspace owes keep-alives; a lapse judged against the row may misjudge a miss",
			dlog.WithCause(dlog.Merge(fields, dlog.Context{"owing": owing}), err))
	}
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
//
// IT IS THE ONE KEEP-ALIVE SUBROUTINE: the scheduled beat (beatsDue) and the
// keep-live verb (beginKeepLive) both call it, and every record it makes
// names which one did (`trigger`).
func (k *Keeper) sendBeat(ctx context.Context, ws ids.WorkspaceID, now time.Time) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "trigger": k.triggerOf(ws)})
	plan, ok := k.beatPlanFor(ctx, log, ws)
	if !ok {
		delete(k.restores, ws)
		if why, known := k.noBeat[ws]; known {
			k.keepLiveFailed(ws, "owes_no_keepalive", owesNoKeepalive(why), false)
		} else {
			k.keepLiveFailed(ws, "registry_unreadable", registryUnreadable("the keeper could not read what the workspace owes; the cause is recorded above"), false)
		}
		return
	}
	due, parked := plan.due, plan.sessionParked
	if due.After(now) {
		k.beats[ws] = due
		if parked {
			k.restores[ws] = due.Add(-RestoreLead)
		}
		log.Debug(opDrive, "a turn moved the cache clock; the keep-alive is re-armed", dlog.Context{"due": due})
		return
	}
	fields := dlog.Context{"due": due}
	observe := func(outcome string) { k.deps.Telemetry.ObserveBeat(ws, outcome, now.Sub(due)) }
	shim, served := k.deps.Shim(ws)
	if !served {
		if parked {
			observe(BeatOutcomeSessionParked)
			// THE RESTORE IS LATE OR STILL RUNNING: the beat goes out when its
			// session comes up (SessionUp schedules it, already due).
			log.Info(opDrive, "the keep-alive is due and its session is still parked; it goes out once the session is restored", fields)
			k.keepLiveRestoring(ws, now)
			k.requestRestore(ctx, ws, due)
			return
		}
		observe(BeatOutcomeNotServed)
		log.Debug(opDrive, "the keep-alive is due and no session is served; the session's coming up sends it", fields)
		return
	}
	// EACH BEAT BEATS BOTH CONVERSATIONS: the parked one's keep-alive goes
	// out beside the session's, on a query of its own (parked.go).
	if plan.parked {
		k.beatParked(ctx, log, ws, shim)
	}
	if !plan.bound {
		k.beatAnswered(ws)
		log.Info(opDrive, "the parked conversation's keep-alive is due and the running conversation owes none", fields)
		k.keepLiveFailed(ws, "owes_no_keepalive", owesNoKeepalive("the conversation the session runs has no warm cache to keep; only its parked conversation's keep-alive was sent"), true)
		return
	}
	answer, err := shim.Beat(ctx)
	if err != nil {
		var failed *BeatSubmitFailed
		switch {
		case errors.Is(err, ErrNoSession):
			observe(BeatOutcomeNoSession)
			log.Debug(opDrive, "the shim holds no session; the session's coming up sends the keep-alive", fields)
			return
		case errors.As(err, &failed):
			observe(BeatOutcomeSubmitFailed)
			k.undelivered(ctx, log, opDrive, ws, due, "submitting the keep-alive", logsite.Msg("the keep-alive's submission failed; it is asked again shortly"),
				dlog.Merge(fields, dlog.Context{"retry_in": BeatRetry.String()}), err)
			k.keepLiveFailed(ws, "beat_failed", beatFailed(failed.Detail), true)
		default:
			observe(BeatOutcomeShimUnreachable)
			const failed = "could not ask the shim for its keep-alive; it is asked again shortly"
			if shimCallFailed(ctx, log, opDrive, shimFailure{
				doing:     "asking the shim for its keep-alive",
				died:      logsite.Msg("the shim died under the keep-alive; the death is recorded by daemon.shimclient.exit, and the next session's coming up sends it"),
				stoodDown: logsite.Msg("the shim was being stood down by this daemon under the keep-alive; the next session's coming up sends it"),
				failed:    logsite.Msg(failed),
			}, fields, err) {
				k.raiseUndelivered(ctx, log, ws, due, failed+": "+err.Error())
			}
			k.keepLiveFailed(ws, "beat_failed", beatFailed(err.Error()), true)
		}
		k.beats[ws] = now.Add(BeatRetry)
		return
	}
	// A BEAT PULLED FORWARD STANDS UNTIL THE SHIM ANSWERS ONE (outage.go),
	// except one that waits on live detached work, which goes out early
	// still once that work ends.
	if answer.GetSkipped().GetDetachedWorkLive() == nil {
		k.beatAnswered(ws)
	}
	switch {
	case answer.GetSent() != nil:
		observe(BeatOutcomeSent)
		log.Info(opDrive, "sent the workspace's keep-alive", fields)
		k.delivered(ctx, log, ws)
		// THE BEAT MOVES THE CACHE CLOCK, and it is recorded before the
		// stand-down, whose edges re-time the next beat from that clock.
		k.sent[ws] = now
		// A RUN A TURN ENDED IS RECORDED BEFORE THIS BEAT STARTS A NEW ONE,
		// and this beat's spend is read from the mark (runspend.go).
		k.endRun(ctx, log, ws, wsm.RunEndingBeat)
		k.markRunSpend(ws)
		if err := k.deps.DB.SetLastKeepalive(ctx, ws, now); err != nil && !leaving(ctx, log, opDrive, "recording the keep-alive that was sent", err) {
			log.Error(opDrive, "could not record the keep-alive that was sent; this daemon times the next beat from it, a successor from the turn before it",
				dlog.WithCause(fields, err))
		}
		// THE RESOURCE ALLOCATION DECIDES WHETHER THE SHIM STAYS UP (owner
		// rulings, 2026-11-24; Deps.Hold): a beat that goes out while the
		// workspace's hold stands leaves the shim up; the next edge after the
		// hold ends stands it down. The keep-alive's own turn is no work, so it
		// never extends the hold.
		idleSince := plan.idleSince
		withinGrace := plan.bringUp.Later(k.deps.Hold(ws)).HoldsAt(now)
		// A KEEP-LIVE RUN AWAITS THE ANSWER EITHER WAY, and a session it
		// started is stood down after it whatever its grace.
		running, standDown := k.keepLiveSent(ws, withinGrace)
		switch {
		case running:
			k.awaitAnswer(log, ws, now, fields, !standDown)
		case withinGrace:
			log.Log(dlog.LevelInfo, opDrive, logsite.Msg(withinGraceRule), dlog.Merge(fields, dlog.Context{"idle_since": idleSince}))
		default:
			// NEVER UNDER ITS OWN KEEP-ALIVE (answer.go): the stand-down waits
			// for the vendor's answer.
			k.awaitAnswer(log, ws, now, fields, false)
		}
		k.beats[ws] = now.Add(k.deps.Interval)
	case answer.GetSkipped().GetMainAgentBusy() != nil:
		observe(BeatOutcomeMainAgentBusy)
		// NOT EVERY BUSY SLOT ENDS IN A FREE EDGE. A turn the daemon serves
		// does, and re-times the beat from its own end; the shim's own
		// keep-alive turn and a vendor turn it never serves hold the slot
		// unseen. So the beat is armed one interval on as well, as the
		// shim's own cadence armed a skipped beat, and a free edge re-times
		// it sooner.
		k.beats[ws] = now.Add(k.deps.Interval)
		log.Info(opDrive, "the main agent is busy, which keeps the cache warm itself; the next beat falls one interval on, or is timed when it falls free",
			dlog.Merge(fields, dlog.Context{"detail": answer.GetSkipped().GetMainAgentBusy().GetDetail(), "next": k.beats[ws]}))
		k.keepLiveBusy(ws, busyOf(answer.GetSkipped()))
	case answer.GetSkipped().GetDetachedWorkLive() != nil:
		observe(BeatOutcomeDetachedWorkLive)
		log.Info(opDrive, "detached work is live, and the rewind a keep-alive owes would kill it; the keep-alive goes out when it ends",
			dlog.Merge(fields, dlog.Context{"live_items": answer.GetSkipped().GetDetachedWorkLive().GetLiveItems()}))
		k.keepLiveBusy(ws, busyOf(answer.GetSkipped()))
	case answer.GetSkipped().GetCacheLapsed() != nil:
		observe(BeatOutcomeCacheLapsed)
		lapsed := time.UnixMilli(answer.GetSkipped().GetCacheLapsed().GetLapsedAtMs())
		log.Info(opDrive, "the cache had already lapsed; no keep-alive is sent until a turn moves its clock",
			dlog.Merge(fields, dlog.Context{"lapsed_at_ms": lapsed.UnixMilli()}))
		k.missed(ctx, log, ws, due, lapsed)
		k.keepLiveFailed(ws, "cache_lapsed", &agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_CacheLapsed{
			CacheLapsed: &agentreplv1.KeepLiveWorkspaceCacheLapsed{LapsedAtMs: lapsed.UnixMilli()},
		}}, false)
		if k.markCold(ctx, ws, "the shim judged the cache lapsed") != nil {
			// UNMARKED, the beat would be asked again at once and judged lapsed
			// again: it is asked again shortly instead, and markCold recorded
			// why at ERROR. The retry's own mark parks the session.
			k.beats[ws] = now.Add(BeatRetry)
			return
		}
		k.parkCold(ws, now, "the shim judged the cache lapsed", false)
	default:
		observe(BeatOutcomeUnnamedJudgment)
		log.Error(opDrive, "the shim answered the keep-alive with a judgment this daemon does not name", dlog.Merge(fields, dlog.Context{
			"invariant_violation": "every SendKeepaliveSuccess arm is handled",
		}))
		k.keepLiveFailed(ws, "beat_failed", beatFailed("the shim answered the keep-alive with a judgment this daemon does not name"), true)
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
// whose session is no longer parked -- revived by a prompt in the meantime --
// is dropped from the line.
func (k *Keeper) pump(ctx context.Context) {
	for len(k.restoring) < MaxRestores && len(k.queued) > 0 {
		next := k.queued[0]
		k.queued = k.queued[1:]
		log := k.deps.Log.Global().With(dlog.Context{"workspace": string(next.ws)})
		if _, served := k.deps.Shim(next.ws); served {
			log.Debug(opRestore, "the parked session is already served again; it is not restored", nil)
			k.keepLiveRetimed(next.ws)
			continue
		}
		session, found, err := k.deps.DB.Session(ctx, next.ws)
		if err != nil {
			if !leaving(ctx, log, opRestore, "reading the parked session to restore it", err) {
				log.Error(opRestore, "could not read the parked session; it is not restored", dlog.Context{"cause": err.Error()})
			}
			continue
		}
		if !found || !session.Parked() {
			log.Debug(opRestore, "the session is no longer parked; it is not restored", nil)
			k.keepLiveRetimed(next.ws)
			continue
		}
		k.restoring[next.ws] = true
		ws := next.ws
		log.Info(opRestore, "restoring the parked session ahead of its keep-alive", dlog.Context{
			"due": next.due, "in_flight": len(k.restoring), "waiting": len(k.queued),
		})
		k.keepLiveRestoreBegan(ws)
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
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "trigger": k.triggerOf(ws)})
	if err != nil {
		k.keepLiveFailed(ws, "start_failed", startFailed(err.Error()), false)
	}
	switch {
	case err == nil:
		log.Info(opRestore, "the parked session is back up for its keep-alive", nil)
	case leaving(ctx, log, opRestore, "restoring the parked session", err):
	case errors.Is(err, dlog.ErrWorkspaceDirGone):
		// A SESSION WHOSE WORKTREE IS GONE CAN NEVER BE RESTORED, so its
		// keep-alive is retired rather than asked again: a landed workspace's
		// last beat, owed since 2026-10-31 to a worktree reaped with the
		// landing, was retried every 30s for two weeks, each try an ERROR
		// here, in the feed and in the footer (2026-11-13,
		// response-bubble-auto-select). An open workspace whose directory is
		// gone is settled by the missing-directory step, not here.
		log.Info(opRestore, "the workspace's directory is gone, so its parked session can never be restored; its keep-alive is retired",
			dlog.Context{"cause": err.Error()})
		k.forget(ws)
	case k.deps.HandedOver(err):
		// A HANDOVER IS THE ROLLOUT'S ORDERING, NOT A FAILURE: a deploy hands
		// every workspace to the successor, whose own keeper restores it, and
		// this daemon's beat for it is refused as transferring_away until the
		// exit ends it (2026-11-15 20:07, an ERROR in the deploy intake). The
		// retry stays armed, because a handover that is withdrawn gives the
		// workspace back to this daemon, and its beat is owed again.
		log.Info(opRestore, "the parked session is served by another daemon now, so this one does not restore it; it is asked again shortly",
			dlog.Context{"cause": err.Error(), "retry_in": BeatRetry.String()})
		k.retryRestore(ws)
	default:
		due := k.beats[ws]
		k.undelivered(ctx, log, opRestore, ws, due, "restoring the parked session", logsite.Msg("the parked session could not be restored for its keep-alive; it is asked again shortly"),
			dlog.Context{"retry_in": BeatRetry.String()}, err)
		if ctx.Err() == nil {
			k.retryRestore(ws)
		}
	}
	k.pump(ctx)
}

// retryRestore asks WS's restore again after BeatRetry, while its beat still
// stands.
func (k *Keeper) retryRestore(ws ids.WorkspaceID) {
	if _, armed := k.beats[ws]; armed {
		k.restores[ws] = k.deps.Clock.Now().Add(BeatRetry)
	}
}

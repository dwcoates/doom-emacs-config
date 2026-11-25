package drain

import (
	"context"
	"errors"
	"time"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwhy"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// opCycle is the operation a keep-alive cycle's stand-down records under.
const opCycle = "daemon.drain.keepalive_cycle"

// AN IDLE SHIM IS NOT KEPT BETWEEN KEEP-ALIVES (owner ruling, 2026-10-26).
// The daemon drives every keep-alive (cachelapse.Keeper), and once a beat has
// gone out a session nobody is using is stood down until a minute before the
// next one, so the ~350MB its shim and vendor child hold is free while it
// would only have waited. It is stood down exactly as the idle sweep stands
// one down -- the same gate, the same hibernate directive and graceful
// KillSession -- leaving the one park (wsm.TerminalParked), which every view draws as the
// live idle session it stands in for, with its own detail on the record.
//
// THE SAME RULES AS EVERY STAND-DOWN, AND FOUR OF ITS OWN. It never
// interrupts a turn or live detached work: a busy session stays PENDING and is
// asked again on the daemon's own edges (RetryWithheld), never by a poll. And
// it stands nothing down for the workspace the user is looking at, for a
// backlogged or closed one with no merge queued (the cache keeper's lapse owns
// those), for one whose
// remote control is on (claude.ai reaches it only through its shim; owner
// ruling, 2026-10-31), or once a real turn has run since the beat -- that turn
// is engagement, and the next beat decides again. A workspace waiting on its
// own sync merge does not wait for this cycle: its enqueue parks it
// (MergeQueuedDown, below).

// THE SAME STAND-DOWN ENDS A SETTINGS VISIT (owner request, 2026-10-29). A
// model, permission-mode or effort pick on a workspace with no shim starts
// one (promptqueue's revivalSettings); once nobody is looking at the
// workspace, the cache keeper asks for its stand-down through this path
// (VisitDown, cachelapse.Keeper.own), under the same rules, unless a real
// turn has run since the pick started it. A pick through the agent API is
// ended the same way, with no switch away. It leaves the one park, so the
// workspace is drawn as the idle session it was.

// opVisit is the operation a settings visit's stand-down records under.
const opVisit = "daemon.drain.settings_visit"

// visitReason is a settings visit's stand-down reason.
var visitReason = standDownReason{
	op:      opVisit,
	detail:  "stood down on the switch away from a settings pick; the transcript was left as it stood",
	done:    logsite.Msg("stood down the shim a settings pick started"),
	life:    shimlife.StandDownSettingsVisit,
	trigger: "settings_visit",
	remote:  remoteSpares,
}

// A SESSION WHOSE CACHE GOES COLD IS PARKED AT ONCE (owner ruling,
// 2026-11-15). A cold cache owes no keep-alive (cachelapse.Keeper), so the
// keep-alive cycle never comes round to park it, and it stayed up until the
// idle sweep's cutoff: 76 of 87 live sessions on 2026-11-14 were marked cold
// when a Wi-Fi outage lapsed their beats, and stayed live all night. The cache
// keeper asks for this stand-down (ColdDown) the moment it knows the cache
// cold, under the keep-alive cycle's rules, a merge-queued workspace
// included; the next prompt revives the park.

// A COLD GATE'S SESSION IS PARKED WHETHER OR NOT THE USER IS LOOKING (owner
// ruling, 2026-11-23): the daemon raises a gate from the shim's projection the
// moment the cache lapses, over a session that may still run on its shim, and
// the gate's answer spawns the shim it resumes through, which refuses while
// another runs the conversation. In remote control the park is taken all the
// same, and the gate then holds its bridge (remoteBridges). The park is
// otherwise a cold park's.

// opCold is the operation a cold cache's stand-down records under.
const opCold = "daemon.drain.cold_park"

// coldReason is a cold cache's stand-down reason.
var coldReason = standDownReason{
	op:      opCold,
	detail:  "parked the moment its cache was known cold; the transcript was left as it stood",
	done:    logsite.Msg("parked an idle session whose cache went cold"),
	life:    shimlife.StandDownCacheCold,
	trigger: "cold_park",
	remote:  remoteSpares,
}

// coldGateReason is the stand-down of a session a cold gate stands over.
var coldGateReason = standDownReason{
	op:      opCold,
	detail:  "parked the moment its cold gate was raised; the transcript was left as it stood",
	done:    logsite.Msg("parked an idle session whose cold gate was raised"),
	life:    shimlife.StandDownColdGate,
	trigger: "cold_gate_park",
	remote:  remoteBridges,
	atGate:  true,
}

// A FINISHED WORKSPACE IS PARKED THE MOMENT ITS SYNC MERGE IS IN LINE (owner
// requirement, 2026-11-21). A workspace that finished itself (its own
// branch's in-place merge) runs nothing until the merge's first prompt, yet
// its shim and vendor child stayed up until the keep-alive cycle came round:
// on 2026-11-21 five such entries waiting in line all held live shims. The
// merge orchestrator asks for this park once per enqueue, the boot's or a
// takeover's adoption of a waiting entry included (MergeQueuedDown), and the
// park is SCOPED TO THAT ENTRY: it is taken only while the same entry (its
// ledger identity) still waits in line, so a pending park retried on a later
// edge never parks a merge that has been admitted, taken off, or put in line
// again. It keeps the rules that protect real work -- never under a turn or
// live detached work (a busy session parks once it falls free), never in
// remote control -- but not the one for the workspace in view: a finished
// workspace's feed is held, and the merge's own prompts revive the park as
// they revive any park. It leaves the ordinary park, so the keep-alive keeper
// restores it before each beat it still owes.

// opMergeQueued is the operation a sync merge's park records under.
const opMergeQueued = "daemon.drain.merge_queued_park"

// mergeQueuedReason is a sync merge's stand-down reason.
var mergeQueuedReason = standDownReason{
	op:      opMergeQueued,
	detail:  "parked the moment its own branch's merge was put in line; the transcript was left as it stood",
	done:    logsite.Msg("parked a finished workspace whose merge waits in line"),
	life:    shimlife.StandDownMergeQueued,
	trigger: "merge_queued",
	remote:  remoteSpares,
}

// queuedMerge names the queue entry a sync merge's park is scoped to.
type queuedMerge struct {
	repo   wsm.RepoKey
	ledger wsm.LeaseID
}

// cycleState is one pending cycle stand-down's bookkeeping, under c.mu.
type cycleState struct {
	// beat is the instant a real turn since which keeps the session up: the
	// keep-alive this stand-down follows, or the settings pick's start.
	beat time.Time
	// reason is why it stands down: the keep-alive cycle, a settings visit, a
	// cold cache or a sync merge put in line.
	reason standDownReason
	// merge is the entry a sync merge's park is scoped to, nil for every
	// other reason.
	merge *queuedMerge
	// outranked is an unscoped ask that arrived while a sync merge's park
	// was pending, nil when none did. It is taken only when the merge's park
	// settles without parking the session (its entry left the line), so
	// neither asker's stand-down is lost to the other's.
	outranked *pendingAsk
	// running is an attempt in flight on the detached lifetime.
	running bool
	// again is an edge that arrived while an attempt ran.
	again bool
	// dones are the asks waiting to hear how the stand-down settled
	// (CycleDownThen); each is called once, when an attempt settles.
	dones []func(CycleOutcome)
}

// CycleOutcome is how one asked-for stand-down settled. A stand-down still
// pending (the session busy, the park deferred) has not settled.
type CycleOutcome struct {
	// Down reports that no shim serves the session once the attempt settled:
	// it was parked, or nothing served it.
	Down bool
	// Failed reports a read or a park that failed, rather than a rule that
	// kept the session up.
	Failed bool
	// Why is the attempt's account, as its record words it.
	Why string
}

// cycleReason is a keep-alive cycle's stand-down reason.
var cycleReason = standDownReason{
	op:      opCycle,
	detail:  "stood down between keep-alives with its cache warm; the transcript was left as it stood",
	done:    logsite.Msg("stood an idle session down until its next keep-alive"),
	life:    shimlife.StandDownKeepaliveCycle,
	trigger: "keepalive_cycle",
	remote:  remoteSpares,
}

// AN IDLE SESSION IS PARKED TWENTY MINUTES AFTER ITS WORK (owner ruling,
// 2026-11-23). The keep-alive cycle stood a session down only once its next
// beat had gone out, 52 minutes after its last turn, so an idle shim and its
// vendor child held ~350MB for most of every hour. The cache keeper now asks
// for this park once the workspace's shim hold ends (wsm.ShimHold)
// (cachelapse.Keeper.ownIdle), under the keep-alive cycle's rules; the keeper
// still restores it a minute before each beat it owes, and the beat's own
// stand-down parks it again after.

// opIdlePark is the operation an idle session's park records under.
const opIdlePark = "daemon.drain.idle_park"

// idleParkReason is an idle session's park reason.
var idleParkReason = standDownReason{
	op:      opIdlePark,
	detail:  "parked after its idle grace with its cache warm; the transcript was left as it stood",
	done:    logsite.Msg("parked an idle session until its next keep-alive"),
	life:    shimlife.StandDownIdle,
	trigger: "idle_park",
	remote:  remoteSpares,
}

// IdleDown asks for the park of WS's session, idle since SINCE. It returns at
// once.
func (c *controller) IdleDown(ws ids.WorkspaceID, since time.Time) {
	c.askDown(ws, since, idleParkReason, nil)
}

// CycleDown asks for the stand-down of WS's session after the keep-alive asked
// at BEAT. It returns at once; the attempt runs on the detached lifetime. A
// later beat's ask replaces the earlier one's instant.
func (c *controller) CycleDown(ws ids.WorkspaceID, beat time.Time) {
	c.askDown(ws, beat, cycleReason, nil)
}

// CycleDownThen is CycleDown, calling DONE once with how the stand-down
// settled. A stand-down that never settles (the daemon leaving under it)
// never calls DONE, so its caller bounds its own wait.
func (c *controller) CycleDownThen(ws ids.WorkspaceID, beat time.Time, done func(CycleOutcome)) {
	c.askDown(ws, beat, cycleReason, done)
}

// VisitDown asks for the stand-down of WS's session, a settings pick having
// started it at SINCE, the user having switched away. It returns at once.
func (c *controller) VisitDown(ws ids.WorkspaceID, since time.Time) {
	c.askDown(ws, since, visitReason, nil)
}

// ColdDown asks for the stand-down of WS's session, its cache having been
// known cold at SINCE. ATGATE is a cold gate standing over the session, which
// is parked for the workspace in view too. It returns at once.
func (c *controller) ColdDown(ws ids.WorkspaceID, since time.Time, atGate bool) {
	reason := coldReason
	if atGate {
		reason = coldGateReason
	}
	c.askDown(ws, since, reason, nil)
}

// MergeQueuedDown asks for the park of WS's session, its own branch's merge
// having been put in line in REPO under LEDGER. It returns at once.
func (c *controller) MergeQueuedDown(ws ids.WorkspaceID, repo wsm.RepoKey, ledger wsm.LeaseID) {
	c.askDownFor(ws, c.deps.Clock.Now(), mergeQueuedReason, &queuedMerge{repo: repo, ledger: ledger}, nil)
}

// pendingAsk is one unscoped stand-down ask: its instant and its reason.
type pendingAsk struct {
	beat   time.Time
	reason standDownReason
}

// askDown asks for WS's stand-down for REASON unless the workspace has done
// real work since BEAT (wsm.Workspace.WorkedSince). A later ask replaces the
// earlier one's instant and reason, except that an unscoped ask never
// replaces a pending sync merge's park (askDownFor); DONE, when given, joins
// the asks told how it settles.
func (c *controller) askDown(ws ids.WorkspaceID, beat time.Time, reason standDownReason, done func(CycleOutcome)) {
	c.askDownFor(ws, beat, reason, nil, done)
}

// askDownFor is askDown scoped to MERGE's queue entry, when given.
func (c *controller) askDownFor(ws ids.WorkspaceID, beat time.Time, reason standDownReason, merge *queuedMerge, done func(CycleOutcome)) {
	c.mu.Lock()
	if c.cycles == nil {
		c.cycles = map[ids.WorkspaceID]*cycleState{}
	}
	st := c.cycles[ws]
	if st == nil {
		st = &cycleState{}
		c.cycles[ws] = st
	}
	switch {
	case st.merge != nil && merge == nil:
		// A SYNC MERGE'S PARK OUTRANKS AN UNSCOPED ASK. It is taken even for
		// the workspace in view, and only while its entry waits, so an idle
		// or keep-alive ask that replaced it lost both rules: the shim hold
		// asks for the idle park the moment a waiting merge's turn ends
		// (2026-11-25, the displaced-turn landing). The unscoped ask is kept
		// for when the merge's park settles without a park.
		st.outranked = &pendingAsk{beat: beat, reason: reason}
	case st.merge == nil && merge != nil && st.reason.op != "":
		st.outranked = &pendingAsk{beat: st.beat, reason: st.reason}
		st.beat, st.reason, st.merge = beat, reason, merge
	default:
		st.beat, st.reason, st.merge = beat, reason, merge
	}
	if done != nil {
		st.dones = append(st.dones, done)
	}
	c.mu.Unlock()
	c.log.Debug(reason.op, "a stand-down was asked for", merge.fields(dlog.Context{"workspace": string(ws), "since": beat}))
	c.kickCycle(ws, st)
}

// retryCycle is a daemon edge for ws: a cycle stand-down pending on it is
// asked again. A workspace with none pending is no change.
func (c *controller) retryCycle(ws ids.WorkspaceID) {
	c.mu.Lock()
	st := c.cycles[ws]
	var merge *queuedMerge
	if st != nil {
		merge = st.merge
	}
	c.mu.Unlock()
	if st == nil {
		return
	}
	c.log.Log(settledLevel(merge), opCycle, logsite.Msg("an edge arrived for a pending stand-down; asking again"),
		merge.fields(dlog.Context{"workspace": string(ws)}))
	c.kickCycle(ws, st)
}

// kickCycle runs an attempt for ws unless one is running, in which case that
// one runs once more when it ends.
func (c *controller) kickCycle(ws ids.WorkspaceID, st *cycleState) {
	c.mu.Lock()
	if st.running {
		st.again = true
		c.mu.Unlock()
		return
	}
	st.running = true
	c.mu.Unlock()
	c.deps.Detach(ws, func(ctx context.Context) {
		for {
			c.mu.Lock()
			beat, reason, merge := st.beat, st.reason, st.merge
			c.mu.Unlock()
			outcome, settled := c.cycleDown(ctx, ws, beat, reason, merge)
			c.mu.Lock()
			if outcome == withheldSettled && merge != nil && st.merge == merge && !settled.Down && !settled.Failed && st.outranked != nil && ctx.Err() == nil {
				// THE MERGE'S PARK WAS NOT TAKEN, so the ask it outranked is.
				st.beat, st.reason, st.merge = st.outranked.beat, st.outranked.reason, nil
				outranked := st.outranked.reason.op
				st.outranked = nil
				st.again = false
				c.mu.Unlock()
				c.log.Debug(reason.op, "the sync merge's park was not taken; the stand-down it outranked is asked now",
					merge.fields(dlog.Context{"workspace": string(ws), "outranked": outranked}))
				continue
			}
			var dones []func(CycleOutcome)
			if outcome == withheldSettled {
				dones, st.dones = st.dones, nil
			}
			if st.again && ctx.Err() == nil {
				st.again = false
				c.mu.Unlock()
				tell(dones, settled)
				continue
			}
			st.running, st.again = false, false
			if outcome == withheldSettled {
				st.outranked = nil
			}
			if outcome == withheldSettled && c.cycles[ws] == st {
				delete(c.cycles, ws)
			}
			c.mu.Unlock()
			tell(dones, settled)
			return
		}
	})
}

// tell calls every waiting ask with how the stand-down settled.
func tell(dones []func(CycleOutcome), settled CycleOutcome) {
	for _, done := range dones {
		done(settled)
	}
}

// fields adds the queue entry M names to FIELDS; a nil M adds nothing.
func (m *queuedMerge) fields(fields dlog.Context) dlog.Context {
	if m == nil {
		return fields
	}
	return dlog.Merge(fields, dlog.Context{"repo": string(m.repo), "merge_entry": string(m.ledger)})
}

// cycleDown is one attempt, and how it settled when it did. MERGE, when
// given, scopes the attempt to that queue entry (MergeQueuedDown).
func (c *controller) cycleDown(ctx context.Context, ws ids.WorkspaceID, beat time.Time, reason standDownReason, merge *queuedMerge) (withheldOutcome, CycleOutcome) {
	op := reason.op
	fields := merge.fields(dlog.Context{"workspace": string(ws), "since": beat})
	// THE HOLD IS THE LAST ATTEMPT'S: each attempt states its own afresh.
	c.deps.Why.ClearHold(ws)
	if err := ctx.Err(); err != nil {
		c.log.Info(op, "the daemon is leaving; the stand-down is not taken", dlog.WithCause(fields, err))
		return withheldPending, CycleOutcome{}
	}
	record, err := c.deps.DB.Workspace(ctx, ws)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			c.log.Info(op, "the workspace is no longer registered; nothing is stood down", fields)
			return withheldSettled, CycleOutcome{Down: true, Why: "the workspace is no longer registered; nothing is stood down"}
		}
		c.log.Error(op, "could not read the workspace's record; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled, CycleOutcome{Failed: true, Why: "could not read the workspace's record; its stand-down is dropped"}
	}
	log := c.workspaceLog(record)
	// A FINISHED WORKSPACE IN VIEW IS PARKED ALL THE SAME: its feed is held,
	// and the merge's prompts revive the park.
	if record.Current && merge == nil && !reason.atGate {
		log.Info(op, "the user is looking at the workspace; its session is kept up", fields)
		return withheldSettled, CycleOutcome{Why: "the user is looking at the workspace; its session is kept up"}
	}
	// A COLD PARK, A SETTINGS VISIT AND A SYNC MERGE'S PARK ARE TAKEN
	// WHATEVER THE VISIBILITY: the
	// cache keeper asks for either only when the lapse owns no stand-down of
	// the session (cachelapse.Keeper.own), so a closed or withheld workspace
	// those two refused was left with no owner at all (2026-11-20). Only the
	// keep-alive cycle leaves a closed or withheld workspace to its lapse.
	lapseOwnsWithheld := reason.op == opCycle
	// A CLOSED WORKSPACE WITH A MERGE QUEUED IS STOOD DOWN AS AN OPEN ONE IS:
	// the cache keeper's lapse keeps every queued workspace's session for as
	// long as its entry stands (cachelapse.Keeper.lapse), so the lapse would
	// never stand it down, and it stayed up between beats (2026-11-14, 27 of
	// 87 live sessions had a merge waiting). The next prompt the merge sends
	// revives it, as it revives any park.
	if record.Closed && lapseOwnsWithheld {
		queued, err := bringup.MergeQueued(ctx, c.deps.DB, record)
		if err != nil {
			log.Error(op, "could not read whether the closed workspace has a merge queued; its stand-down is dropped", dlog.WithCause(fields, err))
			return withheldSettled, CycleOutcome{Failed: true, Why: "could not read whether the closed workspace has a merge queued; its stand-down is dropped"}
		}
		if !queued {
			log.Debug(op, "the workspace is closed; its cache's lapse stands it down", fields)
			return withheldSettled, CycleOutcome{Why: "the workspace is closed; its cache's lapse stands it down"}
		}
	}
	visible := true
	if lapseOwnsWithheld {
		visible, err = bringup.Visible(ctx, c.deps.DB, log, ws, c.deps.Visibility)
	}
	if err != nil {
		log.Error(op, "could not read whether the workspace is visible; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled, CycleOutcome{Failed: true, Why: "could not read whether the workspace is visible; its stand-down is dropped"}
	}
	if !visible {
		log.Debug(op, "the workspace is withheld; its cache's lapse stands it down", fields)
		return withheldSettled, CycleOutcome{Why: "the workspace is withheld; its cache's lapse stands it down"}
	}
	session, found, err := c.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the workspace's session; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled, CycleOutcome{Failed: true, Why: "could not read the workspace's session; its stand-down is dropped"}
	}
	if !found || session.Terminal != nil {
		read := dlog.Context{"found": found}
		if found {
			read["terminal"] = session.Terminal != nil
		}
		log.Log(settledLevel(merge), op, logsite.Msg("the workspace has no live session; nothing to stand down"), dlog.Merge(fields, read))
		return withheldSettled, CycleOutcome{Down: true, Why: "the workspace has no live session; nothing to stand down"}
	}
	// A SYNC MERGE'S PARK HOLDS ONLY WHILE ITS ENTRY WAITS: the turn that
	// asked for the merge may end after the ask, so no turn since keeps it up,
	// and the entry still waiting in line is what makes the park valid.
	if merge != nil {
		waiting, err := c.mergeWaiting(ctx, ws, *merge)
		if err != nil {
			log.Error(op, "could not read whether the workspace's merge still waits in line; its park is dropped", dlog.WithCause(fields, err))
			return withheldSettled, CycleOutcome{Failed: true, Why: "could not read whether the workspace's merge still waits in line; its park is dropped"}
		}
		if !waiting {
			log.Info(op, "the merge no longer waits in line; the park is not taken", fields)
			return withheldSettled, CycleOutcome{Why: "the merge no longer waits in line; the park is not taken"}
		}
	} else if worked, did := record.WorkedSince(beat); did {
		// REAL WORK, NOT ANY TURN'S CLOSE: a turn a bounce orphaned closes at
		// the bounce, and a keep-alive's own turn is no work (wsm lastwork.go),
		// so neither keeps a session up past its stand-down.
		log.Info(op, "a real turn ran since; the session is kept up",
			dlog.Merge(fields, dlog.Context{"worked_at": worked}))
		return withheldSettled, CycleOutcome{Why: "a real turn ran since; the session is kept up"}
	} else if hold := session.BringUpHold(record.LastTurnEndedAt, c.deps.Clock.Now()); hold.HoldsAt(c.deps.Clock.Now()) {
		// A SESSION ITS OWN BRING-UP HOLDS IS NEVER PARKED (owner ruling,
		// 2026-11-24; wsm.Session.BringUpHold): a stand-down asked before a
		// bring-up for work (a cold gate raised, then answered) is not taken
		// over the session that came up, and the cache keeper asks again once
		// the hold ends.
		log.Info(op, "the session's own bring-up holds it up; the stand-down is not taken",
			dlog.Merge(fields, dlog.Context{"indefinite": hold.Indefinite, "until": hold.Until}))
		return withheldSettled, CycleOutcome{Why: "the session's own bring-up holds it up; the stand-down is not taken"}
	}
	switch gate := c.gate(ws); gate {
	case gateOpen:
	case gateNotServing:
		log.Log(settledLevel(merge), op, logsite.Msg("the workspace has no shim to address; nothing to stand down"),
			dlog.Merge(fields, dlog.Context{"gate": gate.String()}))
		return withheldSettled, CycleOutcome{Down: true, Why: "the workspace has no shim to address; nothing to stand down"}
	case gateBusy, gateStarting, gateReviving:
		log.Info(op, "the session is busy; its stand-down waits for it to fall free",
			dlog.Merge(fields, dlog.Context{"gate": gate.String()}))
		c.deps.Why.SetHold(ws, sessionwhy.HoldDeferredBusy)
		return withheldPending, CycleOutcome{}
	default:
		log.Error(op, "the stand-down gate answered a value this stand-down cannot read; it is dropped",
			dlog.Merge(fields, dlog.Context{"gate": gate.String(), "invariant_violation": "every gate value is handled"}))
		return withheldSettled, CycleOutcome{Failed: true, Why: "the stand-down gate answered a value this stand-down cannot read; it is dropped"}
	}
	// A WORKSPACE IN REMOTE CONTROL KEEPS ITS SHIM UP (owner ruling,
	// 2026-10-31): claude.ai reaches the session only through a shim that is
	// up, so neither a keep-alive's park, a cold park nor a settings visit's
	// stand-down is taken, and so no pre-beat restore is ever owed. The mode
	// is read at the act, under its gate (park), so one turned off since the
	// ask parks at the next beat as usual.
	switch c.park(ctx, log, ws, fields, reason, record.IdleSince()) {
	case parkDone:
		return withheldSettled, CycleOutcome{Down: true, Why: reason.done.Template()}
	case parkSpared:
		return withheldSettled, CycleOutcome{Why: remoteSpared}
	case parkDeferred:
		log.Info(op, "the stand-down was deferred; it is taken when the session falls free", fields)
		return withheldPending, CycleOutcome{}
	default:
		// park recorded the failure at its own level; the next beat asks
		// afresh.
		return withheldSettled, CycleOutcome{Failed: true, Why: "the park failed; its record above says why"}
	}
}

// settledLevel answers the level a stand-down records a settling at when it
// stood nothing down, and an edge asking a pending one again. A SYNC MERGE'S
// PARK IS A DECISION OTHERS WAIT ON -- the merge queue, e2e and the sidebar --
// so its settling is a fact at INFO, never silent: a merge's park asked again
// on a bounce's end settled with no record at INFO, and which path it took
// could not be read (2026-11-23, the displaced-turn landing). Every other ask
// stays at DEBUG, because the keep-alive cycle reaches these paths for every
// parked workspace at every beat.
func settledLevel(merge *queuedMerge) string {
	if merge != nil {
		return dlog.LevelInfo
	}
	return dlog.LevelDebug
}

// mergeWaiting answers whether WS's entry in M's queue still waits in line as
// the entry M names: not admitted, and not taken off and put in line again.
func (c *controller) mergeWaiting(ctx context.Context, ws ids.WorkspaceID, m queuedMerge) (bool, error) {
	entries, err := c.deps.DB.MergeQueue(ctx, m.repo)
	if err != nil {
		return false, err
	}
	for _, entry := range entries {
		if entry.Workspace == ws {
			return entry.State == wsm.MergeQueued && entry.Ledger == m.ledger, nil
		}
	}
	return false, nil
}

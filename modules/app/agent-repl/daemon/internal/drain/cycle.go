package drain

import (
	"context"
	"errors"
	"time"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
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
// hidden or closed one with no merge queued (the cache keeper's lapse owns
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
	op:     opVisit,
	detail: "stood down on the switch away from a settings pick; the transcript was left as it stood",
	done:   "stood down the shim a settings pick started",
	life:   shimlife.StandDownSettingsVisit,
}

// A SESSION WHOSE CACHE GOES COLD IS PARKED AT ONCE (owner ruling,
// 2026-11-15). A cold cache owes no keep-alive (cachelapse.Keeper), so the
// keep-alive cycle never comes round to park it, and it stayed up until the
// idle sweep's cutoff: 76 of 87 live sessions on 2026-11-14 were marked cold
// when a Wi-Fi outage lapsed their beats, and stayed live all night. The cache
// keeper asks for this stand-down (ColdDown) the moment it knows the cache
// cold, under the keep-alive cycle's rules, a merge-queued workspace
// included; the next prompt revives the park.

// opCold is the operation a cold cache's stand-down records under.
const opCold = "daemon.drain.cold_park"

// coldReason is a cold cache's stand-down reason.
var coldReason = standDownReason{
	op:     opCold,
	detail: "parked the moment its cache was known cold; the transcript was left as it stood",
	done:   "parked an idle session whose cache went cold",
	life:   shimlife.StandDownCacheCold,
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
	op:     opMergeQueued,
	detail: "parked the moment its own branch's merge was put in line; the transcript was left as it stood",
	done:   "parked a finished workspace whose merge waits in line",
	life:   shimlife.StandDownMergeQueued,
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
	op:     opCycle,
	detail: "stood down between keep-alives with its cache warm; the transcript was left as it stood",
	done:   "stood an idle session down until its next keep-alive",
	life:   shimlife.StandDownKeepaliveCycle,
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
// known cold at SINCE. It returns at once.
func (c *controller) ColdDown(ws ids.WorkspaceID, since time.Time) {
	c.askDown(ws, since, coldReason, nil)
}

// MergeQueuedDown asks for the park of WS's session, its own branch's merge
// having been put in line in REPO under LEDGER. It returns at once.
func (c *controller) MergeQueuedDown(ws ids.WorkspaceID, repo wsm.RepoKey, ledger wsm.LeaseID) {
	c.askDownFor(ws, c.deps.Clock.Now(), mergeQueuedReason, &queuedMerge{repo: repo, ledger: ledger}, nil)
}

// askDown asks for WS's stand-down for REASON unless a real turn has run
// since BEAT. A later ask replaces the earlier one's instant and reason; DONE,
// when given, joins the asks told how it settles.
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
	st.beat, st.reason, st.merge = beat, reason, merge
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
	c.mu.Unlock()
	if st == nil {
		return
	}
	c.log.Debug(opCycle, "an edge arrived for a pending keep-alive cycle stand-down; asking again", dlog.Context{"workspace": string(ws)})
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
	if record.Current && merge == nil {
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
	// A WORKSPACE IN REMOTE CONTROL KEEPS ITS SHIM UP (owner ruling,
	// 2026-10-31): claude.ai reaches the session only through a shim that is
	// up, so neither a keep-alive's park nor a settings visit's stand-down is
	// taken, and so no pre-beat restore is ever owed. The mode is read HERE,
	// at the attempt, so one turned off since the ask parks at the next beat
	// as usual. The idle sweep is not this path and still
	// stands it down.
	remote, err := c.deps.DB.RemoteControl(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the workspace's remote-control mode; its stand-down is dropped", dlog.WithCause(fields, err))
		return withheldSettled, CycleOutcome{Failed: true, Why: "could not read the workspace's remote-control mode; its stand-down is dropped"}
	}
	if remote.On {
		log.Info(op, "remote control is on; the shim stays up for claude.ai", dlog.Merge(fields, dlog.Context{"remote_control_recorded": remote.Recorded}))
		return withheldSettled, CycleOutcome{Why: "remote control is on; the shim stays up for claude.ai"}
	}
	visible := true
	if lapseOwnsWithheld {
		visible, err = bringup.Visible(ctx, c.deps.DB, log, ws)
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
		log.Debug(op, "the workspace has no live session; nothing to stand down", fields)
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
	} else if ended, turned := record.TurnedSince(beat); turned {
		log.Info(op, "a real turn ran since; the session is kept up",
			dlog.Merge(fields, dlog.Context{"turn_ended_at": ended}))
		return withheldSettled, CycleOutcome{Why: "a real turn ran since; the session is kept up"}
	}
	switch gate := c.gate(ws); gate {
	case gateOpen:
	case gateNotServing:
		log.Debug(op, "the workspace has no shim to address; nothing to stand down", fields)
		return withheldSettled, CycleOutcome{Down: true, Why: "the workspace has no shim to address; nothing to stand down"}
	case gateBusy, gateStarting, gateReviving:
		log.Info(op, "the session is busy; its stand-down waits for it to fall free",
			dlog.Merge(fields, dlog.Context{"gate": gate.String()}))
		return withheldPending, CycleOutcome{}
	default:
		log.Error(op, "the stand-down gate answered a value this stand-down cannot read; it is dropped",
			dlog.Merge(fields, dlog.Context{"gate": gate.String(), "invariant_violation": "every gate value is handled"}))
		return withheldSettled, CycleOutcome{Failed: true, Why: "the stand-down gate answered a value this stand-down cannot read; it is dropped"}
	}
	switch c.park(ctx, log, ws, fields, reason) {
	case parkDone:
		return withheldSettled, CycleOutcome{Down: true, Why: reason.done}
	case parkDeferred:
		log.Info(op, "the stand-down was deferred; it is taken when the session falls free", fields)
		return withheldPending, CycleOutcome{}
	default:
		// park recorded the failure at its own level; the next beat asks
		// afresh.
		return withheldSettled, CycleOutcome{Failed: true, Why: "the park failed; its record above says why"}
	}
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

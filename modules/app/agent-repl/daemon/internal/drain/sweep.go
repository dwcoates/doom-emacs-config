package drain

import (
	"context"
	"errors"
	"fmt"
	"time"

	"agentrepl/logging/logsite"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwhy"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

// Sweep runs one pass of the idle sweep and reports what it parked.
//
// THE IDLE SWEEP IS ONE TRIGGER OF THE ONE PARK (owner ruling, 2026-11-09).
// It reaches the sessions no other stand-down does -- the workspace the user
// is looking at, one whose remote control is on (its reason's remoteParks,
// read at the act like every trigger's) -- and parks them as the
// keep-alive cycle parks the rest: the same gate, the same steps, the same
// `parked` terminal (wsm.TerminalParked), with its own detail naming it.
//
// A workspace is parked only when it is BOTH idle past the cutoff and FREE:
// a stand-down never interrupts, so a busy session is deferred to a later
// pass rather than stood down under a running turn.
func (c *controller) Sweep(ctx context.Context, now time.Time) ([]ids.WorkspaceID, error) {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		readFailed(c.log, opSweep, logsite.Msg("could not list the workspaces to sweep"),
			logsite.Msg("the workspace list ended when the drain loop's context was cancelled"), nil, err)
		return nil, fmt.Errorf("drain: sweep: %w", err)
	}
	queued, err := c.queuedSince(ctx)
	if err != nil {
		return nil, err
	}
	// EVERY SESSION IN ONE READ: a read per workspace queued each behind every
	// other reader on the store's one connection, every pass.
	sessions, err := wsm.SessionsByWorkspace(ctx, c.deps.DB)
	if err != nil {
		readFailed(c.log, opSweep, logsite.Msg("could not read the workspaces' sessions"),
			logsite.Msg("the session read ended when the drain loop's context was cancelled"), nil, err)
		return nil, fmt.Errorf("drain: sweep: %w", err)
	}
	var parked []ids.WorkspaceID
	for _, ws := range workspaces {
		fields := dlog.Context{"workspace": string(ws.ID), "cutoff": c.deps.IdleCutoff.String()}
		// A QUEUED MERGE KEEPS ITS SESSION WARM FOR LONGER (owner rulings,
		// 2026-10-13, 2026-10-26 and 2026-11-11): the merge's prompts run on
		// that session, and a long queue holds an entry for hours, so it is
		// kept warm for QueuedIdleFactor cutoffs from when it last engaged or
		// joined its queue (bringup.IdleHorizon); past them it parks like any
		// other session, and its merge waits at its cold gate, skipped.
		enqueued, queuedHere := queued[ws.ID]
		if queuedHere {
			fields["queued_at"] = enqueued.Format(time.RFC3339)
		}
		// EVERY RECORD BELOW IS WORKSPACE-BOUND and goes to that workspace's
		// own sink whenever the workspace can host one. A workspace whose
		// directory is a scratch path or has been deleted still gets swept:
		// resolution falls back to the central sink with the workspace named
		// on the record. Deferring the sweep on it instead cost an ERROR per
		// pass per workspace and left the session unparked forever.
		log := c.workspaceLog(ws)
		session, found := sessions[ws.ID]
		if !found {
			log.Debug(opSweep, "the workspace has no session to park", fields)
			continue
		}
		// THE IDLE CLOCK IS THE WORKSPACE'S LAST REAL WORK (wsm.Workspace.
		// IdleSince), which no session start, keep-alive or bounce moves.
		worked := ws.IdleSince()
		idle := now.Sub(worked)
		fields["idle"] = idle.String()
		// A SESSION ALREADY PARKED STAYS AS IT IS. There is one park, so a park
		// past the cutoff is no deeper than one inside it; the cache keeper owes
		// it no keep-alive past the cutoff, so its park carries none.
		if session.Terminal != nil {
			log.Debug(opSweep, "the workspace's session is already terminal",
				dlog.Merge(fields, dlog.Context{"terminal_kind": session.Terminal.Kind}))
			continue
		}
		if now.Before(bringup.IdleHorizon(c.deps.IdleCutoff, worked, enqueued, queuedHere)) {
			log.Debug(opSweep, "the workspace did real work inside the cutoff", fields)
			continue
		}
		switch gate := c.gate(ws.ID); gate {
		case gateOpen:
		case gateBusy:
			log.Debug(opSweep, "the idle session is not free; deferring its park", fields)
			continue
		case gateNotServing:
			log.Debug(opSweep, "the workspace has no shim to address; skipping its park", fields)
			continue
		case gateStarting:
			log.Debug(opSweep, "a start of the workspace's session is under way; skipping its park", fields)
			continue
		case gateReviving:
			log.Debug(opSweep, "a prompt is reviving the session; deferring its park", fields)
			continue
		default:
			log.Error(opSweep, "the stand-down gate answered a value the sweep cannot read; skipping the park",
				dlog.Merge(fields, dlog.Context{"gate": gate.String(), "invariant_violation": "every gate value is handled"}))
			continue
		}
		if retry, backingOff := c.backingOff(ws.ID, now); backingOff {
			log.Debug(opSweep, "the workspace's last park failed; backing off before the next attempt",
				dlog.Merge(fields, dlog.Context{"failures": retry.failures, "next_attempt": retry.notBefore}))
			continue
		}
		switch outcome := c.park(ctx, log, ws.ID, fields, c.idleReason(), ws.IdleSince()); outcome {
		case parkDone:
			c.clearRetry(ws.ID)
			parked = append(parked, ws.ID)
		case parkDeferred:
			c.clearRetry(ws.ID)
		case parkFailed:
			retry := c.recordFailure(ws.ID, now)
			log.Debug(opSweep, "backing off the failed park",
				dlog.Merge(fields, dlog.Context{"failures": retry.failures, "next_attempt": retry.notBefore}))
		default:
			panic(fmt.Sprintf("drain: the idle sweep's park answered %d, which its remoteParks policy never answers", outcome))
		}
	}
	c.log.Debug(opSweep, "one idle-sweep pass finished",
		dlog.Context{"scanned": len(workspaces), "parked": ids.Strings(parked)})
	return parked, nil
}

// standGate is whether a session may be stood down right now, and if not,
// why not. The idle sweep and a withheld workspace's stand-down ask the SAME
// question (gate), in the same order.
type standGate int

const (
	// gateOpen: nothing stands in the way.
	gateOpen standGate = iota
	// gateBusy: a turn is in flight or detached work is live. Teardown never
	// interrupts.
	gateBusy
	// gateNotServing: this daemon holds no shim it can address.
	gateNotServing
	// gateStarting: a start of the session is still under way.
	gateStarting
	// gateReviving: a prompt is reviving the session.
	gateReviving
)

// String names the gate for the records.
func (g standGate) String() string {
	switch g {
	case gateOpen:
		return "open"
	case gateBusy:
		return "busy"
	case gateNotServing:
		return "not_serving"
	case gateStarting:
		return "starting"
	case gateReviving:
		return "reviving"
	default:
		return "unknown"
	}
}

// gate answers whether ws's session may be stood down right now.
func (c *controller) gate(ws ids.WorkspaceID) standGate {
	if !c.deps.Freeness.Free(ws) {
		return gateBusy
	}
	// A DURABLE SESSION ROW IS NOT A SHIM. A stand-down is a directive to a
	// running shim, so a workspace whose session this daemon cannot
	// address has nothing to stand down: its bring-up is still in flight,
	// its close already is, or the row outlived the process that served
	// it. That is a STATE, and it is skipped here rather than sending
	// a directive whose only possible answer is a failure -- which is what
	// a registry row left behind by a removed worktree cost in the field:
	// the same ERROR every five minutes, forever, for a session no daemon
	// had ever held (realtest 7, 2026-09-12).
	if !c.deps.Stand.Serving(ws) {
		return gateNotServing
	}
	// A START STILL UNDER WAY HAS NOT RECORDED ITS ENGAGEMENT: the row
	// read before it carries the one from before it, so the idle measured is
	// the workspace's time closed, not the session's time idle.
	if c.deps.Stand.StartUnderWay(ws) {
		return gateStarting
	}
	// A SESSION A PROMPT IS REVIVING IS ENGAGED, whatever its record says.
	// The revival's bring-up serves the session and retires its terminal
	// before it records the session's facts, and delivers the prompt it
	// holds only after that, so across that window the record reads live
	// and idle since the engagement before the last park. Standing
	// it down there parked the shim the prompt was about to reach: the
	// hold then met `query_dead`, or no session at all, and was dropped.
	//
	// THE ORDER IS WHAT MAKES THIS TOTAL. Serving was read true above, so
	// any revival behind it had already raised its flag; a flag read false
	// here means that revival has delivered, and the turn it started is
	// the shim's own `turn_in_flight` refusal to the directive below.
	if c.deps.Reviving(ws) {
		return gateReviving
	}
	return gateOpen
}

// parkOutcome is what one park attempt came to.
type parkOutcome int

const (
	// parkDone: the session was stood down and recorded.
	parkDone parkOutcome = iota
	// parkDeferred: an ORDINARY deferral -- another lease holder, a
	// session that went away, a turn in flight. The next pass asks again at
	// the sweep's own cadence.
	parkDeferred
	// parkFailed: something that should have worked did not -- a failed
	// or unreadable directive, a warned refusal, a failed stand-down or
	// record, an unreadable remote-control mode. The workspace is BACKED OFF
	// (see parkRetryDelay).
	parkFailed
	// parkSpared: remote control is on and the reason spares it
	// (remoteSpares); the session keeps its shim.
	parkSpared
)

// remoteSpared is the account of a stand-down remote control spared.
const remoteSpared = "remote control is on; the shim stays up for claude.ai"

// standDownReason is why a session is stood down: the records it is filed
// under, the detail its terminal carries, and the sentence its success says.
// Every trigger -- the idle sweep, a withheld workspace, the keep-alive cycle,
// a settings visit -- takes the SAME steps (park) and leaves the SAME
// park (wsm.TerminalParked); only these words differ, and the detail is where
// the trigger stays on the record as evidence. No view reads it.
type standDownReason struct {
	op     string
	detail string
	done   logsite.Message
	// life is the reason the shim lifecycle series counts it under.
	life shimlife.StandDownReason
	// trigger names what asked for the stand-down; every record park writes
	// carries it.
	trigger string
	// remote is what a remote-control mode that is on does to the
	// stand-down, read at the act (park). Every reason names one.
	remote remotePolicy
	// atGate is a stand-down taken because a cold gate stands: it is taken
	// for the workspace the user is looking at as for any other, since the
	// gate's answer re-opens through a new shim and refuses while one still
	// runs the session.
	atGate bool
}

// remotePolicy is what a stand-down does with a session whose remote control
// is on.
type remotePolicy int

const (
	// remoteSpares: the session keeps its shim, because claude.ai reaches it
	// only through a shim that is up (owner ruling, 2026-10-31). Every
	// trigger but the idle sweep's.
	remoteSpares remotePolicy = iota + 1
	// remoteParks: the session is parked all the same. Only the idle sweep
	// (owner ruling, 2026-11-09): a session nobody, on this machine or on
	// claude.ai, engaged for the whole cutoff is parked like any other.
	remoteParks
	// remoteBridges: the session is parked all the same, and its standing
	// gate then holds the parked conversation's bridge (Deps.BridgeGate).
	// Only a cold gate's park: claude.ai reaches a gated workspace through
	// that bridge (workspace/parkedbridge.go), and a session spared at its
	// gate left every answer refused, since the answer's resume finds a
	// shim already running the conversation (2026-11-23 19:59:20,
	// merge-queue-stall-triage: two compact answers in a row refused
	// no_session, the gate standing again after each).
	remoteBridges
)

// String names the policy for a record.
func (p remotePolicy) String() string {
	switch p {
	case remoteSpares:
		return "spares"
	case remoteParks:
		return "parks"
	case remoteBridges:
		return "bridges"
	default:
		return fmt.Sprintf("remotePolicy(%d)", int(p))
	}
}

// triggerIdle is the idle sweep's trigger.
const triggerIdle = "idle_cutoff"

// idleReason is the idle sweep's.
func (c *controller) idleReason() standDownReason {
	return standDownReason{
		op:      opSweep,
		detail:  fmt.Sprintf("idle past the %s cutoff; the transcript was left as it stood", c.deps.IdleCutoff),
		done:    logsite.Msg("parked a session idle past the cutoff"),
		life:    shimlife.StandDownHibernate,
		trigger: triggerIdle,
		remote:  remoteParks,
	}
}

// park stands one session down: the Hibernate directive, then the ack,
// then a GRACEFUL KillSession, then the durable stand-down record. It reports
// what the attempt came to; every failure and every refusal DEFERS the
// workspace rather than failing the whole pass, because one wedged session
// must not stop the sweep reaching the rest.
//
// IT IS THE ONE GATE EVERY STAND-DOWN PASSES, so no trigger can skip the
// remote-control rule: every record it writes names the trigger
// (standDownReason.trigger), and the mode is read HERE, under the
// workspace's remote-control gate (Deps.HoldRemoteControl), which is held
// through the shim's stand-down and the park's record. A turn-on is recorded
// and put in force under the same gate (workspace.Fleet.SetRemoteControl), so
// it lands either before the read, which then spares the session, or after
// the shim is gone, which its own no-shim path starts again
// (UpdateRemoteControlMode): never between the decision and the act.
func (c *controller) park(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, fields dlog.Context, why standDownReason, idleSince time.Time) parkOutcome {
	outcome := c.parkOnce(ctx, log, ws, fields, why)
	switch outcome {
	case parkDeferred:
		c.deps.Why.SetHold(ws, sessionwhy.HoldDeferredBusy)
	case parkDone:
		c.deps.Why.ClearHold(ws)
		c.observeIdleBeforePark(log, fields, why, idleSince)
	}
	return outcome
}

// ParkTelemetry is where the drain observes how long each session it parked
// stood up idle (runtimemetrics.Surface). It is one histogram update and
// never blocks.
type ParkTelemetry interface {
	// ObserveIdleBeforePark records one park for REASON of a session that
	// stood up IDLE since its last real work.
	ObserveIdleBeforePark(reason shimlife.StandDownReason, idle time.Duration)
}

// observeIdleBeforePark records how long the session just parked for WHY
// stood idle, from SINCE to now: the workspace's last real work
// (wsm.Workspace.IdleSince, the clock every park's grace is measured from,
// which no session start, keep-alive or bounce moves). Every caller of park
// holds the workspace from the reads that decided the park, so nothing is read
// after it. A zero SINCE is a workspace with neither work nor a creation on
// record, a broken invariant recorded at ERROR; the park stands.
func (c *controller) observeIdleBeforePark(log dlog.Logger, fields dlog.Context, why standDownReason, since time.Time) {
	if c.deps.Parks == nil {
		return
	}
	if since.IsZero() {
		log.Error(why.op, "the parked workspace has neither work nor a creation on record; its idle time before the park is not observed",
			dlog.Merge(fields, dlog.Context{"invariant_violation": "a parked workspace has a creation on record"}))
		return
	}
	idle := c.deps.Clock.Now().Sub(since)
	log.Debug(why.op, "observed how long the parked session stood idle", dlog.Merge(fields, dlog.Context{"idle_since": since, "idle": idle.String()}))
	c.deps.Parks.ObserveIdleBeforePark(why.life, idle)
}

// parkOnce is park's one attempt, before its hold and its observation.
func (c *controller) parkOnce(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, fields dlog.Context, why standDownReason) parkOutcome {
	if why.remote != remoteSpares && why.remote != remoteParks && why.remote != remoteBridges {
		panic(fmt.Sprintf("drain: the %s stand-down names no remote-control policy (%s)", why.op, why.remote))
	}
	fields = dlog.Merge(fields, dlog.Context{"trigger": why.trigger})
	lease, err := c.deps.DB.AcquireLease(ctx, ws, wsm.HolderHibernate, wsm.PolicyHold)
	if err != nil {
		log.Debug(why.op, "another holder has the lease; deferring the park",
			dlog.WithCause(fields, err))
		return parkDeferred
	}
	defer func() {
		if err := c.deps.DB.ReleaseLease(context.WithoutCancel(ctx), lease.ID); err != nil {
			log.Warn(why.op, "could not release the stand-down lease",
				dlog.WithCause(dlog.Merge(fields, dlog.Context{"lease": string(lease.ID)}), err))
			return
		}
		// TELLING THE QUEUE IS WHAT DRAINS THE INTAKE. A prompt that arrived
		// inside the stand-down window is held against this lease, and the
		// release alone changes a row the queue is not watching — so without
		// this the prompt that should have REVIVED the session waits forever.
		if c.deps.LeaseChanged != nil {
			c.deps.LeaseChanged(ws)
		}
		// AND THE HOST VIEW IS STALE UNTIL SOMETHING RECOMPOSES IT. The
		// composer arm was `draining` for as long as this lease stood, the
		// last push every host client received was taken while it was held,
		// and no other flow republishes after a park. Emacs refuses a
		// submission while its gate reads draining, so the republish is what
		// makes the parked workspace revivable by a prompt.
		c.publishHost(ws)
	}()

	// THE MODE AND THE ACT ARE ONE DECISION. The gate is released before the
	// lease is (defers run last-in first-out), so a turn-on that waited on
	// the gate finds the shim gone and the lease free for the start it asks.
	release := c.deps.HoldRemoteControl(ws)
	defer release()
	remote, err := c.deps.DB.RemoteControl(ctx, ws)
	if err != nil {
		log.Error(why.op, "could not read the workspace's remote-control mode; its stand-down is dropped", dlog.WithCause(fields, err))
		return parkFailed
	}
	fields = dlog.Merge(fields, dlog.Context{
		"remote_control":          remote.On,
		"remote_control_recorded": remote.Recorded,
		"remote_control_policy":   why.remote.String(),
	})
	if remote.On && why.remote == remoteSpares {
		log.Info(why.op, remoteSpared, fields)
		return parkSpared
	}

	// A DELIVERY UNDER WAY IS ENGAGEMENT. The gate read the session free
	// before the lease was taken, and a submission may have decided in
	// between: it read no lease and stood its shim call. The lease is taken
	// now, and the queue reads it under the lock Delivering answers under, so
	// a delivery not seen here is held against the lease, and one seen here
	// defers the stand-down to the turn it starts.
	if c.deps.Delivering(ws) {
		log.Info(why.op, "a prompt is being delivered to the shim; deferring the park", fields)
		return parkDeferred
	}
	// A BOUNCE DRAINING THE WORKSPACE OWNS IT. The bounce was decided on the
	// same free workspace before this lease was taken, and its relaunch
	// takes a restart hold this lease would refuse (2026-11-16 13:42:27,
	// merge-view-one-source: the bounce failed under the park's lease).
	if c.deps.Bouncing(ws) {
		log.Info(why.op, "a bounce is replacing the workspace's shim; deferring the park", fields)
		return parkDeferred
	}

	directive, cancelDirective := context.WithTimeout(ctx, c.deps.StandBound)
	answer, err := c.deps.Stand.Hibernate(directive, ws)
	cancelDirective()
	if err != nil {
		// THE RACE, NOT A FAULT. The pass selected a serving workspace, and
		// the session went away before the directive reached it. There is
		// nothing to stand down and nothing to fix, so it is recorded as the
		// state it is and deferred; the ERROR below still covers every
		// directive that failed against a shim that WAS there.
		if errors.Is(err, ErrNoLiveSession) {
			log.Debug(why.op, "the session went away before the hibernate directive; deferring the park",
				dlog.WithCause(fields, err))
			return parkDeferred
		}
		// NOR IS A STAND-DOWN THIS DAEMON ORDERED UNDER THE DIRECTIVE. A
		// teardown that does not wait on this lease (a landed merge closing
		// its workspace and removing its worktree) ended the shim while the
		// directive was in flight, so the session is gone for a reason this
		// daemon chose (shimclient.ErrStandDownOrdered), and the park is
		// deferred to find it gone.
		if errors.Is(err, shimclient.ErrStandDownOrdered) {
			log.Info(why.op, "this daemon stood the session down under the hibernate directive; deferring the park",
				dlog.WithCause(fields, err))
			return parkDeferred
		}
		log.Error(why.op, "the hibernate directive failed; deferring the park",
			dlog.WithCause(fields, err))
		return parkFailed
	}
	if refusal := answer.GetError(); refusal != nil {
		// A REFUSAL IS AN ANSWER. turn_in_flight simply defers; the other
		// arm is worth a warning, and defers just the same.
		refused := dlog.Merge(fields, dlog.Context{"refusal": hibernateRefusal(refusal)})
		if refusal.GetTurnInFlight() != nil {
			log.Debug(why.op, "a turn is in flight; deferring the park", refused)
			return parkDeferred
		}
		log.Warn(why.op, "the shim refused to hibernate; deferring", refused)
		return parkFailed
	}
	// AND EVERY OTHER ARM IS A DEFERRAL, NOT A STAND-DOWN. The arms above are
	// the whole of what this daemon knows how to read; anything else is a shim
	// built against a contract this daemon has not been taught, and standing a
	// session down on an answer nobody here understood could kill a turn the
	// shim was refusing to let go of.
	if answer.GetSuccess() == nil {
		log.Error(why.op, "the shim answered the hibernate directive with an arm this daemon cannot read; deferring the park", fields)
		return parkFailed
	}

	// THE FLEET RECORDS THE PARK, under the lock that orders it against the
	// next session's clear (workspace/terminalrecord.go); the sweep hands it
	// the park's own account.
	standDown, cancelStandDown := context.WithTimeout(shimlife.WithStandDownDetail(shimlife.WithStandDown(ctx, why.life), why.detail), c.deps.StandBound)
	err = c.deps.Stand.KillSession(standDown, ws, false)
	cancelStandDown()
	switch {
	case err == nil:
	case errors.Is(err, shimclient.ErrSIGKILLed):
		// THE PARK HAPPENED: the shim acked the hibernate, its stop ran out of
		// the bound, and the SIGKILL that followed ended it. Only its reap is
		// still owed, so the park is recorded below like any other; deferring
		// it left a gone shim's session reading as running (2026-11-23, five
		// workspaces the 15:35 handover then started).
		log.Info(why.op, "the shim acked the hibernate and its stop ran out of the bound, so it was SIGKILLed; the park is recorded",
			dlog.WithCause(fields, err))
	default:
		log.Error(why.op, "the graceful stand-down failed after the hibernate ack; deferring",
			dlog.WithCause(fields, err))
		return parkFailed
	}

	// The shim is gone, so the pid the intent manifest would name is gone with
	// it. A pid left behind would make the next reconciliation read a dead
	// session as preserved.
	if err := c.deps.DB.SetShimPID(ctx, ws, nil); err != nil {
		log.Error(why.op, "could not clear the parked session's shim pid", dlog.WithCause(fields, err))
		return parkFailed
	}
	// AND SO IS THE SPAWN THE REGISTRY RECORDS. That pid exists so a successor
	// daemon waits for a shim that is still starting instead of spawning a
	// second one; a parked workspace's shim is gone, and a pid left behind
	// would make the next boot wait out its whole adoption bound for it.
	if err := c.deps.DB.SetSpawnedShim(ctx, ws, nil); err != nil {
		log.Error(why.op, "could not clear the parked workspace's recorded spawn pid", dlog.WithCause(fields, err))
		return parkFailed
	}
	// AND SO IS THE FOOTER'S, AND THE CONNECTIVITY INDICATOR'S. Both are
	// in-memory accumulations fed by events, so unlike the roster they cannot
	// read the terminal back -- they are told here, after the record exists,
	// so no surface can report the park before the record that justifies it.
	//
	// A failure is not possible to report: these setters publish rather than
	// answer. What they change is that the footer's `disconnected` step stops
	// calling the deliberate stand-down a dead link, which is what reopens the
	// webapp's composer for the prompt that revives the session.
	if c.deps.SetParked != nil {
		c.deps.SetParked(ws, true)
	}
	// THE ROSTER'S ARM FOR THIS ROW IS A FUNCTION OF THE RECORD ABOVE. The
	// roster resolver reads the session terminal to know a park from a fault,
	// and it publishes on the events it is handed -- the last of which, the
	// shim link going dead, arrived during the KillSession above, before this
	// terminal existed. The row resolved on that event says `dead`, which
	// Emacs paints as "something on this machine broke" for a session the
	// daemon stood down on purpose. Nothing else republishes the roster after
	// a park, so this is the republish that makes the park READ as one.
	//
	// A failure here is recorded and nothing more: the park HAPPENED,
	// and reporting it as refused would leave the sweep retrying a session
	// that is already stood down. The stale view is the defect, and the
	// record is what names it.
	if c.deps.PublishRegistry != nil {
		if err := c.deps.PublishRegistry(ctx); err != nil {
			log.Error(why.op, "could not republish the roster after the park",
				dlog.WithCause(fields, err))
		}
	}
	log.Log(dlog.LevelInfo, why.op, why.done, fields)
	if remote.On && why.remote == remoteBridges {
		// THE BRIDGE COMES AFTER THE PARK IS RECORDED, so the shim it spawns
		// finds the conversation free; the fleet holds it detached.
		c.deps.BridgeGate(ctx, ws)
	}
	return parkDone
}

// workspaceLog resolves one workspace's own durable sink, or the central sink
// with the workspace named on the record when the workspace cannot host one.
// The sweep runs over every registry row, so its logging is TOTAL: a
// workspace's unavailable directory can cost the record's placement, never the
// sweep of that workspace.
func (c *controller) workspaceLog(ws wsm.Workspace) dlog.Logger {
	return c.deps.Log.WorkspaceOrCentral(ws.Dir).With(dlog.Context{"workspace": string(ws.ID)})
}

// hibernateRefusal names the shim's refusal arm for a record. The ARM is the
// fact; the detail it carries is evidence.
func hibernateRefusal(err *shimv1.HibernateError) string {
	switch {
	case err.GetTurnInFlight() != nil:
		return "turn_in_flight"
	case err.GetNoSession() != nil:
		return "no_session"
	default:
		return "unspecified"
	}
}

// Run drives the idle sweep on its cadence and fires the standing schedule when
// its deadline passes.
//
// One wait per iteration, sized to whichever comes first — the next sweep or
// the standing deadline — so a schedule armed between two sweeps is honored at
// its own instant. Schedule and Cancel wake the loop so it re-sizes the wait
// at once.
func (c *controller) Run(ctx context.Context) error {
	c.log.Debug(opRun, "the drain loop is running",
		dlog.Context{"sweep_every": c.deps.SweepEvery.String()})
	for {
		schedule, err := c.deps.DB.DrainSchedule(ctx)
		if err != nil {
			readFailed(c.log, opRun, logsite.Msg("could not read the standing drain schedule"),
				logsite.Msg("the schedule read ended when the drain loop's context was cancelled"), nil, err)
			return fmt.Errorf("drain: run: %w", err)
		}
		now := c.deps.Clock.Now()
		if schedule != nil && !now.Before(schedule.Deadline) && c.claimFire() {
			return c.fire(ctx, *schedule)
		}
		wait := c.deps.SweepEvery
		if schedule != nil {
			if until := schedule.Deadline.Sub(now); until > 0 && until < wait {
				wait = until
			}
		}
		select {
		case <-ctx.Done():
			c.log.Debug(opRun, "the drain loop stopped with its context", nil)
			return ctx.Err()
		case <-c.rearm:
			c.log.Debug(opRun, "the standing schedule changed; re-sizing the wait", nil)
		case <-c.deps.Clock.After(wait):
			if _, err := c.Sweep(ctx, c.deps.Clock.Now()); err != nil {
				return err
			}
		}
	}
}

// claimFire reports whether this pass is the one that fires the schedule, so a
// deadline observed twice announces once.
func (c *controller) claimFire() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.fired {
		return false
	}
	c.fired = true
	return true
}

// queuedSince answers when each workspace with a queued merge joined its
// queue.
func (c *controller) queuedSince(ctx context.Context) (map[ids.WorkspaceID]time.Time, error) {
	queues, err := c.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		readFailed(c.log, opSweep, logsite.Msg("could not read the merge queues to sweep"),
			logsite.Msg("the merge queues' read ended when the drain loop's context was cancelled"), nil, err)
		return nil, fmt.Errorf("drain: sweep: read the merge queues: %w", err)
	}
	since := map[ids.WorkspaceID]time.Time{}
	for _, entries := range queues {
		for _, e := range entries {
			since[e.Workspace] = e.EnqueuedAt
		}
	}
	return since, nil
}

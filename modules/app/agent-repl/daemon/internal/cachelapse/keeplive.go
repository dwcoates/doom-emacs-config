package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/drain"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE KEEP-LIVE VERB IS THE SCHEDULED BEAT, ASKED FOR NOW (owner request,
// 2026-11-20; agentrepl.v1.KeepLiveWorkspace). An agent that must touch many
// parked workspaces' sessions -- a model change across all of them -- used to
// start each shim and leave it up for hours (37 settings visits on
// 2026-11-20, 24 idle shims at once). The verb instead runs THE ONE
// KEEP-ALIVE SUBROUTINE the keeper's scheduled beats run (sendBeat), on this
// loop: a parked session is restored through the keeper's own restore slots,
// the keep-alive is sent, the vendor's answer is awaited (answer.go), and the
// keep-alive cycle's own stand-down (drain.Controller.CycleDown) is asked.
// The run only makes the beat due NOW, and remembers who is waiting.
//
// A SESSION THIS VERB STARTED IS ALWAYS ASKED TO STAND DOWN. One that was
// already up is left to the ordinary rules, exactly as after a scheduled
// beat: within the grace after its work it stays up, and the stand-down's own
// rules (the workspace in view, remote control on, a real turn since) keep it
// up too. The verb never pays a cold gate and never interrupts work: a gated
// workspace is refused before anything starts, and a busy session is
// answered busy by the shim's own judgment with nothing sent.

// The triggers a keep-alive is recorded under: the `trigger` field of every
// record the subroutine makes.
const (
	triggerScheduled = "scheduled_beat"
	triggerKeepLive  = "keep_live_verb"
)

// The keep-live verb's step bounds, each just over twice its measured healthy
// worst case (docs/protobuf-design/keep-live-workspace.md). The vendor's
// answer is bounded by AnswerBound, the scheduled beat's own.
const (
	// KeepLiveStartBound bounds a parked session's restore through its
	// keep-alive going out: agentrepl_shim_start_seconds{cause="keepalive"}
	// measured p99.9 27.3s over 30 days (p99 7.8s over 1,990 starts in 7),
	// 2026-11-20. A run waiting for a restore slot gets one bound per round
	// of MaxRestores starts ahead of it.
	KeepLiveStartBound    = 60 * time.Second
	keepLiveStartExpected = 27300 * time.Millisecond
	// KeepLiveStandDownBound bounds the keep-alive cycle's stand-down from
	// the vendor's answer to its park: 87 pairs in the daemon logs since
	// 2026-10-26 measured p50 1.0s, worst 29.3s (a burst of six), 2026-11-20.
	KeepLiveStandDownBound    = 60 * time.Second
	keepLiveStandDownExpected = 29300 * time.Millisecond
	// answerExpected is AnswerBound's measured healthy worst case: p99 57s
	// over 2,798 keep-alives.
	answerExpected = 57 * time.Second
)

// keepLiveStep is the step a keep-live run is in.
type keepLiveStep int

const (
	// stepStart is the session's restore, through its keep-alive going out.
	stepStart keepLiveStep = iota
	// stepAck is the wait for the vendor's answer, bounded by AnswerBound.
	stepAck
	// stepStandDown is the wait for the keep-alive cycle's stand-down.
	stepStandDown
)

func (s keepLiveStep) String() string {
	switch s {
	case stepStart:
		return "start"
	case stepAck:
		return "ack"
	case stepStandDown:
		return "stand_down"
	}
	panic(fmt.Sprintf("cachelapse: a keep-live step with no name (%d)", int(s)))
}

// keepLiveRun is one keep-live verb's run, owned by Run alone.
type keepLiveRun struct {
	ws ids.WorkspaceID
	// started reports that this run restored the session, at startedAt.
	started   bool
	startedAt time.Time
	step      keepLiveStep
	// by is when the start or the stand-down step outruns its bound; the
	// ack step is bounded by the answer's own (answerBy).
	by time.Time
	// ack is the vendor's answer, once it came.
	ack *agentreplv1.KeepLiveWorkspaceAck
	// steps are the steps taken, in order, for the closing record.
	steps []string
	// dones are the verb calls waiting on the run; a second call for the
	// same workspace joins the first run.
	dones []chan<- *agentreplv1.KeepLiveWorkspaceResponse
}

// KeepLive runs the keep-alive subroutine on WS now and answers how it ended
// (agentrepl.v1.KeepLiveWorkspace). The wait is bounded by the run's own step
// bounds, which this keeper's loop takes as it takes every deadline; ctx
// ending abandons the wait, not the run.
func (k *Keeper) KeepLive(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.KeepLiveWorkspaceResponse, error) {
	done := make(chan *agentreplv1.KeepLiveWorkspaceResponse, 1)
	k.enqueue(edge{ws: ws, kind: edgeKeepLive, keepLive: done, why: "an agent asked to keep the workspace live"})
	select {
	case resp := <-done:
		return resp, nil
	case <-ctx.Done():
		return nil, fmt.Errorf("cachelapse: the keep-live call ended before its run did: %w", ctx.Err())
	}
}

// beginKeepLive starts a keep-live run on WS, answering DONE at once when the
// workspace cannot be kept live, and joining a run already under way.
func (k *Keeper) beginKeepLive(ctx context.Context, ws ids.WorkspaceID, done chan<- *agentreplv1.KeepLiveWorkspaceResponse) {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "trigger": triggerKeepLive})
	if run := k.keepLives[ws]; run != nil {
		run.dones = append(run.dones, done)
		log.Info(opDrive, "a keep-live is already running on the workspace; the call waits on it", dlog.Context{"step": run.step.String()})
		return
	}
	refuse := func(outcome string, cause *agentreplv1.KeepLiveWorkspaceError, fields dlog.Context) {
		log.Info(opDrive, "the workspace was not kept live", dlog.Merge(fields, dlog.Context{"outcome": outcome}))
		done <- keepLiveError(cause)
	}
	record, err := k.deps.DB.Workspace(ctx, ws)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		refuse("unknown_workspace", &agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_UnknownWorkspace{
			UnknownWorkspace: &agentreplv1.KeepLiveWorkspaceUnknownWorkspace{},
		}}, nil)
		return
	case err != nil:
		log.Error(opDrive, "could not read the workspace to keep it live", dlog.Context{"cause": err.Error()})
		done <- keepLiveError(registryUnreadable(err.Error()))
		return
	case record.Closed:
		refuse("closed", &agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_Closed{
			Closed: &agentreplv1.KeepLiveWorkspaceClosed{},
		}}, nil)
		return
	}
	if _, gated := k.deps.ColdGate(ws); gated {
		refuse("cold_gated", &agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_ColdGated{
			ColdGated: &agentreplv1.KeepLiveWorkspaceColdGated{},
		}}, nil)
		return
	}
	now := k.deps.Clock.Now()
	run := &keepLiveRun{ws: ws, step: stepStart, by: now.Add(KeepLiveStartBound), dones: []chan<- *agentreplv1.KeepLiveWorkspaceResponse{done}}
	k.keepLives[ws] = run
	log.Info(opDrive, "keeping the workspace live: its keep-alive is due now", nil)
	// THE ONE SUBROUTINE (TestTheVerbAndTheScheduledBeatShareOneSubroutine).
	k.sendBeat(ctx, ws, now)
}

// forcing answers WS's keep-live run when it makes the workspace's beat due
// now: only before its keep-alive has gone out, so an edge that re-plans the
// beat afterwards times the next one from the clock as usual.
func (k *Keeper) forcing(ws ids.WorkspaceID) bool {
	run := k.keepLives[ws]
	return run != nil && run.step == stepStart
}

// triggerOf names who asked for WS's keep-alive, for the records.
func (k *Keeper) triggerOf(ws ids.WorkspaceID) string {
	if k.keepLives[ws] != nil {
		return triggerKeepLive
	}
	return triggerScheduled
}

// keepLiveRestoring notes that WS's run restores its session, which waits in
// line for a restore slot: its start bound covers every round of MaxRestores
// starts ahead of it.
func (k *Keeper) keepLiveRestoring(ws ids.WorkspaceID, now time.Time) {
	run := k.keepLives[ws]
	if run == nil || run.started {
		return
	}
	run.started, run.startedAt = true, now
	// AN UPPER BOUND ON THE LINE: every restore in flight or waiting.
	ahead := len(k.restoring) + len(k.queued)
	run.by = now.Add(time.Duration(ahead/MaxRestores+1) * KeepLiveStartBound)
	run.steps = append(run.steps, "restore_queued")
}

// keepLiveRestoreBegan re-arms WS's start bound from the moment its restore
// took a slot.
func (k *Keeper) keepLiveRestoreBegan(ws ids.WorkspaceID) {
	if run := k.keepLives[ws]; run != nil && run.step == stepStart {
		run.by = k.deps.Clock.Now().Add(KeepLiveStartBound)
		run.steps = append(run.steps, "restore_began")
	}
}

// keepLiveRetimed makes WS's beat due now again when its run's restore was
// dropped from the line because the session came up some other way.
func (k *Keeper) keepLiveRetimed(ws ids.WorkspaceID) {
	if k.forcing(ws) {
		k.beats[ws] = k.deps.Clock.Now()
	}
}

// keepLiveSent moves WS's run to the wait for the vendor's answer, and
// answers whether the session is stood down after it: always for a session
// the run started, after the grace for one that was already up. False for a
// workspace with no run.
func (k *Keeper) keepLiveSent(ws ids.WorkspaceID, withinGrace bool) (running, standDown bool) {
	run := k.keepLives[ws]
	if run == nil {
		return false, false
	}
	run.step = stepAck
	run.steps = append(run.steps, "keepalive_sent")
	return true, run.started || !withinGrace
}

// keepLiveFailed ends WS's run with CAUSE. STANDDOWN asks a session the run
// started to stand down, so no shim it started is left up; a failure whose
// own path already parks the session (a lapsed cache) passes false.
func (k *Keeper) keepLiveFailed(ws ids.WorkspaceID, outcome string, cause *agentreplv1.KeepLiveWorkspaceError, standDown bool) {
	run := k.keepLives[ws]
	if run == nil {
		return
	}
	if standDown && run.started && run.step < stepStandDown {
		k.deps.CycleDown(ws, run.startedAt, nil)
		run.steps = append(run.steps, "stand_down_asked")
	}
	k.finishKeepLive(run, outcome, keepLiveError(cause))
}

// keepLiveBusy ends WS's run: the session's work keeps the cache warm, and
// nothing was sent or stopped.
func (k *Keeper) keepLiveBusy(ws ids.WorkspaceID, busy *agentreplv1.KeepLiveWorkspaceBusy) {
	if run := k.keepLives[ws]; run != nil {
		k.finishKeepLive(run, "busy", &agentreplv1.KeepLiveWorkspaceResponse{Result: &agentreplv1.KeepLiveWorkspaceResponse_Success{
			Success: &agentreplv1.KeepLiveWorkspaceSuccess{Outcome: &agentreplv1.KeepLiveWorkspaceSuccess_Busy{Busy: busy}},
		}})
	}
}

// keepLiveAnswered takes the vendor's answer for WS's run: ACK, or nil when
// none came within AnswerBound. A session the rules stand down waits for the
// stand-down; one they leave up ends the run now.
func (k *Keeper) keepLiveAnswered(ws ids.WorkspaceID, ack *agentreplv1.KeepLiveWorkspaceAck, standDown bool) {
	run := k.keepLives[ws]
	if run == nil {
		return
	}
	if ack == nil {
		k.timedOut(run, stepAck, AnswerBound, answerExpected)
		return
	}
	run.ack = ack
	run.steps = append(run.steps, "vendor_answered")
	if !standDown {
		k.finishBeaten(run, &agentreplv1.KeepLiveWorkspaceBeaten{After: &agentreplv1.KeepLiveWorkspaceBeaten_LeftUp{
			LeftUp: &agentreplv1.KeepLiveWorkspaceLeftUp{Rule: withinGraceRule},
		}})
		return
	}
	run.step = stepStandDown
	run.by = k.deps.Clock.Now().Add(KeepLiveStandDownBound)
	run.steps = append(run.steps, "stand_down_asked")
}

// stoodDownThen is the stand-down's report for WS's run, queued back onto
// the loop; nil for a workspace with no run waiting on its stand-down.
func (k *Keeper) stoodDownThen(ws ids.WorkspaceID) func(drain.CycleOutcome) {
	run := k.keepLives[ws]
	if run == nil || run.step != stepStandDown {
		return nil
	}
	return func(outcome drain.CycleOutcome) {
		k.enqueue(edge{ws: ws, kind: edgeKeepLiveStoodDown, run: run, stoodDown: outcome, why: "the keep-live's stand-down settled"})
	}
}

// keepLiveStoodDown ends RUN with how its stand-down settled. A run already
// ended (its bound fired) takes nothing.
func (k *Keeper) keepLiveStoodDown(run *keepLiveRun, outcome drain.CycleOutcome) {
	if k.keepLives[run.ws] != run || run.step != stepStandDown {
		return
	}
	switch {
	case outcome.Failed:
		k.finishKeepLive(run, "stand_down_failed", keepLiveError(&agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_StandDownFailed{
			StandDownFailed: &agentreplv1.KeepLiveWorkspaceStandDownFailed{Detail: outcome.Why},
		}}))
	case outcome.Down:
		run.steps = append(run.steps, "stood_down")
		k.finishBeaten(run, &agentreplv1.KeepLiveWorkspaceBeaten{After: &agentreplv1.KeepLiveWorkspaceBeaten_StoodDown{
			StoodDown: &agentreplv1.KeepLiveWorkspaceStoodDown{},
		}})
	default:
		run.steps = append(run.steps, "left_up")
		k.finishBeaten(run, &agentreplv1.KeepLiveWorkspaceBeaten{After: &agentreplv1.KeepLiveWorkspaceBeaten_LeftUp{
			LeftUp: &agentreplv1.KeepLiveWorkspaceLeftUp{Rule: outcome.Why},
		}})
	}
}

// keepLivesDue ends every run whose start or stand-down outran its bound.
func (k *Keeper) keepLivesDue() {
	now := k.deps.Clock.Now()
	for _, run := range k.keepLives {
		if run.step == stepAck || now.Before(run.by) {
			continue
		}
		switch run.step {
		case stepStart:
			k.timedOut(run, stepStart, KeepLiveStartBound, keepLiveStartExpected)
		case stepStandDown:
			k.timedOut(run, stepStandDown, KeepLiveStandDownBound, keepLiveStandDownExpected)
		}
	}
}

// timedOut ends RUN with STEP's bound fired, recorded at ERROR. A session the
// run started and whose stand-down was not yet asked is asked to stand down.
func (k *Keeper) timedOut(run *keepLiveRun, step keepLiveStep, bound, expected time.Duration) {
	k.deps.Log.Global().Error(opDrive, "the keep-live outran its step's bound", dlog.Context{
		"workspace": string(run.ws), "trigger": triggerKeepLive, "step": step.String(),
		"bound": bound.String(), "expected": expected.String(), "steps": run.steps,
	})
	timed := &agentreplv1.KeepLiveWorkspaceTimedOut{BoundMs: bound.Milliseconds(), ExpectedMs: expected.Milliseconds()}
	switch step {
	case stepStart:
		timed.Step = &agentreplv1.KeepLiveWorkspaceTimedOut_Start{Start: &agentreplv1.KeepLiveWorkspaceTimedOutStart{}}
	case stepAck:
		timed.Step = &agentreplv1.KeepLiveWorkspaceTimedOut_Ack{Ack: &agentreplv1.KeepLiveWorkspaceTimedOutAck{}}
	case stepStandDown:
		timed.Step = &agentreplv1.KeepLiveWorkspaceTimedOut_StandDown{StandDown: &agentreplv1.KeepLiveWorkspaceTimedOutStandDown{}}
	}
	if run.started && step == stepStart {
		k.deps.CycleDown(run.ws, run.startedAt, nil)
		run.steps = append(run.steps, "stand_down_asked")
	}
	k.finishKeepLive(run, "timed_out_"+step.String(), keepLiveError(&agentreplv1.KeepLiveWorkspaceError{
		Cause: &agentreplv1.KeepLiveWorkspaceError_TimedOut{TimedOut: timed},
	}))
}

// finishBeaten ends RUN beaten, its start and ack as the run took them.
func (k *Keeper) finishBeaten(run *keepLiveRun, beaten *agentreplv1.KeepLiveWorkspaceBeaten) {
	if run.started {
		beaten.Start = &agentreplv1.KeepLiveWorkspaceBeaten_Started{Started: &agentreplv1.KeepLiveWorkspaceStarted{}}
	} else {
		beaten.Start = &agentreplv1.KeepLiveWorkspaceBeaten_AlreadyUp{AlreadyUp: &agentreplv1.KeepLiveWorkspaceAlreadyUp{}}
	}
	beaten.Ack = run.ack
	outcome := "beaten_stood_down"
	if beaten.GetLeftUp() != nil {
		outcome = "beaten_left_up"
	}
	k.finishKeepLive(run, outcome, &agentreplv1.KeepLiveWorkspaceResponse{Result: &agentreplv1.KeepLiveWorkspaceResponse_Success{
		Success: &agentreplv1.KeepLiveWorkspaceSuccess{Outcome: &agentreplv1.KeepLiveWorkspaceSuccess_Beaten{Beaten: beaten}},
	}})
}

// finishKeepLive answers every call waiting on RUN, records how it ended, and
// re-plans the workspace's beat from its clock, since the run made it due
// now.
func (k *Keeper) finishKeepLive(run *keepLiveRun, outcome string, resp *agentreplv1.KeepLiveWorkspaceResponse) {
	delete(k.keepLives, run.ws)
	k.deps.Log.Global().Info(opDrive, "the keep-live ended", dlog.Context{
		"workspace": string(run.ws), "trigger": triggerKeepLive, "outcome": outcome,
		"started": run.started, "steps": run.steps,
	})
	for _, done := range run.dones {
		done <- resp
	}
	k.enqueue(edge{ws: run.ws, why: "a keep-live ended"})
}

// answeredAck and failedAck map a lapse the shim restated, and a
// keep-alive's failed request, onto the verb's ack.
func answeredAck(lapsesAt time.Time) *agentreplv1.KeepLiveWorkspaceAck {
	return &agentreplv1.KeepLiveWorkspaceAck{Answer: &agentreplv1.KeepLiveWorkspaceAck_Answered{
		Answered: &agentreplv1.KeepLiveWorkspaceAnswered{LapsesAtMs: lapsesAt.UnixMilli()},
	}}
}

func failedAck(detail string) *agentreplv1.KeepLiveWorkspaceAck {
	return &agentreplv1.KeepLiveWorkspaceAck{Answer: &agentreplv1.KeepLiveWorkspaceAck_RequestFailed{
		RequestFailed: &agentreplv1.KeepLiveWorkspaceRequestFailed{Detail: detail},
	}}
}

// busyOf maps the shim's busy judgment onto the verb's busy answer.
func busyOf(skipped *shimv1.SendKeepaliveSkipped) *agentreplv1.KeepLiveWorkspaceBusy {
	if live := skipped.GetDetachedWorkLive(); live != nil {
		return &agentreplv1.KeepLiveWorkspaceBusy{Work: &agentreplv1.KeepLiveWorkspaceBusy_DetachedWork{
			DetachedWork: &agentreplv1.KeepLiveWorkspaceBusyDetachedWork{LiveItems: live.GetLiveItems()},
		}}
	}
	return &agentreplv1.KeepLiveWorkspaceBusy{Work: &agentreplv1.KeepLiveWorkspaceBusy_MainAgent{
		MainAgent: &agentreplv1.KeepLiveWorkspaceBusyMainAgent{Detail: skipped.GetMainAgentBusy().GetDetail()},
	}}
}

func keepLiveError(cause *agentreplv1.KeepLiveWorkspaceError) *agentreplv1.KeepLiveWorkspaceResponse {
	return &agentreplv1.KeepLiveWorkspaceResponse{Result: &agentreplv1.KeepLiveWorkspaceResponse_Error{Error: cause}}
}

func registryUnreadable(detail string) *agentreplv1.KeepLiveWorkspaceError {
	return &agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_RegistryUnreadable{
		RegistryUnreadable: &agentreplv1.KeepLiveWorkspaceRegistryUnreadable{Detail: detail},
	}}
}

func beatFailed(detail string) *agentreplv1.KeepLiveWorkspaceError {
	return &agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_BeatFailed{
		BeatFailed: &agentreplv1.KeepLiveWorkspaceBeatFailed{Detail: detail},
	}}
}

func startFailed(detail string) *agentreplv1.KeepLiveWorkspaceError {
	return &agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_StartFailed{
		StartFailed: &agentreplv1.KeepLiveWorkspaceStartFailed{Detail: detail},
	}}
}

func owesNoKeepalive(reason string) *agentreplv1.KeepLiveWorkspaceError {
	return &agentreplv1.KeepLiveWorkspaceError{Cause: &agentreplv1.KeepLiveWorkspaceError_OwesNoKeepalive{
		OwesNoKeepalive: &agentreplv1.KeepLiveWorkspaceOwesNoKeepalive{Reason: reason},
	}}
}

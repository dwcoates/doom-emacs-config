package merge

import (
	"context"
	"errors"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is the STEP ASIDE (owner ruling, 2026-10-31): a merge at the
// head of its queue whose WHOLE test suite finished with at least one
// failure gives up the head at once and fixes its failures OUT OF LINE, so
// the next merge in line takes the head and runs while it does.
//
//   - THE TRIGGER is the end of the whole suite, never its first failing
//     unit: the gate's verdict, read once every unit ran. A head with nobody
//     in line behind it has nobody to yield to, and fixes in place.
//   - STEPPING ASIDE: the fixing prompt is delivered first (the queue's
//     hand-off is whole, so the turn lives on in the session), then the
//     entry leaves the line (wsm.StepAsideMerge, MergeRemediating) and the
//     run is suspended as a race's loser is (suspendFor), keeping its lease
//     and its progress record, and taken back (AdoptWorkspace) as a merge
//     remediating.
//   - RETURNING: once the fixing turn ends, the merge goes back in line at
//     the FIRST WAITING PLACE -- ahead of every merge waiting, behind every
//     merge being worked on, which it never evicts (wsm.ReturnRemediatedMerge)
//     -- admitted, so it resumes from its record once it heads the queue: it
//     reads the fixing turn's end, rebases onto the target's tip and runs its
//     whole suite again, as the one fixing attempt it is.
//   - A RESTART keeps all of it: the entry's state is durable, and the boot's
//     take-back of a remediating entry re-arms the wait on its fixing turn
//     (recoverEntry).

// errSteppedAside is the cause a head that stepped aside to fix its failing
// tests is suspended with.
var errSteppedAside = errors.New("merge: the merge's tests failed; it stepped aside to fix them out of line")

// suspendStepAside is the suspension of a head stepping aside.
var suspendStepAside = suspension{op: opRemediate, cause: errSteppedAside,
	moved:     "the merge's tests failed; it stepped aside to fix them out of line, and the next merge takes the head",
	by:        "its step aside to fix its failing tests",
	resumes:   "it returns to the line once its fixing turn ends, next after the merge being worked on",
	gateRerun: "it runs its whole suite again once it heads the queue again"}

// opRemediate is the step aside's record operation.
const opRemediate = "daemon.merge.remediate"

// remediation is one merge remediating out of line: its queue, and the cancel
// of the wait on its fixing turn.
type remediation struct {
	repo   wsm.RepoKey
	cancel context.CancelFunc
	// since is when this daemon took the merge as remediating: its step
	// aside, or a restart's take-back of it (the sidebar's clock).
	since time.Time
}

// stepsAside reports whether R, whose suite just finished with failures,
// steps aside: it heads its queue and another merge stands in line. A read
// that fails is recorded at ERROR and answers false, so the merge fixes in
// place as it did before stepping aside existed.
func (o *orchestrator) stepsAside(ctx context.Context, r *run) bool {
	o.mu.Lock()
	head := o.running[r.repo] == r
	o.mu.Unlock()
	if !head {
		return false
	}
	entries, err := o.deps.DB.MergeQueue(ctx, r.repo)
	if err != nil {
		o.log(ctx, r.ws).Error(opRemediate, "could not read the queue to decide whether the failing head steps aside; it fixes its tests at the head",
			dlog.Context{"workspace": string(r.ws), "error": err.Error()})
		return false
	}
	for _, entry := range inLine(entries) {
		if entry.Workspace != r.ws {
			return true
		}
	}
	return false
}

// stepAside takes R out of the head once its fixing prompt was handed off:
// its entry leaves the line, its run is suspended, and it is taken back as a
// merge remediating. It runs on its own goroutine, since the suspension
// waits for the very run that asked for it. A step that fails is recorded at
// ERROR; the merge then fixes at the head as before.
func (o *orchestrator) stepAside(r *run) {
	ctx := context.Background()
	log := o.log(ctx, r.ws)
	fields := dlog.Context{"workspace": string(r.ws), "repo": string(r.repo)}
	if err := o.deps.DB.StepAsideMerge(ctx, r.repo, r.ws); err != nil {
		log.Error(opRemediate, "could not take the failing head out of line; it fixes its tests at the head", withField(fields, "error", err.Error()))
		return
	}
	if err := o.suspendFor(ctx, r.ws, suspendStepAside); err != nil {
		log.Error(opRemediate, "could not suspend the failing head that stepped aside", withField(fields, "error", err.Error()))
		return
	}
	if err := o.AdoptWorkspace(ctx, r.ws); err != nil {
		log.Error(opRemediate, "could not take back the merge that stepped aside to remediate", withField(fields, "error", err.Error()))
		return
	}
	log.Info(opRemediate, "the head's tests failed; it stepped aside to fix them out of line, and the next merge takes the head", fields)
	o.kick(r.repo)
}

// goStepAside runs stepAside for R on its own goroutine, counted among the
// orchestrator's live work so a drain waits for it.
func (o *orchestrator) goStepAside(r *run) {
	o.live.Add(1)
	go func() {
		defer o.live.Done()
		o.stepAside(r)
	}()
}

// awaitRemediation arms the wait on WS's fixing TURN, after which the merge
// returns to REPO's line (waitThenReturn). A wait already armed for WS is
// replaced.
func (o *orchestrator) awaitRemediation(ws ids.WorkspaceID, repo wsm.RepoKey, turn ids.TurnID) {
	ctx, cancel := context.WithCancel(context.Background())
	o.mu.Lock()
	since := o.deps.Now()
	if standing, ok := o.remediating[ws]; ok {
		standing.cancel()
		since = standing.since
	}
	o.remediating[ws] = &remediation{repo: repo, cancel: cancel, since: since}
	o.mu.Unlock()
	if !o.async {
		return
	}
	o.live.Add(1)
	go func() {
		defer o.live.Done()
		o.waitThenReturn(ctx, ws, repo, turn)
	}()
}

// waitThenReturn waits for WS's fixing TURN to end, then returns the merge to
// REPO's line. A wait it cannot keep (the workspace has no session to follow
// the turn in) returns the merge at once, recorded at INFO: its run reads the
// turn's end, or delivers it again, when it heads the queue (reattachTurn).
func (o *orchestrator) waitThenReturn(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, turn ids.TurnID) {
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "turn": string(turn)}
	closes, err := o.deps.DB.TurnCloses(ctx, ws, []ids.TurnID{turn})
	if err != nil {
		log.Error(opRemediate, "could not read whether the fixing turn ended; the merge stays out of line until the next boot", withField(fields, "error", err.Error()))
		return
	}
	if _, ended := closes[turn]; !ended {
		log.Info(opRemediate, "the merge remediates out of line; it returns to the line once its fixing turn ends", fields)
		if _, err := o.deps.AwaitTurnEnd(ctx, ws, turn); err != nil {
			if ctx.Err() != nil {
				log.Info(opRemediate, "the wait on the fixing turn was withdrawn", fields)
				return
			}
			log.Info(opRemediate, "could not follow the fixing turn; the merge returns to the line now and reads the turn's end when it heads the queue",
				withField(fields, "cause", err.Error()))
		}
	}
	if err := o.returnRemediated(ctx, ws, repo); err != nil {
		log.Error(opRemediate, "could not return the remediated merge to the line; it stays out of line until the next boot", withField(fields, "error", err.Error()))
	}
}

// returnRemediated puts the remediating merge WS back in REPO's line, next
// after the merge being worked on.
func (o *orchestrator) returnRemediated(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey) error {
	if err := o.deps.DB.ReturnRemediatedMerge(ctx, repo, ws); err != nil {
		return err
	}
	o.mu.Lock()
	delete(o.remediating, ws)
	o.mu.Unlock()
	o.log(ctx, ws).Info(opRemediate, "the merge's fixing turn ended; it is back in line, next after the merge being worked on",
		dlog.Context{"workspace": string(ws), "repo": string(repo)})
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// isRemediating reports whether WS's merge remediates out of line. The caller
// holds o.mu.
func (o *orchestrator) isRemediatingLocked(ws ids.WorkspaceID) bool {
	_, ok := o.remediating[ws]
	return ok
}

// remediatingSince answers when WS took up remediating out of line here, the
// zero time when it does not remediate.
func (o *orchestrator) remediatingSince(ws ids.WorkspaceID) time.Time {
	o.mu.Lock()
	defer o.mu.Unlock()
	if standing, ok := o.remediating[ws]; ok {
		return standing.since
	}
	return time.Time{}
}

// forgetRemediation withdraws WS's wait on its fixing turn, when one stands.
// The caller holds o.mu.
func (o *orchestrator) forgetRemediationLocked(ws ids.WorkspaceID) {
	if standing, ok := o.remediating[ws]; ok {
		standing.cancel()
		delete(o.remediating, ws)
	}
}

// fixOrigin is the origin of a fixing attempt's prompt.
const fixOrigin = conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR

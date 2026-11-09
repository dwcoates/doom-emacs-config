package merge

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// This file is the STEP ASIDE (owner rulings, 2026-10-31 and 2026-11-03): a
// merge at the head of its queue whose next step waits on an agent turn of
// its own -- fixing the failures its WHOLE test suite finished with, or
// resolving the conflicts its rebase met -- gives up the head at once and
// repairs OUT OF LINE, so the next merge in line takes the head and runs
// while it does. Both reasons (AsideReason) take the ONE path below; nothing
// else steps a merge aside.
//
//   - THE TRIGGERS are the end of the whole suite with a failure (the gate's
//     verdict, read once every unit ran, never its first failing unit), and a
//     replayed commit that conflicts. A merge with nobody in line behind it
//     has nobody to yield to, and repairs in place (stepsAside).
//   - WHO STEPS ASIDE: the head, for either reason; and, for a conflict, a
//     member racing behind the head (chain.go), so the next merge in line
//     takes its place in the race while its conflicts are resolved.
//   - STEPPING ASIDE: the repairing prompt is delivered first (the queue's
//     hand-off is whole, so the turn lives on in the session), then the
//     entry leaves the line (wsm.StepAsideMerge, MergeRemediating) BEFORE the
//     run leaves its chain (leaveLine), and the run is cut, on its own
//     goroutine, before it waits for the repairing turn (stepAside), keeping
//     its lease and its progress record, and taken back (AdoptWorkspace) as a
//     merge remediating once it let go.
//   - RETURNING: once the repairing turn ends, the merge goes back in line at
//     the FIRST WAITING PLACE -- ahead of every merge waiting, behind every
//     merge being worked on, which it never evicts (wsm.ReturnRemediatedMerge)
//     -- admitted, so it resumes from its record once it heads the queue: it
//     reads the turn's end, rebases onto the target's tip and runs its whole
//     suite again. After a test step aside that suite run is the one fixing
//     attempt; a conflict step aside spends no fixing attempt (the conflict
//     came from what landed ahead of it), and a rebase onto the new tip that
//     conflicts again steps aside again, since every step aside lets another
//     merge land.
//   - THE REASON IS THE RECORD'S: the progress record names the step whose
//     turn the merge waits on (asideReasonOf), so what the status and the
//     sidebar say a merge repairs survives a restart with nothing else stored.
//   - A RESTART KEEPS ALL OF IT: the entry's state is durable, and the boot's
//     take-back of a remediating entry re-arms the wait on its repairing turn
//     (recoverEntry).

// AsideReason is why a merge stepped aside out of line: the step whose agent
// turn it repairs in.
type AsideReason int

const (
	// AsideForTests is a merge whose whole test suite finished with failures:
	// its fixing turn repairs them.
	AsideForTests AsideReason = iota + 1
	// AsideForConflicts is a merge whose rebase met conflicts: its session
	// resolves them.
	AsideForConflicts
)

// String names the reason in records.
func (why AsideReason) String() string {
	switch why {
	case AsideForTests:
		return "tests"
	case AsideForConflicts:
		return "conflicts"
	}
	panic(fmt.Sprintf("merge: AsideReason(%d) names no reason", int(why)))
}

// suspension is the suspension a merge stepping aside for WHY is cut with.
func (why AsideReason) suspension() suspension {
	switch why {
	case AsideForTests:
		return suspendAsideForTests
	case AsideForConflicts:
		return suspendAsideForConflicts
	}
	panic(fmt.Sprintf("merge: AsideReason(%d) names no reason", int(why)))
}

// racing reports whether a member racing behind the head steps aside for WHY
// too: a conflict does (owner ruling, 2026-11-03), failing tests behind the
// head are fixed where they stand.
func (why AsideReason) racing() bool { return why == AsideForConflicts }

// asideReasonOf answers why the remediating merge DOC records stepped aside:
// the step whose turn its record waits on. A record at any other step is no
// merge remediating, and is answered as the defect it is.
func asideReasonOf(doc progressDoc) (AsideReason, error) {
	switch doc.Step {
	case TabFixes:
		return AsideForTests, nil
	case TabConflicts:
		return AsideForConflicts, nil
	}
	return 0, fmt.Errorf("merge: a remediating merge's record stands at the %q step, which waits on no repairing turn", doc.Step)
}

// asideReasonFor answers why WS's merge, remediating out of line, stepped
// aside, read off its durable progress record (asideReasonOf) through DB. A
// remediating merge with no record, or one that will not decode, is a defect
// and is answered as an error for the caller to record.
func asideReasonFor(ctx context.Context, db wsm.DB, ws ids.WorkspaceID) (AsideReason, error) {
	stored, found, err := db.MergeProgressOf(ctx, ws)
	if err != nil {
		return 0, fmt.Errorf("merge: reading the progress record of the remediating merge of %q: %w", ws, err)
	}
	if !found {
		return 0, fmt.Errorf("merge: the remediating merge of %q has no progress record", ws)
	}
	doc, err := decodeProgress(stored)
	if err != nil {
		return 0, fmt.Errorf("merge: decoding the progress record of the remediating merge of %q: %w", ws, err)
	}
	return asideReasonOf(doc)
}

// roster is the reason as the sidebar's remediating card draws it.
func (why AsideReason) roster() *frontendv1.RosterMergeQueueRemediatingReason {
	switch why {
	case AsideForTests:
		return &frontendv1.RosterMergeQueueRemediatingReason{Reason: &frontendv1.RosterMergeQueueRemediatingReason_Tests{
			Tests: &frontendv1.RosterMergeQueueRemediatingTests{}}}
	case AsideForConflicts:
		return &frontendv1.RosterMergeQueueRemediatingReason{Reason: &frontendv1.RosterMergeQueueRemediatingReason_Conflicts{
			Conflicts: &frontendv1.RosterMergeQueueRemediatingConflicts{}}}
	}
	panic(fmt.Sprintf("merge: AsideReason(%d) names no reason", int(why)))
}

// errAsideForTests and errAsideForConflicts are the causes a merge that
// stepped aside is suspended with.
var (
	errAsideForTests     = errors.New("merge: the merge's tests failed; it stepped aside to fix them out of line")
	errAsideForConflicts = errors.New("merge: the merge's rebase conflicted; it stepped aside to resolve the conflicts out of line")
)

// The suspensions of a merge stepping aside, one per reason.
var (
	suspendAsideForTests = suspension{op: opRemediate, cause: errAsideForTests,
		moved:     "the merge's tests failed; it stepped aside to fix them out of line, and the next merge takes its place",
		by:        "its step aside to fix its failing tests",
		resumes:   "it returns to the line once its fixing turn ends, next after the merge being worked on",
		gateRerun: "it runs its whole suite again once it heads the queue again"}
	suspendAsideForConflicts = suspension{op: opRemediate, cause: errAsideForConflicts,
		moved:     "the merge's rebase conflicted; it stepped aside to resolve the conflicts out of line, and the next merge takes its place",
		by:        "its step aside to resolve its rebase's conflicts",
		resumes:   "it returns to the line once its resolving turn ends, next after the merge being worked on",
		gateRerun: "it rebases onto the target's tip and runs its whole suite once it heads the queue again"}
)

// opRemediate is the step aside's record operation.
const opRemediate = "daemon.merge.remediate"

// remediation is one merge remediating out of line: its queue, and the cancel
// of the wait on its repairing turn.
type remediation struct {
	repo   wsm.RepoKey
	cancel context.CancelFunc
}

// asideOn answers what the prompt of R's step that waits on an agent turn for
// WHY calls once it is handed off: R's step aside, when R steps aside
// (stepsAside), and nil when it repairs where it stands. It is the ONE way a
// step reaches stepAside.
func (r *run) asideOn(ctx context.Context, why AsideReason) func() {
	if !r.o.stepsAside(ctx, r, why) {
		return nil
	}
	return func() { r.o.stepAside(r, why) }
}

// stepsAside reports whether R steps aside for WHY: it heads its queue, or --
// for a reason a racing member steps aside for -- races behind the head, and
// another merge stands in line behind it. A read that fails is recorded at
// ERROR and answers false, so the merge repairs where it stands.
func (o *orchestrator) stepsAside(ctx context.Context, r *run, why AsideReason) bool {
	o.mu.Lock()
	head := o.running[r.repo] == r
	racing := !head && why.racing() && r.speculative && o.inChainLocked(r.repo, r.ws)
	o.mu.Unlock()
	if !head && !racing {
		return false
	}
	entries, err := o.deps.DB.MergeQueue(ctx, r.repo)
	if err != nil {
		o.log(ctx, r.ws).Error(opRemediate, "could not read the queue to decide whether the merge steps aside; it repairs where it stands",
			dlog.Context{"workspace": string(r.ws), "repo": string(r.repo), "reason": why.String(), "error": err.Error()})
		return false
	}
	line := inLine(entries)
	at := slices.IndexFunc(line, func(entry wsm.MergeQueueEntry) bool { return entry.Workspace == r.ws })
	for _, entry := range line[at+1:] {
		if entry.Workspace != r.ws {
			return true
		}
	}
	return false
}

// stepAside takes R out of its place once its repairing prompt for WHY was
// handed off. It runs ON THE RUN'S OWN GOROUTINE, as the hand-off's
// SUBMITTED, before the run waits for the repairing turn's end: the entry
// leaves the line, and the run is marked suspending and cut right here, so
// the wait it goes on to begins already cut and the run can never go on past
// the hand-off -- to read the turn's end, test again and land at the head it
// gave up (2026-10-31: a step aside on its own goroutine lost that race to a
// fixing turn that ended at once, and the merge landed under its remediating
// entry). The take-back waits for the run to let go, so it alone runs on its
// own goroutine (takeBackSteppedAside). A step that fails is recorded at
// ERROR; the merge then repairs where it stands as before.
func (o *orchestrator) stepAside(r *run, why AsideReason) {
	ctx := context.Background()
	log := o.log(ctx, r.ws)
	fields := dlog.Context{"workspace": string(r.ws), "repo": string(r.repo), "reason": why.String()}
	// THE ENTRY LEAVES THE LINE BEFORE THE RUN LEAVES ITS CHAIN (leaveLine),
	// under the line lock: a chain decision that read the line with the merge
	// still in it never meets a chain the merge already left (2026-10-31,
	// TestTwoMergesOfARepositoryAlternateTheTestTurnAndTheirGatesNeverOverlap).
	if err := o.leaveLine(r, func() error { return o.deps.DB.StepAsideMerge(ctx, r.repo, r.ws) }); err != nil {
		log.Error(opRemediate, "could not take the merge out of line to step aside; it repairs where it stands", withField(fields, "error", err.Error()))
		return
	}
	o.mu.Lock()
	// A RUN ANOTHER END TOOK FIRST -- a dequeue's abandonment, or a pause, a
	// transfer or the daemon's exit suspending it -- is that end's: its entry
	// stands remediating, which that end's drop or take-back reads
	// (recoverRemediating).
	if r.suspending || r.abandonedBy != "" {
		o.mu.Unlock()
		log.Info(opRemediate, "the merge stepped aside as another end took its run; that end settles the merge remediating", fields)
		return
	}
	aside := why.suspension()
	o.takenAway[r.ws] = aside
	r.suspendLocked(aside)
	o.mu.Unlock()
	fields["lease"] = string(r.lease.ID)
	o.cutSuspended(o.deps.Log.Global(), r, aside, fields)
	o.live.Add(1)
	inflight.Go(func() {
		defer o.live.Done()
		o.takeBackSteppedAside(r, aside, fields)
	})
}

// takeBackSteppedAside waits for R, cut by its step aside ASIDE, to let go,
// and takes it back as a merge remediating (AdoptWorkspace,
// recoverRemediating). A take-back that fails is recorded at ERROR.
func (o *orchestrator) takeBackSteppedAside(r *run, aside suspension, fields dlog.Context) {
	if o.onStepAside != nil {
		o.onStepAside(r.ws)
	}
	ctx := context.Background()
	log := o.log(ctx, r.ws)
	if err := o.awaitLetGo(ctx, o.deps.Log.Global(), opRemediate, r, fields); err != nil {
		log.Error(opRemediate, "the merge that stepped aside did not let go", withField(fields, "error", err.Error()))
		return
	}
	if err := o.AdoptWorkspace(ctx, r.ws); err != nil {
		log.Error(opRemediate, "could not take back the merge that stepped aside to remediate", withField(fields, "error", err.Error()))
		return
	}
	log.Info(opRemediate, "took back the merge that stepped aside as remediating out of line; "+aside.resumes, fields)
	o.kick(r.repo)
}

// awaitRemediation arms the wait on WS's repairing TURN, after which the merge
// returns to REPO's line (waitThenReturn). A wait already armed for WS is
// replaced.
func (o *orchestrator) awaitRemediation(ws ids.WorkspaceID, repo wsm.RepoKey, turn ids.TurnID) {
	ctx, cancel := context.WithCancel(context.Background())
	o.mu.Lock()
	if standing, ok := o.remediating[ws]; ok {
		standing.cancel()
	}
	o.remediating[ws] = &remediation{repo: repo, cancel: cancel}
	o.mu.Unlock()
	if !o.async {
		return
	}
	o.live.Add(1)
	inflight.Standing(func() {
		defer o.live.Done()
		o.waitThenReturn(ctx, ws, repo, turn)
	})
}

// waitThenReturn waits for WS's repairing TURN to end, then returns the merge to
// REPO's line. A wait it cannot keep (the workspace has no session to follow
// the turn in) returns the merge at once, recorded at INFO: its run reads the
// turn's end, or delivers it again, when it heads the queue (reattachTurn).
func (o *orchestrator) waitThenReturn(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, turn ids.TurnID) {
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "turn": string(turn)}
	closes, err := o.deps.DB.TurnCloses(ctx, ws, []ids.TurnID{turn})
	if err != nil {
		log.Error(opRemediate, "could not read whether the repairing turn ended; the merge stays out of line until the next boot", withField(fields, "error", err.Error()))
		return
	}
	if _, ended := closes[turn]; !ended {
		log.Info(opRemediate, "the merge remediates out of line; it returns to the line once its repairing turn ends", fields)
		if _, err := o.deps.AwaitTurnEnd(ctx, ws, turn); err != nil {
			if ctx.Err() != nil {
				log.Info(opRemediate, "the wait on the repairing turn was withdrawn", fields)
				return
			}
			log.Info(opRemediate, "could not follow the repairing turn; the merge returns to the line now and reads the turn's end when it heads the queue",
				withField(fields, "cause", err.Error()))
		}
	}
	if err := o.returnRemediated(ctx, ws, repo); err != nil {
		log.Error(opRemediate, "could not return the remediated merge to the line; it stays out of line until the next boot", withField(fields, "error", err.Error()))
	}
}

// returnRemediated puts the remediating merge WS back in REPO's line, next
// after the merge being worked on.
//
// THE RETURN IS ORDERED WITH EVERY TAKING-UP OF A MERGE (lineLock). Read
// between the pump's look at its front and that front's admission, the return
// stood ahead of a merge the pump was already starting as the head, and the
// chain then stood behind an entry it held no run for (2026-10-31,
// TestTwoMergesOfARepositoryAlternateTheTestTurnAndTheirGatesNeverOverlap).
func (o *orchestrator) returnRemediated(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey) error {
	if err := o.lineWrite(repo, func() error {
		return o.deps.DB.ReturnRemediatedMerge(ctx, repo, ws, o.workedOn(repo))
	}); err != nil {
		return err
	}
	o.mu.Lock()
	delete(o.remediating, ws)
	o.mu.Unlock()
	o.log(ctx, ws).Info(opRemediate, "the merge's repairing turn ended; it is back in line, next after the merge being worked on",
		dlog.Context{"workspace": string(ws), "repo": string(repo)})
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// lineLock answers REPO's line lock (orchestrator.lines).
func (o *orchestrator) lineLock(repo wsm.RepoKey) *sync.Mutex {
	o.mu.Lock()
	defer o.mu.Unlock()
	line, ok := o.lines[repo]
	if !ok {
		line = &sync.Mutex{}
		o.lines[repo] = line
	}
	return line
}

// lineWrite makes WRITE, a store write that moves an entry of REPO's line
// ahead of, behind or out of the chain's members, under the line lock: a
// decision that read the line (admitFront, extendOnce) never meets a line
// that moved under it.
func (o *orchestrator) lineWrite(repo wsm.RepoKey, write func() error) error {
	line := o.lineLock(repo)
	if o.onLineWait != nil {
		o.onLineWait(repo)
	}
	line.Lock()
	defer line.Unlock()
	return write()
}

// leaveLine takes R's entry out of its line through WRITE (a removal, a step
// aside), under the line lock and with R marked as having left it first
// (run.leftLine), so every read of the line agrees with the chain: one before
// the write meets R in its chain, unmarked, and one after it meets R marked
// or already gone. Every run leaves its line this way BEFORE it leaves its
// chain (releaseSlot). A write that fails leaves the entry standing, and the
// mark is taken back.
func (o *orchestrator) leaveLine(r *run, write func() error) error {
	return o.lineWrite(r.repo, func() error {
		o.mu.Lock()
		r.leftLine = true
		o.mu.Unlock()
		err := write()
		if err != nil {
			o.mu.Lock()
			r.leftLine = false
			o.mu.Unlock()
		}
		return err
	})
}

// workedOn answers the merges of REPO this orchestrator has taken up: its
// running head and every member of its chain.
func (o *orchestrator) workedOn(repo wsm.RepoKey) []ids.WorkspaceID {
	o.mu.Lock()
	defer o.mu.Unlock()
	var out []ids.WorkspaceID
	if head := o.running[repo]; head != nil {
		out = append(out, head.ws)
	}
	for _, member := range o.chains[repo] {
		if !slices.Contains(out, member.ws) {
			out = append(out, member.ws)
		}
	}
	return out
}

// isRemediating reports whether WS's merge remediates out of line. The caller
// holds o.mu.
func (o *orchestrator) isRemediatingLocked(ws ids.WorkspaceID) bool {
	_, ok := o.remediating[ws]
	return ok
}

// forgetRemediation withdraws WS's wait on its repairing turn, when one stands.
// The caller holds o.mu.
func (o *orchestrator) forgetRemediationLocked(ws ids.WorkspaceID) {
	if standing, ok := o.remediating[ws]; ok {
		standing.cancel()
		delete(o.remediating, ws)
	}
}

// fixOrigin is the origin of a fixing attempt's prompt.
const fixOrigin = conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR

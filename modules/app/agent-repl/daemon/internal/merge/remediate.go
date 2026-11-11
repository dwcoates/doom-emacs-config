package merge

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// This file is the STEP ASIDE and the queue's two REPAIR SLOTS (owner
// rulings, 2026-10-31, 2026-11-03 and 2026-11-11): a merge at the head of its
// queue whose next step waits on an agent turn of its own -- fixing the
// failures its WHOLE test suite finished with, or resolving the conflicts its
// rebase met -- gives up the head at once and repairs OUT OF LINE, so the next
// merge in line takes the head and runs while it does. At most ONE merge of a
// queue fixes tests out of line and at most ONE resolves conflicts out of
// line; with the head, a queue has at most three active merges.
//
//   - THE TRIGGERS are the end of the whole suite with a failure (the gate's
//     verdict, read once every unit ran, never its first failing unit), and a
//     replayed commit that conflicts. A merge with nobody in line behind it
//     has nobody to yield to, and repairs in place, holding the head
//     (stepsAside).
//   - THE SLOTS ARE THE STORE'S (wsm.MergeSlot): the step aside's one write
//     (wsm.StepAsideMerge) takes the repair's slot when nobody holds it, or
//     a place awaiting it behind every merge that stepped aside for that
//     repair before, and answers which. A release -- a return to the line, a
//     drop -- grants the slot to the next merge awaiting it IN THE SAME
//     WRITE, and the grant is acted on here (armRepairs) by reading the
//     queue that write left: never a retry or a timer. The store refuses a
//     second holder outright (its one-holder index), and the arming refuses
//     one it would start, loudly (armRepairs).
//   - STEPPING ASIDE: the repairing brief and its turn are RECORDED, never
//     submitted by the run (handOff); the entry leaves the line BEFORE the
//     run leaves its chain (leaveLine), and the run is cut and taken back
//     (AdoptWorkspace) as a merge remediating once it let go, keeping its
//     lease and its progress record. The holder of a repair slot has its
//     brief delivered by the slot's wait (waitThenReturn, through
//     reattachTurn: a turn already delivered is followed, one never delivered
//     is submitted under its recorded id); a merge awaiting the slot has
//     nothing delivered until it is granted.
//   - A WAITING MERGE'S BACKGROUND REBASE RUNS GIT ONLY (rebasewaiting.go): a
//     conflict it meets aborts the rebase and steps the merge aside for the
//     resolving-conflicts slot (wsm.MergeAsideWaitingConflicts), with no
//     lease and no record, and the slot's holder runs its resolving rebase
//     (startResolving).
//   - RETURNING: once the repair ends, the merge goes back in line at the
//     FIRST WAITING PLACE -- ahead of every merge waiting, behind every merge
//     being worked on, which it never evicts (wsm.ReturnRemediatedMerge) --
//     and its bubble's repairing tab is settled at once (drawBackInLine), so
//     no strip reads "fixing tests" or "resolving conflicts" for a merge
//     back in line. A merge that stepped aside from the head returns
//     admitted and resumes from its record once it heads the queue: it reads
//     the turn's end, rebases onto the target's tip and runs its whole suite
//     again. After a test step aside that suite run is the one fixing
//     attempt; a conflict step aside spends no fixing attempt.
//   - A RESTART KEEPS ALL OF IT: the entry's state, aside and slot are
//     durable, and the boot's take-back of a remediating entry re-arms its
//     slot's wait when it holds the slot (recoverRemediating).

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

// aside is the reason as the store records it on the entry.
func (why AsideReason) aside() wsm.MergeAside {
	switch why {
	case AsideForTests:
		return wsm.MergeAsideTests
	case AsideForConflicts:
		return wsm.MergeAsideConflicts
	}
	panic(fmt.Sprintf("merge: AsideReason(%d) names no reason", int(why)))
}

// origin is the origin of the repairing prompt the reason hands the session.
func (why AsideReason) origin() conversationv1.PromptOrigin {
	switch why {
	case AsideForTests:
		return fixOrigin
	case AsideForConflicts:
		return conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR
	}
	panic(fmt.Sprintf("merge: AsideReason(%d) names no reason", int(why)))
}

// step is the step whose record the repair waits at.
func (why AsideReason) step() string {
	switch why {
	case AsideForTests:
		return TabFixes
	case AsideForConflicts:
		return TabConflicts
	}
	panic(fmt.Sprintf("merge: AsideReason(%d) names no reason", int(why)))
}

// asideReasonFromStore answers the reason a remediating entry that stepped
// aside from the head records in the store, and an error for any other
// aside: a waiting merge's conflict has no record to resume from.
func asideReasonFromStore(aside wsm.MergeAside) (AsideReason, error) {
	switch aside {
	case wsm.MergeAsideTests:
		return AsideForTests, nil
	case wsm.MergeAsideConflicts:
		return AsideForConflicts, nil
	}
	return 0, fmt.Errorf("merge: the aside %s is no step aside from the head", aside)
}

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

// remediation is one merge remediating out of line in this daemon: its queue,
// its record (none for a waiting merge's conflict), and, once its slot's wait
// is armed, that wait's cancel. ARMED is set under o.mu by the one arming
// (armRepairs), so no slot's wait is ever armed twice.
type remediation struct {
	repo wsm.RepoKey
	// doc is the record a merge that stepped aside from the head waits at;
	// waiting marks a waiting merge's conflict, which has none.
	doc     progressDoc
	waiting bool
	armed   bool
	cancel  context.CancelFunc
}

// handOff hands the repairing brief TEXT for WHY to R's session as TURN, and
// answers how that turn ended. RECORD writes the step's own facts into the
// record before anything is handed on, so a restart reattaches to TURN.
//
// A MERGE THAT STEPS ASIDE HANDS NOTHING ON (stepsAside): the brief is
// recorded with its turn, the merge steps aside for its repair slot
// (stepAside), and the slot's wait delivers the brief once the merge holds
// the slot. The run answers the step aside's cut, and is taken back as a
// merge remediating. A step aside that could not be made is recorded at
// ERROR, and the merge repairs where it stands.
func (r *run) handOff(ctx context.Context, why AsideReason, turn ids.TurnID, text string, record func(*progressDoc)) (wsm.TurnClose, error) {
	if r.o.stepsAside(ctx, r, why) {
		if err := r.checkpoint(ctx, why.step(), func(d *progressDoc) {
			record(d)
			d.Brief = text
		}); err != nil {
			return wsm.CloseFailed, err
		}
		if stepped, cause := r.o.stepAside(ctx, r, why); stepped {
			return wsm.CloseFailed, cause
		}
	}
	return r.handOffInPlace(ctx, why, turn, text, record)
}

// handOffInPlace is handOff for a merge that repairs where it stands: the
// record is written, and the brief submitted to its session at once.
func (r *run) handOffInPlace(ctx context.Context, why AsideReason, turn ids.TurnID, text string, record func(*progressDoc)) (wsm.TurnClose, error) {
	if err := r.checkpoint(ctx, why.step(), record); err != nil {
		return wsm.CloseFailed, err
	}
	if err := r.ensureSession(ctx); err != nil {
		return wsm.CloseFailed, fmt.Errorf("merge: starting a session for the %s repair: %w", why, err)
	}
	return r.o.submit(ctx, r.ws, turn, text, why.origin())
}

// stepsAside reports whether R steps aside for WHY: it heads its queue and
// another merge stands in line behind it. A read that fails is recorded at
// ERROR and answers false, so the merge repairs where it stands.
func (o *orchestrator) stepsAside(ctx context.Context, r *run, why AsideReason) bool {
	o.mu.Lock()
	head := o.running[r.repo] == r
	o.mu.Unlock()
	if !head {
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

// stepAside takes R out of its place for WHY's repair, its brief recorded: the
// entry leaves the line for the repair's slot or a place awaiting it
// (wsm.StepAsideMerge), and the run is marked suspending and cut right here,
// on its own goroutine, so it can never go on past the step aside. It answers
// true and the cause the run unwinds with once it stepped aside; false when
// the store write failed (recorded at ERROR), and the merge repairs where it
// stands. The take-back waits for the run to let go, so it alone runs on its
// own goroutine (takeBackSteppedAside).
func (o *orchestrator) stepAside(ctx context.Context, r *run, why AsideReason) (bool, error) {
	settle := context.WithoutCancel(ctx)
	log := o.log(settle, r.ws)
	fields := dlog.Context{"workspace": string(r.ws), "repo": string(r.repo), "reason": why.String()}
	var slot wsm.MergeSlot
	// THE ENTRY LEAVES THE LINE BEFORE THE RUN LEAVES ITS CHAIN (leaveLine),
	// under the line lock: a decision that read the line with the merge still
	// in it never meets a chain the merge already left.
	if err := o.leaveLine(r, func() error {
		var err error
		slot, err = o.deps.DB.StepAsideMerge(settle, r.repo, r.ws, why.aside())
		return err
	}); err != nil {
		log.Error(opRemediate, "could not take the merge out of line to step aside; it repairs where it stands", withField(fields, "error", err.Error()))
		return false, nil
	}
	fields["slot"] = slot.String()
	o.mu.Lock()
	// A RUN ANOTHER END TOOK FIRST -- a dequeue's abandonment, or a pause, a
	// transfer or the daemon's exit suspending it -- is that end's: its entry
	// stands remediating, which that end's drop or take-back reads
	// (recoverRemediating).
	if r.suspending || r.abandonedBy != "" {
		o.mu.Unlock()
		log.Info(opRemediate, "the merge stepped aside as another end took its run; that end settles the merge remediating", fields)
		return true, errMergeStopping
	}
	aside := why.suspension()
	o.takenAway[r.ws] = aside
	r.suspendLocked(aside)
	o.mu.Unlock()
	fields["lease"] = string(r.lease.ID)
	if slot.Awaiting() {
		log.Info(opRemediate, "the merge stepped aside; another merge holds its repair slot, so it waits out of line for it, in order", fields)
	} else {
		log.Info(opRemediate, "the merge stepped aside and holds its repair slot; its brief is delivered now", fields)
	}
	o.cutSuspended(o.deps.Log.Global(), r, aside, fields)
	o.live.Add(1)
	inflight.Go(func() {
		defer o.live.Done()
		o.takeBackSteppedAside(r, aside, fields)
	})
	return true, aside.cause
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

// registerRemediation records WS as remediating out of line in REPO, its
// slot's wait not yet armed (armRepairs arms it once the store says it holds
// the slot). A registration already standing is replaced, its wait withdrawn.
func (o *orchestrator) registerRemediation(ws ids.WorkspaceID, rem *remediation) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.forgetRemediationLocked(ws)
	o.remediating[ws] = rem
}

// armRepairs acts on REPO's repair slots as the store last stamped them: each
// merge remediating here that holds a slot and has no wait armed has its
// slot's work started -- the wait that delivers its brief and returns it to
// the line (waitThenReturn), or, for a waiting merge's conflict, its
// resolving rebase (startResolving). It is called after every write that can
// grant a slot, so a release is acted on as it is made.
//
// A SECOND HOLDER IS A DEFECT, never a second wait: the store refuses one
// (wsm's one-holder index), and an arming that finds a wait already armed
// for another merge of the same slot records it at ERROR and fails hard.
func (o *orchestrator) armRepairs(ctx context.Context, repo wsm.RepoKey) {
	const op = opRemediate
	// THE DAEMON'S EXIT STARTS NO REPAIR: the boot's take-back arms the slot
	// again (recoverRemediating), and the store is not read once it closes.
	if !o.enterAdmission() {
		return
	}
	defer o.leaveAdmission()
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the queue to act on its repair slots; a slot released now is acted on at the queue's next write",
			dlog.Context{"repo": string(repo), "error": err.Error()})
		return
	}
	type start struct {
		ws  ids.WorkspaceID
		rem *remediation
		ctx context.Context
	}
	var starts []start
	o.mu.Lock()
	for _, entry := range entries {
		if entry.Slot != wsm.MergeSlotFixingTests && entry.Slot != wsm.MergeSlotResolvingConflicts {
			continue
		}
		rem, ok := o.remediating[entry.Workspace]
		if !ok || rem.repo != repo || rem.armed {
			continue
		}
		for other, standing := range o.remediating {
			if other == entry.Workspace || standing.repo != repo || !standing.armed {
				continue
			}
			if slot, found := slotOf(entries, other); !found || slot == entry.Slot {
				o.mu.Unlock()
				o.deps.Log.Global().Error(op, "a repair slot would have a second merge at work: the merge still armed for it no longer holds it", dlog.Context{
					"repo": string(repo), "slot": entry.Slot.String(), "holder": string(entry.Workspace), "armed": string(other)})
				panic(fmt.Sprintf("merge: the %s slot of %s would hold %s while %s still works in it", entry.Slot, repo, entry.Workspace, other))
			}
		}
		rctx, cancel := context.WithCancel(context.Background())
		rem.armed, rem.cancel = true, cancel
		starts = append(starts, start{ws: entry.Workspace, rem: rem, ctx: rctx})
	}
	o.mu.Unlock()
	for _, st := range starts {
		o.deps.Log.Global().Info(op, "the merge holds its repair slot; its repair starts", dlog.Context{
			"repo": string(repo), "workspace": string(st.ws), "waiting_conflict": st.rem.waiting})
		if !o.async {
			continue
		}
		o.live.Add(1)
		st := st
		inflight.Standing(func() {
			defer o.live.Done()
			if st.rem.waiting {
				o.resolveWaiting(st.ctx, repo, st.ws)
				return
			}
			o.waitThenReturn(st.ctx, st.ws, repo, st.rem.doc)
		})
	}
}

// slotOf answers the slot WS's entry holds among ENTRIES.
func slotOf(entries []wsm.MergeQueueEntry, ws ids.WorkspaceID) (wsm.MergeSlot, bool) {
	for _, entry := range entries {
		if entry.Workspace == ws {
			return entry.Slot, true
		}
	}
	return wsm.MergeSlotNone, false
}

// waitThenReturn delivers the brief DOC records to WS's session when it never
// reached it, follows its repairing turn to its end (reattachTurn), and
// returns the merge to REPO's line. A wait it cannot keep -- the turn cannot be
// read, delivered or followed -- is recorded at ERROR and the merge returns to
// the line at once, giving its slot to the next merge awaiting it: its run
// reads the turn's end, or delivers its brief again, when it heads the queue
// (resumeFix, resumeConflict).
func (o *orchestrator) waitThenReturn(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, doc progressDoc) {
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "turn": doc.Turn, "step": doc.Step}
	why, err := asideReasonOf(doc)
	if err != nil {
		log.Error(opRemediate, "a merge holding a repair slot has a record that waits on no repairing turn; it returns to the line", withField(fields, "error", err.Error()))
	} else {
		log.Info(opRemediate, "the merge remediates out of line; it returns to the line once its repairing turn ends", fields)
		closed, err := o.reattachTurn(ctx, ws, fields, ids.TurnID(doc.Turn), doc.Brief, why.origin())
		switch {
		case err != nil && ctx.Err() != nil:
			log.Info(opRemediate, "the wait on the repairing turn was withdrawn", fields)
			return
		case err != nil:
			log.Error(opRemediate, "could not deliver or follow the repairing turn; the merge returns to the line and takes its turn up again when it heads the queue",
				withField(fields, "error", err.Error()))
		default:
			fields["close"] = closed.String()
		}
	}
	if err := o.returnRemediated(context.WithoutCancel(ctx), ws, repo); err != nil {
		log.Error(opRemediate, "could not return the remediated merge to the line; it stays out of line, holding its slot, until the next boot", withField(fields, "error", err.Error()))
	}
}

// returnRemediated puts the remediating merge WS back in REPO's line, next
// after the merge being worked on, releasing its repair slot to the next
// merge awaiting it, whose repair starts at once (armRepairs). A merge that
// stepped aside from the head has its bubble drawn back in line
// (drawBackInLine).
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
	rem := o.remediating[ws]
	delete(o.remediating, ws)
	o.mu.Unlock()
	o.log(ctx, ws).Info(opRemediate, "the merge's repair ended; it is back in line, next after the merge being worked on, and its repair slot passes on",
		dlog.Context{"workspace": string(ws), "repo": string(repo)})
	if rem != nil && !rem.waiting {
		o.drawBackInLine(ctx, ws, repo, rem.doc)
	}
	o.armRepairs(ctx, repo)
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// drawBackInLine settles the repairing tab of WS's bubble -- "fixing tests" or
// "resolving conflicts" -- now that its repair ended and the merge is back in
// line, and records the merge as back in line on its record (Brief cleared,
// BackInLine set), so a restart or a handover draws it waiting too
// (recoverResumable) and nothing draws the repair live again until the merge
// heads the queue and its run takes the repair's end up (2026-11-11: a merge
// back in line kept a live "fixing tests" tab ticking, because nothing but its
// absent run drew its bubble). A record that cannot be written is recorded at
// ERROR; the tab is settled either way.
func (o *orchestrator) drawBackInLine(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, doc progressDoc) {
	log := o.log(ctx, ws)
	o.mu.Lock()
	ledger, known := o.ledgerOf[ws]
	o.mu.Unlock()
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "step": doc.Step}
	why, err := asideReasonOf(doc)
	if err != nil || !known {
		if err == nil {
			err = errors.New("the merge has no bubble identity in this daemon")
		}
		log.Error(opRemediate, "could not draw the merge back in line: its record or its bubble cannot be read", withField(fields, "error", err.Error()))
		return
	}
	v := o.buildRun(context.Background(), func(error) {}, repo, ws, doc.QueuedMS)
	v.lease = wsm.Lease{ID: ledger, Workspace: ws}
	v.startedMS = doc.StartedMS
	v.restoreFrom(doc)
	now := o.nowMS()
	round := v.activeRound()
	switch why {
	case AsideForTests:
		v.fixing.Span.settle(now, "")
		v.publishTesting(false)
		v.publishFixingTests(false)
		v.publishFixing(v.testsRound(), round.settled(now, ""), doc.FixAttempt)
	case AsideForConflicts:
		if len(v.rebasing.Commits) > 0 {
			i := min(doc.Replayed, len(v.rebasing.Commits)-1)
			if c := v.rebasing.Commits[i].Conflict; c != nil {
				c.Span.settle(now, "")
			}
			v.conflicts.Span.settle(now, "")
			v.publishConflicts(false)
			v.publishResolution(i, false)
		}
	}
	fields["tab"], fields["round"] = round.kind, round.n
	if err := v.checkpoint(ctx, doc.Step, func(d *progressDoc) {
		d.BackInLine = true
		d.Facts.Step = string(footer.StepEnqueued)
	}); err != nil {
		log.Error(opRemediate, "could not record the merge as back in line; a restart before it heads the queue draws its repair live again", withField(fields, "error", err.Error()))
	}
	o.mu.Lock()
	if standing, resuming := o.resumes[ws]; resuming {
		standing.BackInLine = true
		standing.Brief = ""
		standing.Facts.Step = string(footer.StepEnqueued)
		standing.Fixing, standing.Conflicts, standing.Rebasing = v.fixing.clone(), v.conflicts.clone(), v.rebasing.clone()
		o.resumes[ws] = standing
	}
	o.mu.Unlock()
	log.Info(opRemediate, "the merge is back in line: its repairing tab is settled and its strip reads waiting until it heads the queue", fields)
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

package merge

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/cpugov"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// This file is a repository's CHAIN: the merges at the front of its queue,
// rebased one onto the next and tested ahead of their turn (owner rulings,
// 2026-10-11).
//
// THE WINDOW. The chain is the head and the merges right behind it, at most
// cpugov.QueueWindow in all; an entry beyond the window waits untouched until
// a merge ahead of it lands or drops out and it moves in.
//
// EVERY MEMBER STANDS ON THE TARGET (race.go). The head and the merges behind
// it each rebase onto the target's tip, their conflicts resolved by their OWN
// sessions through the same prompts, and none waits on the progress of the
// merge ahead of it: an entry joins the chain the moment the window has room
// for it. A target that moves rebases every member onto its new tip. Every
// queue rebase reuses recorded resolutions (gitclient's rerere), so a rebase
// whose conflicts were resolved before prompts nothing.
//
// A MERGE ONLY EVER CARRIES ITS OWN COMMITS. A rebase replays exactly the
// commits between the branch's chain base -- the tip its last rebase replayed
// it onto -- and its head, so the commits of a merge ahead of it never travel
// with it unless that merge landed: when it lands at the head, it is rebased
// onto the target's tip as it stands after the merge ahead landed, and lands
// as exactly that tip plus its own commits.
//
// SPECULATIVE GATES. A merge whose rebase settled runs its test gate (and its
// fixing attempts) ahead of its turn against its base, when the processor
// governor admits it (cpugov), niced below the head. The pass
// is valid only for the tree it tested: at the head, a branch whose rebased
// tree is that tree lands without testing again; any other tree is tested
// again, and the invalidation is recorded.
//
// ORDER IS STRUCTURAL. The chain is read and changed only under the
// orchestrator's mutex, and every change closes the repository's chain signal
// under that same lock, so a run that read its place and then waits on the
// signal it read never misses a change. Only the pump promotes, and it
// promotes only the chain's first run once that run's entry stands first in
// line.

// errBaseMoved is the cause a speculative gate is cut with when the merge
// ahead of it published a new tip (or left the chain): its result could not
// stand for the new base.
var errBaseMoved = errors.New("merge: the merge ahead moved; this merge's speculative gate runs again on its new tip")

// errPromoted is the cause a speculative gate's wait for admission is cut
// with when its merge reached the head: the head's gate runs ungoverned.
var errPromoted = errors.New("merge: the merge reached its queue's head; its gate runs as the head's")

// isSpeculative reports whether the run stands behind its chain's head.
func (r *run) isSpeculative() bool {
	r.o.mu.Lock()
	defer r.o.mu.Unlock()
	return r.speculative
}

// signalChainLocked tells every waiter on the repository's chain that it
// changed. Called with o.mu held.
func (o *orchestrator) signalChainLocked(repo wsm.RepoKey) {
	if ch, ok := o.chainSignals[repo]; ok {
		close(ch)
	}
	o.chainSignals[repo] = make(chan struct{})
}

// chainSignalLocked answers the repository's current chain signal. Called
// with o.mu held.
func (o *orchestrator) chainSignalLocked(repo wsm.RepoKey) <-chan struct{} {
	ch, ok := o.chainSignals[repo]
	if !ok {
		ch = make(chan struct{})
		o.chainSignals[repo] = ch
	}
	return ch
}

// chainPlace is where a run stands in its chain, read whole under one lock.
type chainPlace struct {
	// head reports the run holds its repository's slot.
	head bool
	// rank is its distance from the chain's front; -1 when it is no member.
	rank int
	// base is the tip of the run ahead of it, empty while that one has not
	// settled (or when the run is the chain's front, waiting to be promoted).
	base string
	// ahead names the workspace of the run ahead of it.
	ahead ids.WorkspaceID
	// changed closes at the chain's next change.
	changed <-chan struct{}
}

// chainPosition reads where r stands in its chain.
func (o *orchestrator) chainPosition(r *run) chainPlace {
	o.mu.Lock()
	defer o.mu.Unlock()
	place := chainPlace{rank: -1, changed: o.chainSignalLocked(r.repo)}
	if !r.speculative {
		place.head, place.rank = true, 0
		return place
	}
	for i, member := range o.chains[r.repo] {
		if member != r {
			continue
		}
		place.rank = i
		if i > 0 {
			// THE MERGE BEHIND THE HEAD STANDS ON THE TARGET, not on the head
			// (race.go): its base is the target's tip as last read.
			place.base, place.ahead = o.targetTipLocked(r.repo), o.chains[r.repo][i-1].ws
		}
		return place
	}
	return place
}

// extendChainAsync offers the repository's chain its next member on its own
// goroutine: the pump's kick, in production.
func (o *orchestrator) extendChainAsync(repo wsm.RepoKey) {
	if !o.async {
		return
	}
	o.live.Add(1)
	go func() {
		defer o.live.Done()
		o.extendChain(context.Background(), repo)
	}()
}

// extendChain starts the next merge of the repository's chain, as ONE
// admission step the shutdown drain waits for: the entry right behind the
// chain's last run, when the window has room and the entry is waiting in line
// -- or an entry the boot recovery handed back to resume there. It repeats
// while it starts resumed runs.
func (o *orchestrator) extendChain(ctx context.Context, repo wsm.RepoKey) {
	for o.extendOnce(ctx, repo) {
	}
}

// extendOnce makes at most one chain start, and reports whether it started a
// resumed run (after which the chain may grow again at once). It never waits
// on the progress of the run ahead (race.go): every member stands on the
// target, not on the merge ahead of it.
func (o *orchestrator) extendOnce(ctx context.Context, repo wsm.RepoKey) bool {
	const op = "daemon.merge.chain"
	o.mu.Lock()
	room := len(o.chains[repo]) > 0 && len(o.chains[repo]) < o.queueWindow()
	o.mu.Unlock()
	if !room || !o.enterAdmission() {
		return false
	}
	defer o.leaveAdmission()
	stored, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the queue to extend its chain", dlog.Context{"repo": string(repo), "error": err.Error()})
		return false
	}
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read whether the queue is paused to extend its chain", dlog.Context{"repo": string(repo), "error": err.Error()})
		return false
	}
	entries := inLine(stored)
	o.mu.Lock()
	chain := o.chains[repo]
	if len(chain) == 0 || len(chain) >= o.queueWindow() || o.draining {
		o.mu.Unlock()
		return false
	}
	// THE CHAIN EXTENDS ONLY BEHIND ITS RUNNING HEAD. A head a pause, a
	// restart or a transfer suspended leaves the merges behind it in the chain
	// until it registers again at the chain's front (register), and the pump
	// promotes the merge behind a head that ended: until then the chain stands
	// behind an entry it holds no run for, and nothing joins it.
	if head := o.running[repo]; head == nil || chain[0] != head {
		o.mu.Unlock()
		return false
	}
	// THE CHAIN IS THE QUEUE'S FRONT, IN ORDER. Every member stands where its
	// entry stands; anything else is a defect, said once here and never
	// extended past.
	for i, member := range chain {
		if i >= len(entries) || entries[i].Workspace != member.ws {
			o.mu.Unlock()
			o.deps.Log.Global().Error(op, "the chain does not stand at the front of its queue in queue order; it is not extended", dlog.Context{
				"repo": string(repo), "member": string(member.ws), "place": i, "entries": len(entries)})
			return false
		}
	}
	if len(chain) >= len(entries) {
		o.mu.Unlock()
		return false
	}
	tail, next := chain[len(chain)-1], entries[len(chain)]
	doc, resuming := o.resumes[next.Workspace]
	switch {
	case resuming:
	case o.runsByWorkspace[next.Workspace] != nil, o.transferred[next.Workspace], o.inChainLocked(repo, next.Workspace):
		o.mu.Unlock()
		return false
	case paused || next.State != wsm.MergeQueued || next.Source.Kind == wsm.MergeSourceMergedUpstream:
		o.mu.Unlock()
		return false
	}
	runCtx, cancel := context.WithCancelCause(context.Background())
	r := o.newRun(runCtx, cancel, repo, next.Workspace)
	// THE RUN JOINS ITS CHAIN NOW AND ITS WORKSPACE ONLY AT ITS REGISTRATION
	// (register), once what it lands has been read: nothing addresses a run
	// whose facts are still being filled in.
	r.speculative, r.joined = true, true
	o.chains[repo] = append(chain, r)
	if resuming {
		delete(o.resumes, r.ws)
	}
	if o.draining {
		r.suspending = true
		r.git.stop()
	}
	o.signalChainLocked(repo)
	o.mu.Unlock()
	o.deps.Log.Global().Info(op, "a merge joined its queue's chain behind the merge ahead of it", dlog.Context{
		"repo": string(repo), "workspace": string(r.ws), "behind": string(tail.ws), "rank": len(chain), "resumed": resuming})
	o.live.Add(1)
	go func() {
		defer o.live.Done()
		defer close(r.finished)
		if resuming {
			// A RESUME'S FAILURE ENDED ITS RUN THROUGH THE ONE
			// CLASSIFICATION, which recorded it.
			_ = o.resumeInto(r.ctx, r, doc)
			return
		}
		o.startSpeculative(r.ctx, r)
	}()
	return resuming
}

// startSpeculative admits one merge behind its chain's head: its lease, its
// admission and its ledger, then the same run a head's admission makes. Its
// steps run ahead of its turn until the pump promotes it.
func (o *orchestrator) startSpeculative(ctx context.Context, r *run) {
	const op = "daemon.merge.start"
	ws := r.ws
	log := o.log(ctx, ws)
	if !o.enterAdmission() {
		o.unstart(ctx, r, nil)
		return
	}
	entry, err := o.entryOf(ctx, r.repo, ws)
	if err == nil {
		_, err = enqueuedMS(entry)
	}
	if err != nil {
		o.leaveAdmission()
		log.Error(op, "could not read what the merge behind the head lands", dlog.Context{"workspace": string(ws), "error": err.Error()})
		o.unstart(ctx, r, err)
		return
	}
	job, _, err := o.deps.DB.CreationJob(ctx, ws)
	if err != nil {
		o.leaveAdmission()
		log.Error(op, "could not read the requester's creation job", dlog.Context{"workspace": string(ws), "error": err.Error()})
		o.unstart(ctx, r, err)
		return
	}
	// THE LEASE GOES FIRST, THE ADMISSION AFTER: an admitted entry always has
	// a run holding a lease, so nothing ever reads one as a merge another
	// daemon drives while this one lets it go.
	lease, err := o.deps.DB.AcquireLeaseAs(ctx, ws, o.mintLedger(ws), wsm.HolderMerge, wsm.PolicyHold)
	if err != nil {
		o.leaveAdmission()
		log.Error(op, "could not take the merge lease of the merge behind the head", dlog.Context{"workspace": string(ws), "error": err.Error()})
		o.unstart(ctx, r, err)
		return
	}
	if err := o.deps.DB.AdmitMerge(ctx, r.repo, ws); err != nil {
		o.leaveAdmission()
		if releaseErr := o.deps.DB.ReleaseLease(ctx, lease.ID); releaseErr != nil {
			log.Error(op, "could not release the lease of a merge whose admission failed", dlog.Context{"workspace": string(ws), "lease": string(lease.ID), "error": releaseErr.Error()})
		}
		log.Error(op, "could not admit the merge behind the head", dlog.Context{"workspace": string(ws), "error": err.Error()})
		o.unstart(ctx, r, err)
		return
	}
	o.leaveAdmission()
	o.deps.Queue.OnLeaseChanged(ws)
	o.mu.Lock()
	r.displaces = o.displaces[ws]
	delete(o.displaces, ws)
	o.mu.Unlock()
	r.mu.Lock()
	r.source, r.job, r.lease = entry.Source, job, lease
	r.mu.Unlock()
	r.startedMS, r.queuedMS = o.deps.Now().UnixMilli(), entry.EnqueuedAt.UnixMilli()
	log.Info(op, "admitted a merge behind its queue's head; it rebases onto the merge ahead and tests ahead of its turn", dlog.Context{
		"workspace": string(ws), "repo": string(r.repo), "lease": string(lease.ID)})
	// THE PUMP MAY BE WAITING ON THIS VERY ADMISSION to promote the merge,
	// when the head ended while it was starting.
	o.kick(r.repo)
	_ = r.admit(ctx)
}

// unstart lets go of a speculative run that never took its lease: it leaves
// its chain, and its entry stands as it was -- still waiting in line, or
// already dropped by an abandon that found no run to address.
func (o *orchestrator) unstart(ctx context.Context, r *run, err error) {
	settle := context.WithoutCancel(ctx)
	o.releaseSlot(settle, r)
	fields := dlog.Context{"workspace": string(r.ws), "repo": string(r.repo)}
	if err != nil {
		fields["error"] = err.Error()
	}
	o.deps.Log.Global().Info("daemon.merge.chain", "a merge that was starting behind the head let go before its lease; its entry waits in line as it was", fields)
}

// inChainLocked reports whether ws has a run in the repository's chain.
// Called with o.mu held.
func (o *orchestrator) inChainLocked(repo wsm.RepoKey, ws ids.WorkspaceID) bool {
	for _, member := range o.chains[repo] {
		if member.ws == ws {
			return true
		}
	}
	return false
}

// inChain reports whether ws has a run in the repository's chain.
func (o *orchestrator) inChain(repo wsm.RepoKey, ws ids.WorkspaceID) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	return o.inChainLocked(repo, ws)
}

// promote moves the chain's first run into the repository's slot once its
// entry stands first in line: the merge ahead of it landed, or left the
// queue. Only the pump calls it.
func (o *orchestrator) promote(repo wsm.RepoKey, entries []wsm.MergeQueueEntry) (*run, bool) {
	line := inLine(entries)
	if len(line) == 0 || line[0].State != wsm.MergeAdmitted {
		return nil, false
	}
	o.mu.Lock()
	chain := o.chains[repo]
	if len(chain) == 0 || chain[0].ws != line[0].Workspace || !chain[0].speculative || o.running[repo] != nil {
		o.mu.Unlock()
		return nil, false
	}
	r := chain[0]
	r.speculative = false
	o.running[repo] = r
	o.signalChainLocked(repo)
	o.mu.Unlock()
	lease, base := r.leaseAndBase()
	o.log(context.Background(), r.ws).Info("daemon.merge.chain", "the merge reached its queue's head; it lands from here", dlog.Context{
		"workspace": string(r.ws), "repo": string(repo), "lease": string(lease), "chain_base": base})
	return r, true
}

// leaseAndBase reads the run's lease and chain base under the run's lock: the
// pump reads them from its own goroutine while the run's goroutine (its
// resume, its admission, its rebase) writes them.
func (r *run) leaseAndBase() (ids.LeaseID, string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.lease.ID, r.chainBase
}

// briefTarget is what a step's brief names as the branch the merge lands on:
// the target itself at the head, and for a step behind the head the target as
// it will stand once the merge ahead lands.
func (r *run) briefTarget(targetBranch string) string {
	// EVERY MERGE STANDS ON THE TARGET ITSELF (race.go), behind the head or
	// not, so its briefs name the target as it is.
	return targetBranch
}

// speculate runs the merge's steps ahead of its turn, for as long as it
// stands behind its chain's head: its chained rebase, its restacks, its
// speculative gate and fixing attempts. It answers nil once the pump promoted
// it, or the failed outcome when its conflicts or its tests gave up.
func (r *run) speculate(ctx context.Context) (*outcome, error) {
	const op = "daemon.merge.speculative"
	target := r.subject.targetDir
	targetBranch, err := r.git.CurrentBranch(ctx, target)
	if err != nil {
		return nil, fmt.Errorf("merge: reading the branch checked out in %s: %w", target, err)
	}
	if targetBranch == "" {
		return nil, fmt.Errorf("merge: the merge target %s has no branch checked out to land on", target)
	}
	if res, ok := r.resumingAt(TabRebasing, TabConflicts, TabTests, TabFixes); ok && res.Speculative {
		r.doneResuming()
		if gaveUp, err := r.resumeSpeculative(ctx, res, targetBranch); err != nil || gaveUp != nil {
			return gaveUp, err
		}
	}
	r.doneResumingAt(TabPrePrompt)
	for {
		if err := stillWanted(ctx); err != nil {
			return nil, err
		}
		place := r.o.chainPosition(r)
		switch {
		case place.head:
			return nil, nil
		case place.rank < 0:
			err := errors.New("merge: a speculative run is no member of its chain")
			r.o.log(ctx, r.ws).Error(op, "a merge behind the head stands in no chain; it cannot go on", dlog.Context{
				"workspace": string(r.ws), "repo": string(r.repo)})
			return nil, err
		case place.base == "":
			// No run has read the target's tip yet, or this merge stands
			// first and waits for the pump to promote it.
		case place.base != r.chainBase:
			if gaveUp, err := r.chainRebase(ctx, place, targetBranch); err != nil || gaveUp != nil {
				return gaveUp, err
			}
			continue
		case r.tested == nil || r.tested.Base != place.base:
			if gaveUp, err := r.speculativeGate(ctx, place, targetBranch); err != nil || gaveUp != nil {
				return gaveUp, err
			}
			continue
		case place.rank == 1 && r.claimedOn != place.base:
			// ITS GATE PASSED ON THE TARGET'S TIP while the head still runs:
			// it claims the race to land, once per base (race.go).
			r.claimedOn = place.base
			r.o.claimRace(ctx, r, place.base)
			continue
		}
		if err := r.idle(ctx, place.changed); err != nil {
			return nil, err
		}
	}
}

// idle waits, at a step boundary, for the chain to change. A restart asking
// the run to stop at its next boundary finds it here and suspends it at once
// (suspendFor reads idling).
func (r *run) idle(ctx context.Context, changed <-chan struct{}) error {
	r.o.mu.Lock()
	r.idling = true
	seam := r.o.onChainIdle
	r.o.mu.Unlock()
	if seam != nil {
		seam(r.ws)
	}
	defer func() {
		r.o.mu.Lock()
		r.idling = false
		r.o.mu.Unlock()
	}()
	select {
	case <-changed:
		return nil
	case <-ctx.Done():
		return context.Cause(ctx)
	}
}

// chainRebase rebases the branch's own commits onto the target's tip behind
// the head: its first rebase, or a restack when the target moved.
func (r *run) chainRebase(ctx context.Context, place chainPlace, targetBranch string) (*outcome, error) {
	r.o.log(ctx, r.ws).Info("daemon.merge.chain", "the merge behind the head rebases its own commits onto the target's tip", dlog.Context{
		"workspace": string(r.ws), "ahead": string(place.ahead), "onto": place.base, "chain_base": r.chainBase, "rank": place.rank,
		"restack": r.chainBase != ""})
	r.briefAhead = place.ahead
	gaveUp, err := r.rebase(ctx, place.base, targetBranch)
	if err != nil || gaveUp != nil {
		return gaveUp, err
	}
	r.settledOn(place.base)
	return nil, nil
}

// settledOn records the branch's own commits as standing on onto, once a
// rebase onto it settled.
func (r *run) settledOn(onto string) {
	r.mu.Lock()
	r.chainBase = onto
	r.mu.Unlock()
}

// ownBase is the commit a rebase onto tip replays the branch's own commits
// from: the chain base its last rebase left it on, or -- before its first --
// tip itself, every commit of the branch the tip does not hold.
func (r *run) ownBase(tip string) string {
	if r.chainBase != "" {
		return r.chainBase
	}
	return tip
}

// speculativeGate runs the gate ahead of the merge's turn against its chained
// base, once the governor admits it, and the fixing attempt a failure takes.
// A base that moves under it cuts it: it runs again on the new base.
func (r *run) speculativeGate(ctx context.Context, place chainPlace, targetBranch string) (*outcome, error) {
	const op = "daemon.merge.speculative"
	gctx, cancel := context.WithCancelCause(ctx)
	defer cancel(nil)
	// THE WAIT FOR ADMISSION ENDS AT A PROMOTION: the head's gate is never
	// governed, so a merge promoted before the governor admitted it runs its
	// gate as the head, from the head's own steps (admitGate answers
	// errPromoted). A gate already running is not cut by a promotion: its
	// pass stands for the tree it tested.
	actx, admitted := context.WithCancelCause(gctx)
	defer admitted(nil)
	stop := r.watchBase(gctx, place.base, cancel, admitted)
	defer stop()
	head, err := r.git.ResolveRef(ctx, r.subject.dir, r.subject.branch)
	if err != nil {
		return nil, fmt.Errorf("merge: resolving %s: %w", r.subject.branch, err)
	}
	round := r.openTab(ctx, TabTests)
	if err := r.checkpoint(ctx, TabTests, func(d *progressDoc) {
		d.TargetBranch, d.Tip = targetBranch, place.base
		d.FixAttempt, d.Turn, d.FailingSuites, d.FailingArchive, d.FailingTail = r.fixesUsed, "", nil, "", ""
	}); err != nil {
		return nil, err
	}
	verdict, err := r.gate(gctx, round, place.base, head, &gateAdmission{ctx: actx, rank: place.rank})
	if errors.Is(err, errPromoted) {
		return nil, nil
	}
	if errors.Is(context.Cause(gctx), errBaseMoved) {
		r.o.log(ctx, r.ws).Info(op, "the merge ahead moved under the speculative gate; it runs again on the new tip", dlog.Context{
			"workspace": string(r.ws), "base": place.base, "round": round.n})
		return nil, nil
	}
	if err == nil {
		err = stillWanted(ctx)
	}
	if err != nil {
		return nil, err
	}
	switch {
	case verdict.broken != "":
		out := failedIn(footer.FailedOther, verdict.broken)
		return &out, nil
	case verdict.result.Passed:
		tree, err := r.git.TreeOf(ctx, r.subject.dir, head)
		if err != nil {
			return nil, fmt.Errorf("merge: reading the tree the speculative gate passed: %w", err)
		}
		r.tested = &testedDoc{Base: place.base, Head: head, Tree: tree}
		// A PASSING GATE ENDS THE RUN OF FAILURES, ahead of its turn too.
		r.fixesUsed = 0
		r.o.log(ctx, r.ws).Info(op, "the merge passed its tests ahead of its turn; the pass stands for exactly this tree", dlog.Context{
			"workspace": string(r.ws), "base": place.base, "head": head, "tree": tree, "rank": place.rank})
		return nil, r.checkpoint(ctx, TabTests, nil)
	case r.fixesUsed >= MaxFixAttempts:
		out := r.fixesExhausted(ctx, verdict.result.ArchivePath, dlog.Context{"speculative": true})
		return &out, nil
	}
	attempt := r.fixesUsed + 1
	r.briefAhead = place.ahead
	if gaveUp, err := r.fix(ctx, attempt, verdict.result, targetBranch); err != nil || gaveUp != nil {
		return gaveUp, err
	}
	r.fixesUsed = attempt
	return nil, nil
}

// watchBase cuts ctx with errBaseMoved the moment the run, still behind its
// head, finds the tip ahead of it is no longer base, and cuts its wait for
// admission with errPromoted the moment it heads its chain. It answers its
// stop, which waits for the watch to end.
func (r *run) watchBase(ctx context.Context, base string, cancel, promoted context.CancelCauseFunc) func() {
	return r.o.watch(ctx, cancel, func() (bool, <-chan struct{}) {
		place := r.o.chainPosition(r)
		if place.head {
			promoted(errPromoted)
			// A GATE ON A TIP THE TARGET LEFT IS CUT AT THE HEAD TOO
			// (race.go): every merge stands on the target, so a pass on the
			// old tip could only be run again.
			if tip := r.o.targetTip(r.repo); tip != "" && tip != base {
				cancel(errBaseMoved)
				return true, nil
			}
		} else if place.base != base {
			cancel(errBaseMoved)
			return true, nil
		}
		return false, place.changed
	})
}

// watch runs LOOK on its own goroutine each time the signal it last answered
// closes, until it reports done or ctx is done: a watch over orchestrator
// state read under its lock, which cuts work through CANCEL. It answers its
// stop, which cancels ctx through CANCEL and waits for the watch to end.
func (o *orchestrator) watch(ctx context.Context, cancel context.CancelCauseFunc, look func() (done bool, changed <-chan struct{})) func() {
	ended := make(chan struct{})
	o.live.Add(1)
	go func() {
		defer o.live.Done()
		defer close(ended)
		for {
			done, changed := look()
			if done {
				return
			}
			select {
			case <-changed:
			case <-ctx.Done():
				return
			}
		}
	}()
	return func() {
		cancel(nil)
		<-ended
	}
}

// resumeSpeculative goes on with a speculative step a restart interrupted,
// from the step its record names; the speculate loop then takes the merge on
// from wherever its chain now stands.
func (r *run) resumeSpeculative(ctx context.Context, res *progressDoc, targetBranch string) (*outcome, error) {
	const op = "daemon.merge.resume"
	if res.Tip == "" {
		return r.contradiction(ctx, fmt.Sprintf("the record of its speculative %s step names no base", res.Step)), nil
	}
	p := &replay{round: r.activeRound(), commits: fromCommitDocs(res.Commits), done: res.Replayed}
	switch res.Step {
	case TabRebasing:
		if gaveUp, err := r.resumeRebase(ctx, p, res.Tip, targetBranch, res.BranchHead); err != nil || gaveUp != nil {
			return gaveUp, err
		}
		r.settledOn(res.Tip)
		return nil, nil
	case TabConflicts:
		if gaveUp, err := r.resumeConflict(ctx, p, res.Tip, targetBranch, res); err != nil || gaveUp != nil {
			return gaveUp, err
		}
		r.settledOn(res.Tip)
		return nil, nil
	case TabTests:
		// THE GATE THE RESTART CUT runs again, in a round of its own, once the
		// governor admits it: its round is settled as cut.
		round := r.activeRound()
		r.o.log(ctx, r.ws).Info(op, "the speculative gate the restart cut runs again once admitted", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "round": round.n})
		if res.Tested == nil {
			const cut = "the daemon restarted under this run; the tests run again"
			r.endRun(ctx, round, cut, nil, nil, "restarted")
		}
		return nil, nil
	default:
		if gaveUp, err := r.resumeFix(ctx, res, targetBranch); err != nil || gaveUp != nil {
			return gaveUp, err
		}
		r.fixesUsed = res.FixAttempt
		return nil, nil
	}
}

// reuseTested decides, at the head, whether the speculative pass stands for
// the branch as it is about to land: it does when the rebased head's tree is
// the very tree the speculative gate passed. Either way the decision is
// recorded, and the pass is spent.
func (r *run) reuseTested(ctx context.Context, tip, head string) (bool, error) {
	const op = "daemon.merge.speculative"
	tested := r.tested
	if tested == nil {
		return false, nil
	}
	r.tested = nil
	tree, err := r.git.TreeOf(ctx, r.subject.dir, head)
	if err != nil {
		return false, fmt.Errorf("merge: reading the tree of %s: %w", short(head), err)
	}
	fields := dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "tip": tip, "head": head, "tree": tree,
		"tested_base": tested.Base, "tested_head": tested.Head, "tested_tree": tested.Tree}
	if tree == tested.Tree {
		if tested.Own {
			r.o.log(ctx, r.ws).Info("daemon.merge.tests", "the branch rebased to the very tree its gate passed; the pass stands and the tests are not run again", fields)
			return true, nil
		}
		r.o.log(ctx, r.ws).Info(op, "the merge ahead landed exactly as tested; the speculative pass stands and the tests are not run again", fields)
		return true, nil
	}
	if tested.Own {
		r.o.log(ctx, r.ws).Info("daemon.merge.tests", "the branch rebased to a tree its gate never passed; the tests run again", fields)
		return false, nil
	}
	r.o.log(ctx, r.ws).Info(op, "the branch about to land is not the tree the speculative gate passed; the pass is invalidated and the tests run again", fields)
	return false, nil
}

// speculativeArgv is the gate's command line for a merge behind the head: the
// whole tree niced below the head's (owner ruling, 2026-10-11), so a gate run
// ahead of its turn always yields the processor to the head's gate and to the
// live runtime. Niceness is applied at the spawn, by exec'ing through nice, so
// every descendant inherits it; no core cap and no I/O throttle is added.
func speculativeArgv(argv []string) []string {
	return append([]string{"/usr/bin/nice", "-n", fmt.Sprint(SpeculativeNice)}, argv...)
}

// SpeculativeNice is the niceness a speculative gate's tree runs at, above the
// head's.
const SpeculativeNice = 12

// gateArgv is every gate's command line. Every gate queues at
// bin/suite-slot.sh's merge priority, ahead of every ad-hoc run, and a
// speculative gate (one behind the head) runs niced so the head's own run is
// never slowed by it.
func gateArgv(argv []string, speculative bool) []string {
	// Every merge-queue gate, the head's and a speculative one alike, queues
	// at bin/suite-slot.sh's merge priority; the speculative one is niced.
	argv = append([]string{"env", "AGENT_REPL_SUITE_PRIORITY=merge"}, argv...)
	if speculative {
		argv = speculativeArgv(argv)
	}
	return argv
}

// queueWindow is the window in force: the test override when one is set, else
// cpugov.QueueWindow.
func (o *orchestrator) queueWindow() int {
	if o.window > 0 {
		return o.window
	}
	return cpugov.QueueWindow
}

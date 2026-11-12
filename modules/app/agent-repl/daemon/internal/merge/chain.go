package merge

import (
	"context"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// This file is a repository's CHAIN: the deploys waiting in its queue that
// run their CONCURRENT STAGES ahead of their turn behind the head
// (concurrent.go). A queue whose head's deploy config runs no stage
// concurrently has no chain beyond its head: its merges never run anything
// ahead of their turn (the queue races no more, owner ruling, 2026-11-11), so
// the head alone holds the testing slot.
//
// ORDER IS STRUCTURAL. The chain is read and changed only under the
// orchestrator's mutex, and every change closes the repository's chain signal
// under that same lock, so a run that read its place and then waits on the
// signal it read never misses a change. Only the pump promotes, and it
// promotes only the chain's first run once that run's entry stands first in
// line.

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
			place.ahead = o.chains[r.repo][i-1].ws
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
	inflight.Go(func() {
		defer o.live.Done()
		o.extendChain(context.Background(), repo)
	})
}

// extendChain starts the next deploy of the repository's chain, as ONE
// admission step the shutdown drain waits for: the entry right behind the
// chain's last run, when the head runs concurrent stages and the entry is
// waiting in line -- or an entry the boot recovery handed back to resume
// there. It repeats while it starts resumed runs.
func (o *orchestrator) extendChain(ctx context.Context, repo wsm.RepoKey) {
	for o.extendOnce(ctx, repo) {
	}
}

// chainable reports whether ENTRY may start its concurrent stages behind its
// queue's head: it waits in line, and is no merge workspace whose pick has yet
// to succeed -- that pick runs at the head (admitFront).
func chainable(entry wsm.MergeQueueEntry) bool {
	return entry.State == wsm.MergeQueued && entry.Source.Pick == ""
}

// extendOnce makes at most one chain start, and reports whether it started a
// resumed run (after which the chain may grow again at once). It never waits
// on the progress of the run ahead: a member runs its concurrent stages on
// its own, and waits for its turn only at its first one-at-a-time stage.
func (o *orchestrator) extendOnce(ctx context.Context, repo wsm.RepoKey) bool {
	const op = "daemon.merge.chain"
	o.mu.Lock()
	members, current := len(o.chains[repo]), o.running[repo]
	o.mu.Unlock()
	room := members > 0 && current != nil && current.concurrentStages()
	if !room || !o.enterAdmission() {
		return false
	}
	defer o.leaveAdmission()
	line := o.lineLock(repo)
	line.Lock()
	defer line.Unlock()
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
	o.mu.Lock()
	head := o.running[repo]
	o.mu.Unlock()
	// A CHAIN FORMS ONLY BEHIND A HEAD THAT RUNS CONCURRENT STAGES
	// (concurrent.go): every other queue works its head alone.
	if head == nil || !head.concurrentStages() {
		return false
	}
	o.mu.Lock()
	chain := o.chains[repo]
	if len(chain) == 0 || o.draining {
		o.mu.Unlock()
		return false
	}
	// THE CHAIN EXTENDS ONLY BEHIND ITS RUNNING HEAD. A head a pause, a
	// restart or a transfer suspended leaves the merges behind it in the chain
	// until it registers again at the chain's front (register), and the pump
	// promotes the merge behind a head that ended: until then the chain stands
	// behind an entry it holds no run for, and nothing joins it.
	if o.running[repo] != head || chain[0] != head {
		o.mu.Unlock()
		return false
	}
	front, entries := standingFront(chain, inLine(stored))
	// THE CHAIN IS THE QUEUE'S FRONT, IN ORDER. Every member still in line
	// stands where its entry stands; anything else is a defect, said once
	// here and never extended past.
	for i, member := range front {
		if i >= len(entries) || entries[i].Workspace != member.ws {
			seen := chainView(chain)
			o.mu.Unlock()
			o.deps.Log.Global().Error(op, "the chain does not stand at the front of its queue in queue order; it is not extended", dlog.Context{
				"repo": string(repo), "member": string(member.ws), "place": i, "entries": len(entries),
				"chain": seen, "line": lineView(inLine(stored))})
			return false
		}
	}
	// A CHAIN WHOSE EVERY MEMBER HAS LEFT THE LINE HAS NO TAIL to extend
	// behind: the next entry is admitted as a head once they release.
	if len(front) == 0 || len(front) >= len(entries) {
		o.mu.Unlock()
		return false
	}
	tail, next := front[len(front)-1], entries[len(front)]
	doc, resuming := o.resumes[next.Workspace]
	switch {
	case resuming && !doc.Speculative:
		// A MERGE SUSPENDED AS THE HEAD resumes only at the head.
		o.mu.Unlock()
		return false
	case resuming:
	case o.runsByWorkspace[next.Workspace] != nil, o.isTakenAwayLocked(next.Workspace), o.inChainLocked(repo, next.Workspace):
		o.mu.Unlock()
		return false
	case paused || !chainable(next):
		o.mu.Unlock()
		return false
	}
	runCtx, cancel := context.WithCancelCause(context.Background())
	r := o.newRun(runCtx, cancel, repo, next.Workspace, next.EnqueuedAt.UnixMilli())
	// THE RUN JOINS ITS CHAIN NOW AND ITS WORKSPACE ONLY AT ITS REGISTRATION
	// (register), once what it lands has been read: nothing addresses a run
	// whose facts are still being filled in.
	r.speculative, r.joined = true, true
	o.chains[repo] = append(chain, r)
	if resuming {
		delete(o.resumes, r.ws)
	}
	if o.draining {
		r.suspendLocked(suspendExit)
		r.git.stop()
	}
	o.signalChainLocked(repo)
	o.mu.Unlock()
	o.deps.Log.Global().Info(op, "a merge joined its queue's chain behind the merge ahead of it", dlog.Context{
		"repo": string(repo), "workspace": string(r.ws), "behind": string(tail.ws), "rank": len(chain), "resumed": resuming})
	o.live.Add(1)
	inflight.Standing(func() {
		defer o.live.Done()
		defer close(r.finished)
		if resuming {
			// A RESUME'S FAILURE ENDED ITS RUN THROUGH THE ONE
			// CLASSIFICATION, which recorded it.
			_ = o.resumeInto(r.ctx, r, doc)
			return
		}
		o.startSpeculative(r.ctx, r)
	})
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
	r.startedMS = o.deps.Now().UnixMilli()
	log.Info(op, "admitted a merge behind its queue's head; it takes its steps ahead of its turn", dlog.Context{
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

// chainView names each member of a chain, in order, with how it stands: the
// evidence a chain off its queue's front leaves nowhere else. Called with o.mu
// held.
func chainView(chain []*run) string {
	parts := make([]string, 0, len(chain))
	for _, member := range chain {
		part := string(member.ws)
		if member.speculative {
			part += "(speculative)"
		}
		if member.leftLine {
			part += "(left its line)"
		}
		parts = append(parts, part)
	}
	return strings.Join(parts, " ")
}

// lineView names each entry of a queue's line, in order, with its state.
func lineView(line []wsm.MergeQueueEntry) string {
	parts := make([]string, 0, len(line))
	for _, entry := range line {
		parts = append(parts, fmt.Sprintf("%s(%s)", entry.Workspace, entry.State))
	}
	return strings.Join(parts, " ")
}

// standingFront answers the chain's members still in line, and the queue's
// line without the entries of the members that left it (run.leftLine).
//
// A RUN LEAVES ITS LINE BEFORE ITS CHAIN (leaveLine). Its teardown removes
// its queue entry, and its step aside takes it out of line, before it
// releases its slot; the queue read the chain is checked against may fall on
// either side of that write. Leaving the run out of BOTH sides makes every
// such read agree with the chain, because the mark precedes the write: a read
// that still holds the entry is answered without it, and a read that lacks
// it meets a chain that no longer counts it. The write is made under the line
// lock the read is held under, so the run can never leave its chain between a
// read that held its entry and the check. Called with o.mu held.
func standingFront(chain []*run, line []wsm.MergeQueueEntry) ([]*run, []wsm.MergeQueueEntry) {
	left := map[ids.WorkspaceID]bool{}
	front := make([]*run, 0, len(chain))
	for _, member := range chain {
		if member.leftLine {
			left[member.ws] = true
			continue
		}
		front = append(front, member)
	}
	if len(left) == 0 {
		return front, line
	}
	entries := make([]wsm.MergeQueueEntry, 0, len(line))
	for _, entry := range line {
		if !left[entry.Workspace] {
			entries = append(entries, entry)
		}
	}
	return front, entries
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
// the target itself, which only the head's steps ever brief.
func (r *run) briefTarget(targetBranch string) string {
	return targetBranch
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

// reuseTested decides, at the head, whether the gate's earlier pass stands
// for the branch as it is about to land: it does when the rebased head's tree
// is the very tree that gate passed. Either way the decision is
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
		r.o.log(ctx, r.ws).Info("daemon.merge.tests", "the branch rebased to the very tree its gate passed; the pass stands and the tests are not run again", fields)
		return true, nil
	}
	r.o.log(ctx, r.ws).Info("daemon.merge.tests", "the branch rebased to a tree its gate never passed; the tests run again", fields)
	return false, nil
}

// THE GATE HAS THE PROCESSOR AHEAD OF AD-HOC RUNS. Every test run is demoted
// by bin/background.sh to the niceness AGENT_REPL_TEST_NICENESS names, 19 by
// default; the gate names a better one, so under contention the queue's head
// yields only to the owner's live runtime and every ad-hoc run yields to it.
// No processor is reserved and no run waits for another: priority only
// orders who runs first when the processors are contended.

// testNicenessEnv names the niceness bin/background.sh demotes a test run to.
const testNicenessEnv = "AGENT_REPL_TEST_NICENESS"

// HeadNice is the niceness the gate runs at: ahead of every other test run,
// behind the owner's processes.
const HeadNice = 10

// testMetricsRootEnv and testMetricsTriggerEnv tell agent-repl's test runner
// (testrun) where to push every test's time when its run ends, and what ran
// it: the merge queue's gate. A gate whose daemon records no metrics names
// neither, and the runner pushes nothing.
const (
	testMetricsRootEnv    = "AGENT_REPL_TEST_METRICS_ROOT"
	testMetricsTriggerEnv = "AGENT_REPL_TEST_METRICS_TRIGGER"
)

// TriggerMergeQueue is the trigger the gate names (testMetricsTriggerEnv).
const TriggerMergeQueue = "merge_queue"

// gateArgv is the gate's command line: the script, told the niceness its
// whole tree runs at (HeadNice) and, when the daemon records metrics under
// metricsRoot, where its test runner pushes every test's time.
func gateArgv(argv []string, metricsRoot string) []string {
	env := []string{"env", fmt.Sprintf("%s=%d", testNicenessEnv, HeadNice)}
	if metricsRoot != "" {
		env = append(env, testMetricsRootEnv+"="+metricsRoot, testMetricsTriggerEnv+"="+TriggerMergeQueue)
	}
	return append(env, argv...)
}

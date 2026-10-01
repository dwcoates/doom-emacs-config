package merge

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// This file holds a merge's THREE DISTINCT ENDS and the teardown they share.
//
// The ends have distinct causes and must not be collapsed: ABANDONED is a merge
// taken off the queue before it ever ran (evicted by the operator, or released
// by the user's answer to the interrupt offer); FAILED is a run that started and
// gave up; MERGED is a run that landed. Only the last two have a teardown.
//
// THE ORDER OF TEARDOWN IS LOAD-BEARING. The lease is released FIRST, then the
// terminal is published, then the displaced turn is resubmitted, then the
// worktree is removed, and only then does the self-reload fire. Publishing the
// terminal before the release would tell a client the workspace was handed
// back while it still refused every prompt as merging (a user answering "merge
// failed" at once was refused); removing the worktree before the terminal
// would delete the tree a reader is still looking at; removing it before the
// resubmission would delete the tree that resubmission writes into, losing the
// turn the user typed; and firing the self-reload before the release would
// bounce the daemon while it still held a lease it would then have to
// recover.

// finish lands a completed run on its terminal and tears it down.
func (r *run) finish(ctx context.Context, out outcome) error {
	const op = "daemon.merge.finish"
	// THE TERMINAL IS REGISTERED BEFORE THE FIRST DURABLE WRITE. The shutdown
	// drain waits for exactly what is registered here, which is why the
	// registration precedes the stamps rather than following them.
	r.enterTerminal(terminalOwed(out))
	defer r.leaveTerminal()
	log := r.o.log(ctx, r.ws)
	endedMS := r.o.nowMS()
	// THE TEST SEAM, NIL IN PRODUCTION. See Deps.PauseInTerminal.
	if r.o.deps.PauseInTerminal != nil {
		r.o.deps.PauseInTerminal(ctx, r.ws)
	}

	if r.exiting() {
		// The stamps and the publishes below all go through the state client
		// that is already closing; teardown states what was left.
		r.teardown(ctx, out, nil)
		return nil
	}
	r.teardown(ctx, out, func() { r.concludedTerminal(ctx, log, op, out, endedMS) })
	return nil
}

// concludedTerminal publishes a concluded run's terminal: failed with its area,
// merged with nothing to land, or landed.
func (r *run) concludedTerminal(ctx context.Context, log dlog.Logger, op string, out outcome, endedMS int64) {
	switch {
	case out.failed != "":
		r.failedTerminal(ctx, out.failed, out.area)
	case out.alreadyOn != "":
		// NOTHING WAS MERGED, AND NOTHING FAILED: the branch was on the target
		// already. It concludes as merged, and says why there is no commit.
		r.head(&frontendv1.FeedMergeSuccess{EndedAtMs: endedMS, Commit: out.landed})
		r.concluded(MergeFacts{State: StateMerged, Detail: fmt.Sprintf("already on %s; nothing to merge", out.alreadyOn)})
		r.stampClosed(ctx, log, op)
		log.Info(op, "a merge concluded with its branch already on the target", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "target": out.alreadyOn, "tip": out.landed})
	default:
		r.head(&frontendv1.FeedMergeSuccess{EndedAtMs: endedMS, Commit: out.landed})
		r.concluded(MergeFacts{State: StateMerged})
		r.stampClosed(ctx, log, op)
		log.Info(op, "a merge landed", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "commit": out.landed, "commits": len(out.commits),
			"source": r.source.Kind.String(), "closes": string(r.subject.closes)})
	}
}

// failedTerminal draws a merge that gave up: the bubble's failed terminal and
// the requester's "merge failed" with its area. The workspace is handed back:
// the merge leaves the queue at once, and the user's next prompt is an
// ordinary turn.
func (r *run) failedTerminal(ctx context.Context, summary string, area footer.MergeFailedArea) {
	if r.lease.ID != "" {
		r.head(&frontendv1.FeedMergeError{
			EndedAtMs: r.o.nowMS(),
			Reason:    &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: summary}},
		})
	}
	r.concluded(MergeFacts{State: StateFailed, FailedArea: area, Detail: summary})
	// AN OUTCOME, NOT A FAULT: every failed merge is shown to the user as
	// "merge failed" with its area. A fault behind a failure is recorded at
	// its own site (daemon.merge.fault for an abort, the gate's and the
	// prompt's own records), so this record is INFO whatever the area.
	r.o.log(ctx, r.ws).Info("daemon.merge.abort", "a merge failed and the workspace is handed back", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "summary": summary, "area": string(area)})
}

// concluded publishes a concluded merge's facts, keeping the testing round's
// count so a later merge's first test round is a new focus.
func (r *run) concluded(facts MergeFacts) {
	r.mu.Lock()
	facts.TestsRound = r.facts.TestsRound
	r.facts = facts
	r.mu.Unlock()
	r.o.publish(r.ws, facts)
}

// stampClosed writes the durable half of the workspace a landed merge closes:
// merged_at and closed, which put its roster row under `recently_merged`, and
// the republish nothing else would make. A merge that closes nothing -- a
// requester kept open, a branch that is no workspace -- stamps nothing.
func (r *run) stampClosed(ctx context.Context, log dlog.Logger, op string) {
	closes := r.subject.closes
	if closes == "" {
		return
	}
	if err := r.o.deps.DB.SetMergedAt(ctx, closes, r.o.deps.Now()); err != nil {
		log.Error(op, "could not stamp the merge's landing", dlog.Context{"workspace": string(closes), "error": err.Error()})
	}
	if err := r.o.deps.DB.SetClosed(ctx, closes, true); err != nil {
		log.Error(op, "could not close the merged workspace", dlog.Context{"workspace": string(closes), "error": err.Error()})
	}
	// THE ROSTER'S DURABLE HALF MOVED: merged_at and closed are what put the
	// row under `recently_merged`, and nothing else republishes them.
	if r.o.deps.PublishRegistry != nil {
		if err := r.o.deps.PublishRegistry(ctx); err != nil {
			log.Error(op, "could not republish the roster after the landing",
				dlog.Context{"workspace": string(closes), "error": err.Error()})
		}
	}
}

// abort ends a run that could not continue. It is the FAILED end reached by an
// error rather than a give-up, in the area "other", and it takes the same
// teardown.
func (r *run) abort(ctx context.Context, summary string) {
	r.enterTerminal(terminalOwedFailed)
	defer r.leaveTerminal()
	// A RUN THAT ENDED BECAUSE THE DAEMON IS EXITING DID NOT FAIL. Its step
	// broke on a git or a shim that the orderly exit had already taken away,
	// so it takes stop's account and abandons what it holds to the recovery.
	if r.exiting() {
		r.o.deps.Log.Global().Info("daemon.merge.stop", "a merge ended when the daemon exited", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "cause": summary})
		r.teardown(ctx, outcome{failed: summary, area: footer.FailedOther}, nil)
		return
	}
	// THE FAULT IS RECORDED HERE, at ERROR: an abort is reached by an error the
	// run could not continue past. The failed terminal's own record is the
	// outcome, at INFO, as for every failed merge.
	r.o.log(ctx, r.ws).Error("daemon.merge.fault", "a merge could not continue past an error; it fails", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "summary": summary})
	r.teardown(ctx, outcome{failed: summary, area: footer.FailedOther}, func() {
		r.failedTerminal(ctx, summary, footer.FailedOther)
	})
}

// stop ends a run whose git THIS DAEMON stopped — the context was cancelled or
// its deadline passed, at shutdown or because the operation was called off. It
// is not a verdict and not a fault, so it records once at INFO and publishes
// NO failure: an abort record here claimed the merge failed when nothing had.
//
// It takes abort's teardown unchanged, because everything the run holds (the
// lease, the occupancy, the queue entry, the displaced turn) must still be
// given back whichever way the run ended.
func (r *run) stop(ctx context.Context, err error) {
	r.enterTerminal(terminalOwedFailed)
	defer r.leaveTerminal()
	r.o.log(ctx, r.ws).Info("daemon.merge.stop", "a merge stopped when its git was cancelled", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "cause": err.Error()})
	r.teardown(ctx, outcome{failed: err.Error()}, nil)
}

// stopped classifies a run-ending error: a git the daemon itself cancelled
// takes stop, and everything else takes abort. It answers whether the run
// ended, so a caller reads as one branch.
func (r *run) stopped(ctx context.Context, err error) bool {
	if !gitclient.IsCancelled(err) {
		return false
	}
	r.stop(ctx, err)
	return true
}

// teardown is THE ONE RELEASE PATH. Every end a run can have -- landed,
// failed, abandoned (a dequeue, an evict, its workspace closed), stopped --
// comes through it, and it gives back everything the run held, in the one
// order that is safe: the output address, the occupancy, the lease, the queue
// entry, the displaced turn, the landed worktree, the queue's tree, and the
// repository's slot and lock. It fires the self-reload last.
//
// terminal, when set, publishes the run's terminal. It runs right after the
// lease is released, so no client is told the workspace was handed back while
// the lease still refuses its prompts; nil publishes nothing (a stop, or an
// exit that leaves the run to the recovery).
func (r *run) teardown(ctx context.Context, out outcome, terminal func()) {
	const op = "daemon.merge.teardown"
	if r.exiting() {
		r.abandonToRecovery(ctx, terminalOwed(out))
		return
	}
	log := r.o.log(ctx, r.ws)

	// The session's rows go back to the root feed the moment the bubble stops
	// being where its output belongs.
	r.o.deps.Feed.SetOutputAddress(r.ws, nil)
	if r.releaseOccupancy != nil {
		r.releaseOccupancy()
	}
	if err := r.o.deps.DB.ReleaseLease(ctx, r.lease.ID); err != nil {
		log.Error(op, "could not release the merge lease", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	r.keepOpenForHeldPrompts(ctx, log)
	r.o.deps.Queue.OnLeaseChanged(r.ws)
	if terminal != nil {
		terminal()
	}

	if err := r.o.deps.DB.RemoveMergeQueueEntry(ctx, r.repo, r.ws, terminalCause(out)); err != nil {
		log.Error(op, "could not drop the finished merge's queue entry", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo), "error": err.Error()})
	}
	r.o.mu.Lock()
	delete(r.o.running, r.repo)
	delete(r.o.runsByWorkspace, r.ws)
	delete(r.o.repoOf, r.ws)
	// The bubble's ledger identity retires with the merge it addressed; a
	// later merge of the same workspace gets a bubble of its own.
	delete(r.o.ledgerOf, r.ws)
	r.o.mu.Unlock()
	r.o.clearOffer(r.ws)

	// THE DISPLACED TURN GOES BACK BEFORE ANY TREE GOES AWAY. The
	// resubmission is a workspace-bound act -- it records a turn and writes to
	// that workspace's own log sink, which is a symlink INSIDE the worktree --
	// so a removal ahead of it makes the "exactly once" guarantee unreachable.
	r.resubmitDisplaced(ctx)

	// THE WORKSPACE A LANDING CLOSES goes only after the terminal was
	// published, and only for a merge that landed: a failed merge's branch
	// still holds work. Its session is ended and REAPED first, because a live
	// shim still has the directory as its working directory. A failed
	// stand-down leaves the tree intact, loudly.
	//
	// THE LOG SURFACES LET GO OF THE DIRECTORY INSIDE RemoveWorktree
	// (Surfaces.DetachDir), so a late record cannot recreate it.
	if out.failed == "" && out.abandoned == "" && out.landed != "" && r.subject.closes != "" {
		r.closeLanded(ctx, log)
	}
	// A WORKTREE THE MERGE MADE FOR A BRANCH goes with the merge -- except
	// when its conflict resolution gave up, which leaves the rebase in
	// progress in it for the user to carry on.
	if r.subject.made && out.area != footer.FailedConflicts {
		if err := r.o.deps.Git.RemoveWorktree(ctx, r.subject.targetDir, r.subject.dir); err != nil {
			log.Error(op, "could not remove the worktree the merge made for the branch", dlog.Context{
				"workspace": string(r.ws), "worktree": r.subject.dir, "error": err.Error()})
		}
	}
	// THE ONE RELEASE PATH ENDS WITH THE SLOT: the queue tree an attempt was
	// using goes, then the repository's slot and lock.
	r.dropTree(ctx)
	r.o.releaseSlot(ctx, r)
	if err := r.o.republishQueue(ctx, r.repo); err != nil {
		log.Error(op, "could not republish the queue", dlog.Context{"repo": string(r.repo), "error": err.Error()})
	}
	r.selfReload(ctx, out)
	r.o.kick(r.repo)
}

// keepOpenForHeldPrompts reads, ONCE THE LEASE IS RELEASED, whether a prompt
// held under the merge marked its queue entry to keep the requester open
// (wsm.bindMergeHold), and if so the requester no longer closes. After the
// release no merge hold can be recorded, so the read is final: a prompt held
// during the merge always has a session to run in once the merge lands.
//
// A read that fails keeps the requester open: closing removes its worktree,
// which cannot be undone, and an open workspace the user did not need is one
// they close themselves.
func (r *run) keepOpenForHeldPrompts(ctx context.Context, log dlog.Logger) {
	const op = "daemon.merge.teardown"
	if r.subject.closes != r.ws {
		return
	}
	entry, err := r.o.entryOf(ctx, r.repo, r.ws)
	if err != nil {
		log.Error(op, "could not read whether a held prompt keeps the requester open; it is kept open", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo), "error": err.Error()})
		r.subject.closes = ""
		return
	}
	if !entry.Source.KeepOpen {
		return
	}
	log.Info(op, "a prompt held during the merge keeps the requester open", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID)})
	r.subject.closes = ""
}

// closeLanded ends the landed-and-closed workspace's session and removes its
// worktree: the requester for its own branch (unless kept open) or a branch
// already merged upstream, the other workspace for another workspace's branch.
func (r *run) closeLanded(ctx context.Context, log dlog.Logger) {
	const op = "daemon.merge.teardown"
	closes := r.subject.closes
	job, err := r.o.layoutFor(ctx, closes)
	if err != nil {
		log.Error(op, "could not read the closed workspace's worktree to remove it", dlog.Context{
			"workspace": string(closes), "error": err.Error()})
		return
	}
	dir := job.Layout.SourceDir
	if err := r.o.deps.StopSession(ctx, closes, true); err != nil {
		log.Error(op, "could not stop the merged workspace's session before removing its worktree", dlog.Context{
			"workspace": string(closes), "worktree": dir, "force": true, "error": err.Error()})
		return
	}
	if err := r.o.deps.Git.RemoveWorktree(ctx, string(r.repo), dir); err != nil {
		log.Error(op, "could not remove the merged worktree", dlog.Context{
			"workspace": string(closes), "worktree": dir, "error": err.Error()})
		return
	}
	log.Debug(op, "stopped the merged workspace's session and removed its worktree", dlog.Context{
		"workspace": string(closes), "worktree": dir, "force": true})
}

// TerminalDrainBound is how long the orderly exit gives the merge runs that
// have already reached their TERMINAL to finish their durable stamps and their
// teardown before the state client closes under them.
//
// THE BOUND IS A LARGE MULTIPLE OF THE WORK IT COVERS. Everything it waits for
// is local: two SQLite writes (merged_at, closed), the roster republish, the
// lease release, the queue entry's removal and the synchronous republishes
// they trigger — tens of milliseconds on a healthy machine, and a worktree
// removal on top of that for a landing. Two seconds is that with more than an
// order of magnitude of headroom, and it is deliberately the same order as
// shutdownGrace so the whole orderly exit stays a predictable few seconds
// rather than an unbounded wait on work a boot recovery can redo anyway.
const TerminalDrainBound = 2 * time.Second

// terminal is one run's registered terminal work: the stamps and the teardown
// that follow the published terminal row. It exists so the shutdown drain can
// wait for THAT, and only that, without knowing anything about the phases.
type terminal struct {
	// ws is the merge's workspace, kept on the mark so the drain's WARN can
	// name it without re-reading a registration it is in the middle of.
	ws ids.WorkspaceID
	// done closes when the run's terminal work is over, teardown included.
	done chan struct{}
	// owed names the durable work the terminal still owes, for the WARN the
	// drain writes when its bound expires: a reader of that record has to know
	// what the boot recovery is going to find unstamped.
	owed string
	// lease is the merge's ledger identity, which is what the recovery's own
	// records are keyed by.
	lease ids.LeaseID
}

// The durable work each kind of terminal owes, spelled where the drain's WARN
// is read rather than at the two call sites.
const (
	terminalOwedLanded = "merged_at, closed, the roster republish and the teardown"
	terminalOwedFailed = "the teardown: the lease release and the queue entry's removal"
)

// terminalOwed names what one outcome's terminal owes.
func terminalOwed(out outcome) string {
	if out.failed != "" || out.abandoned != "" {
		return terminalOwedFailed
	}
	return terminalOwedLanded
}

// enterTerminal registers this run's terminal work with the orchestrator, so
// the shutdown drain knows to wait for it.
func (r *run) enterTerminal(owed string) {
	o := r.o
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.terminals == nil {
		o.terminals = map[ids.WorkspaceID]*terminal{}
	}
	// THE DRAIN'S SNAPSHOT AND THIS REGISTRATION SHARE THE MUTEX, which is
	// what makes the two sides of the window exact rather than likely: a
	// registration that gets in while draining is false is IN the drain's
	// snapshot and will be waited for, so its durable work is safe; one that
	// arrives after is not, and nothing holds the state client open for it.
	r.afterDrain = o.draining
	o.terminals[r.ws] = &terminal{ws: r.ws, done: make(chan struct{}), owed: owed, lease: r.lease.ID}
}

// exiting reports that this run reached its terminal after the shutdown drain
// had closed its snapshot: the daemon is on its way out, the state client is
// closing, and the boot recovery owns everything this run still holds.
func (r *run) exiting() bool { return r.afterDrain }

// abandonToRecovery is the terminal a run takes when the daemon exited out
// from under it. It gives back everything that lives in THIS process -- the
// output address, the occupancy guard, the orchestrator's maps, the queue lock
// -- and touches the state client, the git and the shim not at all: those
// writes would run against a closed store and fail, and the boot recovery
// redoes every one of them from the lease that is still on the row.
//
// IT IS RECORDED, AND AT INFO. An orderly exit is not a fault, so this is not
// an error; but the record names exactly what was left undone and who owns it,
// which is the same account the drain writes for a merge left mid-phase.
func (r *run) abandonToRecovery(ctx context.Context, owed string) {
	r.o.deps.Feed.SetOutputAddress(r.ws, nil)
	if r.releaseOccupancy != nil {
		r.releaseOccupancy()
	}
	r.o.mu.Lock()
	delete(r.o.running, r.repo)
	delete(r.o.runsByWorkspace, r.ws)
	delete(r.o.repoOf, r.ws)
	delete(r.o.ledgerOf, r.ws)
	r.o.mu.Unlock()
	r.o.clearOffer(r.ws)
	r.o.releaseSlot(ctx, r)
	r.o.deps.Log.Global().Info("daemon.merge.teardown",
		"the daemon exited before this merge's terminal could be written; the boot recovery owns what it left",
		dlog.Context{
			"workspace": string(r.ws),
			"lease":     string(r.lease.ID),
			"unstamped": owed,
		})
}

// leaveTerminal retires the registration and releases whatever is waiting on
// it. It runs even when the terminal path failed: a drain must never outlive
// the work it waits for.
func (r *run) leaveTerminal() {
	o := r.o
	o.mu.Lock()
	mark := o.terminals[r.ws]
	delete(o.terminals, r.ws)
	o.mu.Unlock()
	if mark != nil {
		close(mark.done)
	}
}

// Drain stops admitting merges and waits, WITHIN A BOUND, for every run that
// has already reached its terminal to finish its durable stamps and teardown.
//
// It is the daemon's orderly exit calling, with the state client still open. A
// merge still in a LONG PHASE — its tests, its agent turn — is NOT waited for:
// it is abandoned to the boot recovery exactly as it was before, but recorded
// at INFO with the phase it was left in, rather than discovered later through
// writes that failed against a closed store.
func (o *orchestrator) Drain(ctx context.Context) {
	const op = "daemon.merge.drain"
	log := o.deps.Log.Global()

	o.mu.Lock()
	o.draining = true
	// THE ADMISSION STEPS IN FLIGHT ARE COUNTED UNDER THIS LOCK. `draining`
	// is set under the same lock enterAdmission registers under, so none
	// begins after it, and a zero count here means there is nothing to wait
	// for -- known now, not learned later from a goroutine racing the bound.
	var admitted <-chan struct{}
	if o.admissions > 0 {
		if o.admissionsIdle == nil {
			o.admissionsIdle = make(chan struct{})
		}
		admitted = o.admissionsIdle
	}
	inFlight := o.admissions
	waits := make([]*terminal, 0, len(o.terminals))
	for _, mark := range o.terminals {
		waits = append(waits, mark)
	}
	type midPhase struct {
		ws    ids.WorkspaceID
		lease ids.LeaseID
		phase string
	}
	mid := make([]midPhase, 0, len(o.runsByWorkspace))
	for ws, r := range o.runsByWorkspace {
		if _, terminal := o.terminals[ws]; terminal {
			continue
		}
		mid = append(mid, midPhase{ws: ws, lease: r.lease.ID, phase: r.activeTab()})
	}
	o.mu.Unlock()

	// A MID-PHASE MERGE IS ANNOUNCED, NOT WAITED FOR. Its phase is the whole
	// point of the record: what the boot recovery will find, and where.
	for _, m := range mid {
		log.Info(op, "a merge was left mid-phase by the daemon's exit; the boot recovery owns it",
			dlog.Context{"workspace": string(m.ws), "lease": string(m.lease), "phase": m.phase})
	}
	bound := o.terminalDrainBound()
	expired := time.NewTimer(bound)
	defer expired.Stop()
	// THE ADMISSION STEPS ALREADY IN FLIGHT FINISH FIRST. The ones that began
	// before the drain are a few store reads, and the state client must
	// outlive them.
	if admitted != nil {
		select {
		case <-admitted:
		case <-expired.C:
			log.Error(op, "an admission step outlived the drain's bound; the state client closes under it",
				dlog.Context{"bound": bound.String(), "in_flight": inFlight})
			return
		case <-ctx.Done():
			log.Error(op, "the drain was cancelled while an admission step was in flight; the state client closes under it",
				dlog.Context{"bound": bound.String(), "in_flight": inFlight})
			return
		}
	}
	if len(waits) == 0 {
		log.Debug(op, "the merge drain had no terminal work to wait for",
			dlog.Context{"mid_phase": len(mid)})
		return
	}
	// THE TEST SEAM, NIL IN PRODUCTION. See orchestrator.onDrainWait: it is
	// called with the admission already stopped and the terminal work already
	// snapshotted, immediately before the wait begins.
	if o.onDrainWait != nil {
		o.onDrainWait()
	}
	for _, mark := range waits {
		select {
		case <-mark.done:
		case <-expired.C:
			o.warnUndrained(op, waits, bound, "the drain's bound expired")
			return
		case <-ctx.Done():
			o.warnUndrained(op, waits, bound, "the drain was cancelled")
			return
		}
	}
	log.Debug(op, "the merge drain finished every terminal it was holding",
		dlog.Context{"terminals": len(waits), "mid_phase": len(mid), "bound": bound.String()})
}

// warnUndrained names EVERY merge whose terminal work the drain gave up on and
// what each of them left unstamped, so the work the next boot's recovery has
// to redo is visible before that boot rather than after it.
func (o *orchestrator) warnUndrained(op string, waits []*terminal, bound time.Duration, why string) {
	for _, mark := range waits {
		select {
		case <-mark.done:
			continue
		default:
		}
		o.deps.Log.Global().Warn(op, "a merge's terminal did not finish before the daemon exited; the boot recovery owns what it left",
			dlog.Context{
				"workspace": string(mark.ws),
				"lease":     string(mark.lease),
				"unstamped": mark.owed,
				"bound":     bound.String(),
				"why":       why,
			})
	}
}

// terminalDrainBound answers the bound in force: the test override when one is
// set, else the production TerminalDrainBound.
func (o *orchestrator) terminalDrainBound() time.Duration {
	if o.drainBound > 0 {
		return o.drainBound
	}
	return TerminalDrainBound
}

// terminalCause names why a queue entry was dropped, which the queue's own log
// record keeps.
func terminalCause(out outcome) string {
	switch {
	case out.abandoned != "":
		return string(out.abandoned)
	case out.failed != "":
		return "failed"
	}
	return "merged"
}

// resubmitDisplaced puts back the user turn the merge displaced, EXACTLY ONCE.
// The capture is durable, so the resubmission survives a bounce; the run's own
// record is cleared by the submission itself, so a second teardown cannot
// double it, and the DURABLE mark is retired here so the boot recovery's sweep
// (recoverDisplaced) never puts the same turn back a second time.
//
// The text comes from the CAPTURE, not from a re-read of the open turns: the
// displaced turn was ended when the merge took the session away from it, so its
// record is no longer open by the time the lease is released.
func (r *run) resubmitDisplaced(ctx context.Context) {
	if r.displaced == nil {
		return
	}
	displaced := *r.displaced
	r.displaced = nil
	fields := dlog.Context{"workspace": string(r.ws), "turn": string(displaced.Turn)}
	// THE CLAIM GOES DOWN BEFORE THE SUBMISSION, AND THE DATABASE ARBITRATES
	// IT. Clearing the durable mark is what tells the boot recovery's sweep
	// this record is spent; a submission made first, with a crash before the
	// mark came down, would have the next boot put the same turn back a second
	// time. A claim that answers false means the sweep already put it back,
	// and this owner resubmits NOTHING.
	claimed, err := r.o.deps.Queue.ClaimDisplacedTurn(ctx, r.ws, displaced.Turn)
	if err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.resubmit", "could not claim the displaced turn",
			withField(fields, "error", err.Error()))
		return
	}
	if !claimed {
		r.o.log(ctx, r.ws).Debug("daemon.merge.resubmit", "the displaced turn was already put back by a boot recovery", fields)
		return
	}
	if _, err := r.o.deps.Queue.Submit(ctx, promptqueue.Submission{
		WS: r.ws, Turn: wsm.NewTurnID(), Said: saidText(displaced.Text),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
	}); err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.resubmit", "could not resubmit the displaced turn",
			withField(fields, "error", err.Error()))
		return
	}
	r.o.log(ctx, r.ws).Debug("daemon.merge.resubmit", "resubmitted the displaced turn", fields)
}

// withField copies a record's fields with one more on it, so two records built
// from the same base cannot scribble on each other.
func withField(fields dlog.Context, name string, value any) dlog.Context {
	out := make(dlog.Context, len(fields)+1)
	for k, v := range fields {
		out[k] = v
	}
	out[name] = value
	return out
}

// selfReload fires the daemon's own redeploy, and ONLY for a merge that landed
// in the daemon's OWN CHECKOUT. A sibling worktree of the same repository is
// excluded: it shares the common dir but is not the tree the running binary was
// built from.
func (r *run) selfReload(ctx context.Context, out outcome) {
	const op = "daemon.merge.self_reload"
	if out.failed != "" || out.landed == "" || !r.selfCheckout || len(out.commits) == 0 {
		return
	}
	// ONE MERGE IS ONE LANDING, AND ONE DEPLOY: the deploy is told once with
	// every commit the merge landed, never once per commit.
	r.o.log(ctx, r.ws).Info(op, "a merge landed in this daemon's own checkout; the deploy is told once",
		dlog.Context{"workspace": string(r.ws), "commit": out.landed, "commits": len(out.commits)})
	r.o.deps.Rollout.Landed(ctx, out.commits)
}

// publishAbandoned draws the ABANDONED terminal: a merge taken off the queue
// before it ever reached the front. It has no commit, no failure and no
// teardown, because nothing of it ever ran.
// ledger is the bubble's ledger identity, captured before the caller dropped
// it: an empty one means no bubble was ever drawn and there is nothing to end.
//
// THE ABANDONED ARM IS STILL THE ONLY ARM. `FeedMergeError` carries `failed`
// and `abandoned` and nothing else, so a user drop, a dequeue release, a
// workspace close and a daemon shutdown all end here under the same arm. What
// landing 7 added is `FeedMergeAbandoned.summary`: the resolved sentence for
// the collapsed line, composed from the abandon CAUSE, so the cause reaches a
// reader in prose even though the arm does not distinguish it.
func (o *orchestrator) publishAbandoned(ctx context.Context, ws ids.WorkspaceID, ledger ids.LeaseID, entry wsm.MergeQueueEntry, cause AbandonCause) {
	const op = "daemon.merge.abandoned"
	summary, declared := cause.summary()
	if !declared {
		// AN UNDECLARED CAUSE IS A DEFECT, never a silent empty line: the
		// bubble still ends, and the raw cause rides the sentence so the
		// reader is not left with nothing while the ERROR names the gap.
		summary = fmt.Sprintf("this merge left the queue before it ran (%s)", cause)
		o.deps.Log.Global().Error(op, "a merge was abandoned under a cause with no declared summary",
			dlog.Context{"workspace": string(ws), "cause": string(cause)})
	}
	// THE ABANDONMENT IS AN INFO RECORD, keyed by the cause: which of the four
	// ends a merge had is exactly what a reader of the run log comes for, and
	// the cause key is what makes them countable.
	o.deps.Log.Global().Info(op, "a merge left the queue without running",
		dlog.Context{"workspace": string(ws), "cause": string(cause), "summary": summary})
	if ledger != "" {
		// THE HEAD'S CLOCK RUNS FROM WHEN THE MERGE WAS QUEUED, as every other
		// drawing of the head; a queue entry with no queued time is a defect.
		queued, err := enqueuedMS(entry)
		if err != nil {
			o.deps.Log.Global().Error(op, "an abandoned merge's queue entry carries no queued time; its bubble is not ended",
				dlog.Context{"workspace": string(ws), "error": err.Error()})
			return
		}
		label := o.abandonedLabel(ctx, ws, entry.Source)
		o.deps.Feed.UpsertDurable(ws, feedid.Feed{Root: true}, headRow(ws, ledger, label, queued,
			&frontendv1.FeedMergeError{
				EndedAtMs: o.nowMS(),
				Reason: &frontendv1.FeedMergeError_Abandoned{
					Abandoned: &frontendv1.FeedMergeAbandoned{Summary: summary},
				},
			}))
	}
	// The surfaces come AFTER the terminal, in the teardown's own order: the
	// footer leaves its merging state and the roster sheds every merge arm
	// only once the bubble a reader is looking at has ended.
	o.forget(ws)
}

// abandonedLabel is the head's branch line for a merge that never ran. The
// label is read where it is available and the workspace's own id stands in
// where it is not: an unreadable label must not stop the bubble from ending.
func (o *orchestrator) abandonedLabel(ctx context.Context, ws ids.WorkspaceID, source wsm.MergeSource) string {
	label, err := o.sourceLabel(ctx, ws, source)
	if err != nil {
		o.deps.Log.Global().Error("daemon.merge.abandoned",
			"the abandoned merge's label could not be read; its id stands in",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
		return string(ws)
	}
	return label
}

// RetireConcluded retires a concluded merge's standing state once the workspace
// has moved on: the user's next prompt after a failed merge is an ordinary turn,
// and a landed merge of a requester kept open stands until then too. The one release path cannot do it: it runs at the merge's
// end, and the concluded state is what a reader should see until the
// workspace does something else. What retires it is that something.
func (o *orchestrator) RetireConcluded(ctx context.Context, ws ids.WorkspaceID) {
	o.mu.Lock()
	facts, known := o.facts[ws]
	_, running := o.runsByWorkspace[ws]
	o.mu.Unlock()
	if !known || running || (facts.State != StateFailed && facts.State != StateMerged) {
		return
	}
	o.log(ctx, ws).Info("daemon.merge.retire", "retired a concluded merge's standing state; the workspace moved on", dlog.Context{
		"workspace": string(ws), "state": facts.State})
	o.forget(ws)
}

// OnInterrupt raises the dequeue offer for a workspace whose merge is queued.
// An interrupt no longer silently yanks a merge off the queue: the daemon asks.
func (o *orchestrator) OnInterrupt(ctx context.Context, ws ids.WorkspaceID) {
	const op = "daemon.merge.interrupt"
	if _, err := o.queueOf(ctx, ws); err != nil {
		return
	}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not compose the dequeue offer",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
		return
	}
	o.mu.Lock()
	o.offers[ws] = true
	o.mu.Unlock()
	o.deps.Holds.SetOffer(ws, dequeueOffer(record.Name))
	o.log(ctx, ws).Debug(op, "raised the merge dequeue offer", dlog.Context{"workspace": string(ws)})
}

// AnswerDequeue answers the tray's offer: keep the queue slot, or release it.
// Releasing a merge that is already running abandons it (dequeue).
func (o *orchestrator) AnswerDequeue(ctx context.Context, ws ids.WorkspaceID, keep bool) error {
	const op = "daemon.merge.answer_dequeue"
	o.mu.Lock()
	standing := o.offers[ws]
	o.mu.Unlock()
	if !standing {
		return refuse(ArmNoOfferStanding, ws, "no merge dequeue offer is standing for this workspace")
	}
	if keep {
		o.clearOffer(ws)
		o.log(ctx, ws).Debug(op, "kept a queued merge's slot", dlog.Context{"workspace": string(ws)})
		return nil
	}
	// AN ANSWERED MENU IS AN ORDINARY OUTCOME. Both arms of this answer are
	// the user working the offer the daemon itself raised; the keep arm is
	// DEBUG and the release arm is no more of a warning than it is. The work
	// actually abandoned is recorded by dropQueued's own WARN below.
	o.log(ctx, ws).Debug(op, "releasing a queued merge's slot on the user's answer", dlog.Context{"workspace": string(ws)})
	return o.dequeue(ctx, ws, CauseUserDequeue)
}

// clearOffer retires the dequeue offer, whether it was answered, superseded by
// the merge's own terminal, or ended by the merge leaving the queue.
func (o *orchestrator) clearOffer(ws ids.WorkspaceID) {
	o.mu.Lock()
	had := o.offers[ws]
	delete(o.offers, ws)
	o.mu.Unlock()
	if had {
		o.deps.Holds.SetOffer(ws, nil)
	}
}

// abandonment is the cause an abandon cancels a running merge's context with.
type abandonment struct{ cause AbandonCause }

func (a *abandonment) Error() string {
	return fmt.Sprintf("merge: the merge was abandoned (%s)", a.cause)
}

// abandonCauseOf answers the abandon cause a run's context was cancelled with,
// and false when it was not abandoned.
func abandonCauseOf(ctx context.Context) (AbandonCause, bool) {
	var a *abandonment
	if errors.As(context.Cause(ctx), &a) {
		return a.cause, true
	}
	return "", false
}

// abandonRunning ABANDONS a merge that is running, whatever step it is on. Its context is cancelled
// with the cause, whatever phase it is in stops, and it ends on the abandoned
// terminal through the one release path; this returns once that is done, so
// the caller's answer is the released state.
func (o *orchestrator) abandonRunning(ctx context.Context, r *run, cause AbandonCause) error {
	o.log(ctx, r.ws).Info("daemon.merge.abandon", "abandoning a running merge", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "cause": string(cause), "step": string(r.currentStep())})
	r.cancel(&abandonment{cause: cause})
	select {
	case <-r.finished:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// abandonTerminal ends a RUNNING merge that was taken out: the abandoned
// terminal, its ledger's open intervals closed as abandoned, and the one
// teardown. The workspace's work is untouched: nothing landed, so its
// worktree stays.
func (r *run) abandonTerminal(ctx context.Context, cause AbandonCause) {
	const op = "daemon.merge.abandoned"
	r.enterTerminal(terminalOwedFailed)
	defer r.leaveTerminal()
	if r.exiting() {
		r.teardown(ctx, outcome{abandoned: cause}, nil)
		return
	}
	summary, declared := cause.summaryRunning()
	if !declared {
		summary = fmt.Sprintf("this merge was abandoned while it was running (%s)", cause)
		r.o.deps.Log.Global().Error(op, "a merge was abandoned under a cause with no declared summary",
			dlog.Context{"workspace": string(r.ws), "cause": string(cause)})
	}
	r.closeOpenRounds(ctx, "abandoned")
	r.o.log(ctx, r.ws).Info(op, "abandoned a running merge", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "cause": string(cause), "summary": summary})
	r.teardown(ctx, outcome{abandoned: cause}, func() {
		r.head(&frontendv1.FeedMergeError{
			EndedAtMs: r.o.nowMS(),
			Reason:    &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{Summary: summary}},
		})
	})
	// The surfaces come AFTER the terminal and the release, as a queued
	// merge's abandon orders them: the merge never happened as far as the
	// footer and the roster are concerned.
	r.o.forget(r.ws)
}

// closeOpenRounds closes every tab round whose ledger interval is still open,
// with one outcome.
func (r *run) closeOpenRounds(ctx context.Context, outcome string) {
	r.mu.Lock()
	open := make([]tabRound, 0, len(r.openRounds))
	for _, round := range r.openRounds {
		open = append(open, round)
	}
	r.mu.Unlock()
	sort.Slice(open, func(i, j int) bool { return open[i].key() < open[j].key() })
	for _, round := range open {
		r.closeTab(ctx, round, outcome)
	}
}

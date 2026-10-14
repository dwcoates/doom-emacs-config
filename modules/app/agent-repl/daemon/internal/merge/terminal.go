package merge

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
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
		// Nothing landed, so the count its turns last recorded stands.
		r.stampClosed(ctx, log, op, nil)
		r.o.ended(ctx, r.ws, DetachedOutcome{Kind: DetachedLanded, Summary: fmt.Sprintf("the branch was already on %s; nothing needed merging", out.alreadyOn)})
		log.Info(op, "a merge concluded with its branch already on the target", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "target": out.alreadyOn, "tip": out.landed})
	default:
		r.head(landedHead{success: &frontendv1.FeedMergeSuccess{EndedAtMs: endedMS, Commit: out.landed}, version: headVersion(out.version)})
		r.concluded(MergeFacts{State: StateMerged})
		r.stampClosed(ctx, log, op, landedCount(out))
		r.o.ended(ctx, r.ws, DetachedOutcome{Kind: DetachedLanded, Summary: fmt.Sprintf("it landed as commit %s", out.landed), Deploy: r.landingDeploy(ctx, out)})
		fields := dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "commit": out.landed, "commits": len(out.commits),
			"source": r.source.Kind.String(), "closes": string(r.subject.closes)}
		if out.version != nil {
			fields["version"] = out.version.tag
		}
		log.Info(op, "a merge landed", fields)
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
	r.o.ended(ctx, r.ws, DetachedOutcome{Kind: DetachedFailed, Summary: summary})
}

// ended is EVERY MERGE'S RESOLUTION, however it left the queue for good:
// landed, failed, or abandoned after it was put in line. It raises the
// merge's second and last desktop banner, on the workspace the merge ran in,
// and then hands the outcome back to a detached merge's requester (handBack).
// Every terminal goes through it, so no resolution goes unannounced and none
// is announced twice.
func (o *orchestrator) ended(ctx context.Context, ws ids.WorkspaceID, out DetachedOutcome) {
	o.log(ctx, ws).Debug("daemon.merge.ended", "a merge left the queue for good; raising its resolved banner", dlog.Context{
		"workspace": string(ws), "outcome": out.Kind.String()})
	o.deps.Banners.Resolved(ws, out.Kind.mergeEnd(), out.Summary)
	o.handBack(ctx, ws, out)
}

// handBack reports how a merge ended to the workspace whose feed carries the
// bubble: an ASYNC merge's requester whether it landed or not, and a SYNC
// merge's own workspace when it did not land (one that landed closed it). A
// merge that FAILED or was ABANDONED also reveals an async merge's merge
// workspace: it needs the user now.
//
// THE HANDBACK'S OWN FAILURE IS RECORDED BY ITS OWNER (DetachedMerges.HandBack);
// the merge's terminal stands either way, so it is only noted here.
func (o *orchestrator) handBack(ctx context.Context, ws ids.WorkspaceID, out DetachedOutcome) {
	requester, async := o.requesterOf(ws)
	if !async {
		o.handBackInPlace(ctx, ws, out)
		return
	}
	// AN END SOMEBODY ASKED FOR IS NOT A FAILURE: the bubble and the roster
	// say the merge ended, and neither is the merge workspace revealed nor the
	// requester's agent prompted to deal with it.
	if out.Requested {
		o.log(ctx, ws).Info("daemon.merge.detached", "an async merge was taken off at an explicit request; its requester is not handed it back", dlog.Context{
			"workspace": string(ws), "requester": string(requester), "outcome": out.Kind.String(), "summary": out.Summary})
		return
	}
	if out.Kind != DetachedLanded {
		o.reveal(ctx, ws, "its merge "+out.Kind.String())
	}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		o.log(ctx, ws).Error("daemon.merge.detached", "could not read the merge workspace whose merge ended; its requester is not told", dlog.Context{
			"workspace": string(ws), "requester": string(requester), "outcome": out.Kind.String(), "error": err.Error()})
		return
	}
	// THE MERGE IS NAMED BY ITS MERGE WORKSPACE, whose name the requesting
	// agent chose for what it merges.
	out.Branch = record.Name
	if err := o.deps.Detached.HandBack(ctx, requester, record, out); err != nil {
		o.log(ctx, ws).Debug("daemon.merge.detached", "the handback of an async merge failed; its owner recorded why", dlog.Context{
			"workspace": string(ws), "requester": string(requester), "outcome": out.Kind.String(), "error": err.Error()})
	}
}

// handBackInPlace reports a SYNC merge that did not land to its own
// workspace's conversation, which stays open after it: a failure, or an
// abandonment nobody asked for. A landing closed the workspace, and an end
// somebody asked for failed nothing, so neither is reported.
func (o *orchestrator) handBackInPlace(ctx context.Context, ws ids.WorkspaceID, out DetachedOutcome) {
	if out.Kind == DetachedLanded || out.Requested {
		return
	}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		o.log(ctx, ws).Error("daemon.merge.handback", "could not read the workspace whose merge ended; it is not told", dlog.Context{
			"workspace": string(ws), "outcome": out.Kind.String(), "error": err.Error()})
		return
	}
	out.InPlace = true
	out.Branch = record.Branch
	if err := o.deps.Detached.HandBack(ctx, ws, record, out); err != nil {
		o.log(ctx, ws).Debug("daemon.merge.handback", "the handback of an in-place merge failed; its owner recorded why", dlog.Context{
			"workspace": string(ws), "outcome": out.Kind.String(), "error": err.Error()})
	}
}

// concluded publishes a concluded merge's facts.
func (r *run) concluded(facts MergeFacts) {
	r.mu.Lock()
	r.facts = facts
	r.mu.Unlock()
	r.o.publish(r.ws, facts)
}

// landedCount is the commit count a landed merge brought in: the commits its
// merge commit added, read off the commit itself. Nil when that range could not
// be read (run.landed recorded why), so the count the workspace's turns last
// recorded stands rather than a zero nobody counted.
func landedCount(out outcome) *uint32 {
	if len(out.commits) == 0 {
		return nil
	}
	n := uint32(len(out.commits))
	return &n
}

// stampClosed writes the durable half of the workspace a landed merge closes:
// merged_at, the commit count the merge brought in (LANDED, nil to keep the
// recorded one), and closed, which put its roster row under
// `recently_merged`, and the republish nothing else would make. A merge that
// closes nothing -- a requester kept open, a branch that is no workspace --
// stamps nothing.
func (r *run) stampClosed(ctx context.Context, log dlog.Logger, op string, landed *uint32) {
	closings := r.closings(ctx, log)
	if len(closings) == 0 {
		return
	}
	for _, closes := range closings {
		if err := r.o.deps.DB.SetMergedAt(ctx, closes, r.o.deps.Now(), landed); err != nil {
			log.Error(op, "could not stamp the merge's landing", dlog.Context{"workspace": string(closes), "error": err.Error()})
		}
		if err := r.o.deps.DB.SetClosed(ctx, closes, true); err != nil {
			log.Error(op, "could not close the merged workspace", dlog.Context{"workspace": string(closes), "error": err.Error()})
		}
	}
	closes := closings[0]
	// THE ROSTER'S DURABLE HALF MOVED: merged_at and closed are what put the
	// row under `recently_merged`, and nothing else republishes them.
	if r.o.deps.PublishRegistry != nil {
		if err := r.o.deps.PublishRegistry(ctx); err != nil {
			log.Error(op, "could not republish the roster after the landing",
				dlog.Context{"workspace": string(closes), "error": err.Error()})
		}
	}
}

// closings answers the workspaces a landing closes: the subject's own, then
// ANOTHER workspace an async merge's one-shot named, while that one is still
// open. A one-shot workspace that closed meanwhile is passed over (it is
// already closed), and one whose record cannot be read is kept open, loudly.
func (r *run) closings(ctx context.Context, log dlog.Logger) []ids.WorkspaceID {
	if r.closed != nil {
		return *r.closed
	}
	out := r.readClosings(ctx, log)
	r.closed = &out
	return out
}

// readClosings is closings' one read of the workspaces a landing closes.
func (r *run) readClosings(ctx context.Context, log dlog.Logger) []ids.WorkspaceID {
	var out []ids.WorkspaceID
	if r.subject.closes != "" {
		out = append(out, r.subject.closes)
	}
	also := r.subject.closesAlso
	if also == "" || also == r.subject.closes {
		return out
	}
	record, err := r.o.deps.DB.Workspace(ctx, also)
	switch {
	case err != nil:
		log.Error("daemon.merge.teardown", "could not read the workspace an async merge's one-shot closes; it is kept open", dlog.Context{
			"workspace": string(r.ws), "one_shot": string(also), "error": err.Error()})
		return out
	case record.Closed:
		log.Info("daemon.merge.teardown", "the workspace an async merge's one-shot closes is already closed", dlog.Context{
			"workspace": string(r.ws), "one_shot": string(also)})
		return out
	}
	return append(out, also)
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

	// No turn opened from here on is the merge's: the standing address is
	// withdrawn the moment the bubble stops being where the merge's output
	// belongs.
	r.withdrawOutput()
	if r.releaseOccupancy != nil {
		r.releaseOccupancy()
	}
	if err := r.o.deps.DB.ReleaseLease(ctx, r.lease.ID); err != nil {
		log.Error(op, "could not release the merge lease", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	// THE PROGRESS RECORD GOES WITH THE LEASE: the merge is over, and nothing
	// is left to resume.
	if _, err := r.o.deps.DB.DropMergeProgress(ctx, r.lease.ID); err != nil {
		log.Error(op, "could not drop the finished merge's progress record", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	r.keepOpenForHeldPrompts(ctx, log)
	r.o.deps.Queue.OnLeaseChanged(r.ws)
	if terminal != nil {
		terminal()
	}

	if err := r.o.settleLeavingQueueTab(ctx, r.repo, r.ws, r.lease.ID, terminalCause(out)); err != nil {
		log.Error(op, "could not settle the finished merge's queue tab", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo), "error": err.Error()})
	}
	if err := r.o.deps.DB.RemoveMergeQueueEntry(ctx, r.repo, r.ws, terminalCause(out)); err != nil {
		log.Error(op, "could not drop the finished merge's queue entry", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo), "error": err.Error()})
	}
	r.o.mu.Lock()
	if r.o.running[r.repo] == r {
		delete(r.o.running, r.repo)
	}
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
	if out.failed == "" && out.abandoned == "" && out.landed != "" {
		for _, closes := range r.closings(ctx, log) {
			r.closeLanded(ctx, log, closes)
		}
		r.cleanupLanded(ctx, log)
	}
	// A WORKTREE THE MERGE MADE FOR A BRANCH goes with the merge -- except
	// when its conflict resolution gave up, which leaves the rebase in
	// progress in it for the user to carry on.
	if r.subject.made && out.area != footer.FailedConflicts {
		if err := r.git.RemoveWorktree(ctx, r.subject.targetDir, r.subject.dir); err != nil {
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
	// A LANDING MOVED THE TARGET: every merge still waiting in line rebases
	// onto it (rebasewaiting.go).
	if out.failed == "" && out.abandoned == "" && out.landed != "" && out.alreadyOn == "" {
		r.o.rebaseWaiting(r.repo)
	}
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
func (r *run) closeLanded(ctx context.Context, log dlog.Logger, closes ids.WorkspaceID) {
	const op = "daemon.merge.teardown"
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
	if err := r.git.RemoveWorktree(ctx, string(r.repo), dir); err != nil {
		log.Error(op, "could not remove the merged worktree", dlog.Context{
			"workspace": string(closes), "worktree": dir, "error": err.Error()})
		return
	}
	// INFO, not DEBUG: the worktree going is the edge every client sees its
	// workspace's directory vanish at, and a diagnosis of what a client did
	// next reads the removal off this record (owner SOP, 2026-10-06).
	log.Info(op, "stopped the merged workspace's session and removed its worktree", dlog.Context{
		"workspace": string(closes), "worktree": dir, "force": true})
}

// cleanupLanded removes the worktree a landed async merge asked to clean up
// (wsm.MergeOrigin.CleanupDir) through git's clean door, never forced: a
// worktree that holds uncommitted changes is kept, and says so. The branch is
// never deleted.
func (r *run) cleanupLanded(ctx context.Context, log dlog.Logger) {
	const op = "daemon.merge.teardown"
	dir := r.subject.cleanupDir
	if dir == "" {
		return
	}
	fields := dlog.Context{"workspace": string(r.ws), "worktree": dir}
	clean, err := r.git.IsClean(ctx, dir)
	if err != nil {
		log.Error(op, "could not read whether the worktree to clean up is clean; it is kept", withField(fields, "error", err.Error()))
		return
	}
	if !clean {
		log.Info(op, "kept the worktree an async merge asked to clean up: it holds uncommitted changes", fields)
		return
	}
	if err := r.git.RemoveCleanWorktree(ctx, string(r.repo), dir); err != nil {
		log.Error(op, "could not remove the worktree an async merge asked to clean up", withField(fields, "error", err.Error()))
		return
	}
	log.Info(op, "removed the worktree an async merge asked to clean up", fields)
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
	r.suspending = r.suspending || o.draining
	o.terminals[r.ws] = &terminal{ws: r.ws, done: make(chan struct{}), owed: owed, lease: r.lease.ID}
}

// exiting reports that the daemon's exit took this run away: the drain marked
// it mid-step, or it reached its terminal after the drain had closed its
// snapshot. The state client is closing, and the next boot owns -- and
// resumes -- everything this run still holds.
func (r *run) exiting() bool {
	r.o.mu.Lock()
	defer r.o.mu.Unlock()
	return r.suspending
}

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
	r.releaseInProcess(ctx)
	r.o.deps.Log.Global().Info("daemon.merge.teardown",
		"the daemon exited before this merge's terminal could be written; the boot recovery owns what it left",
		dlog.Context{
			"workspace": string(r.ws),
			"lease":     string(r.lease.ID),
			"unstamped": owed,
		})
}

// releaseInProcess gives back what a run holds IN THIS PROCESS -- the output
// address, the occupancy guard, the orchestrator's maps, the slot and the
// queue lock -- and touches the state client, the git and the shim not at
// all.
func (r *run) releaseInProcess(ctx context.Context) {
	r.withdrawOutput()
	if r.releaseOccupancy != nil {
		r.releaseOccupancy()
	}
	r.o.mu.Lock()
	if r.o.running[r.repo] == r {
		delete(r.o.running, r.repo)
	}
	delete(r.o.runsByWorkspace, r.ws)
	delete(r.o.repoOf, r.ws)
	delete(r.o.ledgerOf, r.ws)
	r.o.mu.Unlock()
	r.o.clearOffer(r.ws)
	r.o.releaseSlot(ctx, r)
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

// Drain stops admitting merges, brings every merge still mid-step to a
// STOPPING POINT, and waits, WITHIN A BOUND, for every run that has already
// reached its terminal to finish its durable stamps and teardown.
//
// It is the daemon's orderly exit calling, with the state client still open.
//
// A MID-STEP MERGE IS SUSPENDED, NEVER INTERRUPTED (owner ruling, 2026-10-06).
// Its git gate is stopped -- no git starts after this -- and the command
// already running is waited for to its end (MergeGitStopBound), because a git
// killed mid-command leaves a tree the resume could not read. Only then are
// its waits cut: an agent turn (which lives on in the shim, and the resumed
// merge reattaches to it) and a test gate run (cut here, and run again from
// its start on resume -- the tests are idempotent). The run then lets go of
// what it holds in this process and leaves its lease, its queue entry and its
// progress record to the next boot, which resumes it at the step it recorded.
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
	// EVERY RUN NOT IN ITS TERMINAL IS SUSPENDED, under the lock its terminal
	// registers under: a run is either waited for as a terminal or suspended,
	// never neither.
	var suspended []*run
	for ws, r := range o.runsByWorkspace {
		if _, terminal := o.terminals[ws]; terminal {
			continue
		}
		r.suspending = true
		suspended = append(suspended, r)
	}
	o.mu.Unlock()

	// THE GIT IN FLIGHT FINISHES FIRST. Every suspended run's gate is stopped
	// before any is waited for, so no run starts a command while another's is
	// being waited out.
	o.awaitStoppingPoints(op, suspended)
	for _, r := range suspended {
		step := r.progressStep()
		fields := dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "step": step, "tab": r.activeTab()}
		if r.activeTab() == TabTests {
			log.Info(op, "the merge's test gate is cut by the daemon's exit; the resumed merge runs it again from its start", fields)
		} else {
			log.Info(op, "a merge is suspended at a stopping point by the daemon's exit; the next boot resumes it at the step it recorded", fields)
		}
		r.cancel(errDaemonExiting)
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
	// THE SUSPENDED RUNS LET GO NEXT: a cut wait answers at once, and what a
	// run does on its way out is in-process only.
	for _, r := range suspended {
		select {
		case <-r.finished:
		case <-expired.C:
			log.Error(op, "a suspended merge did not let go before the drain's bound; the state client closes under it",
				dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "bound": bound.String()})
			return
		case <-ctx.Done():
			log.Error(op, "the drain was cancelled while a suspended merge was letting go; the state client closes under it",
				dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "bound": bound.String()})
			return
		}
	}
	if len(waits) == 0 {
		log.Debug(op, "the merge drain had no terminal work to wait for",
			dlog.Context{"suspended": len(suspended)})
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
		dlog.Context{"terminals": len(waits), "suspended": len(suspended), "bound": bound.String()})
}

// errDaemonExiting is the cause the drain cuts a suspended run's waits with.
var errDaemonExiting = errors.New("merge: the daemon is exiting; the merge is suspended for the next boot to resume")

// awaitStoppingPoints stops every suspended run's git gate and waits, within
// MergeGitStopBound, for the commands they had in flight to end. Each wait is
// MEASURED and recorded; a command still running at the bound is an ERROR
// naming it, and the exit goes on.
func (o *orchestrator) awaitStoppingPoints(op string, suspended []*run) {
	log := o.deps.Log.Global()
	type standing struct {
		r       *run
		idle    <-chan struct{}
		running gitInFlight
	}
	all := make([]standing, 0, len(suspended))
	for _, r := range suspended {
		idle, running := r.git.stop()
		all = append(all, standing{r: r, idle: idle, running: running})
	}
	bound := o.gitStopBound()
	expired := time.NewTimer(bound)
	defer expired.Stop()
	began := o.deps.Now()
	// THE TEST SEAM, NIL IN PRODUCTION: called with every gate stopped,
	// immediately before the wait for the commands in flight begins.
	if o.onGitStopWait != nil {
		o.onGitStopWait()
	}
	for _, st := range all {
		fields := dlog.Context{"workspace": string(st.r.ws), "lease": string(st.r.lease.ID)}
		if st.running.name == "" {
			log.Debug(op, "the merge had no git in flight; it stops here", fields)
			continue
		}
		fields["git"] = st.running.name
		select {
		case <-st.idle:
			fields["waited_ms"] = o.deps.Now().Sub(began).Milliseconds()
			fields["ran_ms"] = o.deps.Now().Sub(st.running.since).Milliseconds()
			log.Info(op, "waited for the merge's git command to finish before the exit", fields)
		case <-expired.C:
			fields["bound"] = bound.String()
			fields["ran_ms"] = o.deps.Now().Sub(st.running.since).Milliseconds()
			log.Error(op, "the merge's git command outlived the exit's bound; the exit goes on without it, and the resume reads the tree it leaves", fields)
			return
		}
	}
}

// gitStopBound answers the bound in force: the test override when one is set,
// else MergeGitStopBound.
func (o *orchestrator) gitStopBound() time.Duration {
	if o.gitBound > 0 {
		return o.gitBound
	}
	return MergeGitStopBound
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

// landingDeploy decides, ONCE per run, what the landing does about deploying
// itself, and answers the same decision to every later caller: the report
// (ended) and the deploy (selfReload) read it, so a report never says the
// landing is owed a deploy that was never told, nor the reverse.
//
// ONLY A LANDING IN THE DAEMON'S OWN CHECKOUT DEPLOYS. A sibling worktree of
// the same repository is excluded: it shares the common dir but is not the
// tree the running binary was built from.
//
// A HELD ROLLOUT IS READ AT THE LANDING, never cached, so the hold a
// predecessor took is the one a successor answers (rollouthold.go). A hold
// that cannot be read does not tell the deploy: a landing not deployed is
// recovered by a deploy by hand, a hand-deployed build replaced is not.
func (r *run) landingDeploy(ctx context.Context, out outcome) LandingDeploy {
	if r.deploy != nil {
		return *r.deploy
	}
	decided := r.decideLandingDeploy(ctx, out)
	r.deploy = &decided
	return decided
}

func (r *run) decideLandingDeploy(ctx context.Context, out outcome) LandingDeploy {
	const op = "daemon.merge.self_reload"
	if out.failed != "" || out.landed == "" || !r.selfCheckout || len(out.commits) == 0 {
		return LandingNotDeployed
	}
	hold, err := r.o.deps.DB.MergeRolloutHold(ctx)
	if err != nil {
		r.o.deps.Log.Global().Info(op, "a merge landed in this daemon's own checkout, but the rollout hold could not be read; the deploy is not told",
			dlog.Context{"workspace": string(r.ws), "commit": out.landed, "commits": len(out.commits), "cause": err.Error()})
		return LandingHoldUnread
	}
	if hold.Held {
		shas := make([]string, 0, len(out.commits))
		for _, c := range out.commits {
			shas = append(shas, c.SHA)
		}
		r.o.deps.Log.Global().Info(op, "a merge landed in this daemon's own checkout while the rollouts are held; the deploy is not told",
			dlog.Context{"workspace": string(r.ws), "commit": out.landed, "commits": shas, "held_since": hold.Since})
		return LandingRolloutsHeld
	}
	return LandingDeployOwed
}

// selfReload fires the daemon's own redeploy for a landing whose decision
// (landingDeploy) is to deploy. It runs after the landing's report was owed,
// so the deploy it starts covers that report.
func (r *run) selfReload(ctx context.Context, out outcome) {
	const op = "daemon.merge.self_reload"
	if r.landingDeploy(ctx, out) != LandingDeployOwed {
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
	o.publishAbandonedWith(ctx, ws, ledger, entry, cause, o.abandonedSentence(ws, cause, false))
}

// abandonedSentence is the abandoned terminal's sentence for CAUSE: the one a
// merge that RAN reads, or the one a merge that never did.
//
// AN UNDECLARED CAUSE IS A DEFECT, never a silent empty line: the bubble still
// ends, and the raw cause rides the sentence so the reader is not left with
// nothing while the ERROR names the gap.
func (o *orchestrator) abandonedSentence(ws ids.WorkspaceID, cause AbandonCause, ran bool) string {
	summary, declared := cause.summary()
	fallback := "this merge left the queue before it ran (%s)"
	if ran {
		summary, declared = cause.summaryRunning()
		fallback = "this merge was abandoned while it was running (%s)"
	}
	if declared {
		return summary
	}
	o.deps.Log.Global().Error("daemon.merge.abandoned", "a merge was abandoned under a cause with no declared summary",
		dlog.Context{"workspace": string(ws), "cause": string(cause)})
	return fmt.Sprintf(fallback, cause)
}

// publishAbandonedWith ends a merge with no run in this process: its bubble's
// abandoned terminal reading SUMMARY, its banner and its hand-back, then its
// surfaces. publishAbandoned is the merge that never ran; a merge a restart
// took back and that left before it resumed reads the running sentence.
func (o *orchestrator) publishAbandonedWith(ctx context.Context, ws ids.WorkspaceID, ledger ids.LeaseID, entry wsm.MergeQueueEntry, cause AbandonCause, summary string) {
	const op = "daemon.merge.abandoned"
	// THE ABANDONMENT IS AN INFO RECORD, keyed by the cause: which of the four
	// ends a merge had is exactly what a reader of the run log comes for, and
	// the cause key is what makes them countable.
	o.deps.Log.Global().Info(op, "a merge left the queue without running",
		dlog.Context{"workspace": string(ws), "cause": string(cause), "summary": summary})
	if ledger != "" {
		// THE HEAD'S CLOCK RUNS FROM WHEN THE MERGE WAS QUEUED, as every drawing
		// of a merge still in line (headClockMS): it never ran, so the queued
		// span is all it has. A queue entry with no queued time is a defect.
		queued, err := enqueuedMS(entry)
		if err != nil {
			o.deps.Log.Global().Error(op, "an abandoned merge's queue entry carries no queued time; its bubble is not ended",
				dlog.Context{"workspace": string(ws), "error": err.Error()})
			return
		}
		label := o.abandonedLabel(ctx, ws, entry.Source)
		o.publishHead(ws, ledger, label, queued,
			&frontendv1.FeedMergeError{
				EndedAtMs: o.nowMS(),
				Reason: &frontendv1.FeedMergeError_Abandoned{
					Abandoned: &frontendv1.FeedMergeAbandoned{Summary: summary},
				},
			})
		o.closeReleasedBubble(ctx, ws, ledger, cause)
	}
	out := DetachedOutcome{Kind: DetachedAbandoned, Summary: summary, Requested: cause.releases()}
	if ledger != "" {
		o.ended(ctx, ws, out)
	} else {
		// A MERGE NEVER PUT IN LINE never raised its queued banner, so it
		// leaves no resolved one either: it was never in the queue.
		o.handBack(ctx, ws, out)
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

// OnInterrupt raises the merge-queue question in WS's PINNED bubble, answering
// whether it did. The interrupt changes nothing on the queue: the question's
// answer (AnswerQueueOffer) alone acts.
//
// The question needs a bubble to stand in, so it is raised only for a SYNC
// merge whose live head is drawn, which is the pinned one. A workspace with no
// queued merge, an async merge (whose bubble is placed in its requester's feed),
// or a merge only requested (no bubble yet) raises nothing.
func (o *orchestrator) OnInterrupt(ctx context.Context, ws ids.WorkspaceID) bool {
	const op = "daemon.merge.interrupt"
	log := o.log(ctx, ws)
	if _, err := o.queueOf(ctx, ws); err != nil {
		// A REFUSAL IS THE ORDINARY ANSWER: this workspace has no queued
		// merge. Anything else is a store failure and is surfaced.
		if _, refused := Refused(err); !refused {
			log.Error(op, "could not tell whether the interrupted workspace has a queued merge",
				dlog.Context{"workspace": string(ws), "error": err.Error()})
		}
		return false
	}
	if o.bubbleOf(ws).async() {
		log.Debug(op, "the interrupted workspace's merge is async; its bubble is not pinned, so no question is raised",
			dlog.Context{"workspace": string(ws)})
		return false
	}
	o.mu.Lock()
	head, drawn := o.heads[ws]
	live := drawn && head.result == nil
	if live {
		o.offers[ws] = true
	}
	o.mu.Unlock()
	if !live {
		log.Debug(op, "the interrupted workspace's merge has no live bubble yet; no question is raised",
			dlog.Context{"workspace": string(ws), "drawn": drawn})
		return false
	}
	o.restateHead(ws)
	log.Info(op, "raised the merge-queue question in the pinned merge bubble", dlog.Context{"workspace": string(ws)})
	return true
}

// AnswerQueueOffer answers the merge-queue question: evict the merge, move it
// to the front of the waiting merges, or leave the queue as it is. Every answer
// retires the question.
func (o *orchestrator) AnswerQueueOffer(ctx context.Context, ws ids.WorkspaceID, answer QueueOfferAnswer) error {
	const op = "daemon.merge.answer_queue_offer"
	log := o.log(ctx, ws)
	o.mu.Lock()
	standing := o.offers[ws]
	o.mu.Unlock()
	if !standing {
		return refuse(ArmNoOfferStanding, ws, "no merge-queue question is standing for this workspace")
	}
	fields := dlog.Context{"workspace": string(ws), "answer": answer.String()}
	switch answer {
	case QueueOfferCancel:
		log.Debug(op, "left the queue as it is on the user's answer", fields)
	case QueueOfferEvict:
		// AN ANSWERED QUESTION IS AN ORDINARY OUTCOME. The work actually
		// abandoned is recorded by the release path's own records.
		log.Debug(op, "evicting the merge on the user's answer", fields)
		if err := o.dequeue(ctx, ws, CauseUserDequeue); err != nil {
			fields["error"] = err.Error()
			log.Error(op, "could not evict the merge on the user's answer", fields)
			return err
		}
	case QueueOfferPrioritize:
		// A store failure is recorded once, by the reorder itself.
		if err := o.prioritize(ctx, ws); err != nil {
			return err
		}
	default:
		panic(fmt.Sprintf("merge: undeclared queue offer answer %d", int(answer)))
	}
	o.clearOffer(ws)
	return nil
}

// prioritize moves WS's waiting merge to the front of the merges waiting in
// its queue, and redraws every bubble on that queue when the order changed.
func (o *orchestrator) prioritize(ctx context.Context, ws ids.WorkspaceID) error {
	_, err := o.reorder(ctx, ws, reordering{
		op:      "daemon.merge.prioritize",
		failed:  "could not move a merge to the front of the merges waiting in its queue",
		unmoved: "a merge asked to run next is running or already first in line, and stays where it is",
		moved:   "moved a merge to the front of the merges waiting in its queue",
		do: func(repo wsm.RepoKey) (bool, error) {
			return o.deps.DB.PrioritizeMerge(ctx, repo, ws)
		},
	})
	return err
}

// clearOffer retires the merge-queue question, whether it was answered or
// ended by the merge leaving the queue, and redraws the live head without it.
// A head already settled carries no question, so it is not redrawn.
func (o *orchestrator) clearOffer(ws ids.WorkspaceID) {
	o.mu.Lock()
	had := o.offers[ws]
	delete(o.offers, ws)
	head, drawn := o.heads[ws]
	o.mu.Unlock()
	if had && drawn && head.result == nil {
		o.restateHead(ws)
	}
}

// queueOffer is the question an interrupt raises in a pinned bubble.
func queueOffer() *frontendv1.FeedMergeQueueOffer {
	return &frontendv1.FeedMergeQueueOffer{
		Headline: &frontendv1.FeedMergeQueueOfferHeadline{
			Text: "this merge is in the queue: evict it, run it next, or leave it be?",
		},
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
	summary := r.o.abandonedSentence(r.ws, cause, true)
	r.closeOpenRounds(ctx, "abandoned")
	r.o.log(ctx, r.ws).Info(op, "abandoned a running merge", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "cause": string(cause), "summary": summary})
	r.teardown(ctx, outcome{abandoned: cause}, func() {
		r.head(&frontendv1.FeedMergeError{
			EndedAtMs: r.o.nowMS(),
			Reason:    &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{Summary: summary}},
		})
		r.o.closeReleasedBubble(ctx, r.ws, r.lease.ID, cause)
		r.o.ended(ctx, r.ws, DetachedOutcome{Kind: DetachedAbandoned, Summary: summary, Requested: cause.releases()})
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

// closeReleasedBubble closes the bubble of a merge the user or the operator
// RELEASED from the queue (owner ruling, 2026-10-12), exactly as the reader's
// own close does: the reader's fold, recorded on the durable head, so it holds
// across every push, reload and restart and the reader can open it again. The
// head was just ended (abandoned), which also settled it out of the pinned
// spot a live sync head takes, into its place as an ended row. A merge given
// up under something else ending (its workspace closed, the daemon shut down)
// keeps its bubble open, as every non-success end does.
func (o *orchestrator) closeReleasedBubble(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID, cause AbandonCause) {
	const op = "daemon.merge.abandoned"
	if !cause.releases() {
		return
	}
	at := o.bubbleOf(ws)
	if !o.deps.Feed.SetMergeFold(at.ws, feedid.Encode(headRef(at.ws, lease)), true) {
		o.log(ctx, ws).Error(op, "a released merge's bubble could not be closed: its head is not in the feed", dlog.Context{
			"workspace": string(ws), "bubble_workspace": string(at.ws), "lease": string(lease), "cause": string(cause)})
		return
	}
	o.log(ctx, ws).Info(op, "closed the released merge's bubble", dlog.Context{
		"workspace": string(ws), "bubble_workspace": string(at.ws), "lease": string(lease), "cause": string(cause)})
}

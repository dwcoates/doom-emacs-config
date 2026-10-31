package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// This file is the boot recovery.
//
// A MERGE IS NEVER LEFT STUCK, AND ALWAYS RESUMES WHERE IT LEFT OFF. Every
// workspace whose merge lease survived a restart is RESUMED from the step its
// progress record names (progress.go, resume.go), under that same lease and in
// the same bubble; a merge whose record is gone never took a step, and runs
// again from the queue. Only a resume that finds the tree contradicting its
// record fails, naming the contradiction; nothing about the target's working
// tree that the merge did not write is ever read as evidence. A lease nobody
// holds and nobody releases would refuse that workspace's every prompt
// forever, so there is no outcome that leaves one.
//
// Everything that was merely QUEUED comes back in the order it was waiting in,
// because the queue is durable and the order is the whole point of a queue.

// Recover puts back the turns a merge displaced and never resubmitted, then
// resumes or loudly fails every in-flight merge and re-enqueues the waiting
// ones.
func (o *orchestrator) Recover(ctx context.Context) error {
	const op = "daemon.merge.recover"
	// THE DISPLACED TURNS GO FIRST, before a recovered merge can be admitted
	// again: the sweep is then reading a settled set of marks rather than
	// racing a fresh run's own capture.
	if err := o.recoverDisplaced(ctx); err != nil {
		return err
	}
	if err := o.recordInherited(ctx, o.racingSwitch()); err != nil {
		return err
	}
	registered, err := o.registeredQueues(ctx, op)
	if err != nil {
		return err
	}
	if err := o.recordInheritedHolds(ctx, registered); err != nil {
		return err
	}
	o.sweepOrphanedState(ctx)
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		return err
	}
	repos := make([]wsm.RepoKey, 0, len(queues))
	total := 0
	for repo := range queues {
		repos = append(repos, repo)
		total += len(queues[repo])
	}
	sort.Slice(repos, func(i, j int) bool { return repos[i] < repos[j] })
	// THE RUN LOG CARRIES THE RECOVERY ITSELF. Each merge's own fate is a
	// workspace-scoped record, but "this boot reconciled the merge queues" is a
	// fact about the restart, and the durable run log is where each daemon
	// process's boot sequence is read.
	o.deps.Log.Global().Info(op, "recovering the merge queues", dlog.Context{
		"repositories": len(repos), "entries": total})

	for _, repo := range repos {
		for _, entry := range queues[repo] {
			if err := o.recoverEntry(ctx, repo, entry); err != nil {
				return err
			}
		}
	}
	for _, repo := range repos {
		if err := o.republishQueue(ctx, repo); err != nil {
			return err
		}
		o.kick(repo)
	}
	return o.offerOwedDrains(ctx, op, repos)
}

// recoverEntry takes one durable queue entry back into this daemon: a request
// still waiting for its turn's end is re-armed, a waiting merge is put back
// in line, and an admitted one resumes (recoverAdmitted). It is the boot's
// per-entry recovery and a handover adoption's alike.
func (o *orchestrator) recoverEntry(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) error {
	const op = "daemon.merge.recover"
	switch entry.State {
	case wsm.MergeRequested:
		o.mu.Lock()
		o.repoOf[entry.Workspace] = repo
		o.mu.Unlock()
		// A MERGE WORKSPACE'S PICK is begun, or taken back where the restart
		// left it, before its merge joins the queue. A row that still carries
		// its detach was never handed off (HandOffDetachedMerge rewrites the
		// row onto the merge workspace without one): it names the requester,
		// whose pick it is not, so it is re-armed below like any request, and
		// its wait's end makes the hand-off that begins the pick.
		if entry.Source.Pick != "" && entry.Source.Detach == "" {
			if entry.Source.PickStartedAt == nil {
				o.deps.Log.Global().Info(op, "begins an async merge's pick the restart cut short before it began", dlog.Context{
					"workspace": string(entry.Workspace), "repo": string(repo)})
				return o.startPick(ctx, entry.Workspace, repo, entry)
			}
			return o.resumePick(ctx, repo, entry)
		}
		// A REQUEST STILL WAITING FOR ITS TURN'S END is re-armed: it is put in
		// line once the requester's turn in flight -- if one survived the
		// restart -- has ended.
		o.deps.Log.Global().Info(op, "re-armed a merge request still waiting for its turn to end", dlog.Context{
			"workspace": string(entry.Workspace), "repo": string(repo)})
		o.awaitRequestingTurn(entry.Workspace, repo)
		return nil
	case wsm.MergeAdmitted:
		return o.recoverAdmitted(ctx, repo, entry)
	case wsm.MergeRemediating:
		return o.recoverRemediating(ctx, repo, entry)
	}
	requeued, err := o.recoverWaiting(ctx, repo, entry)
	if err != nil || !requeued {
		return err
	}
	// A PICK THE RESTART CUT SHORT AT THE HEAD is taken back where it stood;
	// one that never began waits for its admission to begin it (admitFront).
	if entry.Source.Pick != "" && entry.Source.PickStartedAt != nil {
		o.deps.Log.Global().Info(op, "resumes the pick a restart cut short at the head of the queue", dlog.Context{
			"workspace": string(entry.Workspace), "repo": string(repo)})
		return o.resumePick(ctx, repo, entry)
	}
	o.deps.Log.Global().Debug(op, "re-enqueued a waiting merge", dlog.Context{
		"workspace": string(entry.Workspace), "repo": string(repo), "position": entry.Position})
	return nil
}

// recoverWaiting puts one merely-WAITING merge back on its queue, or ABANDONS
// it when it cannot go back.
//
// The queue is durable, so the daemon's own shutdown does not end a waiting
// merge — the restart is where a merge that shut down queued either resumes its
// wait or gives up, and the give-up is what the DAEMON SHUTDOWN cause names. A
// merge whose workspace no longer records the geometry the merge would run
// against cannot be re-queued: the workspace was nuked, or its creation job is
// gone, and admitting it later would only fail at the front of the queue with
// nothing said about why it was ever there.
//
// A ROW THAT WILL NOT DECODE STILL REFUSES THE BOOT, exactly as an admitted
// merge's does: half-written state is evidence, not an outcome.
func (o *orchestrator) recoverWaiting(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) (bool, error) {
	const op = "daemon.merge.recover"
	ws := entry.Workspace
	_, jobErr := o.targetDirOf(ctx, ws, entry.Source)
	var decodeErr *wsm.DecodeError
	if errors.As(jobErr, &decodeErr) {
		fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "error": decodeErr.Error()}
		o.log(ctx, ws).Error(op, "refusing the boot: a queued merge's creation_jobs row will not decode", fields)
		o.deps.Log.Global().Error(op, "refusing the boot: a queued merge's creation_jobs row will not decode", fields)
		return false, fmt.Errorf("merge: recover %q: %w", ws, decodeErr)
	}
	if jobErr != nil {
		o.deps.Log.Global().Warn(op, "abandoning a merge the restart could not put back on its queue", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "error": jobErr.Error()})
		// THE BUBBLE THE MERGE WAITED IN ENDS: its identity is on its row
		// (an earlier build's row carries none, and is given one), so the
		// abandoned terminal lands where the reader saw the merge wait.
		ledger := o.ledgerOfEntry(entry)
		o.mu.Lock()
		delete(o.repoOf, ws)
		delete(o.ledgerOf, ws)
		o.mu.Unlock()
		if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, string(CauseDaemonShutdown)); err != nil {
			return false, err
		}
		o.publishAbandoned(ctx, ws, ledger, entry, CauseDaemonShutdown)
		return false, nil
	}
	o.mu.Lock()
	o.repoOf[ws] = repo
	o.mu.Unlock()
	// THE SAME BUBBLE: the identity the merge was put in line with is on its
	// row. Recover (or an adoption) republishes the queue once every entry is
	// back, and redraws it there.
	o.ledgerOfEntry(entry)
	return true, nil
}

// ledgerOfEntry stands a queued merge's bubble identity as the one its row
// carries -- the bubble it was drawn in before a restart or a handover -- and
// mints one only for a row an earlier build queued without it.
func (o *orchestrator) ledgerOfEntry(entry wsm.MergeQueueEntry) ids.LeaseID {
	if entry.Ledger == "" {
		o.deps.Log.Global().Info("daemon.merge.recover", "a queued merge an earlier build recorded carries no bubble identity; it is drawn in a fresh bubble",
			dlog.Context{"workspace": string(entry.Workspace), "repo": string(entry.Repo)})
		return o.mintLedger(entry.Workspace)
	}
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ledgerOf[entry.Workspace] = entry.Ledger
	return entry.Ledger
}

// recoverAdmitted decides one in-flight merge's fate.
//
// A MERGE WITH A PROGRESS RECORD RESUMES (owner ruling, 2026-10-06). The
// record says which step the dead run stood on; the resumed run goes on from
// it under the same lease, in the same bubble, reading from git only what that
// step can have left. Nothing here inspects the trees: what is in a working
// tree that the merge never wrote -- an untracked file, the owner's own edits
// -- is no evidence about the merge.
//
// AN ADMITTED MERGE WITH NO RECORD never took a step: the first record is
// written at admission, before any step acts. It runs again from the queue,
// keeping its bubble when its lease still stands.
func (o *orchestrator) recoverAdmitted(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) error {
	const op = "daemon.merge.recover"
	ws := entry.Workspace
	log := o.log(ctx, ws)
	lease, held, err := o.deps.DB.Lease(ctx, ws)
	if err != nil {
		return err
	}
	doc, recorded, err := o.loadProgress(ctx, ws, lease, held)
	if err != nil {
		return err
	}
	if recorded {
		return o.recoverResumable(ctx, repo, lease, doc, false)
	}
	target, jobErr := o.targetDirOf(ctx, ws, entry.Source)
	// CORRUPTION REFUSES THE LOAD, it is never an unmergeable outcome. A
	// creation_jobs row that will not decode is a half-written record, not a
	// workspace whose geometry was legitimately removed: dropping the merge and
	// serving on would turn state corruption into an ordinary business answer
	// and lose the evidence. The boot fails instead, loudly, naming the row.
	var decodeErr *wsm.DecodeError
	if errors.As(jobErr, &decodeErr) {
		fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "error": decodeErr.Error()}
		log.Error(op, "refusing the boot: a merge's creation_jobs row will not decode", fields)
		// AND IN THE RUN LOG: a boot that refuses is a fact about the restart,
		// and the durable run log is where the boot sequence is read.
		o.deps.Log.Global().Error(op, "refusing the boot: a merge's creation_jobs row will not decode", fields)
		return fmt.Errorf("merge: recover %q: %w", ws, decodeErr)
	}
	if held {
		// THE QUEUE'S TREES OF THE DEAD RUN GO FIRST. They hold nothing the
		// target depends on -- the target never moved for them -- and the
		// next run makes trees of its own under its own lease.
		repoDir := string(repo)
		if jobErr == nil {
			repoDir = target
		}
		o.sweepTrees(ctx, ws, repoDir, lease.ID, false)
		if err := o.deps.DB.ReleaseLease(ctx, lease.ID); err != nil {
			log.Error(op, "could not release a recovered merge's lease", dlog.Context{
				"workspace": string(ws), "lease": string(lease.ID), "error": err.Error()})
			return err
		}
		o.deps.Queue.OnLeaseChanged(ws)
	}
	if jobErr != nil {
		why := fmt.Sprintf("the workspace's merge geometry is gone: %v", jobErr)
		log.Error(op, "failing a merge a restart left unfinished", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "reason": why})
		if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, causeRestartFailed); err != nil {
			return err
		}
		o.mu.Lock()
		delete(o.repoOf, ws)
		delete(o.ledgerOf, ws)
		o.mu.Unlock()
		summary := fmt.Sprintf("the merge did not survive a daemon restart: %s", why)
		if held {
			// The bubble that was live gets its terminal, so the trace of the
			// interrupted merge ends where a reader can see it rather than simply
			// stopping mid-tab.
			o.publishHead(ws, lease.ID, o.abandonedLabel(ctx, ws, entry.Source), o.nowMS(),
				&frontendv1.FeedMergeError{
					EndedAtMs: o.nowMS(),
					Reason:    &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: summary}},
				}, headRecover)
		}
		o.publish(repo, ws, MergeFacts{State: StateFailed, FailedArea: footer.FailedOther, Detail: summary})
		o.ended(ctx, ws, DetachedOutcome{Kind: DetachedFailed, Summary: summary})
		return nil
	}
	log.Info(op, "an admitted merge left no progress record, so it never took a step; it runs again from the queue", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "lease_held": held})
	o.mu.Lock()
	o.repoOf[ws] = repo
	if held {
		// THE SAME BUBBLE: the merge's queued bubble was drawn under this
		// identity, and the admission takes its lease under it again.
		o.ledgerOf[ws] = lease.ID
	}
	o.mu.Unlock()
	// THE ENTRY GOES BACK IN LINE AT ITS OWN PLACE, with the source it was
	// asked with, so the ordinary admission path runs it again under a lease
	// of its own. A remove and re-request once sent the head to the back
	// (2026-10-24: deploy-stage-concurrency went from the head to 19th).
	ledger := o.mintLedger(ws)
	if !held && entry.Ledger != "" {
		ledger = o.ledgerOfEntry(entry)
	}
	return o.deps.DB.ReturnAdmittedMerge(ctx, repo, ws, ledger)
}

// loadProgress reads an admitted merge's progress record, and answers it when
// it is the record of the lease still held. A record of a lease that is gone
// is the trace of a run that ended between its lease's release and its
// record's drop: nothing is left to resume, so it is dropped. A record that
// will not decode refuses the boot.
func (o *orchestrator) loadProgress(ctx context.Context, ws ids.WorkspaceID, lease wsm.Lease, held bool) (progressDoc, bool, error) {
	const op = "daemon.merge.recover"
	stored, found, err := o.deps.DB.MergeProgressOf(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(op, "the merge's progress record could not be read", dlog.Context{
			"workspace": string(ws), "error": err.Error()})
		return progressDoc{}, false, fmt.Errorf("merge: read the progress of %q: %w", ws, err)
	}
	if !found {
		return progressDoc{}, false, nil
	}
	if !held || stored.Lease != lease.ID {
		o.deps.Log.Global().Info(op, "dropped the progress record of a merge whose lease is gone", dlog.Context{
			"workspace": string(ws), "recorded_lease": string(stored.Lease), "lease_held": held, "held_lease": string(lease.ID)})
		if _, err := o.deps.DB.DropMergeProgress(ctx, stored.Lease); err != nil {
			return progressDoc{}, false, fmt.Errorf("merge: drop the stale progress of %q: %w", ws, err)
		}
		return progressDoc{}, false, nil
	}
	doc, err := decodeProgress(stored)
	if err != nil {
		fields := dlog.Context{"workspace": string(ws), "lease": string(stored.Lease), "error": err.Error()}
		o.log(ctx, ws).Error(op, "refusing the boot: a merge's progress record will not decode", fields)
		o.deps.Log.Global().Error(op, "refusing the boot: a merge's progress record will not decode", fields)
		return progressDoc{}, false, fmt.Errorf("merge: recover %q: %w", ws, err)
	}
	return doc, true, nil
}

// recoverResumable takes a merge with a progress record back: its lease
// adopted by this process, its bubble and its footer drawn as they stood, and
// the run itself handed to the admission pump, which resumes it first.
func (o *orchestrator) recoverResumable(ctx context.Context, repo wsm.RepoKey, lease wsm.Lease, doc progressDoc, remediating bool) error {
	const op = "daemon.merge.recover"
	ws := lease.Workspace
	// A PAUSED QUEUE RESUMES NOTHING (pumpOnce), so the record below says the
	// merge stays suspended rather than that it resumes: the pause's own
	// take-back (suspendForPause) and a boot under a pause both land here. It
	// is read before anything is taken, so a read that fails takes nothing.
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read whether the queue of a merge taken back is paused", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "lease": string(lease.ID), "error": err.Error()})
		return fmt.Errorf("merge: read whether the queue of %q is paused: %w", ws, err)
	}
	if _, err := o.deps.DB.AdoptMergeLease(ctx, ws, lease.ID); err != nil {
		o.deps.Log.Global().Error(op, "the merge's lease could not be adopted for its resume", dlog.Context{
			"workspace": string(ws), "lease": string(lease.ID), "error": err.Error()})
		return fmt.Errorf("merge: adopt the lease of %q: %w", ws, err)
	}
	// THE SCRATCH TREES OF THE DEAD RUN GO; the branch's worktree stays. A
	// scratch tree holds only a merge commit the resume makes again (or
	// already fast-forwarded to, which the commit object outlives), while the
	// branch's worktree is where its rebase stands.
	o.sweepTrees(ctx, ws, doc.Subject.TargetDir, lease.ID, true)
	o.mu.Lock()
	o.repoOf[ws] = repo
	o.ledgerOf[ws] = lease.ID
	o.resumes[ws] = doc
	o.mu.Unlock()
	// THE SAME BUBBLE, AT ONCE: its head is redrawn live under the identity it
	// always had, and the footer shows the step the merge stands on before the
	// resumed run's first word.
	label := branchLabel(doc.Subject.Branch, doc.Subject.subject().targetLabel())
	o.publishHead(ws, lease.ID, label, doc.StartedMS, headMerging, headRecover)
	o.publish(repo, ws, doc.footerFacts(o.deps.Now()))
	// THE RUN LOG CARRIES IT TOO: a merge resumed is a fact about the
	// restart, and the durable run log is where a boot sequence is read.
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "lease": string(lease.ID), "step": doc.Step,
		"round": doc.Active.N, "turn": doc.Turn, "paused": paused}
	message := "a merge a restart interrupted resumes at the step it recorded"
	switch {
	case remediating:
		message = "a merge remediating its failing tests out of line is taken back; it returns to the line once its fixing turn ends"
	case paused:
		message = "a suspended merge is taken back at the step it recorded; its queue is paused, so it stays suspended until the queue is resumed"
	}
	o.log(ctx, ws).Info(op, message, fields)
	o.deps.Log.Global().Info(op, message, fields)
	return nil
}

// recoverRemediating takes back a merge remediating out of line (remediate.go):
// its record is taken as any resume's is, held out of the resumes until it
// returns to the line, and the wait on its fixing turn is armed again. A
// remediating merge with no progress record under its lease is a defect and
// refuses the take-back, loudly.
func (o *orchestrator) recoverRemediating(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) error {
	ws := entry.Workspace
	lease, held, err := o.deps.DB.Lease(ctx, ws)
	if err != nil {
		return err
	}
	doc, recorded, err := o.loadProgress(ctx, ws, lease, held)
	if err != nil {
		return err
	}
	if !recorded {
		o.log(ctx, ws).Error(opRemediate, "a merge remediating out of line has no progress record under its lease; it is not taken back",
			dlog.Context{"workspace": string(ws), "repo": string(repo), "lease_held": held})
		return fmt.Errorf("merge: the remediating merge of %q has no progress record", ws)
	}
	o.mu.Lock()
	o.remediating[ws] = &remediation{repo: repo, cancel: func() {}, since: o.deps.Now()}
	o.mu.Unlock()
	if err := o.recoverResumable(ctx, repo, lease, doc, true); err != nil {
		o.mu.Lock()
		delete(o.remediating, ws)
		o.mu.Unlock()
		return err
	}
	o.awaitRemediation(ws, repo, ids.TurnID(doc.Turn))
	return nil
}

// resumedMergeOwns reports whether a displaced turn belongs to a merge this
// boot resumes: one whose progress record, under the lease still held, names
// that turn.
func (o *orchestrator) resumedMergeOwns(ctx context.Context, t wsm.Turn) (bool, error) {
	lease, held, err := o.deps.DB.Lease(ctx, t.Workspace)
	if err != nil {
		return false, fmt.Errorf("merge: read the lease of %q: %w", t.Workspace, err)
	}
	doc, recorded, err := o.loadProgress(ctx, t.Workspace, lease, held)
	if err != nil || !recorded {
		return false, err
	}
	return doc.Displaced != nil && doc.Displaced.Turn == string(t.ID), nil
}

// sweepTrees removes every scratch tree -- and, unless keepBranch, the branch
// worktree -- a dead run of one lease left under the state root. A tree that
// will not go is recorded and left: it is the queue's own and blocks nothing.
func (o *orchestrator) sweepTrees(ctx context.Context, ws wsm.WorkspaceID, repoDir string, lease wsm.LeaseID, keepBranch bool) {
	const op = "daemon.merge.recover"
	pattern := filepath.Join(o.deps.StateDir, mergeTreesDir, string(lease)+"-*")
	trees, err := filepath.Glob(pattern)
	// THE WORKTREE A DEAD RUN MADE FOR A BRANCH goes too: the merge runs again
	// under a lease of its own and finds the branch free to check out.
	if made := worktreeFor(o.deps.StateDir, lease); err == nil && !keepBranch {
		if _, statErr := os.Stat(made); statErr == nil {
			trees = append(trees, made)
		}
	}
	if err != nil {
		o.deps.Log.Global().Error(op, "could not list a dead merge's queue trees", dlog.Context{
			"workspace": string(ws), "lease": string(lease), "pattern": pattern, "error": err.Error()})
		return
	}
	for _, tree := range trees {
		if err := o.deps.Git.RemoveWorktree(ctx, repoDir, tree); err != nil {
			o.deps.Log.Global().Error(op, "could not remove a dead merge's queue tree", dlog.Context{
				"workspace": string(ws), "lease": string(lease), "tree": tree, "error": err.Error()})
			continue
		}
		o.deps.Log.Global().Info(op, "removed a queue tree a dead merge left", dlog.Context{
			"workspace": string(ws), "lease": string(lease), "tree": tree})
	}
}

// recoverDisplaced puts back every turn a merge took the session away from and
// never resubmitted, EXACTLY ONCE across every boot.
//
// A merge captures the user's in-flight turn durably and ends it, and puts it
// back when its lease is released. A merge that DIES between those two acts
// leaves the turn marked and nobody holding it: the interrupted merge is
// re-enqueued, but its second run captures nothing (the first run already
// ended the turn), so without this sweep the turn the user typed stays marked
// displaced forever and is never put back.
//
// ONE OWNER PER RECORD, AND THE DATABASE PICKS IT. Both owners — the merge's
// own release and this sweep — take a record through ClaimDisplacedTurn, whose
// `WHERE displaced = 1` lets exactly one of them win however the two are
// scheduled; the loser is told false and puts nothing back. The claim is taken
// BEFORE the resubmission, so no second boot can double it either.
//
// A merge the recovery RE-ENQUEUES is not an owner of the old record: its
// second run captures nothing (the first run already ended the turn), which is
// precisely the gap this sweep closes. Such a run may displace the resubmitted
// turn all over again and put THAT one back at its own release — one
// displacement, one resubmission, which is the contract.
//
// A submission that fails is recorded and the sweep goes on: one workspace
// whose session refuses a turn is not a reason to leave every other user's
// displaced turn unrecovered. A DURABLE READ OR CLAIM that fails fails the
// boot, as every other recovery step's does.
func (o *orchestrator) recoverDisplaced(ctx context.Context) error {
	const op = "daemon.merge.recover_displaced"
	displaced, err := o.deps.DB.AllDisplacedTurns(ctx)
	if err != nil {
		o.deps.Log.Global().Error(op, "the displaced turns could not be read", dlog.Context{"error": err.Error()})
		return fmt.Errorf("merge: read the displaced turns: %w", err)
	}
	if len(displaced) == 0 {
		return nil
	}
	o.deps.Log.Global().Info(op, "resubmitting the turns a merge displaced and never put back", dlog.Context{
		"turns": len(displaced)})
	for _, t := range displaced {
		fields := dlog.Context{"workspace": string(t.Workspace), "turn": string(t.ID)}
		// A TURN DISPLACED BY A MERGE THAT RESUMES IS THAT MERGE'S TO PUT BACK,
		// at its own end, exactly as if the restart had never happened.
		if owned, err := o.resumedMergeOwns(ctx, t); err != nil {
			return err
		} else if owned {
			o.deps.Log.Global().Debug(op, "a displaced turn waits for the resumed merge that displaced it", fields)
			continue
		}
		claimed, err := o.deps.Queue.ClaimDisplacedTurn(ctx, t.Workspace, t.ID)
		if err != nil {
			o.deps.Log.Global().Error(op, "a displaced turn could not be claimed", withField(fields, "error", err.Error()))
			return fmt.Errorf("merge: claim the displaced turn %q: %w", t.ID, err)
		}
		if !claimed {
			o.deps.Log.Global().Debug(op, "a displaced turn was already put back by its merge", fields)
			continue
		}
		if _, err := o.deps.Queue.Submit(ctx, promptqueue.Submission{
			WS: t.Workspace, Turn: wsm.NewTurnID(), Said: saidText(t.Text),
			Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
		}); err != nil {
			o.log(ctx, t.Workspace).Error(op, "could not resubmit a turn a merge displaced", withField(fields, "error", err.Error()))
			o.deps.Log.Global().Error(op, "could not resubmit a turn a merge displaced", withField(fields, "error", err.Error()))
			continue
		}
		o.log(ctx, t.Workspace).Info(op, "resubmitted a turn a merge displaced and never put back", fields)
		o.deps.Log.Global().Info(op, "resubmitted a turn a merge displaced and never put back", fields)
	}
	return nil
}

// sweepOrphanedState drops what merges taken off their queues left behind (a
// progress record, open ledger rounds; wsm.SweepOrphanedMergeState) before the
// queues are recovered. A sweep that fails is recorded loudly and recovery
// goes on: the leftovers are stale bookkeeping, never a reason to leave a
// queue unrecovered.
func (o *orchestrator) sweepOrphanedState(ctx context.Context) {
	const op = "daemon.merge.recover"
	swept, err := o.deps.DB.SweepOrphanedMergeState(ctx, o.deps.Now())
	if err != nil {
		o.deps.Log.Global().Error(op, "could not sweep the merge state no queue entry names any more", dlog.Context{"error": err.Error()})
		return
	}
	// EACH MERGE LEASE RELEASED IS ITS OWN RECORD, with its workspace: a lease
	// over no queue entry refused every prompt the workspace was sent, and
	// the record is what says the boot healed it.
	for _, l := range swept.ReleasedLeases {
		fields := dlog.Context{"workspace": string(l.Workspace), "lease": string(l.ID), "acquired_at": l.AcquiredAt.UTC().Format(time.RFC3339Nano)}
		o.log(ctx, l.Workspace).Info(op, "released a merge lease no queue entry names any more; the workspace takes prompts again", fields)
		o.deps.Log.Global().Info(op, "released a merge lease no queue entry names any more; the workspace takes prompts again", fields)
		o.deps.Queue.OnLeaseChanged(l.Workspace)
	}
	if swept.ProgressRecords+swept.RoundsClosed+len(swept.ReleasedLeases) > 0 {
		o.deps.Log.Global().Info(op, "swept the merge state no queue entry names any more", dlog.Context{
			"progress_records": swept.ProgressRecords, "rounds_closed": swept.RoundsClosed, "leases_released": len(swept.ReleasedLeases)})
	}
}

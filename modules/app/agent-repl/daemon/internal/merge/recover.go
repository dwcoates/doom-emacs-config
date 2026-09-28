package merge

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/wsm"
)

// This file is the boot recovery.
//
// A MERGE IS NEVER LEFT STUCK. Every workspace whose merge lease survived a
// restart is either RESUMED from the tab the ledger last recorded — when the
// git state still allows it — or LOUDLY FAILED with its lease released. There
// is no third outcome: a lease nobody holds and nobody releases would refuse
// that workspace's every prompt forever, and the user would have nothing to
// read about why.
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
			if entry.State != wsm.MergeAdmitted {
				requeued, err := o.recoverWaiting(ctx, repo, entry, len(queues[repo]))
				if err != nil {
					return err
				}
				if !requeued {
					continue
				}
				o.deps.Log.Global().Debug(op, "re-enqueued a waiting merge", dlog.Context{
					"workspace": string(entry.Workspace), "repo": string(repo), "position": entry.Position})
				continue
			}
			if err := o.recoverAdmitted(ctx, repo, entry); err != nil {
				return err
			}
		}
	}
	for _, repo := range repos {
		o.kick(repo)
	}
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
func (o *orchestrator) recoverWaiting(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry, depth int) (bool, error) {
	const op = "daemon.merge.recover"
	ws := entry.Workspace
	_, jobErr := o.layoutFor(ctx, ws)
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
		// THE LEDGER IDENTITY IS MINTED HERE. A merely-queued merge's bubble
		// is addressed by an identity minted in memory at enqueue and never
		// written down, so the pre-restart bubble is unreachable; without a
		// fresh one the abandoned terminal would have nowhere to land and the
		// cause would reach nobody.
		ledger := o.mintLedger(ws)
		o.mu.Lock()
		delete(o.repoOf, ws)
		delete(o.ledgerOf, ws)
		o.mu.Unlock()
		if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, string(CauseDaemonShutdown)); err != nil {
			return false, err
		}
		o.publishAbandoned(ctx, ws, ledger, CauseDaemonShutdown)
		return false, nil
	}
	o.mu.Lock()
	o.repoOf[ws] = repo
	o.mu.Unlock()
	o.publish(ws, MergeFacts{State: StateQueued, QueuePosition: entry.Position, QueueDepth: depth})
	return true, nil
}

// recoverAdmitted decides one in-flight merge's fate. RESUMABLE means the
// target's working tree is clean: nothing of the interrupted merge is half
// applied, so the run can start again from its recorded tab. An unclean target
// is NOT resumable — the daemon does not know what the dead run had staged, and
// guessing would land a tree nobody reviewed.
func (o *orchestrator) recoverAdmitted(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) error {
	const op = "daemon.merge.recover"
	ws := entry.Workspace
	log := o.log(ctx, ws)
	lease, held, err := o.deps.DB.Lease(ctx, ws)
	if err != nil {
		return err
	}
	job, jobErr := o.layoutFor(ctx, ws)
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
	resumable := false
	var why string
	switch {
	case jobErr != nil:
		why = fmt.Sprintf("the workspace's merge geometry is gone: %v", jobErr)
	default:
		clean, err := o.deps.Git.IsClean(ctx, job.Layout.TargetDir)
		switch {
		case err != nil:
			why = fmt.Sprintf("the merge target %s could not be inspected: %v", job.Layout.TargetDir, err)
		case !clean:
			why = fmt.Sprintf("the merge target %s carries an unfinished merge this daemon did not start", job.Layout.TargetDir)
		default:
			resumable = true
		}
	}
	if held {
		// THE QUEUE'S TREES OF THE DEAD RUN GO FIRST. They hold nothing the
		// target depends on -- the target never moved for them -- and the
		// next run makes trees of its own under its own lease.
		repoDir := string(repo)
		if jobErr == nil {
			repoDir = job.Layout.TargetDir
		}
		o.sweepTrees(ctx, ws, repoDir, lease.ID)
		if err := o.deps.DB.ReleaseLease(ctx, lease.ID); err != nil {
			log.Error(op, "could not release a recovered merge's lease", dlog.Context{
				"workspace": string(ws), "lease": string(lease.ID), "error": err.Error()})
			return err
		}
		o.deps.Queue.OnLeaseChanged(ws)
	}
	if resumable {
		lastTab := o.lastTab(ctx, ws)
		log.Warn(op, "resuming a merge interrupted by a restart", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "last_tab": lastTab})
		o.mu.Lock()
		o.repoOf[ws] = repo
		o.mu.Unlock()
		o.publish(ws, MergeFacts{State: StateQueued, QueuePosition: entry.Position})
		// The entry goes back to WAITING so the ordinary admission path runs it
		// again under a fresh lease; a half-owned admission is what left the
		// lease stuck in the first place.
		if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, "restart_resume"); err != nil {
			return err
		}
		if _, err := o.deps.DB.EnqueueMerge(ctx, repo, ws, o.deps.Now()); err != nil {
			return err
		}
		return nil
	}
	log.Error(op, "failing a merge a restart left unfinished", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "reason": why})
	if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, "restart_failed"); err != nil {
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
		o.deps.Feed.UpsertSynthesized(ws, feedid.Feed{Root: true}, headRow(ws, lease.ID,
			branchLabel(job.Layout.SourceBranch, job.Layout.TargetDir), o.nowMS(),
			&frontendv1.FeedMergeError{
				EndedAtMs: o.nowMS(),
				Reason:    &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: summary}},
			}))
	}
	o.publish(ws, MergeFacts{State: StateFailed, Detail: summary})
	return nil
}

// sweepTrees removes every scratch tree a dead run of one lease left under the
// state root. A tree that will not go is recorded and left: it is the queue's
// own and blocks nothing.
func (o *orchestrator) sweepTrees(ctx context.Context, ws wsm.WorkspaceID, repoDir string, lease wsm.LeaseID) {
	const op = "daemon.merge.recover"
	pattern := filepath.Join(o.deps.StateDir, mergeTreesDir, string(lease)+"-*")
	trees, err := filepath.Glob(pattern)
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

// lastTab reports the tab a merge's ledger last opened, which is where a resumed
// run picks up. The ledger holds intervals and nothing of the content, so this
// is all a replay can reconstruct — and all it needs to.
func (o *orchestrator) lastTab(ctx context.Context, ws wsm.WorkspaceID) string {
	entries, err := o.deps.DB.MergeLedger(ctx, ws)
	if err != nil || len(entries) == 0 {
		return TabQueue
	}
	last := entries[len(entries)-1]
	if len(last.Intervals) == 0 {
		return TabQueue
	}
	return last.Intervals[len(last.Intervals)-1].Kind
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

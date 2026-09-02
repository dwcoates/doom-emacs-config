package merge

import (
	"context"
	"fmt"
	"sort"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
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

// Recover resumes or loudly fails every in-flight merge, then re-enqueues the
// waiting ones.
func (o *orchestrator) Recover(ctx context.Context) error {
	const op = "daemon.merge.recover"
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		return err
	}
	repos := make([]wsm.RepoKey, 0, len(queues))
	for repo := range queues {
		repos = append(repos, repo)
	}
	sort.Slice(repos, func(i, j int) bool { return repos[i] < repos[j] })

	for _, repo := range repos {
		for _, entry := range queues[repo] {
			if entry.State != wsm.MergeAdmitted {
				o.mu.Lock()
				o.repoOf[entry.Workspace] = repo
				o.mu.Unlock()
				o.publish(entry.Workspace, MergeFacts{
					State: StateQueued, QueuePosition: entry.Position, QueueDepth: len(queues[repo]),
				})
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

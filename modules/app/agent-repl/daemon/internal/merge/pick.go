package merge

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is an async merge's PICK (agentrepl.v1
// MergeWorkspaceSourceOwnBranchDetachedPick): the first turn of a merge
// workspace forked onto the target branch's tip, its agent cherry-picking the
// changes the requesting agent named from the requester's branch. It runs
// BEFORE the merge joins the queue, so the queue never waits on an agent
// picking, and it is the bubble's first tab (frontend.v1.FeedMergeTabPick).
//
// THE PICK IS JUDGED BY THE TREE, never by the agent's word: it succeeded when
// its turn ended unfailed with at least one commit ahead of the target and a
// clean worktree. Anything else fails the merge, which reveals the merge
// workspace so the user can finish.
//
// A RESTART REATTACHES. The pick's turn and its bubble's ledger identity are
// written on the merge workspace's request row before the turn is submitted
// (wsm.StartMergePick), so a restart redraws nothing, reattaches to that turn
// (reattachTurn), and judges it when it ends.

// BriefPick is the pick agent's brief.
const BriefPick = "merge-pick"

// pickBase answers the branch an async pick merge's workspace is forked onto:
// the requester's merge target, as its target checkout has it checked out.
func (o *orchestrator) pickBase(ctx context.Context, requester ids.WorkspaceID) (string, error) {
	job, err := o.layoutFor(ctx, requester)
	if err != nil {
		return "", err
	}
	branch, err := o.deps.Git.CurrentBranch(ctx, job.Layout.TargetDir)
	if err != nil {
		return "", fmt.Errorf("merge: reading the target branch checked out in %s: %w", job.Layout.TargetDir, err)
	}
	if branch == "" {
		return "", fmt.Errorf("merge: the target checkout %s has no branch checked out, so there is no target to pick onto", job.Layout.TargetDir)
	}
	return branch, nil
}

// pick is one merge workspace's pick, from its start to its judgement.
type pick struct {
	// ws is the merge workspace.
	ws ids.WorkspaceID
	// requester is the workspace whose branch the changes are picked from.
	requester ids.WorkspaceID
	// repo is the queue the merge joins once the pick has succeeded.
	repo wsm.RepoKey
	// ledger is the bubble's ledger identity.
	ledger ids.LeaseID
	// turn is the pick's turn.
	turn ids.TurnID
	// startedMS is when the pick began: the head's clock and the pick tab's.
	startedMS int64
	// label is the bubble head's line.
	label string
	// instruction is the requesting agent's instruction.
	instruction string
}

// startPick begins merge workspace WS's pick: it mints the bubble, records the
// pick's turn on the request row, draws the head and the live pick tab, stands
// the mirror that streams the pick turn into that tab, submits the brief, and
// waits for the turn to end on a goroutine of its own.
func (o *orchestrator) startPick(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, entry wsm.MergeQueueEntry) error {
	const op = "daemon.merge.pick"
	log := o.log(ctx, ws)
	requester, async := o.requesterOf(ws)
	if !async {
		err := fmt.Errorf("merge: a pick runs in a merge workspace, and %s is none", ws)
		log.Error(op, "a pick was asked of a workspace that is no merge workspace", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return err
	}
	p := &pick{ws: ws, requester: requester, repo: repo, ledger: o.mintLedger(ws), turn: wsm.NewTurnID(), instruction: entry.Source.Pick}
	started := o.deps.Now()
	p.startedMS = started.UnixMilli()
	if err := o.deps.DB.StartMergePick(ctx, repo, ws, p.ledger, p.turn, started); err != nil {
		log.Error(op, "could not record the pick's start; the request stays recorded for the next boot", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "error": err.Error()})
		return err
	}
	label, err := o.sourceLabel(ctx, ws, entry.Source)
	if err != nil {
		log.Error(op, "could not resolve the pick's bubble line", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return err
	}
	p.label = label
	log.Info(op, "an async merge's pick begins in its merge workspace", dlog.Context{
		"workspace": string(ws), "requester": string(requester), "repo": string(repo), "turn": string(p.turn), "ledger": string(p.ledger)})
	o.drawPickLive(p)
	o.awaitPick(p, false)
	return nil
}

// resumePick takes back a pick a restart interrupted, from its request row:
// the bubble is already drawn, so it stands the mirror again and reattaches to
// the pick's turn.
func (o *orchestrator) resumePick(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) error {
	const op = "daemon.merge.pick"
	ws := entry.Workspace
	requester, async := o.requesterOf(ws)
	if !async || entry.Ledger == "" {
		err := fmt.Errorf("merge: the begun pick of %s is no merge workspace's, or carries no ledger identity", ws)
		o.log(ctx, ws).Error(op, "a begun pick's request row contradicts itself", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return err
	}
	label, err := o.sourceLabel(ctx, ws, entry.Source)
	if err != nil {
		o.log(ctx, ws).Error(op, "could not resolve the pick's bubble line", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return err
	}
	o.mu.Lock()
	o.ledgerOf[ws] = entry.Ledger
	o.mu.Unlock()
	p := &pick{
		ws: ws, requester: requester, repo: repo, ledger: entry.Ledger, turn: entry.Source.PickTurn,
		startedMS: entry.Source.PickStartedAt.UnixMilli(), label: label, instruction: entry.Source.Pick,
	}
	o.log(ctx, ws).Info(op, "resumed an async merge's pick the restart interrupted", dlog.Context{
		"workspace": string(ws), "turn": string(p.turn), "ledger": string(p.ledger)})
	o.drawPickLive(p)
	o.awaitPick(p, true)
	return nil
}

// drawPickLive draws the head and the live pick tab, and stands the mirror the
// pick turn is drawn at in the requester's bubble.
func (o *orchestrator) drawPickLive(p *pick) {
	o.publishHead(p.ws, p.ledger, p.label, p.startedMS, nil)
	o.publishRoundTab(p.ws, p.ledger, TabPick, 1, pickTab(tabState{startedMS: p.startedMS, waiting: o.deps.Asks.Waiting(p.ws)}))
	o.deps.Feed.SetOutputMirror(p.ws, &wsm.MirrorAddress{
		Workspace: p.requester, Feed: mergeFeed(p.ledger), Parent: tabRef(p.requester, p.ledger, roundTabKey(TabPick, 1)),
	})
}

// awaitPick waits for the pick's turn to end and judges it, on a goroutine of
// its own registered as the merge workspace's request wait, so an evict or a
// close withdraws it. A resumed pick reattaches to its turn; a fresh one
// submits it.
func (o *orchestrator) awaitPick(p *pick, resumed bool) {
	ctx, cancel := context.WithCancel(context.Background())
	wait := &requestWait{ws: p.ws, repo: p.repo, ctx: ctx, cancel: cancel}
	o.mu.Lock()
	if standing, ok := o.requested[p.ws]; ok {
		standing.cancel()
	}
	o.requested[p.ws] = wait
	o.picks[p.ws] = p
	o.mu.Unlock()
	if !o.async {
		return
	}
	o.live.Add(1)
	go func() {
		defer o.live.Done()
		defer o.forgetWait(wait)
		o.runPick(ctx, p, resumed)
	}()
}

// runPick is the pick's wait and its judgement.
func (o *orchestrator) runPick(ctx context.Context, p *pick, resumed bool) {
	const op = "daemon.merge.pick"
	log := o.log(ctx, p.ws)
	text, err := o.deps.Briefs(BriefPick, o.pickBriefValues(ctx, p))
	if err != nil {
		log.Error(op, "could not compose the pick's brief; the merge fails", dlog.Context{"workspace": string(p.ws), "error": err.Error()})
		o.pickFailed(ctx, p, fmt.Sprintf("the pick's brief could not be composed: %v", err))
		return
	}
	var close wsm.TurnClose
	if resumed {
		close, err = o.reattachTurn(ctx, p.ws, dlog.Context{"ledger": string(p.ledger)}, p.turn, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK)
	} else {
		if err = o.ensureSession(ctx, p.ws); err == nil {
			close, err = o.submit(ctx, p.ws, p.turn, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK)
		}
	}
	if err != nil && ctx.Err() != nil {
		log.Info(op, "the pick's wait was withdrawn: its merge left the queue before the pick ended", dlog.Context{
			"workspace": string(p.ws), "cause": err.Error()})
		o.settlePick(p, "the merge left the queue before the pick ended")
		return
	}
	// THE PICK'S END IS NO LONGER THE WAIT'S: judging it and ending the merge
	// must finish even though ending it withdraws the wait itself.
	ctx = context.WithoutCancel(ctx)
	if err != nil {
		log.Error(op, "the pick's turn could not be run; the merge fails", dlog.Context{"workspace": string(p.ws), "turn": string(p.turn), "error": err.Error()})
		o.pickFailed(ctx, p, fmt.Sprintf("the pick's turn could not be run: %v", err))
		return
	}
	why, err := o.judgePick(ctx, p, close)
	if err != nil {
		log.Error(op, "could not judge the pick; the merge fails", dlog.Context{"workspace": string(p.ws), "error": err.Error()})
		o.pickFailed(ctx, p, fmt.Sprintf("the pick could not be judged: %v", err))
		return
	}
	if why != "" {
		log.Info(op, "the pick did not leave the changes picked; the merge fails and its workspace is revealed", dlog.Context{
			"workspace": string(p.ws), "why": why})
		o.pickFailed(ctx, p, "the pick gave up: "+why)
		return
	}
	o.settlePick(p, "")
	log.Info(op, "the pick succeeded; the merge joins the queue", dlog.Context{"workspace": string(p.ws), "repo": string(p.repo)})
	if err := o.queueRequest(ctx, p.ws, p.repo); err != nil {
		log.Error(op, "could not put the picked merge in line; it stays recorded for the next boot", dlog.Context{"workspace": string(p.ws), "error": err.Error()})
	}
}

// pickBriefValues are the pick brief's values: the instruction, the
// requester's branch it picks from, the target branch the worktree is on, and
// the worktree. A value that cannot be read is spliced as it is known, and the
// failure is recorded: the brief still states the instruction.
func (o *orchestrator) pickBriefValues(ctx context.Context, p *pick) map[string]string {
	values := map[string]string{"pick_instruction": p.instruction}
	if record, err := o.deps.DB.Workspace(ctx, p.requester); err == nil {
		values["source_branch"] = record.Branch
	} else {
		o.log(ctx, p.ws).Error("daemon.merge.pick", "could not read the requester for the pick's brief", dlog.Context{"workspace": string(p.ws), "requester": string(p.requester), "error": err.Error()})
	}
	if job, err := o.layoutFor(ctx, p.ws); err == nil {
		values["worktree_dir"] = job.Layout.SourceDir
		if target, err := o.deps.Git.CurrentBranch(ctx, job.Layout.TargetDir); err == nil {
			values["target_branch"] = target
		} else {
			o.log(ctx, p.ws).Error("daemon.merge.pick", "could not read the target branch for the pick's brief", dlog.Context{"workspace": string(p.ws), "error": err.Error()})
		}
	} else {
		o.log(ctx, p.ws).Error("daemon.merge.pick", "could not read the merge workspace's layout for the pick's brief", dlog.Context{"workspace": string(p.ws), "error": err.Error()})
	}
	return values
}

// judgePick answers why the pick failed, empty when it succeeded: its turn
// ended unfailed, the worktree is clean, and the merge workspace's branch
// stands at least one commit ahead of the target.
func (o *orchestrator) judgePick(ctx context.Context, p *pick, close wsm.TurnClose) (string, error) {
	if close.Failed() {
		return fmt.Sprintf("its turn ended %s", close), nil
	}
	job, err := o.layoutFor(ctx, p.ws)
	if err != nil {
		return "", err
	}
	dir := job.Layout.SourceDir
	clean, err := o.deps.Git.IsClean(ctx, dir)
	if err != nil {
		return "", fmt.Errorf("merge: reading whether %s is clean: %w", dir, err)
	}
	if !clean {
		return fmt.Sprintf("the worktree %s has uncommitted changes or a cherry-pick in progress", dir), nil
	}
	target, err := o.deps.Git.CurrentBranch(ctx, job.Layout.TargetDir)
	if err != nil {
		return "", fmt.Errorf("merge: reading the target branch in %s: %w", job.Layout.TargetDir, err)
	}
	ahead, err := o.deps.Git.CountCommits(ctx, dir, target, job.Layout.SourceBranch)
	if err != nil {
		return "", fmt.Errorf("merge: counting the commits %s picked onto %s: %w", job.Layout.SourceBranch, target, err)
	}
	if ahead == 0 {
		return fmt.Sprintf("no commit stands ahead of %s", target), nil
	}
	return "", nil
}

// settlePick draws the pick tab settled — succeeded, or failed with FAILURE —
// and withdraws the mirror: the merge workspace's later turns are its own, or
// the merge's addressed ones.
func (o *orchestrator) settlePick(p *pick, failure string) {
	o.deps.Feed.SetOutputMirror(p.ws, nil)
	o.mu.Lock()
	delete(o.picks, p.ws)
	o.mu.Unlock()
	o.publishRoundTab(p.ws, p.ledger, TabPick, 1, pickTab(tabState{startedMS: p.startedMS, settled: true, endedMS: o.nowMS(), failure: failure}))
}

// pickFailed ends an async merge whose pick failed: the pick tab and the head
// settle failed, the request leaves the queue, and the requester is handed
// the failure, which reveals the merge workspace.
func (o *orchestrator) pickFailed(ctx context.Context, p *pick, summary string) {
	const op = "daemon.merge.pick"
	o.settlePick(p, summary)
	o.publishHead(p.ws, p.ledger, p.label, p.startedMS, &frontendv1.FeedMergeError{
		EndedAtMs: o.nowMS(),
		Reason:    &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: summary}},
	})
	if err := o.deps.DB.RemoveMergeQueueEntry(ctx, p.repo, p.ws, "pick_failed"); err != nil {
		o.log(ctx, p.ws).Error(op, "could not drop the merge whose pick failed", dlog.Context{"workspace": string(p.ws), "repo": string(p.repo), "error": err.Error()})
	}
	o.mu.Lock()
	delete(o.repoOf, p.ws)
	delete(o.ledgerOf, p.ws)
	o.mu.Unlock()
	o.withdrawRequest(p.ws)
	o.handBack(ctx, p.ws, DetachedOutcome{Kind: DetachedFailed, Summary: summary})
	o.forget(p.ws)
}

// pickTab builds the pick tab at its badge.
func pickTab(st tabState) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabPick{}
	if waiting := st.waitingOnUser(); waiting != nil {
		inner.State = &frontendv1.FeedMergeTabPick_WaitingOnUser{WaitingOnUser: waiting}
	} else if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabPick_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabPick_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Pick{Pick: inner}}
}

// redrawPick draws merge workspace WS's live pick tab again at the waiting
// state an ask change set, and reports whether a pick stood to redraw.
func (o *orchestrator) redrawPick(ws ids.WorkspaceID, waiting bool) bool {
	o.mu.Lock()
	p, picking := o.picks[ws]
	o.mu.Unlock()
	if !picking {
		return false
	}
	o.publishRoundTab(ws, p.ledger, TabPick, 1, pickTab(tabState{startedMS: p.startedMS, waiting: waiting}))
	return true
}

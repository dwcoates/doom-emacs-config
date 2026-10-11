package merge

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// This file is a merge REQUEST: its pre-state refusals, its durable record,
// and the wait for the requesting turn's end that puts it in line.
//
// NO MERGE FACT REACHES ANY CLIENT BEFORE THE TURN THAT ASKED FOR IT HAS ENDED
// (owner, 2026-09-29). The request is recorded the moment it arrives, so a
// daemon exit before that turn's end does not lose it; but it is in nobody's
// line and on no surface -- no bubble, no footer status, no roster status --
// until the requesting turn ends. This holds STRUCTURALLY: a requested merge
// has no ledger identity (so no bubble can be addressed), no facts (so no
// footer or roster can draw it), and no queue place (so no other workspace's
// queue tab can list it) until QueueMerge moves it into line.

// Enqueue records a merge request and, once the requesting turn has ended,
// puts it in line.
//
// IT REFUSES PRE-STATE. An unmergeable request leaves no trail: nothing is
// recorded before the refusal, so there is no state to stamp and nothing to
// clean up.
func (o *orchestrator) Enqueue(ctx context.Context, req Request) error {
	const op = "daemon.merge.enqueue"
	ws := req.Workspace
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "source": req.Source.Kind.String(), "requested_by": req.By.String()}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the requesting workspace", withField(fields, "error", err.Error()))
		return err
	}
	source, err := o.checkSource(ctx, record, req.Source)
	if err != nil {
		log.Warn(op, "refused a merge request", withField(fields, "error", err.Error()))
		return err
	}
	if req.Detached {
		// ONLY THE REQUESTER'S OWN BRANCH RUNS DETACHED, and never kept open:
		// the boundary that builds the request guarantees it, so a request
		// that breaks it is a defect in that caller.
		if source.Kind != wsm.MergeSourceOwnBranch || source.KeepOpen {
			err := fmt.Errorf("merge: a detached merge of %s must be an own-branch merge that never keeps the requester open, not %s (keep_open %v)", ws, source.Kind, source.KeepOpen)
			log.Error(op, "a detached merge request contradicts itself", withField(fields, "error", err.Error()))
			return err
		}
		source.Detach = wsm.NewDetachedMergeID()
		fields["detach"] = string(source.Detach)
	}
	if source.Branch != "" {
		fields["branch"] = source.Branch
	}
	session, found, err := o.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the requesting workspace's session", withField(fields, "error", err.Error()))
		return err
	}
	if found && session.Terminal != nil && session.Terminal.Kind == "deleted" {
		err := refuse(ArmSessionDeleted, ws, "the session was deleted, so the merge's prompts and repairs can never run")
		log.Warn(op, "refused a merge for a deleted session", withField(fields, "error", err.Error()))
		return err
	}
	repo, err := o.repoKeyFor(ctx, record.Dir)
	if err != nil {
		log.Error(op, "could not resolve the requesting workspace's repository", withField(fields, "error", err.Error()))
		return err
	}
	if _, running := o.runFor(ws); running {
		err := refuse(ArmAlreadyMerging, ws, "this workspace's merge is already in flight")
		log.Warn(op, "refused a merge request while the workspace's merge runs", withField(fields, "error", err.Error()))
		return err
	}
	if err := o.deps.DB.RequestMerge(ctx, repo, ws, source, o.deps.Now()); err != nil {
		var queued *wsm.MergeQueuedError
		if errors.As(err, &queued) {
			arm := ArmAlreadyQueued
			if queued.State == wsm.MergeAdmitted {
				arm = ArmAlreadyMerging
			}
			refusal := refuse(arm, ws, "the merge already stands %s in its repository's queue", queued.State)
			log.Warn(op, "refused a duplicate merge request", withField(fields, "repo", string(repo)))
			return refusal
		}
		log.Error(op, "could not record the merge request", withField(fields, "error", err.Error()))
		return err
	}
	o.mu.Lock()
	o.repoOf[ws] = repo
	// ONLY THE USER'S OWN ASK MAY DISPLACE THE TURN IN FLIGHT (see Requester),
	// and a DETACHED merge displaces nothing: it never runs in the requester.
	if req.By == RequestedByUser && !req.Detached {
		o.displaces[ws] = true
	} else {
		delete(o.displaces, ws)
	}
	o.mu.Unlock()
	log.Info(op, "recorded a merge request", withField(fields, "repo", string(repo)))
	if req.By == RequestedByUser {
		// THE USER'S ASK HAS NO REQUESTING TURN: it is put in line at once.
		return o.queueRequest(ctx, ws, repo)
	}
	o.awaitRequestingTurn(ws, repo)
	return nil
}

// checkSource refuses a request whose source cannot be merged -- a requester
// with no recorded geometry for a source that is its own branch, another
// workspace that is not open in the same repository, or a branch that is not
// there -- and answers the source as it is recorded.
//
// A WORKSPACE'S BRANCH IS THE ONE CHECKED OUT IN ITS WORKTREE NOW (owner
// ruling, 2026-10-01), read here and recorded with the request. The branch
// the workspace was created on is not consulted: a worktree that switched or
// renamed its branch would otherwise merge a branch that may not even exist
// (2026-10-01: `footer-activity-updates` was requested while the worktree had
// `merge-queue-rework` checked out, and the merge failed at its first git
// command). A worktree with no branch checked out is refused at once.
func (o *orchestrator) checkSource(ctx context.Context, requester wsm.Workspace, source wsm.MergeSource) (wsm.MergeSource, error) {
	ws := requester.ID
	switch source.Kind {
	case wsm.MergeSourceOwnBranch, wsm.MergeSourceMergedUpstream:
		return o.withCheckedOutBranch(ctx, ws, ws, source)
	case wsm.MergeSourceWorkspace:
		if source.Workspace == ws {
			return wsm.MergeSource{}, refuse(ArmUnknownSourceWorkspace, ws, "a workspace's own branch is the own-branch source, not another workspace")
		}
		other, err := o.deps.DB.Workspace(ctx, source.Workspace)
		if err != nil {
			return wsm.MergeSource{}, refuse(ArmUnknownSourceWorkspace, ws, "no workspace %s is registered", source.Workspace)
		}
		if other.Closed {
			return wsm.MergeSource{}, refuse(ArmUnknownSourceWorkspace, ws, "workspace %s is closed", source.Workspace)
		}
		if other.Repo != requester.Repo {
			return wsm.MergeSource{}, refuse(ArmUnknownSourceWorkspace, ws, "workspace %s is in another repository", source.Workspace)
		}
		return o.withCheckedOutBranch(ctx, ws, source.Workspace, source)
	case wsm.MergeSourceBranch:
		exists, err := o.deps.Git.BranchExists(ctx, requester.Dir, source.Branch)
		if err != nil {
			return wsm.MergeSource{}, fmt.Errorf("merge: asking whether branch %q exists: %w", source.Branch, err)
		}
		if !exists {
			return wsm.MergeSource{}, refuse(ArmUnknownBranch, ws, "no branch %q exists in the repository", source.Branch)
		}
		return source, nil
	default:
		return wsm.MergeSource{}, fmt.Errorf("merge: a request for %s names the undeclared source %s", ws, source.Kind)
	}
}

// withCheckedOutBranch answers source carrying the branch checked out in the
// worktree of workspace of, refused for requester when of has no recorded
// geometry or no branch checked out.
func (o *orchestrator) withCheckedOutBranch(ctx context.Context, requester, of ids.WorkspaceID, source wsm.MergeSource) (wsm.MergeSource, error) {
	job, err := o.layoutFor(ctx, of)
	if err != nil {
		return wsm.MergeSource{}, err
	}
	branch, err := o.checkedOutBranch(ctx, requester, job.Layout.SourceDir)
	if err != nil {
		return wsm.MergeSource{}, err
	}
	source.Branch = branch
	return source, nil
}

// checkedOutBranch answers the branch checked out in a workspace's worktree,
// refusing a detached HEAD: there is no branch to merge.
func (o *orchestrator) checkedOutBranch(ctx context.Context, ws ids.WorkspaceID, dir string) (string, error) {
	branch, err := o.deps.Git.CurrentBranch(ctx, dir)
	if err != nil {
		return "", fmt.Errorf("merge: reading the branch checked out in %s: %w", dir, err)
	}
	if branch == "" {
		return "", refuse(ArmUnknownBranch, ws, "the worktree %s has no branch checked out (its HEAD is detached), so there is no branch to merge", dir)
	}
	return branch, nil
}

// requestWait is one request's wait for its requesting turn's end.
type requestWait struct {
	ws     ids.WorkspaceID
	repo   wsm.RepoKey
	ctx    context.Context
	cancel context.CancelFunc
}

// awaitRequestingTurn puts a recorded request in line once the requester's
// turn in flight has ended, on a goroutine of its own. A requester with no
// turn in flight is put in line at once. The wait ends early -- the request
// staying recorded, for the next boot's recovery -- when the request is
// withdrawn (an evict, the workspace's close).
func (o *orchestrator) awaitRequestingTurn(ws ids.WorkspaceID, repo wsm.RepoKey) *requestWait {
	ctx, cancel := context.WithCancel(context.Background())
	wait := &requestWait{ws: ws, repo: repo, ctx: ctx, cancel: cancel}
	o.mu.Lock()
	if standing, ok := o.requested[ws]; ok {
		standing.cancel()
	}
	o.requested[ws] = wait
	o.mu.Unlock()
	if o.async {
		o.live.Add(1)
		go func() {
			defer o.live.Done()
			o.waitThenQueue(wait)
		}()
	}
	return wait
}

// waitThenQueue is the request's wait: the requesting turn's end, then its
// place in line.
func (o *orchestrator) waitThenQueue(wait *requestWait) {
	const op = "daemon.merge.request"
	ctx, ws := wait.ctx, wait.ws
	defer o.forgetWait(wait)
	log := o.log(ctx, ws)
	if turn, inFlight := o.deps.TurnInFlight(ws); inFlight {
		log.Info(op, "the merge request waits for the turn that asked for it to end; nothing about it is reported before then",
			dlog.Context{"workspace": string(ws), "turn": string(turn)})
		if _, err := o.deps.AwaitTurnEnd(ctx, ws, turn); err != nil {
			if ctx.Err() != nil {
				log.Info(op, "the merge request was withdrawn before the requesting turn ended",
					dlog.Context{"workspace": string(ws), "turn": string(turn), "cause": err.Error()})
				return
			}
			log.Error(op, "could not wait for the requesting turn to end; the request stays recorded for the next boot",
				dlog.Context{"workspace": string(ws), "turn": string(turn), "error": err.Error()})
			return
		}
	}
	if err := o.queueRequest(ctx, ws, wait.repo); err != nil {
		log.Error(op, "could not put the merge request in line", dlog.Context{"workspace": string(ws), "error": err.Error()})
	}
}

// forgetWait retires a finished wait, unless a newer one replaced it.
func (o *orchestrator) forgetWait(wait *requestWait) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.requested[wait.ws] == wait {
		delete(o.requested, wait.ws)
	}
	wait.cancel()
}

// withdrawRequest ends a request's wait and answers whether one stood.
func (o *orchestrator) withdrawRequest(ws ids.WorkspaceID) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	wait, standing := o.requested[ws]
	if standing {
		wait.cancel()
		delete(o.requested, ws)
	}
	return standing
}

// queueRequest moves a recorded request into line: the FIRST moment anything
// about the merge is reported. It mints the bubble's ledger identity,
// publishes the queue, and kicks the pump.
func (o *orchestrator) queueRequest(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey) error {
	const op = "daemon.merge.request"
	if o.isDraining() {
		o.log(ctx, ws).Info(op, "the daemon is draining; the merge request stays recorded for the next boot",
			dlog.Context{"workspace": string(ws)})
		return nil
	}
	// A DETACHED REQUEST IS HANDED TO ITS MERGE WORKSPACE HERE, at the moment
	// it would have been put in line: the requesting turn has ended, so the
	// fork holds that whole turn.
	if entry, found, err := o.requestOf(ctx, repo, ws); err != nil {
		return err
	} else if found && entry.Source.Detach != "" {
		return o.handOffDetached(ctx, ws, repo, entry.Source)
	}
	// THE BUBBLE EXISTS FROM HERE: the ledger identity it is addressed by is
	// minted as the merge takes its place, and written on its row with that
	// place, so republishQueue can draw it -- and the next daemon to serve the
	// workspace draws the same bubble.
	ledger := o.mintLedger(ws)
	position, err := o.deps.DB.QueueMerge(ctx, repo, ws, ledger)
	if err != nil {
		o.mu.Lock()
		delete(o.ledgerOf, ws)
		o.mu.Unlock()
		return err
	}
	o.log(ctx, ws).Info(op, "put a merge in line; it is reported from here", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "position": position})
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// requestOf answers WS's row in REPO's queue, false when it has none.
func (o *orchestrator) requestOf(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID) (wsm.MergeQueueEntry, bool, error) {
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return wsm.MergeQueueEntry{}, false, err
	}
	for _, entry := range entries {
		if entry.Workspace == ws {
			return entry, true, nil
		}
	}
	return wsm.MergeQueueEntry{}, false, nil
}

// handOffDetached runs a detached request: it forks the merge workspace (or
// finds the one a restart left forked), moves the request onto it durably, and
// puts the merge workspace's own in-place merge in line. From there the merge
// is an ordinary merge of the merge workspace, which draws its bubble in the
// requester's feed too (placements).
//
// A FORK THAT FAILS ENDS THE REQUEST: it leaves the requester's queue, and the
// requester is told through its handback, so a request never stands in nobody's
// line waiting for a fork that will not come.
func (o *orchestrator) handOffDetached(ctx context.Context, requester ids.WorkspaceID, repo wsm.RepoKey, source wsm.MergeSource) error {
	const op = "daemon.merge.detached"
	log := o.log(ctx, requester)
	fields := dlog.Context{"workspace": string(requester), "repo": string(repo), "detach": string(source.Detach), "branch": source.Branch}
	record, err := o.deps.DB.Workspace(ctx, requester)
	if err != nil {
		log.Error(op, "could not read the workspace a detached merge was asked for", withField(fields, "error", err.Error()))
		return err
	}
	forked, err := o.deps.Detached.Fork(ctx, record, source.Detach, source.Branch)
	if err != nil {
		log.Error(op, "could not fork the merge workspace for a detached merge; the request is dropped and the requester told", withField(fields, "error", err.Error()))
		if dropErr := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, requester, "detached_fork_failed"); dropErr != nil {
			log.Error(op, "could not drop the detached request whose fork failed", withField(fields, "error", dropErr.Error()))
			return dropErr
		}
		o.mu.Lock()
		delete(o.repoOf, requester)
		o.mu.Unlock()
		return o.deps.Detached.HandBack(ctx, requester, wsm.Workspace{}, DetachedOutcome{
			Kind: DetachedAbandoned, Summary: fmt.Sprintf("the merge workspace could not be created: %v", err), Branch: source.Branch,
		})
	}
	fields["merge_workspace"] = string(forked.ID)
	if err := o.deps.DB.HandOffDetachedMerge(ctx, repo, requester, source.Detach, forked.ID, forked.Branch, o.deps.Now()); err != nil {
		log.Error(op, "could not hand the detached request to its merge workspace; it stays recorded for the next boot", withField(fields, "error", err.Error()))
		return err
	}
	o.mu.Lock()
	delete(o.repoOf, requester)
	o.repoOf[forked.ID] = repo
	o.mu.Unlock()
	log.Info(op, "handed a detached merge to its merge workspace", fields)
	return o.queueRequest(ctx, forked.ID, repo)
}

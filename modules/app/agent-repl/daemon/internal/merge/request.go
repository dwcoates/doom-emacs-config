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
	if err := o.checkSource(ctx, record, req.Source); err != nil {
		log.Warn(op, "refused a merge request", withField(fields, "error", err.Error()))
		return err
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
	if err := o.deps.DB.RequestMerge(ctx, repo, ws, req.Source, o.deps.Now()); err != nil {
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
	// ONLY THE USER'S OWN ASK MAY DISPLACE THE TURN IN FLIGHT (see Requester).
	if req.By == RequestedByUser {
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

// checkSource refuses a request whose source cannot be merged: a requester
// with no recorded geometry for a source that is its own branch, another
// workspace that is not open in the same repository, or a branch that is not
// there.
func (o *orchestrator) checkSource(ctx context.Context, requester wsm.Workspace, source wsm.MergeSource) error {
	ws := requester.ID
	switch source.Kind {
	case wsm.MergeSourceOwnBranch, wsm.MergeSourceMergedUpstream:
		_, err := o.layoutFor(ctx, ws)
		return err
	case wsm.MergeSourceWorkspace:
		if source.Workspace == ws {
			return refuse(ArmUnknownSourceWorkspace, ws, "a workspace's own branch is the own-branch source, not another workspace")
		}
		other, err := o.deps.DB.Workspace(ctx, source.Workspace)
		if err != nil {
			return refuse(ArmUnknownSourceWorkspace, ws, "no workspace %s is registered", source.Workspace)
		}
		if other.Closed {
			return refuse(ArmUnknownSourceWorkspace, ws, "workspace %s is closed", source.Workspace)
		}
		if other.Repo != requester.Repo {
			return refuse(ArmUnknownSourceWorkspace, ws, "workspace %s is in another repository", source.Workspace)
		}
		if _, err := o.layoutFor(ctx, source.Workspace); err != nil {
			return err
		}
		return nil
	case wsm.MergeSourceBranch:
		exists, err := o.deps.Git.BranchExists(ctx, requester.Dir, source.Branch)
		if err != nil {
			return fmt.Errorf("merge: asking whether branch %q exists: %w", source.Branch, err)
		}
		if !exists {
			return refuse(ArmUnknownBranch, ws, "no branch %q exists in the repository", source.Branch)
		}
		return nil
	default:
		return fmt.Errorf("merge: a request for %s names the undeclared source %s", ws, source.Kind)
	}
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
	position, err := o.deps.DB.QueueMerge(ctx, repo, ws)
	if err != nil {
		return err
	}
	// THE BUBBLE EXISTS FROM HERE: the ledger identity it is addressed by is
	// minted as the merge takes its place, so republishQueue can draw it.
	o.mintLedger(ws)
	o.log(ctx, ws).Info(op, "put a merge in line; it is reported from here", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "position": position})
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

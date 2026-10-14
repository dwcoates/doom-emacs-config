package merge

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
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
	fields := dlog.Context{"workspace": string(ws), "source": req.Source.Kind.String(), "requested_by": req.By.String(), "one_shot": req.OneShot}
	// A ONE-SHOT IS AN IN-PLACE OWN-BRANCH MERGE THAT CLOSES ITS REQUESTER:
	// the boundary that builds the request guarantees it, so a request that
	// breaks it is a defect in that caller.
	if req.OneShot && (req.Detached != nil || !req.Source.IsOneShot()) {
		err := fmt.Errorf("merge: a one-shot merge of %s must be an in-place own-branch merge that closes the requester, not %s (keep_open %v, detached %v)",
			ws, req.Source.Kind, req.Source.KeepOpen, req.Detached != nil)
		log.Error(op, "a one-shot merge request contradicts itself", withField(fields, "error", err.Error()))
		return err
	}
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
	if v := req.Source.Version; v != nil {
		fields["version_part"] = v.Part.String()
	}
	// A MERGE ONTO AGENT-REPL'S MASTER MOVES THE VERSION, and no other merge
	// does (checkVersion).
	if err := o.checkVersion(ctx, record, req); err != nil {
		if _, refused := Refused(err); refused {
			log.Warn(op, "refused a merge request for its version increment", withField(fields, "error", err.Error()))
		} else {
			log.Error(op, "could not decide whether the merge moves agent-repl's version", withField(fields, "error", err.Error()))
		}
		return err
	}
	// releaseName lets go of an async request's name claim; a no-op for every
	// other request. It runs once the request is recorded or refused.
	releaseName := func() {}
	defer func() { releaseName() }()
	if req.Detached != nil {
		// AN ASYNC MERGE MERGES A NAMED BRANCH, and always names its merge
		// workspace: the boundary that builds the request guarantees both, so
		// a request that breaks either is a defect in that caller.
		if source.Kind != wsm.MergeSourceBranch || req.Detached.Name == "" {
			err := fmt.Errorf("merge: an async merge of %s must name its branch and its merge workspace, not %s (name %q)", ws, source.Kind, req.Detached.Name)
			log.Error(op, "an async merge request contradicts itself", withField(fields, "error", err.Error()))
			return err
		}
		if source, err = o.checkAsync(ctx, record, source, req.Detached); err != nil {
			log.Warn(op, "refused an async merge request", withField(fields, "error", err.Error()))
			return err
		}
		// THE NAME IS CLAIMED BEFORE THE REQUEST IS RECORDED, and the record is
		// its durable reservation, so between the two nothing else can take
		// it; the claim is let go the moment the record has been written or
		// refused, and before anything forks under the name.
		branch, release, taken, err := o.deps.Names.ClaimMergeName(ctx, record.Dir, req.Detached.Name)
		if err != nil {
			log.Error(op, "could not check an async merge's name", withField(fields, "error", err.Error()))
			return err
		}
		if taken {
			refusal := refuse(ArmNameTaken, ws, "the async merge's name %q resolves to %s, which is already a workspace, a branch or another merge's reservation", req.Detached.Name, branch)
			refusal.Fields = map[string]any{"branch": branch}
			log.Warn(op, "refused an async merge whose name is taken", withField(fields, "error", refusal.Error()))
			return refusal
		}
		releaseName = release
		source.Detach = wsm.NewDetachedMergeID()
		source.DetachName = req.Detached.Name
		source.Pick = req.Detached.Pick
		source.CompactAfter = req.Detached.CompactAfter
		source.Actions = req.Detached.Actions
		fields["actions_before"] = len(source.Actions.Before)
		fields["actions_after"] = len(source.Actions.After)
		fields["close_on_landing"] = string(source.CloseOnLanding)
		fields["cleanup_worktree"] = source.CleanupWorktree
		fields["cleanup_dir"] = source.CleanupDir
		fields["detach"] = string(source.Detach)
		fields["name"] = source.DetachName
		fields["pick"] = source.Pick != ""
		fields["compact_after"] = source.CompactAfter
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
	// AN IN-PLACE MERGE THE AGENT'S TURN ASKED FOR FLUSHES THE HELD QUEUE
	// (owner ruling, 2026-10-12): the merge runs in this session once the
	// turn ends, and what the user sent before then is meant to be handled
	// before the merge, never after it. Every held prompt is sent to the shim
	// at once to join the running turn, nothing interrupted, and the merge
	// waits for the turn that answers them (awaitFree). The user's own ask
	// displaces the turn in flight instead, and a detached merge never runs
	// in this session, so neither flushes. A flush that could not read the
	// queue refuses the request before anything of it is recorded; the queue
	// records why.
	if req.By == RequestedByAgent && req.Detached == nil {
		if err := o.deps.Queue.FlushHeld(ctx, ws); err != nil {
			return fmt.Errorf("merge: flushing the held prompts of %s before its in-place merge: %w", ws, err)
		}
	}
	requestMerge := o.deps.DB.RequestMerge
	if req.OneShot {
		requestMerge = o.deps.DB.RequestOneShotMerge
	}
	err = requestMerge(ctx, repo, ws, source, o.deps.Now())
	releaseName()
	releaseName = func() {}
	if err != nil {
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
	// and an ASYNC merge displaces nothing: it never runs in the requester.
	if req.By == RequestedByUser && req.Detached == nil {
		o.displaces[ws] = true
	} else {
		delete(o.displaces, ws)
	}
	o.mu.Unlock()
	log.Info(op, "recorded a merge request", withField(fields, "repo", string(repo)))
	// AN ANNOUNCED ONE-SHOT is republished, so the roster draws its one-shot
	// mark.
	if req.OneShot {
		o.publishOneShot(ctx, log, ws)
	}
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

// checkAsync refuses an async request the daemon cannot run as asked, and
// answers its source carrying the request's landing effects:
//
//   - THE BRANCH MUST NOT BE THE ONE THE REQUESTER HAS CHECKED OUT: the
//     requester keeps working on it while the merge rebases it, so the
//     request is refused (branch_checked_out_in_requester) rather than
//     merging a branch under its own author.
//   - THE ONE-SHOT WORKSPACE must be registered, open, in the requester's
//     repository and not the requester, which an async merge never closes
//     (unknown_one_shot_workspace).
//   - THE CLEANUP DIRECTORY is the worktree that has the branch checked out
//     now, found by branch name (git checks a branch out in at most one
//     worktree, so the match is exact). It is recorded only when it is no
//     workspace's worktree, not the requester's and not the repository's
//     main worktree: those are never removed by a merge's cleanup.
//
// THE REQUESTER'S BRANCH IS READ OFF ITS OWN WORKTREE, never off a creation
// job: the main checkout, which is the workspace that always merges async, was
// never created by the daemon and records no merge geometry.
func (o *orchestrator) checkAsync(ctx context.Context, requester wsm.Workspace, source wsm.MergeSource, run *DetachedRun) (wsm.MergeSource, error) {
	ws := requester.ID
	own, err := o.deps.Git.CurrentBranch(ctx, requester.Dir)
	if err != nil {
		return wsm.MergeSource{}, fmt.Errorf("merge: reading the branch checked out in %s: %w", requester.Dir, err)
	}
	if own != "" && own == source.Branch {
		return wsm.MergeSource{}, refuse(ArmBranchCheckedOutInRequester, ws, "branch %q is checked out in the requesting worktree %s", source.Branch, requester.Dir)
	}
	if run.CloseOnLanding != "" {
		if run.CloseOnLanding == ws {
			return wsm.MergeSource{}, refuse(ArmUnknownOneShotWorkspace, ws, "an async merge never closes the workspace that asked for it")
		}
		other, err := o.deps.DB.Workspace(ctx, run.CloseOnLanding)
		switch {
		case errors.Is(err, wsm.ErrNotFound):
			return wsm.MergeSource{}, refuse(ArmUnknownOneShotWorkspace, ws, "no workspace %s is registered", run.CloseOnLanding)
		case err != nil:
			return wsm.MergeSource{}, fmt.Errorf("merge: reading the one-shot workspace %s: %w", run.CloseOnLanding, err)
		case other.Closed:
			return wsm.MergeSource{}, refuse(ArmUnknownOneShotWorkspace, ws, "workspace %s is closed", run.CloseOnLanding)
		case other.Repo != requester.Repo:
			return wsm.MergeSource{}, refuse(ArmUnknownOneShotWorkspace, ws, "workspace %s is in another repository", run.CloseOnLanding)
		}
		source.CloseOnLanding = run.CloseOnLanding
	}
	if run.CleanupWorktree {
		source.CleanupWorktree = true
		dir, err := o.cleanupDirFor(ctx, requester, source.Branch)
		if err != nil {
			return wsm.MergeSource{}, err
		}
		source.CleanupDir = dir
	}
	return source, nil
}

// cleanupDirFor answers the worktree that has BRANCH checked out, empty when
// none does or the one that does is never a cleanup's to remove: a registered
// workspace's, the requester's, or the repository's main worktree.
func (o *orchestrator) cleanupDirFor(ctx context.Context, requester wsm.Workspace, branch string) (string, error) {
	main, err := o.deps.Git.MainWorktree(ctx, requester.Dir)
	if err != nil {
		return "", fmt.Errorf("merge: resolving the repository's main worktree: %w", err)
	}
	worktrees, err := o.deps.Git.ListWorktrees(ctx, main)
	if err != nil {
		return "", fmt.Errorf("merge: listing the repository's worktrees: %w", err)
	}
	for _, wt := range worktrees {
		if wt.Bare || wt.Branch != branch {
			continue
		}
		if filepath.Clean(wt.Dir) == filepath.Clean(main) || filepath.Clean(wt.Dir) == filepath.Clean(requester.Dir) {
			return "", nil
		}
		if _, err := o.deps.DB.WorkspaceByDir(ctx, wt.Dir); err == nil {
			return "", nil
		} else if !errors.Is(err, wsm.ErrNotFound) {
			return "", fmt.Errorf("merge: asking whether %s is a workspace's worktree: %w", wt.Dir, err)
		}
		return wt.Dir, nil
	}
	return "", nil
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

// withdrawRequest ends a request's wait and answers whether one stood. The
// roster's request fact is lowered whether or not a wait stood: a request
// whose wait already ended in error is still withdrawn.
func (o *orchestrator) withdrawRequest(ws ids.WorkspaceID) bool {
	o.mu.Lock()
	wait, standing := o.requested[ws]
	if standing {
		wait.cancel()
		delete(o.requested, ws)
	}
	o.mu.Unlock()
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
	entry, found, err := o.requestOf(ctx, repo, ws)
	if err != nil {
		return err
	}
	if !found {
		err := fmt.Errorf("merge: %s has no recorded request to put in line", ws)
		o.log(ctx, ws).Error(op, "a merge to put in line has no recorded request", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "error": err.Error()})
		return err
	}
	if entry.Source.Detach != "" {
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
	// THE FIRST OF THE MERGE'S TWO BANNERS: it is in line. The second is its
	// resolution (ended); nothing between raises one.
	o.deps.Banners.Queued(ws, o.bubbleLabel(ctx, entry))
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
// finds the one a restart left forked) under the name the request reserved,
// moves the request onto it durably, and then either begins its pick or puts
// its own in-place merge in line. From there the merge is an ordinary merge
// of the merge workspace, whose bubble is drawn in the requester's feed
// (bubbleOf).
//
// THE MERGE WORKSPACE FORKS ONTO THE REQUEST'S BRANCH TIP, whether or not the
// request carries an instruction: an instruction prepares that branch in the
// merge workspace's first turn before the merge joins the queue.
//
// THE QUEUE DECIDES WHERE THE MERGE LANDS, HERE AND ONCE: the target by the
// one rule the request's version check also asked (detachedTarget), the
// requester's parent branch, and exactly the merge actions the request named.
// The merge workspace's create lands by what the origin carries and reads
// nothing off the requester, so a main checkout, which has no creation job,
// is forked from like any other requester, and a merge workspace never
// inherits its requester's configured actions.
//
// A FORK THAT FAILS ENDS THE REQUEST: it leaves the requester's queue, and the
// requester is told through its handback, so a request never stands in nobody's
// line waiting for a fork that will not come. A landing that cannot be decided
// ends it the same way.
func (o *orchestrator) handOffDetached(ctx context.Context, requester ids.WorkspaceID, repo wsm.RepoKey, source wsm.MergeSource) error {
	const op = "daemon.merge.detached"
	log := o.log(ctx, requester)
	fields := dlog.Context{"workspace": string(requester), "repo": string(repo), "detach": string(source.Detach), "branch": source.Branch, "name": source.DetachName, "pick": source.Pick != "", "compact_after": source.CompactAfter}
	record, err := o.deps.DB.Workspace(ctx, requester)
	if err != nil {
		log.Error(op, "could not read the workspace a detached merge was asked for", withField(fields, "error", err.Error()))
		return err
	}
	base := source.Branch
	fields["base"] = base
	target, err := o.detachedTarget(ctx, record)
	if err != nil {
		log.Error(op, "could not decide where a detached merge lands; the request is dropped and the requester told", withField(fields, "error", err.Error()))
		return o.dropDetached(ctx, requester, repo, source, fmt.Sprintf("where the merge lands could not be decided: %v", err))
	}
	fields["target_dir"] = target
	fields["parent_branch"] = record.ParentBranch
	fields["actions_before"] = len(source.Actions.Before)
	fields["actions_after"] = len(source.Actions.After)
	origin := wsm.MergeOrigin{Requester: requester, Request: source.Detach, CompactAfter: source.CompactAfter,
		CloseOnLanding: source.CloseOnLanding, CleanupWorktree: source.CleanupWorktree, CleanupDir: source.CleanupDir,
		TargetDir: target, ParentBranch: record.ParentBranch, Actions: source.Actions}
	forked, err := o.deps.Detached.Fork(ctx, record, origin, source.DetachName, base)
	if err != nil {
		log.Error(op, "could not fork the merge workspace for a detached merge; the request is dropped and the requester told", withField(fields, "error", err.Error()))
		return o.dropDetached(ctx, requester, repo, source, fmt.Sprintf("the merge workspace could not be created: %v", err))
	}
	fields["merge_workspace"] = string(forked.ID)
	if err := o.deps.DB.HandOffDetachedMerge(ctx, repo, requester, source.Detach, forked.ID, forked.Branch, source.Pick, o.deps.Now()); err != nil {
		log.Error(op, "could not hand the detached request to its merge workspace; it stays recorded for the next boot", withField(fields, "error", err.Error()))
		return err
	}
	o.mu.Lock()
	delete(o.repoOf, requester)
	o.repoOf[forked.ID] = repo
	o.mu.Unlock()
	log.Info(op, "handed a detached merge to its merge workspace", fields)
	if source.Pick == "" {
		return o.queueRequest(ctx, forked.ID, repo)
	}
	entry, found, err := o.requestOf(ctx, repo, forked.ID)
	if err != nil {
		return err
	}
	if !found {
		err := fmt.Errorf("merge: the merge workspace %s has no request after its hand-off", forked.ID)
		log.Error(op, "the handed-off request is not on the merge workspace's row", withField(fields, "error", err.Error()))
		return err
	}
	return o.startPick(ctx, forked.ID, repo, entry)
}

// dropDetached ends a detached request that could not be handed off: it leaves
// the requester's queue, and the requester is told it did not run.
func (o *orchestrator) dropDetached(ctx context.Context, requester ids.WorkspaceID, repo wsm.RepoKey, source wsm.MergeSource, summary string) error {
	const op = "daemon.merge.detached"
	if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, requester, "detached_fork_failed"); err != nil {
		o.log(ctx, requester).Error(op, "could not drop the detached request that could not be handed off", dlog.Context{
			"workspace": string(requester), "repo": string(repo), "error": err.Error()})
		return err
	}
	o.mu.Lock()
	delete(o.repoOf, requester)
	o.mu.Unlock()
	// A MERGE NEVER FORKED HAS ENDED WITHOUT LANDING: there is no merge
	// workspace whose terminal would raise its resolved banner (ended), so it
	// is raised here.
	out := DetachedOutcome{Kind: DetachedAbandoned, Summary: summary, Branch: source.DetachName}
	o.deps.Banners.Resolved(requester, out.Kind.mergeEnd(), out.Summary)
	return o.deps.Detached.HandBack(ctx, requester, wsm.Workspace{}, out)
}

// publishOneShot republishes the roster's DURABLE half once a one-shot merge
// request has marked its requester a one-shot, so every surface drawing the
// one-shot mark draws it from the announcement on rather than from whatever
// next refreshes the registry. The request already stands, so a failed
// republish is recorded and the request is not undone.
func (o *orchestrator) publishOneShot(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	const op = "daemon.merge.enqueue"
	if o.deps.PublishRegistry == nil {
		return
	}
	if err := o.deps.PublishRegistry(ctx); err != nil {
		log.Error(op, "could not republish the roster after marking the requester a one-shot",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
	}
}

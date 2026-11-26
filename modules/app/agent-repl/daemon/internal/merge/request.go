package merge

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
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
	besideOffQueue, openChildren := logsite.Msg("refused a merge request beside the request standing off its queue"), logsite.Msg("refused a merge that would close a workspace with open children")
	const op = "daemon.merge.enqueue"
	ws := req.Workspace
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "source": req.Source.Kind.String(), "requested_by": req.By.String(), "one_shot": req.OneShot}
	// A ONE-SHOT IS AN IN-PLACE OWN-BRANCH MERGE THAT CLOSES ITS REQUESTER:
	// the boundary that builds the request guarantees it, so a request that
	// breaks it is a defect in that caller.
	if req.OneShot && !req.Source.IsOneShot() {
		err := fmt.Errorf("merge: a one-shot merge of %s must be an in-place own-branch merge that closes the requester, not %s (keep_open %v)",
			ws, req.Source.Kind, req.Source.KeepOpen)
		log.Error(op, "a one-shot merge request contradicts itself", withField(fields, "error", err.Error()))
		return err
	}
	// A MERGE IS PLACED ONE WAY, at the front or behind a named merge: the
	// placement's oneof at the boundary guarantees it.
	if req.Source.Front && req.PlaceBehind != "" {
		err := fmt.Errorf("merge: the merge of %s is placed at the front and behind %q at once", ws, req.PlaceBehind)
		log.Error(op, "a merge request contradicts itself about its placement", withField(fields, "error", err.Error()))
		return err
	}
	// WHO ASKED IS NAMED EXACTLY WHEN AN AGENT ASKED: the boundary that
	// builds the request guarantees it.
	if (req.By == RequestedByAgent) != (req.Asker != "") {
		err := fmt.Errorf("merge: a merge of %s asked by %s names the asking workspace %q", ws, req.By, req.Asker)
		log.Error(op, "a merge request contradicts itself about who asked", withField(fields, "error", err.Error()))
		return err
	}
	if req.Asker != "" {
		fields["asker"] = string(req.Asker)
	}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the requesting workspace", withField(fields, "error", err.Error()))
		return err
	}
	if err := checkAsker(record, req.Asker); err != nil {
		logRefusal(log, op, logsite.Msg("refused a merge its asker may not ask for"), fields, err)
		return err
	}
	source, err := o.checkSource(ctx, record, req.Source)
	if err != nil {
		logRefusal(log, op, logsite.Msg("refused a merge request"), fields, err)
		return err
	}
	if req.PlaceBehind != "" {
		fields["place_behind"] = req.PlaceBehind
		source.BehindName = req.PlaceBehind
		if source.Behind, err = o.placeBehind(ctx, record, req); err != nil {
			if _, refused := Refused(err); refused {
				logRefusal(log, op, logsite.Msg("refused a merge placed behind a merge not in its queue"), fields, err)
			} else {
				log.Error(op, "could not read the queue a merge is placed in", withField(fields, "error", err.Error()))
			}
			return err
		}
	}
	if v := req.Source.Version; v != nil {
		fields["version_part"] = v.Part.String()
	}
	// THE REPOSITORY'S DEPLOY CONFIG DECIDES WHAT THE DEPLOY DOES, so a
	// repository that states none, or an unusable one, refuses the request
	// before anything is recorded.
	deploy, err := o.requestDeploy(ctx, record, req)
	if err != nil {
		if _, refused := Refused(err); refused {
			logRefusal(log, op, logsite.Msg("refused a deploy request for its repository's deploy config"), fields, err)
		} else {
			log.Error(op, "could not read the repository's deploy config", withField(fields, "error", err.Error()))
		}
		return err
	}
	// A DEPLOY ONTO A VERSIONED REPOSITORY'S MAIN BRANCH MOVES ITS VERSION, and
	// no other deploy does (checkVersion).
	if err := checkVersion(ws, req, deploy); err != nil {
		logRefusal(log, op, logsite.Msg("refused a deploy request for its version increment"), fields, err)
		return err
	}
	// AN AGENT'S FINISH RUNS ITS RULE SUITES FIRST (rulecheck.go), once every
	// cheaper refusal has been asked, and before anything is recorded.
	if err := o.checkRules(ctx, record, req, source, deploy, fields); err != nil {
		return err
	}
	if source.Branch != "" {
		fields["branch"] = source.Branch
	}
	// A MERGE NEVER CLOSES A PARENT OVER ITS OPEN CHILDREN (wsm/family.go).
	// Asked before anything is flushed or recorded, so a refused merge
	// changes nothing; the request's own transaction checks again, which is
	// the guarantee. The refusal is the answer, has_open_children.
	if err := o.deps.DB.RequireNoOpenChildren(ctx, source.Closes(ws, record.Effects)...); err != nil {
		if wsm.IsFamilyRefusal(err) {
			logRefusal(log, op, openChildren, fields, err)
			return err
		}
		log.Error(op, "could not read whether the merge closes a workspace with open children", withField(fields, "error", err.Error()))
		return err
	}
	// A WORKSPACE HOLDS ONE MERGE, IN LINE OR OFF IT (offqueue.go). Asked
	// before anything is flushed or recorded, so a refused request changes
	// nothing; the request's own transaction checks again, which is the
	// guarantee.
	rec, found, err := o.deps.DB.OffQueueMergeOf(ctx, ws)
	if err != nil {
		log.Error(op, "could not read whether the workspace's merge stands off its queue", withField(fields, "error", err.Error()))
		return err
	}
	if found {
		refusal := mergeOffQueueRefusal(record, rec.Cause)
		logRefusal(log, op, besideOffQueue, fields, refusal)
		return refusal
	}
	session, found, err := o.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the requesting workspace's session", withField(fields, "error", err.Error()))
		return err
	}
	if found && session.Terminal != nil && session.Terminal.Kind == "deleted" {
		err := refuse(ArmSessionDeleted, ws, "the session was deleted, so the merge's prompts and repairs can never run")
		logRefusal(log, op, logsite.Msg("refused a merge for a deleted session"), fields, err)
		return err
	}
	repo, err := o.repoKeyFor(ctx, record.Dir)
	if err != nil {
		log.Error(op, "could not resolve the requesting workspace's repository", withField(fields, "error", err.Error()))
		return err
	}
	if _, running := o.runFor(ws); running {
		err := refuse(ArmAlreadyMerging, ws, "this workspace's merge is already in flight")
		logRefusal(log, op, logsite.Msg("refused a merge request while the workspace's merge runs"), fields, err)
		return err
	}
	// AN IN-PLACE MERGE THE AGENT'S TURN ASKED FOR FLUSHES THE HELD QUEUE
	// (owner ruling, 2026-10-12): the merge runs in this session once the
	// turn ends, and what the user sent before then is meant to be handled
	// before the merge, never after it. Every held prompt is sent to the shim
	// at once to join the running turn, nothing interrupted, and the merge
	// waits for the turn that answers them (awaitFree). The user's own ask
	// displaces the turn in flight instead, so it does not flush. A flush that
	// could not read the queue refuses the request before anything of it is
	// recorded; the queue records why.
	if req.By == RequestedByAgent {
		if err := o.deps.Queue.FlushHeld(ctx, ws); err != nil {
			return fmt.Errorf("merge: flushing the held prompts of %s before its in-place merge: %w", ws, err)
		}
	}
	requestMerge := o.deps.DB.RequestMerge
	if req.OneShot {
		requestMerge = o.deps.DB.RequestOneShotMerge
	}
	if err := requestMerge(ctx, repo, ws, source, o.deps.Now()); err != nil {
		var queued *wsm.MergeQueuedError
		if errors.As(err, &queued) {
			arm := ArmAlreadyQueued
			if queued.State == wsm.MergeAdmitted {
				arm = ArmAlreadyMerging
			}
			refusal := refuse(arm, ws, "the merge already stands %s in its repository's queue", queued.State)
			// A TYPED REFUSAL THE CALLER ACTS ON, not a condition to remediate:
			// the merge it asked for already stands, and the answer says so.
			logRefusal(log, op, logsite.Msg("refused a duplicate merge request"), withField(fields, "repo", string(repo)), refusal)
			return refusal
		}
		var offQueue *wsm.MergeOffQueueError
		if errors.As(err, &offQueue) {
			refusal := mergeOffQueueRefusal(record, offQueue.Cause)
			logRefusal(log, op, besideOffQueue, withField(fields, "repo", string(repo)), refusal)
			return refusal
		}
		if wsm.IsFamilyRefusal(err) {
			logRefusal(log, op, openChildren, fields, err)
			return err
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
	// AN ANNOUNCED ONE-SHOT is republished, so the roster draws its one-shot
	// mark.
	if req.OneShot {
		o.publishOneShot(ctx, log, ws)
	}
	if req.By == RequestedByUser && !o.hasFinishStage(deploy, record) {
		// THE USER'S ASK HAS NO REQUESTING TURN: it is put in line at once.
		// A workspace whose finish stage runs hooks waits instead, for any
		// turn in flight to end and then for those hooks: a prompt hook is a
		// turn of the workspace, which cannot run beside one (finish.go).
		return o.queueRequest(ctx, ws, repo)
	}
	o.awaitRequestingTurn(ws, repo)
	return nil
}

// checkAsker refuses an in-place merge of WS asked by a workspace that may not
// land it. The user (an empty ASKER) lands anything, WS always lands itself,
// and so do its owner and the workspace it lands into (wsm.Workspace.MayAct);
// any other workspace is refused (requester_may_not_land).
func checkAsker(ws wsm.Workspace, asker ids.WorkspaceID) error {
	if ws.MayAct(asker) {
		return nil
	}
	return refuse(ArmRequesterMayNotLand, ws.ID, "workspace %s is neither the workspace nor its owner", asker)
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
	case wsm.MergeSourceOwnBranch:
		return o.withCheckedOutBranch(ctx, ws, ws, source)
	case wsm.MergeSourceBranch:
		exists, err := o.deps.Git.BranchExists(ctx, requester.Dir, source.Branch)
		if err != nil {
			return wsm.MergeSource{}, fmt.Errorf("merge: asking whether branch %q exists: %w", source.Branch, err)
		}
		if !exists {
			refusal := refuse(ArmUnknownBranch, ws, "no branch %q exists in the repository", source.Branch)
			refusal.Fields = map[string]any{"branch": source.Branch}
			return wsm.MergeSource{}, refusal
		}
		return source, nil
	default:
		return wsm.MergeSource{}, fmt.Errorf("merge: a request for %s names the undeclared source %s", ws, source.Kind)
	}
}

// worktreeLister is what finding a branch's worktree asks of git.
type worktreeLister interface {
	MainWorktree(ctx context.Context, dir string) (string, error)
	ListWorktrees(ctx context.Context, repoDir string) ([]gitclient.Worktree, error)
}

// worktreeHolding answers the worktree of the repository holding IN that has
// BRANCH checked out (git checks a branch out in at most one), with the
// repository's main worktree, and HELD false when none has it: THE ONE
// LOOKUP of a branch's worktree, which an async merge's cleanup, its rule
// check and a branch merge's subject share.
func worktreeHolding(ctx context.Context, git worktreeLister, in, branch string) (dir, main string, held bool, err error) {
	main, err = git.MainWorktree(ctx, in)
	if err != nil {
		return "", "", false, fmt.Errorf("merge: resolving the repository's main worktree: %w", err)
	}
	worktrees, err := git.ListWorktrees(ctx, main)
	if err != nil {
		return "", "", false, fmt.Errorf("merge: listing the repository's worktrees: %w", err)
	}
	for _, wt := range worktrees {
		if !wt.Bare && wt.Branch == branch {
			return wt.Dir, main, true, nil
		}
	}
	return "", main, false, nil
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
		refusal := refuse(ArmNoBranchCheckedOut, ws, "the worktree %s has no branch checked out (its HEAD is detached), so there is no branch to merge", dir)
		refusal.Fields = map[string]any{"dir": dir}
		return "", refusal
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
	turn, inFlight := o.deps.TurnInFlight(ws)
	o.mu.Lock()
	if inFlight {
		o.requestingTurns[ws] = turn
	} else {
		delete(o.requestingTurns, ws)
	}
	if standing, ok := o.requested[ws]; ok {
		standing.cancel()
	}
	o.requested[ws] = wait
	o.mu.Unlock()
	if o.async {
		o.live.Add(1)
		inflight.Standing(func() {
			defer o.live.Done()
			o.waitThenQueue(wait)
		})
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
	// THE FINISH STAGE RUNS HERE, between the requesting turn's end and the
	// deploy's place in line (finish.go).
	failure, err := o.runFinishStage(ctx, ws, wait.repo)
	if err != nil {
		if ctx.Err() != nil {
			log.Info(op, "the merge request was withdrawn during its finish stage", dlog.Context{"workspace": string(ws), "cause": err.Error()})
			return
		}
		log.Error(op, "the finish stage could not be run; the request stays recorded for the next boot", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return
	}
	if failure != "" {
		o.withdrawFinished(ctx, ws, wait.repo, failure)
		return
	}
	if err := o.queueRequest(ctx, ws, wait.repo); err != nil {
		log.Error(op, "could not put the merge request in line", dlog.Context{"workspace": string(ws), "error": err.Error()})
	}
}

// TakeRequestingTurn implements Orchestrator.
func (o *orchestrator) TakeRequestingTurn(ws ids.WorkspaceID, turn ids.TurnID) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.requestingTurns[ws] != turn {
		return false
	}
	delete(o.requestingTurns, ws)
	return true
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
	_, _, err := o.queueRequestAt(ctx, ws, repo)
	return err
}

// queueRequestAt is queueRequest answering the merge's one-based place in
// line, and false for the place when the daemon is draining: the request then
// stays recorded, and joins the line at the next boot.
func (o *orchestrator) queueRequestAt(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey) (int, bool, error) {
	position, inLine, _, err := o.queueRequestGated(ctx, ws, repo, false)
	return position, inLine, err
}

// queueRequestGated is queueRequestAt that, when MARKATGATE, marks the merge
// as standing at its workspace's cold gate in the same write of the line that
// puts it there, answering whether it did (a requeue, Requeue). AN ENTRY IN
// LINE UNMARKED IS ADMISSIBLE, so a merge whose session is parked would be
// admitted between the two writes. The gate is read under the line lock,
// which its retirement's unmark takes too (unmarkColdGated): a gate passed
// meanwhile is either read as passed here, or unmarks after this.
func (o *orchestrator) queueRequestGated(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, markAtGate bool) (int, bool, bool, error) {
	const op = "daemon.merge.request"
	if o.isDraining() {
		o.log(ctx, ws).Info(op, "the daemon is draining; the merge request stays recorded for the next boot",
			dlog.Context{"workspace": string(ws)})
		return 0, false, false, nil
	}
	entry, found, err := o.requestOf(ctx, repo, ws)
	if err != nil {
		return 0, false, false, err
	}
	if !found {
		err := fmt.Errorf("merge: %s has no recorded request to put in line", ws)
		o.log(ctx, ws).Error(op, "a merge to put in line has no recorded request", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "error": err.Error()})
		return 0, false, false, err
	}
	// THE BUBBLE EXISTS FROM HERE: the ledger identity it is addressed by is
	// minted as the merge takes its place, and written on its row with that
	// place, so republishQueue can draw it -- and the next daemon to serve the
	// workspace draws the same bubble.
	ledger := o.mintLedger(ws)
	position, gated := 0, false
	queue := func() error {
		var err error
		if position, err = o.deps.DB.QueueMerge(ctx, repo, ws, ledger); err != nil || !markAtGate || !o.deps.AtColdGate(ws) {
			return err
		}
		gated, err = o.deps.DB.SetMergeColdGated(ctx, repo, ws)
		return err
	}
	if markAtGate {
		err = o.lineWrite(repo, queue)
	} else {
		err = queue()
	}
	if err != nil {
		o.mu.Lock()
		delete(o.ledgerOf, ws)
		o.mu.Unlock()
		return 0, false, false, err
	}
	o.log(ctx, ws).Info(op, "put a merge in line; it is reported from here", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "position": position})
	o.parkQueued(ctx, op, repo, entry.Source, ws, ledger)
	// A merge put in line raises no banner: only its end does (ended), and
	// desktopnotify.MergeBanners raises one only when it did not land.
	if err := o.republishQueue(ctx, repo); err != nil {
		return 0, false, false, err
	}
	o.kick(repo)
	return position, true, gated, nil
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

// publishOneShot republishes the roster's DURABLE half once a one-shot merge
// request has marked its requester a one-shot, so every surface drawing the
// one-shot mark draws it from the announcement on rather than from whatever
// next refreshes the registry. The request already stands, so a failed
// republish is recorded and the request is not undone.
func (o *orchestrator) publishOneShot(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	o.republishRoster(ctx, log, "daemon.merge.enqueue", logsite.Msg("could not republish the roster after marking the requester a one-shot"), dlog.Context{"workspace": string(ws)})
}

// placeBehind resolves the merge REQ is placed behind, by the name the queue
// lists it under, to its queue workspace. It must stand in the requester's
// queue, waiting or running.
func (o *orchestrator) placeBehind(ctx context.Context, record wsm.Workspace, req Request) (wsm.WorkspaceID, error) {
	repo, err := o.repoKeyFor(ctx, record.Dir)
	if err != nil {
		return "", err
	}
	queue, err := o.readQueue(ctx, o.deps.DB, repo)
	if err != nil {
		return "", err
	}
	if anchor, found := o.memberNamed(queue, req.PlaceBehind); found {
		return anchor, nil
	}
	names := namesInLine(queue)
	return "", behindNotInLine(req.Workspace, req.PlaceBehind, names)
}

// namesMerge reports whether ASKED names the merge standing under NAME. THE
// NAME IS MATCHED AS SUPPLIED AND AS RESOLVED: a merge stands under its
// resolved name (prefixed when a workspace prefix is set), and the agent may
// name it either way.
func (o *orchestrator) namesMerge(name, asked string) bool {
	return name == asked || name == o.deps.Names.MergeBranch(asked)
}

// memberNamed answers the workspace of the merge in line on QUEUE (running or
// waiting) that ASKED names (namesMerge).
func (o *orchestrator) memberNamed(queue QueueSnapshot, asked string) (wsm.WorkspaceID, bool) {
	for _, member := range queue.Members {
		if o.namesMerge(member.Name, asked) {
			return member.Workspace, true
		}
	}
	return "", false
}

// namesInLine answers the names of the merges in line on QUEUE, in line order.
func namesInLine(queue QueueSnapshot) []string {
	names := make([]string, 0, len(queue.Members))
	for _, member := range queue.Members {
		names = append(names, member.Name)
	}
	return names
}

// behindNotInLine is the placement_behind_not_in_line refusal of WS's merge,
// asked to go behind ASKED while only NAMES stand in line.
func behindNotInLine(ws wsm.WorkspaceID, asked string, names []string) *RefusalError {
	refusal := refuse(ArmPlacementBehindNotInLine, ws, "no merge named %q stands in the queue to be placed behind (in line: %s)", asked, strings.Join(names, ", "))
	refusal.Fields = map[string]any{"behind": asked}
	return refusal
}

// parkQueued asks for the park of WS's session once its own branch's merge
// stands in REPO's line under LEDGER (owner requirement, 2026-11-21). The
// enqueue is the park's one owner: it is asked once per merge put in line,
// here and when a boot or a takeover puts a waiting one back (recoverEntry),
// and the drain takes it only while that entry still waits, never under a
// turn (drain.Controller.MergeQueuedDown). Every other merge's session is
// left as it stands.
func (o *orchestrator) parkQueued(ctx context.Context, op string, repo wsm.RepoKey, source wsm.MergeSource, ws ids.WorkspaceID, ledger ids.LeaseID) {
	if !source.ParksWhileQueued() {
		return
	}
	o.log(ctx, ws).Info(op, "asked for the park of the workspace's session while its merge waits in line", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "merge_entry": string(ledger)})
	o.deps.ParkQueued(ws, repo, ledger)
}

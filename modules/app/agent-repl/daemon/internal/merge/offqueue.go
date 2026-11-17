package merge

import (
	"context"
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is the orchestrator's side of a merge OFF ITS QUEUE (owner
// design, 2026-11-12; wsm/offqueue.go is the store's): the merges a queue's
// status lists as having left it without landing, putting one back in line
// as itself (Requeue), ending one (Discard), and refusing an async request
// under a name such a merge holds.

// The refusal arms of a merge off its queue, spelled as the landed
// RequeueMergeError, DiscardMergeError and CreateWorkspaceError arms.
const (
	// ArmUnknownMerge refuses a requeue or discard of a workspace whose merge
	// holds no request, in line or off it.
	ArmUnknownMerge = "unknown_merge"
	// ArmStillQueued refuses a requeue or discard of a merge that stands in
	// line.
	ArmStillQueued = "still_queued"
	// ArmBranchGone refuses a requeue whose branch no longer exists.
	ArmBranchGone = "branch_gone"
	// ArmAlreadyLanded refuses a requeue of a merge that landed.
	ArmAlreadyLanded = "already_landed"
	// ArmVersionAlreadyRecorded refuses a requeue that supplies a version
	// increment for a request that recorded its own.
	ArmVersionAlreadyRecorded = "version_already_recorded"
	// ArmWorkspaceClosed refuses a requeue of a closed workspace's merge.
	ArmWorkspaceClosed = "workspace_closed"
	// ArmNameOffQueue refuses an async request whose name a merge off its
	// queue holds: that merge is requeued, never asked for again.
	ArmNameOffQueue = "async_merge_name_off_queue"
)

// offQueueCauses maps every cause a request is kept off its queue for onto
// its arm of agentrepl.v1.MergeQueueStatusOffQueueCause. It is the ONE
// mapping, read by the status, the sidebar and the async-name refusal alike.
// CauseWorkspaceClosed has none: a closed workspace keeps no request.
var offQueueCauses = map[string]func() *agentreplv1.MergeQueueStatusOffQueueCause{
	string(CauseUserDrop): func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_Evicted{Evicted: &agentreplv1.MergeQueueStatusOffQueueCauseEvicted{}}}
	},
	string(CauseUserDequeue): func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_Dequeued{Dequeued: &agentreplv1.MergeQueueStatusOffQueueCauseDequeued{}}}
	},
	causeFailed: func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_Failed{Failed: &agentreplv1.MergeQueueStatusOffQueueCauseFailed{}}}
	},
	causeRestartFailed: func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_RestartFailed{RestartFailed: &agentreplv1.MergeQueueStatusOffQueueCauseRestartFailed{}}}
	},
	string(CauseDaemonShutdown): func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_DaemonShutdown{DaemonShutdown: &agentreplv1.MergeQueueStatusOffQueueCauseDaemonShutdown{}}}
	},
	causePickFailed: func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_PickFailed{PickFailed: &agentreplv1.MergeQueueStatusOffQueueCausePickFailed{}}}
	},
	causeFinishFailed: func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_FinishFailed{FinishFailed: &agentreplv1.MergeQueueStatusOffQueueCauseFinishFailed{}}}
	},
	string(CauseAheadDidNotLand): func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_AheadDidNotLand{AheadDidNotLand: &agentreplv1.MergeQueueStatusOffQueueCauseAheadDidNotLand{}}}
	},
	causeHiddenAtColdGate: func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_HiddenAtColdGate{HiddenAtColdGate: &agentreplv1.MergeQueueStatusOffQueueCauseHiddenAtColdGate{}}}
	},
	string(CauseAgentGaveUp): func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_GaveUp{GaveUp: &agentreplv1.MergeQueueStatusOffQueueCauseGaveUp{}}}
	},
	wsm.MergeLeftAdopted: func() *agentreplv1.MergeQueueStatusOffQueueCause {
		return &agentreplv1.MergeQueueStatusOffQueueCause{Cause: &agentreplv1.MergeQueueStatusOffQueueCause_Adopted{Adopted: &agentreplv1.MergeQueueStatusOffQueueCauseAdopted{}}}
	},
}

// causeHiddenAtColdGate is the cause a merge leaves its queue for when its
// workspace is hidden at its cold gate, as the cold-gate queue spells it.
const causeHiddenAtColdGate = "hidden_at_cold_gate"

// offQueueWords is the sidebar's one word for every merge off its queue
// (frontend.v1.RosterMergeQueueOffQueueCause), whatever its cause (owner
// request, 2026-11-12): the typed cause stays on the wire and in
// `merge-queue status` (OffQueueCause), and the rail says only "evicted".
const offQueueWords = "evicted"

// OffQueueCause answers CAUSE's arm of agentrepl.v1
// .MergeQueueStatusOffQueueCause. A cause no exit declares is a defect in the
// exit that wrote it, and panics: the store keeps only causes the queue
// spells (wsm.leaveQueueInTx).
func OffQueueCause(cause string) *agentreplv1.MergeQueueStatusOffQueueCause {
	build, declared := offQueueCauses[cause]
	if !declared {
		panic(fmt.Sprintf("merge: a request stands off its queue for the undeclared cause %q", cause))
	}
	return build()
}

// OffQueueWords answers the sidebar's word for CAUSE (offQueueWords),
// panicking for an undeclared cause as OffQueueCause does.
func OffQueueWords(cause string) string {
	if _, declared := offQueueCauses[cause]; !declared {
		panic(fmt.Sprintf("merge: a request stands off its queue for the undeclared cause %q", cause))
	}
	return offQueueWords
}

// OffQueueMember is one merge standing off its queue, as the status and the
// sidebar list it.
type OffQueueMember struct {
	// Workspace is the merge's workspace: the one landing its own branch, or
	// an async merge's merge workspace.
	Workspace ids.WorkspaceID
	// Name is that workspace's name, which the merge was listed under in
	// line.
	Name string
	// Branch is the branch the request lands.
	Branch string
	// Requester is the workspace that asked for an ASYNC merge, empty for a
	// workspace's own.
	Requester ids.WorkspaceID
	// Cause is why it left its queue.
	Cause string
	// LeftAt is when it left.
	LeftAt time.Time
}

// offQueueOf answers every merge of REPOSITORY standing off its queue,
// oldest departure first.
func (o *orchestrator) offQueueOf(ctx context.Context, repository wsm.Repository) ([]OffQueueMember, error) {
	records, err := o.deps.DB.OffQueueMerges(ctx)
	if err != nil {
		return nil, err
	}
	var out []OffQueueMember
	for _, record := range records {
		// A MERGE WITH WORK ON MASTER, OR NO BRANCH, IS LEFT OUT
		// (offqueuelanded.go); its record stands.
		if o.offQueueHidden(record.Workspace) {
			continue
		}
		ws, err := o.deps.DB.Workspace(ctx, record.Workspace)
		if err != nil {
			return nil, fmt.Errorf("merge: reading the workspace of the merge %s off its queue: %w", record.Workspace, err)
		}
		if ws.Repo != repository.ID {
			continue
		}
		member := OffQueueMember{Workspace: ws.ID, Name: ws.Name, Branch: requestBranch(ws, record.Source), Cause: record.Cause, LeftAt: record.LeftAt}
		if ws.MergeOf != nil {
			member.Requester = ws.MergeOf.Requester
		}
		out = append(out, member)
	}
	return out, nil
}

// requestBranch is the branch a request lands: the one it recorded, or for a
// workspace's own branch recorded with none, the workspace's branch.
func requestBranch(ws wsm.Workspace, source wsm.MergeSource) string {
	if source.Branch != "" {
		return source.Branch
	}
	return ws.Branch
}

// Requeue is one request to put a merge off its queue back in line.
type Requeue struct {
	// Workspace is the merge's workspace.
	Workspace ids.WorkspaceID
	// Front places it at the front; PlaceBehind, when set, directly behind
	// the merge of that name. Neither places it at the back.
	Front       bool
	PlaceBehind string
	// Version is the increment of an adopted request, which recorded none.
	Version *wsm.VersionIncrement
	// Model, when set, is the model the merge's session resumes under,
	// recorded in the requeue's own store write. The caller checked it
	// against the login's catalog.
	Model string
	// By is who asked, and Asker the asking workspace when an agent did.
	By    Requester
	Asker ids.WorkspaceID
}

// Requeued is what a requeue answers: the merge's one-based place in line,
// and false for it when the daemon is draining (the request is recorded and
// joins the line at the next boot).
type Requeued struct {
	Position int
	InLine   bool
}

// Requeue puts workspace req.Workspace's merge, standing off its queue, back
// in line AS ITSELF: the same workspace (for an async merge, the same merge
// workspace), branch and request, at the placement asked for. It refuses,
// changing nothing, a merge that holds no request (unknown_merge), stands in
// line (still_queued), landed (already_landed), belongs to a closed workspace
// (workspace_closed), lands a branch that is gone (branch_gone), or whose
// version increment is missing or misplaced.
func (o *orchestrator) Requeue(ctx context.Context, req Requeue) (Requeued, error) {
	const op = "daemon.merge.requeue"
	ws := req.Workspace
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "requested_by": req.By.String(), "front": req.Front, "place_behind": req.PlaceBehind, "version_supplied": req.Version != nil, "model": req.Model}
	if req.Front && req.PlaceBehind != "" {
		err := fmt.Errorf("merge: the requeue of %s is placed at the front and behind %q at once", ws, req.PlaceBehind)
		log.Error(op, "a requeue contradicts itself about its placement", withField(fields, "error", err.Error()))
		return Requeued{}, err
	}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the workspace of the merge to requeue", withField(fields, "error", err.Error()))
		return Requeued{}, err
	}
	rec, err := o.offQueueRecord(ctx, op, log, record, fields)
	if err != nil {
		return Requeued{}, err
	}
	fields["cause"] = rec.Cause
	if req.Version != nil && !rec.Adopted {
		refusal := refuse(ArmVersionAlreadyRecorded, ws, "the request recorded its own version increment, which a requeue keeps")
		logRefusal(log, op, "refused a requeue that would change a recorded version increment", fields, refusal)
		return Requeued{}, refusal
	}
	branch := requestBranch(record, rec.Source)
	fields["branch"] = branch
	exists, err := o.deps.Git.BranchExists(ctx, record.Dir, branch)
	if err != nil {
		log.Error(op, "could not read whether the requeued merge's branch exists", withField(fields, "error", err.Error()))
		return Requeued{}, fmt.Errorf("merge: asking whether branch %q exists: %w", branch, err)
	}
	if !exists {
		refusal := refuse(ArmBranchGone, ws, "no branch %q exists in the repository", branch)
		refusal.Fields = map[string]any{"branch": branch}
		logRefusal(log, op, "refused a requeue whose branch is gone", fields, refusal)
		return Requeued{}, refusal
	}
	source := rec.Source
	if req.Version != nil {
		source.Version = req.Version
	}
	// THE VERSION RULE IS THE REQUEST'S OWN (checkVersion): a recorded
	// request passed it when it was asked for, and an adopted one is checked
	// with the increment it is given now.
	asked := Request{Workspace: ws, Source: source, By: req.By, Asker: req.Asker}
	deploy, err := o.requestDeploy(ctx, record, asked)
	if err != nil {
		if _, refused := Refused(err); refused {
			logRefusal(log, op, "refused a requeue for its repository's deploy config", fields, err)
		} else {
			log.Error(op, "could not read the repository's deploy config", withField(fields, "error", err.Error()))
		}
		return Requeued{}, err
	}
	if err := checkVersion(ws, asked, deploy); err != nil {
		logRefusal(log, op, "refused a requeue for its version increment", fields, err)
		return Requeued{}, err
	}
	place := wsm.RequeuePlacement{Front: req.Front}
	if req.PlaceBehind != "" {
		behind, err := o.placeBehind(ctx, record, Request{Workspace: ws, PlaceBehind: req.PlaceBehind})
		if err != nil {
			if _, refused := Refused(err); refused {
				logRefusal(log, op, "refused a requeue placed behind a merge not in its queue", fields, err)
			} else {
				log.Error(op, "could not read the queue a requeue is placed in", withField(fields, "error", err.Error()))
			}
			return Requeued{}, err
		}
		place.Behind, place.BehindName = behind, req.PlaceBehind
	}
	repo, err := o.repoKeyFor(ctx, record.Dir)
	if err != nil {
		log.Error(op, "could not resolve the requeued merge's repository", withField(fields, "error", err.Error()))
		return Requeued{}, err
	}
	fields["repo"] = string(repo)
	var version *wsm.VersionIncrement
	if rec.Adopted {
		version = req.Version
	}
	if _, err := o.deps.DB.RequeueOffQueueMerge(ctx, repo, ws, place, version, req.Model, o.deps.Now()); err != nil {
		log.Error(op, "could not put the request back on its queue", withField(fields, "error", err.Error()))
		return Requeued{}, err
	}
	o.mu.Lock()
	o.repoOf[ws] = repo
	delete(o.displaces, ws)
	o.mu.Unlock()
	log.Info(op, "put a merge that left its queue back on it, as itself", fields)
	position, inLine, err := o.queueRequestAt(ctx, ws, repo)
	if err != nil {
		return Requeued{}, err
	}
	return Requeued{Position: position, InLine: inLine}, nil
}

// offQueueRecord answers RECORD's request off its queue, or the refusal that
// says why it has none: a closed workspace (landed, or closed otherwise), a
// merge standing in line, or no request at all.
func (o *orchestrator) offQueueRecord(ctx context.Context, op string, log dlog.Logger, record wsm.Workspace, fields dlog.Context) (wsm.OffQueueMerge, error) {
	ws := record.ID
	if record.Closed {
		refusal := refuse(ArmWorkspaceClosed, ws, "the merge's workspace is closed, and a closed workspace holds no request")
		if record.Closure != nil && record.Closure.By == wsm.CloseByMerge {
			refusal = refuse(ArmAlreadyLanded, ws, "the merge landed, and its landing closed its workspace")
		}
		logRefusal(log, op, "refused a closed workspace's merge", fields, refusal)
		return wsm.OffQueueMerge{}, refusal
	}
	rec, found, err := o.deps.DB.OffQueueMergeOf(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the merge's request off its queue", withField(fields, "error", err.Error()))
		return wsm.OffQueueMerge{}, err
	}
	if found {
		return rec, nil
	}
	if _, err := o.queueOf(ctx, ws); err == nil {
		refusal := refuse(ArmStillQueued, ws, "the merge stands in its queue")
		logRefusal(log, op, "refused a merge that stands in its queue", fields, refusal)
		return wsm.OffQueueMerge{}, refusal
	} else if _, refused := Refused(err); !refused {
		log.Error(op, "could not read whether the merge stands in its queue", withField(fields, "error", err.Error()))
		return wsm.OffQueueMerge{}, err
	}
	refusal := refuse(ArmUnknownMerge, ws, "no request of this workspace's merge stands, in line or off it")
	if record.MergedAt != nil {
		refusal = refuse(ArmAlreadyLanded, ws, "the merge landed")
	}
	logRefusal(log, op, "refused a workspace whose merge holds no request", fields, refusal)
	return wsm.OffQueueMerge{}, refusal
}

// Discarded is what a discard did: for an async merge, that it closed the
// merge workspace and whether it removed the worktree its request asked to
// clean up.
type Discarded struct {
	Async                  bool
	CleanupWorktreeRemoved bool
}

// Discard ends workspace WS's request standing off its queue without
// landing it. A workspace's own merge loses only its request. An ASYNC
// merge's merge workspace is closed by CLOSE (the daemon's ordinary close,
// whose transaction ends the request with it, wsm.markClosedInTx), and the
// worktree its request asked to clean up is removed through git's clean
// door, never forced, and no branch is deleted. It refuses a merge in line
// (still_queued) and one holding no request (unknown_merge).
func (o *orchestrator) Discard(ctx context.Context, ws ids.WorkspaceID, close func(context.Context) error) (Discarded, error) {
	const op = "daemon.merge.discard"
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws)}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the workspace of the merge to discard", withField(fields, "error", err.Error()))
		return Discarded{}, err
	}
	// A CLOSED WORKSPACE HOLDS NOTHING TO DISCARD: its close ended any
	// request it held.
	if record.Closed {
		refusal := refuse(ArmUnknownMerge, ws, "the merge's workspace is closed, and a closed workspace holds no request")
		logRefusal(log, op, "refused a discard of a closed workspace's merge", fields, refusal)
		return Discarded{}, refusal
	}
	rec, err := o.offQueueRecord(ctx, op, log, record, fields)
	if err != nil {
		return Discarded{}, err
	}
	fields["cause"] = rec.Cause
	// THE QUEUE IS RESOLVED BEFORE ANYTHING CHANGES: a closed merge
	// workspace's directory may not outlive its close.
	repo, err := o.repoKeyFor(ctx, record.Dir)
	if err != nil {
		log.Error(op, "could not resolve the discarded merge's repository", withField(fields, "error", err.Error()))
		return Discarded{}, err
	}
	if record.MergeOf == nil {
		if err := o.deps.DB.DiscardOffQueueMerge(ctx, ws); err != nil {
			log.Error(op, "could not discard the request", withField(fields, "error", err.Error()))
			return Discarded{}, err
		}
		log.Info(op, "discarded a workspace's own merge off its queue; the workspace, its worktree and its branch stay", fields)
		o.republishOffQueue(ctx, log, record.ID, repo)
		return Discarded{}, nil
	}
	if err := close(ctx); err != nil {
		if _, refused := Refused(err); refused {
			logRefusal(log, op, "the merge workspace's close was refused; the request stands", fields, err)
		} else {
			log.Error(op, "could not close the discarded async merge's merge workspace; the request stands", withField(fields, "error", err.Error()))
		}
		return Discarded{}, err
	}
	out := Discarded{Async: true}
	if origin := record.MergeOf; origin.CleanupWorktree && origin.CleanupDir != "" {
		out.CleanupWorktreeRemoved = o.cleanupDiscarded(ctx, log, record, origin.CleanupDir)
	}
	log.Info(op, "discarded an async merge off its queue and closed its merge workspace", withField(fields, "cleanup_worktree_removed", out.CleanupWorktreeRemoved))
	o.republishOffQueue(ctx, log, record.ID, repo)
	return out, nil
}

// cleanupDiscarded removes DIR, the worktree a discarded async merge's request
// asked to clean up, through git's clean door, and answers whether it did. A
// worktree holding uncommitted changes is kept, and a removal that fails is
// recorded: the discard itself stands either way.
func (o *orchestrator) cleanupDiscarded(ctx context.Context, log dlog.Logger, record wsm.Workspace, dir string) bool {
	const op = "daemon.merge.discard"
	fields := dlog.Context{"workspace": string(record.ID), "worktree": dir}
	clean, err := o.deps.Git.IsClean(ctx, dir)
	if err != nil {
		log.Error(op, "could not read whether the worktree to clean up is clean; it is kept", withField(fields, "error", err.Error()))
		return false
	}
	if !clean {
		log.Info(op, "kept the worktree the discarded merge asked to clean up: it holds uncommitted changes", fields)
		return false
	}
	main, err := o.deps.Git.MainWorktree(ctx, record.Dir)
	if err != nil {
		log.Error(op, "could not resolve the repository of the worktree to clean up; it is kept", withField(fields, "error", err.Error()))
		return false
	}
	if err := o.deps.Git.RemoveCleanWorktree(ctx, main, dir); err != nil {
		log.Error(op, "could not remove the worktree the discarded merge asked to clean up", withField(fields, "error", err.Error()))
		return false
	}
	log.Info(op, "removed the worktree the discarded merge asked to clean up", fields)
	return true
}

// republishOffQueue republishes REPO's queue, so the status' subscribers and
// the sidebar stop listing workspace WS's discarded merge. A failed republish
// is recorded; the discard stands.
func (o *orchestrator) republishOffQueue(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, repo wsm.RepoKey) {
	const op = "daemon.merge.discard"
	if err := o.republishQueue(ctx, repo); err != nil {
		log.Error(op, "could not republish the queue after a discard", dlog.Context{"workspace": string(ws), "repo": string(repo), "error": err.Error()})
	}
}

// mergeOffQueueRefusal refuses a merge request from RECORD, whose own merge
// stands off its queue for CAUSE: that request is requeued or discarded,
// never asked for again.
func mergeOffQueueRefusal(record wsm.Workspace, cause string) *RefusalError {
	refusal := refuse(ArmMergeOffQueue, record.ID, "the merge left its queue without landing (%s) and its request stands; requeue it with `agent-repld merge-queue requeue %s`, or discard it with `agent-repld merge-queue discard %s` before asking again", cause, record.Name, record.Name)
	refusal.Fields = map[string]any{"cause": OffQueueCause(cause)}
	return refusal
}

// refuseNameOffQueue refuses an async request under NAME when a merge off its
// queue in REQUESTER's repository holds it: that merge is requeued as
// itself, never asked for again. Nil when no such merge holds the name.
func (o *orchestrator) refuseNameOffQueue(ctx context.Context, requester wsm.Workspace, name string) error {
	records, err := o.deps.DB.OffQueueMerges(ctx)
	if err != nil {
		return fmt.Errorf("merge: reading the merges off their queues: %w", err)
	}
	for _, rec := range records {
		ws, err := o.deps.DB.Workspace(ctx, rec.Workspace)
		if err != nil {
			return fmt.Errorf("merge: reading the workspace of the merge %s off its queue: %w", rec.Workspace, err)
		}
		if ws.Repo != requester.Repo || ws.Name != name {
			continue
		}
		refusal := refuse(ArmNameOffQueue, requester.ID, "the merge %q left its queue without landing and its request stands; requeue it with `agent-repld merge-queue requeue %s`", name, name)
		refusal.Fields = map[string]any{"name": name, "workspace": ws.Ref(), "cause": OffQueueCause(rec.Cause)}
		return refusal
	}
	return nil
}

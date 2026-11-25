package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"agent-repld/internal/dlog"
	"agent-repld/internal/holdfold"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/worktreereap"
	"agent-repld/internal/wsm"
)

// Open opens a registered workspace: its worktree restored when gone, its
// closed flag cleared. IT STARTS NO SHIM (owner ruling, 2026-10-28): the
// conversation is read from the store, and the next prompt brings the shim
// up; a parked workspace stays parked until then.
//
// It is idempotent: opening an open workspace changes nothing.
// reportOpenStage relays one stage to an Open's progress reporter, if the
// caller set one. An open with no reporter (every non-client caller) emits
// nothing.
func reportOpenStage(progress OpenProgress, stage OpenStage) {
	if progress != nil {
		progress.Stage(stage)
	}
}

func (v *verbs) Open(ctx context.Context, ws ids.WorkspaceID, progress OpenProgress) error {
	// ONE OPEN OF A WORKSPACE AT A TIME, AND NONE BESIDE ITS SETTLE
	// (dirGates). Two opens raced on 2026-11-25 (webapp-reload-one-page,
	// opened twice from one mention): the first was cutting the workspace's
	// worktree again, the second's stat found the directory `git worktree
	// add` had just made, skipped the restore and its binding, and answered
	// "opened" -- so Emacs drew the tab and the topbar met its first frame
	// with no directory bound. Under the gate, the record is read only once
	// every earlier open has finished, so a second open finds the workspace
	// opened, bound and named, and changes nothing.
	defer v.dirGates.Lock(ws)()
	record, log, err := v.owned(ctx, "OpenWorkspace", ws)
	if err != nil {
		return err
	}

	// A WORKSPACE WHOSE DIRECTORY IS GONE IS RESTORED FROM ITS BRANCH. Boot
	// CLOSES such a row (internal/boot's closeMissingDirs), but the row is
	// still the user's: picking it means "give me this workspace back", and
	// when its branch survives in the repository that is exactly what a
	// `git worktree add <dir> <branch>` does. The session and its history
	// then come back from the store as for any closed workspace. Only when
	// there is nothing to restore from — the branch is gone too, or the
	// repository is — is the open REFUSED, by name, saying so.
	//
	// A STAT THAT DOES NOT SAY "NOT EXIST" IS NEVER READ AS GONE, the same
	// discipline boot applies: "could not tell" is not an answer, and
	// restoring or refusing on it would act on a workspace that is merely
	// unreachable this instant.
	// A CLOSED CHILD IS RE-OPENED ONLY UNDER AN OPEN PARENT (wsm/family.go).
	// Asked before anything is restored or brought up, so a refused open
	// starts nothing; the re-open below checks again in its own transaction,
	// which is the guarantee.
	if record.Closed && record.Parent != nil {
		if err := v.requireOpenParent(ctx, log, opOpen, *record.Parent); err != nil {
			return fmt.Errorf("open %q: %w", ws, err)
		}
	}
	reportOpenStage(progress, OpenStageCheckingWorktree)
	if _, statErr := os.Stat(record.Dir); errors.Is(statErr, fs.ErrNotExist) {
		restored, err := v.restoreWorktree(ctx, log, record, progress, "OpenWorkspace")
		if err != nil {
			return err
		}
		log = restored
	}
	// A MERGED WORKSPACE COMES BACK TO BE REVISED, so it comes back at its
	// parent's tip, where the work it landed now lives (advanceMerged).
	if record.MergedAt != nil {
		if err := v.advanceMerged(ctx, log, record); err != nil {
			return err
		}
	}

	// AN OPEN RECONCILES; IT DOES NOT TRUST THE LIVENESS IT READ. The
	// workspace's session record may still read `killed` from a
	// KillWorkspace weeks earlier, the roster RECEDES a killed session's
	// row, and Emacs gives a tab only to a row that is not receded. The
	// open leaves the record saying what the fleet says, the check and the
	// write one step, so a stop's end cannot fall between them.
	if _, err := v.deps.Sessions.ReconcileLiveTerminal(ctx, log, opOpen, ws); err != nil {
		return fmt.Errorf("open %q: %w", ws, err)
	}

	// A MERGED ROW IS REOPENED EVEN WHEN IT IS NOT CLOSED: the merge queue
	// stamps merged_at before it closes the row, so a crash can leave an open
	// one, and an open workspace is never merged (wsm.Reopen).
	if record.Closed || record.MergedAt != nil {
		reportOpenStage(progress, OpenStageClearingClosed)
		if err := v.deps.DB.Reopen(ctx, ws, v.deps.Instance); err != nil {
			if refusedByFamily(log, opOpen, err) {
				return err
			}
			log.Error(opOpen, "could not clear the closed flag", dlog.Context{
				"merged": record.MergedAt != nil, "cause": err.Error()})
			return fmt.Errorf("open %q: clear closed: %w", ws, err)
		}
		if record.MergedAt != nil {
			// THE LANDED MERGE'S STANDING STATE GOES WITH ITS SETTLEMENT: the
			// workspace moved on, and its row and footer would otherwise go on
			// drawing it merged.
			v.deps.Merge.RetireConcluded(ctx, ws)
			log.Info(opOpen, "reopened a merged workspace; its merge's settlement is retired", dlog.Context{
				"merged_at": record.MergedAt.UTC(), "branch": record.Branch})
		} else {
			log.Debug(opOpen, "cleared the closed flag", nil)
		}
	}

	// A close refusal is drawn in the footer; re-opening retires it, because
	// the state it described is gone.
	v.deps.Footer.SetClosing(ws, nil)

	// The build-staleness check belongs to the mount: a workspace coming up
	// against a shim older than the installed build goes to the bounce
	// registry now — bounced at once when free, when its work ends otherwise —
	// rather than discovering the mismatch mid-turn. The mount never waits on
	// the bounce.
	reportOpenStage(progress, OpenStageCheckingBuild)
	check, err := v.deps.Rollout.CheckStaleness(ctx, ws, false)
	switch {
	case err != nil:
		// NOT A FAILED MOUNT: the session is up and usable on the build it
		// has. The judgement that could not be made is the rollout's own loud
		// record too.
		log.Error(opOpen, "the build-staleness check could not judge the shim", dlog.Context{"cause": err.Error()})
	case check.Stale:
		log.Info(opOpen, "the shim runs an older build; it went to the bounce registry", dlog.Context{
			"reported_build": check.Reported, "installed_build": check.Installed,
			"bounce_now": check.Bounce.Now, "skipped": check.Skipped,
		})
	default:
		log.Debug(opOpen, "the shim runs the installed build", nil)
	}

	// A CLOSED ONE-SHOT REOPENED IS THE USER'S AGAIN: it no longer finishes
	// unattended, so its mark goes. Cleared once the row is open, because a
	// closed workspace's standing is not asked about.
	if record.Closed {
		if err := v.TakeBackOneShot(ctx, ws, TakenBackByReopen); err != nil {
			return fmt.Errorf("open %q: %w", ws, err)
		}
	}

	log.Info(opOpen, "opened the workspace", dlog.Context{"dir": record.Dir})
	v.republishRegistry(ctx, log, opOpen)
	return nil
}

// closeBlocker names why a close is refused, or nil when the workspace is
// quiet. The four blockers are the ruled ones; a standing cold gate and a
// parked session are deliberately NOT among them.
//
// EVERY BLOCKER IS COMPUTED, not just the first one that fires: the refusal
// carries all four counts as evidence (CloseWorkspaceBlocked, landing 7) and
// the footer draws the same composed sentence, so the check answers the whole
// picture and the ORDER below only decides which one the sentence leads with.
func (v *verbs) closeBlocker(ctx context.Context, ws ids.WorkspaceID) (*footer.CloseBlocked, error) {
	return v.blockedClose(ctx, ws, false)
}

// blockedClose is closeBlocker's judgment. OWNTURN discounts the turn in
// flight: a workspace closing itself asks from inside that turn, so the turn
// is never what blocks its close (CloseSelf).
func (v *verbs) blockedClose(ctx context.Context, ws ids.WorkspaceID, ownTurn bool) (*footer.CloseBlocked, error) {
	blocked := footer.CloseBlocked{}
	var liveDetail string
	if running, live := v.deps.Freeness(ws); live {
		blocked.TurnInFlight = running.Turn != nil && !ownTurn
		agents, shells := len(running.LiveWork.Agents), len(running.LiveWork.Shells)
		blocked.LiveWork = uint32(agents + shells)
		liveDetail = fmt.Sprintf("%d detached agents and %d detached shells are still live", agents, shells)
	}
	holds, err := v.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("read the held prompts: %w", err)
	}
	// ONLY WHAT THE HELD SECTION SHOWS BLOCKS (holdfold.Drawn): the refusal
	// asks the user to release or drop them, which a notice drawn nowhere
	// (a merge queue or workspace notice to the agent) never lets them do.
	var held []wsm.HeldPrompt
	for _, h := range holds {
		if holdfold.Drawn(h) {
			held = append(held, h)
		}
	}
	blocked.HeldPrompts = uint32(len(held))
	mergeState := ""
	if facts, ok := v.deps.Merge.Facts(ws); ok && mergeIsPending(facts.State) {
		blocked.MergeQueued = true
		mergeState = facts.State
	}
	// THE QUEUE'S OWN ROW STANDS FOR THE MERGE TOO: a merge this daemon has
	// published no facts for (one a pause suspended mid-step, one a restart
	// or handover left on its queue) still holds its queue's line, and a
	// workspace closed under it leaves the queue credited to a workspace whose
	// session can no longer run the step (2026-10-27,
	// remove-analyze-position-skill).
	if !blocked.MergeQueued {
		entry, standing, err := queuedMergeOf(ctx, v.deps.DB, ws)
		if err != nil {
			return nil, fmt.Errorf("read the merge queues: %w", err)
		}
		if standing {
			blocked.MergeQueued = true
			mergeState = entry.State.String() + " on its queue"
		}
	}

	switch {
	case blocked.TurnInFlight:
		blocked.Reason = "turn_in_flight"
		blocked.Detail = "a turn is still running; interrupt it or wait for it to end"
	case blocked.LiveWork > 0:
		blocked.Reason = "live_work"
		blocked.Detail = liveDetail
	case blocked.HeldPrompts > 0:
		blocked.Reason = "held_prompts"
		blocked.Detail = fmt.Sprintf("%d held prompts have not been delivered; release or drop them first", len(held))
	case blocked.MergeQueued:
		blocked.Reason = "merge_queued"
		blocked.Detail = fmt.Sprintf("a merge is %s; evict it from the queue first", mergeState)
	default:
		return nil, nil
	}
	return &blocked, nil
}

// queuedMergeOf answers the merge queue entry WS's own merge holds on any
// repository's queue, false when it holds none.
func queuedMergeOf(ctx context.Context, db wsm.DB, ws ids.WorkspaceID) (wsm.MergeQueueEntry, bool, error) {
	queues, err := db.AllMergeQueues(ctx)
	if err != nil {
		return wsm.MergeQueueEntry{}, false, err
	}
	for _, entries := range queues {
		for _, entry := range entries {
			if entry.Workspace == ws {
				return entry, true, nil
			}
		}
	}
	return wsm.MergeQueueEntry{}, false, nil
}

// mergeIsPending reports whether a merge state still owes the workspace work.
// A merge that landed or failed is finished and blocks nothing.
func mergeIsPending(state string) bool {
	switch state {
	case "enqueuing", "queued", "merging", "conflict":
		return true
	default:
		return false
	}
}

// RestoreMissingWorktree is restoreWorktree for SettleMissingDirectory: no
// open is in flight, so there is no progress to report, and an unrestorable
// workspace is an answer (false) rather than a refusal. op names the step
// that asked, for the records.
func (v *verbs) RestoreMissingWorktree(ctx context.Context, op string, ws wsm.Workspace) (bool, error) {
	log := v.deps.Log.WorkspaceOrCentral(ws.Dir).With(dlog.Context{"workspace": string(ws.ID)})
	// A MERGED WORKSPACE'S WORKTREE WAS REMOVED ON PURPOSE. The merge queue
	// stamps merged_at before it closes the row and removes the tree, so the
	// stamp alone -- even on a row a crash left open -- says the directory's
	// absence is the merge's doing. Only the user's own open brings a merged
	// workspace back; the daemon never does.
	if ws.MergedAt != nil {
		log.Info(op, "the merged workspace's directory is gone on purpose; the daemon does not restore it", dlog.Context{
			"dir": ws.Dir, "branch": ws.Branch, "merged_at": ws.MergedAt.UTC()})
		return false, nil
	}
	_, err := v.restoreWorktree(ctx, log, ws, nil, op)
	if _, refused := AsRefusal(err); refused {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return true, nil
}

// restoreWorktree checks a workspace's recorded branch out at its recorded
// directory again, for an open that found the directory gone. It REFUSES
// (ArmWorktreeUnrestorable) when there is nothing to restore from, and fails
// LOUDLY on any git that could not answer or act: a restore that did not
// happen is never passed over to a bring-up in a directory that is not there.
//
// It answers the workspace's logger AFTER the restore: the one the open began
// with resolved while the directory was gone, so it files centrally, and the
// rest of the open belongs in the restored workspace's own log.
//
// rpc names who asked, for the refusal's record: the OpenWorkspace rpc, or the
// boot's missing-directory step.
func (v *verbs) restoreWorktree(ctx context.Context, log dlog.Logger, record wsm.Workspace, progress OpenProgress, rpc string) (dlog.Logger, error) {
	unrestorable := func(detail string) error {
		return refuseUnrestorable(log, rpc, record, detail)
	}
	// A CHAT THREAD'S DIRECTORY HOLDS NOTHING BUT ITS RECORDS: its graph is
	// zettel's and its transcript the store's. So it is made again, with no
	// branch to restore and no repository to ask.
	if record.IsThread() {
		if err := os.MkdirAll(record.Dir, 0o755); err != nil {
			log.Error(opOpen, "could not make the thread's directory again", dlog.Context{"dir": record.Dir, "cause": err.Error()})
			return nil, fmt.Errorf("open %q: make the thread's directory %q: %w", record.ID, record.Dir, err)
		}
		log = v.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(record.ID)})
		log.Info(opOpen, "the thread's directory was gone; it was made again", dlog.Context{"dir": record.Dir})
		if err := v.bindResolvers(ctx, log, record); err != nil {
			return nil, fmt.Errorf("open %q: bind the thread's views: %w", record.ID, err)
		}
		if err := v.publishNaming(ctx, log, record); err != nil {
			log.Error(opOpen, "could not publish the thread's naming", dlog.Context{"dir": record.Dir, "cause": err.Error()})
			return nil, fmt.Errorf("open %q: publish the thread's naming: %w", record.ID, err)
		}
		return log, nil
	}
	// A MERGED WORKSPACE IS RESTORED LIKE ANY OTHER, opening one being the
	// user taking it back to revise it; its landed branch, deleted at its
	// landing, is cut again at its parent's tip below.
	if record.Branch == "" {
		return nil, unrestorable(fmt.Sprintf(
			"the workspace's directory %s is gone and no branch was recorded to restore it from", record.Dir))
	}
	// A LANDED WORKSPACE MOVED OFF ITS DIRECTORY (wsm.VacatedDir) is not
	// restored: a later create took its name, its branch and its directory.
	if wsm.IsVacatedDir(record.Dir) {
		return nil, unrestorable(fmt.Sprintf(
			"the workspace landed and a later workspace now holds its directory and name (it was moved to %s); create a new workspace to revise its work", record.Dir))
	}
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		log.Error(opOpen, "could not read the repositories to restore the workspace's worktree in", dlog.Context{
			"dir": record.Dir, "branch": record.Branch, "cause": err.Error(),
		})
		return nil, fmt.Errorf("open %q: restore the worktree: list the repositories: %w", record.ID, err)
	}
	repository, found := wsm.RepositoryWithID(repositories, record.Repo)
	if !found {
		log.Error(opOpen, "the workspace's repository is not registered; its worktree cannot be restored", dlog.Context{
			"dir": record.Dir, "branch": record.Branch, "repository_id": string(record.Repo),
		})
		return nil, fmt.Errorf("open %q: restore the worktree: repository %q is not registered", record.ID, record.Repo)
	}
	repoDir := repository.Dir
	// THE REPOSITORY ITSELF MAY BE GONE (a top-level workspace IS its main
	// worktree, and a repository can be deleted under its workspaces): no git
	// can be asked in a directory that is not there, and nothing is left to
	// restore from.
	if _, statErr := os.Stat(repoDir); errors.Is(statErr, fs.ErrNotExist) {
		return nil, unrestorable(fmt.Sprintf(
			"the workspace's directory %s is gone and so is its repository %s; there is nothing to restore it from",
			record.Dir, repoDir))
	}
	exists, err := v.deps.Git.BranchExists(ctx, repoDir, record.Branch)
	if err != nil {
		log.Error(opOpen, "could not tell whether the workspace's branch still exists", dlog.Context{
			"dir": record.Dir, "branch": record.Branch, "repository": repoDir, "cause": err.Error(),
		})
		return nil, fmt.Errorf("open %q: probe branch %q: %w", record.ID, record.Branch, err)
	}
	// A MERGED WORKSPACE'S BRANCH WAS DELETED ON PURPOSE when it landed
	// (landedbranch.Retire): its work lives in its parent branch, which is
	// where a reopened merged workspace starts anyway (advanceMerged). So its
	// branch is cut again at the parent's tip.
	recut := false
	if !exists && record.MergedAt != nil && record.ParentBranch != "" {
		parentExists, err := v.deps.Git.BranchExists(ctx, repoDir, record.ParentBranch)
		if err != nil {
			log.Error(opOpen, "could not tell whether the merged workspace's parent branch still exists", dlog.Context{
				"dir": record.Dir, "branch": record.Branch, "parent_branch": record.ParentBranch, "repository": repoDir, "cause": err.Error(),
			})
			return nil, fmt.Errorf("open %q: probe parent branch %q: %w", record.ID, record.ParentBranch, err)
		}
		recut = parentExists
	}
	if !exists && !recut {
		return nil, unrestorable(fmt.Sprintf(
			"the workspace's directory %s is gone and its branch %s no longer exists in %s; there is nothing to restore it from",
			record.Dir, record.Branch, repoDir))
	}

	reportOpenStage(progress, OpenStageRestoringWorktree)
	if err := v.clearStaleRegistration(ctx, log, record, repoDir); err != nil {
		return nil, err
	}
	restored := "restored its worktree from its branch"
	if recut {
		restored = "cut its landed branch again at its parent branch's tip"
		if err := v.deps.Git.CreateWorktree(ctx, repoDir, record.Branch, record.ParentBranch, record.Dir); err != nil {
			log.Error(opOpen, "could not cut the merged workspace's branch again at its parent branch's tip", dlog.Context{
				"dir": record.Dir, "branch": record.Branch, "parent_branch": record.ParentBranch, "repository": repoDir, "cause": err.Error(),
			})
			return nil, fmt.Errorf("open %q: cut %q again at %q: %w", record.ID, record.Branch, record.ParentBranch, err)
		}
	} else if err := v.deps.Git.RestoreWorktree(ctx, repoDir, record.Dir, record.Branch); err != nil {
		log.Error(opOpen, "could not restore the workspace's worktree from its branch", dlog.Context{
			"dir": record.Dir, "branch": record.Branch, "repository": repoDir, "cause": err.Error(),
		})
		return nil, fmt.Errorf("open %q: restore %q from %q: %w", record.ID, record.Dir, record.Branch, err)
	}
	log = v.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(record.ID)})
	log.Info(opOpen, "the workspace's directory was gone; {restored}", dlog.Context{"restored": restored,
		"dir": record.Dir, "branch": record.Branch, "parent_branch": record.ParentBranch, "repository": repoDir,
	})
	v.reportPreserved(ctx, log, record, repoDir)
	// THE VIEWS BOOT PASSED OVER ARE BOUND NOW. Boot binds the resolvers and
	// publishes the topbar's naming only for a workspace whose directory
	// exists (BindViews, PublishRegistry), so a restored one has neither, and
	// its session's first footer and topbar records would meet a resolver
	// that does not know where the workspace is.
	if err := v.bindResolvers(ctx, log, record); err != nil {
		return nil, fmt.Errorf("open %q: bind the restored workspace's views: %w", record.ID, err)
	}
	if err := v.publishNaming(ctx, log, record); err != nil {
		log.Error(opOpen, "could not publish the restored workspace's naming", dlog.Context{
			"dir": record.Dir, "branch": record.Branch, "cause": err.Error(),
		})
		return nil, fmt.Errorf("open %q: publish the restored workspace's naming: %w", record.ID, err)
	}
	return log, nil
}

// reportPreserved says where the work of a restored workspace's expired
// worktree is. The reaper removes a closed workspace's worktree after its
// idle window and keeps the branch, so the restore above brings back only
// what was committed; whatever else the tree held is in the preservation
// commits under the workspace's own ref prefix (worktreereap.PreservedRefs),
// which this names at INFO. Nothing is reapplied: the commits stay until
// somebody reads them.
//
// A LISTING GIT COULD NOT ANSWER IS AN ERROR RECORD AND NOT A FAILED OPEN:
// the worktree is restored and usable, and the refs are still there to list.
func (v *verbs) reportPreserved(ctx context.Context, log dlog.Logger, record wsm.Workspace, repoDir string) {
	prefix := worktreereap.PreservedRefs(record.ID)
	refs, err := v.deps.Git.ListRefs(ctx, repoDir, prefix)
	if err != nil {
		log.Error(opOpen, "could not list what the restored workspace's expired worktree preserved", dlog.Context{
			"dir": record.Dir, "branch": record.Branch, "repository": repoDir, "ref_prefix": prefix, "cause": err.Error(),
		})
		return
	}
	if len(refs) == 0 {
		log.Debug(opOpen, "the restored workspace has no expired worktree's preservation", dlog.Context{
			"repository": repoDir, "ref_prefix": prefix})
		return
	}
	log.Info(opOpen, "the restored workspace's expired worktree was preserved; its uncommitted work is in these commits, and none of it was reapplied", dlog.Context{
		"dir": record.Dir, "branch": record.Branch, "repository": repoDir, "preserved_refs": refs,
		"newest_preserved_ref": refs[len(refs)-1],
	})
}

// advanceMerged fast-forwards a merged workspace being reopened to its parent
// branch's tip. Its branch landed there, so the branch's tip is inside the
// parent and the advance carries nothing away: the reopened workspace starts
// from the code as it stands now, which is what a revision of landed work is
// made against, and its next merge carries only the revision.
//
// A WORKSPACE THE ADVANCE CANNOT CARRY STAYS AT ITS BRANCH'S TIP, and says why
// at INFO: one that names no parent branch, one whose parent branch is gone,
// one holding uncommitted changes, and one whose branch has commits its parent
// does not (it moved on after landing). Nothing here ever moves a branch any
// way but forward. A git that could not answer or act fails the open.
func (v *verbs) advanceMerged(ctx context.Context, log dlog.Logger, record wsm.Workspace) error {
	return v.advanceToParent(ctx, log, record, opOpen, "open", "reopened merged workspace")
}

// advanceToParent fast-forwards RECORD's worktree to its parent branch's
// tip, recording under OP, its failure naming VERB and its records naming
// SUBJECT. A worktree with no parent branch recorded, a parent branch gone,
// uncommitted changes, or commits the parent does not hold stays at its
// branch's tip, and says why: nothing is ever discarded to advance it.
func (v *verbs) advanceToParent(ctx context.Context, log dlog.Logger, record wsm.Workspace, op, verb, subject string) error {
	fields := dlog.Context{"dir": record.Dir, "branch": record.Branch, "parent_branch": record.ParentBranch}
	stays := func(why string) error {
		fields["why"] = why
		log.With(fields).Info(op, "the {subject} stays at its branch's tip", dlog.Context{"subject": subject})
		return nil
	}
	fail := func(what string, err error) error {
		fields["cause"] = err.Error()
		log.With(fields).Error(op, "could not advance the {subject} to its parent's tip: {what}", dlog.Context{"subject": subject, "what": what})
		return fmt.Errorf("%s %q: advance the %s to %q: %s: %w", verb, record.ID, subject, record.ParentBranch, what, err)
	}
	kept, failed, err := v.parentHoldsAll(ctx, record)
	if err != nil {
		return fail(failed, err)
	}
	if kept != "" {
		return stays(kept)
	}
	if err := v.deps.Git.FastForward(ctx, record.Dir, record.ParentBranch); err != nil {
		return fail("fast-forward", err)
	}
	log.With(fields).Info(op, "advanced the {subject} to its parent's tip", dlog.Context{"subject": subject})
	return nil
}

// clearStaleRegistration retires the registration git still holds for the
// workspace's missing directory, when it holds one. A directory deleted with
// `rm` rather than `git worktree remove` leaves `.git/worktrees/<name>`
// behind, and `git worktree add` then refuses the path as "a missing but
// already registered worktree". ONLY THAT ONE REGISTRATION is retired
// (gitclient.UnregisterMissingWorktree): `git worktree prune` would retire
// every missing registration in the repository, and `add -f` would also
// override git's refusal to check out a branch another worktree holds.
//
// A LOCKED REGISTRATION IS NOT OVERRIDDEN: somebody locked that tree on
// purpose (a removable volume, a tree another tool owns), so the restore
// fails loudly, naming the lock, rather than forcing past it.
func (v *verbs) clearStaleRegistration(ctx context.Context, log dlog.Logger, record wsm.Workspace, repoDir string) error {
	worktrees, err := v.deps.Git.ListWorktrees(ctx, repoDir)
	if err != nil {
		log.Error(opOpen, "could not list the repository's worktrees before restoring", dlog.Context{
			"dir": record.Dir, "branch": record.Branch, "repository": repoDir, "cause": err.Error(),
		})
		return fmt.Errorf("open %q: list the worktrees of %q: %w", record.ID, repoDir, err)
	}
	for _, wt := range worktrees {
		if !samePath(wt.Dir, record.Dir) {
			continue
		}
		if wt.Locked {
			log.Error(opOpen, "the workspace's missing worktree is LOCKED in git; it is not forced", dlog.Context{
				"dir": record.Dir, "branch": record.Branch, "repository": repoDir, "lock_reason": wt.LockedReason,
			})
			return fmt.Errorf("open %q: the missing worktree %s is locked in git (%q); unlock it to restore it",
				record.ID, record.Dir, wt.LockedReason)
		}
		if err := v.deps.Git.UnregisterMissingWorktree(ctx, repoDir, wt.Dir); err != nil {
			log.Error(opOpen, "could not retire the missing worktree's stale registration", dlog.Context{
				"dir": record.Dir, "branch": record.Branch, "repository": repoDir, "cause": err.Error(),
			})
			return fmt.Errorf("open %q: retire the stale registration of %q: %w", record.ID, wt.Dir, err)
		}
		log.Info(opOpen, "retired the stale git registration of the workspace's missing worktree", dlog.Context{
			"dir": record.Dir, "registered_dir": wt.Dir, "registered_branch": wt.Branch, "repository": repoDir,
		})
		return nil
	}
	return nil
}

// samePath reports whether two spellings name one path, at least one of
// which no longer exists: equal once cleaned, or equal once each one's PARENT
// is resolved through its symlinks (git may print `/private/var/...` for the
// `/var/...` the registry recorded, and the leaf itself cannot be resolved).
func samePath(a, b string) bool {
	a, b = filepath.Clean(a), filepath.Clean(b)
	if a == b {
		return true
	}
	return resolvedParent(a) == resolvedParent(b)
}

// resolvedParent answers a path with its parent's symlinks resolved, or the
// path itself when the parent cannot be resolved either.
func resolvedParent(path string) string {
	parent, err := filepath.EvalSymlinks(filepath.Dir(path))
	if err != nil {
		return path
	}
	return filepath.Join(parent, filepath.Base(path))
}

// parentHoldsAll judges whether RECORD's worktree holds nothing its parent
// branch lacks: a parent branch recorded and present, a clean worktree, and
// a HEAD inside the parent branch. It answers why it does not (KEPT), empty
// when it does; and FAILED, what could not be read, with the git error.
// Both a reopened merged workspace's advance (advanceMerged) and the release
// of a name a closed workspace holds (releaseMergedHolder) ask it, so
// the package asks git about ancestry in this one place.
func (v *verbs) parentHoldsAll(ctx context.Context, record wsm.Workspace) (kept, failed string, err error) {
	if record.ParentBranch == "" {
		return "no parent branch was recorded", "", nil
	}
	exists, err := v.deps.Git.BranchExists(ctx, record.Dir, record.ParentBranch)
	if err != nil {
		return "", "probe the parent branch", err
	}
	if !exists {
		return "the parent branch no longer exists", "", nil
	}
	clean, err := v.deps.Git.IsClean(ctx, record.Dir)
	if err != nil {
		return "", "read whether the worktree is clean", err
	}
	if !clean {
		return "the worktree holds uncommitted changes", "", nil
	}
	inside, err := v.deps.Git.IsAncestor(ctx, record.Dir, "HEAD", record.ParentBranch)
	if err != nil {
		return "", "read whether the branch is inside its parent", err
	}
	if !inside {
		return "the branch holds commits its parent does not", "", nil
	}
	return "", "", nil
}

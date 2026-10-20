package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// Open brings a registered workspace's session up. MOUNTING A PARKED
// WORKSPACE'S FRONTEND IS AN IMPLICIT REVIVAL: there is no shim-less read path,
// so opening spawns rather than serving history from anywhere else.
//
// It is idempotent: opening a workspace whose session is already live clears
// the closed flag and returns, because the mount it answers has already
// happened.
// reportOpenStage relays one stage to an Open's progress reporter, if the
// caller set one. An open with no reporter (every non-client caller) emits
// nothing.
func reportOpenStage(progress OpenProgress, stage OpenStage) {
	if progress != nil {
		progress.Stage(stage)
	}
}

func (v *verbs) Open(ctx context.Context, ws ids.WorkspaceID, progress OpenProgress) error {
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

	// A PARKED WORKSPACE IS REVIVED THROUGH THE SAME PATH, and the park is
	// LIFTED by it. The start below would have spawned the session either way
	// — that is the implicit revival this verb has always performed — but
	// nothing retracted the sweep's park from the session-scoped views, so the
	// topbar went on drawing the hibernated strip over a live session until
	// the shim's first link state happened to arrive.
	asleep, err := v.parked(ctx, ws)
	if err != nil {
		log.Error(opOpen, "could not tell whether the workspace was hibernated", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("open %q: %w", ws, err)
	}
	if v.deps.Sessions.Live(ws) {
		log.Debug(opOpen, "the session is already live", nil)
		// AN OPEN RECONCILES; IT DOES NOT TRUST THE LIVENESS IT READ. A live
		// session is the reason this verb starts nothing — and it is also the
		// reason the record is never revisited on this path, which is how the
		// open came to answer in under two milliseconds and leave the user
		// with nothing: the workspace's session record still read `killed`
		// from a KillWorkspace weeks earlier, the roster RECEDES a killed
		// session's row, and Emacs gives a tab only to a row that is not
		// receded. The open now leaves the record saying what the fleet says.
		if err := retireTerminalRecord(ctx, log, v.deps.DB, opOpen, ws); err != nil {
			return fmt.Errorf("open %q: %w", ws, err)
		}
	} else {
		// THE SLOW STAGE. Reported only when a bring-up actually runs: a
		// session already live waited for nothing, and a stage announcing
		// work that is not happening is worse than no stage at all.
		reportOpenStage(progress, OpenStageStartingSession)
		if err := v.deps.Sessions.Start(ctx, ws); err != nil {
			// A DEPARTING DAEMON IS NOT A SESSION THAT FAILED TO COME UP. The
			// bring-up refused before it spawned because nothing would be left
			// to own the shim, and the successor opens the workspace from the
			// same record; the caller still gets the refusal, which is what it
			// acts on.
			if errors.Is(err, shimclient.ErrStandingDown) {
				log.Info(opOpen, "the session was not brought up: this daemon is standing down", nil)
			} else {
				log.Error(opOpen, "the session did not come up", dlog.Context{"cause": err.Error()})
			}
			return fmt.Errorf("open %q: start the session: %w", ws, err)
		}
	}
	if asleep {
		reportOpenStage(progress, OpenStageReviving)
		v.unpark(ws)
		log.Info(opOpen, "revived the hibernated workspace", nil)
	}

	// A MERGED ROW IS REOPENED EVEN WHEN IT IS NOT CLOSED: the merge queue
	// stamps merged_at before it closes the row, so a crash can leave an open
	// one, and an open workspace is never merged (wsm.Reopen).
	if record.Closed || record.MergedAt != nil {
		reportOpenStage(progress, OpenStageClearingClosed)
		if err := v.deps.DB.Reopen(ctx, ws); err != nil {
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
	held, err := v.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("read the held prompts: %w", err)
	}
	blocked.HeldPrompts = uint32(len(held))
	mergeState := ""
	if facts, ok := v.deps.Merge.Facts(ws); ok && mergeIsPending(facts.State) {
		blocked.MergeQueued = true
		mergeState = facts.State
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

// RestoreMissingWorktree is restoreWorktree for the boot's missing-directory
// step: no open is in flight, so there is no progress to report, and an
// unrestorable workspace is an answer (false) rather than a refusal.
func (v *verbs) RestoreMissingWorktree(ctx context.Context, ws wsm.Workspace) (bool, error) {
	log := v.deps.Log.WorkspaceOrCentral(ws.Dir).With(dlog.Context{"workspace": string(ws.ID)})
	// A MERGED WORKSPACE'S WORKTREE WAS REMOVED ON PURPOSE. The merge queue
	// stamps merged_at before it closes the row and removes the tree, so the
	// stamp alone -- even on a row a crash left open -- says the directory's
	// absence is the merge's doing. Only the user's own open brings a merged
	// workspace back; the boot never does.
	if ws.MergedAt != nil {
		log.Info("daemon.boot.close_missing_dir", "the merged workspace's directory is gone on purpose; the boot does not restore it", dlog.Context{
			"dir": ws.Dir, "branch": ws.Branch, "merged_at": ws.MergedAt.UTC()})
		return false, nil
	}
	_, err := v.restoreWorktree(ctx, log, ws, nil, "daemon.boot.close_missing_dir")
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
	// A MERGED WORKSPACE IS RESTORED LIKE ANY OTHER: the merge keeps its
	// branch, and opening one is the user taking it back to revise it.
	if record.Branch == "" {
		return nil, unrestorable(fmt.Sprintf(
			"the workspace's directory %s is gone and no branch was recorded to restore it from", record.Dir))
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
	if !exists {
		return nil, unrestorable(fmt.Sprintf(
			"the workspace's directory %s is gone and its branch %s no longer exists in %s; there is nothing to restore it from",
			record.Dir, record.Branch, repoDir))
	}

	reportOpenStage(progress, OpenStageRestoringWorktree)
	if err := v.clearStaleRegistration(ctx, log, record, repoDir); err != nil {
		return nil, err
	}
	if err := v.deps.Git.RestoreWorktree(ctx, repoDir, record.Dir, record.Branch); err != nil {
		log.Error(opOpen, "could not restore the workspace's worktree from its branch", dlog.Context{
			"dir": record.Dir, "branch": record.Branch, "repository": repoDir, "cause": err.Error(),
		})
		return nil, fmt.Errorf("open %q: restore %q from %q: %w", record.ID, record.Dir, record.Branch, err)
	}
	log = v.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(record.ID)})
	log.Info(opOpen, "the workspace's directory was gone; restored its worktree from its branch", dlog.Context{
		"dir": record.Dir, "branch": record.Branch, "repository": repoDir,
	})
	// THE VIEWS BOOT PASSED OVER ARE BOUND NOW. Boot binds the resolvers and
	// publishes the topbar's naming only for a workspace whose directory
	// exists (BindViews, PublishRegistry), so a restored one has neither, and
	// its session's first footer and topbar records would meet a resolver
	// that does not know where the workspace is.
	if err := v.bindResolvers(log, record.ID, record.Dir); err != nil {
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
	fields := dlog.Context{"dir": record.Dir, "branch": record.Branch, "parent_branch": record.ParentBranch}
	stays := func(why string) error {
		fields["why"] = why
		log.Info(opOpen, "the reopened merged workspace stays at its branch's tip", fields)
		return nil
	}
	fail := func(what string, err error) error {
		fields["cause"] = err.Error()
		log.Error(opOpen, "could not advance the reopened merged workspace to its parent's tip: "+what, fields)
		return fmt.Errorf("open %q: advance the merged workspace to %q: %s: %w", record.ID, record.ParentBranch, what, err)
	}
	if record.ParentBranch == "" {
		return stays("no parent branch was recorded")
	}
	exists, err := v.deps.Git.BranchExists(ctx, record.Dir, record.ParentBranch)
	if err != nil {
		return fail("probe the parent branch", err)
	}
	if !exists {
		return stays("the parent branch no longer exists")
	}
	clean, err := v.deps.Git.IsClean(ctx, record.Dir)
	if err != nil {
		return fail("read whether the worktree is clean", err)
	}
	if !clean {
		return stays("the worktree holds uncommitted changes")
	}
	inside, err := v.deps.Git.IsAncestor(ctx, record.Dir, "HEAD", record.ParentBranch)
	if err != nil {
		return fail("read whether the branch is inside its parent", err)
	}
	if !inside {
		return stays("the branch holds commits its parent does not")
	}
	if err := v.deps.Git.FastForward(ctx, record.Dir, record.ParentBranch); err != nil {
		return fail("fast-forward", err)
	}
	log.Info(opOpen, "advanced the reopened merged workspace to its parent's tip", fields)
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

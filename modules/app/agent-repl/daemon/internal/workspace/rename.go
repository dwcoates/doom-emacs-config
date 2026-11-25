package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strings"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/rollout"
	"agent-repld/internal/wsm"
)

// rpcSetName is the rpc a rename's refusals are spelled under.
const rpcSetName = "SetWorkspaceName"

// opRenameMove is the operation a rename's move is recorded under.
const opRenameMove = "daemon.workspace.rename_move"

// The rename's refusals of a move (agentrepl.v1.SetWorkspaceNameError).
const (
	// ArmMerging refuses a rename of a workspace a merge holds or names.
	ArmMerging = "merging"
	// ArmBranchPublished refuses a rename of a workspace whose branch tracks
	// a remote branch, carrying its `upstream`.
	ArmBranchPublished = "branch_published"
	// ArmBranchExists refuses a rename to a name a git branch no workspace
	// holds already has, carrying the `branch`.
	ArmBranchExists = "branch_exists"
	// ArmDirectoryExists refuses a rename whose worktree directory already
	// exists, carrying the `dir`.
	ArmDirectoryExists = "directory_exists"
	// ArmMoving refuses a rename while the workspace's previous rename moves
	// its branch and directory.
	ArmMoving = "moving"
)

// MoveState is where a rename's move stands (SetWorkspaceNameMove).
type MoveState int

// The states of a rename's move.
const (
	// MoveNone moves nothing: the branch and the directory carry the name
	// already, or are the repository's own.
	MoveNone MoveState = iota
	// MoveOwed is recorded and runs at the workspace's next freeness.
	MoveOwed
	// MoveMoved ran before the answer.
	MoveMoved
)

// Renamed is a rename's outcome.
type Renamed struct {
	// Name is the name the workspace now has.
	Name string
	// Move is where the move of its branch and directory stands.
	Move MoveState
	// To is where the branch and the directory move, set unless Move is
	// MoveNone.
	To wsm.MoveTarget
}

// SetName renames WS (agentrepl.v1.SetWorkspaceName).
//
// A RENAME IS TOTAL (owner ruling, 2026-11-25): the workspace's branch and its
// worktree directory move with its name, so its old name, branch and
// directory are held by nothing afterwards. The branch is the name; the
// directory is the one a create of that branch would get (WorktreeDir). A
// repository's main checkout, and a workspace that is no repository's
// worktree, keep their branch and directory: they are not the workspace's to
// move.
//
// THE NAME CHANGES AT ONCE; THE MOVE RUNS WHEN NOTHING SERVES THE SESSION.
// The name and the owed move are recorded in one write (wsm.Rename). A
// closed workspace is moved before the answer. An open one's move is a shim
// replacement (rollout.ReasonRename) the bounce registry runs once the
// workspace falls free, holding its prompts while it moves.
//
// The requested name follows a supplied create name's rule and takes the
// same prefix (ValidateSuppliedName, suppliedBranch), save a mediator's own
// role's name (renamedTo). It is refused when
// another workspace holds it as its name or branch, or a pending reservation
// or claim holds it (holdersLocked); a move is refused while a merge names the
// workspace, when its branch tracks a remote, and when the branch or the
// directory it moves to already exists. The checks and the write happen
// under the naming lock, so no create or async merge can take the name
// between them.
func (v *verbs) SetName(ctx context.Context, ws ids.WorkspaceID, requested string) (Renamed, error) {
	record, log, err := v.owned(ctx, rpcSetName, ws)
	if err != nil {
		return Renamed{}, err
	}
	supplied := strings.TrimSpace(requested)
	name, err := renamedTo(log, record, supplied)
	if err != nil {
		return Renamed{}, err
	}
	fields := dlog.Context{"name": name, "previous": record.Name, "branch": record.Branch}
	target, repoDir, err := v.moveTarget(ctx, log, record, name, fields)
	if err != nil {
		return Renamed{}, err
	}
	if name == record.Name && target == nil {
		log.Info(opSetName, "the workspace already has the name; nothing changed", fields)
		return Renamed{Name: name}, nil
	}
	if err := v.recordName(ctx, log, record, repoDir, name, target, fields); err != nil {
		return Renamed{}, err
	}
	log.Info(opSetName, "renamed the workspace", fields)
	v.publishRename(ctx, log, ws, name)
	if target == nil {
		return Renamed{Name: name}, nil
	}
	renamed := Renamed{Name: name, Move: MoveOwed, To: *target}
	if record.Closed {
		// NOTHING SERVES A CLOSED WORKSPACE, so its move runs now.
		if err := v.ApplyOwedMove(ctx, ws); err != nil {
			return Renamed{}, fmt.Errorf("rename %q: move it: %w", ws, err)
		}
		renamed.Move = MoveMoved
		return renamed, nil
	}
	v.requestMove(ctx, log, ws)
	return renamed, nil
}

// renamedTo is the name SUPPLIED renames RECORD to. A mediator renamed to
// its own role's name takes it as it stands (wsm.MediatorRole.Name), which
// the naming rule refuses every other workspace (IsStandingName); any other
// name follows a supplied create name's rule and takes the same prefix.
func renamedTo(log dlog.Logger, record wsm.Workspace, supplied string) (string, error) {
	if record.IsMediator() && supplied == record.Mediator.Name() {
		return supplied, nil
	}
	if err := refuseSuppliedName(log, rpcSetName, supplied); err != nil {
		return "", err
	}
	return suppliedBranch(supplied), nil
}

// moveTarget answers where RECORD's rename to NAME moves its branch and
// directory, and its repository's directory, or nil when nothing moves: a
// workspace that is no repository's worktree, its repository's main
// checkout, or one whose branch and directory carry the name already with no
// move owed. A move already owed is always retargeted, even back to where
// the workspace stands.
func (v *verbs) moveTarget(ctx context.Context, log dlog.Logger, record wsm.Workspace, name string, fields dlog.Context) (*wsm.MoveTarget, string, error) {
	if record.Kind != wsm.WorkspaceKindRepository {
		return nil, "", nil
	}
	repoDir, err := v.repoDirOf(ctx, record)
	if err != nil {
		log.Error(opSetName, "could not resolve the repository of the workspace to rename", dlog.WithCause(fields, err))
		return nil, "", fmt.Errorf("rename %q: %w", record.ID, err)
	}
	if wsm.IsMainWorktree(record.Dir, repoDir) {
		return nil, repoDir, nil
	}
	dir, err := WorktreeDir(repoDir, name)
	if err != nil {
		log.Error(opSetName, "could not derive the worktree directory of the new name", dlog.WithCause(fields, err))
		return nil, "", fmt.Errorf("rename %q: %w", record.ID, err)
	}
	_, owed, err := v.deps.DB.OwedMoveOf(ctx, record.ID)
	if err != nil {
		log.Error(opSetName, "could not read whether a move is owed the workspace", dlog.WithCause(fields, err))
		return nil, "", fmt.Errorf("rename %q: %w", record.ID, err)
	}
	if !owed && record.Branch == name && record.Dir == dir {
		return nil, repoDir, nil
	}
	return &wsm.MoveTarget{Branch: name, Dir: dir}, repoDir, nil
}

// recordName checks NAME, and the move to TARGET when one is set, are free
// to RECORD and records them, under the naming lock.
func (v *verbs) recordName(ctx context.Context, log dlog.Logger, record wsm.Workspace, repoDir, name string, target *wsm.MoveTarget, fields dlog.Context) error {
	ws := record.ID
	v.names.mu.Lock()
	defer v.names.mu.Unlock()
	held, err := v.holdersLocked(ctx, "")
	if err != nil {
		log.Error(opSetName, "could not read the names already taken; the workspace is not renamed", dlog.WithCause(fields, err))
		return fmt.Errorf("rename %q: %w", ws, err)
	}
	if holder, taken := held[name]; taken && holder.Workspace != ws {
		return v.nameTaken(ctx, log, name, holder)
	}
	if target != nil {
		if err := v.refuseMove(ctx, log, record, repoDir, *target, fields); err != nil {
			return err
		}
	}
	err = v.deps.DB.Rename(ctx, ws, name, target)
	if errors.Is(err, wsm.ErrMoveInFlight) {
		return refuseWith(log, rpcSetName, ArmMoving, fmt.Sprintf("workspace %s is moving its branch and directory for its previous rename", ws), false, nil)
	}
	if err != nil {
		log.Error(opSetName, "could not record the workspace's new name", dlog.WithCause(fields, err))
		return fmt.Errorf("rename %q: %w", ws, err)
	}
	return nil
}

// refuseMove answers the refusal of RECORD's move to TARGET, or nil when it
// may move: a merge that holds or names the workspace, a branch that tracks a
// remote one, and a branch or directory already standing where it moves.
func (v *verbs) refuseMove(ctx context.Context, log dlog.Logger, record wsm.Workspace, repoDir string, target wsm.MoveTarget, fields dlog.Context) error {
	merging, err := v.merging(ctx, record)
	if err != nil {
		log.Error(opSetName, "could not read whether a merge names the workspace; it is not renamed", dlog.WithCause(fields, err))
		return fmt.Errorf("rename %q: %w", record.ID, err)
	}
	if merging {
		return refuseWith(log, rpcSetName, ArmMerging, fmt.Sprintf("a merge holds or names workspace %s", record.ID), false, nil)
	}
	exists, err := v.deps.Git.BranchExists(ctx, repoDir, record.Branch)
	if err != nil {
		log.Error(opSetName, "could not read whether the workspace's branch exists; it is not renamed", dlog.WithCause(fields, err))
		return fmt.Errorf("rename %q: %w", record.ID, err)
	}
	if exists {
		upstream, tracks, err := v.deps.Git.Upstream(ctx, repoDir, record.Branch)
		if err != nil {
			return fmt.Errorf("rename %q: read the branch's upstream: %w", record.ID, err)
		}
		if tracks {
			return refuseWith(log, rpcSetName, ArmBranchPublished, fmt.Sprintf("branch %s tracks %s", record.Branch, upstream.Ref), false,
				map[string]any{"upstream": upstream.Ref})
		}
	}
	if target.Branch != record.Branch {
		taken, err := v.deps.Git.BranchExists(ctx, repoDir, target.Branch)
		if err != nil {
			log.Error(opSetName, "could not read whether the new branch exists; the workspace is not renamed", dlog.WithCause(fields, err))
			return fmt.Errorf("rename %q: %w", record.ID, err)
		}
		if taken {
			return refuseWith(log, rpcSetName, ArmBranchExists, fmt.Sprintf("branch %s already exists", target.Branch), false,
				map[string]any{"branch": target.Branch})
		}
	}
	if target.Dir != record.Dir {
		present, err := pathPresent(target.Dir)
		if err != nil {
			log.Error(opSetName, "could not read whether the new worktree directory exists; the workspace is not renamed", dlog.WithCause(fields, err))
			return fmt.Errorf("rename %q: %w", record.ID, err)
		}
		if present {
			return refuseWith(log, rpcSetName, ArmDirectoryExists, fmt.Sprintf("%s already exists", target.Dir), false,
				map[string]any{"dir": target.Dir})
		}
	}
	return nil
}

// merging reports whether a merge holds or names RECORD: it is a merge's own
// workspace, its merge stands in a queue, a merge holds its lease, or an
// async merge waits to be made of its branch.
func (v *verbs) merging(ctx context.Context, record wsm.Workspace) (bool, error) {
	if record.MergeOf != nil || record.MergePending {
		return true, nil
	}
	lease, held, err := v.deps.DB.Lease(ctx, record.ID)
	if err != nil {
		return false, fmt.Errorf("read the lease: %w", err)
	}
	if held && lease.Holder == wsm.HolderMerge {
		return true, nil
	}
	requests, err := v.deps.DB.AllAsyncMergeRequests(ctx)
	if err != nil {
		return false, fmt.Errorf("read the async merge requests: %w", err)
	}
	for _, request := range requests {
		if request.Source.Branch == record.Branch || request.Source.Workspace == record.ID {
			return true, nil
		}
	}
	return false, nil
}

// requestMove asks the bounce registry for WS's move, which runs once the
// workspace falls free. A workspace moving to another daemon is that daemon's
// to move: it takes the owed move up when it adopts the workspace. Any other
// refusal is recorded, and the move stays owed for the next daemon that
// serves the workspace.
func (v *verbs) requestMove(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	detached := context.WithoutCancel(ctx)
	decision, err := v.deps.Rollout.BounceShim(detached, ws, rollout.ReasonRename, false, nil, func(err error) {
		switch bounce.OutcomeOf(err) {
		case bounce.OutcomeFailed:
			log.Error(opRenameMove, "the rename's move failed; it stays owed for the next daemon that serves the workspace", dlog.Context{"cause": err.Error()})
		default:
			log.Info(opRenameMove, "the rename's move ended", dlog.Context{"outcome": fmt.Sprint(bounce.OutcomeOf(err))})
		}
	})
	if errors.Is(err, bounce.ErrMovedAway) {
		log.Info(opRenameMove, "the workspace is moving to another daemon; that daemon moves it once it adopts it", nil)
		return
	}
	if err != nil {
		log.Error(opRenameMove, "the bounce registry refused the rename's move; it stays owed for the next daemon that serves the workspace", dlog.Context{"cause": err.Error()})
		return
	}
	log.Info(opRenameMove, "asked for the rename's move", dlog.Context{"now": decision.Now, "turn_in_flight": decision.TurnInFlight, "already_registered": decision.AlreadyPending})
}

// ApplyOwedMove implements Verbs. The owed move is marked moving before its
// first step, so a crash part way is finished at the next boot
// (FinishMovingMoves). The steps are the worktree (with the session's
// `.claude` state inside it), the branch and the vendor's project folder,
// each skipped when neither its source nor its destination exists. The
// records move last, in one write (wsm.CompleteMove), and every surface that
// draws the workspace's directory is republished.
func (v *verbs) ApplyOwedMove(ctx context.Context, ws ids.WorkspaceID) error {
	log := v.deps.Log.Global().With(dlog.Context{dlog.KeyWorkspaceID: string(ws)})
	m, owed, err := v.deps.DB.OwedMoveOf(ctx, ws)
	if err != nil {
		log.Error(opRenameMove, "could not read the move owed the workspace", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("move %q: %w", ws, err)
	}
	if !owed {
		log.Debug(opRenameMove, "no move is owed the workspace", nil)
		return nil
	}
	fields := dlog.Context{"from_branch": m.From.Branch, "from_dir": m.From.Dir, "to_branch": m.To.Branch, "to_dir": m.To.Dir}
	if !m.Moving {
		if _, err := v.deps.DB.MarkMoving(ctx, ws); err != nil {
			log.Error(opRenameMove, "could not record that the move begins; nothing is moved", dlog.WithCause(fields, err))
			return fmt.Errorf("move %q: %w", ws, err)
		}
	}
	record, err := v.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(opRenameMove, "could not read the workspace to move", dlog.WithCause(fields, err))
		return fmt.Errorf("move %q: %w", ws, err)
	}
	repoDir, err := v.repoDirOf(ctx, record)
	if err != nil {
		log.Error(opRenameMove, "could not resolve the repository of the workspace to move", dlog.WithCause(fields, err))
		return fmt.Errorf("move %q: %w", ws, err)
	}
	if err := v.moveOnDisk(ctx, log, repoDir, m, fields); err != nil {
		return fmt.Errorf("move %q: %w", ws, err)
	}
	if _, _, err := v.deps.DB.CompleteMove(ctx, ws); err != nil {
		log.Error(opRenameMove, "the branch and directory moved but the records could not take them; the next boot finishes the move", dlog.WithCause(fields, err))
		return fmt.Errorf("move %q: %w", ws, err)
	}
	log.Info(opRenameMove, "moved the workspace's branch and directory", fields)
	v.republishRegistry(ctx, log, opRenameMove)
	v.deps.Host.PublishHostWorkspace(ws)
	return nil
}

// moveOnDisk moves M's worktree, branch and vendor project folder from
// REPODIR. Each step is skipped when its source and destination are one, or
// when neither exists; the git steps are idempotent, so a move a crash
// interrupted resumes where it stopped.
func (v *verbs) moveOnDisk(ctx context.Context, log dlog.Logger, repoDir string, m wsm.OwedMove, fields dlog.Context) error {
	if m.From.Dir != m.To.Dir {
		from, err := pathPresent(m.From.Dir)
		if err != nil {
			log.Error(opRenameMove, "could not read whether the worktree to move exists", dlog.WithCause(fields, err))
			return err
		}
		to, err := pathPresent(m.To.Dir)
		if err != nil {
			log.Error(opRenameMove, "could not read whether the worktree's destination exists", dlog.WithCause(fields, err))
			return err
		}
		if from || to {
			if err := v.deps.Git.MoveWorktree(ctx, repoDir, m.From.Dir, m.To.Dir); err != nil {
				return err
			}
		}
	}
	if m.From.Branch != m.To.Branch {
		from, err := v.deps.Git.BranchExists(ctx, repoDir, m.From.Branch)
		if err != nil {
			log.Error(opRenameMove, "could not read whether the branch to rename exists", dlog.WithCause(fields, err))
			return err
		}
		to, err := v.deps.Git.BranchExists(ctx, repoDir, m.To.Branch)
		if err != nil {
			log.Error(opRenameMove, "could not read whether the renamed branch exists", dlog.WithCause(fields, err))
			return err
		}
		if from || to {
			if err := v.deps.Git.RenameBranch(ctx, repoDir, m.From.Branch, m.To.Branch); err != nil {
				return err
			}
		}
	}
	if m.From.Dir != m.To.Dir {
		if err := v.deps.Accounts.MoveProject(ctx, m.From.Dir, m.To.Dir); err != nil {
			return err
		}
	}
	return nil
}

// MoveOwed implements Verbs: whether a rename owes WS a move not yet under
// way (rollout.MoveApplier).
func (v *verbs) MoveOwed(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	m, owed, err := v.deps.DB.OwedMoveOf(ctx, ws)
	if err != nil {
		return false, fmt.Errorf("read the move owed %q: %w", ws, err)
	}
	return owed && !m.Moving, nil
}

// FinishMovingMoves implements Verbs: every owed move that had stood its
// workspace's shim down is finished, so the boot reads each workspace at the
// branch and directory it actually has. One that cannot be finished is
// recorded and stays owed for the next boot; the rest are still finished.
func (v *verbs) FinishMovingMoves(ctx context.Context) error {
	moves, err := v.deps.DB.OwedMoves(ctx)
	if err != nil {
		v.deps.Log.Global().Error(opRenameMove, "could not list the owed moves to finish", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("finish the moving moves: %w", err)
	}
	for _, m := range moves {
		if !m.Moving {
			continue
		}
		if err := v.ApplyOwedMove(ctx, m.Workspace); err != nil {
			continue
		}
		v.deps.Log.Global().Info(opRenameMove, "finished a move a stop had interrupted", dlog.Context{dlog.KeyWorkspaceID: string(m.Workspace)})
	}
	return nil
}

// pathPresent reports whether PATH exists.
func pathPresent(path string) (bool, error) {
	switch _, err := os.Lstat(path); {
	case err == nil:
		return true, nil
	case errors.Is(err, fs.ErrNotExist):
		return false, nil
	default:
		return false, err
	}
}

// nameTaken is the name_taken refusal of NAME, naming what holds it: the
// registered workspace, by its ref, or the nested `pending` arm.
func (v *verbs) nameTaken(ctx context.Context, log dlog.Logger, name string, holder nameHolder) error {
	if holder.Workspace == "" {
		return refuseWith(log, rpcSetName, ArmNameTaken,
			fmt.Sprintf("the name %q is reserved by a pending async merge or claimed by a create in flight", name), false,
			map[string]any{"name": name, "holder": refusalarm.Nested("pending")})
	}
	ref := &workspacev1.WorkspaceRef{Id: string(holder.Workspace)}
	if record, err := v.deps.DB.Workspace(ctx, holder.Workspace); err == nil {
		ref.Dir = record.Dir
	} else {
		// The refusal stands on the id alone; the directory is the ref's
		// courtesy, and the read that failed is recorded where it failed.
		log.Error(opSetName, "could not read the workspace holding the name; the refusal names it by id", dlog.Context{
			"name": name, "holder": string(holder.Workspace), "cause": err.Error()})
	}
	return refuseWith(log, rpcSetName, ArmNameTaken,
		fmt.Sprintf("the name %q is already workspace %s's name or branch", name, holder.Workspace), false,
		map[string]any{"name": name, "workspace": ref})
}

// publishRename republishes every surface that draws WS's name, once the new
// NAME is recorded:
//
//   - the roster, which renames the Emacs tab in place by its id;
//   - the topbar's workspace-name line;
//   - the host view, whose naming slug names the workspace's Emacs buffers;
//   - the footer's awaiting line of every workspace that awaits WS;
//   - the merge queues WS stands in;
//   - the feed, whose rows naming WS draw its name (feed.Resolver
//     SetWorkspaceName).
//
// The rename is recorded before any of these runs, so a surface that cannot
// be republished is recorded at ERROR by its owner and the rest still are.
func (v *verbs) publishRename(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, name string) {
	v.republishRegistry(ctx, log, opSetName)
	v.deps.Topbar.SetName(ws, name)
	v.deps.Feed.SetWorkspaceName(ws, name)
	v.deps.Host.PublishHostWorkspace(ws)
	v.relabelAwaiters(ctx, log, ws, name)
	if err := v.deps.Merge.Relabel(ctx, ws); err != nil {
		log.Debug(opSetName, "the merge queue could not relabel the renamed workspace; its owner recorded why", dlog.Context{
			"name": name, "cause": err.Error()})
	}
}

// relabelAwaiters tells the footer of every workspace awaiting WS the
// awaited workspace's new NAME, as SetCorrespondence told it the old one.
func (v *verbs) relabelAwaiters(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, name string) {
	records, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opSetName, "could not list the workspaces awaiting the renamed one; their footers keep its old name", dlog.Context{
			"name": name, "cause": err.Error()})
		return
	}
	for _, record := range records {
		if awaitsRenamed(record, ws) {
			v.deps.Footer.SetAwaiting(record.ID, name)
		}
	}
}

// awaitsRenamed reports whether RECORD awaits WS.
func awaitsRenamed(record wsm.Workspace, ws ids.WorkspaceID) bool {
	return record.Awaiting != nil && *record.Awaiting == ws
}

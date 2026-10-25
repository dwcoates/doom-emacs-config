package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strings"
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// This file is the ONE place a workspace name is HANDED OUT, so a name an async
// merge reserves can never be taken before its merge workspace is created
// under it (docs/protobuf-design/sync-async-merges.md).
//
// A name is taken when it is a registered workspace's name or branch, a branch of the
// repository, a standing async request's reservation (durable, on its queue
// row), an occupied worktree directory (worktreeOccupied), or CLAIMED in this
// process: a create holds a claim on the name it
// chose from the moment it chose it until it is registered or failed, and an
// async request holds one from its check until its reservation is written.
// Choosing and claiming happen under one lock, so two allocations can never
// both hand out one name.

// nameClaims is the process's claimed names, by branch.
type nameClaims struct {
	mu      sync.Mutex
	claimed map[string]bool
}

// suppliedBranch is the branch a SUPPLIED name resolves to: taken as it stands
// when it already carries a prefix component, prefixed otherwise. A create's
// supplied name and an async merge's name resolve alike.
func suppliedBranch(name string) string {
	if strings.Contains(name, "/") {
		return name
	}
	return Name(Prefix(), name)
}

// claim marks BRANCH claimed and answers its release, which is idempotent.
// The caller holds v.names.mu.
func (v *verbs) claimLocked(branch string) func() {
	if v.names.claimed == nil {
		v.names.claimed = map[string]bool{}
	}
	v.names.claimed[branch] = true
	var once sync.Once
	return func() {
		once.Do(func() {
			v.names.mu.Lock()
			delete(v.names.claimed, branch)
			v.names.mu.Unlock()
		})
	}
}

// nameHolder is what holds a taken name: a registered workspace, or, when
// Workspace is empty, a standing reservation or a claim whose workspace is
// not registered yet.
type nameHolder struct {
	Workspace ids.WorkspaceID
}

// holdersLocked answers every branch name that is taken without asking git,
// and what holds it: the registered workspaces' names and branches
// (registeredHolders), the standing reservations, and the claims. EXCEPT
// names a reservation the given workspace itself holds, which is free to the
// fork that reservation was made for. A registered holder outranks a pending
// one for the same name. The caller holds v.names.mu.
func (v *verbs) holdersLocked(ctx context.Context, except ids.WorkspaceID) (map[string]nameHolder, error) {
	registered, err := v.registeredHolders(ctx)
	if err != nil {
		return nil, err
	}
	reserved, err := v.deps.DB.ReservedMergeNames(ctx)
	if err != nil {
		return nil, fmt.Errorf("read the reserved merge names: %w", err)
	}
	held := make(map[string]nameHolder, len(registered)+len(reserved)+len(v.names.claimed))
	for name, holder := range reserved {
		if except != "" && holder == except {
			continue
		}
		held[suppliedBranch(name)] = nameHolder{}
	}
	for branch := range v.names.claimed {
		held[branch] = nameHolder{}
	}
	for name, workspace := range registered {
		held[name] = nameHolder{Workspace: workspace}
	}
	return held, nil
}

// takenLocked answers holdersLocked's names, for the allocations that need
// only whether a name is taken. The caller holds v.names.mu.
func (v *verbs) takenLocked(ctx context.Context, except ids.WorkspaceID) (map[string]bool, error) {
	held, err := v.holdersLocked(ctx, except)
	if err != nil {
		return nil, err
	}
	taken := make(map[string]bool, len(held))
	for name := range held {
		taken[name] = true
	}
	return taken, nil
}

// ClaimMergeName checks an async merge's name and claims it (merge.Names).
func (v *verbs) ClaimMergeName(ctx context.Context, repoDir, name string) (string, func(), bool, error) {
	log := v.deps.Log.Global()
	branch := suppliedBranch(name)
	fields := dlog.Context{"name": name, "branch": branch, "repo_dir": repoDir}
	v.names.mu.Lock()
	defer v.names.mu.Unlock()
	taken, err := v.takenLocked(ctx, "")
	if err != nil {
		log.Error(opNaming, "could not read the names already taken; the async merge's name is not checked", dlog.WithCause(fields, err))
		return "", nil, false, err
	}
	if taken[branch] {
		log.Info(opNaming, "an async merge's name is already taken", fields)
		return branch, nil, true, nil
	}
	exists, err := v.deps.Git.BranchExists(ctx, repoDir, branch)
	if err != nil {
		log.Error(opNaming, "could not ask whether the async merge's branch exists", dlog.WithCause(fields, err))
		return "", nil, false, fmt.Errorf("probe the branch %q: %w", branch, err)
	}
	if exists {
		log.Info(opNaming, "an async merge's name is already a branch", fields)
		return branch, nil, true, nil
	}
	main, err := v.deps.Git.MainWorktree(ctx, repoDir)
	if err != nil {
		log.Error(opNaming, "could not resolve the repository the async merge's worktree would be cut in", dlog.WithCause(fields, err))
		return "", nil, false, fmt.Errorf("resolve the main worktree of %q: %w", repoDir, err)
	}
	occupied, dir, err := worktreeOccupied(main, branch)
	if err != nil {
		log.Error(opNaming, "could not ask whether the async merge's worktree directory is free", dlog.WithCause(fields, err))
		return "", nil, false, err
	}
	if occupied {
		log.Info(opNaming, "an async merge's name is already an occupied worktree directory", dlog.Merge(fields, dlog.Context{"worktree_dir": dir}))
		return branch, nil, true, nil
	}
	log.Debug(opNaming, "claimed an async merge's name until its request is recorded", fields)
	return branch, v.claimLocked(branch), false, nil
}

// worktreeOccupied answers whether the directory BRANCH's worktree would be
// materialized at in the repository REPODIR names (WorktreeDir, the same
// derivation Create materializes by) is already on disk, and that directory.
//
// A DIRECTORY IS A NAME TOO. The directory is the branch's BARE component, so
// "fix/outage-recovery" and "outage-recovery" are two branches with one
// directory: an async merge named after a branch whose worktree still stood
// passed every branch check, was recorded, and then failed `git worktree add`
// at the fork after the requesting turn had ended (2026-10-20). Asked when the
// name is chosen, the name is refused, or passed over, before anything is
// recorded.
func worktreeOccupied(repoDir, branch string) (bool, string, error) {
	dir, err := WorktreeDir(repoDir, branch)
	if err != nil {
		return false, "", fmt.Errorf("derive the worktree directory of %q: %w", branch, err)
	}
	switch _, err := os.Lstat(dir); {
	case err == nil:
		return true, dir, nil
	case errors.Is(err, fs.ErrNotExist):
		return false, dir, nil
	default:
		return false, dir, fmt.Errorf("stat the worktree directory %q: %w", dir, err)
	}
}

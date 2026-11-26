package workspace

import (
	"context"
	"fmt"
	"strings"
	"sync"

	"agent-repld/internal/ids"
)

// This file is the ONE place a workspace name is HANDED OUT.
//
// A name is taken when it is a registered workspace's name or branch, a branch
// of the repository, an occupied worktree directory (worktreeOccupied), or
// CLAIMED in this process: a create holds a claim on the name it chose from
// the moment it chose it until it is registered or failed. Choosing and
// claiming happen under one lock, so two allocations can never both hand out
// one name.

// nameClaims is the process's claimed names, by branch.
type nameClaims struct {
	mu      sync.Mutex
	claimed map[string]bool
}

// MergeBranch is the name a SUPPLIED name resolves to: the branch, and the
// registry name, it stands under, with the prefix applied (merge.Names). It
// claims nothing; the merge queue matches a merge named either way by it.
func MergeBranch(name string) string { return suppliedBranch(name) }

// suppliedBranch is the branch a SUPPLIED name resolves to: taken as it stands
// when it already carries a prefix component, prefixed otherwise.
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
// Workspace is empty, a claim whose workspace is not registered yet.
type nameHolder struct {
	Workspace ids.WorkspaceID
}

// holdersLocked answers every branch name that is taken without asking git,
// and what holds it: the registered workspaces' names and branches
// (registeredHolders), and the claims. A registered holder outranks a claim
// for the same name. The caller holds v.names.mu.
func (v *verbs) holdersLocked(ctx context.Context) (map[string]nameHolder, error) {
	registered, err := v.registeredHolders(ctx)
	if err != nil {
		return nil, err
	}
	held := make(map[string]nameHolder, len(registered)+len(v.names.claimed))
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
func (v *verbs) takenLocked(ctx context.Context) (map[string]bool, error) {
	held, err := v.holdersLocked(ctx)
	if err != nil {
		return nil, err
	}
	taken := make(map[string]bool, len(held))
	for name := range held {
		taken[name] = true
	}
	return taken, nil
}

// worktreeOccupied answers whether the directory BRANCH's worktree would be
// materialized at in the repository REPODIR names (WorktreeDir, the same
// derivation Create materializes by) is already taken, and that directory:
// on disk, or by a rename's move owed to go there (wsm.OwedMove), which
// stands there once the workspace falls free.
//
// The directory is a fact apart from the branch: a name whose branch is free
// can still name a worktree directory that stands, which `git worktree add`
// refuses (2026-10-20). Asked when the name is chosen, the name is refused,
// or passed over, before anything is recorded.
func (v *verbs) worktreeOccupied(ctx context.Context, repoDir, branch string) (bool, string, error) {
	dir, err := WorktreeDir(repoDir, branch)
	if err != nil {
		return false, "", fmt.Errorf("derive the worktree directory of %q: %w", branch, err)
	}
	present, err := pathPresent(dir)
	if err != nil {
		return false, dir, fmt.Errorf("stat the worktree directory %q: %w", dir, err)
	}
	if present {
		return true, dir, nil
	}
	moves, err := v.deps.DB.OwedMoves(ctx)
	if err != nil {
		return false, dir, fmt.Errorf("read the owed moves: %w", err)
	}
	for _, m := range moves {
		if m.To.Dir == dir {
			return true, dir, nil
		}
	}
	return false, dir, nil
}

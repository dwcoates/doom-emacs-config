// Package landedbranch deletes a LANDED workspace's branch: the one way every
// landing path (the merge queue's teardown, the retirement of a requester
// whose detached merge landed, and the reaper's healing sweep) lets a merged
// branch go.
//
// A LANDED BRANCH LEFT BEHIND IS A NAME NOBODY CAN USE AGAIN. A create that
// supplies a name hands its branch to git unchecked, so a branch that outlived
// its landed workspace refuses every later create of that name (2026-10-30:
// `ramdisk-leak-investigation` landed on 2026-10-20, its branch survived, and
// every later leak detection's create of the same name failed on it).
//
// NOTHING THAT DID NOT LAND IS EVER DELETED. The branch goes only when all of
// these hold, each read from git as it stands:
//
//   - its tip is reachable from the ref it landed into (the merge is verified,
//     never assumed from a record);
//   - no worktree has it checked out (`update-ref -d` does not refuse a
//     checked-out branch, so this package does);
//   - it still points at the tip that was judged: the deletion is git's
//     compare-and-delete (gitclient.Git.DeleteBranchAt), so a commit that moved
//     the branch after the judgement makes git refuse, and the branch survives
//     with it.
package landedbranch

import (
	"context"
	"fmt"
	"maps"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
)

// Git is the part of gitclient.Git a branch retirement asks.
type Git interface {
	BranchExists(ctx context.Context, repoDir, branch string) (bool, error)
	ResolveRef(ctx context.Context, repoDir, ref string) (string, error)
	IsAncestor(ctx context.Context, dir, ancestor, descendant string) (bool, error)
	ListWorktrees(ctx context.Context, repoDir string) ([]gitclient.Worktree, error)
	DeleteBranchAt(ctx context.Context, repoDir, branch, head string) error
}

// Outcome is what Retire did with the branch.
type Outcome string

// The outcomes. Every one but Deleted keeps the branch.
const (
	// Deleted: the branch landed and is gone.
	Deleted Outcome = "deleted"
	// KeptAbsent: there was no such branch; nothing to do.
	KeptAbsent Outcome = "absent"
	// KeptCheckedOut: a worktree has the branch checked out.
	KeptCheckedOut Outcome = "checked_out"
	// KeptNotLanded: the branch holds commits the ref it landed into does not.
	KeptNotLanded Outcome = "not_landed"
)

// Retire deletes BRANCH of the repository at REPODIR when it has landed in
// INTO (a ref of the same repository, such as the default branch or the
// parent workspace's branch). OP and FIELDS are the caller's operation and
// context: every outcome is recorded under them, a deletion at INFO and a
// kept branch at DEBUG, and every git that could not answer or act at ERROR,
// once, here. An error leaves the branch as it was.
func Retire(ctx context.Context, git Git, log dlog.Logger, op, repoDir, branch, into string, fields dlog.Context) (Outcome, error) {
	record := dlog.Context{"repo": repoDir, "branch": branch, "into": into}
	maps.Copy(record, fields)
	fail := func(what string, err error) (Outcome, error) {
		log.Error(op, "could not retire the landed branch: "+what+"; the branch is kept", dlog.WithCause(record, err))
		return "", fmt.Errorf("landedbranch: %s %s in %s: %w", what, branch, repoDir, err)
	}
	if branch == "" || into == "" {
		return fail("check the request", fmt.Errorf("the branch (%q) and the ref it landed into (%q) are both required", branch, into))
	}
	if branch == into {
		return fail("check the request", fmt.Errorf("the branch %q is the ref it landed into", branch))
	}
	exists, err := git.BranchExists(ctx, repoDir, branch)
	if err != nil {
		return fail("probe the branch", err)
	}
	if !exists {
		log.Debug(op, "the landed branch is already gone", record)
		return KeptAbsent, nil
	}
	worktrees, err := git.ListWorktrees(ctx, repoDir)
	if err != nil {
		return fail("list the worktrees", err)
	}
	for _, wt := range worktrees {
		if wt.Branch == branch {
			record["worktree"] = wt.Dir
			log.Debug(op, "kept the landed branch: a worktree has it checked out", record)
			return KeptCheckedOut, nil
		}
	}
	head, err := git.ResolveRef(ctx, repoDir, "refs/heads/"+branch)
	if err != nil {
		return fail("resolve the branch", err)
	}
	record["head"] = head
	landed, err := git.IsAncestor(ctx, repoDir, head, into)
	if err != nil {
		return fail("read whether the branch landed", err)
	}
	if !landed {
		log.Debug(op, "kept the branch: it holds commits the ref it landed into does not", record)
		return KeptNotLanded, nil
	}
	if err := git.DeleteBranchAt(ctx, repoDir, branch, head); err != nil {
		return fail("delete the branch at the judged head", err)
	}
	log.Info(op, "deleted the landed branch: its tip is inside the ref it landed into", record)
	return Deleted, nil
}

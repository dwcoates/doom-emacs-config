package merge

import (
	"context"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
)

// This file is the merge's IDENTITY CHECK: a target moves only when every
// commit the move brings in was authored AND committed by the user's global
// git identity (`git config --global user.name` / `user.email`). See
// AGENTS.md "Commits carry the running user's own git identity".
//
// WHY AT THE LANDING. The repository's pre-commit hook refuses a commit made
// under another identity, but a rebase or cherry-pick runs no pre-commit and
// stamps every replayed commit's committer from whatever identity is active.
// So the check runs on the merge commit itself, after the rebase, the gate and
// every fixing attempt, just before the target is fast-forwarded to it: its
// range from the tip is exactly what would land, the merge commit included.
//
// OUTSIDE AUTHORS ARE REFUSED HERE. The local merge (a lifecycle's
// pre_deploy.local_merge) lands LOCAL work (a workspace's branch, a named
// branch), and nothing in a commit tells a contributor apart from this user
// under a wrong identity: `Test <test@example.com>` names nobody the user knows
// either. A repository whose deploy has no local merge lands its commits
// through its own upstream, and its deploys are not checked: the upstream
// recorded those identities, not this machine.

// checkIdentities reads the global identity and every commit tip..commit
// would land, and answers the refusal's one-line summary, empty when every
// commit is the global identity's. A refusal is recorded at ERROR with every
// offending commit; a read that fails is the caller's error.
func (r *run) checkIdentities(ctx context.Context, tip, commit, targetBranch string) (string, error) {
	const op = "daemon.merge.identity"
	target := r.subject.targetDir
	global, err := r.git.GlobalIdentity(ctx, target)
	if err != nil {
		return "", fmt.Errorf("merge: reading the global git identity: %w", err)
	}
	commits, err := r.git.CommitIdentities(ctx, target, tip, commit)
	if err != nil {
		return "", fmt.Errorf("merge: reading the identities of %s..%s: %w", short(tip), short(commit), err)
	}
	if len(commits) == 0 {
		err := fmt.Errorf("merge: the merge commit %s brings in no commit from %s", short(commit), short(tip))
		r.o.log(ctx, r.ws).Error(op, "the merge commit's range is empty; it cannot be", dlog.Context{
			"workspace": string(r.ws), "target": targetBranch, "tip": tip, "commit": commit})
		return "", err
	}
	summary, offending := identityRefusal(global, commits)
	if summary == "" {
		return "", nil
	}
	r.o.log(ctx, r.ws).Error(op, "a commit the merge would land is not under the global git identity; the merge fails", dlog.Context{
		"workspace": string(r.ws), "target": targetBranch, "branch": r.subject.branch, "tip": tip, "commit": commit,
		"global_identity": global.String(), "offending": offending, "summary": summary})
	return summary, nil
}

// noGlobalIdentity is the refusal when the user has no global identity.
const noGlobalIdentity = "no global git identity is configured (git config --global user.name / user.email); nothing lands without one"

// identityRefusal judges commits against global: the refusal's one-line
// summary and one line per offending commit, both empty when every commit's
// author and committer are global.
func identityRefusal(global gitclient.Identity, commits []gitclient.CommitIdentity) (string, []string) {
	if global.Name == "" || global.Email == "" {
		return noGlobalIdentity, nil
	}
	var offending []string
	for _, c := range commits {
		var wrong []string
		if c.Author != global {
			wrong = append(wrong, "author "+c.Author.String())
		}
		if c.Committer != global {
			wrong = append(wrong, "committer "+c.Committer.String())
		}
		if len(wrong) > 0 {
			offending = append(offending, fmt.Sprintf("%s %q: %s", short(c.SHA), c.Subject, strings.Join(wrong, ", ")))
		}
	}
	if len(offending) == 0 {
		return "", nil
	}
	return fmt.Sprintf("%d of %d commits are not under the global git identity %s (first: %s)",
		len(offending), len(commits), global, offending[0]), offending
}

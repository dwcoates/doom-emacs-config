// rebasesweep.go is the BOOT SWEEP of rebase worktree directories no merge
// owns any more.
//
// WHAT IT IS FOR. A rebase worktree is a process-local temp directory: it dies
// with the daemon that made it in every sense except the one that matters, the
// bytes. A daemon killed mid-merge — a bounce from a self-merge, a crash, a kill
// during a parked conflict — leaves its $TMPDIR directory behind and its
// registration in the target repository's `git worktree list`. Nothing in the
// pipeline ever looked at the leftovers again: the next merge prunes what it
// finds REGISTERED against its own target, which says nothing about a directory
// whose registration is already gone. Live, that arithmetic produced 893
// agent-repl-merge-rebase-* directories in one $TMPDIR.
//
// THE BOOT IS THE ONLY PARTY THAT CAN NOTICE, exactly as it is for the orphaned
// merge PHASES the coordinator's Drain sweeps (orphansweep_test.go). This is the
// filesystem half of the same idea, and it lives in the merge package because
// what a rebase worktree is, where it lives and who may still be using one are
// merge-subsystem facts; internal/server only calls it.
//
// IT IS CONSERVATIVE BY CONSTRUCTION. A directory is removed only when NO live
// merge names it — a merge mid-rebase, and above all one PARKED on a conflict,
// whose tree is the resolution's workbench and whose removal would destroy a
// user's half-finished merge. The retention set comes from the coordinator's
// own in-flight bookkeeping (QueueCoordinator.RetainedRebaseWorktrees), not from
// the durable records, which deliberately carry no temp worktree at all.
//
// IT LOGS ONE LINE. A sweep of hundreds of directories that says a line per
// directory is a sweep nobody reads.
package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"claude-repld/internal/dlog"
)

// RetainedRebaseWorktrees names every rebase worktree an in-flight merge is
// using right now: one mid-rebase, one parked on a conflict awaiting a
// resolution, one waiting on a test fix. They are the directories a sweep must
// NOT touch.
func (c *QueueCoordinator) RetainedRebaseWorktrees() []string {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]string, 0, len(c.rebaseWork))
	for _, dir := range c.rebaseWork {
		out = append(out, dir)
	}
	sort.Strings(out)
	return out
}

// adoptRebaseWorktree points driven at the worktree res was produced in and
// records it as repo's live tree. Every place settle takes a Result goes through
// it, so "the request the terminal defer cleans up" and "the directory the sweep
// must keep" can never be two different answers.
func (c *QueueCoordinator) adoptRebaseWorktree(repo string, driven *Request, res Result) {
	driven.WorkDir, driven.BaseHead = res.WorkDir, res.BaseHead
	c.mu.Lock()
	defer c.mu.Unlock()
	if driven.WorkDir == "" {
		delete(c.rebaseWork, repo)
		return
	}
	c.rebaseWork[repo] = driven.WorkDir
}

// releaseRebaseWorktree forgets repo's live tree, once its teardown has run.
func (c *QueueCoordinator) releaseRebaseWorktree(repo string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	delete(c.rebaseWork, repo)
}

// SweepOrphanRebaseWorktrees removes every agent-repl-merge-rebase-* directory
// in the temp dir that `retained` does not name, and prunes the repositories
// they belonged to so the administrative record cannot outlive them.
//
// THE REPOSITORY COMES FROM THE ORPHAN ITSELF. A linked worktree carries a
// `.git` FILE naming its repository's gitdir, so the sweep reads each orphan's
// own before removing it rather than being told which repositories to prune.
// That is what lets a boot with an empty merge queue still clear the ~190 stale
// entries a repository's `git worktree list` accumulated.
//
// A SWEEP FAILURE FAILS NOTHING. It is reported to the caller, which logs it;
// there is no merge here to un-merge, and a directory that would not delete is
// not a reason to refuse to boot.
func (e *Driver) SweepOrphanRebaseWorktrees(ctx context.Context, retained []string) error {
	tmp := os.TempDir()
	entries, err := os.ReadDir(tmp)
	if err != nil {
		return fmt.Errorf("merge: reading %s for orphaned rebase worktrees: %w", tmp, err)
	}

	keep := make(map[string]struct{}, len(retained))
	for _, dir := range retained {
		if dir == "" {
			continue
		}
		// The retained paths are the LEAVES (`.../rebase`); what sits in the
		// temp dir is their parent, which is the unit this removes.
		keep[filepath.Clean(rebaseRemovalRoot(dir))] = struct{}{}
	}

	var (
		errs             []error
		removed, kept    int
		failed           int
		repos            = map[string]struct{}{}
		keptExamples     []string
		pruned, pruneErr int
	)
	for _, ent := range entries {
		if !ent.IsDir() || !strings.HasPrefix(ent.Name(), rebaseWorktreeMarker) {
			continue
		}
		dir := filepath.Join(tmp, ent.Name())
		if _, live := keep[dir]; live {
			kept++
			if len(keptExamples) < 3 {
				keptExamples = append(keptExamples, dir)
			}
			continue
		}
		// Read the repository BEFORE the removal takes the file away with it.
		if repo, ok := rebaseWorktreeRepo(filepath.Join(dir, rebaseWorktreeLeaf)); ok {
			repos[repo] = struct{}{}
		}
		if rmErr := os.RemoveAll(dir); rmErr != nil {
			failed++
			errs = append(errs, fmt.Errorf("merge: removing the orphaned rebase worktree %s: %w", dir, rmErr))
			continue
		}
		removed++
	}

	for repo := range repos {
		exit, out, perr := e.gitExit(ctx, repo, "worktree", "prune")
		switch {
		case perr != nil:
			pruneErr++
			errs = append(errs, perr)
		case exit != 0:
			pruneErr++
			errs = append(errs, fmt.Errorf("merge: `git worktree prune` exited %d in %s: %s", exit, repo, dlog.Clamp(out, 400)))
		default:
			pruned++
		}
	}

	// ONE LINE FOR THE WHOLE SWEEP, and it says why the kept ones were kept —
	// which is the only judgement in here a reader can second-guess.
	e.logf("merge: orphaned rebase worktree sweep {tmp=%s removed=%d kept=%d remove_failed=%d repos_pruned=%d prune_failed=%d}%s — kept directories belong to a LIVE or conflict-PARKED merge and are its workbench; everything else outlived the daemon that made it",
		tmp, removed, kept, failed, pruned, pruneErr, keptDetail(keptExamples, kept))
	if len(errs) > 0 {
		return errors.Join(errs...)
	}
	return nil
}

// rebaseWorktreeLeaf is the directory git makes inside the temp parent. It is
// named here rather than repeated, because the sweep and createRebaseWorktree
// disagreeing about it would make every orphan look repository-less.
const rebaseWorktreeLeaf = "rebase"

// keptDetail names the retained directories inline, capped, so the one summary
// line can be checked against the merges that were live without turning into a
// line per directory.
func keptDetail(examples []string, kept int) string {
	if len(examples) == 0 {
		return ""
	}
	detail := " {kept=" + strings.Join(examples, ",")
	if kept > len(examples) {
		detail += fmt.Sprintf(",+%d more", kept-len(examples))
	}
	return detail + "}"
}

// rebaseWorktreeRepo resolves the repository a leftover rebase worktree belonged
// to, from the `.git` FILE every linked worktree carries
// (`gitdir: <repo>/.git/worktrees/<name>`).
//
// It reports ok=false for anything it cannot read that way. A guessed
// repository would be handed to `git worktree prune`, and pruning the wrong
// repository is a worse outcome than pruning none: the per-merge teardown funnel
// prunes each target on every pass anyway, so a missed prune here costs a stale
// registration and nothing more.
func rebaseWorktreeRepo(leaf string) (string, bool) {
	raw, err := os.ReadFile(filepath.Join(leaf, ".git"))
	if err != nil {
		return "", false
	}
	gitdir, ok := strings.CutPrefix(strings.TrimSpace(string(raw)), "gitdir:")
	if !ok {
		return "", false
	}
	// <repo>/.git/worktrees/<name> — the repository is what precedes the
	// administrative path, and only that exact shape is accepted.
	const admin = string(filepath.Separator) + ".git" + string(filepath.Separator) + "worktrees" + string(filepath.Separator)
	idx := strings.LastIndex(strings.TrimSpace(gitdir), admin)
	if idx <= 0 {
		return "", false
	}
	return strings.TrimSpace(gitdir)[:idx], true
}

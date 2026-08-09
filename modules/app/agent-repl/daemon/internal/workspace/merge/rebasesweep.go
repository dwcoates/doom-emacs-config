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

// ManagedRepos names every repository this coordinator owns a queue for: the
// repos with a merge in flight right now, plus every repo key the durable queue
// still files requests under. It is the sweep's REPOSITORY UNIVERSE — a
// leftover naming a repository outside it was made by a different daemon (or a
// different checkout of the same repository), and removing it would destroy a
// live merge that is not ours to end.
func (c *QueueCoordinator) ManagedRepos() []string {
	repos := map[string]struct{}{}
	c.mu.Lock()
	for repo := range c.rebaseWork {
		repos[repo] = struct{}{}
	}
	c.mu.Unlock()
	for repo := range c.queue.Snapshot() {
		repos[repo] = struct{}{}
	}
	out := make([]string, 0, len(repos))
	for repo := range repos {
		out = append(out, repo)
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

// SweepScope is everything the boot sweep is allowed to reason about. The ROOT
// it scans is not in here on purpose: it is the driver's injected
// Config.RebaseRoot, the same directory rebase worktrees are created under, so
// no caller can point a sweep at a directory the driver does not own.
type SweepScope struct {
	// Retained names the rebase worktree LEAVES an in-flight merge is using
	// right now (QueueCoordinator.RetainedRebaseWorktrees). Their parents are
	// never removed.
	Retained []string
	// Repos is the repository universe this daemon manages — the repo keys the
	// coordinator knows about. A leftover whose own `.git` file points at a
	// repository outside this universe belongs to somebody else and is KEPT.
	Repos []string
}

// SweepOrphanRebaseWorktrees removes every agent-repl-merge-rebase-* directory
// UNDER THE DRIVER'S INJECTED REBASE ROOT that scope.Retained does not name, and
// prunes the repositories they belonged to so the administrative record cannot
// outlive them.
//
// IT SCANS ONE INJECTED DIRECTORY AND NOTHING ELSE. It used to resolve
// os.TempDir() itself, which meant every test that reached it — including the
// daemon's boot wiring under test — swept the REAL temp dir with a TEST
// coordinator's (empty) retention set and deleted the live daemon's rebase
// worktrees, up to and including the tree a merge gate's test run was executing
// inside. The root is now a construction-time dependency of the driver.
//
// THE REPOSITORY COMES FROM THE ORPHAN ITSELF. A linked worktree carries a
// `.git` FILE naming its repository's gitdir, so the sweep reads each orphan's
// own before removing it rather than being told which repositories to prune.
// That is what lets a boot with an empty merge queue still clear the ~190 stale
// entries a repository's `git worktree list` accumulated.
//
// THAT SAME FILE IS THE SECOND GUARD. When a leftover names a repository this
// daemon does not manage, it is another daemon's (or another checkout's) tree
// and is KEPT, counted as kept_unknown_repo in the summary. Keeping a directory
// too many leaks bytes; removing somebody else's live tree destroys a running
// merge, so the asymmetry is deliberate. A leftover with no readable `.git`
// file names no repository to be wrong about and is swept as before.
//
// A SWEEP FAILURE FAILS NOTHING. It is reported to the caller, which logs it;
// there is no merge here to un-merge, and a directory that would not delete is
// not a reason to refuse to boot.
func (e *Driver) SweepOrphanRebaseWorktrees(ctx context.Context, scope SweepScope) error {
	tmp := e.rebaseRoot
	if strings.TrimSpace(tmp) == "" {
		return fmt.Errorf("merge: the orphaned rebase worktree sweep has no injected root; refusing to guess one")
	}
	entries, err := os.ReadDir(tmp)
	if err != nil {
		return fmt.Errorf("merge: reading %s for orphaned rebase worktrees: %w", tmp, err)
	}

	keep := make(map[string]struct{}, len(scope.Retained))
	for _, dir := range scope.Retained {
		if dir == "" {
			continue
		}
		// The retained paths are the LEAVES (`.../rebase`); what sits in the
		// root is their parent, which is the unit this removes.
		keep[filepath.Clean(rebaseRemovalRoot(dir))] = struct{}{}
	}
	known := knownRepoUniverse(scope)

	var (
		errs             []error
		removed, kept    int
		unknownRepo      int
		failed           int
		repos            = map[string]struct{}{}
		keptExamples     []string
		unknownExamples  []string
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
		repo, hasRepo := rebaseWorktreeRepo(filepath.Join(dir, rebaseWorktreeLeaf))
		if hasRepo {
			if _, mine := known[repoIdentity(repo)]; !mine {
				unknownRepo++
				if len(unknownExamples) < 3 {
					unknownExamples = append(unknownExamples, dir+"←"+repo)
				}
				continue
			}
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
	e.logf("merge: orphaned rebase worktree sweep {tmp=%s removed=%d kept=%d kept_unknown_repo=%d remove_failed=%d repos_pruned=%d prune_failed=%d}%s%s — kept directories belong to a LIVE or conflict-PARKED merge and are its workbench; kept_unknown_repo ones name a repository this daemon does not manage and are somebody else's tree; everything else outlived the daemon that made it",
		tmp, removed, kept, unknownRepo, failed, pruned, pruneErr,
		keptDetail(keptExamples, kept), unknownDetail(unknownExamples, unknownRepo))
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

// unknownDetail names the directories kept because their repository is not this
// daemon's, capped like keptDetail. A count alone would leave the one judgement
// that can be wrong in the direction of leaking unexplained.
func unknownDetail(examples []string, unknown int) string {
	if len(examples) == 0 {
		return ""
	}
	detail := " {kept_unknown_repo=" + strings.Join(examples, ",")
	if unknown > len(examples) {
		detail += fmt.Sprintf(",+%d more", unknown-len(examples))
	}
	return detail + "}"
}

// knownRepoUniverse is the set of repositories the sweeping daemon manages: the
// repo keys it was handed, plus the repositories its own retained worktrees
// point at (a live merge's tree proves its repository is ours). Membership is
// tested on repoIdentity, so `<repo>` and `<repo>/.git` are one entry.
func knownRepoUniverse(scope SweepScope) map[string]struct{} {
	known := make(map[string]struct{}, len(scope.Repos)+len(scope.Retained))
	for _, repo := range scope.Repos {
		if strings.TrimSpace(repo) == "" {
			continue
		}
		known[repoIdentity(repo)] = struct{}{}
	}
	for _, leaf := range scope.Retained {
		if leaf == "" {
			continue
		}
		if repo, ok := rebaseWorktreeRepo(leaf); ok {
			known[repoIdentity(repo)] = struct{}{}
		}
	}
	return known
}

// repoIdentity normalizes the two spellings of one repository the sweep has to
// compare: the coordinator's key is a git COMMON DIR (`<repo>/.git`), while an
// orphan's `.git` file yields the repository directory (`<repo>`). Symlinks are
// resolved best-effort because macOS hands out /var and /private/var for the
// same temp directory, and two spellings of one repository would read as a
// foreign one.
func repoIdentity(path string) string {
	clean := filepath.Clean(strings.TrimSpace(path))
	if resolved, err := filepath.EvalSymlinks(clean); err == nil {
		clean = resolved
	}
	if base := filepath.Base(clean); base == ".git" {
		clean = filepath.Dir(clean)
	}
	return clean
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

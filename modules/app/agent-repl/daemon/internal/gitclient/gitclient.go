// gitclient.go implements Git. Every method is a dumb executor: it decides
// nothing about WHEN a merge happens, WHICH branch a workspace gets, or WHETHER
// a worktree should go — it only performs the git and reports the truth. The
// policy lives in the workspace verbs, the merge orchestrator and the rollout
// controller, exactly as it does above the shim client.
//
// LOCAL ONLY. No method here fetches, pushes, or names a remote as a
// destination. `refs/remotes/origin/HEAD` is READ because it is a local ref
// that records what the default branch was at clone time; nothing contacts the
// remote to read it.
package gitclient

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"claude-repld/internal/dlog"
)

// fieldSep separates the fields of one commit in a --format template. It is
// ASCII unit separator: a byte git will never emit inside a subject or an
// author name, so the split can never be fooled by punctuation in a commit
// message.
const fieldSep = "\x1f"

// commitFormat is the one commit template: sha, subject, author name, author
// date in strict ISO 8601.
const commitFormat = "%H" + fieldSep + "%s" + fieldSep + "%an" + fieldSep + "%aI"

// client is the Git implementation. It holds nothing but its log surfaces:
// every method's whole state is the repository on disk.
type client struct {
	log dlog.Surfaces
}

// DefaultBranch reports the repository's default branch, in a FIXED order of
// evidence, strongest first:
//
//  1. `symbolic-ref --short refs/remotes/origin/HEAD` — the local ref a clone
//     writes to record the remote's default branch. It is the only source that
//     states the answer rather than guessing it, so it wins outright.
//  2. `init.defaultBranch` from config, when a local branch by that name
//     exists. The configured name is what THIS repository was initialized to
//     use, so it outranks any hardcoded guess.
//  3. A local branch named `main`.
//  4. A local branch named `master`.
//
// Every candidate below the first must EXIST as a local branch: naming a
// branch the repository does not have would hand the caller a ref that every
// later operation fails on. When nothing matches, the answer is a loud
// failure — the daemon never invents a default branch.
func (c *client) DefaultBranch(ctx context.Context, repoDir string) (string, error) {
	const operation = "daemon.gitclient.default_branch"

	in, err := c.runRaw(ctx, operation, repoDir, "symbolic-ref", "--short", "refs/remotes/origin/HEAD")
	if err != nil {
		return "", err
	}
	if in.exitCode == 0 {
		// `origin/main` -> `main`. The remote-tracking ref names the branch
		// under its remote; the caller wants the branch.
		branch := strings.TrimRight(in.stdout, "\n")
		if _, name, found := strings.Cut(branch, "/"); found {
			branch = name
		}
		if branch != "" {
			return branch, nil
		}
	}

	candidates := make([]string, 0, 3)
	configured, err := c.runRaw(ctx, operation, repoDir, "config", "--get", "init.defaultBranch")
	if err != nil {
		return "", err
	}
	if configured.exitCode == 0 {
		if name := strings.TrimRight(configured.stdout, "\n"); name != "" {
			candidates = append(candidates, name)
		}
	}
	candidates = append(candidates, "main", "master")

	for _, name := range candidates {
		exists, err := c.branchExists(ctx, operation, repoDir, name)
		if err != nil {
			return "", err
		}
		if exists {
			return name, nil
		}
	}

	c.log.Global().Error(operation, "no default branch could be determined", dlog.Context{
		"dir":        repoDir,
		"candidates": candidates,
	})
	return "", fmt.Errorf("gitclient: no default branch in %s: refs/remotes/origin/HEAD is unset and none of %s exists locally",
		repoDir, strings.Join(candidates, ", "))
}

// branchExists reports whether a LOCAL branch by that name exists. `show-ref
// --verify` is exact: it neither resolves abbreviations nor falls back to a
// tag, so a `main` that is only a tag can never be mistaken for the branch.
func (c *client) branchExists(ctx context.Context, operation, repoDir, branch string) (bool, error) {
	in, err := c.runRaw(ctx, operation, repoDir, "show-ref", "--verify", "--quiet", "refs/heads/"+branch)
	if err != nil {
		return false, err
	}
	return in.exitCode == 0, nil
}

// BranchExists reports whether a LOCAL branch by that name exists. It is the
// exported half of branchExists, for the naming call's collision probe: an
// absent branch is the answer the probe wants, so it must not be recorded as
// a failure the way a failed ResolveRef is.
func (c *client) BranchExists(ctx context.Context, repoDir, branch string) (bool, error) {
	return c.branchExists(ctx, "daemon.gitclient.branch_exists", repoDir, branch)
}

// ResolveRef resolves a ref to a full commit sha. The `^{commit}` peel means a
// tag resolves to the commit it points at rather than to the tag object, and
// `--verify` means an unknown ref fails loudly instead of being echoed back.
func (c *client) ResolveRef(ctx context.Context, repoDir, ref string) (string, error) {
	return c.run(ctx, "daemon.gitclient.resolve_ref", repoDir, "rev-parse", "--verify", "--end-of-options", ref+"^{commit}")
}

// CreateWorktree creates branch at baseRef and checks it out at worktreeDir.
// One command does both, so there is no window in which the branch exists
// without its tree.
func (c *client) CreateWorktree(ctx context.Context, repoDir, branch, baseRef, worktreeDir string) error {
	const operation = "daemon.gitclient.create_worktree"
	if _, err := c.run(ctx, operation, repoDir,
		"worktree", "add", "-b", branch, worktreeDir, baseRef); err != nil {
		return err
	}
	// A WORKTREE AT A PATH A REMOVAL DETACHED IS A NEW WORKSPACE'S: its log
	// sinks link into it again (see RemoveWorktree).
	if err := c.log.AttachDir(worktreeDir); err != nil {
		c.log.Global().Error(operation, "the new worktree could not be re-attached to its log sinks", dlog.Context{
			"dir": repoDir, "worktree_dir": worktreeDir, "cause": err.Error(),
		})
		return fmt.Errorf("gitclient: attach the log sinks of %s: %w", worktreeDir, err)
	}
	return nil
}

// RemoveWorktree removes a worktree and leaves its branch alone.
//
// THE POSTCONDITION DECIDES SUCCESS, not the exit status of any one step. A
// worktree directory that is already gone gets no `worktree remove` at all
// (git exits 128 for a tree that is not there, and a second teardown of an
// already-removed tree is an ordinary event, not a fault); the prune runs on
// every pass so git's administrative record can never outlive the directory;
// and only a directory that is STILL THERE afterwards is reported as a
// failure. This is the exact shape the old merge teardown had to be corrected
// into after it logged loud failures for directories that were already gone.
func (c *client) RemoveWorktree(ctx context.Context, repoDir, worktreeDir string) error {
	const operation = "daemon.gitclient.remove_worktree"

	// THE LOG SINKS LET GO OF THE DIRECTORY BEFORE ANYTHING REMOVES IT. A
	// workspace sink opened mid-removal re-created `<worktree>/.claude/emacs`
	// with its canonical link, and the postcondition below then found the
	// worktree "still present after removal" (TestHandoverTransfersAtFreeness,
	// 2026-09-23). Detached first, no sink creates anything inside it again.
	if err := c.log.DetachDir(worktreeDir); err != nil {
		c.log.Global().Error(operation, "the worktree could not be detached from its log sinks; it is not removed", dlog.Context{
			"dir": repoDir, "worktree_dir": worktreeDir, "cause": err.Error(),
		})
		return fmt.Errorf("gitclient: detach the log sinks of %s: %w", worktreeDir, err)
	}

	present, err := pathPresent(worktreeDir)
	if err != nil {
		return err
	}

	var removeFailure error
	if present {
		// --force is what a tree parked mid-merge needs: it has a paused
		// operation in it and git refuses a plain remove.
		in, invokeErr := c.runRaw(ctx, operation, repoDir, "worktree", "remove", "--force", worktreeDir)
		if invokeErr != nil {
			return invokeErr
		}
		if in.exitCode != 0 {
			removeFailure = in.fail()
		}
	}

	pruned, err := c.runRaw(ctx, operation, repoDir, "worktree", "prune")
	if err != nil {
		return err
	}
	if pruned.exitCode != 0 {
		failure := pruned.fail()
		c.log.Global().Error(operation, "git exited nonzero", pruned.logContext())
		return failure
	}

	stillThere, err := pathPresent(worktreeDir)
	if err != nil {
		return err
	}
	if !stillThere {
		return nil
	}
	if removeFailure != nil {
		c.log.Global().Error(operation, "the worktree survived `worktree remove --force`", dlog.Context{
			"dir":          repoDir,
			"worktree_dir": worktreeDir,
			"cause":        removeFailure.Error(),
		})
		return removeFailure
	}
	c.log.Global().Error(operation, "the worktree is still present after removal", dlog.Context{
		"dir":          repoDir,
		"worktree_dir": worktreeDir,
	})
	return fmt.Errorf("gitclient: the worktree %s is still present after `git worktree remove --force`", worktreeDir)
}

// Nuke force-removes both the worktree and the branch. This is data
// destruction and has no undo.
//
// The order is forced: git refuses to delete a branch that a registered
// worktree has checked out, so the tree goes first and the branch second. A
// branch that is already gone is a silent no-op for the same reason an
// already-removed worktree is — the postcondition is what the caller asked
// for, and it already holds.
func (c *client) Nuke(ctx context.Context, repoDir, worktreeDir, branch string) error {
	const operation = "daemon.gitclient.nuke"

	if err := c.RemoveWorktree(ctx, repoDir, worktreeDir); err != nil {
		return err
	}

	exists, err := c.branchExists(ctx, operation, repoDir, branch)
	if err != nil {
		return err
	}
	if !exists {
		c.log.Global().Debug(operation, "the branch was already gone", dlog.Context{
			"dir":    repoDir,
			"branch": branch,
		})
		return nil
	}

	_, err = c.run(ctx, operation, repoDir, "branch", "-D", branch)
	return err
}

// CommonDir reports a directory's repository common dir, canonicalized. It is
// the repository's IDENTITY: every worktree cut from one repository reports the
// same common dir, which is what makes the merge-method split and the
// self-reload trigger answerable at all.
//
// Two canonicalizations are load-bearing. `--git-common-dir` may answer
// relatively (`.git` for a plain checkout), so it is resolved against the
// queried directory; and the answer is passed through EvalSymlinks, because on
// macOS the same repository reached through /tmp and through /private/tmp would
// otherwise compare unequal.
func (c *client) CommonDir(ctx context.Context, dir string) (string, error) {
	const operation = "daemon.gitclient.common_dir"

	out, err := c.run(ctx, operation, dir, "rev-parse", "--git-common-dir")
	if err != nil {
		return "", err
	}
	common := out
	if !filepath.IsAbs(common) {
		absDir, err := filepath.Abs(dir)
		if err != nil {
			c.log.Global().Error(operation, "the queried directory has no absolute form", dlog.Context{
				"dir":   dir,
				"cause": err.Error(),
			})
			return "", err
		}
		common = filepath.Join(absDir, common)
	}
	resolved, err := filepath.EvalSymlinks(common)
	if err != nil {
		c.log.Global().Error(operation, "the common dir could not be canonicalized", dlog.Context{
			"dir":        dir,
			"common_dir": common,
			"cause":      err.Error(),
		})
		return "", err
	}
	return filepath.Clean(resolved), nil
}

// MainWorktree reports a directory's repository's MAIN WORKTREE, canonicalized.
//
// `git worktree list --porcelain` lists the main worktree FIRST -- git's own
// documented order -- so its first `worktree <path>` line is the answer. It is
// asked of git rather than derived from the common dir because `.git`'s parent
// is the main worktree only for an ordinary checkout: a bare repository has no
// main worktree at all, and `--separate-git-dir` puts the git dir somewhere
// else entirely.
func (c *client) MainWorktree(ctx context.Context, dir string) (string, error) {
	const operation = "daemon.gitclient.main_worktree"

	out, err := c.run(ctx, operation, dir, "worktree", "list", "--porcelain")
	if err != nil {
		return "", err
	}
	for _, line := range strings.Split(out, "\n") {
		path, ok := strings.CutPrefix(strings.TrimSpace(line), "worktree ")
		if !ok {
			continue
		}
		resolved, err := filepath.EvalSymlinks(path)
		if err != nil {
			c.log.Global().Error(operation, "the main worktree could not be canonicalized", dlog.Context{
				"dir": dir, "main_worktree": path, "cause": err.Error(),
			})
			return "", err
		}
		return filepath.Clean(resolved), nil
	}
	err = fmt.Errorf("gitclient: %s belongs to a repository with no worktree (a bare repository has none)", dir)
	c.log.Global().Error(operation, "the repository has no main worktree", dlog.Context{
		"dir": dir, "stdout": out,
	})
	return "", err
}

// RepositoryOf probes which repository a directory is inside, answering that
// repository's canonicalized main worktree.
//
// IT IS THE PROBE HALF of MainWorktree, and the two differ in ONE thing: what a
// git that declines to answer means. MainWorktree is asked about a directory
// the caller has already established is a worktree, so a refusal there is a
// fault and is recorded as one. This is asked about a path a PERSON picked, so
// "that is not in a repository" is the answer the question was asked to get,
// and recording it at error would put a fault in the log every time somebody
// picked the wrong file. Same reason BranchExists is a probe and ResolveRef is
// not.
//
// The parse is MainWorktree's, for the same reason it is MainWorktree's: `git
// worktree list --porcelain` lists the main worktree FIRST.
func (c *client) RepositoryOf(ctx context.Context, dir string) (string, bool, error) {
	const operation = "daemon.gitclient.repository_of"

	in, err := c.runRaw(ctx, operation, dir, "worktree", "list", "--porcelain")
	if err != nil {
		return "", false, err
	}
	if in.exitCode != 0 {
		c.log.Global().Debug(operation, "the path is inside no git repository", in.logContext())
		return "", false, nil
	}
	for _, line := range strings.Split(in.stdout, "\n") {
		path, ok := strings.CutPrefix(strings.TrimSpace(line), "worktree ")
		if !ok {
			continue
		}
		resolved, err := filepath.EvalSymlinks(path)
		if err != nil {
			c.log.Global().Error(operation, "the main worktree could not be canonicalized", dlog.Context{
				"dir": dir, "main_worktree": path, "cause": err.Error(),
			})
			return "", false, err
		}
		return filepath.Clean(resolved), true, nil
	}
	// A BARE REPOSITORY HAS NO MAIN WORKTREE, and to this probe's caller that
	// is the same answer as no repository at all: there is no directory to
	// record as the repository's. It is DEBUG rather than an error for the
	// same reason the nonzero exit is.
	c.log.Global().Debug(operation, "the repository has no main worktree", in.logContext())
	return "", false, nil
}

// SameRepo reports whether two directories belong to one repository, by the
// canonicalized common dir and by nothing else. A worktree and its parent
// checkout are the SAME repository here, which is the answer the merge
// orchestrator's method split wants.
func (c *client) SameRepo(ctx context.Context, a, b string) (bool, error) {
	commonA, err := c.CommonDir(ctx, a)
	if err != nil {
		return false, err
	}
	commonB, err := c.CommonDir(ctx, b)
	if err != nil {
		return false, err
	}
	return commonA == commonB, nil
}

// MergeNoFF merges sourceBranch into targetDir's checkout with --no-ff, and
// its result is an ANSWER rather than a success-or-failure.
//
// --no-ff is the whole point: the landing is ONE merge commit whatever the
// history looked like, so there is exactly one commit to revert and exactly one
// second-parent range to read the landed commits off. A fast-forward would
// leave neither.
//
// A CONFLICT IS NOT A FAILURE and the index is LEFT STAGED. Nothing here
// aborts: the conflicted worktree IS the resolution flow's workbench, and an
// abort would destroy the very state the conflict agent is dispatched to work
// in. AbortMerge exists for the caller that decides to give up; this method
// never decides that for it.
func (c *client) MergeNoFF(ctx context.Context, targetDir, sourceBranch, message string) (MergeOutcome, error) {
	const operation = "daemon.gitclient.merge_no_ff"

	in, err := c.runRaw(ctx, operation, targetDir,
		"merge", "--no-ff", "--no-edit", "-m", message, sourceBranch)
	if err != nil {
		return MergeOutcome{}, err
	}

	if in.exitCode == 0 {
		landed, err := c.commitAt(ctx, operation, targetDir, "HEAD")
		if err != nil {
			return MergeOutcome{}, err
		}
		return MergeOutcome{Landed: &landed}, nil
	}

	// A nonzero exit is a conflict only when git actually left conflicted
	// paths behind. Everything else (a dirty tree, an unknown branch, a
	// refusal to merge unrelated histories) is a real failure carrying git's
	// own words.
	conflicted, err := c.ConflictedFiles(ctx, targetDir)
	if err != nil {
		return MergeOutcome{}, err
	}
	if len(conflicted) == 0 {
		failure := in.fail()
		c.log.Global().Error(operation, "the merge failed without leaving conflicts", in.logContext())
		return MergeOutcome{}, failure
	}

	c.log.Global().Warn(operation, "the merge conflicted; the index is left staged for resolution", dlog.Context{
		"dir":           targetDir,
		"source_branch": sourceBranch,
		"exit_code":     in.exitCode,
		"conflicted":    conflicted,
		"stdout":        in.stdout,
		"stderr":        in.stderr,
	})
	return MergeOutcome{Conflicted: conflicted}, nil
}

// Commit records whatever is staged as a commit and answers its full sha.
// `--no-edit` keeps git from opening an editor on a merge's prepared message,
// and `-m` supplies the message the caller composed; the sha is read back with
// a separate `rev-parse HEAD` rather than parsed out of commit's own chatter,
// whose shape is porcelain and not a contract.
func (c *client) Commit(ctx context.Context, dir, message string) (string, error) {
	const operation = "daemon.gitclient.commit"

	if _, err := c.run(ctx, operation, dir, "commit", "--no-edit", "-m", message); err != nil {
		return "", err
	}
	sha, err := c.run(ctx, operation, dir, "rev-parse", "HEAD")
	if err != nil {
		return "", err
	}
	c.log.Global().Debug(operation, "the staged index was recorded as a commit", dlog.Context{
		"dir": dir,
		"sha": sha,
	})
	return sha, nil
}

// ConflictedFiles lists the paths currently in conflict. `-z` is not a detail:
// without it git quotes and escapes paths with unusual bytes, and the caller
// would hand a quoted path to a resolution agent as though it were a filename.
func (c *client) ConflictedFiles(ctx context.Context, dir string) ([]string, error) {
	out, err := c.run(ctx, "daemon.gitclient.conflicted_files", dir,
		"diff", "--name-only", "--diff-filter=U", "-z")
	if err != nil {
		return nil, err
	}
	return splitNUL(out), nil
}

// AbortMerge aborts an in-progress merge, restoring the pre-merge state.
func (c *client) AbortMerge(ctx context.Context, dir string) error {
	_, err := c.run(ctx, "daemon.gitclient.abort_merge", dir, "merge", "--abort")
	return err
}

// RevertMerge reverts a landed merge commit. `-m 1` names the FIRST parent as
// the mainline, which is what "undo what the merge brought in, keep the target's
// own history" means; a merge commit cannot be reverted without it.
func (c *client) RevertMerge(ctx context.Context, targetDir, mergeCommit string) error {
	_, err := c.run(ctx, "daemon.gitclient.revert_merge", targetDir,
		"revert", "-m", "1", "--no-edit", mergeCommit)
	return err
}

// LandedRange lists what a merge commit brought in, read off the COMMIT ITSELF
// rather than off any branch: `<commit>^1..<commit>^2` walks the second
// parent's history minus the first parent's, which is precisely the set of
// commits the merge added. Reading it from the commit means the answer survives
// the source branch being deleted, moved, or merged again later.
//
// The order is OLDEST FIRST, because the caller renders it as the story of what
// landed.
func (c *client) LandedRange(ctx context.Context, targetDir, mergeCommit string) ([]Commit, error) {
	const operation = "daemon.gitclient.landed_range"

	out, err := c.run(ctx, operation, targetDir,
		"rev-list", "--reverse", "--format="+commitFormat, "--no-commit-header",
		mergeCommit+"^1.."+mergeCommit+"^2")
	if err != nil {
		return nil, err
	}
	return c.parseCommits(operation, targetDir, out)
}

// ChangedPaths lists the paths a range touched. The rollout controller
// classifies subsystems from it, so the paths must be raw rather than quoted —
// hence `-z`.
func (c *client) ChangedPaths(ctx context.Context, dir, rangeSpec string) ([]string, error) {
	out, err := c.run(ctx, "daemon.gitclient.changed_paths", dir,
		"diff", "--name-only", "-z", rangeSpec)
	if err != nil {
		return nil, err
	}
	return splitNUL(out), nil
}

// IsClean reports whether the working tree and index are clean. Untracked files
// COUNT as unclean: every caller asks this before doing something that would
// either lose them or sweep them into a commit, so "there is unexpected content
// in this tree" is the answer they need.
func (c *client) IsClean(ctx context.Context, dir string) (bool, error) {
	out, err := c.run(ctx, "daemon.gitclient.is_clean", dir, "status", "--porcelain")
	if err != nil {
		return false, err
	}
	return strings.TrimSpace(out) == "", nil
}

// CurrentBranch reports the checked-out branch, or the empty string when HEAD
// is detached. A detached HEAD is a STATE, not a failure, and the caller is the
// one that decides whether it can proceed without a branch.
func (c *client) CurrentBranch(ctx context.Context, dir string) (string, error) {
	const operation = "daemon.gitclient.current_branch"

	out, err := c.run(ctx, operation, dir, "rev-parse", "--abbrev-ref", "HEAD")
	if err != nil {
		return "", err
	}
	if out == "HEAD" {
		c.log.Global().Debug(operation, "HEAD is detached; there is no current branch", dlog.Context{"dir": dir})
		return "", nil
	}
	return out, nil
}

// commitAt reads one commit's facts.
func (c *client) commitAt(ctx context.Context, operation, dir, ref string) (Commit, error) {
	out, err := c.run(ctx, operation, dir, "show", "--no-patch", "--format="+commitFormat, ref)
	if err != nil {
		return Commit{}, err
	}
	commits, err := c.parseCommits(operation, dir, out)
	if err != nil {
		return Commit{}, err
	}
	if len(commits) != 1 {
		c.log.Global().Error(operation, "a single-commit read returned a different number of commits", dlog.Context{
			"dir":   dir,
			"ref":   ref,
			"count": len(commits),
		})
		return Commit{}, fmt.Errorf("gitclient: reading commit %s in %s returned %d commits", ref, dir, len(commits))
	}
	return commits[0], nil
}

// parseCommits turns the commitFormat output into Commits. A line that does not
// carry every field, or an author date git wrote in a shape time cannot read,
// is a LOUD failure: silently dropping it would hand the caller a landed range
// that is quietly short.
func (c *client) parseCommits(operation, dir, out string) ([]Commit, error) {
	trimmed := strings.TrimRight(out, "\n")
	if trimmed == "" {
		return nil, nil
	}
	lines := strings.Split(trimmed, "\n")
	commits := make([]Commit, 0, len(lines))
	for _, line := range lines {
		fields := strings.Split(line, fieldSep)
		if len(fields) != 4 {
			c.log.Global().Error(operation, "a commit line did not carry every field", dlog.Context{
				"dir":    dir,
				"line":   line,
				"fields": len(fields),
			})
			return nil, fmt.Errorf("gitclient: unreadable commit line %q in %s", line, dir)
		}
		at, err := time.Parse(time.RFC3339, fields[3])
		if err != nil {
			c.log.Global().Error(operation, "a commit's author date was unreadable", dlog.Context{
				"dir":   dir,
				"line":  line,
				"cause": err.Error(),
			})
			return nil, fmt.Errorf("gitclient: unreadable author date %q in %s: %w", fields[3], dir, err)
		}
		commits = append(commits, Commit{
			SHA:     fields[0],
			Subject: fields[1],
			Author:  fields[2],
			At:      at,
		})
	}
	return commits, nil
}

// splitNUL splits git's `-z` output into paths, dropping the trailing empty
// piece the final NUL leaves behind.
func splitNUL(out string) []string {
	trimmed := strings.Trim(out, "\x00")
	if trimmed == "" {
		return nil
	}
	return strings.Split(trimmed, "\x00")
}

// pathPresent reports whether a path exists on disk. A stat error that is not
// "not there" is surfaced rather than read as absence: answering "gone" for a
// directory we merely could not look at would turn a permissions problem into
// a silently skipped removal.
func pathPresent(path string) (bool, error) {
	_, err := os.Lstat(path)
	switch {
	case err == nil:
		return true, nil
	case os.IsNotExist(err):
		return false, nil
	default:
		return false, fmt.Errorf("gitclient: reading %s: %w", path, err)
	}
}

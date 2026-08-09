// rebasecleanup.go owns the TEARDOWN of a merge's temporary rebase worktree,
// and it owns ALL of it: every call site in this package routes through the one
// funnel below.
//
// WHY ONE FUNNEL. Teardown used to be spelled twice over the same directory —
// merge.Driver removed the tree from its own `rebaseOnto` defer, and
// merge.Coordinator removed it again from `settle`'s defer, using the WorkDir
// the Result still carried. The first call succeeded and logged REMOVED; the
// second ran `git worktree remove --force` against a path git had already
// forgotten, got exit 128 ("is not a working tree"), and logged a loud FAILED
// line for a directory that was already gone. Both call sites are correct —
// neither can be deleted, because a PARKED outcome hands the tree to the
// coordinator and a terminal one does not — so the fix is that removing an
// already-removed worktree is a SILENT NO-OP rather than a failure.
//
// THE POSTCONDITION IS WHAT DECIDES SUCCESS, not the exit status of any one
// step. The funnel's promise is: the worktree directory is gone, the temp
// PARENT directory createRebaseWorktree made is gone with it, and git no longer
// registers either. A `git worktree remove` that refused a tree the filesystem
// removal then took away leaves nothing behind and is administrative noise; a
// tree that is STILL THERE afterwards is a real failure and stays loud. That is
// the distinction the old code could not draw, because it judged each step
// rather than the result.
//
// A CLEANUP FAILURE NEVER FAILS A MERGE. The commits are on the target or they
// are not, and a directory that would not delete says nothing about either. The
// error is returned so the caller can log it loudly; no caller turns it into an
// outcome.
package merge

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
)

// rebaseWorktreeMarker is the path component every rebase worktree carries. It
// is what makes an abandoned one RECOGNIZABLE — to the next merge (which prunes
// what it finds registered), to the boot sweep (which finds what is left on
// disk), and to the removal root below (which refuses to delete a parent that
// is not one of ours).
const rebaseWorktreeMarker = "agent-repl-merge-rebase-"

// Cleanup removes the rebase worktree req carries, and is a no-op for a request
// that has none.
//
// IT RUNS ON EVERY TERMINAL PATH — merged, failed for good, escalated, a
// superseded target-moved cycle, abandoned, shut down — because the worktree is
// the one piece of a merge that outlives the call that made it. The single
// exception is a CONFLICT-PARKED run, whose tree is the resolution's workbench:
// merge.Coordinator holds it open on purpose and tears it down from `settle`'s
// defer when the run finally reaches its terminal.
//
// IT IS IDEMPOTENT. Calling it twice for one path is a silent no-op the second
// time, which is what lets the driver clean up its own cycles without making
// the coordinator's terminal defer — the one placement no exit path can miss —
// into a source of false alarms.
func (e *Driver) Cleanup(ctx context.Context, req Request) error {
	if req.WorkDir == "" {
		return nil
	}
	return e.removeRebaseWorktree(ctx, req.TargetDir, req.WorkDir, req.Name)
}

// removeRebaseWorktree is THE funnel. Everything that tears down a rebase
// worktree in this package goes through it.
//
// The order is deliberate: git first (so the registration goes while the tree
// it names is still there), the filesystem second (so a git refusal cannot
// leave the bytes behind), the postcondition third (so success is judged on the
// result rather than on any step), and `git worktree prune` last (so the
// repository's administrative record can never outlive the directory).
func (e *Driver) removeRebaseWorktree(ctx context.Context, targetDir, workDir, name string) error {
	present, err := pathPresent(workDir)
	if err != nil {
		return fmt.Errorf("merge: reading the rebase worktree %s for %q: %w", workDir, name, err)
	}

	var errs []error

	// A path that is already gone gets NO `git worktree remove`: git would exit
	// 128 for a working tree that is not there, and a second teardown of a
	// worktree this pipeline already removed is the ordinary case rather than a
	// fault. The prune below is what clears whatever registration survived it.
	var gitErr error
	if present {
		// --force is what a tree parked on a conflict needs: it has a paused
		// cherry-pick in it and git refuses a plain remove.
		exit, out, rerr := e.gitExit(ctx, targetDir, "worktree", "remove", "--force", workDir)
		switch {
		case rerr != nil:
			gitErr = rerr
		case exit != 0:
			gitErr = fmt.Errorf("merge: `git worktree remove --force %s` exited %d in %s: %s",
				workDir, exit, targetDir, dlog.Clamp(out, 400))
		}
	}

	// THE FILESYSTEM REMOVAL RUNS EVEN WHEN git FAILED, and it removes the TEMP
	// PARENT rather than only the leaf. createRebaseWorktree makes the parent
	// (os.MkdirTemp) and lets git make the `rebase` leaf inside it, so a
	// teardown that stops at the leaf leaks one directory per merge — which is
	// exactly the pile of agent-repl-merge-rebase-* directories this repository
	// accumulated in $TMPDIR.
	root := rebaseRemovalRoot(workDir)
	if rmErr := os.RemoveAll(root); rmErr != nil {
		errs = append(errs, fmt.Errorf("merge: removing the rebase worktree's temporary directory %s for %q: %w", root, name, rmErr))
	}

	// THE POSTCONDITION. git's own verdict only becomes a reported failure when
	// the tree survived everything above it.
	stillThere, serr := pathPresent(workDir)
	switch {
	case serr != nil:
		errs = append(errs, fmt.Errorf("merge: re-reading the rebase worktree %s for %q: %w", workDir, name, serr))
	case stillThere && gitErr != nil:
		errs = append(errs, gitErr)
	case stillThere:
		errs = append(errs, fmt.Errorf("merge: the rebase worktree %s for %q is STILL PRESENT after `git worktree remove --force` and after removing %s", workDir, name, root))
	}

	// The prune runs on every pass, including a no-op one: a registration whose
	// directory is already gone is precisely what it exists to retire, and the
	// ~190 stale entries this repository's `git worktree list` carried are what
	// its absence looks like.
	if pexit, pout, perr := e.gitExit(ctx, targetDir, "worktree", "prune"); perr != nil {
		errs = append(errs, perr)
	} else if pexit != 0 {
		errs = append(errs, fmt.Errorf("merge: `git worktree prune` exited %d in %s: %s", pexit, targetDir, dlog.Clamp(pout, 400)))
	}

	if len(errs) > 0 {
		return errors.Join(errs...)
	}
	// ONLY A PASS THAT ACTUALLY REMOVED SOMETHING SAYS SO. A second teardown of
	// an already-removed tree logs nothing at all, which is the whole point of
	// making it a no-op rather than a failure.
	if present {
		e.logf("merge: rebase worktree REMOVED {ws=%s work=%s}", name, workDir)
	}
	return nil
}

// rebaseRemovalRoot is what the funnel deletes from the filesystem: the TEMP
// PARENT createRebaseWorktree owns when workDir is the `rebase` leaf it made
// under one, and workDir itself otherwise.
//
// THE MARKER CHECK IS A SAFETY RAIL, not a formality. This function's answer is
// handed to os.RemoveAll, and a workDir that is not shaped the way this pipeline
// makes them must never widen that removal to a parent directory somebody else
// owns.
func rebaseRemovalRoot(workDir string) string {
	parent := filepath.Dir(workDir)
	if strings.HasPrefix(filepath.Base(parent), rebaseWorktreeMarker) {
		return parent
	}
	return workDir
}

// pathPresent reports whether path exists. A "does not exist" is the ONLY error
// it absorbs: any other stat failure means the answer is unknown, and an
// unknown answer must never be read as "already gone".
func pathPresent(path string) (bool, error) {
	_, err := os.Lstat(path)
	switch {
	case err == nil:
		return true, nil
	case errors.Is(err, fs.ErrNotExist):
		return false, nil
	default:
		return false, err
	}
}

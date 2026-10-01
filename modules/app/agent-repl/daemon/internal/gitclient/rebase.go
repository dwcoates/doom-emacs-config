package gitclient

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
)

// This file holds the merge queue's REBASE and the two other gits the queue's
// rebase-first method needs: a worktree of an existing branch, and a fetch.
//
// THE REBASE REPLAYS ONE COMMIT PER COMMAND. The queue reports "rebasing k/n"
// and the command running for each commit, so the rebase is driven as an
// interactive rebase whose todo list the daemon writes itself: every commit is
// a `pick`, and a `break` follows every pick but the last. StartRebase replays
// the first commit and stops at its break; each ContinueRebase replays the
// next. The command replaying the last commit finishes the rebase, which is
// what moves the branch. A conflict stops the rebase where git stopped it, IN
// PROGRESS, with the conflicted paths answered: the caller hands the worktree
// to a resolution and continues, or leaves the rebase exactly where it is.

// RebaseStep is where a rebase stands after one of its commands. Exactly one
// of three things is true: Done (every commit is replayed and the branch has
// moved), Conflicted (the current commit stopped on conflicts, which are
// listed), or neither (the current commit replayed cleanly and the rebase
// stopped at the break after it, waiting to be continued).
type RebaseStep struct {
	// Done reports the rebase finished.
	Done bool
	// Conflicted lists the paths the current commit left conflicted, nil when
	// it replayed cleanly.
	Conflicted []string
}

// rebaseEnv are the bindings every rebase command carries. The daemon has no
// terminal, so no editor may ever be opened: GIT_EDITOR=true accepts every
// commit message as it stands (the replayed commit's own, or a resolved
// conflict's). The sequence editor is set per start, where the todo is known.
var rebaseEnv = []string{"GIT_EDITOR=true"}

// CommitsBetween lists the commits reachable from tip and not from base,
// OLDEST FIRST, merges excluded: exactly the commits a rebase of tip onto base
// replays, in the order it replays them.
func (c *client) CommitsBetween(ctx context.Context, dir, base, tip string) ([]Commit, error) {
	const operation = "daemon.gitclient.commits_between"
	out, err := c.run(ctx, operation, dir,
		"rev-list", "--reverse", "--no-merges", "--format="+commitFormat, "--no-commit-header",
		base+".."+tip)
	if err != nil {
		return nil, err
	}
	return c.parseCommits(operation, dir, out)
}

// StartRebase begins replaying commits, in order, onto onto in dir's checkout,
// and answers where the rebase stands after the FIRST of them. commits must be
// what CommitsBetween(onto, HEAD) answers; an empty list is refused, because a
// rebase with nothing to replay is a caller that should not have started one.
func (c *client) StartRebase(ctx context.Context, dir, onto string, commits []string) (RebaseStep, error) {
	const operation = "daemon.gitclient.start_rebase"
	if len(commits) == 0 {
		err := fmt.Errorf("gitclient: a rebase onto %s in %s was asked to replay no commits", onto, dir)
		c.log.Global().Error(operation, "refused a rebase with no commits to replay", dlog.Context{"dir": dir, "onto": onto})
		return RebaseStep{}, err
	}
	todo, err := writeRebaseTodo(commits)
	if err != nil {
		c.log.Global().Error(operation, "could not write the rebase's todo list", dlog.Context{"dir": dir, "onto": onto, "cause": err.Error()})
		return RebaseStep{}, err
	}
	defer func() {
		if rmErr := os.Remove(todo); rmErr != nil {
			c.log.Global().Error(operation, "could not remove the rebase's todo list", dlog.Context{"dir": dir, "todo": todo, "cause": rmErr.Error()})
		}
	}()
	// THE SEQUENCE EDITOR COPIES THE DAEMON'S TODO OVER GIT'S. git runs it as
	// `<editor> <todo-path>`, so `cp '<ours>'` makes the list the daemon wrote
	// the list git replays, with its breaks.
	env := append([]string{"GIT_SEQUENCE_EDITOR=cp " + shellQuote(todo)}, rebaseEnv...)
	return c.rebaseCommand(ctx, operation, dir, env, "rebase", "-i", "--empty=drop", onto)
}

// ContinueRebase replays the next commit of the rebase standing in dir: after
// a break, or after a conflict whose resolution is staged.
func (c *client) ContinueRebase(ctx context.Context, dir string) (RebaseStep, error) {
	return c.rebaseCommand(ctx, "daemon.gitclient.continue_rebase", dir, rebaseEnv, "rebase", "--continue")
}

// rebaseCommand runs one rebase command and reads where the rebase stands.
// Exit 0 is a replayed commit: the rebase is Done when no rebase is in
// progress any more, and stopped at a break otherwise. A nonzero exit is a
// conflict only when git left conflicted paths; anything else is a real
// failure carrying git's own words, and the rebase is left as git left it.
func (c *client) rebaseCommand(ctx context.Context, operation, dir string, env []string, args ...string) (RebaseStep, error) {
	in, err := c.runRawWithEnv(ctx, operation, dir, env, args...)
	if err != nil {
		return RebaseStep{}, err
	}
	if in.exitCode == 0 {
		inProgress, err := c.RebaseInProgress(ctx, dir)
		if err != nil {
			return RebaseStep{}, err
		}
		return RebaseStep{Done: !inProgress}, nil
	}
	conflicted, err := c.ConflictedFiles(ctx, dir)
	if err != nil {
		return RebaseStep{}, err
	}
	if len(conflicted) == 0 {
		c.log.Global().Error(operation, "the rebase failed without leaving conflicts", in.logContext())
		return RebaseStep{}, in.fail()
	}
	c.log.Global().Info(operation, "the rebase stopped on conflicts; it is left in progress for their resolution", dlog.Context{
		"dir":        dir,
		"exit_code":  in.exitCode,
		"conflicted": conflicted,
		"stdout":     in.stdout,
		"stderr":     in.stderr,
	})
	return RebaseStep{Conflicted: conflicted}, nil
}

// RebaseInProgress reports whether an interactive rebase stands in dir's
// checkout: git's own rebase-merge directory for that worktree exists.
func (c *client) RebaseInProgress(ctx context.Context, dir string) (bool, error) {
	const operation = "daemon.gitclient.rebase_in_progress"
	out, err := c.run(ctx, operation, dir, "rev-parse", "--git-path", "rebase-merge")
	if err != nil {
		return false, err
	}
	path := strings.TrimSpace(out)
	if !filepath.IsAbs(path) {
		path = filepath.Join(dir, path)
	}
	present, err := pathPresent(path)
	if err != nil {
		c.log.Global().Error(operation, "could not tell whether a rebase stands", dlog.Context{"dir": dir, "path": path, "cause": err.Error()})
		return false, err
	}
	return present, nil
}

// AddWorktree checks an EXISTING branch out at worktreeDir. It is the merge
// queue's worktree for a branch that has none, so it is NOT a workspace: no log
// sink is attached to it.
func (c *client) AddWorktree(ctx context.Context, repoDir, worktreeDir, branch string) error {
	_, err := c.run(ctx, "daemon.gitclient.add_worktree", repoDir, "worktree", "add", worktreeDir, branch)
	return err
}

// Fetch fetches remote into dir's repository. It is the ONE network git the
// daemon runs: a branch already merged upstream is landed locally by fetching
// the default branch and fast-forwarding to it. GIT_TERMINAL_PROMPT=0 (pinned
// on every git) makes a remote that wants a credential fail rather than wait.
func (c *client) Fetch(ctx context.Context, dir, remote string) error {
	_, err := c.run(ctx, "daemon.gitclient.fetch", dir, "fetch", remote)
	return err
}

// writeRebaseTodo writes the todo list a rebase of commits replays: a pick per
// commit, and a break after every pick but the last.
func writeRebaseTodo(commits []string) (string, error) {
	f, err := os.CreateTemp("", "agent-repl-rebase-todo-*")
	if err != nil {
		return "", fmt.Errorf("gitclient: creating the rebase todo list: %w", err)
	}
	var b strings.Builder
	for i, sha := range commits {
		b.WriteString("pick " + sha + "\n")
		if i < len(commits)-1 {
			b.WriteString("break\n")
		}
	}
	if _, err := f.WriteString(b.String()); err != nil {
		f.Close()
		os.Remove(f.Name())
		return "", fmt.Errorf("gitclient: writing the rebase todo list %s: %w", f.Name(), err)
	}
	if err := f.Close(); err != nil {
		os.Remove(f.Name())
		return "", fmt.Errorf("gitclient: closing the rebase todo list %s: %w", f.Name(), err)
	}
	return f.Name(), nil
}

// shellQuote quotes one word for sh, so a todo path with spaces or quotes in
// it reaches cp as one argument.
func shellQuote(word string) string {
	return "'" + strings.ReplaceAll(word, "'", `'\''`) + "'"
}

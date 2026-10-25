package worktreereap

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
)

// A LEFTOVER is a directory in a repository's worktrees directory that git no
// longer lists as a worktree and that holds NOTHING BUT LOG LINKS: symlinks
// named `*.log` directly in a `.claude/emacs` directory, and the directories
// leading to them. A runtime that re-made its log link inside a worktree while
// `git worktree remove` emptied it left exactly this behind, and the removal
// failed with "Directory not empty" (2026-10-25: eight of them under
// ~/.config/doom-worktrees, two links each).
//
// THE SHAPE IS THE WHOLE TEST, and it is strict. Removing one deletes the links
// and never what they name (the targets live under the state root). A
// directory holding anything else -- a file, a `.git`, a link named otherwise,
// a link elsewhere -- is no leftover and is kept; so is an empty one, which a
// worktree being made this instant could be; so is one an OPEN workspace names.

// logLinkDir is where a runtime puts a workspace's log links.
const logLinkDir = ".claude/emacs"

// sweepLeftovers removes the repository's leftovers. WORKTREES is git's
// listing, read once for the sweep: a listed directory is a worktree, never a
// leftover.
func (s *repoSweep) sweepLeftovers(ctx context.Context, worktrees []gitclient.Worktree) {
	fields := dlog.Context{"repo": string(s.repo.ID), "repo_dir": s.repo.Dir}
	parent, err := s.r.deps.WorktreeParent(s.repo.Dir)
	if err != nil {
		s.fail(ctx, opLeftover, "the repository's worktrees directory could not be resolved; no leftover is swept", fields, err)
		return
	}
	fields["worktrees_dir"] = parent
	entries, err := os.ReadDir(parent)
	if errors.Is(err, fs.ErrNotExist) {
		return
	}
	if err != nil {
		s.fail(ctx, opLeftover, "the repository's worktrees directory could not be read; no leftover is swept", fields, err)
		return
	}
	listed := map[string]bool{}
	for _, wt := range worktrees {
		listed[canonical(wt.Dir)] = true
	}
	for _, entry := range entries {
		if ctx.Err() != nil {
			return
		}
		if !entry.IsDir() {
			continue
		}
		dir := filepath.Join(parent, entry.Name())
		if listed[canonical(dir)] {
			continue
		}
		s.judgeLeftover(ctx, dir)
	}
}

// judgeLeftover removes DIR when it is a leftover.
func (s *repoSweep) judgeLeftover(ctx context.Context, dir string) {
	fields := dlog.Context{"repo": string(s.repo.ID), "repo_dir": s.repo.Dir, "dir": dir}
	if ws, ok := s.known.byDir[canonical(dir)]; ok && !ws.Closed {
		fields["workspace"] = string(ws.ID)
		s.keepLeftover(keepOpenWorkspace, fields)
		return
	}
	links, err := onlyLogLinks(dir)
	if err != nil {
		s.fail(ctx, opLeftover, "a directory beside the worktrees could not be read; it is kept", fields, err)
		return
	}
	if links == 0 {
		s.keepLeftover(keepNotLeftover, fields)
		return
	}
	fields["log_links"] = links
	if s.opts.DryRun {
		s.report.Leftovers = append(s.report.Leftovers, dir)
		s.r.deps.Log.Info(opLeftover, "would remove a leftover directory that holds nothing but log links", fields)
		return
	}
	if err := os.RemoveAll(dir); err != nil {
		s.report.Kept[reasonRemoveFailed]++
		s.fail(ctx, opLeftover, "a leftover directory could not be removed; it is kept", fields, err)
		return
	}
	s.report.Leftovers = append(s.report.Leftovers, dir)
	s.r.deps.Log.Info(opLeftover, "removed a leftover directory that held nothing but log links", fields)
}

// keepLeftover counts and records a directory beside the worktrees left alone.
func (s *repoSweep) keepLeftover(reason string, fields dlog.Context) {
	s.report.Kept[reason]++
	fields["reason"] = reason
	s.r.deps.Log.Debug(opLeftover, "kept a directory beside the worktrees", fields)
}

// onlyLogLinks answers how many log links DIR holds when it holds nothing
// else, and 0 when it holds anything else or nothing at all.
func onlyLogLinks(dir string) (int, error) {
	links := 0
	other := errors.New("not only log links")
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		rel, err := filepath.Rel(dir, path)
		if err != nil {
			return err
		}
		if d.Type()&fs.ModeSymlink == 0 || filepath.Dir(rel) != logLinkDir || !strings.HasSuffix(d.Name(), ".log") {
			return other
		}
		links++
		return nil
	})
	if errors.Is(err, other) {
		return 0, nil
	}
	if err != nil {
		return 0, err
	}
	return links, nil
}

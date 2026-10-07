package worktreereap

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"claude-repld/internal/gitclient"
	"claude-repld/internal/wsm"
)

// activity is a worktree's most recent sign of life and where it was read.
type activity struct {
	At     time.Time
	Signal string
}

// adminFiles are the files in a worktree's own git directory that git writes
// whenever somebody works in the tree: HEAD on every checkout, switch and
// reset; the index on every add, commit, checkout and stat refresh; the HEAD
// reflog on every commit, checkout, rebase step and reset. HEAD is always
// there; the other two may legitimately be absent (a fresh tree, reflogs
// turned off).
var adminFiles = []struct {
	name     string
	required bool
}{
	{"HEAD", true},
	{"index", false},
	{filepath.Join("logs", "HEAD"), false},
}

// lastActivity is the NEWEST of every programmatic signal of work in a tree:
//
//   - the mtimes of the worktree's admin files (above), which move for every
//     git operation that changes what the tree holds or points at;
//   - the HEAD commit's COMMITTER time, which covers a commit made elsewhere
//     and fetched or reset in, whose admin-file writes may be older;
//   - for a registered workspace, the record's creation, last activity, last
//     selection and merge instants -- the daemon's own knowledge that a person
//     or an agent was there, and, through the merge stamp, the one fact that
//     keeps a worktree the merge queue is retiring out of this sweep.
//
// Files in the TREE itself are not read: a tree only qualifies when it is
// clean, and a clean tree's content is what its HEAD records.
func lastActivity(ctx context.Context, git Git, repoDir, admin string, wt gitclient.Worktree, record *wsm.Workspace) (activity, error) {
	var newest activity
	consider := func(at time.Time, signal string) {
		if at.After(newest.At) {
			newest = activity{At: at, Signal: signal}
		}
	}
	for _, f := range adminFiles {
		path := filepath.Join(admin, f.name)
		info, err := os.Stat(path)
		switch {
		case err == nil:
			consider(info.ModTime(), "admin:"+f.name)
		case errors.Is(err, os.ErrNotExist) && !f.required:
		case errors.Is(err, os.ErrNotExist):
			return activity{}, fmt.Errorf("worktreereap: the worktree's admin file %s is missing, so its activity cannot be told", path)
		default:
			return activity{}, fmt.Errorf("worktreereap: reading %s: %w", path, err)
		}
	}
	committed, err := git.CommitterTime(ctx, repoDir, wt.Head)
	if err != nil {
		return activity{}, err
	}
	consider(committed, "head_committed")
	if record != nil {
		consider(record.CreatedAt, "workspace_created")
		for _, stamp := range []struct {
			at     *time.Time
			signal string
		}{
			{record.LastActivityAt, "workspace_activity"},
			{record.LastSelectedAt, "workspace_selected"},
			{record.MergedAt, "workspace_merged"},
		} {
			if stamp.at != nil {
				consider(*stamp.at, stamp.signal)
			}
		}
	}
	return newest, nil
}

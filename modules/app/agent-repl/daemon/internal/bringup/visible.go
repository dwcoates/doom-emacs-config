package bringup

import (
	"context"
	"fmt"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// ONLY OPEN AND VISIBLE WORKSPACES RUN (owner principle 1,
// docs/protobuf-design/open-tabs.md). A workspace the user hid, or whose
// repository section is collapsed, has no session started for it by a boot, a
// takeover or an open; it is started when shown, by the editor's OpenTabs. A
// hidden workspace's session already running is kept until its cache lapses
// (cachelapse.Keeper), which this file does not decide.
// Every site that decides what to start reads this file, and this file reads
// the one statement of visibility (wsm.Workspace.Runs).

// RepositoryLister lists the registered repositories, whose folds decide
// which of their workspaces are collapsed, and the merge queues, which decide
// whether a hidden merge workspace still carries a merge. wsm.DB is the
// production one.
type RepositoryLister interface {
	ListRepositories(ctx context.Context) ([]wsm.Repository, error)
	AllMergeQueues(ctx context.Context) (map[wsm.RepoKey][]wsm.MergeQueueEntry, error)
}

// WorkspaceReader reads one workspace's record. wsm.DB is the production one.
type WorkspaceReader interface {
	Workspace(ctx context.Context, id ids.WorkspaceID) (wsm.Workspace, error)
}

// VisibilityReader is what Visible reads.
type VisibilityReader interface {
	RepositoryLister
	WorkspaceReader
}

// folds reads every repository's fold.
func folds(ctx context.Context, repos RepositoryLister) (map[ids.RepoID]bool, error) {
	list, err := repos.ListRepositories(ctx)
	if err != nil {
		return nil, fmt.Errorf("bringup: list the repositories to read their folds: %w", err)
	}
	out := make(map[ids.RepoID]bool, len(list))
	for _, r := range list {
		out[r.ID] = r.Folded
	}
	return out, nil
}

// queuedMerges answers which workspaces have an entry standing in a merge
// queue: every workspace's verdict reads them (wsm.Workspace.Runs).
func queuedMerges(ctx context.Context, repos RepositoryLister) (map[ids.WorkspaceID]bool, error) {
	queues, err := repos.AllMergeQueues(ctx)
	if err != nil {
		return nil, fmt.Errorf("bringup: read the merge queues to tell which workspaces carry a merge: %w", err)
	}
	out := map[ids.WorkspaceID]bool{}
	for _, entries := range queues {
		for _, entry := range entries {
			out[entry.Workspace] = true
		}
	}
	return out, nil
}

// MergeQueued reports whether WS carries a merge: an entry of it stands in a
// merge queue, waiting or running (wsm.Workspace.Runs).
func MergeQueued(ctx context.Context, repos RepositoryLister, ws wsm.Workspace) (bool, error) {
	queued, err := queuedMerges(ctx, repos)
	if err != nil {
		return false, err
	}
	return queued[ws.ID], nil
}

// OnlyVisible splits pending into the workspaces the daemon runs and the ones
// it withholds (hidden, in a collapsed repository section, or a hidden merge
// workspace whose merge left every queue), each in
// pending's order. A fold or a queue that cannot be read is an error and splits nothing:
// starting a workspace that may be hidden would break the principle, and
// starting none silently would leave visible ones session-less, so the
// caller states the failure.
func OnlyVisible(ctx context.Context, repos RepositoryLister, pending []wsm.Workspace) (run, withheld []wsm.Workspace, err error) {
	if len(pending) == 0 {
		return nil, nil, nil
	}
	folded, err := folds(ctx, repos)
	if err != nil {
		return nil, nil, err
	}
	queued, err := queuedMerges(ctx, repos)
	if err != nil {
		return nil, nil, err
	}
	for _, ws := range pending {
		if ws.Runs(folded[ws.Repo], queued[ws.ID]) {
			run = append(run, ws)
			continue
		}
		withheld = append(withheld, ws)
	}
	return run, withheld, nil
}

// Visible reports whether the daemon runs one workspace, read afresh from its
// record and its repository's fold.
func Visible(ctx context.Context, db VisibilityReader, ws ids.WorkspaceID) (bool, error) {
	record, err := db.Workspace(ctx, ws)
	if err != nil {
		return false, fmt.Errorf("bringup: read the workspace record of %q: %w", ws, err)
	}
	run, _, err := OnlyVisible(ctx, db, []wsm.Workspace{record})
	if err != nil {
		return false, err
	}
	return len(run) == 1, nil
}

// IDs names workspaces for a record.
func IDs(list []wsm.Workspace) []string {
	out := make([]string, len(list))
	for i, ws := range list {
		out[i] = string(ws.ID)
	}
	return out
}

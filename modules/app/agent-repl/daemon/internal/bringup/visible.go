package bringup

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// ONLY OPEN AND VISIBLE WORKSPACES ARE OPENED AND KEPT WARM (owner principle
// 1, docs/protobuf-design/open-tabs.md). A workspace the user hid, or whose
// repository section or subsection is collapsed, is answered hidden by an
// open and beats no keep-alive. Whether a boot or a takeover brings a shim
// back is NOT this file's: only the record of which shims were up decides
// that (wasup.go, owner ruling 2026-10-28). A hidden workspace's session
// already running is kept until its cache lapses (cachelapse.Keeper).
// Every site that decides what to start reads this file, and this file reads
// the one statement of visibility (wsm.Workspace.Runs).

// RepositoryLister reads the arrangement, whose folds decide which
// workspaces are collapsed, and the merge queues, which decide whether a
// hidden merge workspace still carries a merge. wsm.DB is the production
// one.
type RepositoryLister interface {
	ReadArrangement(ctx context.Context) (wsm.Arrangement, error)
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

// Folds answers which workspaces a fold keeps off the tab bar, each with the
// collapse that folds it: the TAB BAR'S OWN STATEMENT of a folded tab
// (sidebar.Tabs), so a collapsed repository section, a collapsed subsection
// and a parent's folded children withhold exactly the workspaces whose tabs
// they fold, and what the daemon runs never disagrees with what the bar
// draws. A workspace with no tab (closed, a thread) is folded by nothing.
func Folds(ctx context.Context, repos RepositoryLister, log dlog.Logger) (map[ids.WorkspaceID]sidebar.Fold, error) {
	arrangement, err := repos.ReadArrangement(ctx)
	if err != nil {
		return nil, fmt.Errorf("bringup: read the arrangement to read its folds: %w", err)
	}
	reg := sidebar.Registry{Workspaces: arrangement.Workspaces, Repositories: arrangement.Repositories, Subsections: arrangement.Subsections, RepositoryBlocks: arrangement.RepositoryBlocks}
	out := map[ids.WorkspaceID]sidebar.Fold{}
	for _, tab := range sidebar.Tabs(reg, log) {
		if tab.Withheld != wsm.WithheldCollapsed {
			continue
		}
		if tab.Fold == nil {
			panic(fmt.Sprintf("bringup: the tab bar withheld workspace %s as collapsed and named no fold", tab.Workspace.ID))
		}
		out[tab.Workspace.ID] = *tab.Fold
	}
	return out, nil
}

// queuedMerges answers which workspaces have an entry standing in a merge
// queue, each with when it joined: every workspace's verdict reads them
// (wsm.Workspace.Runs).
func queuedMerges(ctx context.Context, repos RepositoryLister) (map[ids.WorkspaceID]time.Time, error) {
	queues, err := repos.AllMergeQueues(ctx)
	if err != nil {
		return nil, fmt.Errorf("bringup: read the merge queues to tell which workspaces carry a merge: %w", err)
	}
	out := map[ids.WorkspaceID]time.Time{}
	for _, entries := range queues {
		for _, entry := range entries {
			out[entry.Workspace] = entry.EnqueuedAt
		}
	}
	return out, nil
}

// QueuedIdleFactor is how many idle cutoffs a workspace whose merge stands in
// a queue is kept warm for (owner ruling, 2026-11-11): two days of keep-alives
// at the default day's cutoff, so a long queue's entries reach the head warm,
// and a merge whose session parks past them waits at its cold gate, skipped.
const QueuedIdleFactor = 2

// IdleHorizon answers the moment a workspace's session is idle past its
// cutoff: CUTOFF, the idle sweep's own, after ENGAGED; or, while its merge is
// QUEUED, QueuedIdleFactor of them after the later of ENGAGED and QUEUEDAT,
// when it joined its queue. The idle sweep and the cache keeper's keep-alives
// both read it, so a session is never kept warm past the moment the sweep
// parks it.
func IdleHorizon(cutoff time.Duration, engaged, queuedAt time.Time, queued bool) time.Time {
	if !queued {
		return engaged.Add(cutoff)
	}
	from := engaged
	if queuedAt.After(from) {
		from = queuedAt
	}
	return from.Add(QueuedIdleFactor * cutoff)
}

// MergeQueuedAt reports whether WS carries a merge, as MergeQueued does, and
// when its entry joined its queue.
func MergeQueuedAt(ctx context.Context, repos RepositoryLister, ws wsm.Workspace) (time.Time, bool, error) {
	queued, err := queuedMerges(ctx, repos)
	if err != nil {
		return time.Time{}, false, err
	}
	at, ok := queued[ws.ID]
	return at, ok, nil
}

// MergeQueued reports whether WS carries a merge: an entry of it stands in a
// merge queue, waiting or running (wsm.Workspace.Runs).
func MergeQueued(ctx context.Context, repos RepositoryLister, ws wsm.Workspace) (bool, error) {
	_, queued, err := MergeQueuedAt(ctx, repos, ws)
	return queued, err
}

// OnlyVisible splits pending into the workspaces the daemon runs and the ones
// it withholds, each in pending's order, by wsm.Workspace.Runs: the persisted
// visibility, widened by a queued merge, the deploy mediator and a fold's
// spared one-shots. A boot or a redeploy reads the visibility the last daemon
// persisted, so it starts nothing for a workspace that is not visible. A fold or a queue that cannot be read is an error and splits nothing:
// starting a workspace that may be hidden would break the principle, and
// starting none silently would leave visible ones session-less, so the
// caller states the failure.
func OnlyVisible(ctx context.Context, repos RepositoryLister, log dlog.Logger, pending []wsm.Workspace) (run, withheld []wsm.Workspace, err error) {
	if len(pending) == 0 {
		return nil, nil, nil
	}
	folded, err := Folds(ctx, repos, log)
	if err != nil {
		return nil, nil, err
	}
	queued, err := queuedMerges(ctx, repos)
	if err != nil {
		return nil, nil, err
	}
	for _, ws := range pending {
		if verdict(ws, folded, queued).Runs {
			run = append(run, ws)
			continue
		}
		withheld = append(withheld, ws)
	}
	return run, withheld, nil
}

// Visibility is whether the daemon runs one workspace, and the collapse that
// folds it when one does.
type Visibility struct {
	// Runs reports that the daemon runs the workspace (wsm.Workspace.Runs).
	Runs bool
	// Fold is the collapse that folds the workspace's tab off the tab bar,
	// nil when none does.
	Fold *sidebar.Fold
}

// Visible reports whether the daemon runs one workspace, read afresh from its
// record and the folds of its section.
func Visible(ctx context.Context, db VisibilityReader, log dlog.Logger, ws ids.WorkspaceID) (bool, error) {
	visibility, err := VisibilityOf(ctx, db, log, ws)
	return visibility.Runs, err
}

// VisibilityOf answers Visible's verdict and the fold behind it from ONE read
// of the record, the folds and the queues, so the fold a record names is the
// one the verdict was decided on.
func VisibilityOf(ctx context.Context, db VisibilityReader, log dlog.Logger, ws ids.WorkspaceID) (Visibility, error) {
	record, err := db.Workspace(ctx, ws)
	if err != nil {
		return Visibility{}, fmt.Errorf("bringup: read the workspace record of %q: %w", ws, err)
	}
	folded, err := Folds(ctx, db, log)
	if err != nil {
		return Visibility{}, err
	}
	queued, err := queuedMerges(ctx, db)
	if err != nil {
		return Visibility{}, err
	}
	return verdict(record, folded, queued), nil
}

// verdict is WS's Visibility under the FOLDED and QUEUED read once for every
// workspace a caller judges: the one place OnlyVisible and VisibilityOf turn
// them into wsm.Workspace.Runs.
func verdict(ws wsm.Workspace, folded map[ids.WorkspaceID]sidebar.Fold, queued map[ids.WorkspaceID]time.Time) Visibility {
	out := Visibility{}
	fold, isFolded := folded[ws.ID]
	if isFolded {
		out.Fold = &fold
	}
	_, inQueue := queued[ws.ID]
	out.Runs = ws.Runs(isFolded, inQueue)
	return out
}

// IDs names workspaces for a record.
func IDs(list []wsm.Workspace) []string {
	out := make([]string, len(list))
	for i, ws := range list {
		out[i] = string(ws.ID)
	}
	return out
}

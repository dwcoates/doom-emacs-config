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
// that (wasup.go, owner ruling 2026-10-28). A backlogged workspace's session
// already running is kept until its cache lapses (cachelapse.Keeper).
// Every site that decides what to start reads this file, and this file reads
// the one statement of visibility (wsm.Workspace.Runs).

// QueueLister reads the merge queues, which decide whether a hidden merge
// workspace still carries a merge. wsm.DB is the production one.
type QueueLister interface {
	AllMergeQueues(ctx context.Context) (map[wsm.RepoKey][]wsm.MergeQueueEntry, error)
}

// RepositoryLister reads the arrangement, whose folds decide which
// workspaces are collapsed, and the merge queues. wsm.DB is the production
// one.
type RepositoryLister interface {
	QueueLister
	ReadArrangement(ctx context.Context) (wsm.Arrangement, error)
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
	return sidebar.CollapsedFolds(sidebar.Tabs(reg, log)), nil
}

// queuedMerges answers which workspaces have an entry standing in a merge
// queue, each with when it joined: every workspace's verdict reads them
// (wsm.Workspace.Runs).
func queuedMerges(ctx context.Context, repos QueueLister) (map[ids.WorkspaceID]time.Time, error) {
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
func MergeQueuedAt(ctx context.Context, repos QueueLister, ws wsm.Workspace) (time.Time, bool, error) {
	queued, err := queuedMerges(ctx, repos)
	if err != nil {
		return time.Time{}, false, err
	}
	at, ok := queued[ws.ID]
	return at, ok, nil
}

// MergeQueued reports whether WS carries a merge: an entry of it stands in a
// merge queue, waiting or running (wsm.Workspace.Runs).
func MergeQueued(ctx context.Context, repos QueueLister, ws wsm.Workspace) (bool, error) {
	_, queued, err := MergeQueuedAt(ctx, repos, ws)
	return queued, err
}

// OnlyVisible splits pending into the workspaces the daemon runs and the ones
// it withholds, each in pending's order, by wsm.Workspace.Runs: the persisted
// visibility, widened by a queued merge, the deploy mediator and a fold's
// spared one-shots. A boot or a redeploy reads the visibility the last daemon
// persisted, so it starts nothing for a workspace that is not visible. A fold or a queue that cannot be read is an error and splits nothing:
// starting a workspace that may be backlogged would break the principle, and
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
		var fold *sidebar.Fold
		if f, isFolded := folded[ws.ID]; isFolded {
			fold = &f
		}
		_, inQueue := queued[ws.ID]
		if verdict(ws, fold, inQueue).Runs {
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

// LiveVisibility answers a workspace's visibility as the roster derived it
// last (sidebar.Resolver.Visibility).
type LiveVisibility func(ws ids.WorkspaceID) wsm.Visibility

// WithLiveVisibility answers RECORDS, each carrying LIVE's visibility in place
// of the stored one. A running daemon decides by the roster's live value: the
// store's copy is written a moment after the push that changed it, so a
// decision read off the store could act on the visibility the editor has
// already left. Only a boot, which runs before any render, reads the store's.
func WithLiveVisibility(records []wsm.Workspace, live LiveVisibility) []wsm.Workspace {
	out := make([]wsm.Workspace, len(records))
	for i, ws := range records {
		ws.Visibility = live(ws.ID)
		out[i] = ws
	}
	return out
}

// LiveStanding answers a workspace's visibility and the collapse that folds
// its tab (nil when none does), both as the roster's last render derived them
// (sidebar.Resolver.Standing).
type LiveStanding func(ws ids.WorkspaceID) (wsm.Visibility, *sidebar.Fold)

// Judge answers whether the daemon runs RECORD, and the fold behind the
// verdict, by the roster's LIVE standing and QUEUED, whether the caller read
// an entry of it standing in a merge queue. A running daemon's stand-downs
// judge by it, so the fold a record names is the one the verdict was decided
// on, and no stand-down reads the arrangement from the store: the roster
// holds it, and a fleet's detach once read it whole once per withheld
// workspace, holding the state database's one connection for seconds
// (2026-11-25 11:36:24-31). A boot, which runs before any render, reads the
// stored folds instead (OnlyVisible).
func Judge(record wsm.Workspace, queued bool, live LiveStanding) Visibility {
	visibility, fold := live(record.ID)
	record.Visibility = visibility
	return verdict(record, fold, queued)
}

// verdict is WS's Visibility under FOLD, the collapse that folds its tab, and
// INQUEUE, whether an entry of it stands in a merge queue: the one place
// OnlyVisible and Judge turn them into wsm.Workspace.Runs.
func verdict(ws wsm.Workspace, fold *sidebar.Fold, inQueue bool) Visibility {
	return Visibility{Runs: ws.Runs(fold != nil, inQueue), Fold: fold}
}

// IDs names workspaces for a record.
func IDs(list []wsm.Workspace) []string {
	out := make([]string, len(list))
	for i, ws := range list {
		out[i] = string(ws.ID)
	}
	return out
}

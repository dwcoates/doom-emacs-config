package sidebar

import (
	"errors"
	"fmt"
	"slices"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// WithClock replaces the clock a status change is stamped by, and the one
// the Recently Merged summary's last day is measured back from.
func WithClock(now func() time.Time) Option {
	return func(r *resolver) {
		r.now = now
	}
}

// MERGED WORKSPACES ARE IN NO STREAM (frontend.v1.RosterPush). The Recently
// Merged stream carries a summary of them, and their rows are paged on
// demand (agentrepl.v1.ListMergedWorkspaces) by MergedPage, ten at a time,
// from the same merged set the summary was resolved from.

// MergedPageSize is how many merged rows one page holds
// (agentrepl.v1.ListMergedWorkspacesRequest.page).
const MergedPageSize = 10

// recentWindow is how far back from a resolve the summary lists every merge
// (frontend.v1.RosterMergedRepoGroupSummary.recent): a whole day, so today's
// merges at any later instant on any client's local date are within it.
const recentWindow = 24 * time.Hour

// MergedOrder is the order merged workspaces are paged in
// (agentrepl.v1.ListMergedWorkspacesOrder).
type MergedOrder int

const (
	// MergedByMerge is most recently merged first: the band's order.
	MergedByMerge MergedOrder = iota + 1
	// MergedByCreation is most recently created first.
	MergedByCreation
)

// String names the order for a record.
func (o MergedOrder) String() string {
	switch o {
	case MergedByMerge:
		return "most_recently_merged"
	case MergedByCreation:
		return "most_recently_created"
	default:
		return fmt.Sprintf("merged_order(%d)", int(o))
	}
}

// MergedPageQuery is one ListMergedWorkspaces request, as the resolver
// answers it.
type MergedPageQuery struct {
	// Repository narrows the page to one repository's merged workspaces;
	// nil pages every repository's.
	Repository *workspacev1.RepositoryRef
	// Order is the order the pages walk.
	Order MergedOrder
	// Page is the 0-based page: places Page*MergedPageSize onward.
	Page uint32
}

// MergedPage is one page of merged rows and the merged set it was resolved
// from.
type MergedPage struct {
	// Revision is the merged set's token
	// (frontend.v1.RosterMergedSection.merged_revision).
	Revision uint64
	// Rows are at most MergedPageSize rows, in the order asked for.
	Rows []*frontendv1.RosterMergedRow
	// More reports whether merged workspaces follow this page, and Omitted
	// how many.
	More    bool
	Omitted uint32
}

// ErrMergedNotReady is MergedPage's answer before any registry was installed
// and published: the roster holds no merged set to page yet.
var ErrMergedNotReady = errors.New("sidebar: the roster has published no registry yet, so it holds no merged set")

// UnknownRepositoryError is MergedPage's answer for a repository the
// registry does not hold.
type UnknownRepositoryError struct {
	Ref *workspacev1.RepositoryRef
}

func (e *UnknownRepositoryError) Error() string {
	return fmt.Sprintf("sidebar: no registered repository is named by id %q dir %q", e.Ref.GetId(), e.Ref.GetDir())
}

// partition sorts the registry's workspaces into the one list each is drawn
// in (taborder.go). OPEN is neither closed nor hidden, whatever its merge
// time (OPEN BEATS MERGED: a merged workspace that was reopened is open);
// HIDDEN is open but hidden; MERGED is merged AND closed; CLOSED is closed
// and never merged. A HIDDEN MERGE WORKSPACE IS DRAWN NOWHERE until it needs
// the user (owner ruling, 2026-10-11): not even in the hidden section. A
// CHAT THREAD is listed only in the thread section. Caller holds mu.
type partition struct {
	hidden, merged, closed, threads []wsm.Workspace
}

// partition answers the registry's lists, derived once per registry install
// (registryIndex) and shared: a caller that edits a list edits a copy of it.
func (r *resolver) partition() partition {
	return r.state.index.lists
}

// partitionOf sorts WORKSPACES into their lists, as partition answers them.
func partitionOf(workspaces []wsm.Workspace) partition {
	var out partition
	for _, ws := range workspaces {
		switch {
		case ws.IsThread():
			out.threads = append(out.threads, ws)
		case ws.MergeHidden():
		case ws.InMergedBand():
			out.merged = append(out.merged, ws)
		case ws.Closed:
			out.closed = append(out.closed, ws)
		case ws.Hidden:
			out.hidden = append(out.hidden, ws)
		}
	}
	return out
}

// mergedSetOf is the set of merged workspaces' ids, in registry order.
func mergedSetOf(merged []wsm.Workspace) []ids.WorkspaceID {
	out := make([]ids.WorkspaceID, 0, len(merged))
	for _, ws := range merged {
		out = append(out, ws.ID)
	}
	slices.Sort(out)
	return out
}

// mergedToken answers the token of the merged set SET: the last one when
// the set is unchanged since the last published push, and otherwise the
// revision of the push this render becomes. A changed token makes the merged
// stream differ from the last push's, so that push does take the next
// revision (applyBatch). Caller holds mu.
func (r *resolver) mergedToken(set []ids.WorkspaceID) uint64 {
	if r.last != nil && slices.Equal(set, r.mergedSet) {
		return r.mergedRevision
	}
	return r.revision + 1
}

// mergedSection resolves the Recently Merged stream: ONE GROUP PER
// REPOSITORY that has a merged workspace, in the repository grouping's order,
// each carrying the summary of its merges and never a row. The header counts
// every merged workspace of every group, and TOKEN names the merged set.
//
// A group follows no repository fold: it folds by itself on the page.
func (r *resolver) mergedSection(merged []wsm.Workspace, token uint64, log dlog.Logger) *frontendv1.RosterMergedSection {
	byRepo := map[ids.RepoID][]wsm.Workspace{}
	for _, ws := range merged {
		byRepo[ws.Repo] = append(byRepo[ws.Repo], ws)
	}
	section := &frontendv1.RosterMergedSection{MergedRevision: &frontendv1.RosterMergedRevision{Revision: token}}
	since := r.now().Add(-recentWindow)
	total := 0
	for _, repo := range sortRepositories(r.state.reg.Repositories) {
		workspaces, hasMerged := byRepo[repo.ID]
		delete(byRepo, repo.ID)
		if !hasMerged {
			continue
		}
		total += len(workspaces)
		section.Groups = append(section.Groups, &frontendv1.RosterMergedRepoGroup{
			Key:     repoKey(repo),
			Header:  repoGroupHeader(repo),
			Summary: mergedSummary(sortMergedNewestFirst(workspaces), since),
		})
	}
	section.Header = sectionHeader(MergedSectionLabel, total)
	r.assertRepositoryInvariant(byRepo, "daemon.sidebar.merged_section", "the Recently Merged band", log)
	setMergedFold(section, r.state.reg.View.MergedFolded)
	setMergedHeight(section, r.state.reg.View.MergedHeight)
	return section
}

// mergedSummary is one group's summary of NEWEST, its merges most recent
// first: how many, the latest's instant, and every merge landed at or after
// SINCE, each with its commits as its row's detail states them.
func mergedSummary(newest []wsm.Workspace, since time.Time) *frontendv1.RosterMergedRepoGroupSummary {
	out := &frontendv1.RosterMergedRepoGroupSummary{
		Count:  &frontendv1.RosterMergedRepoGroupCount{Merges: uint32(len(newest))},
		Latest: &frontendv1.RosterMergedRepoGroupLatest{MergedAtMs: newest[0].MergedAt.UnixMilli()},
	}
	for _, ws := range newest {
		if ws.MergedAt.Before(since) {
			break
		}
		recent := &frontendv1.RosterMergedRepoGroupRecentMerge{MergedAtMs: ws.MergedAt.UnixMilli()}
		if ws.CommitCount != nil {
			commits := *ws.CommitCount
			recent.Commits = &commits
		}
		out.Recent = append(out.Recent, recent)
	}
	return out
}

// MergedPage implements Resolver.
func (r *resolver) MergedPage(q MergedPageQuery) (MergedPage, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if !r.state.regSeen || r.last == nil {
		return MergedPage{}, ErrMergedNotReady
	}
	// A MERGE WHOSE REPOSITORY THE REGISTRY DOES NOT CARRY is in no group of
	// the summary (mergedSection records it), so no page lists it either.
	merged := slices.DeleteFunc(slices.Clone(r.partition().merged), func(ws wsm.Workspace) bool {
		_, ok := r.repositoryOf(ws)
		return !ok
	})
	if q.Repository != nil {
		repo, ok := wsm.RepositoryNamed(r.state.reg.Repositories, wsm.RepoID(q.Repository.GetId()), q.Repository.GetDir())
		if !ok {
			return MergedPage{}, &UnknownRepositoryError{Ref: q.Repository}
		}
		merged = slices.DeleteFunc(merged, func(ws wsm.Workspace) bool { return ws.Repo != repo.ID })
	}
	switch q.Order {
	case MergedByMerge:
		merged = sortMergedNewestFirst(merged)
	case MergedByCreation:
		merged = sortNewestFirst(merged, "merged", func(ws wsm.Workspace) *time.Time { return &ws.CreatedAt })
	default:
		panic(fmt.Sprintf("sidebar: MergedPage was handed order %d, which names no order", q.Order))
	}
	out := MergedPage{Revision: r.mergedRevision}
	start := int(q.Page) * MergedPageSize
	if start >= len(merged) {
		return out, nil
	}
	end := min(start+MergedPageSize, len(merged))
	rc := r.rowContext("")
	for _, ws := range merged[start:end] {
		out.Rows = append(out.Rows, &frontendv1.RosterMergedRow{Row: r.readRow(ws, rc)})
	}
	if end < len(merged) {
		out.More = true
		out.Omitted = uint32(len(merged) - end)
	}
	return out, nil
}

// MergedRows implements Resolver: every merged workspace's row as a page
// resolves it, in registry order, and the last push, under one lock.
func (r *resolver) MergedRows() ([]*frontendv1.RosterRow, *frontendv1.RosterPush, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if !r.state.regSeen || r.last == nil {
		return nil, nil, false
	}
	rc := r.rowContext("")
	var rows []*frontendv1.RosterRow
	for _, ws := range r.partition().merged {
		rows = append(rows, r.readRow(ws, rc))
	}
	return rows, r.last, true
}

// readRow resolves WS's row ON DEMAND, as a read: it derives no fact, so it
// feeds no durable sink and no departure, and its status is the arm the
// roster last derived for the workspace (row). Caller holds mu.
func (r *resolver) readRow(ws wsm.Workspace, rc rowContext) *frontendv1.RosterRow {
	r.readOnly = true
	defer func() { r.readOnly = false }()
	return r.row(ws, rc, r.logFor(ws.ID))
}

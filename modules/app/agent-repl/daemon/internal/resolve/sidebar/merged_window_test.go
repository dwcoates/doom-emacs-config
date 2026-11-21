package sidebar_test

import (
	"errors"
	"fmt"
	"slices"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// mergedAt is a merged workspace whose merge landed AGO before epoch.
func mergedAt(id string, ago time.Duration) wsm.Workspace {
	ws := workspace(id, id)
	ws.MergedAt = at(-ago)
	ws.Closed = true
	return ws
}

// withCommits is WS with a commit count on its record.
func withCommits(ws wsm.Workspace, n uint32) wsm.Workspace {
	ws.CommitCount = &n
	return ws
}

// summaryOf reads the only Recently Merged group's summary.
func summaryOf(t *testing.T, r sidebar.Resolver) *frontendv1.RosterMergedRepoGroupSummary {
	t.Helper()
	groups := latestPush(t, r).GetRecentlyMerged().GetGroups()
	if len(groups) != 1 {
		t.Fatalf("merged groups = %d, want exactly 1", len(groups))
	}
	return groups[0].GetSummary()
}

// recentOf spells a summary's recent merges as "ago:commits", commits "-"
// when uncounted.
func recentOf(summary *frontendv1.RosterMergedRepoGroupSummary) []string {
	var out []string
	for _, m := range summary.GetRecent() {
		commits := "-"
		if m.Commits != nil {
			commits = fmt.Sprint(m.GetCommits())
		}
		out = append(out, fmt.Sprintf("%s:%s", epoch.Sub(time.UnixMilli(m.GetMergedAtMs())), commits))
	}
	return out
}

func TestTheMergedStreamCarriesNoRow(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(mergedAt("m", time.Hour), workspace("o", "open")))

	// Assert: the merged workspace is in no stream, only the summary.
	push := latestPush(t, r)
	if row := rowFor(rosterwalk.AllRows(push), "m"); row != nil {
		t.Fatalf("a stream carries the merged workspace's row: %v", row)
	}
	if got := summaryOf(t, r).GetCount().GetMerges(); got != 1 {
		t.Fatalf("summary count = %d, want 1", got)
	}
}

func TestTheMergedSummary(t *testing.T) {
	tests := []struct {
		name       string
		merges     []wsm.Workspace
		wantCount  uint32
		wantLatest time.Duration
		wantRecent []string
	}{
		{
			name:       "one merge an hour ago, counted",
			merges:     []wsm.Workspace{withCommits(mergedAt("a", time.Hour), 3)},
			wantCount:  1,
			wantLatest: time.Hour,
			wantRecent: []string{"1h0m0s:3"},
		},
		{
			name:       "an uncounted merge leaves its commits unset",
			merges:     []wsm.Workspace{mergedAt("a", time.Hour)},
			wantCount:  1,
			wantLatest: time.Hour,
			wantRecent: []string{"1h0m0s:-"},
		},
		{
			name:       "the recent merges are listed most recent first",
			merges:     []wsm.Workspace{withCommits(mergedAt("old", 3*time.Hour), 1), withCommits(mergedAt("new", time.Hour), 2)},
			wantCount:  2,
			wantLatest: time.Hour,
			wantRecent: []string{"1h0m0s:2", "3h0m0s:1"},
		},
		{
			name:       "a merge exactly a day old is still recent",
			merges:     []wsm.Workspace{mergedAt("a", 24*time.Hour)},
			wantCount:  1,
			wantLatest: 24 * time.Hour,
			wantRecent: []string{"24h0m0s:-"},
		},
		{
			name:       "a merge older than a day is counted and not recent",
			merges:     []wsm.Workspace{mergedAt("old", 25*time.Hour), mergedAt("new", time.Hour)},
			wantCount:  2,
			wantLatest: time.Hour,
			wantRecent: []string{"1h0m0s:-"},
		},
		{
			name:       "no merge in the last day still states the latest",
			merges:     []wsm.Workspace{mergedAt("a", 72*time.Hour)},
			wantCount:  1,
			wantLatest: 72 * time.Hour,
			wantRecent: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistry(registry(tt.merges...))

			// Assert.
			summary := summaryOf(t, r)
			if got := summary.GetCount().GetMerges(); got != tt.wantCount {
				t.Fatalf("count = %d, want %d", got, tt.wantCount)
			}
			if got := summary.GetLatest().GetMergedAtMs(); got != epoch.Add(-tt.wantLatest).UnixMilli() {
				t.Fatalf("latest = %d, want %d", got, epoch.Add(-tt.wantLatest).UnixMilli())
			}
			if got := recentOf(summary); !equal(got, tt.wantRecent) {
				t.Fatalf("recent = %v, want %v", got, tt.wantRecent)
			}
		})
	}
}

func TestTheMergedHeaderCountsEveryGroupsMerges(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(twoRepoRegistry(mergedIn("a1", repo, 0), mergedIn("a2", repo, 0), mergedIn("b1", otherRepo, 0)))

	// Assert.
	if got := latestPush(t, r).GetRecentlyMerged().GetHeader().GetCount().GetWorkspaces(); got != 3 {
		t.Fatalf("header count = %d, want every group's 3 merges", got)
	}
}

func TestTheMergedToken(t *testing.T) {
	tests := []struct {
		name string
		// before and after are the registries installed in turn.
		before, after sidebar.Registry
		wantMoved     bool
	}{
		{
			name:      "an unchanged merged set keeps its token across another stream's change",
			before:    registry(mergedAt("m", time.Hour), workspace("o", "open")),
			after:     registry(mergedAt("m", time.Hour), workspace("o", "renamed")),
			wantMoved: false,
		},
		{
			name:      "a merge landing raises it",
			before:    registry(workspace("o", "open")),
			after:     registry(mergedAt("o", time.Minute)),
			wantMoved: true,
		},
		{
			name:   "a merged workspace reopened raises it",
			before: registry(mergedAt("m", time.Hour)),
			after: func() sidebar.Registry {
				ws := mergedAt("m", time.Hour)
				ws.Closed = false
				return registry(ws)
			}(),
			wantMoved: true,
		},
		{
			name:      "a merged workspace nuked raises it",
			before:    registry(mergedAt("m", time.Hour), mergedAt("n", time.Hour)),
			after:     registry(mergedAt("n", time.Hour)),
			wantMoved: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetRegistry(tt.before)
			token := latestPush(t, r).GetRecentlyMerged().GetMergedRevision().GetRevision()

			// Act.
			r.SetRegistry(tt.after)

			// Assert: a moved token is the revision of the push that moved it.
			push := latestPush(t, r)
			got := push.GetRecentlyMerged().GetMergedRevision().GetRevision()
			if moved := got != token; moved != tt.wantMoved {
				t.Fatalf("token %d -> %d, want moved = %v", token, got, tt.wantMoved)
			}
			if tt.wantMoved && got != push.GetRevision() {
				t.Fatalf("token = %d, want the push's revision %d", got, push.GetRevision())
			}
		})
	}
}

// pagedRegistry is N merged workspaces of the fixture repository, m-0 the
// newest merge and the oldest creation, each at its own place.
func pagedRegistry(n int) sidebar.Registry {
	var all []wsm.Workspace
	for i := range n {
		ws := placed(mergedAt(fmt.Sprintf("m-%02d", i), time.Duration(i)*time.Minute), int64(i+1))
		ws.CreatedAt = epoch.Add(time.Duration(i) * time.Second)
		all = append(all, ws)
	}
	return registry(all...)
}

// pageNames spells a page's rows by name.
func pageNames(page sidebar.MergedPage) []string {
	var out []string
	for _, row := range page.Rows {
		out = append(out, row.GetRow().GetName().GetText())
	}
	return out
}

func TestMergedPageBounds(t *testing.T) {
	tests := []struct {
		name        string
		merges      int
		page        uint32
		wantFirst   string
		wantRows    int
		wantMore    bool
		wantOmitted uint32
	}{
		{name: "page 0 of 25 is full with 15 after it", merges: 25, page: 0, wantFirst: "m-00", wantRows: 10, wantMore: true, wantOmitted: 15},
		{name: "a middle page is full with what is left after it", merges: 25, page: 1, wantFirst: "m-10", wantRows: 10, wantMore: true, wantOmitted: 5},
		{name: "the last page holds the rest and is the last", merges: 25, page: 2, wantFirst: "m-20", wantRows: 5, wantMore: false},
		{name: "a page past the end is a last page with no rows", merges: 25, page: 3, wantRows: 0, wantMore: false},
		{name: "exactly ten merges are one last page", merges: 10, page: 0, wantFirst: "m-00", wantRows: 10, wantMore: false},
		{name: "no merges answer an empty last page", merges: 0, page: 0, wantRows: 0, wantMore: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetRegistry(pagedRegistry(tt.merges))

			// Act.
			page, err := r.MergedPage(sidebar.MergedPageQuery{Order: sidebar.MergedByMerge, Page: tt.page})

			// Assert.
			if err != nil {
				t.Fatalf("MergedPage: %v", err)
			}
			names := pageNames(page)
			if len(names) != tt.wantRows || (tt.wantRows > 0 && names[0] != tt.wantFirst) {
				t.Fatalf("rows = %v, want %d from %s", names, tt.wantRows, tt.wantFirst)
			}
			if page.More != tt.wantMore || page.Omitted != tt.wantOmitted {
				t.Fatalf("more = %v omitted = %d, want %v %d", page.More, page.Omitted, tt.wantMore, tt.wantOmitted)
			}
		})
	}
}

func TestMergedPageOrders(t *testing.T) {
	// tie is two merges sharing a merge instant and a creation instant, the
	// lower place first in registry order.
	tie := func() sidebar.Registry {
		a := placed(mergedAt("a", time.Hour), 2)
		b := placed(mergedAt("b", time.Hour), 1)
		return registry(a, b)
	}
	tests := []struct {
		name  string
		reg   sidebar.Registry
		order sidebar.MergedOrder
		want  []string
	}{
		{name: "most recently merged first", reg: pagedRegistry(3), order: sidebar.MergedByMerge, want: []string{"m-00", "m-01", "m-02"}},
		{name: "most recently created first", reg: pagedRegistry(3), order: sidebar.MergedByCreation, want: []string{"m-02", "m-01", "m-00"}},
		{name: "a merge-instant tie keeps registry order", reg: tie(), order: sidebar.MergedByMerge, want: []string{"b", "a"}},
		{name: "a creation-instant tie keeps registry order", reg: tie(), order: sidebar.MergedByCreation, want: []string{"b", "a"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetRegistry(tt.reg)

			// Act.
			page, err := r.MergedPage(sidebar.MergedPageQuery{Order: tt.order})

			// Assert.
			if err != nil {
				t.Fatalf("MergedPage: %v", err)
			}
			if got := pageNames(page); !equal(got, tt.want) {
				t.Fatalf("rows = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestMergedPageRepositoryFilter(t *testing.T) {
	tests := []struct {
		name string
		ref  *workspacev1.RepositoryRef
		want []string
	}{
		{name: "unset pages every repository", ref: nil, want: []string{"a1", "b1"}},
		{name: "a repository by id pages only its merges", ref: &workspacev1.RepositoryRef{Id: string(otherRepo.ID)}, want: []string{"b1"}},
		{name: "a repository by dir pages only its merges", ref: &workspacev1.RepositoryRef{Dir: repo.Dir}, want: []string{"a1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetRegistry(twoRepoRegistry(mergedIn("a1", repo, time.Minute), mergedIn("b1", otherRepo, 0)))

			// Act.
			page, err := r.MergedPage(sidebar.MergedPageQuery{Repository: tt.ref, Order: sidebar.MergedByMerge})

			// Assert.
			if err != nil {
				t.Fatalf("MergedPage: %v", err)
			}
			if got := pageNames(page); !equal(got, tt.want) {
				t.Fatalf("rows = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestMergedPageRefusals(t *testing.T) {
	tests := []struct {
		name    string
		install bool
		ref     *workspacev1.RepositoryRef
		check   func(t *testing.T, err error)
	}{
		{
			name: "before any registry the roster is not ready",
			check: func(t *testing.T, err error) {
				if !errors.Is(err, sidebar.ErrMergedNotReady) {
					t.Fatalf("err = %v, want ErrMergedNotReady", err)
				}
			},
		},
		{
			name: "a repository the registry does not hold is unknown", install: true,
			ref: &workspacev1.RepositoryRef{Id: "repo-missing"},
			check: func(t *testing.T, err error) {
				var unknown *sidebar.UnknownRepositoryError
				if !errors.As(err, &unknown) || unknown.Ref.GetId() != "repo-missing" {
					t.Fatalf("err = %v, want an UnknownRepositoryError naming repo-missing", err)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			if tt.install {
				r.SetRegistry(registry(mergedAt("m", time.Hour)))
			}

			// Act.
			_, err := r.MergedPage(sidebar.MergedPageQuery{Repository: tt.ref, Order: sidebar.MergedByMerge})

			// Assert.
			tt.check(t, err)
		})
	}
}

func TestMergedPageCarriesTheSummarysToken(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(mergedAt("m", time.Hour)))
	r.SetRegistry(registry(mergedAt("m", time.Hour), mergedAt("n", time.Minute)))

	// Act.
	page, err := r.MergedPage(sidebar.MergedPageQuery{Order: sidebar.MergedByMerge})

	// Assert.
	if err != nil {
		t.Fatalf("MergedPage: %v", err)
	}
	if want := latestPush(t, r).GetRecentlyMerged().GetMergedRevision().GetRevision(); page.Revision != want {
		t.Fatalf("page token = %d, want the summary's %d", page.Revision, want)
	}
}

func TestMergedPageRowsCarryTheirMergedStatus(t *testing.T) {
	// Arrange: a live workspace whose merge lands.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	r.SetMerge(theWS, mergeStates["merged"])

	// Act.
	r.SetRegistry(registry(mergedAt(string(theWS), 0)))

	// Assert.
	rows := mergedRows(t, r)
	if len(rows) != 1 || statusName(rows[0]) != "merged" {
		t.Fatalf("merged rows = %v, want one on the merged arm", rows)
	}
}

func TestMergedPageFeedsNoDurableSink(t *testing.T) {
	// Arrange: every sink counts its reports, and a merged workspace whose
	// facts are already derived.
	var mu sync.Mutex
	reports := 0
	r, surfaces := newResolver(t,
		sidebar.WithStatusChangeSink(func(ids.WorkspaceID, time.Time) { mu.Lock(); reports++; mu.Unlock() }),
		sidebar.WithResultSink(func(ids.WorkspaceID, *wsm.TurnResult) { mu.Lock(); reports++; mu.Unlock() }))
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.SetRegistry(registry(mergedAt(string(theWS), 0)))
	mu.Lock()
	before := reports
	mu.Unlock()
	records := len(surfaces.Records())

	// Act.
	if _, err := r.MergedPage(sidebar.MergedPageQuery{Order: sidebar.MergedByMerge}); err != nil {
		t.Fatalf("MergedPage: %v", err)
	}
	if _, _, ok := r.MergedRows(); !ok {
		t.Fatal("MergedRows answered nothing")
	}

	// Assert.
	mu.Lock()
	defer mu.Unlock()
	if reports != before {
		t.Fatalf("sink reports %d -> %d, want a read to report nothing", before, reports)
	}
	for _, rec := range surfaces.Records()[records:] {
		switch rec.Operation {
		case "daemon.sidebar.status_arm_changed", "daemon.sidebar.result_changed", "daemon.sidebar.quiet_departure":
			t.Fatalf("a read recorded %s: %v", rec.Operation, rec)
		}
	}
}

func TestMergedRows(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(mergedAt("m", time.Hour), workspace("o", "open")))

	// Act.
	rows, push, ok := r.MergedRows()

	// Assert: the merged rows, with the very push the stream last published.
	if !ok {
		t.Fatal("MergedRows answered nothing")
	}
	if got := rowNames(rows); !equal(got, []string{"m"}) {
		t.Fatalf("rows = %v, want [m]", got)
	}
	if latest := latestPush(t, r); push != latest {
		t.Fatalf("push = %p, want the last published %p", push, latest)
	}
}

func TestMergedRowsBeforeAnyRegistryAnswersNothing(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	_, _, ok := r.MergedRows()

	// Assert.
	if ok {
		t.Fatal("MergedRows answered before any registry")
	}
}

func TestAMergedWorkspacesArmMoveIsStillAStatusChange(t *testing.T) {
	// Arrange: a live workspace lands its merge, so it is merged, on the
	// merged arm, and in no stream.
	var mu sync.Mutex
	var changes []ids.WorkspaceID
	r, surfaces := newResolver(t, sidebar.WithStatusChangeSink(func(ws ids.WorkspaceID, _ time.Time) {
		mu.Lock()
		defer mu.Unlock()
		changes = append(changes, ws)
	}))
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.SetMerge(theWS, mergeStates["merged"])
	r.SetRegistry(registry(mergedAt(string(theWS), 0)))
	mu.Lock()
	changes = nil
	mu.Unlock()

	// Act: its session ends, which recedes it to inactive.
	r.OnLink(theWS, shimclient.LinkDead)

	// Assert.
	mu.Lock()
	defer mu.Unlock()
	if len(changes) == 0 || slices.ContainsFunc(changes, func(ws ids.WorkspaceID) bool { return ws != theWS }) {
		t.Fatalf("status changes = %v, want only %s's", changes, theWS)
	}
	found := false
	for _, rec := range surfaces.Records() {
		if rec.Operation == "daemon.sidebar.status_arm_changed" && rec.Context["arm"] == "inactive" {
			found = true
		}
	}
	if !found {
		t.Fatalf("no status_arm_changed record onto inactive: %v", surfaces.Records())
	}
}

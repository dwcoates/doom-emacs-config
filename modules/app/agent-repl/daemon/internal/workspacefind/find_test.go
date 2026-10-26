package workspacefind

import (
	"reflect"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/wsm"
)

// at is a fixed instant DAYS days into the fixture's calendar.
func at(days int) time.Time {
	return time.Date(2026, 10, 1, 12, 0, 0, 0, time.UTC).AddDate(0, 0, days)
}

// ptr answers a pointer to V.
func ptr[T any](v T) *T { return &v }

var (
	doom  = wsm.Repository{ID: "repo-doom", Dir: "/src/doom"}
	other = wsm.Repository{ID: "repo-other", Dir: "/src/other"}
)

// record is a workspace of REPO created on day CREATED.
func record(id, name string, repo wsm.Repository, created int) wsm.Workspace {
	return wsm.Workspace{ID: wsm.WorkspaceID(id), Kind: wsm.WorkspaceKindRepository, Repo: repo.ID, Name: name, Branch: name, CreatedAt: at(created)}
}

// ids answers the found workspaces' ids, in order.
func ids(r Result) []string {
	var out []string
	for _, f := range r.Found {
		out = append(out, string(f.Workspace.ID))
	}
	return out
}

// fixture is three workspaces of two repositories: one open, one closed, one
// landed, with summaries on two.
func fixture() ([]wsm.Workspace, map[wsm.WorkspaceID]wsm.WorkspaceSummary) {
	open := record("open", "footer-fix", doom, 0)
	open.LastActivityAt = ptr(at(9))
	closed := record("closed", "topbar-title", doom, 2)
	closed.Closed = true
	closed.Closure = &wsm.Closure{At: at(4), By: wsm.CloseByUser}
	landed := record("landed", "store-prune", other, 5)
	landed.Closed = true
	landed.MergedAt = ptr(at(6))
	summaries := map[wsm.WorkspaceID]wsm.WorkspaceSummary{
		"open":   {Title: "Fix the footer's clock"},
		"landed": {Title: "Prune the store's old rows"},
	}
	return []wsm.Workspace{open, closed, landed}, summaries
}

func TestFindNarrowsByEveryCriterion(t *testing.T) {
	tests := []struct {
		name  string
		query Query
		want  []string
	}{
		{name: "nothing set finds every workspace, most recently active first", query: Query{}, want: []string{"open", "landed", "closed"}},
		{name: "a repository", query: Query{Repository: ptr(other.ID)}, want: []string{"landed"}},
		{name: "open", query: Query{Standing: OpenStanding}, want: []string{"open"}},
		{name: "closed includes landed", query: Query{Standing: ClosedStanding}, want: []string{"landed", "closed"}},
		{name: "landed", query: Query{Standing: LandedStanding}, want: []string{"landed"}},
		{name: "since drops a workspace last active before it", query: Query{Since: ptr(at(5))}, want: []string{"open", "landed"}},
		{name: "since is inclusive", query: Query{Since: ptr(at(4))}, want: []string{"open", "landed", "closed"}},
		{name: "until drops a workspace created after it", query: Query{Until: ptr(at(3))}, want: []string{"open", "closed"}},
		{name: "a window overlaps a workspace's life", query: Query{Since: ptr(at(3)), Until: ptr(at(3))}, want: []string{"open", "closed"}},
		{name: "text matches a name case-insensitively", query: Query{Text: ptr("TOPBAR")}, want: []string{"closed"}},
		{name: "text matches a summary", query: Query{Text: ptr("clock")}, want: []string{"open"}},
		{name: "text matching nothing finds nothing", query: Query{Text: ptr("nowhere")}, want: nil},
		{name: "criteria combine", query: Query{Repository: ptr(doom.ID), Standing: ClosedStanding}, want: []string{"closed"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			workspaces, summaries := fixture()

			// Act
			got, err := Find(workspaces, []wsm.Repository{doom, other}, summaries, tt.query)

			// Assert
			if err != nil {
				t.Fatalf("Find: %v", err)
			}
			if !reflect.DeepEqual(ids(got), tt.want) || got.Omitted != 0 {
				t.Fatalf("Find = %v (omitted %d), want %v", ids(got), got.Omitted, tt.want)
			}
		})
	}
}

func TestFindLimitsAndCountsWhatItOmitted(t *testing.T) {
	tests := []struct {
		name        string
		limit       int
		want        []string
		wantOmitted int
	}{
		{name: "a limit below the matches", limit: 1, want: []string{"open"}, wantOmitted: 2},
		{name: "a limit equal to the matches", limit: 3, want: []string{"open", "landed", "closed"}, wantOmitted: 0},
		{name: "a limit above the matches", limit: 10, want: []string{"open", "landed", "closed"}, wantOmitted: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			workspaces, summaries := fixture()

			// Act
			got, err := Find(workspaces, []wsm.Repository{doom, other}, summaries, Query{Limit: ptr(tt.limit)})

			// Assert
			if err != nil {
				t.Fatalf("Find: %v", err)
			}
			if !reflect.DeepEqual(ids(got), tt.want) || got.Omitted != tt.wantOmitted {
				t.Fatalf("Find = %v (omitted %d), want %v (omitted %d)", ids(got), got.Omitted, tt.want, tt.wantOmitted)
			}
		})
	}
}

func TestFindAnswersEachWorkspacesRepositoryAndSummary(t *testing.T) {
	// Arrange
	workspaces, summaries := fixture()

	// Act
	got, err := Find(workspaces, []wsm.Repository{doom, other}, summaries, Query{})

	// Assert
	if err != nil {
		t.Fatalf("Find: %v", err)
	}
	byID := map[string]Found{}
	for _, f := range got.Found {
		byID[string(f.Workspace.ID)] = f
	}
	if f := byID["landed"]; f.Repository != other || f.Summary == nil || *f.Summary != "Prune the store's old rows" || !f.LastActiveAt.Equal(at(6)) {
		t.Fatalf("landed = %+v, want repository %v, its summary and last active on day 6", f, other)
	}
	if f := byID["closed"]; f.Summary != nil || !f.LastActiveAt.Equal(at(4)) {
		t.Fatalf("closed = %+v, want no summary and last active at its close", f)
	}
}

func TestFindLeavesOutThreadsAndMergeWorkspaces(t *testing.T) {
	// Arrange
	thread := record("thread", "chat", doom, 1)
	thread.Kind = wsm.WorkspaceKindThread
	merge := record("merge", "merge-footer", doom, 1)
	merge.MergeOf = &wsm.MergeOrigin{}
	agent := record("agent", "footer", doom, 1)

	// Act
	got, err := Find([]wsm.Workspace{thread, merge, agent}, []wsm.Repository{doom}, nil, Query{})

	// Assert
	if err != nil {
		t.Fatalf("Find: %v", err)
	}
	if !reflect.DeepEqual(ids(got), []string{"agent"}) {
		t.Fatalf("Find = %v, want only the agent workspace", ids(got))
	}
}

func TestFindFailsOnAWorkspaceOfAnUnregisteredRepository(t *testing.T) {
	// Arrange
	orphan := record("orphan", "lost", other, 1)

	// Act
	got, err := Find([]wsm.Workspace{orphan}, []wsm.Repository{doom}, nil, Query{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "repo-other") {
		t.Fatalf("Find = %v, want an error naming the unregistered repository", err)
	}
	if got.Found != nil {
		t.Fatalf("a failed find answered %v", ids(got))
	}
}

func TestLastActiveAtIsTheLatestEdge(t *testing.T) {
	tests := []struct {
		name string
		edit func(*wsm.Workspace)
		want time.Time
	}{
		{name: "never active is its creation", edit: func(*wsm.Workspace) {}, want: at(1)},
		{name: "a turn start", edit: func(w *wsm.Workspace) { w.LastActivityAt = ptr(at(3)) }, want: at(3)},
		{name: "a turn end after the start", edit: func(w *wsm.Workspace) {
			w.LastActivityAt = ptr(at(3))
			w.LastTurnEndedAt = ptr(at(4))
		}, want: at(4)},
		{name: "a close", edit: func(w *wsm.Workspace) { w.Closure = &wsm.Closure{At: at(5)} }, want: at(5)},
		{name: "a landing", edit: func(w *wsm.Workspace) { w.MergedAt = ptr(at(6)) }, want: at(6)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ws := record("ws", "ws", doom, 1)
			tt.edit(&ws)

			// Act
			got := LastActiveAt(ws)

			// Assert
			if !got.Equal(tt.want) {
				t.Fatalf("LastActiveAt = %v, want %v", got, tt.want)
			}
		})
	}
}

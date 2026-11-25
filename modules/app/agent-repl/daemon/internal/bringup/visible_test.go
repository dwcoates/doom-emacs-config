package bringup

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// fakeRegistry answers the arrangement, merge queues and workspace records.
// Its arrangement holds arranged and every record in workspaces.
type fakeRegistry struct {
	repos       []wsm.Repository
	subsections []wsm.Subsection
	arranged    []wsm.Workspace
	reposErr    error
	queues      map[wsm.RepoKey][]wsm.MergeQueueEntry
	queuesErr   error
	workspaces  map[ids.WorkspaceID]wsm.Workspace
}

func (f *fakeRegistry) ReadArrangement(context.Context) (wsm.Arrangement, error) {
	all := slices.Clone(f.arranged)
	for _, ws := range f.workspaces {
		all = append(all, ws)
	}
	return wsm.Arrangement{Workspaces: all, Repositories: f.repos, Subsections: f.subsections}, f.reposErr
}

func (f *fakeRegistry) AllMergeQueues(context.Context) (map[wsm.RepoKey][]wsm.MergeQueueEntry, error) {
	return f.queues, f.queuesErr
}

func (f *fakeRegistry) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	ws, ok := f.workspaces[id]
	if !ok {
		return wsm.Workspace{}, wsm.ErrNotFound
	}
	return ws, nil
}

func TestOnlyVisibleSplitsPending(t *testing.T) {
	cases := []struct {
		name         string
		ws           wsm.Workspace
		wantRun      bool
		wantWithheld bool
	}{
		{name: "a visible workspace runs", ws: wsm.Workspace{ID: "a", Repo: "open", Visibility: wsm.UnselectedVisible}, wantRun: true},
		{name: "the selection runs", ws: wsm.Workspace{ID: "a", Repo: "open", Backlogged: true, Visibility: wsm.Selected}, wantRun: true},
		{name: "a workspace persisted not visible is withheld", ws: wsm.Workspace{ID: "a", Repo: "open", Visibility: wsm.NotVisible}, wantWithheld: true},
		{name: "a backlogged workspace is withheld", ws: wsm.Workspace{ID: "a", Repo: "open", Backlogged: true}, wantWithheld: true},
		{name: "a workspace in a collapsed section is withheld", ws: wsm.Workspace{ID: "a", Repo: "folded"}, wantWithheld: true},
		{name: "a hidden merge workspace whose merge is queued runs", ws: wsm.Workspace{ID: "queued", Repo: "open", MergeOf: &wsm.MergeOrigin{}}, wantRun: true},
		{name: "a hidden merge workspace whose merge left every queue is withheld", ws: wsm.Workspace{ID: "ended", Repo: "open", MergeOf: &wsm.MergeOrigin{}}, wantWithheld: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			reg := &fakeRegistry{repos: []wsm.Repository{{ID: "open", Place: 1}, {ID: "folded", Folded: true, Place: 2}},
				arranged: []wsm.Workspace{tc.ws},
				queues:   map[wsm.RepoKey][]wsm.MergeQueueEntry{"/repo/.git": {{Workspace: "queued"}}}}

			// Act
			run, withheld, err := OnlyVisible(context.Background(), reg, dlog.NewTestLogger(), []wsm.Workspace{tc.ws})

			// Assert
			if err != nil {
				t.Fatalf("OnlyVisible: %v", err)
			}
			if (len(run) == 1) != tc.wantRun || (len(withheld) == 1) != tc.wantWithheld {
				t.Fatalf("run = %v, withheld = %v", run, withheld)
			}
		})
	}
}

func TestOnlyVisibleReadsTheSubsectionFolds(t *testing.T) {
	folded, open := ids.SubsectionID("folded-sub"), ids.SubsectionID("open-sub")
	// Every workspace is a worktree apart from the repository's main one: a
	// one-shot mark on the main worktree's family files nothing.
	cases := []struct {
		name    string
		ws      wsm.Workspace
		wantRun bool
	}{
		{name: "a workspace in a collapsed subsection is withheld", ws: wsm.Workspace{ID: "a", Repo: "r", Place: 1, Dir: "/worktrees/a", Subsection: &folded}},
		{name: "a visible workspace in an expanded subsection runs", ws: wsm.Workspace{ID: "a", Repo: "r", Place: 1, Dir: "/worktrees/a", Subsection: &open, Visibility: wsm.UnselectedVisible}, wantRun: true},
		{name: "a one-shot filed in a collapsed subsection runs", ws: wsm.Workspace{ID: "a", Repo: "r", Place: 1, Dir: "/worktrees/a", Subsection: &folded, OneShot: true}, wantRun: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			reg := &fakeRegistry{
				repos:       []wsm.Repository{{ID: "r", Place: 1, Dir: "/repo"}},
				subsections: []wsm.Subsection{{ID: folded, Repo: "r", Name: "folded", Place: 1, Folded: true}, {ID: open, Repo: "r", Name: "open", Place: 2}},
				arranged:    []wsm.Workspace{tc.ws},
			}

			// Act
			run, withheld, err := OnlyVisible(context.Background(), reg, dlog.NewTestLogger(), []wsm.Workspace{tc.ws})

			// Assert
			if err != nil {
				t.Fatalf("OnlyVisible: %v", err)
			}
			if (len(run) == 1) != tc.wantRun || (len(withheld) == 1) == tc.wantRun {
				t.Fatalf("run = %v, withheld = %v, want run %v", run, withheld, tc.wantRun)
			}
		})
	}
}

func TestOnlyVisibleRefusesWhenTheFoldsCannotBeRead(t *testing.T) {
	// Arrange
	reg := &fakeRegistry{reposErr: errors.New("arranged: the store is gone")}

	// Act
	run, withheld, err := OnlyVisible(context.Background(), reg, dlog.NewTestLogger(), []wsm.Workspace{{ID: "a"}})

	// Assert
	if err == nil || run != nil || withheld != nil {
		t.Fatalf("run = %v, withheld = %v, err = %v; want an error and no split", run, withheld, err)
	}
}

func TestOnlyVisibleRefusesWhenTheQueuesCannotBeRead(t *testing.T) {
	// Arrange
	reg := &fakeRegistry{queuesErr: errors.New("arranged: the store is gone")}

	// Act
	run, withheld, err := OnlyVisible(context.Background(), reg, dlog.NewTestLogger(), []wsm.Workspace{{ID: "a"}})

	// Assert: every workspace's verdict reads the queues, merge workspace or not.
	if err == nil || run != nil || withheld != nil {
		t.Fatalf("run = %v, withheld = %v, err = %v; want an error and no split", run, withheld, err)
	}
}

func TestOnlyVisibleRunsABackloggedWorkspaceWithAMergeQueued(t *testing.T) {
	// Arrange: a workspace the user hid has a one-shot merge waiting.
	reg := &fakeRegistry{queues: map[wsm.RepoKey][]wsm.MergeQueueEntry{"/repo/.git": {{Workspace: "a"}}}}

	// Act
	run, withheld, err := OnlyVisible(context.Background(), reg, dlog.NewTestLogger(), []wsm.Workspace{{ID: "a", Backlogged: true}})

	// Assert
	if err != nil || len(run) != 1 || len(withheld) != 0 {
		t.Fatalf("run = %v, withheld = %v, err = %v; want the queued workspace run", run, withheld, err)
	}
}

func TestOnlyVisibleReadsNothingForNothingPending(t *testing.T) {
	// Arrange: a lister that would fail if read.
	reg := &fakeRegistry{reposErr: errors.New("arranged: must not be read")}

	// Act
	_, _, err := OnlyVisible(context.Background(), reg, dlog.NewTestLogger(), nil)

	// Assert
	if err != nil {
		t.Fatalf("OnlyVisible(nil) = %v, want nothing read", err)
	}
}

func TestWithLiveVisibilityStampsEachRecordAndLeavesTheRestAlone(t *testing.T) {
	// Arrange
	records := []wsm.Workspace{{ID: "a", Name: "one", Visibility: wsm.NotVisible}, {ID: "b", Name: "two", Visibility: wsm.Selected}}
	live := map[ids.WorkspaceID]wsm.Visibility{"a": wsm.UnselectedVisible, "b": wsm.NotVisible}

	// Act
	got := WithLiveVisibility(records, func(ws ids.WorkspaceID) wsm.Visibility { return live[ws] })

	// Assert
	want := []wsm.Workspace{{ID: "a", Name: "one", Visibility: wsm.UnselectedVisible}, {ID: "b", Name: "two", Visibility: wsm.NotVisible}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("WithLiveVisibility = %+v, want %+v", got, want)
	}
	if records[0].Visibility != wsm.NotVisible {
		t.Fatalf("the records handed in were changed: %+v", records)
	}
}

func TestIDsNamesEachWorkspaceInOrder(t *testing.T) {
	// Act
	got := IDs([]wsm.Workspace{{ID: "b"}, {ID: "a"}})

	// Assert
	if !slices.Equal(got, []string{"b", "a"}) {
		t.Fatalf("IDs = %v", got)
	}
}

func TestIdleHorizon(t *testing.T) {
	engaged := time.Date(2026, time.November, 11, 9, 0, 0, 0, time.UTC)
	tests := []struct {
		name     string
		queuedAt time.Time
		queued   bool
		want     time.Time
	}{
		{name: "a workspace with no merge queued is idle a cutoff after it engaged", queued: false, want: engaged.Add(24 * time.Hour)},
		{name: "a merge queued after the engagement is kept warm two cutoffs from joining", queuedAt: engaged.Add(time.Hour), queued: true, want: engaged.Add(49 * time.Hour)},
		{name: "a merge queued before the engagement is kept warm two cutoffs from the engagement", queuedAt: engaged.Add(-time.Hour), queued: true, want: engaged.Add(48 * time.Hour)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := IdleHorizon(24*time.Hour, engaged, tt.queuedAt, tt.queued)

			// Assert
			if !got.Equal(tt.want) {
				t.Fatalf("IdleHorizon = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestMergeQueuedAtAnswersWhenTheEntryJoined(t *testing.T) {
	// Arrange
	joined := time.Date(2026, time.November, 11, 9, 0, 0, 0, time.UTC)
	reg := &fakeRegistry{queues: map[wsm.RepoKey][]wsm.MergeQueueEntry{"/repo/.git": {{Workspace: "a", EnqueuedAt: joined}}}}

	// Act
	at, queued, err := MergeQueuedAt(context.Background(), reg, wsm.Workspace{ID: "a"})

	// Assert
	if err != nil || !queued || !at.Equal(joined) {
		t.Fatalf("MergeQueuedAt = %v, %v, %v; want %v, queued", at, queued, err, joined)
	}
}

func TestMergeQueuedAtAnswersAWorkspaceWithNoEntry(t *testing.T) {
	// Arrange
	reg := &fakeRegistry{queues: map[wsm.RepoKey][]wsm.MergeQueueEntry{"/repo/.git": {{Workspace: "a"}}}}

	// Act
	_, queued, err := MergeQueuedAt(context.Background(), reg, wsm.Workspace{ID: "b"})

	// Assert
	if err != nil || queued {
		t.Fatalf("MergeQueuedAt = %v, %v; want not queued", queued, err)
	}
}

func TestMergeQueuedAtSurfacesAQueueReadFailure(t *testing.T) {
	// Arrange
	reg := &fakeRegistry{queuesErr: errors.New("the store is gone")}

	// Act
	_, _, err := MergeQueuedAt(context.Background(), reg, wsm.Workspace{ID: "a"})

	// Assert
	if err == nil {
		t.Fatal("MergeQueuedAt over a failed read answered no error")
	}
}

func TestJudgeDecidesByTheRostersStanding(t *testing.T) {
	subsectionFold := &sidebar.Fold{By: sidebar.FoldedBySubsection, Repository: "alpha", Subsection: "folded"}
	cases := []struct {
		name     string
		ws       wsm.Workspace
		live     wsm.Visibility
		fold     *sidebar.Fold
		queued   bool
		wantRuns bool
	}{
		{name: "a workspace the roster shows runs under no fold", ws: wsm.Workspace{ID: "a"}, live: wsm.UnselectedVisible, wantRuns: true},
		{name: "the roster's live visibility beats a stale stored one", ws: wsm.Workspace{ID: "a", Visibility: wsm.UnselectedVisible}, live: wsm.NotVisible},
		{name: "a workspace shown before its visibility was stored runs", ws: wsm.Workspace{ID: "a", Visibility: wsm.NotVisible}, live: wsm.Selected, wantRuns: true},
		{name: "a workspace the roster folds names the fold and does not run", ws: wsm.Workspace{ID: "a"}, live: wsm.NotVisible, fold: subsectionFold},
		{name: "a one-shot the roster folds runs and still names the fold", ws: wsm.Workspace{ID: "a", OneShot: true}, live: wsm.NotVisible, fold: subsectionFold, wantRuns: true},
		{name: "a backlogged workspace with a merge queued runs", ws: wsm.Workspace{ID: "a", Backlogged: true}, live: wsm.NotVisible, queued: true, wantRuns: true},
		{name: "a backlogged workspace with no merge queued does not run", ws: wsm.Workspace{ID: "a", Backlogged: true}, live: wsm.NotVisible},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var asked []ids.WorkspaceID
			live := func(ws ids.WorkspaceID) (wsm.Visibility, *sidebar.Fold) {
				asked = append(asked, ws)
				return tc.live, tc.fold
			}

			// Act
			got := Judge(tc.ws, tc.queued, live)

			// Assert
			if got.Runs != tc.wantRuns || got.Fold != tc.fold {
				t.Fatalf("Judge = runs %v under %+v; want runs %v under %+v", got.Runs, got.Fold, tc.wantRuns, tc.fold)
			}
			if !slices.Equal(asked, []ids.WorkspaceID{"a"}) {
				t.Fatalf("the roster was asked for %v, want the judged workspace once", asked)
			}
		})
	}
}

// errFolds is the failure the fake registry answers a read with.
var errFolds = errors.New("the fake registry refused")

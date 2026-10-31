package bringup

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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
		{name: "a drawn workspace runs", ws: wsm.Workspace{ID: "a", Repo: "open"}, wantRun: true},
		{name: "a hidden workspace is withheld", ws: wsm.Workspace{ID: "a", Repo: "open", Hidden: true}, wantWithheld: true},
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
		{name: "a workspace in an expanded subsection runs", ws: wsm.Workspace{ID: "a", Repo: "r", Place: 1, Dir: "/worktrees/a", Subsection: &open}, wantRun: true},
		{name: "a one-shot filed in a collapsed subsection runs, its tab drawn in the one-shot subsection", ws: wsm.Workspace{ID: "a", Repo: "r", Place: 1, Dir: "/worktrees/a", Subsection: &folded, OneShot: true}, wantRun: true},
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

func TestOnlyVisibleRunsAHiddenWorkspaceWithAMergeQueued(t *testing.T) {
	// Arrange: a workspace the user hid has a one-shot merge waiting.
	reg := &fakeRegistry{queues: map[wsm.RepoKey][]wsm.MergeQueueEntry{"/repo/.git": {{Workspace: "a"}}}}

	// Act
	run, withheld, err := OnlyVisible(context.Background(), reg, dlog.NewTestLogger(), []wsm.Workspace{{ID: "a", Hidden: true}})

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

func TestVisibleReadsTheRecordAfresh(t *testing.T) {
	cases := []struct {
		name string
		ws   wsm.Workspace
		want bool
	}{
		{name: "a drawn workspace is visible", ws: wsm.Workspace{ID: "a", Repo: "r"}, want: true},
		{name: "a hidden workspace is not visible", ws: wsm.Workspace{ID: "a", Repo: "r", Hidden: true}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			reg := &fakeRegistry{repos: []wsm.Repository{{ID: "r"}}, workspaces: map[ids.WorkspaceID]wsm.Workspace{"a": tc.ws}}

			// Act
			got, err := Visible(context.Background(), reg, dlog.NewTestLogger(), "a")

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("Visible = %v, %v; want %v", got, err, tc.want)
			}
		})
	}
}

func TestVisibleRefusesAnUnreadableRecord(t *testing.T) {
	// Arrange
	reg := &fakeRegistry{workspaces: map[ids.WorkspaceID]wsm.Workspace{}}

	// Act
	_, err := Visible(context.Background(), reg, dlog.NewTestLogger(), "missing")

	// Assert
	if !errors.Is(err, wsm.ErrNotFound) {
		t.Fatalf("Visible = %v, want the record's own error", err)
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

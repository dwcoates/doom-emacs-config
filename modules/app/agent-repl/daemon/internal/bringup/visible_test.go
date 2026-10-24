package bringup

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// fakeRegistry answers repositories, merge queues and workspace records.
type fakeRegistry struct {
	repos      []wsm.Repository
	reposErr   error
	queues     map[wsm.RepoKey][]wsm.MergeQueueEntry
	queuesErr  error
	workspaces map[ids.WorkspaceID]wsm.Workspace
}

func (f *fakeRegistry) ListRepositories(context.Context) ([]wsm.Repository, error) {
	return f.repos, f.reposErr
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
			reg := &fakeRegistry{repos: []wsm.Repository{{ID: "open"}, {ID: "folded", Folded: true}},
				queues: map[wsm.RepoKey][]wsm.MergeQueueEntry{"/repo/.git": {{Workspace: "queued"}}}}

			// Act
			run, withheld, err := OnlyVisible(context.Background(), reg, []wsm.Workspace{tc.ws})

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

func TestOnlyVisibleRefusesWhenTheFoldsCannotBeRead(t *testing.T) {
	// Arrange
	reg := &fakeRegistry{reposErr: errors.New("arranged: the store is gone")}

	// Act
	run, withheld, err := OnlyVisible(context.Background(), reg, []wsm.Workspace{{ID: "a"}})

	// Assert
	if err == nil || run != nil || withheld != nil {
		t.Fatalf("run = %v, withheld = %v, err = %v; want an error and no split", run, withheld, err)
	}
}

func TestOnlyVisibleRefusesWhenTheQueuesCannotBeRead(t *testing.T) {
	// Arrange
	reg := &fakeRegistry{queuesErr: errors.New("arranged: the store is gone")}

	// Act
	run, withheld, err := OnlyVisible(context.Background(), reg, []wsm.Workspace{{ID: "a", MergeOf: &wsm.MergeOrigin{}}})

	// Assert
	if err == nil || run != nil || withheld != nil {
		t.Fatalf("run = %v, withheld = %v, err = %v; want an error and no split", run, withheld, err)
	}
}

func TestOnlyVisibleReadsNoQueueWhenNoMergeWorkspaceIsPending(t *testing.T) {
	// Arrange: queues that would fail if read.
	reg := &fakeRegistry{queuesErr: errors.New("arranged: must not be read")}

	// Act
	run, _, err := OnlyVisible(context.Background(), reg, []wsm.Workspace{{ID: "a"}})

	// Assert
	if err != nil || len(run) != 1 {
		t.Fatalf("run = %v, err = %v; want the workspace run and no queue read", run, err)
	}
}

func TestOnlyVisibleReadsNothingForNothingPending(t *testing.T) {
	// Arrange: a lister that would fail if read.
	reg := &fakeRegistry{reposErr: errors.New("arranged: must not be read")}

	// Act
	_, _, err := OnlyVisible(context.Background(), reg, nil)

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
			got, err := Visible(context.Background(), reg, "a")

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
	_, err := Visible(context.Background(), reg, "missing")

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

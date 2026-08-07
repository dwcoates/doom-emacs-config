package geometry

import (
	"context"
	"path/filepath"
	"testing"

	"claude-repld/internal/statedb"
)

// listStore opens a geometry store on a throwaway state database.
func listStore(t *testing.T) *Store {
	t.Helper()
	db, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	store, err := Open(db, func(string, ...any) {})
	if err != nil {
		t.Fatalf("geometry.Open: %v", err)
	}
	return store
}

func TestListReturnsNothingForAnEmptyStore(t *testing.T) {
	// Arrange.
	store := listStore(t)

	// Act.
	got, err := store.List(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("List = %+v, want no records", got)
	}
}

func TestListReturnsEveryRecordedWorkspaceSortedByKey(t *testing.T) {
	// Arrange.
	store := listStore(t)
	ctx := context.Background()
	for _, rec := range []Record{
		{Workspace: "/ws/zebra", SourceBranch: "zebra", SourceDir: "/ws/zebra", TargetDir: "/repo", Origin: OriginCreated},
		{Workspace: "/ws/alpha", SourceBranch: "alpha", SourceDir: "/ws/alpha", TargetDir: "/repo", Origin: OriginBackfilled},
	} {
		if err := store.Record(ctx, rec); err != nil {
			t.Fatalf("Record %s: %v", rec.Workspace, err)
		}
	}

	// Act.
	got, err := store.List(ctx)

	// Assert.
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	want := []Record{
		{Workspace: "/ws/alpha", SourceBranch: "alpha", SourceDir: "/ws/alpha", TargetDir: "/repo", Origin: OriginBackfilled},
		{Workspace: "/ws/zebra", SourceBranch: "zebra", SourceDir: "/ws/zebra", TargetDir: "/repo", Origin: OriginCreated},
	}
	if len(got) != len(want) {
		t.Fatalf("List = %+v, want %+v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("List[%d] = %+v, want %+v", i, got[i], want[i])
		}
	}
}

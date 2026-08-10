package main

import (
	"context"
	"testing"

	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/geometry"
)

// openTestGeometry opens a geometry store over a throwaway state database, so
// the gate is exercised against the real store it wraps in production.
func openTestGeometry(t *testing.T) *geometry.Store {
	t.Helper()
	db, err := statedb.Open(t.TempDir() + "/state.db")
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	t.Cleanup(func() {
		if err := db.Close(); err != nil {
			t.Fatalf("close state store: %v", err)
		}
	})
	store, err := geometry.Open(db, func(string, ...any) {})
	if err != nil {
		t.Fatalf("open geometry: %v", err)
	}
	return store
}

func TestDeferredGeometryBackfillLookupWaitsForTheBackfill(t *testing.T) {
	// Arrange — a workspace whose record is written by the still-running
	// backfill, exactly as the boot repair writes it.
	store := openTestGeometry(t)
	gate := newDeferredGeometryBackfill(store)
	want := geometry.Record{Workspace: "/ws", SourceBranch: "feature", SourceDir: "/ws", TargetDir: "/main", Origin: geometry.OriginBackfilled}

	// Act — the lookup is issued while the gate is shut; it may only observe
	// the record the backfill writes before opening it.
	answered := make(chan geometry.Record, 1)
	failed := make(chan error, 1)
	go func() {
		rec, found, err := gate.Lookup(context.Background(), "/ws")
		if err != nil || !found {
			failed <- err
			return
		}
		answered <- rec
	}()
	if err := store.Record(context.Background(), want); err != nil {
		t.Fatalf("record geometry: %v", err)
	}
	gate.Finish()

	// Assert
	select {
	case err := <-failed:
		t.Fatalf("gated lookup did not find the backfilled record: %v", err)
	case got := <-answered:
		if got.SourceBranch != want.SourceBranch || got.TargetDir != want.TargetDir {
			t.Fatalf("record = %#v, want the backfilled %#v", got, want)
		}
	}
}

func TestDeferredGeometryBackfillLookupReportsCancellationAsAnError(t *testing.T) {
	// Arrange — the gate is never opened, so the wait is what the context ends.
	gate := newDeferredGeometryBackfill(openTestGeometry(t))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, found, err := gate.Lookup(ctx, "/ws")

	// Assert — a cancellation must never be reported as "no record", which is
	// what refuses a merge.
	if err == nil {
		t.Fatalf("cancelled lookup returned no error (found=%v)", found)
	}
	if found {
		t.Fatalf("cancelled lookup reported found=true")
	}
}

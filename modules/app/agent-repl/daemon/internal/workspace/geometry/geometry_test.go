package geometry

import (
	"context"
	"database/sql"
	"errors"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/statedb"
)

// openStore opens a real state database in a temp dir. The store must be
// reopen-durable, so statedb refuses ":memory:" and there is nothing to fake.
func openStore(t *testing.T) (*Store, *sql.DB, *[]string) {
	t.Helper()
	db, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	var logs []string
	store, err := Open(db, func(format string, args ...any) { logs = append(logs, format) })
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	return store, db, &logs
}

func sampleRecord() Record {
	return Record{
		Workspace:    "/worktrees/feature-one",
		SourceBranch: "DWC/feature-one",
		SourceDir:    "/worktrees/feature-one",
		TargetDir:    "/repo",
		Origin:       OriginCreated,
	}
}

func TestRecordedGeometryRoundTripsThroughLookup(t *testing.T) {
	// Arrange.
	store, _, _ := openStore(t)
	want := sampleRecord()

	// Act.
	if err := store.Record(context.Background(), want); err != nil {
		t.Fatalf("Record: %v", err)
	}
	got, found, err := store.Lookup(context.Background(), want.Workspace)

	// Assert.
	if err != nil || !found {
		t.Fatalf("Lookup found=%t err=%v", found, err)
	}
	if got != want {
		t.Fatalf("Lookup = %#v, want %#v", got, want)
	}
}

func TestLookupOfAnUnrecordedWorkspaceIsNotFound(t *testing.T) {
	// Arrange.
	store, _, _ := openStore(t)

	// Act.
	got, found, err := store.Lookup(context.Background(), "/worktrees/never-recorded")

	// Assert — a missing record is reported, NEVER synthesized.
	if err != nil {
		t.Fatalf("Lookup: %v", err)
	}
	if found || got != (Record{}) {
		t.Fatalf("Lookup found=%t record=%#v, want a clean miss", found, got)
	}
}

func TestLookupWithoutAWorkspaceKeyIsRefused(t *testing.T) {
	// Arrange.
	store, _, _ := openStore(t)

	// Act.
	_, _, err := store.Lookup(context.Background(), "")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "workspace key") {
		t.Fatalf("Lookup error = %v, want a missing-key refusal", err)
	}
}

func TestRecordRefusesIncompleteOrImpossibleGeometry(t *testing.T) {
	base := sampleRecord()
	tests := []struct {
		name   string
		mutate func(*Record)
		want   string
	}{
		{"no workspace", func(r *Record) { r.Workspace = "" }, "Workspace is required"},
		{"no source branch", func(r *Record) { r.SourceBranch = "" }, "SourceBranch is required"},
		{"no source dir", func(r *Record) { r.SourceDir = "" }, "SourceDir is required"},
		{"no target dir", func(r *Record) { r.TargetDir = "" }, "TargetDir is required"},
		{"unknown origin", func(r *Record) { r.Origin = "invented" }, "unknown origin"},
		{"source equals target", func(r *Record) { r.TargetDir = r.SourceDir }, "cannot merge into itself"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store, _, _ := openStore(t)
			rec := base
			tc.mutate(&rec)

			// Act.
			err := store.Record(context.Background(), rec)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Record error = %v, want one containing %q", err, tc.want)
			}
			if rec.Workspace != "" {
				if _, found, lookupErr := store.Lookup(context.Background(), rec.Workspace); lookupErr != nil || found {
					t.Fatalf("a refused record was persisted: found=%t err=%v", found, lookupErr)
				}
			}
		})
	}
}

func TestRecordingTheSameFactsTwiceIsIdempotent(t *testing.T) {
	// Arrange — the create manager re-records after a resumed worktree stage.
	store, db, _ := openStore(t)
	rec := sampleRecord()
	if err := store.Record(context.Background(), rec); err != nil {
		t.Fatal(err)
	}

	// Act.
	if err := store.Record(context.Background(), rec); err != nil {
		t.Fatalf("second Record: %v", err)
	}

	// Assert — one row, unchanged.
	var rows int
	if err := db.QueryRow(`SELECT COUNT(*) FROM workspace_merge_geometry`).Scan(&rows); err != nil {
		t.Fatal(err)
	}
	if rows != 1 {
		t.Fatalf("rows = %d, want 1", rows)
	}
}

func TestAnObservedRecordReplacesADisagreeingDerivedOne(t *testing.T) {
	// Arrange — a backfilled guess, then the worktree is created for real.
	store, _, logs := openStore(t)
	derived := sampleRecord()
	derived.Origin = OriginBackfilled
	derived.TargetDir = "/some/other/worktree"
	if err := store.Record(context.Background(), derived); err != nil {
		t.Fatal(err)
	}
	observed := sampleRecord()

	// Act.
	if err := store.Record(context.Background(), observed); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Assert.
	got, found, err := store.Lookup(context.Background(), observed.Workspace)
	if err != nil || !found {
		t.Fatalf("Lookup found=%t err=%v", found, err)
	}
	if got != observed {
		t.Fatalf("Lookup = %#v, want the observed record %#v", got, observed)
	}
	if !containsFormat(*logs, "geometry: record REPLACED") {
		t.Fatalf("the replacement was not logged: %#v", *logs)
	}
}

func TestADerivedRecordNeverOverwritesADisagreeingRecordedOne(t *testing.T) {
	// Arrange.
	store, _, logs := openStore(t)
	observed := sampleRecord()
	if err := store.Record(context.Background(), observed); err != nil {
		t.Fatal(err)
	}
	derived := sampleRecord()
	derived.Origin = OriginBackfilled
	derived.TargetDir = "/some/other/worktree"

	// Act.
	err := store.Record(context.Background(), derived)

	// Assert.
	if !errors.Is(err, ErrGeometryConflict) {
		t.Fatalf("Record error = %v, want ErrGeometryConflict", err)
	}
	got, _, lookupErr := store.Lookup(context.Background(), observed.Workspace)
	if lookupErr != nil || got != observed {
		t.Fatalf("stored record = %#v (err %v), want the untouched observed record", got, lookupErr)
	}
	if !containsFormat(*logs, "geometry: record CONFLICT") {
		t.Fatalf("the conflict was not logged: %#v", *logs)
	}
}

func TestOriginIsUpgradedWhenTheSameFactsAreObserved(t *testing.T) {
	// Arrange — the derived answer happened to be right.
	store, _, _ := openStore(t)
	derived := sampleRecord()
	derived.Origin = OriginBackfilled
	if err := store.Record(context.Background(), derived); err != nil {
		t.Fatal(err)
	}

	// Act.
	if err := store.Record(context.Background(), sampleRecord()); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Assert.
	got, _, err := store.Lookup(context.Background(), derived.Workspace)
	if err != nil {
		t.Fatal(err)
	}
	if got.Origin != OriginCreated {
		t.Fatalf("origin = %s, want %s", got.Origin, OriginCreated)
	}
}

func TestKeySpellingsCollapseToOneRecord(t *testing.T) {
	// Arrange — "/a/b/" and "/a//b" name one directory, not two workspaces.
	store, _, _ := openStore(t)
	rec := sampleRecord()
	rec.Workspace = "/worktrees/feature-one/"
	if err := store.Record(context.Background(), rec); err != nil {
		t.Fatal(err)
	}

	// Act.
	got, found, err := store.Lookup(context.Background(), "/worktrees//feature-one")

	// Assert.
	if err != nil || !found {
		t.Fatalf("Lookup found=%t err=%v", found, err)
	}
	if got.Workspace != "/worktrees/feature-one" {
		t.Fatalf("workspace key = %q, want the cleaned spelling", got.Workspace)
	}
}

func TestOpenRefusesMissingCollaborators(t *testing.T) {
	db, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { db.Close() })
	tests := []struct {
		name string
		db   *sql.DB
		logf func(string, ...any)
		want string
	}{
		{"no database", nil, func(string, ...any) {}, "state database"},
		{"no logger", db, nil, "Logf"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			_, err := Open(tc.db, tc.logf)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Open error = %v, want one containing %q", err, tc.want)
			}
		})
	}
}

func containsFormat(logs []string, want string) bool {
	for _, line := range logs {
		if strings.Contains(line, want) {
			return true
		}
	}
	return false
}

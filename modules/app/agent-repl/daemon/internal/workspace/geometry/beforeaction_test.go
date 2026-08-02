package geometry

import (
	"context"
	"testing"
)

// THE GUARANTEE: the before-merge action is a merge coordinate like the other
// three, so it survives the round-trip through the durable record the merge
// reads its geometry out of.
func TestARecordedBeforeActionSurvivesTheRoundTrip(t *testing.T) {
	// Arrange.
	store, _, _ := openStore(t)
	rec := sampleRecord()
	rec.BeforeAction = "bump the version"

	// Act.
	if err := store.Record(context.Background(), rec); err != nil {
		t.Fatalf("Record: %v", err)
	}
	got, found, err := store.Lookup(context.Background(), rec.Workspace)

	// Assert.
	if err != nil {
		t.Fatalf("Lookup: %v", err)
	}
	if !found {
		t.Fatal("Lookup found nothing for a workspace just recorded")
	}
	if got.BeforeAction != "bump the version" {
		t.Fatalf("BeforeAction = %q, want the recorded action", got.BeforeAction)
	}
}

// The common case: a workspace created without one reads back empty, never a
// synthesized action.
func TestAWorkspaceWithNoBeforeActionReadsBackEmpty(t *testing.T) {
	// Arrange.
	store, _, _ := openStore(t)

	// Act.
	if err := store.Record(context.Background(), sampleRecord()); err != nil {
		t.Fatalf("Record: %v", err)
	}
	got, _, err := store.Lookup(context.Background(), sampleRecord().Workspace)

	// Assert.
	if err != nil {
		t.Fatalf("Lookup: %v", err)
	}
	if got.BeforeAction != "" {
		t.Fatalf("BeforeAction = %q, want empty", got.BeforeAction)
	}
}

// THE VIOLATION EDGE: a DERIVED record must never erase a recorded action. Only
// the creation path knows one, so a backfill writing its empty string over the
// real value would silently drop an action the user asked for.
func TestABackfillDoesNotEraseARecordedBeforeAction(t *testing.T) {
	// Arrange — the creation record, action and all.
	store, _, _ := openStore(t)
	rec := sampleRecord()
	rec.BeforeAction = "bump the version"
	if err := store.Record(context.Background(), rec); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act — the boot backfill re-derives the same three coordinates from git,
	// which carries no such fact.
	derived := sampleRecord()
	derived.Origin = OriginBackfilled
	if err := store.Record(context.Background(), derived); err != nil {
		t.Fatalf("Record(backfilled): %v", err)
	}

	// Assert.
	got, _, err := store.Lookup(context.Background(), rec.Workspace)
	if err != nil {
		t.Fatalf("Lookup: %v", err)
	}
	if got.BeforeAction != "bump the version" {
		t.Fatalf("BeforeAction = %q after a backfill, want the recorded action kept", got.BeforeAction)
	}
}

// A creation record that DOES name an action replaces whatever was there: the
// daemon just built that worktree, so its answer is the authoritative one.
func TestACreationRecordReplacesTheBeforeAction(t *testing.T) {
	// Arrange.
	store, _, _ := openStore(t)
	first := sampleRecord()
	first.BeforeAction = "bump the version"
	if err := store.Record(context.Background(), first); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	second := sampleRecord()
	second.BeforeAction = "write the changelog"
	if err := store.Record(context.Background(), second); err != nil {
		t.Fatalf("Record(second): %v", err)
	}

	// Assert.
	got, _, err := store.Lookup(context.Background(), first.Workspace)
	if err != nil {
		t.Fatalf("Lookup: %v", err)
	}
	if got.BeforeAction != "write the changelog" {
		t.Fatalf("BeforeAction = %q, want the newest creation record's action", got.BeforeAction)
	}
}

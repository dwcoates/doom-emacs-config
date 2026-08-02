package geometry

import (
	"context"
	"testing"
)

// `before_action` is a RETIRED column: it once held a second copy of the
// workspace's `before_ws_merge` prompt, written here and read nowhere, while the
// merge read the real one out of the creation records. The tests below pin the
// retirement — nothing writes it, and nothing depends on what it holds.

// THE GUARANTEE: no geometry write populates the retired column, whatever the
// record's origin. A record that filled it in would be a second source of a fact
// the creation records already own.
func TestARecordedGeometryLeavesTheRetiredBeforeActionColumnEmpty(t *testing.T) {
	tests := []struct {
		name   string
		origin Origin
	}{
		{name: "created", origin: OriginCreated},
		{name: "backfilled", origin: OriginBackfilled},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			store, db, _ := openStore(t)
			rec := sampleRecord()
			rec.Origin = tt.origin

			// Act.
			if err := store.Record(context.Background(), rec); err != nil {
				t.Fatalf("Record: %v", err)
			}

			// Assert.
			var beforeAction string
			if err := db.QueryRow(`SELECT before_action FROM workspace_merge_geometry WHERE workspace = ?`, rec.Workspace).
				Scan(&beforeAction); err != nil {
				t.Fatalf("read before_action: %v", err)
			}
			if beforeAction != "" {
				t.Fatalf("before_action = %q, want empty: the column is retired and nothing may write it", beforeAction)
			}
		})
	}
}

// A row left over from the daemon that DID write the column still resolves. The
// retired value is dead weight, never an input to the record the merge reads.
func TestALegacyBeforeActionValueDoesNotAffectTheLookup(t *testing.T) {
	// Arrange — a recorded workspace whose retired column carries legacy text.
	store, db, _ := openStore(t)
	want := sampleRecord()
	if err := store.Record(context.Background(), want); err != nil {
		t.Fatalf("Record: %v", err)
	}
	if _, err := db.Exec(`UPDATE workspace_merge_geometry SET before_action = ? WHERE workspace = ?`,
		"bump the version", want.Workspace); err != nil {
		t.Fatalf("seed the legacy before_action: %v", err)
	}

	// Act.
	got, found, err := store.Lookup(context.Background(), want.Workspace)

	// Assert.
	if err != nil {
		t.Fatalf("Lookup: %v", err)
	}
	if !found {
		t.Fatal("Lookup found nothing for a workspace carrying a legacy before_action")
	}
	if got != want {
		t.Fatalf("Lookup = %+v, want %+v: the retired column must not reach the record", got, want)
	}
}

// The retired column must NOT block a re-record: it is NOT NULL, so a write that
// omits it depends on the empty-string default the migration gave it.
func TestARerecordSucceedsWithoutSupplyingTheRetiredColumn(t *testing.T) {
	// Arrange.
	store, _, _ := openStore(t)
	rec := sampleRecord()
	if err := store.Record(context.Background(), rec); err != nil {
		t.Fatalf("first Record: %v", err)
	}
	rec.SourceBranch = "DWC/feature-one-renamed"

	// Act.
	err := store.Record(context.Background(), rec)

	// Assert.
	if err != nil {
		t.Fatalf("Record after a change: %v", err)
	}
	got, found, lookupErr := store.Lookup(context.Background(), rec.Workspace)
	if lookupErr != nil || !found {
		t.Fatalf("Lookup found=%t err=%v", found, lookupErr)
	}
	if got.SourceBranch != "DWC/feature-one-renamed" {
		t.Fatalf("SourceBranch = %q, want the re-recorded branch", got.SourceBranch)
	}
}

package registry

import (
	"testing"

	"claude-repld/internal/statedb"
)

// A PARTIAL LINEAGE IS NOT SERVABLE. The three fields are written by one Update
// and the shim rejects an empty dropped-turn list, so a row carrying some of
// them is corruption — and serving it would spawn a session that fails at
// startup and comes back with no shim at all.
func TestLoadStateRefusesAPartialRewindLineage(t *testing.T) {
	tests := []struct {
		name                       string
		previous, leaf, droppedIDs string
	}{
		{name: "no dropped turn ids", previous: "old-uuid", leaf: "leaf-uuid"},
		{name: "no retained leaf", previous: "old-uuid", droppedIDs: "ka_1"},
		{name: "no predecessor", leaf: "leaf-uuid", droppedIDs: "ka_1"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			path := testPath(t)
			db, err := statedb.Open(path)
			if err != nil {
				t.Fatalf("statedb.Open: %v", err)
			}
			defer db.Close()
			if err := migrate(db); err != nil {
				t.Fatalf("migrate: %v", err)
			}
			if _, err := db.Exec(`INSERT INTO session_record(session_id, cwd,
				rewind_previous_vendor_session_id, rewind_retained_leaf_uuid, rewind_dropped_turn_ids)
				VALUES (?,?,?,?,?)`, "s1", "/ws", tc.previous, tc.leaf, tc.droppedIDs); err != nil {
				t.Fatalf("insert partial row: %v", err)
			}

			// Act.
			_, _, err = loadState(db, func(string, ...any) {})

			// Assert.
			if err == nil {
				t.Fatal("loadState served a partial rewind lineage instead of refusing")
			}
		})
	}
}

// A COMPLETE LINEAGE LOADS. The refusal must be about the partial shape and not
// about lineages in general — a record carrying one is exactly the crash-after-
// flip state the next bring-up recovers from.
func TestLoadStateServesACompleteRewindLineage(t *testing.T) {
	// Arrange.
	path := testPath(t)
	db, err := statedb.Open(path)
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	defer db.Close()
	if err := migrate(db); err != nil {
		t.Fatalf("migrate: %v", err)
	}
	if _, err := db.Exec(`INSERT INTO session_record(session_id, cwd,
		rewind_previous_vendor_session_id, rewind_retained_leaf_uuid, rewind_dropped_turn_ids)
		VALUES (?,?,?,?,?)`, "s1", "/ws", "old-uuid", "leaf-uuid", "ka_1"); err != nil {
		t.Fatalf("insert row: %v", err)
	}

	// Act.
	records, _, err := loadState(db, func(string, ...any) {})

	// Assert.
	if err != nil {
		t.Fatalf("loadState: %v", err)
	}
	if !records["s1"].Rewind.Armed() {
		t.Fatalf("record lineage = %+v, want the complete one it was stored with", records["s1"].Rewind)
	}
}

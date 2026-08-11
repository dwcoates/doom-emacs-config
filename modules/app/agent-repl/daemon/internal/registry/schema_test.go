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

// A RECORD WRITTEN BEFORE THE ENGAGEMENT CLOCK IS SEEDED FROM ITS TURN END.
// Without the seed the idle cutoff — which declines every unknown — would never
// reap a single pre-existing session, relocating the never-sleeps failure the
// second clock exists to fix into the upgrade itself.
func TestMigrateSeedsLastEngagementFromLastTurnEnd(t *testing.T) {
	// Arrange — a row with a turn end and no engagement instant, as every
	// record written by an earlier daemon has.
	path := testPath(t)
	db, err := statedb.Open(path)
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	defer db.Close()
	if err := migrate(db); err != nil {
		t.Fatalf("migrate: %v", err)
	}
	if _, err := db.Exec(
		`INSERT INTO session_record(session_id, cwd, last_turn_end_ms, last_engagement_ms) VALUES (?,?,?,?)`,
		"s1", "/ws", 1_700_000_000_000, 0,
	); err != nil {
		t.Fatalf("insert legacy row: %v", err)
	}

	// Act — the next daemon opens the same store.
	if err := migrate(db); err != nil {
		t.Fatalf("re-migrate: %v", err)
	}

	// Assert.
	var got int64
	if err := db.QueryRow(`SELECT last_engagement_ms FROM session_record WHERE session_id = ?`, "s1").Scan(&got); err != nil {
		t.Fatalf("read last_engagement_ms: %v", err)
	}
	if got != 1_700_000_000_000 {
		t.Fatalf("last_engagement_ms = %d, want the record's own last turn end 1700000000000", got)
	}
}

// A RECORD WITH NO TURN END IS LEFT AT ZERO. There is nothing to seed it from,
// and inventing an instant is the one thing this whole policy refuses to do.
func TestMigrateLeavesAnUndatedRecordsEngagementAtZero(t *testing.T) {
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
	if _, err := db.Exec(`INSERT INTO session_record(session_id, cwd) VALUES (?,?)`, "s1", "/ws"); err != nil {
		t.Fatalf("insert undated row: %v", err)
	}

	// Act.
	if err := migrate(db); err != nil {
		t.Fatalf("re-migrate: %v", err)
	}

	// Assert.
	var got int64
	if err := db.QueryRow(`SELECT last_engagement_ms FROM session_record WHERE session_id = ?`, "s1").Scan(&got); err != nil {
		t.Fatalf("read last_engagement_ms: %v", err)
	}
	if got != 0 {
		t.Fatalf("last_engagement_ms = %d, want 0 for a record with nothing to date it by", got)
	}
}

// THE SEED DOES NOT OVERWRITE A REAL ENGAGEMENT INSTANT. It runs on every open,
// so a record that has since been engaged must keep the instant it earned rather
// than being reset to whatever a keep-alive ping last left on the cache clock.
func TestMigrateDoesNotOverwriteAnObservedEngagement(t *testing.T) {
	// Arrange — a ping moved the cache clock past the engagement instant.
	path := testPath(t)
	db, err := statedb.Open(path)
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	defer db.Close()
	if err := migrate(db); err != nil {
		t.Fatalf("migrate: %v", err)
	}
	if _, err := db.Exec(
		`INSERT INTO session_record(session_id, cwd, last_turn_end_ms, last_engagement_ms) VALUES (?,?,?,?)`,
		"s1", "/ws", 1_700_000_600_000, 1_700_000_000_000,
	); err != nil {
		t.Fatalf("insert engaged row: %v", err)
	}

	// Act.
	if err := migrate(db); err != nil {
		t.Fatalf("re-migrate: %v", err)
	}

	// Assert.
	var got int64
	if err := db.QueryRow(`SELECT last_engagement_ms FROM session_record WHERE session_id = ?`, "s1").Scan(&got); err != nil {
		t.Fatalf("read last_engagement_ms: %v", err)
	}
	if got != 1_700_000_000_000 {
		t.Fatalf("last_engagement_ms = %d, want the observed 1700000000000 left untouched", got)
	}
}

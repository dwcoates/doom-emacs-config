package registry

import (
	"testing"

	"claude-repld/internal/statedb"
)

// The death card is DERIVED from death_reason on every SessionView push, so
// the instant its window closed has to be durable too. Held anywhere less, the
// next push — never mind the next boot — reopens a supersede completed days
// ago, which is exactly what a restore full of blue cards was.

func TestDeathResolutionSurvivesAReopen(t *testing.T) {
	// Arrange.
	path := testPath(t)
	reg := Open(path, discardLogf)
	if err := reg.Put(Record{SessionID: "s1", CWD: "/ws", Terminal: true,
		DeathReason: "superseded", DeathResolvedAtMs: 4242}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act — a fresh daemon reads the same store.
	reopened := Open(path, discardLogf)
	rec, ok := reopened.Get("s1")

	// Assert.
	if !ok {
		t.Fatal("the record did not survive the reopen")
	}
	if rec.DeathResolvedAtMs != 4242 {
		t.Fatalf("death_resolved_at_ms = %d, want 4242", rec.DeathResolvedAtMs)
	}
}

func TestUnresolvedDeathReadsAsZero(t *testing.T) {
	// Arrange — zero is the value that MEANS "still true", so an unstamped
	// record must never come back as anything else.
	path := testPath(t)
	reg := Open(path, discardLogf)
	if err := reg.Put(Record{SessionID: "s1", CWD: "/ws", Terminal: true,
		DeathReason: "superseded"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act.
	reopened := Open(path, discardLogf)
	rec, _ := reopened.Get("s1")

	// Assert.
	if rec.DeathResolvedAtMs != 0 {
		t.Fatalf("death_resolved_at_ms = %d, want 0", rec.DeathResolvedAtMs)
	}
}

func TestLegacyRowWithoutTheColumnLoadsAsUnresolved(t *testing.T) {
	// Arrange — a store written before the column existed. The migration adds
	// it with a zero default, so pre-existing history reads as OPEN and the
	// boot reconciliation is what closes it, rather than the load failing.
	path := testPath(t)
	db, err := statedb.Open(path)
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	defer db.Close()
	if err := migrate(db); err != nil {
		t.Fatalf("migrate: %v", err)
	}
	if _, err := db.Exec(`INSERT INTO session_record(session_id, cwd, terminal, death_reason)
		VALUES (?,?,?,?)`, "s1", "/ws", 1, "superseded"); err != nil {
		t.Fatalf("insert legacy row: %v", err)
	}

	// Act.
	records, _, err := loadState(db, func(string, ...any) {})

	// Assert.
	if err != nil {
		t.Fatalf("loadState: %v", err)
	}
	if got := records["s1"].DeathResolvedAtMs; got != 0 {
		t.Fatalf("death_resolved_at_ms = %d, want 0", got)
	}
}

// nuke_test.go — SUBJECT: the store is NUKED, never migrated — proved on a real
// process, against a real file.
//
// The rule was only ever pinned in-process. Black-box, the two situations that
// matter are a database stamped by another binary and a --db path that is not a
// database at all; both are a file IN THE WAY of a service whose whole content
// is a cache of what the vendor and the shim can produce again. Refusing to boot
// on either would wedge the store permanently in exchange for preserving bytes
// nobody can read.
package integration

import (
	"database/sql"
	"os"
	"path/filepath"
	"testing"

	_ "modernc.org/sqlite"
)

// stampedForeignSchema writes a database this binary did not create: the right
// file format, the wrong shape.
func stampedForeignSchema(t *testing.T, path string) {
	t.Helper()
	handle, err := sql.Open("sqlite", "file:"+path)
	if err != nil {
		t.Fatalf("staging a foreign database: %v", err)
	}
	defer handle.Close()
	if _, err := handle.Exec(`CREATE TABLE schema_meta (version INTEGER NOT NULL);
	  INSERT INTO schema_meta(version) VALUES (99);
	  CREATE TABLE ancient_messages (session_id TEXT, seq INTEGER);
	  INSERT INTO ancient_messages VALUES ('s1', 1);`); err != nil {
		t.Fatalf("staging a foreign schema: %v", err)
	}
}

func TestAStoreBootsOverADatabaseStampedByAnotherBinary(t *testing.T) {
	// Arrange
	dbPath := filepath.Join(t.TempDir(), "events.db")
	stampedForeignSchema(t, dbPath)

	// Act
	store := startStore(t, storeOptions{dbPath: dbPath})

	// Assert: it serves, and it serves an EMPTY store — the foreign rows are
	// gone rather than half-readable.
	ctx, cancel := callContext(t)
	defer cancel()
	page := openSession(ctx, t, store.client(), "main", 10, nil)
	assertTexts(t, "the book after a nuke", pageTexts(page.GetPage()), nil)
}

func TestNukingAForeignSchemaIsAnnouncedAsAWarning(t *testing.T) {
	// Arrange: dropping a shape with somebody's data in it is the one schema
	// event that deserves the weight.
	dbPath := filepath.Join(t.TempDir(), "events.db")
	stampedForeignSchema(t, dbPath)

	// Act
	store := startStore(t, storeOptions{dbPath: dbPath})

	// Assert
	warned := false
	for _, rec := range store.logRecords() {
		if rec.Operation == "store.db.schema" && rec.Level == "warn" {
			warned = true
		}
	}
	if !warned {
		t.Fatalf("nuking a foreign schema logged no store.db.schema warning\nstderr:\n%s", store.stderrText())
	}
}

func TestAStoreBootsOverADbPathThatIsNotADatabase(t *testing.T) {
	// Arrange: a truncated copy, a half-written restore, somebody's notes.
	dbPath := filepath.Join(t.TempDir(), "events.db")
	if err := os.WriteFile(dbPath, []byte("this is not a SQLite database at all"), 0o600); err != nil {
		t.Fatalf("staging garbage: %v", err)
	}

	// Act
	store := startStore(t, storeOptions{dbPath: dbPath})

	// Assert
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-nuked", "u-nuked",
		frameLine(agentID("main"), responseFrame("main", "act-1", "after the nuke"))))
	page := openSession(ctx, t, store.client(), "main", 10, nil)
	assertTexts(t, "the recreated store", pageTexts(page.GetPage()), []string{"after the nuke"})
}

func TestNukingAnUnreadableDatabaseNamesTheCause(t *testing.T) {
	// Arrange
	dbPath := filepath.Join(t.TempDir(), "events.db")
	if err := os.WriteFile(dbPath, []byte("this is not a SQLite database at all"), 0o600); err != nil {
		t.Fatalf("staging garbage: %v", err)
	}

	// Act
	store := startStore(t, storeOptions{dbPath: dbPath})

	// Assert: the warning says WHY, so an operator is not left guessing whether
	// the store deleted their file for a good reason.
	named := false
	for _, rec := range store.logRecords() {
		if rec.Level == "warn" && rec.Context["error"] != nil {
			named = true
		}
	}
	if !named {
		t.Fatalf("the nuke warning named no cause\nstderr:\n%s", store.stderrText())
	}
}

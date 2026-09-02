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
	"strings"
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
	// NOT AN EMPTY BOOK — NO BOOK. The nuke dropped the agent register with
	// everything else, so the store has never heard of this agent and refuses
	// rather than serving a page that would look like a quiet live agent.
	openUnknownAgent(ctx, t, store.client(), "main")
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
	//
	// IT IS SCOPED TO THE OPERATION THAT OWNS IT. "Some warn carried an error
	// key" passed for a reclaimed socket or an enabled pprof surface as readily
	// as for the nuke, so the assertion held without the record ever existing.
	schema := recordsAtOperation(store.logRecords(), "store.db.schema")
	warned := recordsAtLevel(schema, "warn")
	if len(warned) != 1 {
		t.Fatalf("store.db.schema warn records = %d, want exactly 1: %v\nstderr:\n%s", len(warned), warned, store.stderrText())
	}
	cause, ok := warned[0].Context["error"].(string)
	if !ok || cause == "" {
		t.Fatalf("the nuke warning carries no error context: %v", warned[0].Context)
	}
	if warned[0].Context["db"] != dbPath {
		t.Errorf("the nuke warning names db %v, want %q", warned[0].Context["database_path"], dbPath)
	}
}

// TestADbPathThatIsADirectoryIsRefusedAndLeftIntact: the nuke rule is about a
// FILE this binary cannot read. A directory at --db is a configuration mistake,
// and removing it would be the store deleting a tree an operator pointed it at
// by accident — so the boot fails instead, and the directory is untouched.
func TestADbPathThatIsADirectoryIsRefusedAndLeftIntact(t *testing.T) {
	// Arrange.
	dbPath := filepath.Join(t.TempDir(), "events.db")
	if err := os.MkdirAll(dbPath, 0o755); err != nil {
		t.Fatalf("staging a directory at the database path: %v", err)
	}
	witness := filepath.Join(dbPath, "somebody-elses-file")
	if err := os.WriteFile(witness, []byte("do not delete me"), 0o600); err != nil {
		t.Fatalf("staging the directory's contents: %v", err)
	}

	// Act.
	store := startStore(t, storeOptions{dbPath: dbPath, noWait: true})

	// Assert: the boot failed loudly...
	if err := store.awaitExit(); err == nil {
		t.Fatalf("the store booted over a directory at --db\nstderr:\n%s", store.stderrText())
	}
	// ...and nothing was removed.
	info, err := os.Stat(dbPath)
	if err != nil {
		t.Fatalf("the refused boot removed the directory at %q: %v", dbPath, err)
	}
	if !info.IsDir() {
		t.Fatalf("the refused boot replaced the directory at %q with a %s", dbPath, info.Mode())
	}
	if _, err := os.Stat(witness); err != nil {
		t.Fatalf("the refused boot removed the directory's contents: %v", err)
	}
}

// TestNukingRemovesTheStaleWalSibling: a -wal or a -shm belongs to the file
// that was nuked, and a survivor is a fragment of a database this binary
// already decided it cannot read. Left behind, SQLite meets it on the next open
// and reports corruption for a file that was recreated cleanly.
func TestNukingRemovesTheStaleWalSibling(t *testing.T) {
	// Arrange: a garbage database with siblings beside it.
	dbPath := filepath.Join(t.TempDir(), "events.db")
	if err := os.WriteFile(dbPath, []byte("this is not a SQLite database at all"), 0o600); err != nil {
		t.Fatalf("staging garbage: %v", err)
	}
	for _, sibling := range []string{dbPath + "-wal", dbPath + "-shm"} {
		if err := os.WriteFile(sibling, []byte("stale sibling bytes"), 0o600); err != nil {
			t.Fatalf("staging %q: %v", sibling, err)
		}
	}

	// Act.
	store := startStore(t, storeOptions{dbPath: dbPath})

	// Assert: the store serves, and the STAGED siblings are gone. A live store
	// writes its own -wal, so the assertion is on the staged bytes rather than
	// on the paths existing at all.
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-wal", "u-wal",
		frameLine(agentID("main"), responseFrame("main", "act-1", "after the nuke"))))
	for _, sibling := range []string{dbPath + "-wal", dbPath + "-shm"} {
		data, err := os.ReadFile(sibling)
		if err != nil {
			continue
		}
		if strings.Contains(string(data), "stale sibling bytes") {
			t.Errorf("the nuke left the stale sibling %q behind", sibling)
		}
	}
}

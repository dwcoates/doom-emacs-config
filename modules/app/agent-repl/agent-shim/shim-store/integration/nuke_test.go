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
	"agentrepl/shim-store/internal/testclose"
	"database/sql"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-store/internal/db"

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
	defer testclose.OrFail(t, handle)
	if _, err := handle.Exec(`CREATE TABLE schema_meta (version INTEGER NOT NULL);
	  INSERT INTO schema_meta(version) VALUES (99);
	  CREATE TABLE ancient_messages (session_id TEXT, seq INTEGER);
	  INSERT INTO ancient_messages VALUES ('s1', 1);`); err != nil {
		t.Fatalf("staging a foreign schema: %v", err)
	}
}

// stampedSupersededSchema stages a database at the version THIS binary
// superseded — the ordinary state of an events.db that a schema bump has just
// left behind.
func stampedSupersededSchema(t *testing.T, path string) {
	t.Helper()
	handle, err := sql.Open("sqlite", "file:"+path)
	if err != nil {
		t.Fatalf("staging a superseded database: %v", err)
	}
	defer testclose.OrFail(t, handle)
	if _, err := handle.Exec(fmt.Sprintf(`CREATE TABLE schema_meta (version INTEGER NOT NULL);
	  INSERT INTO schema_meta(version) VALUES (%d);
	  CREATE TABLE entry (id TEXT);`, db.SchemaVersion-1)); err != nil {
		t.Fatalf("staging a superseded schema: %v", err)
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

func TestNukingAForeignSchemaIsAnnouncedAsAnError(t *testing.T) {
	// Arrange: the staged stamp is 99, ABOVE this binary's own — a database
	// only a newer store could have written. Discarding it is not a version
	// bump this binary superseded, and it takes somebody's data with it.
	dbPath := filepath.Join(t.TempDir(), "events.db")
	stampedForeignSchema(t, dbPath)

	// Act
	store := startStore(t, storeOptions{dbPath: dbPath})

	// Assert
	errored := false
	for _, rec := range store.logRecords() {
		if rec.Operation == "store.db.schema" && rec.Level == "error" {
			errored = true
		}
	}
	if !errored {
		t.Fatalf("nuking a foreign schema logged no store.db.schema error\nstderr:\n%s", store.stderrText())
	}
}

func TestASupersededSchemaVersionBootsWithoutAWarning(t *testing.T) {
	// Arrange: a database one version BELOW this binary's, which is what every
	// deploy that bumped the schema meets. The owner's log carried a WARNING
	// for it (found version=5, want version=6, 2026-09-13 16:03:25) though the
	// store was doing exactly what it documents: nuked, never migrated.
	dbPath := filepath.Join(t.TempDir(), "events.db")
	stampedSupersededSchema(t, dbPath)

	// Act
	store := startStore(t, storeOptions{dbPath: dbPath})

	// Assert
	schema := recordsAtOperation(store.logRecords(), "store.db.schema")
	if warned := recordsAtLevel(schema, "warn"); len(warned) != 0 {
		t.Fatalf("a superseded schema logged %d store.db.schema warning(s), want none: %v\nstderr:\n%s",
			len(warned), warned, store.stderrText())
	}
	if errored := recordsAtLevel(schema, "error"); len(errored) != 0 {
		t.Fatalf("a superseded schema logged %d store.db.schema error(s), want none: %v\nstderr:\n%s",
			len(errored), errored, store.stderrText())
	}
}

func TestASupersededSchemaVersionNamesBothVersions(t *testing.T) {
	// Arrange: "from what, to what" is the operator's whole question at a nuke.
	dbPath := filepath.Join(t.TempDir(), "events.db")
	stampedSupersededSchema(t, dbPath)

	// Act
	store := startStore(t, storeOptions{dbPath: dbPath})

	// Assert
	found := fmt.Sprintf("found version=%d", db.SchemaVersion-1)
	want := fmt.Sprintf("want version=%d", db.SchemaVersion)
	named := false
	for _, rec := range recordsAtOperation(store.logRecords(), "store.db.schema") {
		if strings.Contains(rec.Message, found) && strings.Contains(rec.Message, want) {
			named = true
		}
	}
	if !named {
		t.Fatalf("no store.db.schema record naming both %q and %q\nstderr:\n%s", found, want, store.stderrText())
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
	page := openSession(ctx, t, store.client(), "main", nil)
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

	// Assert: the record says WHY, so an operator is not left guessing whether
	// the store deleted their file for a good reason. A file this binary cannot
	// read at all is damaged, not a version bump it superseded, so it is an
	// ERROR — the level a superseded version no longer carries.
	//
	// IT IS SCOPED TO THE OPERATION THAT OWNS IT. "Some record carried an error
	// key" passed for a reclaimed socket or an enabled pprof surface as readily
	// as for the nuke, so the assertion held without the record ever existing.
	schema := recordsAtOperation(store.logRecords(), "store.db.schema")
	errored := recordsAtLevel(schema, "error")
	if len(errored) != 1 {
		t.Fatalf("store.db.schema error records = %d, want exactly 1: %v\nstderr:\n%s", len(errored), errored, store.stderrText())
	}
	cause, ok := errored[0].Context["error"].(string)
	if !ok || cause == "" {
		t.Fatalf("the nuke record carries no error context: %v", errored[0].Context)
	}
	if errored[0].Context["db"] != dbPath {
		t.Errorf("the nuke record names db %v, want %q", errored[0].Context["database_path"], dbPath)
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

// bulkyForeignSchema stages a database this binary did not create AND makes it
// big: one table of megabyte blobs, so the cost of discarding it is measurable
// rather than notional.
func bulkyForeignSchema(t *testing.T, path string, megabytes int) {
	t.Helper()
	handle, err := sql.Open("sqlite", "file:"+path)
	if err != nil {
		t.Fatalf("staging a bulky foreign database: %v", err)
	}
	defer handle.Close() //nolint:errcheck // test staging
	if _, err := handle.Exec(`CREATE TABLE schema_meta (version INTEGER NOT NULL);
	  INSERT INTO schema_meta(version) VALUES (99);
	  CREATE TABLE ancient_messages (session_id TEXT, seq INTEGER, payload BLOB);
	  CREATE INDEX ancient_by_session ON ancient_messages(session_id, seq);`); err != nil {
		t.Fatalf("staging a bulky foreign schema: %v", err)
	}
	payload := make([]byte, 1<<20)
	tx, err := handle.Begin()
	if err != nil {
		t.Fatalf("staging transaction: %v", err)
	}
	for i := 0; i < megabytes; i++ {
		if _, err := tx.Exec(`INSERT INTO ancient_messages VALUES (?, ?, ?)`,
			fmt.Sprintf("s%d", i), i, payload); err != nil {
			t.Fatalf("staging row %d: %v", i, err)
		}
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("staging commit: %v", err)
	}
}

// TestAStoreOverABulkyForeignSchemaIsServingPromptly: what a deploy waits on is
// the SOCKET, and a store handed a foreign database it must discard has to
// reach that socket anyway. On 2026-09-09 it did not: the store met an 11.5 GB
// events.db at a superseded version, spent minutes emptying it with DROP TABLE
// with no socket listening, and the deploy gave up and left the rest of the
// stack un-bounced.
//
// The bound is a small multiple of the observed healthy boot, which is about
// twelve milliseconds over the staged file below. It is a regression fence on
// the SERVING deadline, not the proof of the mechanism — that the file is
// unlinked rather than emptied in place is pinned on the file's own identity in
// internal/db. What this pins black-box is that a real store process over a
// real foreign database on disk is answering calls in milliseconds.
func TestAStoreOverABulkyForeignSchemaIsServingPromptly(t *testing.T) {
	// Arrange
	dbPath := filepath.Join(t.TempDir(), "events.db")
	bulkyForeignSchema(t, dbPath, 64)
	info, err := os.Stat(dbPath)
	if err != nil {
		t.Fatalf("stat the staged database: %v", err)
	}

	// Act: startStore returns only once the socket accepts connections.
	began := time.Now()
	store := startStore(t, storeOptions{dbPath: dbPath})
	elapsed := time.Since(began)

	// Assert: it came up promptly, and it came up EMPTY.
	const bound = 500 * time.Millisecond
	if elapsed > bound {
		t.Fatalf("a store over a %d MiB foreign schema took %v to serve, want under %v — a boot that scales with the discarded file is a DROP, not an unlink\nstderr:\n%s",
			info.Size()>>20, elapsed, bound, store.stderrText())
	}
	ctx, cancel := callContext(t)
	defer cancel()
	openUnknownAgent(ctx, t, store.client(), "main")
}

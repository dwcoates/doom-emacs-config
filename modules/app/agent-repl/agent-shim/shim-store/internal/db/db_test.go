package db

import (
	"bytes"
	"encoding/json"
	"io"
	"path/filepath"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// --- shared test helpers ---------------------------------------------------

// openTemp opens a fresh WAL database in a temp dir (exercises real WAL, unlike
// :memory:) and registers cleanup.
func openTemp(t *testing.T) *DB {
	t.Helper()
	path := filepath.Join(t.TempDir(), "entries.db")
	d, err := Open(path, logging.New(io.Discard, io.Discard, false))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { d.Close() })
	return d
}

// streamEntry is the minimum a stored record must carry: the plane that
// observed it. On store.v1 the plane sits on StoreEntry itself rather than on
// the retired record's internal half.
func streamEntry() *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane: &storev1.Plane{Plane: &storev1.Plane_Stream{Stream: &storev1.PlaneStream{}}},
	}
}

// batch wraps records in the frame a producer actually writes.
func batch(entries ...*storev1.StoreEntry) *storev1.EntryBatch {
	return &storev1.EntryBatch{Entries: entries}
}

// canonicalRecord is one decoded line of the store's JSON log.
type canonicalRecord struct {
	Operation string         `json:"operation"`
	Level     string         `json:"level"`
	Message   string         `json:"message"`
	Context   map[string]any `json:"context"`
}

// findRecord returns the last record matching operation and level.
func findRecord(t *testing.T, logs *bytes.Buffer, operation, level string) (canonicalRecord, bool) {
	t.Helper()
	var found canonicalRecord
	ok := false
	for _, line := range bytes.Split(bytes.TrimSpace(logs.Bytes()), []byte("\n")) {
		if len(line) == 0 {
			continue
		}
		var candidate canonicalRecord
		if err := json.Unmarshal(line, &candidate); err != nil {
			t.Fatalf("store log line is not JSON: %v (%s)", err, line)
		}
		if candidate.Operation == operation && candidate.Level == level {
			found = candidate
			ok = true
		}
	}
	return found, ok
}

// --- schema tests ----------------------------------------------------------

func TestOpenSeedsSchemaMeta(t *testing.T) {
	// Arrange / Act
	d := openTemp(t)
	// Assert
	var version int
	if err := d.sql.QueryRow(`SELECT version FROM schema_meta`).Scan(&version); err != nil {
		t.Fatalf("reading schema_meta: %v", err)
	}
	if version != SchemaVersion {
		t.Fatalf("schema_meta version = %d, want %d", version, SchemaVersion)
	}
}

func TestReopenIsIdempotent(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "entries.db")
	d1, err := Open(path, logging.New(io.Discard, io.Discard, false))
	if err != nil {
		t.Fatalf("first Open: %v", err)
	}
	d1.Close()
	// Act
	d2, err := Open(path, logging.New(io.Discard, io.Discard, false))
	// Assert
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer d2.Close()
	var version int
	if err := d2.sql.QueryRow(`SELECT version FROM schema_meta`).Scan(&version); err != nil {
		t.Fatalf("reading schema_meta: %v", err)
	}
	if version != SchemaVersion {
		t.Fatalf("version after reopen = %d, want %d", version, SchemaVersion)
	}
}

func TestOpenRejectsASchemaThisBinaryDidNotCreate(t *testing.T) {
	// Arrange: a database stamped above this binary's version — which is what a
	// leftover database from the retired `event` lineage looks like from here.
	path := filepath.Join(t.TempDir(), "entries.db")
	d, err := Open(path, logging.New(io.Discard, io.Discard, false))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if _, err := d.sql.Exec(`UPDATE schema_meta SET version = ?`, SchemaVersion+1); err != nil {
		t.Fatalf("bumping version: %v", err)
	}
	d.Close()

	// Act
	var logs bytes.Buffer
	_, err = Open(path, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "db", DatabasePath: path}))

	// Assert
	if err == nil {
		t.Fatal("expected Open to reject a schema this binary did not create, got nil")
	}
	record, found := findRecord(t, &logs, "migrate", "error")
	if !found {
		t.Fatalf("refusal record missing: %s", logs.String())
	}
	if !strings.Contains(record.Message, "schema migration failed") || record.Context["db"] != path || record.Context["table"] != "schema_meta" {
		t.Fatalf("refusal was not canonically logged with context: %#v", record)
	}
}

func TestApplyMigrationRollsBackAndSurfacesABadStep(t *testing.T) {
	// Arrange: a failing step must leave the recorded version untouched, or a
	// later open would claim a shape the database does not have. The step list
	// is empty today, so this exercises the mechanism the NEXT schema change
	// will use rather than a shipped migration.
	d := openTemp(t)
	bad := migrationStep{to: 99, name: "broken", ddl: `ALTER TABLE nonexistent ADD COLUMN x TEXT;`, reason: "test"}

	// Act
	err := d.applyMigration(SchemaVersion, bad)

	// Assert
	if err == nil {
		t.Fatal("applyMigration accepted a broken step")
	}
	if !strings.Contains(err.Error(), "applying migration") {
		t.Fatalf("error = %v, want it to name the failed migration", err)
	}
	var version int
	if scanErr := d.sql.QueryRow(`SELECT version FROM schema_meta`).Scan(&version); scanErr != nil {
		t.Fatalf("reading schema_meta: %v", scanErr)
	}
	if version != SchemaVersion {
		t.Fatalf("version after a failed migration = %d, want %d (rolled back)", version, SchemaVersion)
	}
}

func TestCursorsFailureUsesCanonicalQueryLogger(t *testing.T) {
	// Arrange: a closed database, so the query fails at the driver.
	path := filepath.Join(t.TempDir(), "entries.db")
	var logs bytes.Buffer
	log := logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "db", DatabasePath: path})
	d, err := Open(path, log)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := d.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	logs.Reset()

	// Act
	if _, err := d.Cursors(); err == nil {
		t.Fatal("Cursors on a closed database returned nil error")
	}

	// Assert
	record, found := findRecord(t, &logs, "list-cursors", "error")
	if !found {
		t.Fatalf("canonical error record missing: %s", logs.String())
	}
	if record.Context["component"] != "db" || record.Context["db"] != path || record.Context["table"] != "cursor" ||
		!strings.Contains(record.Message, "database query failed") {
		t.Fatalf("error lacks canonical query context: %#v", record)
	}
}

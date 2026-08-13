package db

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
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

// streamPlane is the internal half every helper below starts from: the minimum
// a stored record must carry, which is the producer that observed it.
func streamPlane() *agentshimv1.InternalEntry {
	return &agentshimv1.InternalEntry{
		Plane: &agentshimv1.Plane{Plane: &agentshimv1.Plane_Stream{Stream: &agentshimv1.PlaneStream{}}},
	}
}

// bookkeeping builds a record that composes no message: a turn boundary. Its
// ownership column is NULL by construction, because BookkeepingEntry has no
// field capable of naming a message.
func bookkeeping(session string) *agentshimv1.Entry {
	return &agentshimv1.Entry{
		Internal: streamPlane(),
		External: &protocolv1.ExternalEntry{
			SessionId: session,
			Entry: &protocolv1.ExternalEntry_Bookkeeping{Bookkeeping: &protocolv1.BookkeepingEntry{
				Kind: &protocolv1.BookkeepingEntry_TurnBegan{TurnBegan: &protocolv1.TurnBegan{TurnId: "t-1"}},
			}},
		},
	}
}

// message builds a record belonging to the message it names, owned by topID.
func message(session, messageID, topID string) *agentshimv1.Entry {
	return &agentshimv1.Entry{
		Internal: streamPlane(),
		External: &protocolv1.ExternalEntry{
			SessionId: session,
			Entry: &protocolv1.ExternalEntry_Message{Message: &conversationv1.MessageEntry{
				MessageId:         messageID,
				TopLevelMessageId: topID,
				Parent:            &conversationv1.MessageParent{Parent: &conversationv1.MessageParent_Root{Root: &conversationv1.MessageParentRoot{}}},
				Author:            &conversationv1.MessageAuthor{Author: &conversationv1.MessageAuthor_User{User: &conversationv1.AuthorUser{}}},
				Payload:           &conversationv1.MessageEntry_UserSaid{UserSaid: &conversationv1.UserSaid{}},
			}},
		},
	}
}

// withWriteID stamps the producer's stable write identity on a record.
func withWriteID(entry *agentshimv1.Entry, writeID string) *agentshimv1.Entry {
	entry.Internal.WriteId = writeID
	return entry
}

// unconverted builds a record the producer could not place: no external half at
// all, so nothing can ever read it back.
func unconverted(parseError string) *agentshimv1.Entry {
	internal := streamPlane()
	internal.Unconverted = &agentshimv1.InternalEntry_Unparsed{Unparsed: &agentshimv1.UnparsedEntry{ParseError: parseError}}
	return &agentshimv1.Entry{Internal: internal}
}

// batch wraps records in the frame a producer actually writes.
func batch(entries ...*agentshimv1.Entry) *agentshimv1.EntryBatch {
	return &agentshimv1.EntryBatch{Entries: entries}
}

// collectReplay materializes a streamed replay only inside tests that need to
// inspect the complete result. Production has no slice-returning replay API.
func collectReplay(t *testing.T, d *DB, session string, fromSeq uint64) []*protocolv1.EntryDelivery {
	t.Helper()
	var deliveries []*protocolv1.EntryDelivery
	if _, err := d.ReplayFrom(context.Background(), session, fromSeq, func(delivery *protocolv1.EntryDelivery) error {
		deliveries = append(deliveries, delivery)
		return nil
	}); err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	return deliveries
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

// --- write serialization ---------------------------------------------------

func TestConcurrentIngestsAssignEverySeqExactlyOnce(t *testing.T) {
	// Arrange: BEGIN IMMEDIATE (`_txlock=immediate`, see Open) is what makes
	// concurrent writers mutually exclusive. Ingest reads MAX(seq) and only then
	// inserts, so if that serialization did NOT hold, two transactions would read
	// the same high-water and derive the same candidate — and the loser's
	// `INSERT OR IGNORE` against PRIMARY KEY (session_id, seq) would be silently
	// ignored and then miscounted as a replay. Every failure mode is therefore
	// observable from outside: a duplicate seq, a gap, a lost record, or an
	// error.
	//
	// The records carry no write_id, so a genuine replay can never be confused
	// with a seq collision here.
	const writers = 12
	d := openTemp(t)

	// Act: release every writer at once from a channel barrier — no sleeps.
	var ready, done sync.WaitGroup
	ready.Add(writers)
	done.Add(writers)
	start := make(chan struct{})
	results := make([]Result, writers)
	errs := make([]error, writers)
	for i := range writers {
		go func() {
			defer done.Done()
			ready.Done()
			<-start
			results[i], errs[i] = d.Ingest("p", batch(bookkeeping("s1")))
		}()
	}
	ready.Wait()
	close(start)
	done.Wait()

	// Assert: every writer succeeded with its one record, and the assigned seqs
	// are exactly 1..writers with no duplicate and no gap.
	seen := make(map[uint64]int, writers)
	for i := range writers {
		if errs[i] != nil {
			t.Fatalf("writer %d: Ingest failed (write serialization did not hold): %v", i, errs[i])
		}
		if results[i].Accepted != 1 || results[i].Replayed != 0 {
			t.Fatalf("writer %d: accepted=%d replayed=%d, want accepted=1 replayed=0 — a seq collision was miscounted as a replay",
				i, results[i].Accepted, results[i].Replayed)
		}
		seen[results[i].LastSeq]++
	}
	for seq := uint64(1); seq <= writers; seq++ {
		switch n := seen[seq]; {
		case n == 0:
			t.Fatalf("seq %d was never assigned; assigned set = %v", seq, seen)
		case n > 1:
			t.Fatalf("seq %d was assigned to %d writers; assigned set = %v", seq, n, seen)
		}
	}

	// Assert: the durable rows agree with what ingest reported.
	replayed := collectReplay(t, d, "s1", 0)
	if len(replayed) != writers {
		t.Fatalf("persisted %d records, want %d", len(replayed), writers)
	}
	for i, delivery := range replayed {
		if want := uint64(i + 1); delivery.GetStored().GetSeq() != want {
			t.Fatalf("persisted record %d has seq %d, want %d", i, delivery.GetStored().GetSeq(), want)
		}
	}
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

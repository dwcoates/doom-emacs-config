package registry

import (
	"database/sql"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/statedb"
)

// testPath names a fresh state store for one test.
func testPath(t *testing.T) string {
	t.Helper()
	return filepath.Join(t.TempDir(), "state.db")
}

func discardLogf(string, ...any) {}

// collectLogf returns a logf that appends every formatted message.
func collectLogf(lines *[]string) func(string, ...any) {
	return func(format string, args ...any) {
		*lines = append(*lines, format)
	}
}

// rawStore opens the state store at path directly, with the registry's schema
// applied, so a test can arrange rows the exported API would never write (an
// externally edited store) or assert on what actually landed in the tables.
func rawStore(t *testing.T, path string) *sql.DB {
	t.Helper()
	db, err := statedb.Open(path)
	if err != nil {
		t.Fatalf("open raw store: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	if err := migrate(db); err != nil {
		t.Fatalf("migrate raw store: %v", err)
	}
	return db
}

func TestRoundTripThroughTheStore(t *testing.T) {
	// Arrange — a registry with a mix of live and terminal records.
	cases := []struct {
		name    string
		records []Record
	}{
		{
			name: "single live record",
			records: []Record{
				{SessionID: "s_1", CWD: "/w", Model: "haiku", PermissionMode: "auto",
					ClaudeSessionID: "uuid-1", CreatedAt: "2026-07-12T00:00:00Z"},
			},
		},
		{
			name: "terminal record keeps its death reason",
			records: []Record{
				{SessionID: "s_2", CWD: "/w", Terminal: true, DeathReason: "shim_died"},
			},
		},
		{
			name: "multiple records",
			records: []Record{
				{SessionID: "s_a", ClaudeSessionID: "uuid-a"},
				{SessionID: "s_b", ClaudeSessionID: "uuid-b"},
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			path := testPath(t)
			r := Open(path, discardLogf)
			for _, rec := range tc.records {
				if err := r.Put(rec); err != nil {
					t.Fatalf("Put(%v): %v", rec, err)
				}
			}
			// Act — reopen from the store, as a restarted daemon would.
			reopened := Open(path, discardLogf)
			// Assert
			if got := reopened.All(); !reflect.DeepEqual(got, r.All()) {
				t.Errorf("round trip: got %+v, want %+v", got, r.All())
			}
		})
	}
}

func TestQueuedPromptsSurviveAReopen(t *testing.T) {
	// Arrange — prompts the user typed that the agent has not seen yet.
	path := testPath(t)
	r := Open(path, discardLogf)
	want := []QueuedPrompt{
		{ID: "q1", Text: "first", PermissionMode: "plan", QueuedAtMs: 1700000000000},
		{ID: "q2", Text: "second"},
	}
	if err := r.Put(Record{SessionID: "s_1", CWD: "/w", QueuedPrompts: want}); err != nil {
		t.Fatal(err)
	}
	// Act.
	rec, ok := Open(path, discardLogf).Get("s_1")
	// Assert.
	if !ok {
		t.Fatal("record did not survive the reopen")
	}
	if !reflect.DeepEqual(rec.QueuedPrompts, want) {
		t.Fatalf("queued prompts = %+v, want %+v", rec.QueuedPrompts, want)
	}
}

func TestUpdateMutatesAndPersists(t *testing.T) {
	// Arrange
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{SessionID: "s_1"}); err != nil {
		t.Fatal(err)
	}
	// Act
	found, err := r.Update("s_1", func(rec *Record) { rec.ClaudeSessionID = "uuid-9" })
	// Assert
	if err != nil || !found {
		t.Fatalf("Update: found=%v err=%v", found, err)
	}
	rec, ok := Open(path, discardLogf).Get("s_1")
	if !ok || rec.ClaudeSessionID != "uuid-9" {
		t.Errorf("persisted record = %+v, ok=%v", rec, ok)
	}
}

func TestUpdateOnAbsentIDReportsNotFound(t *testing.T) {
	// Arrange
	r := Open(testPath(t), discardLogf)
	// Act
	found, err := r.Update("s_ghost", func(rec *Record) { rec.Terminal = true })
	// Assert
	if err != nil {
		t.Fatalf("Update: %v", err)
	}
	if found {
		t.Error("found = true for an absent id")
	}
}

func TestDeleteRemovesAndPersists(t *testing.T) {
	// Arrange
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{SessionID: "s_1"}); err != nil {
		t.Fatal(err)
	}
	// Act
	if err := r.Delete("s_1"); err != nil {
		t.Fatalf("Delete: %v", err)
	}
	// Assert
	if _, ok := Open(path, discardLogf).Get("s_1"); ok {
		t.Error("record survived delete across reopen")
	}
}

func TestFailedMutationLeavesPriorStateReadable(t *testing.T) {
	// Arrange — a valid registry, then a write that cannot complete: the
	// maintenance pass rejects the record, so nothing may land. This is the
	// crash-safety the JSON's temp-plus-rename used to provide, now the
	// transaction's own guarantee.
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{SessionID: "s_1", ClaudeSessionID: "uuid-1"}); err != nil {
		t.Fatal(err)
	}
	// Act
	err := r.Put(Record{SessionID: "s_2", BackfillState: "teleporting"})
	// Assert — the bad write is refused loudly and the prior state stands.
	if err == nil {
		t.Fatal("Put with an invalid backfill_state succeeded")
	}
	reopened := Open(path, discardLogf)
	if rec, ok := reopened.Get("s_1"); !ok || rec.ClaudeSessionID != "uuid-1" {
		t.Errorf("record after the failed write = %+v, ok=%v", rec, ok)
	}
	if _, ok := reopened.Get("s_2"); ok {
		t.Error("the refused record landed anyway; the transaction did not roll back")
	}
}

func TestUnopenableStoreStartsEmptyLoudlyAndIsPreserved(t *testing.T) {
	// Arrange — garbage where the state store should be.
	path := testPath(t)
	if err := os.WriteFile(path, []byte("{not a database"), 0o600); err != nil {
		t.Fatal(err)
	}
	var logged []string
	// Act
	r := Open(path, collectLogf(&logged))
	// Assert — loud error, and preparation refuses to serve fabricated empty
	// state; the file is left in place so every restart still fails loudly.
	if got := r.All(); len(got) != 0 {
		t.Errorf("records = %+v, want empty", got)
	}
	if err := r.Prepare(); err == nil {
		t.Fatal("Prepare succeeded after an unopenable store")
	}
	if !strings.Contains(strings.Join(logged, "\n"), "CORRUPT") {
		t.Errorf("no loud corruption log; got %q", logged)
	}
	if _, err := os.Stat(path); err != nil {
		t.Errorf("the unreadable store was removed, allowing a silent empty next boot: %v", err)
	}
}

func TestMissingStoreStartsEmptySilently(t *testing.T) {
	// Arrange
	var logged []string
	// Act — first boot: no store exists yet.
	r := Open(testPath(t), collectLogf(&logged))
	// Assert
	if got := r.All(); len(got) != 0 {
		t.Errorf("records = %+v, want empty", got)
	}
	if len(logged) != 0 {
		t.Errorf("first boot logged %q, want nothing", logged)
	}
}

func TestRecordWithEmptySessionIDFailsPreparationLoudly(t *testing.T) {
	// Arrange — an externally written store carrying a keyless record.
	path := testPath(t)
	db := rawStore(t, path)
	for _, id := range []string{"", "s_ok"} {
		if _, err := db.Exec(`INSERT INTO session_record(session_id) VALUES (?)`, id); err != nil {
			t.Fatalf("seed record %q: %v", id, err)
		}
	}
	var logged []string
	// Act
	r := Open(path, collectLogf(&logged))
	// Assert — the registry refuses to serve a partial roster.
	if err := r.Prepare(); err == nil {
		t.Fatal("Prepare succeeded with an invalid keyless record")
	}
	if !strings.Contains(strings.Join(logged, "\n"), "INVALID") {
		t.Errorf("invalid record was not logged; got %q", logged)
	}
}

func TestPutRejectsEmptySessionID(t *testing.T) {
	// Arrange
	r := Open(testPath(t), discardLogf)
	// Act / Assert
	if err := r.Put(Record{}); err == nil {
		t.Error("Put with empty session_id did not error")
	}
}

func TestFlushDoesNotResurrectARecordRemovedByAnotherWriter(t *testing.T) {
	// Arrange — a stale process cache still carries a record another daemon
	// removed from the store.
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{SessionID: "s_1"}); err != nil {
		t.Fatal(err)
	}
	if err := Open(path, discardLogf).Delete("s_1"); err != nil {
		t.Fatal(err)
	}
	// Act
	if err := r.Flush(); err != nil {
		t.Fatalf("Flush: %v", err)
	}
	// Assert: stale cache is not overlaid onto current store state.
	if _, ok := Open(path, discardLogf).Get("s_1"); ok {
		t.Error("Flush resurrected a record removed by another process")
	}
}

func TestTheTableIsTheAuthorityAfterAWrite(t *testing.T) {
	// Arrange
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{SessionID: "s_1", CWD: "/w", LastSeq: 42}); err != nil {
		t.Fatal(err)
	}
	// Act — read the row exactly as any other owner of the store would.
	db := rawStore(t, path)
	var (
		cwd  string
		seq  int64
		stmp int64
	)
	if err := db.QueryRow(
		`SELECT cwd, last_seq FROM session_record WHERE session_id = 's_1'`).Scan(&cwd, &seq); err != nil {
		t.Fatalf("read row: %v", err)
	}
	if err := db.QueryRow(
		`SELECT CAST(value AS INTEGER) FROM registry_meta WHERE key = ?`, metaSchemaVersion).Scan(&stmp); err != nil {
		t.Fatalf("read schema stamp: %v", err)
	}
	// Assert
	if cwd != "/w" || seq != 42 {
		t.Errorf("row = (cwd=%q, last_seq=%d), want (/w, 42)", cwd, seq)
	}
	if stmp != schemaVersion {
		t.Errorf("schema stamp = %d, want %d", stmp, schemaVersion)
	}
}

func TestSchemaVersionNewerThanTheBinaryRefusesToOpen(t *testing.T) {
	// Arrange — a store written by a future build.
	path := testPath(t)
	db := rawStore(t, path)
	if _, err := db.Exec(`UPDATE registry_meta SET value = ? WHERE key = ?`, schemaVersion+1, metaSchemaVersion); err != nil {
		t.Fatalf("stamp a future schema: %v", err)
	}
	// Act
	r := Open(path, discardLogf)
	// Assert — no silent downgrade.
	if err := r.Prepare(); err == nil {
		t.Fatal("Prepare succeeded against a newer schema")
	}
}

func TestTwoDaemonsRacingOnOneStoreLoseNoRecords(t *testing.T) {
	// Arrange — two Registry instances on ONE store, exactly as a daemon
	// that is still draining and its freshly-rebuilt replacement would
	// hold it. Each writes its own sessions concurrently.
	path := testPath(t)
	daemonA := Open(path, discardLogf)
	daemonB := Open(path, discardLogf)
	const perDaemon = 25
	var wg sync.WaitGroup
	for i := range perDaemon {
		wg.Add(2)
		go func() {
			defer wg.Done()
			if err := daemonA.Put(Record{SessionID: fmt.Sprintf("s_a%02d", i), ClaudeSessionID: "uuid-a"}); err != nil {
				t.Errorf("daemon A Put: %v", err)
			}
		}()
		go func() {
			defer wg.Done()
			if err := daemonB.Put(Record{SessionID: fmt.Sprintf("s_b%02d", i), ClaudeSessionID: "uuid-b"}); err != nil {
				t.Errorf("daemon B Put: %v", err)
			}
		}()
	}
	// Act
	wg.Wait()
	// Assert — every record from BOTH daemons survives. Without the
	// read-modify-write inside one immediate transaction, each process
	// rewrites the tables from its own map and silently drops the other's
	// sessions.
	reopened := Open(path, discardLogf)
	if got := len(reopened.All()); got != perDaemon*2 {
		t.Fatalf("records in the store = %d, want %d (lost update: one daemon clobbered the other)", got, perDaemon*2)
	}
}

func TestConcurrentWritersLeaveTheTableReadable(t *testing.T) {
	// Arrange — hammer one store from many goroutines across two
	// registries; a torn write would surface as a corrupt-read log.
	path := testPath(t)
	writers := []*Registry{Open(path, discardLogf), Open(path, discardLogf)}
	var wg sync.WaitGroup
	for i := range 40 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if err := writers[i%2].Put(Record{SessionID: fmt.Sprintf("s_%02d", i)}); err != nil {
				t.Errorf("Put: %v", err)
			}
		}()
	}
	wg.Wait()
	// Act — a fresh reader reads whatever the racers left behind.
	var logged []string
	reopened := Open(path, collectLogf(&logged))
	// Assert
	if strings.Contains(strings.Join(logged, "\n"), "CORRUPT") {
		t.Fatalf("concurrent writers tore the store: %q", logged)
	}
	if got := len(reopened.All()); got != 40 {
		t.Fatalf("records = %d, want 40", got)
	}
}

func TestPrepareRepairsConversationCheckpointAcrossRestart(t *testing.T) {
	// Arrange: an externally written store has the checkpoint facts spread
	// across predecessor records for one conversation, with no checkpoint row.
	path := testPath(t)
	db := rawStore(t, path)
	seed := `INSERT INTO session_record(session_id, config_dir, cwd, claude_session_id, terminal,
		last_seq, backfill_state, created_at) VALUES (?,?,?,?,?,?,?,?)`
	if _, err := db.Exec(seed, "s_old", "/cfg", "/w", "uuid-1", 1, 91, "done", "2026-07-01T00:00:00Z"); err != nil {
		t.Fatalf("seed s_old: %v", err)
	}
	if _, err := db.Exec(seed, "s_live", "/cfg", "/w", "uuid-1", 0, 7, "pending", "2026-07-02T00:00:00Z"); err != nil {
		t.Fatalf("seed s_live: %v", err)
	}
	r := Open(path, discardLogf)

	// Act.
	if err := r.Prepare(); err != nil {
		t.Fatalf("Prepare: %v", err)
	}
	reopened := Open(path, discardLogf)

	// Assert: both the compact checkpoint and retained live record carry the
	// repaired maximum/settled state after restart.
	id := ConversationIdentity{ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1"}
	cp, ok := reopened.Checkpoint(id)
	if !ok || cp.LastSeq != 91 || cp.BackfillState != "done" {
		t.Fatalf("checkpoint = %+v ok=%v, want seq=91 backfill=done", cp, ok)
	}
	live, ok := reopened.Get("s_live")
	if !ok || live.LastSeq != 91 || live.BackfillState != "done" {
		t.Fatalf("live record = %+v ok=%v, want hydrated checkpoint", live, ok)
	}
}

func TestConversationCheckpointIdentitySeparatesRootsAndWorkspaces(t *testing.T) {
	// Arrange: the same vendor uuid is legitimate in separate account roots
	// and workspaces; neither may borrow the other's replay watermark.
	r := Open(testPath(t), discardLogf)
	records := []Record{
		{SessionID: "s_a", ConfigDir: "/cfg-a", CWD: "/w", ClaudeSessionID: "shared", LastSeq: 11},
		{SessionID: "s_b", ConfigDir: "/cfg-b", CWD: "/w", ClaudeSessionID: "shared", LastSeq: 22},
		{SessionID: "s_c", ConfigDir: "/cfg-a", CWD: "/other", ClaudeSessionID: "shared", LastSeq: 33},
	}
	for _, rec := range records {
		if err := r.Put(rec); err != nil {
			t.Fatalf("Put(%s): %v", rec.SessionID, err)
		}
	}

	// Act / Assert.
	for _, tc := range []struct {
		id   ConversationIdentity
		want uint64
	}{
		{ConversationIdentity{ConfigDir: "/cfg-a", CWD: "/w", ClaudeSessionID: "shared"}, 11},
		{ConversationIdentity{ConfigDir: "/cfg-b", CWD: "/w", ClaudeSessionID: "shared"}, 22},
		{ConversationIdentity{ConfigDir: "/cfg-a", CWD: "/other", ClaudeSessionID: "shared"}, 33},
	} {
		cp, ok := r.Checkpoint(tc.id)
		if !ok || cp.LastSeq != tc.want {
			t.Fatalf("checkpoint(%+v) = %+v ok=%v, want seq=%d", tc.id, cp, ok, tc.want)
		}
	}
}

func TestRecordUpdatesWriteThroughToTheConversationCheckpoint(t *testing.T) {
	// Arrange.
	path := testPath(t)
	r := Open(path, discardLogf)
	rec := Record{SessionID: "s_1", ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1"}
	if err := r.Put(rec); err != nil {
		t.Fatal(err)
	}

	// Act: model the two hot-path writers (seq store and backfill registrar).
	found, err := r.Update(rec.SessionID, func(stored *Record) {
		stored.LastSeq = 77
		stored.BackfillState = "done"
	})
	if err != nil || !found {
		t.Fatalf("Update: found=%v err=%v", found, err)
	}
	reopened := Open(path, discardLogf)

	// Assert.
	cp, ok := reopened.Checkpoint(ConversationIdentity{
		ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1",
	})
	if !ok || cp.LastSeq != 77 || cp.BackfillState != "done" {
		t.Fatalf("checkpoint = %+v ok=%v, want seq=77 done", cp, ok)
	}
}

func TestTerminalCompactionIsBoundedAndPreservesCheckpointAndRecentViews(t *testing.T) {
	// Arrange: repeated daemon sessions for one durable conversation exceed the
	// terminal SessionView retention cap.
	path := testPath(t)
	r := Open(path, discardLogf)
	base := time.Date(2026, 7, 1, 0, 0, 0, 0, time.UTC)
	total := TerminalRetention + 17
	for i := range total {
		rec := Record{
			SessionID: fmt.Sprintf("s_%03d", i),
			ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1",
			Terminal: true, TerminalAt: base.Add(time.Duration(i) * time.Second).Format(time.RFC3339Nano),
			LastSeq: uint64(i + 1),
		}
		if i == total-2 {
			rec.BackfillState = "done"
		}
		if err := r.Put(rec); err != nil {
			t.Fatalf("Put(%s): %v", rec.SessionID, err)
		}
	}
	if err := r.Put(Record{
		SessionID: "s_live", ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1",
	}); err != nil {
		t.Fatalf("Put(live): %v", err)
	}

	// Act: reopen after all pruning writes.
	reopened := Open(path, discardLogf)

	// Assert: every live record and exactly the newest bounded terminal roster
	// remain, while the checkpoint outlives pruned predecessors.
	var terminal, live int
	for _, rec := range reopened.All() {
		if rec.Terminal {
			terminal++
		} else {
			live++
		}
	}
	if terminal != TerminalRetention || live != 1 {
		t.Fatalf("retained terminal/live = %d/%d, want %d/1", terminal, live, TerminalRetention)
	}
	if _, ok := reopened.Get("s_000"); ok {
		t.Fatal("oldest terminal SessionView survived bounded compaction")
	}
	if _, ok := reopened.Get(fmt.Sprintf("s_%03d", total-1)); !ok {
		t.Fatal("newest terminal SessionView was pruned")
	}
	cp, ok := reopened.Checkpoint(ConversationIdentity{
		ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1",
	})
	if !ok || cp.LastSeq != uint64(total) || cp.BackfillState != "done" {
		t.Fatalf("checkpoint after pruning = %+v ok=%v, want seq=%d done", cp, ok, total)
	}
	liveRec, ok := reopened.Get("s_live")
	if !ok || liveRec.LastSeq != uint64(total) || liveRec.BackfillState != "done" {
		t.Fatalf("live record after pruning = %+v ok=%v, want hydrated checkpoint", liveRec, ok)
	}
}

func TestTerminalCompactionRefusesToPruneUndeliveredQueuedPrompts(t *testing.T) {
	// Arrange: the oldest retained terminal record still carries user input
	// that no agent received.
	r := Open(testPath(t), discardLogf)
	base := time.Date(2026, 7, 1, 0, 0, 0, 0, time.UTC)
	for i := range TerminalRetention {
		rec := Record{
			SessionID: fmt.Sprintf("s_%03d", i), Terminal: true,
			TerminalAt: base.Add(time.Duration(i) * time.Second).Format(time.RFC3339),
		}
		if i == 0 {
			rec.QueuedPrompts = []QueuedPrompt{{ID: "q1", Text: "do not lose me"}}
		}
		if err := r.Put(rec); err != nil {
			t.Fatal(err)
		}
	}

	// Act: this would evict s_000.
	err := r.Put(Record{
		SessionID: "s_new", Terminal: true,
		TerminalAt: base.Add(time.Hour).Format(time.RFC3339),
	})

	// Assert: the whole mutation aborts loudly and the bounded prior state,
	// including the queued prompt, remains intact.
	if err == nil || !strings.Contains(err.Error(), "undelivered queued prompt") {
		t.Fatalf("Put error = %v, want loud queued-prompt compaction refusal", err)
	}
	if got := len(r.All()); got != TerminalRetention {
		t.Fatalf("records after refused compaction = %d, want %d", got, TerminalRetention)
	}
	if rec, ok := r.Get("s_000"); !ok || len(rec.QueuedPrompts) != 1 {
		t.Fatalf("queued-prompt record after refusal = %+v ok=%v", rec, ok)
	}
}

func TestPrepareFailsOnInvalidCheckpointState(t *testing.T) {
	// Arrange — an externally written checkpoint row with a backfill state the
	// vocabulary does not contain.
	path := testPath(t)
	db := rawStore(t, path)
	if _, err := db.Exec(`INSERT INTO conversation_checkpoint(config_dir, cwd, claude_session_id, backfill_state)
		VALUES ('', '/w', 'uuid-1', 'teleporting')`); err != nil {
		t.Fatalf("seed checkpoint: %v", err)
	}
	var logged []string
	// Act
	r := Open(path, collectLogf(&logged))
	// Assert
	if err := r.Prepare(); err == nil {
		t.Fatal("Prepare succeeded with invalid checkpoint state")
	}
	if !strings.Contains(strings.Join(logged, "\n"), "INVALID") {
		t.Fatalf("the invalid checkpoint was not loud: %q", logged)
	}
}

// --- the replay-floor mark ---------------------------------------------------
//
// NewestClearOrCompactSeq is the store seq of the newest clear or compaction on
// a conversation. It rides the same durable paths LastSeq does, because it is
// the same kind of fact and is lost for the same reason if it does not.

func TestReplayFloorWritesThroughToTheConversationCheckpoint(t *testing.T) {
	// Arrange.
	path := testPath(t)
	r := Open(path, discardLogf)
	rec := Record{SessionID: "s_1", ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1"}
	if err := r.Put(rec); err != nil {
		t.Fatal(err)
	}

	// Act: model the hot-path writer (the session controller observing a clear).
	found, err := r.Update(rec.SessionID, func(stored *Record) {
		stored.NewestClearOrCompactSeq = 512
	})
	if err != nil || !found {
		t.Fatalf("Update: found=%v err=%v", found, err)
	}
	reopened := Open(path, discardLogf)

	// Assert: the checkpoint carries it, so a later session id for the same
	// conversation still finds the floor.
	cp, ok := reopened.Checkpoint(ConversationIdentity{
		ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1",
	})
	if !ok || cp.NewestClearOrCompactSeq != 512 {
		t.Fatalf("checkpoint = %+v ok=%v, want newest_clear_or_compact_seq=512", cp, ok)
	}
}

func TestReplayFloorCheckpointKeepsTheHighestAcrossSessions(t *testing.T) {
	// Arrange: two daemon sessions for ONE conversation, the older one holding
	// the higher floor. Merging must not let the newer record's lower mark win,
	// or the resync would replay history a clear already discarded.
	path := testPath(t)
	r := Open(path, discardLogf)
	for _, rec := range []Record{
		{SessionID: "s_old", ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1", NewestClearOrCompactSeq: 900},
		{SessionID: "s_new", ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1", NewestClearOrCompactSeq: 10},
	} {
		if err := r.Put(rec); err != nil {
			t.Fatalf("Put(%s): %v", rec.SessionID, err)
		}
	}

	// Act.
	reopened := Open(path, discardLogf)

	// Assert.
	cp, ok := reopened.Checkpoint(ConversationIdentity{
		ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1",
	})
	if !ok || cp.NewestClearOrCompactSeq != 900 {
		t.Fatalf("checkpoint = %+v ok=%v, want newest_clear_or_compact_seq=900", cp, ok)
	}
}

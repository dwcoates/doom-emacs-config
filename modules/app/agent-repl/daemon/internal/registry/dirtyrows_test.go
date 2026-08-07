package registry

import (
	"database/sql"
	"reflect"
	"testing"
)

// THE DIRTY-ROW WRITE MUST BE INDISTINGUISHABLE FROM THE WHOLE REWRITE.
//
// saveState no longer clears both tables and reinserts every row on every
// mutation; it writes only the rows whose persisted values changed. What these
// tests pin is the ONE property that makes that substitution legitimate: the
// tables' contents afterwards are exactly what a whole rewrite would have left,
// for every shape of mutation — an edit, an insertion, a deletion, and an edit
// that reaches into a record's slice in place.
//
// The tables are read back through a SECOND registry opened over the same
// store, so what is asserted is what landed on disk rather than what this
// process happens to hold in memory.

// reopenRecords reads every persisted session record straight from the tables.
func reopenRecords(t *testing.T, path string) map[string]Record {
	t.Helper()
	reopened := Open(path, discardLogf)
	t.Cleanup(func() { _ = reopened.Close() })
	if err := reopened.Prepare(); err != nil {
		t.Fatalf("prepare reopened registry: %v", err)
	}
	out := map[string]Record{}
	for _, rec := range reopened.All() {
		out[rec.SessionID] = rec
	}
	return out
}

// seededRegistry returns a prepared registry holding three records.
func seededRegistry(t *testing.T) (*Registry, string) {
	t.Helper()
	path := testPath(t)
	r := Open(path, discardLogf)
	t.Cleanup(func() { _ = r.Close() })
	if err := r.Prepare(); err != nil {
		t.Fatalf("prepare: %v", err)
	}
	for _, rec := range []Record{
		{SessionID: "s_1", CWD: "/w1", Model: "opus", ClaudeSessionID: "uuid-1", CreatedAt: "2026-07-12T00:00:00Z"},
		{SessionID: "s_2", CWD: "/w2", Model: "sonnet", ClaudeSessionID: "uuid-2", CreatedAt: "2026-07-12T00:00:00Z"},
		{SessionID: "s_3", CWD: "/w3", Model: "haiku", ClaudeSessionID: "uuid-3", CreatedAt: "2026-07-12T00:00:00Z",
			QueuedPrompts: []QueuedPrompt{{ID: "q1", Text: "held"}}},
	} {
		if err := r.Put(rec); err != nil {
			t.Fatalf("put %s: %v", rec.SessionID, err)
		}
	}
	return r, path
}

func TestDirtyRowWriteLeavesEveryRecordAsAWholeRewriteWould(t *testing.T) {
	tests := []struct {
		name    string
		mutate  func(t *testing.T, r *Registry)
		want    map[string]uint64 // session id -> last_seq
		absent  string
		present string
	}{
		{
			name: "one record edited",
			mutate: func(t *testing.T, r *Registry) {
				if _, err := r.Update("s_2", func(rec *Record) { rec.LastSeq = 4242 }); err != nil {
					t.Fatalf("update: %v", err)
				}
			},
			want: map[string]uint64{"s_1": 0, "s_2": 4242, "s_3": 0},
		},
		{
			name: "a record inserted beside the untouched ones",
			mutate: func(t *testing.T, r *Registry) {
				if err := r.Put(Record{SessionID: "s_4", CWD: "/w4", Model: "opus",
					ClaudeSessionID: "uuid-4", CreatedAt: "2026-07-12T00:00:00Z", LastSeq: 7}); err != nil {
					t.Fatalf("put: %v", err)
				}
			},
			want: map[string]uint64{"s_1": 0, "s_2": 0, "s_3": 0, "s_4": 7},
		},
		{
			name: "a record deleted",
			mutate: func(t *testing.T, r *Registry) {
				if err := r.Delete("s_2"); err != nil {
					t.Fatalf("delete: %v", err)
				}
			},
			want:   map[string]uint64{"s_1": 0, "s_3": 0},
			absent: "s_2",
		},
		{
			name: "every record edited at once",
			mutate: func(t *testing.T, r *Registry) {
				for id, seq := range map[string]uint64{"s_1": 1, "s_2": 2, "s_3": 3} {
					if _, err := r.Update(id, func(rec *Record) { rec.LastSeq = seq }); err != nil {
						t.Fatalf("update %s: %v", id, err)
					}
				}
			},
			want: map[string]uint64{"s_1": 1, "s_2": 2, "s_3": 3},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, path := seededRegistry(t)

			// Act.
			tc.mutate(t, r)

			// Assert.
			got := reopenRecords(t, path)
			if len(got) != len(tc.want) {
				t.Fatalf("persisted records = %d, want %d", len(got), len(tc.want))
			}
			for id, seq := range tc.want {
				rec, ok := got[id]
				if !ok {
					t.Fatalf("record %s is missing from the store", id)
				}
				if rec.LastSeq != seq {
					t.Errorf("record %s last_seq = %d, want %d", id, rec.LastSeq, seq)
				}
			}
			if tc.absent != "" {
				if _, ok := got[tc.absent]; ok {
					t.Errorf("record %s survived its deletion", tc.absent)
				}
			}
		})
	}
}

// AN UNCHANGED RECORD'S OTHER FIELDS MUST SURVIVE A NEIGHBOUR'S EDIT. The
// whole rewrite reinserted every row, so nothing could be left stale; the
// dirty-row write skips them, and skipping the wrong row would silently freeze
// its contents at whatever the previous write left.
func TestDirtyRowWritePreservesAnUntouchedRecordInFull(t *testing.T) {
	// Arrange.
	r, path := seededRegistry(t)

	// Act.
	if _, err := r.Update("s_1", func(rec *Record) { rec.LastSeq = 9 }); err != nil {
		t.Fatalf("update: %v", err)
	}

	// Assert.
	got := reopenRecords(t, path)["s_3"]
	want := Record{SessionID: "s_3", CWD: "/w3", Model: "haiku", ClaudeSessionID: "uuid-3",
		CreatedAt: "2026-07-12T00:00:00Z", QueuedPrompts: []QueuedPrompt{{ID: "q1", Text: "held"}}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("untouched record = %+v, want %+v", got, want)
	}
}

// A SLICE EDITED IN PLACE IS STILL A CHANGE. Update hands the mutation a
// pointer to the record, so an edit through QueuedPrompts' backing array
// changes no field of the struct. The diff compares against a CLONE of the
// state the transaction loaded (cloneState), which is what keeps such an edit
// visible; comparing against the live value would alias it and write nothing.
func TestDirtyRowWriteSeesAQueuedPromptEditedInPlace(t *testing.T) {
	// Arrange.
	r, path := seededRegistry(t)

	// Act.
	if _, err := r.Update("s_3", func(rec *Record) { rec.QueuedPrompts[0].Text = "rewritten" }); err != nil {
		t.Fatalf("update: %v", err)
	}

	// Assert.
	got := reopenRecords(t, path)["s_3"].QueuedPrompts
	if len(got) != 1 || got[0].Text != "rewritten" {
		t.Fatalf("persisted queued prompts = %+v, want the in-place edit", got)
	}
}

// reopenCheckpoints reads every persisted conversation checkpoint from the
// tables, keyed by the conversation's vendor uuid.
func reopenCheckpoints(t *testing.T, path string) map[string]ConversationCheckpoint {
	t.Helper()
	reopened := Open(path, discardLogf)
	t.Cleanup(func() { _ = reopened.Close() })
	if err := reopened.Prepare(); err != nil {
		t.Fatalf("prepare reopened registry: %v", err)
	}
	out := map[string]ConversationCheckpoint{}
	for _, cp := range reopened.AllCheckpoints() {
		out[cp.ClaudeSessionID] = cp
	}
	return out
}

// A CHECKPOINT'S EDIT IS WRITTEN ON THE SAME TERMS AS A RECORD'S. Its diff is
// a plain struct comparison, so a checkpoint that moved must reach the table.
func TestDirtyRowWritePersistsAnAdvancedCheckpoint(t *testing.T) {
	// Arrange.
	r, path := seededRegistry(t)
	if len(r.AllCheckpoints()) == 0 {
		t.Fatal("seeded registry created no conversation checkpoints to advance")
	}

	// Act — the record's cursor advance carries into its conversation's
	// checkpoint through the maintenance pass.
	if _, err := r.Update("s_1", func(rec *Record) { rec.LastSeq = 55 }); err != nil {
		t.Fatalf("update: %v", err)
	}

	// Assert.
	got, ok := reopenCheckpoints(t, path)["uuid-1"]
	if !ok {
		t.Fatal("the advanced conversation has no persisted checkpoint")
	}
	if got.LastSeq != 55 {
		t.Fatalf("persisted checkpoint last_seq = %d, want 55", got.LastSeq)
	}
}

// A CHECKPOINT OUTLIVES ITS RECORD, and the dirty-row write must not start
// retiring one: a checkpoint exists precisely so a conversation's cursor
// survives the session ids that come and go over it.
func TestDirtyRowWriteKeepsACheckpointWhoseRecordWasDeleted(t *testing.T) {
	// Arrange.
	r, path := seededRegistry(t)

	// Act.
	if err := r.Delete("s_1"); err != nil {
		t.Fatalf("delete: %v", err)
	}

	// Assert.
	if _, ok := reopenCheckpoints(t, path)["uuid-1"]; !ok {
		t.Fatal("the conversation's checkpoint was retired with its record")
	}
}

// THE WHOLESALE PATH STILL CLEARS. A nil `before` means the prior contents are
// unknown, and the legacy import relies on both tables being emptied first.
func TestSaveStateWithNoPriorStateClearsWhatTheTablesHeld(t *testing.T) {
	// Arrange.
	path := testPath(t)
	db := rawStore(t, path)
	if _, err := db.Exec(`INSERT INTO session_record(session_id, cwd) VALUES ('s_stale', '/stale')`); err != nil {
		t.Fatalf("seed stale row: %v", err)
	}
	tx, err := db.Begin()
	if err != nil {
		t.Fatalf("begin: %v", err)
	}

	// Act.
	err = saveState(tx, nil, &registryState{
		records:     map[string]Record{"s_fresh": {SessionID: "s_fresh", CWD: "/fresh"}},
		checkpoints: map[ConversationIdentity]ConversationCheckpoint{},
	})
	if err != nil {
		_ = tx.Rollback()
		t.Fatalf("saveState: %v", err)
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("commit: %v", err)
	}

	// Assert.
	var stale int
	if err := db.QueryRow(`SELECT COUNT(*) FROM session_record WHERE session_id = 's_stale'`).Scan(&stale); err != nil && err != sql.ErrNoRows {
		t.Fatalf("count stale rows: %v", err)
	}
	if stale != 0 {
		t.Fatalf("stale rows = %d, want the wholesale write to have cleared them", stale)
	}
}

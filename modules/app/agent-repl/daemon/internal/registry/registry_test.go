package registry

import (
	"encoding/json"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func testPath(t *testing.T) string {
	t.Helper()
	return filepath.Join(t.TempDir(), "sessions.json")
}

func discardLogf(string, ...any) {}

// collectLogf returns a logf that appends every formatted message.
func collectLogf(lines *[]string) func(string, ...any) {
	return func(format string, args ...any) {
		*lines = append(*lines, format)
	}
}

func TestRoundTripThroughDisk(t *testing.T) {
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
			// Act — reopen from disk, as a restarted daemon would.
			reopened := Open(path, discardLogf)
			// Assert
			if got := reopened.All(); !reflect.DeepEqual(got, r.All()) {
				t.Errorf("round trip: got %+v, want %+v", got, r.All())
			}
		})
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

func TestInterruptedWriteLeavesPriorStateReadable(t *testing.T) {
	// Arrange — a valid registry on disk, then a write that "crashed"
	// mid-flight: the temp file exists but the rename never happened.
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{SessionID: "s_1", ClaudeSessionID: "uuid-1"}); err != nil {
		t.Fatal(err)
	}
	partial := filepath.Join(filepath.Dir(path), "sessions.json.tmp-crashed")
	if err := os.WriteFile(partial, []byte(`{"version":1,"sessions":[{"session`), 0o600); err != nil {
		t.Fatal(err)
	}
	// Act
	reopened := Open(path, discardLogf)
	// Assert — the torn temp file is inert; the prior complete state loads.
	if rec, ok := reopened.Get("s_1"); !ok || rec.ClaudeSessionID != "uuid-1" {
		t.Errorf("record after interrupted write = %+v, ok=%v", rec, ok)
	}
}

func TestCorruptFileStartsEmptyLoudlyAndIsPreserved(t *testing.T) {
	// Arrange
	path := testPath(t)
	if err := os.WriteFile(path, []byte("{not json"), 0o600); err != nil {
		t.Fatal(err)
	}
	var logged []string
	// Act
	r := Open(path, collectLogf(&logged))
	// Assert — empty start, loud log, corrupt bytes preserved.
	if got := r.All(); len(got) != 0 {
		t.Errorf("records = %+v, want empty", got)
	}
	joined := strings.Join(logged, "\n")
	if !strings.Contains(joined, "CORRUPT") {
		t.Errorf("no loud corruption log; got %q", joined)
	}
	if _, err := os.Stat(path + ".corrupt"); err != nil {
		t.Errorf("corrupt file not preserved: %v", err)
	}
}

func TestMissingFileStartsEmptySilently(t *testing.T) {
	// Arrange
	var logged []string
	// Act — first boot: no registry file exists yet.
	r := Open(testPath(t), collectLogf(&logged))
	// Assert
	if got := r.All(); len(got) != 0 {
		t.Errorf("records = %+v, want empty", got)
	}
	if len(logged) != 0 {
		t.Errorf("first boot logged %q, want nothing", logged)
	}
}

func TestRecordWithEmptySessionIDIsDroppedOnLoad(t *testing.T) {
	// Arrange — an externally edited file carrying a keyless record.
	path := testPath(t)
	doc := `{"version":1,"sessions":[{"session_id":""},{"session_id":"s_ok"}]}`
	if err := os.WriteFile(path, []byte(doc), 0o600); err != nil {
		t.Fatal(err)
	}
	var logged []string
	// Act
	r := Open(path, collectLogf(&logged))
	// Assert — the keyless record is dropped with a log; the rest load.
	if got := r.All(); len(got) != 1 || got[0].SessionID != "s_ok" {
		t.Errorf("records = %+v, want [s_ok]", got)
	}
	if !strings.Contains(strings.Join(logged, "\n"), "DROPPING") {
		t.Errorf("drop was not logged; got %q", logged)
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

func TestFlushRewritesTheFile(t *testing.T) {
	// Arrange — state in memory, file removed out from under it.
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{SessionID: "s_1"}); err != nil {
		t.Fatal(err)
	}
	if err := os.Remove(path); err != nil {
		t.Fatal(err)
	}
	// Act
	if err := r.Flush(); err != nil {
		t.Fatalf("Flush: %v", err)
	}
	// Assert
	if _, ok := Open(path, discardLogf).Get("s_1"); !ok {
		t.Error("flushed file does not carry the record")
	}
}

func TestFileIsValidVersionedJSON(t *testing.T) {
	// Arrange
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{SessionID: "s_1"}); err != nil {
		t.Fatal(err)
	}
	// Act
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	// Assert
	var doc struct {
		Version  int              `json:"version"`
		Sessions []map[string]any `json:"sessions"`
	}
	if err := json.Unmarshal(data, &doc); err != nil {
		t.Fatalf("on-disk shape unparseable: %v", err)
	}
	if doc.Version != 1 || len(doc.Sessions) != 1 {
		t.Errorf("doc = %+v", doc)
	}
}

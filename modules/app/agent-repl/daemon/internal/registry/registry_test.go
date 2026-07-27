package registry

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"
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
	// Assert — loud error, corrupt bytes preserved, and preparation refuses to
	// serve fabricated empty state.
	if got := r.All(); len(got) != 0 {
		t.Errorf("records = %+v, want empty", got)
	}
	if err := r.Prepare(); err == nil {
		t.Fatal("Prepare succeeded after a corrupt registry load")
	}
	joined := strings.Join(logged, "\n")
	if !strings.Contains(joined, "CORRUPT") {
		t.Errorf("no loud corruption log; got %q", joined)
	}
	if _, err := os.Stat(path + ".corrupt"); err != nil {
		t.Errorf("corrupt file not preserved: %v", err)
	}
	if _, err := os.Stat(path); err != nil {
		t.Errorf("original corrupt file was removed, allowing a silent empty next boot: %v", err)
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

func TestRecordWithEmptySessionIDFailsPreparationLoudly(t *testing.T) {
	// Arrange — an externally edited file carrying a keyless record.
	path := testPath(t)
	doc := `{"version":1,"sessions":[{"session_id":""},{"session_id":"s_ok"}]}`
	if err := os.WriteFile(path, []byte(doc), 0o600); err != nil {
		t.Fatal(err)
	}
	var logged []string
	// Act
	r := Open(path, collectLogf(&logged))
	// Assert — migration refuses to invent a partial registry.
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

func TestFlushDoesNotResurrectARecordRemovedOnDisk(t *testing.T) {
	// Arrange — a stale process cache still carries a record removed on disk.
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
	// Assert: stale cache is not overlaid onto current disk state.
	if _, ok := Open(path, discardLogf).Get("s_1"); ok {
		t.Error("Flush resurrected a record removed by another process")
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
	if doc.Version != fileVersionCheckpointIndex || len(doc.Sessions) != 1 {
		t.Errorf("doc = %+v", doc)
	}
}

func TestTwoDaemonsRacingOnOneRegistryLoseNoRecords(t *testing.T) {
	// Arrange — two Registry instances on ONE path, exactly as a daemon
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
	// Assert — every record from BOTH daemons survives on disk. Without
	// the locked read-modify-write, each process rewrites the file from
	// its own map and silently drops the other's sessions.
	reopened := Open(path, discardLogf)
	if got := len(reopened.All()); got != perDaemon*2 {
		t.Fatalf("records on disk = %d, want %d (lost update: one daemon clobbered the other)", got, perDaemon*2)
	}
}

func TestConcurrentWritersLeaveTheFileParseable(t *testing.T) {
	// Arrange — hammer one path from many goroutines across two
	// registries; a torn write would surface as a corrupt-file log.
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
	// Act — a fresh reader parses whatever the racers left behind.
	var logged []string
	reopened := Open(path, collectLogf(&logged))
	// Assert
	if strings.Contains(strings.Join(logged, "\n"), "CORRUPT") {
		t.Fatalf("concurrent writers tore the file: %q", logged)
	}
	if got := len(reopened.All()); got != 40 {
		t.Fatalf("records = %d, want 40", got)
	}
}

func TestPrepareMigratesAndRepairsConversationCheckpointAcrossRestart(t *testing.T) {
	// Arrange: a v1 registry has the checkpoint facts spread across predecessor
	// records for one conversation.
	path := testPath(t)
	doc := `{"version":1,"sessions":[
	  {"session_id":"s_old","config_dir":"/cfg","cwd":"/w","claude_session_id":"uuid-1","terminal":true,"last_seq":91,"backfill_state":"done","created_at":"2026-07-01T00:00:00Z"},
	  {"session_id":"s_live","config_dir":"/cfg","cwd":"/w","claude_session_id":"uuid-1","last_seq":7,"backfill_state":"pending","created_at":"2026-07-02T00:00:00Z"}
	]}`
	if err := os.WriteFile(path, []byte(doc), 0o600); err != nil {
		t.Fatal(err)
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

func TestCheckpointSidecarRepairsAnOldDaemonV1Overwrite(t *testing.T) {
	// Arrange: v2 has already compacted a predecessor into its checkpoint.
	path := testPath(t)
	r := Open(path, discardLogf)
	if err := r.Put(Record{
		SessionID: "s_old", ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1",
		Terminal: true, LastSeq: 88, BackfillState: "done",
	}); err != nil {
		t.Fatal(err)
	}
	// A draining old daemon knows only v1 and rewrites the main file, dropping
	// embedded checkpoints. It cannot know or touch the sidecar.
	v1 := `{"version":1,"sessions":[
	  {"session_id":"s_live","config_dir":"/cfg","cwd":"/w","claude_session_id":"uuid-1","last_seq":3}
	]}`
	if err := os.WriteFile(path, []byte(v1), 0o600); err != nil {
		t.Fatal(err)
	}

	// Act.
	replacement := Open(path, discardLogf)
	if err := replacement.Prepare(); err != nil {
		t.Fatalf("Prepare after v1 overwrite: %v", err)
	}

	// Assert: the sidecar restores the checkpoint and hydrates the live record.
	live, ok := replacement.Get("s_live")
	if !ok || live.LastSeq != 88 || live.BackfillState != "done" {
		t.Fatalf("live after sidecar repair = %+v ok=%v, want seq=88 done", live, ok)
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
	path := testPath(t)
	doc := `{"version":2,"sessions":[],"conversation_checkpoints":[
	  {"cwd":"/w","claude_session_id":"uuid-1","backfill_state":"teleporting"}
	]}`
	if err := os.WriteFile(path, []byte(doc), 0o600); err != nil {
		t.Fatal(err)
	}
	var logged []string
	r := Open(path, collectLogf(&logged))
	if err := r.Prepare(); err == nil {
		t.Fatal("Prepare succeeded with invalid checkpoint state")
	}
	if !strings.Contains(strings.Join(logged, "\n"), "prepare FAILED") {
		t.Fatalf("migration failure was not loud: %q", logged)
	}
}

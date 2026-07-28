package registry

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeLegacy writes a pre-SQLite registry document (and optionally its
// checkpoint sidecar) and returns its path.
func writeLegacy(t *testing.T, doc string, sidecar string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "claude-repld-sessions.json")
	if err := os.WriteFile(path, []byte(doc), 0o600); err != nil {
		t.Fatalf("write legacy registry: %v", err)
	}
	if sidecar != "" {
		if err := os.WriteFile(path+".checkpoints", []byte(sidecar), 0o600); err != nil {
			t.Fatalf("write legacy sidecar: %v", err)
		}
	}
	return path
}

func TestLegacyJSONIsImportedOnceOnFirstOpen(t *testing.T) {
	// Arrange — the JSON registry a running daemon left behind, with its
	// checkpoint sidecar holding the conversation state that outlived a pruned
	// predecessor.
	legacy := writeLegacy(t, `{"version":2,"sessions":[
	  {"session_id":"s_live","config_dir":"/cfg","cwd":"/w","claude_session_id":"uuid-1","last_seq":7,
	   "queued_prompts":[{"id":"q1","text":"held"}]},
	  {"session_id":"s_dead","cwd":"/w2","terminal":true,"death_reason":"shim_died","terminal_at":"2026-07-01T00:00:00Z"}
	]}`, `{"version":1,"conversation_checkpoints":[
	  {"config_dir":"/cfg","cwd":"/w","claude_session_id":"uuid-1","last_seq":91,"newest_clear_or_compact_seq":88,"backfill_state":"done"}
	]}`)
	var logged []string

	// Act.
	r := OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: legacy, Logf: collectLogf(&logged)})

	// Assert — records, the sidecar's checkpoint, and a loud report of it.
	live, ok := r.Get("s_live")
	if !ok || live.LastSeq != 7 || len(live.QueuedPrompts) != 1 {
		t.Fatalf("imported live record = %+v ok=%v", live, ok)
	}
	dead, ok := r.Get("s_dead")
	if !ok || !dead.Terminal || dead.DeathReason != "shim_died" {
		t.Fatalf("imported terminal record = %+v ok=%v", dead, ok)
	}
	cp, ok := r.Checkpoint(ConversationIdentity{ConfigDir: "/cfg", CWD: "/w", ClaudeSessionID: "uuid-1"})
	if !ok || cp.LastSeq != 91 || cp.NewestClearOrCompactSeq != 88 || cp.BackfillState != "done" {
		t.Fatalf("imported checkpoint = %+v ok=%v", cp, ok)
	}
	if !strings.Contains(strings.Join(logged, "\n"), "ONE-TIME IMPORT") {
		t.Fatalf("the import was not announced loudly: %q", logged)
	}
}

func TestASecondOpenDoesNotReImportTheLegacyJSON(t *testing.T) {
	// Arrange — the file is imported, then the record it carried is deleted
	// (a user deleting the session, say). The file still sits on disk.
	legacy := writeLegacy(t, `{"version":1,"sessions":[{"session_id":"s_1","cwd":"/w"}]}`, "")
	path := testPath(t)
	first := OpenWith(Options{DBPath: path, LegacyJSONPath: legacy, Logf: discardLogf})
	if _, ok := first.Get("s_1"); !ok {
		t.Fatal("the first open did not import the legacy registry")
	}
	if err := first.Delete("s_1"); err != nil {
		t.Fatal(err)
	}

	// Act — the next boot, with the same legacy path still configured.
	second := OpenWith(Options{DBPath: path, LegacyJSONPath: legacy, Logf: discardLogf})

	// Assert — the tables are the sole authority; the deleted session stays
	// deleted rather than being resurrected from the file.
	if _, ok := second.Get("s_1"); ok {
		t.Fatal("the legacy registry was imported a second time, resurrecting a deleted session")
	}
}

func TestNoLegacyJSONImportsNothingSilently(t *testing.T) {
	// Arrange — a state root that never held a JSON registry.
	missing := filepath.Join(t.TempDir(), "claude-repld-sessions.json")
	var logged []string

	// Act.
	r := OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: missing, Logf: collectLogf(&logged)})

	// Assert.
	if got := r.All(); len(got) != 0 {
		t.Fatalf("records = %+v, want empty", got)
	}
	if len(logged) != 0 {
		t.Fatalf("a first boot with no legacy file logged %q, want nothing", logged)
	}
}

func TestAnEmptyLegacyJSONImportsNothingAndIsStamped(t *testing.T) {
	// Arrange — a registry file that exists but holds no sessions.
	legacy := writeLegacy(t, `{"version":2,"sessions":[]}`, "")
	path := testPath(t)

	// Act.
	r := OpenWith(Options{DBPath: path, LegacyJSONPath: legacy, Logf: discardLogf})

	// Assert — nothing imported, and the import is stamped so the emptiness is
	// settled rather than re-examined on every boot.
	if got := r.All(); len(got) != 0 {
		t.Fatalf("records = %+v, want empty", got)
	}
	stamp, err := r.legacyImportStamp()
	if err != nil {
		t.Fatalf("read import stamp: %v", err)
	}
	if stamp == "" {
		t.Fatal("an empty legacy registry left no import stamp")
	}
}

func TestACorruptLegacyJSONFailsTheOpenLoudly(t *testing.T) {
	// Arrange — the daemon's entire session roster, unreadable.
	legacy := writeLegacy(t, `{"version":1,"sessions":[{"session`, "")
	var logged []string

	// Act.
	r := OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: legacy, Logf: collectLogf(&logged)})

	// Assert — no fabricated empty roster: the open is sticky-failed.
	if err := r.Prepare(); err == nil {
		t.Fatal("Prepare succeeded after an unparseable legacy registry")
	}
	if !strings.Contains(strings.Join(logged, "\n"), "CORRUPT") {
		t.Fatalf("the corrupt legacy registry was not loud: %q", logged)
	}
}

func TestALegacyRecordWithNoSessionIDFailsTheImport(t *testing.T) {
	// Arrange.
	legacy := writeLegacy(t, `{"version":1,"sessions":[{"session_id":""},{"session_id":"s_ok"}]}`, "")
	var logged []string

	// Act.
	r := OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: legacy, Logf: collectLogf(&logged)})

	// Assert — a partial roster is never admitted.
	if err := r.Prepare(); err == nil {
		t.Fatal("Prepare succeeded after importing a keyless legacy record")
	}
	if !strings.Contains(strings.Join(logged, "\n"), "INVALID") {
		t.Fatalf("the keyless legacy record was not loud: %q", logged)
	}
}

func TestAPopulatedTableIsNeverOverwrittenByTheLegacyJSON(t *testing.T) {
	// Arrange — a store already carrying records (written by a build that
	// owned the tables) and a stale JSON file beside it, with no stamp.
	path := testPath(t)
	if err := Open(path, discardLogf).Put(Record{SessionID: "s_current", CWD: "/w"}); err != nil {
		t.Fatal(err)
	}
	legacy := writeLegacy(t, `{"version":1,"sessions":[{"session_id":"s_ancient","cwd":"/old"}]}`, "")

	// Act.
	r := OpenWith(Options{DBPath: path, LegacyJSONPath: legacy, Logf: discardLogf})

	// Assert.
	if _, ok := r.Get("s_ancient"); ok {
		t.Fatal("a stale legacy file was imported over a populated table")
	}
	if _, ok := r.Get("s_current"); !ok {
		t.Fatal("the current roster was lost")
	}
}

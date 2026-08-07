package registry

import (
	"encoding/json"
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

// readRetirement decodes the deprecation record planted beside a retired
// legacy registry.
func readRetirement(t *testing.T, legacy string) legacyRetirement {
	t.Helper()
	data, err := os.ReadFile(legacy + retiredMarkerSuffix)
	if err != nil {
		t.Fatalf("read retirement record: %v", err)
	}
	var rec legacyRetirement
	if err := json.Unmarshal(data, &rec); err != nil {
		t.Fatalf("parse retirement record: %v", err)
	}
	return rec
}

func TestTheRetiredLegacyJSONGetsADeprecationRecordNamingItsSuccessor(t *testing.T) {
	// Arrange — a retired roster file, and the store that actually owns the
	// registry now.
	legacy := writeLegacy(t, `{"version":1,"sessions":[{"session_id":"s_1","cwd":"/w"}]}`, "")
	store := testPath(t)

	// Act.
	OpenWith(Options{DBPath: store, LegacyJSONPath: legacy, Logf: discardLogf})

	// Assert — the dead file now says it is dead and points at the live store.
	rec := readRetirement(t, legacy)
	if rec.SuccessorAuthority != store {
		t.Fatalf("successor_authority = %q, want %q", rec.SuccessorAuthority, store)
	}
	if rec.ImportStamp == "" {
		t.Fatal("the deprecation record does not say when the file's contents were frozen")
	}
	if !strings.Contains(rec.Note, "RETIRED") {
		t.Fatalf("note does not state the retirement: %q", rec.Note)
	}
}

func TestTheRetirementIsAnnouncedLoudly(t *testing.T) {
	// Arrange.
	legacy := writeLegacy(t, `{"version":1,"sessions":[]}`, "")
	var logged []string

	// Act.
	OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: legacy, Logf: collectLogf(&logged)})

	// Assert.
	if !strings.Contains(strings.Join(logged, "\n"), "is RETIRED") {
		t.Fatalf("the retirement was not announced: %q", logged)
	}
}

func TestTheRetiredFileAndItsSidecarSurviveAsHistory(t *testing.T) {
	// Arrange — the roster and the checkpoint sidecar it shipped with.
	legacy := writeLegacy(t, `{"version":2,"sessions":[]}`,
		`{"version":1,"conversation_checkpoints":[]}`)

	// Act.
	OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: legacy, Logf: discardLogf})

	// Assert — retirement labels the history, it never destroys it.
	for _, path := range []string{legacy, legacy + ".checkpoints"} {
		if _, err := os.Stat(path); err != nil {
			t.Fatalf("pre-migration history at %s was destroyed: %v", path, err)
		}
	}
}

func TestOrphanedResidueOfTheRetiredWriterIsSwept(t *testing.T) {
	// Arrange — every leftover shape the retired atomic write-rename path could
	// strand: a partial roster write, a partial sidecar write, and the lock that
	// used to guard the rename.
	tests := []struct {
		name    string
		residue string
	}{
		{name: "partial roster write", residue: ".tmp-16500999"},
		{name: "partial sidecar write", residue: ".checkpoints.tmp-3041979441"},
		{name: "stale rename lock", residue: ".lock"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			legacy := writeLegacy(t, `{"version":1,"sessions":[]}`, "")
			orphan := legacy + tc.residue
			if err := os.WriteFile(orphan, []byte("partial"), 0o600); err != nil {
				t.Fatalf("write orphaned residue: %v", err)
			}

			// Act.
			OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: legacy, Logf: discardLogf})

			// Assert — swept from disk and accounted for in the record, so the
			// freeze can no longer be read as a live writer failing its renames.
			if _, err := os.Stat(orphan); !os.IsNotExist(err) {
				t.Fatalf("orphaned residue %s survived: err=%v", orphan, err)
			}
			rec := readRetirement(t, legacy)
			want := filepath.Base(orphan)
			found := false
			for _, name := range rec.ResidueRemoved {
				if name == want {
					found = true
				}
			}
			if !found {
				t.Fatalf("residue_removed = %v, want it to record %q", rec.ResidueRemoved, want)
			}
		})
	}
}

func TestASettledRetirementIsSilentOnEveryLaterBoot(t *testing.T) {
	// Arrange — a boot that already planted the record and swept the residue.
	legacy := writeLegacy(t, `{"version":1,"sessions":[]}`, "")
	store := testPath(t)
	OpenWith(Options{DBPath: store, LegacyJSONPath: legacy, Logf: discardLogf})
	var logged []string

	// Act — the next boot, with nothing left to do.
	OpenWith(Options{DBPath: store, LegacyJSONPath: legacy, Logf: collectLogf(&logged)})

	// Assert — retirement is idempotent, not a line of boot noise per restart.
	if len(logged) != 0 {
		t.Fatalf("a settled retirement logged %q, want nothing", logged)
	}
}

func TestAnUnwritableDeprecationRecordIsSurfacedLoudly(t *testing.T) {
	// Arrange — the marker path is occupied by a directory, so the record's
	// atomic install cannot land. Residue is present so the pass is not short
	// circuited by the already-settled check.
	legacy := writeLegacy(t, `{"version":1,"sessions":[]}`, "")
	if err := os.Mkdir(legacy+retiredMarkerSuffix, 0o700); err != nil {
		t.Fatalf("occupy the marker path: %v", err)
	}
	if err := os.WriteFile(legacy+".tmp-1", []byte("partial"), 0o600); err != nil {
		t.Fatalf("write orphaned residue: %v", err)
	}
	var logged []string

	// Act.
	OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: legacy, Logf: collectLogf(&logged)})

	// Assert — never silently skipped.
	if !strings.Contains(strings.Join(logged, "\n"), "MARKER WRITE FAILED") {
		t.Fatalf("a failed deprecation record was not surfaced: %q", logged)
	}
}

func TestUnremovableResidueIsSurfacedLoudly(t *testing.T) {
	// Arrange — residue that cannot be removed (a non-empty directory wearing
	// the retired writer's temp-file name).
	legacy := writeLegacy(t, `{"version":1,"sessions":[]}`, "")
	stuck := legacy + ".tmp-42"
	if err := os.Mkdir(stuck, 0o700); err != nil {
		t.Fatalf("create stuck residue: %v", err)
	}
	if err := os.WriteFile(filepath.Join(stuck, "occupant"), []byte("x"), 0o600); err != nil {
		t.Fatalf("occupy stuck residue: %v", err)
	}
	var logged []string

	// Act.
	OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: legacy, Logf: collectLogf(&logged)})

	// Assert — the sweep's failure is reported rather than swallowed, and the
	// record does not claim to have removed it.
	if !strings.Contains(strings.Join(logged, "\n"), "could not remove orphaned residue") {
		t.Fatalf("the failed sweep was not surfaced: %q", logged)
	}
	if rec := readRetirement(t, legacy); len(rec.ResidueRemoved) != 0 {
		t.Fatalf("residue_removed = %v, want nothing claimed removed", rec.ResidueRemoved)
	}
}

func TestNoLegacyFileMeansNoDeprecationRecord(t *testing.T) {
	// Arrange — a fresh install that never held a JSON registry.
	missing := filepath.Join(t.TempDir(), "claude-repld-sessions.json")

	// Act.
	OpenWith(Options{DBPath: testPath(t), LegacyJSONPath: missing, Logf: discardLogf})

	// Assert — nothing was retired, so nothing is labelled retired.
	if _, err := os.Stat(missing + retiredMarkerSuffix); !os.IsNotExist(err) {
		t.Fatalf("a deprecation record was planted with no retired file: err=%v", err)
	}
}

func TestStorePathOverridesDBPathAsTheSuccessorAuthority(t *testing.T) {
	// Arrange — production shares an already-open handle, so the store's path
	// arrives out of band rather than as DBPath.
	legacy := writeLegacy(t, `{"version":1,"sessions":[]}`, "")
	shared := Open(testPath(t), discardLogf)
	t.Cleanup(func() { _ = shared.Close() })

	// Act.
	OpenWith(Options{DB: shared.db, StorePath: "/var/state.db", LegacyJSONPath: legacy, Logf: discardLogf})

	// Assert.
	if got := readRetirement(t, legacy).SuccessorAuthority; got != "/var/state.db" {
		t.Fatalf("successor_authority = %q, want the shared store's path", got)
	}
}

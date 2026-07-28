package registry

import (
	"path/filepath"
	"testing"
)

// The replay floor across a DAEMON RESTART.
//
// Every restart mints a fresh s_<hex> id for the same conversation, so the
// floor's durability is only half the contract: it also has to come BACK onto
// whatever record the new daemon files the conversation under. A floor that
// survived on the checkpoint but never rehydrated onto the live record would
// leave the driver reading zero and replaying the whole conversation a clear
// already discarded — the exact failure the floor exists to end.

// floorTestPath is this file's own registry path helper, kept local so these
// cases do not depend on a helper another test file owns.
func floorTestPath(t *testing.T) string {
	t.Helper()
	return filepath.Join(t.TempDir(), "state.db")
}

// floorDiscardLogf drops the registry's log lines; these cases assert on state.
func floorDiscardLogf(string, ...any) {}

func TestReplayFloorRehydratesOntoAFreshSessionRecordAfterARestart(t *testing.T) {
	// Arrange — one conversation, its floor recorded under the session id the
	// PREVIOUS daemon minted.
	const (
		configDir = "/cfg"
		cwd       = "/w"
		vendorID  = "uuid-1"
	)
	path := floorTestPath(t)
	before := Open(path, floorDiscardLogf)
	if err := before.Put(Record{
		SessionID: "s_before", ConfigDir: configDir, CWD: cwd, ClaudeSessionID: vendorID,
		NewestClearOrCompactSeq: 512,
	}); err != nil {
		t.Fatalf("Put(s_before): %v", err)
	}

	// Act — a restart: a fresh registry over the persisted state, and a fresh
	// session id for the SAME conversation, filed knowing nothing of the floor.
	restarted := Open(path, floorDiscardLogf)
	if err := restarted.Put(Record{
		SessionID: "s_after", ConfigDir: configDir, CWD: cwd, ClaudeSessionID: vendorID,
	}); err != nil {
		t.Fatalf("Put(s_after): %v", err)
	}

	// Assert — the checkpoint handed the floor back to the new record.
	rec, ok := restarted.Get("s_after")
	if !ok {
		t.Fatal("the restarted registry lost the fresh session record")
	}
	if rec.NewestClearOrCompactSeq != 512 {
		t.Fatalf("newest_clear_or_compact_seq = %d, want 512 — the floor must outlive the session id it was first filed under",
			rec.NewestClearOrCompactSeq)
	}
}

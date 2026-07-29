package server

import (
	"testing"

	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// ADOPT LATE — a vendor conversation id is persisted only once the conversation
// durably EXISTS.
//
// The SDK mints a uuid at startup and writes nothing for it until a turn runs.
// A session created, never prompted, and then dead left that uuid on its record
// pointing at a transcript that never existed, so every later bring-up ran
// `claude --resume <uuid>`, the CLI exited 1, and the workspace sat in
// `starting` with nothing to explain it.
// ---------------------------------------------------------------------------

func TestFirstAdoptionWithoutTurnEvidenceWritesNothing(t *testing.T) {
	// Arrange — a session that has not yet run a turn.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	_, _, adopted := r.AdoptVendorSessionID("s1", "uuid-first", false)

	// Assert.
	if adopted {
		t.Fatal("a first adoption with no turn behind it reported adopted")
	}
	if rec, _ := reg.Get("s1"); rec.ClaudeSessionID != "" {
		t.Fatalf("claude_session_id = %q, want empty: a turn-less session must leave no pointer", rec.ClaudeSessionID)
	}
}

func TestFirstAdoptionWithoutTurnEvidenceIsLoud(t *testing.T) {
	// Arrange — the hold is the gate that keeps a bad pointer off the record,
	// and a silent one is indistinguishable from a lost write.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var logged []string
	r := &RegistryRegistrar{Reg: reg, Logf: func(f string, a ...any) { logged = append(logged, f) }}

	// Act.
	r.AdoptVendorSessionID("s1", "uuid-first", false)

	// Assert.
	if len(logged) == 0 {
		t.Fatal("the held adoption passed SILENTLY")
	}
}

func TestFirstAdoptionWithTurnEvidenceWritesThrough(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	_, _, adopted := r.AdoptVendorSessionID("s1", "uuid-first", true)

	// Assert.
	if !adopted {
		t.Fatal("a first adoption backed by a turn was refused")
	}
	if rec, _ := reg.Get("s1"); rec.ClaudeSessionID != "uuid-first" {
		t.Fatalf("claude_session_id = %q, want uuid-first", rec.ClaudeSessionID)
	}
}

func TestRotationIsNeverGatedOnTurnEvidence(t *testing.T) {
	// Arrange — a record already carrying a uuid names a conversation the
	// vendor demonstrably wrote, so the rotation off it (and the cursor reset
	// that must accompany it) can never be held back.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ClaudeSessionID: "uuid-old", LastSeq: 5990}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act — durable=false, the strictest case.
	rotated, previous, adopted := r.AdoptVendorSessionID("s1", "uuid-new", false)

	// Assert.
	if !rotated || !adopted || previous != "uuid-old" {
		t.Fatalf("AdoptVendorSessionID = (%v, %q, %v), want (true, \"uuid-old\", true)", rotated, previous, adopted)
	}
	if got := NewRegistrySeqStore(reg, nil).LastSeq("s1"); got != 0 {
		t.Fatalf("last_seq after the rotation = %d, want 0", got)
	}
}

func TestClaudeSessionIDChangedHoldsAFirstWriteWithoutTurnEvidence(t *testing.T) {
	// Arrange — the live-stream write path takes the same gate as the
	// handshake one; a uuid learned off an event is no more proof of a written
	// transcript than one learned off a hello.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	adopted := r.ClaudeSessionIDChanged("s1", "uuid-first", false)

	// Assert.
	if adopted {
		t.Fatal("a first write with no turn behind it reported adopted")
	}
	if rec, _ := reg.Get("s1"); rec.ClaudeSessionID != "" {
		t.Fatalf("claude_session_id = %q, want empty", rec.ClaudeSessionID)
	}
}

func TestClaudeSessionIDChangedUpdatesAnAlreadyAdoptedRecord(t *testing.T) {
	// Arrange — the record already names a conversation the vendor wrote, so
	// keeping it current off the live stream is not a first claim.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ClaudeSessionID: "uuid-old"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	adopted := r.ClaudeSessionIDChanged("s1", "uuid-new", false)

	// Assert.
	if !adopted {
		t.Fatal("an update to an already-adopted record was refused")
	}
	if rec, _ := reg.Get("s1"); rec.ClaudeSessionID != "uuid-new" {
		t.Fatalf("claude_session_id = %q, want uuid-new", rec.ClaudeSessionID)
	}
}

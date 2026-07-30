package server

import (
	"testing"

	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// ADOPT EAGERLY — a vendor conversation id is persisted the moment it is
// announced, and whether the conversation EXISTS is checked at resume.
//
// This file used to assert the opposite. The SDK mints a uuid at startup and
// writes nothing for it until a turn runs, so a session created, never
// prompted, then dead left that uuid on its record pointing at a transcript
// that never existed — and every later bring-up ran `claude --resume <uuid>`,
// the CLI exited 1, and the workspace sat in `starting`.
//
// The fix for that was to refuse the write. The fix now is to check the disk
// where the answer is USED: ConversationResolver stats the transcript and skips
// a conversation the vendor never wrote. Same authority, consulted later, and
// it also catches a uuid that stops being resumable AFTER adoption — which
// refusing-to-write never could.
// ---------------------------------------------------------------------------

func TestFirstAdoptionIsWrittenImmediately(t *testing.T) {
	// Arrange — a session that has not yet run a turn.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	_, _, adopted := r.AdoptVendorSessionID("s1", "uuid-first")

	// Assert.
	if !adopted {
		t.Fatal("a first adoption was refused; adoption is eager now")
	}
	if rec, _ := reg.Get("s1"); rec.ClaudeSessionID != "uuid-first" {
		t.Fatalf("claude_session_id = %q, want uuid-first", rec.ClaudeSessionID)
	}
}

func TestFirstAdoptionLeavesNoEmptyUUIDForClientLogsToDisagreeWith(t *testing.T) {
	// Arrange — the shim and the webapp both know the uuid from bring-up. A
	// registry that reports "" while they report a real value makes every
	// client log fail identity validation and nack, forever, for a session
	// that never ran a turn. That is what the hold actually cost.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}
	r.AdoptVendorSessionID("s1", "uuid-first")

	// Act.
	identity, known := (&RegistryClientLogIdentityResolver{Reg: reg}).ResolveClientLogIdentity("/w")

	// Assert.
	if !known {
		t.Fatal("the client-log identity is unresolvable for a live session")
	}
	if identity.ClaudeSessionID != "uuid-first" {
		t.Fatalf("client-log ClaudeSessionID = %q, want uuid-first: a webapp stamping the real uuid would nack against anything else", identity.ClaudeSessionID)
	}
}

func TestRotationResetsTheCursorInTheSameWrite(t *testing.T) {
	// Arrange — a rotation retires one seq space for another, so the adoption
	// and the cursor reset that must accompany it are one indivisible write.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ClaudeSessionID: "uuid-old", LastSeq: 5990}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	rotated, previous, adopted := r.AdoptVendorSessionID("s1", "uuid-new")

	// Assert.
	if !rotated || !adopted || previous != "uuid-old" {
		t.Fatalf("AdoptVendorSessionID = (%v, %q, %v), want (true, \"uuid-old\", true)", rotated, previous, adopted)
	}
	if got := NewRegistrySeqStore(reg, nil).LastSeq("s1"); got != 0 {
		t.Fatalf("last_seq after the rotation = %d, want 0", got)
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
	adopted := r.ClaudeSessionIDChanged("s1", "uuid-new")

	// Assert.
	if !adopted {
		t.Fatal("an update to an already-adopted record was refused")
	}
	if rec, _ := reg.Get("s1"); rec.ClaudeSessionID != "uuid-new" {
		t.Fatalf("claude_session_id = %q, want uuid-new", rec.ClaudeSessionID)
	}
}

package server

import (
	"testing"

	"claude-repld/internal/registry"
)

// RegistryRegistrar.AdoptVendorSessionID — the daemon's half of a VENDOR
// SESSION UUID ROTATION.
//
// The vendor retires one transcript identity mid-stream (a `/clear` does
// exactly this) and mints another, which retires the store seq space keyed by
// the old uuid and starts a fresh one at 1. Both of the daemon's cursors count
// in that retired space, so both must reset — and in the SAME write as the new
// uuid, because registry maintenance hydrates a record's cursors up from the
// conversation checkpoint filed under its CURRENT uuid on every mutation.

func TestAdoptVendorSessionIDReportsARotation(t *testing.T) {
	// Arrange — a session already filing under one vendor uuid.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ClaudeSessionID: "uuid-old"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	rotated, previous := r.AdoptVendorSessionID("s1", "uuid-new")

	// Assert.
	if !rotated || previous != "uuid-old" {
		t.Fatalf("AdoptVendorSessionID = (%v, %q), want (true, \"uuid-old\")", rotated, previous)
	}
}

func TestAdoptVendorSessionIDPersistsTheRotatedUUID(t *testing.T) {
	// Arrange — the uuid is the --resume target and the SSM's binding key, so
	// it must land on the durable record.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ClaudeSessionID: "uuid-old"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	r.AdoptVendorSessionID("s1", "uuid-new")

	// Assert.
	rec, _ := reg.Get("s1")
	if rec.ClaudeSessionID != "uuid-new" {
		t.Fatalf("claude_session_id = %q, want %q", rec.ClaudeSessionID, "uuid-new")
	}
}

func TestAdoptVendorSessionIDResetsTheStoreCursorOnARotation(t *testing.T) {
	// Arrange — a high-water mark counted entirely in the RETIRED seq space.
	// Resuming from it would ask the new space for events past its end and
	// then read the new space's own seq=1 as a terminal regression.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ClaudeSessionID: "uuid-old", LastSeq: 5990}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	r.AdoptVendorSessionID("s1", "uuid-new")

	// Assert — read through the SAME adapter the resubscribe reads through, so
	// a checkpoint hydrating the mark back up would be caught here.
	if got := NewRegistrySeqStore(reg, nil).LastSeq("s1"); got != 0 {
		t.Fatalf("last_seq after the rotation = %d, want 0: the new store seq space begins at 1", got)
	}
}

func TestAdoptVendorSessionIDResetsTheReplayFloorOnARotation(t *testing.T) {
	// Arrange — a replay floor counted in the retired space. Left standing it
	// sits above every seq the new space will produce for a long while, and
	// would floor away the whole post-rotation conversation, the very clear
	// that caused the rotation included.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ClaudeSessionID: "uuid-old", NewestClearOrCompactSeq: 5000}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	r.AdoptVendorSessionID("s1", "uuid-new")

	// Assert.
	if got := NewRegistrySeqStore(reg, nil).NewestClearOrCompactSeq("s1"); got != 0 {
		t.Fatalf("replay floor after the rotation = %d, want 0", got)
	}
}

func TestAdoptVendorSessionIDResetsNothingForTheSameUUID(t *testing.T) {
	// Arrange — every re-handshake re-announces the uuid it already had. That
	// is not a rotation and must not throw the cursor away.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ClaudeSessionID: "uuid-old", LastSeq: 5990}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	rotated, _ := r.AdoptVendorSessionID("s1", "uuid-old")

	// Assert.
	if rotated {
		t.Fatal("a re-announcement of the SAME uuid reported a rotation")
	}
	if got := NewRegistrySeqStore(reg, nil).LastSeq("s1"); got != 5990 {
		t.Fatalf("last_seq = %d, want 5990 left untouched", got)
	}
}

func TestAdoptVendorSessionIDIsNoRotationForAFirstAdoption(t *testing.T) {
	// Arrange — a fresh session learning its uuid for the first time. There is
	// no retired space, so nothing is being replaced.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", LastSeq: 12}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	rotated, previous := r.AdoptVendorSessionID("s1", "uuid-first")

	// Assert.
	if rotated || previous != "" {
		t.Fatalf("AdoptVendorSessionID = (%v, %q), want (false, \"\") for a first adoption", rotated, previous)
	}
	rec, _ := reg.Get("s1")
	if rec.ClaudeSessionID != "uuid-first" || rec.LastSeq != 12 {
		t.Fatalf("record = (uuid %q, last_seq %d), want (uuid-first, 12)", rec.ClaudeSessionID, rec.LastSeq)
	}
}

func TestAdoptVendorSessionIDOnAnUnknownSessionIsLoud(t *testing.T) {
	// Arrange — an announcement for a session that was never registered.
	reg := openTestRegistry(t)
	var logged []string
	r := &RegistryRegistrar{Reg: reg, Logf: func(f string, a ...any) { logged = append(logged, f) }}

	// Act.
	r.AdoptVendorSessionID("ghost", "uuid-new")

	// Assert.
	if len(logged) == 0 {
		t.Fatal("an adoption for an unknown session passed SILENTLY")
	}
}

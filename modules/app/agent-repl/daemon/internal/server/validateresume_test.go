package server

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// VALIDATE BEFORE RESUME. The CLI hard-exits when handed a --resume whose
// transcript does not exist, and the shim's death during bring-up left the
// workspace in `starting` with nothing to explain it. So the pointer is checked
// against the disk before it is honoured.
// ---------------------------------------------------------------------------

// spawnerWithRecord builds a spawner over a record naming resumeID, capturing
// the CreateOpts the spawn was called with.
//
// lastTurnEndMs is the record's turn history and it is NOT decoration: it is
// the registry's durable answer to "has this uuid ever named an exchange", and
// the resume gate treats a missing transcript completely differently on either
// side of it. Zero is a session whose vendor uuid came from the bring-up
// handshake and nothing else.
func spawnerWithRecord(t *testing.T, cfgDir, resumeID string, lastTurnEndMs int64) (*ShimSpawner, *registry.Registry, *CreateOpts, *int) {
	t.Helper()
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", Model: "haiku",
		ConfigDir: cfgDir, ClaudeSessionID: resumeID, LastTurnEndMs: lastTurnEndMs,
	}); err != nil {
		t.Fatalf("put: %v", err)
	}
	got := &CreateOpts{}
	spawned := 0
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil },
		nil,
		func(_ string, opts CreateOpts) (ShimHandle, error) { spawned++; *got = opts; return ShimHandle{}, nil },
		func(string, ...any) {})
	return sp, reg, got, &spawned
}

func TestSpawnResumesWhenTheTranscriptExists(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	writeTranscript(t, cfg, "uuid-live")
	sp, _, got, _ := spawnerWithRecord(t, cfg, "uuid-live", 1786117000000)

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if got.Resume != "uuid-live" || res.Resumed != "uuid-live" {
		t.Fatalf("spawn resume = %q, result resumed = %q, want uuid-live", got.Resume, res.Resumed)
	}
}

func TestSpawnHardFailsWhenTheRecordedTranscriptIsMissing(t *testing.T) {
	// Arrange — the record names a conversation that RAN and whose transcript
	// is now unreadable. The turn history is what makes this the case the gate
	// exists for: there is real history to lose, so a fresh conversation in its
	// place would destroy it.
	cfg := t.TempDir()
	sp, reg, got, spawned := spawnerWithRecord(t, cfg, "uuid-gone", 1786117000000)

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert — no fresh spawn, no mutable repair, and no SpawnResult encoding a
	// successful downgrade.
	var missing *ResumeTranscriptMissingError
	if !errors.As(err, &missing) {
		t.Fatalf("EnsureShim error = %v, want *ResumeTranscriptMissingError", err)
	}
	if got.Resume != "" {
		t.Fatalf("spawn resume = %q, want no spawn", got.Resume)
	}
	if *spawned != 0 {
		t.Fatalf("spawn calls = %d, want zero", *spawned)
	}
	if res.Resumed != "" {
		t.Fatalf("SpawnResult.Resumed = %q, want empty on hard failure", res.Resumed)
	}
	if rec, ok := reg.Get("s1"); !ok || rec.ClaudeSessionID != "uuid-gone" || rec.CWD != "/w" || rec.ConfigDir != cfg {
		t.Fatalf("registry record after failed restore = %+v, want unchanged uuid/cwd/config_dir", rec)
	}
}

func TestSpawnStartsFreshForAHandshakeOnlyResumeTargetWithNoTurnHistory(t *testing.T) {
	// Arrange — a workspace brought up during a create whose shim died before
	// the first turn. The vendor minted the uuid at system:init, the daemon
	// recorded it, and no transcript was ever written because nothing was ever
	// said. The job's held initial prompt is riding on this respawn.
	cfg := t.TempDir()
	sp, reg, got, spawned := spawnerWithRecord(t, cfg, "uuid-handshake-only", 0)

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert — the spawn happens, fresh, and the prompt behind it survives.
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if *spawned != 1 {
		t.Fatalf("spawn calls = %d, want one", *spawned)
	}
	if got.Resume != "" || res.Resumed != "" {
		t.Fatalf("spawn resume = %q, result resumed = %q, want a fresh conversation", got.Resume, res.Resumed)
	}
	// Automatic restoration never mutates durable identity; the spawn's own
	// system:init is what replaces the pointer.
	if rec, ok := reg.Get("s1"); !ok || rec.ClaudeSessionID != "uuid-handshake-only" {
		t.Fatalf("registry record after the waiver = %+v, want the uuid untouched", rec)
	}
}

func TestSpawnLogsTheHandshakeOnlyResumeWaiverWithItsReason(t *testing.T) {
	// Arrange — the waiver is a decision to start a different conversation than
	// the record names, so it may never be silent.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: t.TempDir(), ClaudeSessionID: "uuid-handshake-only"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var logged []string
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil },
		nil,
		func(string, CreateOpts) (ShimHandle, error) { return ShimHandle{}, nil },
		func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) })

	// Act.
	if _, err := sp.EnsureShim(context.Background(), "s1"); err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}

	// Assert.
	if !containsFormat(logged, "reason=handshake_only_no_turn_ever_ran") {
		t.Fatalf("logged = %q, want the waiver and its reason", logged)
	}
	if containsFormat(logged, "event=resume_continuity_failure") {
		t.Fatalf("logged = %q, want no continuity failure for a conversation that never existed", logged)
	}
}

func TestSpawnInForceFakeModeRestoresTheRecordedResumeWithoutATranscript(t *testing.T) {
	// Arrange — the scripted SDK has no vendor transcript, but the durable
	// conversation pointer must remain exactly intact across hibernation.
	cfg := t.TempDir()
	sp, reg, got, spawned := spawnerWithRecord(t, cfg, "fake-session-id", 1786117000000)
	sp.ForceFake(true)

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert — fake mode bypasses only the transcript check; it never turns a
	// restore into a fresh conversation.
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if *spawned != 1 {
		t.Fatalf("spawn calls = %d, want one", *spawned)
	}
	if !got.Fake || got.Resume != "fake-session-id" || res.Resumed != "fake-session-id" {
		t.Fatalf("spawn opts = %+v, result = %+v, want fake resume fake-session-id", *got, res)
	}
	if rec, ok := reg.Get("s1"); !ok || rec.ClaudeSessionID != "fake-session-id" {
		t.Fatalf("registry record after fake restore = %+v, want intact resume", rec)
	}
}

func TestSpawnLogsTheCanonicalResumeContinuityFailure(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: t.TempDir(), ClaudeSessionID: "uuid-gone", LastTurnEndMs: 1786117000000}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var logged []string
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil },
		nil,
		func(string, CreateOpts) (ShimHandle, error) { return ShimHandle{}, nil },
		func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) })

	// Act.
	if _, err := sp.EnsureShim(context.Background(), "s1"); err == nil {
		t.Fatal("EnsureShim succeeded for a missing recorded transcript")
	}

	// Assert — exactly one owner-point record carries the identity and decision.
	if len(logged) != 1 {
		t.Fatalf("resume failure records = %d, want one: %v", len(logged), logged)
	}
	for _, field := range []string{
		"event=resume_continuity_failure",
		"operation=automatic_restore",
		"decision=hard_fail",
		"agent_repl_session_id=s1",
		"claude_session_id=uuid-gone",
		"cwd=/w",
		"config_dir=",
		"resolved_config_dir=",
		"transcript_path=",
	} {
		if !strings.Contains(logged[0], field) {
			t.Fatalf("resume failure missing %q: %s", field, logged[0])
		}
	}
}

func TestDropResumeReportsWhatItDropped(t *testing.T) {
	// Arrange. Explicit administrative callers need the removed identity for
	// durable-state audit logs.
	sp, _, _, _ := spawnerWithRecord(t, t.TempDir(), "uuid-gone", 1786117000000)

	// Act.
	dropped, err := sp.DropResume("s1")

	// Assert.
	if err != nil {
		t.Fatalf("DropResume: %v", err)
	}
	if dropped != "uuid-gone" {
		t.Fatalf("dropped = %q, want uuid-gone", dropped)
	}
}

func TestDropResumeOnAFreshSessionReportsNothing(t *testing.T) {
	// Arrange. A session without durable identity has nothing to remove.
	sp, _, _, _ := spawnerWithRecord(t, t.TempDir(), "", 0)

	// Act.
	dropped, err := sp.DropResume("s1")

	// Assert.
	if err != nil {
		t.Fatalf("DropResume: %v", err)
	}
	if dropped != "" {
		t.Fatalf("dropped = %q, want empty", dropped)
	}
}

func TestDropResumeOnAnUnknownSessionIsLoud(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	sp := NewShimSpawner(reg, nil, nil, func(string, CreateOpts) (ShimHandle, error) { return ShimHandle{}, nil }, nil)

	// Act.
	_, err := sp.DropResume("ghost")

	// Assert.
	if err == nil {
		t.Fatal("dropping the pointer of a session with no record must error")
	}
}

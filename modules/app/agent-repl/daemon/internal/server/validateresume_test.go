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
func spawnerWithRecord(t *testing.T, cfgDir, resumeID string) (*ShimSpawner, *registry.Registry, *CreateOpts, *int) {
	t.Helper()
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", Model: "haiku",
		ConfigDir: cfgDir, ClaudeSessionID: resumeID,
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
	sp, _, got, _ := spawnerWithRecord(t, cfg, "uuid-live")

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
	// Arrange — the record names a conversation the vendor never wrote.
	cfg := t.TempDir()
	sp, reg, got, spawned := spawnerWithRecord(t, cfg, "uuid-gone")

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

// A FAKE daemon's respawn must reach the same verdict its create did. The
// scripted offline SDK writes no vendor transcript, so gating the respawn on
// one made every fake session unrecoverable after a hibernation or a bounce.
func TestSpawnWaivesTheResumeGateForAFakeDaemon(t *testing.T) {
	// Arrange — a recorded conversation with no transcript, under -fake.
	cfg := t.TempDir()
	sp, _, got, spawned := spawnerWithRecord(t, cfg, "uuid-fake")
	sp.ForceFake(true)

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert — the same conversation is resumed rather than refused.
	if err != nil {
		t.Fatalf("EnsureShim under -fake: %v", err)
	}
	if *spawned != 1 {
		t.Fatalf("spawn calls = %d, want 1", *spawned)
	}
	if got.Resume != "uuid-fake" || res.Resumed != "uuid-fake" {
		t.Fatalf("spawn resume = %q, result resumed = %q, want uuid-fake", got.Resume, res.Resumed)
	}
}

func TestSpawnLogsTheCanonicalResumeContinuityFailure(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: t.TempDir(), ClaudeSessionID: "uuid-gone"}); err != nil {
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
	sp, _, _, _ := spawnerWithRecord(t, t.TempDir(), "uuid-gone")

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
	sp, _, _, _ := spawnerWithRecord(t, t.TempDir(), "")

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

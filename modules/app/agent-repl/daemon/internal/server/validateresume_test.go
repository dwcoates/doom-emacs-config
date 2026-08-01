package server

import (
	"context"
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
func spawnerWithRecord(t *testing.T, cfgDir, resumeID string) (*ShimSpawner, *registry.Registry, *CreateOpts) {
	t.Helper()
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", Model: "haiku",
		ConfigDir: cfgDir, ClaudeSessionID: resumeID,
	}); err != nil {
		t.Fatalf("put: %v", err)
	}
	got := &CreateOpts{}
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil },
		nil,
		func(_ string, opts CreateOpts) (func() error, error) { *got = opts; return nil, nil },
		func(string, ...any) {})
	return sp, reg, got
}

func TestSpawnResumesWhenTheTranscriptExists(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	writeTranscript(t, cfg, "uuid-live")
	sp, _, got := spawnerWithRecord(t, cfg, "uuid-live")

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if got.Resume != "uuid-live" || res.Resumed != "uuid-live" {
		t.Fatalf("spawn resume = %q, result resumed = %q, want uuid-live", got.Resume, res.Resumed)
	}
	if res.StaleResumeDropped != "" {
		t.Fatalf("dropped %q for a transcript that exists", res.StaleResumeDropped)
	}
}

func TestSpawnStartsFreshWhenTheTranscriptIsMissing(t *testing.T) {
	// Arrange — the record names a conversation the vendor never wrote.
	sp, _, got := spawnerWithRecord(t, t.TempDir(), "uuid-gone")

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if got.Resume != "" {
		t.Fatalf("spawn resume = %q, want empty: the CLI would exit 1 on it", got.Resume)
	}
	if res.StaleResumeDropped != "uuid-gone" {
		t.Fatalf("StaleResumeDropped = %q, want uuid-gone so the caller can note it", res.StaleResumeDropped)
	}
}

func TestSpawnDropsTheStalePointerFromTheRecord(t *testing.T) {
	// Arrange — a pointer left standing makes the NEXT bring-up repeat the
	// same failed resume, which is the loop this ends.
	sp, reg, _ := spawnerWithRecord(t, t.TempDir(), "uuid-gone")

	// Act.
	if _, err := sp.EnsureShim(context.Background(), "s1"); err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}

	// Assert.
	if rec, _ := reg.Get("s1"); rec.ClaudeSessionID != "" {
		t.Fatalf("claude_session_id = %q, want dropped", rec.ClaudeSessionID)
	}
}

func TestSpawnAnnouncesAStalePointerLoudly(t *testing.T) {
	// Arrange — a silent downgrade to a fresh conversation is exactly the
	// invisible data loss the note and this log line exist to prevent.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: t.TempDir(), ClaudeSessionID: "uuid-gone"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var logged []string
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil },
		nil,
		func(string, CreateOpts) (func() error, error) { return nil, nil },
		func(f string, a ...any) { logged = append(logged, f) })

	// Act.
	if _, err := sp.EnsureShim(context.Background(), "s1"); err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}

	// Assert.
	var found bool
	for _, l := range logged {
		if strings.Contains(l, "STALE RESUME POINTER") {
			found = true
		}
	}
	if !found {
		t.Fatalf("the stale pointer was dropped SILENTLY; lines=%v", logged)
	}
}

func TestDropResumeReportsWhatItDropped(t *testing.T) {
	// Arrange. Explicit administrative callers need the removed identity for
	// durable-state audit logs.
	sp, _, _ := spawnerWithRecord(t, t.TempDir(), "uuid-gone")

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
	sp, _, _ := spawnerWithRecord(t, t.TempDir(), "")

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
	sp := NewShimSpawner(reg, nil, nil, func(string, CreateOpts) (func() error, error) { return nil, nil }, nil)

	// Act.
	_, err := sp.DropResume("ghost")

	// Assert.
	if err == nil {
		t.Fatal("dropping the pointer of a session with no record must error")
	}
}

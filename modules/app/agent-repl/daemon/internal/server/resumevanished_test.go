package server

import (
	"context"
	"errors"
	"fmt"
	"os"
	"testing"
	"time"

	"claude-repld/internal/errclass"
	"claude-repld/internal/session"
)

// ---------------------------------------------------------------------------
// A VANISHED RESUME TARGET HAS TWO SHAPES, and only one of them is terminal.
// See resumevanished.go.
// ---------------------------------------------------------------------------

// ageTranscript back-dates a transcript so "newest" is a fact the test states
// rather than one it races the filesystem clock for.
func ageTranscript(t *testing.T, cfg, uuid string, age time.Duration) {
	t.Helper()
	path := session.TranscriptPath(session.ClaudeConfigDir(cfg), "/w", uuid)
	when := time.Now().Add(-age)
	if err := os.Chtimes(path, when, when); err != nil {
		t.Fatalf("chtimes %s: %v", path, err)
	}
}

// TestVanishedResumeTargetWithNoSiblingIsTerminal: nothing to resume and
// nothing to fall back to is a disposition, not just a failure — otherwise the
// ensure machinery re-runs the identical doomed bring-up forever.
func TestVanishedResumeTargetWithNoSiblingIsTerminal(t *testing.T) {
	// Arrange — a conversation that RAN, whose transcript is gone, in a config
	// dir holding no other transcript for this workspace.
	cfg := t.TempDir()
	sp, _, _, spawned := spawnerWithRecord(t, cfg, "uuid-gone", 1786117000000)

	// Act.
	_, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	if !errors.Is(err, errclass.ErrResumeTargetVanished) {
		t.Fatalf("EnsureShim error = %v, want it to classify as ErrResumeTargetVanished", err)
	}
	if *spawned != 0 {
		t.Fatalf("spawn calls = %d, want zero — the refusal to start a fresh conversation stands", *spawned)
	}
}

// TestTerminalVanishedResumeStillCarriesTheContinuityVerdict: the new
// disposition WRAPS the gate's verdict, so every existing reader of it is
// unchanged.
func TestTerminalVanishedResumeStillCarriesTheContinuityVerdict(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	sp, _, _, _ := spawnerWithRecord(t, cfg, "uuid-gone", 1786117000000)

	// Act.
	_, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	var missing *ResumeTranscriptMissingError
	if !errors.As(err, &missing) {
		t.Fatalf("EnsureShim error = %v, want the wrapped *ResumeTranscriptMissingError", err)
	}
	if missing.ResumeID != "uuid-gone" {
		t.Fatalf("wrapped verdict names %q, want uuid-gone", missing.ResumeID)
	}
	var terminal *VanishedResumeTargetError
	if !errors.As(err, &terminal) || terminal.SessionResumeFailureDetail() == nil {
		t.Fatalf("EnsureShim error = %v, want a terminal error carrying wire detail for its card", err)
	}
}

// TestTerminalRefusalCarriesItsDispositionOnTheOneRecord: the disposition rides
// the continuity diagnostic rather than adding a second account of one failure.
func TestTerminalRefusalCarriesItsDispositionOnTheOneRecord(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	sp, _, _, _ := spawnerWithRecord(t, cfg, "uuid-gone", 1786117000000)
	var logged []string
	sp.logf = func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) }

	// Act.
	if _, err := sp.EnsureShim(context.Background(), "s1"); err == nil {
		t.Fatal("EnsureShim succeeded for a vanished resume target")
	}

	// Assert.
	if len(logged) != 1 {
		t.Fatalf("records = %d, want exactly one: %v", len(logged), logged)
	}
	if !containsFormat(logged, "disposition=terminal_vanished_resume") {
		t.Fatalf("logged = %q, want the terminal disposition on the continuity record", logged)
	}
}

// TestVanishedResumeTargetFallsBackToTheNewestSibling: the workspace still has
// a conversation under another uuid, and resuming it keeps the continuity the
// stale pointer stood for.
func TestVanishedResumeTargetFallsBackToTheNewestSibling(t *testing.T) {
	// Arrange — two surviving transcripts, one clearly newer.
	cfg := t.TempDir()
	writeTranscript(t, cfg, "uuid-older")
	writeTranscript(t, cfg, "uuid-newer")
	ageTranscript(t, cfg, "uuid-older", time.Hour)
	sp, _, got, spawned := spawnerWithRecord(t, cfg, "uuid-gone", 1786117000000)

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if *spawned != 1 {
		t.Fatalf("spawn calls = %d, want one", *spawned)
	}
	if got.Resume != "uuid-newer" || res.Resumed != "uuid-newer" {
		t.Fatalf("spawn resume = %q, result resumed = %q, want the newest sibling uuid-newer", got.Resume, res.Resumed)
	}
}

// TestSiblingFallbackIsLoggedAsACorrection: resuming a conversation the record
// does not name is a decision, so it may never be silent.
func TestSiblingFallbackIsLoggedAsACorrection(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	writeTranscript(t, cfg, "uuid-sibling")
	sp, _, _, _ := spawnerWithRecord(t, cfg, "uuid-gone", 1786117000000)
	var logged []string
	sp.logf = func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) }

	// Act.
	if _, err := sp.EnsureShim(context.Background(), "s1"); err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}

	// Assert.
	if !containsFormat(logged, "is CORRECTED to sibling uuid-sibling") {
		t.Fatalf("logged = %q, want the correction naming both uuids", logged)
	}
}

// TestExistingResumeIgnoresSiblings: the fallback is reached only by a target
// that is GONE. A live target resumes itself, whatever else is on disk.
func TestExistingResumeIgnoresSiblings(t *testing.T) {
	// Arrange — the recorded target exists, and a newer sibling sits beside it.
	cfg := t.TempDir()
	writeTranscript(t, cfg, "uuid-live")
	writeTranscript(t, cfg, "uuid-newer")
	ageTranscript(t, cfg, "uuid-live", time.Hour)
	sp, _, got, _ := spawnerWithRecord(t, cfg, "uuid-live", 1786117000000)

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if got.Resume != "uuid-live" || res.Resumed != "uuid-live" {
		t.Fatalf("spawn resume = %q, result resumed = %q, want the recorded target uuid-live", got.Resume, res.Resumed)
	}
}

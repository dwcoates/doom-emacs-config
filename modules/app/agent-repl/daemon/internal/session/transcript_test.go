package session

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

// writeTranscript creates <configDir>/projects/<slug(cwd)>/<uuid>.jsonl with
// the given mtime and returns its path.
func writeTranscript(t *testing.T, configDir, cwd, uuid string, mtime time.Time) string {
	t.Helper()
	dir := ProjectDir(configDir, cwd)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
	path := filepath.Join(dir, uuid+".jsonl")
	if err := os.WriteFile(path, []byte("{}\n"), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	if err := os.Chtimes(path, mtime, mtime); err != nil {
		t.Fatalf("chtimes %s: %v", path, err)
	}
	return path
}

func TestEncodeCWDReplacesEveryNonAlphanumericByte(t *testing.T) {
	// Arrange / Act
	got := EncodeCWD("/Users/me/.config/doom_x")

	// Assert
	want := "-Users-me--config-doom-x"
	if got != want {
		t.Fatalf("EncodeCWD = %q; want %q", got, want)
	}
}

func TestTranscriptPathComposesTheProjectDirAndUUID(t *testing.T) {
	// Arrange / Act
	got := TranscriptPath("/cfg", "/w", "uuid-1")

	// Assert
	want := filepath.Join("/cfg", "projects", "-w", "uuid-1.jsonl")
	if got != want {
		t.Fatalf("TranscriptPath = %q; want %q", got, want)
	}
}

func TestClaudeConfigDirPrefersThePerSessionDir(t *testing.T) {
	// Arrange / Act
	got := ClaudeConfigDir("/per-session")

	// Assert
	if got != "/per-session" {
		t.Fatalf("ClaudeConfigDir = %q; want /per-session", got)
	}
}

func TestNewestTranscriptMissesWhenTheProjectDirDoesNotExist(t *testing.T) {
	// Arrange — a workspace nobody has ever talked to.
	cfg := t.TempDir()

	// Act
	_, ok, err := NewestTranscript(cfg, "/never-used")

	// Assert — a miss, NOT an error.
	if err != nil {
		t.Fatalf("NewestTranscript err = %v; want nil", err)
	}
	if ok {
		t.Fatalf("NewestTranscript found a transcript for an absent project dir")
	}
}

func TestNewestTranscriptPicksTheMostRecentlyModifiedFile(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	base := time.Date(2026, 7, 25, 12, 0, 0, 0, time.UTC)
	writeTranscript(t, cfg, "/w", "uuid-old", base)
	writeTranscript(t, cfg, "/w", "uuid-new", base.Add(time.Hour))

	// Act
	got, ok, err := NewestTranscript(cfg, "/w")

	// Assert
	if err != nil || !ok {
		t.Fatalf("NewestTranscript = _,%v,%v; want a hit", ok, err)
	}
	if got.SessionID != "uuid-new" {
		t.Fatalf("SessionID = %q; want uuid-new", got.SessionID)
	}
}

func TestNewestTranscriptIgnoresNonJSONLEntries(t *testing.T) {
	// Arrange — a stray file newer than the real transcript.
	cfg := t.TempDir()
	base := time.Date(2026, 7, 25, 12, 0, 0, 0, time.UTC)
	writeTranscript(t, cfg, "/w", "uuid-1", base)
	stray := filepath.Join(ProjectDir(cfg, "/w"), "notes.txt")
	if err := os.WriteFile(stray, []byte("x"), 0o644); err != nil {
		t.Fatalf("write stray: %v", err)
	}
	if err := os.Chtimes(stray, base.Add(time.Hour), base.Add(time.Hour)); err != nil {
		t.Fatalf("chtimes stray: %v", err)
	}

	// Act
	got, ok, err := NewestTranscript(cfg, "/w")

	// Assert
	if err != nil || !ok {
		t.Fatalf("NewestTranscript = _,%v,%v; want a hit", ok, err)
	}
	if got.SessionID != "uuid-1" {
		t.Fatalf("SessionID = %q; want uuid-1", got.SessionID)
	}
}

func TestDiscoverAdoptsTheTranscriptUnderTheSessionsOwnConfigDir(t *testing.T) {
	// Arrange
	own := t.TempDir()
	writeTranscript(t, own, "/w", "uuid-own", time.Date(2026, 7, 25, 12, 0, 0, 0, time.UTC))

	// Act
	d, err := Discover(own, []string{own}, "/w")

	// Assert
	if err != nil {
		t.Fatalf("Discover err = %v", err)
	}
	if !d.Found || d.Adopted.SessionID != "uuid-own" {
		t.Fatalf("Adopted = %+v, found=%v; want uuid-own", d.Adopted, d.Found)
	}
}

func TestDiscoverReportsAnotherConfigDirAsAMigrationCandidate(t *testing.T) {
	// Arrange — the conversation lives under a DIFFERENT account's root.
	own := t.TempDir()
	other := t.TempDir()
	writeTranscript(t, other, "/w", "uuid-elsewhere", time.Date(2026, 7, 25, 12, 0, 0, 0, time.UTC))

	// Act
	d, err := Discover(own, []string{own, other}, "/w")

	// Assert — reported, never adopted.
	if err != nil {
		t.Fatalf("Discover err = %v", err)
	}
	if d.Found {
		t.Fatalf("Discover adopted a transcript from a foreign config dir: %+v", d.Adopted)
	}
	if len(d.Migrations) != 1 || d.Migrations[0].SessionID != "uuid-elsewhere" {
		t.Fatalf("Migrations = %+v; want one uuid-elsewhere", d.Migrations)
	}
}

func TestDiscoverNeverReportsTheOwnDirAsItsOwnMigrationCandidate(t *testing.T) {
	// Arrange — the own dir also appears in the daemon-wide roster.
	own := t.TempDir()
	writeTranscript(t, own, "/w", "uuid-own", time.Date(2026, 7, 25, 12, 0, 0, 0, time.UTC))

	// Act
	d, err := Discover(own, []string{own}, "/w")

	// Assert
	if err != nil {
		t.Fatalf("Discover err = %v", err)
	}
	if len(d.Migrations) != 0 {
		t.Fatalf("Migrations = %+v; want none", d.Migrations)
	}
}

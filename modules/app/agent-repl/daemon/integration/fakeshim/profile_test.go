package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoadProfileWithoutADirectoryIsTheZeroProfile(t *testing.T) {
	// Arrange / Act
	got, err := LoadProfile("", "/w/one")

	// Assert
	if err != nil {
		t.Fatalf("LoadProfile = error %v, want the zero profile", err)
	}
	if got != (Profile{}) {
		t.Fatalf("LoadProfile = %+v, want the zero profile", got)
	}
}

func TestLoadProfilePrefersThePerWorkspaceFile(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	write(t, filepath.Join(dir, "default.json"), `{"build_sha":"shared"}`)
	write(t, filepath.Join(dir, ProfileFileName("/w/one")), `{"build_sha":"mine"}`)

	// Act
	got, err := LoadProfile(dir, "/w/one")

	// Assert
	if err != nil {
		t.Fatalf("LoadProfile = error %v, want the workspace's own profile", err)
	}
	if got.BuildSHA != "mine" {
		t.Fatalf("LoadProfile BuildSHA = %q, want the per-workspace file to win", got.BuildSHA)
	}
}

func TestLoadProfileFallsBackToDefault(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	write(t, filepath.Join(dir, "default.json"), `{"delay_diagnostics":true}`)

	// Act
	got, err := LoadProfile(dir, "/w/unprofiled")

	// Assert
	if err != nil {
		t.Fatalf("LoadProfile = error %v, want the default profile", err)
	}
	if !got.DelayDiagnostics {
		t.Fatalf("LoadProfile DelayDiagnostics = false, want the default profile applied")
	}
}

func TestLoadProfileRefusesAMalformedFile(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	write(t, filepath.Join(dir, "default.json"), `{not json`)

	// Act
	_, err := LoadProfile(dir, "/w/one")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "parse profile") {
		t.Fatalf("LoadProfile on a malformed file = %v, want a parse error and never a default", err)
	}
}

func TestProfileFileNameIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := ProfileFileName("/w/one")
	noisy := ProfileFileName("/w/two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("ProfileFileName = %q and %q, want one name for one cleaned path", plain, noisy)
	}
}

func write(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

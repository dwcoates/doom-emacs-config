package harness

import (
	"os"
	"path/filepath"
	"testing"
)

// withFakeGitBinary points the suite's fake git at a stand-in for one test.
func withFakeGitBinary(t *testing.T, path string) {
	t.Helper()
	saved := gitBinary
	gitBinary = path
	t.Cleanup(func() { gitBinary = saved })
}

func TestInstallFakeGitLinksTheSuitesBinaryRatherThanCopyingIt(t *testing.T) {
	// Arrange
	built := filepath.Join(t.TempDir(), "git")
	if err := os.WriteFile(built, []byte("#!/bin/sh\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	withFakeGitBinary(t, built)
	dir := filepath.Join(t.TempDir(), "fakebin")

	// Act
	InstallFakeGit(t, dir)

	// Assert
	want, err := os.Stat(built)
	if err != nil {
		t.Fatal(err)
	}
	got, err := os.Stat(filepath.Join(dir, "git"))
	if err != nil {
		t.Fatalf("stat the installed git: %v", err)
	}
	if !os.SameFile(want, got) {
		t.Fatal("the installed git is a separate file; want a hard link to the suite's binary")
	}
}

func TestInstallFakeGitInstallsAnExecutable(t *testing.T) {
	// Arrange
	built := filepath.Join(t.TempDir(), "git")
	if err := os.WriteFile(built, []byte("#!/bin/sh\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	withFakeGitBinary(t, built)
	dir := filepath.Join(t.TempDir(), "fakebin")

	// Act
	InstallFakeGit(t, dir)

	// Assert
	info, err := os.Stat(filepath.Join(dir, "git"))
	if err != nil {
		t.Fatalf("stat the installed git: %v", err)
	}
	if info.Mode().Perm()&0o111 == 0 {
		t.Fatalf("installed git mode = %v, want it executable", info.Mode())
	}
}

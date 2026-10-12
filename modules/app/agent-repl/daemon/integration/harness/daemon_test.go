package harness

import (
	"os"
	"path/filepath"
	"strings"
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

func TestFreshRootInMemory(t *testing.T) {
	tests := []struct {
		name string
		opts Opts
		want bool
	}{
		{name: "a plain fresh root keeps its state in memory", opts: Opts{}, want: true},
		{name: "a root asked to be durable keeps the file", opts: Opts{DurableState: true}, want: false},
		{name: "a joining daemon keeps the file it joins", opts: Opts{Joining: "127.0.0.1:1"}, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := freshRootInMemory(tc.opts)

			// Assert
			if got != tc.want {
				t.Fatalf("freshRootInMemory(%+v) = %v, want %v", tc.opts, got, tc.want)
			}
		})
	}
}

func TestRefuseInMemoryReuseRefusesARootWhoseFirstDaemonRanInMemory(t *testing.T) {
	// Arrange
	root := filepath.Join(t.TempDir(), "state")
	inMemoryRoots.Store(root, true)
	t.Cleanup(func() { inMemoryRoots.Delete(root) })

	// Act
	err := refuseInMemoryReuse(root)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "DurableState") {
		t.Fatalf("refuseInMemoryReuse = %v, want a refusal naming DurableState", err)
	}
}

func TestRefuseInMemoryReuseAcceptsADurableRoot(t *testing.T) {
	// Act
	err := refuseInMemoryReuse(filepath.Join(t.TempDir(), "state"))

	// Assert
	if err != nil {
		t.Fatalf("refuseInMemoryReuse = %v, want a root never run in memory accepted", err)
	}
}

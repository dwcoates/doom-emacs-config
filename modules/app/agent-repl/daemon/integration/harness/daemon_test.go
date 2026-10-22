package harness

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/daemonaddr"
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

func TestRootIsFresh(t *testing.T) {
	tests := []struct {
		name string
		opts Opts
		want bool
	}{
		{name: "a root StartDaemon mints is fresh", opts: Opts{}, want: true},
		{name: "a root the caller minted and declared is fresh", opts: Opts{StateDir: "/x/state", FreshStateDir: true}, want: true},
		{name: "a root handed over from another daemon is not", opts: Opts{StateDir: "/x/state"}, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := rootIsFresh(tc.opts)

			// Assert
			if got != tc.want {
				t.Fatalf("rootIsFresh(%+v) = %v, want %v", tc.opts, got, tc.want)
			}
		})
	}
}

func TestKilledHoldsNoLock(t *testing.T) {
	answer := func(boot, serving int, err error) func(string) (daemonaddr.Holder, daemonaddr.Holder, error) {
		return func(string) (daemonaddr.Holder, daemonaddr.Holder, error) {
			return daemonaddr.Holder{PID: boot}, daemonaddr.Holder{PID: serving}, err
		}
	}
	killed := []int{100, 101}
	tests := []struct {
		name    string
		holders func(string) (daemonaddr.Holder, daemonaddr.Holder, error)
		wantErr string
	}{
		{name: "locks nobody holds", holders: answer(0, 0, nil)},
		{name: "a successor holding both", holders: answer(200, 200, nil)},
		{name: "the killed leader holding the boot claim", holders: answer(100, 0, nil), wantErr: "pid 100 still holds daemon.lock"},
		{name: "a killed member holding the serving lock", holders: answer(200, 101, nil), wantErr: "pid 101 still holds daemon.serving"},
		{name: "holders that cannot be named", holders: answer(0, 0, errors.New("denied")), wantErr: "name the locks' holders after the kill: denied"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := killedHoldsNoLock("/state/daemon.addr", killed, tt.holders)

			// Assert
			switch {
			case tt.wantErr == "" && err != nil:
				t.Fatalf("killedHoldsNoLock = %v, want nil", err)
			case tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)):
				t.Fatalf("killedHoldsNoLock = %v, want it to name %q", err, tt.wantErr)
			}
		})
	}
}

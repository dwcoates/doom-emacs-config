package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestStateDirRefusesWhenUnset(t *testing.T) {
	// Arrange: no state root in the environment.
	t.Setenv(stateDirEnv, "")

	// Act.
	_, err := stateDir()

	// Assert: COMMON.md's STATE ROOT contract has no fallback — divergence is
	// a loud misconfig, never a silent default.
	if err == nil {
		t.Fatalf("stateDir() with %s unset returned no error", stateDirEnv)
	}
}

func TestWriteAddrFileContent(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	if err := writeAddrFile(dir, "127.0.0.1:4321"); err != nil {
		t.Fatalf("writeAddrFile: %v", err)
	}

	// Assert: the literal "127.0.0.1:<port>" plus a newline (COMMON.md,
	// DAEMON ADDRESS) — clients parse exactly that.
	got, err := os.ReadFile(addrFilePath(dir))
	if err != nil {
		t.Fatalf("read daemon.addr: %v", err)
	}
	if string(got) != "127.0.0.1:4321\n" {
		t.Fatalf("daemon.addr = %q, want %q", string(got), "127.0.0.1:4321\n")
	}
}

func TestWriteAddrFileLeavesNoTempBehind(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act: the write is an atomic replace, so the temp file must be gone.
	if err := writeAddrFile(dir, "127.0.0.1:1"); err != nil {
		t.Fatalf("writeAddrFile: %v", err)
	}

	// Assert.
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("read state dir: %v", err)
	}
	if len(entries) != 1 || entries[0].Name() != addrFileName {
		names := make([]string, 0, len(entries))
		for _, e := range entries {
			names = append(names, e.Name())
		}
		t.Fatalf("state dir holds %v, want only %s", names, addrFileName)
	}
}

func TestWriteAddrFileReplacesExisting(t *testing.T) {
	// Arrange: a stale address already on disk.
	dir := t.TempDir()
	if err := os.WriteFile(addrFilePath(dir), []byte("127.0.0.1:1\n"), 0o644); err != nil {
		t.Fatalf("seed daemon.addr: %v", err)
	}

	// Act: a successor daemon binds a fresh port and rewrites the file.
	if err := writeAddrFile(dir, "127.0.0.1:2"); err != nil {
		t.Fatalf("writeAddrFile: %v", err)
	}

	// Assert.
	got, _ := os.ReadFile(addrFilePath(dir))
	if string(got) != "127.0.0.1:2\n" {
		t.Fatalf("daemon.addr = %q, want the successor's address", string(got))
	}
}

func TestRemoveAddrFileDeletes(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := writeAddrFile(dir, "127.0.0.1:9"); err != nil {
		t.Fatalf("writeAddrFile: %v", err)
	}

	// Act: an orderly exit removes the file (COMMON.md, DAEMON ADDRESS).
	if err := removeAddrFile(dir); err != nil {
		t.Fatalf("removeAddrFile: %v", err)
	}

	// Assert.
	if _, err := os.Stat(addrFilePath(dir)); !os.IsNotExist(err) {
		t.Fatalf("daemon.addr still present after removeAddrFile (stat err = %v)", err)
	}
}

func TestRemoveAddrFileToleratesAbsence(t *testing.T) {
	// Arrange: nothing was ever written — a second instance may legitimately
	// have replaced the file already.
	dir := t.TempDir()

	// Act.
	err := removeAddrFile(dir)

	// Assert.
	if err != nil {
		t.Fatalf("removeAddrFile on an absent file returned %v, want nil", err)
	}
}

func TestAddrFilePathIsUnderStateDir(t *testing.T) {
	// Arrange.
	dir := "/somewhere/state"

	// Act.
	got := addrFilePath(dir)

	// Assert: every client discovers the daemon at <state dir>/daemon.addr.
	if got != filepath.Join(dir, "daemon.addr") {
		t.Fatalf("addrFilePath = %q", got)
	}
}

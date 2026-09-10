package main

import (
	"os"
	"path/filepath"
	"strings"
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

func TestStateDirAnswersTheConfiguredRoot(t *testing.T) {
	// Arrange.
	t.Setenv(stateDirEnv, "/tmp/some-state-root")

	// Act.
	got, err := stateDir()

	// Assert.
	if err != nil {
		t.Fatalf("stateDir: %v", err)
	}
	if got != "/tmp/some-state-root" {
		t.Fatalf("stateDir = %q, want the configured root", got)
	}
}

func TestWriteAddrFileFailsWhenTheStateDirCannotBeCreated(t *testing.T) {
	// Arrange: a regular file standing where the state root would go.
	blocker := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(blocker, []byte("x"), 0o600); err != nil {
		t.Fatalf("writing the blocker: %v", err)
	}

	// Act.
	err := writeAddrFile(filepath.Join(blocker, "state"), "127.0.0.1:1")

	// Assert: discovery failing is loud; a fake nobody can find is useless.
	if err == nil {
		t.Fatal("writeAddrFile into an uncreatable state dir returned no error")
	}
	if !strings.Contains(err.Error(), "create state dir") {
		t.Fatalf("error = %v, want the state-dir failure named", err)
	}
}

func TestWriteAddrFileFailsWhenTheTempFileCannotBeCreated(t *testing.T) {
	// Arrange: the state dir exists but is not writable, so MkdirAll succeeds
	// and the atomic write's temp file is what cannot be made.
	dir := filepath.Join(t.TempDir(), "readonly")
	if err := os.Mkdir(dir, 0o555); err != nil {
		t.Fatalf("making the read-only state dir: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })

	// Act.
	err := writeAddrFile(dir, "127.0.0.1:1")

	// Assert.
	if err == nil {
		t.Fatal("writeAddrFile into a read-only state dir returned no error")
	}
	if !strings.Contains(err.Error(), "create temp addr file") {
		t.Fatalf("error = %v, want the temp-file failure named", err)
	}
}

func TestRemoveAddrFileSurfacesAFailureThatIsNotAbsence(t *testing.T) {
	// Arrange: daemon.addr is a non-empty DIRECTORY, so the remove fails for a
	// reason that is not "someone else already replaced it".
	dir := t.TempDir()
	if err := os.Mkdir(addrFilePath(dir), 0o755); err != nil {
		t.Fatalf("planting the directory: %v", err)
	}
	if err := os.WriteFile(filepath.Join(addrFilePath(dir), "child"), []byte("x"), 0o600); err != nil {
		t.Fatalf("populating the directory: %v", err)
	}

	// Act.
	err := removeAddrFile(dir)

	// Assert.
	if err == nil {
		t.Fatal("removeAddrFile over an undeletable path returned no error")
	}
}

package replog

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// backups lists the rotated log files in dir, sorted by ReadDir's name
// order.
func backups(t *testing.T, dir string) []string {
	t.Helper()
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	var names []string
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), FileName+".") {
			names = append(names, e.Name())
		}
	}
	return names
}

func TestOpenCreatesLogFileInFreshDir(t *testing.T) {
	// Arrange
	dir := filepath.Join(t.TempDir(), "state-root")
	// Act
	f, warnings, err := Open(dir)
	// Assert
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer f.Close()
	if len(warnings) != 0 {
		t.Fatalf("unexpected warnings: %v", warnings)
	}
	if f.Name() != filepath.Join(dir, FileName) {
		t.Fatalf("log file at %s, want %s", f.Name(), filepath.Join(dir, FileName))
	}
}

func TestOpenRotatesPreviousLogWithMtimeStamp(t *testing.T) {
	// Arrange — a previous run's log with a known mtime.
	dir := t.TempDir()
	current := filepath.Join(dir, FileName)
	if err := os.WriteFile(current, []byte("previous run\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	mtime := time.Date(2026, 7, 21, 14, 30, 0, 0, time.Local)
	if err := os.Chtimes(current, mtime, mtime); err != nil {
		t.Fatal(err)
	}
	// Act
	f, _, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer f.Close()
	// Assert — backup carries the old file's mtime and its content.
	want := FileName + ".20260721-143000"
	data, err := os.ReadFile(filepath.Join(dir, want))
	if err != nil {
		t.Fatalf("rotated backup %s: %v", want, err)
	}
	if string(data) != "previous run\n" {
		t.Fatalf("backup content = %q, want previous run's bytes", data)
	}
}

func TestOpenStartsTheNewLogEmpty(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, FileName), []byte("old\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	// Act
	f, _, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer f.Close()
	// Assert
	fi, err := f.Stat()
	if err != nil {
		t.Fatal(err)
	}
	if fi.Size() != 0 {
		t.Fatalf("new log starts at %d bytes, want 0", fi.Size())
	}
}

func TestOpenDisambiguatesASameStampRotation(t *testing.T) {
	// Arrange — a backup already holds the stamp the rotation would use.
	dir := t.TempDir()
	current := filepath.Join(dir, FileName)
	if err := os.WriteFile(current, []byte("newer\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	mtime := time.Date(2026, 7, 21, 14, 30, 0, 0, time.Local)
	if err := os.Chtimes(current, mtime, mtime); err != nil {
		t.Fatal(err)
	}
	taken := filepath.Join(dir, FileName+".20260721-143000")
	if err := os.WriteFile(taken, []byte("older\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	// Act
	f, _, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer f.Close()
	// Assert — both survive, the collision under a -1 suffix.
	data, err := os.ReadFile(taken + "-1")
	if err != nil {
		t.Fatalf("disambiguated backup: %v", err)
	}
	if string(data) != "newer\n" {
		t.Fatalf("disambiguated backup content = %q, want the newer run's bytes", data)
	}
}

func TestCappedWriterWritesBelowCapWithoutRotating(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	f, _, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	w := NewCappedWriter(dir, f, 100)
	// Act
	if _, err := w.Write([]byte("hello\n")); err != nil {
		t.Fatalf("Write: %v", err)
	}
	// Assert — no backup created, content lands in the (only) current file.
	if got := backups(t, dir); len(got) != 0 {
		t.Fatalf("backups = %v, want none below the cap", got)
	}
	data, err := os.ReadFile(filepath.Join(dir, FileName))
	if err != nil {
		t.Fatal(err)
	}
	if string(data) != "hello\n" {
		t.Fatalf("current log content = %q, want %q", data, "hello\n")
	}
}

func TestCappedWriterRotatesAtCapBoundary(t *testing.T) {
	// Arrange — a tiny cap that the first write already exceeds, but the
	// second write (post-rotation) stays under, so exactly one rotation
	// happens.
	dir := t.TempDir()
	f, _, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	w := NewCappedWriter(dir, f, 8)
	// Act
	preRotation := []byte("123456789\n")
	if _, err := w.Write(preRotation); err != nil {
		t.Fatalf("Write: %v", err)
	}
	postRotation := []byte("ok\n")
	if _, err := w.Write(postRotation); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if err := w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	// Assert — exactly one backup, holding the pre-rotation bytes.
	got := backups(t, dir)
	if len(got) != 1 {
		t.Fatalf("backups = %v, want exactly one", got)
	}
	backupData, err := os.ReadFile(filepath.Join(dir, got[0]))
	if err != nil {
		t.Fatal(err)
	}
	if string(backupData) != string(preRotation) {
		t.Fatalf("backup content = %q, want the pre-rotation bytes %q", backupData, preRotation)
	}
	// Assert — the new current file holds the post-rotation bytes plus
	// the mid-run rotation note, not the pre-rotation bytes.
	currentData, err := os.ReadFile(filepath.Join(dir, FileName))
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(currentData), "123456789") {
		t.Fatalf("current log %q must not contain pre-rotation bytes", currentData)
	}
	if !strings.Contains(string(currentData), "ok\n") {
		t.Fatalf("current log %q missing post-rotation bytes", currentData)
	}
	if !strings.Contains(string(currentData), "mid-run rotation") {
		t.Fatalf("current log %q missing the mid-run rotation note", currentData)
	}
}

func TestCappedWriterMidRunRotationPrunesBackups(t *testing.T) {
	// Arrange — KeepBackups existing backups plus a fresh current file
	// whose cap the first write already exceeds, so the mid-run rotation
	// itself must push the backup count over KeepBackups and prune.
	dir := t.TempDir()
	for i, stamp := range []string{
		"20260701-000000", "20260702-000000", "20260703-000000",
		"20260704-000000", "20260705-000000",
	} {
		name := filepath.Join(dir, fmt.Sprintf("%s.%s", FileName, stamp))
		if err := os.WriteFile(name, []byte(fmt.Sprintf("backup %d", i)), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	if len(backups(t, dir)) != KeepBackups {
		t.Fatalf("setup: want %d pre-existing backups", KeepBackups)
	}
	f, _, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	w := NewCappedWriter(dir, f, 5)
	// Act
	if _, err := w.Write([]byte("over the cap\n")); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if err := w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	// Assert — still bounded at KeepBackups, oldest pruned first.
	got := backups(t, dir)
	if len(got) != KeepBackups {
		t.Fatalf("backups = %v (%d), want %d", got, len(got), KeepBackups)
	}
	for _, name := range got {
		if strings.Contains(name, "20260701") {
			t.Fatalf("oldest backup %s should have been pruned, got %v", name, got)
		}
	}
}

func TestCappedWriterResetsCountAfterRotation(t *testing.T) {
	// Arrange — cap high enough that only the SECOND write should trip
	// it once the first write's bytes are counted.
	dir := t.TempDir()
	f, _, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	w := NewCappedWriter(dir, f, 10)
	// Act — first write (8 bytes) stays under the cap.
	if _, err := w.Write([]byte("12345678")); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if got := backups(t, dir); len(got) != 0 {
		t.Fatalf("backups after first write = %v, want none yet", got)
	}
	// Second write (8 more bytes) crosses the cap (16 >= 10).
	if _, err := w.Write([]byte("87654321")); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if err := w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	// Assert — exactly one rotation happened, not two.
	if got := backups(t, dir); len(got) != 1 {
		t.Fatalf("backups = %v, want exactly one rotation", got)
	}
}

func TestOpenPrunesBackupsBeyondKeep(t *testing.T) {
	// Arrange — KeepBackups+2 existing backups plus a current log; after
	// rotation the current becomes one more backup.
	dir := t.TempDir()
	stamps := []string{
		"20260701-000000", "20260702-000000", "20260703-000000",
		"20260704-000000", "20260705-000000", "20260706-000000",
		"20260707-000000",
	}
	for _, s := range stamps {
		if err := os.WriteFile(filepath.Join(dir, FileName+"."+s), []byte(s), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	current := filepath.Join(dir, FileName)
	if err := os.WriteFile(current, []byte("current\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	mtime := time.Date(2026, 7, 8, 0, 0, 0, 0, time.Local)
	if err := os.Chtimes(current, mtime, mtime); err != nil {
		t.Fatal(err)
	}
	// Act
	f, warnings, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer f.Close()
	if len(warnings) != 0 {
		t.Fatalf("unexpected warnings: %v", warnings)
	}
	// Assert — only the newest KeepBackups remain, oldest deleted first.
	got := backups(t, dir)
	want := []string{
		FileName + ".20260704-000000",
		FileName + ".20260705-000000",
		FileName + ".20260706-000000",
		FileName + ".20260707-000000",
		FileName + ".20260708-000000",
	}
	if len(got) != len(want) {
		t.Fatalf("kept %d backups (%v), want %d", len(got), got, len(want))
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("backups[%d] = %s, want %s", i, got[i], want[i])
		}
	}
}

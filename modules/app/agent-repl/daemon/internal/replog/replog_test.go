package replog

import (
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

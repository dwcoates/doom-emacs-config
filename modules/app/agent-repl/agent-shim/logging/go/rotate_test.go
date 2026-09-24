package logging

import (
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestRotatingFileAppendsToWhatItFinds pins the shared process-restart rule:
// opening does not roll, so a bounced service keeps its history instead of
// evicting a generation per bounce.
func TestRotatingFileAppendsToWhatItFinds(t *testing.T) {
	// Arrange: a log file that already holds a prior run's record.
	path := filepath.Join(t.TempDir(), "svc.log")
	if err := os.WriteFile(path, []byte("prior\n"), 0o644); err != nil {
		t.Fatalf("seeding the prior run's log: %v", err)
	}

	// Act: open it and write this run's record.
	w := openForTest(t, path, 1<<20, 5)
	mustWrite(t, w, "current\n")

	// Assert: both records are in the one file, and no generation was minted.
	if got := readFile(t, path); got != "prior\ncurrent\n" {
		t.Errorf("the log holds %q, want the prior run's record followed by this one", got)
	}
	if _, err := os.Lstat(path + ".1"); !os.IsNotExist(err) {
		t.Errorf("opening the log minted a generation at %s.1; opening must not roll", path)
	}
}

// TestRotatingFileRollsAtTheCap covers the cap itself: the write that would
// carry the file past it lands in a fresh generation, whole.
func TestRotatingFileRollsAtTheCap(t *testing.T) {
	// Arrange: a cap of ten bytes and a file already holding eight.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 10, 5)
	mustWrite(t, w, "12345678\n")

	// Act: a record that cannot fit beside it.
	mustWrite(t, w, "abcdefgh\n")

	// Assert: the first record rolled to .1 and the second stands alone.
	if got := readFile(t, path+".1"); got != "12345678\n" {
		t.Errorf("generation .1 holds %q, want the record that was there at the cap", got)
	}
	if got := readFile(t, path); got != "abcdefgh\n" {
		t.Errorf("the current log holds %q, want only the record written after the roll", got)
	}
}

func TestExplicitRollKeepsTheRetiredDescriptorOnItsGeneration(t *testing.T) {
	// Arrange: a descriptor held by the process being replaced.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 1<<20, 2)
	mustWrite(t, w, "old\n")
	held, err := os.Open(path)
	if err != nil {
		t.Fatalf("open the retiring process's descriptor: %v", err)
	}
	defer func() {
		if err := held.Close(); err != nil {
			t.Errorf("close the retiring process's descriptor: %v", err)
		}
	}()

	// Act: roll explicitly and write through the fresh generation.
	if err := w.Roll(); err != nil {
		t.Fatalf("Roll: %v", err)
	}
	mustWrite(t, w, "new\n")

	// Assert: the held descriptor and generation retain the old bytes while
	// the canonical path names only the fresh bytes.
	heldBytes, err := io.ReadAll(held)
	if err != nil {
		t.Fatalf("read the retiring process's descriptor: %v", err)
	}
	if got := string(heldBytes); got != "old\n" {
		t.Fatalf("the retiring descriptor reads %q, want the old generation", got)
	}
	if got := readFile(t, path+".1"); got != "old\n" {
		t.Fatalf("generation .1 holds %q, want the old generation", got)
	}
	if got := readFile(t, path); got != "new\n" {
		t.Fatalf("the current log holds %q, want the fresh generation", got)
	}
}

// TestRotatingFileRetainsExactlyNGenerations covers the disk bound: the oldest
// generation is discarded rather than accumulating.
func TestRotatingFileRetainsExactlyNGenerations(t *testing.T) {
	// Arrange: a two-generation writer with a cap of one record.
	dir := t.TempDir()
	path := filepath.Join(dir, "svc.log")
	w := openForTest(t, path, 4, 2)

	// Act: four records, so three rolls happen.
	for _, record := range []string{"aaa\n", "bbb\n", "ccc\n", "ddd\n"} {
		mustWrite(t, w, record)
	}

	// Assert: the current file plus exactly two generations remain.
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("reading the log directory: %v", err)
	}
	var names []string
	for _, e := range entries {
		names = append(names, e.Name())
	}
	if len(names) != 3 {
		t.Fatalf("the directory holds %v, want the current log and exactly 2 generations", names)
	}
}

// TestRotatingFileGenerationsAreOrderedNewestFirst covers the rename scheme:
// .1 is the most recent evicted record, .N the oldest.
func TestRotatingFileGenerationsAreOrderedNewestFirst(t *testing.T) {
	// Arrange: a three-generation writer with a cap of one record.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 4, 3)

	// Act: three records, so two rolls happen.
	for _, record := range []string{"aaa\n", "bbb\n", "ccc\n"} {
		mustWrite(t, w, record)
	}

	// Assert: .1 holds the newer evicted record, .2 the older.
	if got := readFile(t, path+".1"); got != "bbb\n" {
		t.Errorf("generation .1 holds %q, want the most recently evicted record", got)
	}
	if got := readFile(t, path+".2"); got != "aaa\n" {
		t.Errorf("generation .2 holds %q, want the oldest retained record", got)
	}
}

// TestRotatingFileKeepsARecordLargerThanTheCapWhole covers the oversized
// record: losing it would be worse than exceeding the cap once.
func TestRotatingFileKeepsARecordLargerThanTheCapWhole(t *testing.T) {
	// Arrange: a cap far smaller than the record about to be written.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 4, 2)
	oversized := strings.Repeat("x", 64) + "\n"

	// Act.
	mustWrite(t, w, oversized)

	// Assert: it landed whole rather than being split or refused.
	if got := readFile(t, path); got != oversized {
		t.Errorf("the log holds %d bytes, want the oversized record's %d written whole", len(got), len(oversized))
	}
}

// TestRotatingFileRefusesWritesOnceClosed covers the closed writer: a dropped
// record must be an error, never a silent loss.
func TestRotatingFileRefusesWritesOnceClosed(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 1<<20, 5)

	// Act.
	if err := w.Close(); err != nil {
		t.Fatalf("closing the writer: %v", err)
	}
	_, err := w.Write([]byte("after\n"))

	// Assert.
	if err == nil {
		t.Fatal("a write to a closed rotating log succeeded, want it refused")
	}
}

// TestOpenRotatingRefusesANegativeCap covers the refusal: "no cap" is the state
// this type exists to make unreachable, so a negative one is not clamped.
func TestOpenRotatingRefusesANegativeCap(t *testing.T) {
	// Arrange / Act.
	_, err := OpenRotating(filepath.Join(t.TempDir(), "svc.log"), -1, 5)

	// Assert.
	if err == nil {
		t.Fatal("a negative cap opened successfully, want it refused")
	}
}

func openForTest(t *testing.T, path string, capBytes int64, backups int) *RotatingFile {
	t.Helper()
	w, err := OpenRotating(path, capBytes, backups)
	if err != nil {
		t.Fatalf("opening the rotating log: %v", err)
	}
	t.Cleanup(func() { _ = w.Close() })
	return w
}

func mustWrite(t *testing.T, w *RotatingFile, s string) {
	t.Helper()
	if _, err := w.Write([]byte(s)); err != nil {
		t.Fatalf("writing %q: %v", s, err)
	}
}

func readFile(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("reading %q: %v", path, err)
	}
	return string(data)
}

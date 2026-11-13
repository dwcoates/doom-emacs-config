package cores

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestLedgerReadsWhatALiveRunPosted(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	l := &ledger{dir: dir}
	started := time.Date(2026, 11, 13, 9, 0, 0, 0, time.UTC)
	if _, err := l.post(UnitRecord{Unit: "a", Rank: 10, Width: 1, Cores: 1.2}); err != nil {
		t.Fatal(err)
	}
	path, err := l.post(UnitRecord{Unit: "b", Rank: 10, Width: 2, Cores: 3})
	if err != nil {
		t.Fatal(err)
	}
	if err := writeRecord(path, UnitRecord{Unit: "b", Rank: 10, Width: 2, Cores: 3, Started: started}); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := readLedger(dir)

	// Assert
	if err != nil || len(got) != 2 {
		t.Fatalf("readLedger = %+v, %v; want both units", got, err)
	}
	byUnit := map[string]UnitRecord{got[0].Unit: got[0], got[1].Unit: got[1]}
	if !byUnit["a"].Started.IsZero() || byUnit["a"].Cores != 1.2 || !byUnit["b"].Started.Equal(started) || byUnit["b"].Width != 2 {
		t.Fatalf("readLedger = %+v; want a waiting at 1.2 cores and b started", got)
	}
}

func TestLedgerRemovesADeadRunsRecordsAndLock(t *testing.T) {
	// Arrange: a run posted a unit, then died, its lock dropped.
	dir := t.TempDir()
	l := &ledger{dir: dir}
	if _, err := l.post(UnitRecord{Unit: "a", Width: 1, Cores: 1}); err != nil {
		t.Fatal(err)
	}
	if err := l.lock.Close(); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := readLedger(dir)

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("readLedger = %+v, %v; want the dead run's unit gone", got, err)
	}
	if left, _ := os.ReadDir(dir); len(left) != 0 {
		t.Fatalf("left %v; want the dead run's record and lock removed", left)
	}
}

func TestLedgerSweepsTheLockOfADeadRunWithNoUnits(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	l := &ledger{dir: dir}
	if err := l.open(); err != nil {
		t.Fatal(err)
	}
	if err := l.lock.Close(); err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := readLedger(dir); err != nil {
		t.Fatal(err)
	}

	// Assert
	if left, _ := os.ReadDir(dir); len(left) != 0 {
		t.Fatalf("left %v; want the dead run's lock removed", left)
	}
}

func TestLedgerRemovesRecordsWhoseRunLockIsGone(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, ledgerUnitPrefix+"1.2-1.json"), []byte(`{"unit":"a"}`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := readLedger(dir)

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("readLedger = %+v, %v; want the orphaned record ignored", got, err)
	}
	if left, _ := os.ReadDir(dir); len(left) != 0 {
		t.Fatalf("left %v; want the orphaned record removed", left)
	}
}

func TestLedgerOfAMissingDirectoryIsEmpty(t *testing.T) {
	// Act
	got, err := readLedger(filepath.Join(t.TempDir(), "missing"))

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("readLedger = %+v, %v; want nothing", got, err)
	}
}

func TestLedgerSurfacesARecordThatDoesNotRead(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	l := &ledger{dir: dir}
	path, err := l.post(UnitRecord{Unit: "a", Width: 1, Cores: 1})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("{"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err = readLedger(dir)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "does not read") {
		t.Fatalf("readLedger = %v; want the corrupt record surfaced", err)
	}
}

func TestLedgerSurfacesARecordNamingNoRun(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, ledgerUnitPrefix+"norun.json"), []byte("{}"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := readLedger(dir)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "names no run") {
		t.Fatalf("readLedger = %v; want the nameless record surfaced", err)
	}
}

func TestLedgerSurfacesARunLockItCannotOpen(t *testing.T) {
	// Arrange: the run's lock is a directory.
	dir := t.TempDir()
	if err := os.Mkdir(filepath.Join(dir, ledgerRunPrefix+"1.2.lock"), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := readLedger(dir)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "ledger lock") {
		t.Fatalf("readLedger = %v; want the unopenable lock surfaced", err)
	}
}

func TestLedgerPostSurfacesAnUnmakeableDirectory(t *testing.T) {
	// Arrange: the ledger's directory is a file.
	parent := t.TempDir()
	dir := filepath.Join(parent, "file")
	if err := os.WriteFile(dir, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	l := &ledger{dir: dir}

	// Act
	_, err := l.post(UnitRecord{Unit: "a"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "make the core seats directory") {
		t.Fatalf("post = %v; want the directory's failure surfaced", err)
	}
}

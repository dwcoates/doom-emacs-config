package sweepguard

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

const op = "daemon.test.sweep"

// logged is a guard and the logger its runs record on.
type logged struct {
	g   *Guard
	log *dlog.TestLogger
}

func (l logged) Do(fn func()) (bool, error) { return l.g.Do(l.log, fn) }

func newGuard(t *testing.T, path string) (logged, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	return logged{g: New("testsweep", path, op), log: log}, log
}

func messages(log *dlog.TestLogger, level string) []string {
	var out []string
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == op {
			out = append(out, r.Message)
		}
	}
	return out
}

func TestDoRunsTheSweepHoldingTheLock(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "sweep.lock")
	g, _ := newGuard(t, path)
	other, _ := newGuard(t, path)
	var heldInside bool

	// Act: a second guard on the same lock is refused while the first sweeps.
	ran, err := g.Do(func() {
		otherRan, otherErr := other.Do(func() {})
		heldInside = !otherRan && otherErr == nil
	})

	// Assert.
	if !ran || err != nil || !heldInside {
		t.Fatalf("Do = (%v, %v), held inside %v, want the sweep run with the lock held", ran, err, heldInside)
	}
}

func TestDoReleasesTheLockAfterTheSweep(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "sweep.lock")
	g, _ := newGuard(t, path)
	if _, err := g.Do(func() {}); err != nil {
		t.Fatalf("first Do: %v", err)
	}
	other, _ := newGuard(t, path)

	// Act.
	ran, err := other.Do(func() {})

	// Assert.
	if !ran || err != nil {
		t.Fatalf("Do after a finished sweep = (%v, %v), want it run", ran, err)
	}
}

func TestDoSkipsWhileAnotherHolderSweeps(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "sweep.lock")
	g, _ := newGuard(t, path)
	other, log := newGuard(t, path)
	called := false

	// Act.
	g.Do(func() { other.Do(func() { called = true }) })

	// Assert.
	if got := messages(log, dlog.LevelInfo); called || len(got) != 1 || got[0] != "another daemon is sweeping; this one does not" {
		t.Fatalf("called %v, info records %v, want the sweep skipped and recorded", called, got)
	}
}

func TestDoRefusesASecondSweepInTheSameProcess(t *testing.T) {
	// Arrange.
	g, log := newGuard(t, filepath.Join(t.TempDir(), "sweep.lock"))
	var inner error

	// Act.
	g.Do(func() { _, inner = g.Do(func() {}) })

	// Assert.
	if !errors.Is(inner, ErrRunning) {
		t.Fatalf("the nested Do = %v, want ErrRunning", inner)
	}
	if got := messages(log, dlog.LevelInfo); len(got) != 1 || !strings.Contains(got[0], "while one is running") {
		t.Fatalf("info records %v, want the refusal recorded", got)
	}
}

func TestDoRefusesALockItCannotTellAbout(t *testing.T) {
	// Arrange: the lock's directory is a regular file.
	notADir := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(notADir, nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	g, log := newGuard(t, filepath.Join(notADir, "sweep.lock"))
	called := false

	// Act.
	ran, err := g.Do(func() { called = true })

	// Assert: the error names the sweep's package and is recorded with the lock.
	if ran || called || err == nil || !strings.HasPrefix(err.Error(), "testsweep: ") {
		t.Fatalf("Do = (%v, %v), called %v, want a refusal prefixed with the package", ran, err, called)
	}
	recs := log.Records()
	if len(recs) != 1 || recs[0].Level != dlog.LevelError || recs[0].Context["lock"] != filepath.Join(notADir, "sweep.lock") || recs[0].Context["cause"] != err.Error() {
		t.Fatalf("records %+v, want one ERROR naming the lock and the cause", recs)
	}
}

// TestNoSweepHandRollsItsOwnGuard holds every daemon source to this guard: a
// file that both TryLocks a mutex and takes a record lock is a sweep guarding
// itself by hand, which drifts from the records and refusals here.
func TestNoSweepHandRollsItsOwnGuard(t *testing.T) {
	// Arrange: a test runs in its package's directory.
	here, err := os.Getwd()
	if err != nil {
		t.Fatalf("the working directory, which is this package's: %v", err)
	}
	internal := filepath.Join(here, "..")
	var offenders []string

	// Act
	err = filepath.WalkDir(internal, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Dir(path) == here {
			return nil
		}
		b, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(b), ".TryLock()") && strings.Contains(string(b), "recordlock.Hold(") {
			rel, _ := filepath.Rel(internal, path)
			offenders = append(offenders, rel)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these sources guard a sweep by hand; use sweepguard.Guard:\n%s", strings.Join(offenders, "\n"))
	}
}

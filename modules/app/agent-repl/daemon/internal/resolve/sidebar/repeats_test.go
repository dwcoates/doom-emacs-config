package sidebar

import (
	"testing"

	"agent-repld/internal/dlog"
)

// renderOnce logs one row decision for each named workspace through a fresh
// filter against last, as one render does, and flushes it.
func renderOnce(log dlog.Logger, last map[string]string, arms map[string]string) {
	f := newRepeatFilter(log, last)
	for ws, arm := range arms {
		f.With(dlog.Context{"workspace_id": ws}).Debug("daemon.sidebar.row", "the roster resolved a row", dlog.Context{"status": arm})
	}
	f.flush()
}

// countOf counts the captured records of one operation.
func countOf(log *dlog.TestLogger, operation string) int {
	n := 0
	for _, r := range log.Records() {
		if r.Operation == operation {
			n++
		}
	}
	return n
}

func TestARenderWhoseRowsDidNotChangeLogsNoneOfThemAgain(t *testing.T) {
	// Arrange.
	log, last := dlog.NewTestLogger(), map[string]string{}
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Act.
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Assert.
	if got := countOf(log, "daemon.sidebar.row"); got != 2 {
		t.Fatalf("row records = %d, want 2: the second render changed nothing", got)
	}
}

func TestARenderThatSkippedRowsSaysHowMany(t *testing.T) {
	// Arrange.
	log, last := dlog.NewTestLogger(), map[string]string{}
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Act.
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Assert.
	for _, r := range log.Records() {
		if r.Operation == "daemon.sidebar.unchanged_rows" {
			if r.Context["unchanged"] != 2 {
				t.Fatalf("unchanged = %v, want 2", r.Context["unchanged"])
			}
			return
		}
	}
	t.Fatal("no daemon.sidebar.unchanged_rows record for a render that skipped rows")
}

func TestARowWhoseDecisionChangedIsLoggedAgain(t *testing.T) {
	// Arrange.
	log, last := dlog.NewTestLogger(), map[string]string{}
	renderOnce(log, last, map[string]string{"w1": "ready"})

	// Act.
	renderOnce(log, last, map[string]string{"w1": "thinking"})

	// Assert.
	if got := countOf(log, "daemon.sidebar.row"); got != 2 {
		t.Fatalf("row records = %d, want 2: the row's decision changed", got)
	}
}

func TestARecordNamingNoWorkspacePassesStraightThrough(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	f := newRepeatFilter(log, map[string]string{})

	// Act: logged, and read before the render's end.
	f.Debug("daemon.sidebar.section", "a section drew", nil)
	got := countOf(log, "daemon.sidebar.section")
	f.flush()

	// Assert.
	if got != 1 {
		t.Fatalf("section records before the flush = %d, want 1", got)
	}
}

func TestARecordAboveDebugIsNeverHeldBack(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	f := newRepeatFilter(log, map[string]string{})

	// Act.
	f.With(dlog.Context{"workspace_id": "w1"}).Info("daemon.sidebar.result_restored", "restored", nil)
	got := countOf(log, "daemon.sidebar.result_restored")
	f.flush()

	// Assert.
	if got != 1 {
		t.Fatalf("info records before the flush = %d, want 1", got)
	}
}

func TestARowThatWasNotDrawnLogsItsDecisionsWhenItComesBack(t *testing.T) {
	// Arrange.
	log, last := dlog.NewTestLogger(), map[string]string{}
	renderOnce(log, last, map[string]string{"w1": "ready"})
	renderOnce(log, last, map[string]string{"w2": "done"})

	// Act.
	renderOnce(log, last, map[string]string{"w1": "ready"})

	// Assert.
	if got := countOf(log, "daemon.sidebar.row"); got != 3 {
		t.Fatalf("row records = %d, want 3: w1 left the roster and came back", got)
	}
}

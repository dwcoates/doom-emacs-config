package sidebar

import (
	"testing"

	"agent-repld/internal/dlog"
)

// THE GATHERING ITSELF (which rows are held, skipped and logged again) is
// dlog.RepeatFilter's, tested in dlog/repeats_test.go; what is the roster's
// own is the record of what a render skipped.

// renderOnce logs one row decision for each named workspace through a fresh
// filter against last, as one render does, and flushes it.
func renderOnce(log dlog.Logger, last map[string]string, arms map[string]string) {
	f := newRepeatFilter(log, last)
	for ws, arm := range arms {
		f.With(dlog.Context{"workspace_id": ws}).Debug("daemon.sidebar.row", "the roster resolved a row", dlog.Context{"status": arm})
	}
	f.flush()
}

func TestARenderThatSkippedRowsSaysHowMany(t *testing.T) {
	// Arrange.
	log, last := dlog.NewTestLogger(), map[string]string{}
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Act.
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Assert.
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.sidebar.unchanged_rows"})
	if !ok || rec.Context["unchanged"] != 2 || rec.Context["drawn"] != 2 {
		t.Fatalf("record = %+v, %v; want one counting 2 unchanged of 2 drawn", rec, ok)
	}
}

func TestARenderThatSkippedNothingSaysNothing(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	renderOnce(log, map[string]string{}, map[string]string{"w1": "ready"})

	// Assert.
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.sidebar.unchanged_rows"}); ok {
		t.Fatal("a render that skipped no row recorded skipped rows")
	}
}

package dlog

import (
	"testing"

	"agentrepl/logging/logsite"
)

// renderOnce logs one row decision for each named workspace through a fresh
// filter against last, as one render does, and flushes it, answering what
// the flush counted.
func renderOnce(log Logger, last map[string]string, arms map[string]string) (skipped, drawn int) {
	f := NewRepeatFilter(log, last)
	for ws, arm := range arms {
		f.With(Context{KeyWorkspaceID: ws}).Debug("daemon.sidebar.row", "the roster resolved a row", Context{"status": arm})
	}
	return f.Flush()
}

// countOf counts the captured records of one operation.
func countOf(log *TestLogger, operation string) int {
	return len(FilterRecords(log.Records(), RecordQuery{Operation: operation}))
}

func TestARenderWhoseRowsDidNotChangeLogsNoneOfThemAgain(t *testing.T) {
	// Arrange.
	log, last := NewTestLogger(), map[string]string{}
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Act.
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Assert.
	if got := countOf(log, "daemon.sidebar.row"); got != 2 {
		t.Fatalf("row records = %d, want 2: the second render changed nothing", got)
	}
}

func TestAFlushCountsTheUnchangedAndTheDrawnGroups(t *testing.T) {
	// Arrange.
	log, last := NewTestLogger(), map[string]string{}
	renderOnce(log, last, map[string]string{"w1": "ready", "w2": "done"})

	// Act.
	skipped, drawn := renderOnce(log, last, map[string]string{"w1": "ready", "w2": "thinking"})

	// Assert.
	if skipped != 1 || drawn != 2 {
		t.Fatalf("skipped, drawn = %d, %d; want 1, 2", skipped, drawn)
	}
}

func TestARowWhoseDecisionChangedIsLoggedAgain(t *testing.T) {
	// Arrange.
	log, last := NewTestLogger(), map[string]string{}
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
	log := NewTestLogger()
	f := NewRepeatFilter(log, map[string]string{})

	// Act: logged, and read before the render's end.
	f.Debug("daemon.sidebar.section", "a section drew", nil)
	got := countOf(log, "daemon.sidebar.section")
	f.Flush()

	// Assert.
	if got != 1 {
		t.Fatalf("section records before the flush = %d, want 1", got)
	}
}

func TestARecordAboveDebugIsNeverHeldBack(t *testing.T) {
	tests := []struct {
		name string
		emit func(Logger)
	}{
		{name: "a direct info", emit: func(l Logger) { l.Info("daemon.sidebar.result_restored", "restored", nil) }},
		{name: "a forwarded warn", emit: func(l Logger) {
			l.Log(LevelWarn, "daemon.sidebar.result_restored", logsite.Msg("restored, with a warning"), nil)
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := NewTestLogger()
			f := NewRepeatFilter(log, map[string]string{})

			// Act.
			tc.emit(f.With(Context{KeyWorkspaceID: "w1"}))
			got := countOf(log, "daemon.sidebar.result_restored")
			f.Flush()

			// Assert.
			if got != 1 {
				t.Fatalf("records before the flush = %d, want 1", got)
			}
		})
	}
}

func TestAForwardedDebugRecordIsHeldUntilTheFlush(t *testing.T) {
	// Arrange.
	log := NewTestLogger()
	f := NewRepeatFilter(log, map[string]string{})

	// Act.
	f.With(Context{KeyWorkspaceID: "w1"}).Log(LevelDebug, "daemon.sidebar.row", logsite.Msg("a forwarded row decision"), nil)
	before := countOf(log, "daemon.sidebar.row")
	f.Flush()

	// Assert.
	if before != 0 || countOf(log, "daemon.sidebar.row") != 1 {
		t.Fatalf("records before, after the flush = %d, %d; want 0, 1", before, countOf(log, "daemon.sidebar.row"))
	}
}

func TestAHeldRecordIsReplayedKeyedByTheCallThatWroteIt(t *testing.T) {
	// Arrange.
	log := NewTestLogger()
	f := NewRepeatFilter(log, map[string]string{})
	f.With(Context{KeyWorkspaceID: "w1"}).Log(LevelDebug, "daemon.sidebar.row", logsite.Msg("held for the flush"), nil)

	// Act.
	f.Flush()

	// Assert.
	want := logsite.Key(RuntimeDaemon, "agent-repld/internal/dlog.TestAHeldRecordIsReplayedKeyedByTheCallThatWroteIt", "held for the flush")
	if rec, ok := FindRecord(log.Records(), RecordQuery{Operation: "daemon.sidebar.row"}); !ok || rec.Site != want {
		t.Fatalf("record = %+v, %v; want it keyed %s, not by the flush", rec, ok, want)
	}
}

func TestARowThatWasNotDrawnLogsItsDecisionsWhenItComesBack(t *testing.T) {
	// Arrange.
	log, last := NewTestLogger(), map[string]string{}
	renderOnce(log, last, map[string]string{"w1": "ready"})
	renderOnce(log, last, map[string]string{"w2": "done"})

	// Act.
	renderOnce(log, last, map[string]string{"w1": "ready"})

	// Assert.
	if got := countOf(log, "daemon.sidebar.row"); got != 3 {
		t.Fatalf("row records = %d, want 3: w1 left the roster and came back", got)
	}
}

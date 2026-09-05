//go:build perf

// perf_decline_test.go — the DECLINED path of an area whose samples come from
// a child process, per PERF-SPEC.md §D2.
//
// WHY THIS FILE EXISTS. §D2's "a DECLINED assertion reports its samples and
// asserts nothing" is implemented at the END of `PerfRecorder.Assert` and
// `PerfShipped.Assert`, which is the right place for a measurement already in
// hand. An area whose samples are produced by a vitest child has no samples in
// hand at that point: it has a child under a wall-clock bound. A run of
// `TestPerfWebappLayer` on a saturated box declined at calibration, started the
// child anyway, spent the whole `WebappLayerPerfTimeout` and FAILED on the
// bound — a red produced by a phase that had already decided it would assert
// nothing.
//
// `perfDeclineArea` is what ends such an area before its child is started, and
// this file holds it to the two things §D2 requires of a decline: it must not
// pass and must not fail (so: SKIP), and it must be loud (so: a DECLINED
// summary row for every assertion the area would have made).
//
// The harness's own guard is exercised directly rather than through a world:
// what is under test is the decision, and building a daemon to reach it would
// measure something else entirely.
package e2e

import (
	"strings"
	"testing"
	"time"
)

// perfWithDeclinedCalibration installs a DECLINED calibration for the duration
// of one test and restores whatever was there before.
//
// The state is package-level because the guard is per-assertion and the phase
// is serial (§D4); a test that changed it must put it back, or the assertion
// that runs next inherits a verdict it never measured.
func perfWithDeclinedCalibration(t *testing.T) {
	t.Helper()
	perfCalibrationMu.Lock()
	before := perfCalibrationState
	beforeHad := perfPhaseHadDecline
	perfCalibrationState = perfCalibration{
		Loopback: 4 * time.Millisecond,
		CPU:      PerfCalibrationCPUBaseline,
		Declined: true,
		Reason:   "a test installed this verdict",
	}
	perfPhaseHadDecline = true
	perfCalibrationMu.Unlock()

	t.Cleanup(func() {
		perfCalibrationMu.Lock()
		perfCalibrationState = before
		perfPhaseHadDecline = beforeHad
		perfCalibrationMu.Unlock()
	})
}

// perfSummaryRowsSince answers the rows recorded after the given watermark, and
// the watermark to pass is the length `perfSummaryLen` answered beforehand.
func perfSummaryRowsSince(mark int) []perfSummaryRow {
	perfSummaryMu.Lock()
	defer perfSummaryMu.Unlock()
	return append([]perfSummaryRow(nil), perfSummaryRows[mark:]...)
}

func perfSummaryLen() int {
	perfSummaryMu.Lock()
	defer perfSummaryMu.Unlock()
	return len(perfSummaryRows)
}

// TestPerfDeclineAreaSkipsBeforeTheChildStarts is the whole point of the
// helper: a declined area must END, not proceed to the work whose bound it
// would then miss.
//
// The statement under test is the CONTROL FLOW — that nothing after the call
// runs — which is exactly what a wall-bound child would have been. A subtest is
// used because `t.Skipf` unwinds its own goroutine, so the flag can only be
// read from outside it.
func TestPerfDeclineAreaSkipsBeforeTheChildStarts(t *testing.T) {
	// Arrange
	perfWithDeclinedCalibration(t)
	reachedTheWorkAfterTheDecline := false

	// Act
	skipped := false
	t.Run("declined-area", func(t *testing.T) {
		defer func() { skipped = t.Skipped() }()
		perfDeclineArea(t, "perf-example-assertion")
		// A child would be started HERE. Nothing below a decline may run.
		reachedTheWorkAfterTheDecline = true
	})

	// Assert
	if reachedTheWorkAfterTheDecline {
		t.Fatalf("a DECLINED area ran the work after the decline; it must end before its child is started")
	}
	if !skipped {
		t.Fatalf("a DECLINED area was not skipped; a decline is neither a pass nor a failure")
	}
}

// TestPerfDeclineAreaRecordsOneDeclinedRowPerAssertion pins the LOUDNESS half
// of §D2: a green run that measured nothing must still say so in the phase
// summary, once per assertion the area owns.
func TestPerfDeclineAreaRecordsOneDeclinedRowPerAssertion(t *testing.T) {
	// Arrange
	perfWithDeclinedCalibration(t)
	mark := perfSummaryLen()
	names := []string{"perf-row-one", "perf-row-two"}

	// Act
	t.Run("declined-area", func(t *testing.T) { perfDeclineArea(t, names...) })

	// Assert
	rows := perfSummaryRowsSince(mark)
	if len(rows) != len(names) {
		t.Fatalf("a DECLINED area recorded %d summary rows, want one per assertion (%d)", len(rows), len(names))
	}
	for i, row := range rows {
		if row.Name != names[i] {
			t.Errorf("summary row %d names %q, want the area's own assertion %q", i, row.Name, names[i])
		}
		if row.Verdict != perfVerdictDeclined {
			t.Errorf("summary row %q carries the verdict %q, want %q", row.Name, row.Verdict, perfVerdictDeclined)
		}
	}
}

// TestPerfDeclineAreaNamesTheCalibrationInItsSkip pins the other half of
// "never a silent skip": the reason must carry the probe numbers, because a
// bare skip is the thing a reader's eye passes over.
func TestPerfDeclineAreaNamesTheCalibrationInItsSkip(t *testing.T) {
	// Arrange
	perfWithDeclinedCalibration(t)

	// Act
	summary := perfCalibrationSummary()

	// Assert
	if !strings.Contains(summary, "DECLINED") {
		t.Fatalf("the calibration summary a declined skip carries is %q, want it to name the decline", summary)
	}
	if !strings.Contains(summary, "a test installed this verdict") {
		t.Fatalf("the calibration summary is %q, want it to carry the reason that tripped the guard", summary)
	}
}

// TestPerfWebappAreaNamesEveryShippedAssertion keeps the decline's summary
// honest against the assertions the area actually makes: the two readers of
// `wlPerfRows` must not drift, or a declined area reports on rows nobody
// measures and measures rows nobody declared.
func TestPerfWebappAreaNamesEveryShippedAssertion(t *testing.T) {
	// Arrange
	rows := wlPerfRows

	// Act
	names := wlPerfAssertions()

	// Assert
	if len(names) != len(rows) {
		t.Fatalf("the area declares %d assertions but names %d to a decline", len(rows), len(names))
	}
	for i, row := range rows {
		if names[i] != row.name {
			t.Errorf("assertion %d is named %q to a decline, want %q", i, names[i], row.name)
		}
	}
}

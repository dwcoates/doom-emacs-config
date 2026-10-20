package procmetrics

import (
	"testing"

	"agent-repld/internal/dlog"
)

func TestTheMeasuredEmacsStandsWhileItsStreamDoes(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	e := NewEmacsProcess(log)

	// Act
	release := e.Attach(4242)
	during, standing := e.PID()
	release()
	_, after := e.PID()

	// Assert
	if !standing || during != 4242 || after {
		t.Fatalf("during = %d (%v), after = %v; want 4242 while the stream stands and nothing after", during, standing, after)
	}
	attached, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: Operation,
		Message: "an Emacs stream stated its process; it is measured while the stream stands"})
	if !ok || attached.Context["pid"] != 4242 {
		t.Fatalf("records = %+v, want the attach recorded with its pid", log.Records())
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: Operation,
		Message: "the measured Emacs's stream ended; no Emacs is measured"}); !ok {
		t.Fatalf("records = %+v, want the release recorded", log.Records())
	}
}

func TestASupersededEmacsStreamEndingKeepsItsSuccessorMeasured(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	e := NewEmacsProcess(log)
	releaseOld := e.Attach(1)
	e.Attach(2)

	// Act
	releaseOld()

	// Assert
	if pid, ok := e.PID(); !ok || pid != 2 {
		t.Fatalf("PID() = %d, %v; want the successor's 2", pid, ok)
	}
}

func TestNewEmacsProcessRequiresALogger(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("NewEmacsProcess(nil) did not panic")
		}
	}()
	NewEmacsProcess(nil)
}

func TestTheEmacsGroupLeavesWithItsStream(t *testing.T) {
	// Arrange
	h := newHarness(t, newWorld())
	h.scrape(t)

	// Act
	h.release()
	get := h.scrape(t)

	// Assert: the group keeps its counters, and its gauges read nothing live.
	mustValue(t, get, 0, "agentrepl_process_resident_bytes", "process", ProcessEmacs)
	mustValue(t, get, 0, "agentrepl_process_count", "process", ProcessEmacs)
}

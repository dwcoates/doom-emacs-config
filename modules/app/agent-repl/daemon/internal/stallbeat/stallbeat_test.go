package stallbeat

import (
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// base is a moment on phase 0 of the cycle.
var base = time.Unix(1_800_000_000, 0)

// onBeat answers N moments one Period apart from START, each shifted by OFF.
func onBeat(start time.Time, n int, off time.Duration) []time.Time {
	out := make([]time.Time, n)
	for i := range out {
		out[i] = start.Add(time.Duration(i)*Period + off)
	}
	return out
}

func newDetector(t *testing.T) (*Detector, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	d, err := New(Deps{Log: log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return d, log
}

func raisedErrors(log *dlog.TestLogger) []dlog.Record {
	return dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error"})
}

func TestObserveRaisesOnlyAtTheBeatThreshold(t *testing.T) {
	tests := []struct {
		name   string
		stalls []time.Time
		raised int
	}{
		{name: "raises when Beat stalls fall on one phase", stalls: onBeat(base, Beat, 0), raised: 1},
		{name: "stays quiet one stall short of Beat", stalls: onBeat(base, Beat-1, 0), raised: 0},
		{
			name: "stays quiet for stalls spread across the cycle",
			stalls: func() []time.Time {
				out := make([]time.Time, 20)
				for i := range out {
					out[i] = base.Add(time.Duration(i) * 29 * time.Second)
				}
				return out
			}(),
			raised: 0,
		},
		{name: "counts a stall just inside Tolerance", stalls: append(onBeat(base, Beat-1, 0), base.Add(time.Duration(Beat)*Period+Tolerance-time.Millisecond)), raised: 1},
		{name: "does not count a stall just outside Tolerance", stalls: append(onBeat(base, Beat-1, 0), base.Add(time.Duration(Beat)*Period+Tolerance+time.Millisecond)), raised: 0},
		{name: "counts stalls either side of the cycle's wrap as one phase", stalls: append(onBeat(base, Beat/2, 100*time.Millisecond), onBeat(base.Add(time.Duration(Beat/2)*Period), Beat/2, -100*time.Millisecond)...), raised: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, log := newDetector(t)

			// Act
			for _, at := range tt.stalls {
				d.Observe(at, 300)
			}

			// Assert
			if got := len(raisedErrors(log)); got != tt.raised {
				t.Fatalf("raised %d errors, want %d: %+v", got, tt.raised, log.Records())
			}
		})
	}
}

func TestObserveForgetsStallsOlderThanTheWindow(t *testing.T) {
	// Arrange: Beat-1 stalls, then one on the same phase long after the window.
	d, log := newDetector(t)
	for _, at := range onBeat(base, Beat-1, 0) {
		d.Observe(at, 300)
	}

	// Act
	d.Observe(base.Add(Window+time.Duration(Beat)*Period), 300)

	// Assert
	if got := len(raisedErrors(log)); got != 0 {
		t.Fatalf("raised %d errors for stalls a window apart, want 0", got)
	}
}

func TestObserveRaisesOncePerWindowWhileThePatternHolds(t *testing.T) {
	// Arrange
	d, log := newDetector(t)

	// Act: a beat on every cycle for one window and a half.
	for _, at := range onBeat(base, int((Window+Window/2)/Period), 0) {
		d.Observe(at, 300)
	}

	// Assert
	if got := len(raisedErrors(log)); got != 2 {
		t.Fatalf("raised %d errors over a window and a half of beats, want 2", got)
	}
}

func TestObserveRecordsTheBeatsDiagnosis(t *testing.T) {
	// Arrange
	d, log := newDetector(t)

	// Act
	for _, at := range onBeat(base, Beat, 2*time.Second) {
		d.Observe(at, 250)
	}

	// Assert
	raised := raisedErrors(log)
	if len(raised) != 1 {
		t.Fatalf("raised %d errors, want 1", len(raised))
	}
	got := raised[0]
	want := dlog.Context{
		"stalls_on_beat": Beat, "stalls_in_window": Beat, "blocked_ms_on_beat": int64(250 * Beat),
		"phase_ms": int64(2000), "window_s": int(Window.Seconds()), "period_s": int(Period.Seconds()),
	}
	if got.Operation != op {
		t.Fatalf("operation = %q, want %q", got.Operation, op)
	}
	for k, v := range want {
		if got.Context[k] != v {
			t.Fatalf("context[%q] = %#v, want %#v (whole: %+v)", k, got.Context[k], v, got.Context)
		}
	}
}

func TestNewRequiresALogger(t *testing.T) {
	// Act
	_, err := New(Deps{})

	// Assert
	if err == nil {
		t.Fatal("New with no logger succeeded, want an error")
	}
}

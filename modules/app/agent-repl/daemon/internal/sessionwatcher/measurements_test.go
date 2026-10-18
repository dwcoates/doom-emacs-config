package sessionwatcher

import (
	"testing"

	shimv1 "agentrepl/proto/shim/v1"
)

func measurementsFrame(sent uint64) *shimv1.ShimMeasurements {
	return &shimv1.ShimMeasurements{
		CountedSinceUnixMs: 1_700_000_000_000,
		VendorTraffic:      &shimv1.ShimVendorTraffic{BytesSent: sent},
	}
}

// TestAMeasurementsFrameReachesTheMetricsSinkAndTheStreamGoesOn pins the
// session stream's measurements arm: the frame is handed to the metrics sink
// for this workspace, and the frames after it are routed as before.
func TestAMeasurementsFrameReachesTheMetricsSinkAndTheStreamGoesOn(t *testing.T) {
	// Arrange
	h := newHarnessAttachingPurely(t)

	// Act
	h.sendMeasurements(t, measurementsFrame(512))
	h.sendSessionUpdate(t, compactingUpdate())

	// Assert
	folded := <-h.measurements.folded
	if folded.ws != "ws-1" || folded.m.GetVendorTraffic().GetBytesSent() != 512 {
		t.Fatalf("folded = %+v, want ws-1's 512 bytes", folded)
	}
	h.rec.until(t, "footer.OnSessionUpdate")
	if r, ok := firstError(h); ok {
		t.Fatalf("a measurements frame logged an error: %+v", r)
	}
}

// TestAMeasurementsFrameWithNoSinkIsDropped pins the optional sink: a watcher
// wired without one drops the frame and routes the next frame as before.
func TestAMeasurementsFrameWithNoSinkIsDropped(t *testing.T) {
	// Arrange: the sink is cleared before any frame is sent; the frame's own
	// channel hand-off orders this write before the watcher's read.
	h := newHarnessAttachingPurely(t)
	h.w.sinks.Measurements = nil

	// Act
	h.sendMeasurements(t, measurementsFrame(512))
	h.sendSessionUpdate(t, compactingUpdate())

	// Assert
	h.rec.until(t, "footer.OnSessionUpdate")
	if len(h.measurements.folded) != 0 {
		t.Fatalf("a cleared sink was still handed the frame")
	}
	if r, ok := firstError(h); ok {
		t.Fatalf("a dropped measurements frame logged an error: %+v", r)
	}
}

// firstError answers the harness's first ERROR record, if any.
func firstError(h *harness) (any, bool) {
	for _, r := range h.log.Records() {
		if r.Level == "error" {
			return r, true
		}
	}
	return nil, false
}

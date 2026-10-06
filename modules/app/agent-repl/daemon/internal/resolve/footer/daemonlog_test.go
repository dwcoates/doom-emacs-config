package footer

import (
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// feedbackBound is how long the footer's own teed record may take to return.
// MEASURED: the call's worst case was 58-162µs across 1500 runs alone; the
// bound leaves room for the CPU contention of the whole unit suite running
// packages in parallel, and a real feedback never returns at all.
const feedbackBound = 250 * time.Millisecond

// teed binds the harness's resolver as its log surfaces' record tee, the way
// the daemon binds it at boot, with the workspace's directory minting the
// workspace's own id as the daemon's roster does — the footer's own logger
// included, so its records reach the tee under the workspace they are about —
// and answers a workspace logger of some other component.
func teed(t *testing.T, h *harness) dlog.Logger {
	t.Helper()
	dir := h.r.states[testWS].dir
	h.log.BindWorkspaceIDs(func(string) (string, error) { return string(testWS), nil })
	if err := h.r.SetWorkspaceDir(testWS, dir); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	h.log.BindRecordTee(h.r)
	log, err := h.log.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	return log
}

func TestAWorkspaceWarningReachesTheStrip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	log := teed(t, h)

	// Act
	log.Warn("daemon.promptqueue.deliver", "the held prompt was re-queued", nil)

	// Assert
	warning := transientOf(t, h).GetDaemonWarning()
	if warning.GetOperation() != "daemon.promptqueue.deliver" || warning.GetMessage() != "the held prompt was re-queued" {
		t.Fatalf("daemon warning = %+v, want the record's operation and message", warning)
	}
}

func TestAWorkspaceErrorReachesTheStrip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	log := teed(t, h)

	// Act
	log.Error("daemon.feed.activity_undrawable", "an activity could not be resolved into a row", nil)

	// Assert
	if got := transientOf(t, h).GetDaemonError().GetOperation(); got != "daemon.feed.activity_undrawable" {
		t.Fatalf("daemon error operation = %q, want the record's", got)
	}
}

func TestAFooterRecordNeverFeedsBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	teed(t, h)

	// Act: the footer itself records an ERROR (a submission naming no stage).
	// The record is written under the resolver's own lock, so a feedback would
	// DEADLOCK rather than draw a line: the call runs on its own goroutine and
	// the test fails at a bound instead of hanging.
	done := make(chan struct{})
	go func() {
		defer close(done)
		h.r.OnSubmission(testWS, Submission{Stage: SubmissionStage(255)})
	}()
	select {
	case <-done:
	case <-time.After(feedbackBound):
		t.Fatalf("OnSubmission did not return within %s: the footer's own ERROR fed back through the record tee and deadlocked on the resolver's lock", feedbackBound)
	}

	// Assert
	if !hasLevel(h.log.Records(), "error", "daemon.footer.on_submission") {
		t.Fatalf("the footer's own warning was not recorded; the test proves nothing")
	}
	if got := transientOf(t, h); got != nil {
		t.Fatalf("transient = %+v, want none: the footer's own record fed back into it", got)
	}
}

func TestARecordForAnUnboundWorkspaceIsNotDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	other := ids.WorkspaceID("ws-never-bound")

	// Act
	h.r.OnWorkspaceRecord(dlog.WorkspaceRecord{
		WorkspaceID: string(other), Level: dlog.LevelWarn, Operation: "daemon.x.y", Message: "m"})

	// Assert
	if _, ok := h.r.Topic(other).Latest(); ok {
		t.Fatalf("a view was published for a workspace the footer never bound")
	}
	if !hasLevel(h.log.Records(), "debug", "daemon.footer.daemon_record_unbound") {
		t.Fatalf("no DEBUG daemon.footer.daemon_record_unbound record notes the skipped record")
	}
}

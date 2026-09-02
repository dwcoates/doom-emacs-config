package integration

import (
	"testing"
)

// SUBJECT — CLOSING an outage, as its own record.
//
// The suspension subjects in store_unreachable_test.go prove the outage is
// OPENED once. Closing it is the other half of the same contract: recovery is
// stated exactly once, at info, and never as a warning or an error — a resumed
// producer is not a problem, and a reader tallying the log's warnings must not
// see the recovery among them.

// TestRecoveryClosesTheOutageWithExactlyOneInfoRecord asserts that late-binding
// the store yields exactly one production-resumed record, at info and at no
// other level.
func TestRecoveryClosesTheOutageWithExactlyOneInfoRecord(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	socket := shortSocketPath(t, "close")
	opts := defaultSidecarOptions(t, socket, tree)

	// Act: the sidecar starts against a socket nobody is listening on, the
	// outage is opened, and only then does the store appear.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitLog(ctx, t, opts.LogPath, "the suspension record", func(r logRecord) bool {
		return r.Operation == "production-suspended" && r.Level == "warn"
	})
	fake := startFakeStoreAt(t, socket)
	// The whole file becoming durable is the signal that production has resumed
	// and settled; the recovery record is written before it.
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the closing record is stated once, at info, and nowhere else.
	records := readLog(t, opts.LogPath)
	if got := recordsAt(records, "production-resumed", "info"); len(got) != 1 {
		t.Errorf("an outage is closed ONCE at info; the log carries %d production-resumed info records: %v",
			len(got), got)
	}
	for _, level := range []string{"warn", "error", "debug", "verbose"} {
		if got := recordsAt(records, "production-resumed", level); len(got) != 0 {
			t.Errorf("production-resumed was recorded %d time(s) at %q; recovery is an info-level fact only: %v",
				len(got), level, got)
		}
	}
}

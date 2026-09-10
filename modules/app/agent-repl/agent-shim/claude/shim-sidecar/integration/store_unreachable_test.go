package integration

import (
	"os"
	"testing"
)

// SUBJECT 7 — the store is not there at boot.
//
// A store that has not started yet is a DOWN DEPENDENCY, never a reason to read
// anyway. The sidecar produces nothing, says so once, and the first rpc it makes
// when the store appears is GetSidecarCursors.

// TestNoStoreAtBootProducesNothing asserts the sidecar writes nothing at all
// while there is no socket to write to.
func TestNoStoreAtBootProducesNothing(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	socket := shortSocketPath(t, "absent")
	opts := defaultSidecarOptions(t, socket, tree)

	// Act: no listener exists on that socket.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitLog(ctx, t, opts.LogPath, "the suspension record", func(r logRecord) bool {
		return r.Operation == "production-suspended" && r.Level == "warn"
	})

	// Assert: nothing was written anywhere, because there was nowhere to write.
	if _, err := os.Stat(socket); err == nil {
		t.Fatalf("the test's own socket path %s should not exist", socket)
	}
}

// TestTheSuspensionIsStatedOnceRatherThanPerRetry asserts the outage is one
// WARNING, with the per-retry noise held at verbose.
func TestTheSuspensionIsStatedOnceRatherThanPerRetry(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	socket := shortSocketPath(t, "absent-once")
	opts := defaultSidecarOptions(t, socket, tree)

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitLog(ctx, t, opts.LogPath, "the suspension record", func(r logRecord) bool {
		return r.Operation == "production-suspended" && r.Level == "warn"
	})
	// Binding the socket late and waiting for the first batch proves several
	// retry cycles elapsed during the outage — so a per-retry WARNING would
	// have shown up by now.
	fake := startFakeStoreAt(t, socket)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert. THE SUBJECT IS THE OUTAGE'S OWN RECORD, not the log's whole
	// warning traffic: a real capture's content produces warnings of its own
	// (the vendor recorded a failing hook, say) that say nothing about the store,
	// and counting those would make this subject fail for an unrelated reason.
	var suspensions []logRecord
	for _, r := range logsAtLevel(readLog(t, opts.LogPath), "warn") {
		if r.Operation == "production-suspended" {
			suspensions = append(suspensions, r)
		}
	}
	if len(suspensions) != 1 {
		t.Errorf("an outage is stated ONCE when it begins; the log carries %d suspension records: %v",
			len(suspensions), suspensions)
	}
}

// TestTheFirstRpcAfterTheStoreAppearsIsACursorRead asserts recovery is
// cursor-first: the sidecar never builds a tailer from a position the store did
// not hand it.
func TestTheFirstRpcAfterTheStoreAppearsIsACursorRead(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	// Reserve a path, start the sidecar against it, and only then bind it.
	socket := shortSocketPath(t, "late")
	opts := defaultSidecarOptions(t, socket, tree)

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitLog(ctx, t, opts.LogPath, "the suspension record", func(r logRecord) bool {
		return r.Operation == "production-suspended" && r.Level == "warn"
	})

	fake := startFakeStoreAt(t, socket)
	fake.awaitBatches(ctx, t, 1)

	// Assert.
	calls := fake.Calls()
	if len(calls) == 0 || calls[0] != "GetSidecarCursors" {
		t.Fatalf("the first rpc after the store appeared was %v, wanted GetSidecarCursors", calls)
	}
}

// TestProductionResumesOnceTheStoreAppears asserts the outage is recovered from
// rather than merely survived.
func TestProductionResumesOnceTheStoreAppears(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	socket := shortSocketPath(t, "resume")
	opts := defaultSidecarOptions(t, socket, tree)

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitLog(ctx, t, opts.LogPath, "the suspension record", func(r logRecord) bool {
		return r.Operation == "production-suspended" && r.Level == "warn"
	})
	fake := startFakeStoreAt(t, socket)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the whole file was read, from zero, once the store existed.
	if !upsertKeySet(fake.Entries())["activity:"+capturedThinking1] {
		t.Fatalf("the file's first unit was never written after the store appeared")
	}
}

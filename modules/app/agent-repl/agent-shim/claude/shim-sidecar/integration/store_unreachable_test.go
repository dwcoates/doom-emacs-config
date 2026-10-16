package integration

import (
	"testing"
)

// SUBJECT 7 — the store is not there at boot.
//
// A store that has not started yet is a DOWN DEPENDENCY, never a reason to read
// anyway. The sidecar produces nothing, says so once, and the first rpc it makes
// when the store appears is GetSidecarCursors.

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

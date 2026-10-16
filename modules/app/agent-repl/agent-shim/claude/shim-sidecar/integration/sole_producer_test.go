package integration

import (
	"strings"
	"testing"
)

// SUBJECT — the facts the SIDECAR IS THE ONLY PRODUCER OF.
//
// The pinned SDK stream carries no attachment records at all, so these facts
// exist on the file plane or nowhere: AgentContextInjected (the memory files and
// skills the vendor silently pulled in) and the write/edit `diagnostics`
// consequence. If the sidecar stops
// producing one, nothing else starts — and no error is raised anywhere, because
// a withheld attachment is an ordinary outcome. These subjects are what break
// that silence end to end, against the real binary and the real store.

// TestTheSidecarNeverAnnouncesDetachedWork asserts the sidecar produces no
// detached-work announcement over a whole real ingest.
//
// THE STREAM PLANE OWNS THAT ANNOUNCEMENT. The shim is first to know a call
// detached; the sidecar only ever sees the spool that appears afterwards, so a
// file-plane announcement would be a SECOND announcement of one detachment,
// keyed the same and racing the stream's.
func TestTheSidecarNeverAnnouncesDetachedWork(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/work/no-announcement-probe",
		"c2b2c2b2-c2b2-4c2b-8c2b-c2b2c2b2c2b2")

	// Act: the whole detached lifecycle, which is where an announcement would
	// most plausibly be minted.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("output\nEXIT=0\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	for _, e := range fake.Entries() {
		frame := e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame()
		if frame.GetDetachedWork() != nil {
			t.Errorf("entry %q announces detached work; the stream plane owns that announcement", e.GetUpsertKey())
		}
		if strings.HasPrefix(e.GetUpsertKey(), "detached:") {
			t.Errorf("entry %q uses the announcement's key space", e.GetUpsertKey())
		}
	}
}

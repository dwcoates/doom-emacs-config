package integration

import (
	"context"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
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

// TestInjectedContextReachesTheAgentsBook asserts the other two sole-producer
// facts arrive as page lines: the memory files and skills the vendor pulled in
// with no tool call of their own.
func TestInjectedContextReachesTheAgentsBook(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/injected-context-probe"
	slug := cwdSlug(cwd)
	session := "c1b1c1b1-c1b1-4c1b-8c1b-c1b1c1b1c1b1"
	memory := retargetSession(t,
		decodeRecord(t, corpusLine(t, "attachments/nested_memory.jsonl", 0)), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	for _, line := range captured.Lines[:8] {
		g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, line), session, cwd)))
	}
	g.AppendLine(encodeRecord(t, memory))

	// Assert.
	line := awaitBookLine(ctx, t, store.Client, session, func(line *storev1.StorePageLine) bool {
		return activityOf(line).GetContextInjected().GetMemory() != nil
	})
	if activityOf(line).GetContextInjected().GetMemory().GetPath() == "" {
		t.Errorf("the injected memory names no path: %v", activityOf(line).GetContextInjected())
	}
}

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

// TestABashRunsHandleIsItsSpawningCall asserts landing 4's identity equality on
// the wire: every row's run is the spawning call's AgentActivityId, so a vendor
// task id never becomes a handle a consumer would have to resolve.
func TestABashRunsHandleIsItsSpawningCall(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/work/handle-identity-probe",
		"c3b3c3b3-c3b3-4c3b-8c3b-c3b3c3b3c3b3")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("output\nEXIT=0\n"))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, spool.Offset())

	// Assert.
	var checked int
	for _, row := range bashFramesOf(fake.Entries()) {
		checked++
		if got := row.GetRun().GetValue(); got != fx.CallID {
			t.Errorf("a bash row names run %q, wanted the spawning call %q", got, fx.CallID)
		}
		if got := row.GetRun().GetValue(); got == fx.TaskID {
			t.Errorf("a bash row names the vendor task id %q as its handle", got)
		}
	}
	if checked == 0 {
		t.Fatal("no bash row was written, so the handle identity was never checked")
	}
}

// awaitBookLine re-reads one book until a line satisfies match.
func awaitBookLine(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string, match func(*storev1.StorePageLine) bool) *storev1.StorePageLine {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		held, _ := bookLinesIfKnown(ctx, t, c, agent)
		for _, at := range held {
			if match(at.GetLine()) {
				return at.GetLine()
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("book %s never held the line the subject waited for within the deadline", agent)
			return nil
		case <-tick.C:
		}
	}
}

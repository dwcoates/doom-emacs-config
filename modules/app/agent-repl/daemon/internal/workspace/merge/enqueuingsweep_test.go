package merge

import (
	"context"
	"strings"
	"testing"
)

// THE BOOT SWEEP of orphaned merge_enqueuing marks.
//
// merge_enqueuing is the one merge phase with nothing durable behind it: the
// command handler emits it before the geometry is resolved and before the queue
// write. A daemon that dies in that window has lost the attempt outright, so
// the next boot must fail it rather than leave a workspace pinned on a phase
// nothing will ever advance. A workspace whose durable entry DID survive is a
// different story entirely, and must be left alone for the drain to run.

func TestDrainFailsAnEnqueuingMarkWithNoDurableEntry(t *testing.T) {
	// Arrange — the log says a workspace is enqueuing; the queue has nothing.
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMergeEnqueuing: {"/ws/lost"}},
	}})

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert
	got := <-h.sink.ch
	if got.ws != "/ws/lost" || got.phase != PhaseMergeFailed {
		t.Fatalf("transition = %+v, want merge_failed for /ws/lost", got)
	}
}

func TestDrainExplainsWhyAnOrphanedEnqueuingMarkFailed(t *testing.T) {
	// Arrange
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMergeEnqueuing: {"/ws/lost"}},
	}})

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert — the cause is the only account the user ever gets of an attempt
	// that left no other trace.
	got := <-h.sink.ch
	if !strings.Contains(got.cause, "daemon restarted") {
		t.Fatalf("cause = %q, want the lost-enqueue explanation", got.cause)
	}
}

func TestDrainLeavesAnEnqueuingMarkWithADurableEntryAlone(t *testing.T) {
	// Arrange — the workspace is at merge_enqueuing AND its durable entry
	// survived the bounce, so its merge is about to be drained normally.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	h := newHarnessWith(t, harnessOpts{queue: next, dir: dir, phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMergeEnqueuing: {req.Workspace}},
	}})

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert — the merge is driven, and the drive is what proves nothing failed
	// it: the sweep would have had to record merge_failed BEFORE Drain returned,
	// so a merge reaching the picker with an empty sink is the retention.
	if got := <-h.picker.merges; !sameRequest(got, req) {
		t.Fatalf("driven merge = %+v, want %+v", got, req)
	}
	select {
	case tr := <-h.sink.ch:
		t.Fatalf("transition = %+v, want the durable-backed workspace left alone", tr)
	default:
	}
}

func TestDrainSurfacesAPhaseReadFailure(t *testing.T) {
	// Arrange — the pushed-state record cannot be read.
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{err: sentinelError("db locked")}})

	// Act
	err := h.coord.Drain(context.Background())

	// Assert — an unreadable record is not "nothing to sweep": any workspace
	// left at merge_enqueuing stays pinned, so the boot says so.
	if err == nil || !strings.Contains(err.Error(), "db locked") {
		t.Fatalf("Drain error = %v, want the read failure surfaced", err)
	}
}

func TestDrainSurfacesASweepRecordFailure(t *testing.T) {
	// Arrange — the sweep's own merge_failed cannot be recorded.
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMergeEnqueuing: {"/ws/lost"}},
	}})
	h.sink.err = sentinelError("sink down")

	// Act
	err := h.coord.Drain(context.Background())

	// Assert — a sweep that could not record its verdict is a boot that left a
	// workspace pinned, and it is never reported as a clean drain.
	if err == nil {
		t.Fatalf("Drain error = nil, want the record failure surfaced")
	}
	<-h.sink.ch
}

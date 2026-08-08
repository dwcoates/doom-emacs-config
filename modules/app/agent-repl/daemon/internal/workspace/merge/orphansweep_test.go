package merge

import (
	"context"
	"strings"
	"testing"
)

// THE BOOT SWEEP of orphaned NON-TERMINAL merge phases.
//
// A daemon killed mid-merge can leave a workspace resting on merge_queued,
// merging, merge_before_action or merge_conflict with nothing left alive to
// advance it: the run died with the process, the lease was dropped by the
// bounce, and the durable queue entry is gone. The axis then rests non-terminal
// forever, which refuses every later prompt and holds the workspace's teardown
// guard shut. The next boot is the only party that can notice.
//
// The sweep is conservative by construction: a durable queue entry, an open
// lease, or a live run each mean the merge is still accounted for.

func TestDrainFailsAnOrphanedNonTerminalMergePhase(t *testing.T) {
	tests := []struct {
		name  string
		phase Phase
	}{
		{name: "queued behind another merge", phase: PhaseMergeQueued},
		{name: "mid cherry-pick", phase: PhaseMerging},
		{name: "running its before-action", phase: PhaseMergeBeforeAction},
		{name: "parked on a conflict", phase: PhaseMergeConflict},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange — the log says the workspace rests there; the queue,
			// the lease table and the coordinator all have nothing for it.
			h := newHarnessWith(t, harnessOpts{phases: fakePhases{
				byPhase: map[Phase][]string{tt.phase: {"/ws/orphan"}},
			}})

			// Act
			if err := h.coord.Drain(context.Background()); err != nil {
				t.Fatalf("Drain: %v", err)
			}

			// Assert
			got := <-h.sink.ch
			if got.ws != "/ws/orphan" || got.phase != PhaseMergeFailed {
				t.Fatalf("transition = %+v, want merge_failed for /ws/orphan", got)
			}
		})
	}
}

func TestDrainExplainsWhyAnOrphanedMergeFailed(t *testing.T) {
	// Arrange
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMergeConflict: {"/ws/orphan"}},
	}})

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert — the cause is the only account the user ever gets of a merge that
	// stopped existing between two daemons, so it names the orphaning and the
	// phase it was orphaned at.
	got := <-h.sink.ch
	if !strings.Contains(got.cause, "orphaned_by_restart") || !strings.Contains(got.cause, string(PhaseMergeConflict)) {
		t.Fatalf("cause = %q, want the orphaned-by-restart explanation naming %s", got.cause, PhaseMergeConflict)
	}
}

func TestDrainPublishesAnOrphanSweepThroughTheStateFunnel(t *testing.T) {
	// Arrange
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMerging: {"/ws/orphan"}},
	}})

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert — the terminal word arrives on the StateSink every real transition
	// goes through, which is what makes the WorkspaceState push and the
	// merge-axis clear follow from it. A sweep that wrote the row some other way
	// would leave this sink empty.
	<-h.sink.ch
	h.sink.mu.Lock()
	defer h.sink.mu.Unlock()
	if len(h.sink.got) != 1 || h.sink.got[0].phase != PhaseMergeFailed {
		t.Fatalf("sink transitions = %+v, want exactly the swept merge_failed", h.sink.got)
	}
}

func TestDrainLeavesAnOrphanCandidateWithADurableEntryAlone(t *testing.T) {
	// Arrange — the workspace is at merging AND its durable entry survived the
	// bounce, so this very boot replays its merge.
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
		byPhase: map[Phase][]string{PhaseMerging: {req.Workspace}},
	}})

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert — the merge is driven, and the drive is what proves nothing failed
	// it: the sweep records synchronously before Drain returns, so a merge
	// reaching the picker with an empty sink is the retention.
	if got := <-h.picker.merges; !sameRequest(got, req) {
		t.Fatalf("driven merge = %+v, want %+v", got, req)
	}
	select {
	case tr := <-h.sink.ch:
		t.Fatalf("transition = %+v, want the durable-backed workspace left alone", tr)
	default:
	}
}

func TestDrainLeavesAnOrphanCandidateWithAnOpenLeaseAlone(t *testing.T) {
	// Arrange — the lease outlived the bounce. Reconstructing it is the drain's
	// job, and failing the merge here would preempt that.
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMergeConflict: {"/ws/leased"}},
	}})
	if _, err := h.lease.Acquire(context.Background(), "/ws/leased"); err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	<-h.lease.acquires

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert
	select {
	case tr := <-h.sink.ch:
		t.Fatalf("transition = %+v, want the leased workspace left alone", tr)
	default:
	}
}

func TestDrainLeavesAnOrphanCandidateWithALiveRunAlone(t *testing.T) {
	// Arrange — the coordinator is driving that workspace's merge right now,
	// which is the opposite of orphaned.
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMerging: {"/ws/live"}},
	}})
	h.coord.mu.Lock()
	h.coord.parks[testRepoKey] = &conflictPark{req: Request{Workspace: "/ws/live"}}
	h.coord.mu.Unlock()

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert
	select {
	case tr := <-h.sink.ch:
		t.Fatalf("transition = %+v, want the live run left alone", tr)
	default:
	}
}

func TestDrainNeverSweepsAWorkspaceRunningItsAfterAction(t *testing.T) {
	// Arrange — merge_after_action means every commit is already on the target.
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMergeAfterAction: {"/ws/landed"}},
	}})

	// Act
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert — failing it would deny a merge that demonstrably happened.
	select {
	case tr := <-h.sink.ch:
		t.Fatalf("transition = %+v, want no sweep of a merge whose commits landed", tr)
	default:
	}
}

func TestDrainSurfacesAnOrphanSweepPhaseReadFailure(t *testing.T) {
	// Arrange — the pushed-state record cannot be read.
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{err: sentinelError("db locked")}})

	// Act
	err := h.coord.Drain(context.Background())

	// Assert — an unreadable record is not "nothing to sweep": any workspace
	// left on a non-terminal phase stays wedged, so the boot says so.
	if err == nil || !strings.Contains(err.Error(), string(PhaseMergeConflict)) {
		t.Fatalf("Drain error = %v, want the read failure surfaced for the orphan phases", err)
	}
}

func TestDrainSurfacesAnOrphanSweepRecordFailure(t *testing.T) {
	// Arrange — the sweep's own merge_failed cannot be recorded.
	h := newHarnessWith(t, harnessOpts{phases: fakePhases{
		byPhase: map[Phase][]string{PhaseMerging: {"/ws/orphan"}},
	}})
	h.sink.err = sentinelError("sink down")

	// Act
	err := h.coord.Drain(context.Background())

	// Assert — a sweep that could not record its verdict is a boot that left a
	// workspace wedged, and it is never reported as a clean drain.
	if err == nil {
		t.Fatalf("Drain error = nil, want the record failure surfaced")
	}
	<-h.sink.ch
}

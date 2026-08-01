// THE FIRST INSTANT OF A MERGE, end to end over the REAL processes.
//
// `merge_enqueuing` is the command handler's own mark, emitted the moment a
// MergeWorkspaceCmd arrives and before anything durable exists for it. What
// this file proves is the property the mark is worth having for: the phase
// really does reach a frontend, and it really is SUPERSEDED — a merge that is
// admitted at the head goes enqueuing -> merging -> merged, and a merge that is
// admitted behind a held head goes enqueuing -> merge_queued.
//
// It reuses mergequeue_e2e_test.go's fixtures and mergeWatch read-only, the way
// the rest of the merge e2e suite does.
package e2e

import "testing"

// phaseOrder returns the index of the FIRST recorded WorkspaceState for ws in
// phase, or -1. Ordering between phases is the whole assertion here, and the
// accumulating watch preserves the order frames arrived in.
func phaseOrder(w *mergeWatch, ws, phase string) int {
	for i, state := range w.states {
		if state.GetWorkspace() == ws && state.GetMergePhase() == phase {
			return i
		}
	}
	return -1
}

// TestE2EAMergeAtTheHeadGoesEnqueuingThenMerging is the happy path's ordering:
// nothing else holds the repository, so the mark is followed by the cherry-pick
// itself rather than by a queue place.
func TestE2EAMergeAtTheHeadGoesEnqueuingThenMerging(t *testing.T) {
	// Arrange — one repository, one clean sibling, nothing holding the head.
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	clean := repo.cleanWorktree("feature-head")
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)

	// Act
	sendMerge(t, conn, "r-merge-head", mergeCmdFor(repo, clean, "feature-head"))
	w.awaitOKAck("r-merge-head")
	w.awaitPhase(clean, phaseMerged)

	// Assert — the mark arrived, and it arrived FIRST.
	enqueuing := phaseOrder(w, clean, phaseMergeEnqueuing)
	merging := phaseOrder(w, clean, phaseMerging)
	if enqueuing < 0 {
		t.Fatalf("no %s state was ever pushed for %s: the merge attempt was invisible until the coordinator reached it", phaseMergeEnqueuing, clean)
	}
	if merging < 0 {
		t.Fatalf("no %s state was ever pushed for %s", phaseMerging, clean)
	}
	if enqueuing > merging {
		t.Fatalf("%s arrived after %s (indices %d and %d), want the mark first", phaseMergeEnqueuing, phaseMerging, enqueuing, merging)
	}
}

// TestE2EAQueuedMergeIsMarkedEnqueuingBeforeItIsQueued is the deferred path's
// ordering: the mark precedes the queue place, so the window between the
// command and the durable enqueue is covered there too.
func TestE2EAQueuedMergeIsMarkedEnqueuingBeforeItIsQueued(t *testing.T) {
	// Arrange — a parked conflict genuinely holds the repository's head.
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	first := repo.conflictingWorktree("feature-first")
	second := repo.cleanWorktree("feature-second")
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)
	parkTheQueueHead(t, w, repo, first, "feature-first", "r-merge-first")

	// Act
	sendMerge(t, conn, "r-merge-second", mergeCmdFor(repo, second, "feature-second"))
	w.awaitOKAck("r-merge-second")
	w.awaitPhase(second, phaseMergeQueued)

	// Assert
	enqueuing := phaseOrder(w, second, phaseMergeEnqueuing)
	queued := phaseOrder(w, second, phaseMergeQueued)
	if enqueuing < 0 {
		t.Fatalf("no %s state was ever pushed for %s", phaseMergeEnqueuing, second)
	}
	if enqueuing > queued {
		t.Fatalf("%s arrived after %s (indices %d and %d), want the mark first", phaseMergeEnqueuing, phaseMergeQueued, enqueuing, queued)
	}
}

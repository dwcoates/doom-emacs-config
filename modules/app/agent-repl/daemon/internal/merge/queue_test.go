package merge

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TestEnqueueRefusesAWorkspaceWithNoLayoutFacts covers the geometry refusal:
// merge geometry is recorded at creation and never inferred, so its absence
// means this workspace can never be merged.
func TestEnqueueRefusesAWorkspaceWithNoLayoutFacts(t *testing.T) {
	// Arrange: a workspace whose creation job holds no geometry.
	h := newHarness(t)
	h.db.mu.Lock()
	delete(h.db.jobs, theWorkspace)
	h.db.mu.Unlock()

	// Act.
	err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmNoLayoutFacts {
		t.Fatalf("Enqueue answered %v, want the %s refusal", err, ArmNoLayoutFacts)
	}
}

// TestEnqueueRefusesAnIncompleteLayout covers the half-recorded geometry, which
// is as unmergeable as none at all.
func TestEnqueueRefusesAnIncompleteLayout(t *testing.T) {
	// Arrange: a creation job with no target directory.
	h := newHarness(t)
	h.db.mu.Lock()
	job := h.db.jobs[theWorkspace]
	job.Layout.TargetDir = ""
	h.db.jobs[theWorkspace] = job
	h.db.mu.Unlock()

	// Act.
	err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmNoLayoutFacts {
		t.Fatalf("Enqueue answered %v, want the %s refusal", err, ArmNoLayoutFacts)
	}
}

// TestEnqueueRefusesADeletedSession covers the deleted session: it refuses
// resurrection, so the configured prompts could never run.
func TestEnqueueRefusesADeletedSession(t *testing.T) {
	// Arrange: a workspace whose session was deleted.
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.sessions[theWorkspace] = wsm.Session{
		Workspace: theWorkspace,
		Terminal:  &wsm.SessionTerminal{Kind: "deleted", Detail: "the user deleted it"},
	}
	h.db.mu.Unlock()

	// Act.
	err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmSessionDeleted {
		t.Fatalf("Enqueue answered %v, want the %s refusal", err, ArmSessionDeleted)
	}
}

// TestEnqueueRefusesASecondEnqueue covers the duplicate: a merge is queued once.
func TestEnqueueRefusesASecondEnqueue(t *testing.T) {
	// Arrange: an already-queued merge.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("the first enqueue failed: %v", err)
	}

	// Act.
	err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmAlreadyQueued {
		t.Fatalf("the second Enqueue answered %v, want the %s refusal", err, ArmAlreadyQueued)
	}
}

// TestEnqueueRefusesAWorkspaceAlreadyMerging covers the in-flight case, whose
// refusal names a different arm from the merely-queued one.
func TestEnqueueRefusesAWorkspaceAlreadyMerging(t *testing.T) {
	// Arrange: a queue entry already admitted.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("the first enqueue failed: %v", err)
	}
	if err := h.db.AdmitMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("admitting: %v", err)
	}

	// Act.
	err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmAlreadyMerging {
		t.Fatalf("Enqueue answered %v, want the %s refusal", err, ArmAlreadyMerging)
	}
}

// TestEnqueueLeavesNoStateWhenItRefuses covers the pre-state rule: a refusal
// records nothing, so no enqueuing-then-failed trail is left behind.
func TestEnqueueLeavesNoStateWhenItRefuses(t *testing.T) {
	// Arrange: a workspace with no geometry.
	h := newHarness(t)
	h.db.mu.Lock()
	delete(h.db.jobs, theWorkspace)
	h.db.mu.Unlock()

	// Act.
	_ = h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser)

	// Assert.
	if _, has := h.o.Facts(theWorkspace); has {
		t.Fatal("a refused enqueue published merge facts")
	}
	queues, _ := h.db.AllMergeQueues(context.Background())
	if len(queues) != 0 {
		t.Fatalf("a refused enqueue left %d queue(s) behind", len(queues))
	}
}

// TestEnqueueKeepsFifoOrder covers the queue's whole point: merges run in the
// order they were asked for.
func TestEnqueueKeepsFifoOrder(t *testing.T) {
	// Arrange: three workspaces of one repository, enqueued in order.
	h := newHarness(t)
	order := []ids.WorkspaceID{"ws-1", "ws-2", "ws-3"}
	h.register("ws-2", "ws-two")
	h.register("ws-3", "ws-three")
	for _, ws := range order {
		if err := h.o.Enqueue(context.Background(), ws, RequestedByUser); err != nil {
			t.Fatalf("enqueueing %s: %v", ws, err)
		}
	}

	// Act.
	entries, err := h.db.MergeQueue(context.Background(), h.repoKey())
	if err != nil {
		t.Fatalf("reading the queue: %v", err)
	}

	// Assert.
	for i, entry := range entries {
		if entry.Workspace != order[i] || entry.Position != i+1 {
			t.Fatalf("entry %d is %s at position %d, want %s at %d", i, entry.Workspace, entry.Position, order[i], i+1)
		}
	}
}

// TestPauseStopsAdmission covers the operator's pause: queued merges hold in
// place and nothing new starts.
func TestPauseStopsAdmission(t *testing.T) {
	// Arrange: a queued merge on a paused queue.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	if err := h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("pausing: %v", err)
	}

	// Act.
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil {
		t.Fatalf("the pump errored on a paused queue: %v", err)
	}
	if ran {
		t.Fatal("a paused queue admitted a merge")
	}
}

// TestUnpauseResumesAdmission covers the other half of the pause: the queue
// admits again once it is resumed.
func TestUnpauseResumesAdmission(t *testing.T) {
	// Arrange: a paused queue with a merge waiting, then resumed.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def456")
	h.gatePasses("daemon")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	if err := h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("pausing: %v", err)
	}
	if err := h.o.Unpause(context.Background(), nil); err != nil {
		t.Fatalf("unpausing: %v", err)
	}

	// Act.
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil || !ran {
		t.Fatalf("a resumed queue did not admit: ran=%v err=%v", ran, err)
	}
}

// TestPauseRefusesAnAlreadyPausedQueue covers the no-op refusal: an operator
// asked for a change that did not happen.
func TestPauseRefusesAnAlreadyPausedQueue(t *testing.T) {
	// Arrange: an already-paused queue.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	if err := h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("the first pause failed: %v", err)
	}

	// Act.
	err := h.o.Pause(context.Background(), nil)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmAlreadyPaused {
		t.Fatalf("the second pause answered %v, want the %s refusal", err, ArmAlreadyPaused)
	}
}

// TestUnpauseRefusesAQueueThatIsNotPaused covers the mirror no-op refusal.
func TestUnpauseRefusesAQueueThatIsNotPaused(t *testing.T) {
	// Arrange: a running queue.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}

	// Act.
	err := h.o.Unpause(context.Background(), nil)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmNotPaused {
		t.Fatalf("Unpause answered %v, want the %s refusal", err, ArmNotPaused)
	}
}

// TestEvictRemovesAQueuedMerge covers the operator's eviction, one of the three
// distinct ends a merge can reach before it runs.
func TestEvictRemovesAQueuedMerge(t *testing.T) {
	// Arrange: two queued merges.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	for _, ws := range []ids.WorkspaceID{theWorkspace, "ws-2"} {
		if err := h.o.Enqueue(context.Background(), ws, RequestedByUser); err != nil {
			t.Fatalf("enqueueing %s: %v", ws, err)
		}
	}

	// Act.
	err := h.o.Evict(context.Background(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("Evict failed: %v", err)
	}
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || entries[0].Workspace != "ws-2" || entries[0].Position != 1 {
		t.Fatalf("after the eviction the queue is %+v, want ws-2 alone at position 1", entries)
	}
}

// TestEvictRecordsItsOwnCause covers the three-ends rule: an eviction is
// recorded as an eviction, never as a merge that finished.
func TestEvictRecordsItsOwnCause(t *testing.T) {
	// Arrange: a queued merge.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict failed: %v", err)
	}

	// Assert.
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if len(h.db.dropped) != 1 || h.db.dropped[0] != string(theWorkspace)+":evicted" {
		t.Fatalf("the drop was recorded as %v, want the evicted cause", h.db.dropped)
	}
}

// TestEvictRefusesAWorkspaceWithNothingQueued covers the refusal an operator
// gets for a merge that is not there.
func TestEvictRefusesAWorkspaceWithNothingQueued(t *testing.T) {
	// Arrange: a workspace with no queued merge.
	h := newHarness(t)

	// Act.
	err := h.o.Evict(context.Background(), theWorkspace)

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmNoSuchQueuedMerge {
		t.Fatalf("Evict answered %v, want the %s refusal", err, ArmNoSuchQueuedMerge)
	}
}

// TestAdmissionTakesTheRepositoryLock covers the exclusivity: while a merge
// runs, the repository's lock is held so no second daemon admits from the same
// queue.
func TestAdmissionTakesTheRepositoryLock(t *testing.T) {
	// Arrange: a merge held inside its gate, so the run holds its slot when
	// the assert happens.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	inGate, release := holdTheGate(h)
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	_, taken, err := acquireRepoLock(h.o.lockDir, string(h.repoKey()))

	// Assert.
	close(release)
	<-done
	if err != nil {
		t.Fatalf("probing the repository lock errored: %v", err)
	}
	if taken {
		t.Fatal("the repository's queue lock was free while a merge was running")
	}
}

// TestPumpAdmitsNothingWhileARunHoldsTheRepository covers the in-process half of
// the same exclusivity.
func TestPumpAdmitsNothingWhileARunHoldsTheRepository(t *testing.T) {
	// Arrange: a run held inside its gate, holding the repository.
	h := newHarness(t)
	h.emacsRepo()
	h.register("ws-2", "ws-two")
	h.landsCleanly("abc123def4567")
	inGate, release := holdTheGate(h)
	for _, ws := range []ids.WorkspaceID{theWorkspace, "ws-2"} {
		if err := h.o.Enqueue(context.Background(), ws, RequestedByUser); err != nil {
			t.Fatalf("enqueueing %s: %v", ws, err)
		}
	}
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	close(release)
	<-done
	if err != nil {
		t.Fatalf("the second pump errored: %v", err)
	}
	if ran {
		t.Fatal("a second merge was admitted while the first held the repository")
	}
}

// holdTheGate holds the harness's next gate run until release is closed, and
// signals inGate once it is held: the rendezvous a test needs to act while a
// merge is in its long phase.
func holdTheGate(h *harness) (inGate, release chan struct{}) {
	inGate, release = make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		h.runner.mu.Lock()
		h.runner.before = nil
		h.runner.mu.Unlock()
		close(inGate)
		<-release
	}
	return inGate, release
}

// TestEnqueuePublishesTheQueuePosition covers the facts a waiting user reads:
// where in the queue their merge sits, and how deep the queue is.
func TestEnqueuePublishesTheQueuePosition(t *testing.T) {
	// Arrange: two merges on one repository.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	for _, ws := range []ids.WorkspaceID{theWorkspace, "ws-2"} {
		if err := h.o.Enqueue(context.Background(), ws, RequestedByUser); err != nil {
			t.Fatalf("enqueueing %s: %v", ws, err)
		}
	}

	// Act.
	facts, ok := h.o.Facts("ws-2")

	// Assert.
	if !ok {
		t.Fatal("the second merge published no facts")
	}
	if facts.State != StateQueued || facts.QueuePosition != 2 || facts.QueueDepth != 2 {
		t.Fatalf("facts are %+v, want queued at position 2 of 2", facts)
	}
}

// TestEnqueueSurfacesAnUnexpectedStoreFailure covers the error path: a store
// failure is not a refusal, and nothing of the merge is left published.
func TestEnqueueSurfacesAnUnexpectedStoreFailure(t *testing.T) {
	// Arrange: a store whose next enqueue fails.
	h := newHarness(t)
	boom := errors.New("the database is gone")
	h.db.mu.Lock()
	h.db.enqueueErr = boom
	h.db.mu.Unlock()

	// Act.
	err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser)

	// Assert.
	if !errors.Is(err, boom) {
		t.Fatalf("Enqueue answered %v, want the store's own failure", err)
	}
	if _, arm := Refused(err); arm {
		t.Fatal("a store failure was reported as a refusal")
	}
}

// TestPauseWithNoScopePausesEveryRepository covers the UNSET repository ref:
// the daemon-wide switch touches every repository that has a queue.
func TestPauseWithNoScopePausesEveryRepository(t *testing.T) {
	// Arrange: two repositories with a queue each.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	other := wsm.RepoKey("/other/repo/.git")
	if _, err := h.db.EnqueueMerge(context.Background(), other, ids.WorkspaceID("ws-2"), h.clock()); err != nil {
		t.Fatalf("enqueueing on the second repository: %v", err)
	}

	// Act.
	err := h.o.Pause(context.Background(), nil)

	// Assert.
	if err != nil {
		t.Fatalf("the daemon-wide pause failed: %v", err)
	}
	for _, repo := range []wsm.RepoKey{h.repoKey(), other} {
		paused, err := h.db.MergeQueuePaused(context.Background(), repo)
		if err != nil {
			t.Fatalf("reading %s's pause state: %v", repo, err)
		}
		if !paused {
			t.Fatalf("%s is not paused after the daemon-wide pause", repo)
		}
	}
}

// TestPauseWithAScopePausesOnlyThatRepository covers a set repository ref:
// exactly the named repository's queue stops admitting.
func TestPauseWithAScopePausesOnlyThatRepository(t *testing.T) {
	// Arrange: two repositories with a queue each, both registered.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	other := wsm.RepoKey("/other/repo/.git")
	if _, err := h.db.EnqueueMerge(context.Background(), other, ids.WorkspaceID("ws-2"), h.clock()); err != nil {
		t.Fatalf("enqueueing on the second repository: %v", err)
	}
	// THE REGISTRY HOLDS THE WORKTREE DIR; the queue is keyed by its common
	// dir, which is what the scope has to resolve through.
	h.registerRepo("repo-one", h.targetD)
	h.registerRepo("repo-two", "/other/repo")

	// Act.
	err := h.o.Pause(context.Background(), &RepositoryScope{ID: "repo-one"})

	// Assert.
	if err != nil {
		t.Fatalf("the scoped pause failed: %v", err)
	}
	paused, err := h.db.MergeQueuePaused(context.Background(), other)
	if err != nil {
		t.Fatalf("reading the unnamed repository's pause state: %v", err)
	}
	if paused {
		t.Fatal("the scoped pause paused a repository it did not name")
	}
}

// TestUnpauseWithAScopeResumesOnlyThatRepository covers the resume half of the
// scoping: the repository the ref does not name stays paused.
func TestUnpauseWithAScopeResumesOnlyThatRepository(t *testing.T) {
	// Arrange: two paused repositories, both registered.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	other := wsm.RepoKey("/other/repo/.git")
	if _, err := h.db.EnqueueMerge(context.Background(), other, ids.WorkspaceID("ws-2"), h.clock()); err != nil {
		t.Fatalf("enqueueing on the second repository: %v", err)
	}
	// THE REGISTRY HOLDS THE WORKTREE DIR; the queue is keyed by its common
	// dir, which is what the scope has to resolve through.
	h.registerRepo("repo-one", h.targetD)
	h.registerRepo("repo-two", "/other/repo")
	if err := h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("pausing: %v", err)
	}

	// Act.
	err := h.o.Unpause(context.Background(), &RepositoryScope{ID: "repo-one"})

	// Assert.
	if err != nil {
		t.Fatalf("the scoped resume failed: %v", err)
	}
	paused, err := h.db.MergeQueuePaused(context.Background(), other)
	if err != nil {
		t.Fatalf("reading the unnamed repository's pause state: %v", err)
	}
	if !paused {
		t.Fatal("the scoped resume resumed a repository it did not name")
	}
}

// TestAScopedPauseKeysTheQueueByTheRepositorysCommonDir pins the keying the
// scope resolves through: the registry holds the WORKTREE dir, the queue is
// stored under the common dir, so a scoped pause must be visible to an
// unscoped read of the queue's own key.
func TestAScopedPauseKeysTheQueueByTheRepositorysCommonDir(t *testing.T) {
	// Arrange: one queue, registered by its worktree dir.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	h.registerRepo("repo-one", h.targetD)

	// Act.
	if err := h.o.Pause(context.Background(), &RepositoryScope{ID: "repo-one"}); err != nil {
		t.Fatalf("the scoped pause failed: %v", err)
	}

	// Assert: the queue's OWN key carries the pause.
	paused, err := h.db.MergeQueuePaused(context.Background(), h.repoKey())
	if err != nil {
		t.Fatalf("reading the queue's pause state: %v", err)
	}
	if !paused {
		t.Fatal("the scoped pause wrote a key the queue is not stored under")
	}
}

// TestPauseRefusesAnUnknownRepositoryRef covers the ref the registry does not
// hold: it is refused rather than pausing a key nobody owns.
func TestPauseRefusesAnUnknownRepositoryRef(t *testing.T) {
	// Arrange: a registry holding one repository, and a ref naming another.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	// THE REGISTRY HOLDS THE WORKTREE DIR; the queue is keyed by its common
	// dir, which is what the scope has to resolve through.
	h.registerRepo("repo-one", h.targetD)

	// Act.
	err := h.o.Pause(context.Background(), &RepositoryScope{ID: "repo-nope"})

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmUnknownRepository {
		t.Fatalf("the pause answered %v, want the %s refusal", err, ArmUnknownRepository)
	}
}

// TestUnpauseRefusesAnUnknownRepositoryRef covers the mirror refusal on the
// resume verb.
func TestUnpauseRefusesAnUnknownRepositoryRef(t *testing.T) {
	// Arrange: a paused queue and a registry holding one repository.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}
	// THE REGISTRY HOLDS THE WORKTREE DIR; the queue is keyed by its common
	// dir, which is what the scope has to resolve through.
	h.registerRepo("repo-one", h.targetD)
	if err := h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("pausing: %v", err)
	}

	// Act.
	err := h.o.Unpause(context.Background(), &RepositoryScope{Dir: "/not/registered/.git"})

	// Assert.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmUnknownRepository {
		t.Fatalf("the resume answered %v, want the %s refusal", err, ArmUnknownRepository)
	}
}

// TestEvictEndsTheQueuedBubbleWithTheAbandonedTerminal covers the bubble half of
// a merge dropped before it ran: the queue tab simply stopping would leave a
// reader with no ending at all, so the head row gets FeedMergeError.abandoned.
func TestEvictEndsTheQueuedBubbleWithTheAbandonedTerminal(t *testing.T) {
	// Arrange: one queued merge, whose bubble is showing its queue tab.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict failed: %v", err)
	}

	// Assert.
	if got := h.feed.lastMergeErrorArm(); got != "abandoned" {
		t.Fatalf("the evicted merge's terminal arm = %q, want abandoned", got)
	}
}

// TestEvictsAbandonedTerminalCarriesTheOperatorsCause is landing 7's half: the
// arm does not distinguish the three ends, so the CAUSE reaches a reader as
// FeedMergeAbandoned.summary.
func TestEvictsAbandonedTerminalCarriesTheOperatorsCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict failed: %v", err)
	}

	// Assert.
	if got := h.feed.lastAbandonedSummary(); got != "the operator evicted this merge from the queue" {
		t.Fatalf("the evicted merge's summary = %q, want the operator's cause", got)
	}
}

// TestEvictLeavesTheFooterAndSidebarWithNoMergeStanding covers the surface half
// of the same drop: a merge that never ran leaves no merge state behind.
func TestEvictLeavesTheFooterAndSidebarWithNoMergeStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if err := h.o.Enqueue(context.Background(), theWorkspace, RequestedByUser); err != nil {
		t.Fatalf("enqueueing: %v", err)
	}

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict failed: %v", err)
	}

	// Assert.
	if got := h.footer.last().State; got != "none" {
		t.Fatalf("footer merge state after the evict = %q, want none", got)
	}
}

// TestOneMergesFailureDoesNotStopTheQueueBehindIt covers the pump's own
// invariant: a run that ended on its own terminal has already reported itself,
// so the next queued merge still gets admitted.
func TestOneMergesFailureDoesNotStopTheQueueBehindIt(t *testing.T) {
	// Arrange: a merge git refuses to make.
	h := newHarness(t)
	h.emacsRepo()
	h.git.mergeErr = errors.New("refusing to merge unrelated histories")
	enqueue(t, h)

	// Act.
	err := h.admit(context.Background())

	// Assert: the pump reports no queue-level failure, so its loop goes on.
	if err != nil {
		t.Fatalf("pumpOnce after a merge that ended on its own terminal = %v, want no queue-level error", err)
	}
}

// TestEveryAbandonCauseResolvesItsOwnSentence pins the cause vocabulary: the
// wire has ONE abandoned arm for all four ends, so a cause with no sentence of
// its own would be an end a reader cannot tell from any other.
func TestEveryAbandonCauseResolvesItsOwnSentence(t *testing.T) {
	// Arrange.
	tests := []struct {
		name  string
		cause AbandonCause
		want  string
	}{
		{"user drop", CauseUserDrop, "the operator evicted this merge from the queue"},
		{"user dequeue", CauseUserDequeue, "the user released this merge's queue slot"},
		{"workspace closed", CauseWorkspaceClosed, "the workspace was closed while this merge was waiting in the queue"},
		{"daemon shutdown", CauseDaemonShutdown, "the daemon shut down while this merge was waiting in the queue, and the restart could not put it back"},
	}
	seen := map[string]AbandonCause{}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, declared := tt.cause.summary()

			// Assert.
			if !declared || got != tt.want {
				t.Fatalf("%s.summary() = %q, %v, want %q declared", tt.cause, got, declared, tt.want)
			}
			if other, clash := seen[got]; clash {
				t.Fatalf("%s draws the same sentence as %s, so the two ends read alike", tt.cause, other)
			}
			seen[got] = tt.cause
		})
	}
}

// TestClosingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause is the
// workspace-close producer: a merge whose workspace is torn down can never
// run, and the bubble says so rather than simply stopping.
func TestClosingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert.
	if got := h.feed.lastAbandonedSummary(); got != "the workspace was closed while this merge was waiting in the queue" {
		t.Fatalf("the closed workspace's merge summary = %q, want the close's own cause", got)
	}
}

// TestClosingAWorkspaceTakesItsMergeOffTheQueue covers the other half of the
// same close: a merge left queued behind a workspace that is gone would hold
// the repository's queue against a workspace nobody can merge.
func TestClosingAWorkspaceTakesItsMergeOffTheQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 0 {
		t.Fatalf("the queue after the close is %+v, want it empty", entries)
	}
}

// TestClosingAWorkspaceWithNoQueuedMergeAbandonsNothing is the no-op edge: the
// teardown verbs call this for every workspace, and most have no merge at all.
func TestClosingAWorkspaceWithNoQueuedMergeAbandonsNothing(t *testing.T) {
	// Arrange: a registered workspace that never enqueued a merge.
	h := newHarness(t)

	// Act.
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert.
	if got := h.feed.lastMergeErrorArm(); got != "" {
		t.Fatalf("a close with no queued merge drew the %q terminal, want no terminal at all", got)
	}
}

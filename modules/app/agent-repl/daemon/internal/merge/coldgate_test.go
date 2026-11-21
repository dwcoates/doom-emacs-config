package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// entryIn answers WS's entry in the harness's queue, failing the test when it
// has none.
func entryIn(t *testing.T, h *harness, ws ids.WorkspaceID) wsm.MergeQueueEntry {
	t.Helper()
	entries, err := h.db.MergeQueue(context.Background(), h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, entry := range entries {
		if entry.Workspace == ws {
			return entry
		}
	}
	t.Fatalf("no entry of %s in %+v", ws, entries)
	return wsm.MergeQueueEntry{}
}

// queueOrderOf answers the workspaces of the harness's queue, in order.
func queueOrderOf(t *testing.T, h *harness) []ids.WorkspaceID {
	t.Helper()
	entries, err := h.db.MergeQueue(context.Background(), h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	out := make([]ids.WorkspaceID, 0, len(entries))
	for _, entry := range entries {
		out = append(out, entry.Workspace)
	}
	return out
}

// coldGateRecord answers the record a cold_gate operation wrote at LEVEL
// saying MESSAGE.
func coldGateRecord(h *harness, level, message string) (dlog.Record, bool) {
	return dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: level, Operation: opColdGate, Message: message})
}

// twoInLine queues the harness's merge, then ws-2's behind it.
func twoInLine(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")
	return h
}

func TestAdmissibleSkipsEveryMergeAtItsColdGate(t *testing.T) {
	// Arrange
	line := []wsm.MergeQueueEntry{
		{Workspace: "a", ColdGated: true, Position: 1},
		{Workspace: "b", Position: 2},
		{Workspace: "c", ColdGated: true, Position: 3},
		{Workspace: "d", Position: 4},
	}

	// Act
	got := admissible(line)

	// Assert
	if len(got) != 2 || got[0].Workspace != "b" || got[1].Workspace != "d" {
		t.Fatalf("admissible = %+v, want b then d", got)
	}
	if got[0].Position != 2 || got[1].Position != 4 {
		t.Fatalf("positions = %d, %d; want each merge's own place in line", got[0].Position, got[1].Position)
	}
}

func TestNextInLineSkipsAWaitingMergeAtItsColdGate(t *testing.T) {
	// Arrange
	entries := []wsm.MergeQueueEntry{
		{Workspace: "a", State: wsm.MergeQueued, ColdGated: true},
		{Workspace: "b", State: wsm.MergeQueued},
	}

	// Act
	front, found, _ := nextInLine(entries, func(ids.WorkspaceID) bool { return true })

	// Assert
	if !found || front.Workspace != "b" {
		t.Fatalf("nextInLine = %+v, %v; want b", front, found)
	}
}

func TestAWaitingMergeAtItsColdGateKeepsItsPlace(t *testing.T) {
	// Arrange
	h := twoInLine(t)

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("queue = %v, want the merge kept at its place", got)
	}
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge at its cold gate is not marked")
	}
	if _, ok := coldGateRecord(h, "info", "the session parked at its cold gate; its merge keeps its place in line and is skipped until the gate is passed"); !ok {
		t.Fatal("the kept place was not recorded")
	}
}

func TestTheQueueAdmitsTheMergeBehindAMergeAtItsColdGate(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)

	// Act
	front, _, admitted, err := h.o.admitFront(context.Background(), h.repoKey())

	// Assert
	if err != nil || !admitted || front != "ws-2" {
		t.Fatalf("admitFront = %s, %v, %v; want ws-2 admitted past the merge at its cold gate", front, admitted, err)
	}
	if entryIn(t, h, theWorkspace).State != wsm.MergeQueued {
		t.Fatal("the merge at its cold gate was admitted")
	}
}

func TestTheQueueAdmitsNothingWhenEveryMergeStandsAtItsColdGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	h.o.ColdGateRaised(theWorkspace)

	// Act
	_, _, admitted, err := h.o.admitFront(context.Background(), h.repoKey())

	// Assert
	if err != nil || admitted {
		t.Fatalf("admitFront = %v, %v; want nothing admitted", admitted, err)
	}
}

func TestAMergeResumesAtItsPlaceOnceItsColdGateIsPassed(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)

	// Act
	h.o.ColdGateRetired(theWorkspace)

	// Assert
	if entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge whose gate was passed is still marked")
	}
	if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("queue = %v, want the merge at its own place", got)
	}
	front, _, admitted, err := h.o.admitFront(context.Background(), h.repoKey())
	if err != nil || !admitted || front != theWorkspace {
		t.Fatalf("admitFront = %s, %v, %v; want the resumed merge admitted first", front, admitted, err)
	}
	if _, ok := coldGateRecord(h, "info", "the merge resumes at its place in line: its cold gate was passed"); !ok {
		t.Fatal("the resume was not recorded")
	}
}

func TestAGateRetiredOverAnUnmarkedMergeChangesNothing(t *testing.T) {
	// Arrange
	h := twoInLine(t)

	// Act
	h.o.ColdGateRetired(theWorkspace)

	// Assert
	if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("queue = %v, want it unchanged", got)
	}
	if _, ok := coldGateRecord(h, "debug", "the workspace's merge was not kept at a cold gate"); !ok {
		t.Fatal("the unmarked retirement was not recorded at DEBUG")
	}
}

func TestAColdGateOnAWorkspaceWithNoMergeChangesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	if got := queueOrderOf(t, h); len(got) != 0 {
		t.Fatalf("queue = %v, want none", got)
	}
	if _, ok := coldGateRecord(h, "debug", "the workspace has no merge on any queue; its gate's edge changes nothing there"); !ok {
		t.Fatal("the gate of a workspace with no merge was not recorded at DEBUG")
	}
}

func TestAMarkThatCannotBeWrittenIsAnErrorAndMarksNothing(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.db.mu.Lock()
	h.db.coldGateErr = errors.New("the store refused")
	h.db.mu.Unlock()

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	if entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the refused mark was written")
	}
	rec, ok := coldGateRecord(h, "error", "could not keep the merge of a workspace at its cold gate at its place in line; the queue may admit it")
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["repo"] == nil || rec.Context["error"] == nil {
		t.Fatalf("the refused mark was recorded as %+v, want an ERROR naming the workspace, the queue and the cause", rec)
	}
}

func TestAnUnmarkThatCannotBeWrittenIsAnErrorAndKeepsTheMark(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)
	h.db.mu.Lock()
	h.db.coldGateErr = errors.New("the store refused")
	h.db.mu.Unlock()

	// Act
	h.o.ColdGateRetired(theWorkspace)

	// Assert
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the refused unmark changed the mark")
	}
	rec, ok := coldGateRecord(h, "error", "could not resume the merge of a workspace whose cold gate was passed; it stays skipped")
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["error"] == nil {
		t.Fatalf("the refused unmark was recorded as %+v, want an ERROR naming the workspace and the cause", rec)
	}
}

func TestAnOvertakenMergeResumesBehindTheMergeWorkedOn(t *testing.T) {
	// Arrange: ws-2 stands first and at its gate; the harness's merge
	// overtakes it and holds inside its gate.
	c := newChain(t)
	c.queue(t, "ws-2", "b1")
	c.h.o.ColdGateRaised("ws-2")
	enqueue(t, c.h)
	inGate, release := c.runner.hold(c.dirs[theWorkspace])
	done := admitAsync(c.h, context.Background())
	<-inGate

	// Act
	c.h.o.ColdGateRetired("ws-2")

	// Assert
	got := queueOrderOf(t, c.h)
	close(release)
	<-done
	if !slices.Equal(got, []ids.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("queue = %v, want the resumed merge right behind the merge worked on", got)
	}
}

func TestARunningMergeWhoseSessionParksAtItsColdGateGoesBackToItsPlace(t *testing.T) {
	// Arrange: the harness's merge holds inside its gate.
	c := newChain(t)
	inGate, _ := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	oldHead := headRowID(t, c.h, theWorkspace)
	oldLedger, _ := c.h.o.leaseOf(theWorkspace)
	done := admitAsync(c.h, context.Background())
	<-inGate

	// Act
	c.h.o.ColdGateRaised(theWorkspace)
	<-done

	// Assert
	if got := queueOrderOf(t, c.h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("queue = %v, want the merge back at its own place", got)
	}
	entry := entryIn(t, c.h, theWorkspace)
	if entry.State != wsm.MergeQueued || !entry.ColdGated {
		t.Fatalf("entry = %s, cold gated %v; want waiting and marked", entry.State, entry.ColdGated)
	}
	newLedger, ok := c.h.o.leaseOf(theWorkspace)
	if !ok || newLedger == oldLedger || entry.Ledger != newLedger {
		t.Fatalf("ledger = %s (known %v), entry's %s; want a new bubble the entry names, not %s", newLedger, ok, entry.Ledger, oldLedger)
	}
	if _, held, _ := c.h.db.Lease(context.Background(), theWorkspace); held {
		t.Fatal("the merge put back in line still holds its merge lease")
	}
	if got, want := c.h.feed.lastAbandonedSummary(), runningSummaries[CauseColdGate]; got != want {
		t.Fatalf("the old bubble ended %q, want %q", got, want)
	}
	folds := c.h.feed.foldsOf(theWorkspace)
	if len(folds) != 1 || folds[0].row != oldHead || !folds[0].folded {
		t.Fatalf("folds = %+v, want the old bubble closed once", folds)
	}
}

func TestAMergeTakenBackButNotResumedGoesBackToItsPlaceAtItsColdGate(t *testing.T) {
	// Arrange
	h := pendingResume(t)

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	entry := entryIn(t, h, theWorkspace)
	if entry.State != wsm.MergeQueued || !entry.ColdGated {
		t.Fatalf("entry = %s, cold gated %v; want waiting and marked", entry.State, entry.ColdGated)
	}
	_, held, _ := h.db.Lease(context.Background(), theWorkspace)
	_, progress, _ := h.db.MergeProgressOf(context.Background(), theWorkspace)
	if held || progress {
		t.Fatalf("lease held %v, progress %v; want both gone", held, progress)
	}
	h.o.mu.Lock()
	_, pending := h.o.resumes[theWorkspace]
	h.o.mu.Unlock()
	if pending {
		t.Fatal("the merge put back in line is still handed to the pump to resume")
	}
	if got, want := h.feed.lastAbandonedSummary(), runningSummaries[CauseColdGate]; got != want {
		t.Fatalf("the old bubble ended %q, want %q", got, want)
	}
	if !recordedAt(h, "info", "put a merge a restart took back before it resumed back at its place in line: its lease and progress record go, its entry waits") {
		t.Fatal("the requeue was not recorded")
	}
}

func TestARequeueOfAMergeTakenBackThatCannotBeWrittenLeavesItToResume(t *testing.T) {
	// Arrange
	h := pendingResume(t)
	h.db.mu.Lock()
	h.db.coldGateErr = errors.New("the store refused")
	h.db.mu.Unlock()

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	h.o.mu.Lock()
	_, pending := h.o.resumes[theWorkspace]
	h.o.mu.Unlock()
	if !pending || entryIn(t, h, theWorkspace).State != wsm.MergeAdmitted {
		t.Fatalf("pending %v, state %s; want the merge left to resume", pending, entryIn(t, h, theWorkspace).State)
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.drop_pending_resume"})
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["error"] == nil {
		t.Fatalf("the refused requeue was recorded as %+v, want an ERROR naming the workspace and the cause", rec)
	}
}

func TestHidingAWorkspaceAtItsColdGateTakesItsMergeOffTheQueue(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)
	head := headRowID(t, h, theWorkspace)

	// Act
	h.o.OnHidden(theWorkspace)

	// Assert
	if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{"ws-2"}) {
		t.Fatalf("queue = %v, want the hidden workspace's merge gone", got)
	}
	if got, want := h.feed.lastAbandonedSummary(), abandonSummaries[CauseHiddenAtColdGate]; got != want {
		t.Fatalf("the bubble ended %q, want %q", got, want)
	}
	if folds := h.feed.foldsOf(theWorkspace); len(folds) != 1 || folds[0].row != head || !folds[0].folded {
		t.Fatalf("folds = %+v, want the released bubble closed once", folds)
	}
}

func TestHidingAWorkspaceWhoseMergeIsNotAtItsColdGateLeavesItQueued(t *testing.T) {
	// Arrange
	h := twoInLine(t)

	// Act
	h.o.OnHidden(theWorkspace)

	// Assert
	if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("queue = %v, want it unchanged", got)
	}
}

func TestATakenBackMarkedMergeWhoseGateHasNoRecordResumes(t *testing.T) {
	// Arrange: the mark was written, the gate passed where no edge reached
	// this queue, and a successor takes the queue back.
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert
	if entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge whose gate has no record is still marked")
	}
}

func TestATakenBackMarkedMergeWhoseGateStandsStaysMarked(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)
	h.db.mu.Lock()
	h.db.coldGates = map[ids.WorkspaceID]wsm.ColdGateRecord{theWorkspace: {VendorSessionID: "v-1"}}
	h.db.mu.Unlock()
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge whose gate still stands lost its mark at the take-back")
	}
}

func TestATakenBackMarkedMergeWhoseGateCannotBeReadStaysMarkedAndIsAnError(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)
	h.db.mu.Lock()
	h.db.coldGateReadErr = errors.New("the store is gone")
	h.db.mu.Unlock()
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("an unreadable gate cleared the mark")
	}
	rec, ok := coldGateRecord(h, "error", "could not read whether the cold gate of a merge kept at it still stands; it stays skipped")
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["error"] == nil {
		t.Fatalf("the unreadable gate was recorded as %+v, want an ERROR naming the workspace and the cause", rec)
	}
}

func TestAnAsynchronousQueueAppliesAGatesEdgesInTheOrderTheyHappened(t *testing.T) {
	// Arrange: a paused queue admits nothing, so only the edges move it.
	h := twoInLine(t)
	h.db.mu.Lock()
	h.db.paused[h.repoKey()] = true
	h.db.mu.Unlock()
	h.o.async = true

	// Act
	h.o.ColdGateRaised(theWorkspace)
	h.o.ColdGateRetired(theWorkspace)
	h.o.live.Wait()

	// Assert
	if entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the retirement was applied before the raise it follows")
	}
	marked, resumed := -1, -1
	for i, rec := range h.logs.Records() {
		switch {
		case rec.Operation != opColdGate:
		case rec.Message == "the session parked at its cold gate; its merge keeps its place in line and is skipped until the gate is passed":
			marked = i
		case rec.Message == "the merge resumes at its place in line: its cold gate was passed":
			resumed = i
		}
	}
	if marked < 0 || resumed < marked {
		t.Fatalf("the mark is record %d and the resume record %d, want the mark first", marked, resumed)
	}
}

func TestColdGateCausesRequeueOrRelease(t *testing.T) {
	tests := []struct {
		name     string
		cause    AbandonCause
		requeues bool
		releases bool
	}{
		{"a cold gate puts the merge back", CauseColdGate, true, false},
		{"hiding at the gate releases the merge", CauseHiddenAtColdGate, false, true},
		{"an eviction releases the merge", CauseUserDrop, false, true},
		{"a closed workspace gives the merge up", CauseWorkspaceClosed, false, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			requeues, releases := tt.cause.requeues(), tt.cause.releases()

			// Assert
			if requeues != tt.requeues || releases != tt.releases {
				t.Fatalf("%s requeues %v releases %v, want %v and %v", tt.cause, requeues, releases, tt.requeues, tt.releases)
			}
		})
	}
}

func TestEndAbandonedBubbleEndsAndClosesTheBubbleOfAMergePutBackInLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	ledger, _ := h.o.leaseOf(theWorkspace)
	head := headRowID(t, h, theWorkspace)
	entry := entryIn(t, h, theWorkspace)

	// Act
	ended := h.o.endAbandonedBubble(context.Background(), theWorkspace, ledger, entry, CauseColdGate, "put back")

	// Assert
	if !ended || h.feed.lastAbandonedSummary() != "put back" {
		t.Fatalf("ended %v with %q, want the bubble ended reading the summary", ended, h.feed.lastAbandonedSummary())
	}
	if folds := h.feed.foldsOf(theWorkspace); len(folds) != 1 || folds[0].row != head || !folds[0].folded {
		t.Fatalf("folds = %+v, want the bubble closed once", folds)
	}
}

func TestEndAbandonedBubbleLeavesTheBubbleOfAMergeGivenUpOpen(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	ledger, _ := h.o.leaseOf(theWorkspace)
	entry := entryIn(t, h, theWorkspace)

	// Act
	ended := h.o.endAbandonedBubble(context.Background(), theWorkspace, ledger, entry, CauseWorkspaceClosed, "given up")

	// Assert
	if !ended {
		t.Fatal("the bubble was not ended")
	}
	if folds := h.feed.foldsOf(theWorkspace); len(folds) != 0 {
		t.Fatalf("folds = %+v, want a bubble given up under its workspace closing left open", folds)
	}
}

func TestEndAbandonedBubbleOfAnEntryWithNoQueuedTimeIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	ledger, _ := h.o.leaseOf(theWorkspace)
	entry := entryIn(t, h, theWorkspace)
	entry.EnqueuedAt = time.Time{}

	// Act
	ended := h.o.endAbandonedBubble(context.Background(), theWorkspace, ledger, entry, CauseColdGate, "put back")

	// Assert
	if ended {
		t.Fatal("a bubble with no queued time was ended")
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.abandoned",
		Message: "an abandoned merge's queue entry carries no queued time; its bubble is not ended"})
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["error"] == nil {
		t.Fatalf("the defect was recorded as %+v, want an ERROR naming the workspace and the cause", rec)
	}
}

func TestOnlyEndAbandonedBubbleDrawsAnAbandonedHeadForAMergeWithNoRun(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "}, headAbandoned)")

	// Assert
	if !slices.Equal(sites, []string{"terminal.go"}) {
		t.Fatalf("an abandoned head is drawn at %v, want only endAbandonedBubble in terminal.go", sites)
	}
}

func TestAColdGateRaisedOnAQueuedMergesWorkspaceIsAnsweredByACompaction(t *testing.T) {
	// Arrange
	h := twoInLine(t)

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	if !slices.Equal(h.gatesCompacted, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("gates compacted = %v, want the queued merge's workspace once", h.gatesCompacted)
	}
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge was not marked skipped while the compaction runs")
	}
	rec, ok := coldGateRecord(h, "info", "the cold gate of a queued merge's workspace was answered by a compaction; the merge resumes at its place once the session is up")
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["repo"] == nil {
		t.Fatalf("the compaction was recorded as %+v, want an INFO naming the workspace and the queue", rec)
	}
}

func TestAColdGateRaisedOnAWorkspaceWithNoQueuedMergeIsNotCompacted(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	if len(h.gatesCompacted) != 0 {
		t.Fatalf("gates compacted = %v, want none", h.gatesCompacted)
	}
}

func TestACompactionOfAQueuedMergesGateIsAskedAgainWhenItsRecordIsRaisedAgain(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)
	h.db.mu.Lock()
	h.db.coldGates = map[ids.WorkspaceID]wsm.ColdGateRecord{theWorkspace: {VendorSessionID: "v-1"}}
	h.db.mu.Unlock()
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act
	fresh.ColdGateRaised(theWorkspace)

	// Assert
	if !slices.Equal(h.gatesCompacted, []ids.WorkspaceID{theWorkspace, theWorkspace}) {
		t.Fatalf("gates compacted = %v, want one compaction per raise, the recorded one included", h.gatesCompacted)
	}
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge lost its mark")
	}
}

func TestACompactionThatFailsIsAnErrorAndLeavesTheMergeMarkedAndSkipped(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.compactGateErr = errors.New("the summarizer is down")

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge was unmarked though its gate stands")
	}
	rec, ok := coldGateRecord(h, "error", "the cold gate of a queued merge's workspace could not be answered by a compaction; the merge stays skipped until the gate is answered")
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["repo"] == nil || rec.Context["error"] == nil {
		t.Fatalf("the failure was recorded as %+v, want an ERROR naming the workspace, the queue and the cause", rec)
	}
}

func TestAGateStoodAgainAfterAFailedAnswerIsNeverAnsweredAgainByTheQueue(t *testing.T) {
	// Arrange: the queue answered the raise once, and the answer's re-open
	// failed, which stood the gate again.
	h := twoInLine(t)
	h.o.ColdGateRaised(theWorkspace)

	// Act
	h.o.ColdGateStoodAfterFailedAnswer(theWorkspace)

	// Assert
	if !slices.Equal(h.gatesCompacted, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("gates compacted = %v, want the one answer to the first raise and none after it failed", h.gatesCompacted)
	}
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge was unmarked though its gate stands")
	}
	rec, ok := coldGateRecord(h, "error", "an answer to the cold gate of a queued merge's workspace failed and the gate stands again; the queue does not answer it again, and the merge stays skipped until the user answers it")
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["repo"] == nil {
		t.Fatalf("the standing gate was recorded as %+v, want an ERROR naming the workspace and the queue", rec)
	}
}

func TestAGateStoodAgainOnAWorkspaceWithNoQueuedMergeIsNeitherAnsweredNorAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.ColdGateStoodAfterFailedAnswer(theWorkspace)

	// Assert
	if len(h.gatesCompacted) != 0 {
		t.Fatalf("gates compacted = %v, want none", h.gatesCompacted)
	}
	if rec, ok := coldGateRecord(h, "error", "an answer to the cold gate of a queued merge's workspace failed and the gate stands again; the queue does not answer it again, and the merge stays skipped until the user answers it"); ok {
		t.Fatalf("an ERROR %+v was recorded for a workspace with no merge", rec)
	}
}

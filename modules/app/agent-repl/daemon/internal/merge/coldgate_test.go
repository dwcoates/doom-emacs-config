package merge

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
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
	h.o.OnBacklogged(theWorkspace)

	// Assert
	if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{"ws-2"}) {
		t.Fatalf("queue = %v, want the backlogged workspace's merge gone", got)
	}
	if got, want := h.feed.lastAbandonedSummary(), abandonSummaries[CauseBackloggedAtColdGate]; got != want {
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
	h.o.OnBacklogged(theWorkspace)

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
		{"hiding at the gate releases the merge", CauseBackloggedAtColdGate, false, true},
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
	rec, ok := coldGateRecord(h, "info", "the cold gate of a queued merge's workspace was answered by a compaction; the session is up, and the merge resumes at its place")
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["repo"] == nil || rec.Context["ledger"] == "" {
		t.Fatalf("the compaction was recorded as %+v, want an INFO naming the workspace, the queue and the merge's ledger", rec)
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

// compactionFailsOnce queues two merges and answers theWorkspace's gate with
// a compaction that fails, the gate raised again inside it as a failed
// bring-up raises it (2026-11-21: 260 answers in 44 minutes).
func compactionFailsOnce(t *testing.T) *harness {
	t.Helper()
	h := twoInLine(t)
	h.compactGateErr = errors.New("the summarizer is down")
	h.duringCompactGate = func(ws ids.WorkspaceID) { h.o.ColdGateRaised(ws) }
	h.o.ColdGateRaised(theWorkspace)
	return h
}

func TestAFailedAnswerToAQueuedMergesColdGateIsFinal(t *testing.T) {
	t.Run("the gate raised while the answer runs is not answered again", func(t *testing.T) {
		// Arrange, Act
		h := compactionFailsOnce(t)

		// Assert
		if !slices.Equal(h.gatesCompacted, []ids.WorkspaceID{theWorkspace}) {
			t.Fatalf("gates compacted = %v, want one answer", h.gatesCompacted)
		}
	})
	t.Run("the gate raised after the failure is not answered again", func(t *testing.T) {
		// Arrange
		h := compactionFailsOnce(t)
		h.duringCompactGate = nil

		// Act
		h.o.ColdGateRaised(theWorkspace)

		// Assert
		if !slices.Equal(h.gatesCompacted, []ids.WorkspaceID{theWorkspace}) {
			t.Fatalf("gates compacted = %v, want one answer", h.gatesCompacted)
		}
	})
	t.Run("the merge leaves the queue's line", func(t *testing.T) {
		// Arrange, Act
		h := compactionFailsOnce(t)

		// Assert
		if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{"ws-2"}) {
			t.Fatalf("queue = %v, want the merge whose gate was not answered gone", got)
		}
	})
	t.Run("the merge stands off its queue as failed", func(t *testing.T) {
		// Arrange, Act
		h := compactionFailsOnce(t)

		// Assert
		rec, kept, err := h.db.OffQueueMergeOf(context.Background(), theWorkspace)
		if err != nil || !kept || rec.Cause != causeFailed {
			t.Fatalf("off-queue record %+v (kept %v, err %v), want the request kept as failed for a requeue", rec, kept, err)
		}
		if OffQueueCause(rec.Cause).GetFailed() == nil {
			t.Fatalf("off-queue cause %v, want the failed arm", OffQueueCause(rec.Cause))
		}
	})
	t.Run("the merge's bubble names the failure", func(t *testing.T) {
		// Arrange, Act
		h := compactionFailsOnce(t)

		// Assert
		if got, want := h.feed.lastAbandonedSummary(), abandonSummaries[CauseColdGateAnswerFailed]; got != want {
			t.Fatalf("the bubble ended %q, want %q", got, want)
		}
	})
	t.Run("the failure is one ERROR naming the workspace, the merge and the cause", func(t *testing.T) {
		// Arrange, Act
		h := compactionFailsOnce(t)

		// Assert
		const message = "the cold gate of a queued merge's workspace could not be answered by a compaction; the merge leaves its queue as failed, and the gate stands for the user"
		found := dlog.FilterRecords(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opColdGate, Message: message})
		if len(found) != 1 {
			t.Fatalf("records %v, want the failure recorded once", found)
		}
		ctx := found[0].Context
		if ctx["workspace"] != string(theWorkspace) || ctx["repo"] != string(h.repoKey()) || ctx["ledger"] == "" || ctx["error"] != "the summarizer is down" {
			t.Fatalf("the failure was recorded as %+v, want the workspace, the queue, the merge's ledger and the cause", ctx)
		}
	})
}

func TestTheRaiseWhileAnAnswerRunsIsRecordedAtDebug(t *testing.T) {
	// Arrange, Act
	h := compactionFailsOnce(t)

	// Assert
	rec, ok := coldGateRecord(h, "debug", "the cold gate of a queued merge's workspace is being answered already; the gate raised again is not answered twice")
	if !ok || rec.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("the skipped raise was recorded as %+v, want a DEBUG naming the workspace", rec)
	}
}

func TestASlowButHealthyAnswerToAQueuedMergesColdGateIsNotJudgedFailed(t *testing.T) {
	// Arrange: the gate is raised again while the compaction still runs, as
	// a summarizer reading a long conversation runs past the shim's bring-up.
	h := twoInLine(t)
	var duringAnswer []ids.WorkspaceID
	h.duringCompactGate = func(ws ids.WorkspaceID) {
		h.o.ColdGateRaised(ws)
		duringAnswer = queueOrderOf(t, h)
	}

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	if !slices.Equal(duringAnswer, []ids.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("queue while compacting = %v, want the merge still at its place", duringAnswer)
	}
	if !slices.Equal(h.gatesCompacted, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("gates compacted = %v, want one answer", h.gatesCompacted)
	}
	if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("queue = %v, want the slow answer's merge kept at its place", got)
	}
	if _, failed := coldGateRecord(h, "error", ""); failed {
		t.Fatalf("records %v, want no failure recorded for a slow answer", h.logs.Records())
	}
}

func TestAnAbandonedAnswerToAQueuedMergesColdGateLeavesTheMergeAtItsPlace(t *testing.T) {
	// Arrange
	h := twoInLine(t)
	h.compactGateAbandoned = true

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	if got := queueOrderOf(t, h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("queue = %v, want the merge kept at its place", got)
	}
	if !entryIn(t, h, theWorkspace).ColdGated {
		t.Fatal("the merge was unmarked though its gate stands")
	}
	if _, ok := coldGateRecord(h, "info", "the cold gate of a queued merge's workspace was not answered: no gate stood, or the answer's re-open was abandoned; the merge stays at its place"); !ok {
		t.Fatalf("records %v, want the unanswered gate recorded at INFO", h.logs.Records())
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

func TestAGateStoodAgainWhileItsAnswerRunsLeavesTheFailureToTheAnswersOwnRecord(t *testing.T) {
	// Arrange: the answer's failed re-open stands the gate again before the
	// answer returns its failure.
	h := twoInLine(t)
	h.compactGateErr = errors.New("the summarizer is down")
	h.duringCompactGate = func(ws ids.WorkspaceID) { h.o.ColdGateStoodAfterFailedAnswer(ws) }

	// Act
	h.o.ColdGateRaised(theWorkspace)

	// Assert
	errs := 0
	for _, r := range h.logs.Records() {
		if r.Level == "error" && r.Operation == opColdGate {
			errs++
		}
	}
	if errs != 1 {
		t.Fatalf("%d ERROR records under %s, want only the answer's own: %v", errs, opColdGate, h.logs.Records())
	}
}

// standAtColdGate stands WS's cold gate in the harness as the fleet and the
// prompt queue see it: AtColdGate answers it parked, the queue refuses a
// submission to it by the gate's name, and an answer that does not fail
// passes it, as the session it brings up retires the gate.
func standAtColdGate(h *harness, ws ids.WorkspaceID) {
	h.mu.Lock()
	if h.coldGated == nil {
		h.coldGated = map[ids.WorkspaceID]bool{}
	}
	h.coldGated[ws] = true
	h.duringCompactGate = func(gated ids.WorkspaceID) {
		h.mu.Lock()
		defer h.mu.Unlock()
		if h.compactGateErr == nil && !h.compactGateAbandoned {
			delete(h.coldGated, gated)
		}
	}
	h.mu.Unlock()
	h.queue.mu.Lock()
	h.queue.onSubmit = func(sub promptqueue.Submission) {
		h.mu.Lock()
		gated := h.coldGated[sub.WS]
		h.mu.Unlock()
		h.queue.mu.Lock()
		defer h.queue.mu.Unlock()
		h.queue.err = nil
		if gated {
			h.queue.err = &promptqueue.ColdGateRefusal{Detail: "the conversation is cold at 574,592 context tokens"}
		}
	}
	h.queue.mu.Unlock()
}

// compacted answers every workspace whose gate CompactColdGate was asked to
// answer.
func compacted(h *harness) []ids.WorkspaceID {
	h.mu.Lock()
	defer h.mu.Unlock()
	return slices.Clone(h.gatesCompacted)
}

// submitted answers every submission the harness's prompt queue took.
func submitted(h *harness) []promptqueue.Submission {
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	return slices.Clone(h.queue.submissions)
}

func TestEveryMergeTurnKindPassesAStandingColdGateBeforeItIsHandedOff(t *testing.T) {
	tests := []struct {
		name   string
		origin conversationv1.PromptOrigin
	}{
		{"a pick", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK},
		{"a configured prompt before an action", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION},
		{"a configured prompt after an action", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION},
		{"a deploy hook's prompt", conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_HOOK},
		{"a conflict repair", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR},
		{"a test repair", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR},
		{"a finish hook's prompt", conversationv1.PromptOrigin_PROMPT_ORIGIN_FINISH_HOOK},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			standAtColdGate(h, theWorkspace)

			// Act
			err := h.o.handOff(context.Background(), theWorkspace, "turn-1", "the brief", tt.origin)

			// Assert
			if err != nil {
				t.Fatalf("handOff = %v, want the turn handed off past the gate", err)
			}
			if got := compacted(h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace}) {
				t.Fatalf("gates compacted = %v, want the workspace's gate answered once", got)
			}
			if subs := submitted(h); len(subs) != 1 || subs[0].Turn != "turn-1" || subs[0].Origin != tt.origin {
				t.Fatalf("submissions = %+v, want the turn submitted once past the gate", subs)
			}
			rec, ok := coldGateRecord(h, "info", "a merge turn met its workspace's cold gate; the gate is answered by a compaction before the turn is submitted")
			if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["origin"] != tt.origin.String() || rec.Context["turn"] != "turn-1" {
				t.Fatalf("the pass was recorded as %+v, want an INFO naming the workspace, the turn and its origin", rec)
			}
		})
	}
}

func TestAMergeTurnWithNoColdGateStandingNeverCompacts(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.o.handOff(context.Background(), theWorkspace, "turn-1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK)

	// Assert
	if err != nil {
		t.Fatalf("handOff = %v, want the turn handed off", err)
	}
	if got := compacted(h); len(got) != 0 {
		t.Fatalf("gates compacted = %v, want none", got)
	}
}

func TestADeployCheckTurnLeavesItsColdGateToTheDeployChecksOwnAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	standAtColdGate(h, theWorkspace)

	// Act
	_ = h.o.handOff(context.Background(), theWorkspace, "turn-1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION)

	// Assert
	if got := compacted(h); len(got) != 0 {
		t.Fatalf("gates compacted = %v, want none: the deploy check answers its own gate at its raise", got)
	}
}

func TestAMergeTurnsFailedColdGateAnswerFailsTheTurnWithOneError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	standAtColdGate(h, theWorkspace)
	h.compactGateErr = errors.New("the summarizer is down")

	// Act
	err := h.o.handOff(context.Background(), theWorkspace, "turn-1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the summarizer is down") {
		t.Fatalf("handOff = %v, want the answer's failure", err)
	}
	if subs := submitted(h); len(subs) != 0 {
		t.Fatalf("submissions = %+v, want none past a gate that still stands", subs)
	}
	found := dlog.FilterRecords(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opColdGate,
		Message: "the cold gate of a merge's workspace could not be answered by a compaction; the merge turn waiting on the answer fails the merge, and the gate stands for the user"})
	if len(found) != 1 || found[0].Context["workspace"] != string(theWorkspace) || found[0].Context["error"] != "the summarizer is down" {
		t.Fatalf("records %v, want the failure recorded once naming the workspace and the cause", found)
	}
}

func TestAMergeTurnAtAGateWhoseOneAnswerFailedIsNotAnsweredAgain(t *testing.T) {
	// Arrange: an answer to the gate failed, and the gate stood again.
	h := newHarness(t)
	standAtColdGate(h, theWorkspace)
	h.o.ColdGateStoodAfterFailedAnswer(theWorkspace)

	// Act
	err := h.o.handOff(context.Background(), theWorkspace, "turn-1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)

	// Assert
	if err == nil {
		t.Fatal("handOff succeeded at a gate whose answer failed")
	}
	if got := compacted(h); len(got) != 0 {
		t.Fatalf("gates compacted = %v, want none after the failed answer", got)
	}
	if subs := submitted(h); len(subs) != 0 {
		t.Fatalf("submissions = %+v, want none", subs)
	}
	rec, ok := coldGateRecord(h, "error", "a merge turn met its workspace's cold gate standing after its one answer failed; the gate is not answered again, and the merge fails with the gate standing for the user")
	if !ok || rec.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("the refusal was recorded as %+v, want an ERROR naming the workspace", rec)
	}
}

func TestAMergeTurnAtAGateRetiredAfterAFailedAnswerIsAnsweredAgain(t *testing.T) {
	// Arrange: the failed gate was passed (the user answered it), and a new
	// gate stands at a later bring-up.
	h := newHarness(t)
	standAtColdGate(h, theWorkspace)
	h.o.ColdGateStoodAfterFailedAnswer(theWorkspace)
	h.o.ColdGateRetired(theWorkspace)

	// Act
	err := h.o.handOff(context.Background(), theWorkspace, "turn-1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK)

	// Assert
	if err != nil {
		t.Fatalf("handOff = %v, want the new gate answered and the turn handed off", err)
	}
	if got := compacted(h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("gates compacted = %v, want the new gate answered once", got)
	}
}

func TestAGateRaisedWhileAMergeTurnsAnswerRunsIsNotAnsweredTwice(t *testing.T) {
	// Arrange: the queue's merge is in line, and its raise edge arrives
	// while the merge turn's own answer runs.
	h := newHarness(t)
	enqueue(t, h)
	standAtColdGate(h, theWorkspace)
	pass := h.duringCompactGate
	h.duringCompactGate = func(ws ids.WorkspaceID) {
		h.o.ColdGateRaised(ws)
		pass(ws)
	}

	// Act
	err := h.o.handOff(context.Background(), theWorkspace, "turn-1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK)

	// Assert
	if err != nil {
		t.Fatalf("handOff = %v, want the turn handed off", err)
	}
	if got := compacted(h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("gates compacted = %v, want one answer", got)
	}
}

func TestAMergeTurnsAnswerThatBringsNoSessionUpWithTheGateStandingFailsTheTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	standAtColdGate(h, theWorkspace)
	h.compactGateAbandoned = true

	// Act
	err := h.o.handOff(context.Background(), theWorkspace, "turn-1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK)

	// Assert
	if err == nil {
		t.Fatal("handOff succeeded with the gate still standing")
	}
	if subs := submitted(h); len(subs) != 0 {
		t.Fatalf("submissions = %+v, want none", subs)
	}
	if _, ok := coldGateRecord(h, "error", "the answer to a merge's workspace cold gate settled with no session up and the gate still standing; the merge's turn is not submitted, and the merge fails"); !ok {
		t.Fatalf("records %v, want the abandoned answer recorded at ERROR", h.logs.Records())
	}
}

// heldQueueAnswer queues the harness's merge at a standing cold gate and
// starts the queue's own answer to its raise, held inside its compaction
// until the test closes the answer's release: a merge turn that meets the
// gate meanwhile meets an answer already running. It answers the release and
// a channel closed once the queue's answer settled.
func heldQueueAnswer(t *testing.T, h *harness) (release, settled chan struct{}) {
	t.Helper()
	enqueue(t, h)
	standAtColdGate(h, theWorkspace)
	pass := h.duringCompactGate
	entered := make(chan struct{})
	release, settled = make(chan struct{}), make(chan struct{})
	h.duringCompactGate = func(ws ids.WorkspaceID) {
		close(entered)
		<-release
		pass(ws)
	}
	go func() {
		defer close(settled)
		h.o.ColdGateRaised(theWorkspace)
	}()
	<-entered
	return release, settled
}

// joinedMessage is the record of a merge turn joining an answer running.
const joinedMessage = "a merge turn met its workspace's cold gate while the gate's answer runs; the turn waits for that answer before it is submitted"

// joinHeldAnswer hands a merge turn off while the queue's answer is held,
// and waits for the join's own record: the turn is then counted as waiting
// on that answer. It answers the hand-off's result once it returns.
func joinHeldAnswer(t *testing.T, h *harness, ctx context.Context) <-chan error {
	t.Helper()
	handedOff := make(chan error, 1)
	go func() {
		handedOff <- h.o.handOff(ctx, theWorkspace, "turn-1", "the brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK)
	}()
	if _, err := h.logs.Await(t.Context(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opColdGate, Message: joinedMessage}); err != nil {
		t.Fatalf("awaiting the join: %v", err)
	}
	return handedOff
}

func TestAMergeTurnThatMeetsTheQueuesAnswerRunningJoinsIt(t *testing.T) {
	t.Run("the turn waits for the answer, which runs once, and is then submitted", func(t *testing.T) {
		// Arrange
		h := newHarness(t)
		release, settled := heldQueueAnswer(t, h)
		handedOff := joinHeldAnswer(t, h, t.Context())
		before := submitted(h)

		// Act
		close(release)
		err := <-handedOff
		<-settled

		// Assert
		if len(before) != 0 {
			t.Fatalf("submissions before the release = %+v, want none while the answer runs", before)
		}
		if err != nil {
			t.Fatalf("handOff = %v, want the turn handed off past the gate", err)
		}
		if got := compacted(h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace}) {
			t.Fatalf("gates compacted = %v, want the one answer the queue made", got)
		}
		if subs := submitted(h); len(subs) != 1 || subs[0].Turn != "turn-1" {
			t.Fatalf("submissions = %+v, want the turn once", subs)
		}
		rec, ok := coldGateRecord(h, "info", joinedMessage)
		if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["turn"] != "turn-1" {
			t.Fatalf("the join was recorded as %+v, want an INFO naming the workspace and the turn", rec)
		}
	})
	t.Run("a failed answer fails the turn, recorded once, and the queue does not end the merge too", func(t *testing.T) {
		// Arrange
		h := newHarness(t)
		h.compactGateErr = errors.New("the summarizer is down")
		release, settled := heldQueueAnswer(t, h)
		handedOff := joinHeldAnswer(t, h, t.Context())

		// Act
		close(release)
		err := <-handedOff
		<-settled

		// Assert
		if err == nil || !strings.Contains(err.Error(), "the summarizer is down") {
			t.Fatalf("handOff = %v, want the answer's failure", err)
		}
		if subs := submitted(h); len(subs) != 0 {
			t.Fatalf("submissions = %+v, want none", subs)
		}
		errs := dlog.FilterRecords(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opColdGate})
		if len(errs) != 1 || errs[0].Message != "the cold gate of a merge's workspace could not be answered by a compaction; the merge turn waiting on the answer fails the merge, and the gate stands for the user" {
			t.Fatalf("ERROR records %v, want only the failure the waiting turn ends the merge on", errs)
		}
		if _, ok := h.entryOf(theWorkspace); !ok {
			t.Fatal("the queue took the merge off as failed; the waiting turn's own failure path ends it")
		}
	})
}

func TestAMergeTurnsWithdrawnWaitLeavesTheAnswerRunning(t *testing.T) {
	// Arrange: the turn joins the queue's held answer, which then fails.
	h := newHarness(t)
	h.compactGateErr = errors.New("the summarizer is down")
	release, settled := heldQueueAnswer(t, h)
	ctx, cancel := context.WithCancelCause(t.Context())
	handedOff := joinHeldAnswer(t, h, ctx)
	withdrawn := errors.New("the merge left its queue")

	// Act
	cancel(withdrawn)
	err := <-handedOff
	close(release)
	<-settled

	// Assert
	if !errors.Is(err, withdrawn) {
		t.Fatalf("handOff = %v, want the withdrawal's cause wrapped", err)
	}
	if got := compacted(h); !slices.Equal(got, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("gates compacted = %v, want the one answer, run on to its end", got)
	}
	if subs := submitted(h); len(subs) != 0 {
		t.Fatalf("submissions = %+v, want none", subs)
	}
	if _, ok := coldGateRecord(h, "error", "the cold gate of a queued merge's workspace could not be answered by a compaction; the merge leaves its queue as failed, and the gate stands for the user"); !ok {
		t.Fatalf("records %v, want the failure no turn waits on recorded as the queue's", h.logs.Records())
	}
	if _, ok := h.entryOf(theWorkspace); ok {
		t.Fatal("the merge whose answer failed with no turn waiting still stands in the queue")
	}
}

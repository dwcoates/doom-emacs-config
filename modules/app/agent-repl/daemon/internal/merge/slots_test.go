package merge

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// remediatingEntry is a merge out of line for ASIDE at SLOT.
func remediatingEntry(ws ids.WorkspaceID, aside wsm.MergeAside, slot wsm.MergeSlot) wsm.MergeQueueEntry {
	return wsm.MergeQueueEntry{Workspace: ws, State: wsm.MergeRemediating, Aside: aside, Slot: slot}
}

func TestArmRepairsArmsOnlyTheHolderOfEachSlot(t *testing.T) {
	cases := []struct {
		name      string
		entries   []wsm.MergeQueueEntry
		wantArmed map[ids.WorkspaceID]bool
	}{
		{name: "the holder of the fixing-tests slot is armed, the merge awaiting it is not",
			entries: []wsm.MergeQueueEntry{admittedEntry("ws-head"),
				remediatingEntry("ws-a", wsm.MergeAsideTests, wsm.MergeSlotFixingTests),
				remediatingEntry("ws-b", wsm.MergeAsideTests, wsm.MergeSlotAwaitingFixingTests)},
			wantArmed: map[ids.WorkspaceID]bool{"ws-a": true, "ws-b": false}},
		{name: "each repair slot's holder is armed",
			entries: []wsm.MergeQueueEntry{admittedEntry("ws-head"),
				remediatingEntry("ws-a", wsm.MergeAsideTests, wsm.MergeSlotFixingTests),
				remediatingEntry("ws-b", wsm.MergeAsideWaitingConflicts, wsm.MergeSlotResolvingConflicts)},
			wantArmed: map[ids.WorkspaceID]bool{"ws-a": true, "ws-b": true}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := resumeWorld(t, tc.entries)
			for ws := range tc.wantArmed {
				h.o.registerRemediation(ws, &remediation{repo: h.repoKey(), waiting: ws == "ws-b"})
			}

			// Act
			h.o.armRepairs(context.Background(), h.repoKey())

			// Assert
			h.o.mu.Lock()
			defer h.o.mu.Unlock()
			for ws, want := range tc.wantArmed {
				if got := h.o.remediating[ws].armed; got != want {
					t.Errorf("%s armed = %v, want %v", ws, got, want)
				}
			}
		})
	}
}

func TestArmRepairsArmsAHolderOnce(t *testing.T) {
	// Arrange: the holder is already armed.
	h := resumeWorld(t, []wsm.MergeQueueEntry{remediatingEntry("ws-a", wsm.MergeAsideTests, wsm.MergeSlotFixingTests)})
	h.o.registerRemediation("ws-a", &remediation{repo: h.repoKey()})
	h.o.armRepairs(context.Background(), h.repoKey())
	h.o.mu.Lock()
	first := h.o.remediating["ws-a"].cancel
	h.o.mu.Unlock()

	// Act
	h.o.armRepairs(context.Background(), h.repoKey())

	// Assert
	h.o.mu.Lock()
	again := h.o.remediating["ws-a"].cancel
	h.o.mu.Unlock()
	if first == nil || again == nil || !h.o.remediating["ws-a"].armed {
		t.Fatal("the holder was not armed")
	}
	if n := countRecords(h, "info", opRemediate, "the merge holds its repair slot; its repair starts"); n != 1 {
		t.Fatalf("the repair started %d times, want once", n)
	}
}

func TestArmRepairsFailsHardOnASecondMergeAtWorkInOneSlot(t *testing.T) {
	// Arrange: ws-b holds the fixing slot by the store; ws-a, no longer on
	// the queue, still has a wait armed for it.
	h := resumeWorld(t, []wsm.MergeQueueEntry{remediatingEntry("ws-b", wsm.MergeAsideTests, wsm.MergeSlotFixingTests)})
	h.o.registerRemediation("ws-a", &remediation{repo: h.repoKey(), armed: true, cancel: func() {}})
	h.o.registerRemediation("ws-b", &remediation{repo: h.repoKey()})

	// Act
	panicked := func() (recovered any) {
		defer func() { recovered = recover() }()
		h.o.armRepairs(context.Background(), h.repoKey())
		return nil
	}()

	// Assert
	if panicked == nil || !strings.Contains(fmtAny(panicked), "ws-b") {
		t.Fatalf("armRepairs panicked with %v, want the second holder named", panicked)
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediate,
		Message: "a repair slot would have a second merge at work: the merge still armed for it no longer holds it"})
	if !ok || rec.Context["holder"] != "ws-b" || rec.Context["armed"] != "ws-a" || rec.Context["slot"] != "fixing_tests" {
		t.Fatalf("ERROR record = %+v (found %v), want the slot, its holder and the merge still armed", rec, ok)
	}
}

func TestArmRepairsRecordsAQueueItCannotRead(t *testing.T) {
	// Arrange
	h := resumeWorld(t, nil)
	h.db.mu.Lock()
	h.db.queueReadErr = errors.New("store gone")
	h.db.mu.Unlock()

	// Act
	h.o.armRepairs(context.Background(), h.repoKey())

	// Assert
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediate,
		Message: "could not read the queue to act on its repair slots; a slot released now is acted on at the queue's next write"})
	if !ok || rec.Context["repo"] != string(h.repoKey()) || rec.Context["error"] == nil {
		t.Fatalf("ERROR record = %+v (found %v), want the queue and the error", rec, ok)
	}
}

func TestAReturnGrantsTheFixingSlotToTheMergeAwaitingIt(t *testing.T) {
	// Arrange: the failing head stepped aside holding the fixing slot; ws-b
	// then failed at the head too and awaits it, with ws-c behind.
	c := stepAsideScenario(t)
	c.queue(t, "ws-c", "c1")
	c.runner.script(c.dirs["ws-b"], failingSuiteRun)
	if err := receiveWithin(t, admitAsync(c.h, context.Background()), "ws-b's admission ending"); err != nil {
		t.Fatalf("ws-b's admission ended on: %v", err)
	}
	c.settle(t, "ws-b's step aside settling")
	if entry, _ := c.entryOf(t, "ws-b"); entry.Slot != wsm.MergeSlotAwaitingFixingTests {
		t.Fatalf("ws-b = %+v, want it awaiting the fixing slot", entry)
	}
	if n := c.prompts("ws-b", fixOrigin); n != 0 {
		t.Fatalf("ws-b got %d fixing prompts while it awaited the slot, want none", n)
	}

	// Act
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("returnRemediated: %v", err)
	}

	// Assert
	if entry, _ := c.entryOf(t, "ws-b"); entry.Slot != wsm.MergeSlotFixingTests {
		t.Fatalf("ws-b = %+v, want it granted the fixing slot by the return", entry)
	}
	c.h.o.mu.Lock()
	rem := c.h.o.remediating["ws-b"]
	c.h.o.mu.Unlock()
	if rem == nil || !rem.armed {
		t.Fatalf("ws-b's remediation = %+v, want its slot's wait armed by the grant", rem)
	}
}

func TestAMergeBackInLineHasItsFixingTabSettledAndIsDrawnWaiting(t *testing.T) {
	// Arrange: the failing head stepped aside; ws-b holds the head.
	c := stepAsideScenario(t)

	// Act
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("returnRemediated: %v", err)
	}

	// Assert
	if tab := c.h.feed.lastTabOfKind(TabFixes).GetFixingTests(); tab.GetSettled() == nil {
		t.Fatalf("the fixing-tests tab = %v, want it settled once the merge is back in line", tab)
	}
	if facts := c.h.footer.lastFor(theWorkspace); facts.State != StateQueued || facts.Step != footer.StepEnqueued {
		t.Fatalf("footer = %+v, want the merge drawn waiting in line", facts)
	}
	stored, _, err := c.h.db.MergeProgressOf(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("MergeProgressOf: %v", err)
	}
	doc, err := decodeProgress(stored)
	if err != nil {
		t.Fatalf("decodeProgress: %v", err)
	}
	if !doc.BackInLine || doc.Brief != "" || doc.Fixing.Span.EndedMS == 0 {
		t.Fatalf("record = back %v brief %q fixing %+v, want it back in line, its brief spent, its fixing settled", doc.BackInLine, doc.Brief, doc.Fixing.Span)
	}
	if !c.recorded("info", opRemediate, "the merge is back in line: its repairing tab is settled and its strip reads waiting until it heads the queue") {
		t.Fatal("the redraw back in line was not recorded")
	}
}

func TestAConflictStepAsideBackInLineHasItsResolvingTabSettled(t *testing.T) {
	// Arrange
	c := conflictAsideScenario(t)

	// Act
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("returnRemediated: %v", err)
	}

	// Assert
	if tab := c.h.feed.lastTabOfKind(TabConflicts).GetResolvingConflicts(); tab.GetSettled() == nil {
		t.Fatalf("the resolving-conflicts tab = %v, want it settled once the merge is back in line", tab)
	}
	if res := c.h.feed.lastResolution(); res.GetSettled() == nil {
		t.Fatalf("the resolution subtab = %v, want it settled", res)
	}
}

func TestARestartDrawsAMergeBackInLineWaiting(t *testing.T) {
	// Arrange: the head stepped aside and came back in line; the daemon
	// restarts before it heads the queue.
	c := stepAsideScenario(t)
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("returnRemediated: %v", err)
	}
	c.h.restart(t)
	c.wire(c.h.o)

	// Act
	if err := c.h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert
	if facts := c.h.footer.lastFor(theWorkspace); facts.State != StateQueued || facts.Step != footer.StepEnqueued {
		t.Fatalf("footer after the restart = %+v, want the merge drawn waiting, never fixing tests", facts)
	}
	if _, ok := dlog.FindRecord(c.h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.merge.recover",
		Message: "a merge back in line from its repair is taken back waiting; it resumes at the step it recorded once it heads the queue"}); !ok {
		t.Fatal("the take-back back in line was not recorded")
	}
}

func TestRecoverRemediatingRefusesAnEntryNamingNoRepair(t *testing.T) {
	// Arrange
	h := newHarness(t)
	entry := remediatingEntry(theWorkspace, wsm.MergeAsideNone, wsm.MergeSlotNone)

	// Act
	err := h.o.recoverRemediating(context.Background(), h.repoKey(), entry)

	// Assert
	if err == nil {
		t.Fatal("the take-back of an entry naming no repair was accepted")
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediate,
		Message: "a merge remediating out of line names no repair on its entry; it is not taken back"})
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["error"] == nil {
		t.Fatalf("ERROR record = %+v (found %v), want the workspace and the error", rec, ok)
	}
}

func TestRecoverRemediatingRefusesARecordOfAnotherRepair(t *testing.T) {
	// Arrange: the entry fixes tests; its record waits on a conflict.
	h := newHarness(t)
	enqueue(t, h)
	ctx := context.Background()
	if _, err := h.db.AcquireLeaseAs(ctx, theWorkspace, deadLease, wsm.HolderMerge, wsm.PolicyHold); err != nil {
		t.Fatalf("taking the lease: %v", err)
	}
	putProgressAt(t, h, theWorkspace, TabConflicts)
	entry := remediatingEntry(theWorkspace, wsm.MergeAsideTests, wsm.MergeSlotFixingTests)

	// Act
	err := h.o.recoverRemediating(ctx, h.repoKey(), entry)

	// Assert
	if err == nil {
		t.Fatal("the take-back of a record of another repair was accepted")
	}
	if !loggedLike(h, logLike{level: "error", op: opRemediate, text: "waits on no repairing turn of its repair"}) {
		t.Fatal("the disagreement was not recorded at ERROR")
	}
}

func TestRecoverRemediatingRegistersAWaitingMergesConflictWithNoRecord(t *testing.T) {
	// Arrange
	h := resumeWorld(t, []wsm.MergeQueueEntry{remediatingEntry("ws-c", wsm.MergeAsideWaitingConflicts, wsm.MergeSlotAwaitingResolvingConflicts)})
	entry := h.db.queues[h.repoKey()][0]

	// Act
	err := h.o.recoverRemediating(context.Background(), h.repoKey(), entry)

	// Assert
	if err != nil {
		t.Fatalf("recoverRemediating: %v", err)
	}
	h.o.mu.Lock()
	rem := h.o.remediating["ws-c"]
	h.o.mu.Unlock()
	if rem == nil || !rem.waiting || rem.armed {
		t.Fatalf("remediation = %+v, want a waiting merge's conflict registered, unarmed while it awaits the slot", rem)
	}
}

func TestAStepAsideTheStoreRefusesRepairsInPlace(t *testing.T) {
	// Arrange: the head fails with ws-b behind it; the store refuses the step
	// aside.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	c.runner.script(c.dirs[theWorkspace], failingSuiteRun)
	c.h.db.mu.Lock()
	c.h.db.stepAsideErr = errors.New("the store is unwell")
	c.h.db.mu.Unlock()

	// Act
	c.drainUntil(t, "the head landing", func() bool { return len(c.git.landedOwn()) == 1 })

	// Assert
	if got := c.git.landedOwn(); len(got[0]) != 1 || got[0][0] != "a1" {
		t.Fatalf("first landing brought %v, want a1 fixed at the head", got)
	}
	if n := c.prompts(theWorkspace, fixOrigin); n != 1 {
		t.Fatalf("fixing prompts = %d, want the brief handed its session in place", n)
	}
	if !c.recorded("error", opRemediate, "could not take the merge out of line to step aside; it repairs where it stands") {
		t.Fatal("the refused step aside was not recorded at ERROR")
	}
	c.drain(t)
}

func TestTheGateFailsHardForARunNotHoldingTheTestingSlot(t *testing.T) {
	// Arrange: a run that does not head its queue.
	h := newHarness(t)
	r := failingTestsRun(t, h)
	round := r.openTab(context.Background(), TabTests)

	// Act
	panicked := func() (recovered any) {
		defer func() { recovered = recover() }()
		_, _ = r.gate(context.Background(), round, "tip", "head")
		return nil
	}()

	// Assert
	if panicked == nil {
		t.Fatal("the gate of a run not holding the testing slot ran")
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.slot",
		Message: "a merge that does not hold its queue's testing slot reached the test gate"})
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["round"] == nil {
		t.Fatalf("ERROR record = %+v (found %v), want the workspace and the round", rec, ok)
	}
}

func TestRosterRepair(t *testing.T) {
	cases := []struct {
		slot wsm.MergeSlot
		want sidebar.QueueRepair
	}{
		{slot: wsm.MergeSlotNone, want: sidebar.RepairNone},
		{slot: wsm.MergeSlotWaiting, want: sidebar.RepairNone},
		{slot: wsm.MergeSlotHead, want: sidebar.RepairNone},
		{slot: wsm.MergeSlotFixingTests, want: sidebar.RepairFixingTests},
		{slot: wsm.MergeSlotResolvingConflicts, want: sidebar.RepairResolvingConflicts},
		{slot: wsm.MergeSlotAwaitingFixingTests, want: sidebar.RepairAwaitingFixingTests},
		{slot: wsm.MergeSlotAwaitingResolvingConflicts, want: sidebar.RepairAwaitingResolvingConflicts},
	}
	for _, tc := range cases {
		t.Run(tc.slot.String(), func(t *testing.T) {
			// Act
			got := rosterRepair(tc.slot)

			// Assert
			if got != tc.want {
				t.Fatalf("rosterRepair(%s) = %s, want %s", tc.slot, got, tc.want)
			}
		})
	}
}

func TestAReattachedTurnThatNeverReachedTheSessionNeedsItsText(t *testing.T) {
	// Arrange: a record naming a turn no store knows, with no text.
	h := newHarness(t)

	// Act
	_, err := h.o.reattachTurn(context.Background(), theWorkspace, dlog.Context{}, "turn-unknown", "", fixOrigin)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "carries no text to submit") {
		t.Fatalf("reattachTurn = %v, want the missing text refused", err)
	}
	h.queue.mu.Lock()
	submitted := len(h.queue.submissions)
	h.queue.mu.Unlock()
	if submitted != 0 {
		t.Fatalf("%d prompts were submitted, want none", submitted)
	}
}

// lastFor reports the most recent facts set on WS's footer.
func (f *fakeFooter) lastFor(ws ids.WorkspaceID) footer.MergeFacts {
	f.mu.Lock()
	defer f.mu.Unlock()
	for i := len(f.facts) - 1; i >= 0; i-- {
		if f.told[i] == ws {
			return f.facts[i]
		}
	}
	return footer.MergeFacts{}
}

// countRecords counts the records at LEVEL of OP whose message is MESSAGE.
func countRecords(h *harness, level, op, message string) int {
	n := 0
	for _, rec := range h.logs.Records() {
		if rec.Level == level && rec.Operation == op && rec.Message == message {
			n++
		}
	}
	return n
}

// fmtAny renders a recovered panic value.
func fmtAny(v any) string { return fmt.Sprint(v) }

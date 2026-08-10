package sessioncontroller

import (
	"errors"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// A BOUNCE MUST NOT SILENTLY THROW AWAY THE TURN IT LANDS ON.
//
// These pin the TEARDOWN half of that: which stops owe the user a resumption,
// that the record is durable before the interrupt is delivered, and that a
// teardown which cannot record one still tears down while saying so.

// newResumptionRig is newTurnStopRig with a receipt store wired, since the
// resumption is durable evidence and there is nowhere to put it otherwise.
func newResumptionRig(t *testing.T) (*Manager, *fakeApplier, *fakeReceiptStore, *logCapture) {
	t.Helper()
	receipts := newFakeReceiptStore()
	applier := &fakeApplier{}
	cl := &logCapture{}
	m, err := New(Config{
		Logf:              cl.logf,
		Push:              &fakePusher{},
		Progress:          &fakeProgress{},
		SSM:               applier,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   emptyTurnAccountingStore{},
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		PromptReceipts:    receipts,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	applier.setCurrent("ws", thinkingState())
	return m, applier, receipts, cl
}

func TestABounceInterruptingALiveTurnRecordsAResumption(t *testing.T) {
	// Arrange — a daemon shutdown in stop-shims mode is the canonical bounce:
	// the shim must die, and the turn it was running is still wanted.
	m, _, receipts, _ := newResumptionRig(t)
	client := &stubInterrupter{outcome: corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED}

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "t-1", client, nil)

	// Assert.
	owed := receipts.owedResumptions("ws")
	if len(owed) != 1 {
		t.Fatalf("owed = %+v, want one resumption for the interrupted turn", owed)
	}
	if owed[0].TurnID != "t-1" {
		t.Fatalf("turn id = %q, want the interrupted turn named on the record", owed[0].TurnID)
	}
}

func TestTheResumptionIsRecordedBeforeTheInterruptIsDelivered(t *testing.T) {
	// Arrange — the window between the two is a window in which a crash loses
	// the user's work, so the ORDER is the guarantee, not an implementation
	// detail. The fake records both on one call log.
	m, _, receipts, _ := newResumptionRig(t)
	client := &stubInterrupter{outcome: corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED}

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "t-1", client, nil)

	// Assert.
	calls := receipts.callLog()
	if len(calls) == 0 || !strings.HasPrefix(calls[0], "record-resumption:") {
		t.Fatalf("calls = %v, want the resumption recorded first", calls)
	}
	if client.calls != 1 {
		t.Fatalf("interrupt calls = %d, want the interrupt to follow the record", client.calls)
	}
}

func TestAScheduledDrainOwesAResumptionToo(t *testing.T) {
	// Arrange — a deploy's scheduled drain is the same displacement as an
	// ordinary shutdown, arriving through a different door.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDrainExecution(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	if len(receipts.owedResumptions("ws")) != 1 {
		t.Fatal("a scheduled drain's interrupted turn is owed a resumption")
	}
}

func TestAHardRestartOwesAResumption(t *testing.T) {
	// Arrange.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseHardRestartLive(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	if len(receipts.owedResumptions("ws")) != 1 {
		t.Fatal("an explicit hard restart's interrupted turn is owed a resumption")
	}
}

func TestAnIdleSweepOwesNoResumption(t *testing.T) {
	// Arrange — the sweeper reaped a workspace nobody has touched in an hour.
	// Re-driving that turn would restart work the system was right to stop.
	m, _, receipts, cl := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseHibernateIdleSweep(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	if len(receipts.owedResumptions("ws")) != 0 {
		t.Fatal("an idle sweep must not owe a resumption")
	}
	if !cl.contains("teardown turn resumption NOT OWED") {
		t.Fatalf("missing the canonical not-owed record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestAUserForcedHibernationOwesNoResumption(t *testing.T) {
	// Arrange — the user asked for the session to stand down.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseHibernateForced(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	if len(receipts.owedResumptions("ws")) != 0 {
		t.Fatal("a user-forced hibernation must not owe a resumption")
	}
}

func TestAMergedTeardownOwesNoResumption(t *testing.T) {
	// Arrange — the workspace merged and the session stood down with it.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseMergedTeardown(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	if len(receipts.owedResumptions("ws")) != 0 {
		t.Fatal("a merged teardown must not owe a resumption")
	}
}

func TestASupersededRecordOwesNoResumptionEvenUnderABounceCause(t *testing.T) {
	// Arrange — the refinement answers for what the daemon FOUND. A
	// replacement session drives this workspace now, and its turn is not this
	// retired record's to resume.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown().supersededRecord(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	if len(receipts.owedResumptions("ws")) != 0 {
		t.Fatal("a superseded record must not owe a resumption its successor owns")
	}
}

func TestAnIdleWorkspaceRecordsNoResumptionUnderABounce(t *testing.T) {
	// Arrange — nothing was interrupted, so nothing is owed. The drain returns
	// before the record for the same reason it skips the interrupt.
	m, applier, receipts, _ := newResumptionRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_DONE})

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "", &stubInterrupter{}, nil)

	// Assert.
	if len(receipts.owedResumptions("ws")) != 0 {
		t.Fatal("an idle workspace has no interrupted turn to owe a resumption for")
	}
}

func TestAnUnnameableTurnStillRecordsAResumption(t *testing.T) {
	// Arrange — an adopted turn (a shim that outlived the previous daemon and
	// reattached mid-turn) is unambiguously the user's work. Refusing to record
	// it because this process never saw it begin would abandon exactly the
	// turns a bounce is most likely to land on.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "", &stubInterrupter{}, nil)

	// Assert.
	owed := receipts.owedResumptions("ws")
	if len(owed) != 1 {
		t.Fatalf("owed = %+v, want a resumption for the unnameable turn", owed)
	}
	if owed[0].TurnID != "" {
		t.Fatalf("turn id = %q, want the record to name nothing rather than guess", owed[0].TurnID)
	}
}

func TestTwoTeardownsOfTheSameTurnOweOneResumption(t *testing.T) {
	// Arrange — exactly-once starts at the point the record is CREATED: a
	// retried bounce must not accumulate one owed resumption per attempt.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "t-1", &stubInterrupter{}, nil)
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	if owed := receipts.owedResumptions("ws"); len(owed) != 1 {
		t.Fatalf("owed = %+v, want one resumption after two teardowns of one turn", owed)
	}
}

func TestATeardownThatCannotRecordItsResumptionStillTearsDownLoudly(t *testing.T) {
	// Arrange — refusing to shut down because a bookkeeping row would not
	// write trades a lost turn for a stuck deploy. But the loss is never
	// silent.
	m, _, receipts, cl := newResumptionRig(t)
	receipts.resumptionRecordErr = errors.New("state store is read-only")
	client := &stubInterrupter{outcome: corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED}

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "t-1", client, nil)

	// Assert.
	if client.calls != 1 {
		t.Fatalf("interrupt calls = %d, want the teardown to proceed", client.calls)
	}
	if !cl.contains("teardown turn resumption RECORD FAILED") || !cl.contains("state store is read-only") {
		t.Fatalf("missing the canonical record-failure line; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestABounceWithNoReceiptStoreSaysTheTurnWillNotBeReDriven(t *testing.T) {
	// Arrange — a manager with nowhere to record the resumption must say so
	// rather than let the turn vanish unremarked.
	m, applier, _, cl := newTurnStopRigWithoutReceipts(t)
	applier.setCurrent("ws", thinkingState())

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	if !cl.contains("teardown turn resumption NOT RECORDED") {
		t.Fatalf("missing the canonical no-store record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// newTurnStopRigWithoutReceipts is newTurnStopRig, named for what this file
// asks of it: a manager with NO PromptReceiptStore wired.
func newTurnStopRigWithoutReceipts(t *testing.T) (*Manager, *fakeApplier, *fakeSpawner, *logCapture) {
	t.Helper()
	m, spawner, applier, cl := newTurnStopRig(t)
	return m, applier, spawner, cl
}

func TestTheResumptionInstructionDoesNotRestateTheUsersPrompt(t *testing.T) {
	// Arrange — the vendor session is resumed with its full transcript, so the
	// interrupted request is already in context verbatim. Repeating it would
	// put a second copy of the user's words in the conversation.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	owed := receipts.owedResumptions("ws")
	if len(owed) != 1 {
		t.Fatalf("owed = %+v, want the one resumption", owed)
	}
	if !strings.Contains(owed[0].Text, "interrupted") || !strings.Contains(owed[0].Text, "Continue") {
		t.Fatalf("text = %q, want an instruction naming the situation", owed[0].Text)
	}
}

func TestAResumptionRequestIDIsMarkedInternal(t *testing.T) {
	// Arrange — the prefix is what every later layer keys "this submit is not
	// the user's" on, so it is minted at teardown rather than inferred later.
	m, _, receipts, _ := newResumptionRig(t)

	// Act.
	m.drainLiveTurnForStop("ws", "s1", StopCauseDaemonShutdown(), "t-1", &stubInterrupter{}, nil)

	// Assert.
	owed := receipts.owedResumptions("ws")
	if len(owed) != 1 || !strings.HasPrefix(owed[0].RequestID, resumptionRequestIDPrefix) {
		t.Fatalf("owed = %+v, want a request id marked with %q", owed, resumptionRequestIDPrefix)
	}
}

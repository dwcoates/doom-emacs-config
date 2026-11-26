package promptqueue

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// blockPasses scripts every pass of the harness's gate to wait for the test:
// each call is announced on the first channel and answers what the test sends
// on the second.
func blockPasses(h *harness) (<-chan struct{}, chan<- gatePassAnswer) {
	called := make(chan struct{}, 8)
	answer := make(chan gatePassAnswer)
	h.gatePass.run = func(ids.WorkspaceID) (bool, error) {
		called <- struct{}{}
		a := <-answer
		return a.up, a.err
	}
	return called, answer
}

// gatePassAnswer is what a scripted pass answers.
type gatePassAnswer struct {
	up  bool
	err error
}

// passesOfTheWorkspace fails the test unless WANT passes of theWorkspace's
// gate were asked for, once every pass started has ended.
func passesOfTheWorkspace(t *testing.T, h *harness, want int) {
	t.Helper()
	h.q.gatePasses.Wait()
	if got := h.gatePass.passed(); len(got) != want || (want > 0 && slices.ContainsFunc(got, func(ws ids.WorkspaceID) bool { return ws != theWorkspace })) {
		t.Fatalf("passes = %v, want %d of %s", got, want, theWorkspace)
	}
}

func TestAnAgentsPromptAtAColdGateStartsOnePassAndIsDeliveredAfterIt(t *testing.T) {
	// Arrange: the gate stands, and its pass waits for the test.
	h := newHarness(t)
	h.coldGate = gateDetail
	called, answer := blockPasses(h)

	// Act
	got, err := h.q.Submit(context.Background(), agentSubmission("t1", "hello"))

	// Assert: it waits at the gate with the pass running, and nothing went.
	if err != nil || got.Waiting == nil || *got.Waiting != wsm.WaitColdGate {
		t.Fatalf("Submit = (%+v, %v), want t1 waiting at the gate", got, err)
	}
	<-called
	waitsUnder(t, h, "t1", wsm.WaitColdGate)
	if len(h.sender.started()) != 0 {
		t.Fatalf("started = %v, want nothing before the pass ends", h.sender.started())
	}

	// Act: the pass answers the gate (what the verbs do), and brings the
	// session up.
	h.coldGate = ""
	if err := h.q.ColdGateAnswered(context.Background(), theWorkspace, GateKeep); err != nil {
		t.Fatalf("ColdGateAnswered: %v", err)
	}
	answer <- gatePassAnswer{up: true}

	// Assert
	passesOfTheWorkspace(t, h, 1)
	if !started(h, "t1") {
		t.Fatalf("started = %v, want t1 delivered once the gate was passed", h.sender.started())
	}
	noWaits(t, h)
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opGatePass,
		Message: "an agent's prompt waits at the cold gate; the gate is passed at once by a Haiku compaction of the whole context"}); !ok {
		t.Fatalf("records = %+v, want the pass's start recorded", h.log.Records())
	}
}

func TestTwoAgentsPromptsAtAColdGateStartOnePass(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.coldGate = gateDetail
	called, answer := blockPasses(h)

	// Act
	for _, turn := range []ids.TurnID{"t1", "t2"} {
		if _, err := h.q.Submit(context.Background(), agentSubmission(turn, "hello")); err != nil {
			t.Fatalf("Submit(%s): %v", turn, err)
		}
	}
	<-called
	answer <- gatePassAnswer{up: true}

	// Assert
	passesOfTheWorkspace(t, h, 1)
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opGatePass,
		Message: "an agent's prompt waits at the cold gate a pass is already compacting; it joins that pass"}); !ok {
		t.Fatalf("records = %+v, want t2 joining the running pass", h.log.Records())
	}
}

func TestOnlyAnAgentsPromptPassesTheColdGate(t *testing.T) {
	tests := []struct {
		name string
		sub  Submission
		// wantRefused is the gate's refusal; otherwise the prompt waits there.
		wantRefused bool
	}{
		{name: "the user's own prompt is refused", sub: submission("t1", "the owner's words"), wantRefused: true},
		{name: "a daemon notice waits out the gate", sub: notice("t1")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.coldGate = gateDetail

			// Act
			_, err := h.q.Submit(context.Background(), tt.sub)

			// Assert
			var refusal *ColdGateRefusal
			if refused := errors.As(err, &refusal); refused != tt.wantRefused {
				t.Fatalf("Submit = %v, want refused %v", err, tt.wantRefused)
			}
			passesOfTheWorkspace(t, h, 0)
		})
	}
}

func TestAnAgentsPromptWhoseRevivalParksAtTheGateWaitsThereAndPassesIt(t *testing.T) {
	// Arrange: the workspace has no shim, and the prompt's revival parks
	// at the cold gate (sidebar-draws-only-changes, 2026-11-25: the prompt
	// stayed on the session's wait, so nothing passed the gate and the
	// gate's answer found nothing to settle).
	h := newHarness(t)
	h.noSession = true
	h.reviveHook = func() { parkAtGate(h) }

	// Act
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.waitRevivals()

	// Assert
	waitsUnder(t, h, "t1", wsm.WaitColdGate)
	passesOfTheWorkspace(t, h, 1)
}

func TestAHandoverBetweenTheWaitAndTheAnswerKeepsThePromptAtTheGate(t *testing.T) {
	// Arrange: t1 waits at the predecessor's gate, whose pass the transfer
	// abandoned.
	h := newHarness(t)
	h.coldGate = gateDetail
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	passesOfTheWorkspace(t, h, 1)

	// Act: the move's lease change on the predecessor, then the successor's
	// boot over the same store, which raises the gate again on its revival.
	h.q.driveArrivals(context.Background(), theWorkspace, arrivalLeaseChanged, h.log.Global())
	waitsUnder(t, h, "t1", wsm.WaitColdGate)
	successor, err := newQueue(h.q.deps)
	if err != nil {
		t.Fatalf("newQueue: %v", err)
	}
	h.q = successor
	h.coldGate = ""
	h.noSession = true
	h.reviveHook = func() { parkAtGate(h) }
	if err := successor.DriveArrivalWaits(context.Background()); err != nil {
		t.Fatalf("DriveArrivalWaits: %v", err)
	}
	h.waitRevivals()

	// Assert: the prompt waits at the successor's gate, which it passes.
	waitsUnder(t, h, "t1", wsm.WaitColdGate)
	passesOfTheWorkspace(t, h, 2)

	// Act: the successor's answer settles it.
	answerGate(h)
	if err := successor.ColdGateAnswered(context.Background(), theWorkspace, GateKeep); err != nil {
		t.Fatalf("ColdGateAnswered: %v", err)
	}

	// Assert
	if !started(h, "t1") {
		t.Fatalf("started = %v, want t1 delivered by the successor's answer", h.sender.started())
	}
	noWaits(t, h)
}

func TestAFailedPassTellsTheSenderAndLeavesThePromptAtTheGate(t *testing.T) {
	compactionFailed := errors.New("the compaction's summarizer refused the context")
	tests := []struct {
		name string
		// answerDrove puts t1 on the session's wait before the pass fails, as
		// the gate's answer drives it toward the session it brings up.
		answerDrove bool
	}{
		{name: "the pass failed before its answer was taken"},
		{name: "the pass's answer drove the prompt on before its re-open failed", answerDrove: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			parkAtGate(h)
			h.gatePass.run = func(ids.WorkspaceID) (bool, error) {
				if tt.answerDrove {
					if err := h.db.PutArrivalWait(context.Background(), wsm.ArrivalWait{
						Workspace: theWorkspace, Turn: "t1", Said: userSaid("hello"), Origin: agentSubmission("t1", "").Origin.String(),
						SentBy: agentSubmission("t1", "").SentBy, QueuedAt: instant, Reason: wsm.WaitSession, Why: "the answer brings the session up",
					}); err != nil {
						t.Errorf("PutArrivalWait: %v", err)
					}
				}
				return false, compactionFailed
			}

			// Act
			if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "hello")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			passesOfTheWorkspace(t, h, 1)

			// Assert: held at the gate, its sender told, the failure logged.
			waitsUnder(t, h, "t1", wsm.WaitColdGate)
			want := []toldPassFailed{{sender: theSender, ws: theWorkspace, cause: compactionFailed.Error()}}
			if got := h.gatePass.toldFailed(); !slices.Equal(got, want) {
				t.Fatalf("told = %+v, want %+v", got, want)
			}
			record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opGatePass,
				Message: "the pass of the cold gate an agent's prompt started failed; the prompts wait at the gate for the user's answer"})
			if !ok || record.Context["cause"] != compactionFailed.Error() {
				t.Fatalf("records = %+v, want the failure at ERROR with its cause", h.log.Records())
			}

			// Act: the prompt meets the gate again.
			h.q.driveArrivals(context.Background(), theWorkspace, arrivalBoot, h.log.Global())

			// Assert: it passed the gate once, so it starts no second pass.
			passesOfTheWorkspace(t, h, 1)
			waitsUnder(t, h, "t1", wsm.WaitColdGate)
		})
	}
}

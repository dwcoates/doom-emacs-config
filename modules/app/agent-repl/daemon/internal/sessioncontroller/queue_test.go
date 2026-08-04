package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"
)

// --- harness ----------------------------------------------------------------

// fakeClassifier returns a canned verdict, and records what it was asked.
// Nothing in this package's tests ever reaches a real model: the Manager only
// knows the Classifier interface, and this is what it is given.
type fakeClassifier struct {
	mu      sync.Mutex
	res     ClassifyResult
	err     error
	reqs    []ClassifyRequest
	release chan struct{} // when non-nil, Classify blocks until it is closed
}

func (f *fakeClassifier) Classify(_ context.Context, req ClassifyRequest) (ClassifyResult, error) {
	f.mu.Lock()
	f.reqs = append(f.reqs, req)
	rel := f.release
	f.mu.Unlock()
	notifyTestActivity()
	if rel != nil {
		<-rel
	}
	return f.res, f.err
}

func (f *fakeClassifier) requests() []ClassifyRequest {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]ClassifyRequest(nil), f.reqs...)
}

// failingClient is a fakeClient whose SubmitPrompt fails, for the
// delivery-failure path.
type failingClient struct {
	fakeClient
	err error
}

func (c *failingClient) SubmitPrompt(_ context.Context, _, _, _ string) error { return c.err }
func (c *failingClient) SetModel(_ context.Context, _ string) (string, error) { return "", c.err }

// queueHarness is one workspace's controller plus the doubles around it.
type queueHarness struct {
	t       *testing.T
	m       *Manager
	push    *fakePusher
	client  *fakeClient
	cls     *fakeClassifier
	reg     *fakeRegistrar
	applier *fakeApplier
	prog    *fakeProgress
	// newestClient reports the most recently constructed fake shim client, so a
	// harness wired after construction can bind to the one its bring-up made.
	newestClient func() *fakeClient
}

// newQueueHarness brings a session up for workspace "ws" and returns the
// harness. classifier may be nil (the unconfigured case).
func newQueueHarness(t *testing.T, cls *fakeClassifier) *queueHarness {
	t.Helper()
	return newQueueHarnessWithPusher(t, cls, nil, nil)
}

// newQueueHarnessWithPusher is newQueueHarness with two injection points:
//
//   - wrap decorates the recording fakePusher, so a test can act while the
//     manager mutex is RELEASED across a push (that release is the window the
//     interject race lives in, and a decorator is the only way to be inside it
//     deterministically);
//   - logf captures the daemon log, for tests asserting on a loud line.
//
// Both may be nil, which is the plain harness.
func newQueueHarnessWithPusher(t *testing.T, cls *fakeClassifier, wrap func(*fakePusher) Pusher, logf func(string, ...any)) *queueHarness {
	t.Helper()
	return newQueueHarnessFull(t, cls, wrap, logf, nil)
}

// newQueueHarnessWithHolds is newQueueHarness with the drain lease's durable
// parking ledger wired (shutdownlease_test.go).
func newQueueHarnessWithHolds(t *testing.T, cls *fakeClassifier, holds ShutdownHoldStore, logf func(string, ...any)) *queueHarness {
	t.Helper()
	return newQueueHarnessFull(t, cls, nil, logf, holds)
}

// newQueueHarnessFull is the one constructor the variants above delegate to, so
// a new injection point is added in exactly one place.
func newQueueHarnessFull(t *testing.T, cls *fakeClassifier, wrap func(*fakePusher) Pusher, logf func(string, ...any), holds ShutdownHoldStore) *queueHarness {
	t.Helper()
	return buildQueueHarness(t, cls, wrap, logf, holds, true)
}

// newQueueHarnessUnwired is the harness with the fleet left EMPTY: no session
// is brought up, so m.byWS has no controller for "ws".
//
// It is what the boot materialization needs to be tested against. That path
// runs before any session has wired, and a harness that had already wired one
// could only ever exercise the case the defect was NOT in.
func newQueueHarnessUnwired(t *testing.T, holds ShutdownHoldStore, logf func(string, ...any)) *queueHarness {
	t.Helper()
	return buildQueueHarness(t, nil, nil, logf, holds, false)
}

// buildQueueHarness constructs the manager and its doubles, bringing "ws" up
// only when wire is set.
func buildQueueHarness(t *testing.T, cls *fakeClassifier, wrap func(*fakePusher) Pusher, logf func(string, ...any), holds ShutdownHoldStore, wire bool) *queueHarness {
	t.Helper()
	rec := &fakePusher{}
	var push Pusher = rec
	if wrap != nil {
		push = wrap(rec)
	}
	reg := &fakeRegistrar{}
	applier := &fakeApplier{}
	prog := &fakeProgress{}
	var mu sync.Mutex
	var last *fakeClient
	cfg := Config{
		Push:              push,
		Progress:          prog,
		SSM:               applier,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		Registrar:         reg,
		ProtocolVersion:   "1",
		ShutdownHolds:     holds,
		Logf:              logf,
		now:               func() int64 { return 1000 },
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient: func(c shimclient.Config) sessionClient {
			fc := &fakeClient{cfg: c}
			mu.Lock()
			last = fc
			mu.Unlock()
			return fc
		},
	}
	if cls != nil {
		cfg.Classifier = cls
	}
	m, err := New(cfg)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	h := &queueHarness{t: t, m: m, push: rec, cls: cls, reg: reg, applier: applier, prog: prog}
	h.newestClient = func() *fakeClient {
		mu.Lock()
		defer mu.Unlock()
		return last
	}
	if wire {
		h.wire()
	}
	return h
}

// wire brings "ws" up and rebinds the harness to the client that came with it.
func (h *queueHarness) wire() {
	h.t.Helper()
	if err := h.m.Ensure("ws"); err != nil {
		h.t.Fatalf("Ensure: %v", err)
	}
	h.client = h.newestClient()
}

// controller returns the live session controller for "ws".
func (h *queueHarness) controller() *sessionController {
	h.t.Helper()
	d, err := h.m.existing("ws")
	if err != nil {
		h.t.Fatalf("existing: %v", err)
	}
	return d
}

// turn drives an observed turn boundary through the same callback the shim
// stream drives, so the queue is exercised on its real trigger.
func (h *queueHarness) turn(active bool) {
	h.m.onTurnBoundary(h.controller(), active)
}

// submit submits a prompt for "ws".
func (h *queueHarness) submit(text string) error {
	return h.m.SubmitPrompt(context.Background(), "ws", "", text, "")
}

// entries returns VALUE copies of the current queue entries.
//
// Copies, not pointers: the classify and deliver goroutines mutate entries
// under the manager mutex, so handing a test a pointer it then reads outside
// the lock would be a real race (and the race detector rightly says so). The
// controller is also resolved BEFORE the lock, since existing() takes the same
// mutex.
func (h *queueHarness) entries() []queueEntry {
	d := h.controller()
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	out := make([]queueEntry, 0, len(d.queue.entries))
	for _, e := range d.queue.entries {
		out = append(out, *e)
	}
	return out
}

// activitySignal is the package's test-side WAKEUP: a broadcast every fake
// fires after it records something a test can observe.
//
// It replaces a sleep-poll. The queue's classification and delivery run on
// their own goroutines by design (they must not block the shim read loop), so a
// test has to observe their EFFECT rather than the call — but observing it by
// re-checking every millisecond made the wait a race against a guessed
// interval, and every wait cost real wall-clock time it did not need. A
// broadcast is the same observation with the guess removed: the waiter is woken
// by the very write it is waiting for.
//
// The channel is CLOSED rather than sent on, so one notification wakes every
// waiter and no fake can block on a waiter that has gone away.
type activitySignal struct {
	mu sync.Mutex
	ch chan struct{}
}

// testActivity is the one signal the whole package's fakes and waiters share.
var testActivity = &activitySignal{ch: make(chan struct{})}

// wake returns the channel closed by the NEXT notification. It must be taken
// BEFORE the condition is evaluated: taken after, a write landing between the
// check and the wait would be missed and the waiter would sleep through the
// very thing it is waiting for.
func (s *activitySignal) wake() <-chan struct{} {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.ch
}

// notify wakes every waiter and arms the next round.
func (s *activitySignal) notify() {
	s.mu.Lock()
	defer s.mu.Unlock()
	close(s.ch)
	s.ch = make(chan struct{})
}

// notifyTestActivity is what a fake calls, after recording, to say that
// something a test may be waiting on has moved.
func notifyTestActivity() { testActivity.notify() }

// waitFor waits until cond holds, woken by the fakes rather than by a timer.
// The bound that remains is a DEADLINE, not a poll interval: it exists so a
// condition that never becomes true fails the test instead of hanging it.
func waitFor(t *testing.T, what string, cond func() bool) {
	t.Helper()
	deadline := time.NewTimer(2 * time.Second)
	defer deadline.Stop()
	for {
		wake := testActivity.wake()
		if cond() {
			return
		}
		select {
		case <-wake:
		case <-deadline.C:
			t.Fatalf("timed out waiting for %s", what)
		}
	}
}

// waitEntryClass waits until the single queued entry reaches cls.
func (h *queueHarness) waitEntryClass(cls frontendv1.QueueClassification) {
	h.t.Helper()
	waitFor(h.t, "classification "+cls.String(), func() bool {
		es := h.entries()
		return len(es) == 1 && es[0].classification == cls
	})
}

// --- interception -----------------------------------------------------------

func TestSubmitWhileIdleIsForwardedNotQueued(t *testing.T) {
	// Arrange — no turn running.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submit("hello"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert — straight through, and no queue formed at all.
	if got := h.client.promptTexts(); len(got) != 1 || got[0] != "hello" {
		t.Fatalf("prompts = %v, want [hello]", got)
	}
	if len(h.entries()) != 0 {
		t.Fatalf("an idle submit must not queue")
	}
}

func TestSubmitDuringTurnIsQueuedNotForwarded(t *testing.T) {
	// Arrange — a turn is running.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	if err := h.submit("later"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert — held, and NOT sent to the shim.
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want none forwarded", got)
	}
	if es := h.entries(); len(es) != 1 || es[0].text != "later" {
		t.Fatalf("entries = %+v, want one holding 'later'", es)
	}
}

func TestQueuedSubmitReportsSuccess(t *testing.T) {
	// Arrange — queueing IS acceptance; the command must not fail.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act / Assert.
	if err := h.submit("later"); err != nil {
		t.Fatalf("a queued submit must ack ok, got %v", err)
	}
}

func TestQueuedSubmitPushesAQueueView(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	waitFor(t, "a queue view push", func() bool {
		v := h.push.lastQueue()
		return v != nil && len(v.GetEntries()) == 1
	})
}

func TestQueuedEntryStartsPending(t *testing.T) {
	// Arrange — a classifier that never returns, so the entry stays as queued.
	cls := &fakeClassifier{release: make(chan struct{})}
	h := newQueueHarness(t, cls)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	if es := h.entries(); len(es) != 1 ||
		es[0].classification != frontendv1.QueueClassification_QUEUE_CLASSIFICATION_PENDING {
		t.Fatalf("entries = %+v, want one PENDING", es)
	}
	close(cls.release)
}

func TestQueuedEntryIsPersistedToTheRegistry(t *testing.T) {
	// Arrange — crash honesty: a held prompt is something the user typed that
	// the agent has not seen.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	waitFor(t, "a registry queue write", func() bool {
		h.reg.mu.Lock()
		defer h.reg.mu.Unlock()
		q := h.reg.queued["s1"]
		return len(q) == 1 && q[0].Text == "later"
	})
}

func TestQueueStampsTheQueuedAtClock(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	if got := h.entries()[0].queuedAtMs; got != 1000 {
		t.Fatalf("queued_at_ms = %d, want 1000", got)
	}
}

// --- turn-end drain ---------------------------------------------------------

func TestTurnEndDeliversTheQueuedPrompt(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the delivery", func() bool { return len(h.client.promptTexts()) == 1 })
	if got := h.client.promptTexts()[0]; got != "later" {
		t.Fatalf("delivered %q, want later", got)
	}
}

func TestTurnEndDrainsFIFO(t *testing.T) {
	// Arrange — three held prompts; order must be preserved.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("one")
	_ = h.submit("two")
	_ = h.submit("three")

	// Act — one turn end delivers ONE prompt (the next turn it starts is what
	// eventually ends and delivers the next).
	h.turn(false)

	// Assert.
	waitFor(t, "the first delivery", func() bool { return len(h.client.promptTexts()) == 1 })
	if got := h.client.promptTexts()[0]; got != "one" {
		t.Fatalf("delivered %q first, want one", got)
	}
}

func TestSuccessiveTurnEndsDrainInOrder(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("one")
	_ = h.submit("two")

	// Act.
	h.turn(false)
	waitFor(t, "the first delivery", func() bool { return len(h.client.promptTexts()) == 1 })
	h.turn(false)

	// Assert.
	waitFor(t, "the second delivery", func() bool { return len(h.client.promptTexts()) == 2 })
	if got := h.client.promptTexts(); got[1] != "two" {
		t.Fatalf("second delivery = %q, want two", got[1])
	}
}

func TestTurnEndWithAnEmptyQueueDeliversNothing(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	h.turn(false)

	// Assert.
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want none", got)
	}
}

func TestDeliveredEntryLeavesTheQueue(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the queue to empty", func() bool { return len(h.entries()) == 0 })
}

func TestDeliveryFailureRequeuesTheEntryAsError(t *testing.T) {
	// Arrange — a client whose submit fails. Losing the prompt would lose
	// something the user typed.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")
	d := h.controller()
	h.m.mu.Lock()
	d.client = &failingClient{err: errors.New("shim gone")}
	h.m.mu.Unlock()

	// Act.
	h.turn(false)

	// Assert — still queued, and visibly errored.
	waitFor(t, "the requeue", func() bool {
		es := h.entries()
		return len(es) == 1 &&
			es[0].classification == frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR
	})
	if got := h.entries()[0].text; got != "later" {
		t.Fatalf("requeued text = %q, want later", got)
	}
}

// --- classification ---------------------------------------------------------

func TestHoldVerdictLeavesTheEntryQueued(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{
		Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD,
		Rationale:      "unrelated",
	}}
	h := newQueueHarness(t, cls)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert — HOLD does not interrupt and does not deliver.
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD)
	if h.client.interruptCount() != 0 {
		t.Fatal("a HOLD verdict must not interrupt")
	}
}

func TestHoldVerdictCarriesItsRationale(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{
		Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD,
		Rationale:      "unrelated to the running turn",
	}}
	h := newQueueHarness(t, cls)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD)
	if got := h.entries()[0].rationale; got != "unrelated to the running turn" {
		t.Fatalf("rationale = %q", got)
	}
}

func TestClassifierFailureBecomesErrorNotHold(t *testing.T) {
	// Arrange — the load-bearing case: a failed classification must NOT be
	// silently resolved to a real verdict.
	cls := &fakeClassifier{err: errors.New("both tokens")}
	h := newQueueHarness(t, cls)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR)
}

func TestClassifierFailureSurfacesItsReason(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{err: errors.New("answered with both tokens")}
	h := newQueueHarness(t, cls)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR)
	if got := h.entries()[0].rationale; got != "answered with both tokens" {
		t.Fatalf("rationale = %q, want the classifier's own reason", got)
	}
}

func TestErroredEntryIsStillDeliveredAtTurnEnd(t *testing.T) {
	// Arrange — an unclassifiable prompt is DELAYED, never dropped.
	cls := &fakeClassifier{err: errors.New("nope")}
	h := newQueueHarness(t, cls)
	h.turn(true)
	_ = h.submit("later")
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR)

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the delivery", func() bool { return len(h.client.promptTexts()) == 1 })
}

func TestNoClassifierConfiguredMarksTheEntryError(t *testing.T) {
	// Arrange — with nothing wired, NOTHING decided the entry, which is what
	// ERROR means. PENDING would be a lie (no classifier is running).
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	_ = h.submit("later")

	// Assert.
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR)
}

func TestClassifierIsGivenTheRunningPrompt(t *testing.T) {
	// Arrange — the classifier judges the new prompt AGAINST the running one.
	cls := &fakeClassifier{res: ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD}}
	h := newQueueHarness(t, cls)
	_ = h.submit("the running work") // forwarded while idle; becomes runningText
	h.turn(true)

	// Act.
	_ = h.submit("the new thing")

	// Assert.
	waitFor(t, "the classify call", func() bool { return len(cls.requests()) == 1 })
	req := cls.requests()[0]
	if req.RunningPrompt != "the running work" || req.QueuedPrompt != "the new thing" {
		t.Fatalf("classify request = %+v", req)
	}
}

func TestVerdictForAnAlreadyDeliveredEntryIsMoot(t *testing.T) {
	// Arrange — the turn ends (draining the entry) while the classifier runs.
	cls := &fakeClassifier{
		res:     ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT},
		release: make(chan struct{}),
	}
	h := newQueueHarness(t, cls)
	h.turn(true)
	_ = h.submit("later")
	waitFor(t, "the classify call", func() bool { return len(cls.requests()) == 1 })
	h.turn(false)
	waitFor(t, "the drain", func() bool { return len(h.client.promptTexts()) == 1 })

	// Act — the verdict lands after the entry is gone.
	close(cls.release)

	// Assert — no interrupt fired for a prompt that already reached the agent.
	waitFor(t, "the verdict to settle", func() bool { return len(h.entries()) == 0 })
	if h.client.interruptCount() != 0 {
		t.Fatal("a moot verdict must not interrupt a later turn")
	}
}

// --- interject sequence -----------------------------------------------------

func TestInterjectVerdictInterruptsTheRunningTurn(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT}}
	h := newQueueHarness(t, cls)
	h.turn(true)

	// Act.
	_ = h.submit("stop, wrong file")

	// Assert.
	waitFor(t, "the interrupt", func() bool { return h.client.interruptCount() == 1 })
}

func TestInterjectDoesNotSubmitBeforeTheTurnEnds(t *testing.T) {
	// Arrange — THE race the evented sequence exists to prevent: submitting
	// into a turn that is still tearing down.
	cls := &fakeClassifier{res: ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT}}
	h := newQueueHarness(t, cls)
	h.turn(true)

	// Act.
	_ = h.submit("stop, wrong file")
	waitFor(t, "the interrupt", func() bool { return h.client.interruptCount() == 1 })

	// Assert — interrupted, but NOT yet submitted.
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("submitted %v before TurnEnded", got)
	}
}

func TestInterjectSubmitsOnTheObservedTurnEnd(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT}}
	h := newQueueHarness(t, cls)
	h.turn(true)
	_ = h.submit("stop, wrong file")
	waitFor(t, "the interrupt", func() bool { return h.client.interruptCount() == 1 })

	// Act — the shim reports the turn really ended.
	h.turn(false)

	// Assert.
	waitFor(t, "the delivery", func() bool { return len(h.client.promptTexts()) == 1 })
	if got := h.client.promptTexts()[0]; got != "stop, wrong file" {
		t.Fatalf("delivered %q", got)
	}
}

func TestInterjectJumpsAheadOfAnEarlierHoldEntry(t *testing.T) {
	// Arrange — an entry that interjects is delivered before an earlier FIFO
	// entry, which is the whole point of interjecting.
	cls := &fakeClassifier{res: ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD}}
	h := newQueueHarness(t, cls)
	h.turn(true)
	_ = h.submit("first, unrelated")
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD)
	cls.mu.Lock()
	cls.res = ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT}
	cls.mu.Unlock()
	_ = h.submit("second, urgent")
	waitFor(t, "the interrupt", func() bool { return h.client.interruptCount() == 1 })

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the delivery", func() bool { return len(h.client.promptTexts()) == 1 })
	if got := h.client.promptTexts()[0]; got != "second, urgent" {
		t.Fatalf("delivered %q, want the interjecting entry first", got)
	}
}

func TestInterjectVerdictAfterTheTurnEndedDeliversWithoutInterrupting(t *testing.T) {
	// Arrange — the verdict is moot as an INTERRUPT (nothing is running) but
	// the prompt must still reach the agent.
	cls := &fakeClassifier{
		res:     ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT},
		release: make(chan struct{}),
	}
	h := newQueueHarness(t, cls)
	h.turn(true)
	_ = h.submit("later")
	waitFor(t, "the classify call", func() bool { return len(cls.requests()) == 1 })
	// The turn ends with the entry still classifying, so nothing drains it:
	// force turnActive false WITHOUT draining, by ending after taking the entry
	// out of drain reach is not possible — instead end the turn normally and
	// re-queue is not needed; the drain takes it, so use a second entry.
	d := h.controller()
	h.m.mu.Lock()
	d.turn = turnRecord{}
	h.m.mu.Unlock()

	// Act.
	close(cls.release)

	// Assert — delivered, and no interrupt fired.
	waitFor(t, "the delivery", func() bool { return len(h.client.promptTexts()) == 1 })
	if h.client.interruptCount() != 0 {
		t.Fatal("nothing was running; there was nothing to interrupt")
	}
}

// --- commands ---------------------------------------------------------------

func TestForceRunsTheInterjectSequence(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD}}
	h := newQueueHarness(t, cls)
	h.turn(true)
	_ = h.submit("later")
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD)

	// Act.
	if err := h.m.ForceQueueEntry("ws", h.entries()[0].id); err != nil {
		t.Fatalf("force: %v", err)
	}

	// Assert.
	waitFor(t, "the interrupt", func() bool { return h.client.interruptCount() == 1 })
}

func TestForceDeliversOnTheTurnEnd(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR)
	if err := h.m.ForceQueueEntry("ws", h.entries()[0].id); err != nil {
		t.Fatalf("force: %v", err)
	}
	waitFor(t, "the interrupt", func() bool { return h.client.interruptCount() == 1 })

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the delivery", func() bool { return len(h.client.promptTexts()) == 1 })
}

func TestForceOnAnUnknownEntryErrors(t *testing.T) {
	// Arrange — the user asked for something specific; pretending is worse.
	h := newQueueHarness(t, nil)

	// Act / Assert.
	if err := h.m.ForceQueueEntry("ws", "q_nope"); err == nil {
		t.Fatal("forcing an unknown entry must error")
	}
}

func TestAcceptMarksTheEntryAccepted(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")

	// Act.
	if err := h.m.AcceptQueueEntry("ws", h.entries()[0].id); err != nil {
		t.Fatalf("accept: %v", err)
	}

	// Assert.
	if !h.entries()[0].accepted {
		t.Fatal("accept must record the acknowledgement")
	}
}

func TestAcceptDoesNotDeliverOrInterrupt(t *testing.T) {
	// Arrange — accept is VIEW STATE ONLY.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")

	// Act.
	_ = h.m.AcceptQueueEntry("ws", h.entries()[0].id)

	// Assert.
	if len(h.client.promptTexts()) != 0 || h.client.interruptCount() != 0 {
		t.Fatal("accept must change nothing about delivery")
	}
}

func TestAcceptOnAnUnknownEntryErrors(t *testing.T) {
	// Arrange / Act / Assert.
	h := newQueueHarness(t, nil)
	if err := h.m.AcceptQueueEntry("ws", "q_nope"); err == nil {
		t.Fatal("accepting an unknown entry must error")
	}
}

func TestCancelRemovesTheEntry(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")

	// Act.
	if err := h.m.CancelQueueEntry("ws", h.entries()[0].id); err != nil {
		t.Fatalf("cancel: %v", err)
	}

	// Assert.
	if len(h.entries()) != 0 {
		t.Fatal("cancel must drop the entry")
	}
}

func TestCancelledEntryIsNeverDelivered(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")
	_ = h.m.CancelQueueEntry("ws", h.entries()[0].id)

	// Act.
	h.turn(false)

	// Assert.
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("a cancelled prompt must never be delivered, got %v", got)
	}
}

func TestCancelOnAnUnknownEntryErrors(t *testing.T) {
	// Arrange / Act / Assert.
	h := newQueueHarness(t, nil)
	if err := h.m.CancelQueueEntry("ws", "q_nope"); err == nil {
		t.Fatal("cancelling an unknown entry must error")
	}
}

// --- semantics --------------------------------------------------------------

func TestUserInterruptDoesNotDropQueuedEntries(t *testing.T) {
	// Arrange — an interrupt stops the TURN; it says nothing about prompts the
	// user has lined up behind it.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")

	// Act.
	if err := h.m.Interrupt(context.Background(), "ws"); err != nil {
		t.Fatalf("interrupt: %v", err)
	}

	// Assert.
	if len(h.entries()) != 1 {
		t.Fatal("a user interrupt must not empty the queue")
	}
}

func TestQueueViewsCarryEveryLiveSession(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.turn(true)
	_ = h.submit("later")

	// Act.
	views := h.m.QueueViews()

	// Assert.
	if len(views) != 1 || views[0].GetWorkspace() != "ws" || len(views[0].GetEntries()) != 1 {
		t.Fatalf("QueueViews = %+v", views)
	}
}

func TestQueueViewsIncludeAnEmptyQueue(t *testing.T) {
	// Arrange — a reconnecting frontend must be TOLD the queue is empty.
	h := newQueueHarness(t, nil)

	// Act.
	views := h.m.QueueViews()

	// Assert.
	if len(views) != 1 || len(views[0].GetEntries()) != 0 {
		t.Fatalf("QueueViews = %+v, want one empty view", views)
	}
}

func TestQueueViewCarriesTheClassificationAndRationale(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{
		Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD,
		Rationale:      "independent",
	}}
	h := newQueueHarness(t, cls)
	h.turn(true)
	_ = h.submit("later")
	h.waitEntryClass(frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD)

	// Act.
	v := h.m.QueueViews()[0]

	// Assert.
	e := v.GetEntries()[0]
	if e.GetClassification() != frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD ||
		e.GetRationale() != "independent" {
		t.Fatalf("entry = %+v", e)
	}
}

// --- the pure queue structure ------------------------------------------------

func TestQueuePopFrontIsFIFO(t *testing.T) {
	// Arrange
	q := &promptQueue{}
	q.add(&queueEntry{id: "a"})
	q.add(&queueEntry{id: "b"})
	// Act
	got := q.popFront()
	// Assert
	if got.id != "a" {
		t.Fatalf("popFront = %s, want a", got.id)
	}
}

func TestQueuePopFrontOnEmptyIsNil(t *testing.T) {
	// Arrange / Act / Assert
	q := &promptQueue{}
	if q.popFront() != nil {
		t.Fatal("popFront on an empty queue must be nil")
	}
}

func TestQueueTakeInterjectingSkipsNonInterjecting(t *testing.T) {
	// Arrange
	q := &promptQueue{}
	q.add(&queueEntry{id: "a"})
	q.add(&queueEntry{id: "b", interjecting: true})
	// Act
	got := q.takeInterjecting()
	// Assert
	if got == nil || got.id != "b" {
		t.Fatalf("takeInterjecting = %+v, want b", got)
	}
}

func TestQueueTakeInterjectingLeavesTheRest(t *testing.T) {
	// Arrange
	q := &promptQueue{}
	q.add(&queueEntry{id: "a"})
	q.add(&queueEntry{id: "b", interjecting: true})
	// Act
	q.takeInterjecting()
	// Assert
	if len(q.entries) != 1 || q.entries[0].id != "a" {
		t.Fatalf("remaining = %+v, want [a]", q.entries)
	}
}

func TestQueueTakeInterjectingIsNilWhenNoneIs(t *testing.T) {
	// Arrange / Act / Assert
	q := &promptQueue{}
	q.add(&queueEntry{id: "a"})
	if q.takeInterjecting() != nil {
		t.Fatal("no entry is interjecting; want nil")
	}
}

func TestQueueRemoveReturnsTheEntry(t *testing.T) {
	// Arrange
	q := &promptQueue{}
	q.add(&queueEntry{id: "a"})
	// Act / Assert
	if got := q.remove("a"); got == nil || got.id != "a" {
		t.Fatalf("remove = %+v", got)
	}
}

func TestQueueRemoveUnknownIsNil(t *testing.T) {
	// Arrange / Act / Assert
	q := &promptQueue{}
	if q.remove("nope") != nil {
		t.Fatal("removing an unknown id must be nil")
	}
}

func TestQueueViewRendersEntriesInOrder(t *testing.T) {
	// Arrange
	q := &promptQueue{}
	q.add(&queueEntry{id: "a", text: "one"})
	q.add(&queueEntry{id: "b", text: "two"})
	// Act
	v := q.view("ws", "s1")
	// Assert
	if len(v.GetEntries()) != 2 || v.GetEntries()[0].GetText() != "one" {
		t.Fatalf("view = %+v", v)
	}
}

func TestQueueViewOfAnEmptyQueueStillCarriesIdentity(t *testing.T) {
	// Arrange / Act
	q := &promptQueue{}
	v := q.view("ws", "s1")
	// Assert — "the queue is empty" is a value, not an absence.
	if v.GetWorkspace() != "ws" || v.GetSessionId() != "s1" || len(v.GetEntries()) != 0 {
		t.Fatalf("view = %+v", v)
	}
}

func TestQueueEntryIDsAreUnique(t *testing.T) {
	// Arrange / Act — a collision would misroute a force or a cancel.
	seen := map[string]bool{}
	for i := 0; i < 500; i++ {
		id := newQueueEntryID()
		if seen[id] {
			t.Fatalf("duplicate queue id %s", id)
		}
		seen[id] = true
	}
}

// unusedCoreImport keeps corev1 referenced if a future test drops its last use.
var _ = corev1.Event{}

// --- the moot-path race ------------------------------------------------------

// gatedPusher hands control to the test on ONE chosen PushQueueView call and
// blocks there until released. beginInterject's moot path releases the manager
// mutex across exactly that push, so a test holding the gate is standing inside
// the race window with the lock free — which is what makes the race
// reproducible without a single sleep.
type gatedPusher struct {
	*fakePusher
	mu      sync.Mutex
	armed   bool
	entered chan struct{}
	release chan struct{}
}

func newGatedPusher(rec *fakePusher) *gatedPusher {
	return &gatedPusher{
		fakePusher: rec,
		entered:    make(chan struct{}),
		release:    make(chan struct{}),
	}
}

// arm makes the NEXT PushQueueView the gated one.
func (p *gatedPusher) arm() {
	p.mu.Lock()
	p.armed = true
	p.mu.Unlock()
}

func (p *gatedPusher) PushQueueView(v *frontendv1.QueueView) {
	p.fakePusher.PushQueueView(v)
	p.mu.Lock()
	hit := p.armed
	p.armed = false // gate exactly one push
	p.mu.Unlock()
	if hit {
		close(p.entered)
		<-p.release
	}
}

// TestInterjectMootPathDoesNotSubmitIntoATurnThatStartedMeanwhile pins the
// moot-path race. beginInterject's "no turn is running, so deliver directly"
// branch drops the manager mutex to publish, then RE-takes it to take and
// deliver the entry. A TurnStarted landing in that window makes the entry's
// delivery a submit into a running turn — the exact thing the queue exists to
// prevent.
//
// Driven deterministically: the pusher blocks inside the window, the test
// drives the TurnStarted through the real onTurnBoundary callback while the
// lock is free, and only then releases the push. No sleeps.
func TestInterjectMootPathDoesNotSubmitIntoATurnThatStartedMeanwhile(t *testing.T) {
	// Arrange — two prompts queued behind a running turn; the turn then ends,
	// which drains the FIRST and leaves the second held with no turn running.
	var gate *gatedPusher
	var logMu sync.Mutex
	var logged []string
	h := newQueueHarnessWithPusher(t, nil,
		func(rec *fakePusher) Pusher { gate = newGatedPusher(rec); return gate },
		func(format string, args ...any) {
			logMu.Lock()
			logged = append(logged, fmt.Sprintf(format, args...))
			logMu.Unlock()
		})
	h.turn(true)
	if err := h.submit("first"); err != nil {
		t.Fatalf("submit first: %v", err)
	}
	if err := h.submit("second"); err != nil {
		t.Fatalf("submit second: %v", err)
	}
	// BOTH VERDICTS MUST BE PUBLISHED BEFORE THE GATE IS ARMED. Classification
	// runs on its own goroutine and publishes when it lands, so a verdict still
	// in flight would push a queue view of its own — and the gate below arms on
	// the NEXT queue push, whichever it is. Stealing the gate that way leaves
	// the racing TurnStarted landing outside the window this test is about.
	// Waiting on the PUSH rather than on the entry is what makes it a fact: the
	// entry is marked under the mutex and published after it is released.
	waitFor(t, "both queued prompts' verdicts to be published", func() bool {
		v := h.push.lastQueue()
		if v == nil || len(v.GetEntries()) != 2 {
			return false
		}
		for _, e := range v.GetEntries() {
			if e.GetClassification() == frontendv1.QueueClassification_QUEUE_CLASSIFICATION_PENDING {
				return false
			}
		}
		return true
	})
	h.turn(false)
	waitFor(t, "the first entry to drain", func() bool { return len(h.entries()) == 1 })
	held := h.entries()[0]
	if held.text != "second" {
		t.Fatalf("held entry = %q, want 'second'", held.text)
	}
	// The whole delivery has to SETTLE before the premise below is installed,
	// not merely reach the shim. The delivery goroutine records the running
	// prompt as its last step, AFTER the accepted and delivered turn edges have
	// both been applied — and those edges write the very turn record this test
	// is about to zero, so acting on the submit alone would let one of them land
	// on top of the premise and quietly turn this into a test of the ordinary
	// interject path.
	waitFor(t, "the first delivery's turn edges to settle", func() bool {
		return len(h.applier.promptDeliverCalls()) == 1
	})
	// Prompt acceptance now closes the old accepted-but-not-yet-observed race
	// by setting the queue latch immediately. Put the fixture back at the
	// no-turn premise this test needs while deliberately retaining the second
	// entry; the test below owns the racing start explicitly.
	d := h.controller()
	h.m.mu.Lock()
	d.turn = turnRecord{}
	h.m.mu.Unlock()

	// Act — force the held entry. With no turn running this takes the moot
	// path; the gate stops it mid-window and a TurnStarted lands there.
	gate.arm()
	forced := make(chan error, 1)
	go func() { forced <- h.m.ForceQueueEntry("ws", held.id) }()
	<-gate.entered
	h.turn(true) // the racing TurnStarted, with the manager mutex free
	close(gate.release)
	if err := <-forced; err != nil {
		t.Fatalf("ForceQueueEntry: %v", err)
	}

	// Assert — the entry was NOT submitted into the turn that just started, is
	// back at the head of the queue with its classification, and the reversal
	// was reported loudly.
	if got := h.client.promptTexts(); len(got) != 1 || got[0] != "first" {
		t.Fatalf("prompts = %v, want only [first] — 'second' must not enter the running turn", got)
	}
	es := h.entries()
	if len(es) != 1 || es[0].id != held.id {
		t.Fatalf("entries = %+v, want the forced entry back at the head", es)
	}
	if !es[0].interjecting {
		t.Fatal("the requeued entry must stay flagged interjecting so the next TurnEnded delivers it first")
	}
	logMu.Lock()
	joined := strings.Join(logged, "\n")
	logMu.Unlock()
	if !strings.Contains(joined, "a turn STARTED while the moot path was publishing") {
		t.Fatalf("the reversal must be logged loudly; log was:\n%s", joined)
	}

	// And it is delivered at the next real turn end, never stranded.
	h.turn(false)
	waitFor(t, "the requeued entry to deliver at the next turn end", func() bool {
		got := h.client.promptTexts()
		return len(got) == 2 && got[1] == "second"
	})
}

package sessioncontroller

import (
	"context"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/statedb"
)

// --- doubles ----------------------------------------------------------------

// fakeLease is a drain lease under the test's direct control. It records the
// activity notifications the fleet sends, which is how a test observes that a
// hold edge was reported without sleeping for one.
type fakeLease struct {
	mu         sync.Mutex
	scheduleID string
	cause      string
	activity   int
}

func (f *fakeLease) HeldSchedule() (string, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.scheduleID, f.scheduleID != ""
}

func (f *fakeLease) LeaseProvenance() (string, string, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.scheduleID, f.cause, f.scheduleID != ""
}

func (f *fakeLease) NoteDrainActivity() {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.activity++
}

func (f *fakeLease) hold(scheduleID string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.scheduleID = scheduleID
}

func (f *fakeLease) release() {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.scheduleID = ""
}

func (f *fakeLease) activityCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.activity
}

// fakeHoldStore is an in-memory ShutdownHoldStore.
type fakeHoldStore struct {
	mu   sync.Mutex
	rows map[string]statedb.HeldPrompt
	err  error
}

func newFakeHoldStore() *fakeHoldStore {
	return &fakeHoldStore{rows: map[string]statedb.HeldPrompt{}}
}

func (f *fakeHoldStore) RecordHeldPrompt(p statedb.HeldPrompt) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.err != nil {
		return f.err
	}
	f.rows[p.EntryID] = p
	return nil
}

func (f *fakeHoldStore) DropHeldPrompt(entryID string) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	_, ok := f.rows[entryID]
	delete(f.rows, entryID)
	return ok, nil
}

func (f *fakeHoldStore) DropHeldPromptsForSchedule(scheduleID string) (int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	n := 0
	for id, p := range f.rows {
		if p.ScheduleID == scheduleID {
			delete(f.rows, id)
			n++
		}
	}
	return n, nil
}

func (f *fakeHoldStore) HeldPrompts(workspace string) ([]statedb.HeldPrompt, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []statedb.HeldPrompt
	for _, p := range f.rows {
		if p.Workspace == workspace {
			out = append(out, p)
		}
	}
	return out, nil
}

func (f *fakeHoldStore) count() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.rows)
}

// fakeTaskCounter answers the live-task half of a drain hold.
type fakeTaskCounter struct {
	counts map[string]int64
	known  bool
}

func (f fakeTaskCounter) LiveTasks(workspace string) (int64, bool) {
	n, ok := f.counts[workspace]
	if !ok {
		return 0, f.known
	}
	return n, true
}

// leaseHarness is a queueHarness with a drain lease and a durable hold store
// bound to it.
type leaseHarness struct {
	*queueHarness
	lease *fakeLease
	store *fakeHoldStore
}

func newLeaseHarness(t *testing.T) *leaseHarness {
	t.Helper()
	store := newFakeHoldStore()
	qh := newQueueHarnessWithHolds(t, nil, store, nil)
	lease := &fakeLease{}
	if err := qh.m.BindShutdownLease(lease); err != nil {
		t.Fatalf("BindShutdownLease: %v", err)
	}
	return &leaseHarness{queueHarness: qh, lease: lease, store: store}
}

// --- binding ----------------------------------------------------------------

func TestBindShutdownLeaseRefusesANilEngine(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)

	// Act.
	err := h.m.BindShutdownLease(nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "lease engine") {
		t.Fatalf("BindShutdownLease(nil) = %v, want a refusal naming the missing engine", err)
	}
}

func TestAnUnboundFleetHoldsNoSchedule(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)

	// Act.
	id, held := h.m.heldSchedule()

	// Assert.
	if held || id != "" {
		t.Fatalf("heldSchedule on an unbound fleet = %q, %v; want no lease", id, held)
	}
}

// --- parking ----------------------------------------------------------------

func TestAPromptSubmittedToAnIdleSessionUnderTheLeaseIsParked(t *testing.T) {
	// The lease is the ONE condition under which an idle session shows a chip.
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")

	// Act.
	if err := h.submit("hello"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	entries := h.entries()
	if len(entries) != 1 {
		t.Fatalf("queue = %d entries, want the prompt parked", len(entries))
	}
	if entries[0].shutdownHoldScheduleID != "sd_1" {
		t.Fatalf("entry hold = %q, want sd_1", entries[0].shutdownHoldScheduleID)
	}
}

func TestAParkedPromptIsNeverClassified(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{Classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT}}
	store := newFakeHoldStore()
	qh := newQueueHarnessWithHolds(t, cls, store, nil)
	lease := &fakeLease{}
	if err := qh.m.BindShutdownLease(lease); err != nil {
		t.Fatalf("BindShutdownLease: %v", err)
	}
	lease.hold("sd_1")
	qh.turn(true)

	// Act.
	if err := qh.m.SubmitPrompt(context.Background(), "ws", "", "hello", ""); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if got := cls.requests(); len(got) != 0 {
		t.Fatalf("classifier ran %d time(s) on a drain-held entry, want 0", len(got))
	}
}

func TestAParkedPromptIsStampedHoldWithNoRationale(t *testing.T) {
	// PENDING would claim the classifier is running; it never runs on a parked
	// entry, so the honest stamp is HOLD — deliver later, never interrupt.
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")

	// Act.
	if err := h.submit("hello"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	entries := h.entries()
	if len(entries) != 1 {
		t.Fatalf("queue = %d entries, want the prompt parked", len(entries))
	}
	if got := entries[0].classification; got != frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD {
		t.Fatalf("parked entry classification = %v, want HOLD", got)
	}
	if got := entries[0].rationale; got != "" {
		t.Fatalf("parked entry rationale = %q, want empty — no classifier produced one", got)
	}
}

func TestARestoredParkedPromptIsStampedHoldWithNoRationale(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_live")
	if err := h.store.RecordHeldPrompt(statedb.HeldPrompt{
		EntryID: "q_restored", ScheduleID: "sd_live", Workspace: "ws", SessionID: "s1", Text: "delayed",
	}); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	entries := h.entries()
	if len(entries) != 1 {
		t.Fatalf("queue = %d entries, want the parked prompt restored", len(entries))
	}
	if got := entries[0].classification; got != frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD {
		t.Fatalf("restored parked entry classification = %v, want HOLD", got)
	}
	if got := entries[0].rationale; got != "" {
		t.Fatalf("restored parked entry rationale = %q, want empty", got)
	}
}

func TestAParkedPromptIsRecordedDurably(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")

	// Act.
	if err := h.submit("hello"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if h.store.count() != 1 {
		t.Fatalf("durable hold rows = %d, want the parked prompt recorded", h.store.count())
	}
}

func TestAParkedEntryCarriesItsScheduleOnTheWire(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")

	// Act.
	if err := h.submit("hello"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	view := h.controller().queue.view("ws", "s1")
	if len(view.GetEntries()) != 1 {
		t.Fatalf("view = %d entries, want 1", len(view.GetEntries()))
	}
	if got := view.GetEntries()[0].GetShutdownHold().GetScheduleId(); got != "sd_1" {
		t.Fatalf("entry shutdown_hold schedule_id = %q, want sd_1", got)
	}
}

func TestAnOrdinaryEntryCarriesNoShutdownHoldOnTheWire(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.turn(true)

	// Act.
	if err := h.submit("hello"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	view := h.controller().queue.view("ws", "s1")
	if len(view.GetEntries()) != 1 {
		t.Fatalf("view = %d entries, want 1", len(view.GetEntries()))
	}
	if view.GetEntries()[0].GetShutdownHold() != nil {
		t.Fatal("an ordinary queue entry carried a shutdown_hold, want none")
	}
}

// --- acquisition ------------------------------------------------------------

func TestAcquireParksThePromptsAlreadyQueuedWhenTheLeaseIsTaken(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.turn(true)
	if err := h.submit("queued first"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.lease.hold("sd_1")
	parked := h.m.AcquireShutdownHolds("sd_1")

	// Assert.
	if parked != 1 {
		t.Fatalf("AcquireShutdownHolds parked %d, want 1", parked)
	}
	if got := h.entries()[0].shutdownHoldScheduleID; got != "sd_1" {
		t.Fatalf("pre-existing entry hold = %q, want sd_1", got)
	}
}

func TestAcquireWithNoScheduleIDParksNothing(t *testing.T) {
	// An unnamed hold could never be released by a cancel.
	// Arrange.
	h := newLeaseHarness(t)
	h.turn(true)
	if err := h.submit("queued first"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	parked := h.m.AcquireShutdownHolds("")

	// Assert.
	if parked != 0 {
		t.Fatalf("AcquireShutdownHolds(\"\") parked %d, want 0", parked)
	}
}

// --- the drain --------------------------------------------------------------

func TestTheTurnEndDrainDoesNotDeliverAParkedEntry(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	h.turn(true)
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.turn(false)

	// Assert.
	if got := len(h.entries()); got != 1 {
		t.Fatalf("queue = %d entries after the turn ended, want the parked prompt retained", got)
	}
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("shim received %v, want nothing delivered during a drain", got)
	}
}

func TestTheTurnEndDrainStillDeliversAnUnparkedEntryBehindAParkedOne(t *testing.T) {
	// The skip is the structural guarantee: a parked entry blocks itself, never
	// an ordinary entry that a later schedule has no claim on.
	// Arrange.
	h := newLeaseHarness(t)
	h.turn(true)
	if err := h.submit("ordinary"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.lease.hold("sd_1")
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	// Only the newcomer is parked: Acquire is deliberately NOT run here.

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the ordinary entry to be delivered", func() bool {
		return len(h.client.promptTexts()) == 1
	})
	if got := h.client.promptTexts()[0]; got != "ordinary" {
		t.Fatalf("delivered %q, want the un-parked entry", got)
	}
}

func TestATurnBoundaryTellsTheLeaseEngineAHoldMoved(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	before := h.lease.activityCount()

	// Act.
	h.turn(true)

	// Assert.
	if h.lease.activityCount() <= before {
		t.Fatal("a turn starting did not notify the lease engine; a drain would never learn a hold appeared")
	}
}

func TestDeliverRefusesAParkedEntryAndRequeuesIt(t *testing.T) {
	// The backstop at the one delivery funnel: loud requeue, never a drop.
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	d := h.controller()
	id := h.entries()[0].id
	h.m.mu.Lock()
	e := d.queue.remove(id)
	h.m.mu.Unlock()

	// Act.
	h.m.deliver(d, e)

	// Assert.
	if got := len(h.entries()); got != 1 {
		t.Fatalf("queue = %d entries after the refused delivery, want the entry requeued", got)
	}
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("shim received %v, want nothing", got)
	}
}

// --- force and cancel -------------------------------------------------------

func TestForcingAParkedEntryShedsItsHold(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	id := h.entries()[0].id

	// Act.
	if err := h.m.ForceQueueEntry("ws", id); err != nil {
		t.Fatalf("ForceQueueEntry: %v", err)
	}

	// Assert.
	waitFor(t, "the forced prompt to reach the shim", func() bool {
		return len(h.client.promptTexts()) == 1
	})
}

func TestForcingAParkedEntryDropsItsDurableRow(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	id := h.entries()[0].id

	// Act.
	if err := h.m.ForceQueueEntry("ws", id); err != nil {
		t.Fatalf("ForceQueueEntry: %v", err)
	}

	// Assert.
	if h.store.count() != 0 {
		t.Fatalf("durable hold rows = %d after a force, want 0", h.store.count())
	}
}

func TestCancellingAParkedEntryDropsItsDurableRow(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	id := h.entries()[0].id

	// Act.
	if err := h.m.CancelQueueEntry("ws", id); err != nil {
		t.Fatalf("CancelQueueEntry: %v", err)
	}

	// Assert.
	if h.store.count() != 0 {
		t.Fatalf("durable hold rows = %d after a cancel, want 0", h.store.count())
	}
}

func TestReleaseShedsTheHoldFromEveryEntryOfItsSchedule(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	h.turn(true)
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.lease.release()
	h.m.ReleaseShutdownHolds("sd_1")

	// Assert.
	entries := h.entries()
	if len(entries) != 1 {
		t.Fatalf("queue = %d entries, want the entry retained", len(entries))
	}
	if entries[0].drainHeld() {
		t.Fatalf("entry still holds %q after its schedule was cancelled", entries[0].shutdownHoldScheduleID)
	}
}

func TestReleaseLeavesAnotherSchedulesEntryParked(t *testing.T) {
	// A cancel aimed at one schedule must never free another's entries.
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	h.turn(true)
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.m.ReleaseShutdownHolds("sd_other")

	// Assert.
	if !h.entries()[0].drainHeld() {
		t.Fatal("an entry of schedule sd_1 was freed by a cancel naming sd_other")
	}
}

func TestReleaseWithNoScheduleIDFreesNothing(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	h.turn(true)
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.m.ReleaseShutdownHolds("")

	// Assert.
	if !h.entries()[0].drainHeld() {
		t.Fatal("an unnamed release freed a parked entry; it could never be attributed to a schedule")
	}
}

func TestReleaseDeliversAFreedEntryWhenNoTurnIsRunning(t *testing.T) {
	// With no turn running, no boundary is coming to drain the queue.
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_1")
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.lease.release()
	h.m.ReleaseShutdownHolds("sd_1")

	// Assert.
	waitFor(t, "the freed prompt to reach the shim", func() bool {
		return len(h.client.promptTexts()) == 1
	})
}

// --- holds ------------------------------------------------------------------

func TestASessionWithNeitherATurnNorTasksHoldsNothing(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{}})

	// Assert.
	if len(holds) != 0 {
		t.Fatalf("DrainHolds = %+v, want none for a quiescent session", holds)
	}
}

func TestAnInFlightTurnHoldsTheDrain(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.turn(true)

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{}})

	// Assert.
	if len(holds) != 1 || !holds[0].TurnActive {
		t.Fatalf("DrainHolds = %+v, want one turn hold", holds)
	}
}

func TestLiveBackgroundTasksHoldTheDrainWithNoTurnRunning(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{"ws": 3}})

	// Assert.
	if len(holds) != 1 || holds[0].LiveTasks != 3 || holds[0].TurnActive {
		t.Fatalf("DrainHolds = %+v, want one task-only hold of 3", holds)
	}
}

func TestATurnAndLiveTasksAreBothReportedOnOneHold(t *testing.T) {
	// Co-occurring facts, not mutually exclusive states.
	// Arrange.
	h := newLeaseHarness(t)
	h.turn(true)

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{"ws": 2}})

	// Assert.
	if len(holds) != 1 || !holds[0].TurnActive || holds[0].LiveTasks != 2 {
		t.Fatalf("DrainHolds = %+v, want one hold carrying both facts", holds)
	}
}

func TestAHoldNamesTheTurnItIsWaitingOn(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	d := h.controller()
	h.m.mu.Lock()
	d.noteActiveTurnID(true, "t_42")
	h.m.mu.Unlock()
	h.turn(true)

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{}})

	// Assert.
	if len(holds) != 1 || holds[0].TurnID != "t_42" {
		t.Fatalf("DrainHolds = %+v, want the hold to name turn t_42", holds)
	}
}

func TestAnAdoptedTurnHoldsTheDrainWithNoTurnID(t *testing.T) {
	// A turn this daemon never saw start is still unambiguously in flight.
	// Arrange.
	h := newLeaseHarness(t)
	h.turn(true)

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{}})

	// Assert.
	if len(holds) != 1 || !holds[0].TurnActive || holds[0].TurnID != "" {
		t.Fatalf("DrainHolds = %+v, want a turn hold with no id", holds)
	}
}

func TestATurnsIDIsClearedAtItsOwnEnd(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	d := h.controller()
	h.m.mu.Lock()
	d.noteActiveTurnID(true, "t_42")

	// Act.
	d.noteActiveTurnID(false, "t_42")
	got := d.activeTurnID
	h.m.mu.Unlock()

	// Assert.
	if got != "" {
		t.Fatalf("activeTurnID = %q after its own end, want cleared", got)
	}
}

func TestADifferentTurnsEndDoesNotClearTheActiveTurnID(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	d := h.controller()
	h.m.mu.Lock()
	d.noteActiveTurnID(true, "t_42")

	// Act.
	d.noteActiveTurnID(false, "t_other")
	got := d.activeTurnID
	h.m.mu.Unlock()

	// Assert.
	if got != "t_42" {
		t.Fatalf("activeTurnID = %q, want t_42 to survive another turn's end", got)
	}
}

// --- restore ----------------------------------------------------------------

func TestARestoredPromptComesBackUnheldWhenItsScheduleIsGone(t *testing.T) {
	// The bounce it was waiting for has happened, which is exactly when it runs.
	// Arrange.
	h := newLeaseHarness(t)
	if err := h.store.RecordHeldPrompt(statedb.HeldPrompt{
		EntryID: "q_restored", ScheduleID: "sd_old", Workspace: "ws", SessionID: "s1", Text: "delayed",
	}); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}
	h.turn(true) // keep it queued so the assertion reads the entry, not the shim

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	entries := h.entries()
	if len(entries) != 1 || entries[0].id != "q_restored" {
		t.Fatalf("queue = %+v, want the parked prompt restored", entries)
	}
	if entries[0].drainHeld() {
		t.Fatal("the restored prompt came back still held by a schedule that no longer exists")
	}
}

func TestARestoredPromptStaysHeldWhenItsScheduleStillStands(t *testing.T) {
	// The crash was mid-drain; the lease was re-taken, so the hold stands.
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_live")
	if err := h.store.RecordHeldPrompt(statedb.HeldPrompt{
		EntryID: "q_restored", ScheduleID: "sd_live", Workspace: "ws", SessionID: "s1", Text: "delayed",
	}); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	entries := h.entries()
	if len(entries) != 1 || entries[0].shutdownHoldScheduleID != "sd_live" {
		t.Fatalf("queue = %+v, want the prompt restored still held by sd_live", entries)
	}
}

func TestARestoredPromptIsDeliveredWhenTheSessionIsIdle(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	if err := h.store.RecordHeldPrompt(statedb.HeldPrompt{
		EntryID: "q_restored", ScheduleID: "sd_old", Workspace: "ws", SessionID: "s1", Text: "delayed",
	}); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	waitFor(t, "the restored prompt to reach the shim", func() bool {
		return len(h.client.promptTexts()) == 1
	})
	if got := h.client.promptTexts()[0]; got != "delayed" {
		t.Fatalf("shim received %q, want the restored prompt", got)
	}
}

func TestADeliveredRestoredPromptDropsItsDurableRow(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	if err := h.store.RecordHeldPrompt(statedb.HeldPrompt{
		EntryID: "q_restored", ScheduleID: "sd_old", Workspace: "ws", SessionID: "s1", Text: "delayed",
	}); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	waitFor(t, "the durable row to be released", func() bool { return h.store.count() == 0 })
}

func TestRestoreDoesNotDuplicateAnEntryTheQueueAlreadyHolds(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.hold("sd_live")
	h.turn(true)
	if err := h.submit("parked"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	if got := len(h.entries()); got != 1 {
		t.Fatalf("queue = %d entries after a restore over a live queue, want 1", got)
	}
}

// --- shim-stop provenance ---------------------------------------------------

func TestShimStopProvenanceStatesAnUnwiredLease(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)

	// Act.
	got := h.m.shimStopProvenance()

	// Assert.
	if got != "drain_lease=unwired" {
		t.Fatalf("shimStopProvenance = %q, want the unwired statement", got)
	}
}

func TestShimStopProvenanceStatesThatNoLeaseIsHeld(t *testing.T) {
	// An omitted schedule is indistinguishable from logging that forgot to look.
	// Arrange.
	h := newLeaseHarness(t)

	// Act.
	got := h.m.shimStopProvenance()

	// Assert.
	if got != "drain_lease=none" {
		t.Fatalf("shimStopProvenance = %q, want the explicit no-lease statement", got)
	}
}

func TestShimStopProvenanceNamesTheHeldScheduleAndItsCause(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	h.lease.mu.Lock()
	h.lease.scheduleID, h.lease.cause = "sd_1", "merge rebuilt the daemon"
	h.lease.mu.Unlock()

	// Act.
	got := h.m.shimStopProvenance()

	// Assert.
	if !strings.Contains(got, "sd_1") || !strings.Contains(got, "merge rebuilt the daemon") {
		t.Fatalf("shimStopProvenance = %q, want it to name the schedule and its cause", got)
	}
}

package sessioncontroller

import (
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/statedb"
)

// parkedHarness is an UNWIRED fleet — no controller for "ws" — with the drain
// lease and the durable parking ledger bound. It is the successor daemon at the
// moment it boots: the ledger has rows in it and nothing has wired yet.
type parkedHarness struct {
	*queueHarness
	lease *fakeLease
	store *fakeHoldStore
	log   *logCapture
}

func newParkedHarness(t *testing.T) *parkedHarness {
	t.Helper()
	return newParkedHarnessOn(t, newFakeHoldStore())
}

// newParkedHarnessOn is newParkedHarness over an EXISTING ledger, which is how
// a successor daemon is expressed: the store outlives the process that wrote
// it, so a second harness over the same store is the daemon that came back from
// the crash reading what the first one left.
func newParkedHarnessOn(t *testing.T, store *fakeHoldStore) *parkedHarness {
	t.Helper()
	cl := &logCapture{}
	qh := newQueueHarnessUnwired(t, store, cl.logf)
	lease := &fakeLease{}
	if err := qh.m.BindShutdownLease(lease); err != nil {
		t.Fatalf("BindShutdownLease: %v", err)
	}
	return &parkedHarness{queueHarness: qh, lease: lease, store: store, log: cl}
}

// record writes one parked row into the ledger a previous daemon left behind.
func (h *parkedHarness) record(entryID, scheduleID, text string) {
	h.t.Helper()
	if err := h.store.RecordHeldPrompt(statedb.HeldPrompt{
		EntryID: entryID, ScheduleID: scheduleID, Workspace: "ws", SessionID: "s1",
		Text: text, PromptOrigin: int32(testPromptOrigin), QueuedAtMs: 10,
	}); err != nil {
		h.t.Fatalf("RecordHeldPrompt: %v", err)
	}
}

// materialize runs the boot materialization and returns how many rows it seeded.
func (h *parkedHarness) materialize() int {
	h.t.Helper()
	n, err := h.m.MaterializeShutdownHolds()
	if err != nil {
		h.t.Fatalf("MaterializeShutdownHolds: %v", err)
	}
	return n
}

// --- the snapshot ------------------------------------------------------------

func TestMaterializedHeldPromptsReachQueueViewsWithNoLiveController(t *testing.T) {
	// The defect: QueueViews walked only the live fleet, so on a successor
	// daemon every parked prompt was invisible to every client until its
	// session happened to wire.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")

	// Act.
	h.materialize()

	// Assert.
	views := h.m.QueueViews()
	if len(views) != 1 {
		t.Fatalf("QueueViews = %d views on an unwired fleet, want the materialized one", len(views))
	}
	entries := views[0].GetEntries()
	if len(entries) != 1 || entries[0].GetId() != "q_parked" {
		t.Fatalf("materialized view entries = %+v, want q_parked", entries)
	}
}

func TestAMaterializedViewCarriesTheSessionItsDurableRowNamed(t *testing.T) {
	// The frontend keys a QueueView by session id, and a client needs that id
	// to route a cancel back for a session no controller exists for.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")

	// Act.
	h.materialize()

	// Assert.
	views := h.m.QueueViews()
	if len(views) != 1 || views[0].GetFence() != "s1" {
		t.Fatalf("materialized view session = %+v, want s1", views)
	}
}

func TestAMaterializedEntryStillHeldByTheLiveScheduleCarriesItsHold(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")

	// Act.
	h.materialize()

	// Assert.
	got := h.m.QueueViews()[0].GetEntries()[0].GetShutdown().GetScheduleId()
	if got != "sd_live" {
		t.Fatalf("materialized entry hold = %q, want sd_live", got)
	}
}

func TestAMaterializedEntryOfADeadScheduleComesBackUnheld(t *testing.T) {
	// The bounce it was waiting for has happened, which is exactly when it
	// should stop being held.
	// Arrange — the live lease is a DIFFERENT schedule from the row's.
	h := newParkedHarness(t)
	h.lease.hold("sd_new")
	h.record("q_parked", "sd_old", "delayed")

	// Act.
	h.materialize()

	// Assert.
	if hold := h.m.QueueViews()[0].GetEntries()[0].GetShutdown(); hold != nil {
		t.Fatalf("materialized entry hold = %+v, want none — its schedule is gone", hold)
	}
}

func TestTheMaterializationIsRecordedInTheCanonicalLog(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")

	// Act.
	h.materialize()

	// Assert.
	if !h.log.contains("drain-held prompts MATERIALIZED") {
		t.Fatal("the boot materialization left no canonical log line naming it")
	}
}

// --- the unresolved-set gate -------------------------------------------------

func TestAMaterializedRecordDoesNotReadAsAWiredSession(t *testing.T) {
	// THE ONE THAT MUST NOT REGRESS. A restored lease treats a wired session as
	// AFFIRMATIVE resolution of its unresolved set, so a materialized record
	// answering yes would let a mid-drain lease execute its shutdown over a shim
	// that is still reattaching mid-turn.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")

	// Act.
	h.materialize()

	// Assert.
	if got := h.m.WiredSessions(); len(got) != 0 {
		t.Fatalf("WiredSessions = %v after materialization, want none — nothing has wired", got)
	}
}

// --- idempotence against the ShimReady replay --------------------------------

func TestTheShimReadyReplayAddsNothingOverAMaterializedEntry(t *testing.T) {
	// The replay's `d.queue.get(row.EntryID)` dedupe is what makes the two
	// seeding paths idempotent, and adoption is what puts the entry where the
	// dedupe can see it.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	entries := h.entries()
	if len(entries) != 1 || entries[0].id != "q_parked" {
		t.Fatalf("queue after materialize+replay = %+v, want exactly one q_parked", entries)
	}
}

func TestAWiredSessionAdoptsTheMaterializedEntryRatherThanKeepingTwoLedgers(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert — the workspace is in exactly one ledger, the controller's.
	h.m.mu.Lock()
	parked := len(h.m.parked)
	h.m.mu.Unlock()
	if parked != 0 {
		t.Fatalf("parked ledger holds %d workspace(s) after the session wired, want 0", parked)
	}
}

func TestMaterializationSkipsAWorkspaceThatAlreadyHasAController(t *testing.T) {
	// Its controller owns its queue; a second copy beside it would be two
	// answers to one workspace's queue.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.wire()

	// Act.
	n := h.materialize()

	// Assert.
	if n != 0 {
		t.Fatalf("materialized %d row(s) for a wired workspace, want 0", n)
	}
}

// --- cancel ------------------------------------------------------------------

func TestCancellingAMaterializedEntryNeedsNoSession(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	err := h.m.CancelQueueEntry("ws", "q_parked")

	// Assert.
	if err != nil {
		t.Fatalf("CancelQueueEntry on a materialized entry = %v, want it honored with no live session", err)
	}
}

func TestCancellingAMaterializedEntryShrinksTheView(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	if err := h.m.CancelQueueEntry("ws", "q_parked"); err != nil {
		t.Fatalf("CancelQueueEntry: %v", err)
	}

	// Assert.
	if views := h.m.QueueViews(); len(views) != 0 {
		t.Fatalf("QueueViews = %+v after cancelling the only materialized entry, want none", views)
	}
}

func TestCancellingAMaterializedEntryDropsItsDurableRow(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	if err := h.m.CancelQueueEntry("ws", "q_parked"); err != nil {
		t.Fatalf("CancelQueueEntry: %v", err)
	}

	// Assert — a row left behind would be replayed by the next daemon as a
	// prompt the user has already taken back.
	if got := h.store.count(); got != 0 {
		t.Fatalf("durable rows = %d after cancelling the materialized entry, want 0", got)
	}
}

// --- cancel: the durable drop is what licenses forgetting the entry ----------

func TestCancellingAMaterializedEntryIsRefusedWhenItsDurableRowCannotBeDropped(t *testing.T) {
	// The ack is the only place the user learns their prompt is NOT gone. A
	// cancel that reports success over a row that still stands is the daemon
	// promising a withdrawal it has not made.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.store.failDropsWith(errors.New("ledger is down"))

	// Act.
	err := h.m.CancelQueueEntry("ws", "q_parked")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "ledger is down") {
		t.Fatalf("CancelQueueEntry over a failing drop = %v, want a refusal carrying the store failure", err)
	}
}

func TestARefusedCancelLeavesTheDurableRowIntact(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.store.failDropsWith(errors.New("ledger is down"))

	// Act.
	if err := h.m.CancelQueueEntry("ws", "q_parked"); err == nil {
		t.Fatal("CancelQueueEntry over a failing drop succeeded, want a refusal")
	}

	// Assert.
	if !h.store.has("q_parked") {
		t.Fatal("the durable row is gone after a refused cancel; the refusal must leave the ledger exactly as it found it")
	}
}

func TestARefusedCancelLeavesTheEntryInTheQueue(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.store.failDropsWith(errors.New("ledger is down"))

	// Act.
	if err := h.m.CancelQueueEntry("ws", "q_parked"); err == nil {
		t.Fatal("CancelQueueEntry over a failing drop succeeded, want a refusal")
	}

	// Assert — the prompt is still there to cancel again once the store answers.
	views := h.m.QueueViews()
	if len(views) != 1 || len(views[0].GetEntries()) != 1 {
		t.Fatalf("QueueViews = %+v after a refused cancel, want the entry still parked", views)
	}
}

func TestARefusedCancelIsRecordedInTheCanonicalLog(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.store.failDropsWith(errors.New("ledger is down"))

	// Act.
	if err := h.m.CancelQueueEntry("ws", "q_parked"); err == nil {
		t.Fatal("CancelQueueEntry over a failing drop succeeded, want a refusal")
	}

	// Assert.
	if !h.log.contains("queue cancel REFUSED entry=q_parked") {
		t.Fatalf("the refused cancel left no canonical log line naming it")
	}
}

func TestACancelledPromptIsNotResurrectedByTheDaemonThatComesBack(t *testing.T) {
	// THE POINT OF DROPPING THE ROW FIRST. The row is the only thing a successor
	// daemon reads, so a cancel that forgets the entry before the row is gone
	// can be undone by a crash in that window.
	// Arrange — a daemon materializes the parked prompt and the user cancels it.
	store := newFakeHoldStore()
	first := newParkedHarnessOn(t, store)
	first.lease.hold("sd_live")
	first.record("q_parked", "sd_live", "delayed")
	first.materialize()
	if err := first.m.CancelQueueEntry("ws", "q_parked"); err != nil {
		t.Fatalf("CancelQueueEntry: %v", err)
	}

	// Act — the daemon dies and the successor materializes the same ledger.
	successor := newParkedHarnessOn(t, store)
	successor.lease.hold("sd_live")
	successor.materialize()

	// Assert.
	if views := successor.m.QueueViews(); len(views) != 0 {
		t.Fatalf("the successor daemon materialized %+v, want nothing — the prompt was cancelled before it died", views)
	}
}

// --- accept ------------------------------------------------------------------

func TestAcceptingAMaterializedEntryIsHonored(t *testing.T) {
	// Accept is VIEW STATE and needs no shim — the same argument cancel makes.
	// It used to report "no queued prompt" for a prompt the client was looking
	// at, because it alone did not consult the materialized ledger.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	err := h.m.AcceptQueueEntry("ws", "q_parked")

	// Assert.
	if err != nil {
		t.Fatalf("AcceptQueueEntry on a materialized entry = %v, want it honored with no live session", err)
	}
}

func TestAcceptingAMaterializedEntryPublishesItAsAccepted(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	if err := h.m.AcceptQueueEntry("ws", "q_parked"); err != nil {
		t.Fatalf("AcceptQueueEntry: %v", err)
	}

	// Assert.
	view := h.push.lastQueue()
	if view == nil || len(view.GetEntries()) != 1 || !view.GetEntries()[0].GetHoldForTurnEnd().GetAccepted() {
		t.Fatalf("last pushed queue view = %+v, want the materialized entry marked accepted", view)
	}
}

func TestAcceptingAMaterializedEntryIsRecordedInTheCanonicalLog(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	if err := h.m.AcceptQueueEntry("ws", "q_parked"); err != nil {
		t.Fatalf("AcceptQueueEntry: %v", err)
	}

	// Assert.
	if !h.log.contains("queue entry=q_parked accepted") {
		t.Fatalf("the honored accept left no canonical log line naming it")
	}
}

// --- force -------------------------------------------------------------------

func TestForcingAMaterializedEntryIsRefusedNamingTheUnwiredSession(t *testing.T) {
	// A force is a DELIVERY, and a delivery needs a shim this daemon does not
	// have. The refusal names the session so the user is told what to do about
	// it rather than merely told no.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	err := h.m.ForceQueueEntry("ws", "q_parked")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "s1") {
		t.Fatalf("ForceQueueEntry on a materialized entry = %v, want a refusal naming session s1", err)
	}
}

func TestForcingAMaterializedEntryIsNeverASilentNoOp(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	if err := h.m.ForceQueueEntry("ws", "q_parked"); err == nil {
		t.Fatal("ForceQueueEntry on a materialized entry succeeded, want a refusal")
	}

	// Assert — refused, and the prompt is still there to force later or cancel.
	views := h.m.QueueViews()
	if len(views) != 1 || len(views[0].GetEntries()) != 1 {
		t.Fatalf("QueueViews = %+v after a refused force, want the entry still parked", views)
	}
}

func TestForcingAMaterializedEntryIsRecordedInTheCanonicalLog(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	if err := h.m.ForceQueueEntry("ws", "q_parked"); err == nil {
		t.Fatal("ForceQueueEntry on a materialized entry succeeded, want a refusal")
	}

	// Assert.
	if !h.log.contains("force REFUSED for materialized queue entry") {
		t.Fatal("the refused force left no canonical log line naming it")
	}
}

func TestAForcedMaterializedEntryClassifiesAsAnUnwiredSession(t *testing.T) {
	// TYPED, not raw. Without the sentinel this ordinary refusal reached a human
	// as internal.unclassified — the loud fallthrough for failures NOBODY
	// classified — and was logged a second time on the way.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	err := h.m.ForceQueueEntry("ws", "q_parked")

	// Assert.
	if !errors.Is(err, errclass.ErrQueueEntrySessionUnwired) {
		t.Fatalf("ForceQueueEntry on a materialized entry = %v, want it to carry ErrQueueEntrySessionUnwired", err)
	}
}

func TestForcingAnUnadoptedEntryOfAWiredSessionIsRefusedSayingSo(t *testing.T) {
	// The wired-but-unadopted window: a controller exists, and the boot ledger
	// still holds the prompt, so the controller's queue cannot deliver it yet.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()

	// Act.
	err := h.m.ForceQueueEntry("ws", "q_parked")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "has not yet adopted") {
		t.Fatalf("ForceQueueEntry in the wired-but-unadopted window = %v, want a refusal naming the unfinished adoption", err)
	}
}

// --- the row-drop/restore interleaving ---------------------------------------

func TestAForceDuringARestoreIsNotResurrectedByItsStaleRowSnapshot(t *testing.T) {
	// THE HALF THE CANCEL-ONLY TOMBSTONE MISSED. A force drops the entry's
	// durable row and then DELIVERS it, so the entry leaves the queue and the
	// apply loop's `d.queue.get` dedupe stops covering it. Holding a row snapshot
	// taken before the drop, the restore re-queued a prompt that had already run
	// — the user forced one prompt and the agent got two.
	// Arrange — the session wires, and the row read is where the force lands.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()
	h.store.duringHeldPromptsRead(func() {
		if err := h.m.ForceQueueEntry("ws", "q_parked"); err != nil {
			t.Errorf("the mid-restore force failed: %v", err)
		}
	})

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert — the forced prompt reached the shim exactly once and the restore
	// did not put a second copy of it back in the queue.
	waitFor(t, "the forced prompt to reach the shim", func() bool {
		return len(h.client.promptTexts()) == 1
	})
	if es := h.entries(); len(es) != 0 {
		t.Fatalf("queue = %+v after a force mid-restore, want empty — the restore re-added a prompt that has already been delivered", es)
	}
}

func TestAScheduleReleaseDuringARestoreIsNotResurrectedByItsStaleRowSnapshot(t *testing.T) {
	// THE THIRD ROW-DROPPING LEG. Cancelling the schedule drops every one of its
	// durable rows and returns the entries to ordinary delivery — and with no
	// turn running the release's own kick submits one immediately, so the entry
	// leaves the queue and the apply loop's `d.queue.get` dedupe stops covering
	// it. The restore's snapshot predates all of it.
	// Arrange — the session wires, and the row read is where the release lands.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()
	h.store.duringHeldPromptsRead(func() {
		h.lease.release()
		h.m.ReleaseShutdownHolds("sd_live")
		// The release's own kick has already submitted the entry and taken it out
		// of the queue. PAUSING NOW suppresses the RESTORE's kick, which fires on
		// the same condition — without it a row the apply loop wrongly re-adds is
		// popped and delivered a second time in the same breath, leaving the
		// queue empty again and the re-add invisible. The defect is the re-add
		// either way; this is what makes it observable in the queue.
		d := h.controller()
		h.m.mu.Lock()
		d.paused = true
		h.m.mu.Unlock()
	})

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert — the restore added nothing back for a row whose prompt has already
	// been delivered.
	waitFor(t, "the released prompt to reach the shim", func() bool {
		return len(h.client.promptTexts()) == 1
	})
	if es := h.entries(); len(es) != 0 {
		t.Fatalf("queue = %+v after a schedule release mid-restore, want empty — the restore re-added a prompt that has already been delivered", es)
	}
}

func TestACancelDuringARestoreIsNotResurrectedByItsStaleRowSnapshot(t *testing.T) {
	// THE RACE. The restore adopts the materialized entries under the manager
	// mutex, reads the durable rows with it RELEASED, and applies them under it
	// again. A cancel landing in that window removes the entry and drops its
	// row, and the apply loop — holding a snapshot taken before either — used to
	// add the prompt straight back.
	// Arrange — the session wires, and the row read is where the cancel lands.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()
	h.store.duringHeldPromptsRead(func() {
		if err := h.m.CancelQueueEntry("ws", "q_parked"); err != nil {
			t.Errorf("the mid-restore cancel failed: %v", err)
		}
	})

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert — the prompt the user took back is not queued.
	if es := h.entries(); len(es) != 0 {
		t.Fatalf("queue = %+v after a cancel mid-restore, want empty — the restore re-added a prompt the user cancelled", es)
	}
}

func TestARestoreThatSkipsARowDroppedByACancelSaysSoInTheCanonicalLog(t *testing.T) {
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()
	h.store.duringHeldPromptsRead(func() {
		if err := h.m.CancelQueueEntry("ws", "q_parked"); err != nil {
			t.Errorf("the mid-restore cancel failed: %v", err)
		}
	})

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	if !h.log.contains("restore SKIPPED row-dropped entries") {
		t.Fatalf("the skipped resurrection left no canonical log line naming it")
	}
}

func TestACancelLandingDuringARestoreIsTombstonedAsARowDrop(t *testing.T) {
	// The tombstone is what the apply loop consults, so it must be RECORDED at
	// the moment the entry leaves memory rather than inferred afterwards.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()
	h.store.duringHeldPromptsRead(func() {
		if err := h.m.CancelQueueEntry("ws", "q_parked"); err != nil {
			t.Errorf("the mid-restore cancel failed: %v", err)
		}
	})

	// Act.
	h.m.restoreShutdownHolds(h.controller())

	// Assert.
	if !h.log.contains(`queue row-drop TOMBSTONED entry=q_parked ws="ws" reason=cancelled_by_user`) {
		t.Fatalf("the mid-restore cancel left no tombstone record")
	}
}

func TestARestoreClearsItsTombstonesWhenItEnds(t *testing.T) {
	// A set that is never cleared is a leak, and worse: a later restore of a
	// re-submitted prompt with the same id would refuse to seed it.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()
	h.wire()
	h.store.duringHeldPromptsRead(func() {
		if err := h.m.CancelQueueEntry("ws", "q_parked"); err != nil {
			t.Errorf("the mid-restore cancel failed: %v", err)
		}
	})
	h.m.restoreShutdownHolds(h.controller())

	// Act.
	h.m.mu.Lock()
	tombstoned := h.m.restoreTombstones.rowDropped("ws", "q_parked")
	h.m.mu.Unlock()

	// Assert.
	if tombstoned {
		t.Fatal("the workspace's tombstones outlived the restore that took them")
	}
}

// --- schedule cancel ---------------------------------------------------------

func TestCancellingTheScheduleShedsTheHoldFromMaterializedEntries(t *testing.T) {
	// Otherwise a client whose session never wired keeps rendering a lease
	// bubble for a schedule that no longer exists.
	// Arrange.
	h := newParkedHarness(t)
	h.lease.hold("sd_live")
	h.record("q_parked", "sd_live", "delayed")
	h.materialize()

	// Act.
	h.m.ReleaseShutdownHolds("sd_live")

	// Assert.
	if hold := h.m.QueueViews()[0].GetEntries()[0].GetShutdown(); hold != nil {
		t.Fatalf("materialized entry hold = %+v after the schedule was cancelled, want none", hold)
	}
}

// --- the empty and unwirable cases ------------------------------------------

func TestMaterializationOfAnEmptyLedgerContributesNoViews(t *testing.T) {
	// A daemon that boots with nothing parked must not invent a queue for a
	// workspace nobody is holding a prompt for.
	// Arrange.
	h := newParkedHarness(t)

	// Act.
	h.materialize()

	// Assert.
	if views := h.m.QueueViews(); len(views) != 0 {
		t.Fatalf("QueueViews = %+v on an empty ledger, want none", views)
	}
}

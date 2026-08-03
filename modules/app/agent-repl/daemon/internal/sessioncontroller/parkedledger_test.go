package sessioncontroller

import (
	"strings"
	"testing"

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
	store := newFakeHoldStore()
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
		Text: text, QueuedAtMs: 10,
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
	if len(views) != 1 || views[0].GetSessionId() != "s1" {
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
	got := h.m.QueueViews()[0].GetEntries()[0].GetShutdownHold().GetScheduleId()
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
	if hold := h.m.QueueViews()[0].GetEntries()[0].GetShutdownHold(); hold != nil {
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
	if hold := h.m.QueueViews()[0].GetEntries()[0].GetShutdownHold(); hold != nil {
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

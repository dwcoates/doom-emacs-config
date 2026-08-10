package sessioncontroller

import (
	"errors"
	"sync"
	"sync/atomic"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// THE INVARIANT: a teardown cannot strand a turn's ACCOUNTING STAMP.
//
// The turn's answer is no longer at stake here and cannot be: a terminal result
// is published the moment it arrives (terminalsettlement.go), so a teardown has
// no answer left to strand. What a teardown CAN strand is the stamp — a turn
// whose corrections never all arrived has an enrichment hold outstanding, and
// the teardown kills the only process that could ever file the rest.
//
// These tests pin the settlement to the teardown PROLOGUE, which is the only
// window where it is possible: the axis close that follows runs after the
// eviction and the cancel, with no route back to the consumer.
// ---------------------------------------------------------------------------

// settlementRecordingStore stamps every accounting record with whether the
// session controller context had already been cancelled when it landed. It is
// what makes "the settlement runs BEFORE the cancel" an assertion about
// behavior rather than an inspection of source order.
type settlementRecordingStore struct {
	cancelled *atomic.Bool
	// refuse names one turn the store rejects, so a test can prove the refusal
	// did not abort the settlement of the others. Empty refuses nothing.
	refuse string

	mu          sync.Mutex
	recorded    []string
	afterCancel []bool
}

func (s *settlementRecordingStore) Record(_ string, accounting *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error) {
	s.mu.Lock()
	s.recorded = append(s.recorded, accounting.GetTurnId())
	s.afterCancel = append(s.afterCancel, s.cancelled.Load())
	s.mu.Unlock()
	if accounting.GetTurnId() == s.refuse {
		return nil, errors.New("accounting store is unwritable for this turn")
	}
	return accounting, nil
}

func (s *settlementRecordingStore) List(string) ([]*frontendv1.TurnAccounting, error) {
	return nil, nil
}

// turnsRecorded reports one entry per settlement attempt, in order.
func (s *settlementRecordingStore) turnsRecorded() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.recorded...)
}

// settlementsAfterCancel reports one entry per settlement attempt, true when
// the attempt landed after the cancel.
func (s *settlementRecordingStore) settlementsAfterCancel() []bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]bool(nil), s.afterCancel...)
}

// newHeldResultRig is a wired, settled workspace whose consumer has one
// outstanding accounting stamp per named turn, with the cancel instrumented so
// each settlement can say which side of it landed on.
func newHeldResultRig(t *testing.T, turnIDs ...string) (*Manager, *sessionController, *settlementRecordingStore) {
	t.Helper()
	m, _, _ := newHibernationRig(t)
	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()
	if d == nil {
		t.Fatal("the rig brought up no session controller for ws")
	}
	cancelled := &atomic.Bool{}
	underlying := d.cancel
	d.cancel = func() {
		cancelled.Store(true)
		underlying()
	}
	store := &settlementRecordingStore{cancelled: cancelled}
	d.consumer.accountingStore = store
	for i, turnID := range turnIDs {
		d.consumer.accounting.turns[turnID] = &accountingTurn{}
		d.consumer.noteTerminalResult(turnID, terminalResultEvent(t, uint64(100+i)))
	}
	return m, d, store
}

// THE REPORTED DEFECT, IN ITS SURVIVING HALF. A workspace hibernated with a
// stamp outstanding settled nothing: the shim died, no `TurnEnded` followed,
// and the turn sat unaccounted until the daemon exited — hibernation refused
// every thirty seconds on a turn nothing could ever finish.
func TestHibernateSettlesTheOutstandingAccountingStamp(t *testing.T) {
	// Arrange.
	const turnID = "t-held-1"
	m, d, store := newHeldResultRig(t, turnID)

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert.
	if held := d.consumer.heldTerminalResult(turnID); held != nil {
		t.Fatal("the turn's stamp is still outstanding after the teardown; nothing will ever settle it")
	}
	if got := store.turnsRecorded(); len(got) != 1 || got[0] != turnID {
		t.Fatalf("settlements = %v, want the outstanding turn settled by the teardown", got)
	}
}

// The settlement has to land in the teardown PROLOGUE. After the cancel the
// consumer has been stood down, which is why the axis close cannot own this.
func TestHibernateSettlesTheStampBeforeTheCancel(t *testing.T) {
	// Arrange.
	m, _, store := newHeldResultRig(t, "t-held-order")

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert.
	settlements := store.settlementsAfterCancel()
	if len(settlements) == 0 {
		t.Fatal("nothing was settled, so there is no ordering to assert")
	}
	for i, afterCancel := range settlements {
		if afterCancel {
			t.Fatalf("settlement %d landed AFTER the session controller cancel; the consumer it travelled through was already stood down", i)
		}
	}
}

// A SESSION-SCOPED STOP AIMED AT A SUPERSEDED RECORD MUST NOT SPEAK FOR THE
// LIVE ONE. The only consumer in reach belongs to the session still driving the
// workspace, and its outstanding stamps belong to turns that session will still
// settle itself.
func TestSessionScopedHibernateLeavesTheLiveControllersOutstandingStamp(t *testing.T) {
	// Arrange — the workspace is driven by s1; the stop names a different record.
	const turnID = "t-held-live"
	m, d, store := newHeldResultRig(t, turnID)

	// Act.
	if err := m.HibernateSession("ws", "s-superseded", StopCauseSessionDeleted()); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}

	// Assert.
	if held := d.consumer.heldTerminalResult(turnID); held == nil {
		t.Fatal("a stop aimed at a superseded record settled the LIVE session's stamp, accounting for a turn its own shim is still running")
	}
	if got := len(store.turnsRecorded()); got != 0 {
		t.Fatalf("settlements = %d, want 0 — this stop owns no accounting on the live session", got)
	}
}

// ONE UNPERSISTABLE TURN MUST NOT STRAND THE OTHERS. The settlement runs per
// turn and continues past a failure, because each outstanding stamp is
// independent evidence about a different turn.
func TestTeardownSettlesEveryOutstandingTurnWhenOneCannotPersist(t *testing.T) {
	// Arrange — three outstanding turns, the middle one unwritable.
	const doomed = "t-held-b"
	m, d, store := newHeldResultRig(t, "t-held-a", doomed, "t-held-c")
	store.refuse = doomed

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert — every turn was attempted, and the refused one stays outstanding
	// rather than being silently discharged.
	if got := len(store.turnsRecorded()); got != 3 {
		t.Fatalf("turns attempted = %d (%v), want all 3 — a failure aborted the settlement loop", got, store.turnsRecorded())
	}
	if held := d.consumer.heldTerminalResult(doomed); held == nil {
		t.Fatal("the unpersistable turn's stamp was discharged anyway; its accounting record does not exist, so the discharge would claim a settlement nothing accounts for")
	}
}

// The ordinary teardown holds nothing, and it must stay silent rather than
// manufacturing an empty settlement for turns that already closed honestly.
func TestTeardownWithNothingOutstandingSettlesNothing(t *testing.T) {
	// Arrange.
	m, _, store := newHeldResultRig(t)

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert.
	if got := len(store.turnsRecorded()); got != 0 {
		t.Fatalf("settlements = %d, want 0 for a teardown with nothing outstanding", got)
	}
}

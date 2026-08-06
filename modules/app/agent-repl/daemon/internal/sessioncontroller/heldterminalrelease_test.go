package sessioncontroller

import (
	"errors"
	"sync"
	"sync/atomic"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// THE INVARIANT: a teardown cannot strand a terminal result.
//
// A turn's terminal result is parked until that turn's end boundary is known
// (sinks.go, pendingTerminal). A teardown kills the only process that could
// ever supply that boundary, so unless the teardown itself publishes the
// parked result the user is left with a fully rendered answer that the webapp
// can never mark complete — every streamed chunk present and no result to pair
// them with.
//
// These tests pin the release to the teardown PROLOGUE, which is the only
// window where it is possible: the axis close that follows runs after the
// eviction and the cancel, with no route back to the consumer.
// ---------------------------------------------------------------------------

// releaseRecordingPusher stamps every conversation push with whether the
// session controller context had already been cancelled when it landed. It is
// what makes "the release runs BEFORE the cancel" an assertion about behavior
// rather than an inspection of source order.
type releaseRecordingPusher struct {
	Pusher
	cancelled *atomic.Bool

	mu          sync.Mutex
	afterCancel []bool
}

func (p *releaseRecordingPusher) PushConversationDelta(cd *frontendv1.ConversationDelta) {
	p.mu.Lock()
	p.afterCancel = append(p.afterCancel, p.cancelled.Load())
	p.mu.Unlock()
	p.Pusher.PushConversationDelta(cd)
}

// conversationPushes reports one entry per conversation push, true when the
// push landed after the cancel.
func (p *releaseRecordingPusher) conversationPushes() []bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]bool(nil), p.afterCancel...)
}

// selectivelyFailingTurnAccountingStore refuses exactly one turn and records every turn it
// was asked about, so a test can prove the refusal did not abort the rest.
type selectivelyFailingTurnAccountingStore struct {
	refuse string

	mu       sync.Mutex
	recorded []string
}

func (s *selectivelyFailingTurnAccountingStore) Record(_ string, accounting *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error) {
	s.mu.Lock()
	s.recorded = append(s.recorded, accounting.GetTurnId())
	s.mu.Unlock()
	if accounting.GetTurnId() == s.refuse {
		return nil, errors.New("accounting store is unwritable for this turn")
	}
	return accounting, nil
}

func (s *selectivelyFailingTurnAccountingStore) List(string) ([]*frontendv1.TurnAccounting, error) {
	return nil, nil
}

func (s *selectivelyFailingTurnAccountingStore) turnsRecorded() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.recorded...)
}

// newHeldResultRig is a wired, settled workspace whose consumer holds one
// terminal result per named turn, with the cancel instrumented so each push can
// say which side of it landed on.
func newHeldResultRig(t *testing.T, turnIDs ...string) (*Manager, *sessionController, *releaseRecordingPusher) {
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
	push := &releaseRecordingPusher{Pusher: d.consumer.push, cancelled: cancelled}
	d.consumer.push = push
	for i, turnID := range turnIDs {
		d.consumer.accounting.turns[turnID] = &accountingTurn{}
		d.consumer.holdTerminalResult(turnID, terminalResultEvent(t, uint64(100+i)))
	}
	return m, d, push
}

// THE REPORTED DEFECT. A workspace hibernated while a result is held published
// nothing: the shim died, no `TurnEnded` followed, and the answer sat in the
// map until the daemon exited. The user saw the whole response with no
// completion border, because the webapp derives finality by pairing a turn's
// last text with the result that closes it.
func TestHibernateReleasesTheHeldTerminalResult(t *testing.T) {
	// Arrange.
	const turnID = "t-held-1"
	m, d, push := newHeldResultRig(t, turnID)

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert.
	if held := d.consumer.heldTerminalResult(turnID); held != nil {
		t.Fatal("the terminal result is still held after the teardown; nothing will ever release it")
	}
	if len(push.conversationPushes()) == 0 {
		t.Fatal("the held result was discarded rather than published; the user's answer never reaches the feed")
	}
}

// The release has to land in the teardown PROLOGUE. After the cancel the
// consumer has been stood down, which is why the axis close cannot own this.
func TestHibernateReleasesTheHeldResultBeforeTheCancel(t *testing.T) {
	// Arrange.
	m, _, push := newHeldResultRig(t, "t-held-order")

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert.
	pushes := push.conversationPushes()
	if len(pushes) == 0 {
		t.Fatal("nothing was published, so there is no ordering to assert")
	}
	for i, afterCancel := range pushes {
		if afterCancel {
			t.Fatalf("conversation push %d landed AFTER the session controller cancel; the consumer it travelled through was already stood down", i)
		}
	}
}

// A SESSION-SCOPED STOP AIMED AT A SUPERSEDED RECORD MUST NOT SPEAK FOR THE
// LIVE ONE. The only consumer in reach belongs to the session still driving the
// workspace, and its held results belong to turns that session will still
// close itself.
func TestSessionScopedHibernateLeavesTheLiveControllersHeldResult(t *testing.T) {
	// Arrange — the workspace is driven by s1; the stop names a different record.
	const turnID = "t-held-live"
	m, d, push := newHeldResultRig(t, turnID)

	// Act.
	if err := m.HibernateSession("ws", "s-superseded", StopCauseSessionDeleted()); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}

	// Assert.
	if held := d.consumer.heldTerminalResult(turnID); held == nil {
		t.Fatal("a stop aimed at a superseded record released the LIVE session's held result, answering a turn its own shim is still running")
	}
	if got := len(push.conversationPushes()); got != 0 {
		t.Fatalf("conversation pushes = %d, want 0 — this stop owns no conversation on the live session", got)
	}
}

// ONE UNPERSISTABLE TURN MUST NOT STRAND THE OTHERS. The release settles per
// turn and continues past a failure, because each retained result is
// independent evidence of a different answer the user is owed.
func TestTeardownReleasesEveryHeldTurnWhenOneCannotPersist(t *testing.T) {
	// Arrange — three held turns, the middle one unwritable.
	const doomed = "t-held-b"
	m, d, push := newHeldResultRig(t, "t-held-a", doomed, "t-held-c")
	store := &selectivelyFailingTurnAccountingStore{refuse: doomed}
	d.consumer.accountingStore = store

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert — every turn was attempted, the two writable ones published, and
	// the refused one stays held rather than being silently dropped.
	if got := len(store.turnsRecorded()); got != 3 {
		t.Fatalf("turns attempted = %d (%v), want all 3 — a failure aborted the release loop", got, store.turnsRecorded())
	}
	if got := len(push.conversationPushes()); got != 2 {
		t.Fatalf("conversation pushes = %d, want 2 — the writable turns must publish regardless of their neighbour", got)
	}
	if held := d.consumer.heldTerminalResult(doomed); held == nil {
		t.Fatal("the unpersistable turn's result was released anyway; its accounting record does not exist, so publishing it would report an answer nothing accounts for")
	}
}

// The ordinary teardown holds nothing, and it must stay silent rather than
// manufacturing an empty settlement for turns that already closed honestly.
func TestTeardownWithNothingHeldPublishesNothing(t *testing.T) {
	// Arrange.
	m, _, push := newHeldResultRig(t)

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert.
	if got := len(push.conversationPushes()); got != 0 {
		t.Fatalf("conversation pushes = %d, want 0 for a teardown with nothing held", got)
	}
}

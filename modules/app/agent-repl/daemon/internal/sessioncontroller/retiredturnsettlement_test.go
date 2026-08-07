package sessioncontroller

import (
	"errors"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// THE INVARIANT: a settlement naming an already-settled turn must not kill the
// daemon.
//
// A turn's accounting is released by three independent authorities (see
// settleTurnAccounting): a stream `TurnEnded`, a synthesized close, and a
// teardown. Each snapshots the held set and then settles it, and nothing
// serializes one snapshot against the other's discharge — so two of them can
// name the same turn. Live, a synthesized close (cause=bringup_failed) and a
// teardown (path=session_controller_exit) did exactly that, and the second
// settlement reached a reducer entry the first had already retired. The commit
// panicked, the daemon exited 2, and the replayed rows that produced the
// collision would reproduce it on the next boot.
// ---------------------------------------------------------------------------

// settlingTurnAccountingStore is a store that actually keeps what it records,
// which is what makes "the second settlement is served from the store" an
// assertion about behavior rather than about a stub's return value.
type settlingTurnAccountingStore struct {
	mu       sync.Mutex
	byTurn   map[string]*frontendv1.TurnAccounting
	listErr  error
	recorded []string
}

func newSettlingTurnAccountingStore() *settlingTurnAccountingStore {
	return &settlingTurnAccountingStore{byTurn: map[string]*frontendv1.TurnAccounting{}}
}

func (s *settlingTurnAccountingStore) Record(_ string, accounting *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.recorded = append(s.recorded, accounting.GetTurnId())
	s.byTurn[accounting.GetTurnId()] = accounting
	return accounting, nil
}

func (s *settlingTurnAccountingStore) List(string) ([]*frontendv1.TurnAccounting, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.listErr != nil {
		return nil, s.listErr
	}
	out := make([]*frontendv1.TurnAccounting, 0, len(s.byTurn))
	for _, accounting := range s.byTurn {
		out = append(out, accounting)
	}
	return out, nil
}

func (s *settlingTurnAccountingStore) turnsRecorded() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.recorded...)
}

// newRetiredTurnRig builds a consumer whose turn has already been settled once:
// the reducer entry is retired, the hold is discharged, and the store holds the
// settlement. That is the exact state the second release authority arrives in.
func newRetiredTurnRig(t *testing.T, turnID string) (*consumer, *settlingTurnAccountingStore, *fakePusher, *levelSplitLogs) {
	t.Helper()
	push := &fakePusher{}
	logs := &levelSplitLogs{}
	store := newSettlingTurnAccountingStore()
	c := newConsumer("ws", "s1", push, &fakeApplier{}, &fakeProgress{}, newFakeClearCompactStore(), store,
		logs.logf, nil, nil, nil, nil, nil)
	c.warnf = logs.warnf
	c.accounting.turns[turnID] = &accountingTurn{}
	c.holdTerminalResult(turnID, terminalResultEvent(t, 3481))
	if err := c.settleTurnAccounting(turnID); err != nil {
		t.Fatalf("first settlement: %v", err)
	}
	return c, store, push, logs
}

func TestCommitResolvedRetiresAKnownTurn(t *testing.T) {
	// Arrange.
	r := newTurnAccountingReducer()
	r.turns["t"] = &accountingTurn{}
	r.activeTurnID = "t"

	// Act.
	err := r.commitResolved("t")

	// Assert.
	if err != nil {
		t.Fatalf("commitResolved error = %v, want nil for a turn the reducer holds", err)
	}
	if r.hasTurn("t") || r.activeTurnID != "" {
		t.Fatalf("hasTurn = %v activeTurnID = %q, want the turn retired and the active id cleared", r.hasTurn("t"), r.activeTurnID)
	}
}

// THE PANIC ITSELF. The condition is reachable from replayed data, so it may
// never take the process down with it.
func TestCommitResolvedReportsAnUnknownTurnInsteadOfPanicking(t *testing.T) {
	// Arrange.
	r := newTurnAccountingReducer()

	// Act.
	err := r.commitResolved("fe-1266-4cf1")

	// Assert.
	if !errors.Is(err, ErrAccountingCommitUnknownTurn) {
		t.Fatalf("commitResolved error = %v, want ErrAccountingCommitUnknownTurn", err)
	}
}

func TestCommitResolvedNamesTheUnknownTurnInItsError(t *testing.T) {
	// Arrange.
	r := newTurnAccountingReducer()

	// Act.
	err := r.commitResolved("fe-1266-4cf1")

	// Assert -- an error that omits the turn cannot be correlated with the
	// stream row that produced it.
	if err == nil || !strings.Contains(err.Error(), "fe-1266-4cf1") {
		t.Fatalf("commitResolved error = %v, want the turn id named", err)
	}
}

// THE LIVE CRASH, END TO END: a second release authority settles a turn the
// first already retired.
func TestSecondSettlementOfARetiredTurnSucceeds(t *testing.T) {
	// Arrange.
	const turnID = "fe-1266-4cf1"
	c, _, _, _ := newRetiredTurnRig(t, turnID)

	// Act.
	err := c.settleTurnAccounting(turnID)

	// Assert.
	if err != nil {
		t.Fatalf("second settlement error = %v, want the already-settled turn served rather than refused", err)
	}
}

// The store's row is authoritative on the second pass; recomputing would write
// an evidence-free record over a measured one.
func TestSecondSettlementOfARetiredTurnDoesNotRecordAgain(t *testing.T) {
	// Arrange.
	const turnID = "fe-1266-4cf1"
	c, store, _, _ := newRetiredTurnRig(t, turnID)

	// Act.
	if err := c.settleTurnAccounting(turnID); err != nil {
		t.Fatalf("second settlement: %v", err)
	}

	// Assert.
	if got := len(store.turnsRecorded()); got != 1 {
		t.Fatalf("Record calls = %d (%v), want 1 — the second settlement must serve the persisted row, not overwrite it", got, store.turnsRecorded())
	}
}

// The hold is discharged by whichever authority settles first, so the second
// must publish nothing: a duplicate result would draw the answer twice.
func TestSecondSettlementOfARetiredTurnPublishesNothingTwice(t *testing.T) {
	// Arrange.
	const turnID = "fe-1266-4cf1"
	c, _, push, _ := newRetiredTurnRig(t, turnID)
	before := len(push.conversationDeltas())

	// Act.
	if err := c.settleTurnAccounting(turnID); err != nil {
		t.Fatalf("second settlement: %v", err)
	}

	// Assert.
	if got := len(push.conversationDeltas()); got != before {
		t.Fatalf("conversation deltas = %d, want %d — the second settlement republished a result the first already delivered", got, before)
	}
}

// The collision is served correctly AND recorded loudly; a silent recovery
// would hide a real ordering defect behind a working feed.
func TestSecondSettlementOfARetiredTurnTakesTheWarnChannel(t *testing.T) {
	// Arrange.
	const turnID = "fe-1266-4cf1"
	c, _, _, logs := newRetiredTurnRig(t, turnID)

	// Act.
	if err := c.settleTurnAccounting(turnID); err != nil {
		t.Fatalf("second settlement: %v", err)
	}

	// Assert.
	if !strings.Contains(strings.Join(logs.warn, "\n"), "ACCOUNTING SETTLEMENT NAMES RETIRED TURN") {
		t.Fatalf("warn = %v, want the retired-turn settlement recorded at warn", logs.warn)
	}
}

func TestSecondSettlementOfARetiredTurnRecordsItsBranchDecision(t *testing.T) {
	// Arrange.
	const turnID = "fe-1266-4cf1"
	c, _, _, logs := newRetiredTurnRig(t, turnID)

	// Act.
	if err := c.settleTurnAccounting(turnID); err != nil {
		t.Fatalf("second settlement: %v", err)
	}

	// Assert -- the branch taken is the whole diagnostic value of the record.
	if !strings.Contains(strings.Join(logs.warn, "\n"), "decision=served_persisted_settlement") {
		t.Fatalf("warn = %v, want the served-from-store decision named", logs.warn)
	}
}

func TestSecondSettlementOfARetiredTurnNamesItsSessionWorkspaceAndTurn(t *testing.T) {
	// Arrange.
	const turnID = "fe-1266-4cf1"
	c, _, _, logs := newRetiredTurnRig(t, turnID)

	// Act.
	if err := c.settleTurnAccounting(turnID); err != nil {
		t.Fatalf("second settlement: %v", err)
	}

	// Assert.
	record := strings.Join(logs.warn, "\n")
	for _, want := range []string{"session=s1", "ws=ws", "turn_id=" + turnID, "seq="} {
		if !strings.Contains(record, want) {
			t.Fatalf("warn = %v, want %q in the retired-turn record", logs.warn, want)
		}
	}
}

// NO REDUCER ENTRY AND NO PERSISTED ROW IS NOT A RE-SETTLEMENT, it is a turn
// nothing accounts for. The error must reach the caller rather than be softened
// into a success that reports accounting the store never held.
func TestSettlementOfATurnWithNoPersistedRecordReportsTheUnknownTurn(t *testing.T) {
	// Arrange.
	logs := &levelSplitLogs{}
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, &fakeProgress{}, newFakeClearCompactStore(),
		newSettlingTurnAccountingStore(), logs.logf, nil, nil, nil, nil, nil)
	c.warnf = logs.warnf

	// Act.
	err := c.settleTurnAccounting("t-never-admitted")

	// Assert.
	if !errors.Is(err, ErrAccountingCommitUnknownTurn) {
		t.Fatalf("settleTurnAccounting error = %v, want ErrAccountingCommitUnknownTurn", err)
	}
}

func TestSettlementOfATurnWithNoPersistedRecordLeavesItsResultHeld(t *testing.T) {
	// Arrange -- a result is held for a turn the reducer never admitted.
	const turnID = "t-never-admitted"
	logs := &levelSplitLogs{}
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, &fakeProgress{}, newFakeClearCompactStore(),
		newSettlingTurnAccountingStore(), logs.logf, nil, nil, nil, nil, nil)
	c.warnf = logs.warnf
	c.holdTerminalResult(turnID, terminalResultEvent(t, 7))

	// Act.
	if err := c.settleTurnAccounting(turnID); err == nil {
		t.Fatal("settleTurnAccounting error = nil, want the unaccounted turn refused")
	}

	// Assert -- publishing it would report an answer nothing accounts for.
	if c.heldTerminalResult(turnID) == nil {
		t.Fatal("the unaccounted turn's result was discharged anyway")
	}
}

// A store that cannot be read is not evidence that the turn is unaccounted, so
// the read failure travels rather than collapsing into "no settlement exists".
func TestSettlementOfARetiredTurnSurfacesAPersistedLookupFailure(t *testing.T) {
	// Arrange.
	const turnID = "fe-1266-4cf1"
	c, store, _, _ := newRetiredTurnRig(t, turnID)
	store.mu.Lock()
	store.listErr = errors.New("state db unreadable")
	store.mu.Unlock()

	// Act.
	err := c.settleTurnAccounting(turnID)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "state db unreadable") {
		t.Fatalf("settleTurnAccounting error = %v, want the store read failure surfaced", err)
	}
}

// THE TWO-AUTHORITY COLLISION AS THE DAEMON SAW IT: the same held turn released
// twice, which used to be an exit code 2.
func TestReleasingOneHeldTurnTwiceDoesNotKillTheDaemon(t *testing.T) {
	// Arrange.
	const turnID = "fe-1266-4cf1"
	push := &fakePusher{}
	logs := &levelSplitLogs{}
	c := newConsumer("ws", "s1", push, &fakeApplier{}, &fakeProgress{}, newFakeClearCompactStore(),
		newSettlingTurnAccountingStore(), logs.logf, nil, nil, nil, nil, nil)
	c.warnf = logs.warnf
	c.accounting.turns[turnID] = &accountingTurn{}
	c.holdTerminalResult(turnID, terminalResultEvent(t, 3481))

	// Act -- a synthesized close and a teardown, each having snapshotted the
	// held set before the other discharged it.
	c.ReleaseSynthesizedTurnClose([]string{turnID}, "bringup_failed")
	c.ReleaseSynthesizedTurnClose([]string{turnID}, "session_controller_exit")

	// Assert.
	if c.heldTerminalResult(turnID) != nil {
		t.Fatal("the terminal result is still held after both releases")
	}
	if got := len(push.conversationDeltas()); got != 1 {
		t.Fatalf("conversation deltas = %d, want exactly 1 — the answer is published once, by whichever authority ran first", got)
	}
}

package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/statedb"
)

// ---------------------------------------------------------------------------
// THE TERMINAL FENCE. A bring-up refused for a vanished resume target is
// published once and then short-circuited, never re-run. See vanishedresume.go.
// ---------------------------------------------------------------------------

// errVanishedTarget is the spawner's terminal refusal, classified exactly as
// the server's resume gate classifies it: a wrapped cause carrying the
// vanished-target sentinel.
var errVanishedTarget = fmt.Errorf("server: session s1: resume target uuid-gone has no transcript in this daemon's config dir: %w", errclass.ErrResumeTargetVanished)

// TestVanishedResumeTargetFencesTheSession: the first terminal refusal publishes
// a card and spawns nothing further.
func TestVanishedResumeTargetFencesTheSession(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, errclass.ErrResumeTargetVanished) {
		t.Fatalf("ensure = %v, want the terminal vanished-resume refusal", err)
	}
	if len(h.failureCards()) != 1 {
		t.Fatalf("failure cards = %d, want exactly one standing card", len(h.failureCards()))
	}
	if !h.log.contains("bring-up TERMINALLY FAILED") {
		t.Fatal("the terminal disposition was not reported")
	}
}

// TestASecondOpenShortCircuitsWithoutBringingUp: this is the defect itself —
// the ensure machinery used to re-run the identical doomed bring-up on every
// open, sweep and reattach.
func TestASecondOpenShortCircuitsWithoutBringingUp(t *testing.T) {
	// Arrange — one terminal refusal, already fenced.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}
	spawnsAtFence := len(h.spawner.calls)

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, errclass.ErrResumeTargetVanished) {
		t.Fatalf("second ensure = %v, want the same standing refusal", err)
	}
	if len(h.spawner.calls) != spawnsAtFence {
		t.Fatalf("EnsureShim calls went %d -> %d past the fence; nothing may be brought up again",
			spawnsAtFence, len(h.spawner.calls))
	}
	if !h.log.contains("SHORT-CIRCUITED by the vanished-resume fence") {
		t.Fatal("the short-circuit was not recorded")
	}
}

// TestTheFenceCardIsPublishedOnce: a session refused N times owes the user one
// account of it, not N.
func TestTheFenceCardIsPublishedOnce(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act — three opens.
	for i := 0; i < 3; i++ {
		if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
			t.Fatalf("ensure #%d succeeded; the harness must refuse it terminally", i+1)
		}
	}

	// Assert.
	if got := len(h.failureCards()); got != 1 {
		t.Fatalf("failure cards = %d, want one", got)
	}
	if got := h.warn.count("bring-up TERMINALLY FAILED"); got != 1 {
		t.Fatalf("terminal reports = %d, want exactly one loud record", got)
	}
}

// TestTheShortCircuitIsNotReportedAsAnError: the failure was reported once; the
// refusals that follow are diagnostic detail, not new faults.
func TestTheShortCircuitIsNotReportedAsAnError(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the fenced ensure succeeded; it must return the standing refusal")
	}

	// Assert.
	if h.warn.contains("SHORT-CIRCUITED by the vanished-resume fence") {
		t.Fatal("the short-circuit took the warn channel; it belongs at debug")
	}
	if !h.log.contains("level=debug") {
		t.Fatal("the short-circuit was not tagged as a debug record")
	}
}

// TestRestartRechecksTheDiskAndUnfences: the way out is an explicit user
// action, and it re-derives the verdict rather than asserting one.
func TestRestartRechecksTheDiskAndUnfences(t *testing.T) {
	// Arrange — a fenced session whose transcript then reappears (the spawner
	// stops refusing).
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}
	h.spawner.err = nil

	// Act.
	err := h.m.RestartSession(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("RestartSession = %v, want the restart to bring the session back up", err)
	}
	if !h.log.contains("vanished-resume fence CLEARED") {
		t.Fatal("the restart did not clear the fence")
	}
	if h.m.vanishedResumeFenced("s1") {
		t.Fatal("the session is still fenced after a bring-up that succeeded")
	}
}

// TestRestartRefencesWhenTheTranscriptIsStillGone: clearing the fence entitles
// the bring-up to look again, and nothing more.
func TestRestartRefencesWhenTheTranscriptIsStillGone(t *testing.T) {
	// Arrange — a fenced session whose transcript is still missing.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}

	// Act.
	err := h.m.RestartSession(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, errclass.ErrResumeTargetVanished) {
		t.Fatalf("RestartSession = %v, want the same terminal refusal", err)
	}
	if !h.m.vanishedResumeFenced("s1") {
		t.Fatal("the session was not re-fenced; the next open would loop again")
	}
}

// TestAnOrdinarySpawnFailureIsNotFenced: the fence applies to the terminal
// disposition alone. A transient spawn failure keeps its retries.
func TestAnOrdinarySpawnFailureIsNotFenced(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errors.New("spawn: transient failure")

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the ensure succeeded; the harness must fail the spawn")
	}
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the second ensure succeeded; the harness must fail the spawn")
	}

	// Assert.
	if h.m.vanishedResumeFenced("s1") {
		t.Fatal("a transient spawn failure fenced the session; only a vanished resume target may")
	}
	if len(h.spawner.calls) != 2 {
		t.Fatalf("EnsureShim calls = %d, want both attempts to have reached the spawner", len(h.spawner.calls))
	}
}

// TestTheFenceCardIsTerminal: the card must state that its failure has no
// closing edge, because that is the only thing on the wire a client can read to
// stop its own automatic re-open loop.
func TestTheFenceCardIsTerminal(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the ensure succeeded; the harness must refuse it terminally")
	}

	// Assert.
	cards := h.failureCards()
	if len(cards) != 1 {
		t.Fatalf("failure cards = %d, want exactly one standing card", len(cards))
	}
	if !errclass.IsTerminal(cards[0]) {
		t.Fatalf("fence card lifecycle = %T, want the terminal arm", cards[0].GetLifecycle())
	}
}

// ---------------------------------------------------------------------------
// THE DURABLE HALF OF THE FENCE. A live push reaches only the clients connected
// at that instant; the card the store holds is what every later reader is
// served. See vanishedresume.go and durablereplay.go.
// ---------------------------------------------------------------------------

// fakeTerminalCardStore is a TerminalFailureCardStore over a map, with the same
// replace-on-record rule the table's ON CONFLICT clause enforces.
type fakeTerminalCardStore struct {
	mu   sync.Mutex
	rows map[string]statedb.TerminalFailureCard
	// records counts every Record call, so a test can tell a REPLACED row from
	// a row that was only written once.
	records   int
	recordErr error
	readErr   error
}

func newFakeTerminalCardStore() *fakeTerminalCardStore {
	return &fakeTerminalCardStore{rows: map[string]statedb.TerminalFailureCard{}}
}

func (f *fakeTerminalCardStore) Record(rec statedb.TerminalFailureCard) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.records++
	if f.recordErr != nil {
		return f.recordErr
	}
	f.rows[rec.SessionID] = rec
	return nil
}

func (f *fakeTerminalCardStore) Standing(sessionID string) (statedb.TerminalFailureCard, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.readErr != nil {
		return statedb.TerminalFailureCard{}, false, f.readErr
	}
	rec, ok := f.rows[sessionID]
	return rec, ok, nil
}

func (f *fakeTerminalCardStore) Withdraw(sessionID string) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	_, ok := f.rows[sessionID]
	delete(f.rows, sessionID)
	return ok, nil
}

// seed installs a standing card as a previous daemon would have left it.
func (f *fakeTerminalCardStore) seed(rec statedb.TerminalFailureCard) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.rows[rec.SessionID] = rec
}

// standingCount reports how many sessions hold a standing card.
func (f *fakeTerminalCardStore) standingCount() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.rows)
}

// recordCalls reports how many writes the fence made.
func (f *fakeTerminalCardStore) recordCalls() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.records
}

// TestTheFenceCardIsPersisted: the defect itself — the card used to exist only
// as a live push, so a client connecting after the refusal was served history
// with no account of why nothing drives it.
func TestTheFenceCardIsPersisted(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the ensure succeeded; the harness must refuse it terminally")
	}

	// Assert.
	rec, standing, err := h.cards.Standing("s1")
	if err != nil {
		t.Fatalf("Standing: %v", err)
	}
	if !standing {
		t.Fatal("the fence published its card and persisted nothing; every later reader sees no account of the refusal")
	}
	if rec.Workspace != "ws" || rec.UUID != startFailedCardUUID("s1") || len(rec.Card) == 0 {
		t.Fatalf("persisted card = %+v, want the fence's own workspace, uuid and rendered card", rec)
	}
}

// TestThePersistedCardCarriesTheLivePushesIdentity: a client that saw the live
// card and then resyncs must update ONE item, not draw a second account.
func TestThePersistedCardCarriesTheLivePushesIdentity(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the ensure succeeded; the harness must refuse it terminally")
	}

	// Assert.
	rec, _, err := h.cards.Standing("s1")
	if err != nil {
		t.Fatalf("Standing: %v", err)
	}
	pushed := h.failureCardItems()
	if len(pushed) != 1 {
		t.Fatalf("pushed failure items = %d, want exactly one", len(pushed))
	}
	if pushed[0].GetUuid() != rec.UUID {
		t.Fatalf("pushed uuid = %q, persisted uuid = %q; the two accounts must share one identity", pushed[0].GetUuid(), rec.UUID)
	}
}

// TestTheLivePushStillReachesConnectedClients: persistence is additive. A
// client connected at the instant of the refusal must still be told
// immediately rather than waiting for its next resync.
func TestTheLivePushStillReachesConnectedClients(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the ensure succeeded; the harness must refuse it terminally")
	}

	// Assert.
	if got := len(h.failureCards()); got != 1 {
		t.Fatalf("live-pushed failure cards = %d, want the one card a connected client always got", got)
	}
}

// TestARefenceReplacesTheStandingCard: the fence is cleared by a hard restart
// and re-established when the re-check finds the transcript still gone. That
// flow must restate one standing card rather than accumulate a second.
func TestARefenceReplacesTheStandingCard(t *testing.T) {
	// Arrange — fenced once, then restarted into a re-fence.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}

	// Act.
	if err := h.m.RestartSession(context.Background(), "ws"); err == nil {
		t.Fatal("the restart succeeded; the harness must refuse it terminally")
	}

	// Assert.
	if got := h.cards.standingCount(); got != 1 {
		t.Fatalf("standing terminal cards = %d, want one session to hold exactly one", got)
	}
}

// TestARefenceRewritesTheStandingCard: the re-fence must actually restate the
// claim rather than leave a stale row standing beside a fresh fence.
func TestARefenceRewritesTheStandingCard(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}

	// Act.
	if err := h.m.RestartSession(context.Background(), "ws"); err == nil {
		t.Fatal("the restart succeeded; the harness must refuse it terminally")
	}

	// Assert.
	if got := h.cards.recordCalls(); got != 2 {
		t.Fatalf("card writes = %d, want the re-fence to have REWRITTEN the standing row", got)
	}
}

// TestClearingTheFenceWithdrawsTheStandingCard: the card is durable evidence of
// a STANDING condition. A hard restart withdraws the claim, so a bring-up that
// then succeeds is not described by a card saying it cannot come up.
func TestClearingTheFenceWithdrawsTheStandingCard(t *testing.T) {
	// Arrange — fenced, then the spawner heals under the restart.
	h := newEscapeHarness(t, &fakeClient{})
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}
	h.spawner.err = nil

	// Act.
	if err := h.m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession: %v", err)
	}

	// Assert.
	if got := h.cards.standingCount(); got != 0 {
		t.Fatalf("standing terminal cards = %d, want the withdrawn claim to be gone", got)
	}
}

// TestAnUnpersistableFenceCardIsLoud: a card that could not be written is a
// card no later reader will ever see, which must never pass in silence.
func TestAnUnpersistableFenceCardIsLoud(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.cards.recordErr = errors.New("statedb: disk is gone")
	h.spawner.err = errVanishedTarget

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the ensure succeeded; the harness must refuse it terminally")
	}

	// Assert.
	if !h.log.contains("could not be persisted") {
		t.Fatal("a terminal failure card that could not be persisted was not reported")
	}
}

package sessioncontroller

import (
	"context"
	"errors"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/shimclient"
)

// cardDetails renders the details of every published failure card, for the
// failure message of a test that could not find the one it wanted.
func cardDetails(cards []*frontendv1.FailureCardView) []string {
	out := make([]string, 0, len(cards))
	for _, c := range cards {
		out = append(out, c.GetDetail())
	}
	return out
}

// ---------------------------------------------------------------------------
// A FAILED BRING-UP WITH RETRIES REMAINING IS ACTUALLY RETRIED. See
// bringupretry.go: the budget in bringupescape.go had no consumer, so a
// workspace nobody opened sat unwired with given_up=false forever.
// ---------------------------------------------------------------------------

// retryWatch reads the workspace's owed-retry watch, or nil.
func (h *escapeHarness) retryWatch(workspace string) bringUpRetryWatch {
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	w := h.m.bringUpRetries[workspace]
	if w == nil {
		return bringUpRetryWatch{}
	}
	return *w
}

// gaveUpCards counts the terminal budget-exhausted cards published so far, by
// the stable identity bringUpGaveUpUUID mints.
func (h *escapeHarness) gaveUpCards(sessionID string) int {
	n := 0
	for _, item := range h.failureCardItems() {
		if item.GetUuid() == bringUpGaveUpUUID(sessionID) {
			n++
		}
	}
	return n
}

// TestFailedBringUpIsRetriedAfterBackoff: the defect itself. One resolved
// failure with attempts remaining arms a watch, the sweep declines it until the
// backoff has elapsed, and then launches an attempt with nobody having opened
// the workspace.
func TestFailedBringUpIsRetriedAfterBackoff(t *testing.T) {
	// Arrange — one climb of the ladder that resolves in failure.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}

	// Act/Assert — nothing is due yet.
	if got := h.retryWatch("ws").failures; got != 1 {
		t.Fatalf("armed watch failures = %d, want 1", got)
	}
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts before the backoff elapsed, want 0", n)
	}

	// Act — the backoff elapses.
	h.advance(bringUpRetryBaseDelay + time.Second)

	// Assert — the sweep spends the budget on its own.
	if n := h.m.SweepFailedBringUps(); n != 1 {
		t.Fatalf("sweep launched %d attempts once due, want 1 — a workspace with retries remaining must not sit unwired", n)
	}
	if !h.log.contains("bring-up retry ARMED") {
		t.Fatal("the owed retry was never announced")
	}
}

// TestRetrySweepDoesNotStackAttempts: the sweep is idempotent. A second tick
// arriving while an attempt is still climbing must select nothing.
func TestRetrySweepDoesNotStackAttempts(t *testing.T) {
	// Arrange — a due watch whose attempt is in flight.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}
	h.advance(bringUpRetryBaseDelay + time.Second)

	// Act
	first := h.m.SweepFailedBringUps()
	second := h.m.SweepFailedBringUps()

	// Assert
	if first != 1 || second != 0 {
		t.Fatalf("sweeps launched %d then %d attempts, want 1 then 0 — the in-flight latch must stop retries stacking", first, second)
	}
	if got := h.retryWatch("ws").attempts; got != 1 {
		t.Fatalf("watch attempts = %d, want 1", got)
	}
}

// TestSuccessfulRetryClearsTheFailureCount: a retry that wires retires both the
// streak and the owed retry, so an intermittent workspace never accumulates
// toward the give-up bound.
func TestSuccessfulRetryClearsTheFailureCount(t *testing.T) {
	// Arrange — one failed climb, then a client that handshakes.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}
	h.advance(bringUpRetryBaseDelay + time.Second)

	// Act — the attempt itself, run inline so the assertion needs no timer.
	h.attemptBringUpRetrySynchronously(t, "ws")

	// Assert
	if got := h.m.bringUpFailuresFor("s1"); got != 0 {
		t.Fatalf("consecutive failures after a wired retry = %d, want 0", got)
	}
	if got := h.retryWatch("ws"); got.sessionID != "" {
		t.Fatalf("the owed-retry watch survived a wired retry: %+v", got)
	}
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts against a wired workspace, want 0", n)
	}
}

// attemptBringUpRetrySynchronously runs one automatic attempt on the calling
// goroutine, which is what SweepFailedBringUps launches. Calling it directly
// keeps the assertion above deterministic without waiting on a goroutine.
func (h *escapeHarness) attemptBringUpRetrySynchronously(t *testing.T, workspace string) {
	t.Helper()
	h.mu.Lock()
	h.clients = append(h.clients, &fakeClient{})
	h.mu.Unlock()
	h.m.attemptBringUpRetry(workspace)
}

// TestExhaustingTheBudgetCardsTheTerminalStateOnce: the budget cannot be spent
// silently. Reaching the bound publishes ONE typed terminal card naming the
// park and the way out, and a further refused attempt does not draw a second.
func TestExhaustingTheBudgetCardsTheTerminalStateOnce(t *testing.T) {
	// Arrange
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter)...)

	// Act — exhaust the bound, then ask once more (refused by the park).
	failBringUpsToTheBound(t, h)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure past the bound succeeded; it must be refused by the park")
	}

	// Assert
	if got := h.gaveUpCards("s1"); got != 1 {
		t.Fatalf("terminal budget-exhausted cards = %d, want exactly 1", got)
	}
	if !h.warn.contains("bring-up budget EXHAUSTED") {
		t.Fatal("the exhausted budget was not reported loudly")
	}
	if !h.retryWatch("ws").givenUp {
		t.Fatal("the watch does not record the exhausted budget, so the park's own cooldown is not what gates the next attempt")
	}
}

// TestExhaustedBudgetIsStillRetriedWhenTheParkExpires: terminal is LOUD, not
// forever. The park's cooldown gates the next automatic attempt rather than
// cancelling it.
func TestExhaustedBudgetIsStillRetriedWhenTheParkExpires(t *testing.T) {
	// Arrange
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter)...)
	failBringUpsToTheBound(t, h)

	// Act/Assert — inside the park, nothing is launched.
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts inside the park, want 0", n)
	}
	h.advance(bringUpParkCooldown + time.Second)
	if n := h.m.SweepFailedBringUps(); n != 1 {
		t.Fatalf("sweep launched %d attempts after the park expired, want 1 — an exhausted budget must not dead-end the workspace", n)
	}
}

// TestRetryWatchIsDroppedForAWorkspaceThatCameUpAnyway: a user who opens the
// workspace answers the owed retry, and the sweep must not re-ensure behind
// them.
func TestRetryWatchIsDroppedForAWorkspaceThatCameUpAnyway(t *testing.T) {
	// Arrange — a due watch, then a bring-up that wires.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}
	h.advance(bringUpRetryBaseDelay + time.Second)
	h.mu.Lock()
	h.clients = append(h.clients, &fakeClient{})
	h.mu.Unlock()
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("the user's own open failed: %v", err)
	}

	// Act/Assert
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts against a live workspace, want 0", n)
	}
}

// TestBringUpRetryDelayBacksOff: the cadence widens with the streak and is
// capped, so a workspace that cannot start does not re-climb in a tight loop.
func TestBringUpRetryDelayBacksOff(t *testing.T) {
	tests := []struct {
		name     string
		failures int
		want     time.Duration
	}{
		{name: "first failure", failures: 1, want: bringUpRetryBaseDelay},
		{name: "second failure doubles", failures: 2, want: 2 * bringUpRetryBaseDelay},
		{name: "a long streak is capped", failures: 20, want: bringUpRetryMaxDelay},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := bringUpRetryDelay(tc.failures); got != tc.want {
				t.Fatalf("bringUpRetryDelay(%d) = %s, want %s", tc.failures, got, tc.want)
			}
		})
	}
}

// TestHibernationDropsTheOwedRetry: a session put to sleep on purpose must not
// be woken by the retry sweep.
func TestHibernationDropsTheOwedRetry(t *testing.T) {
	// Arrange — an owed retry standing over a failed bring-up.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}

	// Act — the deliberate stand-down edge.
	h.m.clearBringUpRetry("ws", "hibernated")

	// Assert
	h.advance(bringUpRetryMaxDelay)
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts for a hibernated workspace, want 0", n)
	}
}

// ---------------------------------------------------------------------------
// THE DAEMON OWNS REVIVING A WORKSPACE WHOSE SESSION DIED. See bringupretry.go:
// a session that came up, wired and then lost its shim owed nothing at all, so
// coming back was a property of whether a person happened to be looking.
// ---------------------------------------------------------------------------

// mutableLocator is the registry's workspace->session binding under the test's
// control, so a test can CLOSE a workspace (its record goes terminal, and the
// locator stops naming a session for it) while a watch is standing.
type mutableLocator struct {
	mu sync.Mutex
	m  map[string]string
}

func (l *mutableLocator) Locate(ws string) (string, bool) {
	l.mu.Lock()
	defer l.mu.Unlock()
	id, ok := l.m[ws]
	return id, ok
}

func (l *mutableLocator) close(ws string) {
	l.mu.Lock()
	defer l.mu.Unlock()
	delete(l.m, ws)
}

// revivalHarness is one manager with a durable hibernation record, a mutable
// workspace binding, and an injected clock, which is everything the revival
// duty's three suppressors are expressed in terms of.
type revivalHarness struct {
	m       *Manager
	locator *mutableLocator
	hib     *fakeHibernations
	spawner *fakeSpawner
	log     *logCapture
	clockMs atomic.Int64

	mu      sync.Mutex
	clients []*fakeClient
}

func (h *revivalHarness) advance(d time.Duration) { h.clockMs.Add(d.Milliseconds()) }

func newRevivalHarness(t *testing.T, clients ...*fakeClient) *revivalHarness {
	t.Helper()
	h := &revivalHarness{
		locator: &mutableLocator{m: map[string]string{"ws": "s1"}},
		hib:     newFakeHibernations(),
		spawner: &fakeSpawner{resume: map[string]string{}},
		log:     &logCapture{},
		clients: clients,
	}
	h.clockMs.Store(1_000_000)
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               &fakeApplier{},
		Spawner:           h.spawner,
		Locator:           h.locator,
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   emptyTurnAccountingStore{},
		Registrar:         &fakeRegistrar{},
		Hibernations:      h.hib,
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		Now:               h.clockMs.Load,
		Logf:              h.log.logf,
		Warnf:             h.log.logf,
		newClient: func(c shimclient.Config) sessionClient {
			h.mu.Lock()
			defer h.mu.Unlock()
			if len(h.clients) == 0 {
				return &fakeClient{cfg: c}
			}
			next := h.clients[0]
			h.clients = h.clients[1:]
			next.cfg = c
			return next
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	h.m = m
	return h
}

// watch reads the workspace's owed watch, or the zero value plus false.
func (h *revivalHarness) watch(workspace string) (bringUpRetryWatch, bool) {
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	w := h.m.bringUpRetries[workspace]
	if w == nil {
		return bringUpRetryWatch{}, false
	}
	return *w, true
}

// sleep writes the durable record a deliberate stand-down leaves behind, which
// is the same record a merge's teardown writes (mergedteardown.go).
func (h *revivalHarness) sleep(t *testing.T, sessionID, cause string) {
	t.Helper()
	if err := h.hib.HibernationChanged(sessionID, registry.HibernationDetail{
		Cause: cause, SinceMs: h.clockMs.Load(),
	}); err != nil {
		t.Fatalf("recording the hibernation: %v", err)
	}
}

// TestASessionThatDiedArmsARevival: the defect. A shim that dies under a wired
// session used to leave the axis blue and nothing else, so the workspace stayed
// dead until a human opened it.
func TestASessionThatDiedArmsARevival(t *testing.T) {
	// Arrange — a live session whose client is about to fail terminally.
	died := make(chan error, 1)
	h := newRevivalHarness(t, &fakeClient{runResult: died})
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("ensure: %v", err)
	}

	// Act — the shim dies. The exit tail runs on its own goroutine, and the
	// record it writes is what releases the wait.
	died <- errors.New("shim died after connect")
	h.log.waitFor(t, "session revival ARMED")

	// Assert
	w, ok := h.watch("ws")
	if !ok {
		t.Fatal("no watch was armed; a session that died owes the daemon a bring-up")
	}
	if !w.revival {
		t.Fatal("the watch does not record that a DEATH armed it, so the log cannot say why the daemon is climbing")
	}
}

// TestACleanTeardownArmsNoRevival: the discriminator. Every clean cancel of a
// controller ctx belongs to something that ASKED for the session to stop, and
// reviving those would fight the caller that asked.
func TestACleanTeardownArmsNoRevival(t *testing.T) {
	// Arrange
	stopped := make(chan error, 1)
	h := newRevivalHarness(t, &fakeClient{runResult: stopped})
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("ensure: %v", err)
	}

	// Act — Run ends with no error, which is what an asked-for teardown looks
	// like from the exit tail.
	stopped <- nil
	h.log.waitFor(t, "session controller exited CLEANLY")

	// Assert
	if _, ok := h.watch("ws"); ok {
		t.Fatal("a deliberate teardown armed a revival; the daemon would fight whoever asked for the stop")
	}
}

// TestAHibernatedSessionIsNeverArmedForRevival: the arming-time suppressor. A
// session put to sleep must not acquire an owed bring-up at all.
func TestAHibernatedSessionIsNeverArmedForRevival(t *testing.T) {
	// Arrange
	h := newRevivalHarness(t)
	h.sleep(t, "s1", registry.HibernationCauseIdleCutoff)

	// Act
	h.m.armSessionRevival("ws", "s1", errors.New("shim died after connect"))

	// Assert
	if _, ok := h.watch("ws"); ok {
		t.Fatal("a sleeping session was armed for revival; the sleep exists to put the cost to the user first")
	}
}

// TestAHibernatedWorkspaceIsNotRevivedBySweep: the SWEEP-time suppressor, which
// is the one that matters. A workspace can be put to sleep between the death
// that armed the watch and the attempt it is due, and a decision taken only at
// arming time would spawn a session the user had since put away.
func TestAHibernatedWorkspaceIsNotRevivedBySweep(t *testing.T) {
	// Arrange — armed while awake, asleep by the time it is due.
	h := newRevivalHarness(t)
	h.m.armSessionRevival("ws", "s1", errors.New("shim died after connect"))
	h.advance(bringUpRetryBaseDelay + time.Second)
	h.sleep(t, "s1", registry.HibernationCauseIdleCutoff)

	// Act
	launched := h.m.SweepFailedBringUps()

	// Assert
	if launched != 0 {
		t.Fatalf("sweep launched %d attempts over a sleeping workspace, want 0", launched)
	}
	if _, ok := h.watch("ws"); ok {
		t.Fatal("the owed watch survived the sleep that made it meaningless")
	}
}

// TestAMergedWorkspaceIsNotRevived: a merge stands its workspace down through
// the same durable sleep (mergedteardown.go), and its commits are on the target
// — bringing its session back would hand the user a workspace they finished.
func TestAMergedWorkspaceIsNotRevived(t *testing.T) {
	// Arrange
	h := newRevivalHarness(t)
	h.m.armSessionRevival("ws", "s1", errors.New("shim died after connect"))
	h.advance(bringUpRetryBaseDelay + time.Second)
	// A merge stands its workspace down through Manager.Hibernate, which is
	// the deliberate FORCED sleep rather than an idle one.
	h.sleep(t, "s1", registry.HibernationCauseForced)

	// Act
	launched := h.m.SweepFailedBringUps()

	// Assert
	if launched != 0 {
		t.Fatalf("sweep launched %d attempts over a merged workspace, want 0", launched)
	}
}

// TestAClosedWorkspaceIsNotRevived: closing retires the workspace's record, so
// the locator names no session for it. Nothing may be brought up on behalf of a
// workspace the user put away.
func TestAClosedWorkspaceIsNotRevived(t *testing.T) {
	// Arrange
	h := newRevivalHarness(t)
	h.m.armSessionRevival("ws", "s1", errors.New("shim died after connect"))
	h.advance(bringUpRetryBaseDelay + time.Second)
	h.locator.close("ws")

	// Act
	launched := h.m.SweepFailedBringUps()

	// Assert
	if launched != 0 {
		t.Fatalf("sweep launched %d attempts over a closed workspace, want 0", launched)
	}
	if _, ok := h.watch("ws"); ok {
		t.Fatal("the owed watch survived the close; no later sweep can make that record live again")
	}
}

// TestRevivalIsIdempotentUnderConcurrentTriggers: two triggers must not produce
// two shims for one session. The selection and the in-flight latch are taken
// under ONE acquisition, so however many sweepers run at once exactly one climb
// exists for a workspace.
func TestRevivalIsIdempotentUnderConcurrentTriggers(t *testing.T) {
	// Arrange — one due watch, armed twice, and eight concurrent sweepers.
	h := newRevivalHarness(t)
	h.m.armSessionRevival("ws", "s1", errors.New("shim died after connect"))
	h.m.armSessionRevival("ws", "s1", errors.New("shim died after connect"))
	h.advance(bringUpRetryBaseDelay + time.Second)

	const sweepers = 8
	launched := make([]int, sweepers)
	var start sync.WaitGroup
	var done sync.WaitGroup
	start.Add(1)
	done.Add(sweepers)

	// Act
	for i := range launched {
		go func() {
			defer done.Done()
			start.Wait()
			launched[i] = h.m.SweepFailedBringUps()
		}()
	}
	start.Done()
	done.Wait()

	// Assert
	total := 0
	for _, n := range launched {
		total += n
	}
	if total != 1 {
		t.Fatalf("concurrent sweeps launched %d attempts in total, want exactly 1 — two triggers must never produce two shims for one session", total)
	}
}

package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shim"
	"claude-repld/internal/ssm"
)

// ---------------------------------------------------------------------------
// THE STALE-SHIM REFRESH'S TURN-BOUNDARY LEASE (turnboundaryrefresh.go).
//
// A shim that survives a daemon bounce is very often STILL WORKING on the turn
// the user submitted before it. The automatic stale-build refresh used to fire
// at ShimReady regardless, SIGTERMing that turn moments after reattaching to
// it. These pin the deferral: the arm, the boundary that fires it, the prompts
// parked across it, and the two things the redesign was not allowed to break —
// the at-most-once latch and the failure card.
// ---------------------------------------------------------------------------

// logWatch is a RENDEZVOUS with a decision that produces no other observable
// effect: the daemon's own canonical log line. Nothing here polls or sleeps —
// the waiter is woken by the very record it is waiting for.
type logWatch struct {
	want string
	ch   chan string
}

func newLogWatch(want string) *logWatch { return &logWatch{want: want, ch: make(chan string, 8)} }

func (w *logWatch) logf(format string, args ...any) {
	line := fmt.Sprintf(format, args...)
	if strings.Contains(line, w.want) {
		select {
		case w.ch <- line:
		default:
		}
	}
}

// await blocks until a matching line is recorded. The bound is a FAILURE bound:
// it exists so a decision that never happens fails the test instead of hanging
// it.
func (w *logWatch) await(t *testing.T) string {
	t.Helper()
	select {
	case line := <-w.ch:
		return line
	case <-time.After(10 * time.Second):
		t.Fatalf("no log line containing %q was recorded", w.want)
		return ""
	}
}

// newStaleRefreshHarness brings "ws" up against a daemon whose CURRENT bundle
// identity is `current`, with an optional log watcher bound.
func newStaleRefreshHarness(t *testing.T, current string, watch *logWatch) *queueHarness {
	t.Helper()
	var logf func(string, ...any)
	if watch != nil {
		logf = watch.logf
	}
	h := newQueueHarnessWithPusher(t, nil, nil, logf)
	h.m.cfg.ShimBuildSHA = func() string { return current }
	h.spawner.stops = make(chan shim.Stop, 8)
	return h
}

// reattach drives the ShimReady hook a returning shim's handshake ends in —
// the exact production entry point the build comparison is made from.
func (h *queueHarness) reattach(build string, turnInFlight bool, activeTurnIDs []string) {
	h.t.Helper()
	h.m.onConnected("ws", "s1", &corev1.ShimHello{
		BuildSha:      build,
		TurnInFlight:  turnInFlight,
		ActiveTurnIds: activeTurnIDs,
	})
}

// armedSession reports the session an armed turn-boundary lease names.
func (h *queueHarness) armedSession() (string, bool) { return h.m.staleRefreshArmedFor("ws") }

// awaitLeaseCleared blocks until "ws" holds no turn-boundary arm, which is how
// a FIRED lease's aftermath is known to have finished.
//
// THE BUILD LATCH IS NOT THAT SIGNAL. runStaleRefresh sets the latch and
// returns, and only then does runStaleRefreshAtBoundary retake the lock to
// clear the arm — deliberately, since the arm has to span the restart so no
// prompt is admitted into a session being SIGTERMed. A test that waited on the
// latch and then asked whether anything was armed was reading the FIRST lease
// mid-teardown and calling it a second lease.
//
// A rendezvous with the manager's own bookkeeping, in waitForBuildLatch's
// style, rather than a poll of a side effect the clearing happens to produce.
func (h *queueHarness) awaitLeaseCleared() {
	h.t.Helper()
	for {
		if _, armed := h.armedSession(); !armed {
			return
		}
		runtime.Gosched()
	}
}

// A STALE SHIM THAT REATTACHES MID-TURN IS NOT INTERRUPTED. This is the whole
// defect: an automatic stop firing at ShimReady threw away the turn the user
// was waiting on.
func TestAStaleShimReattachingMidTurnArmsTheLeaseInsteadOfBouncing(t *testing.T) {
	// Arrange — a turn is in flight when the returning shim announces a
	// superseded bundle.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)

	// Act.
	h.reattach("sha-old", true, []string{"turn-1"})

	// Assert — armed, and NOTHING was stopped.
	if session, armed := h.armedSession(); !armed || session != "s1" {
		t.Fatalf("armed lease = (%q, %v), want s1 armed — a mid-turn stale reattach must defer its refresh", session, armed)
	}
	if stopped := h.spawner.stoppedSessions(); len(stopped) != 0 {
		t.Fatalf("stopped %v; the shim was interrupted mid-turn, which is exactly what the lease exists to prevent", stopped)
	}
}

// A SETTLED reattach keeps the old behavior: there is no work to protect, so
// the refresh happens now.
func TestAStaleShimReattachingSettledRefreshesImmediately(t *testing.T) {
	// Arrange — no turn in flight.
	h := newStaleRefreshHarness(t, "sha-new", nil)

	// Act.
	h.reattach("sha-old", false, nil)

	// Assert — bounced straight away, and no lease was armed.
	by := waitForStop(t, h.spawner)
	if by.Initiator != "hard_restart" {
		t.Fatalf("stop initiator = %q, want hard_restart", by.Initiator)
	}
	if session, armed := h.armedSession(); armed {
		t.Fatalf("a settled reattach armed a lease for %q; there was no turn to wait for", session)
	}
}

// THE BOUNDARY IS THE TRIGGER. The turn the reattach found running ends, and
// the deferred refresh runs.
func TestTheArmedLeaseFiresTheRefreshWhenTheTurnEnds(t *testing.T) {
	// Arrange.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})

	// Act — the turn resolves.
	h.turn(false)

	// Assert.
	by := waitForStop(t, h.spawner)
	if by.Initiator != "hard_restart" {
		t.Fatalf("stop initiator = %q, want hard_restart", by.Initiator)
	}
}

// AN INTERRUPTED TURN IS STILL A RESOLVED TURN. The lease fires on the turn
// CLOSING for any reason, so a user's stop releases it exactly as a clean end
// does — and no timeout of the lease's own is needed for the case.
func TestTheArmedLeaseFiresTheRefreshWhenTheTurnIsInterrupted(t *testing.T) {
	// Arrange — the turn the stale shim was running has been stopped by the
	// user, so its end arrives carrying the interrupt mark.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})
	d := h.controller()
	h.m.mu.Lock()
	d.stoppedTurn = true
	h.m.mu.Unlock()

	// Act.
	h.turn(false)

	// Assert.
	by := waitForStop(t, h.spawner)
	if by.Initiator != "hard_restart" {
		t.Fatalf("stop initiator = %q, want hard_restart", by.Initiator)
	}
}

// A BOUNDARY IS NOT A SETTLEDNESS VERDICT. One turn ending while the workspace
// is still working must not fire the restart — the lease stays armed for the
// next boundary rather than cutting the work that is still running.
func TestABoundaryFoundUnsettledKeepsTheLeaseArmed(t *testing.T) {
	// Arrange — a boundary arrives, but the SSM still reads the workspace as
	// working.
	watch := newLogWatch("stale-shim turn-boundary lease STAYS ARMED")
	h := newStaleRefreshHarness(t, "sha-new", watch)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		Workspace: "ws", SessionId: "s1",
		State: frontendv1.RenderState_RENDER_STATE_THINKING, TurnActive: true,
	})

	// Act.
	h.turn(false)
	watch.await(t)

	// Assert — still armed, nothing stopped.
	if session, armed := h.armedSession(); !armed || session != "s1" {
		t.Fatalf("armed lease = (%q, %v), want s1 still armed", session, armed)
	}
	if stopped := h.spawner.stoppedSessions(); len(stopped) != 0 {
		t.Fatalf("stopped %v while the workspace was still working", stopped)
	}
}

// PROMPTS TYPED WHILE THE LEASE IS ARMED ARE PARKED, not refused and not
// classified: the shim they would run on is about to be replaced.
func TestAPromptSubmittedWhileTheLeaseIsArmedIsParked(t *testing.T) {
	// Arrange.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})

	// Act.
	if err := h.submit("held-across-the-refresh"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert — one entry, HELD by the refresh, and nothing submitted.
	es := h.entries()
	if len(es) != 1 || es[0].buildRefreshHoldSessionID != "s1" || es[0].classification != VerdictHold {
		t.Fatalf("entries = %+v, want one entry held by session s1's stale-build refresh", es)
	}
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want none: the prompt must not reach a shim that is about to be replaced", got)
	}
}

// AND THE PARKED PROMPT IS DELIVERED, to the RESTARTED shim. That is the
// other half of delayed-never-dropped.
func TestAParkedPromptIsDeliveredToTheRestartedShim(t *testing.T) {
	// Arrange.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})
	if err := h.submit("held-across-the-refresh"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.turn(false)

	// Assert — the prompt reached the shim the restart brought up.
	waitFor(t, "the parked prompt to reach the restarted shim", func() bool {
		replacement := h.newestClient()
		if replacement == nil || replacement == h.client {
			return false
		}
		got := replacement.promptTexts()
		return len(got) == 1 && got[0] == "held-across-the-refresh"
	})
}

// MULTIPLE PARKED PROMPTS KEEP THEIR ORDER. The user typed them in an order and
// a refresh is not permitted to shuffle them.
func TestParkedPromptsKeepTheirOrderAcrossTheRefresh(t *testing.T) {
	// Arrange.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})
	for _, text := range []string{"one", "two", "three"} {
		if err := h.submit(text); err != nil {
			t.Fatalf("submit %q: %v", text, err)
		}
	}

	// Act.
	h.turn(false)

	// Assert — the front one is delivered and the rest wait, un-held, in the
	// order they were typed.
	waitFor(t, "the front parked prompt to reach the restarted shim", func() bool {
		replacement := h.newestClient()
		if replacement == nil || replacement == h.client {
			return false
		}
		got := replacement.promptTexts()
		return len(got) == 1 && got[0] == "one"
	})
	var texts []string
	for _, e := range h.entries() {
		if e.buildRefreshHeld() {
			t.Fatalf("entry %q is still held after the refresh completed", e.text)
		}
		texts = append(texts, e.text)
	}
	if len(texts) != 2 || texts[0] != "two" || texts[1] != "three" {
		t.Fatalf("remaining queue = %v, want [two three] in the order they were typed", texts)
	}
}

// AN EXPLICIT RESTART IS THE USER'S OWN INTENT TO INTERRUPT, and it stays
// immediate even mid-turn. Making it wait for a turn the user has just decided
// to abandon would leave the one control that reaches a wedged session unable
// to reach it.
func TestAnExplicitRestartStaysImmediateMidTurn(t *testing.T) {
	// Arrange — a live turn and an armed lease waiting on it.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})

	// Act — the user commands a restart.
	if err := h.m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession: %v", err)
	}

	// Assert — the shim was stopped by the command itself, with no boundary
	// ever reported.
	if stopped := h.spawner.stoppedSessions(); len(stopped) == 0 || stopped[0] != "s1" {
		t.Fatalf("stopped = %v, want s1 stopped immediately by the user's own restart", stopped)
	}
}

// A REFRESH THAT COULD NOT COMPLETE IS STILL EVIDENCE THAT SOMETHING BROKE.
// The lease path must reach the same close edge the immediate path does.
func TestAFailedTurnBoundaryRefreshStillCards(t *testing.T) {
	// Arrange — a shim that cannot be stopped, so the restart fails.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})
	h.spawner.mu.Lock()
	h.spawner.stopErr = errors.New("operation not permitted")
	h.spawner.mu.Unlock()

	// Act.
	h.turn(false)

	// Assert.
	edge := waitForConnectivityCause(h.applier, staleShimRefreshFailedCause)
	if edge.state != ssm.SessionConnectivityUnavailable {
		t.Fatalf("connectivity edge state = %q, want %q", edge.state, ssm.SessionConnectivityUnavailable)
	}
}

// THE AT-MOST-ONCE LATCH SURVIVES THE REDESIGN. A shim that comes back still
// reporting a mismatch after a lease-path refresh is loud, not bounced again.
func TestTheAtMostOnceLatchHoldsAcrossTheLeasePath(t *testing.T) {
	// Arrange — one refresh, performed through the turn-boundary lease.
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})
	h.turn(false)
	waitForBuildLatch(h.m, "s1", true)
	h.awaitLeaseCleared()
	stopsAfterFirst := len(h.spawner.stoppedSessions())

	// Act — the replacement reports the same stale identity, mid-turn again.
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-2"})

	// Assert — no second lease and no second bounce.
	if session, armed := h.armedSession(); armed {
		t.Fatalf("a second lease was armed for %q against an already-refreshed session", session)
	}
	if stops := len(h.spawner.stoppedSessions()); stops != stopsAfterFirst {
		t.Fatalf("stops = %d, want %d: a refreshed session must not be bounced again", stops, stopsAfterFirst)
	}
}

// A CONTROLLER EXIT TAKES THE BOUNDARY WITH IT. The connection that would have
// reported the turn's end is gone, so an arm left standing would park every
// later prompt on the workspace forever.
func TestAControllerExitDisarmsAnArmedLease(t *testing.T) {
	// Arrange.
	watch := newLogWatch("stale-shim turn-boundary lease DISARMED BY A CONTROLLER EXIT")
	h := newStaleRefreshHarness(t, "sha-new", watch)
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})
	d := h.controller()

	// Act — the controller dies.
	d.cancel()
	watch.await(t)

	// Assert.
	if session, armed := h.armedSession(); armed {
		t.Fatalf("the lease for %q outlived the controller that could have fired it", session)
	}
}

// THE CLASSIFIER NEVER RUNS ON A PARKED PROMPT, and this is the hold that
// proved the four separate skip tests were one too few: the stale-build case
// had none, so an entry the queue created HOLD was re-stamped ERROR by a
// classifier that should never have seen it, and the user's chip said nothing
// had decided their prompt. The skip is one `held()` test now (queue.go), so a
// fifth hold is honored without a fifth place to remember.
func TestAPromptParkedByTheRefreshIsNeverClassified(t *testing.T) {
	// Arrange — a classifier that would answer if it were asked.
	cls := &fakeClassifier{res: ClassifyResult{Classification: VerdictInterject}}
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.m.cfg.Classifier = cls
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})

	// Act.
	if err := h.submit("held-across-the-refresh"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if reqs := cls.requests(); len(reqs) != 0 {
		t.Fatalf("classify requests = %+v, want none: a parked prompt has no turn an interject may reach", reqs)
	}
}

// AND ITS VERDICT STAYS THE QUEUE'S. A classifier that never runs cannot
// overwrite the hold, which is the user-visible half of the same defect.
func TestAPromptParkedByTheRefreshKeepsItsHoldVerdict(t *testing.T) {
	// Arrange.
	cls := &fakeClassifier{res: ClassifyResult{Classification: VerdictInterject}}
	h := newStaleRefreshHarness(t, "sha-new", nil)
	h.m.cfg.Classifier = cls
	h.turn(true)
	h.reattach("sha-old", true, []string{"turn-1"})

	// Act.
	if err := h.submit("held-across-the-refresh"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	es := h.entries()
	if len(es) != 1 || es[0].classification != VerdictHold {
		t.Fatalf("entries = %+v, want the queue's own HOLD verdict left standing", es)
	}
}

package sessionwatcher

import (
	"context"
	"errors"
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// TestAwaitFreeReturnsAtOnceWhenNothingIsInFlight covers the already-free
// case: a lease holder that asks for freeness on an idle session waits for
// nothing.
func TestAwaitFreeReturnsAtOnceWhenNothingIsInFlight(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})

	// Act.
	err := h.w.AwaitFree(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("AwaitFree on an idle session = %v, want nil", err)
	}
}

// TestAwaitFreeIsReleasedByTheTurnEnd covers the turn half of freeness: the
// terminal that closes the turn is what answers the standing wait, with no
// poll and no timer between them. The waiter is filed synchronously, so the
// terminal cannot be routed before the wait is standing.
func TestAwaitFreeIsReleasedByTheTurnEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	ch, standing, err := h.w.registerFreeWaiter()
	if !standing {
		t.Fatalf("a turn is in flight but the wait did not stand (err %v)", err)
	}

	// Act.
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	if err := <-ch; err != nil {
		t.Fatalf("the freeness waiter = %v, want nil once the turn ended", err)
	}
}

// TestAwaitFreeIsReleasedWhenTheLastDetachedItemEnds covers the live-work half
// of freeness, raised with no turn ever in flight.
func TestAwaitFreeIsReleasedWhenTheLastDetachedItemEnds(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("", createdWork("act-1", monitorWork()))})
	h.quiet()
	ch, standing, err := h.w.registerFreeWaiter()
	if !standing {
		t.Fatalf("a monitor is live but the wait did not stand (err %v)", err)
	}

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(monitorActivity("act-1", true)))))

	// Assert.
	if err := <-ch; err != nil {
		t.Fatalf("the freeness waiter = %v, want nil once the last detached item ended", err)
	}
}

// TestAwaitFreeStaysStandingWhileDetachedWorkIsLive covers the half that is
// NOT freeness: a turn that ended over a live detached item leaves the wait
// standing.
func TestAwaitFreeStaysStandingWhileDetachedWorkIsLive(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1", createdWork("w-1", bashWork()))})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	ch, standing, err := h.w.registerFreeWaiter()
	if !standing {
		t.Fatalf("a turn is in flight but the wait did not stand (err %v)", err)
	}

	// Act.
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	select {
	case got := <-ch:
		t.Fatalf("the freeness waiter was released with %v while a detached shell is still live", got)
	default:
	}
}

// TestAwaitFreeAnswersItsContext covers the abandoned wait: a caller whose
// context ended stops waiting.
func TestAwaitFreeAnswersItsContext(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := h.w.AwaitFree(ctx)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitFree with an ended context = %v, want context.Canceled", err)
	}
}

// TestAwaitFreeDropsAnAbandonedWaiter covers the bookkeeping: an abandoned
// wait must not accumulate on a long-lived watcher.
func TestAwaitFreeDropsAnAbandonedWaiter(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_ = h.w.AwaitFree(ctx)

	// Assert.
	h.w.mu.Lock()
	defer h.w.mu.Unlock()
	if len(h.w.freeWaiters) != 0 {
		t.Fatalf("freeWaiters = %d after an abandoned wait, want none left behind", len(h.w.freeWaiters))
	}
}

// TestAwaitFreeAnswersACloseLoudly covers the torn-down watcher: a closed
// watcher will never become free, so the wait is ended rather than left
// hanging on a state that can no longer change.
func TestAwaitFreeAnswersACloseLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	ch, standing, err := h.w.registerFreeWaiter()
	if !standing {
		t.Fatalf("a turn is in flight but the wait did not stand (err %v)", err)
	}

	// Act.
	if err := h.w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if err := <-ch; !errors.Is(err, ErrWatcherClosed) {
		t.Fatalf("the freeness waiter after Close = %v, want ErrWatcherClosed", err)
	}
}

// TestAwaitFreeOnAClosedWatcherRefuses covers the wait filed after the close.
func TestAwaitFreeOnAClosedWatcherRefuses(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	if err := h.w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act.
	err := h.w.AwaitFree(context.Background())

	// Assert.
	if !errors.Is(err, ErrWatcherClosed) {
		t.Fatalf("AwaitFree on a closed watcher = %v, want ErrWatcherClosed", err)
	}
}

func TestAwaitSessionFactsAnswersAtOnceForAStartedSession(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})

	// Act.
	err := h.w.AwaitSessionFacts(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("AwaitSessionFacts on a started session = %v, want nil", err)
	}
}

func TestAwaitSessionFactsIsReleasedByTheReannouncementWithItsTurn(t *testing.T) {
	// Arrange: an adopting daemon's pure attach.
	h := newHarnessAttachingPurely(t)
	answered := make(chan error, 1)
	go func() { answered <- h.w.AwaitSessionFacts(context.Background()) }()

	// Act.
	h.sendSessionStarted(t, sessionStarted("turn-1"))

	// Assert: the waiter reads the adopted turn once it is released.
	if err := <-answered; err != nil {
		t.Fatalf("AwaitSessionFacts = %v, want nil once the facts arrived", err)
	}
	if got := h.w.TurnInFlight(); got == nil || *got != "turn-1" {
		t.Fatalf("TurnInFlight after the facts = %v, want turn-1", got)
	}
}

func TestSessionFactsInAnswersTrueForAStartedSession(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})

	// Act / Assert.
	if !h.w.SessionFactsIn() {
		t.Fatal("SessionFactsIn on a started session = false, want true")
	}
}

func TestSessionFactsInAnswersFalseForAPureAttachBeforeItsFacts(t *testing.T) {
	// Arrange.
	h := startHarness(t, Session{}, nil)
	h.session = h.client.nextSessionOpen(t)

	// Act / Assert.
	if h.w.SessionFactsIn() {
		t.Fatal("SessionFactsIn on a pure attach with no facts = true, want false")
	}
}

func TestSessionFactsInAnswersTrueOnceTheReannouncementLands(t *testing.T) {
	// Arrange.
	h := newHarnessAttachingPurely(t)
	answered := make(chan error, 1)
	go func() { answered <- h.w.AwaitSessionFacts(context.Background()) }()

	// Act.
	h.sendSessionStarted(t, sessionStarted("turn-1"))
	if err := <-answered; err != nil {
		t.Fatalf("AwaitSessionFacts = %v, want nil", err)
	}

	// Assert.
	if !h.w.SessionFactsIn() {
		t.Fatal("SessionFactsIn after the re-announcement = false, want true")
	}
}

func TestAwaitSessionFactsAnswersItsContext(t *testing.T) {
	// Arrange.
	h := newHarnessAttachingPurely(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := h.w.AwaitSessionFacts(ctx)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitSessionFacts on an ended context = %v, want context.Canceled", err)
	}
}

func TestAwaitSessionFactsAnswersACloseLoudly(t *testing.T) {
	// Arrange.
	h := newHarnessAttachingPurely(t)
	answered := make(chan error, 1)
	go func() { answered <- h.w.AwaitSessionFacts(context.Background()) }()

	// Act.
	if err := h.w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if err := <-answered; !errors.Is(err, ErrWatcherClosed) {
		t.Fatalf("AwaitSessionFacts after Close = %v, want ErrWatcherClosed", err)
	}
}

// TestAwaitTurnEndIsReleasedByTheTerminal covers the ordinary wait: the
// terminal that closes the turn reports HOW it closed.
func TestAwaitTurnEndIsReleasedByTheTerminal(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	ch, standing, answer := h.w.registerTurnWaiter(ids.TurnID("turn-1"))
	if !standing {
		t.Fatalf("the turn is in flight but the wait did not stand (%+v)", answer)
	}

	// Act.
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	got := <-ch
	if got.err != nil {
		t.Fatalf("the turn waiter = error %v, want the close", got.err)
	}
	if got.how != wsm.CloseCompleted {
		t.Fatalf("the turn waiter close = %v, want CloseCompleted", got.how)
	}
}

// TestAwaitTurnEndAnswersATurnThatAlreadyEnded covers the race the caller
// cannot avoid: submitting a turn and waiting on it are two steps, and the
// turn can end between them.
func TestAwaitTurnEndAnswersATurnThatAlreadyEnded(t *testing.T) {
	// Arrange: the turn ends BEFORE anybody waits on it.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Act.
	how, err := h.w.AwaitTurnEnd(context.Background(), ids.TurnID("turn-1"))

	// Assert.
	if err != nil {
		t.Fatalf("AwaitTurnEnd on an already-ended turn = %v, want the remembered close", err)
	}
	if how != wsm.CloseCompleted {
		t.Fatalf("AwaitTurnEnd close = %v, want CloseCompleted", how)
	}
}

// TestAwaitTurnEndAnswersItsContext covers the abandoned wait on a turn.
func TestAwaitTurnEndAnswersItsContext(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := h.w.AwaitTurnEnd(ctx, ids.TurnID("turn-1"))

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitTurnEnd with an ended context = %v, want context.Canceled", err)
	}
}

// TestAwaitTurnEndAnswersACloseLoudly covers the torn-down watcher on a turn
// wait.
func TestAwaitTurnEndAnswersACloseLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1")})
	ch, standing, answer := h.w.registerTurnWaiter(ids.TurnID("turn-1"))
	if !standing {
		t.Fatalf("the turn is in flight but the wait did not stand (%+v)", answer)
	}

	// Act.
	if err := h.w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if got := <-ch; !errors.Is(got.err, ErrWatcherClosed) {
		t.Fatalf("the turn waiter after Close = %v, want ErrWatcherClosed", got.err)
	}
}

// TestClosedTurnMemoryIsBounded covers the eviction: the memory of ended turns
// is a fixed window, not an unbounded ledger on a long-lived session.
func TestClosedTurnMemoryIsBounded(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})

	// Act.
	h.w.mu.Lock()
	for i := 0; i < closedTurnMemory+5; i++ {
		h.w.rememberClosedTurnLocked(ids.TurnID("turn-"+itoa(i)), wsm.CloseCompleted)
	}
	held := len(h.w.closedTurns)
	h.w.mu.Unlock()

	// Assert.
	if held != closedTurnMemory {
		t.Fatalf("closedTurns holds %d turns, want the bounded %d", held, closedTurnMemory)
	}
}

// TestAwaitFreeIsReleasedByADetachedSubagentSettling covers freeness over a
// DETACHED RUN: the run's settle arrives as its spawn unit's terminal arm and
// nowhere else, so that frame has to be what releases a standing waiter.
func TestAwaitFreeIsReleasedByADetachedSubagentSettling(t *testing.T) {
	// Arrange.
	h := detachedSubagentHarness(t)
	ch, standing, err := h.w.registerFreeWaiter()
	if !standing {
		t.Fatalf("a detached subagent is live but the wait did not stand (err %v)", err)
	}

	// Act.
	h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(settledSubagentActivity("spawn-1", false)))))

	// Assert.
	if err := <-ch; err != nil {
		t.Fatalf("the freeness waiter = %v, want nil once the detached run settled", err)
	}
}

// TestTheFreenessEdgeReachesTheLifecycleSink covers OnFree, the edge the
// bounce registry is driven by: told once, off the lock, the moment the last
// piece of work in flight ends — a turn, or the last detached item.
func TestTheFreenessEdgeReachesTheLifecycleSink(t *testing.T) {
	tests := []struct {
		name    string
		session Session
		edge    func(h *harness)
	}{
		{
			name:    "the turn ends",
			session: Session{Started: sessionStarted("turn-1")},
			edge: func(h *harness) {
				h.route(h.main, entryFrame(frameSuccess("main-1", completed())))
			},
		},
		{
			name:    "the last monitor ends",
			session: Session{Started: sessionStarted("", createdWork("act-1", monitorWork()))},
			edge: func(h *harness) {
				h.route(h.main, entryFrame(frameUpdate("main-1", activityUpdate(monitorActivity("act-1", true)))))
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, tc.session)
			h.w.SetMainAgent(agentID("main-1"))
			h.quiet()

			// Act.
			tc.edge(h)
			h.w.dispatching.Wait()

			// Assert.
			select {
			case ws := <-h.rec.frees:
				if ws != h.w.ws {
					t.Fatalf("OnFree named %q, want %q", ws, h.w.ws)
				}
			default:
				t.Fatalf("the workspace fell free and OnFree was never told")
			}
		})
	}
}

// TestATurnOpenedOnAnIdleSessionRaisesTheFreenessEdgeAtItsEnd pins the edge
// for the commonest turn of all: one the queue opens on a session that came up
// idle, with no detached work changing under it. Opening it is what makes the
// session busy, so its end is a transition the sink is told of; without that,
// a stand-down a collapse deferred under the turn waited for an edge that never
// came (2026-10-25).
func TestATurnOpenedOnAnIdleSessionRaisesTheFreenessEdgeAtItsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	h.w.OnTurnOpened(h.w.ws, &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-1"},
		Agent: agentID("main-1"),
	}, nil)
	h.w.dispatching.Wait()
	select {
	case ws := <-h.rec.frees:
		t.Fatalf("OnFree(%q) was told while the turn runs", ws)
	default:
	}

	// Act.
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))
	h.w.dispatching.Wait()

	// Assert.
	select {
	case ws := <-h.rec.frees:
		if ws != h.w.ws {
			t.Fatalf("OnFree named %q, want %q", ws, h.w.ws)
		}
	default:
		t.Fatal("the turn ended and OnFree was never told")
	}
}

// TestNoFreenessEdgeWhileDetachedWorkIsLive covers the half that is not
// freeness: a turn ending over a live detached item tells the sink nothing.
func TestNoFreenessEdgeWhileDetachedWorkIsLive(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("turn-1", createdWork("w-1", bashWork()))})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))
	h.w.dispatching.Wait()

	// Assert.
	select {
	case ws := <-h.rec.frees:
		t.Fatalf("OnFree(%q) was told while a detached shell is still live", ws)
	default:
	}
}

// TestNoFreenessEdgeForAWatcherThatWasNeverBusy covers the edge's shape: it is
// a TRANSITION, so a session that opens idle raises none.
func TestNoFreenessEdgeForAWatcherThatWasNeverBusy(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})

	// Act.
	h.quiet()
	h.w.dispatching.Wait()

	// Assert.
	select {
	case ws := <-h.rec.frees:
		t.Fatalf("OnFree(%q) was told for a session that was never busy", ws)
	default:
	}
}

// UNKNOWN IS NEVER FREE. A pure attach knows nothing of the turn its adopted
// shim may be running until the shim re-announces its facts, and a stale-build
// relaunch judged at the opening diagnostics once read that as free and
// force-killed a running conversation (2026-09-30).
func TestAPureAttachIsNotFreeBeforeItsFacts(t *testing.T) {
	// Arrange.
	h := startHarness(t, Session{}, nil)
	h.session = h.client.nextSessionOpen(t)

	// Act.
	free := h.w.Free()

	// Assert.
	if free {
		t.Fatal("a pure attach with no facts answered free")
	}
}

func TestAPureAttachFallsFreeWhenItsFactsNameNothingInFlight(t *testing.T) {
	// Arrange.
	h := startHarness(t, Session{}, nil)
	h.session = h.client.nextSessionOpen(t)

	// Act.
	h.reannounce(t, sessionStarted(""))

	// Assert.
	h.awaitFree(t)
	if !h.w.Free() {
		t.Fatal("the attach's facts named nothing in flight, and it is still not free")
	}
}

func TestAPureAttachWhoseFactsNameATurnStaysBusy(t *testing.T) {
	// Arrange.
	h := startHarness(t, Session{}, nil)
	h.session = h.client.nextSessionOpen(t)

	// Act.
	h.reannounce(t, sessionStarted("turn-1"))

	// Assert.
	if h.w.Free() {
		t.Fatal("the attach's facts named a turn in flight, and it answered free")
	}
	h.noFree(t)
}

// freeWorkEnded answers the one free edge's workEnded, failing when none was told.
func (h *harness) freeWorkEnded(t *testing.T) bool {
	t.Helper()
	h.w.dispatching.Wait()
	select {
	case ended := <-h.rec.freeWork:
		return ended
	default:
		t.Fatal("OnFree was never told")
		return false
	}
}

func TestAPureAttachsOwnFreeEdgeSaysNoWorkEnded(t *testing.T) {
	// Arrange.
	h := startHarness(t, Session{}, nil)
	h.session = h.client.nextSessionOpen(t)

	// Act.
	h.reannounce(t, sessionStarted(""))
	h.awaitFree(t)

	// Assert.
	if h.freeWorkEnded(t) {
		t.Fatal("the attach's own edge said work ended")
	}
}

func TestATurnsEndSaysWorkEnded(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()
	h.w.OnTurnOpened(h.w.ws, &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-1"},
		Agent: agentID("main-1"),
	}, nil)

	// Act.
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	if !h.freeWorkEnded(t) {
		t.Fatal("a turn's end said no work ended")
	}
}

func TestAnAttachedTurnsEndSaysWorkEnded(t *testing.T) {
	// Arrange.
	h := startHarness(t, Session{}, nil)
	h.session = h.client.nextSessionOpen(t)
	h.reannounce(t, sessionStarted("turn-1"))
	h.noFree(t)
	open := h.client.nextAgentOpenFor(t, "")
	h.main, h.mainReq = open.stream, open.req
	h.w.SetMainAgent(agentID("main-1"))
	h.quiet()

	// Act.
	h.route(h.main, entryFrame(frameSuccess("main-1", completed())))

	// Assert.
	if !h.freeWorkEnded(t) {
		t.Fatal("the end of a turn the attach found running said no work ended")
	}
}

// mainWatched reads whether the main watch has served its opening page.
func (h *harness) mainWatched() bool {
	h.w.mu.Lock()
	defer h.w.mu.Unlock()
	return h.w.mainWatched
}

func TestTheMainWatchIsNotServingBeforeItsOpeningPage(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})

	// Act: an entry, not the page, rides the main watch.
	h.quiet()

	// Assert.
	if h.mainWatched() {
		t.Fatal("the main watch reads serving before its opening page")
	}
}

func TestAwaitMainWatchingAnswersOnceTheMainWatchServesItsOpeningPage(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})

	// Act.
	h.route(h.main, pageFrame())
	err := h.w.AwaitMainWatching(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("AwaitMainWatching after the opening page = %v, want nil", err)
	}
}

func TestAwaitMainWatchingIsReleasedByTheOpeningPage(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	answered := make(chan error, 1)
	go func() { answered <- h.w.AwaitMainWatching(context.Background()) }()

	// Act.
	h.route(h.main, pageFrame())

	// Assert.
	if err := <-answered; err != nil {
		t.Fatalf("AwaitMainWatching = %v, want nil once the opening page arrived", err)
	}
}

func TestASubagentsOpeningPageDoesNotReleaseTheMainWatchLatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", subagentWork("sub-1")))))
	sub := h.client.nextAgentOpen(t)

	// Act.
	h.route(sub.stream, pageFrame())

	// Assert.
	if h.mainWatched() {
		t.Fatal("a subagent's opening page released the main watch's latch")
	}
}

func TestAwaitMainWatchingAnswersItsContext(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := h.w.AwaitMainWatching(ctx)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitMainWatching on an ended context = %v, want context.Canceled", err)
	}
}

func TestAwaitMainWatchingAnswersACloseLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	answered := make(chan error, 1)
	go func() { answered <- h.w.AwaitMainWatching(context.Background()) }()

	// Act.
	if err := h.w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if err := <-answered; !errors.Is(err, ErrWatcherClosed) {
		t.Fatalf("AwaitMainWatching after Close = %v, want ErrWatcherClosed", err)
	}
}

func TestAwaitMainWatchingOnAClosedWatcherAnswersAtOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	if err := h.w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act.
	err := h.w.AwaitMainWatching(context.Background())

	// Assert.
	if !errors.Is(err, ErrWatcherClosed) {
		t.Fatalf("AwaitMainWatching on a closed watcher = %v, want ErrWatcherClosed", err)
	}
}

func TestTheMainWatchServingIsRecordedAtInfoWithHowLongItTook(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})

	// Act.
	h.route(h.main, pageFrame())

	// Assert.
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.sessionwatcher.main_watching"})
	if !ok {
		t.Fatal("no info record of the main watch serving")
	}
	if _, has := record.Context["since_open_ms"]; !has {
		t.Fatalf("main_watching context = %v, want since_open_ms", record.Context)
	}
}

func TestATurnThatWaitedForTheMainWatchIsRecordedWithHowLong(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	answered := make(chan error, 1)
	go func() { answered <- h.w.waitMainWatching(context.Background()) }()

	// Act.
	h.route(h.main, pageFrame())
	if err := <-answered; err != nil {
		t.Fatalf("AwaitMainWatching = %v", err)
	}

	// Assert.
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.sessionwatcher.main_watching_waited"})
	if !ok {
		t.Fatal("no info record of a turn waiting for the main watch")
	}
	if _, has := record.Context["waited_ms"]; !has {
		t.Fatalf("main_watching_waited context = %v, want waited_ms", record.Context)
	}
}

func TestATurnThatNeverWaitedRecordsNoWait(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.route(h.main, pageFrame())

	// Act.
	if err := h.w.AwaitMainWatching(context.Background()); err != nil {
		t.Fatalf("AwaitMainWatching = %v", err)
	}

	// Assert.
	if h.hasRecord("info", "daemon.sessionwatcher.main_watching_waited") {
		t.Fatal("a turn that found the watch standing recorded a wait")
	}
}

// TestAwaitLatch covers the one wait both of the watcher's latches stand on.
func TestAwaitLatch(t *testing.T) {
	tests := []struct {
		name        string
		latchClosed bool
		closeFirst  bool
		cancelFirst bool
		want        error
	}{
		{name: "a closed latch answers at once", latchClosed: true},
		{name: "a closed watcher with its latch open answers the close", closeFirst: true, want: ErrWatcherClosed},
		{name: "a closed watcher whose latch had closed answers the latch", latchClosed: true, closeFirst: true},
		{name: "an ended context answers its own error", cancelFirst: true, want: context.Canceled},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, Session{Started: sessionStarted("")})
			latch := make(chan struct{})
			if tt.latchClosed {
				close(latch)
			}
			if tt.closeFirst {
				if err := h.w.Close(); err != nil {
					t.Fatalf("Close: %v", err)
				}
			}
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if tt.cancelFirst {
				cancel()
			}

			// Act.
			err := h.w.awaitLatch(ctx, latch)

			// Assert.
			if !errors.Is(err, tt.want) || (tt.want == nil && err != nil) {
				t.Fatalf("awaitLatch = %v, want %v", err, tt.want)
			}
		})
	}
}

// TestTheWatchersLatchesShareOneWait fails a latch wait hand-rolled beside
// awaitLatch: a select on the watcher's own close belongs in it alone.
func TestTheWatchersLatchesShareOneWait(t *testing.T) {
	// Arrange.
	source, err := os.ReadFile("await.go")
	if err != nil {
		t.Fatalf("read await.go: %v", err)
	}

	// Act.
	selects := strings.Count(string(source), "case <-w.ctx.Done():")

	// Assert.
	if selects != 1 {
		t.Fatalf("await.go selects on the watcher's close %d times, want once, in awaitLatch", selects)
	}
}

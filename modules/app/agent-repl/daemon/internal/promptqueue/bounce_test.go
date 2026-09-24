package promptqueue

import (
	"context"
	"errors"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// gate is a bounce action a test drives: it reports that it STARTED, then
// holds until the test releases it with the outcome. It is how the tests
// observe the draining window deterministically — no sleep, only channels.
type gate struct {
	started chan struct{}
	release chan error
	runs    atomic.Int32
	done    chan error
}

func newGate() *gate {
	return &gate{started: make(chan struct{}, 8), release: make(chan error, 1), done: make(chan error, 8)}
}

func (g *gate) request(reason string, force bool) bounce.Request {
	return bounce.Request{
		Reason: reason,
		Force:  force,
		Run: func(context.Context, ids.WorkspaceID) error {
			g.runs.Add(1)
			g.started <- struct{}{}
			return <-g.release
		},
		Done: func(err error) { g.done <- err },
	}
}

// bounceStartBound is how long a test waits for a bounce the queue decided to
// start. The start is one goroutine launch away from the decision, so the
// bound is a failure ceiling, never a delay a passing test pays.
const bounceStartBound = 2 * time.Second

// awaitStart blocks until the gate's action has started, failing the test if
// the bounce was never started.
func (g *gate) awaitStart(t *testing.T) {
	t.Helper()
	select {
	case <-g.started:
	case <-time.After(bounceStartBound):
		t.Fatalf("the bounce was not started")
	}
}

// finish releases the action with an outcome and joins the queue's bounce
// goroutine, so every assertion after it sees the bounce's finish.
func (g *gate) finish(h *harness, err error) {
	g.release <- err
	h.q.bouncing.Wait()
}

func monitors(n int) sessionwatcher.LiveWorkSet {
	var live sessionwatcher.LiveWorkSet
	for i := 0; i < n; i++ {
		live.Monitors = append(live.Monitors, &conversationv1.DetachedWorkId{Value: "monitor"})
	}
	return live
}

// recordWith reports whether the test logger holds a record at the level and
// operation whose message is exactly msg.
func recordWith(records []dlog.Record, level, operation, msg string) bool {
	for _, r := range records {
		if r.Level == level && r.Operation == operation && r.Message == msg {
			return true
		}
	}
	return false
}

func TestRequestBounceDecidesByWhatIsInFlight(t *testing.T) {
	tests := []struct {
		name        string
		turn        bool
		live        sessionwatcher.LiveWorkSet
		force       bool
		wantNow     bool
		wantForced  bool
		wantTurn    bool
		wantDetach  int
		wantMessage string
	}{
		{
			name:        "a free workspace is bounced now",
			wantNow:     true,
			wantMessage: "the workspace is free; bouncing it now",
		},
		{
			name:        "a turn in flight registers the bounce",
			turn:        true,
			wantTurn:    true,
			wantMessage: "the workspace has work in flight; registered the bounce for when it ends",
		},
		{
			name:        "a live monitor registers the bounce",
			live:        monitors(1),
			wantDetach:  1,
			wantMessage: "the workspace has work in flight; registered the bounce for when it ends",
		},
		{
			name:        "a background shell registers the bounce",
			live:        sessionwatcher.LiveWorkSet{Shells: []*conversationv1.DetachedWorkId{{Value: "bash-1"}}},
			wantDetach:  1,
			wantMessage: "the workspace has work in flight; registered the bounce for when it ends",
		},
		{
			name:        "a background subagent registers the bounce",
			live:        sessionwatcher.LiveWorkSet{Agents: []*conversationv1.AgentId{{Value: "sub-1"}}},
			wantDetach:  1,
			wantMessage: "the workspace has work in flight; registered the bounce for when it ends",
		},
		{
			name:        "a forced bounce goes now over a turn and its monitors",
			turn:        true,
			live:        monitors(2),
			force:       true,
			wantNow:     true,
			wantForced:  true,
			wantTurn:    true,
			wantDetach:  2,
			wantMessage: "a FORCED bounce: bouncing now over the work in flight, which ends with it",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tc.turn {
				running(t, h, "running-turn", "the running work")
			}
			h.watcher.detached(tc.live)
			g := newGate()

			// Act
			got, err := h.q.RequestBounce(context.Background(), theWorkspace, g.request("build_stale", tc.force))

			// Assert
			if err != nil {
				t.Fatalf("RequestBounce: %v", err)
			}
			if got.Now != tc.wantNow || got.Forced != tc.wantForced || got.TurnInFlight != tc.wantTurn || got.DetachedWork != tc.wantDetach {
				t.Fatalf("decision = %+v, want now=%v forced=%v turn=%v detached=%d",
					got, tc.wantNow, tc.wantForced, tc.wantTurn, tc.wantDetach)
			}
			if tc.wantNow {
				g.awaitStart(t)
				g.finish(h, nil)
			} else if g.runs.Load() != 0 {
				t.Fatalf("a registered bounce ran %d times before its work ended", g.runs.Load())
			}
			if !recordWith(h.log.Records(), "info", opBounce, tc.wantMessage) {
				t.Fatalf("records = %+v, want %q", h.log.Records(), tc.wantMessage)
			}
		})
	}
}

func TestARegisteredBounceIsTakenWhenItsWorkEndsInEitherOrder(t *testing.T) {
	tests := []struct {
		name  string
		order []string
	}{
		{name: "the turn ends, then the detached work", order: []string{"turn", "detached"}},
		{name: "the detached work ends, then the turn", order: []string{"detached", "turn"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.watcher.detached(monitors(1))
			g := newGate()
			if _, err := h.q.RequestBounce(context.Background(), theWorkspace, g.request("build_stale", false)); err != nil {
				t.Fatalf("RequestBounce: %v", err)
			}

			// Act
			for i, edge := range tc.order {
				switch edge {
				case "turn":
					h.watcher.idle()
					h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
				case "detached":
					h.watcher.detached(sessionwatcher.LiveWorkSet{})
					h.q.OnFree(theWorkspace)
				}
				// Assert (after the first edge): still waiting.
				if i == 0 && g.runs.Load() != 0 {
					t.Fatalf("the bounce ran after %s ended while the other work was still in flight", edge)
				}
			}

			// Assert
			g.awaitStart(t)
			g.finish(h, nil)
			if err := <-g.done; err != nil {
				t.Fatalf("done = %v, want the bounce to finish cleanly", err)
			}
			if h.q.isDraining(theWorkspace) {
				t.Fatalf("the workspace is still draining after its bounce finished")
			}
		})
	}
}

func TestAQueuedPromptDoesNotBlockABounceAndGoesToTheNewShim(t *testing.T) {
	// Arrange: a prompt waits behind the running turn; the turn ends into a
	// registered bounce.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "queued", classifier.Verdict{Interject: false, Reason: "independent"})
	g := newGate()
	if _, err := h.q.RequestBounce(context.Background(), theWorkspace, g.request("build_stale", false)); err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert: the bounce started with the prompt still queued, not started on
	// the shim being stood down.
	g.awaitStart(t)
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v while the bounce ran, want nothing dispatched", started)
	}
	g.finish(h, nil)
	if started := h.sender.started(); len(started) != 1 || started[0] != "queued" {
		t.Fatalf("started = %v after the bounce, want the queued prompt delivered to the new shim", started)
	}
}

func TestADrainingWorkspaceDispatchesNothing(t *testing.T) {
	tests := []struct {
		name string
		act  func(t *testing.T, h *harness)
		want func(t *testing.T, h *harness)
	}{
		{
			name: "a submission is held for the new shim",
			act: func(t *testing.T, h *harness) {
				disposition, err := h.q.Submit(context.Background(), submission("during", "sent while draining"))
				if err != nil {
					t.Fatalf("Submit: %v", err)
				}
				if disposition.Delivered {
					t.Fatalf("disposition = %+v, want held", disposition)
				}
			},
			want: func(t *testing.T, h *harness) {
				held, err := h.db.HeldPrompts(context.Background(), theWorkspace)
				if err != nil {
					t.Fatal(err)
				}
				for _, prompt := range held {
					if prompt.Turn == "during" {
						if prompt.Hold == nil || *prompt.Hold != wsm.HoldBuildRefresh {
							t.Fatalf("hold = %+v, want build_refresh", prompt)
						}
						return
					}
				}
				t.Fatalf("held = %+v, want the submission held", held)
			},
		},
		{
			name: "a release is refused",
			act: func(t *testing.T, h *harness) {
				if err := h.q.Release(context.Background(), theWorkspace, "queued"); !errors.Is(err, ErrReleaseRefused) {
					t.Fatalf("Release = %v, want ErrReleaseRefused", err)
				}
			},
			want: func(t *testing.T, h *harness) {},
		},
		{
			name: "a lease change delivers nothing",
			act: func(t *testing.T, h *harness) {
				h.q.OnLeaseChanged(theWorkspace)
			},
			want: func(t *testing.T, h *harness) {},
		},
		{
			name: "a session act waits behind the bounce",
			act: func(t *testing.T, h *harness) {
				if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"}); err != nil {
					t.Fatalf("SubmitSessionAct: %v", err)
				}
			},
			want: func(t *testing.T, h *harness) {
				if got := h.sender.modelsSet(); len(got) != 0 {
					t.Fatalf("models = %v while draining, want the act queued", got)
				}
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a queued prompt, and a bounce running (the workspace
			// drained) that the test holds open.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			heldPrompt(t, h, "queued", classifier.Verdict{Interject: false, Reason: "independent"})
			h.watcher.idle()
			g := newGate()
			if _, err := h.q.RequestBounce(context.Background(), theWorkspace, g.request("build_stale", false)); err != nil {
				t.Fatalf("RequestBounce: %v", err)
			}
			g.awaitStart(t)

			// Act
			tc.act(t, h)

			// Assert
			if started := h.sender.started(); len(started) != 0 {
				t.Fatalf("started = %v while draining, want nothing dispatched", started)
			}
			tc.want(t, h)
			g.finish(h, nil)
		})
	}
}

func TestTheDrainClosesTheRaceBetweenFreeAndBounce(t *testing.T) {
	// Arrange: the one interleaving the design makes impossible — a turn ends
	// into a registered bounce while a submission is in flight. The
	// submission's delivery blocks INSIDE StartTurn only if it was dispatched;
	// the test holds the bounce open and proves the submission never reached
	// the shim, whichever of the two took the delivery lock first.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	g := newGate()
	if _, err := h.q.RequestBounce(context.Background(), theWorkspace, g.request("build_stale", false)); err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	h.watcher.idle()
	ended := make(chan struct{})
	go func() {
		h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
		close(ended)
	}()
	<-ended
	g.awaitStart(t)

	// Act: the submission arrives while the bounce runs.
	disposition, err := h.q.Submit(context.Background(), submission("racer", "sent at the turn's end"))

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if disposition.Delivered || len(h.sender.started()) != 0 {
		t.Fatalf("the submission reached the shim being bounced: disposition %+v, started %v", disposition, h.sender.started())
	}
	g.finish(h, nil)
	if started := h.sender.started(); len(started) != 1 || started[0] != "racer" {
		t.Fatalf("started = %v, want the racer delivered to the new shim", started)
	}
}

func TestAFailedBounceResumesDispatchAndSaysSo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "queued", classifier.Verdict{Interject: false, Reason: "independent"})
	h.watcher.idle()
	g := newGate()
	if _, err := h.q.RequestBounce(context.Background(), theWorkspace, g.request("build_stale", false)); err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	g.awaitStart(t)

	// Act
	g.finish(h, errors.New("the prelaunch failed"))

	// Assert
	if err := <-g.done; err == nil {
		t.Fatalf("done = nil, want the bounce's failure")
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "queued" {
		t.Fatalf("started = %v, want the held prompt delivered to the shim that still serves", started)
	}
	if !recordWith(h.log.Records(), "error", opBounce, "the bounce failed; dispatch resumes on what serves the workspace") {
		t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
	}
}

func TestAKeepDrainingBounceLeavesTheWorkspaceDrained(t *testing.T) {
	// Arrange
	h := newHarness(t)
	g := newGate()
	req := g.request("handover_transfer", false)
	req.KeepDraining = true
	if _, err := h.q.RequestBounce(context.Background(), theWorkspace, req); err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	g.awaitStart(t)

	// Act
	g.finish(h, nil)
	disposition, err := h.q.Submit(context.Background(), submission("after", "sent after the transfer"))

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if !h.q.isDraining(theWorkspace) || disposition.Delivered {
		t.Fatalf("draining=%v disposition=%+v, want the workspace to stay drained and hold the prompt", h.q.isDraining(theWorkspace), disposition)
	}
}

func TestASecondRequestJoinsThePendingBounce(t *testing.T) {
	tests := []struct {
		name        string
		secondForce bool
		wantNow     bool
	}{
		{name: "an unforced second request joins the registered bounce", wantNow: false},
		{name: "a forced second request takes the registered bounce now", secondForce: true, wantNow: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			first, second := newGate(), newGate()
			if _, err := h.q.RequestBounce(context.Background(), theWorkspace, first.request("build_stale", false)); err != nil {
				t.Fatalf("RequestBounce: %v", err)
			}

			// Act
			got, err := h.q.RequestBounce(context.Background(), theWorkspace, second.request("build_stale", tc.secondForce))

			// Assert
			if err != nil {
				t.Fatalf("RequestBounce: %v", err)
			}
			if !got.AlreadyPending || got.Now != tc.wantNow {
				t.Fatalf("decision = %+v, want already pending, now=%v", got, tc.wantNow)
			}
			if tc.wantNow {
				// The NEWEST action runs, and both requesters are told.
				second.awaitStart(t)
				second.finish(h, nil)
				if first.runs.Load() != 0 {
					t.Fatalf("the superseded action ran")
				}
				if err := <-first.done; err != nil {
					t.Fatalf("first done = %v", err)
				}
			}
		})
	}
}

func TestARequestWhileDrainingJoinsTheRunningBounce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first, second := newGate(), newGate()
	if _, err := h.q.RequestBounce(context.Background(), theWorkspace, first.request("build_stale", false)); err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	first.awaitStart(t)

	// Act
	got, err := h.q.RequestBounce(context.Background(), theWorkspace, second.request("build_stale", false))

	// Assert
	if err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	if !got.AlreadyPending || !got.Now {
		t.Fatalf("decision = %+v, want it to join the running bounce", got)
	}
	first.finish(h, nil)
	if second.runs.Load() != 0 {
		t.Fatalf("the joining request ran a second bounce")
	}
	if err := <-second.done; err != nil {
		t.Fatalf("second done = %v, want the running bounce's outcome", err)
	}
}

// TestASubmissionsDeliveryHoldsTheBounceDecisionOff covers what replaced the
// keep-alive re-drive's place in the registry: a prompt the shim is holding
// behind its own keep-alive is a StartTurn still in flight, and the bounce is
// decided under the delivery lock that call is made under, so no bounce can
// judge the workspace free while it is being delivered to.
func TestASubmissionsDeliveryHoldsTheBounceDecisionOff(t *testing.T) {
	// Arrange
	h := newHarness(t)
	free := true
	h.sender.startHook = func() { free = h.q.state(theWorkspace).drain.TryLock() }

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "go")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if free {
		t.Fatal("the delivery lock a bounce decides under was free while a StartTurn was in flight")
	}
}

func TestAMalformedBounceRequestIsRefusedLoudly(t *testing.T) {
	tests := []struct {
		name string
		req  bounce.Request
	}{
		{name: "no reason", req: bounce.Request{Run: func(context.Context, ids.WorkspaceID) error { return nil }}},
		{name: "no action", req: bounce.Request{Reason: "build_stale"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.q.RequestBounce(context.Background(), theWorkspace, tc.req)

			// Assert
			if !errors.Is(err, ErrBounceMalformed) {
				t.Fatalf("RequestBounce = %v, want ErrBounceMalformed", err)
			}
			if !recordWith(h.log.Records(), "error", opBounce, "refused a malformed bounce request") {
				t.Fatalf("records = %+v, want the refusal at ERROR", h.log.Records())
			}
		})
	}
}

func TestOnFreeWithNoBounceRegisteredDispatchesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.q.OnFree(theWorkspace)

	// Assert
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want nothing", started)
	}
	if h.q.isDraining(theWorkspace) {
		t.Fatalf("a freeness edge with nothing registered drained the workspace")
	}
}

// ---- a departed shim resolves the work its registered bounce waited on ----

// departedUnasked is a shim that died on its own; departedOrdered is one this
// daemon ended itself.
var (
	departedUnasked = sessionwatcher.Departure{Ordered: false, Cause: sessionwatcher.DepartureLinkDead}
	departedOrdered = sessionwatcher.Departure{Ordered: true, Cause: sessionwatcher.DepartureClosed}
)

// relaunching is a gate's request marked as replacing the shim, which is what
// every rollout shim bounce asks.
func (g *gate) relaunching(reason string) bounce.Request {
	req := g.request(reason, false)
	req.ReplacesShim = true
	return req
}

// registerBehindWork registers req on a workspace whose shim has a turn and a
// monitor in flight, failing the test unless it was registered rather than
// started.
func registerBehindWork(t *testing.T, h *harness, req bounce.Request) {
	t.Helper()
	running(t, h, "running-turn", "the running work")
	h.watcher.detached(monitors(1))
	decision, err := h.q.RequestBounce(context.Background(), theWorkspace, req)
	if err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	if decision.Now {
		t.Fatalf("decision = %+v, want the bounce registered behind the work", decision)
	}
}

// depart tells the queue the watcher's shim is gone and joins the decision.
func depart(h *harness, w *fakeWatcher, d sessionwatcher.Departure) {
	w.depart(d)
	h.q.OnDeparted(theWorkspace, w, d)
	h.q.departing.Wait()
}

// awaitDone answers the gate's Done, failing the test if it never came.
func (g *gate) awaitDone(t *testing.T) error {
	t.Helper()
	select {
	case err := <-g.done:
		return err
	case <-time.After(bounceStartBound):
		t.Fatalf("the bounce's requester was never told how it ended")
		return nil
	}
}

func TestADepartureDecidesTheRegisteredBounceAtOnce(t *testing.T) {
	tests := []struct {
		name string
		// relaunch marks the request as replacing the shim.
		relaunch bool
		// closed records the workspace as closed before the departure.
		closed bool
		// forgotten makes the workspace read answer not-found.
		forgotten bool
		departure sessionwatcher.Departure
		wantRun   bool
		wantMsg   string
	}{
		{
			name:      "a shim that died under a relaunch is relaunched now",
			relaunch:  true,
			departure: departedUnasked,
			wantRun:   true,
			wantMsg:   "the shim died with work recorded in flight; that work ended with it, so its registered bounce relaunches it now",
		},
		{
			name:      "a shim this daemon ended under a relaunch is unregistered",
			relaunch:  true,
			departure: departedOrdered,
			wantMsg:   "the shim departed under a registered bounce with nothing left to replace; unregistered it",
		},
		{
			name:      "a shim that died in a closed workspace is unregistered",
			relaunch:  true,
			closed:    true,
			departure: departedUnasked,
			wantMsg:   "the shim departed under a registered bounce with nothing left to replace; unregistered it",
		},
		{
			name:      "a shim that died in a forgotten workspace is unregistered",
			relaunch:  true,
			forgotten: true,
			departure: departedUnasked,
			wantMsg:   "the shim departed under a registered bounce with nothing left to replace; unregistered it",
		},
		{
			name:      "a transfer is taken whoever ended the shim",
			relaunch:  false,
			departure: departedOrdered,
			wantRun:   true,
			wantMsg:   "the shim departed; the work its registered bounce waited on ended with it, so the bounce is taken now",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			g := newGate()
			req := g.request("build_stale", false)
			if tc.relaunch {
				req = g.relaunching("build_stale")
			}
			registerBehindWork(t, h, req)
			if tc.closed {
				h.db.workspaces[theWorkspace] = wsm.Workspace{ID: theWorkspace, Dir: "/tmp/ws-1", Closed: true}
			}
			if tc.forgotten {
				h.db.workspaceErr = wsm.ErrNotFound
			}

			// Act
			depart(h, h.watcher, tc.departure)

			// Assert
			if tc.wantRun {
				g.awaitStart(t)
				g.finish(h, nil)
				if err := g.awaitDone(t); err != nil {
					t.Fatalf("done = %v, want the bounce to finish cleanly", err)
				}
			} else {
				if err := g.awaitDone(t); !errors.Is(err, bounce.ErrUnregistered) {
					t.Fatalf("done = %v, want ErrUnregistered", err)
				}
				if runs := g.runs.Load(); runs != 0 {
					t.Fatalf("an unregistered bounce ran %d times", runs)
				}
			}
			if !recordWith(h.log.Records(), "info", opBounce, tc.wantMsg) {
				t.Fatalf("records = %+v, want %q", h.log.Records(), tc.wantMsg)
			}
		})
	}
}

func TestADepartureWhoseWorkspaceCannotBeReadFailsTheBounceLoudly(t *testing.T) {
	// Arrange
	h := newHarness(t)
	g := newGate()
	registerBehindWork(t, h, g.relaunching("build_stale"))
	h.db.workspaceErr = errors.New("the state client is gone")

	// Act
	depart(h, h.watcher, departedUnasked)

	// Assert
	err := g.awaitDone(t)
	if err == nil || errors.Is(err, bounce.ErrUnregistered) {
		t.Fatalf("done = %v, want the read failure", err)
	}
	if runs := g.runs.Load(); runs != 0 {
		t.Fatalf("the bounce ran %d times on a workspace that could not be read", runs)
	}
	const want = "the shim departed under a registered bounce and its workspace could not be read; the bounce is failed rather than left waiting"
	if !recordWith(h.log.Records(), "error", opBounce, want) {
		t.Fatalf("records = %+v, want %q at ERROR", h.log.Records(), want)
	}
}

func TestADepartureAfterANewerShimServesDoesNotBounceIt(t *testing.T) {
	tests := []struct {
		name     string
		relaunch bool
		// newerBusy leaves the newer shim with a turn in flight.
		newerBusy bool
		wantRun   bool
		wantDone  error
	}{
		{name: "a relaunch is unregistered: the newer shim runs the installed build", relaunch: true, wantDone: bounce.ErrUnregistered},
		{name: "a transfer waits on the newer shim's own work", relaunch: false, newerBusy: true},
		{name: "a transfer is taken when the newer shim is free", relaunch: false, wantRun: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the bounce registers behind the old shim's work, the old
			// shim dies, and a revival brings a newer one up before the
			// departure is decided.
			h := newHarness(t)
			g := newGate()
			req := g.request("handover_transfer", false)
			if tc.relaunch {
				req = g.relaunching("build_stale")
			}
			registerBehindWork(t, h, req)
			old := h.watcher
			newer := &fakeWatcher{}
			if tc.newerBusy {
				newer.detached(monitors(1))
			}
			h.watcher = newer

			// Act
			depart(h, old, departedUnasked)

			// Assert
			switch {
			case tc.wantRun:
				g.awaitStart(t)
				g.finish(h, nil)
			case tc.wantDone != nil:
				if err := g.awaitDone(t); !errors.Is(err, tc.wantDone) {
					t.Fatalf("done = %v, want %v", err, tc.wantDone)
				}
			default:
				h.q.mu.Lock()
				pending := h.q.states[theWorkspace].bounce
				h.q.mu.Unlock()
				if pending == nil || pending.draining {
					t.Fatalf("pending = %+v, want the bounce still registered behind the newer shim's work", pending)
				}
			}
			if !tc.wantRun && g.runs.Load() != 0 {
				t.Fatalf("the bounce ran over the newer shim")
			}
		})
	}
}

func TestALateDepartureOfAShimTheBounceDoesNotWaitOnJudgesTheCurrentShim(t *testing.T) {
	// Arrange: the bounce registers behind the CURRENT shim's work; an edge
	// then arrives for a shim that was replaced before the registration.
	h := newHarness(t)
	g := newGate()
	earlier := &fakeWatcher{}
	registerBehindWork(t, h, g.relaunching("build_stale"))

	// Act
	depart(h, earlier, departedUnasked)

	// Assert
	h.q.mu.Lock()
	pending := h.q.states[theWorkspace].bounce
	h.q.mu.Unlock()
	if pending == nil || pending.draining {
		t.Fatalf("pending = %+v, want the bounce still waiting on the current shim's work", pending)
	}
	if runs := g.runs.Load(); runs != 0 {
		t.Fatalf("a late edge for another shim ran the bounce %d times", runs)
	}
}

func TestADepartureAndAFreeEdgeTakeTheBounceOnce(t *testing.T) {
	// Arrange: the shim dies with its recorded work, and a freeness edge
	// races the departure for the same registered bounce.
	h := newHarness(t)
	g := newGate()
	registerBehindWork(t, h, g.relaunching("build_stale"))
	h.watcher.depart(departedUnasked)
	var edges sync.WaitGroup
	edges.Add(2)

	// Act
	go func() {
		defer edges.Done()
		h.q.OnDeparted(theWorkspace, h.watcher, departedUnasked)
		h.q.departing.Wait()
	}()
	go func() {
		defer edges.Done()
		h.q.OnFree(theWorkspace)
	}()
	edges.Wait()

	// Assert
	g.awaitStart(t)
	g.finish(h, nil)
	if runs := g.runs.Load(); runs != 1 {
		t.Fatalf("the bounce ran %d times, want exactly once", runs)
	}
}

func TestABounceAskedOfADepartedShimDoesNotWaitOnItsRecordedWork(t *testing.T) {
	// Arrange: the dead shim's watcher still records a turn and a monitor.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.watcher.detached(monitors(1))
	h.watcher.depart(departedUnasked)
	g := newGate()

	// Act
	decision, err := h.q.RequestBounce(context.Background(), theWorkspace, g.relaunching("build_stale"))

	// Assert
	if err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	if !decision.Now || decision.Forced {
		t.Fatalf("decision = %+v, want an unforced bounce now: the recorded work ended with the shim", decision)
	}
	g.awaitStart(t)
	g.finish(h, nil)
}

func TestADepartureWithNoBounceRegisteredDecidesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	depart(h, h.watcher, departedUnasked)

	// Assert
	if !recordWith(h.log.Records(), "debug", opBounce, "the shim departed with no bounce registered") {
		t.Fatalf("records = %+v, want the no-op recorded", h.log.Records())
	}
	if h.q.isDraining(theWorkspace) {
		t.Fatalf("a departure with nothing registered drained the workspace")
	}
}

func TestADepartureAfterTheExitsDrainDecidesNothing(t *testing.T) {
	// Arrange: the exit has joined the queue's work; a watcher closed by the
	// exit then reports its shim departed.
	h := newHarness(t)
	g := newGate()
	registerBehindWork(t, h, g.relaunching("build_stale"))
	if !h.q.Drain(bounceStartBound) {
		t.Fatalf("Drain did not join the queue's work")
	}

	// Act
	depart(h, h.watcher, departedOrdered)

	// Assert
	if runs := g.runs.Load(); runs != 0 {
		t.Fatalf("a departure at the exit ran the bounce %d times", runs)
	}
	if !recordWith(h.log.Records(), "debug", opBounce, "the daemon is exiting; a shim departure at the exit decides nothing") {
		t.Fatalf("records = %+v, want the exit's no-op recorded", h.log.Records())
	}
}

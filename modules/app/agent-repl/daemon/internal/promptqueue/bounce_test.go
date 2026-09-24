package promptqueue

import (
	"context"
	"errors"
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

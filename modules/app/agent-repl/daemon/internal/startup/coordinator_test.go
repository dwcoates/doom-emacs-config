package startup

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/sidebar"
)

// waitBound is how long a test waits for the run's next event before it fails.
const waitBound = 5 * time.Second

// bringUpCall is one BringUp the coordinator asked for.
type bringUpCall struct {
	pending []ids.WorkspaceID
	done    func(ws ids.WorkspaceID, err error)
}

// world is a coordinator over fake collaborators.
type world struct {
	c       *Coordinator
	log     *dlog.TestLogger
	live    map[ids.WorkspaceID]bool
	bringUp chan bringUpCall
	events  chan *agentreplv1.DaemonStartupEvent
	order   []sidebar.TabEntry
}

func newWorld(t *testing.T, names ...string) *world {
	t.Helper()
	w := &world{
		log:     dlog.NewTestLogger(),
		live:    map[ids.WorkspaceID]bool{},
		bringUp: make(chan bringUpCall, 4),
		events:  make(chan *agentreplv1.DaemonStartupEvent, 256),
	}
	for _, n := range names {
		w.order = append(w.order, sidebar.TabEntry{Ref: &workspacev1.WorkspaceRef{Id: n}, Name: n})
	}
	c, err := New(Deps{
		Order:   func() []sidebar.TabEntry { return w.order },
		Live:    func(ws ids.WorkspaceID) bool { return w.live[ws] },
		BringUp: func(p []ids.WorkspaceID, done func(ids.WorkspaceID, error)) { w.bringUp <- bringUpCall{p, done} },
		Now:     func() time.Time { return time.Unix(100, 0) },
		Log:     w.log,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	w.c = c
	return w
}

// start runs one startup on its own goroutine; the returned cancel ends it and
// the channel closes when Run has returned.
func (w *world) start() (context.CancelFunc, <-chan struct{}) {
	ctx, cancel := context.WithCancel(context.Background())
	returned := make(chan struct{})
	go func() {
		defer close(returned)
		w.c.Run(ctx, func(e *agentreplv1.DaemonStartupEvent) { w.events <- e })
	}()
	return cancel, returned
}

// next answers the run's next event, failing after the bound.
func (w *world) next(t *testing.T) string {
	t.Helper()
	select {
	case e := <-w.events:
		return describe(e)
	case <-time.After(waitBound):
		t.Fatal("the run sent no further event")
		return ""
	}
}

// expect reads the next events and requires exactly want.
func (w *world) expect(t *testing.T, want ...string) {
	t.Helper()
	for _, wanted := range want {
		if got := w.next(t); got != wanted {
			t.Fatalf("event = %q, want %q", got, wanted)
		}
	}
}

// awaitBringUp answers the BringUp the run asked for.
func (w *world) awaitBringUp(t *testing.T) bringUpCall {
	t.Helper()
	select {
	case call := <-w.bringUp:
		return call
	case <-time.After(waitBound):
		t.Fatal("the run asked for no bring-up")
		return bringUpCall{}
	}
}

// describe spells an event compactly for the assertions.
func describe(e *agentreplv1.DaemonStartupEvent) string {
	switch ev := e.GetEvent().(type) {
	case *agentreplv1.DaemonStartupEvent_Opening:
		return fmt.Sprintf("opening %d", ev.Opening.GetWorkspaces())
	case *agentreplv1.DaemonStartupEvent_WorkspaceOpen:
		return "open " + ev.WorkspaceOpen.GetWorkspace().GetId()
	case *agentreplv1.DaemonStartupEvent_Finished:
		names := []string{}
		for _, f := range ev.Finished.GetFailed() {
			names = append(names, f.GetName())
		}
		return fmt.Sprintf("finished %d/%d failed=%s", ev.Finished.GetReady(), ev.Finished.GetTotal(), strings.Join(names, ","))
	case *agentreplv1.DaemonStartupEvent_WorkspaceStep:
		step := ev.WorkspaceStep
		ws := step.GetWorkspace().GetId()
		switch s := step.GetStep().(type) {
		case *agentreplv1.DaemonStartupWorkspaceStep_StartingSession:
			return ws + " starting_session"
		case *agentreplv1.DaemonStartupWorkspaceStep_Waking:
			return ws + " waking"
		case *agentreplv1.DaemonStartupWorkspaceStep_Resuming:
			return ws + " resuming"
		case *agentreplv1.DaemonStartupWorkspaceStep_VendorRetrying:
			return fmt.Sprintf("%s vendor_retrying %d", ws, s.VendorRetrying.GetAttempt())
		case *agentreplv1.DaemonStartupWorkspaceStep_VendorRejected:
			return ws + " vendor_rejected " + s.VendorRejected.GetCause()
		case *agentreplv1.DaemonStartupWorkspaceStep_VendorFailed:
			return ws + " vendor_failed"
		case *agentreplv1.DaemonStartupWorkspaceStep_ColdGate:
			return ws + " cold_gate"
		case *agentreplv1.DaemonStartupWorkspaceStep_Offline:
			return ws + " offline"
		case *agentreplv1.DaemonStartupWorkspaceStep_WaitingFor:
			return ws + " waiting_for " + s.WaitingFor.GetAhead().GetId()
		case *agentreplv1.DaemonStartupWorkspaceStep_Failed:
			return ws + " failed " + s.Failed.GetReason()
		}
	}
	return "unset"
}

func TestARunOfLiveWorkspacesOpensThemAllInRegistryOrder(t *testing.T) {
	// Arrange
	w := newWorld(t, "a", "b", "c")
	w.live["a"], w.live["b"], w.live["c"] = true, true, true

	// Act
	_, returned := w.start()

	// Assert
	w.expect(t, "opening 3", "open a", "open b", "open c", "finished 3/3 failed=")
	<-returned
}

// THE OWNER'S ORDERING RULE: workspace 3 is ready first and still waits for 1
// and 2, saying whom it waits on each time that changes.
func TestAWorkspaceReadyFirstWaitsForTheOnesAheadOfIt(t *testing.T) {
	// Arrange
	w := newWorld(t, "one", "two", "three")
	_, returned := w.start()
	w.expect(t, "opening 3")
	w.awaitBringUp(t)

	// Act
	w.c.Step("three", Step{Kind: StepServing})
	w.expect(t, "three waiting_for one")
	w.c.Step("one", Step{Kind: StepServing})
	w.expect(t, "open one", "three waiting_for two")
	w.c.Step("two", Step{Kind: StepServing})

	// Assert
	w.expect(t, "open two", "open three", "finished 3/3 failed=")
	<-returned
}

func TestAFailedWorkspaceStillGetsItsGoAheadInOrder(t *testing.T) {
	// Arrange
	w := newWorld(t, "one", "two")
	w.live["two"] = true
	_, returned := w.start()
	w.expect(t, "opening 2", "two waiting_for one")
	w.awaitBringUp(t)

	// Act
	w.c.Step("one", Step{Kind: StepFailed, Text: "spawn refused"})

	// Assert
	w.expect(t, "one failed spawn refused", "open one", "open two", "finished 1/2 failed=one")
	<-returned
}

func TestTheVendorNeverGatesTheGoAhead(t *testing.T) {
	tests := []struct {
		name string
		step Step
		want string
	}{
		{name: "a vendor start being retried", step: Step{Kind: StepVendorRetrying, Attempt: 2}, want: "one vendor_retrying 2"},
		{name: "a vendor that refused", step: Step{Kind: StepVendorRejected, Text: "bad key"}, want: "one vendor_rejected bad key"},
		{name: "a vendor that failed for the window", step: Step{Kind: StepVendorFailed}, want: "one vendor_failed"},
		{name: "the network unreachable", step: Step{Kind: StepOffline}, want: "one offline"},
		{name: "a cold gate awaiting the user", step: Step{Kind: StepColdGate}, want: "one cold_gate"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t, "one")
			_, returned := w.start()
			w.expect(t, "opening 1")
			w.awaitBringUp(t)

			// Act
			w.c.Step("one", tt.step)

			// Assert
			w.expect(t, tt.want, "open one", "finished 1/1 failed=")
			<-returned
		})
	}
}

func TestEveryPrintedStepIsRelayed(t *testing.T) {
	// Arrange
	w := newWorld(t, "one")
	_, returned := w.start()
	w.expect(t, "opening 1")
	w.awaitBringUp(t)

	// Act
	w.c.Step("one", Step{Kind: StepWaking})
	w.c.Step("one", Step{Kind: StepResuming})

	// Assert: serving was never relayed; resuming proves the services serve.
	w.expect(t, "one waking", "one resuming", "open one", "finished 1/1 failed=")
	<-returned
}

func TestOnlyTheWorkspacesNotUpAreBroughtUp(t *testing.T) {
	// Arrange
	w := newWorld(t, "a", "b", "c")
	w.live["b"] = true

	// Act
	cancel, returned := w.start()
	defer func() { cancel(); <-returned }()
	call := w.awaitBringUp(t)

	// Assert
	if !slices.Equal(call.pending, []ids.WorkspaceID{"a", "c"}) {
		t.Fatalf("brought up %v, want [a c]", call.pending)
	}
}

func TestAStartThatEndedWithoutAStepSettlesFailed(t *testing.T) {
	// Arrange
	w := newWorld(t, "one")
	_, returned := w.start()
	w.expect(t, "opening 1")
	call := w.awaitBringUp(t)

	// Act
	call.done("one", errors.New("the session record could not be read"))

	// Assert
	w.expect(t, "one failed the session record could not be read", "open one", "finished 0/1 failed=one")
	<-returned
}

func TestAStartThatReturnedUpIsReady(t *testing.T) {
	// Arrange
	w := newWorld(t, "one")
	_, returned := w.start()
	w.expect(t, "opening 1")
	call := w.awaitBringUp(t)

	// Act
	call.done("one", nil)

	// Assert
	w.expect(t, "open one", "finished 1/1 failed=")
	<-returned
}

// A BRING-UP SOMEONE ELSE STARTED (the boot's) is not started again, and how
// far it has got before the run began counts.
func TestABringUpAlreadyServingWhenTheRunBeginsIsReady(t *testing.T) {
	// Arrange
	w := newWorld(t, "one")
	w.c.Step("one", Step{Kind: StepStartingSession})
	w.c.Step("one", Step{Kind: StepServing})

	// Act
	_, returned := w.start()

	// Assert
	w.expect(t, "opening 1", "open one", "finished 1/1 failed=")
	<-returned
	select {
	case call := <-w.bringUp:
		t.Fatalf("brought up %v again", call.pending)
	default:
	}
}

func TestABringUpInFlightWhenTheRunBeginsIsFollowed(t *testing.T) {
	// Arrange
	w := newWorld(t, "one")
	w.c.Step("one", Step{Kind: StepStartingSession})
	_, returned := w.start()
	w.expect(t, "opening 1")

	// Act
	w.c.Step("one", Step{Kind: StepServing})

	// Assert
	w.expect(t, "open one", "finished 1/1 failed=")
	<-returned
}

// EVENTS ARE NOT REPLAYED: a step taken before a run began reaches no run,
// and a run that has finished hands nothing to the next.
func TestEventsAreNeverReplayedToALaterRun(t *testing.T) {
	// Arrange
	w := newWorld(t, "one")
	w.live["one"] = true
	w.c.Step("one", Step{Kind: StepVendorRetrying, Attempt: 1})
	w.c.Step("one", Step{Kind: StepUp})
	_, first := w.start()
	w.expect(t, "opening 1", "open one", "finished 1/1 failed=")
	<-first

	// Act
	_, second := w.start()

	// Assert
	w.expect(t, "opening 1", "open one", "finished 1/1 failed=")
	<-second
	select {
	case e := <-w.events:
		t.Fatalf("an extra event %q reached the second run", describe(e))
	default:
	}
}

func TestAStreamThatEndsEndsTheRunUnfinished(t *testing.T) {
	// Arrange
	w := newWorld(t, "one")
	cancel, returned := w.start()
	w.expect(t, "opening 1")
	w.awaitBringUp(t)

	// Act
	cancel()

	// Assert
	<-returned
	select {
	case e := <-w.events:
		t.Fatalf("event %q after the stream ended", describe(e))
	default:
	}
}

func TestAnEmptyRegistryFinishesAtOnce(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	_, returned := w.start()

	// Assert
	w.expect(t, "opening 0", "finished 0/0 failed=")
	<-returned
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	full := Deps{
		Order:   func() []sidebar.TabEntry { return nil },
		Live:    func(ids.WorkspaceID) bool { return false },
		BringUp: func([]ids.WorkspaceID, func(ids.WorkspaceID, error)) {},
		Now:     time.Now,
		Log:     dlog.NewTestLogger(),
	}
	tests := []struct {
		name  string
		strip func(*Deps)
	}{
		{"no order", func(d *Deps) { d.Order = nil }},
		{"no liveness", func(d *Deps) { d.Live = nil }},
		{"no bring-up", func(d *Deps) { d.BringUp = nil }},
		{"no clock", func(d *Deps) { d.Now = nil }},
		{"no logger", func(d *Deps) { d.Log = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			deps := full
			tt.strip(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatal("New = nil, want a refusal")
			}
		})
	}
}

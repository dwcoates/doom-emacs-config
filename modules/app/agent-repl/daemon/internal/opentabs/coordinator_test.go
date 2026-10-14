package opentabs

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// waitBound is how long a test waits for the open's next frame before it
// fails: a small multiple of an in-process channel hop.
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
	bringUp chan bringUpCall
	frames  chan *agentreplv1.OpenTabsResponse

	mu      sync.Mutex
	live    map[ids.WorkspaceID]bool
	opening []string
	// leaveDuringOwed makes the owed read the moment the client goes away:
	// it cancels the open and fails the read with the cancellation.
	leaveDuringOwed bool
	// cancel ends the running open (startEmitting sets it).
	cancel context.CancelFunc
	// owed are the workspaces owed a cut turn; owedErr fails the read.
	owed    map[ids.WorkspaceID]bool
	owedErr error
	// readErr fails every conversation read; reads counts them; readGate,
	// when set, holds each read until the test sends on it.
	readErr  error
	reads    map[ids.WorkspaceID]int
	readGate chan struct{}
	// revision is the fake roster's revision, raised by every opening edge.
	revision uint64
	// owedReads counts the owed-resumption reads.
	owedReads int
}

func newWorld(t *testing.T) *world {
	t.Helper()
	w := &world{
		log:     dlog.NewTestLogger(),
		live:    map[ids.WorkspaceID]bool{},
		owed:    map[ids.WorkspaceID]bool{},
		reads:   map[ids.WorkspaceID]int{},
		bringUp: make(chan bringUpCall, 4),
		frames:  make(chan *agentreplv1.OpenTabsResponse, 256),
	}
	c, err := New(Deps{
		Live: func(ws ids.WorkspaceID) bool {
			w.mu.Lock()
			defer w.mu.Unlock()
			return w.live[ws]
		},
		BringUp: func(p []ids.WorkspaceID, done func(ids.WorkspaceID, error)) { w.bringUp <- bringUpCall{p, done} },
		Opening: func(ws ids.WorkspaceID, opening bool) {
			w.mu.Lock()
			defer w.mu.Unlock()
			w.opening = append(w.opening, fmt.Sprintf("%s:%v", ws, opening))
			w.revision++
		},
		ReadConversation: func(ctx context.Context, ws ids.WorkspaceID) error {
			w.mu.Lock()
			gate := w.readGate
			w.reads[ws]++
			err := w.readErr
			w.mu.Unlock()
			if gate != nil {
				select {
				case <-gate:
				case <-ctx.Done():
					return ctx.Err()
				}
			}
			return err
		},
		Owed: func(_ context.Context, ws ids.WorkspaceID) (bool, error) {
			w.mu.Lock()
			defer w.mu.Unlock()
			w.owedReads++
			if w.leaveDuringOwed {
				w.cancel()
				return false, context.Canceled
			}
			return w.owed[ws], w.owedErr
		},
		Row: func(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool) {
			w.mu.Lock()
			defer w.mu.Unlock()
			return &frontendv1.RosterRow{
				Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: string(ws)}},
				Name:      &frontendv1.RosterRowName{Text: fmt.Sprintf("%s@%d", ws, w.revision)},
			}, w.revision, true
		},
		Now: func() time.Time { return time.Unix(100, 0) },
		Log: w.log,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	w.c = c
	return w
}

func (w *world) setLive(ws ids.WorkspaceID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.live[ws] = true
}

func (w *world) openingEdges() []string {
	w.mu.Lock()
	defer w.mu.Unlock()
	return append([]string(nil), w.opening...)
}

// none and loading build a request's workspaces.
func none(id string) Workspace {
	return Workspace{Ref: &workspacev1.WorkspaceRef{Id: id}, Name: id}
}

func loading(id string) Workspace {
	ws := none(id)
	ws.Page = true
	return ws
}

// start runs one open on its own goroutine; the returned cancel ends it and
// the channel closes when Run has returned.
func (w *world) start(workspaces ...Workspace) (context.CancelFunc, <-chan struct{}) {
	return w.startEmitting(func(f *agentreplv1.OpenTabsResponse) error { w.frames <- f; return nil }, workspaces...)
}

func (w *world) startEmitting(emit Emit, workspaces ...Workspace) (context.CancelFunc, <-chan struct{}) {
	ctx, cancel := context.WithCancel(context.Background())
	w.mu.Lock()
	w.cancel = cancel
	w.mu.Unlock()
	returned := make(chan struct{})
	go func() {
		defer close(returned)
		w.c.Run(ctx, workspaces, emit)
	}()
	return cancel, returned
}

// next answers the open's next frame, failing after the bound.
func (w *world) next(t *testing.T) string {
	t.Helper()
	select {
	case f := <-w.frames:
		return describe(f)
	case <-time.After(waitBound):
		t.Fatal("the open sent no further frame")
		return ""
	}
}

// expect reads the next frames and requires exactly want.
func (w *world) expect(t *testing.T, want ...string) {
	t.Helper()
	for _, wanted := range want {
		if got := w.next(t); got != wanted {
			t.Fatalf("frame = %q, want %q", got, wanted)
		}
	}
}

// awaitBringUp answers the BringUp the open asked for.
func (w *world) awaitBringUp(t *testing.T) bringUpCall {
	t.Helper()
	select {
	case call := <-w.bringUp:
		return call
	case <-time.After(waitBound):
		t.Fatal("the open asked for no bring-up")
		return bringUpCall{}
	}
}

// awaitReturned waits for Run to return.
func awaitReturned(t *testing.T, returned <-chan struct{}) {
	t.Helper()
	select {
	case <-returned:
	case <-time.After(waitBound):
		t.Fatal("Run did not return")
	}
}

// describe spells a frame compactly for the assertions.
func describe(f *agentreplv1.OpenTabsResponse) string {
	switch fr := f.GetFrame().(type) {
	case *agentreplv1.OpenTabsResponse_Opened:
		o := fr.Opened
		ws := o.GetWorkspace().GetId()
		switch arm := o.GetOutcome().(type) {
		case *agentreplv1.OpenTabsOpened_Ready:
			return ws + " ready"
		case *agentreplv1.OpenTabsOpened_Failed:
			return ws + " failed " + arm.Failed.GetReason()
		case *agentreplv1.OpenTabsOpened_Hidden:
			return ws + " hidden"
		case *agentreplv1.OpenTabsOpened_Refused:
			return ws + " refused " + refusedCause(arm.Refused)
		}
		return ws + " opened?"
	case *agentreplv1.OpenTabsResponse_Finished:
		names := []string{}
		for _, f := range fr.Finished.GetFailed() {
			names = append(names, f.GetName())
		}
		return fmt.Sprintf("finished %d/%d failed=%s", fr.Finished.GetReady(), fr.Finished.GetTotal(), strings.Join(names, ","))
	case *agentreplv1.OpenTabsResponse_Step:
		step := fr.Step
		ws := step.GetWorkspace().GetId()
		switch s := step.GetStep().(type) {
		case *agentreplv1.OpenTabsStep_StartingSession:
			return ws + " starting_session"
		case *agentreplv1.OpenTabsStep_Waking:
			return ws + " waking"
		case *agentreplv1.OpenTabsStep_Resuming:
			return ws + " resuming"
		case *agentreplv1.OpenTabsStep_VendorRetrying:
			return fmt.Sprintf("%s vendor_retrying %d", ws, s.VendorRetrying.GetAttempt())
		case *agentreplv1.OpenTabsStep_VendorRejected:
			return ws + " vendor_rejected " + s.VendorRejected.GetCause()
		case *agentreplv1.OpenTabsStep_VendorFailed:
			return ws + " vendor_failed"
		case *agentreplv1.OpenTabsStep_ColdGate:
			return ws + " cold_gate"
		case *agentreplv1.OpenTabsStep_Offline:
			return ws + " offline"
		case *agentreplv1.OpenTabsStep_Served:
			return ws + " served"
		case *agentreplv1.OpenTabsStep_PageConnected:
			return ws + " page_connected"
		case *agentreplv1.OpenTabsStep_PageDrawn:
			return ws + " page_drawn"
		case *agentreplv1.OpenTabsStep_ConversationRead:
			return ws + " conversation_read"
		case *agentreplv1.OpenTabsStep_ResumingCutTurn:
			return ws + " resuming_cut_turn"
		}
	}
	return "?"
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	full := func() Deps {
		return Deps{
			Live:             func(ids.WorkspaceID) bool { return false },
			BringUp:          func([]ids.WorkspaceID, func(ids.WorkspaceID, error)) {},
			Opening:          func(ids.WorkspaceID, bool) {},
			ReadConversation: func(context.Context, ids.WorkspaceID) error { return nil },
			Owed:             func(context.Context, ids.WorkspaceID) (bool, error) { return false, nil },
			Row:              func(ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool) { return nil, 0, false },
			Now:              time.Now,
			Log:              dlog.NewTestLogger(),
		}
	}
	cases := []struct {
		name  string
		strip func(*Deps)
	}{
		{"liveness", func(d *Deps) { d.Live = nil }},
		{"bring-up", func(d *Deps) { d.BringUp = nil }},
		{"opening marker", func(d *Deps) { d.Opening = nil }},
		{"conversation read", func(d *Deps) { d.ReadConversation = nil }},
		{"owed read", func(d *Deps) { d.Owed = nil }},
		{"roster row", func(d *Deps) { d.Row = nil }},
		{"clock", func(d *Deps) { d.Now = nil }},
		{"logger", func(d *Deps) { d.Log = nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tc.strip(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatal("New succeeded without a required collaborator")
			}
		})
	}
}

func TestARefusedWorkspaceIsAnsweredAtOnce(t *testing.T) {
	// Arrange
	w := newWorld(t)
	ws := none("a")
	ws.Refused = &agentreplv1.OpenTabsOpenedRefused{Cause: &agentreplv1.OpenTabsOpenedRefused_UnknownWorkspace{
		UnknownWorkspace: &agentreplv1.OpenTabsRefusedUnknownWorkspace{}}}

	// Act
	_, returned := w.start(ws)

	// Assert
	w.expect(t, "a refused unknown_workspace", "finished 0/1 failed=")
	awaitReturned(t, returned)
	if len(w.bringUp) != 0 || len(w.openingEdges()) != 0 {
		t.Fatalf("a refused workspace was started or marked opening: %v", w.openingEdges())
	}
}

func TestAHiddenWorkspaceIsAnsweredHiddenAndNothingStarts(t *testing.T) {
	// Arrange
	w := newWorld(t)
	ws := loading("a")
	ws.Hidden = true

	// Act
	_, returned := w.start(ws)

	// Assert
	w.expect(t, "a hidden", "finished 0/1 failed=")
	awaitReturned(t, returned)
	if len(w.bringUp) != 0 || len(w.openingEdges()) != 0 {
		t.Fatalf("a hidden workspace was started or marked opening: %v", w.openingEdges())
	}
}

func TestALiveWorkspaceWithNoPageIsReadyOnceServed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")

	// Act
	_, returned := w.start(none("a"))

	// Assert
	w.expect(t, "a served", "a conversation_read", "a ready", "finished 1/1 failed=")
	awaitReturned(t, returned)
}

func TestALiveWorkspaceWithAPageLoadingWaitsForItToDraw(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")
	w.c.PageAttached("p")
	cancel, _ := w.start(loading("a"))
	defer cancel()
	w.expect(t, "a served", "a conversation_read")

	// Act
	w.c.PageServes("p", "a")
	w.expect(t, "a page_connected")
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}

	// Assert
	w.expect(t, "a page_drawn", "a ready", "finished 1/1 failed=")
}

func TestAPageThatDrewBeforeTheOpenIsTakenAtOnce(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")
	w.c.PageAttached("p")
	w.c.PageServes("p", "a")
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}

	// Act
	w.start(loading("a"))

	// Assert
	w.expect(t, "a served", "a page_connected", "a page_drawn", "a conversation_read", "a ready", "finished 1/1 failed=")
}

func TestAPageThatDrewWithoutServingIsConnectedFirst(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")
	w.c.PageAttached("p")
	cancel, _ := w.start(loading("a"))
	defer cancel()
	w.expect(t, "a served", "a conversation_read")

	// Act
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}

	// Assert
	w.expect(t, "a page_connected", "a page_drawn", "a ready")
}

func TestAWorkspaceWithNoPageIsToldNoPageStep(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.c.PageAttached("p")
	w.c.PageServes("p", "a")
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}
	cancel, _ := w.start(none("a"))
	defer cancel()
	call := w.awaitBringUp(t)

	// Act
	call.done("a", nil)

	// Assert
	w.expect(t, "a served", "a conversation_read", "a ready", "finished 1/1 failed=")
}

func TestADetachedPagesDrawnReportIsForgotten(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")
	w.c.PageAttached("p")
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}

	// Act
	w.c.PageDetached("p")
	cancel, _ := w.start(loading("a"))
	defer cancel()

	// Assert: served, and then nothing until a page draws again.
	w.expect(t, "a served", "a conversation_read")
	w.c.PageAttached("q")
	if err := w.c.PageDrawn("q", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}
	w.expect(t, "a page_connected", "a page_drawn", "a ready")
}

func TestPageDrawnRefusesAnUnknownPage(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	err := w.c.PageDrawn("never", "a")

	// Assert
	if !errors.Is(err, ErrUnknownPage) {
		t.Fatalf("PageDrawn = %v, want ErrUnknownPage", err)
	}
}

func TestPageDrawnRefusesAPageThatDetached(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.c.PageAttached("p")
	w.c.PageDetached("p")

	// Act
	err := w.c.PageDrawn("p", "a")

	// Assert
	if !errors.Is(err, ErrUnknownPage) {
		t.Fatalf("PageDrawn = %v, want ErrUnknownPage", err)
	}
}

func TestPageDetachedOfAnUnknownPageIsAnInvariantViolation(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	w.c.PageDetached("never")

	// Assert
	for _, r := range w.log.Records() {
		if r.Level == dlog.LevelError && r.Operation == opPage {
			return
		}
	}
	t.Fatalf("records = %+v, want an ERROR naming the unknown detach", w.log.Records())
}

func TestAWorkspaceNotLiveIsBroughtUp(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	w.start(none("a"), none("b"))

	// Assert
	call := w.awaitBringUp(t)
	if !slices.Equal(call.pending, []ids.WorkspaceID{"a", "b"}) {
		t.Fatalf("pending = %v, want both workspaces", call.pending)
	}
}

func TestABroughtUpWorkspaceIsReadyWhenItsStartReturns(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"))
	call := w.awaitBringUp(t)

	// Act
	call.done("a", nil)

	// Assert
	w.expect(t, "a served", "a conversation_read", "a ready", "finished 1/1 failed=")
}

func TestAStartThatFailsWithNoStepOpensFailed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"))
	call := w.awaitBringUp(t)

	// Act
	call.done("a", errors.New("boom"))

	// Assert
	w.expect(t, "a failed boom", "finished 0/1 failed=a")
}

func TestAFailedStepOpensFailedWithItsReason(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(loading("a"))
	w.awaitBringUp(t)

	// Act
	w.c.Step("a", Step{Kind: StepFailed, Text: "the shim never answered"})

	// Assert: a failure does not wait for the page.
	w.expect(t, "a failed the shim never answered", "finished 0/1 failed=a")
}

func TestAVendorThatRefusesStillOpensReady(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"))
	w.awaitBringUp(t)

	// Act
	w.c.Step("a", Step{Kind: StepVendorRejected, Text: "logged out"})

	// Assert
	w.expect(t, "a vendor_rejected logged out", "a served", "a conversation_read", "a ready")
}

func TestEveryRelayedStepIsTold(t *testing.T) {
	cases := []struct {
		step Step
		want string
	}{
		{Step{Kind: StepStartingSession}, "a starting_session"},
		{Step{Kind: StepWaking}, "a waking"},
		{Step{Kind: StepResuming}, "a resuming"},
		{Step{Kind: StepVendorRetrying, Attempt: 2}, "a vendor_retrying 2"},
		{Step{Kind: StepVendorFailed}, "a vendor_failed"},
		{Step{Kind: StepColdGate}, "a cold_gate"},
		{Step{Kind: StepOffline}, "a offline"},
	}
	for _, tc := range cases {
		t.Run(tc.step.Kind.String(), func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			cancel, _ := w.start(loading("a"))
			defer cancel()
			w.awaitBringUp(t)

			// Act
			w.c.Step("a", tc.step)

			// Assert
			w.expect(t, tc.want)
		})
	}
}

func TestTheServingStepIsToldAsServed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	cancel, _ := w.start(loading("a"))
	defer cancel()
	w.awaitBringUp(t)

	// Act
	w.c.Step("a", Step{Kind: StepServing})

	// Assert
	w.expect(t, "a served")
}

func TestNoWorkspaceWaitsOnAnEarlierOne(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"), none("b"))
	call := w.awaitBringUp(t)

	// Act: the second one opens first.
	call.done("b", nil)

	// Assert
	w.expect(t, "b served", "b conversation_read", "b ready")
}

func TestAnotherBringUpInFlightIsFollowedNotRepeated(t *testing.T) {
	// Arrange: the boot's own bring-up of the workspace is under way.
	w := newWorld(t)
	w.c.Step("a", Step{Kind: StepStartingSession})
	w.start(none("a"))

	// Act
	w.c.Step("a", Step{Kind: StepServing})

	// Assert
	w.expect(t, "a served", "a conversation_read", "a ready", "finished 1/1 failed=")
	if len(w.bringUp) != 0 {
		t.Fatal("the open started a workspace another bring-up was already starting")
	}
}

func TestTheOpeningMarkerStandsUntilTheWorkspaceIsAnswered(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"))
	call := w.awaitBringUp(t)
	if got := w.openingEdges(); !slices.Equal(got, []string{"a:true"}) {
		t.Fatalf("opening edges before the answer = %v, want the raise", got)
	}

	// Act
	call.done("a", nil)
	w.expect(t, "a served", "a conversation_read", "a ready")

	// Assert
	if got := w.openingEdges(); !slices.Equal(got, []string{"a:true", "a:false"}) {
		t.Fatalf("opening edges = %v, want raised then lowered", got)
	}
}

func TestTwoOpensOfOneWorkspaceLowerTheMarkerOnlyOnTheLast(t *testing.T) {
	// Arrange
	w := newWorld(t)
	cancelFirst, firstReturned := w.start(none("a"))
	w.awaitBringUp(t)
	w.start(none("a"))
	w.awaitBringUp(t)

	// Act
	cancelFirst()
	awaitReturned(t, firstReturned)

	// Assert
	if got := w.openingEdges(); !slices.Equal(got, []string{"a:true"}) {
		t.Fatalf("opening edges = %v, want the marker standing for the second open", got)
	}
}

func TestAnOpenWhoseClientWentAwayLowersItsMarker(t *testing.T) {
	// Arrange
	w := newWorld(t)
	cancel, returned := w.start(none("a"))
	w.awaitBringUp(t)

	// Act
	cancel()
	awaitReturned(t, returned)

	// Assert
	if got := w.openingEdges(); !slices.Equal(got, []string{"a:true", "a:false"}) {
		t.Fatalf("opening edges = %v, want the marker lowered as the open ended", got)
	}
}

func TestAnOpenEndsWhenItsFrameCannotBeSent(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")

	// Act
	_, returned := w.startEmitting(func(*agentreplv1.OpenTabsResponse) error { return errors.New("gone") }, loading("a"))

	// Assert
	awaitReturned(t, returned)
}

func TestTheOpenedFrameIsLoggedAtInfo(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")

	// Act
	w.start(none("a"))
	w.expect(t, "a served", "a conversation_read", "a ready", "finished 1/1 failed=")

	// Assert
	for _, r := range w.log.Records() {
		if r.Level == dlog.LevelInfo && r.Operation == op && r.Context[dlog.KeyWorkspaceID] == "a" && strings.Contains(r.Message, "is open") {
			return
		}
	}
	t.Fatalf("records = %+v, want an INFO naming the opened workspace", w.log.Records())
}

// nextOpened answers the open's next frame, which must be an `opened`.
func (w *world) nextOpened(t *testing.T) *agentreplv1.OpenTabsOpened {
	t.Helper()
	select {
	case f := <-w.frames:
		opened := f.GetOpened()
		if opened == nil {
			t.Fatalf("frame = %q, want an opened", describe(f))
		}
		return opened
	case <-time.After(waitBound):
		t.Fatal("the open sent no further frame")
		return nil
	}
}

// owe records a cut turn the workspace is owed.
func (w *world) owe(ws ids.WorkspaceID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.owed[ws] = true
}

// gateReads makes every conversation read wait for a send on the answer.
func (w *world) gateReads() chan struct{} {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.readGate = make(chan struct{})
	return w.readGate
}

func (w *world) owedReadCount() int {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.owedReads
}

// ---- the conversation read ----

func TestReadyWaitsForTheConversationRead(t *testing.T) {
	// Arrange: the page drew before the open, so only the read is left.
	w := newWorld(t)
	w.setLive("a")
	gate := w.gateReads()
	w.c.PageAttached("p")
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}
	w.start(loading("a"))
	w.expect(t, "a served", "a page_connected", "a page_drawn")

	// Act
	close(gate)

	// Assert
	w.expect(t, "a conversation_read", "a ready")
}

func TestAConversationReadThatFailsOpensFailed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")
	w.readErr = errors.New("store unreachable")

	// Act
	w.start(none("a"))

	// Assert
	w.expect(t, "a served", "a failed the conversation could not be read: store unreachable", "finished 0/1 failed=a")
}

func TestTheConversationIsReadOncePerOpen(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"))
	call := w.awaitBringUp(t)
	w.c.Step("a", Step{Kind: StepServing})
	w.expect(t, "a served", "a conversation_read")

	// Act: the start returning serves it again.
	call.done("a", nil)
	w.expect(t, "a ready")

	// Assert
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.reads["a"] != 1 {
		t.Fatalf("reads = %d, want one", w.reads["a"])
	}
}

// ---- the cut turn ----

func TestReadyWaitsForTheCutTurnToStart(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.owe("a")
	w.start(none("a"))
	w.awaitBringUp(t)
	w.c.Step("a", Step{Kind: StepServing})
	w.expect(t, "a served", "a conversation_read")
	w.c.Resumption("a", Resume{Kind: ResumeStarting})
	w.expect(t, "a resuming_cut_turn")

	// Act
	w.c.Resumption("a", Resume{Kind: ResumeStarted})

	// Assert
	w.expect(t, "a ready", "finished 1/1 failed=")
}

func TestAWorkspaceOwedNoCutTurnSendsNoResumeStep(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"))
	call := w.awaitBringUp(t)

	// Act
	call.done("a", nil)

	// Assert
	w.expect(t, "a served", "a conversation_read", "a ready", "finished 1/1 failed=")
}

func TestAResumptionThatFailsOpensFailed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.owe("a")
	w.start(none("a"))
	w.awaitBringUp(t)
	w.c.Step("a", Step{Kind: StepServing})
	w.expect(t, "a served", "a conversation_read")
	w.c.Resumption("a", Resume{Kind: ResumeStarting})
	w.expect(t, "a resuming_cut_turn")

	// Act
	w.c.Resumption("a", Resume{Kind: ResumeFailed, Text: "no watcher"})

	// Assert
	w.expect(t, "a failed the turn the restart cut could not be resumed: no watcher", "finished 0/1 failed=a")
}

func TestAReleaseThatResumesNothingNowLetsTheOpenAnswer(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.owe("a")
	w.start(none("a"))
	w.awaitBringUp(t)
	w.c.Step("a", Step{Kind: StepServing})
	w.expect(t, "a served", "a conversation_read")

	// Act
	w.c.Resumption("a", Resume{Kind: ResumeNotNow, Text: "a turn already runs"})

	// Assert
	w.expect(t, "a ready")
}

func TestABringUpParkedAtItsColdGateDoesNotWaitOnTheCutTurn(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.owe("a")
	w.start(none("a"))
	w.awaitBringUp(t)

	// Act
	w.c.Step("a", Step{Kind: StepColdGate})

	// Assert
	w.expect(t, "a cold_gate", "a served", "a conversation_read", "a ready")
}

func TestAnOfflineVendorDoesNotHoldTheOpenOnItsCutTurn(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.owe("a")
	w.start(none("a"))
	w.awaitBringUp(t)
	w.c.Step("a", Step{Kind: StepServing})
	w.expect(t, "a served", "a conversation_read")

	// Act
	w.c.Step("a", Step{Kind: StepOffline})

	// Assert
	w.expect(t, "a offline", "a ready")
}

func TestARetryingVendorDoesNotHoldAClaimedResumption(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.owe("a")
	w.start(none("a"))
	w.awaitBringUp(t)
	w.c.Step("a", Step{Kind: StepServing})
	w.expect(t, "a served", "a conversation_read")
	w.c.Resumption("a", Resume{Kind: ResumeStarting})
	w.expect(t, "a resuming_cut_turn")

	// Act
	w.c.Step("a", Step{Kind: StepVendorRetrying, Attempt: 1})

	// Assert
	w.expect(t, "a vendor_retrying 1", "a ready")
}

func TestAnOpenJoiningADeliveredResumptionWaitsForItsStart(t *testing.T) {
	// Arrange: the boot's bring-up is resuming the cut turn as the open joins.
	w := newWorld(t)
	w.c.Step("a", Step{Kind: StepStartingSession})
	w.c.Step("a", Step{Kind: StepServing})
	w.c.Resumption("a", Resume{Kind: ResumeStarting})
	w.start(none("a"))
	w.expect(t, "a served", "a resuming_cut_turn", "a conversation_read")

	// Act
	w.c.Resumption("a", Resume{Kind: ResumeStarted})

	// Assert
	w.expect(t, "a ready")
}

func TestAnAlreadyServingWorkspaceDoesNotWaitOnWhatItStillOwes(t *testing.T) {
	// Arrange: its session-up release already ran and left the debt.
	w := newWorld(t)
	w.setLive("a")
	w.owe("a")

	// Act
	w.start(none("a"))

	// Assert
	w.expect(t, "a served", "a conversation_read", "a ready")
}

func TestAnOwedReadThatFailsOpensFailed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.owedErr = errors.New("database is locked")

	// Act
	w.start(none("a"))

	// Assert
	w.expect(t, "a failed whether a turn the restart cut is owed could not be read: database is locked")
}

func TestAnOwedReadTheClientLeftIsNoFailure(t *testing.T) {
	// Arrange: the client goes away while the owed read is in flight.
	w := newWorld(t)
	w.leaveDuringOwed = true

	// Act
	_, returned := w.start(none("a"))
	awaitReturned(t, returned)

	// Assert: recorded at INFO with its cause, and no ERROR at all.
	for _, r := range w.log.Records() {
		if r.Level == dlog.LevelError {
			t.Fatalf("records = %+v, want no error for a client that left", w.log.Records())
		}
	}
	found := false
	for _, r := range w.log.Records() {
		if r.Level == dlog.LevelInfo && r.Context[dlog.KeyWorkspaceID] == "a" && r.Context["cause"] == context.Canceled.Error() &&
			strings.Contains(r.Message, "went away while whether the workspace is owed a cut turn was read") {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want the INFO naming the workspace and the cancellation", w.log.Records())
	}
}

func TestAHiddenWorkspacesCutTurnIsKeptAndPaidWhenItIsOpened(t *testing.T) {
	// Arrange: hidden, its debt is not even read.
	w := newWorld(t)
	w.owe("a")
	hidden := none("a")
	hidden.Hidden = true
	_, returned := w.start(hidden)
	w.expect(t, "a hidden", "finished 0/1 failed=")
	awaitReturned(t, returned)
	if got := w.owedReadCount(); got != 0 {
		t.Fatalf("owed reads = %d, want a hidden workspace's debt left alone", got)
	}

	// Act: shown and opened.
	w.start(none("a"))
	w.awaitBringUp(t)
	w.c.Step("a", Step{Kind: StepServing})
	w.expect(t, "a served", "a conversation_read")
	w.c.Resumption("a", Resume{Kind: ResumeStarting})
	w.c.Resumption("a", Resume{Kind: ResumeStarted})

	// Assert
	w.expect(t, "a resuming_cut_turn", "a ready")
}

// ---- the roster row ----

func TestReadyCarriesTheWorkspacesRosterRow(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.setLive("a")
	w.start(none("a"))
	w.expect(t, "a served", "a conversation_read")

	// Act
	opened := w.nextOpened(t)

	// Assert
	if got := opened.GetReady().GetRow().GetWorkspace().GetWorkspace().GetId(); got != "a" {
		t.Fatalf("ready row workspace = %q, want a", got)
	}
}

func TestFailedCarriesTheWorkspacesRosterRow(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"))
	call := w.awaitBringUp(t)

	// Act
	call.done("a", errors.New("boom"))
	opened := w.nextOpened(t)

	// Assert
	if got := opened.GetFailed().GetRow().GetWorkspace().GetWorkspace().GetId(); got != "a" {
		t.Fatalf("failed row workspace = %q, want a", got)
	}
}

func TestTheAnswersRevisionIsThatOfTheRosterItsRowCameFrom(t *testing.T) {
	// Arrange: the fake roster names its row by the revision it resolved at.
	w := newWorld(t)
	w.setLive("a")
	w.start(none("a"))
	w.expect(t, "a served", "a conversation_read")

	// Act
	ready := w.nextOpened(t).GetReady()

	// Assert
	if want := fmt.Sprintf("a@%d", ready.GetRosterRevision()); ready.GetRow().GetName().GetText() != want {
		t.Fatalf("row %q with revision %d, want the row of that revision", ready.GetRow().GetName().GetText(), ready.GetRosterRevision())
	}
}

func TestTheAnswersRowIsTakenAfterTheOpeningIsLowered(t *testing.T) {
	// Arrange: raising the opening is revision 1, lowering it revision 2.
	w := newWorld(t)
	w.setLive("a")
	w.start(none("a"))
	w.expect(t, "a served", "a conversation_read")

	// Act
	ready := w.nextOpened(t).GetReady()

	// Assert
	if ready.GetRosterRevision() != 2 {
		t.Fatalf("revision = %d, want the roster that no longer marks the workspace opening", ready.GetRosterRevision())
	}
}

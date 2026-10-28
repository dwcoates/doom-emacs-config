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

// world is a coordinator over fake collaborators.
type world struct {
	c      *Coordinator
	log    *dlog.TestLogger
	frames chan *agentreplv1.OpenTabsResponse
	// readBegun is told the workspace of every conversation read as it
	// begins.
	readBegun chan ids.WorkspaceID

	mu      sync.Mutex
	opening []string
	// readErr fails every conversation read; reads counts them; readGates,
	// when one is set for a workspace, holds that workspace's reads until
	// the test closes it.
	readErr   error
	reads     map[ids.WorkspaceID]int
	readGates map[ids.WorkspaceID]chan struct{}
	// revision is the fake roster's revision, raised by every opening edge.
	revision uint64
	// threads are the workspaces the fake roster lists as chat threads, with
	// no row.
	threads map[ids.WorkspaceID]bool
}

func newWorld(t *testing.T) *world {
	t.Helper()
	w := &world{
		log:       dlog.NewTestLogger(),
		reads:     map[ids.WorkspaceID]int{},
		readGates: map[ids.WorkspaceID]chan struct{}{},
		frames:    make(chan *agentreplv1.OpenTabsResponse, 256),
		readBegun: make(chan ids.WorkspaceID, 64),
	}
	c, err := New(Deps{
		Opening: func(ws ids.WorkspaceID, opening bool) {
			w.mu.Lock()
			defer w.mu.Unlock()
			w.opening = append(w.opening, fmt.Sprintf("%s:%v", ws, opening))
			w.revision++
		},
		ReadConversation: func(ctx context.Context, ws ids.WorkspaceID) error {
			w.mu.Lock()
			gate := w.readGates[ws]
			w.reads[ws]++
			err := w.readErr
			w.mu.Unlock()
			w.readBegun <- ws
			if gate != nil {
				select {
				case <-gate:
				case <-ctx.Done():
					return ctx.Err()
				}
			}
			return err
		},
		Row: func(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool) {
			w.mu.Lock()
			defer w.mu.Unlock()
			if w.threads[ws] {
				return nil, 0, false
			}
			return &frontendv1.RosterRow{
				Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: string(ws)}},
				Name:      &frontendv1.RosterRowName{Text: fmt.Sprintf("%s@%d", ws, w.revision)},
			}, w.revision, true
		},
		Thread: func(ws ids.WorkspaceID) (uint64, bool) {
			w.mu.Lock()
			defer w.mu.Unlock()
			return w.revision, w.threads[ws]
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

func (w *world) openingEdges() []string {
	w.mu.Lock()
	defer w.mu.Unlock()
	return append([]string(nil), w.opening...)
}

// gateRead makes WS's conversation reads wait until the answer is closed.
func (w *world) gateRead(ws ids.WorkspaceID) chan struct{} {
	w.mu.Lock()
	defer w.mu.Unlock()
	gate := make(chan struct{})
	w.readGates[ws] = gate
	return gate
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

// awaitRead waits until WS's conversation read has begun.
func (w *world) awaitRead(t *testing.T, ws ids.WorkspaceID) {
	t.Helper()
	w.awaitReads(t, ws, 1)
}

// awaitReads waits until N of WS's conversation reads have begun.
func (w *world) awaitReads(t *testing.T, ws ids.WorkspaceID, n int) {
	t.Helper()
	for seen := 0; seen < n; {
		select {
		case got := <-w.readBegun:
			if got == ws {
				seen++
			}
		case <-time.After(waitBound):
			t.Fatalf("%d of %s's conversation reads began, want %d", seen, ws, n)
		}
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
			Opening:          func(ids.WorkspaceID, bool) {},
			ReadConversation: func(context.Context, ids.WorkspaceID) error { return nil },
			Row:              func(ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool) { return nil, 0, false },
			Thread:           func(ids.WorkspaceID) (uint64, bool) { return 0, false },
			Now:              time.Now,
			Log:              dlog.NewTestLogger(),
		}
	}
	cases := []struct {
		name  string
		strip func(*Deps)
	}{
		{"opening marker", func(d *Deps) { d.Opening = nil }},
		{"conversation read", func(d *Deps) { d.ReadConversation = nil }},
		{"roster row", func(d *Deps) { d.Row = nil }},
		{"roster thread listing", func(d *Deps) { d.Thread = nil }},
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
	if len(w.openingEdges()) != 0 {
		t.Fatalf("a refused workspace was marked opening: %v", w.openingEdges())
	}
}

func TestAHiddenWorkspaceIsAnsweredHiddenAndNothingIsRead(t *testing.T) {
	// Arrange
	w := newWorld(t)
	ws := loading("a")
	ws.Hidden = true

	// Act
	_, returned := w.start(ws)

	// Assert
	w.expect(t, "a hidden", "finished 0/1 failed=")
	awaitReturned(t, returned)
	if len(w.openingEdges()) != 0 || w.reads["a"] != 0 {
		t.Fatalf("a hidden workspace was marked opening (%v) or read (%d)", w.openingEdges(), w.reads["a"])
	}
}

// TestAWorkspaceIsReadyOnceItsConversationIsRead pins that an open waits on no
// shim: nothing but the store read stands between the open and its answer.
func TestAWorkspaceIsReadyOnceItsConversationIsRead(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	_, returned := w.start(none("a"))

	// Assert
	w.expect(t, "a conversation_read", "a ready", "finished 1/1 failed=")
	awaitReturned(t, returned)
}

func TestAWorkspaceWithAPageLoadingWaitsForItToDraw(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.c.PageAttached("p")
	cancel, _ := w.start(loading("a"))
	defer cancel()
	w.expect(t, "a conversation_read")

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
	w.c.PageAttached("p")
	w.c.PageServes("p", "a")
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}

	// Act
	w.start(loading("a"))

	// Assert
	w.expect(t, "a page_connected", "a page_drawn", "a conversation_read", "a ready", "finished 1/1 failed=")
}

func TestAPageThatDrewWithoutServingIsConnectedFirst(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.c.PageAttached("p")
	cancel, _ := w.start(loading("a"))
	defer cancel()
	w.expect(t, "a conversation_read")

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

	// Act
	w.start(none("a"))

	// Assert
	w.expect(t, "a conversation_read", "a ready", "finished 1/1 failed=")
}

func TestADetachedPagesDrawnReportIsForgotten(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.c.PageAttached("p")
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}

	// Act
	w.c.PageDetached("p")
	cancel, _ := w.start(loading("a"))
	defer cancel()

	// Assert: read, and then nothing until a page draws again.
	w.expect(t, "a conversation_read")
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

// ---- a bring-up in flight is relayed, never waited for ----

func TestEveryRelayedStepIsTold(t *testing.T) {
	cases := []struct {
		step Step
		want string
	}{
		{Step{Kind: StepStartingSession}, "a starting_session"},
		{Step{Kind: StepWaking}, "a waking"},
		{Step{Kind: StepResuming}, "a resuming"},
		{Step{Kind: StepVendorRetrying, Attempt: 2}, "a vendor_retrying 2"},
		{Step{Kind: StepVendorRejected, Text: "logged out"}, "a vendor_rejected logged out"},
		{Step{Kind: StepVendorFailed}, "a vendor_failed"},
		{Step{Kind: StepColdGate}, "a cold_gate"},
		{Step{Kind: StepOffline}, "a offline"},
	}
	for _, tc := range cases {
		t.Run(tc.step.Kind.String(), func(t *testing.T) {
			// Arrange: the read is held, so the open is still answering.
			w := newWorld(t)
			w.gateRead("a")
			cancel, _ := w.start(loading("a"))
			defer cancel()
			w.awaitRead(t, "a")

			// Act
			w.c.Step("a", tc.step)

			// Assert
			w.expect(t, tc.want)
		})
	}
}

func TestTheServingStepIsToldAsServedOnce(t *testing.T) {
	// Arrange
	w := newWorld(t)
	release := w.gateRead("a")
	w.start(none("a"))
	w.awaitRead(t, "a")

	// Act
	w.c.Step("a", Step{Kind: StepServing})
	w.c.Step("a", Step{Kind: StepUp})
	close(release)

	// Assert
	w.expect(t, "a served", "a conversation_read", "a ready")
}

func TestAFailedBringUpDoesNotFailTheOpen(t *testing.T) {
	// Arrange
	w := newWorld(t)
	release := w.gateRead("a")
	w.start(none("a"))
	w.awaitRead(t, "a")

	// Act
	w.c.Step("a", Step{Kind: StepFailed, Text: "the shim never answered"})
	close(release)

	// Assert
	w.expect(t, "a conversation_read", "a ready", "finished 1/1 failed=")
}

func TestAResumptionStartingIsRelayed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	release := w.gateRead("a")
	w.start(none("a"))
	w.awaitRead(t, "a")

	// Act
	w.c.Resumption("a", Resume{Kind: ResumeStarting})
	w.c.Resumption("a", Resume{Kind: ResumeStarting})
	close(release)

	// Assert: told once.
	w.expect(t, "a resuming_cut_turn", "a conversation_read", "a ready")
}

func TestAnOpenDoesNotWaitOnAResumptionUnderWay(t *testing.T) {
	// Arrange: a resumption is being delivered as the open begins.
	w := newWorld(t)
	w.c.Resumption("a", Resume{Kind: ResumeStarting})

	// Act
	w.start(none("a"))

	// Assert
	w.expect(t, "a conversation_read", "a ready", "finished 1/1 failed=")
}

func TestAStepOfAWorkspaceTheOpenDoesNotNameIsNotTold(t *testing.T) {
	// Arrange
	w := newWorld(t)
	release := w.gateRead("a")
	w.start(none("a"))
	w.awaitRead(t, "a")

	// Act
	w.c.Step("b", Step{Kind: StepStartingSession})
	close(release)

	// Assert
	w.expect(t, "a conversation_read", "a ready")
}

func TestNoWorkspaceWaitsOnAnEarlierOne(t *testing.T) {
	// Arrange: the first workspace's read is held.
	w := newWorld(t)
	w.gateRead("a")

	// Act
	cancel, _ := w.start(none("a"), none("b"))
	defer cancel()

	// Assert
	w.expect(t, "b conversation_read", "b ready")
}

func TestTheOpeningMarkerStandsUntilTheWorkspaceIsAnswered(t *testing.T) {
	// Arrange
	w := newWorld(t)
	release := w.gateRead("a")
	w.start(none("a"))
	w.awaitRead(t, "a")
	if got := w.openingEdges(); !slices.Equal(got, []string{"a:true"}) {
		t.Fatalf("opening edges before the answer = %v, want the raise", got)
	}

	// Act
	close(release)
	w.expect(t, "a conversation_read", "a ready")

	// Assert
	if got := w.openingEdges(); !slices.Equal(got, []string{"a:true", "a:false"}) {
		t.Fatalf("opening edges = %v, want raised then lowered", got)
	}
}

func TestTwoOpensOfOneWorkspaceLowerTheMarkerOnlyOnTheLast(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.gateRead("a")
	cancelFirst, firstReturned := w.start(none("a"))
	cancelSecond, _ := w.start(none("a"))
	defer cancelSecond()
	w.awaitReads(t, "a", 2)

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
	w.gateRead("a")
	cancel, returned := w.start(none("a"))
	w.awaitRead(t, "a")

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

	// Act
	_, returned := w.startEmitting(func(*agentreplv1.OpenTabsResponse) error { return errors.New("gone") }, loading("a"))

	// Assert
	awaitReturned(t, returned)
}

func TestTheOpenedFrameIsLoggedAtInfo(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	w.start(none("a"))
	w.expect(t, "a conversation_read", "a ready", "finished 1/1 failed=")

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

// ---- the conversation read ----

func TestReadyWaitsForTheConversationRead(t *testing.T) {
	// Arrange: the page drew before the open, so only the read is left.
	w := newWorld(t)
	release := w.gateRead("a")
	w.c.PageAttached("p")
	if err := w.c.PageDrawn("p", "a"); err != nil {
		t.Fatalf("PageDrawn: %v", err)
	}
	w.start(loading("a"))
	w.expect(t, "a page_connected", "a page_drawn")

	// Act
	close(release)

	// Assert
	w.expect(t, "a conversation_read", "a ready")
}

func TestAConversationReadThatFailsOpensFailed(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.readErr = errors.New("store unreachable")

	// Act
	w.start(none("a"))

	// Assert
	w.expect(t, "a failed the conversation could not be read: store unreachable", "finished 0/1 failed=a")
}

func TestTheConversationIsReadOncePerOpen(t *testing.T) {
	// Arrange
	w := newWorld(t)
	release := w.gateRead("a")
	w.start(none("a"))
	w.awaitRead(t, "a")

	// Act: a bring-up's steps arrive while the read runs.
	w.c.Step("a", Step{Kind: StepServing})
	w.c.Step("a", Step{Kind: StepUp})
	close(release)
	w.expect(t, "a served", "a conversation_read", "a ready")

	// Assert
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.reads["a"] != 1 {
		t.Fatalf("reads = %d, want one", w.reads["a"])
	}
}

// ---- the roster row ----

func TestReadyCarriesTheWorkspacesRosterRow(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.start(none("a"))
	w.expect(t, "a conversation_read")

	// Act
	opened := w.nextOpened(t)

	// Assert
	if got := opened.GetReady().GetRow().GetWorkspace().GetWorkspace().GetId(); got != "a" {
		t.Fatalf("ready row workspace = %q, want a", got)
	}
}

func TestAThreadIsAnsweredReadyAsAThread(t *testing.T) {
	// Arrange: the roster lists "t" in its thread section, with no row.
	w := newWorld(t)
	w.mu.Lock()
	w.threads = map[ids.WorkspaceID]bool{"t": true}
	w.mu.Unlock()
	w.start(none("t"))
	w.expect(t, "t conversation_read")

	// Act
	opened := w.nextOpened(t)

	// Assert
	if opened.GetReady().GetThread() == nil || opened.GetReady().GetRow() != nil {
		t.Fatalf("ready = %v, want the thread listing and no row", opened.GetReady())
	}
}

func TestFailedCarriesTheWorkspacesRosterRow(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.readErr = errors.New("boom")

	// Act
	w.start(none("a"))
	opened := w.nextOpened(t)

	// Assert
	if got := opened.GetFailed().GetRow().GetWorkspace().GetWorkspace().GetId(); got != "a" {
		t.Fatalf("failed row workspace = %q, want a", got)
	}
}

func TestTheAnswersRevisionIsThatOfTheRosterItsRowCameFrom(t *testing.T) {
	// Arrange: the fake roster names its row by the revision it resolved at.
	w := newWorld(t)
	w.start(none("a"))
	w.expect(t, "a conversation_read")

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
	w.start(none("a"))
	w.expect(t, "a conversation_read")

	// Act
	ready := w.nextOpened(t).GetReady()

	// Assert
	if ready.GetRosterRevision() != 2 {
		t.Fatalf("revision = %d, want the roster that no longer marks the workspace opening", ready.GetRosterRevision())
	}
}

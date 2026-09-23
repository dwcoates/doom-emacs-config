package server

import (
	"context"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/workspace"
)

// footerView is one complete footer view, distinguished by the clock's instant
// so a test can tell two publications apart.
func footerView(mark int64) *frontendv1.FooterView {
	return &frontendv1.FooterView{
		Strip: &frontendv1.FooterStrip{
			Clock: &frontendv1.FooterClock{TurnStartedAtMs: &mark},
		},
	}
}

// footerMark reads back the instant footerView stamped.
func footerMark(view *frontendv1.FooterView) int64 {
	return view.GetStrip().GetClock().GetTurnStartedAtMs()
}

// TestStandingStreamFlushesHeadersOnAccept pins the acceptance rule: a watch
// with NOTHING published yet is still an accepted watch, and the client learns
// that from the response headers before any frame.
func TestStandingStreamFlushesHeadersOnAccept(t *testing.T) {
	// Arrange: no view has ever been published for this workspace.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, dialErr := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	// A standing watch is ended by CANCELLING its context, never by Close:
	// ServerStreamForClient.Close drains the body and blocks forever on a
	// stream that never ends (ARCHITECTURE.md "Standing-stream mechanics").
	header := stream.ResponseHeader()

	// Assert: CallServerStream returns once headers arrive, so a non-empty
	// content type proves acceptance was observable before the first view.
	if header.Get("Content-Type") == "" {
		t.Fatal("the stream's response headers were not flushed on acceptance")
	}
}

// TestStandingStreamSendsLatestViewFirst pins the first half of the
// subscription invariant: the most recently published view arrives first.
func TestStandingStreamSendsLatestViewFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Footer.topic.Publish(footerView(11))
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, dialErr := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	if !stream.Receive() {
		t.Fatalf("receive the latest view: %v", stream.Err())
	}

	// Assert.
	if got := footerMark(stream.Msg().GetFooter()); got != 11 {
		t.Fatalf("first view = %d, want the latest published one", got)
	}
}

// TestStandingStreamSendsLaterViewsInOrder pins the second half: every later
// view follows in publication order, skipping none.
func TestStandingStreamSendsLaterViewsInOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, dialErr := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}

	// Act.
	h.Footer.topic.Publish(footerView(1))
	h.Footer.topic.Publish(footerView(2))
	var got []int64
	for range 2 {
		if !stream.Receive() {
			t.Fatalf("receive a view: %v", stream.Err())
		}
		got = append(got, footerMark(stream.Msg().GetFooter()))
	}

	// Assert.
	if got[0] != 1 || got[1] != 2 {
		t.Fatalf("views = %v, want [1 2] in publication order", got)
	}
}

// TestStandingStreamEndsOnClientCancel pins that a watch ends when the CLIENT
// cancels, which is the only way a standing stream ends.
func TestStandingStreamEndsOnClientCancel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Footer.topic.Publish(footerView(1))
	ctx, cancel := context.WithCancel(context.Background())
	stream, dialErr := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	if !stream.Receive() {
		t.Fatalf("receive the first view: %v", stream.Err())
	}

	// Act.
	cancel()

	// Assert.
	if stream.Receive() {
		t.Fatal("the stream kept delivering after the client cancelled")
	}
}

// TestStandingStreamRefusesUnownedWorkspace pins that a per-workspace stream
// refuses a workspace this daemon does not serve — before any frame.
func TestStandingStreamRefusesUnownedWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Ownership.standing = workspace.StandingTransferringAway
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, dialErr := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	stream.Receive()

	// Assert.
	if code := connectCode(t, stream.Err()); code != connect.CodeFailedPrecondition {
		t.Fatalf("code = %v, want FailedPrecondition", code)
	}
}

// TestWatchDaemonServesEveryClient pins that the daemon-level stream carries the
// stand-down announcement to whoever holds it, Emacs and webview alike.
func TestWatchDaemonServesEveryClient(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, dialErr := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}

	// Act.
	h.Server.ShutdownAnnounced(&agentreplv1.DaemonShutdownAnnounced{MintedAtMs: 7})
	if !stream.Receive() {
		t.Fatalf("receive the announcement: %v", stream.Err())
	}

	// Assert.
	if got := stream.Msg().GetShutdownAnnounced().GetMintedAtMs(); got != 7 {
		t.Fatalf("minted_at_ms = %d, want 7", got)
	}
}

// TestShutdownAnnouncedReachesAClientBeforeTheSurfaceCloses pins the ONE
// ordering the announcement exists for: every caller of ShutdownAnnounced ends
// the process immediately afterwards, so an announcement that were merely
// queued would be lost to the teardown and the client would see its stream end
// with no error and no reason at all.
func TestShutdownAnnouncedReachesAClientBeforeTheSurfaceCloses(t *testing.T) {
	// Arrange: a client holding the daemon stream, PROVEN subscribed by a push
	// it has already taken -- waiting on the open alone would race the
	// subscription the announcement has to reach.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, dialErr := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	h.Server.DrainScheduled(&agentreplv1.DaemonDrainScheduled{AtMs: 11})
	if !stream.Receive() {
		t.Fatalf("receive the schedule that proves the subscription: %v", stream.Err())
	}

	// Act: announce, then tear the surface down at once, exactly as the
	// orderly exit does.
	h.Server.ShutdownAnnounced(&agentreplv1.DaemonShutdownAnnounced{MintedAtMs: 7})
	if err := h.Server.Close(); err != nil {
		t.Fatalf("close the surface: %v", err)
	}

	// Assert.
	if !stream.Receive() {
		t.Fatalf("the stream ended before the announcement: %v", stream.Err())
	}
	if got := stream.Msg().GetShutdownAnnounced().GetMintedAtMs(); got != 7 {
		t.Fatalf("minted_at_ms = %d, want 7", got)
	}
}

// TestShutdownAnnouncedDoesNotWaitOnAStreamThatHasGone pins the other half of
// that wait: a client that has already left satisfies it at once, so a
// departed stream can never hold the orderly exit open for the flush bound.
func TestShutdownAnnouncedDoesNotWaitOnAStreamThatHasGone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	stream, dialErr := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	h.Server.DrainScheduled(&agentreplv1.DaemonDrainScheduled{AtMs: 11})
	if !stream.Receive() {
		t.Fatalf("receive the schedule that proves the subscription: %v", stream.Err())
	}
	cancel()

	// Act.
	done := make(chan struct{})
	go func() {
		defer close(done)
		h.Server.ShutdownAnnounced(&agentreplv1.DaemonShutdownAnnounced{MintedAtMs: 7})
	}()

	// Assert: it returns well inside the flush bound rather than riding it.
	select {
	case <-done:
	case <-time.After(announcementFlush / 2):
		t.Fatal("ShutdownAnnounced is still waiting on a stream whose client has gone")
	}
}

// TestHostStreamCountsItsParticipant pins that a held host stream is what the
// adoption rendezvous sees as an expected participant.
func TestHostStreamCountsItsParticipant(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, dialErr := h.Client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	// The relay publishes, which both proves the stream is live and
	// synchronizes the assertion against the handler's registration.
	h.Server.Relay().ReloadWebapp(testWorkspaceID)
	receiveHostEvent(t, stream)

	// Act.
	participants := h.Server.Participants(testWorkspaceID)

	// Assert.
	if !participants.Host {
		t.Fatal("the held host stream was not counted as a participant")
	}
}

// TestAHostStreamOpenStatesTheHopToTheResolvers pins that the host stream's
// open edge publishes the participant liveness the connectivity truth needs.
func TestAHostStreamOpenStatesTheHopToTheResolvers(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, dialErr := h.Client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	h.Server.Relay().ReloadWebapp(testWorkspaceID)
	receiveHostEvent(t, stream)

	// Act.
	edges := h.Footer.Participants()

	// Assert.
	if len(edges) == 0 || !edges[len(edges)-1].Host {
		t.Fatalf("footer participant edges = %+v, want the host hop stated live", edges)
	}
}

// TestAHostStreamOpenStatesTheHopToTheTopbar pins the same edge on the topbar,
// which draws the connectivity glyph from it.
func TestAHostStreamOpenStatesTheHopToTheTopbar(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, dialErr := h.Client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	h.Server.Relay().ReloadWebapp(testWorkspaceID)
	receiveHostEvent(t, stream)

	// Act.
	edges := h.Topbar.Participants()

	// Assert.
	if len(edges) == 0 || !edges[len(edges)-1].Host {
		t.Fatalf("topbar participant edges = %+v, want the host hop stated live", edges)
	}
}

// TestAHostStreamCloseStatesTheHopDown pins the CLOSE edge: a hop going down
// is published rather than waited for.
func TestAHostStreamCloseStatesTheHopDown(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	stream, dialErr := h.Client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	h.Server.Relay().ReloadWebapp(testWorkspaceID)
	receiveHostEvent(t, stream)

	h.Footer.AwaitEdge(t) // the open edge

	// Act: the client goes away, and the handler's deferred release runs.
	cancel()
	edge := h.Footer.AwaitEdge(t)

	// Assert.
	if edge.Host {
		t.Fatalf("footer participant edge = %+v, want the host hop stated down", edge)
	}
}

// TestTheLastHostStreamClosingReleasesTheEdit pins the held-prompt edit's
// scope: the editor's host stream going away tells the queue its editor is gone.
func TestTheLastHostStreamClosingReleasesTheEdit(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	stream, dialErr := h.Client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	h.Server.Relay().ReloadWebapp(testWorkspaceID)
	receiveHostEvent(t, stream)
	h.Footer.AwaitEdge(t) // the open edge

	// Act.
	cancel()
	h.Footer.AwaitEdge(t) // the close edge, stated after the release

	// Assert.
	if gone := h.Queue.editorsGone(); len(gone) != 1 || gone[0] != testWorkspaceID {
		t.Fatalf("editors gone = %v, want the test workspace once", gone)
	}
}

// TestAHostStreamOpeningReleasesNoEdit pins that only a close is a departure.
func TestAHostStreamOpeningReleasesNoEdit(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, dialErr := h.Client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	h.Server.Relay().ReloadWebapp(testWorkspaceID)
	receiveHostEvent(t, stream)
	h.Footer.AwaitEdge(t)

	// Assert.
	if gone := h.Queue.editorsGone(); len(gone) != 0 {
		t.Fatalf("editors gone = %v, want none on an open", gone)
	}
}

// TestWatchDaemonReplaysTheDrainBannerBesideNotInsteadOfAProgressEvent pins the
// state/event topic separation: after a drain schedule is armed and a
// mutation-progress event is pushed, a LATE subscriber must still replay the
// standing drain schedule. If progress rode the state topic, the schedule would
// be the event's casualty and the banner would be lost to whoever attached next.
func TestWatchDaemonReplaysTheDrainBannerBesideNotInsteadOfAProgressEvent(t *testing.T) {
	// Arrange: a standing drain schedule (state) and, after it, a
	// mutation-progress event (event).
	h := newHarness(t)
	s := h.Server.(*server)
	h.Server.DrainScheduled(&agentreplv1.DaemonDrainScheduled{AtMs: 42})
	s.MutationProgress(&agentreplv1.WorkspaceMutationProgress{
		OpId: "op-x",
		Event: &agentreplv1.WorkspaceMutationProgress_Create{
			Create: &agentreplv1.WorkspaceCreateProgress{
				Step: &agentreplv1.WorkspaceCreateProgress_Stage{
					Stage: agentreplv1.WorkspaceCreateStage_WORKSPACE_CREATE_STAGE_DERIVING_NAME,
				},
			},
		},
	})

	// Act: a late subscriber replays each topic's latest.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{}))
	if err != nil {
		t.Fatalf("open the stream: %v", err)
	}

	// Assert: among the replayed pushes, the drain schedule survived.
	sawDrain := false
	for i := 0; i < 2; i++ {
		if !stream.Receive() {
			break
		}
		if stream.Msg().GetDrainScheduled().GetAtMs() == 42 {
			sawDrain = true
		}
	}
	if !sawDrain {
		t.Fatal("a late subscriber lost the drain banner; the progress event replaced the standing state")
	}
}

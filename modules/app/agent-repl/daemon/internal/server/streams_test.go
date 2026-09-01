package server

import (
	"context"
	"testing"

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
	if !stream.Receive() {
		t.Fatalf("receive the push: %v", stream.Err())
	}

	// Act.
	participants := h.Server.Participants(testWorkspaceID)

	// Assert.
	if !participants.Host {
		t.Fatal("the held host stream was not counted as a participant")
	}
}

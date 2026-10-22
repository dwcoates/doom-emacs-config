package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
)

// composerOpen is an OpenFeedImage naming an image in the Emacs input window.
func composerOpen() *agentreplv1.OpenFeedImageRequest {
	return &agentreplv1.OpenFeedImageRequest{
		Workspace: ref(),
		Image: &agentreplv1.OpenFeedImageRequest_Composer{Composer: &agentreplv1.OpenFeedImageComposer{
			Image: &conversationv1.ImageBlock{
				Location:  &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: "/w/clip.png"}},
				MediaType: "image/png",
			},
		}},
	}
}

// drawnOpen is an OpenFeedImage echoing an image the webapp drew.
func drawnOpen(src string) *agentreplv1.OpenFeedImageRequest {
	return &agentreplv1.OpenFeedImageRequest{
		Workspace: ref(),
		Image: &agentreplv1.OpenFeedImageRequest_Drawn{Drawn: &agentreplv1.OpenFeedImageDrawn{
			Image: &frontendv1.FeedImageBlock{Src: src, Alt: "clip.png"},
		}},
	}
}

// openedImage is the image the workspace's overlay topic holds open, or nil.
func openedImage(t *testing.T, h *harness) *frontendv1.FeedImageBlock {
	t.Helper()
	latest, ok := h.Server.(*server).imageOverlayTopic(testWorkspaceID).Latest()
	if !ok {
		return nil
	}
	return latest.GetOpen().GetImage()
}

// TestOpenFeedImage covers both arms: each opens the image the overlay draws.
func TestOpenFeedImage(t *testing.T) {
	tests := []struct {
		name    string
		req     *agentreplv1.OpenFeedImageRequest
		wantSrc string
	}{
		{"a composer image opens as the feed resolves it", composerOpen(), "/feed-images/abc"},
		{"a drawn image opens as it was drawn", drawnOpen("/feed-images/drawn"), "/feed-images/drawn"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Images.resolved = &frontendv1.FeedImageBlock{Src: "/feed-images/abc", Alt: "clip.png"}

			// Act.
			resp, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("OpenFeedImage = %v, %v; want success", resp, err)
			}
			if got := openedImage(t, h).GetSrc(); got != tt.wantSrc {
				t.Errorf("the overlay holds %q open, want %q", got, tt.wantSrc)
			}
		})
	}
}

// TestOpenFeedImageRefusals covers each typed refusal, and that a refused
// open leaves the overlay as it was.
func TestOpenFeedImageRefusals(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(*harness)
		req     *agentreplv1.OpenFeedImageRequest
		want    func(*agentreplv1.OpenFeedImageError) bool
	}{
		{
			name:    "an unservable composer image",
			arrange: func(h *harness) { h.Images.composerErr = errors.New("feed: the image path is not absolute") },
			req:     composerOpen(),
			want: func(e *agentreplv1.OpenFeedImageError) bool {
				return strings.Contains(e.GetUnservable().GetDetail(), "not absolute")
			},
		},
		{
			name:    "a drawn image the feed would not draw",
			arrange: func(h *harness) { h.Images.drawnErr = errors.New("feed: not a source the feed serves") },
			req:     drawnOpen("file:///etc/passwd"),
			want: func(e *agentreplv1.OpenFeedImageError) bool {
				return e.GetNotDrawn().GetSrc() == "file:///etc/passwd"
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tt.arrange(h)

			// Act.
			resp, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if err != nil || !tt.want(resp.Msg.GetError()) {
				t.Fatalf("OpenFeedImage = %v, %v; want the typed refusal", resp, err)
			}
			if got := openedImage(t, h); got != nil {
				t.Errorf("a refused open left %v open, want the overlay untouched", got)
			}
		})
	}
}

// TestOpenFeedImageNamingNoImageIsInvalid covers the validation: an open with
// no image arm is InvalidArgument, never an arm.
func TestOpenFeedImageNamingNoImageIsInvalid(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(&agentreplv1.OpenFeedImageRequest{Workspace: ref()}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("OpenFeedImage with no image = %v, want InvalidArgument", err)
	}
}

// TestOpenFeedImageOnAnUnknownWorkspaceIsRefused covers the shared subject
// refusal: an id the registry does not hold is the unknown_workspace arm.
func TestOpenFeedImageOnAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := composerOpen()
	req.Workspace.Id = "ws-nobody"

	// Act.
	resp, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("OpenFeedImage = %v, %v; want unknown_workspace", resp, err)
	}
}

// TestOpenFeedImageIsRecordedAtInfo covers the record: an opened image is a
// visible daemon action in the workspace's log, naming where it came from.
func TestOpenFeedImageIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

	// Act.
	if _, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(drawnOpen("/feed-images/drawn"))); err != nil {
		t.Fatalf("OpenFeedImage: %v", err)
	}

	// Assert.
	found := false
	for _, r := range log.Records() {
		if r.Level == "info" && r.Operation == opFeedImage && r.Context["from"] == "drawn" {
			found = true
		}
	}
	if !found {
		t.Errorf("the open was not recorded at info naming its arm; records: %v", log.Records())
	}
}

// TestCloseFeedImageClosesTheOverlay covers the close: the topic holds the
// closed arm afterwards.
func TestCloseFeedImageClosesTheOverlay(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if _, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(drawnOpen("/feed-images/drawn"))); err != nil {
		t.Fatalf("OpenFeedImage: %v", err)
	}

	// Act.
	resp, err := h.Client.CloseFeedImage(context.Background(), connect.NewRequest(&agentreplv1.CloseFeedImageRequest{Workspace: ref()}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseFeedImage = %v, %v; want success", resp, err)
	}
	latest, _ := h.Server.(*server).imageOverlayTopic(testWorkspaceID).Latest()
	if latest.GetClosed() == nil {
		t.Errorf("the overlay is %v after a close, want closed", latest)
	}
}

// TestCloseFeedImageWithNothingOpenAnswersSuccess covers the closed-already
// case: the overlay is closed either way, so it is an answer.
func TestCloseFeedImageWithNothingOpenAnswersSuccess(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.CloseFeedImage(context.Background(), connect.NewRequest(&agentreplv1.CloseFeedImageRequest{Workspace: ref()}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseFeedImage = %v, %v; want success", resp, err)
	}
}

// TestNewRefusesMissingImageSources pins that the overlay's image sources are
// required: an open with nothing to resolve it would draw nothing.
func TestNewRefusesMissingImageSources(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.Images = nil })

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "image sources") {
		t.Fatalf("New = %v, want the refusal naming the image sources", err)
	}
}

// receiveOverlay reads the watch stream until an image-overlay frame arrives.
func receiveOverlay(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchFeedResponse],
) *frontendv1.FeedImageOverlay {
	t.Helper()
	return receivePush(t, stream, "an image overlay", (*agentreplv1.WatchFeedResponse).GetImageOverlay)
}

// TestWatchFeedPushesTheOpenedImageOnTheRootFeed pins that an open reaches the
// root feed's watch as a FeedImageOverlay frame holding the image open.
func TestWatchFeedPushesTheOpenedImageOnTheRootFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)

	// Act.
	if _, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(drawnOpen("/feed-images/drawn"))); err != nil {
		t.Fatalf("OpenFeedImage: %v", err)
	}
	overlay := receiveOverlay(t, stream)

	// Assert.
	if got := overlay.GetOpen().GetImage().GetSrc(); got != "/feed-images/drawn" {
		t.Fatalf("overlay = %v, want the drawn image open", overlay)
	}
}

// TestWatchFeedTellsALateRootWatchWhatIsOpen pins the replay: a watch opened
// after the image was opened is told at once, so a page that opens late draws
// the overlay.
func TestWatchFeedTellsALateRootWatchWhatIsOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if _, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(drawnOpen("/feed-images/drawn"))); err != nil {
		t.Fatalf("OpenFeedImage: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	overlay := receiveOverlay(t, openRootWatch(t, h, ctx))

	// Assert.
	if got := overlay.GetOpen().GetImage().GetSrc(); got != "/feed-images/drawn" {
		t.Fatalf("overlay = %v, want the image opened before the watch", overlay)
	}
}

// TestWatchFeedPushesTheClosedOverlay pins that a close reaches the watch as
// the closed arm, which is what takes the overlay down.
func TestWatchFeedPushesTheClosedOverlay(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.OpenFeedImage(context.Background(), connect.NewRequest(drawnOpen("/feed-images/drawn"))); err != nil {
		t.Fatalf("OpenFeedImage: %v", err)
	}
	receiveOverlay(t, stream)

	// Act.
	if _, err := h.Client.CloseFeedImage(context.Background(), connect.NewRequest(&agentreplv1.CloseFeedImageRequest{Workspace: ref()})); err != nil {
		t.Fatalf("CloseFeedImage: %v", err)
	}
	overlay := receiveOverlay(t, stream)

	// Assert.
	if overlay.GetClosed() == nil {
		t.Fatalf("overlay = %v, want closed", overlay)
	}
}

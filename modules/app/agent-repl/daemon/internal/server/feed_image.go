package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
)

const opFeedImage = "daemon.server.feed_image"

// OpenFeedImage opens one image large over the workspace's feed. See
// endpoint_open_feed_image.proto.
//
// THE DAEMON IS THE ONLY HOLDER OF THE OVERLAY. Emacs and the webapp each name
// the image in the form they hold -- the composer's reference, or a drawn
// image echoed back -- and the daemon resolves it to what the overlay draws
// (feed.ImageSources), so either click opens the same overlay, pushed on the
// workspace's root feed watch.
func (s *server) OpenFeedImage(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenFeedImageRequest],
) (*connect.Response[agentreplv1.OpenFeedImageResponse], error) {
	const rpc = "OpenFeedImage"
	if err := validateOpenFeedImageRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.OpenFeedImageResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	var (
		image *frontendv1.FeedImageBlock
		from  string
	)
	switch arm := req.Msg.GetImage().(type) {
	case *agentreplv1.OpenFeedImageRequest_Composer:
		from = "composer"
		resolved, err := s.deps.Images.Composer(arm.Composer.GetImage())
		if err != nil {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refusal{
				Arm:    "unservable",
				Reason: err.Error(),
				Fields: map[string]any{"detail": err.Error()},
			}))
		}
		image = resolved
	case *agentreplv1.OpenFeedImageRequest_Drawn:
		from = "drawn"
		if err := s.deps.Images.Drawn(arm.Drawn.GetImage()); err != nil {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refusal{
				Arm:    "not_drawn",
				Reason: err.Error(),
				Fields: map[string]any{"src": arm.Drawn.GetImage().GetSrc()},
			}))
		}
		image = arm.Drawn.GetImage()
	}
	s.imageOverlayTopic(subject.Record.ID).Publish(&frontendv1.FeedImageOverlay{
		State: &frontendv1.FeedImageOverlay_Open{Open: &frontendv1.FeedImageOverlayOpen{Image: image}},
	})
	subject.Log.Info(opFeedImage, "an image was opened over the feed", dlog.Context{
		"from": from, "src": image.GetSrc(), "alt": image.GetAlt(),
	})
	resp.Result = &agentreplv1.OpenFeedImageResponse_Success{Success: &agentreplv1.OpenFeedImageSuccess{}}
	return connect.NewResponse(resp), nil
}

// CloseFeedImage closes the image open over the workspace's feed. See
// endpoint_close_feed_image.proto. Closing a closed overlay answers success:
// the overlay is closed either way.
func (s *server) CloseFeedImage(
	ctx context.Context,
	req *connect.Request[agentreplv1.CloseFeedImageRequest],
) (*connect.Response[agentreplv1.CloseFeedImageResponse], error) {
	const rpc = "CloseFeedImage"
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.CloseFeedImageResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	s.imageOverlayTopic(subject.Record.ID).Publish(closedImageOverlay())
	subject.Log.Info(opFeedImage, "the image over the feed was closed", nil)
	resp.Result = &agentreplv1.CloseFeedImageResponse_Success{Success: &agentreplv1.CloseFeedImageSuccess{}}
	return connect.NewResponse(resp), nil
}

// closedImageOverlay is the overlay with no image open.
func closedImageOverlay() *frontendv1.FeedImageOverlay {
	return &frontendv1.FeedImageOverlay{
		State: &frontendv1.FeedImageOverlay_Closed{Closed: &frontendv1.FeedImageOverlayClosed{}},
	}
}

// imageOverlayTopic answers a workspace's image overlay topic, creating it on
// first use. It replays its latest value, so a root feed watch accepted after
// an open is told what is open at once.
func (s *server) imageOverlayTopic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FeedImageOverlay] {
	return perWorkspace(s, s.imageOverlayTopics, ws)
}

// validateOpenFeedImageRequest refuses a request naming no workspace or no
// image as InvalidArgument.
func validateOpenFeedImageRequest(req *agentreplv1.OpenFeedImageRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	switch arm := req.GetImage().(type) {
	case *agentreplv1.OpenFeedImageRequest_Composer:
		if arm.Composer.GetImage() == nil {
			return invalid("image.composer.image", "the composer's image is required")
		}
		return nil
	case *agentreplv1.OpenFeedImageRequest_Drawn:
		if arm.Drawn.GetImage() == nil {
			return invalid("image.drawn.image", "the drawn image is required")
		}
		return nil
	}
	return invalid("image", "an image to open is required")
}

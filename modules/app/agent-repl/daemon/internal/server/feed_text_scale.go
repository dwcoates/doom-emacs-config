package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// THE FEED TEXT ZOOM lives here: the daemon owns the scale, applies a fixed
// small step per AdjustFeedTextScale, clamps it to a sane range, persists it so
// the zoom survives a restart, and pushes it to every open feed's watch. See
// endpoint_adjust_feed_text_scale.proto.

const (
	// feedTextScaleStep is how much one AdjustFeedTextScale nudge moves the
	// scale. It is DELIBERATELY SMALL so the user can zoom the feed text in
	// fine increments by holding the key down (Emacs auto-repeat re-invokes the
	// command); 2% per press means many presses cross the range, which is the
	// point.
	feedTextScaleStep = 0.02
	// feedTextScaleMin and feedTextScaleMax clamp the scale to a legible range:
	// half size to triple size. A nudge past either end is not an error — it
	// answers the clamped value and pushes nothing new.
	feedTextScaleMin = 0.5
	feedTextScaleMax = 3.0
)

// AdjustFeedTextScale nudges the single daemon-global feed text zoom one small
// step and answers the scale now in force. The scale is GLOBAL — one value for
// every feed — so the request carries no workspace ref and the response has no
// refusal arms: an already-clamped nudge simply answers the current value, and
// an UNSPECIFIED direction is a Connect InvalidArgument (validated below), not
// an arm. On a change the daemon persists the new value and pushes it on every
// open feed's watch (FeedTextScale) so the webapp re-scales its feed text.
func (s *server) AdjustFeedTextScale(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdjustFeedTextScaleRequest],
) (*connect.Response[agentreplv1.AdjustFeedTextScaleResponse], error) {
	const rpc = "AdjustFeedTextScale"
	if err := validateAdjustFeedTextScaleRequest(req.Msg); err != nil {
		return nil, err
	}
	scale, err := s.applyFeedTextScaleStep(ctx, req.Msg.GetDirection())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	return connect.NewResponse(&agentreplv1.AdjustFeedTextScaleResponse{Scale: scale}), nil
}

// applyFeedTextScaleStep computes the clamped next scale for one direction and,
// when it actually changes, PERSISTS it and PUSHES it before returning. It
// holds feedTextScaleMu across the whole read-compute-persist-publish so two
// concurrent nudges (a held key and a second client) never race the value, and
// it uses the DEDICATED mutex rather than the server's main mu so a feed watch
// or a host push is never blocked on the state store. A persistence failure
// aborts with NO in-memory mutation and NO push, so the stored value and the
// pushed value never disagree.
func (s *server) applyFeedTextScaleStep(
	ctx context.Context,
	direction agentreplv1.AdjustFeedTextScaleDirection,
) (float64, error) {
	s.feedTextScaleMu.Lock()
	defer s.feedTextScaleMu.Unlock()

	current := s.feedTextScale
	next := clampFeedTextScale(nextFeedTextScale(current, direction))
	if next == current {
		return current, nil
	}
	if err := s.deps.DB.PutFeedTextScale(ctx, next); err != nil {
		return 0, err
	}
	s.feedTextScale = next
	s.feedTextScaleTopic.Publish(&frontendv1.FeedTextScale{Scale: next})
	s.log.Debug("daemon.server.adjust_feed_text_scale", "moved the feed text scale",
		dlog.Context{"direction": direction.String(), "scale": next})
	return next, nil
}

// nextFeedTextScale is the pure step arithmetic, factored out so the clamp and
// the direction rules are tested directly. It never touches server state and
// never clamps — clampFeedTextScale does that so a value already at a bound
// stays put rather than drifting past it.
func nextFeedTextScale(current float64, direction agentreplv1.AdjustFeedTextScaleDirection) float64 {
	switch direction {
	case agentreplv1.AdjustFeedTextScaleDirection_ADJUST_FEED_TEXT_SCALE_DIRECTION_INCREASE:
		return current + feedTextScaleStep
	case agentreplv1.AdjustFeedTextScaleDirection_ADJUST_FEED_TEXT_SCALE_DIRECTION_DECREASE:
		return current - feedTextScaleStep
	default:
		return current
	}
}

// clampFeedTextScale holds a scale inside [feedTextScaleMin, feedTextScaleMax].
func clampFeedTextScale(scale float64) float64 {
	if scale < feedTextScaleMin {
		return feedTextScaleMin
	}
	if scale > feedTextScaleMax {
		return feedTextScaleMax
	}
	return scale
}

// SeedFeedTextScale sets the in-memory scale and publishes it onto the feed
// watch topic. Prime calls it at boot with the persisted value so a feed opened
// before the first nudge is handed the zoom the user last chose (a topic
// replays its latest value to a late subscriber). It is idempotent and safe to
// call before anything is served.
func (s *server) SeedFeedTextScale(scale float64) {
	s.feedTextScaleMu.Lock()
	defer s.feedTextScaleMu.Unlock()
	s.feedTextScale = scale
	s.feedTextScaleTopic.Publish(&frontendv1.FeedTextScale{Scale: scale})
	s.log.Debug("daemon.server.seed_feed_text_scale", "seeded the feed text scale from the store",
		dlog.Context{"scale": scale})
}

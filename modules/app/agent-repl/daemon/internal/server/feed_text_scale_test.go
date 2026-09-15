package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

const (
	scaleUp   = agentreplv1.AdjustFeedTextScaleDirection_ADJUST_FEED_TEXT_SCALE_DIRECTION_INCREASE
	scaleDown = agentreplv1.AdjustFeedTextScaleDirection_ADJUST_FEED_TEXT_SCALE_DIRECTION_DECREASE
)

// receiveScale reads the watch stream until a feed-text-scale frame arrives,
// skipping any row or selection frames.
func receiveScale(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchFeedResponse],
) *frontendv1.FeedTextScale {
	t.Helper()
	for stream.Receive() {
		if sc := stream.Msg().GetFeedTextScale(); sc != nil {
			return sc
		}
	}
	t.Fatalf("the watch stream ended before a feed-text-scale frame arrived: %v", stream.Err())
	return nil
}

// TestNextFeedTextScale pins the pure step arithmetic: increase adds the step,
// decrease subtracts it, and it never clamps (clampFeedTextScale does that).
func TestNextFeedTextScale(t *testing.T) {
	tests := []struct {
		name      string
		current   float64
		direction agentreplv1.AdjustFeedTextScaleDirection
		want      float64
	}{
		{name: "increase adds the step", current: 1.0, direction: scaleUp, want: 1.0 + feedTextScaleStep},
		{name: "decrease subtracts the step", current: 1.0, direction: scaleDown, want: 1.0 - feedTextScaleStep},
		{name: "increase past the max is not clamped here", current: feedTextScaleMax, direction: scaleUp, want: feedTextScaleMax + feedTextScaleStep},
		{name: "decrease past the min is not clamped here", current: feedTextScaleMin, direction: scaleDown, want: feedTextScaleMin - feedTextScaleStep},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := nextFeedTextScale(tc.current, tc.direction)

			// Assert.
			if got != tc.want {
				t.Fatalf("nextFeedTextScale = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestClampFeedTextScale pins the clamp: below the min snaps to the min, above
// the max snaps to the max, and a value inside the range is unchanged.
func TestClampFeedTextScale(t *testing.T) {
	tests := []struct {
		name  string
		scale float64
		want  float64
	}{
		{name: "below the min snaps to the min", scale: feedTextScaleMin - 1, want: feedTextScaleMin},
		{name: "above the max snaps to the max", scale: feedTextScaleMax + 1, want: feedTextScaleMax},
		{name: "inside the range is unchanged", scale: 1.5, want: 1.5},
		{name: "at the min is unchanged", scale: feedTextScaleMin, want: feedTextScaleMin},
		{name: "at the max is unchanged", scale: feedTextScaleMax, want: feedTextScaleMax},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := clampFeedTextScale(tc.scale)

			// Assert.
			if got != tc.want {
				t.Fatalf("clampFeedTextScale(%v) = %v, want %v", tc.scale, got, tc.want)
			}
		})
	}
}

// TestAdjustFeedTextScaleAppliesAndPersistsAStep pins that an increase from the
// default moves the scale up by one step, answers it, and persists it.
func TestAdjustFeedTextScaleAppliesAndPersistsAStep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	want := clampFeedTextScale(nextFeedTextScale(1.0, scaleUp))

	// Act.
	resp, err := h.Client.AdjustFeedTextScale(context.Background(),
		connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{Direction: scaleUp}))

	// Assert.
	if err != nil {
		t.Fatalf("AdjustFeedTextScale: %v", err)
	}
	if got := resp.Msg.GetScale(); got != want {
		t.Fatalf("scale = %v, want %v", got, want)
	}
	if len(h.DB.feedScalePut) != 1 || h.DB.feedScalePut[0] != want {
		t.Fatalf("persisted = %v, want one write of %v", h.DB.feedScalePut, want)
	}
}

// TestAdjustFeedTextScaleDecreaseAppliesAStep pins the mirror direction.
func TestAdjustFeedTextScaleDecreaseAppliesAStep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	want := clampFeedTextScale(nextFeedTextScale(1.0, scaleDown))

	// Act.
	resp, err := h.Client.AdjustFeedTextScale(context.Background(),
		connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{Direction: scaleDown}))

	// Assert.
	if err != nil {
		t.Fatalf("AdjustFeedTextScale: %v", err)
	}
	if got := resp.Msg.GetScale(); got != want {
		t.Fatalf("scale = %v, want %v", got, want)
	}
}

// TestAdjustFeedTextScaleClampsAtMax pins that a nudge up from the max answers
// the max and persists NOTHING new (the value did not change).
func TestAdjustFeedTextScaleClampsAtMax(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Server.SeedFeedTextScale(feedTextScaleMax)

	// Act.
	resp, err := h.Client.AdjustFeedTextScale(context.Background(),
		connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{Direction: scaleUp}))

	// Assert.
	if err != nil {
		t.Fatalf("AdjustFeedTextScale: %v", err)
	}
	if got := resp.Msg.GetScale(); got != feedTextScaleMax {
		t.Fatalf("scale = %v, want the max %v", got, feedTextScaleMax)
	}
	if len(h.DB.feedScalePut) != 0 {
		t.Fatalf("persisted = %v, want no write for an unchanged clamp", h.DB.feedScalePut)
	}
}

// TestAdjustFeedTextScaleClampsAtMin pins the min end of the clamp.
func TestAdjustFeedTextScaleClampsAtMin(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Server.SeedFeedTextScale(feedTextScaleMin)

	// Act.
	resp, err := h.Client.AdjustFeedTextScale(context.Background(),
		connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{Direction: scaleDown}))

	// Assert.
	if err != nil {
		t.Fatalf("AdjustFeedTextScale: %v", err)
	}
	if got := resp.Msg.GetScale(); got != feedTextScaleMin {
		t.Fatalf("scale = %v, want the min %v", got, feedTextScaleMin)
	}
	if len(h.DB.feedScalePut) != 0 {
		t.Fatalf("persisted = %v, want no write for an unchanged clamp", h.DB.feedScalePut)
	}
}

// TestAdjustFeedTextScalePushesOnTheFeedWatch pins that a change is pushed on
// the open feed's watch as a FeedTextScale frame carrying the new scale.
func TestAdjustFeedTextScalePushesOnTheFeedWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	want := clampFeedTextScale(nextFeedTextScale(1.0, scaleUp))

	// Act.
	if _, err := h.Client.AdjustFeedTextScale(context.Background(),
		connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{Direction: scaleUp})); err != nil {
		t.Fatalf("AdjustFeedTextScale: %v", err)
	}
	got := receiveScale(t, stream)

	// Assert.
	if got.GetScale() != want {
		t.Fatalf("pushed scale = %v, want %v", got.GetScale(), want)
	}
}

// TestAdjustFeedTextScaleRefusesAnUnspecifiedDirection pins that UNSPECIFIED is
// a validation failure (InvalidArgument), never an answer.
func TestAdjustFeedTextScaleRefusesAnUnspecifiedDirection(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.AdjustFeedTextScale(context.Background(),
		connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{
			Direction: agentreplv1.AdjustFeedTextScaleDirection_ADJUST_FEED_TEXT_SCALE_DIRECTION_UNSPECIFIED,
		}))

	// Assert.
	if connectCode(t, err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", connectCode(t, err))
	}
}

// TestAdjustFeedTextScalePersistFailureAborts pins that a persistence failure
// surfaces to the caller and persists nothing — no partial state.
func TestAdjustFeedTextScalePersistFailureAborts(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.feedScalePutErr = errors.New("state store closed")

	// Act.
	_, err := h.Client.AdjustFeedTextScale(context.Background(),
		connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{Direction: scaleUp}))

	// Assert.
	if err == nil {
		t.Fatalf("AdjustFeedTextScale succeeded, want a surfaced persistence failure")
	}
	if len(h.DB.feedScalePut) != 0 {
		t.Fatalf("persisted = %v, want nothing after a failed write", h.DB.feedScalePut)
	}
}

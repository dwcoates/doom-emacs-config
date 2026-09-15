package wsm

import (
	"context"
	"math"
	"testing"
)

func TestPutFeedTextScaleRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	if err := s.PutFeedTextScale(context.Background(), 1.5); err != nil {
		t.Fatalf("PutFeedTextScale: %v", err)
	}
	got, err := s.FeedTextScale(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("FeedTextScale: %v", err)
	}
	if got != 1.5 {
		t.Fatalf("scale = %v, want 1.5", got)
	}
}

func TestFeedTextScaleDefaultsWhenUnset(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.FeedTextScale(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("FeedTextScale: %v", err)
	}
	if got != DefaultFeedTextScale {
		t.Fatalf("scale = %v, want the default %v", got, DefaultFeedTextScale)
	}
}

func TestPutFeedTextScaleReplacesTheOneInForce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.PutFeedTextScale(context.Background(), 1.5); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	if err := s.PutFeedTextScale(context.Background(), 2.0); err != nil {
		t.Fatalf("PutFeedTextScale: %v", err)
	}

	// Assert
	got, err := s.FeedTextScale(context.Background())
	if err != nil {
		t.Fatalf("FeedTextScale: %v", err)
	}
	if got != 2.0 {
		t.Fatalf("scale = %v, want the replacement 2.0", got)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM feed_text_scale`); n != 1 {
		t.Fatalf("%d rows are in force, want exactly 1", n)
	}
}

func TestPutFeedTextScaleRefusesANonPositiveScale(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.PutFeedTextScale(context.Background(), 0)

	// Assert
	if err == nil {
		t.Fatalf("PutFeedTextScale accepted a non-positive scale, want a refusal")
	}
}

func TestPutFeedTextScaleRefusesANonFiniteScale(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.PutFeedTextScale(context.Background(), math.Inf(1))

	// Assert
	if err == nil {
		t.Fatalf("PutFeedTextScale accepted a non-finite scale, want a refusal")
	}
}

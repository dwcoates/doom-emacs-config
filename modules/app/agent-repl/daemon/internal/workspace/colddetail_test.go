package workspace

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestColdGateCostGroupsDigits(t *testing.T) {
	tests := []struct {
		name   string
		tokens uint64
		want   string
	}{
		{"under a thousand", 999, "the conversation is cold at 999 context tokens"},
		{"thousands", 409051, "the conversation is cold at 409,051 context tokens"},
		{"millions", 1234567, "the conversation is cold at 1,234,567 context tokens"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cold := &conversationv1.SessionCold{ContextTokens: tt.tokens}

			// Act
			got := coldGateCost(cold, 2_000_000).Text()

			// Assert
			if got != tt.want {
				t.Fatalf("coldGateCost.Text = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestColdGateCostFigureIsTheGroupedCount(t *testing.T) {
	// Arrange
	cold := &conversationv1.SessionCold{ContextTokens: 409051}

	// Act
	got := coldGateCost(cold, 1_000_000)

	// Assert
	if got.Lead != "the conversation is cold at " || got.Figure != "409,051" || got.Tail != " context tokens" {
		t.Fatalf("coldGateCost parts = %q | %q | %q", got.Lead, got.Figure, got.Tail)
	}
}

func TestColdGateCostWindowFill(t *testing.T) {
	tests := []struct {
		name   string
		tokens uint64
		window int64
		want   float64
	}{
		{"against the vendor's window", 50_000, 200_000, 0.25},
		{"against the assumed million", 409_051, 1_000_000, 0.409051},
		{"an empty context", 0, 200_000, 0},
		{"past the window clamps to one", 300_000, 200_000, 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cold := &conversationv1.SessionCold{ContextTokens: tt.tokens}

			// Act
			got := coldGateCost(cold, tt.window).WindowFill

			// Assert
			if got != tt.want {
				t.Fatalf("window fill = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestColdGateCostTextIsExactlyItsParts(t *testing.T) {
	// Arrange
	cold := &conversationv1.SessionCold{ContextTokens: 1234567}

	// Act
	got := coldGateCost(cold, 1_000_000)

	// Assert
	if got.Text() != got.Lead+got.Figure+got.Tail {
		t.Fatalf("Text = %q, want %q", got.Text(), got.Lead+got.Figure+got.Tail)
	}
}

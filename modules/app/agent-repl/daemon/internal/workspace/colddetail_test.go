package workspace

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestColdGateDetailGroupsDigits(t *testing.T) {
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
			got := coldGateDetail(cold)

			// Assert
			if got != tt.want {
				t.Fatalf("coldGateDetail = %q, want %q", got, tt.want)
			}
		})
	}
}

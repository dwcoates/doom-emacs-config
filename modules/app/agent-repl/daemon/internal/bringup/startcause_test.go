package bringup

import (
	"testing"

	"agent-repld/internal/restarttiming"
	"agent-repld/internal/shimlife"
)

func TestStartCauseNamesTheBringUp(t *testing.T) {
	tests := []struct {
		name string
		via  restarttiming.StartVia
		want shimlife.StartCause
	}{
		{name: "a boot's", via: restarttiming.StartBoot, want: shimlife.StartBoot},
		{name: "a takeover's", via: restarttiming.StartTakeover, want: shimlife.StartTakeover},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := startCause(tt.via)

			// Assert
			if got != tt.want {
				t.Fatalf("startCause = %q, want %q", got, tt.want)
			}
		})
	}
}

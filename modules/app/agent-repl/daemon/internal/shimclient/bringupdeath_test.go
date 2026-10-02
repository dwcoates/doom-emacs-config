package shimclient

import (
	"errors"
	"testing"
)

func TestABringUpDeathIsAStandDownOnlyWhenTheSweepKilledIt(t *testing.T) {
	tests := []struct {
		name string
		attr *KillAttribution
		want bool
	}{
		{name: "the supervisor's stand-down sweep", attr: &KillAttribution{Actor: ActorStandDown}, want: true},
		{name: "another kill this daemon asked for", attr: &KillAttribution{Actor: "shimclient.bringup"}, want: false},
		{name: "a death nobody asked for", attr: nil, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			err := &BringUpDeathError{Exit: ExitInfo{PID: 7, Code: -1, Signal: "killed", Attribution: tt.attr}}

			// Act
			got := errors.Is(err, ErrStandingDown)

			// Assert
			if got != tt.want {
				t.Fatalf("errors.Is(death, ErrStandingDown) = %v, want %v", got, tt.want)
			}
		})
	}
}

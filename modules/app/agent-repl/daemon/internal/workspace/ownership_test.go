package workspace

import (
	"context"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
)

// fakeRolloutStanding is a rollout.Controller answering only the standing.
type fakeRolloutStanding struct {
	rollout.Controller
	standing rollout.Standing
}

func (f *fakeRolloutStanding) Standing(ids.WorkspaceID) rollout.Standing { return f.standing }

// TestOwnershipTranslatesEveryStanding covers the whole translation: the two
// packages keep separate vocabularies, so every arm must be carried across.
func TestOwnershipTranslatesEveryStanding(t *testing.T) {
	tests := []struct {
		name string
		from rollout.Standing
		want Standing
	}{
		{name: "owned", from: rollout.StandingOwned, want: StandingOwned},
		{name: "transferring away", from: rollout.StandingTransferringAway, want: StandingTransferringAway},
		{name: "not yet adopted", from: rollout.StandingNotYetAdopted, want: StandingNotYetAdopted},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			o := NewOwnership(&fakeRolloutStanding{standing: tc.from})

			// Act
			got, err := o.Standing(context.Background(), "ws-1")

			// Assert
			if err != nil {
				t.Fatalf("Standing = error %v, want %v", err, tc.want)
			}
			if got != tc.want {
				t.Fatalf("Standing = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestOwnershipRefusesAnUnrecognizedStanding covers the arm nobody has
// declared: a new rollout standing must fail loudly here rather than resolve
// to "owned" and let an rpc serve a workspace this daemon does not own.
func TestOwnershipRefusesAnUnrecognizedStanding(t *testing.T) {
	// Arrange
	o := NewOwnership(&fakeRolloutStanding{standing: rollout.Standing(99)})

	// Act
	_, err := o.Standing(context.Background(), "ws-1")

	// Assert
	if err == nil {
		t.Fatal("Standing accepted an unrecognized rollout standing, want a refusal")
	}
}

package merge

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/wsm"
)

func TestHoldRolloutsHoldsRolloutsThatAreNotHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.o.HoldRollouts(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("HoldRollouts: %v", err)
	}
	if hold, _ := h.db.MergeRolloutHold(context.Background()); !hold.Held {
		t.Fatal("the rollouts are not held after HoldRollouts")
	}
	if _, ok := h.recordFor("info", opHoldRollouts); !ok {
		t.Fatalf("no INFO %s record for the hold", opHoldRollouts)
	}
}

func TestReleaseRolloutsEndsAHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.rolloutHold = wsm.RolloutHold{Held: true, Since: time.Unix(1, 0)}

	// Act
	err := h.o.ReleaseRollouts(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ReleaseRollouts: %v", err)
	}
	if hold, _ := h.db.MergeRolloutHold(context.Background()); hold.Held {
		t.Fatal("the rollouts are still held after ReleaseRollouts")
	}
	if _, ok := h.recordFor("info", opReleaseRollouts); !ok {
		t.Fatalf("no INFO %s record for the release", opReleaseRollouts)
	}
}

func TestTheRolloutHoldRefusesAChangeThatChangesNothing(t *testing.T) {
	tests := []struct {
		name    string
		held    bool
		act     func(Orchestrator) error
		wantArm string
	}{
		{name: "a hold of held rollouts", held: true, act: func(o Orchestrator) error { return o.HoldRollouts(context.Background()) }, wantArm: ArmRolloutsAlreadyHeld},
		{name: "a release of rollouts that are not held", held: false, act: func(o Orchestrator) error { return o.ReleaseRollouts(context.Background()) }, wantArm: ArmRolloutsNotHeld},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tt.held {
				h.db.rolloutHold = wsm.RolloutHold{Held: true, Since: time.Unix(1, 0)}
			}

			// Act
			err := tt.act(h.o)

			// Assert
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
				t.Fatalf("error = %v, want the %s refusal", err, tt.wantArm)
			}
		})
	}
}

func TestTheRolloutHoldPassesAnUnreadableHoldThrough(t *testing.T) {
	tests := []struct {
		name string
		act  func(Orchestrator) error
	}{
		{name: "a hold", act: func(o Orchestrator) error { return o.HoldRollouts(context.Background()) }},
		{name: "a release", act: func(o Orchestrator) error { return o.ReleaseRollouts(context.Background()) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			want := errors.New("state database unreadable")
			h.db.rolloutHoldErr = want

			// Act
			err := tt.act(h.o)

			// Assert
			if !errors.Is(err, want) {
				t.Fatalf("error = %v, want the read's own failure", err)
			}
		})
	}
}

package sidebar

import (
	"testing"

	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/wsm"
)

// The roster's three turn-end arms ARE ladder.TurnEnd's names: the row's arm
// and the desktop banner read one table, so an arm spelled here that the
// ladder spells differently fails rather than drifting.
func TestTurnEndArmsAreTheLadderNames(t *testing.T) {
	cases := []struct {
		arm string
		end ladder.TurnEnd
	}{
		{arm: armDone, end: ladder.TurnEndDone},
		{arm: armInterrupted, end: ladder.TurnEndInterrupted},
		{arm: armTurnFailed, end: ladder.TurnEndFailed},
	}
	for _, tc := range cases {
		t.Run(tc.arm, func(t *testing.T) {
			// Act
			got := tc.end.String()

			// Assert
			if got != tc.arm {
				t.Fatalf("ladder names %q, the roster draws %q", got, tc.arm)
			}
		})
	}
}

func TestTurnEndArmReadsTheLadder(t *testing.T) {
	cases := []struct {
		name    string
		close   wsm.TurnClose
		failure ladder.FailureClass
		want    string
	}{
		{name: "a completion is done", close: wsm.CloseCompleted, failure: ladder.NoFailure, want: armDone},
		{name: "an interrupt is interrupted", close: wsm.CloseKilled, failure: ladder.NoFailure, want: armInterrupted},
		{name: "a failure is turn_failed", close: wsm.CloseFailed, failure: ladder.TurnFailed, want: armTurnFailed},
		{name: "an expected stop is done", close: wsm.CloseFailed, failure: ladder.ExpectedStop, want: armDone},
		{name: "an unknown close keeps done", close: wsm.TurnClose(99), failure: ladder.NoFailure, want: armDone},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s := &wsState{lastClose: tc.close, lastFailure: tc.failure}

			// Act
			got := s.turnEndArm()

			// Assert
			if got != tc.want {
				t.Fatalf("turnEndArm() = %q, want %q", got, tc.want)
			}
		})
	}
}

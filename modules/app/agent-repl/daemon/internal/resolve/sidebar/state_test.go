package sidebar

import (
	"testing"
	"time"

	"agent-repld/internal/resolve/ladder"
	"agent-repld/internal/wsm"
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
		{name: "a failure is turn_failed", close: wsm.CloseFailed, failure: ladder.VendorFailed, want: armTurnFailed},
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

func TestReadResultStampsTheReadInstant(t *testing.T) {
	// Arrange.
	s := newWSState()
	at := time.UnixMilli(1_700_000_000_000)

	// Act.
	s.readResult(at)

	// Assert.
	if s.result != resultRead || s.readAtNow() == nil || !s.readAtNow().Equal(at) {
		t.Fatalf("result = %v read at %v, want read at %v", s.result, s.readAtNow(), at)
	}
}

func TestSetResultClearsTheReadInstant(t *testing.T) {
	tests := []struct {
		name  string
		state resultState
	}{{"none", resultNone}, {"unread", resultUnread}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s := newWSState()
			s.readResult(time.UnixMilli(1_700_000_000_000))

			// Act.
			s.setResult(tt.state)

			// Assert.
			if s.result != tt.state || s.readAt != nil || s.readAtNow() != nil {
				t.Fatalf("result = %v read at %v, want %v with no read instant", s.result, s.readAt, tt.state)
			}
		})
	}
}

func TestSetResultRefusesARead(t *testing.T) {
	// Arrange.
	s := newWSState()
	defer func() {
		// Assert: a read without its instant is a caller defect.
		if recover() == nil {
			t.Fatal("setResult(resultRead) did not panic")
		}
	}()

	// Act.
	s.setResult(resultRead)
}

func TestTheResultSnapshotCarriesTheReadInstantOnlyWhenRead(t *testing.T) {
	// Arrange.
	s := newWSState()
	at := time.UnixMilli(1_700_000_000_000)
	s.readResult(at)

	// Act.
	read := s.resultSnapshot()
	s.setResult(resultUnread)
	unread := s.resultSnapshot()

	// Assert.
	if !read.Read || read.ReadAt == nil || !read.ReadAt.Equal(at) {
		t.Fatalf("read snapshot = %+v, want read at %v", read, at)
	}
	if unread.Read || unread.ReadAt != nil {
		t.Fatalf("unread snapshot = %+v, want no read instant", unread)
	}
}

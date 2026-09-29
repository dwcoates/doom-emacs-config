package ladder

import (
	"testing"

	"claude-repld/internal/wsm"
)

func TestResolveTurnEndReadsEveryCloseAndClass(t *testing.T) {
	cases := []struct {
		name  string
		how   wsm.TurnClose
		class FailureClass
		want  TurnEnd
	}{
		{name: "a completion is done", how: wsm.CloseCompleted, class: NoFailure, want: TurnEndDone},
		{name: "an interrupt is interrupted", how: wsm.CloseKilled, class: NoFailure, want: TurnEndInterrupted},
		{name: "a failed turn is turn_failed", how: wsm.CloseFailed, class: TurnFailed, want: TurnEndFailed},
		{name: "a vendor block is turn_failed", how: wsm.CloseFailed, class: VendorBlocked, want: TurnEndFailed},
		{name: "an orphaned turn is turn_failed", how: wsm.CloseOrphaned, class: NoFailure, want: TurnEndFailed},
		{name: "an agent death is turn_failed", how: wsm.CloseAgentDied, class: NoFailure, want: TurnEndFailed},
		{name: "an expected stop on a failed close is done", how: wsm.CloseFailed, class: ExpectedStop, want: TurnEndDone},
		{name: "an expected stop on an agent death is done", how: wsm.CloseAgentDied, class: ExpectedStop, want: TurnEndDone},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := ResolveTurnEnd(tc.how, tc.class)

			// Assert
			if !ok {
				t.Fatalf("ResolveTurnEnd(%d, %s) reported an unknown close", tc.how, tc.class)
			}
			if got != tc.want {
				t.Fatalf("ResolveTurnEnd(%d, %s) = %s, want %s", tc.how, tc.class, got, tc.want)
			}
		})
	}
}

func TestResolveTurnEndRefusesAnUnknownClose(t *testing.T) {
	// Act
	_, ok := ResolveTurnEnd(wsm.TurnClose(99), NoFailure)

	// Assert
	if ok {
		t.Fatal("ResolveTurnEnd reported a close this build does not know as known")
	}
}

func TestTurnEndNamesTheRosterArm(t *testing.T) {
	cases := []struct {
		end  TurnEnd
		want string
	}{
		{end: TurnEndDone, want: "done"},
		{end: TurnEndInterrupted, want: "interrupted"},
		{end: TurnEndFailed, want: "turn_failed"},
	}
	for _, tc := range cases {
		t.Run(tc.want, func(t *testing.T) {
			// Act
			got := tc.end.String()

			// Assert
			if got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

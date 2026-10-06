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
		{name: "a failed turn is turn_failed", how: wsm.CloseFailed, class: VendorFailed, want: TurnEndFailed},
		{name: "a vendor block is turn_failed", how: wsm.CloseFailed, class: VendorBlocked, want: TurnEndFailed},
		{name: "an orphaned turn is turn_failed", how: wsm.CloseOrphaned, class: NoFailure, want: TurnEndFailed},
		{name: "an agent death is turn_failed", how: wsm.CloseAgentDied, class: NoFailure, want: TurnEndFailed},
		{name: "an expected stop on a failed close is done", how: wsm.CloseFailed, class: ExpectedStop, want: TurnEndDone},
		{name: "an expected stop on an agent death is done", how: wsm.CloseAgentDied, class: ExpectedStop, want: TurnEndDone},
		{name: "a query death is turn_failed", how: wsm.CloseFailed, class: AgentReplFailed, want: TurnEndFailed},
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

func TestResolveTurnEndReadsNoEndForAFoldedPrompt(t *testing.T) {
	// Act
	_, ok := ResolveTurnEnd(wsm.CloseFolded, NoFailure)

	// Assert
	if ok {
		t.Fatal("a folded prompt's turn never ran, so it has no turn end to read")
	}
}

func TestResolveTurnFaultReadsEveryCloseAndClass(t *testing.T) {
	cases := []struct {
		name  string
		how   wsm.TurnClose
		class FailureClass
		want  TurnFault
	}{
		{name: "a completion raises no fault", how: wsm.CloseCompleted, class: NoFailure, want: NoTurnFault},
		{name: "an interrupt raises no fault", how: wsm.CloseKilled, class: NoFailure, want: NoTurnFault},
		{name: "a folded prompt raises no fault", how: wsm.CloseFolded, class: NoFailure, want: NoTurnFault},
		{name: "a vendor-failed turn is a vendor fault", how: wsm.CloseFailed, class: VendorFailed, want: VendorTurnFault},
		{name: "a vendor-blocked turn is a vendor fault", how: wsm.CloseFailed, class: VendorBlocked, want: VendorTurnFault},
		{name: "a failed close no terminal explained is a vendor fault", how: wsm.CloseFailed, class: NoFailure, want: VendorTurnFault},
		{name: "a query death is an agent-repl fault", how: wsm.CloseFailed, class: AgentReplFailed, want: AgentReplTurnFault},
		{name: "an orphaned turn is a vendor fault", how: wsm.CloseOrphaned, class: NoFailure, want: VendorTurnFault},
		{name: "an agent process death is an agent-repl fault", how: wsm.CloseAgentDied, class: NoFailure, want: AgentReplTurnFault},
		{name: "an expected stop on a failed close raises no fault", how: wsm.CloseFailed, class: ExpectedStop, want: NoTurnFault},
		{name: "an expected stop on an agent death raises no fault", how: wsm.CloseAgentDied, class: ExpectedStop, want: NoTurnFault},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := ResolveTurnFault(tc.how, tc.class)

			// Assert
			if !ok {
				t.Fatalf("ResolveTurnFault(%s, %s) reported an unknown close", tc.how, tc.class)
			}
			if got != tc.want {
				t.Fatalf("ResolveTurnFault(%s, %s) = %s, want %s", tc.how, tc.class, got, tc.want)
			}
		})
	}
}

func TestResolveTurnFaultRefusesAnUnknownClose(t *testing.T) {
	// Act
	_, ok := ResolveTurnFault(wsm.TurnClose(99), NoFailure)

	// Assert
	if ok {
		t.Fatal("ResolveTurnFault reported a close this build does not know as known")
	}
}

func TestTurnFaultNamesItsDomain(t *testing.T) {
	cases := []struct {
		fault TurnFault
		want  string
	}{
		{fault: NoTurnFault, want: "none"},
		{fault: VendorTurnFault, want: "vendor"},
		{fault: AgentReplTurnFault, want: "agent_repl"},
		{fault: TurnFault(99), want: "unknown"},
	}
	for _, tc := range cases {
		t.Run(tc.want, func(t *testing.T) {
			// Act
			got := tc.fault.String()

			// Assert
			if got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

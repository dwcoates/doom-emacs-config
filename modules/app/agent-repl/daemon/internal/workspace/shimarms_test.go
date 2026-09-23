package workspace

import (
	"errors"
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

func TestUpdateAgentArmNamesEveryLandedKind(t *testing.T) {
	tests := []struct {
		name    string
		failure *shimv1.UpdateAgentFailure
		want    string
	}{
		{
			name:    "unknown agent",
			failure: &shimv1.UpdateAgentFailure{Kind: &shimv1.UpdateAgentFailure_UnknownAgent{UnknownAgent: &shimv1.UpdateAgentUnknownAgent{}}},
			want:    ArmShimUnknownAgent,
		},
		{
			name:    "no open ask",
			failure: &shimv1.UpdateAgentFailure{Kind: &shimv1.UpdateAgentFailure_NoOpenAsk{NoOpenAsk: &shimv1.UpdateAgentNoOpenAsk{}}},
			want:    ArmShimNoOpenAsk,
		},
		{
			name:    "answer mismatch",
			failure: &shimv1.UpdateAgentFailure{Kind: &shimv1.UpdateAgentFailure_AnswerMismatch{AnswerMismatch: &shimv1.UpdateAgentAnswerMismatch{}}},
			want:    ArmShimAnswerMismatch,
		},
		{
			name:    "nothing running",
			failure: &shimv1.UpdateAgentFailure{Kind: &shimv1.UpdateAgentFailure_NothingRunning{NothingRunning: &shimv1.UpdateAgentNothingRunning{}}},
			want:    ArmShimNothingRunning,
		},
		{
			name:    "no session",
			failure: &shimv1.UpdateAgentFailure{Kind: &shimv1.UpdateAgentFailure_NoSession{NoSession: &shimv1.UpdateAgentNoSession{}}},
			want:    ArmShimNoSession,
		},
		{
			name:    "not deliverable",
			failure: &shimv1.UpdateAgentFailure{Kind: &shimv1.UpdateAgentFailure_NotDeliverable{NotDeliverable: &shimv1.UpdateAgentNotDeliverable{}}},
			want:    ArmShimNotDeliverable,
		},
		{
			name:    "agent busy",
			failure: &shimv1.UpdateAgentFailure{Kind: &shimv1.UpdateAgentFailure_AgentBusy{AgentBusy: &shimv1.UpdateAgentAgentBusy{}}},
			want:    ArmShimAgentBusy,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := updateAgentArm(tt.failure)
			// Assert.
			if got != tt.want {
				t.Fatalf("updateAgentArm() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestUpdateAgentArmOfAnUnsetKindIsUnspecified(t *testing.T) {
	// Arrange: an unset kind oneof is illegal on the wire; it is surfaced
	// rather than guessed at.
	// Act.
	got := updateAgentArm(&shimv1.UpdateAgentFailure{Detail: "something"})

	// Assert.
	if got != ArmShimUnspecified {
		t.Fatalf("updateAgentArm(unset kind) = %q, want %q", got, ArmShimUnspecified)
	}
}

func TestStopBashArmNamesEveryKind(t *testing.T) {
	tests := []struct {
		name    string
		failure *shimv1.StopBashFailure
		want    string
	}{
		{
			name:    "unknown work",
			failure: &shimv1.StopBashFailure{Kind: &shimv1.StopBashFailure_UnknownWork{UnknownWork: &shimv1.StopBashUnknownWork{}}},
			want:    ArmShimUnknownWork,
		},
		{
			name:    "already ended",
			failure: &shimv1.StopBashFailure{Kind: &shimv1.StopBashFailure_AlreadyEnded{AlreadyEnded: &shimv1.StopBashAlreadyEnded{}}},
			want:    ArmShimAlreadyEnded,
		},
		{name: "unset", failure: &shimv1.StopBashFailure{}, want: ArmShimUnspecified},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := stopBashArm(tt.failure)
			// Assert.
			if got != tt.want {
				t.Fatalf("stopBashArm() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestKillTurnArmNamesEveryCause(t *testing.T) {
	tests := []struct {
		name    string
		failure *shimv1.KillTurnFailure
		want    string
	}{
		{
			name:    "live",
			failure: &shimv1.KillTurnFailure{Cause: &shimv1.KillTurnFailure_Live{Live: &conversationv1.TurnLive{}}},
			want:    ArmShimTurnLive,
		},
		{
			name:    "not the open turn",
			failure: &shimv1.KillTurnFailure{Cause: &shimv1.KillTurnFailure_NotTheOpenTurn{NotTheOpenTurn: &shimv1.KillTurnNotTheOpenTurn{}}},
			want:    ArmShimNotTheOpenTurn,
		},
		{
			name:    "no turn open",
			failure: &shimv1.KillTurnFailure{Cause: &shimv1.KillTurnFailure_NoTurnOpen{NoTurnOpen: &shimv1.KillTurnNoTurnOpen{}}},
			want:    ArmShimNoTurnOpen,
		},
		{
			name:    "no session",
			failure: &shimv1.KillTurnFailure{Cause: &shimv1.KillTurnFailure_NoSession{NoSession: &shimv1.KillTurnNoSession{}}},
			want:    ArmShimNoSession,
		},
		{name: "unset", failure: &shimv1.KillTurnFailure{}, want: ArmShimUnspecified},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := killTurnArm(tt.failure)
			// Assert.
			if got != tt.want {
				t.Fatalf("killTurnArm() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestKillSessionArmNamesEveryCause(t *testing.T) {
	tests := []struct {
		name    string
		failure *shimv1.KillSessionFailure
		want    string
	}{
		{
			name:    "live",
			failure: &shimv1.KillSessionFailure{Cause: &shimv1.KillSessionFailure_Live{Live: &conversationv1.SessionLive{}}},
			want:    ArmShimTurnLive,
		},
		{
			name:    "no session",
			failure: &shimv1.KillSessionFailure{Cause: &shimv1.KillSessionFailure_NoSession{NoSession: &shimv1.KillSessionNoSession{}}},
			want:    ArmShimNoSession,
		},
		{
			name:    "query refused to end",
			failure: &shimv1.KillSessionFailure{Cause: &shimv1.KillSessionFailure_QueryRefusedToEnd{QueryRefusedToEnd: &shimv1.KillSessionQueryRefusedToEnd{}}},
			want:    ArmShimQueryRefusedToEnd,
		},
		{name: "unset", failure: &shimv1.KillSessionFailure{}, want: ArmShimUnspecified},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := killSessionArm(tt.failure)
			// Assert.
			if got != tt.want {
				t.Fatalf("killSessionArm() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestBenignMarksOnlyTheDomainOutcomes(t *testing.T) {
	tests := []struct {
		arm  string
		want bool
	}{
		{arm: ArmShimNothingRunning, want: true},
		{arm: ArmShimAlreadyEnded, want: true},
		{arm: ArmShimNoTurnOpen, want: true},
		{arm: ArmShimNotDeliverable, want: false},
		{arm: ArmShimUnknownAgent, want: false},
		{arm: ArmShimNoOpenAsk, want: false},
		{arm: ArmShimAnswerMismatch, want: false},
		{arm: ArmShimNoSession, want: false},
		{arm: ArmShimUnknownWork, want: false},
		{arm: ArmShimTurnLive, want: false},
		{arm: ArmShimNotTheOpenTurn, want: false},
		{arm: ArmShimQueryRefusedToEnd, want: false},
		{arm: ArmShimUnspecified, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.arm, func(t *testing.T) {
			// Arrange in the table. Act.
			got := (&ShimRefusal{Arm: tt.arm}).Benign()
			// Assert.
			if got != tt.want {
				t.Fatalf("Benign(%q) = %v, want %v", tt.arm, got, tt.want)
			}
		})
	}
}

func TestKillRefusedLiveMarksOnlyAKillTurnRefusedAsLive(t *testing.T) {
	tests := []struct {
		name    string
		refusal ShimRefusal
		want    bool
	}{
		{name: "a kill refused as live", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimTurnLive}, want: true},
		{name: "a kill refused as not the open turn", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimNotTheOpenTurn}, want: false},
		{name: "a kill refused with no session", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimNoSession}, want: false},
		{name: "a session kill refused as live", refusal: ShimRefusal{Verb: "KillSession", Arm: ArmShimTurnLive}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := tt.refusal.KillRefusedLive()
			// Assert.
			if got != tt.want {
				t.Fatalf("KillRefusedLive(%+v) = %v, want %v", tt.refusal, got, tt.want)
			}
		})
	}
}

func TestKillFoundNoTurnOpenMarksOnlyAKillTurnThatFoundNoTurn(t *testing.T) {
	tests := []struct {
		name    string
		refusal ShimRefusal
		want    bool
	}{
		{name: "a kill that found no turn open", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimNoTurnOpen}, want: true},
		{name: "a kill refused as live", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimTurnLive}, want: false},
		{name: "a kill refused as not the open turn", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimNotTheOpenTurn}, want: false},
		{name: "another verb's no_turn_open", refusal: ShimRefusal{Verb: "StartTurn", Arm: ArmShimNoTurnOpen}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := tt.refusal.KillFoundNoTurnOpen()
			// Assert.
			if got != tt.want {
				t.Fatalf("KillFoundNoTurnOpen(%+v) = %v, want %v", tt.refusal, got, tt.want)
			}
		})
	}
}

func TestShimRefusalCarriesTheShimsOwnWords(t *testing.T) {
	// Arrange: the evidence is the point.
	refusal := &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimNotDeliverable, Detail: "the SDK has no route to a subagent"}

	// Act.
	got := refusal.Error()

	// Assert.
	want := "shim UpdateAgent refused: not_deliverable: the SDK has no route to a subagent"
	if got != want {
		t.Fatalf("Error() = %q, want %q", got, want)
	}
}

func TestShimRefusalWithoutDetailStillNamesTheArm(t *testing.T) {
	// Arrange. Act.
	got := (&ShimRefusal{Verb: "StopBash", Arm: ArmShimUnknownWork}).Error()

	// Assert.
	want := "shim StopBash refused: unknown_work"
	if got != want {
		t.Fatalf("Error() = %q, want %q", got, want)
	}
}

func TestAsShimRefusalFindsAWrappedRefusal(t *testing.T) {
	// Arrange.
	wrapped := fmt.Errorf("interrupt: %w", &ShimRefusal{Arm: ArmShimNotDeliverable})

	// Act.
	refusal, ok := AsShimRefusal(wrapped)

	// Assert.
	if !ok || refusal.Arm != ArmShimNotDeliverable {
		t.Fatalf("AsShimRefusal() = (%v, %v), want the not_deliverable refusal", refusal, ok)
	}
}

func TestAsShimRefusalRejectsATransportFailure(t *testing.T) {
	// Arrange. Act.
	_, ok := AsShimRefusal(errors.New("connection reset"))

	// Assert.
	if ok {
		t.Fatal("AsShimRefusal(transport failure) reported a shim refusal")
	}
}

// TestGoneFromTheSweep pins which refusals a fan-wide sweep reads as "this item
// is no longer there to stop". It is deliberately WIDER than Benign: an
// addressed stop reports a stale row to the caller by name, but a sweep has no
// addressed row to report on.
func TestGoneFromTheSweep(t *testing.T) {
	tests := []struct {
		name string
		arm  string
		want bool
	}{
		{name: "nothing running", arm: ArmShimNothingRunning, want: true},
		{name: "already ended", arm: ArmShimAlreadyEnded, want: true},
		{name: "no turn open", arm: ArmShimNoTurnOpen, want: true},
		{name: "an agent the shim forgot", arm: ArmShimUnknownAgent, want: true},
		{name: "a shell the shim forgot", arm: ArmShimUnknownWork, want: true},
		{name: "no session", arm: ArmShimNoSession, want: false},
		{name: "not deliverable", arm: ArmShimNotDeliverable, want: false},
		{name: "agent busy", arm: ArmShimAgentBusy, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			refusal := &ShimRefusal{Verb: "UpdateAgent", Arm: tc.arm}

			// Act.
			got := refusal.GoneFromTheSweep()

			// Assert.
			if got != tc.want {
				t.Fatalf("GoneFromTheSweep(%q) = %v, want %v", tc.arm, got, tc.want)
			}
		})
	}
}

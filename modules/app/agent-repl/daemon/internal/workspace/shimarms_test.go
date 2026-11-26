package workspace

import (
	"context"
	"errors"
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/shimclient"
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

func TestKillRefusedNotTheOpenTurnMarksOnlyAKillTurnNamingAnotherTurn(t *testing.T) {
	tests := []struct {
		name    string
		refusal ShimRefusal
		want    bool
	}{
		{name: "a kill refused as not the open turn", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimNotTheOpenTurn}, want: true},
		{name: "a kill that found no turn open", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimNoTurnOpen}, want: false},
		{name: "a kill refused as live", refusal: ShimRefusal{Verb: "KillTurn", Arm: ArmShimTurnLive}, want: false},
		{name: "another verb's not_the_open_turn", refusal: ShimRefusal{Verb: "StartTurn", Arm: ArmShimNotTheOpenTurn}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := tt.refusal.KillRefusedNotTheOpenTurn()
			// Assert.
			if got != tt.want {
				t.Fatalf("KillRefusedNotTheOpenTurn(%+v) = %v, want %v", tt.refusal, got, tt.want)
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

func TestAStartTurnAskOpenRefusalMatchesTheQueuesAskSentinel(t *testing.T) {
	tests := []struct {
		name    string
		refusal *ShimRefusal
		want    bool
	}{
		{"StartTurn ask_open", &ShimRefusal{Verb: "StartTurn", Arm: ArmShimAskOpen}, true},
		{"StartTurn no_session", &ShimRefusal{Verb: "StartTurn", Arm: ArmShimNoSession}, false},
		{"UpdateAgent ask_open", &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimAskOpen}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := errors.Is(fmt.Errorf("join: %w", tt.refusal), promptqueue.ErrAskOpen)

			// Assert.
			if got != tt.want {
				t.Fatalf("errors.Is(..., ErrAskOpen) = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAStartTurnPromptRecordedRefusalMatchesTheQueuesRecordedSentinel(t *testing.T) {
	tests := []struct {
		name    string
		refusal *ShimRefusal
		want    bool
	}{
		{"StartTurn prompt_recorded", &ShimRefusal{Verb: "StartTurn", Arm: ArmShimPromptRecorded}, true},
		{"StartTurn ask_open", &ShimRefusal{Verb: "StartTurn", Arm: ArmShimAskOpen}, false},
		{"UpdateAgent prompt_recorded", &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimPromptRecorded}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := errors.Is(fmt.Errorf("start: %w", tt.refusal), promptqueue.ErrPromptRecorded)

			// Assert.
			if got != tt.want {
				t.Fatalf("errors.Is(..., ErrPromptRecorded) = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAStartTurnAskOpenRefusalIsNotTheReconnectSentinel(t *testing.T) {
	// Arrange
	refusal := &ShimRefusal{Verb: "StartTurn", Arm: ArmShimAskOpen}

	// Act
	got := errors.Is(refusal, promptqueue.ErrShimHasNoSession)

	// Assert
	if got {
		t.Fatal("an ask_open refusal must not hold the prompt for a reconnect")
	}
}

func TestAStartTurnNoSessionRefusalMatchesTheQueuesReconnectSentinel(t *testing.T) {
	tests := []struct {
		name    string
		refusal *ShimRefusal
		want    bool
	}{
		{"StartTurn no_session", &ShimRefusal{Verb: "StartTurn", Arm: ArmShimNoSession}, true},
		{"StartTurn query_dead", &ShimRefusal{Verb: "StartTurn", Arm: "query_dead"}, false},
		{"SetSessionModel no_session", &ShimRefusal{Verb: "SetSessionModel", Arm: ArmShimNoSession}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := errors.Is(fmt.Errorf("deliver: %w", tt.refusal), promptqueue.ErrShimHasNoSession)

			// Assert.
			if got != tt.want {
				t.Fatalf("errors.Is(..., ErrShimHasNoSession) = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRefusedNoSession(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{"a no_session refusal", &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimNoSession}, true},
		{"a wrapped no_session refusal", fmt.Errorf("start: %w", &ShimRefusal{Verb: "SetSessionEffort", Arm: ArmShimNoSession}), true},
		{"another arm", &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimVendorRefused}, false},
		{"no refusal", errors.New("transport"), false},
		{"no error", nil, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := refusedNoSession(tt.err)

			// Assert.
			if got != tt.want {
				t.Fatalf("refusedNoSession(%v) = %v, want %v", tt.err, got, tt.want)
			}
		})
	}
}

func TestShimRefusedAnswersARefusalFromAShimStandingDownAsTheStandDown(t *testing.T) {
	tests := []struct {
		name          string
		standingDown  bool
		wantStoodDown bool
	}{
		{name: "a shim this daemon is standing down", standingDown: true, wantStoodDown: true},
		{name: "a shim left serving", standingDown: false, wantStoodDown: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			client := &fakeSenderClient{standingDown: tt.standingDown}

			// Act.
			err := shimRefused(client, "SetSessionModel", "vendor_refused", "Query closed before response received")

			// Assert.
			refusal, ok := AsShimRefusal(err)
			if !ok || refusal.Verb != "SetSessionModel" || refusal.Arm != "vendor_refused" || refusal.Detail != "Query closed before response received" {
				t.Fatalf("err = %v, want the refusal kept whole in the chain", err)
			}
			if got := errors.Is(err, shimclient.ErrStandDownOrdered); got != tt.wantStoodDown {
				t.Fatalf("errors.Is(err, ErrStandDownOrdered) = %v, want %v", got, tt.wantStoodDown)
			}
		})
	}
}

// stoodDownFailureDetail is the shim's words for a call its session's end cut.
const stoodDownFailureDetail = "Query closed before response received"

// fakeStopBashTransport answers StopBash with a scripted response.
type fakeStopBashTransport struct {
	shimclient.Client
	resp *shimv1.StopBashResponse
}

func (c *fakeStopBashTransport) StopBash(context.Context, *shimv1.StopBashRequest) (*shimv1.StopBashResponse, error) {
	return c.resp, nil
}

// StandingDown answers that this daemon is standing the fake down.
func (c *fakeStopBashTransport) StandingDown() bool { return true }

// TestEveryShimVerbAnswersARefusalFromAShimStandingDownAsTheStandDown holds
// every verb that reads a typed refusal to the one reading (shimRefused): a
// shim this daemon is standing down refuses a call because it is going, and
// each verb answers that as the stand-down, its refusal kept. The queue's
// verbs answer it as promptqueue.ErrShimStoodDown too (forQueue).
func TestEveryShimVerbAnswersARefusalFromAShimStandingDownAsTheStandDown(t *testing.T) {
	ctx := context.Background()
	tests := []struct {
		name     string
		verb     string
		call     func(t *testing.T) error
		forQueue bool
	}{
		{name: "SetSessionEffort", verb: "SetSessionEffort", call: func(*testing.T) error {
			_, err := applyEffort(ctx, &fakeClient{stoodDown: true, effortResponse: effortFailure(&shimv1.SetSessionEffortFailure{Detail: stoodDownFailureDetail})}, effortLow)
			return err
		}},
		{name: "ResolvePeerDelivery", verb: "ResolvePeerDelivery", call: func(*testing.T) error {
			_, err := resolvePeerDelivery(ctx, &fakePeerDeliveryResolver{standingDown: true, resp: &shimv1.ResolvePeerDeliveryResponse{
				Result: &shimv1.ResolvePeerDeliveryResponse_Failure{Failure: &shimv1.ResolvePeerDeliveryFailure{Detail: stoodDownFailureDetail}},
			}}, "delivery-1")
			return err
		}},
		{name: "ResolveVendorTask", verb: "ResolveVendorTask", call: func(*testing.T) error {
			_, err := resolveVendorTask(ctx, &fakeVendorTaskResolver{standingDown: true, resp: &shimv1.ResolveVendorTaskResponse{
				Result: &shimv1.ResolveVendorTaskResponse_Failure{Failure: &shimv1.ResolveVendorTaskFailure{Detail: stoodDownFailureDetail}},
			}}, "main", "task-1")
			return err
		}},
		{name: "SetSessionRemoteControl", verb: "SetSessionRemoteControl", call: func(t *testing.T) error {
			f, ws := startedFleet(t)
			f.client.stoodDown = true
			f.client.remoteControlResponse = &shimv1.SetSessionRemoteControlResponse{Result: &shimv1.SetSessionRemoteControlResponse_Failure{
				Failure: &shimv1.SetSessionRemoteControlFailure{Detail: stoodDownFailureDetail},
			}}
			return f.fleet.applyRemoteControl(ctx, f.log.logger, ws.ID, f.client, true)
		}},
		{name: "RollBackSession", verb: "RollBackSession", call: func(*testing.T) error {
			_, err := rollBackSession(ctx, &fakeRollBackTransport{standingDown: true, resp: &shimv1.RollBackSessionResponse{
				Result: &shimv1.RollBackSessionResponse_Failure{Failure: &shimv1.RollBackSessionFailure{Detail: stoodDownFailureDetail}},
			}}, []ids.TurnID{"turn-1"}, false)
			return err
		}},
		{name: "KillTurn", verb: "KillTurn", call: func(*testing.T) error {
			return killTurn(ctx, &fakeSenderClient{standingDown: true, killTurn: &shimv1.KillTurnResponse{
				Result: &shimv1.KillTurnResponse_Failure{Failure: &shimv1.KillTurnFailure{Detail: stoodDownFailureDetail}},
			}}, "turn-1", false, nil)
		}},
		{name: "UpdateAgent through the verbs' adapter", verb: "UpdateAgent", call: func(*testing.T) error {
			adapter := &shimAdapter{client: &fakeUpdateAgentTransport{standingDown: true, resp: &shimv1.UpdateAgentResponse{
				Result: &shimv1.UpdateAgentResponse_Failure{Failure: &shimv1.UpdateAgentFailure{Detail: stoodDownFailureDetail}},
			}}}
			return adapter.Answer(ctx, &conversationv1.AgentId{Value: "main"}, &conversationv1.AgentAnswer{})
		}},
		{name: "StopBash", verb: "StopBash", call: func(*testing.T) error {
			adapter := &shimAdapter{client: &fakeStopBashTransport{resp: &shimv1.StopBashResponse{
				Result: &shimv1.StopBashResponse_Failure{Failure: &shimv1.StopBashFailure{Detail: stoodDownFailureDetail}},
			}}}
			return adapter.StopBash(ctx, &conversationv1.DetachedWorkId{Value: "shell-1"})
		}},
		{name: "StartTurn", verb: "StartTurn", forQueue: true, call: func(*testing.T) error {
			_, err := newTestSender(&fakeSenderClient{standingDown: true, startTurn: &shimv1.StartTurnResponse{
				Result: &shimv1.StartTurnResponse_Failure{Failure: &shimv1.StartTurnFailure{
					Detail: stoodDownFailureDetail, Kind: &shimv1.StartTurnFailure_QueryDead{QueryDead: &shimv1.StartTurnQueryDead{}},
				}},
			}}).StartTurn(ctx, "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, nil)
			return err
		}},
		{name: "UpdateAgent through the queue's sender", verb: "UpdateAgent", forQueue: true, call: func(*testing.T) error {
			return newTestSender(&fakeSenderClient{standingDown: true, updateAgent: &shimv1.UpdateAgentResponse{
				Result: &shimv1.UpdateAgentResponse_Failure{Failure: &shimv1.UpdateAgentFailure{Detail: stoodDownFailureDetail}},
			}}).PromptAgent(ctx, &conversationv1.AgentId{Value: "sub-1"}, nil)
		}},
		{name: "KillTurn through the queue's sender", verb: "KillTurn", forQueue: true, call: func(*testing.T) error {
			return newTestSender(&fakeSenderClient{standingDown: true, killTurn: &shimv1.KillTurnResponse{
				Result: &shimv1.KillTurnResponse_Failure{Failure: &shimv1.KillTurnFailure{Detail: stoodDownFailureDetail}},
			}}).KillTurn(ctx, "turn-1", false, nil)
		}},
		{name: "SetSessionModel", verb: "SetSessionModel", forQueue: true, call: func(*testing.T) error {
			return newTestSender(&fakeSenderClient{standingDown: true, setModel: &shimv1.SetSessionModelResponse{
				Result: &shimv1.SetSessionModelResponse_Failure{Failure: &shimv1.SetSessionModelFailure{Detail: stoodDownFailureDetail}},
			}}).SetModel(ctx, "claude-opus-5-5")
		}},
		{name: "SetSessionPermissionMode", verb: "SetSessionPermissionMode", forQueue: true, call: func(*testing.T) error {
			return newTestSender(&fakeSenderClient{standingDown: true, setMode: &shimv1.SetSessionPermissionModeResponse{
				Result: &shimv1.SetSessionPermissionModeResponse_Failure{Failure: &shimv1.SetSessionPermissionModeFailure{Detail: stoodDownFailureDetail}},
			}}).SetPermissionMode(ctx, "default")
		}},
		{name: "RollBackSession through the queue's sender", verb: "RollBackSession", forQueue: true, call: func(*testing.T) error {
			return newTestSender(&fakeSenderClient{standingDown: true, rollBack: &shimv1.RollBackSessionResponse{
				Result: &shimv1.RollBackSessionResponse_Failure{Failure: &shimv1.RollBackSessionFailure{Detail: stoodDownFailureDetail}},
			}}).RollBackTurn(ctx, "turn-1")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: each row's shim is being stood down and refuses. Act.
			err := tt.call(t)

			// Assert.
			refusal, ok := AsShimRefusal(err)
			if !ok || refusal.Verb != tt.verb || refusal.Detail != stoodDownFailureDetail {
				t.Fatalf("err = %v, want %s's refusal kept in the chain", err, tt.verb)
			}
			if !errors.Is(err, shimclient.ErrStandDownOrdered) {
				t.Fatalf("err = %v, want the stand-down", err)
			}
			if got := errors.Is(err, promptqueue.ErrShimStoodDown); got != tt.forQueue {
				t.Fatalf("errors.Is(err, promptqueue.ErrShimStoodDown) = %v, want %v", got, tt.forQueue)
			}
		})
	}
}

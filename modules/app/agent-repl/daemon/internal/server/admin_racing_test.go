package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
)

// racingRequest is an UpdateMergeQueue request flipping the racing switch:
// disable_racing, or (enable) enable_racing.
func racingRequest(enable bool) *connect.Request[agentreplv1.UpdateMergeQueueRequest] {
	if enable {
		return connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
			Action: &agentreplv1.UpdateMergeQueueRequest_EnableRacing{EnableRacing: &agentreplv1.UpdateMergeQueueEnableRacing{}},
		})
	}
	return connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_DisableRacing{DisableRacing: &agentreplv1.UpdateMergeQueueDisableRacing{}},
	})
}

// TestEveryDaemonWideSwitchArmReachesItsOwnVerb pins that each daemon-wide
// switch arm goes through daemonWideMergeAction to its own orchestrator verb
// alone, answers success, and is recorded under its action name, so an arm
// hand-routed beside the shared dispatch fails here.
func TestEveryDaemonWideSwitchArmReachesItsOwnVerb(t *testing.T) {
	tests := []struct {
		name   string
		req    *connect.Request[agentreplv1.UpdateMergeQueueRequest]
		action string
		want   [4]int
	}{
		{name: "disable_racing", req: racingRequest(false), action: "disable_racing", want: [4]int{0, 0, 1, 0}},
		{name: "enable_racing", req: racingRequest(true), action: "enable_racing", want: [4]int{0, 0, 0, 1}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), tt.req)

			// Assert.
			if err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			got := [4]int{h.Merge.holds, h.Merge.releases, h.Merge.disables, h.Merge.enables}
			if got != tt.want {
				t.Fatalf("holds, releases, disables, enables = %v; want %v", got, tt.want)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.server.update_merge_queue"})
			if !ok || rec.Context["action"] != tt.action {
				t.Fatalf("INFO record = %v (%v), want the applied %s action", rec.Context, ok, tt.action)
			}
		})
	}
}

// TestTheRacingSwitchRefusalsAreAnswers pins that each no-op refusal is its
// own UpdateMergeQueueError arm.
func TestTheRacingSwitchRefusalsAreAnswers(t *testing.T) {
	tests := []struct {
		name   string
		enable bool
		arm    string
		got    func(*agentreplv1.UpdateMergeQueueError) bool
	}{
		{name: "a disable of disabled racing", enable: false, arm: merge.ArmRacingAlreadyDisabled,
			got: func(e *agentreplv1.UpdateMergeQueueError) bool { return e.GetRacingAlreadyDisabled() != nil }},
		{name: "an enable of racing not disabled", enable: true, arm: merge.ArmRacingNotDisabled,
			got: func(e *agentreplv1.UpdateMergeQueueError) bool { return e.GetRacingNotDisabled() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			refusal := &merge.RefusalError{Arm: tt.arm, Reason: "nothing to change"}
			h.Merge.disableRacingErr, h.Merge.enableRacingErr = refusal, refusal

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), racingRequest(tt.enable))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}
			if !tt.got(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.arm)
			}
		})
	}
}

// TestARacingSwitchFailureIsATransportError pins that a failure that is not a
// refusal (an unreadable switch) stays an error, never a success.
func TestARacingSwitchFailureIsATransportError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.disableRacingErr = errors.New("state database unreadable")

	// Act.
	_, err := h.Client.UpdateMergeQueue(context.Background(), racingRequest(false))

	// Assert.
	if err == nil {
		t.Fatal("UpdateMergeQueue answered a failed disable without an error")
	}
}

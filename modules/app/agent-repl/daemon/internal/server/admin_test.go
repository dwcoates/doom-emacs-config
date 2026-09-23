package server

import (
	"context"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/merge"
)

// TestScheduleEncodesTheReasonBeforeItIsDurable pins that the drain reason is
// STORED AS ITS ENCODED FORM: a guessed string would not decode back.
func TestScheduleEncodesTheReasonBeforeItIsDurable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	reason := &agentreplv1.DrainReason{
		Kind: &agentreplv1.DrainReason_Operator{
			Operator: &agentreplv1.DrainReasonOperator{Note: "rolling out"},
		},
	}

	// Act.
	if _, err := h.Client.UpdateShutdownSchedule(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
			Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{
				Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{AtMs: 1_700_000_000_000, Reason: reason},
			},
		})); err != nil {
		t.Fatalf("UpdateShutdownSchedule: %v", err)
	}

	// Assert.
	if len(h.Drain.scheduled) != 1 {
		t.Fatalf("scheduled %d times, want 1", len(h.Drain.scheduled))
	}
	decoded, err := drain.DecodeReason(h.Drain.scheduled[0].Reason)
	if err != nil {
		t.Fatalf("the stored reason does not decode: %v", err)
	}
	if decoded.GetOperator().GetNote() != "rolling out" {
		t.Fatalf("decoded reason = %v, want the operator's note", decoded)
	}
}

// TestCancelWithNothingScheduledIsRefused pins drain's one refusal.
func TestCancelWithNothingScheduledIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Drain.cancelErr = drain.ErrNothingScheduled

	// Act.
	resp, err := h.Client.UpdateShutdownSchedule(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
			Action: &agentreplv1.UpdateShutdownScheduleRequest_Cancel{
				Cancel: &agentreplv1.UpdateShutdownScheduleCancel{},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateShutdownSchedule: %v", err)
	}
	if resp.Msg.GetError().GetNothingScheduled() == nil {
		t.Fatalf("result = %v, want nothing_scheduled", resp.Msg.GetResult())
	}
}

// TestShutdownNowCarriesTheReasonUnencoded pins that the immediate shutdown
// hands the controller the TYPED reason, which the announcement carries.
func TestShutdownNowCarriesTheReasonUnencoded(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.UpdateShutdownSchedule(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
			Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{
				Now: &agentreplv1.UpdateShutdownScheduleNow{
					Reason: &agentreplv1.DrainReason{
						Kind: &agentreplv1.DrainReason_Deploy{Deploy: &agentreplv1.DrainReasonDeploy{}},
					},
				},
			},
		})); err != nil {
		t.Fatalf("UpdateShutdownSchedule: %v", err)
	}

	// Assert.
	if h.Drain.nowReason.GetDeploy() == nil {
		t.Fatalf("reason = %v, want deploy", h.Drain.nowReason)
	}
}

// TestUnsetRepositoryPausesEveryQueue pins that an UNSET repository ref is the
// daemon-wide switch.
func TestUnsetRepositoryPausesEveryQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.UpdateMergeQueue(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
			Action: &agentreplv1.UpdateMergeQueueRequest_Pause{
				Pause: &agentreplv1.UpdateMergeQueuePause{},
			},
		})); err != nil {
		t.Fatalf("UpdateMergeQueue: %v", err)
	}

	// Assert.
	if h.Merge.pauseScope != nil {
		t.Fatalf("scope = %v, want nil (every repository)", h.Merge.pauseScope)
	}
}

// TestSetRepositoryScopesThePause pins that a set ref scopes the pause to that
// repository's queue.
func TestSetRepositoryScopesThePause(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.UpdateMergeQueue(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
			Action: &agentreplv1.UpdateMergeQueueRequest_Pause{
				Pause: &agentreplv1.UpdateMergeQueuePause{
					Repository: &workspacev1.RepositoryRef{Id: "repo-1"},
				},
			},
		})); err != nil {
		t.Fatalf("UpdateMergeQueue: %v", err)
	}

	// Assert.
	if h.Merge.pauseScope == nil || string(h.Merge.pauseScope.ID) != "repo-1" {
		t.Fatalf("scope = %v, want repo-1", h.Merge.pauseScope)
	}
}

// TestUnknownRepositoryIsAnAnswer pins the landed arm: a pause naming no
// registered repository answers UpdateMergeQueueError.unknown_repository.
func TestUnknownRepositoryIsAnAnswer(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.pauseErr = &merge.RefusalError{
		Arm: merge.ArmUnknownRepository, Reason: "no such repository",
	}

	// Act.
	resp, err := h.Client.UpdateMergeQueue(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
			Action: &agentreplv1.UpdateMergeQueueRequest_Pause{
				Pause: &agentreplv1.UpdateMergeQueuePause{
					Repository: &workspacev1.RepositoryRef{Id: "repo-nope"},
				},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateMergeQueue: %v", err)
	}
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("result = %v, want unknown_repository", resp.Msg.GetResult())
	}
}

// TestUnhealthyDaemonIsAnAnswer pins that unhealthy is an ANSWER, never a
// transport error.
func TestUnhealthyDaemonIsAnAnswer(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Health.daemon = &agentreplv1.DaemonHealthResponse{
		Result: &agentreplv1.DaemonHealthResponse_Success{
			Success: &agentreplv1.DaemonHealthSuccess{
				Health: &agentreplv1.DaemonHealthSuccess_Unhealthy{
					Unhealthy: &agentreplv1.DaemonUnhealthy{
						Faults: []*agentreplv1.DaemonFault{{Detail: "the prompts directory is gone"}},
					},
				},
			},
		},
	}

	// Act.
	resp, err := h.Client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("DaemonHealth: %v", err)
	}
	if len(resp.Msg.GetSuccess().GetUnhealthy().GetFaults()) != 1 {
		t.Fatalf("faults = %v, want one", resp.Msg.GetSuccess().GetUnhealthy())
	}
}

// TestClientLogRoutesEachRuntimeToItsOwningSink pins all three routing arms,
// including the historical webapp route for a sender that predates the oneof.
func TestClientLogRoutesEachRuntimeToItsOwningSink(t *testing.T) {
	tests := []struct {
		name    string
		runtime func(*agentreplv1.ClientLogRecord)
		want    string
	}{
		{name: "an unset runtime keeps the historical webapp route", want: dlog.RuntimeWebapp},
		{name: "the webapp arm", runtime: func(r *agentreplv1.ClientLogRecord) {
			r.Runtime = &agentreplv1.ClientLogRecord_Webapp{Webapp: &agentreplv1.ClientLogRuntimeWebapp{}}
		}, want: dlog.RuntimeWebapp},
		{name: "the sidecar arm", runtime: func(r *agentreplv1.ClientLogRecord) {
			r.Runtime = &agentreplv1.ClientLogRecord_Sidecar{Sidecar: &agentreplv1.ClientLogRuntimeSidecar{}}
		}, want: dlog.RuntimeSidecar},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			record := &agentreplv1.ClientLogRecord{
				Level:     &agentreplv1.ClientLogRecord_Warn{Warn: &agentreplv1.ClientLogLevelWarn{}},
				Operation: "client.render",
				Message:   "a render stalled",
			}
			if tc.runtime != nil {
				tc.runtime(record)
			}

			// Act.
			if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
				Workspace: ref(),
				Record:    record,
			})); err != nil {
				t.Fatalf("ClientLog: %v", err)
			}

			// Assert.
			if len(h.Surfaces.clientRecords) != 1 {
				t.Fatalf("persisted %d records, want 1", len(h.Surfaces.clientRecords))
			}
			if got := h.Surfaces.clientRecords[0].ClientKind; got != tc.want {
				t.Fatalf("client kind = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestClientLogForwardsTheClientsTimestamp(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	want := time.Date(2026, 9, 10, 12, 34, 56, 789000000, time.FixedZone("client", -4*60*60)).Format(time.RFC3339Nano)

	// Act.
	if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: ref(),
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.render",
			Message:   "rendered",
			Timestamp: want,
		},
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	if got := h.Surfaces.clientRecords[0].Timestamp; got != want {
		t.Fatalf("timestamp = %q, want %q", got, want)
	}
}

func TestClientLogForwardsTheClientsVerboseClass(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: ref(),
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.trace",
			Message:   "traced",
			Verbose:   true,
		},
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	if !h.Surfaces.clientRecords[0].Verbose {
		t.Fatal("verbose = false, want the client's true value")
	}
}

func TestClientLogForwardsTheClientsNormalClass(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: ref(),
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.render",
			Message:   "rendered",
			Verbose:   false,
		},
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	if h.Surfaces.clientRecords[0].Verbose {
		t.Fatal("verbose = true, want the client's false value")
	}
}

func TestClientLogRecordsTheSuccessfulRequestBoundaryAtDebug(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = &fakeSurfaces{workspace: log}
	})

	// Act.
	if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: ref(),
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.render",
			Message:   "rendered",
		},
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	var persisted *logRecord
	for i := range log.records {
		if log.records[i].Message == "persisted a forwarded client record" {
			persisted = &log.records[i]
			break
		}
	}
	if persisted == nil {
		t.Fatalf("records = %v, want the forwarded-record success", log.records)
	}
	if persisted.Level != "DEBUG" || persisted.Operation != "daemon.server.client_log" {
		t.Fatalf("record = %+v, want the client-log debug operation", persisted)
	}
	if persisted.Context["operation"] != "webapp.render" {
		t.Fatalf("context = %v, want the forwarded operation", persisted.Context)
	}
}

// TestClientLogAnswersTheLandedUnknownWorkspaceArm pins realtest 8's finding E:
// a ClientLog naming a workspace the registry has forgotten is answered with
// the typed `unknown_workspace` arm, never a Connect error.
func TestClientLogAnswersTheLandedUnknownWorkspaceArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-forgotten"},
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.render",
			Message:   "a late line after the close",
		},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ClientLog: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// TestClientLogRecordsTheUnknownWorkspaceRefusalAtInfo pins that the refusal is
// ORDINARY traffic: a late log line after a close is recorded at INFO, and the
// unlanded-arm WARN realtest 8 caught is gone.
func TestClientLogRecordsTheUnknownWorkspaceRefusalAtInfo(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = &fakeSurfaces{global: log}
	})

	// Act.
	if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-forgotten"},
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.render",
			Message:   "a late line after the close",
		},
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	if warnings := log.at("WARN"); len(warnings) != 0 {
		t.Fatalf("warnings = %v, want none", warnings)
	}
	var refusal *logRecord
	for i := range log.records {
		if log.records[i].Context["arm"] == "unknown_workspace" {
			refusal = &log.records[i]
			break
		}
	}
	if refusal == nil {
		t.Fatalf("records = %v, want the typed refusal", log.records)
	}
	if refusal.Level != "INFO" {
		t.Fatalf("record = %+v, want the refusal at INFO", refusal)
	}
}

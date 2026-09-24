package server

import (
	"context"
	"errors"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/rollout"
	"claude-repld/internal/workspace"
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

// TestRollOutBuildHandsTheControllerWhatWasRebuilt pins that each marker on
// the request reaches the controller as the subsystem it names.
func TestRollOutBuildHandsTheControllerWhatWasRebuilt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.accepted = rollout.Acceptance{Action: rollout.ActionHandover}

	// Act.
	_, err := h.Client.RollOutBuild(context.Background(),
		connect.NewRequest(&agentreplv1.RollOutBuildRequest{
			Daemon: &agentreplv1.RollOutBuildDaemon{},
			Webapp: &agentreplv1.RollOutBuildWebapp{},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("RollOutBuild: %v", err)
	}
	if want := (rollout.Rebuilt{Daemon: true, Webapp: true}); h.Rollout.rolledOut != want {
		t.Fatalf("rolled out %+v, want %+v", h.Rollout.rolledOut, want)
	}
}

// TestRollOutBuildAnswersTheActionTaken pins each acceptance's success arm and
// the counts it carries.
func TestRollOutBuildAnswersTheActionTaken(t *testing.T) {
	cases := []struct {
		name     string
		accepted rollout.Acceptance
		check    func(*agentreplv1.RollOutBuildSuccess) bool
	}{
		{"a handover", rollout.Acceptance{Action: rollout.ActionHandover, Workspaces: 3, Busy: 1},
			func(s *agentreplv1.RollOutBuildSuccess) bool {
				return s.GetHandover().GetWorkspaces() == 3 && s.GetHandover().GetBusy() == 1
			}},
		{"a shim relaunch", rollout.Acceptance{Action: rollout.ActionShimRelaunch, Workspaces: 2, Busy: 2},
			func(s *agentreplv1.RollOutBuildSuccess) bool {
				return s.GetShimRelaunch().GetWorkspaces() == 2 && s.GetShimRelaunch().GetBusy() == 2
			}},
		{"a webapp reload", rollout.Acceptance{Action: rollout.ActionWebappReload, Workspaces: 4},
			func(s *agentreplv1.RollOutBuildSuccess) bool { return s.GetWebappReload().GetWebviews() == 4 }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Rollout.accepted = tc.accepted

			// Act.
			resp, err := h.Client.RollOutBuild(context.Background(),
				connect.NewRequest(&agentreplv1.RollOutBuildRequest{Daemon: &agentreplv1.RollOutBuildDaemon{}}))

			// Assert.
			if err != nil {
				t.Fatalf("RollOutBuild: %v", err)
			}
			if !tc.check(resp.Msg.GetSuccess()) {
				t.Fatalf("result = %v, want the arm and counts of %+v", resp.Msg.GetResult(), tc.accepted)
			}
		})
	}
}

// TestRollOutBuildNamesWhatTheRolloutInFlightWaitsOn pins that a second
// rollout is an ANSWER carrying the holdouts, not a transport failure.
func TestRollOutBuildNamesWhatTheRolloutInFlightWaitsOn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.rollOutErr = &rollout.ErrAlreadyRollingOut{WaitingOn: []ids.WorkspaceID{"ws-busy"}}

	// Act.
	resp, err := h.Client.RollOutBuild(context.Background(),
		connect.NewRequest(&agentreplv1.RollOutBuildRequest{Daemon: &agentreplv1.RollOutBuildDaemon{}}))

	// Assert.
	if err != nil {
		t.Fatalf("RollOutBuild: %v", err)
	}
	waiting := resp.Msg.GetError().GetAlreadyRollingOut().GetWaitingOn()
	if len(waiting) != 1 || waiting[0] != "ws-busy" {
		t.Fatalf("result = %v, want already_rolling_out waiting on ws-busy", resp.Msg.GetResult())
	}
}

// TestRollOutBuildOnAJoiningSuccessorIsAnAnswer pins the joining arm.
func TestRollOutBuildOnAJoiningSuccessorIsAnAnswer(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.rollOutErr = rollout.ErrJoining

	// Act.
	resp, err := h.Client.RollOutBuild(context.Background(),
		connect.NewRequest(&agentreplv1.RollOutBuildRequest{Shim: &agentreplv1.RollOutBuildShim{}}))

	// Assert.
	if err != nil {
		t.Fatalf("RollOutBuild: %v", err)
	}
	if resp.Msg.GetError().GetJoining() == nil {
		t.Fatalf("result = %v, want joining", resp.Msg.GetResult())
	}
}

// TestRollOutBuildNamingNothingIsMalformed pins that an empty request never
// reaches the controller.
func TestRollOutBuildNamingNothingIsMalformed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.RollOutBuild(context.Background(),
		connect.NewRequest(&agentreplv1.RollOutBuildRequest{}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want invalid_argument", err)
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

// A DIAGNOSTIC RECORD IS FILED BY WHICHEVER DAEMON IT REACHES. During a
// handover a forwarder keeps dialing the incumbent until the successor
// publishes its address, and can reach the successor before it has adopted;
// a record is not intake, so neither standing refuses it, and no unlanded-arm
// WARN is written for it (TestRefusalOrderingDuringHandover's flake).
func TestClientLogIsFiledWhateverTheServingStanding(t *testing.T) {
	tests := []struct {
		name     string
		standing workspace.Standing
	}{
		{name: "a workspace this daemon transferred away", standing: workspace.StandingTransferringAway},
		{name: "a workspace this daemon has not adopted yet", standing: workspace.StandingNotYetAdopted},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}
			surfaces := &fakeSurfaces{global: log, workspace: log}
			h := newHarness(t, func(deps *Deps) {
				deps.Log = surfaces
			})
			h.Ownership.standing = tc.standing

			// Act.
			resp, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
				Workspace: ref(),
				Record: &agentreplv1.ClientLogRecord{
					Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
					Operation: "sidecar.tail.pickup",
					Message:   "a record forwarded across the handover",
				},
			}))

			// Assert.
			if err != nil {
				t.Fatalf("ClientLog: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			if len(surfaces.clientRecords) != 1 {
				t.Fatalf("persisted %d records, want 1", len(surfaces.clientRecords))
			}
			if warnings := log.at("WARN"); len(warnings) != 0 {
				t.Fatalf("warnings = %v, want none", warnings)
			}
		})
	}
}

// clientLogOnce sends one ordinary forwarded record.
func clientLogOnce(h *harness) error {
	_, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: ref(),
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.drain.banner",
			Message:   "a record forwarded while the daemon goes away",
		},
	}))
	return err
}

// TestClientLogAfterCloseIsUnavailableWithNoError: a record that reaches a
// daemon already exiting — the webapp layer's drain area sends them as the
// drain fires — is answered UNAVAILABLE, and the daemon going away is not
// restated as a failure.
func TestClientLogAfterCloseIsUnavailableWithNoError(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.DB.workspaceErr = errors.New("sql: database is closed")
	if err := h.Server.Close(); err != nil {
		t.Fatalf("close the surface: %v", err)
	}

	// Act.
	err := clientLogOnce(h)

	// Assert.
	if connect.CodeOf(err) != connect.CodeUnavailable {
		t.Fatalf("ClientLog after Close = %v, want unavailable", err)
	}
	if errs := log.at("ERROR"); len(errs) != 0 {
		t.Fatalf("a ClientLog after Close recorded %d ERROR(s): %+v", len(errs), errs)
	}
}

// TestClientLogWhoseCallerLeftRecordsNoError: the request's own context ending
// under the registry read is the caller leaving, recorded at INFO only.
func TestClientLogWhoseCallerLeftRecordsNoError(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.DB.workspaceErr = context.Canceled

	// Act.
	_ = clientLogOnce(h)

	// Assert.
	if errs := log.at("ERROR"); len(errs) != 0 {
		t.Fatalf("a ClientLog whose caller left recorded %d ERROR(s): %+v", len(errs), errs)
	}
}

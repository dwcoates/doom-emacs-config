package server

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/drain"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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
		connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
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
		connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
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

// hardPause sends a hard pause scoped to repo-1.
func hardPause(t *testing.T, h *harness) {
	t.Helper()
	if _, err := h.Client.UpdateMergeQueue(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
			Action: &agentreplv1.UpdateMergeQueueRequest_Pause{
				Pause: &agentreplv1.UpdateMergeQueuePause{
					Repository: &workspacev1.RepositoryRef{Id: "repo-1"},
					Hard:       true,
				},
			},
		})); err != nil {
		t.Fatalf("UpdateMergeQueue: %v", err)
	}
}

func TestAHardPauseHardStopsTheScopedQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	hardPause(t, h)

	// Assert.
	if !h.Merge.hardStopped || h.Merge.hardStopScope == nil || string(h.Merge.hardStopScope.ID) != "repo-1" {
		t.Fatalf("hard stop = (%v, %v), want repo-1 hard-stopped", h.Merge.hardStopped, h.Merge.hardStopScope)
	}
}

func TestAHardPauseIsNotAlsoAnOrdinaryPause(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	hardPause(t, h)

	// Assert.
	if h.Merge.pauseScope != nil {
		t.Fatalf("pause scope = %v, want the hard stop alone", h.Merge.pauseScope)
	}
}

func TestAnOrdinaryPauseIsNoHardStop(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.UpdateMergeQueue(context.Background(),
		connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
			Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{}},
		})); err != nil {
		t.Fatalf("UpdateMergeQueue: %v", err)
	}

	// Assert.
	if h.Merge.hardStopped {
		t.Fatal("an ordinary pause hard-stopped the queue")
	}
}

func TestAPauseCarriesWhetherItWasMadeToLand(t *testing.T) {
	tests := []struct {
		name   string
		toLand bool
	}{
		{name: "a pause to land a change", toLand: true},
		{name: "a pause the owner asked for"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			if _, err := h.Client.UpdateMergeQueue(context.Background(),
				connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
					Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{ToLand: tt.toLand}},
				})); err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}

			// Assert.
			if h.Merge.pauser == nil || h.Merge.pauser.ToLand != tt.toLand {
				t.Fatalf("pauser = %+v, want to_land %v", h.Merge.pauser, tt.toLand)
			}
		})
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
		connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
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

// moveRequests are UpdateMergeQueue's two move arms, each naming the harness
// workspace, with the move each asks for.
func moveRequests() []struct {
	name string
	req  *agentreplv1.UpdateMergeQueueRequest
	move wsm.MergeMove
} {
	return []struct {
		name string
		req  *agentreplv1.UpdateMergeQueueRequest
		move wsm.MergeMove
	}{
		{name: "up", move: wsm.MergeMoveUp, req: &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
			Action: &agentreplv1.UpdateMergeQueueRequest_MoveUp{MoveUp: &agentreplv1.UpdateMergeQueueMoveUp{Workspace: ref()}}}},
		{name: "down", move: wsm.MergeMoveDown, req: &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
			Action: &agentreplv1.UpdateMergeQueueRequest_MoveDown{MoveDown: &agentreplv1.UpdateMergeQueueMoveDown{Workspace: ref()}}}},
	}
}

// TestMoveMovesTheNamedWorkspacesMerge pins that each move arm reaches the
// orchestrator for the workspace it names, with its own move.
func TestMoveMovesTheNamedWorkspacesMerge(t *testing.T) {
	for _, tt := range moveRequests() {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			want := []fakeMove{{ws: ids.WorkspaceID(ref().GetId()), move: tt.move}}
			if !slices.Equal(h.Merge.moves, want) {
				t.Fatalf("moves = %v, want %v", h.Merge.moves, want)
			}
		})
	}
}

// TestMoveToMovesTheNamedMergeBesideItsAnchor pins that each side of a move
// to a place reaches the orchestrator with the merge, its anchor and the side.
func TestMoveToMovesTheNamedMergeBesideItsAnchor(t *testing.T) {
	tests := []struct {
		name  string
		place func(*agentreplv1.UpdateMergeQueueMoveTo)
		side  wsm.MergeSide
	}{
		{name: "ahead of", side: wsm.MergeSideAheadOf, place: func(to *agentreplv1.UpdateMergeQueueMoveTo) {
			to.Place = &agentreplv1.UpdateMergeQueueMoveTo_AheadOf{AheadOf: ref()}
		}},
		{name: "behind", side: wsm.MergeSideBehind, place: func(to *agentreplv1.UpdateMergeQueueMoveTo) {
			to.Place = &agentreplv1.UpdateMergeQueueMoveTo_Behind{Behind: ref()}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			to := &agentreplv1.UpdateMergeQueueMoveTo{Workspace: ref()}
			tt.place(to)

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_MoveTo{MoveTo: to}}))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			id := ids.WorkspaceID(ref().GetId())
			if want := []fakeMoveTo{{ws: id, anchor: id, side: tt.side}}; !slices.Equal(h.Merge.movesTo, want) {
				t.Fatalf("moves to = %v, want %v", h.Merge.movesTo, want)
			}
		})
	}
}

// TestMoveToAnUnknownAnchorIsAnAnswer pins that an anchor the registry does
// not hold answers unknown_workspace and moves nothing.
func TestMoveToAnUnknownAnchorIsAnAnswer(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	to := &agentreplv1.UpdateMergeQueueMoveTo{Workspace: ref(),
		Place: &agentreplv1.UpdateMergeQueueMoveTo_Behind{Behind: &workspacev1.WorkspaceRef{Id: "nobody", Dir: "/nowhere"}}}

	// Act.
	resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_MoveTo{MoveTo: to}}))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateMergeQueue: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Merge.movesTo) != 0 {
		t.Fatalf("moves to = %v, want none", h.Merge.movesTo)
	}
}

// TestMoveToWithNoPlaceOrNoWorkspaceIsInvalid pins the validation: a move to
// a place names its merge and the merge it goes beside.
func TestMoveToWithNoPlaceOrNoWorkspaceIsInvalid(t *testing.T) {
	tests := []struct {
		name string
		to   *agentreplv1.UpdateMergeQueueMoveTo
	}{
		{name: "no place", to: &agentreplv1.UpdateMergeQueueMoveTo{Workspace: ref()}},
		{name: "no workspace", to: &agentreplv1.UpdateMergeQueueMoveTo{Place: &agentreplv1.UpdateMergeQueueMoveTo_Behind{Behind: ref()}}},
		{name: "an empty anchor", to: &agentreplv1.UpdateMergeQueueMoveTo{Workspace: ref(), Place: &agentreplv1.UpdateMergeQueueMoveTo_AheadOf{AheadOf: &workspacev1.WorkspaceRef{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_MoveTo{MoveTo: tt.to}}))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("UpdateMergeQueue = %v, want InvalidArgument", err)
			}
			if len(h.Merge.movesTo) != 0 {
				t.Fatalf("moves to = %v, want none", h.Merge.movesTo)
			}
		})
	}
}

// TestAPauseNamesWhoAskedForIt pins that a pause and a hard stop reach the
// orchestrator with their asker: the user, or the asking workspace.
func TestAPauseNamesWhoAskedForIt(t *testing.T) {
	tests := []struct {
		name      string
		requester *agentreplv1.WorkspaceRequester
		hard      bool
		want      merge.Pauser
	}{
		{name: "the user's pause", requester: userRequester(), want: merge.Pauser{By: merge.RequestedByUser}},
		{name: "a workspace's pause", requester: workspaceRequester(ref()), want: merge.Pauser{By: merge.RequestedByAgent, Asker: ids.WorkspaceID(ref().GetId())}},
		{name: "a workspace's hard stop", requester: workspaceRequester(ref()), hard: true, want: merge.Pauser{By: merge.RequestedByAgent, Asker: ids.WorkspaceID(ref().GetId())}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			if _, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: tt.requester,
				Action:    &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{Hard: tt.hard}},
			})); err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}

			// Assert.
			if h.Merge.pauser == nil || *h.Merge.pauser != tt.want {
				t.Fatalf("pauser = %+v, want %+v", h.Merge.pauser, tt.want)
			}
		})
	}
}

// TestMoveOfNothingQueuedIsAnAnswer pins that a move for a workspace with no
// queued merge answers no_such_queued_merge.
func TestMoveOfNothingQueuedIsAnAnswer(t *testing.T) {
	for _, tt := range moveRequests() {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.moveErr = &merge.RefusalError{Arm: merge.ArmNoSuchQueuedMerge, Reason: "nothing queued"}

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}
			if resp.Msg.GetError().GetNoSuchQueuedMerge() == nil {
				t.Fatalf("result = %v, want no_such_queued_merge", resp.Msg.GetResult())
			}
		})
	}
}

// TestAMoveThatMovesNothingIsAnsweredMoveBlocked pins the wire answer of a
// one-place move refused for each cause: the move_blocked arm with that
// cause set, and the admitted blocker named inside its cause, never a
// success.
func TestAMoveThatMovesNothingIsAnsweredMoveBlocked(t *testing.T) {
	blocker := &agentreplv1.UpdateMergeQueueMoveBlocker{Workspace: &workspacev1.WorkspaceRef{Id: "ws-head", Dir: "/w/head"}, Name: "pr-status-link-badges"}
	tests := []struct {
		name   string
		fields map[string]any
		check  func(t *testing.T, blocked *agentreplv1.UpdateMergeQueueMoveBlocked)
	}{
		{name: "not waiting", fields: map[string]any{"cause": refusalarm.Nested("not_waiting")},
			check: func(t *testing.T, blocked *agentreplv1.UpdateMergeQueueMoveBlocked) {
				if blocked.GetNotWaiting() == nil {
					t.Fatalf("cause = %v, want not_waiting", blocked.GetCause())
				}
			}},
		{name: "at the edge", fields: map[string]any{"cause": refusalarm.Nested("at_edge")},
			check: func(t *testing.T, blocked *agentreplv1.UpdateMergeQueueMoveBlocked) {
				if blocked.GetAtEdge() == nil {
					t.Fatalf("cause = %v, want at_edge", blocked.GetCause())
				}
			}},
		{name: "blocked by an admitted merge", fields: map[string]any{"cause": refusalarm.Nested("admitted"), "blocker": blocker},
			check: func(t *testing.T, blocked *agentreplv1.UpdateMergeQueueMoveBlocked) {
				if got := blocked.GetAdmitted().GetBlocker(); !proto.Equal(got, blocker) {
					t.Fatalf("blocker = %v, want %v", got, blocker)
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.moveErr = &merge.RefusalError{Arm: merge.ArmMoveBlocked, Reason: "stays", Fields: tt.fields}

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(moveRequests()[0].req))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}
			blocked := resp.Msg.GetError().GetMoveBlocked()
			if blocked == nil {
				t.Fatalf("result = %v, want move_blocked", resp.Msg.GetResult())
			}
			tt.check(t, blocked)
		})
	}
}

// TestMoveWithNoWorkspaceIsInvalid pins the validation: each move arm names
// the workspace it moves.
func TestMoveWithNoWorkspaceIsInvalid(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.UpdateMergeQueueRequest
	}{
		{name: "up", req: &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
			Action: &agentreplv1.UpdateMergeQueueRequest_MoveUp{MoveUp: &agentreplv1.UpdateMergeQueueMoveUp{}}}},
		{name: "down", req: &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(),
			Action: &agentreplv1.UpdateMergeQueueRequest_MoveDown{MoveDown: &agentreplv1.UpdateMergeQueueMoveDown{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("UpdateMergeQueue = %v, want InvalidArgument", err)
			}
			if len(h.Merge.moves) != 0 {
				t.Fatalf("moves = %v, want none", h.Merge.moves)
			}
		})
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

// TestDaemonHealthRendersItsReportsErrorByItsCause pins that a report the
// caller withdrew (recorded by the reporter, where it decided so) keeps its
// Connect code and adds no ERROR, while any other report error is an internal
// failure recorded at ERROR.
func TestDaemonHealthRendersItsReportsErrorByItsCause(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantCode  connect.Code
		wantError bool
	}{
		{name: "the caller withdrew the ask", err: fmt.Errorf("health: self-check: %w", context.Canceled), wantCode: connect.CodeCanceled, wantError: false},
		{name: "the report failed", err: errors.New("health: broke"), wantCode: connect.CodeInternal, wantError: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			h.Health.daemonErr = tt.err

			// Act.
			_, err := h.Client.DaemonHealth(context.Background(), connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))

			// Assert.
			if connect.CodeOf(err) != tt.wantCode {
				t.Fatalf("DaemonHealth err = %v (code %v), want code %v", err, connect.CodeOf(err), tt.wantCode)
			}
			recorded := false
			for _, r := range log.Records() {
				recorded = recorded || (r.Level == dlog.LevelError && r.Operation == "DaemonHealth")
			}
			if recorded != tt.wantError {
				t.Fatalf("an ERROR was recorded = %v, want %v: %+v", recorded, tt.wantError, log.Records())
			}
		})
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
				Site: "0123456789ab", Template: "a test record",
				Level:     &agentreplv1.ClientLogRecord_Warn{Warn: &agentreplv1.ClientLogLevelWarn{}},
				Operation: "client.render",
				Message:   "a render stalled",
			}
			if tc.runtime != nil {
				tc.runtime(record)
			}

			// Act.
			if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
				Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()},
				Record: record,
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
		Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
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
		Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
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
		Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
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

// TestClientLogRecordsNothingOfItsOwnOnSuccess pins that a forwarded record
// is its own evidence: the daemon persists it and writes no record about it.
func TestClientLogRecordsNothingOfItsOwnOnSuccess(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	surfaces := &fakeSurfaces{workspace: log}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = surfaces
	})

	// Act.
	if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.render",
			Message:   "rendered",
		},
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	for _, record := range log.records() {
		if record.Operation == "daemon.server.client_log" {
			t.Fatalf("records = %v, want none of ClientLog's own", log.records())
		}
	}
	if len(surfaces.clientRecords) != 1 || surfaces.clientRecords[0].Operation != "webapp.render" {
		t.Fatalf("client records = %+v, want the forwarded one persisted", surfaces.clientRecords)
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
		Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-forgotten"}},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
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
		Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-forgotten"}},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
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
	for i := range log.records() {
		if log.records()[i].Context["arm"] == "unknown_workspace" {
			refusal = &log.records()[i]
			break
		}
	}
	if refusal == nil {
		t.Fatalf("records = %v, want the typed refusal", log.records())
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
				Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()},
				Record: &agentreplv1.ClientLogRecord{
					Site: "0123456789ab", Template: "a test record",
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
		Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
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

// TestEveryQueuedMergeArmSharesOneAnswer pins that every UpdateMergeQueue arm
// acting on one workspace's merge answers through the one shared body
// (actOnQueuedMerge): an unknown workspace is unknown_workspace with the
// orchestrator untouched, an orchestrator refusal is its typed arm, and
// anything else is success.
func TestEveryQueuedMergeArmSharesOneAnswer(t *testing.T) {
	arms := []struct {
		name string
		req  func(ref *workspacev1.WorkspaceRef) *agentreplv1.UpdateMergeQueueRequest
	}{
		{name: "evict", req: func(ref *workspacev1.WorkspaceRef) *agentreplv1.UpdateMergeQueueRequest {
			return &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: ref}}}
		}},
		{name: "move_up", req: func(ref *workspacev1.WorkspaceRef) *agentreplv1.UpdateMergeQueueRequest {
			return &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_MoveUp{MoveUp: &agentreplv1.UpdateMergeQueueMoveUp{Workspace: ref}}}
		}},
		{name: "move_down", req: func(ref *workspacev1.WorkspaceRef) *agentreplv1.UpdateMergeQueueRequest {
			return &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_MoveDown{MoveDown: &agentreplv1.UpdateMergeQueueMoveDown{Workspace: ref}}}
		}},
		{name: "give_up", req: func(ref *workspacev1.WorkspaceRef) *agentreplv1.UpdateMergeQueueRequest {
			return giveUpRequest(ref, ref, "the conflict needs a redesign")
		}},
	}
	cases := []struct {
		name      string
		ref       *workspacev1.WorkspaceRef
		refused   bool
		wantArm   func(*agentreplv1.UpdateMergeQueueResponse) bool
		wantDrove bool
	}{
		{name: "an unknown workspace", ref: &workspacev1.WorkspaceRef{Id: "ws-forgotten", Dir: "/nowhere"},
			wantArm: func(r *agentreplv1.UpdateMergeQueueResponse) bool { return r.GetError().GetUnknownWorkspace() != nil }},
		{name: "an orchestrator refusal", ref: ref(), refused: true, wantDrove: true,
			wantArm: func(r *agentreplv1.UpdateMergeQueueResponse) bool { return r.GetError().GetNoSuchQueuedMerge() != nil }},
		{name: "success", ref: ref(), wantDrove: true,
			wantArm: func(r *agentreplv1.UpdateMergeQueueResponse) bool { return r.GetSuccess() != nil }},
	}
	for _, arm := range arms {
		for _, tc := range cases {
			t.Run(arm.name+"/"+tc.name, func(t *testing.T) {
				// Arrange.
				h := newHarness(t)
				if tc.refused {
					refusal := &merge.RefusalError{Arm: merge.ArmNoSuchQueuedMerge, Reason: "nothing queued"}
					h.Merge.evictErr, h.Merge.moveErr, h.Merge.giveUpErr = refusal, refusal, refusal
				}

				// Act.
				resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(arm.req(tc.ref)))

				// Assert.
				if err != nil {
					t.Fatalf("UpdateMergeQueue: %v", err)
				}
				if !tc.wantArm(resp.Msg) {
					t.Fatalf("result = %v, want the %s answer", resp.Msg.GetResult(), tc.name)
				}
				if drove := len(h.Merge.evicted)+len(h.Merge.moves)+len(h.Merge.givenUp) > 0; drove != tc.wantDrove {
					t.Fatalf("orchestrator driven = %v, want %v", drove, tc.wantDrove)
				}
			})
		}
	}
}

// giveUpRequest is a give-up of WS's merge, asked by ASKER, saying REASON.
func giveUpRequest(asker, ws *workspacev1.WorkspaceRef, reason string) *agentreplv1.UpdateMergeQueueRequest {
	return &agentreplv1.UpdateMergeQueueRequest{Requester: workspaceRequester(asker), Action: &agentreplv1.UpdateMergeQueueRequest_GiveUp{
		GiveUp: &agentreplv1.UpdateMergeQueueGiveUp{Workspace: ws, Reason: reason}}}
}

func TestUpdateMergeQueueGivesUpTheAskersOwnMergeWithItsReason(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(giveUpRequest(ref(), ref(), "the conflict needs a redesign")))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateMergeQueue = (%v, %v), want success", resp, err)
	}
	if want := string(testWorkspaceID) + ": the conflict needs a redesign"; len(h.Merge.givenUp) != 1 || h.Merge.givenUp[0] != want {
		t.Fatalf("given up %v, want %q", h.Merge.givenUp, want)
	}
}

func TestUpdateMergeQueueRefusesAGiveUpThatIsNotTheMergesOwnAgents(t *testing.T) {
	other := &workspacev1.WorkspaceRef{Id: "ws-other", Dir: "/tmp/other"}
	tests := []struct {
		name string
		req  *agentreplv1.UpdateMergeQueueRequest
	}{
		{name: "another workspace's agent", req: giveUpRequest(other, ref(), "the conflict needs a redesign")},
		{name: "the user", req: &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_GiveUp{
			GiveUp: &agentreplv1.UpdateMergeQueueGiveUp{Workspace: ref(), Reason: "the conflict needs a redesign"}}}},
		{name: "a blank reason", req: giveUpRequest(ref(), ref(), "  ")},
		{name: "no workspace", req: &agentreplv1.UpdateMergeQueueRequest{Requester: workspaceRequester(ref()), Action: &agentreplv1.UpdateMergeQueueRequest_GiveUp{
			GiveUp: &agentreplv1.UpdateMergeQueueGiveUp{Reason: "the conflict needs a redesign"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Merge.givenUp) != 0 {
				t.Fatalf("UpdateMergeQueue = %v, given up %v; want invalid and nothing given up", err, h.Merge.givenUp)
			}
		})
	}
}

func TestUpdateMergeQueueAnswersAGiveUpWithNoRepairTurnAsItsArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.giveUpErr = &merge.RefusalError{Arm: merge.ArmNoRepairTurn, Reason: "no repair turn runs"}

	// Act.
	resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(giveUpRequest(ref(), ref(), "the conflict needs a redesign")))

	// Assert.
	if err != nil || resp.Msg.GetError().GetNoRepairTurn() == nil {
		t.Fatalf("UpdateMergeQueue = (%v, %v), want no_repair_turn", resp, err)
	}
}

func TestClientLogFilesAWebhostRecordCentrally(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.ClientLog(context.Background(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Source: &agentreplv1.ClientLogRequest_Webhost{Webhost: &agentreplv1.ClientLogWebhost{}},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "sidebar.roster",
			Message:   "drew the roster",
		},
	})); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	if len(h.Surfaces.centralRecords) != 1 || len(h.Surfaces.clientRecords) != 0 {
		t.Fatalf("central = %d, workspace = %d, want the one record filed centrally",
			len(h.Surfaces.centralRecords), len(h.Surfaces.clientRecords))
	}
	if got := h.Surfaces.centralRecords[0]; got.ClientKind != dlog.RuntimeWebapp || got.Operation != "sidebar.roster" {
		t.Fatalf("record = %+v, want the webapp's sidebar.roster record", got)
	}
}

// TestUpdateMergeQueueRefusesAnUnnamedRequester pins the requester as
// required: an action that names nobody is refused before the queue is touched.
func TestUpdateMergeQueueRefusesAnUnnamedRequester(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: ref()}}}))

	// Assert.
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument for a request naming no requester", got)
	}
}

// TestUpdateMergeQueueRecordsWhoAsked pins the one record naming the asker of
// every queue action: an eviction recorded only as "the operator" could not be
// told from a loss (2026-10-24).
func TestUpdateMergeQueueRecordsWhoAsked(t *testing.T) {
	tests := []struct {
		name      string
		requester *agentreplv1.WorkspaceRequester
		action    func() *agentreplv1.UpdateMergeQueueRequest
		want      dlog.Context
	}{
		{
			name:      "a workspace's agent evicting a merge",
			requester: agent(),
			action: func() *agentreplv1.UpdateMergeQueueRequest {
				return &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: ref()}}}
			},
			want: dlog.Context{"action": "evict", "requested_by": merge.RequestedByAgent.String(), "asker": "ws-asker", "target": ref().GetId()},
		},
		{
			name:      "a workspace's agent moving a merge to the front",
			requester: agent(),
			action: func() *agentreplv1.UpdateMergeQueueRequest {
				return &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_Move{Move: &agentreplv1.UpdateMergeQueueMove{
					Workspace: ref(), Place: &agentreplv1.UpdateMergeQueueMove_Front{Front: &agentreplv1.UpdateMergeQueueMoveFront{}}}}}
			},
			want: dlog.Context{"action": "move", "requested_by": merge.RequestedByAgent.String(), "asker": "ws-asker", "target": ref().GetId()},
		},
		{
			name:      "the user pausing every queue",
			requester: userRequester(),
			action: func() *agentreplv1.UpdateMergeQueueRequest {
				return &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{}}}
			},
			want: dlog.Context{"action": "pause", "requested_by": merge.RequestedByUser.String()},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log} })
			req := tt.action()
			req.Requester = tt.requester

			// Act.
			if _, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(req)); err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}

			// Assert.
			for _, rec := range log.at("INFO") {
				if rec.Operation == "daemon.server.update_merge_queue" && rec.Message == "asked to act on a merge queue" {
					if !reflect.DeepEqual(rec.Context, tt.want) {
						t.Fatalf("record context = %+v, want %+v", rec.Context, tt.want)
					}
					return
				}
			}
			t.Fatalf("no record of who asked among %+v", log.at("INFO"))
		})
	}
}

// aDispatch is a dispatch of one problem to REMEDIATION's workspace.
func aDispatch(remediation *agentreplv1.UpdateMergeQueueMediationDispatch) *agentreplv1.UpdateMergeQueueMediation {
	remediation.Problems = []*agentreplv1.DeployProblem{{Statement: "the reaper errors", Evidence: []string{"{record}"}}}
	return &agentreplv1.UpdateMergeQueueMediation{Summary: "clean", Decision: &agentreplv1.UpdateMergeQueueMediation_Dispatch{Dispatch: remediation}}
}

func TestUpdateMergeQueueHandsTheIntakesDecisionOn(t *testing.T) {
	problems := []merge.DeployProblem{{Statement: "the reaper errors", Evidence: []string{"{record}"}}}
	tests := []struct {
		name     string
		decision *agentreplv1.UpdateMergeQueueMediation
		want     merge.Decision
	}{
		{name: "an approval", decision: &agentreplv1.UpdateMergeQueueMediation{Summary: "clean",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Approve{Approve: &agentreplv1.UpdateMergeQueueMediationApprove{}}},
			want: merge.Decision{Kind: merge.MediationApprove, Summary: "clean"}},
		{name: "an approval naming its ignored groups", decision: &agentreplv1.UpdateMergeQueueMediation{Summary: "clean",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Approve{Approve: &agentreplv1.UpdateMergeQueueMediationApprove{
				Ignored: []*agentreplv1.DeployProblem{{Statement: "the reaper errors", Evidence: []string{"{record}"}}}}}},
			want: merge.Decision{Kind: merge.MediationApprove, Summary: "clean", Problems: problems}},
		{name: "an escalation", decision: &agentreplv1.UpdateMergeQueueMediation{Summary: "clean",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Escalate{Escalate: &agentreplv1.UpdateMergeQueueMediationEscalate{}}},
			want: merge.Decision{Kind: merge.MediationEscalate, Summary: "clean"}},
		{name: "an escalation with its findings", decision: &agentreplv1.UpdateMergeQueueMediation{Summary: "clean",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Escalate{Escalate: &agentreplv1.UpdateMergeQueueMediationEscalate{
				Problems: []*agentreplv1.DeployProblem{{Statement: "the reaper errors", Evidence: []string{"{record}"}}}}}},
			want: merge.Decision{Kind: merge.MediationEscalate, Summary: "clean", Problems: problems}},
		{name: "a dispatch", decision: aDispatch(&agentreplv1.UpdateMergeQueueMediationDispatch{}),
			want: merge.Decision{Kind: merge.MediationDispatch, Summary: "clean", Problems: problems}},
		{name: "a dispatch naming the race findings a problem is", decision: &agentreplv1.UpdateMergeQueueMediation{Summary: "clean",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Dispatch{Dispatch: &agentreplv1.UpdateMergeQueueMediationDispatch{
				Problems: []*agentreplv1.DeployProblem{{Statement: "TestRacy races", Evidence: []string{"lost"},
					RaceFindings: []*agentreplv1.QueueRaceFindingRef{{Suite: "daemon", Test: "TestRacy"}}}}}}},
			want: merge.Decision{Kind: merge.MediationDispatch, Summary: "clean", Problems: []merge.DeployProblem{{Statement: "TestRacy races", Evidence: []string{"lost"},
				RaceFindings: []wsm.RaceFindingRef{{Suite: "daemon", Test: "TestRacy"}}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: agent(), Action: &agentreplv1.UpdateMergeQueueRequest_Mediation{Mediation: tt.decision}}))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("UpdateMergeQueue = (%v, %v), want success", resp, err)
			}
			if len(h.Merge.decisions) != 1 || fmt.Sprint(h.Merge.decisions[0].decision) != fmt.Sprint(tt.want) || h.Merge.decisions[0].ws != "ws-asker" {
				t.Fatalf("decisions = %+v, want the asker's %+v", h.Merge.decisions, tt.want)
			}
		})
	}
}

func TestUpdateMergeQueueReleasesTheRepositorysEscalation(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Requester: userRequester(),
		Action: &agentreplv1.UpdateMergeQueueRequest_ReleaseEscalation{ReleaseEscalation: &agentreplv1.UpdateMergeQueueReleaseEscalation{
			Repository: &workspacev1.RepositoryRef{Id: "repo-1", Dir: "/tmp/repo"}}}}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateMergeQueue = (%v, %v), want success", resp, err)
	}
	if len(h.Merge.releasedEscalations) != 1 || h.Merge.releasedEscalations[0] != (merge.RepositoryScope{ID: "repo-1", Dir: "/tmp/repo"}) {
		t.Fatalf("releases = %+v, want the repository's", h.Merge.releasedEscalations)
	}
}

func TestUpdateMergeQueueRefusesAReleaseNamingNoRepository(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Requester: userRequester(),
		Action:    &agentreplv1.UpdateMergeQueueRequest_ReleaseEscalation{ReleaseEscalation: &agentreplv1.UpdateMergeQueueReleaseEscalation{}}}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Merge.releasedEscalations) != 0 {
		t.Fatalf("UpdateMergeQueue = %v, releases %d; want invalid and nothing released", err, len(h.Merge.releasedEscalations))
	}
}

func TestUpdateMergeQueueAnswersAMediationRefusal(t *testing.T) {
	tests := []struct {
		name    string
		refusal *merge.RefusalError
		got     func(*agentreplv1.UpdateMergeQueueError) bool
	}{
		{name: "not the intake", refusal: &merge.RefusalError{Arm: merge.ArmNotMediator, Reason: "no intake"},
			got: func(e *agentreplv1.UpdateMergeQueueError) bool { return e.GetNotMediator() != nil }},
		{name: "no escalation held", refusal: &merge.RefusalError{Arm: merge.ArmNoEscalationHeld, Reason: "none"},
			got: func(e *agentreplv1.UpdateMergeQueueError) bool { return e.GetNoEscalationHeld() != nil }},
		{name: "the bound reached", refusal: &merge.RefusalError{Arm: merge.ArmRemediationBoundReached, Reason: "four",
			Fields: map[string]any{"remediations_before_escalation": uint32(4)}},
			got: func(e *agentreplv1.UpdateMergeQueueError) bool {
				return e.GetRemediationBoundReached().GetRemediationsBeforeEscalation() == 4
			}},
		{name: "groups no one owns", refusal: &merge.RefusalError{Arm: merge.ArmProblemsUnowned, Reason: "one",
			Fields: map[string]any{"groups": []string{"op: msg (new)"}}},
			got: func(e *agentreplv1.UpdateMergeQueueError) bool {
				groups := e.GetProblemsUnowned().GetGroups()
				return len(groups) == 1 && groups[0] == "op: msg (new)"
			}},
		{name: "an unknown race finding", refusal: &merge.RefusalError{Arm: merge.ArmUnknownRaceFinding, Reason: "none",
			Fields: map[string]any{"finding": &agentreplv1.QueueRaceFindingRef{Suite: "e2e", Test: "TestLanding"}}},
			got: func(e *agentreplv1.UpdateMergeQueueError) bool {
				return e.GetUnknownRaceFinding().GetFinding().GetTest() == "TestLanding"
			}},
		{name: "groups past their fixes", refusal: &merge.RefusalError{Arm: merge.ArmProblemsRecurring, Reason: "one",
			Fields: map[string]any{"groups": []string{"op: msg (3 fixes)"}}},
			got: func(e *agentreplv1.UpdateMergeQueueError) bool {
				groups := e.GetProblemsRecurring().GetGroups()
				return len(groups) == 1 && groups[0] == "op: msg (3 fixes)"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.decideErr = tt.refusal

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: agent(), Action: &agentreplv1.UpdateMergeQueueRequest_Mediation{Mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
					Decision: &agentreplv1.UpdateMergeQueueMediation_Approve{Approve: &agentreplv1.UpdateMergeQueueMediationApprove{}}}}}))

			// Assert.
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("UpdateMergeQueue = (%v, %v), want the %s arm", resp, err, tt.refusal.Arm)
			}
		})
	}
}

// ignoredAdd and ignoredRemove are the two changes to the records the deploy
// checks leave out.
func ignoredAdd(kind *agentreplv1.IgnoredRecordKind, why string) *agentreplv1.UpdateMergeQueueIgnoredRecords {
	return &agentreplv1.UpdateMergeQueueIgnoredRecords{Change: &agentreplv1.UpdateMergeQueueIgnoredRecords_Add{
		Add: &agentreplv1.UpdateMergeQueueIgnoredRecordsAdd{Kind: kind, Why: why}}}
}

func ignoredRemove(kind *agentreplv1.IgnoredRecordKind) *agentreplv1.UpdateMergeQueueIgnoredRecords {
	return &agentreplv1.UpdateMergeQueueIgnoredRecords{Change: &agentreplv1.UpdateMergeQueueIgnoredRecords_Remove{
		Remove: &agentreplv1.UpdateMergeQueueIgnoredRecordsRemove{Kind: kind}}}
}

func TestUpdateMergeQueueChangesTheIgnoredRecordsOnTheMediatorsWord(t *testing.T) {
	text := "Parent comment not found"
	kind := &agentreplv1.IgnoredRecordKind{Operation: "daemon.ghclient.post", MessageContains: &text}
	tests := []struct {
		name        string
		change      *agentreplv1.UpdateMergeQueueIgnoredRecords
		wantAdded   int
		wantRemoved int
	}{
		{name: "an add records the kind with its reason", change: ignoredAdd(kind, "GitHub's own"), wantAdded: 1},
		{name: "a remove withdraws the kind", change: ignoredRemove(kind), wantRemoved: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: agent(), Action: &agentreplv1.UpdateMergeQueueRequest_IgnoredRecords{IgnoredRecords: tt.change}}))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("UpdateMergeQueue = (%v, %v), want success", resp, err)
			}
			if len(h.Merge.ignoredKinds) != tt.wantAdded || len(h.Merge.unignoredKinds) != tt.wantRemoved {
				t.Fatalf("added %v and removed %v, want %d and %d", h.Merge.ignoredKinds, h.Merge.unignoredKinds, tt.wantAdded, tt.wantRemoved)
			}
			got := append(h.Merge.ignoredKinds, h.Merge.unignoredKinds...)[0]
			if got.kind.Operation != "daemon.ghclient.post" || got.kind.MessageContains != text || (tt.wantAdded == 1 && got.kind.Why != "GitHub's own") {
				t.Fatalf("the change carried %+v", got.kind)
			}
		})
	}
}

func TestUpdateMergeQueueAnswersARefusedChangeToTheIgnoredRecords(t *testing.T) {
	// Arrange: the asking workspace is no deploy mediator.
	h := newHarness(t)
	h.Merge.ignoreErr = &merge.RefusalError{Arm: merge.ArmNotMediator, Reason: "this workspace is no repository's deploy mediator"}

	// Act.
	resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Requester: agent(), Action: &agentreplv1.UpdateMergeQueueRequest_IgnoredRecords{
			IgnoredRecords: ignoredAdd(&agentreplv1.IgnoredRecordKind{Operation: "op"}, "why")}}))

	// Assert.
	if err != nil || resp.Msg.GetError() == nil {
		t.Fatalf("UpdateMergeQueue = (%v, %v), want the refusal as the error arm", resp, err)
	}
}

func TestUpdateMergeQueueRefusesAMalformedChangeToTheIgnoredRecords(t *testing.T) {
	empty := ""
	tests := []struct {
		name      string
		requester *agentreplv1.WorkspaceRequester
		change    *agentreplv1.UpdateMergeQueueIgnoredRecords
	}{
		{name: "no change", requester: agent(), change: &agentreplv1.UpdateMergeQueueIgnoredRecords{}},
		{name: "the user changing them", requester: userRequester(), change: ignoredAdd(&agentreplv1.IgnoredRecordKind{Operation: "op"}, "why")},
		{name: "an add with no reason", requester: agent(), change: ignoredAdd(&agentreplv1.IgnoredRecordKind{Operation: "op"}, " ")},
		{name: "an add naming no operation", requester: agent(), change: ignoredAdd(&agentreplv1.IgnoredRecordKind{Operation: " "}, "why")},
		{name: "a remove naming no operation", requester: agent(), change: ignoredRemove(&agentreplv1.IgnoredRecordKind{})},
		{name: "a message text set empty", requester: agent(), change: ignoredAdd(&agentreplv1.IgnoredRecordKind{Operation: "op", MessageContains: &empty}, "why")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: tt.requester, Action: &agentreplv1.UpdateMergeQueueRequest_IgnoredRecords{IgnoredRecords: tt.change}}))

			// Assert.
			if got := connectCode(t, err); got != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", got)
			}
			if len(h.Merge.ignoredKinds)+len(h.Merge.unignoredKinds) != 0 {
				t.Fatal("a malformed change reached the merge orchestrator")
			}
		})
	}
}

func TestUpdateMergeQueueRefusesAMalformedMediation(t *testing.T) {
	tests := []struct {
		name      string
		requester *agentreplv1.WorkspaceRequester
		mediation *agentreplv1.UpdateMergeQueueMediation
	}{
		{name: "no decision", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x"}},
		{name: "a blank summary", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: " ",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Approve{Approve: &agentreplv1.UpdateMergeQueueMediationApprove{}}}},
		{name: "the user deciding", requester: userRequester(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Approve{Approve: &agentreplv1.UpdateMergeQueueMediationApprove{}}}},
		{name: "a dispatch stating no problem", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Dispatch{Dispatch: &agentreplv1.UpdateMergeQueueMediationDispatch{}}}},
		{name: "a problem with no statement", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Dispatch{Dispatch: &agentreplv1.UpdateMergeQueueMediationDispatch{
				Problems: []*agentreplv1.DeployProblem{{Statement: " ", Evidence: []string{"r"}}}}}}},
		{name: "a problem with no evidence", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Dispatch{Dispatch: &agentreplv1.UpdateMergeQueueMediationDispatch{
				Problems: []*agentreplv1.DeployProblem{{Statement: "s"}}}}}},
		{name: "an escalation's problem with no evidence", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Escalate{Escalate: &agentreplv1.UpdateMergeQueueMediationEscalate{
				Problems: []*agentreplv1.DeployProblem{{Statement: "s"}}}}}},
		{name: "an approval's ignored group with no evidence", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Approve{Approve: &agentreplv1.UpdateMergeQueueMediationApprove{
				Ignored: []*agentreplv1.DeployProblem{{Statement: "s"}}}}}},
		{name: "a race finding with no test", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Dispatch{Dispatch: &agentreplv1.UpdateMergeQueueMediationDispatch{
				Problems: []*agentreplv1.DeployProblem{{Statement: "s", Evidence: []string{"r"}, RaceFindings: []*agentreplv1.QueueRaceFindingRef{{Suite: "daemon"}}}}}}}},
		{name: "a blank evidence record", requester: agent(), mediation: &agentreplv1.UpdateMergeQueueMediation{Summary: "x",
			Decision: &agentreplv1.UpdateMergeQueueMediation_Dispatch{Dispatch: &agentreplv1.UpdateMergeQueueMediationDispatch{
				Problems: []*agentreplv1.DeployProblem{{Statement: "s", Evidence: []string{" "}}}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: tt.requester, Action: &agentreplv1.UpdateMergeQueueRequest_Mediation{Mediation: tt.mediation}}))

			// Assert.
			if got := connectCode(t, err); got != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", got)
			}
		})
	}
}

// moveWith is a move arm request putting ref()'s merge on PLACE.
func moveWith(place func(*agentreplv1.UpdateMergeQueueMove)) *agentreplv1.UpdateMergeQueueRequest {
	move := &agentreplv1.UpdateMergeQueueMove{Workspace: ref()}
	place(move)
	return &agentreplv1.UpdateMergeQueueRequest{Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_Move{Move: move}}
}

// TestMovePutsTheNamedMergeOnItsPlace pins that each place of a move reaches
// the orchestrator for the workspace it names, as the orchestrator's place.
func TestMovePutsTheNamedMergeOnItsPlace(t *testing.T) {
	tests := []struct {
		name  string
		place func(*agentreplv1.UpdateMergeQueueMove)
		want  merge.MovePlace
	}{
		{name: "front", want: merge.MovePlace{Kind: wsm.MergePlaceFront}, place: func(m *agentreplv1.UpdateMergeQueueMove) {
			m.Place = &agentreplv1.UpdateMergeQueueMove_Front{Front: &agentreplv1.UpdateMergeQueueMoveFront{}}
		}},
		{name: "back", want: merge.MovePlace{Kind: wsm.MergePlaceBack}, place: func(m *agentreplv1.UpdateMergeQueueMove) {
			m.Place = &agentreplv1.UpdateMergeQueueMove_Back{Back: &agentreplv1.UpdateMergeQueueMoveBack{}}
		}},
		{name: "behind", want: merge.MovePlace{Kind: wsm.MergePlaceBehind, Behind: "two"}, place: func(m *agentreplv1.UpdateMergeQueueMove) {
			m.Place = &agentreplv1.UpdateMergeQueueMove_Behind{Behind: &agentreplv1.UpdateMergeQueueMoveBehind{Merge: "two"}}
		}},
		{name: "index", want: merge.MovePlace{Kind: wsm.MergePlaceIndex, Index: 3}, place: func(m *agentreplv1.UpdateMergeQueueMove) {
			m.Place = &agentreplv1.UpdateMergeQueueMove_Index{Index: &agentreplv1.UpdateMergeQueueMoveIndex{Index: 3}}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(moveWith(tt.place)))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			if want := []fakePlace{{ws: ids.WorkspaceID(ref().GetId()), place: tt.want}}; !slices.Equal(h.Merge.places, want) {
				t.Fatalf("places = %v, want %v", h.Merge.places, want)
			}
		})
	}
}

// TestMoveAnswersEachRefusalAsItsArm pins that every refusal of a move is
// answered as its typed arm, carrying its evidence.
func TestMoveAnswersEachRefusalAsItsArm(t *testing.T) {
	tests := []struct {
		name    string
		refusal *merge.RefusalError
		wantArm func(*agentreplv1.UpdateMergeQueueResponse) bool
	}{
		{name: "no waiting merge", refusal: &merge.RefusalError{Arm: merge.ArmNoSuchQueuedMerge, Reason: "nothing waiting"},
			wantArm: func(r *agentreplv1.UpdateMergeQueueResponse) bool { return r.GetError().GetNoSuchQueuedMerge() != nil }},
		{name: "behind a merge not in line", refusal: &merge.RefusalError{Arm: merge.ArmPlacementBehindNotInLine, Reason: "none", Fields: map[string]any{"behind": "nowhere"}},
			wantArm: func(r *agentreplv1.UpdateMergeQueueResponse) bool {
				return r.GetError().GetPlacementBehindNotInLine().GetBehind() == "nowhere"
			}},
		{name: "an index out of range", refusal: &merge.RefusalError{Arm: merge.ArmIndexOutOfRange, Reason: "past the end", Fields: map[string]any{"index": uint32(5), "waiting": uint32(2)}},
			wantArm: func(r *agentreplv1.UpdateMergeQueueResponse) bool {
				arm := r.GetError().GetIndexOutOfRange()
				return arm.GetIndex() == 5 && arm.GetWaiting() == 2
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.moveErr = tt.refusal

			// Act.
			resp, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(moveWith(func(m *agentreplv1.UpdateMergeQueueMove) {
				m.Place = &agentreplv1.UpdateMergeQueueMove_Back{Back: &agentreplv1.UpdateMergeQueueMoveBack{}}
			})))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateMergeQueue: %v", err)
			}
			if !tt.wantArm(resp.Msg) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.refusal.Arm)
			}
		})
	}
}

// TestMoveMissingItsWorkspaceOrPlaceIsInvalid pins the validation of a move:
// it names a workspace and a place, and a place behind names a merge.
func TestMoveMissingItsWorkspaceOrPlaceIsInvalid(t *testing.T) {
	tests := []struct {
		name string
		move *agentreplv1.UpdateMergeQueueMove
	}{
		{name: "no workspace", move: &agentreplv1.UpdateMergeQueueMove{Place: &agentreplv1.UpdateMergeQueueMove_Front{Front: &agentreplv1.UpdateMergeQueueMoveFront{}}}},
		{name: "no place", move: &agentreplv1.UpdateMergeQueueMove{Workspace: ref()}},
		{name: "behind no merge", move: &agentreplv1.UpdateMergeQueueMove{Workspace: ref(),
			Place: &agentreplv1.UpdateMergeQueueMove_Behind{Behind: &agentreplv1.UpdateMergeQueueMoveBehind{Merge: " "}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateMergeQueue(context.Background(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
				Requester: userRequester(), Action: &agentreplv1.UpdateMergeQueueRequest_Move{Move: tt.move}}))

			// Assert.
			if got := connect.CodeOf(err); got != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", got)
			}
			if len(h.Merge.places) != 0 {
				t.Fatalf("places = %v, want none", h.Merge.places)
			}
		})
	}
}

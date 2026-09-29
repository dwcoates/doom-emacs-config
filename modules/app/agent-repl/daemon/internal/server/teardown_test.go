package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/workspace"
)

// ---- kill and nuke acknowledge at once when the request carries an op_id ----

// receiveProgress reads the next mutation-progress push off STREAM.
func receiveProgress(t *testing.T, stream *connect.ServerStreamForClient[agentreplv1.WatchDaemonResponse]) *agentreplv1.WorkspaceMutationProgress {
	t.Helper()
	for stream.Receive() {
		if prog := stream.Msg().GetMutationProgress(); prog != nil {
			return prog
		}
	}
	t.Fatalf("no mutation progress arrived: %v", stream.Err())
	return nil
}

// hasRecordAt reports whether LOG holds a record at LEVEL for OPERATION.
func hasRecordAt(log *dlog.TestLogger, level, operation string) bool {
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == operation {
			return true
		}
	}
	return false
}

func TestAKillWithAnOpIDIsAcceptedAndItsEndPushed(t *testing.T) {
	tests := []struct {
		name        string
		teardownErr error
		wantFailed  bool
	}{
		{name: "the session died", teardownErr: nil, wantFailed: false},
		{name: "the teardown failed", teardownErr: errors.New("could not stop the shim"), wantFailed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			log := dlog.NewTestLogger()
			h.Surfaces.workspace = log
			h.Verbs.teardownErr = tt.teardownErr
			stream := proveDaemonSubscription(t, h)

			// Act.
			resp, err := h.Client.KillWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: ref(), OpId: proto.String("op-kill")}))

			// Assert.
			if err != nil {
				t.Fatalf("KillWorkspace: %v", err)
			}
			if got := resp.Msg.GetAccepted().GetOpId(); got != "op-kill" {
				t.Fatalf("result = %v, want accepted echoing op-kill", resp.Msg.GetResult())
			}
			prog := receiveProgress(t, stream)
			if prog.GetOpId() != "op-kill" {
				t.Fatalf("op_id = %q, want op-kill", prog.GetOpId())
			}
			if got := prog.GetKill().GetFailed() != nil; got != tt.wantFailed {
				t.Fatalf("kill step = %v, want failed=%v", prog.GetKill().GetStep(), tt.wantFailed)
			}
			if tt.wantFailed {
				if prog.GetKill().GetFailed().GetInternal() != tt.teardownErr.Error() {
					t.Fatalf("failed = %v, want the teardown's own sentence", prog.GetKill().GetFailed())
				}
				if !hasRecordAt(log, dlog.LevelError, opServerTeardown) {
					t.Fatalf("records = %+v, want the failed teardown at ERROR", log.Records())
				}
			}
		})
	}
}

func TestANukeWithAnOpIDPushesItsEnd(t *testing.T) {
	tests := []struct {
		name        string
		teardownErr error
		// check asserts the terminal step.
		check func(t *testing.T, nuke *agentreplv1.WorkspaceNukeProgress)
	}{
		{
			name: "the workspace was destroyed",
			check: func(t *testing.T, nuke *agentreplv1.WorkspaceNukeProgress) {
				if nuke.GetSucceeded() == nil {
					t.Fatalf("nuke step = %v, want succeeded", nuke.GetStep())
				}
			},
		},
		{
			name:        "git refused the destruction",
			teardownErr: &workspace.Refusal{Rpc: "NukeWorkspace", Arm: workspace.ArmGitFailed, Reason: "the worktree is locked"},
			check: func(t *testing.T, nuke *agentreplv1.WorkspaceNukeProgress) {
				if nuke.GetFailed().GetRefusal().GetGitFailed() == nil {
					t.Fatalf("nuke step = %v, want the typed git_failed refusal", nuke.GetStep())
				}
			},
		},
		{
			name:        "an internal failure",
			teardownErr: errors.New("forget failed"),
			check: func(t *testing.T, nuke *agentreplv1.WorkspaceNukeProgress) {
				if nuke.GetFailed().GetInternal() != "forget failed" {
					t.Fatalf("nuke step = %v, want the internal sentence", nuke.GetStep())
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.teardownErr = tt.teardownErr
			stream := proveDaemonSubscription(t, h)

			// Act.
			resp, err := h.Client.NukeWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: ref(), OpId: proto.String("op-nuke")}))

			// Assert.
			if err != nil {
				t.Fatalf("NukeWorkspace: %v", err)
			}
			if got := resp.Msg.GetAccepted().GetOpId(); got != "op-nuke" {
				t.Fatalf("result = %v, want accepted echoing op-nuke", resp.Msg.GetResult())
			}
			prog := receiveProgress(t, stream)
			tt.check(t, prog.GetNuke())
		})
	}
}

func TestAnAcceptedTeardownOutlivesTheRequestThatAcceptedIt(t *testing.T) {
	// Arrange: the teardown waits until the request's context is cancelled.
	h := newHarness(t)
	h.Verbs.teardownRan = make(chan error, 1)
	h.Verbs.teardownRelease = make(chan struct{})
	reqCtx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	resp, err := h.Client.KillWorkspace(reqCtx,
		connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: ref(), OpId: proto.String("op-kill")}))
	if err != nil || resp.Msg.GetAccepted() == nil {
		t.Fatalf("KillWorkspace = (%v, %v), want accepted", resp, err)
	}
	cancel()
	close(h.Verbs.teardownRelease)

	// Assert.
	if err := <-h.Verbs.teardownRan; err != nil {
		t.Fatalf("teardown context = %v, want it alive after the request was cancelled", err)
	}
}

func TestARefusedFastHalfIsAnsweredOnTheRPC(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.beginErr = &workspace.Refusal{Rpc: "KillWorkspace", Arm: "unknown_workspace", Reason: "no such workspace", NotFound: true}
	h.Verbs.teardownRan = make(chan error, 1)

	// Act.
	resp, err := h.Client.KillWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: ref(), OpId: proto.String("op-kill")}))

	// Assert.
	if err != nil {
		t.Fatalf("KillWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	select {
	case <-h.Verbs.teardownRan:
		t.Fatal("a refused kill ran its teardown")
	default:
	}
}

func TestAKillWithoutAnOpIDAnswersOnceTheTeardownIsDone(t *testing.T) {
	tests := []struct {
		name        string
		teardownErr error
		wantSuccess bool
	}{
		{name: "the teardown finished", wantSuccess: true},
		{name: "the teardown failed", teardownErr: errors.New("could not stop the shim"), wantSuccess: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.teardownErr = tt.teardownErr

			// Act.
			resp, err := h.Client.KillWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: ref()}))

			// Assert.
			if tt.wantSuccess {
				if err != nil || resp.Msg.GetSuccess() == nil {
					t.Fatalf("KillWorkspace = (%v, %v), want success", resp, err)
				}
				return
			}
			if err == nil {
				t.Fatalf("KillWorkspace = %v, want the teardown's failure", resp.Msg.GetResult())
			}
		})
	}
}

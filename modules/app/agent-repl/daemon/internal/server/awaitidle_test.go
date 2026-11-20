package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
)

func TestAwaitIdle(t *testing.T) {
	cases := []struct {
		name        string
		testBarrier bool
		// busy leaves work in flight, and cancels the request's context;
		// busyIn, when set, begins it as that workspace's.
		busy   bool
		busyIn string
		// scope names the workspaces the barrier asks about.
		scope   []string
		wantArm string
	}{
		{name: "a daemon no harness started refuses it", wantArm: "not_testing"},
		{name: "an idle daemon answers with its streams", testBarrier: true, wantArm: "success"},
		{name: "a caller that gives up while work is in flight is told so", testBarrier: true, busy: true, wantArm: "still_busy"},
		{name: "a barrier scoped to a workspace answers past another workspace's work", testBarrier: true, busy: true, busyIn: "other", scope: []string{"mine"}, wantArm: "success"},
		{name: "a barrier scoped to a workspace waits for its own work", testBarrier: true, busy: true, busyIn: "mine", scope: []string{"mine"}, wantArm: "still_busy"},
		{name: "a barrier scoped to a workspace waits for work no workspace owns", testBarrier: true, busy: true, scope: []string{"mine"}, wantArm: "still_busy"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			tracker := &inflight.Tracker{}
			s := &server{deps: Deps{TestBarrier: tc.testBarrier, InFlight: tracker}, log: dlog.NewTestLogger()}
			s.frames.register("tagged")
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if tc.busy {
				if tc.busyIn != "" {
					defer tracker.Workspace(tc.busyIn).Begin()()
				} else {
					defer tracker.Begin()()
				}
				cancel()
			}
			req := &agentreplv1.AwaitIdleRequest{BoundMs: 5000}
			for _, ws := range tc.scope {
				req.Workspaces = append(req.Workspaces, &workspacev1.WorkspaceRef{Id: ws})
			}

			// Act
			resp, err := s.AwaitIdle(ctx, connect.NewRequest(req))

			// Assert
			if err != nil {
				t.Fatalf("AwaitIdle = %v, want an answer", err)
			}
			got := ""
			switch r := resp.Msg.GetResult().(type) {
			case *agentreplv1.AwaitIdleResponse_Success:
				got = "success"
				if len(r.Success.GetStreams()) != 1 || r.Success.GetStreams()[0].GetStreamTag() != "tagged" {
					t.Fatalf("streams = %v, want the tagged stream", r.Success.GetStreams())
				}
			case *agentreplv1.AwaitIdleResponse_Error:
				switch c := r.Error.GetCause().(type) {
				case *agentreplv1.AwaitIdleError_NotTesting:
					got = "not_testing"
				case *agentreplv1.AwaitIdleError_StillBusy:
					got = "still_busy"
					if c.StillBusy.GetInFlight() != 1 {
						t.Fatalf("in_flight = %d, want 1", c.StillBusy.GetInFlight())
					}
				}
			}
			if got != tc.wantArm {
				t.Fatalf("arm = %q, want %q", got, tc.wantArm)
			}
		})
	}
}

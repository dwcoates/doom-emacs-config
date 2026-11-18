package main

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"agent-repld/internal/rollout"
	"agent-repld/internal/wsm"
)

// newestDaemon is the newest daemon's half of AcceptHandedWorkspace: it
// records each announcement and answers ANSWER.
type newestDaemon struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	got    []*agentreplv1.AcceptHandedWorkspaceRequest
	answer *agentreplv1.AcceptHandedWorkspaceResponse
}

func (d *newestDaemon) AcceptHandedWorkspace(ctx context.Context, req *connect.Request[agentreplv1.AcceptHandedWorkspaceRequest]) (*connect.Response[agentreplv1.AcceptHandedWorkspaceResponse], error) {
	d.got = append(d.got, req.Msg)
	if d.answer == nil {
		// A daemon that never answers: the call ends only on its caller's bound.
		<-ctx.Done()
		return nil, connect.NewError(connect.CodeDeadlineExceeded, ctx.Err())
	}
	return connect.NewResponse(d.answer), nil
}

// serveNewestDaemon serves D over h2c and answers its loopback address.
func serveNewestDaemon(t *testing.T, d *newestDaemon) string {
	t.Helper()
	path, handler := agentreplv1connect.NewAgentReplHandler(d)
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	server := httptest.NewServer(h2c.NewHandler(mux, &http2.Server{}))
	t.Cleanup(server.Close)
	return server.Listener.Addr().String()
}

func TestSendHandedTellsTheNewestDaemonOfTheMove(t *testing.T) {
	tests := []struct {
		name    string
		answer  *agentreplv1.AcceptHandedWorkspaceResponse
		wantErr func(error) bool
	}{
		{name: "an accepted move", answer: &agentreplv1.AcceptHandedWorkspaceResponse{Result: &agentreplv1.AcceptHandedWorkspaceResponse_Success{Success: &agentreplv1.AcceptHandedWorkspaceSuccess{}}},
			wantErr: func(err error) bool { return err == nil }},
		{name: "a daemon no longer the newest", answer: &agentreplv1.AcceptHandedWorkspaceResponse{Result: &agentreplv1.AcceptHandedWorkspaceResponse_Error{Error: &agentreplv1.AcceptHandedWorkspaceError{
			Cause: &agentreplv1.AcceptHandedWorkspaceError_NotNewest{NotNewest: &agentreplv1.AcceptHandedWorkspaceNotNewest{}}}}},
			wantErr: func(err error) bool { return errors.Is(err, rollout.ErrNotNewest) }},
		{name: "any other refusal", answer: &agentreplv1.AcceptHandedWorkspaceResponse{Result: &agentreplv1.AcceptHandedWorkspaceResponse_Error{Error: &agentreplv1.AcceptHandedWorkspaceError{
			Cause: &agentreplv1.AcceptHandedWorkspaceError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.AcceptHandedWorkspaceUnknownWorkspace{}}}}},
			wantErr: func(err error) bool { return err != nil && !errors.Is(err, rollout.ErrNotNewest) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d := &newestDaemon{answer: tt.answer}
			address := serveNewestDaemon(t, d)

			// Act: unbounded, so the answer's reading is tested apart from
			// the production bound, which a loaded test host can outlast.
			err := sendHanded(context.Background(), address, wsm.Workspace{ID: "ws-1", Dir: "/tmp/ws-1"}, "elder", rollout.Participants{Host: true})

			// Assert
			if !tt.wantErr(err) {
				t.Fatalf("sendHanded = %v", err)
			}
			if len(d.got) != 1 || d.got[0].GetWorkspace().GetId() != "ws-1" || d.got[0].GetOutgoingDaemon() != "elder" || !d.got[0].GetExpectedHost() || d.got[0].GetExpectedWeb() {
				t.Fatalf("announcements = %v, want ws-1 from elder with its host expected", d.got)
			}
		})
	}
}

func TestAnnounceHandedEndsAtItsBound(t *testing.T) {
	// Arrange: a newest daemon that never answers.
	d := &newestDaemon{}
	address := serveNewestDaemon(t, d)

	// Act
	err := announceHanded(context.Background(), address, wsm.Workspace{ID: "ws-1", Dir: "/tmp/ws-1"}, "elder", rollout.Participants{Host: true})

	// Assert
	if connect.CodeOf(err) != connect.CodeDeadlineExceeded {
		t.Fatalf("announceHanded = %v, want the call cut at acceptHandedBound", err)
	}
	if errors.Is(err, rollout.ErrNotNewest) {
		t.Fatalf("announceHanded = %v, want a failure, not a not_newest refusal", err)
	}
}

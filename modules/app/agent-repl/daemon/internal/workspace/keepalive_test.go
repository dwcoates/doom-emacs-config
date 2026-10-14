package workspace

import (
	"context"
	"errors"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/cachelapse"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

type fakeKeepaliveTransport struct {
	shimclient.Client

	req  *shimv1.SetKeepaliveRequest
	resp *shimv1.SetKeepaliveResponse
	err  error
}

func (c *fakeKeepaliveTransport) SetKeepalive(_ context.Context, req *shimv1.SetKeepaliveRequest) (*shimv1.SetKeepaliveResponse, error) {
	c.req = req
	return c.resp, c.err
}

func keepaliveSuccess() *shimv1.SetKeepaliveResponse {
	return &shimv1.SetKeepaliveResponse{Result: &shimv1.SetKeepaliveResponse_Success{Success: &shimv1.SetKeepaliveSuccess{}}}
}

// TestShimAdapterKeepaliveRequestShape covers that Hold and Run each send
// their own cadence arm.
func TestShimAdapterKeepaliveRequestShape(t *testing.T) {
	tests := []struct {
		name string
		set  func(*shimAdapter) error
		hold bool
	}{
		{name: "hold", set: func(a *shimAdapter) error { return a.Hold(context.Background()) }, hold: true},
		{name: "run", set: func(a *shimAdapter) error { return a.Run(context.Background()) }, hold: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			transport := &fakeKeepaliveTransport{resp: keepaliveSuccess()}

			// Act.
			err := tt.set(&shimAdapter{client: transport})

			// Assert.
			if err != nil {
				t.Fatalf("set = %v, want nil", err)
			}
			if (transport.req.GetHold() != nil) != tt.hold || (transport.req.GetRun() != nil) == tt.hold {
				t.Fatalf("cadence = %+v, want hold %v", transport.req.GetCadence(), tt.hold)
			}
		})
	}
}

// TestShimAdapterKeepaliveFailures covers every way the shim's answer is not
// a success.
func TestShimAdapterKeepaliveFailures(t *testing.T) {
	transportErr := errors.New("connection refused")
	tests := []struct {
		name    string
		resp    *shimv1.SetKeepaliveResponse
		err     error
		wantErr func(error) bool
	}{
		{
			name: "no session",
			resp: &shimv1.SetKeepaliveResponse{Result: &shimv1.SetKeepaliveResponse_Error{Error: &shimv1.SetKeepaliveError{
				Kind: &shimv1.SetKeepaliveError_NoSession{NoSession: &shimv1.SetKeepaliveNoSession{}},
			}}},
			wantErr: func(err error) bool { return errors.Is(err, cachelapse.ErrNoSession) },
		},
		{
			name:    "a refusal this daemon does not name",
			resp:    &shimv1.SetKeepaliveResponse{Result: &shimv1.SetKeepaliveResponse_Error{Error: &shimv1.SetKeepaliveError{}}},
			wantErr: func(err error) bool { var r *ShimRefusal; return errors.As(err, &r) && r.Verb == "SetKeepalive" },
		},
		{
			name:    "no outcome",
			resp:    &shimv1.SetKeepaliveResponse{},
			wantErr: func(err error) bool { return err != nil && !errors.Is(err, cachelapse.ErrNoSession) },
		},
		{
			name:    "a transport failure",
			err:     transportErr,
			wantErr: func(err error) bool { return errors.Is(err, transportErr) },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			adapter := &shimAdapter{client: &fakeKeepaliveTransport{resp: tt.resp, err: tt.err}}

			// Act.
			err := adapter.Hold(context.Background())

			// Assert.
			if !tt.wantErr(err) {
				t.Fatalf("Hold = %v, not the error this case wants", err)
			}
		})
	}
}

func TestAHiddenWorkspacesSessionStartsHeld(t *testing.T) {
	tests := []struct {
		name string
		edit func(*wsm.Workspace)
		held bool
	}{
		{name: "a visible workspace", edit: func(*wsm.Workspace) {}, held: false},
		{name: "a hidden workspace", edit: func(ws *wsm.Workspace) { ws.Hidden = true }, held: true},
		{name: "a merged hidden workspace", edit: func(ws *wsm.Workspace) {
			merged := time.Unix(100, 0)
			ws.Hidden, ws.MergedAt = true, &merged
		}, held: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			tt.edit(&ws)
			f.db.with(ws)

			// Act
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}

			// Assert
			if got := f.client.requests[0].GetHoldKeepalive() != nil; got != tt.held {
				t.Fatalf("hold_keepalive set = %v, want %v", got, tt.held)
			}
		})
	}
}

func TestAHiddenWorkspacesColdGateReopenStartsHeld(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	ws.Hidden = true
	f.db.with(ws)
	parkedGate(t, f, ws)

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if f.client.requests[1].GetHoldKeepalive() == nil {
		t.Fatalf("the re-open = %v, want its keep-alives held", f.client.requests[1])
	}
}

func TestAHiddenWorkspacesRolloutResumeStartsHeld(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	ws.Hidden = true
	f.db.with(ws)
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}

	// Act.
	if _, err := f.fleet.Resume(context.Background(), ws.ID, f.client); err != nil {
		t.Fatalf("Resume: %v", err)
	}

	// Assert.
	if f.client.requests[0].GetHoldKeepalive() == nil {
		t.Fatalf("the resume = %v, want its keep-alives held", f.client.requests[0])
	}
}

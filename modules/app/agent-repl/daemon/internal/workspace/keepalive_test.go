package workspace

import (
	"context"
	"errors"
	"testing"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/cachelapse"
	"agent-repld/internal/shimclient"
)

type fakeBeatTransport struct {
	shimclient.Client

	req  *shimv1.SendKeepaliveRequest
	resp *shimv1.SendKeepaliveResponse
	err  error
}

func (c *fakeBeatTransport) SendKeepalive(_ context.Context, req *shimv1.SendKeepaliveRequest) (*shimv1.SendKeepaliveResponse, error) {
	c.req = req
	return c.resp, c.err
}

func TestShimAdapterBeatAnswersTheShimsJudgment(t *testing.T) {
	// Arrange.
	success := &shimv1.SendKeepaliveSuccess{Outcome: &shimv1.SendKeepaliveSuccess_Sent{Sent: &shimv1.SendKeepaliveSent{}}}
	transport := &fakeBeatTransport{resp: &shimv1.SendKeepaliveResponse{Result: &shimv1.SendKeepaliveResponse_Success{Success: success}}}

	// Act.
	got, err := (&shimAdapter{client: transport}).Beat(context.Background())

	// Assert.
	if err != nil || got != success || transport.req == nil {
		t.Fatalf("Beat = %v, %v with request %v; want the shim's success, sent once", got, err, transport.req)
	}
}

// TestShimAdapterBeatFailures covers every way the shim's answer is not a
// judgment.
func TestShimAdapterBeatFailures(t *testing.T) {
	transportErr := errors.New("connection refused")
	refused := func(kind *shimv1.SendKeepaliveError) *shimv1.SendKeepaliveResponse {
		return &shimv1.SendKeepaliveResponse{Result: &shimv1.SendKeepaliveResponse_Error{Error: kind}}
	}
	tests := []struct {
		name    string
		resp    *shimv1.SendKeepaliveResponse
		err     error
		wantErr func(error) bool
	}{
		{
			name:    "no session",
			resp:    refused(&shimv1.SendKeepaliveError{Kind: &shimv1.SendKeepaliveError_NoSession{NoSession: &shimv1.SendKeepaliveNoSession{}}}),
			wantErr: func(err error) bool { return errors.Is(err, cachelapse.ErrNoSession) },
		},
		{
			name: "the submission failed",
			resp: refused(&shimv1.SendKeepaliveError{Kind: &shimv1.SendKeepaliveError_SubmitFailed{SubmitFailed: &shimv1.SendKeepaliveSubmitFailed{Detail: "stream closed"}}}),
			wantErr: func(err error) bool {
				var failed *cachelapse.BeatSubmitFailed
				return errors.As(err, &failed) && failed.Detail == "stream closed"
			},
		},
		{
			name:    "a refusal this daemon does not name",
			resp:    refused(&shimv1.SendKeepaliveError{}),
			wantErr: func(err error) bool { var r *ShimRefusal; return errors.As(err, &r) && r.Verb == "SendKeepalive" },
		},
		{
			name:    "no outcome",
			resp:    &shimv1.SendKeepaliveResponse{},
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
			adapter := &shimAdapter{client: &fakeBeatTransport{resp: tt.resp, err: tt.err}}

			// Act.
			_, err := adapter.Beat(context.Background())

			// Assert.
			if !tt.wantErr(err) {
				t.Fatalf("Beat = %v, not the error this case wants", err)
			}
		})
	}
}

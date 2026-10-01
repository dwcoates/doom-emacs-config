package shimclient

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

// validRollBackSessionRequest is the smallest legal RollBackSession.
func validRollBackSessionRequest() *shimv1.RollBackSessionRequest {
	toBefore := &conversationv1.TurnId{Value: "turn-1"}
	return &shimv1.RollBackSessionRequest{
		ToBefore:     toBefore,
		DroppedTurns: []*conversationv1.TurnId{toBefore},
		Files: &shimv1.RollBackSessionRequest_RestoreFiles{
			RestoreFiles: &shimv1.RollBackSessionRestoreFiles{},
		},
	}
}

// TestValidateRollBackSessionRequest covers each way a rollback can be
// illegal.
func TestValidateRollBackSessionRequest(t *testing.T) {
	tests := []struct {
		name  string
		req   *shimv1.RollBackSessionRequest
		field string
	}{
		{
			name:  "nil request",
			req:   nil,
			field: "RollBackSessionRequest",
		},
		{
			name: "no to_before",
			req: func() *shimv1.RollBackSessionRequest {
				req := validRollBackSessionRequest()
				req.ToBefore = nil
				return req
			}(),
			field: "RollBackSessionRequest.to_before",
		},
		{
			name: "empty dropped_turns",
			req: func() *shimv1.RollBackSessionRequest {
				req := validRollBackSessionRequest()
				req.DroppedTurns = nil
				return req
			}(),
			field: "RollBackSessionRequest.dropped_turns",
		},
		{
			name: "dropped_turns does not begin with to_before",
			req: func() *shimv1.RollBackSessionRequest {
				req := validRollBackSessionRequest()
				req.DroppedTurns = []*conversationv1.TurnId{{Value: "turn-other"}}
				return req
			}(),
			field: "RollBackSessionRequest.dropped_turns",
		},
		{
			name: "dropped_turns holds an empty turn",
			req: func() *shimv1.RollBackSessionRequest {
				req := validRollBackSessionRequest()
				req.DroppedTurns = []*conversationv1.TurnId{req.ToBefore, {}}
				return req
			}(),
			field: "RollBackSessionRequest.dropped_turns.value",
		},
		{
			name: "files oneof unset",
			req: func() *shimv1.RollBackSessionRequest {
				req := validRollBackSessionRequest()
				req.Files = nil
				return req
			}(),
			field: "RollBackSessionRequest.files",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateRollBackSessionRequest(tc.req)

			// Assert.
			assertInvalidField(t, err, tc.field)
		})
	}
}

// TestValidateRollBackSessionRequestAccepts asserts both legal files arms
// pass.
func TestValidateRollBackSessionRequestAccepts(t *testing.T) {
	tests := []struct {
		name string
		req  *shimv1.RollBackSessionRequest
	}{
		{
			name: "restore files",
			req:  validRollBackSessionRequest(),
		},
		{
			name: "keep files",
			req: func() *shimv1.RollBackSessionRequest {
				req := validRollBackSessionRequest()
				req.Files = &shimv1.RollBackSessionRequest_KeepFiles{
					KeepFiles: &shimv1.RollBackSessionKeepFiles{},
				}
				return req
			}(),
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateRollBackSessionRequest(tc.req)

			// Assert.
			if err != nil {
				t.Fatalf("validateRollBackSessionRequest() = %v, want nil", err)
			}
		})
	}
}

// TestRollBackSessionReachesTheShim asserts a valid rollback dials the shim
// like every other unary verb and the success response comes back.
func TestRollBackSessionReachesTheShim(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds)

	// Act.
	resp, err := client.RollBackSession(context.Background(), validRollBackSessionRequest())

	// Assert.
	if err != nil {
		t.Fatalf("RollBackSession() error = %v", err)
	}
	if resp == nil {
		t.Fatal("RollBackSession() returned a nil response alongside a nil error")
	}
	if got := f.count("RollBackSession"); got != 1 {
		t.Fatalf("RollBackSession calls = %d, want 1", got)
	}
}

// TestRollBackSessionSurfacesTheShimsError asserts a refusal the shim answers
// comes back from the call rather than being swallowed.
func TestRollBackSessionSurfacesTheShimsError(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	wantErr := errors.New("vendor refused the cut")
	f.rollBackSessionErr = wantErr
	client := adoptReady(t, f, dir, uds)

	// Act.
	resp, err := client.RollBackSession(context.Background(), validRollBackSessionRequest())

	// Assert.
	if err == nil {
		t.Fatal("RollBackSession() error = nil, want the shim's refusal")
	}
	if resp != nil {
		t.Fatal("RollBackSession() returned a response alongside an error")
	}
	if got := f.count("RollBackSession"); got != 1 {
		t.Fatalf("RollBackSession calls = %d, want 1", got)
	}
}

// TestRollBackSessionRefusesALocallyInvalidRequest asserts base-function
// validation refuses before the wire, as every other unary verb does.
func TestRollBackSessionRefusesALocallyInvalidRequest(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds)

	// Act.
	_, err := client.RollBackSession(context.Background(), &shimv1.RollBackSessionRequest{})

	// Assert.
	var invalidErr *InvalidRequestError
	if !errors.As(err, &invalidErr) {
		t.Fatalf("RollBackSession() error = %v, want *InvalidRequestError", err)
	}
	if got := f.count("RollBackSession"); got != 0 {
		t.Fatalf("RollBackSession calls = %d, want 0", got)
	}
}

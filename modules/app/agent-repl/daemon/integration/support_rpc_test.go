package integration

import (
	"fmt"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

// fataler is the slice of *testing.T requireRPCSuccess reports through, so
// its own failure arms can be tested without failing the test that tests them.
type fataler interface {
	Helper()
	Fatalf(format string, args ...any)
}

// requireRPCAnswered fails the test when an rpc did not answer at all, and is
// the ONE step every assertion on an rpc's answer takes before it reads
// resp.Msg.
//
// A failed rpc answers a NIL response. The hand-rolled
// `if err != nil || resp.Msg... { t.Fatalf(..., resp.Msg, err) }` formatted
// resp.Msg into its own failure line, so a failed rpc PANICKED instead of
// failing, which killed the whole package run and with it every sibling
// test's verdict and the failing test's preserved logs.
func requireRPCAnswered[T any](t fataler, label string, resp *connect.Response[T], err error) bool {
	t.Helper()
	if err != nil {
		t.Fatalf("%s = error %v, want an answer", label, err)
		return false
	}
	return true
}

// requireRPCSuccess fails the test unless an rpc SUCCEEDED, and answers its
// message: it answered (requireRPCAnswered), and the response's `error` arm is
// unset.
func requireRPCSuccess[T any, PT interface {
	*T
	proto.Message
}](t fataler, label string, resp *connect.Response[T], err error) *T {
	t.Helper()
	if !requireRPCAnswered(t, label, resp, err) {
		return nil
	}
	msg := PT(resp.Msg).ProtoReflect()
	if field := msg.Descriptor().Fields().ByName("error"); field != nil && msg.Has(field) {
		t.Fatalf("%s = %v, want a success", label, resp.Msg)
		return nil
	}
	return resp.Msg
}

// recordingFataler records the failure requireRPCSuccess reports instead of
// ending the test.
type recordingFataler struct{ failure string }

func (r *recordingFataler) Helper() {}

func (r *recordingFataler) Fatalf(format string, args ...any) {
	r.failure = fmt.Sprintf(format, args...)
}

func TestRequireRPCSuccess(t *testing.T) {
	tests := []struct {
		name        string
		resp        *connect.Response[agentreplv1.UpdateMergeQueueResponse]
		err         error
		wantFailure string
		wantMsg     bool
	}{
		{
			name:        "a transport error with a nil response fails without reading it",
			resp:        nil,
			err:         fmt.Errorf("unavailable"),
			wantFailure: "pause = error unavailable, want an answer",
		},
		{
			name: "an error arm fails",
			resp: connect.NewResponse(&agentreplv1.UpdateMergeQueueResponse{Result: &agentreplv1.UpdateMergeQueueResponse_Error{
				Error: &agentreplv1.UpdateMergeQueueError{}}}),
			wantFailure: "pause = ",
		},
		{
			name: "a success answers the message",
			resp: connect.NewResponse(&agentreplv1.UpdateMergeQueueResponse{Result: &agentreplv1.UpdateMergeQueueResponse_Success{
				Success: &agentreplv1.UpdateMergeQueueSuccess{}}}),
			wantMsg: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			rec := &recordingFataler{}

			// Act.
			msg := requireRPCSuccess(rec, "pause", tt.resp, tt.err)

			// Assert.
			if tt.wantFailure == "" && rec.failure != "" {
				t.Fatalf("failure = %q, want none", rec.failure)
			}
			if tt.wantFailure != "" && (len(rec.failure) < len(tt.wantFailure) || rec.failure[:len(tt.wantFailure)] != tt.wantFailure) {
				t.Fatalf("failure = %q, want it to start %q", rec.failure, tt.wantFailure)
			}
			if (msg != nil) != tt.wantMsg {
				t.Fatalf("msg = %v, want present=%v", msg, tt.wantMsg)
			}
		})
	}
}

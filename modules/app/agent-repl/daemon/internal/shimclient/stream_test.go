package shimclient

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"connectrpc.com/connect"
)

// TestAMidRunReceiveTheShimAnsweredUnavailableIsStoreUnreachable pins that a
// stream the shim ends mid-run with its own wire unavailable answer reads as
// the store's outage, exactly as a refused open does, with the connect error
// still in the chain; any other wire code is returned as it came.
func TestAMidRunReceiveTheShimAnsweredUnavailableIsStoreUnreachable(t *testing.T) {
	tests := []struct {
		name            string
		end             error
		wantStoreOutage bool
		wantCode        connect.Code
	}{
		{
			name:            "the shim's wire unavailable answer",
			end:             connect.NewError(connect.CodeUnavailable, errors.New("WatchBash(toolu_1): the store is unreachable")),
			wantStoreOutage: true,
			wantCode:        connect.CodeUnavailable,
		},
		{
			name:            "another wire code",
			end:             connect.NewError(connect.CodeInternal, errors.New("boom")),
			wantStoreOutage: false,
			wantCode:        connect.CodeInternal,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			dir := shortDir(t)
			f, uds := startFakeShim(t, dir)
			f.watchBashEnd = tt.end
			client := adoptReady(t, f, dir, uds)
			stream, err := client.WatchBash(context.Background(), &conversationv1.DetachedWorkId{Value: "toolu_1"})
			if err != nil {
				t.Fatalf("WatchBash() error = %v", err)
			}
			t.Cleanup(stream.Close)
			if _, err := stream.Recv(); err != nil {
				t.Fatalf("first Recv() error = %v, want the opening frame", err)
			}

			// Act.
			_, err = stream.Recv()

			// Assert.
			if got := errors.Is(err, ErrStoreUnreachable); got != tt.wantStoreOutage {
				t.Fatalf("errors.Is(%v, ErrStoreUnreachable) = %v, want %v", err, got, tt.wantStoreOutage)
			}
			if got := connect.CodeOf(err); got != tt.wantCode {
				t.Fatalf("connect.CodeOf(%v) = %v, want %v", err, got, tt.wantCode)
			}
		})
	}
}

// TestReceiveFailureMarksOnlyTheShimsWireAnswer pins that a receive failure
// this client made itself is never mistaken for the shim's store outage: an
// unavailable that never crossed the wire (a transport cut) and a failure
// with no connect code are returned as they came.
func TestReceiveFailureMarksOnlyTheShimsWireAnswer(t *testing.T) {
	tests := []struct {
		name string
		err  error
	}{
		{name: "a client-synthesized unavailable", err: connect.NewError(connect.CodeUnavailable, errors.New("read: connection reset"))},
		{name: "no connect error", err: errors.New("unexpected EOF")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := receiveFailure(tt.err)

			// Assert.
			if got != tt.err {
				t.Fatalf("receiveFailure(%v) = %v, want the failure as it came", tt.err, got)
			}
			if errors.Is(got, ErrStoreUnreachable) {
				t.Fatalf("receiveFailure(%v) = %v, want no ErrStoreUnreachable in its chain", tt.err, got)
			}
		})
	}
}

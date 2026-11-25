package server

import (
	"context"
	"fmt"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// receivePush reads STREAM until PICK answers a non-nil value for a push, and
// answers that value. PICK answering nil skips the push. The test fails,
// naming WHAT it waited for, when the stream ends first. Every receive*
// reader in this package's tests is a PICK over this one loop.
func receivePush[R, V any](
	t testing.TB,
	stream *connect.ServerStreamForClient[R],
	what string,
	pick func(*R) *V,
) *V {
	t.Helper()
	for stream.Receive() {
		if v := pick(stream.Msg()); v != nil {
			return v
		}
	}
	t.Fatalf("the stream ended before %s arrived: %v", what, stream.Err())
	return nil
}

// fatalRecorder is a testing.TB whose Fatalf records the message and
// returns, so a test can observe receivePush's failure without failing.
type fatalRecorder struct {
	testing.TB
	fatals []string
}

func (r *fatalRecorder) Helper() {}

func (r *fatalRecorder) Fatalf(format string, args ...any) {
	r.fatals = append(r.fatals, fmt.Sprintf(format, args...))
}

func TestReceivePushAnswersTheFirstPushPickAccepts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	stream := dialHostStream(t, h, t.Context())
	markHostStream(h, testWorkspaceID)
	h.Server.(*server).hostEventStream(testWorkspaceID).Emit(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_FinalResponseClicked{
			FinalResponseClicked: &agentreplv1.HostWorkspaceFinalResponseClicked{},
		},
	})

	// Act — the host state and the mark are skipped.
	got := receivePush(t, stream, "a final-response click",
		func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.HostWorkspaceFinalResponseClicked {
			return r.GetFinalResponseClicked()
		})

	// Assert
	if got == nil {
		t.Fatalf("receivePush answered nil, want the final-response click")
	}
}

func TestReceivePushFailsNamingWhatItWaitedFor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	stream := dialHostStream(t, h, ctx)
	cancel()
	rec := &fatalRecorder{TB: t}

	// Act
	got := receivePush(rec, stream, "a never-sent push",
		func(*agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.HostWorkspaceFinalResponseClicked {
			return nil
		})

	// Assert
	if got != nil || len(rec.fatals) != 1 || !strings.Contains(rec.fatals[0], "before a never-sent push arrived") {
		t.Fatalf("answer = %v, fatals = %q; want nil and one failure naming the push", got, rec.fatals)
	}
}

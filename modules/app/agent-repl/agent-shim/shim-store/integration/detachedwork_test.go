// detachedwork_test.go — GetDetachedWork over the socket: the kind and the end
// of the detached work one unit left, as the record holds them.
package integration

import (
	"testing"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

func TestGetDetachedWorkAnswersEachUnitByItsRecord(t *testing.T) {
	tests := []struct {
		name   string
		unit   string
		assert func(t *testing.T, resp *storev1.GetDetachedWorkResponse)
	}{
		{name: "an ended shell run", unit: "run-ended", assert: func(t *testing.T, resp *storev1.GetDetachedWorkResponse) {
			if s := resp.GetSuccess(); s.GetKind().GetBash() == nil || s.GetEnded().GetEndedAtMs() <= 0 {
				t.Fatalf("result = %v, want a bash run with its end instant", resp.GetResult())
			}
		}},
		{name: "a live shell run", unit: "run-live", assert: func(t *testing.T, resp *storev1.GetDetachedWorkResponse) {
			if s := resp.GetSuccess(); s.GetKind().GetBash() == nil || s.GetLive() == nil {
				t.Fatalf("result = %v, want a live bash run", resp.GetResult())
			}
		}},
		{name: "a unit no row locates", unit: "run-unknown", assert: func(t *testing.T, resp *storev1.GetDetachedWorkResponse) {
			if resp.GetNotFound() == nil {
				t.Fatalf("result = %v, want not_found", resp.GetResult())
			}
		}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			cli := store.client()
			sidecar := fileProducer(cli)
			sidecar.write(ctx, t,
				sidecar.agentEntry("w-ended-start", "bash:run-ended:start", bashRun(nil, "run-ended", bashStart("make", 1000))),
				sidecar.agentEntry("w-ended-end", "bash:run-ended:terminal", bashRun(nil, "run-ended", bashSuccess("make", 0))),
				sidecar.agentEntry("w-live-start", "bash:run-live:start", bashRun(nil, "run-live", bashStart("make", 1000))),
			)

			// Act.
			resp, err := cli.GetDetachedWork(ctx, connect.NewRequest(&storev1.GetDetachedWorkRequest{
				Unit: &conversationv1.AgentActivityId{Value: test.unit},
			}))

			// Assert.
			if err != nil {
				t.Fatalf("GetDetachedWork transport error: %v", err)
			}
			test.assert(t, resp.Msg)
			store.assertNoErrorRecords()
		})
	}
}

func TestGetDetachedWorkRefusesAnUnsetUnit(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()

	// Act.
	resp, err := cli.GetDetachedWork(ctx, connect.NewRequest(&storev1.GetDetachedWorkRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("GetDetachedWork transport error: %v", err)
	}
	if got := resp.Msg.GetFailure().GetInvalidRequest().GetField(); got != "unit" {
		t.Fatalf("failure = %v, want invalid_request naming unit", resp.Msg.GetResult())
	}
}

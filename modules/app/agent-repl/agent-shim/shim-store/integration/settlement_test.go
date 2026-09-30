// settlement_test.go — GetRunSettlements over the socket: which detached runs
// the record already holds as ended. A run absent from the answer is not
// settled, whether it has no row or its row is still live.
package integration

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
)

// runSettlements asks the store over the socket and fails the test on
// anything but the success arm.
func runSettlements(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, runs ...string) []*storev1.RunSettlement {
	t.Helper()
	resp, err := cli.GetRunSettlements(ctx, connect.NewRequest(&storev1.GetRunSettlementsRequest{RunIds: runs}))
	if err != nil {
		t.Fatalf("GetRunSettlements transport error: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("GetRunSettlements answered no success: %v", resp.Msg)
	}
	return success.GetSettled()
}

func TestRunSettlementsAnswersAnEndedRunAndLeavesOutLiveAndUnknownOnes(t *testing.T) {
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
	settled := runSettlements(ctx, t, cli, "run-ended", "run-live", "run-unknown")

	// Assert.
	if len(settled) != 1 || settled[0].GetRunId() != "run-ended" || settled[0].GetEndedAtMs() <= 0 {
		t.Fatalf("settled = %v, want only run-ended with its end instant", settled)
	}
	store.assertNoErrorRecords()
}

func TestRunSettlementsRefusesAnEmptyRunID(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()

	// Act.
	resp, err := cli.GetRunSettlements(ctx, connect.NewRequest(&storev1.GetRunSettlementsRequest{RunIds: []string{""}}))

	// Assert.
	if err != nil {
		t.Fatalf("GetRunSettlements transport error: %v", err)
	}
	if got := resp.Msg.GetFailure().GetInvalidRequest().GetField(); got != "run_ids[0]" {
		t.Fatalf("failure = %v, want invalid_request naming run_ids[0]", resp.Msg.GetResult())
	}
}

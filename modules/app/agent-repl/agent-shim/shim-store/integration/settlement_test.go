// settlement_test.go — GetRunSettlements over the socket: which detached runs
// the record already holds as ended. A run absent from the answer is not
// settled, whether it has no row or its row is still live.
package integration

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
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

// bashLostFrame is the sidecar's LOST terminal for a shell run.
func bashLostFrame() *conversationv1.AgentBash {
	return &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
		Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
			Cause: &conversationv1.AgentBashInterrupted_Lost{Lost: &conversationv1.DetachedLost{
				How: &conversationv1.DetachedLost_WentSilent{WentSilent: &conversationv1.DetachedLostWentSilent{}},
			}},
		}},
	}}}
}

func TestALostTerminalOverAnEndedRunIsRefusedAndRecordedOnceAtError(t *testing.T) {
	// Arrange: the run ended on its spool's own terminator.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	sidecar.write(ctx, t,
		sidecar.agentEntry("w-exit", "bash:run-done:terminal", bashRun(nil, "run-done", bashSuccess("make", 0))))
	before := runSettlements(ctx, t, cli, "run-done")
	mark := store.logMark()

	// Act
	failure := sidecar.writeExpectingFailure(ctx, t, nil,
		sidecar.agentEntry("w-lost", "bash:run-done:terminal", bashRun(nil, "run-done", bashLostFrame())))

	// Assert: refused naming the entry, recorded once at ERROR under the
	// site, and the run's recorded ending stands.
	assertWriteInvalidRequest(t, failure, "entries[0]")
	rec := assertExactlyOneNormalRecordAtLevel(t, recordsAtOperation(store.logRecordsAfter(mark), "store.rpc.write-batch"), "the refused LOST terminal", "error")
	assertRefusalKeys(t, rec, "lost_over_settled", "invalid_request")
	after := runSettlements(ctx, t, cli, "run-done")
	if len(before) != 1 || len(after) != 1 || after[0].GetEndedAtMs() != before[0].GetEndedAtMs() {
		t.Fatalf("settlement before = %v after = %v, want the original ending to stand", before, after)
	}
}

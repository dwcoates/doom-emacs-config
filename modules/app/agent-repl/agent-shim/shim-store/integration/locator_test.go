// locator_test.go — the vendor task locator pairing, end to end over the
// socket: the sidecar states a subagent's locator with the agent's rows, and a
// shim that never saw the spawn resolves the locator to the agent, scoped to
// its own lineage.
package integration

import (
	"testing"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// agentByVendorTask asks which agent of `session`'s lineage a locator names.
func agentByVendorTask(t *testing.T, store *storeProcess, session, task string) *storev1.GetAgentByVendorTaskResponse {
	t.Helper()
	ctx, cancel := callContext(t)
	defer cancel()
	resp, err := store.client().GetAgentByVendorTask(ctx, connect.NewRequest(&storev1.GetAgentByVendorTaskRequest{
		Session: agentID(session), VendorTaskId: task,
	}))
	if err != nil {
		t.Fatalf("GetAgentByVendorTask transport error: %v", err)
	}
	return resp.Msg
}

// bookSubagentWithLocator writes, as the two planes do, a spawn of `agent` by
// `main` on the stream plane and the sidecar's batch pairing `task` with it.
func bookSubagentWithLocator(t *testing.T, store *storeProcess, main, agent, task string) {
	t.Helper()
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t, shim.agentEntry("w-spawn-"+agent, "u-spawn-"+agent,
		frameLine(agentID(main), subagentSpawnFrame(main, "act-"+agent, agent, "go do it", 1000))))
	sidecar := fileProducer(cli)
	resp, err := sidecar.attempt(ctx, &storev1.EntryBatch{
		Entries: []*storev1.StoreEntry{sidecar.agentEntry("w-line-"+agent, "u-line-"+agent,
			frameLine(agentID(agent), responseFrame(agent, "act-line-"+agent, "working")))},
		CursorAdvance: cursorState("1:"+task, "/tmp/agent-"+task+".jsonl", 10, nil),
		AgentLocators: []*storev1.AgentLocator{{VendorTaskId: task, Agent: &conversationv1.AgentId{Value: agent}}},
	})
	if err != nil {
		t.Fatalf("WriteBatch transport error: %v", err)
	}
	if resp.GetSuccess() == nil {
		t.Fatalf("WriteBatch refused the sidecar's batch: %v", resp)
	}
}

func TestAWrittenLocatorResolvesToItsAgentWithinTheLineage(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	bookSubagentWithLocator(t, store, "main", "toolu_spawn", "a1b2")

	// Act.
	resp := agentByVendorTask(t, store, "main", "a1b2")

	// Assert.
	if got := resp.GetSuccess().GetAgent().GetValue(); got != "toolu_spawn" {
		t.Fatalf("answer = %v, want success naming toolu_spawn", resp)
	}
	store.assertNoErrorRecords()
}

func TestALocatorOfAnotherSessionAnswersNotFound(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	bookSubagentWithLocator(t, store, "other-main", "toolu_spawn", "a1b2")

	// Act.
	resp := agentByVendorTask(t, store, "main", "a1b2")

	// Assert.
	if resp.GetNotFound() == nil {
		t.Fatalf("answer = %v, want not_found: the pairing is another session's", resp)
	}
	store.assertNoErrorRecords()
}

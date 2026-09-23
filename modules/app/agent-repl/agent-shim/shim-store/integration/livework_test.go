// livework_test.go — SUBJECT 8: GetLiveWork, the open obligations.
//
// "Live" is a claim about the RECORD — a start was written and no terminal
// ever was — which is timeless and cannot go stale. It is the ended_at IS NULL
// scan across the agent and detached_work tables, and it deliberately never
// lists a main agent: a main agent has no spawner, and nothing is owed for it.
package integration

import (
	"testing"

	"connectrpc.com/connect"

	storev1 "agentrepl/proto/store/v1"
)

// TestLiveWorkListsASpawnedAgentUntilItsTerminal.
func TestLiveWorkListsASpawnedAgentUntilItsTerminal(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-lw-spawn", "u-lw-spawn",
			frameLine(agentID("main"), subagentSpawnFrame("main", "act-spawn", "sub-1", "go do it", 1000))),
	)

	// Assert.
	if got := agentValues(liveWork(ctx, t, cli, "main").GetLiveAgents()); !contains(got, "sub-1") {
		t.Errorf("a started agent is missing from live_agents: %v", got)
	}
	store.assertNoErrorRecords()
}

// TestLiveWorkNeverListsAMainAgent: a main agent has neither spawner, so it is
// never an open obligation of anyone's.
func TestLiveWorkNeverListsAMainAgent(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-lw-main", "u-lw-main", frameLine(agentID("main"), responseFrame("main", "act-1", "working"))),
	)

	// Assert.
	if got := agentValues(liveWork(ctx, t, cli, "main").GetLiveAgents()); contains(got, "main") {
		t.Errorf("GetLiveWork listed the main agent: %v", got)
	}
	store.assertNoErrorRecords()
}

// TestLiveWorkScansDetachedWorkBesideAgents: the answer spans both tables in
// one observation.
func TestLiveWorkScansDetachedWorkBesideAgents(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-lw-both-1", "u-lw-both-1",
			frameLine(agentID("main"), subagentSpawnFrame("main", "act-spawn", "sub-live", "work", 2000))),
		shim.agentEntry("w-lw-both-2", "detached:work-bash-live",
			frameLine(agentID("main"), detachedBashFrame("main", "work-bash-live", "tail -f log", 2100))),
	)

	// Assert.
	live := liveWork(ctx, t, cli, "main")
	if got := agentValues(live.GetLiveAgents()); !contains(got, "sub-live") {
		t.Errorf("live_agents is missing the started subagent: %v", got)
	}
	if got := workValues(live.GetLiveDetached()); !contains(got, "work-bash-live") {
		t.Errorf("live_detached is missing the announced run: %v", got)
	}
	store.assertNoErrorRecords()
}

// TestLiveWorkListsNoWorkflowsThisWave: nothing routes into the workflow
// table, so the list is empty by construction.
func TestLiveWorkListsNoWorkflowsThisWave(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-lw-wf", "u-lw-wf",
			workflowRun(agentID("main"), "run-agent-1", workflowStartFrame("nightly", 3000))),
	)

	// Assert.
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveWorkflows()); len(got) != 0 {
		t.Errorf("live_workflows is %v, want empty for a wave where nothing routes into the workflow table", got)
	}
}

// TestLiveWorkSurvivesARestart: the claim is about the record, so it is
// answered from durable state and not from anything the process held.
func TestLiveWorkSurvivesARestart(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t,
		shim.agentEntry("w-lw-restart", "u-lw-restart",
			frameLine(agentID("main"), subagentSpawnFrame("main", "act-spawn", "sub-restart", "work", 4000))),
	)

	// Act.
	store.restart()

	// Assert.
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	if got := agentValues(liveWork(after, t, store.client(), "main").GetLiveAgents()); !contains(got, "sub-restart") {
		t.Errorf("an open obligation did not survive a restart: live_agents is %v", got)
	}
	store.assertNoErrorRecords()
}

// TestLiveDetachedSurvivesARestart: the detached side of the same claim. A
// store bounce mid-run must not silently retire an obligation the shim is still
// holding a spool open for — the answer comes from the lifecycle table, which
// is on disk, and not from anything the dead process knew.
func TestLiveDetachedSurvivesARestart(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t,
		shim.agentEntry("w-lwd-restart", "detached:work-restart",
			frameLine(agentID("main"), detachedBashFrame("main", "work-restart", "tail -f log", 5000))),
	)

	// Act.
	store.restart()

	// Assert.
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	if got := workValues(liveWork(after, t, store.client(), "main").GetLiveDetached()); !contains(got, "work-restart") {
		t.Errorf("an announced run did not survive a restart: live_detached is %v", got)
	}
	store.assertNoErrorRecords()
}

// TestARunConcludedBeforeARestartStaysClosedAcrossIt is the negative half: the
// terminal is as durable as the announcement, so a bounce must not resurrect a
// run the record already closed.
func TestARunConcludedBeforeARestartStaysClosedAcrossIt(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t,
		shim.agentEntry("w-lwd-closed-1", "detached:work-closed",
			frameLine(agentID("main"), detachedBashFrame("main", "work-closed", "make test", 5100))),
		shim.agentEntry("w-lwd-closed-2", "bash:work-closed:terminal",
			bashRun(agentID("main"), "work-closed", bashSuccess("make test", 0))),
	)

	// Act.
	store.restart()

	// Assert.
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	if got := workValues(liveWork(after, t, store.client(), "main").GetLiveDetached()); contains(got, "work-closed") {
		t.Errorf("a restart resurrected a concluded run: live_detached is %v", got)
	}
	store.assertNoErrorRecords()
}

// TestLiveWorkAnswersEachSessionOnlyItsOwnObligations is the 2026-09-23
// defect's store half: ONE store serves every session on the host, and a
// session's read must never hand it another session's running work to close.
func TestLiveWorkAnswersEachSessionOnlyItsOwnObligations(t *testing.T) {
	tests := []struct {
		name      string
		session   string
		wantAgent string
		wantRun   string
		notAgent  string
		notRun    string
	}{
		{name: "session A", session: "main-a", wantAgent: "sub-a", wantRun: "run-a", notAgent: "sub-b", notRun: "run-b"},
		{name: "session B", session: "main-b", wantAgent: "sub-b", wantRun: "run-b", notAgent: "sub-a", notRun: "run-a"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: two sessions' live subagents and shell runs, one store.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			cli := store.client()
			shim := streamProducer(cli)
			shim.write(ctx, t,
				shim.agentEntry("w-2s-a1", "u-2s-a1", frameLine(agentID("main-a"), subagentSpawnFrame("main-a", "act-a", "sub-a", "work", 1000))),
				shim.agentEntry("w-2s-a2", "detached:run-a", frameLine(agentID("main-a"), detachedBashFrame("main-a", "run-a", "sleep 60", 1100))),
				shim.agentEntry("w-2s-b1", "u-2s-b1", frameLine(agentID("main-b"), subagentSpawnFrame("main-b", "act-b", "sub-b", "work", 1200))),
				shim.agentEntry("w-2s-b2", "detached:run-b", frameLine(agentID("main-b"), detachedBashFrame("main-b", "run-b", "sleep 60", 1300))),
			)

			// Act.
			live := liveWork(ctx, t, cli, tc.session)

			// Assert.
			agents, runs := agentValues(live.GetLiveAgents()), workValues(live.GetLiveDetached())
			if !contains(agents, tc.wantAgent) || contains(agents, tc.notAgent) {
				t.Errorf("live_agents = %v, want %s and never %s", agents, tc.wantAgent, tc.notAgent)
			}
			if !contains(runs, tc.wantRun) || contains(runs, tc.notRun) {
				t.Errorf("live_detached = %v, want %s and never %s", runs, tc.wantRun, tc.notRun)
			}
			store.assertNoErrorRecords()
		})
	}
}

// TestLiveWorkRefusesARequestNamingNoSession: there is no unscoped answer. The
// refusal is the typed invalid_request arm naming `session`, recorded once.
func TestLiveWorkRefusesARequestNamingNoSession(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	mark := store.logMark()

	// Act.
	resp, err := store.client().GetLiveWork(ctx, connect.NewRequest(&storev1.GetLiveWorkRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("GetLiveWork answered a transport error where a typed failure was owed: %v", err)
	}
	if got := resp.Msg.GetFailure().GetInvalidRequest().GetField(); got != "session" {
		t.Fatalf("result = %v, want invalid_request naming session", resp.Msg.GetResult())
	}
	rec := assertExactlyOneNormalRecord(t, store.logRecordsAfter(mark), "an unscoped live-work read")
	assertRefusalKeys(t, rec, "session_empty", "invalid_request")
}

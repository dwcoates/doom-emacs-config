// livework_test.go — SUBJECT 8: GetLiveWork, the open obligations.
//
// "Live" is a claim about the RECORD — a start was written and no terminal
// ever was — which is timeless and cannot go stale. It is the ended_at IS NULL
// scan across the agent and detached_work tables, and it deliberately never
// lists a main agent: a main agent has no spawner, and nothing is owed for it.
package integration

import "testing"

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
	if got := agentValues(liveWork(ctx, t, cli).GetLiveAgents()); !contains(got, "sub-1") {
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
	if got := agentValues(liveWork(ctx, t, cli).GetLiveAgents()); contains(got, "main") {
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
		shim.agentEntry("w-lw-both-2", "u-lw-both-2",
			frameLine(agentID("main"), detachedBashFrame("main", "work-bash-live", "tail -f log", 2100))),
	)

	// Assert.
	live := liveWork(ctx, t, cli)
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
	if got := workValues(liveWork(ctx, t, cli).GetLiveWorkflows()); len(got) != 0 {
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
	if got := agentValues(liveWork(after, t, store.client()).GetLiveAgents()); !contains(got, "sub-restart") {
		t.Errorf("an open obligation did not survive a restart: live_agents is %v", got)
	}
	store.assertNoErrorRecords()
}

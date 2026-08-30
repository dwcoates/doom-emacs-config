package convert

// subagent_test.go — the spawn, and the agent it created.

import "testing"

const agentResult = `{"agentId":"aef975b7bc3422d4b","agentType":"general-purpose","status":"completed",` +
	`"content":[{"type":"text","text":"the report"}],"prompt":"do the thing","resolvedModel":"claude-opus-5",` +
	`"totalDurationMs":134000,"totalToolUseCount":18,"totalTokens":4242,` +
	`"toolStats":{"readCount":3,"searchCount":1,"bashCount":2,"editFileCount":4,"linesAdded":40,"linesRemoved":5,"otherToolCount":8,"frameCount":11},` +
	`"usage":{"input_tokens":10,"cache_creation_input_tokens":20,"cache_read_input_tokens":30,"output_tokens":40}}`

func TestSpawnUnitAnnouncesOnlyOnceTheCreatedAgentIdIsKnown(t *testing.T) {
	// Arrange. created_agent_id is the JOIN KEY THE WHOLE FLAT MODEL RESTS ON and
	// it is NOT optional, but the vendor names the created agent only in the
	// launch's answer. Announcing at the call with it unset would break presence;
	// inventing one would break identity. So the unit appears at its settle.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"do the thing"}`))

	// Act: the call alone.
	callOnly := convertLines(t, c, call)

	// Assert.
	for _, e := range callOnly {
		if activityOf(e).GetSubagent() != nil {
			t.Fatal("the spawn must not be announced before its created agent id is known")
		}
	}
}

func TestSettledSpawnCarriesTheCreatedAgentIdAndTheOriginalInstant(t *testing.T) {
	// Arrange. The settled frame carries the ORIGINAL call instant, so a drawn
	// clock measures the spawn rather than the answer.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", "2026-07-22T19:58:36.000Z",
		toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"do the thing","subagent_type":"Explore"}`))
	result := toolResultLine("u1", "toolu_spawn", "2026-07-22T20:00:50.000Z", `[{"type":"text","text":"the report"}]`, agentResult)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	settled := entryByKey(t, entries, ActivityKey("toolu_spawn"))
	success := activityOf(settled).GetSubagent().GetSuccess()
	if success == nil {
		t.Fatal("a returned spawn must settle on the success arm")
	}
	if got := success.GetPrompt().GetSubagentType(); got != "Explore" {
		t.Fatalf("subagent_type = %q, want the requested type from the CALL", got)
	}
	if got := success.GetReport().GetProse().GetMarkdown(); got != "the report" {
		t.Fatalf("report = %q, want the subagent's own words", got)
	}
	if got := success.GetSettledAt().GetAtMs(); got != 1784750450000 {
		t.Fatalf("settled_at = %d, want the RESULT record's instant", got)
	}
}

func TestSyncSpawnCarriesTheFullBilledBreakdown(t *testing.T) {
	// Arrange. THE SET ARM IS THE SPAWN PATH'S HONESTY: a sync completion carries
	// the full breakdown; an async one carries at most a total-tokens scalar, so a
	// full breakdown on an async run is unproducible and unrepresentable.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
	result := toolResultLine("u1", "toolu_spawn", ts2, `[{"type":"text","text":"r"}]`, agentResult)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	totals := activityOf(entryByKey(t, entries, ActivityKey("toolu_spawn"))).GetSubagent().GetSuccess().GetTotals()
	if totals.GetFull() == nil {
		t.Fatal("a sync run must carry the full billed breakdown")
	}
	if got := totals.GetFull().GetInputHits().GetRead(); got != 30 {
		t.Fatalf("cache read = %d, want 30", got)
	}
	if got := totals.GetToolStats().GetFrameCount(); got != 11 {
		t.Fatalf("frame_count = %d, want 11 (never summed with the tool counters)", got)
	}
}

func TestAsyncLaunchAnnouncesTheSpawnAndCarriesOnlyATotal(t *testing.T) {
	// Arrange. AN ASYNC LAUNCH IS AN ANNOUNCEMENT, NOT A CONCLUSION: the vendor
	// answers with a bare acknowledgement and the work's own stream carries it.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
	launch := `{"isAsync":true,"status":"async_launched","agentId":"a15b5267244c1360e",` +
		`"description":"d","prompt":"p","resolvedModel":"claude-fable-5","outputFile":"/tmp/a15.output"}`
	result := toolResultLine("u1", "toolu_spawn", ts2, `[{"type":"text","text":"launched"}]`, launch)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	start := activityOf(entryByKey(t, entries, ActivityKey("toolu_spawn"))).GetSubagent().GetStart()
	if start == nil {
		t.Fatal("an async launch must land on the START arm: the run has not concluded")
	}
	// THE CROSS-PLANE MINTING RULE: the created agent's id is the tool_use_id of
	// the call that spawned it, which is the id the stream plane names the same
	// agent by. The vendor's own `agentId` is a locator for its files, kept for
	// owner resolution and never used as an identity.
	if got := start.GetCreatedAgentId().GetValue(); got != "toolu_spawn" {
		t.Fatalf("created_agent_id = %q, want the spawning call's id", got)
	}
}

func TestTheCreatedAgentIsNeverNamedByTheVendorsOwnAgentId(t *testing.T) {
	// Arrange. Reading the vendor id as an identity would have the file plane and
	// the stream plane name one agent differently, and no consumer could join the
	// two books.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
	result := toolResultLine("u1", "toolu_spawn", ts2, `[{"type":"text","text":"launched"}]`,
		`{"isAsync":true,"agentId":"a15b5267244c1360e","outputFile":"/tmp/a15.output"}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	start := activityOf(entryByKey(t, entries, ActivityKey("toolu_spawn"))).GetSubagent().GetStart()
	if got := start.GetCreatedAgentId().GetValue(); got == "a15b5267244c1360e" {
		t.Fatalf("created_agent_id = %q, which is the vendor's locator rather than an identity", got)
	}
}

func TestStoppedSpawnIsNotDrawnAsAnError(t *testing.T) {
	// Arrange. A person stopped it by hand: the run did what it was told, and a
	// consumer must not draw it as a fault.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
	result := `{"type":"user","uuid":"u1","isSidechain":false,"timestamp":"` + ts2 +
		`","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_spawn","is_error":true,` +
		`"content":[{"type":"text","text":"stopped"}]}]},"toolUseResult":{"stoppedByUser":true,"agentId":"abc"}}`

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	failure := activityOf(entryByKey(t, entries, ActivityKey("toolu_spawn"))).GetSubagent().GetFailure()
	if failure.GetStoppedByUser() == nil {
		t.Fatal("a hand-stopped subagent must name the user as the cause, which is not a fault")
	}
}

func TestSpawnUnitIsALineInTheCallersBookNotTheSubagentsOwn(t *testing.T) {
	// Arrange. A SPAWN IS A UNIT OF THE CALLER'S WORK; the agent it produced is a
	// different thing whose constituents form its own book.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
	result := toolResultLine("u1", "toolu_spawn", ts2, `[{"type":"text","text":"r"}]`, agentResult)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	settled := entryByKey(t, entries, ActivityKey("toolu_spawn"))
	if got := pageLine(settled).GetPageAgentId().GetValue(); got != "session-uuid" {
		t.Fatalf("page_agent_id = %q, want the CALLING agent's book", got)
	}
	created := activityOf(settled).GetSubagent().GetSuccess().GetPrompt()
	if created == nil {
		t.Fatal("the spawn must restate what the subagent was asked to do")
	}
}

func TestAsyncRunWithNoReportedTotalLeavesItUnset(t *testing.T) {
	// Arrange. Absence means UNREPORTED, never zero.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
	result := toolResultLine("u1", "toolu_spawn", ts2, `[{"type":"text","text":"r"}]`,
		`{"agentId":"abc","content":[{"type":"text","text":"r"}],"prompt":"p"}`)

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	totals := activityOf(entryByKey(t, entries, ActivityKey("toolu_spawn"))).GetSubagent().GetSuccess().GetTotals()
	if totals.GetTotalOnly() == nil {
		t.Fatal("a run with no usage breakdown must land on the total-only arm")
	}
	if totals.GetTotalOnly().TotalTokens != nil {
		t.Fatal("an unreported total must stay UNSET rather than becoming zero")
	}
}

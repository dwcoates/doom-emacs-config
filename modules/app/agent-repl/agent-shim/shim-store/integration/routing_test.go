// routing_test.go — SUBJECT 2: routing by wire arm.
//
// The store opens a frame exactly far enough to route it and no further. What
// each arm does is observable from outside: whether a line ever appears in a
// page, whether a terminal closed an agent's record, and — for the arms that
// are held but never served — that nothing anywhere returns them.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// TestUpdateArmBecomesAPageLine is the ordinary growth arm.
func TestUpdateArmBecomesAPageLine(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-update-1", "u-act-1", frameLine(agentID("main"), responseFrame("main", "act-1", "growing"))),
	)

	// Assert.
	page := openSession(ctx, t, store.client(), "main", 10, nil)
	assertTexts(t, "an update arm's book", pageTexts(page.GetPage()), []string{"growing"})
	store.assertNoErrorRecords()
}

// TestContextCutArmPaginatesLikeAnyUpdate covers the landed `context_cut` arm:
// a page line of the main agent's book, instantaneous, no lifecycle.
func TestContextCutArmPaginatesLikeAnyUpdate(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-cut-1", "u-line-1", frameLine(agentID("main"), responseFrame("main", "act-1", "before"))),
		shim.agentEntry("w-cut-2", "u-line-2", frameLine(agentID("main"), contextCutFrame("main"))),
		shim.agentEntry("w-cut-3", "u-line-3", frameLine(agentID("main"), responseFrame("main", "act-2", "after"))),
	)

	// Assert.
	cli := store.client()
	page := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "a book containing a context cut", pageTexts(page.GetPage()), []string{"after", "cut:main", "before"})

	live := liveWork(ctx, t, cli, "main")
	if len(live.GetLiveAgents()) != 0 {
		t.Errorf("a context cut put %v into live work; an update arm has no lifecycle", agentValues(live.GetLiveAgents()))
	}
	store.assertNoErrorRecords()
}

// TestApiErrorArmPaginatesAsEvidenceNotATerminal covers the landed `api_error`
// arm: a mid-turn vendor failure the turn recovered from is a page line and
// never ends the agent's record.
func TestApiErrorArmPaginatesAsEvidenceNotATerminal(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t,
		shim.agentEntry("w-api-spawn", "u-spawn-1", frameLine(agentID("main"), subagentSpawnFrame("main", "act-spawn", "sub-a", "do the thing", 1000))),
	)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-api-1", "u-sub-line-1", frameLine(agentID("main"), apiErrorFrame("sub-a", "overloaded, retried"))),
	)

	// Assert.
	cli := store.client()
	page := openSession(ctx, t, cli, "sub-a", 10, nil)
	assertTexts(t, "a subagent's book carrying an api error", pageTexts(page.GetPage()), []string{"api_error:overloaded, retried"})

	live := liveWork(ctx, t, cli, "main")
	if !contains(agentValues(live.GetLiveAgents()), "sub-a") {
		t.Errorf("an api_error ended the agent's record: live agents are %v, want sub-a still open", agentValues(live.GetLiveAgents()))
	}
	store.assertNoErrorRecords()
}

// TestTerminalArmsWriteAPageLineAndCloseTheAgent is the dual-write arm: the
// stop notice has no other source, so it is a page line AND the agent's
// terminal, in one transaction.
func TestTerminalArmsWriteAPageLineAndCloseTheAgent(t *testing.T) {
	tests := []struct {
		name     string
		agent    string
		writeID  string
		upsert   string
		frame    func(string) *storev1.StoreAgentUpdate
		wantLine string
	}{
		{
			name:    "success",
			agent:   "sub-success",
			writeID: "w-term-success",
			upsert:  "u-term-success",
			frame: func(agent string) *storev1.StoreAgentUpdate {
				return frameLine(agentID("main"), successFrame(agent, "act-answer"))
			},
			wantLine: "success:sub-success",
		},
		{
			name:    "failure",
			agent:   "sub-failure",
			writeID: "w-term-failure",
			upsert:  "u-term-failure",
			frame: func(agent string) *storev1.StoreAgentUpdate {
				return frameLine(agentID("main"), failureFrame(agent, "it broke"))
			},
			wantLine: "failure:sub-failure",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			cli := store.client()
			shim := streamProducer(cli)
			shim.write(ctx, t,
				shim.agentEntry("w-spawn-"+tc.name, "u-spawn-"+tc.name,
					frameLine(agentID("main"), subagentSpawnFrame("main", "act-spawn-"+tc.name, tc.agent, "work", 2000))),
			)
			if !contains(agentValues(liveWork(ctx, t, cli, "main").GetLiveAgents()), tc.agent) {
				t.Fatalf("the spawned agent %q was not live before its terminal", tc.agent)
			}

			// Act.
			shim.write(ctx, t, shim.agentEntry(tc.writeID, tc.upsert, tc.frame(tc.agent)))

			// Assert.
			page := openSession(ctx, t, cli, tc.agent, 10, nil)
			assertTexts(t, "the terminal's own book", pageTexts(page.GetPage()), []string{tc.wantLine})

			if got := agentValues(liveWork(ctx, t, cli, "main").GetLiveAgents()); contains(got, tc.agent) {
				t.Errorf("GetLiveWork still lists %q after its terminal; live agents are %v", tc.agent, got)
			}
			store.assertNoErrorRecords()
		})
	}
}

// TestDetachedWorkAnnouncementIsAPageLineOfTheAnnouncersBook: "work left this
// stream" is the HANDOFF, and a book that omitted it would keep claiming work
// that is no longer in the turn.
func TestDetachedWorkAnnouncementIsAPageLineOfTheAnnouncersBook(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-detach-1", "detached:work-bash-1",
			frameLine(agentID("main"), detachedBashFrame("main", "work-bash-1", "sleep 60", 3000))),
	)

	// Assert.
	page := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "the announcer's book", pageTexts(page.GetPage()), []string{"detached:work-bash-1"})
	store.assertNoErrorRecords()
}

// TestDetachedWorkAnnouncementIsTheSourceOfLiveDetached: the same write is both
// the served handoff and the open obligation.
func TestDetachedWorkAnnouncementIsTheSourceOfLiveDetached(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-detach-live-1", "detached:work-bash-live",
			frameLine(agentID("main"), detachedBashFrame("main", "work-bash-live", "sleep 60", 3000))),
	)

	// Assert.
	live := liveWork(ctx, t, cli, "main")
	if !contains(workValues(live.GetLiveDetached()), "work-bash-live") {
		t.Errorf("the announced run is missing from live_detached: %v", workValues(live.GetLiveDetached()))
	}
	store.assertNoErrorRecords()
}

// TestReAnnouncingDetachedWorkNeitherDuplicatesTheLineNorTheObligation: the
// handle is the identity, so a replay under a new write_id still names one run.
func TestReAnnouncingDetachedWorkNeitherDuplicatesTheLineNorTheObligation(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-reannounce-1", "detached:work-again",
			frameLine(agentID("main"), detachedBashFrame("main", "work-again", "sleep 60", 3000))),
	)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-reannounce-2", "detached:work-again",
			frameLine(agentID("main"), detachedBashFrame("main", "work-again", "sleep 60", 3000))),
	)

	// Assert.
	page := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "the announcer's book after a re-announcement", pageTexts(page.GetPage()), []string{"detached:work-again"})
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); len(got) != 1 {
		t.Errorf("live_detached = %v, want exactly one entry per run", got)
	}
	store.assertNoErrorRecords()
}

// TestDetachedWorkLeavesLiveWorkAtItsTerminal closes the announcement's other
// half: the row is live UNTIL its terminal, and not after.
func TestDetachedWorkLeavesLiveWorkAtItsTerminal(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-detach-run-1", "detached:work-bash-2",
			frameLine(agentID("main"), detachedBashFrame("main", "work-bash-2", "make test", 4000))),
	)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-detach-run-2", "u-detach-run-2",
			bashRun(agentID("main"), "work-bash-2", bashSuccess("make test", 0))),
	)

	// Assert.
	live := liveWork(ctx, t, cli, "main")
	if contains(workValues(live.GetLiveDetached()), "work-bash-2") {
		t.Errorf("a concluded run is still in live_detached: %v", workValues(live.GetLiveDetached()))
	}
	store.assertNoErrorRecords()
}

// TestBashRunFramesNeverPaginate: a detached shell run's frames are lifecycle
// state, structurally not page lines.
func TestBashRunFramesNeverPaginate(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-bash-anchor", "u-bash-anchor", frameLine(agentID("main"), responseFrame("main", "act-1", "anchor"))),
	)

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-bash-1", "u-bash-run-1", bashRun(agentID("main"), "act-bash-1", bashStart("go test ./...", 5000))),
		shim.agentEntry("w-bash-2", "u-bash-run-1", bashRun(agentID("main"), "act-bash-1", bashSuccess("go test ./...", 0))),
	)

	// Assert.
	page := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "a book beside bash run frames", pageTexts(page.GetPage()), []string{"anchor"})
	store.assertNoErrorRecords()
}

// TestSessionUpdateNeverPaginates: a session fact belongs to the main agent's
// scope, not to any book.
func TestSessionUpdateNeverPaginates(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-su-anchor", "u-su-anchor", frameLine(agentID("main"), responseFrame("main", "act-1", "anchor"))),
	)

	// Act.
	shim.write(ctx, t,
		shim.sessionEntry("w-su-1", "u-session-rotate-1", identityRotated("vendor-a", "vendor-b")),
	)

	// Assert.
	page := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "a book beside a session update", pageTexts(page.GetPage()), []string{"anchor"})
	store.assertNoErrorRecords()
}

// TestUnservedArmsNeverAppearAnywhere: held durably, returned by nothing —
// not an open, not a read, not a watch.
func TestUnservedArmsNeverAppearAnywhere(t *testing.T) {
	tests := []struct {
		name   string
		update *storev1.StoreAgentUpdate
	}{
		{name: "vendor_specific", update: vendorSpecificLine("vendor_only_thing")},
		{name: "unknown", update: unknownLine("some_new_kind", "type")},
		{name: "unparsed", update: unparsedLine("/transcripts/x.jsonl", 900, "unexpected byte", "{oops")},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			cli := store.client()
			shim := streamProducer(cli)
			shim.write(ctx, t,
				shim.agentEntry("w-anchor-"+tc.name, "u-anchor-"+tc.name, frameLine(agentID("main"), responseFrame("main", "act-1", "anchor"))),
			)
			opened := openSession(ctx, t, cli, "main", 10, nil)
			stream := watchStream(ctx, t, cli, opened.GetWatch())
			defer testclose.OrFail(t, stream)

			// Act.
			shim.write(ctx, t, shim.agentEntry("w-unserved-"+tc.name, "u-unserved-"+tc.name, tc.update))
			shim.write(ctx, t,
				shim.agentEntry("w-after-"+tc.name, "u-after-"+tc.name, frameLine(agentID("main"), responseFrame("main", "act-2", "after"))),
			)

			// Assert: the tail skips straight from the pin to the next real line.
			assertTexts(t, "the watched tail", receivedTexts(receiveLines(t, stream, 1)), []string{"after"})

			repaint := openSession(ctx, t, cli, "main", 10, nil)
			assertTexts(t, "a full repaint", pageTexts(repaint.GetPage()), []string{"after", "anchor"})

			pointer := pagePointers(repaint.GetPage())[0]
			walked := readPage(ctx, t, cli, "main", 10, &storev1.StoreItemPointer{Value: pointer})
			assertTexts(t, "the continuation walk", readTexts(walked), []string{"anchor"})
			store.assertNoErrorRecords()
		})
	}
}

// TestWorkflowArmIsAcceptedDurablyWithAWarning: nothing routes into the
// workflow table this wave, so the frame lands whole and GetWorkflow refuses.
func TestWorkflowArmIsAcceptedDurablyWithAWarning(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	mark := store.logMark()

	// Act.
	shim.write(ctx, t,
		shim.agentEntry("w-workflow-1", "u-workflow-run-1",
			workflowRun(agentID("main"), "run-agent-1", workflowStartFrame("nightly", 6000))),
	)

	// Assert: durable and warned about, never dropped and never served.
	//
	// THE WARNING IS SCOPED AND COUNTED. "Some warn was logged in this window"
	// passed for a reclaimed socket or a slow query as readily as for the
	// disposition under test, and it said nothing about the warning being
	// written once per entry rather than once per retry.
	written := recordsAtLevel(recordsAtOperation(store.logRecordsAfter(mark), "store.db.write-batch"), "warn")
	if len(written) != 1 {
		t.Fatalf("store.db.write-batch warn records = %d, want exactly 1 for one workflow entry: %v", len(written), written)
	}
	if written[0].Context["write_id"] != "w-workflow-1" {
		t.Errorf("the workflow warning names write_id %v, want w-workflow-1", written[0].Context["write_id"])
	}

	// A workflow frame is not a page line and it is not an agent's first
	// sight either, so "main" is still an agent this store has never heard of.
	openUnknownAgent(ctx, t, cli, "main")

	// THE ARM IS THE ANSWER, NEVER THE DETAIL. `detail` is prose for a human
	// and nothing may switch on it; a caller learns from `not_implemented` that
	// re-asking cannot help, which is the whole point of the arm.
	resp, err := cli.GetWorkflow(ctx, connectGetWorkflow("work-run-1"))
	if err != nil {
		t.Fatalf("GetWorkflow answered a transport error where a typed failure was owed: %v", err)
	}
	failure := resp.Msg.GetFailure()
	if failure == nil {
		t.Fatalf("GetWorkflow answered success for a wave where nothing routes into the workflow table: %v", resp.Msg)
	}
	if failure.GetNotImplemented() == nil {
		t.Fatalf("GetWorkflow failure kind = %v, want not_implemented", failure.GetKind())
	}
	assertDetail(t, "the GetWorkflow refusal", failure.GetDetail())
}

// TestAnAcceptedWorkflowFrameIsDurableAndWarnedAboutOnlyOnce proves the other
// half of the disposition: the entry LANDED.
//
// Durability is shown by the write ledger surviving the process. After a
// restart the same write_id is absorbed rather than applied again — and the
// absorption is observable precisely because the not-implemented warning is
// written on the apply path, so a replay that was absorbed writes none.
func TestAnAcceptedWorkflowFrameIsDurableAndWarnedAboutOnlyOnce(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	entry := shim.agentEntry("w-workflow-durable", "u-workflow-run-durable",
		workflowRun(agentID("main"), "run-agent-durable", workflowStartFrame("nightly", 6000)))
	shim.write(ctx, t, entry)

	// Act.
	store.restart()
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	revived := streamProducer(store.client())
	mark := store.logMark()
	revived.write(after, t, entry)

	// Assert: the replay was absorbed, so no second warning was written.
	written := recordsAtLevel(recordsAtOperation(store.logRecordsAfter(mark), "store.db.write-batch"), "warn")
	if len(written) != 0 {
		t.Fatalf("replaying a landed workflow write warned again = %v; the write did not survive the restart", written)
	}
}

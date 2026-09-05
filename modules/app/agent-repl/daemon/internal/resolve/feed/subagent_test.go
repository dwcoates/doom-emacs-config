package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// THE BUBBLE IS A FEED, and sync-vs-detached is PLACEMENT rather than a second
// drawing: the same FeedSubagent rides the activity arm and the detached
// wrapper alike.

// spawnSubagent announces one spawn and returns the bubble's row.
func (h *harness) spawnSubagent(unit string, created *conversationv1.AgentId, subagentType, description string) *frontendv1.FeedRow {
	h.t.Helper()
	prompt := &conversationv1.AgentSubagentPrompt{Text: "go and look"}
	if subagentType != "" {
		prompt.SubagentType = &subagentType
	}
	if description != "" {
		prompt.Description = &description
	}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: created,
				Prompt:         prompt,
				StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			}},
		}},
	})
	return h.bubbleRow(unit, created)
}

// bubbleRow finds a spawn's bubble row on the root feed.
func (h *harness) bubbleRow(unit string, created *conversationv1.AgentId) *frontendv1.FeedRow {
	h.t.Helper()
	want := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindActivity, ID: unit, Sub: created.GetValue()},
	}).GetValue()
	for _, row := range h.rows(rootFeed()) {
		if row.GetId().GetValue() == want {
			return row
		}
	}
	h.t.Fatalf("no bubble row for spawn %q", unit)
	return nil
}

// bubbleOf reads whichever arm carries the head.
func bubbleOf(row *frontendv1.FeedRow) *frontendv1.FeedSubagent {
	if detached := row.GetDetachedSubagent(); detached != nil {
		return detached.GetSubagent()
	}
	return row.GetActivity().GetSubagent()
}

// settleSubagent settles a spawn with the given failure cause, or successfully
// when cause is nil.
func (h *harness) settleSubagent(unit string, created *conversationv1.AgentId, failure *conversationv1.AgentSubagentFailure) {
	h.t.Helper()
	spawn := &conversationv1.AgentSubagent{}
	if failure != nil {
		spawn.Result = &conversationv1.AgentSubagent_Failure{Failure: failure}
	} else {
		spawn.Result = &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
			Prompt:    &conversationv1.AgentSubagentPrompt{Text: "go and look"},
			Report:    &conversationv1.AgentSubagentReport{Prose: &conversationv1.AgentResponseProse{Markdown: "found it"}},
			Totals:    &conversationv1.AgentSubagentTotals{ToolUseCount: 18},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
		}}
	}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item:       &conversationv1.AgentActivity_Subagent{Subagent: spawn},
	})
}

func TestASpawnDrawsTheCollapsedHeadOnItsCallersFeed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	row := h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Assert: the head, on the parent's feed, with the label and description.
	bubble := bubbleOf(row)
	if bubble.GetLabel().GetText() != "Explore" {
		t.Fatalf("label = %q, want the subagent type", bubble.GetLabel().GetText())
	}
	if bubble.GetDescription().GetText() != "map the daemon" {
		t.Fatalf("description = %q", bubble.GetDescription().GetText())
	}
	if bubble.GetLive() == nil {
		t.Fatalf("state = %T, want live", bubble.GetState())
	}
}

func TestASpawnWithNoDescriptionDrawsTheLabelAlone(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	row := h.spawnSubagent("spawn-1", created, "Explore", "")

	// Assert: never a synthesized description.
	if bubbleOf(row).GetDescription() != nil {
		t.Fatalf("description = %+v, want unset", bubbleOf(row).GetDescription())
	}
}

func TestABubblesOwnIdAddressesItsSubFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	row := h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act: a page opened on the sub-feed.
	page, _ := h.openPage(feedid.Feed{Agent: created}, "reader-1")

	// Assert: the crumb names the bubble's own row.
	crumbs := page.GetResult().(*frontendv1.FeedPage_Success).Success.GetBreadcrumbs().GetCrumbs()
	if len(crumbs) != 1 || crumbs[0].GetTarget().GetValue() != row.GetId().GetValue() {
		t.Fatalf("crumbs = %+v, want the bubble's own row", crumbs)
	}
}

func TestASubagentsOwnRowsLandOnItsSubFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act: the subagent's own prose.
	h.resolver.OnActivity(testWorkspace, created,
		responseSuccessActivity("unit-9", "here is what I found"), noAddress())

	// Assert: on the bubble's feed, never carried on the parent's row.
	rows := h.rows(feedid.Feed{Agent: created})
	if last(rows).GetActivity().GetResponse() == nil {
		t.Fatalf("sub-feed rows = %+v, want the subagent's prose", rows)
	}
}

func TestAnUpdateKeepsTheOriginalClockAndCarriesTheTokenSum(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{
				Prompt:   &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Progress: &conversationv1.AgentSubagentProgress{TotalTokens: 12_400},
			}},
		}},
	})

	// Assert.
	bubble := bubbleOf(h.bubbleRow("spawn-1", created))
	if got := bubble.GetRuntime().GetStartedAtMs(); got != 1_000 {
		t.Fatalf("runtime = %d, want the original instant", got)
	}
	if got := bubble.GetTokens().GetText(); got != "12.4k tok" {
		t.Fatalf("tokens = %q, want the running sum", got)
	}
	if bubble.GetLive().GetLastProgress().GetAtMs() != h.nowMs {
		t.Fatalf("last_progress = %d, want the observed beat", bubble.GetLive().GetLastProgress().GetAtMs())
	}
}

func TestASettledSpawnStopsTheClockAndSucceeds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act.
	h.settleSubagent("spawn-1", created, nil)

	// Assert.
	settled := bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled()
	if settled == nil || settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("settled = %+v, want the clock stopped at 9000", settled)
	}
	if settled.GetSucceeded() == nil {
		t.Fatalf("outcome = %T, want succeeded", settled.GetOutcome())
	}
}

func TestSettledOutcomesDistinguishFailedCancelledAndLost(t *testing.T) {
	tests := []struct {
		name    string
		failure *conversationv1.AgentSubagentFailure
		want    string
	}{
		{
			name:    "an ordinary failure",
			failure: &conversationv1.AgentSubagentFailure{},
			want:    "failed",
		},
		{
			name: "a person's stop is not a fault",
			failure: &conversationv1.AgentSubagentFailure{
				Cause: &conversationv1.AgentSubagentFailure_StoppedByUser{
					StoppedByUser: &conversationv1.AgentSubagentStoppedByUser{},
				},
			},
			want: "cancelled",
		},
		{
			name: "work we stopped being able to see is LOST, not failed",
			failure: &conversationv1.AgentSubagentFailure{
				Cause: &conversationv1.AgentSubagentFailure_Lost{
					Lost: &conversationv1.DetachedLost{
						How: &conversationv1.DetachedLost_WentSilent{
							WentSilent: &conversationv1.DetachedLostWentSilent{},
						},
					},
				},
			},
			want: "lost",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			created := &conversationv1.AgentId{Value: "agent-explore"}
			h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

			// Act.
			h.settleSubagent("spawn-1", created, tc.failure)

			// Assert.
			got := subagentOutcomeWord(bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled())
			if got != tc.want {
				t.Fatalf("outcome = %q, want %q", got, tc.want)
			}
		})
	}
}

// subagentOutcomeWord names a settled bubble's outcome arm.
func subagentOutcomeWord(settled *frontendv1.FeedSubagentSettled) string {
	switch settled.GetOutcome().(type) {
	case *frontendv1.FeedSubagentSettled_Succeeded:
		return "succeeded"
	case *frontendv1.FeedSubagentSettled_Failed:
		return "failed"
	case *frontendv1.FeedSubagentSettled_Cancelled:
		return "cancelled"
	case *frontendv1.FeedSubagentSettled_Lost:
		return "lost"
	}
	return "unset"
}

func TestASyncTotalsTokenSumIsTheFullBreakdown(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
				Prompt: &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Report: &conversationv1.AgentSubagentReport{},
				Totals: &conversationv1.AgentSubagentTotals{
					Usage: &conversationv1.AgentSubagentTotals_Full{Full: &conversationv1.TokenUsage{
						InputMisses:  &conversationv1.TokenCacheMisses{Written: 10_000, Unwritten: 400},
						OutputTokens: 2_000,
					}},
				},
			}},
		}},
	})

	// Assert.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetTokens().GetText(); got != "12.4k tok" {
		t.Fatalf("tokens = %q", got)
	}
}

func TestAnAsyncTotalWithNoUsageReportedDrawsNoTokenSum(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act: an async run whose notification carried no usage at all.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
				Prompt: &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Report: &conversationv1.AgentSubagentReport{},
				Totals: &conversationv1.AgentSubagentTotals{
					Usage: &conversationv1.AgentSubagentTotals_TotalOnly{
						TotalOnly: &conversationv1.AgentSubagentAsyncUsage{},
					},
				},
			}},
		}},
	})

	// Assert: absence means unreported, never zero.
	if bubbleOf(h.bubbleRow("spawn-1", created)).GetTokens() != nil {
		t.Fatalf("tokens = %+v, want unset", bubbleOf(h.bubbleRow("spawn-1", created)).GetTokens())
	}
}

func TestDetachingMovesTheSameBubbleIntoItsPlacementWrapper(t *testing.T) {
	// Arrange: a bubble already on screen.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	before := h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	if before.GetActivity().GetSubagent() == nil {
		t.Fatal("the sync spawn did not draw on the activity arm")
	}

	// Act.
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-1"},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: "spawn-1"},
			Cause: &conversationv1.DetachedWorkDetached_Requested{
				Requested: &conversationv1.DetachedCauseRequested{},
			},
		}},
	}, noAddress())

	// Assert: ONE identity spans the move, and the drawing is unchanged.
	after := h.bubbleRow("spawn-1", created)
	if after.GetId().GetValue() != before.GetId().GetValue() {
		t.Fatalf("id changed across the move: %q → %q", before.GetId().GetValue(), after.GetId().GetValue())
	}
	if after.GetDetachedSubagent() == nil {
		t.Fatalf("row = %T, want the detached wrapper", after.GetRow())
	}
	if after.GetDetachedSubagent().GetSubagent().GetLabel().GetText() != "Explore" {
		t.Fatal("the wrapper carried a second drawing rather than the same bubble")
	}
	if !h.hasRecord("debug", "daemon.feed.detached_subagent") {
		t.Fatalf("records = %+v, want the move recorded", h.records())
	}
}

func TestWorkDetachedFromAUnitWeNeverDrewIsWarnedWhenTheTurnEnds(t *testing.T) {
	// Arrange: a detachment naming a unit nothing ever draws.
	h := newHarness(t)
	h.detachWork("work-1", "never-seen")

	// Act: the turn ends, which is the last moment the mark could have been
	// claimed.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.detached_unknown_unit") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.detached_unknown_unit", h.records())
	}
}

func TestASubagentCreatedDetachedDrawsThroughTheWrapperAtOnce(t *testing.T) {
	// Arrange, Act: work that is detached from the moment we hear of it.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-remote"}
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-1"},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{
				Work: &conversationv1.DetachableWork_Subagent{Subagent: &conversationv1.AgentSubagent{
					Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
						CreatedAgentId: created,
						Prompt:         &conversationv1.AgentSubagentPrompt{Text: "go"},
						StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
					}},
				}},
			},
		}},
	}, noAddress())

	// Assert.
	rows := h.rows(rootFeed())
	if len(rows) != 1 || rows[0].GetDetachedSubagent() == nil {
		t.Fatalf("rows = %+v, want one detached bubble", rows)
	}
}

func TestAMonitorDrawsNoFeedRow(t *testing.T) {
	// Arrange, Act: a monitor is FOOTER-ONLY.
	h := newHarness(t)
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-1"},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{
				Work: &conversationv1.DetachableWork_Monitor{Monitor: &conversationv1.AgentMonitor{}},
			},
		}},
	}, noAddress())

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want 0 for a monitor", len(rows))
	}
	if !h.hasRecord("debug", "daemon.feed.detached_draws_nothing") {
		t.Fatalf("records = %+v, want the not-a-row branch recorded", h.records())
	}
}

// ---- THE DETACHED SHELL BUBBLE ----

// shellRow finds the detached shell bubble on the root feed.
func (h *harness) shellRow() *frontendv1.FeedShell {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if detached := row.GetDetachedShell(); detached != nil {
			return detached.GetShell()
		}
	}
	h.t.Fatal("no detached shell bubble on the root feed")
	return nil
}

// bash sends one frame on a detached shell's own stream.
func (h *harness) bash(work string, result any) {
	h.t.Helper()
	item := &conversationv1.AgentBash{}
	switch r := result.(type) {
	case *conversationv1.AgentBashStart:
		item.Result = &conversationv1.AgentBash_Start{Start: r}
	case *conversationv1.AgentBashUpdate:
		item.Result = &conversationv1.AgentBash_Update{Update: r}
	case *conversationv1.AgentToolCallProgress:
		item.Result = &conversationv1.AgentBash_Progress{Progress: r}
	case *conversationv1.AgentBashSuccess:
		item.Result = &conversationv1.AgentBash_Success{Success: r}
	case *conversationv1.AgentBashFailure:
		item.Result = &conversationv1.AgentBash_Failure{Failure: r}
	}
	h.resolver.OnBash(testWorkspace, &conversationv1.DetachedWorkId{Value: work}, item, noAddress())
}

func TestADetachedShellDrawsItsCommandAndItsClock(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: the "$" chrome is the client's, so the text is the command.
	shell := h.shellRow()
	if shell.GetCommand().GetText() != "npm run dev" {
		t.Fatalf("command = %q", shell.GetCommand().GetText())
	}
	if shell.GetRuntime().GetStartedAtMs() != 1_000 {
		t.Fatalf("runtime = %d", shell.GetRuntime().GetStartedAtMs())
	}
	if shell.GetLive() == nil {
		t.Fatalf("state = %T, want live", shell.GetState())
	}
}

func TestASpoolWithNoOutputYetIsUnsetRatherThanEmpty(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "sleep 1"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: present from the FIRST output, and not before.
	if h.shellRow().GetSpool() != nil {
		t.Fatalf("spool = %+v, want unset before any output", h.shellRow().GetSpool())
	}
}

func TestSpoolUpdatesAccumulateAndStampTheBeat(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act: two deltas, each continuing the sequence.
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "ready\n", FromOffset: 10})

	// Assert: spool growth IS the beat.
	shell := h.shellRow()
	if shell.GetSpool().GetText() != "compiling\nready\n" {
		t.Fatalf("spool = %q", shell.GetSpool().GetText())
	}
	if shell.GetLive().GetLastProgress().GetAtMs() != h.nowMs {
		t.Fatalf("last_progress = %d, want the observed append", shell.GetLive().GetLastProgress().GetAtMs())
	}
}

func TestASpoolGapIsRefusedRatherThanConcatenatedAcross(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})

	// Act: an offset that does not continue the sequence — bytes were lost.
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "ready\n", FromOffset: 999})

	// Assert: output that never existed is never drawn.
	if got := h.shellRow().GetSpool().GetText(); got != "compiling\n" {
		t.Fatalf("spool = %q, want the frame refused", got)
	}
	if !h.hasRecord("error", "daemon.feed.spool_gap") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.spool_gap", h.records())
	}
}

func TestACappedSpoolKeepsItsTailAndSaysWhatItDropped(t *testing.T) {
	// Arrange: more output than the daemon carries.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "yes"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	var offset uint64
	line := "a line of output\n"
	for i := 0; i < 2_000; i++ {
		h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: line, FromOffset: offset})
		offset += uint64(len(line))
	}

	// Act.
	spool := h.shellRow().GetSpool()

	// Assert: the TAIL, cut on a line boundary, with the drop stated.
	if len(spool.GetText()) > spoolCap {
		t.Fatalf("spool = %d bytes, want at most the cap %d", len(spool.GetText()), spoolCap)
	}
	if spool.GetText()[0] != 'a' {
		t.Fatalf("spool begins %q, want a line boundary", spool.GetText()[:20])
	}
	if spool.GetOmitted() == nil || !contains(spool.GetOmitted().GetText(), "earlier lines not shown") {
		t.Fatalf("omitted = %+v, want the truncation line", spool.GetOmitted())
	}
}

func TestAnUncappedSpoolStatesNoTruncation(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "echo hi"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "hi\n", FromOffset: 0})

	// Assert.
	if h.shellRow().GetSpool().GetOmitted() != nil {
		t.Fatalf("omitted = %+v, want unset", h.shellRow().GetSpool().GetOmitted())
	}
}

func TestASettledShellCarriesItsExitChip(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm test"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm test"},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Output: &conversationv1.AgentBashOutput{},
			Termination: &conversationv1.AgentBashTermination{
				How: &conversationv1.AgentBashTermination_Exited{
					Exited: &conversationv1.AgentBashExited{Code: 1},
				},
			},
		}},
		SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
	})

	// Assert: a non-zero exit still COMPLETED; the tone is the client's reading
	// of the code.
	settled := h.shellRow().GetSettled()
	if settled.GetCompleted() == nil {
		t.Fatalf("outcome = %T, want completed", settled.GetOutcome())
	}
	if settled.GetExit().GetCode() != 1 {
		t.Fatalf("exit = %+v, want the chip", settled.GetExit())
	}
	if settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("ended_at = %d", settled.GetEndedAtMs())
	}
}

func TestAShellKilledBySignalCarriesNoExitChip(t *testing.T) {
	// Arrange, Act: a kill never reported a status, so there is no number.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm test"},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Output: &conversationv1.AgentBashOutput{},
			Termination: &conversationv1.AgentBashTermination{
				How: &conversationv1.AgentBashTermination_Killed{
					Killed: &conversationv1.AgentBashKilled{},
				},
			},
		}},
	})

	// Assert: absence draws no chip, never a zero.
	if h.shellRow().GetSettled().GetExit() != nil {
		t.Fatalf("exit = %+v, want unset", h.shellRow().GetSettled().GetExit())
	}
}

func TestAForegroundShellsMissingTerminationCarriesNoExitChip(t *testing.T) {
	// Arrange, Act: the fact exists for exactly one of the two paths.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm test"},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Output: &conversationv1.AgentBashOutput{},
		}},
	})

	// Assert.
	if h.shellRow().GetSettled().GetExit() != nil {
		t.Fatalf("exit = %+v, want unset", h.shellRow().GetSettled().GetExit())
	}
}

func TestACompletedShellWithNoObservedOutputLeavesItsSpoolUnset(t *testing.T) {
	// Arrange, Act: the producer states that it does not know what the command
	// printed, which is a different claim from "it printed nothing".
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm run build"},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Output: &conversationv1.AgentBashOutput{
				Form: &conversationv1.AgentBashOutput_NotObserved{
					NotObserved: &conversationv1.AgentBashOutputNotObserved{},
				},
			},
			Termination: &conversationv1.AgentBashTermination{
				How: &conversationv1.AgentBashTermination_Exited{
					Exited: &conversationv1.AgentBashExited{Code: 0},
				},
			},
		}},
		SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
	})

	// Assert: UNSET, never an empty spool — an empty spool would draw as a
	// command that printed nothing.
	if spool := h.shellRow().GetSpool(); spool != nil {
		t.Fatalf("spool = %+v, want unset for unobserved output", spool)
	}
}

func TestACompletedShellWithNoObservedOutputStillSettlesAsCompleted(t *testing.T) {
	// Arrange, Act: not seeing the output says nothing about the outcome.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm run build"},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Output: &conversationv1.AgentBashOutput{
				Form: &conversationv1.AgentBashOutput_NotObserved{
					NotObserved: &conversationv1.AgentBashOutputNotObserved{},
				},
			},
			Termination: &conversationv1.AgentBashTermination{
				How: &conversationv1.AgentBashTermination_Exited{
					Exited: &conversationv1.AgentBashExited{Code: 0},
				},
			},
		}},
		SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
	})

	// Assert.
	if h.shellRow().GetSettled().GetCompleted() == nil {
		t.Fatalf("outcome = %T, want completed", h.shellRow().GetSettled().GetOutcome())
	}
}

func TestAnInterruptedShellWithNoObservedOutputLeavesItsSpoolUnset(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm run dev"},
		Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
			Output: &conversationv1.AgentBashOutput{
				Form: &conversationv1.AgentBashOutput_NotObserved{
					NotObserved: &conversationv1.AgentBashOutputNotObserved{},
				},
			},
			Cause: &conversationv1.AgentBashInterrupted_ByUser{
				ByUser: &conversationv1.AgentBashInterruptedByUser{},
			},
		}},
	})

	// Assert.
	if spool := h.shellRow().GetSpool(); spool != nil {
		t.Fatalf("spool = %+v, want unset for unobserved output", spool)
	}
}

func TestAnInterruptedShellWithNoObservedOutputSettlesAsTheRecordStatesIt(t *testing.T) {
	// Arrange, Act: the cause is the whole claim, and an unobserved spool does
	// not soften it into something else.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm run dev"},
		Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
			Output: &conversationv1.AgentBashOutput{
				Form: &conversationv1.AgentBashOutput_NotObserved{
					NotObserved: &conversationv1.AgentBashOutputNotObserved{},
				},
			},
			Cause: &conversationv1.AgentBashInterrupted_Lost{Lost: &conversationv1.DetachedLost{
				How: &conversationv1.DetachedLost_FileVanished{
					FileVanished: &conversationv1.DetachedLostFileVanished{},
				},
			}},
		}},
	})

	// Assert.
	if h.shellRow().GetSettled().GetLost() == nil {
		t.Fatalf("outcome = %T, want lost", h.shellRow().GetSettled().GetOutcome())
	}
}

func TestOutputObservedBeforeAnUnobservedSettleIsStillDrawn(t *testing.T) {
	// Arrange: the spool the daemon watched arrive.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})

	// Act: the settle states no observed output of its own.
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm run dev"},
		Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
			Output: &conversationv1.AgentBashOutput{
				Form: &conversationv1.AgentBashOutput_NotObserved{
					NotObserved: &conversationv1.AgentBashOutputNotObserved{},
				},
			},
			Cause: &conversationv1.AgentBashInterrupted_ByUser{
				ByUser: &conversationv1.AgentBashInterruptedByUser{},
			},
		}},
	})

	// Assert: what WAS observed is never discarded by a later "not observed".
	if got := h.shellRow().GetSpool().GetText(); got != "compiling\n" {
		t.Fatalf("spool = %q, want the observed output kept", got)
	}
}

func TestAShellWeStoppedSeeingIsLostAndNotCancelled(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm run dev"},
		Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
			Output: &conversationv1.AgentBashOutput{},
			Cause: &conversationv1.AgentBashInterrupted_Lost{Lost: &conversationv1.DetachedLost{
				How: &conversationv1.DetachedLost_FileVanished{
					FileVanished: &conversationv1.DetachedLostFileVanished{},
				},
			}},
		}},
	})

	// Assert: not known to have failed, and never drawn as a cancel.
	settled := h.shellRow().GetSettled()
	if settled.GetLost() == nil {
		t.Fatalf("outcome = %T, want lost", settled.GetOutcome())
	}
}

func TestAShellStoppedByHandIsCancelled(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm run dev"},
		Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
			Output: &conversationv1.AgentBashOutput{},
			Cause: &conversationv1.AgentBashInterrupted_ByUser{
				ByUser: &conversationv1.AgentBashInterruptedByUser{},
			},
		}},
	})

	// Assert.
	if h.shellRow().GetSettled().GetCancelled() == nil {
		t.Fatalf("outcome = %T, want cancelled", h.shellRow().GetSettled().GetOutcome())
	}
}

func TestAForegroundShellThatDetachesKeepsItsCommandAndClock(t *testing.T) {
	// Arrange: a foreground call already on screen.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Act: it moves rather than ending.
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-1"},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: "unit-1"},
			Cause: &conversationv1.DetachedWorkDetached_ByUser{
				ByUser: &conversationv1.DetachedCauseByUser{},
			},
		}},
	}, noAddress())

	// Assert: the ORIGINAL instant, so the drawn clock does not reset.
	shell := h.shellRow()
	if shell.GetCommand().GetText() != "npm run dev" {
		t.Fatalf("command = %q, want the command carried across the move", shell.GetCommand().GetText())
	}
	if shell.GetRuntime().GetStartedAtMs() != 1_000 {
		t.Fatalf("runtime = %d, want the original instant", shell.GetRuntime().GetStartedAtMs())
	}
	if !h.hasRecord("debug", "daemon.feed.detached_shell") {
		t.Fatalf("records = %+v, want the move recorded", h.records())
	}
}

func TestADetachedShellsFailureSettlesTheBubble(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashFailure{
		Error: &conversationv1.AgentToolFailure{
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
		},
	})

	// Assert.
	settled := h.shellRow().GetSettled()
	if settled == nil || settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("settled = %+v, want the bubble concluded", settled)
	}
}

// TestAReannouncedStartDoesNotUnsettleASettledBubble covers the re-announcement
// a settled spawn's start rides in on: SessionStarted.live_work and the work's
// own stream both replay it, and taking it as live would leave a finished
// bubble spinning with nothing left to settle it.
func TestAReannouncedStartDoesNotUnsettleASettledBubble(t *testing.T) {
	// Arrange: a spawn that has already settled.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	h.settleSubagent("spawn-1", created, nil)

	// Act: the same start again.
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Assert.
	bubble := bubbleOf(h.bubbleRow("spawn-1", created))
	if bubble.GetSettled() == nil {
		t.Fatalf("state = %T, want the bubble still settled", bubble.GetState())
	}
}

// TestAFreshStartIsLive covers the ordinary start: nothing has settled it, so
// the bubble opens live.
func TestAFreshStartIsLive(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act.
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Assert.
	bubble := bubbleOf(h.bubbleRow("spawn-1", created))
	if bubble.GetLive() == nil {
		t.Fatalf("state = %T, want live", bubble.GetState())
	}
}

// last answers a feed's newest row, for assertions whose subject is WHERE a
// row landed rather than what else the feed carries.
func last(rows []*frontendv1.FeedRow) *frontendv1.FeedRow {
	if len(rows) == 0 {
		return nil
	}
	return rows[len(rows)-1]
}

// detachWork announces one detachment of a unit.
func (h *harness) detachWork(work, unit string) {
	h.t.Helper()
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: work},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: unit},
			Cause: &conversationv1.DetachedWorkDetached_Requested{
				Requested: &conversationv1.DetachedCauseRequested{},
			},
		}},
	}, noAddress())
}

// commissionRow finds a spawn's commission row on the created agent's feed.
func (h *harness) commissionRow(unit string, created *conversationv1.AgentId) *frontendv1.FeedRow {
	h.t.Helper()
	want := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: feedid.Feed{Agent: created},
		Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: unit, Sub: "commission"},
	}).GetValue()
	for _, row := range h.rows(feedid.Feed{Agent: created}) {
		if row.GetId().GetValue() == want {
			return row
		}
	}
	return nil
}

// ---- THE COMMISSION: the instruction, in the bubble's BODY ----

func TestASpawnsCommissionDrawsOnTheSubagentsOwnFeed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Assert: the instruction is a row on the bubble's own feed.
	row := h.commissionRow("spawn-1", created)
	if row == nil {
		t.Fatalf("sub-feed rows = %+v, want the commission", h.rows(feedid.Feed{Agent: created}))
	}
	blocks := row.GetAgentPrompt().GetBody().GetBlocks()
	if len(blocks) != 1 || blocks[0].GetText().GetText() != "go and look" {
		t.Fatalf("commission body = %+v, want the instruction verbatim", blocks)
	}
}

func TestACommissionNamesTheFeedThatSentIt(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Assert: the recipient's address line, exactly as an agent prompt draws it.
	row := h.commissionRow("spawn-1", created)
	if got := row.GetAgentPrompt().GetAddress().GetText(); got != "from the main agent" {
		t.Fatalf("address = %q, want the sender's feed named", got)
	}
}

func TestACommissionDrawsNoSecondRowOnTheCallersFeed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Assert: the caller's end IS the bubble; the head is not doubled.
	for _, row := range h.rows(rootFeed()) {
		if row.GetAgentPrompt() != nil {
			t.Fatalf("root feed carried an agent_prompt row %+v; the bubble is the sender's end", row)
		}
	}
}

func TestASpawnWithNoInstructionDrawsNoCommission(t *testing.T) {
	// Arrange, Act: a spawn whose commission carries no text.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: created,
				Prompt:         &conversationv1.AgentSubagentPrompt{},
				StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			}},
		}},
	})

	// Assert: never an empty body.
	if row := h.commissionRow("spawn-1", created); row != nil {
		t.Fatalf("commission = %+v, want none for a spawn that stated no instruction", row)
	}
}

func TestARestatedCommissionDoesNotRepublishItsRow(t *testing.T) {
	// Arrange: every frame of a spawn restates the commission.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	before := len(h.rows(feedid.Feed{Agent: created}))

	// Act.
	h.settleSubagent("spawn-1", created, nil)

	// Assert: one commission row, not one per frame.
	if got := len(h.rows(feedid.Feed{Agent: created})); got != before {
		t.Fatalf("sub-feed rows = %d, want the same %d (the commission is one row)", got, before)
	}
}

// ---- ORDER INDEPENDENCE: a detachment announced before its unit drew ----

func TestADetachmentAnnouncedBeforeItsSpawnStillDrawsTheWrapper(t *testing.T) {
	// Arrange: the detachment arrives first, as a store replay delivers it —
	// the unit's row replays at its LAST upsert, after the announcement.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.detachWork("spawn-1", "spawn-1")

	// Act.
	row := h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Assert: the bubble rides the detached wrapper, not the sync arm.
	if row.GetDetachedSubagent() == nil {
		t.Fatalf("row = %T, want the detached wrapper", row.GetRow())
	}
}

func TestAClaimedDetachmentIsNotReportedAsUnknownWhenTheTurnEnds(t *testing.T) {
	// Arrange: the detachment arrives before its spawn, and the spawn draws.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.detachWork("spawn-1", "spawn-1")
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert.
	if h.hasRecord("warn", "daemon.feed.detached_unknown_unit") {
		t.Fatalf("records = %+v, want no unknown-unit warning for a claimed detachment", h.records())
	}
}

// ---- THE COMMAND IS STATED ONCE ----

// TestATerminalRestatementNeverRedefinesTheCommand pins the one rule behind
// AgentBashSuccess.command being a RESTATEMENT ("repeated here so a settled
// frame describes itself"): it can fill a command nothing has stated, and it
// can never rewrite or blank one that was.
//
// This is the shape the detached-shell e2e family met head on. A detached
// shell's terminal is composed by the sidecar tailing the run's spool file,
// which knows the run only by its vendor handle — so the settled frame's line
// is that handle, not the command the caller typed. Taking it made the settled
// bubble draw a task id where its command had been.
func TestATerminalRestatementNeverRedefinesTheCommand(t *testing.T) {
	tests := []struct {
		name         string
		startLine    string
		terminalLine string
		want         string
	}{
		{
			name:         "a disagreeing terminal keeps the command the run stated",
			startLine:    "tail -f build.log",
			terminalLine: "b16522b11",
			want:         "tail -f build.log",
		},
		{
			name:         "an empty terminal does not blank the command",
			startLine:    "tail -f build.log",
			terminalLine: "",
			want:         "tail -f build.log",
		},
		{
			name:         "a terminal alone still describes itself",
			startLine:    "",
			terminalLine: "tail -f build.log",
			want:         "tail -f build.log",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tt.startLine != "" {
				h.bash("work-1", &conversationv1.AgentBashStart{
					Command:   &conversationv1.AgentBashCommand{Line: tt.startLine},
					StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
				})
			}

			// Act: the run's terminal restates a command.
			h.bash("work-1", &conversationv1.AgentBashSuccess{
				Command: &conversationv1.AgentBashCommand{Line: tt.terminalLine},
				Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
					Termination: &conversationv1.AgentBashTermination{
						How: &conversationv1.AgentBashTermination_Exited{
							Exited: &conversationv1.AgentBashExited{Code: 0},
						},
					},
				}},
			})

			// Assert.
			shell := h.shellRow()
			if got := shell.GetCommand().GetText(); got != tt.want {
				t.Fatalf("settled command = %q, want %q", got, tt.want)
			}
			if shell.GetSettled() == nil {
				t.Fatalf("state = %T, want the bubble settled", shell.GetState())
			}
		})
	}
}

// TestADetachedForegroundShellsTerminalKeepsTheCallsOwnCommand is the whole
// e2e path in one: a foreground Bash call moves to the background and its
// terminal arrives from the sidecar under the run's handle. The bubble must
// still say what was run.
func TestADetachedForegroundShellsTerminalKeepsTheCallsOwnCommand(t *testing.T) {
	// Arrange: a foreground call that then moved.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "for i in 1 2 3; do echo line-$i; done"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))
	h.detachWork("unit-1", "unit-1")

	// Act: the run's terminal, composed by a reader that knows only the handle.
	h.bash("unit-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "b6bf040a8"},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Termination: &conversationv1.AgentBashTermination{
				How: &conversationv1.AgentBashTermination_Exited{
					Exited: &conversationv1.AgentBashExited{Code: 0},
				},
			},
		}},
	})

	// Assert.
	shell := h.shellRow()
	if got := shell.GetCommand().GetText(); got != "for i in 1 2 3; do echo line-$i; done" {
		t.Fatalf("settled command = %q, want the command the call itself stated", got)
	}
	if shell.GetSettled().GetExit().GetCode() != 0 {
		t.Fatalf("settled = %+v, want the exit chip the terminal carried", shell.GetSettled())
	}
}

// Landing 11: the settled row carries the DetachedLost arm the producer named,
// on both the spawn's bubble and the shell's.

func TestSubagentFailureOutcomeCarriesEachLostArm(t *testing.T) {
	tests := []struct {
		name string
		lost *conversationv1.DetachedLost
		want func(*frontendv1.FeedSubagentLost) bool
	}{
		{
			name: "file vanished",
			lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_FileVanished{FileVanished: &conversationv1.DetachedLostFileVanished{}}},
			want: func(l *frontendv1.FeedSubagentLost) bool { return l.GetFileVanished() != nil },
		},
		{
			name: "went silent",
			lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_WentSilent{WentSilent: &conversationv1.DetachedLostWentSilent{}}},
			want: func(l *frontendv1.FeedSubagentLost) bool { return l.GetWentSilent() != nil },
		},
		{
			name: "swept up",
			lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_SweptUp{SweptUp: &conversationv1.DetachedLostSweptUp{}}},
			want: func(l *frontendv1.FeedSubagentLost) bool { return l.GetSweptUp() != nil },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			failure := &conversationv1.AgentSubagentFailure{
				Cause: &conversationv1.AgentSubagentFailure_Lost{
					Lost: tc.lost,
				},
			}
			settled := &frontendv1.FeedSubagentSettled{}

			// Act.
			subagentFailureOutcome(log, "unit-1", failure)(settled)

			// Assert.
			lost := settled.GetLost()
			if lost == nil {
				t.Fatalf("outcome = %T, want lost", settled.GetOutcome())
			}
			if !tc.want(lost) {
				t.Fatalf("how = %T, want the %s arm", lost.GetHow(), tc.name)
			}
		})
	}
}

func TestSubagentFailureWithAnUnsetLostArmIsNotDrawnAsLost(t *testing.T) {
	// Arrange: the producer said "lost" and named no way.
	log := dlog.NewTestLogger()
	failure := &conversationv1.AgentSubagentFailure{
		Cause: &conversationv1.AgentSubagentFailure_Lost{Lost: &conversationv1.DetachedLost{}},
	}
	settled := &frontendv1.FeedSubagentSettled{}

	// Act.
	subagentFailureOutcome(log, "unit-1", failure)(settled)

	// Assert: an unnamed way is no lost claim, so no lost row states one.
	if settled.GetLost() != nil {
		t.Fatalf("outcome = lost with how %T, want no lost claim", settled.GetLost().GetHow())
	}
}

func TestShellSettledCarriesEachLostArm(t *testing.T) {
	tests := []struct {
		name string
		lost *conversationv1.DetachedLost
		want func(*frontendv1.FeedShellLost) bool
	}{
		{
			name: "file vanished",
			lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_FileVanished{FileVanished: &conversationv1.DetachedLostFileVanished{}}},
			want: func(l *frontendv1.FeedShellLost) bool { return l.GetFileVanished() != nil },
		},
		{
			name: "went silent",
			lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_WentSilent{WentSilent: &conversationv1.DetachedLostWentSilent{}}},
			want: func(l *frontendv1.FeedShellLost) bool { return l.GetWentSilent() != nil },
		},
		{
			name: "swept up",
			lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_SweptUp{SweptUp: &conversationv1.DetachedLostSweptUp{}}},
			want: func(l *frontendv1.FeedShellLost) bool { return l.GetSweptUp() != nil },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			success := &conversationv1.AgentBashSuccess{
				Outcome: &conversationv1.AgentBashSuccess_Interrupted{
					Interrupted: &conversationv1.AgentBashInterrupted{
						Cause: &conversationv1.AgentBashInterrupted_Lost{
							Lost: tc.lost,
						},
					},
				},
			}

			// Act.
			settled := shellSettled(log, "work-1", success)

			// Assert.
			lost := settled.GetLost()
			if lost == nil {
				t.Fatalf("outcome = %T, want lost", settled.GetOutcome())
			}
			if !tc.want(lost) {
				t.Fatalf("how = %T, want the %s arm", lost.GetHow(), tc.name)
			}
		})
	}
}

func TestShellSettledWithAnUnsetLostArmIsNotDrawnAsLost(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	success := &conversationv1.AgentBashSuccess{
		Outcome: &conversationv1.AgentBashSuccess_Interrupted{
			Interrupted: &conversationv1.AgentBashInterrupted{
				Cause: &conversationv1.AgentBashInterrupted_Lost{Lost: &conversationv1.DetachedLost{}},
			},
		},
	}

	// Act.
	settled := shellSettled(log, "work-1", success)

	// Assert.
	if settled.GetLost() != nil {
		t.Fatalf("outcome = lost with how %T, want no lost claim", settled.GetLost().GetHow())
	}
}

// monitorActivity is a footer-only unit: proto/src/conversation/v1's
// AgentMonitor is "FOOTER-ONLY: no feed bubble exists", and it is "Always
// detached", so its announcement always names a unit the feed never draws.
func monitorActivity(unit string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Monitor{Monitor: &conversationv1.AgentMonitor{
			Result: &conversationv1.AgentMonitor_Start{Start: &conversationv1.AgentMonitorStart{
				Description: "build log",
				StartedAtMs: 1_000,
			}},
		}},
	}
}

func TestADetachmentNamingAFooterOnlyUnitIsNotWarnedWhenTheTurnEnds(t *testing.T) {
	// Arrange: the monitor's own unit arrives first, then its detachment.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(), monitorActivity("monitor-1"), noAddress())
	h.detachWork("work-1", "monitor-1")

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert: the footer carries the watch, so nothing was lost and the
	// terminal reports no producer fault.
	if h.hasRecord("warn", "daemon.feed.detached_unknown_unit") {
		t.Fatalf("records = %+v, want NO detached_unknown_unit for a footer-only unit", h.records())
	}
}

func TestADetachmentHeldBeforeAFooterOnlyUnitDrawsIsRetired(t *testing.T) {
	// Arrange: the announcement beats the unit, so it is held first.
	h := newHarness(t)
	h.detachWork("work-1", "monitor-1")

	// Act: the unit arrives, and its kind draws no row.
	h.resolver.OnActivity(testWorkspace, mainAgent(), monitorActivity("monitor-1"), noAddress())
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert.
	if !h.hasRecord("debug", "daemon.feed.detachment_retired") {
		t.Fatalf("records = %+v, want the held mark retired", h.records())
	}
	if h.hasRecord("warn", "daemon.feed.detached_unknown_unit") {
		t.Fatalf("records = %+v, want NO detached_unknown_unit once the mark is retired", h.records())
	}
}

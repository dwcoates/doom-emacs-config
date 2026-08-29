package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

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
	if len(rows) != 1 || rows[0].GetActivity().GetResponse() == nil {
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

func TestWorkDetachedFromAUnitWeNeverDrewIsWarned(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-1"},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: "never-seen"},
		}},
	}, noAddress())

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

package feed

import (
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/figures"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
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

// progressBeat advances a spawn's bubble with one running token sum.
func (h *harness) progressBeat(unit string, tokens uint64) {
	h.t.Helper()
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{
				Progress: &conversationv1.AgentSubagentProgress{TotalTokens: tokens},
			}},
		}},
	})
}

// TestSuccessiveProgressBeatsReplaceTheBubbleTokenSum locks the bubble's running
// figure to REPLACE, never sum: each beat is a whole-state running total.
func TestSuccessiveProgressBeatsReplaceTheBubbleTokenSum(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	h.progressBeat("spawn-1", 12_400)

	// Act.
	h.progressBeat("spawn-1", 20_000)

	// Assert.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetTokens().GetText(); got != "20k tok" {
		t.Fatalf("tokens = %q, want the latest beat's whole sum, never 12.4k + 20k", got)
	}
}

// TestASettledTotalSupersedesTheRunningBeat locks the proto's rule that the
// running figure is superseded by the full accounting at conclusion: a settled
// async total replaces whatever the last running beat had drawn.
func TestASettledTotalSupersedesTheRunningBeat(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	h.progressBeat("spawn-1", 8_600)

	// Act: the run settles with its reconciled total.
	settledTotal := uint64(11_114)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
				Prompt: &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Report: &conversationv1.AgentSubagentReport{},
				Totals: &conversationv1.AgentSubagentTotals{
					Usage: &conversationv1.AgentSubagentTotals_TotalOnly{
						TotalOnly: &conversationv1.AgentSubagentAsyncUsage{TotalTokens: &settledTotal},
					},
				},
			}},
		}},
	})

	// Assert.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetTokens().GetText(); got != "11.1k tok" {
		t.Fatalf("tokens = %q, want the settled total to supersede the running beat", got)
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

// TestWorkDetachedFromAUnitWeNeverDrewIsReportedUnplaceableWhenTheTurnEnds:
// the head belongs at the spawning call's row and no such row was ever drawn,
// so the work is UNPLACEABLE (owner's rule, 2026-09-23) — an ERROR naming the
// work, and a line on the topbar — and nothing is drawn for it.
func TestWorkDetachedFromAUnitWeNeverDrewIsReportedUnplaceableWhenTheTurnEnds(t *testing.T) {
	// Arrange: a detachment naming a unit nothing ever draws.
	h := newHarness(t)
	h.detachWork("work-1", "never-seen")

	// Act: the turn ends, which is the last moment the mark could have been
	// claimed.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert.
	if !h.hasRecord("error", "daemon.feed.detached_unplaceable") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.detached_unplaceable", h.records())
	}
	if got := h.warnings.keys(); !slices.Equal(got, []string{"detached_unplaceable:work-1"}) {
		t.Fatalf("raised = %v, want the unplaceable work on the topbar", got)
	}
	for _, row := range h.everyRow() {
		if row.GetShellHead() != nil {
			t.Fatalf("rows = %+v, want no shell head for unplaceable work", h.everyRow())
		}
	}
}

func TestASubagentCreatedDetachedDrawsThroughTheWrapperAtOnce(t *testing.T) {
	// Arrange, Act: work that is detached from the moment we hear of it.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-remote"}
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work:  &conversationv1.DetachedWorkId{Value: "work-1"},
		Owner: mainAgent(),
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
		Work:  &conversationv1.DetachedWorkId{Value: "work-1"},
		Owner: mainAgent(),
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

// shellHead finds the detached shell's HEAD bubble — the command, clock and
// stop, carried on whichever parent feed it landed on (shell_head arm). It is
// the canonical bubble; there is no top-level detached-shell row any more.
func (h *harness) shellHead() *frontendv1.FeedShell {
	h.t.Helper()
	for _, row := range h.everyRow() {
		if head := row.GetShellHead(); head != nil {
			return head
		}
	}
	h.t.Fatal("no detached shell HEAD bubble on any feed")
	return nil
}

// shellBody finds the detached shell's spool BODY row on the shell's sub-feed
// (detached_shell arm). It is present only once there is output; a body with no
// output is drawn nowhere, which is what onlyShellBody exists to assert.
func (h *harness) shellBody(work string) *frontendv1.FeedShell {
	h.t.Helper()
	for _, row := range h.rows(shellSubFeed(work)) {
		if body := row.GetDetachedShell(); body != nil {
			return body.GetShell()
		}
	}
	h.t.Fatalf("no detached shell BODY row on the sub-feed for %q", work)
	return nil
}

// hasShellBody reports whether the shell's spool BODY row exists on its
// sub-feed — false before any output.
func (h *harness) hasShellBody(work string) bool {
	h.t.Helper()
	for _, row := range h.rows(shellSubFeed(work)) {
		if row.GetDetachedShell() != nil {
			return true
		}
	}
	return false
}

// everyRow returns every row across every feed the workspace holds, for
// assertions that a row does — or does not — appear anywhere.
func (h *harness) everyRow() []*frontendv1.FeedRow {
	h.t.Helper()
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)
	var out []*frontendv1.FeedRow
	for _, f := range s.feeds {
		for _, id := range f.order {
			out = append(out, f.rows[id])
		}
	}
	return out
}

// bash sends one frame on a detached shell's own stream, first ARRANGING the
// run's head on the root feed when nothing has placed it yet.
//
// THE ARRANGEMENT IS EXPLICIT STATE, NOT AN INVENTED CALL. The tests that send
// through here are about the shell bubble's own rendering — its command, clock,
// spool and ending — and placement is what drawDetachedWork decides from the
// spawning card, which placement_test-style cases below exercise through
// bashUnplaced. A run with no placement draws nothing at all.
func (h *harness) bash(work string, result any) {
	h.t.Helper()
	h.resolver.mu.Lock()
	sh := h.resolver.state(testWorkspace).shell(work)
	if sh.feed.feed == (feedid.Feed{}) {
		sh.feed = placement{feed: rootFeed()}
	}
	h.resolver.mu.Unlock()
	h.bashUnplaced(work, result)
}

// bashUnplaced sends one frame on a detached shell's own stream exactly as the
// watcher would, arranging nothing.
func (h *harness) bashUnplaced(work string, result any) {
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
	shell := h.shellHead()
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

// TestABornDetachedShellMintsItsOwnCanonicalBubble pins the born-detached case:
// a shell with no spawning tool-call (the Created/OnBash path) mints its OWN
// canonical bubble — a HEAD on the parent feed, the spool as the BODY on the
// shell's own sub-feed — and NO top-level detached-shell row anywhere.
func TestABornDetachedShellMintsItsOwnCanonicalBubble(t *testing.T) {
	// Arrange, Act: a born-detached run with output.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})

	// Assert: exactly ONE head bubble on the root feed, and NO top-level
	// detached-shell (spool body) row beside it.
	var heads, topLevelBodies int
	var headRow *frontendv1.FeedRow
	for _, row := range h.rows(rootFeed()) {
		if row.GetShellHead() != nil {
			heads++
			headRow = row
		}
		if row.GetDetachedShell() != nil {
			topLevelBodies++
		}
	}
	if heads != 1 {
		t.Fatalf("head bubbles on root = %d, want exactly 1", heads)
	}
	if topLevelBodies != 0 {
		t.Fatalf("top-level detached-shell rows on root = %d, want none", topLevelBodies)
	}

	// The head carries the command and clock; the spool is the BODY on the
	// shell's own sub-feed.
	if got := headRow.GetShellHead().GetCommand().GetText(); got != "npm run dev" {
		t.Fatalf("head command = %q, want the command on the head", got)
	}
	if headRow.GetShellHead().GetSpool() != nil {
		t.Fatalf("the head carries a spool, want it spool-less (spool is the body)")
	}
	if got := h.shellBody("work-1").GetSpool().GetText(); got != "compiling\n" {
		t.Fatalf("body spool = %q, want the output on the sub-feed body", got)
	}

	// The head is a KindShellHead row keyed by the work id — the row kind whose
	// FeedId feedid.DecodeFeed resolves to the shell's own sub-feed (proven in
	// feedid's TestDecodeFeedDerivesShellSubFeedFromShellHead). The bubble's
	// body rides that sub-feed, whose OWN key the head is minted against.
	wantHeadID := testEncode(feedid.Ref{
		WS:   testWorkspace,
		Feed: rootFeed(),
		Row:  feedid.RowKey{Kind: feedid.KindShellHead, ID: "work-1"},
	}).GetValue()
	if got := headRow.GetId().GetValue(); got != wantHeadID {
		t.Fatalf("head id = %q, want the KindShellHead row keyed by the work id %q", got, wantHeadID)
	}
	// The head's sub-feed was minted (its body, asserted above, rides it), so an
	// expand's OpenFeed resolves.
	if !h.hasRecord("debug", "daemon.feed.sub_feed") {
		t.Fatalf("records = %+v, want the shell bubble's sub-feed recorded", h.records())
	}
}

// TestAShellSeenFirstWithoutAStartCountsFromWhenItWasObserved covers the
// two-plane race: the sidecar's spool tail delivers an `update` before the
// shim's re-announced `start`, so the run's first frame carries no start
// instant. The clock must count from the daemon's first-observed instant, never
// from the epoch (which drew the run as ~56 years old).
func TestAShellSeenFirstWithoutAStartCountsFromWhenItWasObserved(t *testing.T) {
	// Arrange, Act: the first frame the daemon ever sees is an update.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})

	// Assert: the runtime is stamped with the observed instant, not zero.
	if got := h.shellHead().GetRuntime().GetStartedAtMs(); got != h.nowMs {
		t.Fatalf("started_at = %d, want the first-observed instant %d (never the epoch)", got, h.nowMs)
	}
}

// TestAnAuthoritativeStartReplacesTheObservedFallback covers the correction: the
// re-announced `start` lands after the update-first fallback, and it carries the
// ORIGINAL instant, so the clock corrects back to the true start.
func TestAnAuthoritativeStartReplacesTheObservedFallback(t *testing.T) {
	// Arrange: an update-first shell drew its fallback clock.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})

	// Act: the run's own `start` re-announcement arrives, naming the true start.
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: the authoritative start wins over the fallback.
	if got := h.shellHead().GetRuntime().GetStartedAtMs(); got != 1_000 {
		t.Fatalf("started_at = %d, want the authoritative start 1000", got)
	}
}

// TestTheObservedFallbackIsStampedOnce covers that the fallback is a fixed
// instant, not the wall clock: a later push while the start is still unknown
// keeps the instant the run was first seen, so the clock does not creep forward.
func TestTheObservedFallbackIsStampedOnce(t *testing.T) {
	// Arrange: an update-first shell was first observed at the initial clock.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})
	first := h.shellHead().GetRuntime().GetStartedAtMs()

	// Act: time moves and another update lands, still with no authoritative start.
	h.nowMs += 5_000
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "ready\n", FromOffset: 10})

	// Assert: the observed instant did not creep with the clock.
	if got := h.shellHead().GetRuntime().GetStartedAtMs(); got != first {
		t.Fatalf("started_at = %d, want the once-stamped instant %d", got, first)
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
	if h.hasShellBody("work-1") {
		t.Fatalf("a spool body row exists, want none before any output")
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
	if got := h.shellBody("work-1").GetSpool().GetText(); got != "compiling\nready\n" {
		t.Fatalf("spool = %q", got)
	}
	if got := h.shellHead().GetLive().GetLastProgress().GetAtMs(); got != h.nowMs {
		t.Fatalf("last_progress = %d, want the observed append", got)
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
	if got := h.shellBody("work-1").GetSpool().GetText(); got != "compiling\n" {
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
	spool := h.shellBody("work-1").GetSpool()

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
	if h.shellBody("work-1").GetSpool().GetOmitted() != nil {
		t.Fatalf("omitted = %+v, want unset", h.shellBody("work-1").GetSpool().GetOmitted())
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
	settled := h.shellHead().GetSettled()
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
	if h.shellHead().GetSettled().GetExit() != nil {
		t.Fatalf("exit = %+v, want unset", h.shellHead().GetSettled().GetExit())
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
	if h.shellHead().GetSettled().GetExit() != nil {
		t.Fatalf("exit = %+v, want unset", h.shellHead().GetSettled().GetExit())
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
	if h.hasShellBody("work-1") {
		t.Fatalf("a spool body row exists, want none for unobserved output")
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
	if h.shellHead().GetSettled().GetCompleted() == nil {
		t.Fatalf("outcome = %T, want completed", h.shellHead().GetSettled().GetOutcome())
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
	if h.hasShellBody("work-1") {
		t.Fatalf("a spool body row exists, want none for unobserved output")
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
	if h.shellHead().GetSettled().GetLost() == nil {
		t.Fatalf("outcome = %T, want lost", h.shellHead().GetSettled().GetOutcome())
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
	if got := h.shellBody("work-1").GetSpool().GetText(); got != "compiling\n" {
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
	settled := h.shellHead().GetSettled()
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
	if h.shellHead().GetSettled().GetCancelled() == nil {
		t.Fatalf("outcome = %T, want cancelled", h.shellHead().GetSettled().GetOutcome())
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
	shell := h.shellHead()
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
	settled := h.shellHead().GetSettled()
	if settled == nil || settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("settled = %+v, want the bubble concluded", settled)
	}
}

// A SETTLED SHELL STAYS SETTLED. A run ends once, and every push after its
// terminal is a restatement of a finished run: an announcement replayed by the
// next turn's live-work reconciliation, the other plane's spool replay, a beat.
// None of them carries the ending, so a bubble drawn from the frame in hand
// walked BACK to live -- an orange dot and a stop button over a spool holding
// `EXIT=0`. Owner 13's F43 pictures caught it in the running application.

// settledShell is a run that has ended, exit 0, with output on its spool.
func settledShell(h *harness, work string) {
	h.t.Helper()
	h.bash(work, &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm test"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash(work, &conversationv1.AgentBashUpdate{FromOffset: 0, NewOutput: "line-1\n"})
	h.bash(work, &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "npm test"},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Output: &conversationv1.AgentBashOutput{},
			Termination: &conversationv1.AgentBashTermination{
				How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: 0}},
			},
		}},
		SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
	})
}

func TestAReannouncedDetachmentDoesNotUnsettleASettledShell(t *testing.T) {
	// Arrange: a run that has already ended.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "npm test"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))
	settledShell(h, "work-1")

	// Act: the next turn's live-work reconciliation announces it again.
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-1"},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: "unit-1"},
			Cause:          &conversationv1.DetachedWorkDetached_ByUser{ByUser: &conversationv1.DetachedCauseByUser{}},
		}},
	}, noAddress())

	// Assert.
	if h.shellHead().GetSettled() == nil {
		t.Fatalf("state = %T, want the bubble still settled after a replayed announcement", h.shellHead().GetState())
	}
}

func TestASpoolReplayDoesNotUnsettleASettledShell(t *testing.T) {
	// Arrange: the run ended, and the OTHER plane then re-delivers bytes it
	// already holds -- a routine two-plane replay, carrying no ending.
	h := newHarness(t)
	settledShell(h, "work-1")

	// Act
	h.bash("work-1", &conversationv1.AgentBashUpdate{FromOffset: 0, NewOutput: "line-1\n"})

	// Assert
	if h.shellHead().GetSettled() == nil {
		t.Fatalf("state = %T, want the bubble still settled after a spool replay", h.shellHead().GetState())
	}
}

func TestABeatAfterTheEndDoesNotUnsettleASettledShell(t *testing.T) {
	// Arrange: a beat says the producer saw the call alive, and it says nothing
	// about an ending.
	h := newHarness(t)
	settledShell(h, "work-1")

	// Act
	h.bash("work-1", &conversationv1.AgentToolCallProgress{LastProgressAtMs: 12_000})

	// Assert
	if h.shellHead().GetSettled() == nil {
		t.Fatalf("state = %T, want the bubble still settled after a beat", h.shellHead().GetState())
	}
}

func TestASettledShellKeepsTheEXITBytesAReplayDelivers(t *testing.T) {
	// Arrange: staying settled must not mean freezing the body -- a replay of
	// the run's own trailing bytes still belongs on the spool.
	h := newHarness(t)
	settledShell(h, "work-1")

	// Act
	h.bash("work-1", &conversationv1.AgentBashUpdate{FromOffset: 7, NewOutput: "EXIT=0\n"})

	// Assert
	if h.shellHead().GetSettled() == nil {
		t.Fatalf("state = %T, want the bubble still settled", h.shellHead().GetState())
	}
	if got := h.shellBody("work-1").GetSpool().GetText(); got != "line-1\nEXIT=0\n" {
		t.Fatalf("spool = %q, want the appended bytes on a settled row", got)
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
	if h.hasRecord("error", "daemon.feed.detached_unplaceable") {
		t.Fatalf("records = %+v, want no unplaceable report for a claimed detachment", h.records())
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
			shell := h.shellHead()
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
	shell := h.shellHead()
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
	if h.hasRecord("error", "daemon.feed.detached_unplaceable") {
		t.Fatalf("records = %+v, want NO detached_unplaceable for a footer-only unit", h.records())
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
	if h.hasRecord("error", "daemon.feed.detached_unplaceable") {
		t.Fatalf("records = %+v, want NO detached_unplaceable once the mark is retired", h.records())
	}
}

func TestARedeliveredSpoolPrefixIsAReplayRatherThanAGap(t *testing.T) {
	// Arrange: the stream plane's first chunk is already held.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})

	// Act: the file plane re-delivers the same bytes and carries the next
	// ones with them.
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\nready\n", FromOffset: 0})

	// Assert: the overlap is dropped, the new tail lands, and nothing errors.
	if got := h.shellBody("work-1").GetSpool().GetText(); got != "compiling\nready\n" {
		t.Fatalf("spool = %q, want the replayed prefix folded rather than doubled", got)
	}
	if h.hasRecord("error", "daemon.feed.spool_gap") {
		t.Fatalf("records = %+v, want NO spool_gap for a re-delivered prefix", h.records())
	}
}

func TestASpoolFrameThatRestatesHeldBytesDifferentlyIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})

	// Act: a frame that overlaps the held bytes but disagrees with them.
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "COMPILING\nready\n", FromOffset: 0})

	// Assert: a disagreement is real loss, so the frame is refused loudly.
	if got := h.shellBody("work-1").GetSpool().GetText(); got != "compiling\n" {
		t.Fatalf("spool = %q, want the frame refused", got)
	}
	if !h.hasRecord("error", "daemon.feed.spool_gap") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.spool_gap", h.records())
	}
}

// ---- CROSS-PLANE ORDER: THE START MAY LAND LAST ----
//
// One run's frames reach the daemon from TWO producers under one upsert key —
// the shim's live stream and the sidecar's file tail (shim-fanout.md, "the
// CROSS-PLANE rule") — so the file plane's settled frame can land before the
// stream plane's start. Only the start states `created_agent_id`
// (agent_activity.proto: "THE AGENT THIS SPAWN CREATED — the join key the whole
// flat model rests on"), and the bubble's own row id IS that agent's sub-feed
// address, so a bubble drawn before the start lands is a row nothing can open.

// startSubagentOnly announces one spawn without asserting a row, so a test can
// deliver the frames in either order.
func (h *harness) startSubagentOnly(unit string, created *conversationv1.AgentId, subagentType string) {
	h.t.Helper()
	prompt := &conversationv1.AgentSubagentPrompt{Text: "go and look"}
	if subagentType != "" {
		prompt.SubagentType = &subagentType
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
}

// heldSpawnFrames answers how many of a spawn's frames are still waiting for
// its start.
func (h *harness) heldSpawnFrames(unit string) int {
	h.t.Helper()
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	state, ok := h.resolver.state(testWorkspace).subagents[unit]
	if !ok {
		return 0
	}
	return len(state.held)
}

func TestASettledSpawnThatLandsBeforeItsStartStillNamesTheCreatedAgent(t *testing.T) {
	// Arrange: the file plane's terminal arrives first.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.settleSubagent("spawn-1", created, nil)

	// Act: the stream plane's start lands afterwards.
	h.startSubagentOnly("spawn-1", created, "Explore")

	// Assert: ONE row, addressing the created agent's sub-feed, settled.
	row := h.bubbleRow("spawn-1", created)
	if got := bubbleOf(row).GetSettled().GetEndedAtMs(); got != 9_000 {
		t.Fatalf("settled = %+v, want the terminal that landed first", bubbleOf(row).GetSettled())
	}
}

func TestASettledSpawnThatLandsBeforeItsStartDrawsNoUnaddressableRow(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.settleSubagent("spawn-1", created, nil)
	h.startSubagentOnly("spawn-1", created, "Explore")

	// Assert: no second bubble keyed on an empty created agent.
	unaddressable := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "spawn-1"},
	}).GetValue()
	for _, row := range h.rows(rootFeed()) {
		if row.GetId().GetValue() == unaddressable {
			t.Fatalf("rows = %v, want no bubble row that addresses no sub-feed", rowIDs(h.rows(rootFeed())))
		}
	}
}

func TestASpawnFrameHeldBeforeItsStartDrawsNothingYet(t *testing.T) {
	// Arrange, Act: only the terminal has landed.
	h := newHarness(t)
	h.settleSubagent("spawn-1", &conversationv1.AgentId{Value: "agent-explore"}, nil)

	// Assert: nothing is published until the start names the agent.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %v, want none before the start lands", rowIDs(rows))
	}
}

func TestASpawnWhoseStartLandsFirstIsDrawnAtOnce(t *testing.T) {
	// Arrange: the ordinary order, so the hold never engages.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.startSubagentOnly("spawn-1", created, "Explore")

	// Act.
	h.settleSubagent("spawn-1", created, nil)

	// Assert.
	row := h.bubbleRow("spawn-1", created)
	if got := bubbleOf(row).GetSettled().GetEndedAtMs(); got != 9_000 {
		t.Fatalf("settled = %+v, want the terminal folded onto the started bubble", bubbleOf(row).GetSettled())
	}
	if h.hasRecord("debug", "daemon.feed.subagent_held") {
		t.Fatalf("records = %+v, want no hold for a spawn whose start landed first", h.records())
	}
}

func TestAnUpdateHeldBeforeItsStartKeepsTheStartsOwnClock(t *testing.T) {
	// Arrange: an update arrives before the start.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{
				Prompt:   &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Progress: &conversationv1.AgentSubagentProgress{TotalTokens: 12_400},
			}},
		}},
	})

	// Act.
	h.startSubagentOnly("spawn-1", created, "Explore")

	// Assert: the start's instant stands and the held figure is folded.
	bubble := bubbleOf(h.bubbleRow("spawn-1", created))
	if got := bubble.GetRuntime().GetStartedAtMs(); got != 1_000 {
		t.Fatalf("runtime = %d, want the start's own instant", got)
	}
	if got := bubble.GetTokens().GetText(); got != "12.4k tok" {
		t.Fatalf("tokens = %q, want the held update's running sum", got)
	}
}

func TestNoSpawnFrameIsStillHeldOnceTheTurnEnds(t *testing.T) {
	// Arrange: a spawn whose start never arrives.
	h := newHarness(t)
	h.settleSubagent("spawn-1", &conversationv1.AgentId{Value: "agent-explore"}, nil)

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert: nothing survives the terminal.
	if held := h.heldSpawnFrames("spawn-1"); held != 0 {
		t.Fatalf("held frames = %d, want none after the turn's terminal", held)
	}
}

func TestASpawnWhoseStartNeverArrivedIsWarnedWhenTheTurnEnds(t *testing.T) {
	// Arrange: a bound producer's running beat, which names no agent and is
	// held for a start that never comes.
	h := newHarness(t)
	h.send(bound(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{
				Prompt:   &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Progress: &conversationv1.AgentSubagentProgress{TotalTokens: 12_400},
			}},
		}},
	}))

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.subagent_without_start") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.subagent_without_start", h.records())
	}
}

func TestAPreContractSpawnWhoseStartNeverArrivedIsRecordedAtInfoWhenTheTurnEnds(t *testing.T) {
	// Arrange: a settle written before the contract, which names no agent.
	h := newHarness(t)
	h.settleSubagent("spawn-1", nil, nil)

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert: expected old data, neither warned nor an error.
	if !h.hasRecord("info", "daemon.feed.settle_predates_contract") ||
		h.hasRecord("warn", "daemon.feed.subagent_without_start") || len(h.anyErrors()) != 0 {
		t.Fatalf("records = %+v, want an INFO daemon.feed.settle_predates_contract and no WARN or ERROR", h.records())
	}
}

func TestASpawnWhoseStartNeverArrivedIsStillDrawnWhenTheTurnEnds(t *testing.T) {
	// Arrange: the file plane's terminal alone, which is what a transcript-only
	// delivery carries.
	h := newHarness(t)
	h.settleSubagent("spawn-1", &conversationv1.AgentId{Value: "agent-explore"}, nil)

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert: a spawn is never lost to a start that did not come.
	found := false
	for _, row := range h.rows(rootFeed()) {
		if bubbleOf(row) != nil {
			found = true
		}
	}
	if !found {
		t.Fatalf("rows = %v, want the spawn's bubble drawn from what did arrive", rowIDs(h.rows(rootFeed())))
	}
}

func TestAStartAfterTheHoldRetiredDoesNotUnsettleTheBubble(t *testing.T) {
	// Arrange: the hold was retired at the turn's terminal.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.settleSubagent("spawn-1", created, nil)
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Act: a late start.
	h.startSubagentOnly("spawn-1", created, "Explore")

	// Assert: the settled bubble stays settled.
	if bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled() == nil {
		t.Fatalf("state = %T, want the bubble still settled", bubbleOf(h.bubbleRow("spawn-1", created)).GetState())
	}
}

// ---- A SETTLED FRAME THAT NAMES THE AGENT IT SETTLES ----
//
// AgentSubagentSuccess.created_agent_id (landing 12) exists because the success
// arm is the ONLY frame some deliveries ever carry: a replayed history, or a
// transcript-only session the sidecar read with nothing watching live. A frame
// that names the agent is drawn at once and addresses its sub-feed; one that
// does not keeps the hold-then-warn path exactly as it was.

// settleSubagentNaming settles a spawn successfully with the terminal itself
// naming the agent the spawn created.
func (h *harness) settleSubagentNaming(unit string, created *conversationv1.AgentId) {
	h.t.Helper()
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
				CreatedAgentId: created,
				Prompt:         &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Report:         &conversationv1.AgentSubagentReport{Prose: &conversationv1.AgentResponseProse{Markdown: "found it"}},
				Totals:         &conversationv1.AgentSubagentTotals{ToolUseCount: 18},
				SettledAt:      &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
			}},
		}},
	})
}

func TestASettledOnlyFrameNamingTheCreatedAgentDrawsWithNoStart(t *testing.T) {
	// Arrange, Act: the whole delivery is one settled frame.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.settleSubagentNaming("spawn-1", created)

	// Assert: the bubble is on the caller's feed already, settled.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled().GetEndedAtMs(); got != 9_000 {
		t.Fatalf("settled = %+v, want the terminal drawn without waiting for a start",
			bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled())
	}
}

func TestASettledOnlyFrameNamingTheCreatedAgentIsNotHeld(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.settleSubagentNaming("spawn-1", &conversationv1.AgentId{Value: "agent-explore"})

	// Assert: nothing waits for a start that is not coming.
	if held := h.heldSpawnFrames("spawn-1"); held != 0 {
		t.Fatalf("held frames = %d, want none for a terminal that names the agent", held)
	}
}

func TestASettledOnlyFrameNamingTheCreatedAgentIsNotWarnedAtTheTurnsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.settleSubagentNaming("spawn-1", &conversationv1.AgentId{Value: "agent-explore"})

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert: the warning is for a spawn nothing named, and this one was named.
	if h.hasRecord("warn", "daemon.feed.subagent_without_start") {
		t.Fatalf("records = %+v, want no producer-fault warning for a named terminal", h.records())
	}
}

func TestASettledOnlyFramesRowAddressesItsSubFeed(t *testing.T) {
	// Arrange: the settled-only delivery, which used to draw an unopenable row.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.settleSubagentNaming("spawn-1", created)
	row := h.bubbleRow("spawn-1", created)

	// Act: a page opened on the sub-feed the row addresses.
	page, _ := h.openPage(feedid.Feed{Agent: created}, "reader-1")

	// Assert: the crumb names the bubble's own row, so an expand resolves.
	crumbs := page.GetResult().(*frontendv1.FeedPage_Success).Success.GetBreadcrumbs().GetCrumbs()
	if len(crumbs) != 1 || crumbs[0].GetTarget().GetValue() != row.GetId().GetValue() {
		t.Fatalf("crumbs = %+v, want the bubble's own row", crumbs)
	}
}

func TestASettledOnlyFrameWithNoCreatedAgentIsStillHeld(t *testing.T) {
	// Arrange, Act: the same delivery from a producer that could not name it.
	h := newHarness(t)
	h.settleSubagent("spawn-1", &conversationv1.AgentId{Value: "agent-explore"}, nil)

	// Assert: today's behavior is untouched — held, not drawn.
	if held := h.heldSpawnFrames("spawn-1"); held != 1 {
		t.Fatalf("held frames = %d, want the unnamed terminal held as before", held)
	}
}

func TestAStartThenASuccessNamingTheSameAgentDrawsOneBubble(t *testing.T) {
	// Arrange: the ordinary live order, with the terminal now also naming.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.startSubagentOnly("spawn-1", created, "Explore")

	// Act.
	h.settleSubagentNaming("spawn-1", created)

	// Assert: one row, settled, exactly as it was before the field existed.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled().GetEndedAtMs(); got != 9_000 {
		t.Fatalf("settled = %+v, want the terminal folded onto the started bubble",
			bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled())
	}
}

func TestANamingSuccessBeforeItsStartStaysSettledWhenTheStartLands(t *testing.T) {
	// Arrange: the terminal outran the start AND named the agent, so it drew.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.settleSubagentNaming("spawn-1", created)

	// Act: the start lands afterwards and must not reopen the bubble.
	h.startSubagentOnly("spawn-1", created, "Explore")

	// Assert.
	if bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled() == nil {
		t.Fatalf("state = %T, want the bubble still settled",
			bubbleOf(h.bubbleRow("spawn-1", created)).GetState())
	}
}

func TestAnUpdateHeldBeforeANamingSuccessDoesNotRedrawItLive(t *testing.T) {
	// Arrange: an update arrives first, with nothing yet naming the agent.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{
				Prompt:   &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Progress: &conversationv1.AgentSubagentProgress{TotalTokens: 12_400},
			}},
		}},
	})

	// Act: the naming terminal releases the hold.
	h.settleSubagentNaming("spawn-1", created)

	// Assert: the run's order wins over the arrival order — settled, not live.
	if bubbleOf(h.bubbleRow("spawn-1", created)).GetSettled() == nil {
		t.Fatalf("state = %T, want the terminal to fold after the frames it outran",
			bubbleOf(h.bubbleRow("spawn-1", created)).GetState())
	}
}

func TestAnUpdateHeldBeforeANamingSuccessIsStillFoldedIn(t *testing.T) {
	// Arrange: the held update carries the only running token sum.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{
				Prompt:   &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Progress: &conversationv1.AgentSubagentProgress{TotalTokens: 12_400},
			}},
		}},
	})

	// Act.
	h.settleSubagentNaming("spawn-1", created)

	// Assert: the hold is drained rather than dropped.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetTokens().GetText(); got != "12.4k tok" {
		t.Fatalf("tokens = %q, want the held update's running sum folded in", got)
	}
}

func TestABeatCarryingAnOlderProducerInstantDoesNotWindTheShellsAgeBackwards(t *testing.T) {
	// Arrange: an append the daemon stamps on receipt. This is the drawn
	// instant the contract names — FeedShellLive is "spool growth IS the beat,
	// the daemon stamps it on each append it observes".
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bash("work-1", &conversationv1.AgentBashUpdate{NewOutput: "compiling\n", FromOffset: 0})
	appended := h.nowMs

	// Act: a liveness beat whose PRODUCER instant predates that append, which
	// is routine — the shim stamps when it observed the vendor's beat, the
	// daemon stamps when the bytes reached it, and the two are different
	// observers at different points in the pipe.
	h.bash("work-1", &conversationv1.AgentToolCallProgress{LastProgressAtMs: appended - 30_000})

	// Assert: the drawn instant is still the daemon's own append stamp. The
	// beat reports no growth, so it has nothing to say about the last growth,
	// and the client's "quiet for N" never runs backwards.
	shell := h.shellHead()
	if got := shell.GetLive().GetLastProgress().GetAtMs(); got != appended {
		t.Fatalf("last_progress = %d, want the daemon's own append stamp %d", got, appended)
	}
}

func TestABeatBeforeAnyOutputLeavesTheShellsLastProgressUnset(t *testing.T) {
	// Arrange: a shell that has announced but printed nothing.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act: a beat arrives with a producer instant on it.
	h.bash("work-1", &conversationv1.AgentToolCallProgress{LastProgressAtMs: 5_000})

	// Assert: UNSET, because the field is "the last output the daemon
	// observed" and no output has been observed. A beat must not manufacture
	// one out of a producer's stamp for a different fact.
	if h.shellHead().GetLive().GetLastProgress() != nil {
		t.Fatalf("last_progress = %v, want unset before the first byte", h.shellHead().GetLive().GetLastProgress())
	}
}

// settleSubagentOnly delivers a SETTLED-ONLY success — the shape a replayed
// history carries, with no start frame ever arriving — naming the created agent
// on the success itself so the bubble is addressable.
func (h *harness) settleSubagentOnly(unit string, created *conversationv1.AgentId, totals *conversationv1.AgentSubagentTotals, endedAtMs int64) {
	h.t.Helper()
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
				CreatedAgentId: created,
				Prompt:         &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				Report:         &conversationv1.AgentSubagentReport{},
				Totals:         totals,
				SettledAt:      &conversationv1.AgentActivitySettledAt{AtMs: endedAtMs},
			}},
		}},
	})
}

// TestASettledFullTotalCountsEveryTokenIncludingCacheReads locks the head's
// figure to the RUN'S TOTAL — every token, cache reads included — rather than
// the expensive-input-plus-output partial it once drew, which understated the
// total by the cached context a subagent reads.
func TestASettledFullTotalCountsEveryTokenIncludingCacheReads(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act: a sync run's full billed breakdown, cache reads dominating.
	h.settleSubagentOnly("spawn-1", created, &conversationv1.AgentSubagentTotals{
		Usage: &conversationv1.AgentSubagentTotals_Full{Full: &conversationv1.TokenUsage{
			InputHits:    &conversationv1.TokenCacheHits{Read: 470_000},
			InputMisses:  &conversationv1.TokenCacheMisses{Written: 3_000, Unwritten: 1_000},
			OutputTokens: 1_000,
		}},
	}, 9_000)

	// Assert: 470k + 3k + 1k + 1k = 475k, never the 5k the old sum drew.
	want := figures.Tokens(475_000) + " tok"
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetTokens().GetText(); got != want {
		t.Fatalf("tokens = %q, want %q (the run's total, cache reads included)", got, want)
	}
}

// TestASettledOnlyReplayReconstructsTheClockFromItsDuration locks the fix for
// the absurd clock: a settled bubble delivered with no start frame reconstructs
// its start as end − duration, so the settled clock shows the run's real span
// rather than the whole age of the epoch.
func TestASettledOnlyReplayReconstructsTheClockFromItsDuration(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act: a settled-only delivery whose totals state the run took 540s.
	const endedAtMs, durationMs = int64(1_700_000_009_000), uint64(540_000)
	h.settleSubagentOnly("spawn-1", created, &conversationv1.AgentSubagentTotals{
		DurationMs: durationMs,
	}, endedAtMs)

	// Assert: start = end − duration, never zero.
	got := bubbleOf(h.bubbleRow("spawn-1", created)).GetRuntime().GetStartedAtMs()
	if got != endedAtMs-int64(durationMs) {
		t.Fatalf("started_at_ms = %d, want end − duration %d (never the epoch)", got, endedAtMs-int64(durationMs))
	}
}

// TestALiveSpawnWithNoStartInstantCountsFromFirstObserved locks the live-path
// fallback: a start that named no instant leaves the clock at zero, so the
// bubble is stamped with the first-observed instant rather than counting up
// from the epoch.
func TestALiveSpawnWithNoStartInstantCountsFromFirstObserved(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act: a start that names the agent but carries no started_at instant.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: created,
				Prompt:         &conversationv1.AgentSubagentPrompt{Text: "go and look"},
			}},
		}},
	})

	// Assert: the first-observed instant (the injected clock), never zero.
	got := bubbleOf(h.bubbleRow("spawn-1", created)).GetRuntime().GetStartedAtMs()
	if got != h.nowMs {
		t.Fatalf("started_at_ms = %d, want the first-observed instant %d (never zero)", got, h.nowMs)
	}
}

// A MOVE THAT LANDS AFTER THE CALL ENDED. The call's result and its move are
// separate records and can arrive in either order; a result drawn first is
// still the work's ending, so the head is drawn settled from it, never live.

func TestAMoveAnnouncedAfterTheCallSucceededDrawsTheHeadSettled(t *testing.T) {
	// Arrange: the call ran and returned where it ran.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")
	h.send(activityOf("unit-1", exitedSuccess("go test ./...", 0, 9_000)))

	// Act: the move is announced late.
	h.detachWork("work-1", "unit-1")

	// Assert.
	settled := h.shellHead().GetSettled()
	if settled.GetCompleted() == nil || settled.GetExit().GetCode() != 0 || settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("settled = %+v, want completed exit 0 at 9000", settled)
	}
}

func TestAMoveAnnouncedAfterTheCallFailedDrawsTheHeadCancelled(t *testing.T) {
	// Arrange: the call's input line comes from its start; its failure states none.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")
	h.send(activityOf("unit-1", failedCall(9_000)))

	// Act.
	h.detachWork("work-1", "unit-1")

	// Assert.
	settled := h.shellHead().GetSettled()
	if settled.GetCancelled() == nil || settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("settled = %+v, want cancelled at 9000", settled)
	}
}

func TestAMoveOfAStillRunningCallDrawsTheHeadLive(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")

	// Act.
	h.detachWork("work-1", "unit-1")

	// Assert.
	if h.shellHead().GetLive() == nil {
		t.Fatalf("state = %T, want the head live", h.shellHead().GetState())
	}
}

func TestShellEndingRendersNothingForAFrameThatIsNotATerminal(t *testing.T) {
	tests := []struct {
		name string
		bash *conversationv1.AgentBash
	}{
		{name: "no frame at all", bash: nil},
		{name: "a start", bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{}}}},
		{name: "an update", bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Update{Update: &conversationv1.AgentBashUpdate{}}}},
		{name: "a beat", bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Progress{Progress: &conversationv1.AgentToolCallProgress{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			settled := shellEnding(log, "work-1", tt.bash)

			// Assert.
			if settled != nil {
				t.Fatalf("settled = %+v, want nil", settled)
			}
		})
	}
}

// A RUN THAT LEFT THE LIVE SET HAS ENDED FOR EVERY READER. The live set is the
// watcher's open watch set; a shell that leaves it with no terminal of its own
// will never report again, so its head settles lost rather than drawing an
// orange dot forever.

// liveShells is the watcher's live-work publication naming these shells.
func liveShells(works ...string) sessionwatcher.LiveWorkSet {
	var live sessionwatcher.LiveWorkSet
	for _, work := range works {
		live.Shells = append(live.Shells, &conversationv1.DetachedWorkId{Value: work})
	}
	return live
}

// runningShell draws a detached shell that has started and not ended.
func runningShell(h *harness, work string) {
	h.t.Helper()
	h.bash(work, &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm run dev"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
}

func TestAShellThatLeftTheLiveSetUnsettledIsSettledLost(t *testing.T) {
	// Arrange: the run is drawn and the live set holds it.
	h := newHarness(t)
	runningShell(h, "work-1")
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells("work-1"))

	// Act: the set is republished without it, and no terminal came.
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells())

	// Assert: lost, with no cause claimed, ended now.
	settled := h.shellHead().GetSettled()
	if settled.GetLost() == nil || settled.GetLost().GetHow() != nil {
		t.Fatalf("settled = %+v, want lost with no cause", settled)
	}
	if settled.GetEndedAtMs() != h.nowMs {
		t.Fatalf("ended = %d, want the daemon's clock %d", settled.GetEndedAtMs(), h.nowMs)
	}
}

func TestAShellThatLeftTheLiveSetUnsettledIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	runningShell(h, "work-1")
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells("work-1"))

	// Act.
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells())

	// Assert.
	if !h.hasRecord("info", "daemon.feed.detached_shell_left_live") {
		t.Fatalf("records = %+v, want the settle recorded at info", h.records())
	}
}

func TestAShellItsOwnTerminalSettledKeepsThatEndingWhenItLeavesTheLiveSet(t *testing.T) {
	// Arrange: the store's conclusion arrives first, as it does at a terminal.
	h := newHarness(t)
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells("work-1"))
	settledShell(h, "work-1")

	// Act.
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells())

	// Assert: completed exit 0, as the run itself said.
	settled := h.shellHead().GetSettled()
	if settled.GetCompleted() == nil || settled.GetExit().GetCode() != 0 || settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("settled = %+v, want the run's own completed ending", settled)
	}
	if h.hasRecord("info", "daemon.feed.detached_shell_left_live") {
		t.Fatalf("records = %+v, want no lost settle for a run that ended itself", h.records())
	}
}

func TestAShellTheLiveSetNeverHeldIsNotSettledByASetWithoutIt(t *testing.T) {
	// Arrange: drawn, but the watcher never held it (its first open refused).
	h := newHarness(t)
	runningShell(h, "work-1")

	// Act.
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells())

	// Assert.
	if h.shellHead().GetLive() == nil {
		t.Fatalf("state = %T, want the head still live", h.shellHead().GetState())
	}
}

func TestAShellStillInTheLiveSetStaysLive(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	runningShell(h, "work-1")
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells("work-1"))

	// Act: another change that still lists it.
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells("work-1", "work-2"))

	// Assert.
	if h.shellHead().GetLive() == nil {
		t.Fatalf("state = %T, want the head still live", h.shellHead().GetState())
	}
}

func TestAShellHeldBeforeItWasDrawnIsNotDrawnWhenItLeaves(t *testing.T) {
	// Arrange: the set held a run the feed has drawn nothing for.
	h := newHarness(t)
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells("work-1"))

	// Act.
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells())

	// Assert: nothing is invented for a run no bubble shows.
	for _, row := range h.everyRow() {
		if row.GetShellHead() != nil {
			t.Fatalf("a shell head was drawn for a run the feed never drew")
		}
	}
}

func TestAShellHeldBeforeItWasDrawnIsSettledLostWhenItLeavesAfterDrawing(t *testing.T) {
	// Arrange: the set held it first, then its bubble was drawn.
	h := newHarness(t)
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells("work-1"))
	runningShell(h, "work-1")

	// Act.
	h.resolver.OnLiveWorkChanged(testWorkspace, liveShells())

	// Assert.
	if h.shellHead().GetSettled().GetLost() == nil {
		t.Fatalf("state = %T, want the head settled lost", h.shellHead().GetState())
	}
}

// ---- DETACHED WORK IS DRAWN ONLY IN THE FEED IT BELONGS TO ----
//
// Owner's rule (2026-09-23): work spawned by the main agent is drawn on the
// root; work a subagent spawned is drawn in that subagent's own feed; each at
// its spawning call's row, and never anywhere by default. The regression these
// pin: a subagent's `npm test`, announced on the main agent's book, drawn on the
// root under the last final answer with no turn.

// sendAs pushes one activity as the given agent's own frame.
func (h *harness) sendAs(agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, agent, act, noAddress())
}

// bashCall is a Bash call's running card, as the calling agent's stream states it.
func bashCall(unit, command string) *conversationv1.AgentActivity {
	return activityOf(unit, &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: command},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 2_000},
	}}})
}

// spawnCall is an Agent call's start, naming the agent it created.
func spawnCall(unit string, created *conversationv1.AgentId) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: created,
				Prompt:         &conversationv1.AgentSubagentPrompt{Text: "go"},
				StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			}},
		}},
	}
}

// announceDetachment announces, on the announcer's book, that the unit's work
// left — naming its owner when owner is non-nil.
func (h *harness) announceDetachment(announcer, owner *conversationv1.AgentId, work, unit string) {
	h.t.Helper()
	h.resolver.OnDetachedWork(testWorkspace, announcer, &conversationv1.AgentDetachedWork{
		Work:  &conversationv1.DetachedWorkId{Value: work},
		Owner: owner,
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: unit},
			Cause:          &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}},
		}},
	}, noAddress())
}

// agentFeed is one agent's sub-feed.
func agentFeed(agent *conversationv1.AgentId) feedid.Feed { return feedid.Feed{Agent: agent} }

// rowKinds names each row of a feed by its arm, in order, for an assertion on
// where a head landed among its neighbours.
func (h *harness) rowKinds(feed feedid.Feed) []string {
	h.t.Helper()
	var out []string
	for _, row := range h.rows(feed) {
		switch {
		case row.GetShellHead() != nil:
			out = append(out, "shell_head")
		case row.GetActivity().GetSimpleToolCall() != nil:
			out = append(out, "tool_card")
		case row.GetActivity().GetResponse() != nil:
			out = append(out, "response")
		case row.GetActivity().GetSubagent() != nil:
			out = append(out, "subagent")
		case row.GetDetachedSubagent() != nil:
			out = append(out, "detached_subagent")
		case row.GetAgentPrompt() != nil:
			out = append(out, "agent_prompt")
		default:
			out = append(out, "other")
		}
	}
	return out
}

// shellHeadFeeds answers every feed a shell head was drawn on, by its test
// spelling.
func (h *harness) shellHeadFeeds() []string {
	h.t.Helper()
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)
	var out []string
	for key, f := range s.feeds {
		for _, id := range f.order {
			if f.rows[id].GetShellHead() != nil {
				out = append(out, testFeedValue(s.feedAddrs[key]))
			}
		}
	}
	slices.Sort(out)
	return out
}

func TestADetachedShellIsDrawnInItsOwnersFeedAtItsCallsRow(t *testing.T) {
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	nested := &conversationv1.AgentId{Value: "agent-nested"}
	for _, tc := range []struct {
		name string
		// arrange draws the call's card and a row after it, answering the
		// agent that carried the call.
		arrange func(h *harness) *conversationv1.AgentId
		// owner is what the announcement states; nil states none.
		owner     func(carrier *conversationv1.AgentId) *conversationv1.AgentId
		wantFeed  func(carrier *conversationv1.AgentId) feedid.Feed
		wantKinds []string
	}{
		{
			name: "the main agent's shell, its owner stated, is drawn on the root in its card's place",
			arrange: func(h *harness) *conversationv1.AgentId {
				h.send(bashCall("toolu_bash", "npm test"))
				h.send(responseSuccessActivity("unit-later", "still working"))
				return mainAgent()
			},
			owner:     func(c *conversationv1.AgentId) *conversationv1.AgentId { return c },
			wantFeed:  func(*conversationv1.AgentId) feedid.Feed { return rootFeed() },
			wantKinds: []string{"shell_head", "response"},
		},
		{
			name: "the main agent's shell, no owner stated, is placed by its call's carrier",
			arrange: func(h *harness) *conversationv1.AgentId {
				h.send(bashCall("toolu_bash", "npm test"))
				h.send(responseSuccessActivity("unit-later", "still working"))
				return mainAgent()
			},
			owner:     func(*conversationv1.AgentId) *conversationv1.AgentId { return nil },
			wantFeed:  func(*conversationv1.AgentId) feedid.Feed { return rootFeed() },
			wantKinds: []string{"shell_head", "response"},
		},
		{
			name: "a subagent's shell announced on the main book, no owner stated, is drawn in the subagent's feed",
			arrange: func(h *harness) *conversationv1.AgentId {
				h.send(spawnCall("toolu_spawn", sub))
				h.sendAs(sub, bashCall("toolu_bash", "npm test"))
				h.sendAs(sub, responseSuccessActivity("unit-later", "still working"))
				return sub
			},
			owner:     func(*conversationv1.AgentId) *conversationv1.AgentId { return nil },
			wantFeed:  agentFeed,
			wantKinds: []string{"agent_prompt", "shell_head", "response"},
		},
		{
			name: "a subagent's shell whose announcement names the subagent is drawn in the subagent's feed",
			arrange: func(h *harness) *conversationv1.AgentId {
				h.send(spawnCall("toolu_spawn", sub))
				h.sendAs(sub, bashCall("toolu_bash", "npm test"))
				h.sendAs(sub, responseSuccessActivity("unit-later", "still working"))
				return sub
			},
			owner:     func(c *conversationv1.AgentId) *conversationv1.AgentId { return c },
			wantFeed:  agentFeed,
			wantKinds: []string{"agent_prompt", "shell_head", "response"},
		},
		{
			name: "a nested subagent's shell is drawn in the nested subagent's feed",
			arrange: func(h *harness) *conversationv1.AgentId {
				h.send(spawnCall("toolu_spawn", sub))
				h.sendAs(sub, spawnCall("toolu_spawn_nested", nested))
				h.sendAs(nested, bashCall("toolu_bash", "npm test"))
				h.sendAs(nested, responseSuccessActivity("unit-later", "still working"))
				return nested
			},
			owner:     func(*conversationv1.AgentId) *conversationv1.AgentId { return nil },
			wantFeed:  agentFeed,
			wantKinds: []string{"agent_prompt", "shell_head", "response"},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			carrier := tc.arrange(h)

			// Act: the task stream announces it on the MAIN agent's book.
			h.announceDetachment(mainAgent(), tc.owner(carrier), "toolu_bash", "toolu_bash")

			// Assert: one head, on the owner's feed only, where the card stood.
			want := testFeedValue(tc.wantFeed(carrier))
			if got := h.shellHeadFeeds(); !slices.Equal(got, []string{want}) {
				t.Fatalf("shell heads on %v, want exactly one on %s", got, want)
			}
			if got := h.rowKinds(tc.wantFeed(carrier)); !slices.Equal(got, tc.wantKinds) {
				t.Fatalf("rows of %s = %v, want %v: the head replaces its card in place", want, got, tc.wantKinds)
			}
			if len(h.warnings.keys()) != 0 {
				t.Fatalf("raised = %v, want nothing raised for placeable work", h.warnings.keys())
			}
		})
	}
}

func TestADetachedShellHeadCarriesItsSpawningTurn(t *testing.T) {
	// Arrange: the call was made in turn-1; turn-2 is running when it moves.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "run the tests")
	h.send(bashCall("toolu_bash", "npm test"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))

	// Act.
	h.announceDetachment(mainAgent(), mainAgent(), "toolu_bash", "toolu_bash")

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetShellHead() != nil {
			if got := row.GetTurn().GetValue(); got != "turn-1" {
				t.Fatalf("head turn = %q, want the spawning turn turn-1", got)
			}
			return
		}
	}
	t.Fatal("no shell head on the root feed")
}

func TestADetachedSubagentShellHeadNeverTakesTheRunningTurn(t *testing.T) {
	// Arrange: a subagent's card drawn between turns carries no turn; a turn
	// is running when its work moves.
	h := newHarness(t)
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	h.send(spawnCall("toolu_spawn", sub))
	h.sendAs(sub, bashCall("toolu_bash", "npm test"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-later"))

	// Act.
	h.announceDetachment(mainAgent(), nil, "toolu_bash", "toolu_bash")

	// Assert.
	for _, row := range h.rows(agentFeed(sub)) {
		if row.GetShellHead() != nil {
			if got := row.GetTurn().GetValue(); got == "turn-later" {
				t.Fatalf("head turn = %q, want the card's own turn, never the one running", got)
			}
			return
		}
	}
	t.Fatal("no shell head on the subagent's feed")
}

func TestADetachmentWhoseOwnerContradictsItsCallsCarrierDrawsNothing(t *testing.T) {
	// Arrange: the main agent made the call; the announcement names another.
	h := newHarness(t)
	h.send(bashCall("toolu_bash", "npm test"))

	// Act.
	h.announceDetachment(mainAgent(), &conversationv1.AgentId{Value: "agent-other"}, "toolu_bash", "toolu_bash")

	// Assert.
	if got := h.shellHeadFeeds(); len(got) != 0 {
		t.Fatalf("shell heads on %v, want none for contradictory ownership", got)
	}
	if got := h.rowKinds(rootFeed()); !slices.Equal(got, []string{"tool_card"}) {
		t.Fatalf("root rows = %v, want the card left as it stood", got)
	}
	if !h.hasRecord("error", "daemon.feed.detached_unplaceable") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.detached_unplaceable", h.records())
	}
	if got := h.warnings.keys(); !slices.Equal(got, []string{"detached_unplaceable:toolu_bash"}) {
		t.Fatalf("raised = %v, want the work on the topbar", got)
	}
}

func TestTheUnplaceableRecordNamesTheWorkKindOwnerAnnouncerAndReason(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(bashCall("toolu_bash", "npm test"))

	// Act.
	h.announceDetachment(mainAgent(), &conversationv1.AgentId{Value: "agent-other"}, "toolu_bash", "toolu_bash")

	// Assert.
	for _, record := range h.records() {
		if record.Level != "error" || record.Operation != "daemon.feed.detached_unplaceable" {
			continue
		}
		for field, want := range map[string]string{
			"work": "toolu_bash", "kind": "shell", "owner": "agent-other", "announcer": "agent-main",
		} {
			if got := record.Context[field]; got != want {
				t.Errorf("%s = %v, want %q", field, got, want)
			}
		}
		if reason, _ := record.Context["reason"].(string); reason == "" {
			t.Error("reason is empty, want why the work could not be placed")
		}
		return
	}
	t.Fatalf("records = %+v, want the ERROR record", h.records())
}

func TestASubagentsMonitorDrawsNothingInAnyFeed(t *testing.T) {
	// Arrange: a monitor is footer-only, whoever arms it.
	h := newHarness(t)
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	h.send(spawnCall("toolu_spawn", sub))
	h.sendAs(sub, monitorActivity("toolu_monitor"))

	// Act.
	h.announceDetachment(mainAgent(), nil, "toolu_monitor", "toolu_monitor")

	// Assert.
	if got := h.rowKinds(rootFeed()); !slices.Equal(got, []string{"subagent"}) {
		t.Fatalf("root rows = %v, want the spawn's bubble alone", got)
	}
	if got := h.rowKinds(agentFeed(sub)); !slices.Equal(got, []string{"agent_prompt"}) {
		t.Fatalf("subagent rows = %v, want the commission alone", got)
	}
	if len(h.warnings.keys()) != 0 {
		t.Fatalf("raised = %v, want nothing: a monitor drawing no row is its design", h.warnings.keys())
	}
}

func TestANestedSubagentsDetachmentKeepsItsBubbleInItsSpawnersFeed(t *testing.T) {
	// Arrange: a subagent spawned a subagent of its own.
	h := newHarness(t)
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	nested := &conversationv1.AgentId{Value: "agent-nested"}
	h.send(spawnCall("toolu_spawn", sub))
	h.sendAs(sub, spawnCall("toolu_nested", nested))

	// Act: the task stream announces the move on the MAIN book, then reports
	// the run's progress there too.
	h.announceDetachment(mainAgent(), nil, "toolu_nested", "toolu_nested")
	h.progressBeat("toolu_nested", 1_200)

	// Assert.
	if got := h.rowKinds(agentFeed(sub)); !slices.Equal(got, []string{"agent_prompt", "detached_subagent"}) {
		t.Fatalf("spawner's rows = %v, want the nested bubble there, detached", got)
	}
	if got := h.rowKinds(rootFeed()); !slices.Equal(got, []string{"subagent"}) {
		t.Fatalf("root rows = %v, want only the first spawn's bubble", got)
	}
}

func TestACreatedSubagentWithNoKnownOwnerDrawsNothing(t *testing.T) {
	// Arrange, Act: work detached from birth, naming no owner, whose spawn
	// was never drawn.
	h := newHarness(t)
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "work-1"},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Subagent{
				Subagent: spawnCall("work-1", &conversationv1.AgentId{Value: "agent-remote"}).GetSubagent(),
			}},
		}},
	}, noAddress())

	// Assert.
	if rows := h.everyRow(); len(rows) != 0 {
		t.Fatalf("rows = %+v, want nothing drawn for work with no known owner", rows)
	}
	if !h.hasRecord("error", "daemon.feed.detached_unplaceable") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.detached_unplaceable", h.records())
	}
	if got := h.warnings.keys(); !slices.Equal(got, []string{"detached_unplaceable:work-1"}) {
		t.Fatalf("raised = %v, want the work on the topbar", got)
	}
}

func TestACreatedShellIsHeldUntilItsCallDrawsAndThenReplacesIt(t *testing.T) {
	// Arrange: a re-announced live shell whose call the feed has not drawn.
	h := newHarness(t)
	h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
		Work:  &conversationv1.DetachedWorkId{Value: "toolu_bash"},
		Owner: mainAgent(),
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Bash{
				Bash: bashCall("toolu_bash", "npm test").GetBash(),
			}},
		}},
	}, noAddress())
	if got := h.shellHeadFeeds(); len(got) != 0 {
		t.Fatalf("shell heads on %v before the call drew, want none", got)
	}

	// Act: the call's card draws.
	h.send(bashCall("toolu_bash", "npm test"))

	// Assert.
	if got := h.rowKinds(rootFeed()); !slices.Equal(got, []string{"shell_head"}) {
		t.Fatalf("root rows = %v, want the head in the card's place", got)
	}
}

func TestAnUnplacedShellsFramesPublishNothing(t *testing.T) {
	// Arrange, Act: the run's own stream reports before anything placed it.
	h := newHarness(t)
	h.bashUnplaced("work-1", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm test"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.bashUnplaced("work-1", &conversationv1.AgentBashUpdate{NewOutput: "PASS\n", FromOffset: 0})

	// Assert: nothing on any feed — and never on the root in its place.
	if rows := h.everyRow(); len(rows) != 0 {
		t.Fatalf("rows = %+v, want nothing published for an unplaced run", rows)
	}
	if !h.hasRecord("debug", "daemon.feed.detached_shell_unplaced") {
		t.Fatalf("records = %+v, want the fold recorded", h.records())
	}
}

func TestAnUnplacedShellsSpoolIsDrawnWholeOnceItsCallPlacesIt(t *testing.T) {
	// Arrange: output arrives on the run's stream before its call's card.
	h := newHarness(t)
	h.announceDetachment(mainAgent(), mainAgent(), "toolu_bash", "toolu_bash")
	h.bashUnplaced("toolu_bash", &conversationv1.AgentBashUpdate{NewOutput: "PASS\n", FromOffset: 0})

	// Act.
	h.send(bashCall("toolu_bash", "npm test"))

	// Assert.
	if got := h.shellBody("toolu_bash").GetSpool().GetText(); got != "PASS\n" {
		t.Fatalf("spool = %q, want the output folded while unplaced", got)
	}
}

func TestARestatedMoveKeepsTheHeadWhereAndWhenItWasSpawned(t *testing.T) {
	// Arrange: the move landed, and turn-2 is running when it is restated.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "run the tests")
	h.send(bashCall("toolu_bash", "npm test"))
	h.send(responseSuccessActivity("unit-later", "still working"))
	h.announceDetachment(mainAgent(), mainAgent(), "toolu_bash", "toolu_bash")
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))

	// Act: the next turn's reconciliation restates the same move.
	h.announceDetachment(mainAgent(), mainAgent(), "toolu_bash", "toolu_bash")

	// Assert.
	if got := h.rowKinds(rootFeed()); !slices.Equal(got, []string{"other", "shell_head", "response"}) {
		t.Fatalf("root rows = %v, want the head still in its card's place", got)
	}
	for _, row := range h.rows(rootFeed()) {
		if row.GetShellHead() != nil && row.GetTurn().GetValue() != "turn-1" {
			t.Fatalf("head turn = %q, want the spawning turn kept", row.GetTurn().GetValue())
		}
	}
}

// ---- the detached-work id and the entry placement ---------------------------

func TestTheDetachedWorkIdIsOnEveryAsyncHeadAndNoSyncOne(t *testing.T) {
	created := &conversationv1.AgentId{Value: "agent-explore"}
	tests := []struct {
		name string
		act  func(h *harness) *frontendv1.FeedDetachedWorkId
		want string
	}{
		{
			name: "a synchronous spawn names no work",
			act: func(h *harness) *frontendv1.FeedDetachedWorkId {
				return bubbleOf(h.spawnSubagent("spawn-1", created, "Explore", "map")).GetWorkId()
			},
			want: "",
		},
		{
			name: "a spawn that detached names the announcement's handle",
			act: func(h *harness) *frontendv1.FeedDetachedWorkId {
				h.spawnSubagent("spawn-1", created, "Explore", "map")
				h.detachWork("work-1", "spawn-1")
				return bubbleOf(h.bubbleRow("spawn-1", created)).GetWorkId()
			},
			want: "work-1",
		},
		{
			name: "a detachment announced before the spawn drew is named once it draws",
			act: func(h *harness) *frontendv1.FeedDetachedWorkId {
				h.detachWork("work-1", "spawn-1")
				return bubbleOf(h.spawnSubagent("spawn-1", created, "Explore", "map")).GetWorkId()
			},
			want: "work-1",
		},
		{
			name: "a subagent created detached names its own handle",
			act: func(h *harness) *frontendv1.FeedDetachedWorkId {
				h.resolver.OnDetachedWork(testWorkspace, mainAgent(), &conversationv1.AgentDetachedWork{
					Work: &conversationv1.DetachedWorkId{Value: "work-7"},
					// The producer states the owner of work created detached.
					Owner: mainAgent(),
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
				return bubbleOf(h.rows(rootFeed())[0]).GetWorkId()
			},
			want: "work-7",
		},
		{
			name: "a shell head names its work",
			act: func(h *harness) *frontendv1.FeedDetachedWorkId {
				h.bash("work-3", &conversationv1.AgentBashStart{
					Command:   &conversationv1.AgentBashCommand{Line: "npm test"},
					StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
				})
				return h.shellHead().GetWorkId()
			},
			want: "work-3",
		},
		{
			name: "a shell's spool body names none",
			act: func(h *harness) *frontendv1.FeedDetachedWorkId {
				h.bash("work-3", &conversationv1.AgentBashStart{
					Command:   &conversationv1.AgentBashCommand{Line: "npm test"},
					StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
				})
				h.bash("work-3", &conversationv1.AgentBashUpdate{NewOutput: "ok\n", FromOffset: 0})
				return h.shellBody("work-3").GetWorkId()
			},
			want: "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			got := tt.act(h)

			// Assert
			if got.GetText() != tt.want {
				t.Fatalf("work id = %q, want %q", got.GetText(), tt.want)
			}
			if tt.want == "" && got != nil {
				t.Fatalf("work id element = %+v, want it unset", got)
			}
		})
	}
}

func TestAnEntryIsAnnouncedOnTheFeedThatDrawsIt(t *testing.T) {
	// Arrange: a subagent spawned by the main agent, then one spawned BY it.
	h := newHarness(t)
	outer := &conversationv1.AgentId{Value: "agent-outer"}
	inner := &conversationv1.AgentId{Value: "agent-inner"}
	h.spawnSubagent("spawn-outer", outer, "opus", "lead")
	prompt := &conversationv1.AgentSubagentPrompt{Text: "go"}

	// Act: the nested spawn arrives on the OUTER agent's stream.
	h.resolver.OnActivity(testWorkspace, outer, &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-inner"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: inner, Prompt: prompt,
				StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			}},
		}},
	}, noAddress())

	// Assert
	want := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: feedid.Feed{Agent: outer},
		Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "spawn-inner", Sub: "agent-inner"},
	}).GetValue()
	last := h.placed[len(h.placed)-1]
	if last.unit != "spawn-inner" || last.row != want {
		t.Fatalf("placed = %+v, want spawn-inner at %q on the outer agent's sub-feed", last, want)
	}
}

func TestARedrawAtTheSameAddressAnnouncesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map")
	before := len(h.placed)

	// Act
	h.progressBeat("spawn-1", 10)

	// Assert
	if len(h.placed) != before || before != 1 {
		t.Fatalf("placements = %d then %d, want exactly one for the first draw", before, len(h.placed))
	}
}

func TestAShellHeadIsAnnouncedByItsWorkId(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	h.bash("work-3", &conversationv1.AgentBashStart{
		Command:   &conversationv1.AgentBashCommand{Line: "npm test"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert
	if len(h.placed) != 1 || h.placed[0].unit != "work-3" {
		t.Fatalf("placed = %+v, want the shell head announced under its work id", h.placed)
	}
}

// A DETACHED SHELL'S FAILURE RESTATES ITS COMMAND, as the success does, so a
// head drawn from the ending alone still names what ran.
func TestADetachedShellsFailureNamesTheCommandItRestated(t *testing.T) {
	// Arrange, Act: the ending alone, as a replay serves it.
	h := newHarness(t)
	h.bash("work-1", &conversationv1.AgentBashFailure{
		Command: &conversationv1.AgentBashCommand{Line: "npm run dev"},
		Error:   &conversationv1.AgentToolFailure{SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 2_000}},
	})

	// Assert.
	if got := h.shellHead().GetCommand().GetText(); got != "npm run dev" {
		t.Fatalf("command = %q, want the restated command", got)
	}
}

// THE FOOTER'S JUMP ADDRESS IS THE ONE THE FEED ANNOUNCES (Deps.EntryPlaced),
// so a subagent's shell must be announced at its head on the subagent's own
// sub-feed. It used to be addressed on the root for every shell, a row the root
// never held once the head was drawn where it belongs.
func TestADetachedShellsHeadIsAnnouncedOnItsOwnersFeed(t *testing.T) {
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	nested := &conversationv1.AgentId{Value: "agent-nested"}
	for _, tc := range []struct {
		name string
		// arrange draws the call's card, answering the agent that carried it.
		arrange func(h *harness) *conversationv1.AgentId
		want    func(carrier *conversationv1.AgentId) feedid.Feed
	}{
		{
			name: "the main agent's shell is announced on the root",
			arrange: func(h *harness) *conversationv1.AgentId {
				h.send(bashCall("toolu_bash", "npm test"))
				return mainAgent()
			},
			want: func(*conversationv1.AgentId) feedid.Feed { return rootFeed() },
		},
		{
			name: "a subagent's shell is announced on the subagent's sub-feed",
			arrange: func(h *harness) *conversationv1.AgentId {
				h.send(spawnCall("toolu_spawn", sub))
				h.sendAs(sub, bashCall("toolu_bash", "npm test"))
				return sub
			},
			want: agentFeed,
		},
		{
			name: "a nested subagent's shell is announced on the nested subagent's sub-feed",
			arrange: func(h *harness) *conversationv1.AgentId {
				h.send(spawnCall("toolu_spawn", sub))
				h.sendAs(sub, spawnCall("toolu_spawn_nested", nested))
				h.sendAs(nested, bashCall("toolu_bash", "npm test"))
				return nested
			},
			want: agentFeed,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			carrier := tc.arrange(h)

			// Act: the task stream announces it on the MAIN agent's book.
			h.announceDetachment(mainAgent(), nil, "toolu_bash", "toolu_bash")

			// Assert: the last address announced for the work is its head on
			// the owner's feed.
			want := testEncode(feedid.Ref{
				WS: testWorkspace, Feed: tc.want(carrier),
				Row: feedid.RowKey{Kind: feedid.KindShellHead, ID: "toolu_bash"},
			}).GetValue()
			var got string
			for _, placed := range h.placed {
				if placed.unit == "toolu_bash" {
					got = placed.row
				}
			}
			if got != want {
				t.Fatalf("toolu_bash announced at %q, want %q (placed = %+v)", got, want, h.placed)
			}
		})
	}
}

// ---- A SETTLE STANDS ALONE: the failure restates the spawn ----

// failedSpawn is a bound producer's failed spawn, restating the commission and
// the created agent, settled at 9000 and restating its start at 1000.
func failedSpawn(created *conversationv1.AgentId) *conversationv1.AgentActivity {
	description := "map the daemon"
	subagentType := "Explore"
	return bound(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{
				Error: &conversationv1.AgentToolFailure{SettledAt: &conversationv1.AgentActivitySettledAt{
					AtMs:      9_000,
					StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
				}},
				Prompt: &conversationv1.AgentSubagentPrompt{
					Text: "go and look", Description: &description, SubagentType: &subagentType,
				},
				CreatedAgentId: created,
			}},
		}},
	})
}

func TestAReplayedFailedSpawnDrawsItsDescription(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act: the failure alone, as a replay serves it.
	h.send(failedSpawn(created))

	// Assert.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetDescription().GetText(); got != "map the daemon" {
		t.Fatalf("description = %q, want the restated commission's", got)
	}
}

func TestAReplayedFailedSpawnAddressesItsSubFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act.
	h.send(failedSpawn(created))

	// Assert: the commission is drawn on the created agent's own feed.
	if row := h.commissionRow("spawn-1", created); row == nil {
		t.Fatalf("sub-feed rows = %+v, want the commission on the restated agent's feed", h.rows(feedid.Feed{Agent: created}))
	}
}

func TestAReplayedFailedSpawnIsNotHeld(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(failedSpawn(&conversationv1.AgentId{Value: "agent-explore"}))

	// Assert.
	if held := h.heldSpawnFrames("spawn-1"); held != 0 {
		t.Fatalf("held frames = %d, want none for a failure that names its agent", held)
	}
}

func TestAReplayedSettledSpawnShowsItsRuntimeFromTheRestatedStart(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act.
	h.send(failedSpawn(created))

	// Assert.
	if got := bubbleOf(h.bubbleRow("spawn-1", created)).GetRuntime().GetStartedAtMs(); got != 1_000 {
		t.Fatalf("runtime start = %d, want the restated start", got)
	}
}

func TestABoundFailedSpawnNamingNoAgentIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act: a bound producer's failure that restated no created agent.
	h.send(failedSpawn(nil))

	// Assert.
	if !h.hasRecord("error", "daemon.feed.activity_undrawable") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.activity_undrawable", h.records())
	}
}

func TestABoundFailedSpawnNamingNoAgentDrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(failedSpawn(nil))

	// Assert: neither drawn nor held for a start a replay will never serve.
	if rows := h.rows(rootFeed()); len(rows) != 0 || h.heldSpawnFrames("spawn-1") != 0 {
		t.Fatalf("rows = %d, held = %d, want neither", len(rows), h.heldSpawnFrames("spawn-1"))
	}
}

func TestABoundFailedSpawnRestatingNoCommissionIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	act := failedSpawn(created)
	act.GetSubagent().GetFailure().Prompt = nil

	// Act.
	h.send(act)

	// Assert.
	if !h.hasRecord("error", "daemon.feed.settle_not_restated") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.settle_not_restated", h.records())
	}
}

func TestABoundSettledSpawnRestatingNoStartIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	act := failedSpawn(&conversationv1.AgentId{Value: "agent-explore"})
	act.GetSubagent().GetFailure().GetError().GetSettledAt().StartedAt = nil

	// Act.
	h.send(act)

	// Assert.
	if !h.hasRecord("error", "daemon.feed.settle_not_restated") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.settle_not_restated", h.records())
	}
}

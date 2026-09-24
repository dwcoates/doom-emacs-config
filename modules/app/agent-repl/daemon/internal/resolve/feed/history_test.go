package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// HISTORY REPLAY goes through THE SAME per-family functions a live frame does,
// so a replayed row and a live one cannot differ.

// historyPage builds a page from entries stated NEWEST FIRST, as the producer
// serves them.
func historyPage(boundary any, newestFirst ...*conversationv1.HistoryEntry) *conversationv1.HistoryPage {
	page := &conversationv1.HistoryPage{}
	for i, entry := range newestFirst {
		page.Entries = append(page.Entries, &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: string(rune('a' + i))},
			Entry: entry,
		})
	}
	switch b := boundary.(type) {
	case *conversationv1.HistoryFloor:
		page.Boundary = &conversationv1.HistoryPage_Floor{Floor: b}
	case *conversationv1.HistoryMore:
		page.Boundary = &conversationv1.HistoryPage_More{More: b}
	}
	return page
}

// promptEntry is a replayed prompt.
func promptEntry(turn, text string) *conversationv1.HistoryEntry {
	return &conversationv1.HistoryEntry{
		Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
			Id:     &conversationv1.TurnId{Value: turn},
			Agent:  mainAgent(),
			Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
			Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
				Blocks: []*conversationv1.UserContentBlock{textBlock(text)},
			}},
		}},
	}
}

// frameEntry is a replayed agent frame.
func frameEntry(agent *conversationv1.AgentId, result any) *conversationv1.HistoryEntry {
	frame := &conversationv1.AgentFrame{AgentId: agent}
	switch r := result.(type) {
	case *conversationv1.AgentUpdate:
		frame.Result = &conversationv1.AgentFrame_Update{Update: r}
	case *conversationv1.AgentSuccess:
		frame.Result = &conversationv1.AgentFrame_Success{Success: r}
	case *conversationv1.AgentFailure:
		frame.Result = &conversationv1.AgentFrame_Failure{Failure: r}
	case *conversationv1.AgentDetachedWork:
		frame.Result = &conversationv1.AgentFrame_DetachedWork{DetachedWork: r}
	}
	return &conversationv1.HistoryEntry{
		Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: frame},
	}
}

// replay pushes one history page through the sink.
func (h *harness) replay(page *conversationv1.HistoryPage) {
	h.t.Helper()
	h.resolver.OnHistoryPage(testWorkspace, mainAgent(), page, noAddress())
}

func TestAReplayLandsRowsOldestFirstEvenThoughThePageIsNewestFirst(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		promptEntry("turn-3", "third"),
		promptEntry("turn-2", "second"),
		promptEntry("turn-1", "first"),
	))

	// Assert: the feed's order is oldest → newest.
	got := rowIDs(h.rows(rootFeed()))
	want := []string{h.promptRowID("turn-1"), h.promptRowID("turn-2"), h.promptRowID("turn-3")}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("rows = %v, want %v", got, want)
		}
	}
}

func TestAReplayedPromptIsDrawnByTheSameFamilyAsALiveOne(t *testing.T) {
	// Arrange: the same prompt, once replayed and once live, in two resolvers.
	replayed := newHarness(t)
	replayed.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "hello")))
	live := newHarness(t)
	live.deliverPrompt("turn-1", "hello")

	// Act, Assert: identical identity and identical drawing.
	replayedRow := replayed.only(rootFeed())
	liveRow := live.only(rootFeed())
	if replayedRow.GetId().GetValue() != liveRow.GetId().GetValue() {
		t.Fatalf("ids differ: replayed %q, live %q",
			replayedRow.GetId().GetValue(), liveRow.GetId().GetValue())
	}
	if replayedRow.GetUserPrompt().GetAuthor().GetLabel() != liveRow.GetUserPrompt().GetAuthor().GetLabel() {
		t.Fatal("a replayed prompt drew a different author label from a live one")
	}
}

func TestAReplayedSettledActivityDrawsItsCard(t *testing.T) {
	// Arrange, Act: no `start` is replayed — the settled frame carries the
	// start's facts.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{
				Activity: activityOf("unit-1", &conversationv1.AgentRead{
					Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
						Path: &conversationv1.ReadPath{Path: "row.go"},
						Extent: &conversationv1.AgentReadSuccess_Whole{
							Whole: &conversationv1.AgentReadWhole{Contents: "package feed"},
						},
					}},
				}),
			},
		}),
	))

	// Assert.
	card := h.card()
	if card.GetName().GetText() != "Read" || card.GetReturned() == nil {
		t.Fatalf("card = %+v, want a settled read", card)
	}
}

func TestAFrameIsAttributedByItsOwnAgentIdAndNotThePagesAgent(t *testing.T) {
	// Arrange: a page requested for the main agent that carries a subagent's
	// frame — frames are FLAT and their agent_id is the whole attribution.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.deliverPrompt("turn-1", "spawn one")
	h.spawnSubagent("spawn-1", created, "Explore", "map it")

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(created, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{
				Activity: responseSuccessActivity("unit-9", "what I found"),
			},
		}),
	))

	// Assert: on the subagent's sub-feed. The row is SOUGHT rather than taken
	// from the end, because this test's subject is ATTRIBUTION and a replayed
	// row's PLACE is the ordering rule's subject: a history-plane row stands
	// above the commission the arrangement drew live.
	rows := h.rows(feedid.Feed{Agent: created})
	var prose *frontendv1.FeedRow
	for _, row := range rows {
		if row.GetActivity().GetResponse() != nil {
			prose = row
		}
	}
	if prose == nil {
		t.Fatalf("sub-feed rows = %+v, want the subagent's prose", rows)
	}
}

func TestAReplayedTerminalDrawsTheTurnItStandsIn(t *testing.T) {
	// Arrange, Act: the prompt establishes the turn, then its terminal.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
		}),
		promptEntry("turn-1", "hello"),
	))

	// Assert: history replays how the turn ended.
	if h.terminalRow("turn-1").GetConcluded() == nil {
		t.Fatalf("outcome = %T, want concluded", h.terminalRow("turn-1").GetOutcome())
	}
}

// THE PAGE'S HEAD CAN OPEN MID-TURN. Every turn open repaints the opening page,
// and that page's oldest terminal can end a turn whose prompt is older than the
// page; it must never be charged to the turn the queue has just opened.
func TestAReplayedTerminalBeforeEveryPromptDoesNotEndTheLiveTurn(t *testing.T) {
	cases := []struct {
		name string
		page *conversationv1.HistoryPage
	}{
		{
			name: "the live turn's prompt is on the page",
			page: historyPage(&conversationv1.HistoryMore{},
				promptEntry("turn-live", "now"),
				frameEntry(mainAgent(), &conversationv1.AgentSuccess{
					Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
				}),
			),
		},
		{
			name: "the live turn's prompt is not on the page yet",
			page: historyPage(&conversationv1.HistoryMore{},
				frameEntry(mainAgent(), &conversationv1.AgentSuccess{
					Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
				}),
			),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the queue opened the live turn.
			h := newHarness(t)
			h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-live"))

			// Act: the opening page replays with an orphan terminal at its head.
			h.replay(tc.page)

			// Assert: the live turn has not ended.
			if h.hasTerminalRow("turn-live") {
				t.Fatal("the page's orphan terminal was drawn as the live turn's terminal")
			}
		})
	}
}

func TestAReplayedContextCutDrawsItsDivider(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextCut{
				ContextCut: &conversationv1.ContextCut{
					Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
				},
			},
		}),
	))

	// Assert.
	if h.separationRow().GetSeparation().GetCleared() == nil {
		t.Fatal("a replayed clear drew no divider")
	}
}

func TestAReplayedApiErrorStaysEvidenceAndNeverBecomesATerminal(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ApiError{
				ApiError: &conversationv1.ApiRequestFailed{Message: "connection reset"},
			},
		}),
		promptEntry("turn-1", "hello"),
	))

	// Assert: the prompt alone — replaying it as a terminal would invent a turn
	// ending.
	rows := h.rows(rootFeed())
	if len(rows) != 1 || rows[0].GetUserPrompt() == nil {
		t.Fatalf("rows = %+v, want the prompt alone", rowIDs(rows))
	}
}

func TestAReplayedContextBudgetWarningDrawsNoRow(t *testing.T) {
	// Arrange, Act: the vendor's own budget warning is a PAGE LINE whose home
	// is the footer's activity line.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextBudgetWarning{
				ContextBudgetWarning: &conversationv1.ContextBudgetWarning{
					Text: "the context window is filling",
				},
			},
		}),
	))

	// Assert: no row — and not the unset-arm warning either, because the arm
	// is handled and simply draws nothing.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	if h.hasRecord("warn", "daemon.feed.history_update_unset") {
		t.Fatalf("records = %+v, want the arm handled rather than unrecognized", h.records())
	}
	if !h.hasRecord("debug", "daemon.feed.context_budget_warning_draws_nothing") {
		t.Fatalf("records = %+v, want the draws-nothing branch recorded", h.records())
	}
}

func TestAReplayedQuestionAndPermissionDrawTheirCards(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Permission{
				Permission: &conversationv1.AgentPermission{
					Id:        &conversationv1.AgentPermissionId{Value: "ask-2"},
					GatedCall: &conversationv1.AgentActivityId{Value: "unit-1"},
					Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{
						Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to read foo.txt"},
						StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
					}},
				},
			},
		}),
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Question{
				Question: &conversationv1.AgentQuestion{
					Id: &conversationv1.AgentQuestionId{Value: "ask-1"},
					Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
						Batch:     twoQuestionBatch(),
						StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
					}},
				},
			},
		}),
	))

	// Assert.
	if h.questionCard().GetOpen() == nil {
		t.Fatal("a replayed question drew no open card")
	}
	if h.permissionCard().GetOpen() == nil {
		t.Fatal("a replayed permission drew no open card")
	}
}

func TestAReplayedDetachedWorkDrawsItsBubble(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-remote"}
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentDetachedWork{
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
		}),
	))

	// Assert.
	rows := h.rows(rootFeed())
	if len(rows) != 1 || rows[0].GetDetachedSubagent() == nil {
		t.Fatalf("rows = %+v, want one detached bubble", rows)
	}
}

func TestAFlooredReplayClearsAnyStandingTruncation(t *testing.T) {
	// Arrange: a page that stopped short.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryMore{
		LastEntry: &conversationv1.HistoryPointer{Value: "a"},
	}, promptEntry("turn-2", "second")))

	// Act: a later page reaches the oldest retained entry.
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "first")))

	// Assert: the hole is gone, so a walk may claim the start.
	h.resolver.mu.Lock()
	more := h.resolver.feed(h.resolver.state(testWorkspace), rootFeed()).historyMore
	h.resolver.mu.Unlock()
	if more != nil {
		t.Fatalf("historyMore = %+v, want cleared by the floor", more)
	}
}

func TestAReplayRecordsWhatItDelivered(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryMore{
		LastEntry: &conversationv1.HistoryPointer{Value: "a"},
	}, promptEntry("turn-2", "second"), promptEntry("turn-1", "first")))

	// Assert.
	var entries any
	for _, record := range h.records() {
		if record.Operation == "daemon.feed.history_page" {
			entries = record.Context["entries"]
		}
	}
	if entries != 2 {
		t.Fatalf("logged entries = %v, want 2", entries)
	}
}

func TestAReplayedEntryWithNoArmIsWarned(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{}, &conversationv1.HistoryEntry{}))

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.history_entry_unset") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.history_entry_unset", h.records())
	}
}

func TestAReplayedFrameWithNoArmIsWarned(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		&conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_AgentFrame{
				AgentFrame: &conversationv1.AgentFrame{AgentId: mainAgent()},
			},
		}))

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.history_frame_unset") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.history_frame_unset", h.records())
	}
}

func TestAReplayedUpdateWithNoArmIsWarned(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{})))

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.history_update_unset") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.history_update_unset", h.records())
	}
}

// TestAReplayedDetachedSubagentSettlesSucceededNeverFailed locks the resume
// guarantee behind BUG B: once the child watch (opened on resume by the session
// watcher for a spawn on the opening page) delivers the run's head-settle, the
// detached bubble takes the STORED outcome — a success settles Succeeded, never
// a failed default. The delivery mirrors the store: the announcement is one page
// line and the run's SubagentSuccess settle is another on the owner's book.
func TestAReplayedDetachedSubagentSettlesSucceededNeverFailed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	created := &conversationv1.AgentId{Value: "agent-remote"}
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		// NEWEST FIRST: the success settle is newer than the announcement.
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: &conversationv1.AgentActivity{
				ActivityId: &conversationv1.AgentActivityId{Value: "work-1"},
				Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
					Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
						CreatedAgentId: created,
						SettledAt:      &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
					}},
				}},
			}},
		}),
		frameEntry(mainAgent(), &conversationv1.AgentDetachedWork{
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
		}),
	))

	// Assert: the detached bubble is settled Succeeded, never a failed default.
	var bubble *frontendv1.FeedSubagent
	for _, row := range h.everyRow() {
		if detached := row.GetDetachedSubagent(); detached != nil {
			bubble = detached.GetSubagent()
		}
	}
	if bubble == nil {
		t.Fatalf("rows = %+v, want a detached subagent bubble", h.everyRow())
	}
	if bubble.GetSettled().GetSucceeded() == nil {
		t.Fatalf("outcome = %T, want Succeeded (never a failed default)", bubble.GetSettled().GetOutcome())
	}
}

// THE CRUX REGRESSION: A REPLAYED CONCLUDED TURN STAMPS ITS ANSWER ROW GREEN.
// This is the reload/reconnect/adopt case — a feed rebuilt from the store with
// NO live turn-ended event. The recurring bug was that the green depended on a
// live event and never appeared on a replayed feed. Because the flag is a data
// property re-stamped along the same terminal path replay walks, the replayed
// answer row must carry final_answer=true just as a live one does.
func TestAReplayedConcludedTurnStampsItsAnswerRowFinal(t *testing.T) {
	// Arrange, Act: a stored turn — prompt, its answering response, and the
	// concluded terminal naming that response — replayed newest-first.
	h := newHarness(t)
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{
				Answer: &conversationv1.AgentActivityId{Value: "unit-1"},
			}},
		}),
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{
				Activity: responseSuccessActivity("unit-1", "the replayed answer"),
			},
		}),
		promptEntry("turn-1", "ask it"),
	))

	// Assert: the replayed answer row is green, with no live turn-ended event.
	var answer *frontendv1.FeedResponse
	for _, row := range h.rows(rootFeed()) {
		if resp := row.GetActivity().GetResponse(); resp != nil {
			answer = resp
		}
	}
	if answer == nil {
		t.Fatal("no response row was replayed")
	}
	if !answer.GetFinalAnswer() {
		t.Fatal("a replayed concluded turn's answer row was not stamped final_answer=true")
	}
}

// TestAnEmptyPageOfAnUnnamedWatchDrawsNothingAndReportsNothing: a fresh
// session's main watch opens on an empty floor before any row has named the
// main agent. There is nothing to place, so nothing is reported unplaceable.
func TestAnEmptyPageOfAnUnnamedWatchDrawsNothingAndReportsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.OnHistoryPage(testWorkspace, nil, historyPage(&conversationv1.HistoryFloor{}), noAddress())

	// Assert.
	if h.hasRecord("error", "daemon.feed.unplaceable_agent") {
		t.Fatalf("records = %+v, want no unplaceable report for an empty page", h.records())
	}
	if !h.hasRecord("debug", "daemon.feed.history_page_empty") {
		t.Fatalf("records = %+v, want the empty page recorded", h.records())
	}
}

// TestAPageOfAnUnnamedAgentWithRowsIsReportedUnplaceable: a page that DOES
// carry rows for an agent nothing named cannot be placed, and says so.
func TestAPageOfAnUnnamedAgentWithRowsIsReportedUnplaceable(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.OnHistoryPage(testWorkspace, &conversationv1.AgentId{Value: "agent-ghost"},
		historyPage(&conversationv1.HistoryFloor{}, frameEntry(&conversationv1.AgentId{Value: "agent-ghost"},
			&conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{
				Activity: responseSuccessActivity("unit-1", "orphaned prose"),
			}})), noAddress())

	// Assert.
	if rows := h.everyRow(); len(rows) != 0 {
		t.Fatalf("rows = %+v, want nothing drawn", rows)
	}
	if !h.hasRecord("error", "daemon.feed.unplaceable_agent") {
		t.Fatalf("records = %+v, want the ERROR", h.records())
	}
}

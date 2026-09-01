package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
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

	// Assert: on the subagent's sub-feed.
	rows := h.rows(feedid.Feed{Agent: created})
	if len(rows) != 1 || rows[0].GetActivity().GetResponse() == nil {
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

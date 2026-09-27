package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
)

// The routing table: one frame in, one family, one row. These cases pin the
// ROUTING rather than any family's drawing.

func TestEveryActivityKindThatDrawsNothingDrawsNothing(t *testing.T) {
	tests := []struct {
		name string
		item any
	}{
		{name: "a task act draws in the footer's checklist only", item: &conversationv1.AgentTaskAct{}},
		{name: "a monitor is footer-only", item: &conversationv1.AgentMonitor{}},
		{name: "a self-wakeup drives the footer", item: &conversationv1.AgentScheduleWakeup{}},
		{name: "cron acts are footer-only", item: &conversationv1.AgentCron{}},
		{name: "a push notification fans out elsewhere", item: &conversationv1.AgentPushNotification{}},
		{name: "injected context is not a row", item: &conversationv1.AgentContextInjected{}},
		{name: "a send with no result arm is not a row", item: &conversationv1.AgentSendMessage{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			act := &conversationv1.AgentActivity{
				ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
			}
			switch i := tc.item.(type) {
			case *conversationv1.AgentTaskAct:
				act.Item = &conversationv1.AgentActivity_TaskAct{TaskAct: i}
			case *conversationv1.AgentMonitor:
				act.Item = &conversationv1.AgentActivity_Monitor{Monitor: i}
			case *conversationv1.AgentScheduleWakeup:
				act.Item = &conversationv1.AgentActivity_ScheduleWakeup{ScheduleWakeup: i}
			case *conversationv1.AgentCron:
				act.Item = &conversationv1.AgentActivity_Cron{Cron: i}
			case *conversationv1.AgentPushNotification:
				act.Item = &conversationv1.AgentActivity_PushNotification{PushNotification: i}
			case *conversationv1.AgentContextInjected:
				act.Item = &conversationv1.AgentActivity_ContextInjected{ContextInjected: i}
			case *conversationv1.AgentSendMessage:
				act.Item = &conversationv1.AgentActivity_SendMessage{SendMessage: i}
			}

			// Act.
			h.send(act)

			// Assert.
			if rows := h.rows(rootFeed()); len(rows) != 0 {
				t.Fatalf("rows = %d, want 0", len(rows))
			}
		})
	}
}

func TestASessionUpdateThatChangesNoRowIsRecordedByItsArm(t *testing.T) {
	tests := []struct {
		name   string
		update any
		want   string
	}{
		{
			name:   "the model changed",
			update: &conversationv1.SessionModelChanged{EffectiveModel: &conversationv1.AgentModel{Name: "m"}},
			want:   "model_changed",
		},
		{
			name:   "context usage is the topbar's",
			update: &conversationv1.SessionContextUsage{},
			want:   "context_usage",
		},
		{
			name:   "diagnostics are the topbar's",
			update: &conversationv1.SessionDiagnostics{},
			want:   "diagnostics",
		},
		{
			name:   "compaction beginning is relayed, and the cut is the end signal",
			update: &conversationv1.SessionCompacting{},
			want:   "compacting",
		},
		{
			name:   "identity rotation re-keys the store, not a row",
			update: &conversationv1.SessionIdentityRotated{},
			want:   "identity_rotated",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			update := &conversationv1.SessionUpdate{}
			switch u := tc.update.(type) {
			case *conversationv1.SessionModelChanged:
				update.Update = &conversationv1.SessionUpdate_ModelChanged{ModelChanged: u}
			case *conversationv1.SessionContextUsage:
				update.Update = &conversationv1.SessionUpdate_ContextUsage{ContextUsage: u}
			case *conversationv1.SessionDiagnostics:
				update.Update = &conversationv1.SessionUpdate_Diagnostics{Diagnostics: u}
			case *conversationv1.SessionCompacting:
				update.Update = &conversationv1.SessionUpdate_Compacting{Compacting: u}
			case *conversationv1.SessionIdentityRotated:
				update.Update = &conversationv1.SessionUpdate_IdentityRotated{IdentityRotated: u}
			}

			// Act.
			h.resolver.OnSessionUpdate(testWorkspace, update)

			// Assert: no row, and the arm named in the record.
			if rows := h.rows(rootFeed()); len(rows) != 0 {
				t.Fatalf("rows = %d, want 0", len(rows))
			}
			var arm any
			for _, record := range h.records() {
				if record.Operation == "daemon.feed.session_update_ignored" {
					arm = record.Context["arm"]
				}
			}
			if arm != tc.want {
				t.Fatalf("logged arm = %v, want %q", arm, tc.want)
			}
		})
	}
}

func TestAnUnsetSessionUpdateIsNamedRatherThanGuessed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{})

	// Assert.
	var arm any
	for _, record := range h.records() {
		if record.Operation == "daemon.feed.session_update_ignored" {
			arm = record.Context["arm"]
		}
	}
	if arm != "unset" {
		t.Fatalf("logged arm = %v, want unset", arm)
	}
}

func TestEveryRowThisTurnProducesCarriesItsTurnStamp(t *testing.T) {
	// Arrange: the prompt establishes the turn in flight.
	h := newHarness(t)
	h.deliverPrompt("turn-7", "hello")

	// Act: a later row that names no turn of its own.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseSuccessActivity("unit-1", "answering"), nil, noAddress())

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetTurn().GetValue() != "turn-7" {
			t.Fatalf("row %q turn = %q, want turn-7", row.GetId().GetValue(), row.GetTurn().GetValue())
		}
	}
}

func TestRowsProducedWithNoTurnInFlightCarryNoStamp(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseSuccessActivity("unit-1", "unattributed"), nil, noAddress())

	// Assert: a row that belongs to no turn says so rather than borrowing one.
	if got := h.only(rootFeed()).GetTurn(); got != nil {
		t.Fatalf("turn = %+v, want unset", got)
	}
}

func TestTheTurnStampIsClearedByItsTerminal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Act: work after the turn ended.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseSuccessActivity("unit-1", "after the end"), nil, noAddress())

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetActivity().GetResponse() != nil && row.GetTurn() != nil {
			t.Fatalf("a row after the terminal was stamped with %q", row.GetTurn().GetValue())
		}
	}
}

func TestAnAgentsOwnRowsFollowItsSubFeedOnceItsSpawnIsSeen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn one")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map it")

	// Act: a question from the SUBAGENT.
	h.resolver.OnQuestion(testWorkspace, created, &conversationv1.AgentQuestion{
		Id: &conversationv1.AgentQuestionId{Value: "ask-1"},
		Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
			Batch:     twoQuestionBatch(),
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}, nil, noAddress())

	// Assert: THE CONNECTION IS THE PLACEMENT — the card is on the bubble's own
	// feed, and it carries no parent naming the bubble.
	rows := h.rows(feedid.Feed{Agent: created})
	if last(rows).GetQuestion() == nil {
		t.Fatalf("sub-feed rows = %+v, want the subagent's question", rows)
	}
	if last(rows).GetParent() != nil {
		t.Fatal("a subagent's row named a parent; its rows arrive on the bubble's own feed")
	}
}

// A UNIT'S TURN SURVIVES ITS REPLAY. The store replays a unit long after the
// turn that ran it closed: the replayed frame names no turn and no turn is in
// flight, so the only place its turn can come from is the row already
// published for it.
func TestAReplayedUnitKeepsTheTurnItsLiveRowWasStampedWith(t *testing.T) {
	// Arrange: a live edit inside an open turn.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "edit it")
	edit := activityOf("unit-1", &conversationv1.AgentEdit{
		Result: &conversationv1.AgentEdit_Success{Success: &conversationv1.AgentEditSuccess{
			Path: &conversationv1.ReadPath{Path: "a.go"},
		}},
	})
	h.send(edit)
	if got := h.activityRow("unit-1").GetTurn().GetValue(); got != "turn-1" {
		t.Fatalf("live turn = %q, want turn-1", got)
	}
	h.terminal("turn-1", &conversationv1.AgentSuccess{}, nil)

	// Act: the store replays the very same unit, with no turn of its own.
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: edit},
		}),
	))

	// Assert.
	if got := h.activityRow("unit-1").GetTurn().GetValue(); got != "turn-1" {
		t.Fatalf("replayed turn = %q, want turn-1", got)
	}
}

// A subagent's hand-back is its RESULT, never an ordinary tool card
// (conversation.v1 AgentSubagentHandback).
func TestASubagentHandbackIsNeverDrawnAsAToolCard(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(bound(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_SubagentHandback{SubagentHandback: &conversationv1.AgentSubagentHandback{
			Result: &conversationv1.AgentSubagentHandback_Success{Success: &conversationv1.AgentSubagentHandbackSuccess{
				Report: &conversationv1.AgentSubagentHandbackReport{Text: "the report"},
			}},
		}},
	}))

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetActivity().GetSimpleToolCall() != nil {
			t.Fatalf("a hand-back drew an ordinary tool card: %+v", row)
		}
	}
}

// activityRow is the root feed's row for one activity unit.
func (h *harness) activityRow(unit string) *frontendv1.FeedRow {
	h.t.Helper()
	want := testEncode(feedid.Ref{
		WS:   testWorkspace,
		Feed: rootFeed(),
		Row:  feedid.RowKey{Kind: feedid.KindActivity, ID: unit},
	}).GetValue()
	for _, row := range h.rows(rootFeed()) {
		if row.GetId().GetValue() == want {
			return row
		}
	}
	h.t.Fatalf("no row for unit %q", unit)
	return nil
}

// hasActivityRow reports whether the root feed still carries the activity row
// for one unit — false once the row is retired.
func (h *harness) hasActivityRow(unit string) bool {
	h.t.Helper()
	want := testEncode(feedid.Ref{
		WS:   testWorkspace,
		Feed: rootFeed(),
		Row:  feedid.RowKey{Kind: feedid.KindActivity, ID: unit},
	}).GetValue()
	for _, row := range h.rows(rootFeed()) {
		if row.GetId().GetValue() == want {
			return true
		}
	}
	return false
}

// TestARowOwedByADeadTurnKeepsThatTurnsStampWhenItsTerminalLandsFirst is the
// same debt when the turn's own query_died terminal reaches the resolver before
// the session's push: that terminal is the death too, so it leaves the stamp
// standing for the stand-down's denials.
func TestARowOwedByADeadTurnKeepsThatTurnsStampWhenItsTerminalLandsFirst(t *testing.T) {
	// Arrange: a turn concluded by its query_died terminal.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, queryDiedTerminal(iteratorDeath()))

	// Act: the stand-down's denial arrives after the terminal.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionFailure{})

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetPermission() == nil {
			continue
		}
		if got := row.GetTurn().GetValue(); got != "turn-1" {
			t.Fatalf("denied ask's turn = %q, want turn-1", got)
		}
		return
	}
	t.Fatalf("rows = %+v, want the denied ask's row", h.rows(rootFeed()))
}

// TestARowOwedByADeadTurnKeepsThatTurnsStamp covers the rows a query death
// OWES: the gate's stand-down denies every pending permission ask, and those
// denials are drawn AFTER the death's terminal row. They belong to the turn
// that was running -- a reader scoped to that turn must see them -- so a
// DEATH keeps the stamp standing where an ordinary terminal clears it
// (TestTheTurnStampIsClearedByItsTerminal is the other half).
func TestARowOwedByADeadTurnKeepsThatTurnsStamp(t *testing.T) {
	// Arrange: a turn whose query then dies.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{
			Cause: &conversationv1.SessionQueryDied_UnexpectedEof{
				UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{},
			},
		}},
	})

	// Act: the stand-down's denial arrives after the terminal.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionFailure{})

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetPermission() == nil {
			continue
		}
		if got := row.GetTurn().GetValue(); got != "turn-1" {
			t.Fatalf("denied ask's turn = %q, want turn-1", got)
		}
		return
	}
	t.Fatalf("rows = %+v, want the denied ask's row", h.rows(rootFeed()))
}

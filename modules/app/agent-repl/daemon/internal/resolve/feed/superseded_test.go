package feed

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
)

// A THINKING BUBBLE IS SUPERSEDED ONCE A LATER AGENT RESPONSE LANDS IN ITS FEED
// (owner rule, 2026-09-23): another thinking bubble, mid-turn prose or a final
// answer supersedes it; a tool call or a prompt never does; a sub-feed follows
// the rule within itself only; and replay, pages and live agree.

// settledThinking is a reasoning block that settled with TEXT.
func settledThinking(unit, text string) *conversationv1.AgentActivity {
	return thinkingResultFrame(unit, &conversationv1.AgentThinkingSuccess{
		Reasoning: &conversationv1.AgentThinkingSuccess_Text{
			Text: &conversationv1.AgentThinkingText{Text: text},
		},
	})
}

// settledRead is a settled tool call: a row that is not an agent response.
func settledRead(unit string) *conversationv1.AgentActivity {
	return activityOf(unit, &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
			Path: &conversationv1.ReadPath{Path: "row.go"},
			Extent: &conversationv1.AgentReadSuccess_Whole{
				Whole: &conversationv1.AgentReadWhole{Contents: "package feed"},
			},
		}},
	})
}

// activityRowID is the row identity an activity unit draws on FEED.
func activityRowID(feed feedid.Feed, unit string) string {
	return testEncode(feedid.Ref{
		WS: testWorkspace, Feed: feed,
		Row: feedid.RowKey{Kind: feedid.KindActivity, ID: unit},
	}).GetValue()
}

// responseOn answers the response bubble UNIT drew on FEED, failing when it
// drew none.
func (h *harness) responseOn(feed feedid.Feed, unit string) *frontendv1.FeedResponse {
	h.t.Helper()
	want := activityRowID(feed, unit)
	for _, row := range h.rows(feed) {
		if row.GetId().GetValue() == want {
			if resp := row.GetActivity().GetResponse(); resp != nil {
				return resp
			}
		}
	}
	h.t.Fatalf("no response row for unit %q on feed %s", unit, testFeedValue(feed))
	return nil
}

// supersededFlags answers every thinking row's flag among ROWS, by row id.
func supersededFlags(rows []*frontendv1.FeedRow) map[string]bool {
	out := map[string]bool{}
	for _, row := range rows {
		if resp := row.GetActivity().GetResponse(); resp.GetThinking() {
			out[row.GetId().GetValue()] = resp.GetSuperseded()
		}
	}
	return out
}

func TestAThinkingRowIsSupersededOnlyByALaterResponseInItsFeed(t *testing.T) {
	tests := []struct {
		name  string
		after func(h *harness)
		unit  string
		want  bool
	}{
		{
			name:  "a thinking row that is the latest response is not superseded",
			after: func(h *harness) {},
			unit:  "think-1",
			want:  false,
		},
		{
			name:  "a later thinking row supersedes it",
			after: func(h *harness) { h.send(settledThinking("think-2", "more reasoning")) },
			unit:  "think-1",
			want:  true,
		},
		{
			name:  "the later thinking row is itself the latest and not superseded",
			after: func(h *harness) { h.send(settledThinking("think-2", "more reasoning")) },
			unit:  "think-2",
			want:  false,
		},
		{
			name:  "later mid-turn prose supersedes it",
			after: func(h *harness) { h.send(responseSuccessActivity("prose-1", "an interim note")) },
			unit:  "think-1",
			want:  true,
		},
		{
			name: "the turn's final answer supersedes it",
			after: func(h *harness) {
				h.send(responseSuccessActivity("answer-1", "the answer"))
				h.terminal("turn-1", completedWith("answer-1"), nil)
			},
			unit: "think-1",
			want: true,
		},
		{
			name:  "a later tool call does not supersede it",
			after: func(h *harness) { h.send(settledRead("read-1")) },
			unit:  "think-1",
			want:  false,
		},
		{
			name:  "a later prompt does not supersede it",
			after: func(h *harness) { h.deliverPrompt("turn-2", "and another thing") },
			unit:  "think-1",
			want:  false,
		},
		{
			name: "prose after an intervening tool call supersedes it",
			after: func(h *harness) {
				h.send(settledRead("read-1"))
				h.send(responseSuccessActivity("prose-1", "an interim note"))
			},
			unit: "think-1",
			want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a turn drew one thinking bubble.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			h.send(settledThinking("think-1", "weighing options"))

			// Act.
			tt.after(h)

			// Assert.
			if got := h.responseOn(rootFeed(), tt.unit).GetSuperseded(); got != tt.want {
				t.Fatalf("superseded = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestANonThinkingResponseIsNeverStampedSuperseded(t *testing.T) {
	// Arrange: prose, then a later response.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(responseSuccessActivity("prose-1", "an interim note"))

	// Act.
	h.send(settledThinking("think-1", "weighing options"))

	// Assert: only a thinking bubble carries the flag.
	if h.responseOn(rootFeed(), "prose-1").GetSuperseded() {
		t.Fatal("a prose bubble was stamped superseded")
	}
}

func TestARedrawOfASupersededThinkingFoldKeepsTheFlag(t *testing.T) {
	// Arrange: a streaming thinking block is superseded mid-stream.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(thinkingResultFrame("think-1", thinkingTextDelta("weigh")))
	h.send(responseSuccessActivity("prose-1", "an interim note"))

	// Act: the thinking block's own settle redraws its row afterwards.
	h.send(settledThinking("think-1", "weighing options"))

	// Assert: the fresh draw restates the flag rather than un-collapsing it.
	if !h.responseOn(rootFeed(), "think-1").GetSuperseded() {
		t.Fatal("a redraw of a superseded thinking row dropped the flag")
	}
}

func TestSupersedingRecordsTheEdgeAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(settledThinking("think-1", "weighing options"))

	// Act.
	h.send(responseSuccessActivity("prose-1", "an interim note"))

	// Assert.
	if !h.hasRecord("info", "daemon.feed.thinking_superseded") {
		t.Fatal("superseding a thinking row recorded no INFO daemon.feed.thinking_superseded")
	}
}

func TestASubFeedFollowsTheRuleWithinItselfOnly(t *testing.T) {
	created := &conversationv1.AgentId{Value: "agent-explore"}
	sub := feedid.Feed{Agent: created}
	tests := []struct {
		name string
		act  func(h *harness)
		feed feedid.Feed
		unit string
		want bool
	}{
		{
			name: "a subagent's prose does not supersede the root feed's thinking",
			act: func(h *harness) {
				h.send(settledThinking("think-root", "weighing options"))
				h.resolver.OnActivity(testWorkspace, created,
					responseSuccessActivity("prose-sub", "what I found"), nil, noAddress())
			},
			feed: rootFeed(),
			unit: "think-root",
			want: false,
		},
		{
			name: "the root feed's prose does not supersede a subagent's thinking",
			act: func(h *harness) {
				h.resolver.OnActivity(testWorkspace, created,
					settledThinking("think-sub", "looking around"), nil, noAddress())
				h.send(responseSuccessActivity("prose-root", "an interim note"))
			},
			feed: sub,
			unit: "think-sub",
			want: false,
		},
		{
			name: "a subagent's prose supersedes its own earlier thinking",
			act: func(h *harness) {
				h.resolver.OnActivity(testWorkspace, created,
					settledThinking("think-sub", "looking around"), nil, noAddress())
				h.resolver.OnActivity(testWorkspace, created,
					responseSuccessActivity("prose-sub", "what I found"), nil, noAddress())
			},
			feed: sub,
			unit: "think-sub",
			want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a turn spawned a subagent with its own sub-feed.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

			// Act.
			tt.act(h)

			// Assert.
			if got := h.responseOn(tt.feed, tt.unit).GetSuperseded(); got != tt.want {
				t.Fatalf("superseded = %v, want %v", got, tt.want)
			}
		})
	}
}

// liveConversation draws a turn with two thinking blocks, a tool call and a
// closing answer as it happens.
func liveConversation(h *harness) {
	h.deliverPrompt("turn-1", "do the thing")
	h.send(settledThinking("think-1", "weighing options"))
	h.send(settledRead("read-1"))
	h.send(settledThinking("think-2", "almost there"))
	h.send(responseSuccessActivity("answer-1", "the answer"))
}

// replayedConversation replays the same turn from the store.
func replayedConversation(h *harness) {
	activity := func(act *conversationv1.AgentActivity) *conversationv1.HistoryEntry {
		return frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: act},
		})
	}
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		activity(responseSuccessActivity("answer-1", "the answer")),
		activity(settledThinking("think-2", "almost there")),
		activity(settledRead("read-1")),
		activity(settledThinking("think-1", "weighing options")),
		promptEntry("turn-1", "do the thing"),
	))
}

func TestReplayAndPagesAgreeWithLive(t *testing.T) {
	tests := []struct {
		name string
		rows func(h *harness) []*frontendv1.FeedRow
	}{
		{
			name: "the live feed",
			rows: func(h *harness) []*frontendv1.FeedRow {
				liveConversation(h)
				return h.rows(rootFeed())
			},
		},
		{
			name: "a replayed feed",
			rows: func(h *harness) []*frontendv1.FeedRow {
				replayedConversation(h)
				return h.rows(rootFeed())
			},
		},
		{
			name: "a page served from the live feed",
			rows: func(h *harness) []*frontendv1.FeedRow {
				liveConversation(h)
				page, _ := h.openPage(rootFeed(), "reader-1")
				return pageRows(h.t, page)
			},
		},
		{
			name: "a page served from a replayed feed",
			rows: func(h *harness) []*frontendv1.FeedRow {
				replayedConversation(h)
				page, _ := h.openPage(rootFeed(), "reader-1")
				return pageRows(h.t, page)
			},
		},
	}
	want := map[string]bool{
		activityRowID(rootFeed(), "think-1"): true,
		activityRowID(rootFeed(), "think-2"): true,
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			got := supersededFlags(tt.rows(h))

			// Assert: every thinking row the page holds carries the live answer
			// (a page is PageSize rows, so it may hold fewer than the feed).
			for id, flag := range got {
				if flag != want[id] {
					t.Fatalf("row %s superseded = %v, want %v", id, flag, want[id])
				}
			}
			if len(got) == 0 {
				t.Fatal("no thinking row was served")
			}
		})
	}
}

func TestAHistoryPageLandingAboveLiveRows(t *testing.T) {
	tests := []struct {
		name string
		unit string
		want bool
	}{
		{name: "an older replayed thinking row is placed superseded", unit: "think-old", want: true},
		{name: "the live thinking row below it stays the latest", unit: "think-live", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a live thinking row already stands.
			h := newHarness(t)
			h.deliverPrompt("turn-2", "the newer turn")
			h.send(settledThinking("think-live", "current reasoning"))

			// Act: older history lands ABOVE it.
			h.replay(historyPage(&conversationv1.HistoryFloor{},
				frameEntry(mainAgent(), &conversationv1.AgentUpdate{
					Update: &conversationv1.AgentUpdate_Activity{
						Activity: settledThinking("think-old", "older reasoning"),
					},
				}),
				promptEntry("turn-1", "the older turn"),
			))

			// Assert.
			if got := h.responseOn(rootFeed(), tt.unit).GetSuperseded(); got != tt.want {
				t.Fatalf("superseded = %v, want %v", got, tt.want)
			}
		})
	}
}

// tailOf opens a page and a tail on the root feed, answering the tail's rows.
func (h *harness) tailOf(ctx context.Context) <-chan *frontendv1.FeedRow {
	h.t.Helper()
	_, token := h.openPage(rootFeed(), "reader-1")
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		h.t.Fatalf("Tail: %v", err)
	}
	return tail.Rows(ctx)
}

func TestTheSupersededRowIsRePushed(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *harness)
		// want is the rows the tail delivers after the act, by unit and flag.
		want []string
	}{
		{
			name: "a later response re-pushes the thinking row superseded, after its own push",
			act:  func(h *harness) { h.send(responseSuccessActivity("prose-1", "an interim note")) },
			want: []string{"prose-1", "think-1 superseded"},
		},
		{
			name: "retiring the only later response re-pushes the thinking row un-superseded",
			act: func(h *harness) {
				h.send(responseSuccessActivity("prose-1", "an interim note"))
				h.resolver.RetireRow(testWorkspace, rootFeed(),
					&frontendv1.FeedId{Value: activityRowID(rootFeed(), "prose-1")})
			},
			want: []string{"prose-1", "think-1 superseded", "prose-1 removed", "think-1"},
		},
	}
	units := map[string]string{
		activityRowID(rootFeed(), "think-1"): "think-1",
		activityRowID(rootFeed(), "prose-1"): "prose-1",
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a tail follows a feed whose latest response is thinking.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			h.send(settledThinking("think-1", "weighing options"))
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			rows := h.tailOf(ctx)

			// Act.
			tt.act(h)

			// Assert.
			var got []string
			for range tt.want {
				row := <-rows
				word := units[row.GetId().GetValue()]
				switch {
				case row.GetRemoved() != nil:
					word += " removed"
				case row.GetActivity().GetResponse().GetSuperseded():
					word += " superseded"
				}
				got = append(got, word)
			}
			for i := range tt.want {
				if got[i] != tt.want[i] {
					t.Fatalf("tail pushes = %v, want %v", got, tt.want)
				}
			}
		})
	}
}

func TestSupersededErrorPathsAreRecordedAtError(t *testing.T) {
	tests := []struct {
		name      string
		act       func(r *resolver, s *wsState, f *feedState)
		operation string
	}{
		{
			name: "a re-push of a row the feed does not hold",
			act: func(r *resolver, s *wsState, _ *feedState) {
				r.republishSuperseded(s, rootFeed(), "row-that-is-not-there")
			},
			operation: "daemon.feed.superseded_row_missing",
		},
		{
			name: "a placement whose row is not in the feed order",
			act: func(r *resolver, s *wsState, f *feedState) {
				r.supersedeOnPlace(s, f, "row-that-is-not-there", &frontendv1.FeedRow{})
			},
			operation: "daemon.feed.superseded_unplaced",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.resolver.mu.Lock()
			s := h.resolver.state(testWorkspace)
			f := h.resolver.feed(s, rootFeed())

			// Act.
			tt.act(h.resolver, s, f)
			h.resolver.mu.Unlock()

			// Assert.
			if !h.hasRecord("error", tt.operation) {
				t.Fatalf("no ERROR %s was recorded", tt.operation)
			}
		})
	}
}

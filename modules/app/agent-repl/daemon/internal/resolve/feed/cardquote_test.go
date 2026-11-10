package feed

import (
	"strings"
	"testing"

	"google.golang.org/protobuf/encoding/protojson"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
)

func TestCardTextQuotesEveryCardByTheOneRule(t *testing.T) {
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	tests := []struct {
		name     string
		row      func(h *harness) string
		wantKind CardKind
		wantArm  string
		wantText string // "" for a row quoted as its own JSON
	}{
		{
			name:     "a settled response by its markdown",
			row:      func(h *harness) string { return h.responseRowID("unit-r") },
			wantKind: CardResponse, wantArm: "activity.response", wantText: "the answer",
		},
		{
			name:     "a prompt by its markdown",
			row:      func(h *harness) string { return h.promptRowID("turn-1") },
			wantKind: CardPrompt, wantArm: "user_prompt", wantText: "do the thing",
		},
		{
			name:     "a turn's ending as the row's JSON",
			row:      func(h *harness) string { return h.turnEndedRowID("turn-1") },
			wantKind: CardRow, wantArm: "turn_ended",
		},
		{
			name: "a tool card on a subagent's feed as the row's JSON",
			row: func(h *harness) string {
				return testEncode(feedid.Ref{
					WS: testWorkspace, Feed: agentFeed(sub),
					Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "toolu_monitor"},
				}).GetValue()
			},
			wantKind: CardRow, wantArm: "activity.simple_tool_call",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			h.liveResponse("unit-r", "the answer", "turn-1")
			h.send(spawnCall("toolu_spawn", sub))
			h.sendAs(sub, monitorArmed("toolu_monitor", watchedCommand("tail -f build.log")))
			h.terminal("turn-1", completed(""), nil)
			id := tc.row(h)

			// Act.
			got, ok := h.resolver.CardText(testWorkspace, &frontendv1.FeedId{Value: id})

			// Assert.
			if !ok || got.Kind != tc.wantKind || got.Arm != tc.wantArm {
				t.Fatalf("CardText(%s) = (%+v, %v), want kind %d arm %q", id, got, ok, tc.wantKind, tc.wantArm)
			}
			if tc.wantText != "" {
				if got.Text != tc.wantText {
					t.Fatalf("CardText(%s).Text = %q, want %q", id, got.Text, tc.wantText)
				}
				return
			}
			var row frontendv1.FeedRow
			if err := protojson.Unmarshal([]byte(got.Text), &row); err != nil || row.GetId().GetValue() != id || !strings.Contains(got.Text, "\n") {
				t.Fatalf("CardText(%s).Text = %q (%v), want the row itself as multi-line JSON", id, got.Text, err)
			}
		})
	}
}

func TestCardTextMissesARowThatIsNoCard(t *testing.T) {
	tests := []struct {
		name string
		id   string
	}{
		{name: "an id that does not decode", id: "not-a-feed-id"},
		{name: "a row no feed holds", id: activityRowID(rootFeed(), "unit-gone")},
		{name: "a row of another workspace", id: testEncode(feedid.Ref{
			WS: "ws-other", Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-r"},
		}).GetValue()},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			h.liveResponse("unit-r", "the answer", "turn-1")

			// Act.
			got, ok := h.resolver.CardText(testWorkspace, &frontendv1.FeedId{Value: tc.id})

			// Assert.
			if ok {
				t.Fatalf("CardText(%s) = %+v, want a miss", tc.id, got)
			}
		})
	}
}

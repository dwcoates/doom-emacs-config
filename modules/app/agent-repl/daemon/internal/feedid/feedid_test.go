package feedid

import (
	"encoding/base64"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

func agent(v string) *conversationv1.AgentId { return &conversationv1.AgentId{Value: v} }

func lease(v string) *LeaseID { l := LeaseID(v); return &l }

func TestEncodeDecodeRoundTripsEveryRowKind(t *testing.T) {
	for _, kind := range AllRowKinds {
		t.Run(string(kind), func(t *testing.T) {
			// Arrange.
			want := Ref{WS: "ws-1", Feed: Feed{Root: true}, Row: RowKey{Kind: kind, ID: "id-" + string(kind), Sub: "sub"}}

			// Act.
			id := Encode(want)
			got, err := Decode(id)

			// Assert.
			if err != nil {
				t.Fatalf("Decode: %v", err)
			}
			if got != want {
				t.Fatalf("round trip = %+v, want %+v", got, want)
			}
		})
	}
}

func TestEncodeRoundTripsEachFeedArm(t *testing.T) {
	tests := []struct {
		name string
		feed Feed
	}{
		{name: "root", feed: Feed{Root: true}},
		{name: "agent", feed: Feed{Agent: agent("agent-7")}},
		{name: "merge", feed: Feed{Merge: lease("lease-9")}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ref := Ref{WS: "ws-1", Feed: tc.feed, Row: RowKey{Kind: KindPrompt, ID: "p1"}}

			// Act.
			got, err := Decode(Encode(ref))

			// Assert.
			if err != nil {
				t.Fatalf("Decode: %v", err)
			}
			if got.Feed.Root != tc.feed.Root {
				t.Fatalf("Root = %v, want %v", got.Feed.Root, tc.feed.Root)
			}
			if (got.Feed.Agent == nil) != (tc.feed.Agent == nil) {
				t.Fatalf("Agent presence = %v, want %v", got.Feed.Agent != nil, tc.feed.Agent != nil)
			}
			if tc.feed.Agent != nil && got.Feed.Agent.GetValue() != tc.feed.Agent.GetValue() {
				t.Fatalf("Agent = %q, want %q", got.Feed.Agent.GetValue(), tc.feed.Agent.GetValue())
			}
			if (got.Feed.Merge == nil) != (tc.feed.Merge == nil) {
				t.Fatalf("Merge presence = %v, want %v", got.Feed.Merge != nil, tc.feed.Merge != nil)
			}
			if tc.feed.Merge != nil && *got.Feed.Merge != *tc.feed.Merge {
				t.Fatalf("Merge = %q, want %q", *got.Feed.Merge, *tc.feed.Merge)
			}
		})
	}
}

func TestEncodeIsDeterministic(t *testing.T) {
	// Arrange.
	ref := Ref{WS: "ws-1", Feed: Feed{Agent: agent("a")}, Row: RowKey{Kind: KindActivity, ID: "u1", Sub: "a"}}

	// Act.
	first, second := Encode(ref), Encode(Ref{WS: "ws-1", Feed: Feed{Agent: agent("a")}, Row: RowKey{Kind: KindActivity, ID: "u1", Sub: "a"}})

	// Assert.
	if first.GetValue() != second.GetValue() {
		t.Fatalf("encodings differ: %q vs %q", first.GetValue(), second.GetValue())
	}
}

func TestEncodeIsDelimiterSafe(t *testing.T) {
	// Arrange: ids full of characters that would break a naive delimiter.
	ref := Ref{
		WS:   "ws/with.:-_+=|weird",
		Feed: Feed{Merge: lease("lease.with:colons/and.dots")},
		Row:  RowKey{Kind: KindMergeTab, ID: "id.with:colons", Sub: "sub/with.dots"},
	}

	// Act.
	got, err := Decode(Encode(ref))

	// Assert.
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if got.WS != ref.WS || got.Row != ref.Row || *got.Feed.Merge != *ref.Feed.Merge {
		t.Fatalf("round trip = %+v, want %+v", got, ref)
	}
}

func TestEncodeRefusesUnknownRowKind(t *testing.T) {
	// Arrange.
	ref := Ref{WS: "ws-1", Feed: Feed{Root: true}, Row: RowKey{Kind: RowKind("invented"), ID: "x"}}

	// Act.
	got := Encode(ref)

	// Assert.
	if got != nil {
		t.Fatalf("Encode = %q, want nil", got.GetValue())
	}
}

func TestEncodeRefusesNoFeedArm(t *testing.T) {
	// Arrange.
	ref := Ref{WS: "ws-1", Row: RowKey{Kind: KindPrompt, ID: "p"}}

	// Act.
	got := Encode(ref)

	// Assert.
	if got != nil {
		t.Fatalf("Encode = %q, want nil", got.GetValue())
	}
}

func TestEncodeRefusesTwoFeedArms(t *testing.T) {
	// Arrange.
	ref := Ref{WS: "ws-1", Feed: Feed{Root: true, Agent: agent("a")}, Row: RowKey{Kind: KindPrompt, ID: "p"}}

	// Act.
	got := Encode(ref)

	// Assert.
	if got != nil {
		t.Fatalf("Encode = %q, want nil", got.GetValue())
	}
}

func TestEncodeRefusesEmptyWorkspace(t *testing.T) {
	// Arrange.
	ref := Ref{Feed: Feed{Root: true}, Row: RowKey{Kind: KindPrompt, ID: "p"}}

	// Act.
	got := Encode(ref)

	// Assert.
	if got != nil {
		t.Fatalf("Encode = %q, want nil", got.GetValue())
	}
}

func TestDecodeRejectsMalformedInput(t *testing.T) {
	tests := []struct {
		name string
		id   *frontendv1.FeedId
	}{
		{name: "nil", id: nil},
		{name: "empty", id: &frontendv1.FeedId{Value: ""}},
		{name: "unknown version", id: &frontendv1.FeedId{Value: "f2.YWJj"}},
		{name: "no version prefix", id: &frontendv1.FeedId{Value: "YWJj"}},
		{name: "not base64url", id: &frontendv1.FeedId{Value: "f1.not base64!"}},
		{name: "too few fields", id: &frontendv1.FeedId{Value: "f1." + b64("ws\x1fr\x1f\x1fprompt")}},
		{name: "too many fields", id: &frontendv1.FeedId{Value: "f1." + b64("ws\x1fr\x1f\x1fprompt\x1fid\x1fsub\x1fextra")}},
		{name: "empty workspace", id: &frontendv1.FeedId{Value: "f1." + b64("\x1fr\x1f\x1fprompt\x1fid\x1fsub")}},
		{name: "unknown feed discriminator", id: &frontendv1.FeedId{Value: "f1." + b64("ws\x1fz\x1f\x1fprompt\x1fid\x1fsub")}},
		{name: "root feed with a value", id: &frontendv1.FeedId{Value: "f1." + b64("ws\x1fr\x1fv\x1fprompt\x1fid\x1fsub")}},
		{name: "agent feed with no agent id", id: &frontendv1.FeedId{Value: "f1." + b64("ws\x1fa\x1f\x1fprompt\x1fid\x1fsub")}},
		{name: "merge feed with no lease id", id: &frontendv1.FeedId{Value: "f1." + b64("ws\x1fm\x1f\x1fprompt\x1fid\x1fsub")}},
		{name: "unknown row kind", id: &frontendv1.FeedId{Value: "f1." + b64("ws\x1fr\x1f\x1finvented\x1fid\x1fsub")}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := Decode(tc.id)

			// Assert.
			if err == nil {
				t.Fatal("Decode accepted a malformed id")
			}
		})
	}
}

func TestEncodeFeedDecodeFeedRoundTrips(t *testing.T) {
	tests := []struct {
		name string
		feed Feed
	}{
		{name: "root", feed: Feed{Root: true}},
		{name: "agent", feed: Feed{Agent: agent("agent-7")}},
		{name: "merge", feed: Feed{Merge: lease("lease-9")}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			ws, got, err := DecodeFeed(EncodeFeed("ws-1", tc.feed))

			// Assert.
			if err != nil {
				t.Fatalf("DecodeFeed: %v", err)
			}
			if ws != "ws-1" {
				t.Fatalf("workspace = %q, want ws-1", ws)
			}
			if got.Root != tc.feed.Root {
				t.Fatalf("Root = %v, want %v", got.Root, tc.feed.Root)
			}
			if tc.feed.Agent != nil && got.Agent.GetValue() != tc.feed.Agent.GetValue() {
				t.Fatalf("Agent = %q, want %q", got.Agent.GetValue(), tc.feed.Agent.GetValue())
			}
			if tc.feed.Merge != nil && *got.Merge != *tc.feed.Merge {
				t.Fatalf("Merge = %q, want %q", *got.Merge, *tc.feed.Merge)
			}
		})
	}
}

func TestDecodeFeedDerivesSubagentSubFeedFromBubbleRow(t *testing.T) {
	// Arrange: the subagent bubble row on the root feed.
	ref := Ref{WS: "ws-1", Feed: Feed{Root: true}, Row: SubagentRowKey("spawn-unit-1", agent("created-agent"))}

	// Act.
	ws, feed, err := DecodeFeed(Encode(ref))

	// Assert.
	if err != nil {
		t.Fatalf("DecodeFeed: %v", err)
	}
	if ws != "ws-1" {
		t.Fatalf("workspace = %q, want ws-1", ws)
	}
	if feed.Agent.GetValue() != "created-agent" {
		t.Fatalf("Agent = %q, want created-agent", feed.Agent.GetValue())
	}
}

func TestDecodeFeedDerivesMergeSubFeedFromMergeHead(t *testing.T) {
	// Arrange.
	ref := Ref{WS: "ws-1", Feed: Feed{Root: true}, Row: MergeHeadRowKey("lease-4")}

	// Act.
	_, feed, err := DecodeFeed(Encode(ref))

	// Assert.
	if err != nil {
		t.Fatalf("DecodeFeed: %v", err)
	}
	if feed.Merge == nil || *feed.Merge != "lease-4" {
		t.Fatalf("Merge = %v, want lease-4", feed.Merge)
	}
}

func TestDecodeFeedRejectsRowWithNoSubFeed(t *testing.T) {
	// Arrange: an ordinary prompt row addresses no sub-feed.
	ref := Ref{WS: "ws-1", Feed: Feed{Root: true}, Row: RowKey{Kind: KindPrompt, ID: "p1"}}

	// Act.
	_, _, err := DecodeFeed(Encode(ref))

	// Assert.
	if err == nil {
		t.Fatal("DecodeFeed accepted a row that addresses no sub-feed")
	}
}

func TestDecodeFeedRejectsActivityRowWithoutCreatedAgent(t *testing.T) {
	// Arrange: an activity row that is not a subagent bubble.
	ref := Ref{WS: "ws-1", Feed: Feed{Root: true}, Row: RowKey{Kind: KindActivity, ID: "act-1"}}

	// Act.
	_, _, err := DecodeFeed(Encode(ref))

	// Assert.
	if err == nil {
		t.Fatal("DecodeFeed accepted an activity row with no created agent")
	}
}

func TestPlanRowKeyUsesThePlanBubbleSpelling(t *testing.T) {
	// Act.
	got := PlanRowKey("agent-1", "episode-2")

	// Assert.
	if got.Kind != KindActivity {
		t.Fatalf("Kind = %q, want activity", got.Kind)
	}
	if got.ID != "plan:agent-1:episode-2" {
		t.Fatalf("ID = %q, want plan:agent-1:episode-2", got.ID)
	}
}

func TestPlanRowKeyRoundTrips(t *testing.T) {
	// Arrange.
	ref := Ref{WS: "ws-1", Feed: Feed{Root: true}, Row: PlanRowKey("agent-1", "episode-2")}

	// Act.
	got, err := Decode(Encode(ref))

	// Assert.
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if got != ref {
		t.Fatalf("round trip = %+v, want %+v", got, ref)
	}
}

func TestSubagentRowKeyWithNilAgentHasNoSub(t *testing.T) {
	// Act.
	got := SubagentRowKey("unit", nil)

	// Assert.
	if got.Sub != "" {
		t.Fatalf("Sub = %q, want empty", got.Sub)
	}
}

// b64 renders a raw payload the way the encoder does, so a table can state a
// malformed payload directly.
func b64(payload string) string {
	return base64.RawURLEncoding.EncodeToString([]byte(payload))
}

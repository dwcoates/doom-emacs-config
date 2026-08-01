package sessioncontroller

import (
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// newProvenanceConsumer arranges a consumer over a fake applier whose merge
// lease ledger the test controls.
func newProvenanceConsumer(applier *fakeApplier) (*consumer, *fakePusher) {
	push := &fakePusher{}
	c := newConsumer("ws", "s1", push, applier, nil, newFakeClearCompactStore(),
		nil, nil, nil, nil, nil, nil)
	return c, push
}

func TestStampConversationProvenance(t *testing.T) {
	// Arrange.
	tests := []struct {
		name    string
		windows [][2]int64
		tsMs    int64
		want    frontendv1.ConversationSource
	}{
		{
			name:    "an item produced inside a lease window is the merge's",
			windows: [][2]int64{{100, 200}},
			tsMs:    150,
			want:    frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
		},
		{
			name:    "an item produced after the lease was released is the user's",
			windows: [][2]int64{{100, 200}},
			tsMs:    250,
			want:    frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		},
		{
			name:    "an item on a workspace that never merged is the user's",
			windows: nil,
			tsMs:    150,
			want:    frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		},
		{
			name:    "an item produced inside a STILL-OPEN window is the merge's",
			windows: [][2]int64{{100, 0}},
			tsMs:    1_000_000,
			want:    frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			applier := &fakeApplier{mergeWindows: map[string][][2]int64{"ws": tt.windows}}
			c, _ := newProvenanceConsumer(applier)
			cd := &frontendv1.ConversationDelta{
				Workspace: "ws",
				Items:     []*frontendv1.ConversationItem{{Uuid: "u1", TsMs: tt.tsMs}},
			}

			// Act.
			ok := c.stampConversationProvenance(cd)

			// Assert.
			if !ok {
				t.Fatal("stampConversationProvenance refused a placeable delta")
			}
			if got := cd.GetItems()[0].GetSource(); got != tt.want {
				t.Fatalf("source = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestStampConversationProvenanceRefusesAnUnplaceableDelta(t *testing.T) {
	// Arrange.
	applier := &fakeApplier{conversationSrcErr: errors.New("no timestamp")}
	c, push := newProvenanceConsumer(applier)
	cd := &frontendv1.ConversationDelta{
		Workspace: "ws",
		Items:     []*frontendv1.ConversationItem{{Uuid: "u1"}},
	}

	// Act.
	ok := c.stampConversationProvenance(cd)

	// Assert.
	if ok {
		t.Fatal("stampConversationProvenance accepted a delta whose provenance could not be resolved")
	}
	if got := cd.GetItems()[0].GetSource(); got != frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED {
		t.Fatalf("source = %v, want the item left unstamped", got)
	}
	if len(push.convo) != 0 {
		t.Fatalf("pushed %d delta(s), want none for an unresolvable provenance", len(push.convo))
	}
}

func TestStampConversationProvenanceRefusesANilItem(t *testing.T) {
	// Arrange.
	c, _ := newProvenanceConsumer(&fakeApplier{})
	cd := &frontendv1.ConversationDelta{Workspace: "ws", Items: []*frontendv1.ConversationItem{nil}}

	// Act.
	ok := c.stampConversationProvenance(cd)

	// Assert.
	if ok {
		t.Fatal("stampConversationProvenance accepted a delta carrying a nil item")
	}
}

func TestPushLocalItemStampsTheLiveLeaseVerdict(t *testing.T) {
	// Arrange.
	tests := []struct {
		name string
		held bool
		want frontendv1.ConversationSource
	}{
		{
			name: "composed while the merge owns the shim",
			held: true,
			want: frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE,
		},
		{
			name: "composed while the user owns the shim",
			held: false,
			want: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			applier := &fakeApplier{mergeLeases: map[string]bool{"ws": tt.held}}
			c, push := newProvenanceConsumer(applier)
			item := &frontendv1.ConversationItem{Uuid: "local-1"}

			// Act.
			c.pushLocalItem(item)

			// Assert.
			if item.GetSource() != tt.want {
				t.Fatalf("source = %v, want %v", item.GetSource(), tt.want)
			}
			if len(push.convo) != 1 {
				t.Fatalf("pushed %d delta(s), want 1", len(push.convo))
			}
			if got := push.convo[0].GetItems()[0].GetSource(); got != tt.want {
				t.Fatalf("pushed source = %v, want %v", got, tt.want)
			}
		})
	}
}

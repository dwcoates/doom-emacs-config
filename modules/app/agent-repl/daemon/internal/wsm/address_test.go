package wsm

import (
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
)

func TestToStoredFeedRefusesAFeedWithNoArm(t *testing.T) {
	// Arrange / Act
	_, err := toStoredFeed(feedid.Feed{})

	// Assert
	if err == nil {
		t.Fatalf("toStoredFeed on a feed naming nothing succeeded")
	}
}

func TestToStoredFeedRefusesAFeedWithTwoArms(t *testing.T) {
	// Arrange
	lease := LeaseID("lease-1")

	// Act
	_, err := toStoredFeed(feedid.Feed{Root: true, Merge: &lease})

	// Assert
	if err == nil {
		t.Fatalf("toStoredFeed on a feed naming two arms succeeded")
	}
}

func TestEncodeRefRoundTripsEachFeedArm(t *testing.T) {
	lease := LeaseID("lease-1")
	tests := []struct {
		name string
		feed feedid.Feed
	}{
		{name: "root", feed: feedid.Feed{Root: true}},
		{name: "agent", feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "agent-1"}}},
		{name: "merge", feed: feedid.Feed{Merge: &lease}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			ref := feedid.Ref{WS: WorkspaceID("ws-1"), Feed: tc.feed, Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: "row-1"}}

			// Act
			raw, err := encodeRef(&ref)
			if err != nil {
				t.Fatalf("encodeRef: %v", err)
			}
			got, err := decodeRef("held_prompts", "row-1", raw.(string))

			// Assert
			if err != nil {
				t.Fatalf("decodeRef: %v", err)
			}
			if got.WS != ref.WS || got.Row != ref.Row {
				t.Fatalf("ref = %+v, want %+v", got, ref)
			}
			if got.Feed.Root != tc.feed.Root {
				t.Fatalf("feed root = %v, want %v", got.Feed.Root, tc.feed.Root)
			}
		})
	}
}

func TestEncodeRefRefusesARowWithNoKind(t *testing.T) {
	// Arrange
	ref := feedid.Ref{WS: WorkspaceID("ws-1"), Feed: feedid.Feed{Root: true}, Row: feedid.RowKey{ID: "row-1"}}

	// Act
	_, err := encodeRef(&ref)

	// Assert
	if err == nil {
		t.Fatalf("encodeRef on a row with no kind succeeded")
	}
}

func TestEncodeRefRendersNullForAnAbsentRef(t *testing.T) {
	// Arrange / Act
	got, err := encodeRef(nil)

	// Assert
	if err != nil {
		t.Fatalf("encodeRef(nil): %v", err)
	}
	if got != nil {
		t.Fatalf("encodeRef(nil) = %v, want NULL", got)
	}
}

func TestDecodeRefRefusesUnparseableJSON(t *testing.T) {
	// Arrange / Act
	_, err := decodeRef("held_prompts", "row-1", "not json")

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "held_prompts" {
		t.Fatalf("decodeRef = %v, want a *DecodeError naming the table", err)
	}
}

func TestDecodeRefRefusesAFeedWithNoArm(t *testing.T) {
	// Arrange / Act
	_, err := decodeRef("held_prompts", "row-1", `{"ws":"x","feed":{},"row":{"kind":"prompt","id":"1"}}`)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "feed" {
		t.Fatalf("decodeRef = %v, want a *DecodeError naming the feed", err)
	}
}

func TestEncodeAddressRoundTripsTheParent(t *testing.T) {
	// Arrange
	parent := feedid.Ref{WS: WorkspaceID("ws-1"), Feed: feedid.Feed{Root: true}, Row: feedid.RowKey{Kind: feedid.KindMergeHead, ID: "lease-1"}}
	address := OutputAddress{Feed: feedid.Feed{Root: true}, Parent: &parent}

	// Act
	raw, err := encodeAddress(&address)
	if err != nil {
		t.Fatalf("encodeAddress: %v", err)
	}
	got, err := decodeAddress("turns", "turn-1", raw.(string))

	// Assert
	if err != nil {
		t.Fatalf("decodeAddress: %v", err)
	}
	if got.Parent == nil || got.Parent.Row != parent.Row {
		t.Fatalf("parent = %+v, want %+v", got.Parent, parent)
	}
}

// An address an older build wrote carries the retired root-feed mirror flag;
// it still reads, as the address it names.
func TestDecodeAddressReadsAStoredMirrorFlag(t *testing.T) {
	// Arrange
	raw := `{"feed":{"merge":"lease-1"},"mirror":true}`

	// Act
	got, err := decodeAddress("turns", "turn-1", raw)

	// Assert
	if err != nil {
		t.Fatalf("decodeAddress: %v", err)
	}
	if got.Feed.Merge == nil || *got.Feed.Merge != LeaseID("lease-1") || got.Parent != nil {
		t.Fatalf("decoded address = %+v, want the merge sub-feed it names", got)
	}
}

// A write drops the retired flag: nothing it encodes names a mirror.
func TestEncodeAddressWritesNoMirrorFlag(t *testing.T) {
	// Arrange
	lease := LeaseID("lease-1")
	address := OutputAddress{Feed: feedid.Feed{Merge: &lease}}

	// Act
	raw, err := encodeAddress(&address)

	// Assert
	if err != nil {
		t.Fatalf("encodeAddress: %v", err)
	}
	if strings.Contains(raw.(string), "mirror") {
		t.Fatalf("encoded address = %s, want no mirror key", raw)
	}
}

func TestEncodeAddressRendersNullForAnAbsentAddress(t *testing.T) {
	// Arrange / Act
	got, err := encodeAddress(nil)

	// Assert
	if err != nil {
		t.Fatalf("encodeAddress(nil): %v", err)
	}
	if got != nil {
		t.Fatalf("encodeAddress(nil) = %v, want NULL", got)
	}
}

func TestDecodeAddressRefusesAnUnknownParentRowKind(t *testing.T) {
	// Arrange / Act
	_, err := decodeAddress("turns", "turn-1",
		`{"feed":{"root":true},"parent":{"ws":"x","feed":{"root":true},"row":{"kind":"invented","id":"1"}}}`)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "row_kind" {
		t.Fatalf("decodeAddress = %v, want a *DecodeError naming the row kind", err)
	}
}

func TestKnownRowKindCoversEveryDeclaredKind(t *testing.T) {
	tests := []feedid.RowKind{
		feedid.KindPrompt, feedid.KindActivity, feedid.KindTurnEnded, feedid.KindPermission,
		feedid.KindQuestion, feedid.KindSeparation, feedid.KindColdGate, feedid.KindMergeHead,
		feedid.KindMergeTab, feedid.KindDetachedSubagent, feedid.KindDetachedShell, feedid.KindSynth,
	}
	for _, kind := range tests {
		t.Run(string(kind), func(t *testing.T) {
			// Arrange / Act / Assert
			if !knownRowKind(kind) {
				t.Fatalf("knownRowKind(%q) = false, want true", kind)
			}
		})
	}
}

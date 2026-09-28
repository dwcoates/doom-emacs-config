package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
)

// THE PEER MESSAGE ROW. A message another Claude session sent in draws as its
// own abbreviated bubble on the recipient's feed — never a prompt, never the
// turn's answer.

// peerMessage sends one peer message to the main agent.
func (h *harness) peerMessage(id, sender, body string) {
	h.t.Helper()
	h.resolver.OnPeerMessage(testWorkspace, &conversationv1.PeerMessage{
		Agent:  mainAgent(),
		Sender: sender,
		Body:   body,
		Id:     id,
	}, nil, nil, noAddress())
}

func TestPeerMessageDrawsAPeerRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.peerMessage("p1", "Explore", "found it")

	// Assert.
	if h.only(rootFeed()).GetPeerMessage() == nil {
		t.Fatal("a peer message must draw a FeedPeerMessage row")
	}
}

func TestPeerMessageLabelNamesTheSender(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.peerMessage("p1", "Explore", "found it")

	// Assert.
	if got := h.only(rootFeed()).GetPeerMessage().GetSender(); got != "agent Explore" {
		t.Fatalf("label = %q, want \"agent Explore\"", got)
	}
}

func TestPeerMessageCarriesTheBodyForExpansion(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.peerMessage("p1", "Explore", "the whole message body")

	// Assert.
	if got := h.only(rootFeed()).GetPeerMessage().GetBody(); got != "the whole message body" {
		t.Fatalf("body = %q, want the message body", got)
	}
}

func TestPeerMessageWithNoSenderFallsBackToAnotherAgent(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.peerMessage("p1", "", "body")

	// Assert.
	if got := h.only(rootFeed()).GetPeerMessage().GetSender(); got != "another agent" {
		t.Fatalf("label = %q, want the empty-sender fallback \"another agent\"", got)
	}
}

func TestPeerMessageIsNeverAPromptRow(t *testing.T) {
	// Arrange. Regression: a peer message must not be drawn as a user prompt
	// (blue "You" bubble) — the whole point of the feature.
	h := newHarness(t)

	// Act.
	h.peerMessage("p1", "Explore", "body")

	// Assert.
	row := h.only(rootFeed())
	if row.GetUserPrompt() != nil || row.GetAgentPrompt() != nil {
		t.Fatal("a peer message must never draw a prompt row")
	}
}

func TestPeerMessageRowKeyedOnItsStableId(t *testing.T) {
	// Arrange. Two deliveries of one message (the same id from both planes) must
	// upsert one row, not two.
	h := newHarness(t)

	// Act.
	h.peerMessage("p1", "Explore", "first observation")
	h.peerMessage("p1", "Explore", "corrected observation")

	// Assert.
	rows := h.rows(rootFeed())
	if len(rows) != 1 {
		t.Fatalf("rows = %d, want exactly 1 (both deliveries of id p1 collapse)", len(rows))
	}
	if got := rows[0].GetPeerMessage().GetBody(); got != "corrected observation" {
		t.Fatalf("body = %q, want the second delivery to upsert the first", got)
	}
}

// peerRowID is the FeedId a peer message with this id is drawn under.
func (h *harness) peerRowID(id string) string {
	return testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindPeer, ID: id},
	}).GetValue()
}

func TestPeerMessageRowIdIsMintedFromTheKindAndId(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.peerMessage("p1", "Explore", "body")

	// Assert. The row is keyed by KindPeer under the message's stable id.
	if got := h.only(rootFeed()).GetId().GetValue(); got != h.peerRowID("p1") {
		t.Fatalf("row id = %q, want the KindPeer row id for p1 %q", got, h.peerRowID("p1"))
	}
}

// peerEntry is a replayed peer message.
func peerEntry(id, sender, body string) *conversationv1.HistoryEntry {
	return &conversationv1.HistoryEntry{
		Entry: &conversationv1.HistoryEntry_PeerMessage{PeerMessage: &conversationv1.PeerMessage{
			Agent:  mainAgent(),
			Sender: sender,
			Body:   body,
			Id:     id,
		}},
	}
}

func TestReplayedPeerMessageDrawsThePeerRow(t *testing.T) {
	// Arrange. A resumed session's history page carries a peer message; it must
	// replay through the same drawPeerMessage a live one uses.
	h := newHarness(t)

	// Act.
	h.replay(historyPage(&conversationv1.HistoryFloor{}, peerEntry("p1", "Explore", "found it")))

	// Assert.
	if h.only(rootFeed()).GetPeerMessage().GetSender() != "agent Explore" {
		t.Fatal("a replayed peer message must draw the peer bubble with its sender label")
	}
}

func TestReplayedAndLivePeerMessageResolveToOneRow(t *testing.T) {
	// Arrange. The same message seen live and on a replayed page shares its id,
	// so the two must upsert one row rather than draw two.
	h := newHarness(t)

	// Act.
	h.peerMessage("p1", "Explore", "live body")
	h.replay(historyPage(&conversationv1.HistoryFloor{}, peerEntry("p1", "Explore", "replayed body")))

	// Assert.
	if got := len(h.rows(rootFeed())); got != 1 {
		t.Fatalf("rows = %d, want exactly 1 (live and replayed collapse on id p1)", got)
	}
}

// peerMessageOf sends one peer message of a stated kind to the main agent.
func (h *harness) peerMessageOf(id, sender string, peer *conversationv1.PeerMessage) {
	h.t.Helper()
	peer.Agent, peer.Sender, peer.Body, peer.Id = mainAgent(), sender, "the whole report", id
	h.resolver.OnPeerMessage(testWorkspace, peer, nil, nil, noAddress())
}

func handbackPeer() *conversationv1.PeerMessage {
	return &conversationv1.PeerMessage{Kind: &conversationv1.PeerMessage_SubagentHandback{SubagentHandback: &conversationv1.PeerMessageSubagentHandback{}}}
}

func interSessionPeer() *conversationv1.PeerMessage {
	return &conversationv1.PeerMessage{Kind: &conversationv1.PeerMessage_InterSession{InterSession: &conversationv1.PeerMessageInterSession{}}}
}

func TestHandbackPeerMessageDrawsTheBadge(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.peerMessageOf("p1", "Explore", handbackPeer())

	// Assert.
	if h.only(rootFeed()).GetSubagentHandback() == nil {
		t.Fatal("a subagent's hand-back must draw the FeedSubagentHandbackBadge row, not a bubble")
	}
}

func TestHandbackBadgeLabelSaysTheSenderReportedBack(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.peerMessageOf("p1", "Explore", handbackPeer())

	// Assert.
	if got := h.only(rootFeed()).GetSubagentHandback().GetLabel().GetText(); got != "agent Explore reported back" {
		t.Fatalf("label = %q, want \"agent Explore reported back\"", got)
	}
}

func TestHandbackBadgeTakesThePeerRowId(t *testing.T) {
	// Arrange. Live and adopted deliveries collapse on the id minted from
	// PeerMessage.id, whichever row kind draws it.
	h := newHarness(t)

	// Act.
	h.peerMessageOf("p1", "Explore", handbackPeer())

	// Assert.
	if got := h.only(rootFeed()).GetId().GetValue(); got != h.peerRowID("p1") {
		t.Fatalf("row id = %q, want the KindPeer row id for p1 %q", got, h.peerRowID("p1"))
	}
}

func TestInterSessionPeerMessageDrawsTheBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.peerMessageOf("p1", "Explore", interSessionPeer())

	// Assert.
	if h.only(rootFeed()).GetPeerMessage() == nil {
		t.Fatal("an inter-session peer message must draw the FeedPeerMessage bubble")
	}
}

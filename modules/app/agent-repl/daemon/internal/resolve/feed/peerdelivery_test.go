package feed

import (
	"context"
	"errors"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// THE TWO ENDS OF A CROSS-SESSION MESSAGE. The sender's send and the
// recipient's bubble each name the workspace at the other end by the delivery
// id both records carry, from the rows drawn here or from the store, and each
// row is drawn again in place when the other end becomes known.

// otherWorkspace is the workspace at the other end of the messages here.
const otherWorkspace ids.WorkspaceID = "ws-2"

// otherMainAgent is otherWorkspace's main agent: its original vendor session
// id, which is how the store names its session.
func otherMainAgent() *conversationv1.AgentId { return &conversationv1.AgentId{Value: "agent-other"} }

// fakePeerDeliveries is the store behind Deps.PeerDeliveries: it answers each
// delivery from `ends` (or `err`) and records every lookup.
type fakePeerDeliveries struct {
	mu      sync.Mutex
	ends    map[string]PeerDeliveryEnds
	err     error
	lookups []string
}

func (f *fakePeerDeliveries) ResolvePeerDelivery(_ context.Context, ws ids.WorkspaceID, deliveryID string) (PeerDeliveryEnds, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.lookups = append(f.lookups, string(ws)+"/"+deliveryID)
	return f.ends[deliveryID], f.err
}

// runSpawned runs the work the resolver started off its lock, as a goroutine
// would, at the moment the test chooses.
func (h *harness) runSpawned() {
	h.t.Helper()
	spawned := h.spawned
	h.spawned = nil
	for _, f := range spawned {
		f()
	}
}

// peerSend is a send from WS's main agent, in unit UNIT, that reached another
// Claude session under DELIVERY, addressed to TO.
func (h *harness) peerSend(ws ids.WorkspaceID, unit, to, delivery string) {
	h.t.Helper()
	act := sendActivity(h.t, unit, &conversationv1.AgentSendMessageSuccess{
		Recipient: &conversationv1.AgentSendMessageSuccess_PeerSession{PeerSession: &conversationv1.AgentSendMessagePeerSession{
			DeliveryId: &conversationv1.PeerDeliveryId{Value: delivery},
		}},
		AddressedTo: to,
	})
	h.resolver.OnActivity(ws, mainAgentOf(ws), bound(act), nil, nil)
}

// peerReceived is an inter-session message to WS's main agent, with id ID,
// naming NAME as its sender and DELIVERY as its delivery id.
func (h *harness) peerReceived(ws ids.WorkspaceID, id, name, delivery string) {
	h.t.Helper()
	h.resolver.OnPeerMessage(ws, &conversationv1.PeerMessage{
		Agent: mainAgentOf(ws),
		Body:  "review the fix",
		Id:    id,
		Kind: &conversationv1.PeerMessage_InterSession{InterSession: &conversationv1.PeerMessageInterSession{
			SenderSessionName: &conversationv1.PeerMessageSenderSessionName{Text: name},
			DeliveryId:        &conversationv1.PeerDeliveryId{Value: delivery},
		}},
	}, nil, nil)
}

// mainAgentOf is WS's main agent in these tests.
func mainAgentOf(ws ids.WorkspaceID) *conversationv1.AgentId {
	if ws == otherWorkspace {
		return otherMainAgent()
	}
	return mainAgent()
}

// rootRowsOf answers WS's root feed rows, in order.
func (h *harness) rootRowsOf(ws ids.WorkspaceID) []*frontendv1.FeedRow {
	h.t.Helper()
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	f := h.resolver.feed(h.resolver.state(ws), feedid.Feed{Root: true})
	out := make([]*frontendv1.FeedRow, 0, len(f.order))
	for _, id := range f.order {
		out = append(out, f.rows[id])
	}
	return out
}

// onlyRootRowOf answers WS's one root row, failing when there is not exactly one.
func (h *harness) onlyRootRowOf(ws ids.WorkspaceID) *frontendv1.FeedRow {
	h.t.Helper()
	rows := h.rootRowsOf(ws)
	if len(rows) != 1 {
		h.t.Fatalf("rows of %s = %d, want exactly 1", ws, len(rows))
	}
	return rows[0]
}

// twoWorkspaces is a harness whose other workspace is named and has its main
// agent named, as the session watcher names it.
func twoWorkspaces(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.names[testWorkspace] = "fix-flaky"
	h.names[otherWorkspace] = "review-bot"
	h.resolver.OnMainAgent(otherWorkspace, otherMainAgent())
	return h
}

func TestASendToAnotherSessionDrawsItsAddressUnresolvedWhileNoRecipientIsKnown(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)

	// Act.
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")

	// Assert.
	if got := addressLine(h.onlyRootRowOf(testWorkspace).GetAgentPrompt().GetAddress()); got != "unresolved review-bot-3a →" {
		t.Fatalf("address = %q, want the address the caller wrote, unresolved", got)
	}
}

func TestASendNamesTheRecipientWorkspaceOnceItsCopyIsDrawn(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")

	// Act.
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Assert.
	if got := addressLine(h.onlyRootRowOf(testWorkspace).GetAgentPrompt().GetAddress()); got != "workspace review-bot →" {
		t.Fatalf("address = %q, want the recipient workspace's own name", got)
	}
}

func TestASendRedrawnForItsRecipientKeepsItsRowIdentity(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")
	before := h.onlyRootRowOf(testWorkspace).GetId().GetValue()

	// Act.
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Assert.
	if got := h.onlyRootRowOf(testWorkspace).GetId().GetValue(); got != before {
		t.Fatalf("row id = %q, want the send's own row %q drawn again in place", got, before)
	}
}

func TestAReceivedMessageNamesTheSendingWorkspaceWhoseSendIsDrawn(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")

	// Act.
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Assert.
	if got := fromLine(h.onlyRootRowOf(otherWorkspace).GetPeerMessage().GetFrom()); got != "← workspace fix-flaky" {
		t.Fatalf("header = %q, want the sending workspace's own name", got)
	}
}

func TestAReceivedMessageDrawnFirstNamesItsSenderWhenTheSendIsDrawn(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Act.
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")

	// Assert.
	if got := fromLine(h.onlyRootRowOf(otherWorkspace).GetPeerMessage().GetFrom()); got != "← workspace fix-flaky" {
		t.Fatalf("header = %q, want the bubble drawn again naming its sender", got)
	}
}

func TestAReceivedMessageWithNoKnownSenderAsksTheStoreThroughItsOwnShim(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)

	// Act.
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")
	h.runSpawned()

	// Assert.
	if got := h.peerSource.lookups; len(got) != 1 || got[0] != string(otherWorkspace)+"/m-1" {
		t.Fatalf("lookups = %v, want one, through the drawing workspace's shim", got)
	}
}

func TestTheStoresAnswerNamesTheSenderAndDrawsTheBubbleAgain(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSource.ends = map[string]PeerDeliveryEnds{"m-1": {Sender: mainAgent().GetValue(), Recipient: otherMainAgent().GetValue()}}
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Act.
	h.runSpawned()

	// Assert.
	if got := fromLine(h.onlyRootRowOf(otherWorkspace).GetPeerMessage().GetFrom()); got != "← workspace fix-flaky" {
		t.Fatalf("header = %q, want the session the store named, as its workspace", got)
	}
}

func TestAStoreAnswerForASessionNotYetNamedResolvesWhenItsMainAgentIsNamed(t *testing.T) {
	// Arrange: the store names a session no workspace's main agent is yet.
	h := newHarness(t)
	h.names[otherWorkspace] = "review-bot"
	h.peerSource.ends = map[string]PeerDeliveryEnds{"m-1": {Recipient: otherMainAgent().GetValue()}}
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")
	h.runSpawned()

	// Act.
	h.resolver.OnMainAgent(otherWorkspace, otherMainAgent())

	// Assert.
	if got := addressLine(h.onlyRootRowOf(testWorkspace).GetAgentPrompt().GetAddress()); got != "workspace review-bot →" {
		t.Fatalf("address = %q, want the send drawn again once the session's workspace is named", got)
	}
}

func TestAnEndNoRecordedSessionHoldsStaysUnresolvedUnderTheSendersName(t *testing.T) {
	// Arrange: a sender outside agent-repl records no send in the store.
	h := twoWorkspaces(t)
	h.peerSource.ends = map[string]PeerDeliveryEnds{"m-1": {Recipient: otherMainAgent().GetValue()}}
	h.peerReceived(otherWorkspace, "p1", "desktop-session-9f", "m-1")

	// Act.
	h.runSpawned()

	// Assert.
	if got := fromLine(h.onlyRootRowOf(otherWorkspace).GetPeerMessage().GetFrom()); got != "← unresolved desktop-session-9f" {
		t.Fatalf("header = %q, want the outside session's own name, unresolved", got)
	}
}

func TestADeliveryIsLookedUpOnceHoweverOftenItsRowIsDrawn(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)

	// Act: both planes deliver the same message.
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")
	h.runSpawned()

	// Assert.
	if got := len(h.peerSource.lookups); got != 1 {
		t.Fatalf("lookups = %d, want one per delivery", got)
	}
}

func TestAFailedLookupIsRecordedAtErrorNamingTheDelivery(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSource.err = errors.New("store unavailable")
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Act.
	h.runSpawned()

	// Assert.
	if !h.hasRecordWith("error", "daemon.feed.peer_delivery_lookup_failed", "delivery_id", "m-1") {
		t.Fatalf("records = %+v, want the failed lookup at ERROR naming the delivery", h.records())
	}
}

func TestAFailedLookupIsAskedAgainWhenTheRowIsDrawnAgain(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSource.err = errors.New("store unavailable")
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")
	h.runSpawned()

	// Act.
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")
	h.runSpawned()

	// Assert.
	if got := len(h.peerSource.lookups); got != 2 {
		t.Fatalf("lookups = %d, want the failed one asked again", got)
	}
}

func TestALookupWithNoShimToAskIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSource.err = ErrNoHistorySource
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Act.
	h.runSpawned()

	// Assert.
	if !h.hasRecordWith("info", "daemon.feed.peer_delivery_lookup_no_source", "delivery_id", "m-1") {
		t.Fatalf("records = %+v, want no shim recorded at INFO", h.records())
	}
}

func TestAWorkspaceNameThatCannotBeReadDrawsTheEndUnresolved(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")
	h.nameErr = errors.New("database is locked")

	// Act.
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Assert.
	if got := addressLine(h.onlyRootRowOf(testWorkspace).GetAgentPrompt().GetAddress()); got != "unresolved review-bot-3a →" {
		t.Fatalf("address = %q, want the end unresolved", got)
	}
}

func TestAWorkspaceNameThatCannotBeReadIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")
	h.nameErr = errors.New("database is locked")

	// Act.
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Assert.
	if !h.hasRecordWith("error", "daemon.feed.peer_workspace_name_failed", "other_workspace", string(otherWorkspace)) {
		t.Fatalf("records = %+v, want the unreadable name at ERROR", h.records())
	}
}

func TestTwoWorkspacesDrawingOneEndKeepTheFirstAndRecordTheConflict(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerReceived(otherWorkspace, "p1", "fix-flaky-5e", "m-1")

	// Act.
	h.peerReceived(testWorkspace, "p2", "fix-flaky-5e", "m-1")

	// Assert.
	if !h.hasRecordWith("error", "daemon.feed.peer_delivery_end_conflict", "kept_workspace", string(otherWorkspace)) {
		t.Fatalf("records = %+v, want the conflicting end at ERROR keeping the first", h.records())
	}
}

func TestASendToAnAgentOfThisSessionDrawsTheInSessionLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.sendMessage("unit-1", &conversationv1.AgentSendMessageSuccess{
		Recipient:   &conversationv1.AgentSendMessageSuccess_RecipientAgentId{RecipientAgentId: &conversationv1.AgentId{Value: "agent-unknown"}},
		AddressedTo: "vetter",
	})

	// Assert.
	if got := addressLine(h.sendRow().GetAddress()); got != "→ vetter" {
		t.Fatalf("address = %q, want the in-session line", got)
	}
}

func TestASendThatHasNotSettledIsUnclassified(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.sendMessage("unit-1", startTo("review-bot-3a", "review the fix"))

	// Assert.
	if got := addressLine(h.sendRow().GetAddress()); got != "unclassified review-bot-3a →" {
		t.Fatalf("address = %q, want the unclassified address the caller wrote", got)
	}
}

func TestASendThatSettledOnAnotherSessionStaysSoUnderAReplayedStart(t *testing.T) {
	// Arrange.
	h := twoWorkspaces(t)
	h.peerSend(testWorkspace, "unit-1", "review-bot-3a", "m-1")

	// Act: the file plane replays the unit's start after its settle.
	h.resolver.OnActivity(testWorkspace, mainAgent(), sendActivity(t, "unit-1", startTo("review-bot-3a", "")), nil, nil)

	// Assert.
	if got := addressLine(h.onlyRootRowOf(testWorkspace).GetAgentPrompt().GetAddress()); got != "unresolved review-bot-3a →" {
		t.Fatalf("address = %q, want the settled peer-session arm kept", got)
	}
}

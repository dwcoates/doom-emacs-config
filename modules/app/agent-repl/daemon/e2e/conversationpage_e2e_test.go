// A frontend COLD OPEN costs a bounded read, not the whole conversation.
//
// THE DEFECT THIS PINS. A mounting webview's only route to its history was
// ResyncCmd{from_seq: 0}, which replays every store event the session ever
// produced — 259,000 events and 186MB in the worst workspace observed — to
// draw a screen whose visible tail is about ten items. The replay was correct;
// it answered a question no client had any reason to ask.
//
// WHAT IS REAL HERE. Everything durableresync_e2e_test.go makes real: the
// shim-store is the launchd binary over a real SQLite database written through
// its real producer protocol, and the registry, SSM, session-controller
// manager, storehistory reader, frontend server and WebSocket transport are
// production types. The harness is reused WHOLE (newBouncedHarness,
// dialFrontend, storedAssistantEvent, workspaceStateInSnapshot) rather than
// re-founded, because the premise is the same one: a post-bounce workspace
// with no live shim, which is the state a cold webview actually mounts against.
// The spawner still fails the test if anything starts a shim, so "a read costs
// a read" is asserted for paging exactly as it is for the durable resync.
package e2e

import (
	"fmt"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// pageResult is what one ConversationPageCmd produced on the wire.
type pageResult struct {
	page *frontendv1.ConversationPage
	// acceptedBeforePage records whether the ok ack arrived BEFORE the page.
	// It is the property the early-ack contract exists for, and reading it off
	// arrival order is the only way to assert it end to end.
	acceptedBeforePage bool
}

// pageFrom sends one conversation page request and reads until its page lands.
//
// Unlike resyncFrom, the ACK does not terminate the read: a page command is
// acked at ACCEPTANCE, before the read runs, and the page follows it. That
// inversion is the whole point of the early ack, so this helper observes the
// order rather than assuming it.
func (h *bouncedHarness) pageFrom(t *testing.T, conn *websocket.Conn, state *frontendv1.WorkspaceState, requestID, anchor string) pageResult {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"workspace":%q,"conversationPage":{%s,"fence":%q}}`,
		requestID, h.workspace, anchor, state.GetFence()))
	var accepted bool
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_CommandAck:
			if f.CommandAck.GetRequestId() != requestID {
				continue
			}
			if !f.CommandAck.GetOk() {
				t.Fatalf("conversation page refused: %s", f.CommandAck.GetError())
			}
			accepted = true
		case *frontendv1.FrontendFrame_ConversationPage:
			if f.ConversationPage.GetRequestId() != requestID {
				continue
			}
			return pageResult{page: f.ConversationPage, acceptedBeforePage: accepted}
		}
	}
	t.Fatalf("no ConversationPage for %s arrived before the deadline", requestID)
	return pageResult{}
}

// tailAnchorJSON is the cold open's anchor; beforeAnchorJSON is load-more's.
func tailAnchorJSON(limit int) string { return fmt.Sprintf(`"tail":{"limit":%d}`, limit) }

func beforeAnchorJSON(cursor string, limit int) string {
	return fmt.Sprintf(`"before":{"cursor":%q,"limit":%d}`, cursor, limit)
}

// pageTexts is the assistant text of every item the page carried, in order.
func pageTexts(page *frontendv1.ConversationPage) []string {
	var out []string
	for _, item := range page.GetItems() {
		if text := assistantText(item); text != "" {
			out = append(out, text)
		}
	}
	return out
}

// --- tests ------------------------------------------------------------------

func TestAColdOpenReceivesOnlyTheConversationsTail(t *testing.T) {
	// Arrange — six replies in the store, of which a cold open wants two.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	for i := 1; i <= 6; i++ {
		producer.write(storedAssistantEvent(t, h.vendorSessionID, fmt.Sprintf("u-%d", i), fmt.Sprintf("reply %d", i)))
	}
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act — exactly what a mounting webview now sends instead of from_seq=0.
	got := h.pageFrom(t, conn, state, "e2e-page-1", tailAnchorJSON(2))

	// Assert — the newest two, oldest first, and nothing older.
	texts := pageTexts(got.page)
	if len(texts) != 2 || texts[0] != "reply 5" || texts[1] != "reply 6" {
		t.Fatalf("cold-open page texts = %v, want [reply 5, reply 6]", texts)
	}
}

func TestAColdOpenPageIsAckedBeforeItArrives(t *testing.T) {
	// Arrange — an unanswered history request is what produced the 5,069-deep
	// command queue this daemon was already repaired for once, and a page read
	// of a very large conversation is exactly that shape.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-1", "the only reply"))
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act.
	got := h.pageFrom(t, conn, state, "e2e-page-2", tailAnchorJSON(10))

	// Assert.
	if !got.acceptedBeforePage {
		t.Fatalf("the page arrived before its acceptance ack, so a slow read would still look unanswered")
	}
}

func TestServingAConversationPageNeverSpawnsAShim(t *testing.T) {
	// Arrange — a frontend mounting is not a reason to start a vendor process,
	// and paging must not become the exception.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-1", "the only reply"))
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act.
	h.pageFrom(t, conn, state, "e2e-page-3", tailAnchorJSON(10))

	// Assert.
	if got := h.spawns.Load(); got != 0 {
		t.Fatalf("shim spawn attempts = %d, want 0 — a read must cost a read", got)
	}
}

func TestLoadMoreWalksBackwardsToTheConversationsBeginning(t *testing.T) {
	// Arrange — four replies, paged two at a time. This is the whole
	// affordance: the tail, then the page before it, then the retirement.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	for i := 1; i <= 4; i++ {
		producer.write(storedAssistantEvent(t, h.vendorSessionID, fmt.Sprintf("u-%d", i), fmt.Sprintf("reply %d", i)))
	}
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)
	tail := h.pageFrom(t, conn, state, "e2e-page-4a", tailAnchorJSON(2))
	cursor := tail.page.GetMore().GetCursor()
	if cursor == "" {
		t.Fatalf("tail page continuation = %v, want a `more` arm: two of four replies are still unserved", tail.page.GetContinuation())
	}

	// Act — the cursor is the daemon's own token, sent back byte-for-byte.
	older := h.pageFrom(t, conn, state, "e2e-page-4b", beforeAnchorJSON(cursor, 2))

	// Assert — the two immediately before, with no overlap and no gap, and the
	// conversation's beginning reported so load-more retires.
	texts := pageTexts(older.page)
	if len(texts) != 2 || texts[0] != "reply 1" || texts[1] != "reply 2" {
		t.Fatalf("load-more page texts = %v, want [reply 1, reply 2]", texts)
	}
	if older.page.GetStart() == nil {
		t.Fatalf("second page continuation = %v, want the `start` arm", older.page.GetContinuation())
	}
}

func TestATailPageReportsTheSeqTheLiveStreamIsRejoinedAt(t *testing.T) {
	// Arrange — the gap-free splice's wire half. The client stores this as its
	// from_seq, and because the daemon's replay is INCLUSIVE of the mark, an
	// item produced after the mint is replayed rather than skipped.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-1", "reply 1"))
	last := producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-2", "reply 2"))
	h.seq.SetLastSeq(h.sessionID, last.GetLastSeq())
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act.
	got := h.pageFrom(t, conn, state, "e2e-page-5", tailAnchorJSON(1))

	// Assert.
	if got.page.GetLiveJoinSeq() != last.GetLastSeq() {
		t.Fatalf("live_join_seq = %d, want the seq of the page's newest item (%d)",
			got.page.GetLiveJoinSeq(), last.GetLastSeq())
	}
}

func TestAPageEchoesTheRequestItAnswersAndCarriesTheFence(t *testing.T) {
	// Arrange — the two facts a client rules on before adopting a page: is this
	// mine, and is it current.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-1", "the only reply"))
	conn := h.dialFrontend(t)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act.
	got := h.pageFrom(t, conn, state, "e2e-page-6", tailAnchorJSON(10))

	// Assert.
	if got.page.GetRequestId() != "e2e-page-6" {
		t.Fatalf("page request_id = %q, want the requesting command's id", got.page.GetRequestId())
	}
	if got.page.GetFence() != state.GetFence() {
		t.Fatalf("page fence = %q, want the workspace's live fence %q", got.page.GetFence(), state.GetFence())
	}
}

func TestAPageEchoingARetiredFenceIsRefused(t *testing.T) {
	// Arrange — a tab that outlived a daemon bounce still echoes the generation
	// it was reading when it decided to ask.
	h := newBouncedHarness(t)
	producer := dialStoreProducer(t)
	producer.write(storedAssistantEvent(t, h.vendorSessionID, "u-1", "the only reply"))
	conn := h.dialFrontend(t)
	_ = workspaceStateInSnapshot(t, readFrame(t, conn), h.workspace)

	// Act — the acceptance still goes out (it reports enqueue, not the read),
	// and the refusal follows under the same request id.
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":"e2e-page-7","workspace":%q,"conversationPage":{%s,"fence":"not-the-live-fence"}}`,
		h.workspace, tailAnchorJSON(10)))

	// Assert.
	var acks int
	var refusal string
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) && refusal == "" {
		frame := readFrame(t, conn)
		if page := frame.GetConversationPage(); page != nil && page.GetRequestId() == "e2e-page-7" {
			t.Fatalf("a page was served against a retired fence")
		}
		ack := frame.GetCommandAck()
		if ack == nil || ack.GetRequestId() != "e2e-page-7" {
			continue
		}
		acks++
		if !ack.GetOk() {
			refusal = ack.GetError()
		}
	}
	if refusal == "" {
		t.Fatalf("no refusal for the stale-fence page arrived before the deadline")
	}
	if acks != 2 {
		t.Fatalf("acks for the refused page = %d, want the acceptance and the refusal", acks)
	}
	if !strings.Contains(refusal, "superseded") {
		t.Fatalf("refusal = %q, want it to name the superseded generation", refusal)
	}
}

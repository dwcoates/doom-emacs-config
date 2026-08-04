// THE KEEP-ALIVE HOLD: what happens to a real prompt that arrives while the
// daemon's own ping is in flight.
//
// It is held, and the hold is a DIFFERENT KIND from every other one the queue
// knows. The classifier never runs on it — there is nothing to decide, because
// the ping must complete and the conversation must be rewound before anything
// real can be submitted — and there is no force-through, because forcing would
// mean submitting the user's prompt on top of the ping the rewind exists to
// erase. The only exits are delivery when the ping ends and the user cancelling.
//
// A NOTE ON THE WINDOW, because it is the one timing dependency in this
// feature's coverage. The offline engine answers a text turn synchronously, so
// the ping's turn is in flight only briefly. These tests submit the prompt
// immediately after firing the check that causes the ping. If that proves too
// tight against the real implementation, the fix is a daemon-side seam that
// publishes the ping's submission as an event a test can wait on — never a
// sleep here, and never a retry, which would hide exactly the ordering these
// tests exist to pin.
package e2e

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// keepAliveHold is a real prompt held by an in-flight keep-alive ping, plus the
// ping that is holding it.
type keepAliveHold struct {
	session    *keepAliveSession
	pingTurnID string
	entry      *frontendv1.QueueEntry
	// text is the held prompt's own text, so a test can recognize its reply.
	text string
}

// heldByKeepAlivePing drives the whole setup: the check fires, the daemon pings,
// the user types, and the daemon holds what they typed.
func heldByKeepAlivePing(t *testing.T, s *keepAliveSession, text string) keepAliveHold {
	t.Helper()
	s.idleFor(t, s.policy.pingAt())
	writeCmd(t, s.conn, fmt.Sprintf(
		`{"requestId":"r-held","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, text))

	var entry *frontendv1.QueueEntry
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueEntry bearing keep_alive_hold": func(frame *frontendv1.FrontendFrame) bool {
			for _, e := range queueViewFor(frame, s.cwd).GetEntries() {
				if e.GetKeepAliveHold() != nil {
					entry = e
					return true
				}
			}
			return false
		},
	})
	ping := s.store.await(t, "the keep-alive ping holding the prompt", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})
	return keepAliveHold{session: s, pingTurnID: keepAlivePing(ping).GetTurnId(), entry: entry, text: text}
}

// --- (8) the hold ---------------------------------------------------------------

// TestE2EAPromptHeldByAPingNamesTheTurnHoldingIt covers the JOIN: the hold
// carries the ping's turn_id, so the bubble the webapp renders ("waiting on a
// keep-alive response") is joined to the turn whose completion releases it
// rather than being an unattributable "please wait".
func TestE2EAPromptHeldByAPingNamesTheTurnHoldingIt(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByKeepAlivePing(t, s, "held-by-the-ping")

	// Assert
	if got := held.entry.GetKeepAliveHold().GetTurnId(); got != held.pingTurnID {
		t.Errorf("keep_alive_hold turn_id = %q, want the in-flight ping's turn %q", got, held.pingTurnID)
	}
}

// TestE2EAPromptHeldByAPingIsNeverClassified covers the CLASSIFIER-FREE hold.
// The frozen comment states the classifier never runs on such an entry but does
// not say what classification it then carries, so this asserts what the
// contract DOES state — that nothing classified it — through the states only a
// classifier can produce.
func TestE2EAPromptHeldByAPingIsNeverClassified(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByKeepAlivePing(t, s, "held-and-unclassified")

	// Assert
	switch got := held.entry.GetClassification(); got {
	case frontendv1.QueueClassification_QUEUE_CLASSIFICATION_PENDING,
		frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT,
		frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR:
		t.Errorf("the keep-alive-held entry's classification is %s: that state is only reachable by running the classifier, which never runs on such an entry", got)
	}
	if got := held.entry.GetRationale(); got != "" {
		t.Errorf("the keep-alive-held entry carries a classifier rationale %q: nothing classified it", got)
	}
}

// TestE2EAPromptHeldByAPingCannotBeForcedThrough covers the ABSENT EXIT. A
// force is "deliver now", and delivering now would submit the user's prompt on
// top of the very ping the rewind exists to erase — so the request is refused
// rather than honored into a corrupted conversation.
func TestE2EAPromptHeldByAPingCannotBeForcedThrough(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByKeepAlivePing(t, s, "held-and-forced")

	// Act
	writeCmd(t, s.conn, fmt.Sprintf(`{"requestId":"r-force","queueForce":{"entryId":%q}}`, held.entry.GetId()))

	// Assert
	ack := awaitAck(t, s.conn, "r-force", "the force of a keep-alive-held entry")
	if ack.GetOk() {
		t.Fatal("queueForce on a keep-alive-held entry was acked ok: forcing it submits the user's prompt on top of a ping that has not been rewound away")
	}
	if ack.GetError() == "" {
		t.Error("the refusal carries no error text: the user must be told why their force did nothing")
	}
}

// TestE2EAPromptHeldByAPingCanStillBeCancelled covers the EXIT THAT REMAINS.
// The user changing their mind must always work: a hold the user cannot escape
// would be a prompt they can neither run nor discard.
func TestE2EAPromptHeldByAPingCanStillBeCancelled(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByKeepAlivePing(t, s, "held-and-cancelled")

	// Act
	writeCmd(t, s.conn, fmt.Sprintf(`{"requestId":"r-cancel","queueCancel":{"entryId":%q}}`, held.entry.GetId()))

	// Assert
	if ack := awaitAck(t, s.conn, "r-cancel", "the cancel of a keep-alive-held entry"); !ack.GetOk() {
		t.Fatalf("queueCancel on a keep-alive-held entry nacked: %s — cancelling is the one exit the user always has", ack.GetError())
	}
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueView with the cancelled entry gone": func(frame *frontendv1.FrontendFrame) bool {
			view := queueViewFor(frame, s.cwd)
			if view == nil {
				return false
			}
			for _, e := range view.GetEntries() {
				if e.GetId() == held.entry.GetId() {
					return false
				}
			}
			return true
		},
	})
}

// TestE2EACancelledHoldRunsNoTurnForIt covers what a cancel MEANS: the prompt
// was discarded, so it must never be submitted after the ping ends. A cancel
// that merely removed the bubble would deliver the prompt anyway a moment
// later, which is worse than not offering a cancel at all.
func TestE2EACancelledHoldRunsNoTurnForIt(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByKeepAlivePing(t, s, "cancelled-never-runs")
	writeCmd(t, s.conn, fmt.Sprintf(`{"requestId":"r-cancel","queueCancel":{"entryId":%q}}`, held.entry.GetId()))
	if ack := awaitAck(t, s.conn, "r-cancel", "the cancel"); !ack.GetOk() {
		t.Fatalf("queueCancel nacked: %s", ack.GetError())
	}

	// Act — a later prompt whose reply is the sentinel: it was submitted after
	// the cancelled one, so its answer cannot precede an answer to that one.
	writeCmd(t, s.conn, `{"requestId":"r-sentinel","submitPrompt":{"text":"sentinel-after-cancel","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert
	reject := func(frame *frontendv1.FrontendFrame) string {
		for _, item := range deltaItems(frame, s.cwd) {
			if strings.Contains(assistantText(item), echoOf(held.text)) {
				return "the cancelled prompt was submitted anyway once the ping ended"
			}
		}
		return ""
	}
	awaitAll(t, s.conn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the sentinel prompt's reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf("sentinel-after-cancel")) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2EAHeldPromptIsDeliveredWhenThePingEnds covers the ORDINARY EXIT: the
// hold is a delay, never a loss. Once the ping's turn ends and the conversation
// has been rewound, the prompt the user typed is submitted and answered.
func TestE2EAHeldPromptIsDeliveredWhenThePingEnds(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByKeepAlivePing(t, s, "delivered-after-the-ping")

	// Assert
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the held prompt's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(held.text)) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2EAHeldPromptIsDeliveredOnTheRewoundConversation covers WHERE it is
// delivered. The rewind produces a NEW vendor session id, so a delivery that
// still went to the old one would be submitted into the seq space the rewind
// retired — the prompt would run against a conversation that still contains the
// ping the rewind existed to erase.
func TestE2EAHeldPromptIsDeliveredOnTheRewoundConversation(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	before := s.vendorID
	held := heldByKeepAlivePing(t, s, "delivered-on-the-new-conversation")

	// Act — the identity the daemon files this session under after the rewind.
	after := vendorSessionID(t, s.h, s.sessionID,
		func(id string) bool { return id != "" && id != before },
		fmt.Sprintf("a conversation identity other than %q", before))

	// Assert — the held prompt's turn is in the NEW seq space.
	rewound := tailStore(t, after)
	rewound.await(t, "the held prompt's TurnStarted in the rewound conversation", func(ev *corev1.Event) bool {
		started := userTurnStart(ev)
		return started != nil && strings.Contains(started.GetPromptPreview(), held.text)
	})
}

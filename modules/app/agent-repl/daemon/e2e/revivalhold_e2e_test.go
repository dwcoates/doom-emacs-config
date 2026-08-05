// THE REVIVAL HOLD, AS THE FRONTEND SEES IT.
//
// revive_e2e_test.go already pins the BEHAVIOR of a compact-first revival's
// gate: a prompt typed while the compaction is in flight is not answered, and
// it is answered the moment the compaction lands. Those tests read the
// conversation. This file reads the QUEUE.
//
// The queue is where the user finds out. A gated prompt that produced no
// QueueEntry, or one carried under the wrong hold, leaves the webapp with
// nothing true to draw: it would run the classifier bubble on an entry no
// classifier ever touched, or render "waiting on a keep-alive response" for a
// wait that has nothing to do with a ping. So the projection is its own
// contract — QueueEntry.revival_hold, naming the session whose revival holds
// the entry — and it is asserted here on the wire, separately from the gate.
package e2e

import (
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// revivalHold is a prompt held by a compact-first revival whose compaction has
// not landed yet, plus the entry the daemon projected for it.
type revivalHold struct {
	session *keepAliveSession
	entry   *frontendv1.QueueEntry
	// text is the held prompt's own text, so a test can recognize its reply.
	text string
}

// heldByPendingCompaction drives the whole setup: a hibernated session is
// revived compact-first, the user types while the daemon's own compaction is
// still in flight, and the daemon projects the hold.
//
// The compaction turn's reply is awaited in the SAME await as the QueueView,
// not before or after it. The two come from different producers (the queue is
// pushed from the daemon's own dispatch, the reply travels the store plane), so
// a sequential wait on either one would consume and discard the other. Waiting
// for the reply here also leaves the caller at a defined point: the compaction
// turn has run, and only its LANDING — the ContextCompacted the sidecar
// records — is still outstanding.
func heldByPendingCompaction(t *testing.T, s *keepAliveSession, text string) revivalHold {
	t.Helper()
	s.hibernate(t, "r-hibernate")
	sendReviveCompactFirst(t, s.conn, "r-revive")
	if ack := awaitAck(t, s.conn, "r-revive", "the compact-first revival"); !ack.GetOk() {
		t.Fatalf("reviveSession(compact_first) nacked while setting up a revival hold: %s", ack.GetError())
	}
	writeCmd(t, s.conn, fmt.Sprintf(
		`{"requestId":"r-held","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, text))

	var entry *frontendv1.QueueEntry
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueEntry for the prompt typed during the revival": func(frame *frontendv1.FrontendFrame) bool {
			for _, e := range queueViewFor(frame, s.cwd).GetEntries() {
				if strings.Contains(e.GetText(), text) {
					entry = e
					return true
				}
			}
			return false
		},
		"the compaction turn's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(compactCommand)) {
					return true
				}
			}
			return false
		},
	})
	return revivalHold{session: s, entry: entry, text: text}
}

// --- the hold ----------------------------------------------------------------

// TestE2EAPromptHeldByARevivalNamesTheSessionBeingRevived covers the JOIN: the
// hold carries the id of the session whose revival is holding the entry, so the
// bubble the webapp renders ("waiting on the revival's compaction") is joined to
// the revival whose completed compaction releases it rather than being an
// unattributable "please wait".
func TestE2EAPromptHeldByARevivalNamesTheSessionBeingRevived(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByPendingCompaction(t, s, "held-by-the-revival")

	// Assert
	hold := held.entry.GetRevivalHold()
	if hold == nil {
		t.Fatalf("the entry for a prompt typed during a pending compact-first revival carries no revival_hold: the webapp has nothing to draw but the classifier bubble, and no classifier ran on it")
	}
	if got := hold.GetSessionId(); got != s.sessionID {
		t.Errorf("revival_hold session_id = %q, want the session being revived %q", got, s.sessionID)
	}
}

// TestE2EAPromptHeldByARevivalIsNotHeldByAKeepAlivePing covers the
// MISATTRIBUTION. The three holds are different kinds and the webapp draws a
// different bubble from each; an entry gated by a revival's compaction but
// carried under keep_alive_hold would tell the user their prompt is waiting on
// a ping that is not running, and would join the bubble to a turn that does not
// exist.
func TestE2EAPromptHeldByARevivalIsNotHeldByAKeepAlivePing(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByPendingCompaction(t, s, "held-and-not-by-a-ping")

	// Assert
	if hold := held.entry.GetKeepAliveHold(); hold != nil {
		t.Errorf("the revival-held entry carries keep_alive_hold (turn %q): nothing pinged this session, and the revival's compaction is what holds the entry", hold.GetTurnId())
	}
}

// --- the release --------------------------------------------------------------

// TestE2EALandedCompactionReleasesTheRevivalHold covers the EXIT the projection
// must show. The hold is a delay, so the bubble must stop claiming the prompt
// is waiting once it is not: the moment the compaction lands and the gate
// opens, a QueueView shows the entry no longer holding — either released in
// place or gone because it was delivered.
//
// The delivered reply is awaited alongside it, so "gone" is proven to be
// delivery rather than a silent drop.
func TestE2EALandedCompactionReleasesTheRevivalHold(t *testing.T) {
	// Arrange — a held entry, already observed on the wire, so every frame read
	// from here on was pushed after the hold existed. Without that drain, the
	// empty QueueView pushed before the prompt was ever typed would satisfy the
	// "entry gone" half of the assertion for the wrong reason.
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByPendingCompaction(t, s, "released-by-the-landed-compaction")

	// Act — the compaction lands, exactly as the sidecar records one.
	store := dialStoreProducer(t)
	store.write(sidecarCompactEvent(revivedVendorID(t, s), "e2e-revival-hold-compact-1", "the conversation so far"))

	// Assert
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueView in which the entry no longer holds": func(frame *frontendv1.FrontendFrame) bool {
			view := queueViewFor(frame, s.cwd)
			if view == nil {
				return false
			}
			for _, e := range view.GetEntries() {
				if e.GetId() == held.entry.GetId() {
					return e.GetRevivalHold() == nil
				}
			}
			return true
		},
		"the released prompt's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(held.text)) {
					return true
				}
			}
			return false
		},
	})
}

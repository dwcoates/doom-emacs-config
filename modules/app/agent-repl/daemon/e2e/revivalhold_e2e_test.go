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
// contract — QueueEntry's `revival` hold arm — and it is asserted here on the
// wire, separately from the gate.
//
// The arm names no session. It used to, and the id is now reserved: a revival is
// a WORKSPACE-level event and the entry rides its workspace's QueueView, so the
// attribution lives on the queue rather than inside the arm. The arm's presence
// is the whole fact it carries.
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
	// workspace is the QueueView the entry was actually found on. It is the
	// join the retired session id used to carry: a revival is a workspace-level
	// event, so the queue the entry rides IS its attribution.
	workspace string
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
	// The ack is READ KEEPING what it reads past. The revival submits its
	// "/compact" from inside the command's own dispatch, so the compaction
	// turn's reply travels this same socket and can overtake the ack; an await
	// that dropped it would then wait out its budget for a frame already
	// delivered.
	ack, beforeAck := awaitAckKeeping(t, s.conn, "r-revive", "the compact-first revival")
	if !ack.GetOk() {
		t.Fatalf("reviveSession(compact_first) nacked while setting up a revival hold: %s", ack.GetError())
	}
	writeCmd(t, s.conn, fmt.Sprintf(
		`{"requestId":"r-held","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, text))

	var entry *frontendv1.QueueEntry
	var workspace string
	awaitAllSeeded(t, s.conn, frameTimeout, beforeAck, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueEntry for the prompt typed during the revival": func(frame *frontendv1.FrontendFrame) bool {
			view := queueViewFor(frame, s.cwd)
			for _, e := range view.GetEntries() {
				if strings.Contains(e.GetText(), text) {
					entry = e
					workspace = view.GetWorkspace()
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
	return revivalHold{session: s, entry: entry, text: text, workspace: workspace}
}

// --- the hold ----------------------------------------------------------------

// TestE2EAPromptHeldByARevivalCarriesTheRevivalHold covers the DISTINGUISHER:
// the entry selects the `revival` arm of its hold oneof, which is the whole
// fact the arm carries. That presence is what tells the webapp to draw "waiting
// on the revival's compaction" instead of the classifier bubble no classifier
// ever ran.
//
// The arm's PRESENCE is the assertion because the contract deliberately left it
// with nothing else to state; see the workspace test below for where the
// attribution went.
func TestE2EAPromptHeldByARevivalCarriesTheRevivalHold(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByPendingCompaction(t, s, "held-by-the-revival")

	// Assert
	if hold := held.entry.GetRevival(); hold == nil {
		t.Fatalf("the entry for a prompt typed during a pending compact-first revival carries no revival_hold: the webapp has nothing to draw but the classifier bubble, and no classifier ran on it")
	}
}

// TestE2EAPromptHeldByARevivalRidesItsOwnWorkspacesQueue covers the JOIN, which
// is now the QUEUE the entry rides rather than an id the hold carries.
//
// QueueEntryRevivalHold used to name the session being revived. That field is
// reserved: a revival is a WORKSPACE-level event and the entry already rides its
// workspace's QueueView, so the id joined the bubble to nothing a client could
// not already reach. The guarantee the old assertion existed for — a held prompt
// is attributable, never an unattributable "please wait" — is unchanged, so it
// is asserted here against the surface that actually carries it.
func TestE2EAPromptHeldByARevivalRidesItsOwnWorkspacesQueue(t *testing.T) {
	// Arrange + Act
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	held := heldByPendingCompaction(t, s, "held-on-its-own-queue")

	// Assert
	if held.workspace != s.cwd {
		t.Errorf("the revival-held entry rode the QueueView for workspace %q, want %q: the queue is what attributes the hold now that the arm names no session", held.workspace, s.cwd)
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
	if hold := held.entry.GetKeepAlive(); hold != nil {
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
					return e.GetRevival() == nil
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

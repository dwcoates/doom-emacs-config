// THE REVIVAL GATE: waking a hibernated session is a DECISION the user makes,
// and no model use may precede it.
//
// The gate is enforced by refusing prompts, not by rendering. A webapp that
// forgot to draw the gate, an Emacs client that never learned about it, or a
// stale frontend still holding a session it remembers as awake must all be
// unable to spend the user's money on a conversation they have not chosen to
// resume — so SubmitPromptCmd on a hibernated session is nacked, loudly, and
// exactly one ReviveSessionCmd opens it.
//
// The two revival modes are the two ways that conversation can be worth
// resuming. `direct` says "I know it's big, resume it verbatim".
// `compact_first` pays the full-context cost ONCE, in a compaction, instead of
// on every subsequent turn — and until that compaction lands, the gate stays
// shut, because a prompt admitted early would pay exactly the cost the choice
// exists to avoid.
//
// WHY THE COMPACTION IS INJECTED. The shim-claude-sidecar is the sole producer
// of ContextCompacted and it produces it by tailing a real vendor transcript,
// which a `--fake` run does not have (clearcompact_e2e_test.go's header). These
// tests write exactly what the sidecar writes, so everything downstream of the
// producer — store ingest, fan-out, the shim's forward, the daemon's curation,
// and the gate reading it — is exercised for real.
package e2e

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// compactCommand is what the daemon submits on the user's behalf under
// compact_first. It is the vendor's own slash command, not a daemon invention.
const compactCommand = "/compact"

// --- the gate is shut ----------------------------------------------------------

// TestE2EAPromptIsRefusedWhileTheSessionIsHibernated is the gate itself: no
// model use can precede the revival decision, so the prompt is refused rather
// than quietly waking the session to serve it.
func TestE2EAPromptIsRefusedWhileTheSessionIsHibernated(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	s.hibernate(t, "r-hibernate")

	// Act
	writeCmd(t, s.conn, `{"requestId":"r-prompt","submitPrompt":{"text":"wake up and work","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert
	ack := awaitAck(t, s.conn, "r-prompt", "the prompt sent to a hibernated session")
	if ack.GetOk() {
		t.Fatal("submitPrompt was acked ok on a hibernated session: the revival decision is the user's, and admitting a prompt makes it for them")
	}
	if ack.GetError() == "" {
		t.Error("the refusal carries no error text: the user must be told the session is asleep, not left with a prompt that vanished")
	}
}

// --- direct revival --------------------------------------------------------------

// TestE2EADirectRevivalOpensTheGate covers the deliberate "resume it as-is"
// path: the session comes up along the ordinary bring-up, and the very next
// prompt is served.
func TestE2EADirectRevivalOpensTheGate(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	s.hibernate(t, "r-hibernate")

	// Act
	sendReviveDirect(t, s.conn, "r-revive")
	if ack := awaitAck(t, s.conn, "r-revive", "the direct revival"); !ack.GetOk() {
		t.Fatalf("reviveSession(direct) nacked: %s", ack.GetError())
	}
	s.awaitAwake(t)
	writeCmd(t, s.conn, `{"requestId":"r-prompt","submitPrompt":{"text":"after-direct-revival","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — the prompt's own reply, which only a live shim can produce.
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the revived session's reply to the first prompt after revival": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf("after-direct-revival")) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2EADirectRevivalCompactsNothing covers the OTHER half of the choice:
// `direct` means verbatim, so the daemon must not quietly compact anyway. A
// revival that compacted regardless would silently discard the context the user
// chose to keep.
func TestE2EADirectRevivalCompactsNothing(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	s.hibernate(t, "r-hibernate")

	// Act
	sendReviveDirect(t, s.conn, "r-revive")
	if ack := awaitAck(t, s.conn, "r-revive", "the direct revival"); !ack.GetOk() {
		t.Fatalf("reviveSession(direct) nacked: %s", ack.GetError())
	}
	s.awaitAwake(t)
	writeCmd(t, s.conn, `{"requestId":"r-prompt","submitPrompt":{"text":"after-direct-revival","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — the user's own prompt is the FIRST turn of the revived session.
	reject := func(frame *frontendv1.FrontendFrame) string {
		for _, item := range deltaItems(frame, s.cwd) {
			if strings.Contains(assistantText(item), echoOf(compactCommand)) {
				return "a direct revival submitted " + compactCommand + " anyway"
			}
		}
		return ""
	}
	awaitAll(t, s.conn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the user's own first post-revival turn": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf("after-direct-revival")) {
					return true
				}
			}
			return false
		},
	})
}

// --- compact-first revival ----------------------------------------------------

// TestE2EACompactFirstRevivalSubmitsCompactItself covers the FIRST ORDER OF
// BUSINESS: the daemon drives the compaction, so the user does not have to know
// that resuming a large conversation cheaply means typing a slash command.
func TestE2EACompactFirstRevivalSubmitsCompactItself(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	s.hibernate(t, "r-hibernate")

	// Act
	sendReviveCompactFirst(t, s.conn, "r-revive")
	if ack := awaitAck(t, s.conn, "r-revive", "the compact-first revival"); !ack.GetOk() {
		t.Fatalf("reviveSession(compact_first) nacked: %s", ack.GetError())
	}

	// Assert
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a turn carrying the daemon-submitted " + compactCommand: func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(compactCommand)) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2EPromptsAreGatedUntilTheCompactionLands covers WHY compact_first is a
// gate and not merely an ordering: a prompt admitted while the compaction is
// still running would be answered against the whole uncompacted history, which
// is precisely the cost the user chose this mode to pay only once.
func TestE2EPromptsAreGatedUntilTheCompactionLands(t *testing.T) {
	// Arrange — a compact-first revival with its compaction underway.
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	s.hibernate(t, "r-hibernate")
	sendReviveCompactFirst(t, s.conn, "r-revive")
	if ack := awaitAck(t, s.conn, "r-revive", "the compact-first revival"); !ack.GetOk() {
		t.Fatalf("reviveSession(compact_first) nacked: %s", ack.GetError())
	}

	// Act — the user types while the compaction is in flight.
	writeCmd(t, s.conn, `{"requestId":"r-early","submitPrompt":{"text":"typed-before-compaction","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — the compaction's own turn runs and the early prompt does not.
	// The compaction turn is the sentinel: it was submitted BEFORE this prompt,
	// so its reply cannot arrive after a reply to a prompt that jumped it.
	reject := func(frame *frontendv1.FrontendFrame) string {
		for _, item := range deltaItems(frame, s.cwd) {
			if strings.Contains(assistantText(item), echoOf("typed-before-compaction")) {
				return "a prompt submitted during a compact-first revival was answered before the compaction landed"
			}
		}
		return ""
	}
	awaitAll(t, s.conn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the compaction turn's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(compactCommand)) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2EACompletedCompactionReleasesTheGatedPrompt covers the RELEASE: gated
// is delayed, never dropped. The moment the compaction lands, the prompt the
// user typed during it is delivered.
func TestE2EACompletedCompactionReleasesTheGatedPrompt(t *testing.T) {
	// Arrange — a gated prompt behind a compact-first revival.
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	s.hibernate(t, "r-hibernate")
	sendReviveCompactFirst(t, s.conn, "r-revive")
	if ack := awaitAck(t, s.conn, "r-revive", "the compact-first revival"); !ack.GetOk() {
		t.Fatalf("reviveSession(compact_first) nacked: %s", ack.GetError())
	}
	writeCmd(t, s.conn, `{"requestId":"r-gated","submitPrompt":{"text":"released-by-compaction","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitItem(t, s.conn, s.cwd, "the compaction turn's reply", func(item *frontendv1.ConversationItem) bool {
		return strings.Contains(assistantText(item), echoOf(compactCommand))
	})

	// Act — the compaction lands, exactly as the sidecar records one.
	store := dialStoreProducer(t)
	store.write(sidecarCompactEvent(revivedVendorID(t, s), "e2e-revival-compact-1", "the conversation so far"))

	// Assert
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the gated prompt's own reply, once the compaction had landed": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf("released-by-compaction")) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2EACompactionThatNeverLandsKeepsTheGateShut covers the FAILURE: the gate
// is opened by the compaction LANDING, never by the compaction turn merely
// ending. A compaction that ran and produced nothing has not paid the cost the
// mode exists to pay, so admitting prompts afterwards would quietly give the
// user the expensive path they explicitly declined.
//
// The sentinel is an injected CLEAR: a store event that travels the same pipe a
// compaction would, so its arrival on the frontend proves the pipe carried
// everything written before it — and no compaction was.
func TestE2EACompactionThatNeverLandsKeepsTheGateShut(t *testing.T) {
	// Arrange — the compaction turn has ENDED with no ContextCompacted behind it.
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	s.hibernate(t, "r-hibernate")
	sendReviveCompactFirst(t, s.conn, "r-revive")
	if ack := awaitAck(t, s.conn, "r-revive", "the compact-first revival"); !ack.GetOk() {
		t.Fatalf("reviveSession(compact_first) nacked: %s", ack.GetError())
	}
	awaitItem(t, s.conn, s.cwd, "the compaction turn's result item", isResult)

	// Act
	writeCmd(t, s.conn, `{"requestId":"r-after","submitPrompt":{"text":"after-a-failed-compaction","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	store := dialStoreProducer(t)
	store.write(sidecarClearEvent(revivedVendorID(t, s), "e2e-revival-gate-sentinel"))

	// Assert
	reject := func(frame *frontendv1.FrontendFrame) string {
		for _, item := range deltaItems(frame, s.cwd) {
			if strings.Contains(assistantText(item), echoOf("after-a-failed-compaction")) {
				return "a prompt was answered after a compact-first revival whose compaction never landed"
			}
		}
		return ""
	}
	awaitAll(t, s.conn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the sentinel clear": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if isClear(item) {
					return true
				}
			}
			return false
		},
	})
}

// revivedVendorID reports the conversation uuid the store files this session
// under NOW.
//
// It is re-read rather than reused from bring-up because a revival re-runs the
// handshake, and an event written under a uuid the session has left lands in a
// seq space nothing is subscribed to — the store answers such a subscription
// with a silent empty replay, which would make every assertion above pass or
// fail for the wrong reason.
func revivedVendorID(t *testing.T, s *keepAliveSession) string {
	t.Helper()
	return vendorSessionID(t, s.h, s.sessionID, func(id string) bool { return id != "" },
		"a conversation identity for the revived session")
}

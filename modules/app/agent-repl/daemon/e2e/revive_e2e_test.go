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
// The revival modes are the ways that conversation can be worth resuming.
// `direct` says "I know it's big, resume it verbatim". `compact_first` pays the
// full-context cost ONCE, in a compaction, instead of on every subsequent turn
// — and until that compaction lands, the gate stays shut, because a prompt
// admitted early would pay exactly the cost the choice exists to avoid.
//
// A COMPACTION ALSO CARRIES A SCOPE, which says how much of the conversation it
// may summarize away: everything, only the assistant's responses, only the
// user's prompts, or both of those while the tool calls and their results
// survive. The scope reaches the CLI as `/compact <instructions>` and nothing
// else, so these tests assert on the text the vendor actually received.
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

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/daemonturn"
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
	// KEEPING what the ack await reads past: the awake SessionView rides the
	// bulk lane and the ack the control lane, so the view may already have been
	// delivered by the time the ack is matched.
	ack, beforeAck := awaitAckKeeping(t, s.conn, "r-revive", "the direct revival")
	if !ack.GetOk() {
		t.Fatalf("reviveSession(direct) nacked: %s", ack.GetError())
	}
	s.awaitAwake(t, beforeAck...)
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
	// KEEPING what the ack await reads past: the awake SessionView rides the
	// bulk lane and the ack the control lane, so the view may already have been
	// delivered by the time the ack is matched.
	ack, beforeAck := awaitAckKeeping(t, s.conn, "r-revive", "the direct revival")
	if !ack.GetOk() {
		t.Fatalf("reviveSession(direct) nacked: %s", ack.GetError())
	}
	s.awaitAwake(t, beforeAck...)
	writeCmd(t, s.conn, `{"requestId":"r-prompt","submitPrompt":{"text":"after-direct-revival","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — the user's own prompt is the FIRST turn of the revived session.
	//
	// THE DURABLE RECORD IS WHAT PROVES THE NEGATIVE. A context cut the daemon
	// submitted is withheld from every rendering
	// (sessioncontroller/contextcutexclude.go), so its absence from the feed
	// would hold whether or not one had run; the store keeps the turn either
	// way, and its arrival order makes reading past the user's own turn proof
	// that no compaction preceded it.
	tail := tailStore(t, revivedVendorID(t, s))
	tail.awaitSentinel(t, "the user's own first turn after a direct revival",
		func(ev *corev1.Event) string {
			if started := daemonCompactStart(ev); started != nil {
				return "a direct revival submitted " + compactCommand + " anyway (turn_id=" + started.GetTurnId() + ")"
			}
			return ""
		}, func(ev *corev1.Event) bool {
			started := userTurnStart(ev)
			return started != nil && strings.Contains(started.GetPromptPreview(), "after-direct-revival")
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

	// Assert — read from the durable record, which is where a cut the daemon
	// submitted is observable at all.
	if got := awaitDaemonCompact(t, s); !strings.HasPrefix(got, compactCommand) {
		t.Fatalf("the compact-first revival submitted %q, want the vendor's own %s", got, compactCommand)
	}
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
	// The compaction turn's END is the sentinel: it was submitted BEFORE this
	// prompt, so the store's arrival order makes a missing earlier turn start
	// proof of absence rather than of lateness.
	tail := tailStore(t, revivedVendorID(t, s))
	tail.awaitSentinel(t, "the revival's own /compact turn ending in the durable record",
		func(ev *corev1.Event) string {
			if started := userTurnStart(ev); started != nil && strings.Contains(started.GetPromptPreview(), "typed-before-compaction") {
				return "a prompt submitted during a compact-first revival reached the vendor before the compaction landed"
			}
			return ""
		}, daemonCompactEnded)
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
	awaitDaemonCompactRan(t, s)

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
	awaitDaemonCompactRan(t, s)

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

// --- scoped compact-first revival ---------------------------------------------

// scopedCompactCases is the wire scope paired with the phrase its submitted
// instruction must carry. The phrase is the part a user would check: what the
// compaction is told to LEAVE ALONE.
var scopedCompactCases = []struct {
	name  string
	scope string
	// want is a substring of the text the daemon submits, distinctive enough
	// that no other scope's instruction contains it.
	want string
}{
	{
		name:  "responses only",
		scope: "COMPACTION_SCOPE_RESPONSES",
		want:  "Summarize ONLY the assistant's own response messages.",
	},
	{
		name:  "prompts only",
		scope: "COMPACTION_SCOPE_PROMPTS",
		want:  "Summarize ONLY the user's prompt messages. Preserve every assistant response",
	},
	{
		name:  "prompts and responses",
		scope: "COMPACTION_SCOPE_PROMPTS_AND_RESPONSES",
		want:  "Summarize ONLY the user's prompt messages and the assistant's own response messages.",
	},
}

// TestE2EAScopedCompactFirstRevivalSubmitsItsSteeredCompact covers the WHOLE
// POINT of the scope: it must reach the CLI as steering on the compaction the
// daemon submits, not as a daemon-side preference nothing acts on. A scope that
// changed no submitted text would compact the entire conversation while the
// user was told only their responses were going.
func TestE2EAScopedCompactFirstRevivalSubmitsItsSteeredCompact(t *testing.T) {
	for _, tc := range scopedCompactCases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s := newKeepAliveSession(t, testKeepAlivePolicy())
			s.hibernate(t, "r-hibernate")

			// Act
			sendReviveCompactScoped(t, s.conn, "r-revive", tc.scope)
			if ack := awaitAck(t, s.conn, "r-revive", "the scoped compact-first revival"); !ack.GetOk() {
				t.Fatalf("reviveSession(%s) nacked: %s", tc.scope, ack.GetError())
			}

			// Assert — the durable record carries the prompt the vendor was
			// actually handed, which states both facts at once: that the turn
			// was a `/compact`, and that the instructions the scope adds
			// travelled with it. The exact wording is the session controller's
			// own unit test; what an e2e can prove is that the CLI got it.
			got := awaitDaemonCompact(t, s)
			if !strings.HasPrefix(got, compactCommand+" ") || !strings.Contains(got, tc.want) {
				t.Fatalf("the %s revival submitted %q, want a %s carrying %q", tc.scope, got, compactCommand, tc.want)
			}
		})
	}
}

// TestE2EACompactFirstRevivalWithNoScopeIsNacked covers the REFUSED ZERO over
// the real wire: an omitted scope is a compact-first arm that never said what
// it may summarize away, and answering it by compacting everything would
// discard the conversation on nobody's instruction.
func TestE2EACompactFirstRevivalWithNoScopeIsNacked(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	s.hibernate(t, "r-hibernate")

	// Act
	sendReviveCompactNoScope(t, s.conn, "r-revive")

	// Assert
	ack := awaitAck(t, s.conn, "r-revive", "the scopeless compact-first revival")
	if ack.GetOk() {
		t.Fatal("reviveSession(compact_first with no scope) was accepted, want a nack: the daemon has no default for what a compaction may swallow")
	}
	if !strings.Contains(ack.GetError(), "scope") {
		t.Fatalf("nack error = %q, want it to name the missing scope", ack.GetError())
	}
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


// --- observing the daemon's own compaction ------------------------------------
//
// A CONTEXT CUT THE DAEMON SUBMITTED LEAVES NO RESIDUE IN ANY RENDERING. Its
// echo, its terminal result and any notice the CLI answered with are withheld
// from the live push, the ring resync, the store re-pull and the durable replay
// alike (sessioncontroller/contextcutexclude.go): the turn is the daemon's own
// bookkeeping, and a duration chip over a turn the user cannot see is a
// statement they have no way to read.
//
// So the feed is deliberately NOT where a revival's `/compact` is observable,
// and the assertions below read the DURABLE RECORD instead. The turn is written
// to the store under its `revive-compact:` id, which carries both facts these
// tests need: what the CLI received, and that the turn ran.

// daemonCompactStart returns the TurnStarted of a revival's own `/compact`, or
// nil. The id's family is the verdict, read through the one vocabulary the
// minting site uses.
func daemonCompactStart(ev *corev1.Event) *corev1.TurnStarted {
	started := ev.GetTurnStarted()
	if started == nil || !strings.HasPrefix(started.GetTurnId(), daemonturn.ReviveCompactPrefix) {
		return nil
	}
	return started
}

// daemonCompactEnded reports whether the event ends a revival's own `/compact`.
func daemonCompactEnded(ev *corev1.Event) bool {
	ended := ev.GetTurnEnded()
	return ended != nil && strings.HasPrefix(ended.GetTurnId(), daemonturn.ReviveCompactPrefix)
}

// awaitDaemonCompact returns the prompt the revival's own `/compact` carried to
// the vendor. The preview is the whole prompt here: every instruction this
// daemon composes is one line well under the shim's 200-character cap.
func awaitDaemonCompact(t *testing.T, s *keepAliveSession) string {
	t.Helper()
	tail := tailStore(t, revivedVendorID(t, s))
	ev := tail.await(t, "the revival's own /compact in the durable record", func(ev *corev1.Event) bool {
		return daemonCompactStart(ev) != nil
	})
	return daemonCompactStart(ev).GetPromptPreview()
}

// awaitDaemonCompactRan returns once the revival's own `/compact` turn has
// ENDED, which is the point every "the compaction has run, only its LANDING is
// outstanding" arrangement needs to stand on.
func awaitDaemonCompactRan(t *testing.T, s *keepAliveSession) {
	t.Helper()
	tail := tailStore(t, revivedVendorID(t, s))
	tail.await(t, "the revival's own /compact turn ending in the durable record", daemonCompactEnded)
}

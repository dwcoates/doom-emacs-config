// THE PING NOBODY WAS WAITING BEHIND.
//
// keepaliverewind_e2e_test.go covers the rewind the daemon has always
// performed: a user's prompt was HELD by an in-flight ping, and the delivery of
// that prompt is what rewound the ping out of the conversation. That path was
// never the common one. The overwhelmingly common shape is a ping that fires
// against a session nobody is touching, completes with nothing waiting behind
// it, and leaves its two turns at the tail of the vendor transcript.
//
// Nothing used to rewind those. The next prompt — typed a minute later, or an
// hour later, or after a restart — was submitted on top of them, so the model
// answered with "respond to this message with only a '.'" in its context and
// occasionally said so out loud. THAT is the defect this file covers, and the
// only evidence about it is the conversation identity: a rewind happened
// exactly when the daemon flipped the session to a truncated copy BEFORE the
// prompt's own turn began.
package e2e

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// pingCompletedWithNothingWaiting fires one keep-alive ping and returns once its
// turn has ENDED with no prompt held behind it — the state in which the ping's
// turns are standing in the transcript and no aftermath ran.
func pingCompletedWithNothingWaiting(t *testing.T, s *keepAliveSession) string {
	t.Helper()
	writeDefaultRewindFixture(t, s)
	s.idleFor(t, s.policy.pingAt())
	s.syncSweep(t)
	ping := s.store.await(t, "the keep-alive ping's own turn start", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})
	turnID := keepAlivePing(ping).GetTurnId()
	s.store.await(t, "the keep-alive ping's turn end", func(ev *corev1.Event) bool {
		return turnEndedOf(ev, turnID)
	})
	// THE STORE'S TurnEnded IS NOT THE BARRIER THESE TESTS NEED. It says the
	// shim reported the end; it does not say the DAEMON has processed it. Until
	// the daemon's own boundary has run, the ping's claim still stands and a
	// prompt arriving would be HELD by it — which drives the pre-existing
	// held-prompt rewind and would make every assertion below pass for the
	// wrong reason. The workspace publishing itself un-busy is the daemon's own
	// statement that the boundary is behind it.
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the daemon publishing the workspace un-busy after the ping": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, s.cwd)
			return state != nil && !state.GetTurnActive()
		},
	})
	return turnID
}

// TestE2EAPromptAfterAnUnattendedPingRewindsBeforeItIsSubmitted covers THE GAP.
// The ping is over, nothing was waiting behind it, and the user then types. The
// daemon must rewind the completed ping out of the conversation before that
// prompt is submitted — otherwise the prompt is answered against a context that
// contains the ping.
func TestE2EAPromptAfterAnUnattendedPingRewindsBeforeItIsSubmitted(t *testing.T) {
	// Arrange — a ping that fired and finished with nobody waiting.
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	previous := s.vendorID
	pingCompletedWithNothingWaiting(t, s)

	// Act — the user types, some time after the ping is over.
	writeCmd(t, s.conn, fmt.Sprintf(
		`{"requestId":"r-after-ping","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`,
		"what were we doing?"))

	// Assert — the conversation the prompt landed on is a rewound copy, not the
	// one carrying the ping.
	next := vendorSessionID(t, s.h, s.sessionID,
		func(id string) bool { return id != "" && id != previous },
		fmt.Sprintf("a rewound conversation identity other than %q", previous))
	if next == previous {
		t.Fatalf("the conversation identity never moved off %q: the prompt was submitted on top of the completed keep-alive turns, which is exactly the context leak this rewind exists to prevent", previous)
	}
}

// TestE2EAPromptAfterAnUnattendedPingStillRunsOnTheRewoundConversation covers
// WHO PAYS. Rewinding is housekeeping; the user's prompt must still run, and it
// must run on the conversation the rewind produced rather than on the retired
// one.
func TestE2EAPromptAfterAnUnattendedPingStillRunsOnTheRewoundConversation(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	previous := s.vendorID
	pingCompletedWithNothingWaiting(t, s)
	const text = "still-runs-after-the-rewind"

	// Act
	writeCmd(t, s.conn, fmt.Sprintf(
		`{"requestId":"r-after-ping-runs","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, text))
	next := vendorSessionID(t, s.h, s.sessionID,
		func(id string) bool { return id != "" && id != previous },
		fmt.Sprintf("a rewound conversation identity other than %q", previous))
	// The production sidecar tails the truncated copy and backfills it into the
	// new seq space; the harness runs no sidecar, so the backfill is replayed
	// here, exactly where the real one would begin tailing.
	ingestTranscriptAsSidecar(t, s, next)

	// Assert
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the prompt's own reply on the rewound conversation": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if strings.Contains(assistantText(item), echoOf(text)) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2EAPromptWithNoPingBehindItRewindsNothing covers the NEGATIVE, which is
// the overwhelmingly common case and the one a careless fix breaks: a workspace
// that was never pinged must not pay a shim bounce and a conversation flip on
// every prompt.
func TestE2EAPromptWithNoPingBehindItRewindsNothing(t *testing.T) {
	// Arrange — no ping has ever fired on this session.
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	writeDefaultRewindFixture(t, s)
	previous := s.vendorID

	// Act
	s.runRealTurn(t, "r-no-ping", "an ordinary prompt")

	// Assert — the conversation identity is untouched.
	if got := readSessionRow(t, s.h, s.sessionID).claudeSessionID; got != previous {
		t.Errorf("the conversation identity moved to %q on a workspace that was never pinged (was %q): an unconditional rewind costs a bring-up on every prompt", got, previous)
	}
}

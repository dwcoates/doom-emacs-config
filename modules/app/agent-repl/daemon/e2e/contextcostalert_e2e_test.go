// THE UNCACHED-INPUT ALARM: the loud "this turn re-ingested the context"
// signal, and — for a keep-alive ping — the loud "the cache was already gone"
// one.
//
// WHY THE MEASURE IS input_tokens PLUS cache_creation_input_tokens, which is
// the single fact most of this file exists to pin. The vendor marks nearly all
// input cacheable, so a cold prompt surfaces as cache CREATION and raw
// input_tokens stays near zero even on a full re-ingest. An alarm wired to
// input_tokens alone would therefore be silent on exactly the turns it exists
// to catch.
//
// HOW AN EXPENSIVE TURN IS PRODUCED HERE. Not by faking a usage payload — the
// offline engine's result usage is a fixed, honest shape (fake-query.ts) — but
// by moving the THRESHOLD, which is a first-class configuration knob
// (AGENT_REPL_UNCACHED_COST_ALERT_TOKENS). That makes the tests stronger rather
// than weaker: the engine's usage is known exactly, so the alert's
// uncached_input_tokens can be asserted as a NUMBER, and getting the formula
// wrong in either direction fails.
package e2e

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// The offline engine's result usage for an ordinary turn (fake-query.ts
// `usage`): input_tokens 7, cache_read_input_tokens 80, and
// cache_creation_input_tokens 4.
const (
	fakeTurnInputTokens         int64 = 7
	fakeTurnCacheCreationTokens int64 = 4
	fakeTurnCacheReadTokens     int64 = 80
)

// fakeTurnUncachedTokens is what the contract's formula makes of that usage.
// The cache READ is deliberately absent from it: input the vendor served from
// cache is exactly what did NOT cost anything to re-ingest, and it is far the
// largest number in the payload, so a formula that included it would alarm on
// every cache-efficient turn.
const fakeTurnUncachedTokens = fakeTurnInputTokens + fakeTurnCacheCreationTokens

// alertingPolicy trips the alert on any ordinary turn by putting the threshold
// below the engine's known uncached cost.
func alertingPolicy() keepAlivePolicy {
	p := testKeepAlivePolicy()
	p.costAlert = fakeTurnUncachedTokens - 1
	return p
}

// awaitExpensiveTurn reads conn until a ProgressView carries the cost alert FOR
// THE NAMED TURN.
//
// The turn is named rather than taken as "whichever alert arrives" because
// every session here begins with a warm-up turn that also crosses the lowered
// threshold, so its alert is already standing when a test acts. Matching on the
// turn id is what makes each assertion a statement about the turn the test
// actually ran.
func awaitExpensiveTurn(t *testing.T, s *keepAliveSession, turnID string) *frontendv1.ContextCostAlert {
	t.Helper()
	var alert *frontendv1.ContextCostAlert
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a ProgressView carrying the expensive_turn alert for " + turnID: func(frame *frontendv1.FrontendFrame) bool {
			view := progressFor(frame, s.cwd)
			if view.GetExpensiveTurn().GetTurnId() != turnID {
				return false
			}
			alert = view.GetExpensiveTurn()
			return true
		},
	})
	return alert
}

// --- (11) the alert ---------------------------------------------------------------

// TestE2EAnExpensiveTurnRaisesTheContextCostAlert covers the SIGNAL ITSELF, and
// the join that makes it actionable: the alert names the turn that crossed the
// threshold, so the user can see WHICH prompt re-ingested the context.
func TestE2EAnExpensiveTurnRaisesTheContextCostAlert(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, alertingPolicy())

	// Act
	s.runRealTurn(t, "r-expensive", "an expensive turn")

	// Assert — the alert names the turn that crossed the threshold, which is
	// the join awaitExpensiveTurn waits on.
	if alert := awaitExpensiveTurn(t, s, "r-expensive"); alert.GetUncachedInputTokens() == 0 {
		t.Errorf("the alert for r-expensive carries uncached_input_tokens = 0: an alarm with no measurement behind it is not actionable")
	}
}

// TestE2ETheCostAlertMeasuresInputPlusCacheCreation covers THE FORMULA, as an
// exact number. The engine's usage is known, so both ways of getting this wrong
// are caught: input_tokens alone (7) is the silent-alarm bug the field comment
// names, and adding the cache READ (80) would alarm on every cache-efficient
// turn in existence.
func TestE2ETheCostAlertMeasuresInputPlusCacheCreation(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, alertingPolicy())

	// Act
	s.runRealTurn(t, "r-measure", "a turn to measure")

	// Assert
	alert := awaitExpensiveTurn(t, s, "r-measure")
	switch got := alert.GetUncachedInputTokens(); got {
	case fakeTurnUncachedTokens:
		// input_tokens + cache_creation_input_tokens: correct.
	case fakeTurnInputTokens:
		t.Errorf("uncached_input_tokens = %d, want %d: measuring input_tokens ALONE is the silent alarm — the vendor marks nearly all input cacheable, so a cold prompt shows up as cache creation and this number stays near zero on a full re-ingest",
			got, fakeTurnUncachedTokens)
	case fakeTurnUncachedTokens + fakeTurnCacheReadTokens:
		t.Errorf("uncached_input_tokens = %d, want %d: the cache READ is input that cost nothing to re-ingest, and counting it alarms on exactly the cache-efficient turns this signal exists to distinguish",
			got, fakeTurnUncachedTokens)
	default:
		t.Errorf("uncached_input_tokens = %d, want input_tokens + cache_creation_input_tokens = %d", got, fakeTurnUncachedTokens)
	}
}

// TestE2ETheCostAlertCarriesTheThresholdThatTripped covers SELF-DESCRIPTION:
// the rendering says "N over M" without the frontend knowing any daemon
// configuration, so the number it compares against travels with the alert.
func TestE2ETheCostAlertCarriesTheThresholdThatTripped(t *testing.T) {
	// Arrange
	policy := alertingPolicy()
	s := newKeepAliveSession(t, policy)

	// Act
	s.runRealTurn(t, "r-threshold", "a turn over the threshold")

	// Assert
	if got := awaitExpensiveTurn(t, s, "r-threshold").GetThresholdTokens(); got != policy.costAlert {
		t.Errorf("expensive_turn threshold_tokens = %d, want the configured threshold %d", got, policy.costAlert)
	}
}

// TestE2ETheCostAlertIsStampedWithWhenTheUsageArrived covers the other field a
// rendering needs: an alert with no time cannot be told from a stale one the
// footer never cleared.
func TestE2ETheCostAlertIsStampedWithWhenTheUsageArrived(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, alertingPolicy())

	// Act
	s.runRealTurn(t, "r-stamp", "a turn to stamp")

	// Assert
	if got := awaitExpensiveTurn(t, s, "r-stamp").GetAtMs(); got == 0 {
		t.Error("expensive_turn at_ms = 0: an alert with no time cannot be distinguished from a stale one")
	}
}

// TestE2EACacheEfficientTurnRaisesNoAlert covers ABSENCE AS A READING. Unset
// means the last turn was cache-efficient, and it is the only reading of unset
// — so an alert on a turn well under the threshold would make the signal
// meaningless.
func TestE2EACacheEfficientTurnRaisesNoAlert(t *testing.T) {
	// Arrange — the DOCUMENTED threshold, which the engine's usage is nowhere
	// near.
	s := newKeepAliveSession(t, testKeepAlivePolicy())

	// Act
	writeCmd(t, s.conn, `{"requestId":"r-cheap","submitPrompt":{"text":"a cheap turn","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — read to the turn's own end, rejecting any alert on the way.
	reject := func(frame *frontendv1.FrontendFrame) string {
		if alert := progressFor(frame, s.cwd).GetExpensiveTurn(); alert != nil {
			return "a cache-efficient turn raised an expensive_turn alert: " + alert.String()
		}
		return ""
	}
	awaitAll(t, s.conn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the cheap turn's result item": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, s.cwd) {
				if isResult(item) {
					return true
				}
			}
			return false
		},
	})
}

// TestE2ETheCostAlertClearsAtTheNextTurnStart covers ITS LIFETIME: like the
// failure card it sits beside, the alert persists until the next turn STARTS —
// not on a timer, and not until the next turn ends. A user who has already
// started their next prompt should not still be reading about the last one.
func TestE2ETheCostAlertClearsAtTheNextTurnStart(t *testing.T) {
	// Arrange — an alert standing.
	s := newKeepAliveSession(t, alertingPolicy())
	s.runRealTurn(t, "r-first", "the expensive first turn")
	awaitExpensiveTurn(t, s, "r-first")

	// Act
	writeCmd(t, s.conn, `{"requestId":"r-second","submitPrompt":{"text":"the next turn","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — a view belonging to the NEW turn (its clock is open) with the
	// alert gone. The open clock anchors this to the new turn, so a stale
	// pre-submit view cannot masquerade as the cleared one.
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a ProgressView for the NEW turn with the cost alert cleared": func(frame *frontendv1.FrontendFrame) bool {
			view := progressFor(frame, s.cwd)
			return view.GetTurnStartedAtMs() != 0 && view.GetExpensiveTurn() == nil
		},
	})
}

// --- the cold-ping alarm ------------------------------------------------------------

// TestE2EAColdKeepAlivePingRaisesTheAlertUnderItsOwnOrigin covers THE ALARM THE
// KEEP-ALIVE EXISTS TO MAKE UNNECESSARY. A ping that comes back having paid
// full freight means the cache had already expired: the window was mistimed, or
// the machine slept between the check and the answer. Carrying the origin
// verbatim is what lets a frontend render that as its own message rather than
// as a generic "that prompt was expensive" note about a prompt the user never
// wrote.
func TestE2EAColdKeepAlivePingRaisesTheAlertUnderItsOwnOrigin(t *testing.T) {
	// Arrange
	policy := alertingPolicy()
	s := newKeepAliveSession(t, policy)

	// Act — the check fires, and the ping's own usage crosses the threshold.
	s.idleFor(t, policy.pingAt())
	ping := s.store.await(t, "the keep-alive ping", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})

	// Assert
	alert := awaitExpensiveTurn(t, s, keepAlivePing(ping).GetTurnId())
	if got := alert.GetPromptOrigin(); got != corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE {
		t.Errorf("expensive_turn prompt_origin = %s, want PROMPT_ORIGIN_CACHE_KEEP_ALIVE: this alert IS the cold-ping alarm, and a frontend cannot tell it apart from an expensive user prompt without the origin", got)
	}
}

// TestE2EAnOrdinaryExpensiveTurnIsAttributedToTheUser covers the OTHER SIDE of
// the same field: the origin is carried verbatim, so a user's own expensive
// prompt is not rendered as a cold-ping alarm about daemon plumbing.
func TestE2EAnOrdinaryExpensiveTurnIsAttributedToTheUser(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, alertingPolicy())

	// Act
	s.runRealTurn(t, "r-user-expensive", "the user's own expensive turn")

	// Assert
	if got := awaitExpensiveTurn(t, s, "r-user-expensive").GetPromptOrigin(); got != corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT {
		t.Errorf("expensive_turn prompt_origin = %s, want PROMPT_ORIGIN_USER_SENT: the attribution is the turn's own, verbatim", got)
	}
}

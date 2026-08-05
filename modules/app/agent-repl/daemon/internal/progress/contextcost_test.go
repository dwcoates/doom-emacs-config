package progress

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// costHarness is a harness whose Manager carries an exact alert threshold.
type costHarness struct {
	*harness
	threshold int64
}

func newCostHarness(t *testing.T, threshold int64) *costHarness {
	t.Helper()
	h := newHarnessWithOptions(t, Options{
		Logf:                func(string, ...any) {},
		CoalesceWindow:      -1,
		UncachedAlertTokens: threshold,
	})
	return &costHarness{harness: h, threshold: threshold}
}

// startTurn opens a turn with an id and an origin, then drops the push.
func (h *costHarness) startTurn(turnID string, origin corev1.PromptOrigin) {
	h.t.Helper()
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{
			TurnId: turnID, PromptOrigin: origin,
		}},
	})
	h.drain()
}

// result folds a terminal ResultMessage carrying the given usage.
func (h *costHarness) result(input, cacheCreation int64) {
	h.t.Helper()
	h.apply(streamEvent(h.t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{
			Usage: &datav1.Usage{InputTokens: input, CacheCreationInputTokens: cacheCreation},
		}},
	}))
}

// alert reads the current expensive-turn alert, failing when there is none.
func (h *costHarness) alert() *frontendv1.ContextCostAlert {
	h.t.Helper()
	view, ok := h.m.Current(testWS)
	if !ok {
		h.t.Fatal("no progress view for the workspace")
	}
	got := view.GetExpensiveTurn()
	if got == nil {
		h.t.Fatal("no expensive-turn alert was raised")
	}
	return got
}

// THE MEASURE IS input_tokens PLUS cache_creation_input_tokens. The CLI marks
// nearly all input cacheable, so a full context re-ingest — the most expensive
// thing that can happen — surfaces as cache CREATION while raw input_tokens
// stays near zero. Alerting on input_tokens alone would stay silent for exactly
// the case the alert exists to catch.
func TestExpensiveTurnCountsCacheCreationAsUncached(t *testing.T) {
	// Arrange.
	h := newCostHarness(t, 1_000)
	h.startTurn("t1", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Act: raw input alone is trivial; the re-ingest is all cache creation.
	h.result(12, 50_000)

	// Assert.
	if got := h.alert().GetUncachedInputTokens(); got != 50_012 {
		t.Fatalf("uncached = %d, want 50012 (input + cache creation)", got)
	}
}

// A turn at or under the threshold raises nothing: absence means the turn was
// cache-efficient, which is the only reading of an unset field.
func TestExpensiveTurnSilentBelowTheThreshold(t *testing.T) {
	// Arrange.
	h := newCostHarness(t, 20_000)
	h.startTurn("t1", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Act.
	h.result(100, 900)

	// Assert.
	view, _ := h.m.Current(testWS)
	if view.GetExpensiveTurn() != nil {
		t.Fatalf("expensive-turn alert = %+v for a cheap turn, want none", view.GetExpensiveTurn())
	}
}

// THE COLD-PING VARIANT. The ping is a dozen tokens of prompt; if it came back
// having paid for the whole conversation, the cache it was sent to refresh had
// already expired. The origin is carried verbatim so a frontend renders that as
// its own message rather than a generic expensive-turn note.
func TestExpensiveTurnCarriesTheKeepAliveOrigin(t *testing.T) {
	// Arrange.
	h := newCostHarness(t, 1_000)
	h.startTurn("ka_1", corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE)

	// Act.
	h.result(0, 80_000)

	// Assert.
	if got := h.alert().GetPromptOrigin(); got != corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE {
		t.Fatalf("prompt_origin = %s, want CACHE_KEEP_ALIVE; the cold-ping alarm IS the origin", got)
	}
}

// The alert names the turn it is about, so a frontend can join it to that
// turn's own bubble rather than to whatever happens to be on screen.
func TestExpensiveTurnNamesItsTurn(t *testing.T) {
	// Arrange.
	h := newCostHarness(t, 1_000)
	h.startTurn("t-42", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Act.
	h.result(0, 9_000)

	// Assert.
	if got := h.alert().GetTurnId(); got != "t-42" {
		t.Fatalf("turn_id = %q, want t-42", got)
	}
}

// The threshold rides along so a rendering can say "N over M" without knowing
// daemon config.
func TestExpensiveTurnCarriesTheThreshold(t *testing.T) {
	// Arrange.
	h := newCostHarness(t, 1_234)
	h.startTurn("t1", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Act.
	h.result(0, 9_000)

	// Assert.
	if got := h.alert().GetThresholdTokens(); got != 1_234 {
		t.Fatalf("threshold_tokens = %d, want 1234", got)
	}
}

// THE ALERT PERSISTS UNTIL THE NEXT TURN STARTS, like the failure field: it
// reports what the turn that just finished cost, and a new turn beginning is
// the moment that report stops being the news.
func TestExpensiveTurnClearsOnTheNextTurnStart(t *testing.T) {
	// Arrange.
	h := newCostHarness(t, 1_000)
	h.startTurn("t1", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)
	h.result(0, 9_000)
	h.alert()
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t1"}},
	})

	// Act.
	h.startTurn("t2", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	view, _ := h.m.Current(testWS)
	if view.GetExpensiveTurn() != nil {
		t.Fatalf("expensive-turn alert = %+v after the next turn started, want it cleared", view.GetExpensiveTurn())
	}
}

// A result with no usage raises nothing rather than alerting on a zero.
func TestExpensiveTurnIgnoresAResultWithNoUsage(t *testing.T) {
	// Arrange.
	h := newCostHarness(t, 1_000)
	h.startTurn("t1", corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Act.
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{}},
	}))

	// Assert.
	view, _ := h.m.Current(testWS)
	if view.GetExpensiveTurn() != nil {
		t.Fatalf("expensive-turn alert = %+v for a usage-less result, want none", view.GetExpensiveTurn())
	}
}

// --- cache-age evidence on the alert's log line ------------------------------

// loggedCostHarness is a cost harness that keeps every daemon log line, so the
// evidence the alert prints can be asserted rather than merely compiled.
type loggedCostHarness struct {
	*costHarness
	lines *[]string
}

func newLoggedCostHarness(t *testing.T, threshold int64) *loggedCostHarness {
	t.Helper()
	lines := &[]string{}
	h := newHarnessWithOptions(t, Options{
		Logf:                func(format string, args ...any) { *lines = append(*lines, fmt.Sprintf(format, args...)) },
		CoalesceWindow:      -1,
		UncachedAlertTokens: threshold,
	})
	return &loggedCostHarness{costHarness: &costHarness{harness: h, threshold: threshold}, lines: lines}
}

// alertLine returns the single expensive-turn or cold-keep-alive log line.
func (h *loggedCostHarness) alertLine() string {
	h.t.Helper()
	var found []string
	for _, line := range *h.lines {
		if strings.Contains(line, "EXPENSIVE TURN") || strings.Contains(line, "CAME BACK COLD") {
			found = append(found, line)
		}
	}
	if len(found) != 1 {
		h.t.Fatalf("expensive-turn log lines = %d, want exactly 1: %v", len(found), found)
	}
	return found[0]
}

// startTurnAt opens a turn stamped at the given producer time.
func (h *loggedCostHarness) startTurnAt(turnID string, at int64) {
	h.t.Helper()
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: at,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{
			TurnId: turnID, PromptOrigin: corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		}},
	})
	h.drain()
}

// endTurnAt closes a turn stamped at the given producer time.
func (h *loggedCostHarness) endTurnAt(turnID string, at int64) {
	h.t.Helper()
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: at,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: turnID}},
	})
	h.drain()
}

// resultAt folds a terminal result stamped at the given producer time.
func (h *loggedCostHarness) resultAt(cacheCreation, at int64) {
	h.t.Helper()
	ev := streamEvent(h.t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{
			Usage: &datav1.Usage{CacheCreationInputTokens: cacheCreation},
		}},
	})
	ev.ProducedAtMs = at
	h.apply(ev)
}

// A first alert with no prior turn in this process is the slack-ceac-tech-ptn
// case: a session revived from hibernation re-ingests everything, and the log
// used to name the cost without naming that cause.
func TestExpensiveTurnEvidenceNamesTheAbsentPriorTurn(t *testing.T) {
	// Arrange: no turn has ever ended in this manager.
	h := newLoggedCostHarness(t, 20_000)
	h.startTurnAt("t1", atMs)

	// Act.
	h.resultAt(144_345, atMs)

	// Assert.
	if got := h.alertLine(); !strings.Contains(got, "prior_turn=none") {
		t.Fatalf("alert line = %q, want it to report prior_turn=none", got)
	}
}

// A gap past the longest cache lifetime makes the miss arithmetic, and the line
// says so rather than leaving the reader to compare the figures.
func TestExpensiveTurnEvidenceCallsAnOverTtlGapCertain(t *testing.T) {
	// Arrange: a prior turn ended two hours before this one's result.
	h := newLoggedCostHarness(t, 20_000)
	h.startTurnAt("t1", atMs)
	h.endTurnAt("t1", atMs)
	h.startTurnAt("t2", atMs+2*longestCacheTtlMs)

	// Act.
	h.resultAt(144_345, atMs+2*longestCacheTtlMs)

	// Assert.
	if got := h.alertLine(); !strings.Contains(got, "outlived the longest cache lifetime") {
		t.Fatalf("alert line = %q, want it to call an over-TTL gap certain", got)
	}
}

// A gap INSIDE the cache lifetime rules idle time out, which points the reader
// at a changed prompt prefix instead of at the clock.
func TestExpensiveTurnEvidenceRulesOutIdleTimeInsideTheTtl(t *testing.T) {
	// Arrange: a prior turn ended one minute before this one's result.
	h := newLoggedCostHarness(t, 20_000)
	h.startTurnAt("t1", atMs)
	h.endTurnAt("t1", atMs)
	h.startTurnAt("t2", atMs+60_000)

	// Act.
	h.resultAt(144_345, atMs+60_000)

	// Assert.
	if got := h.alertLine(); !strings.Contains(got, "INSIDE the longest cache lifetime") {
		t.Fatalf("alert line = %q, want it to rule idle time out inside the TTL", got)
	}
}

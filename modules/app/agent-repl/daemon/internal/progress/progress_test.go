package progress

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

const (
	testWS  = "ws-alpha"
	testSID = "sess-1"
	// atMs is the producer stamp every fixture event carries, so a window's
	// since_ms and the turn clock are assertable exact values.
	atMs int64 = 1_700_000_000_000
)

// --- harness ----------------------------------------------------------------

// fakeSched captures the deferred flush instead of running it on a real clock,
// so the coalescing window is driven deterministically (no wall-clock waits).
type fakeSched struct {
	pending func()
	armed   int
	stopped int
}

func (f *fakeSched) AfterFunc(_ time.Duration, fn func()) func() bool {
	f.pending = fn
	f.armed++
	return func() bool {
		f.stopped++
		f.pending = nil
		return true
	}
}

// fire runs the captured flush, if one is armed.
func (f *fakeSched) fire() {
	fn := f.pending
	f.pending = nil
	if fn != nil {
		fn()
	}
}

// harness is a Manager plus its subscription, with the coalescing window
// DISABLED by default: most cases assert the fold, not the batching, and want
// one push per change.
type harness struct {
	t     *testing.T
	m     *Manager
	ch    <-chan *frontendv1.ProgressView
	sched *fakeSched
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	return newHarnessWindow(t, -1)
}

func newHarnessWindow(t *testing.T, window time.Duration) *harness {
	t.Helper()
	sched := &fakeSched{}
	m := New(Options{
		Logf:           func(string, ...any) {},
		Clock:          func() int64 { return atMs },
		Sched:          sched,
		CoalesceWindow: window,
	})
	ch, cancel := m.Subscribe()
	t.Cleanup(func() {
		cancel()
		_ = m.Close()
	})
	return &harness{t: t, m: m, ch: ch, sched: sched}
}

// drain returns every buffered push, without blocking.
func (h *harness) drain() []*frontendv1.ProgressView {
	h.t.Helper()
	var out []*frontendv1.ProgressView
	for {
		select {
		case v := <-h.ch:
			out = append(out, v)
		default:
			return out
		}
	}
}

// last returns the newest buffered push, failing when there was none.
func (h *harness) last() *frontendv1.ProgressView {
	h.t.Helper()
	got := h.drain()
	if len(got) == 0 {
		h.t.Fatal("wanted a pushed ProgressView, got none")
	}
	return got[len(got)-1]
}

// apply folds an event, failing on error.
func (h *harness) apply(ev *corev1.Event) {
	h.t.Helper()
	if err := h.m.Apply(testWS, ev); err != nil {
		h.t.Fatalf("Apply: %v", err)
	}
}

// openTurn puts the workspace mid-turn, then drops the resulting push so a
// case asserts only what it exercises.
func (h *harness) openTurn() {
	h.t.Helper()
	h.m.NoteTurnAccepted(testWS, testSID)
	h.drain()
}

// --- fixture builders -------------------------------------------------------

func mustAny(t *testing.T, m proto.Message) *anypb.Any {
	t.Helper()
	a, err := anypb.New(m)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return a
}

// vendorEvent wraps a data.v1 message as the vendor payload of a store event.
func vendorEvent(t *testing.T, m proto.Message) *corev1.Event {
	t.Helper()
	return &corev1.Event{
		SessionId:    testSID,
		ProducedAtMs: atMs,
		Payload:      &corev1.Event_Vendor{Vendor: mustAny(t, m)},
	}
}

// streamEvent wraps a ClaudeStreamMessage arm as a vendor store event.
func streamEvent(t *testing.T, msg *datav1.ClaudeStreamMessage) *corev1.Event {
	t.Helper()
	return vendorEvent(t, msg)
}

// assistantWithUsage builds a stream-plane assistant message carrying usage.
func assistantWithUsage(uuid string, u *datav1.ApiUsage) *datav1.ClaudeStreamMessage {
	return &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{
		Assistant: &datav1.AssistantMessage{
			Uuid:    uuid,
			Message: &datav1.ApiAssistantMessage{Usage: u},
		},
	}}
}

// latencyEvent builds the EPHEMERAL first-token-latency relay the shim's delta
// bypass emits off a message_start's ttft stamp. Ephemeral, so it carries no
// seq.
func latencyEvent(uuid string, ttftMs int64) *corev1.Event {
	return &corev1.Event{
		SessionId:    testSID,
		Class:        corev1.EventClass_EVENT_CLASS_EPHEMERAL,
		ProducedAtMs: atMs,
		Payload: &corev1.Event_MessageLatency{
			MessageLatency: &corev1.MessageLatency{Uuid: uuid, TtftMs: ttftMs},
		},
	}
}

// apiErrorLine builds the on-disk system line carrying an API error/retry.
func apiErrorLine(uuid, message string, attempt, max int64) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_System{
		System: &datav1.SystemLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Subtype: &datav1.SystemLine_ApiError{ApiError: &datav1.ApiErrorLine{
				Level:        "error",
				Error:        &datav1.ApiErrorDetail{Message: message},
				RetryAttempt: attempt,
				MaxRetries:   max,
			}},
		},
	}}
}

// --- ObserveWorkspaceState: the SSM seam ------------------------------------

func TestObserveWorkspaceStateMirrorsPhase(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if err := h.m.ObserveWorkspaceState(&frontendv1.WorkspaceState{
		Workspace: testWS, SessionId: testSID,
		State: frontendv1.RenderState_RENDER_STATE_THINKING,
	}); err != nil {
		t.Fatalf("ObserveWorkspaceState: %v", err)
	}
	// Assert
	if got := h.last().GetState(); got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %v, want THINKING", got)
	}
}

func TestObserveWorkspaceStateAdoptsLiveTaskCount(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if err := h.m.ObserveWorkspaceState(&frontendv1.WorkspaceState{
		Workspace: testWS, LiveTaskCount: 3,
	}); err != nil {
		t.Fatalf("ObserveWorkspaceState: %v", err)
	}
	// Assert
	if got := h.last().GetLiveTaskCount(); got != 3 {
		t.Fatalf("liveTaskCount = %d, want 3", got)
	}
}

func TestObserveWorkspaceStateRejectsNil(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	err := h.m.ObserveWorkspaceState(nil)
	// Assert
	if err == nil {
		t.Fatal("wanted an error for a nil WorkspaceState, got nil")
	}
}

func TestObserveWorkspaceStateRejectsEmptyWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	err := h.m.ObserveWorkspaceState(&frontendv1.WorkspaceState{SessionId: testSID})
	// Assert
	if err == nil {
		t.Fatal("wanted an error for a state with no workspace, got nil")
	}
}

// --- the turn clock ---------------------------------------------------------

func TestNoteTurnAcceptedStartsTheTurnClock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.m.NoteTurnAccepted(testWS, testSID)
	// Assert
	if got := h.last().GetTurnStartedAtMs(); got != atMs {
		t.Fatalf("turnStartedAtMs = %d, want %d", got, atMs)
	}
}

func TestNoteTurnAcceptedIsIdempotentMidTurn(t *testing.T) {
	// Arrange — a turn is running and has banked some input usage.
	h := newHarness(t)
	h.openTurn()
	h.apply(streamEvent(t, assistantWithUsage("m1", &datav1.ApiUsage{InputTokens: 500})))
	h.drain()
	// Act — a second accept (a queued prompt's, say) must not restart the turn.
	h.m.NoteTurnAccepted(testWS, testSID)
	// Assert
	if pushes := h.drain(); len(pushes) != 0 {
		t.Fatalf("wanted no push from a redundant accept, got %d", len(pushes))
	}
	cur, _ := h.m.Current(testWS)
	if got := cur.GetInputTokens(); got != 500 {
		t.Fatalf("inputTokens = %d, want the accumulated 500", got)
	}
}

func TestTurnStartedEventStartsTheTurnClock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}},
	})
	// Assert
	if got := h.last().GetTurnStartedAtMs(); got != atMs {
		t.Fatalf("turnStartedAtMs = %d, want %d", got, atMs)
	}
}

func TestTurnEndedStopsTheTurnClock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.openTurn()
	// Act
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{}},
	})
	// Assert
	if got := h.last().GetTurnStartedAtMs(); got != 0 {
		t.Fatalf("turnStartedAtMs = %d, want 0 off-turn", got)
	}
}

func TestTurnEndedWithErrorSetsTheSummary(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.openTurn()
	// Act
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{
			IsError: true, StopReason: "max_tokens",
		}},
	})
	// Assert
	if got := h.last().GetErrorSummary(); got != "turn ended in error: max_tokens" {
		t.Fatalf("errorSummary = %q, want the stop reason", got)
	}
}

func TestTurnStartClearsThePreviousError(t *testing.T) {
	// Arrange — an error from the turn that just failed.
	h := newHarness(t)
	h.openTurn()
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{IsError: true}},
	})
	h.drain()
	// Act — the NEXT turn starting is what clears it (design: it persists until then).
	h.m.NoteTurnAccepted(testWS, testSID)
	// Assert
	if got := h.last().GetErrorSummary(); got != "" {
		t.Fatalf("errorSummary = %q, want cleared by the next turn start", got)
	}
}

// --- the tickers ------------------------------------------------------------

func TestThinkingTokensTicks(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_ThinkingTokens{
			ThinkingTokens: &datav1.ThinkingTokens{EstimatedTokens: 1400},
		},
	}))
	// Assert
	if got := h.last().GetThinkingTokens(); got != 1400 {
		t.Fatalf("thinkingTokens = %d, want 1400", got)
	}
}

func TestInputTokensSumTheCacheHalves(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.openTurn()
	// Act
	h.apply(streamEvent(t, assistantWithUsage("m1", &datav1.ApiUsage{
		InputTokens: 100, CacheReadInputTokens: 40_000, CacheCreationInputTokens: 1_100,
		OutputTokens: 9_999, // deliberately ignored: the footer shows input only
	})))
	// Assert
	if got := h.last().GetInputTokens(); got != 41_200 {
		t.Fatalf("inputTokens = %d, want 41200 (input + both cache halves, no output)", got)
	}
}

func TestInputTokensExcludeOutputTokens(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.openTurn()
	// Act — a usage frame with ONLY output tokens contributes nothing.
	h.apply(streamEvent(t, assistantWithUsage("m1", &datav1.ApiUsage{OutputTokens: 5_000})))
	// Assert
	cur, _ := h.m.Current(testWS)
	if got := cur.GetInputTokens(); got != 0 {
		t.Fatalf("inputTokens = %d, want 0 for an output-only usage frame", got)
	}
}

func TestInputTokensAccumulateAcrossRequests(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.openTurn()
	// Act
	h.apply(streamEvent(t, assistantWithUsage("m1", &datav1.ApiUsage{InputTokens: 1_000})))
	h.apply(streamEvent(t, assistantWithUsage("m2", &datav1.ApiUsage{InputTokens: 2_500})))
	// Assert
	if got := h.last().GetInputTokens(); got != 3_500 {
		t.Fatalf("inputTokens = %d, want 3500 accumulated across the turn", got)
	}
}

func TestInputTokensDedupeAcrossObservationPlanes(t *testing.T) {
	// Arrange — the SAME message arrives on the stream plane and the disk plane.
	h := newHarness(t)
	h.openTurn()
	usage := &datav1.ApiUsage{InputTokens: 1_000}
	h.apply(streamEvent(t, assistantWithUsage("dup", usage)))
	// Act — the transcript twin of the very same uuid.
	h.apply(vendorEvent(t, &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{
		Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{Uuid: "dup"},
			Message:  &datav1.ApiAssistantMessage{Usage: usage},
		},
	}}))
	// Assert
	cur, _ := h.m.Current(testWS)
	if got := cur.GetInputTokens(); got != 1_000 {
		t.Fatalf("inputTokens = %d, want 1000 (the twin plane must not double-count)", got)
	}
}

func TestInputTokensResetAtTurnStart(t *testing.T) {
	// Arrange — a turn that spent tokens, then ended.
	h := newHarness(t)
	h.openTurn()
	h.apply(streamEvent(t, assistantWithUsage("m1", &datav1.ApiUsage{InputTokens: 9_000})))
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{}},
	})
	h.drain()
	// Act
	h.m.NoteTurnAccepted(testWS, testSID)
	// Assert
	if got := h.last().GetInputTokens(); got != 0 {
		t.Fatalf("inputTokens = %d, want 0 at the start of a fresh turn", got)
	}
}

func TestInputTokensIgnoredOffTurn(t *testing.T) {
	// Arrange — no turn open.
	h := newHarness(t)
	// Act
	h.apply(streamEvent(t, assistantWithUsage("m1", &datav1.ApiUsage{InputTokens: 700})))
	// Assert — the figure is turn-scoped, so off-turn usage belongs to no turn.
	cur, _ := h.m.Current(testWS)
	if got := cur.GetInputTokens(); got != 0 {
		t.Fatalf("inputTokens = %d, want 0 for usage observed off-turn", got)
	}
}

// --- the ttft relay ---------------------------------------------------------
//
// The shim's delta bypass relays the vendor's mid-stream first-token stamp as
// an EPHEMERAL MessageLatency. This resolver is its only consumer: the same
// number reaches the daemon again on the turn's terminal result, but only once
// the turn is over.

func TestMessageLatencyFeedsTtft(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.openTurn()
	// Act
	h.apply(latencyEvent("msg_01ABC", 865))
	// Assert
	if got := h.last().GetTtftMs(); got != 865 {
		t.Fatalf("ttftMs = %d, want 865", got)
	}
}

func TestMessageLatencyLatestMessageWins(t *testing.T) {
	// Arrange — two API requests in one turn, each with its own stamp.
	h := newHarness(t)
	h.openTurn()
	h.apply(latencyEvent("msg_FIRST", 865))
	// Act
	h.apply(latencyEvent("msg_SECOND", 412))
	// Assert — the field reports the CURRENT message, not the turn's first.
	if got := h.last().GetTtftMs(); got != 412 {
		t.Fatalf("ttftMs = %d, want 412 from the newer message", got)
	}
}

func TestMessageLatencyResetsAtTurnStart(t *testing.T) {
	// Arrange — a turn that measured a latency, then ended.
	h := newHarness(t)
	h.openTurn()
	h.apply(latencyEvent("msg_01ABC", 865))
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{}},
	})
	h.drain()
	// Act
	h.m.NoteTurnAccepted(testWS, testSID)
	// Assert
	if got := h.last().GetTtftMs(); got != 0 {
		t.Fatalf("ttftMs = %d, want 0 at the start of a fresh turn", got)
	}
}

func TestMessageLatencyIsStructuralNotCoalesced(t *testing.T) {
	// Arrange — coalescing ON, so a ticker would be held back.
	h := newHarnessWindow(t, DefaultCoalesceWindow)
	h.openTurn()
	// Act
	h.apply(latencyEvent("msg_01ABC", 865))
	// Assert — latency moves at most once per message, so it goes out at once.
	if got := h.last().GetTtftMs(); got != 865 {
		t.Fatalf("ttftMs = %d, want 865 pushed without waiting on the window", got)
	}
}

func TestRepeatedMessageLatencyIsQuiet(t *testing.T) {
	// Arrange — the same stamp observed twice.
	h := newHarness(t)
	h.openTurn()
	h.apply(latencyEvent("msg_01ABC", 865))
	h.drain()
	// Act
	h.apply(latencyEvent("msg_01ABC", 865))
	// Assert — no movement, no frame.
	if got := h.drain(); len(got) != 0 {
		t.Fatalf("pushes = %d, want 0 for an unchanged latency", len(got))
	}
}

func TestZeroMessageLatencyLeavesTheFigureStanding(t *testing.T) {
	// Arrange — absence-tolerance: a stampless relay must not blank a real one.
	h := newHarness(t)
	h.openTurn()
	h.apply(latencyEvent("msg_01ABC", 865))
	h.drain()
	// Act
	h.apply(latencyEvent("msg_SECOND", 0))
	// Assert
	cur, _ := h.m.Current(testWS)
	if got := cur.GetTtftMs(); got != 865 {
		t.Fatalf("ttftMs = %d, want the standing 865 preserved", got)
	}
}

// --- activity windows -------------------------------------------------------

func TestCompactingStatusOpensItsWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Status{Status: &datav1.StatusMessage{Status: "compacting"}},
	}))
	// Assert
	w := h.last().GetCompacting()
	if !w.GetActive() || w.GetSinceMs() != atMs {
		t.Fatalf("compacting window = %+v, want active since %d", w, atMs)
	}
}

func TestNullStatusClosesTheCompactingWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Status{Status: &datav1.StatusMessage{Status: "compacting"}},
	}))
	h.drain()
	// Act — an empty status is the vendor's null, i.e. the window closing.
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Status{Status: &datav1.StatusMessage{Status: ""}},
	}))
	// Assert
	if w := h.last().GetCompacting(); w.GetActive() {
		t.Fatalf("compacting window = %+v, want closed", w)
	}
}

func TestWindowSinceMsSurvivesADetailRefresh(t *testing.T) {
	// Arrange — a retry window opened at atMs.
	h := newHarness(t)
	h.apply(vendorEvent(t, apiErrorLine("e1", "overloaded", 1, 10)))
	h.drain()
	// Act — a later attempt refreshes the detail at a LATER stamp.
	later := atMs + 5_000
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: later,
		Payload: &corev1.Event_Vendor{Vendor: mustAny(t, apiErrorLine("e2", "overloaded", 2, 10))},
	})
	// Assert — the window's age counts from when it OPENED.
	if got := h.last().GetRetrying().GetSinceMs(); got != atMs {
		t.Fatalf("retrying.sinceMs = %d, want the original open stamp %d", got, atMs)
	}
}

func TestHookStartedOpensTheHookWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_HookStarted{
			HookStarted: &datav1.HookStarted{HookName: "SessionStart:startup"},
		},
	}))
	// Assert
	w := h.last().GetHook()
	if !w.GetActive() || w.GetDetail() != "SessionStart:startup" {
		t.Fatalf("hook window = %+v, want active named by the hook", w)
	}
}

func TestHookResponseClosesTheHookWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_HookStarted{HookStarted: &datav1.HookStarted{HookName: "h"}},
	}))
	h.drain()
	// Act
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_HookResponse{HookResponse: &datav1.HookResponse{HookName: "h"}},
	}))
	// Assert
	if w := h.last().GetHook(); w.GetActive() {
		t.Fatalf("hook window = %+v, want closed by the response", w)
	}
}

func TestAuthStatusOpensTheAuthWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_AuthStatus{AuthStatus: &datav1.AuthStatus{
			IsAuthenticating: true, Output: []string{"first", "paste your code"},
		}},
	}))
	// Assert
	w := h.last().GetAuthenticating()
	if !w.GetActive() || w.GetDetail() != "paste your code" {
		t.Fatalf("auth window = %+v, want active showing the newest output line", w)
	}
}

func TestRateLimitWarningOpensItsWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_RateLimitEvent{RateLimitEvent: &datav1.RateLimitEvent{
			RateLimitInfo: &datav1.RateLimitInfo{
				Status: "allowed_warning", ResetsAt: 1_700_000_900, Utilization: 0.91,
			},
		}},
	}))
	// Assert
	w := h.last().GetRateLimited()
	if !w.GetActive() || w.GetStatus() != "allowed_warning" || w.GetUtilization() != 0.91 {
		t.Fatalf("rate-limit window = %+v, want the warning carried through", w)
	}
}

func TestPlainAllowedClosesTheRateLimitWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_RateLimitEvent{RateLimitEvent: &datav1.RateLimitEvent{
			RateLimitInfo: &datav1.RateLimitInfo{Status: "allowed_warning"},
		}},
	}))
	h.drain()
	// Act — a bare "allowed" is the vendor saying everything is fine again.
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_RateLimitEvent{RateLimitEvent: &datav1.RateLimitEvent{
			RateLimitInfo: &datav1.RateLimitInfo{Status: "allowed"},
		}},
	}))
	// Assert
	if w := h.last().GetRateLimited(); w.GetActive() {
		t.Fatalf("rate-limit window = %+v, want closed", w)
	}
}

// --- the API error family ---------------------------------------------------

func TestRetryableApiErrorOpensTheRetryingWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(vendorEvent(t, apiErrorLine("e1", "overloaded", 3, 10)))
	// Assert
	w := h.last().GetRetrying()
	if !w.GetActive() || w.GetDetail() != "attempt 3/10 · overloaded" {
		t.Fatalf("retrying window = %+v, want the attempt detail", w)
	}
}

func TestRetryableApiErrorReportsNoError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act — mid-backoff: the turn has not failed, so no error row.
	h.apply(vendorEvent(t, apiErrorLine("e1", "overloaded", 3, 10)))
	// Assert
	if got := h.last().GetErrorSummary(); got != "" {
		t.Fatalf("errorSummary = %q, want empty while retries remain", got)
	}
}

func TestExhaustedApiErrorSetsTheErrorSummary(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(vendorEvent(t, apiErrorLine("e9", "overloaded (529)", 10, 10)))
	// Assert
	if got := h.last().GetErrorSummary(); got != "overloaded (529) (after 10 attempts)" {
		t.Fatalf("errorSummary = %q, want the exhausted-retry account", got)
	}
}

func TestExhaustedApiErrorAddressesItsFeedItem(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.apply(vendorEvent(t, apiErrorLine("e9", "boom", 10, 10)))
	// Assert — the footer's error row scrolls the feed to this uuid.
	if got := h.last().GetErrorItemUuid(); got != "e9" {
		t.Fatalf("errorItemUuid = %q, want the error line's own uuid", got)
	}
}

func TestExhaustedApiErrorClosesTheRetryingWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.apply(vendorEvent(t, apiErrorLine("e1", "overloaded", 1, 10)))
	h.drain()
	// Act
	h.apply(vendorEvent(t, apiErrorLine("e9", "overloaded", 10, 10)))
	// Assert
	if w := h.last().GetRetrying(); w.GetActive() {
		t.Fatalf("retrying window = %+v, want closed once retries are exhausted", w)
	}
}

// --- daemon-local counts ----------------------------------------------------

func TestSetCountsCarriesThePendingAndQueuedFigures(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.m.SetCounts(testWS, 2, 5)
	// Assert
	v := h.last()
	if v.GetPendingPermissions() != 2 || v.GetQueueDepth() != 5 {
		t.Fatalf("counts = (%d, %d), want (2, 5)", v.GetPendingPermissions(), v.GetQueueDepth())
	}
}

func TestSetCountsIsQuietWhenNothingMoved(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.m.SetCounts(testWS, 1, 1)
	h.drain()
	// Act
	h.m.SetCounts(testWS, 1, 1)
	// Assert — latest-wins pushes on CHANGE, not on every report.
	if pushes := h.drain(); len(pushes) != 0 {
		t.Fatalf("wanted no push for an unchanged count, got %d", len(pushes))
	}
}

// --- coalescing -------------------------------------------------------------

func TestTickerBurstCoalescesIntoOnePush(t *testing.T) {
	// Arrange — coalescing ON.
	h := newHarnessWindow(t, 120*time.Millisecond)
	// Act — three ticker steps inside one window.
	for _, n := range []int64{100, 200, 300} {
		h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
			Msg: &datav1.ClaudeStreamMessage_ThinkingTokens{
				ThinkingTokens: &datav1.ThinkingTokens{EstimatedTokens: n},
			},
		}))
	}
	if pending := h.drain(); len(pending) != 0 {
		t.Fatalf("wanted nothing pushed before the window fires, got %d", len(pending))
	}
	h.sched.fire()
	// Assert
	got := h.drain()
	if len(got) != 1 {
		t.Fatalf("wanted 1 coalesced push, got %d", len(got))
	}
	if got[0].GetThinkingTokens() != 300 {
		t.Fatalf("thinkingTokens = %d, want the newest value 300", got[0].GetThinkingTokens())
	}
}

func TestTickerBurstArmsTheFlushOnlyOnce(t *testing.T) {
	// Arrange
	h := newHarnessWindow(t, 120*time.Millisecond)
	// Act
	for _, n := range []int64{1, 2, 3, 4} {
		h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
			Msg: &datav1.ClaudeStreamMessage_ThinkingTokens{
				ThinkingTokens: &datav1.ThinkingTokens{EstimatedTokens: n},
			},
		}))
	}
	// Assert — later movement rides the flush already armed.
	if h.sched.armed != 1 {
		t.Fatalf("armed = %d, want exactly 1 deferred flush for the burst", h.sched.armed)
	}
}

func TestStructuralPushFlushesPendingTickerMovement(t *testing.T) {
	// Arrange — a coalesced ticker value waiting on the window.
	h := newHarnessWindow(t, 120*time.Millisecond)
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_ThinkingTokens{
			ThinkingTokens: &datav1.ThinkingTokens{EstimatedTokens: 777},
		},
	}))
	// Act — a structural change goes out at once.
	h.m.SetCounts(testWS, 1, 0)
	// Assert — and takes the pending ticker value with it.
	got := h.drain()
	if len(got) != 1 {
		t.Fatalf("wanted 1 push, got %d", len(got))
	}
	if got[0].GetThinkingTokens() != 777 {
		t.Fatalf("thinkingTokens = %d, want the pending 777 carried out", got[0].GetThinkingTokens())
	}
}

func TestFlushAfterAStructuralPushIsQuiet(t *testing.T) {
	// Arrange
	h := newHarnessWindow(t, 120*time.Millisecond)
	h.apply(streamEvent(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_ThinkingTokens{
			ThinkingTokens: &datav1.ThinkingTokens{EstimatedTokens: 5},
		},
	}))
	h.m.SetCounts(testWS, 1, 0)
	h.drain()
	// Act — the armed flush fires after its movement already went out.
	h.m.Flush()
	// Assert
	if pushes := h.drain(); len(pushes) != 0 {
		t.Fatalf("wanted no duplicate push from the stale flush, got %d", len(pushes))
	}
}

// --- reads, fan-out, and guards --------------------------------------------

func TestSnapshotIsInStableWorkspaceOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	for _, ws := range []string{"zeta", "alpha", "mid"} {
		h.m.SetCounts(ws, 1, 0)
	}
	// Act
	got := h.m.Snapshot()
	// Assert
	want := []string{"alpha", "mid", "zeta"}
	if len(got) != len(want) {
		t.Fatalf("snapshot len = %d, want %d", len(got), len(want))
	}
	for i, ws := range want {
		if got[i].GetWorkspace() != ws {
			t.Fatalf("snapshot[%d] = %q, want %q", i, got[i].GetWorkspace(), ws)
		}
	}
}

func TestCurrentReportsAnExplicitMiss(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, found := h.m.Current("never-seen")
	// Assert
	if found {
		t.Fatal("Current reported a view for an unknown workspace; want an explicit miss")
	}
}

func TestPushedViewIsIsolatedFromLaterMutation(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.m.SetCounts(testWS, 1, 0)
	first := h.last()
	// Act — the resolver moves on.
	h.m.SetCounts(testWS, 9, 0)
	h.drain()
	// Assert — the earlier subscriber's copy is untouched.
	if got := first.GetPendingPermissions(); got != 1 {
		t.Fatalf("earlier push mutated to %d; want the 1 it was pushed with", got)
	}
}

func TestUnsubscribeStopsDelivery(t *testing.T) {
	// Arrange
	m := New(Options{Logf: func(string, ...any) {}, CoalesceWindow: -1})
	t.Cleanup(func() { _ = m.Close() })
	ch, cancel := m.Subscribe()
	// Act
	cancel()
	m.SetCounts(testWS, 1, 0)
	// Assert — the channel is closed, so a receive yields the zero value.
	if v, open := <-ch; open {
		t.Fatalf("received %v on a cancelled subscription; want a closed channel", v)
	}
}

func TestSlowSubscriberIsDroppedNotBlocked(t *testing.T) {
	// Arrange — never read the channel, and overflow its bounded buffer.
	var dropped int
	m := New(Options{
		Logf:           func(string, ...any) { dropped++ },
		CoalesceWindow: -1,
	})
	t.Cleanup(func() { _ = m.Close() })
	_, cancel := m.Subscribe()
	t.Cleanup(cancel)
	// Act — more pushes than the buffer holds. This must not deadlock.
	for i := 0; i < subBufferSize+5; i++ {
		m.SetCounts(testWS, int64(i), 0)
	}
	// Assert
	if dropped == 0 {
		t.Fatal("wanted the overflow loud-logged as a slow-consumer drop, got silence")
	}
}

func TestApplyRejectsANilEvent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	err := h.m.Apply(testWS, nil)
	// Assert
	if err == nil {
		t.Fatal("wanted an error for a nil event, got nil")
	}
}

func TestApplyRejectsAnEmptyWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	err := h.m.Apply("", &corev1.Event{SessionId: testSID})
	// Assert
	if err == nil {
		t.Fatal("wanted an error for an event with no workspace, got nil")
	}
}

func TestApplyRejectsACorruptVendorPayload(t *testing.T) {
	// Arrange — an Any whose type URL names nothing the schema set knows.
	h := newHarness(t)
	// Act
	err := h.m.Apply(testWS, &corev1.Event{
		SessionId: testSID,
		Payload:   &corev1.Event_Vendor{Vendor: &anypb.Any{TypeUrl: "type.googleapis.com/nope.Nope"}},
	})
	// Assert — a genuine anomaly, surfaced rather than swallowed.
	if err == nil {
		t.Fatal("wanted an error for an unresolvable vendor Any, got nil")
	}
}

func TestNonProgressPayloadIsQuiet(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act — task lifecycle reaches the footer as the SSM's live_task_count.
	h.apply(&corev1.Event{
		SessionId: testSID, ProducedAtMs: atMs,
		Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "t1"}},
	})
	// Assert
	if pushes := h.drain(); len(pushes) != 0 {
		t.Fatalf("wanted no push for a non-progress payload, got %d", len(pushes))
	}
}

func TestCloseIsIdempotent(t *testing.T) {
	// Arrange
	m := New(Options{Logf: func(string, ...any) {}})
	// Act
	first, second := m.Close(), m.Close()
	// Assert
	if first != nil || second != nil {
		t.Fatalf("Close errors = (%v, %v), want both nil", first, second)
	}
}

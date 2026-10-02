package footer

import (
	"testing"
	"time"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/shimclient"
)

// ptr is the address of a value, which is how an optional scalar is set.
func ptr[T any](v T) *T { return &v }

func TestAMidTurnApiFailureDrawsTheRetryLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{
		Message: "overloaded",
		Kind: &conversationv1.ApiRequestFailed_Overloaded{
			Overloaded: &conversationv1.ApiOverloaded{},
		},
	})

	// Assert
	retry := h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRetrying()
	if retry.GetAttempt() != 2 || retry.GetStatus() != "overloaded" {
		t.Fatalf("retry = %+v, want attempt 2 with the vendor's summary", retry)
	}
}

func TestASecondApiFailureCountsTheNextAttempt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	failed := &conversationv1.ApiRequestFailed{Message: "overloaded"}
	h.r.OnApiError(testWS, mainAgent, failed)

	// Act
	h.r.OnApiError(testWS, mainAgent, failed)

	// Assert
	retry := h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRetrying()
	if retry.GetAttempt() != 3 {
		t.Fatalf("attempt = %d, want 3 after two recorded failures", retry.GetAttempt())
	}
}

func TestTheWaitingActivityIsAlwaysPresent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: a waiting state with nothing composable of its own.
	h.r.SetColdGate(testWS, ColdGate{Standing: true})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetWaiting().GetActivity().GetSalient().GetKind() == nil {
		t.Fatalf("the waiting activity is REQUIRED and must never be unset")
	}
}

// startFailedLine is the drawn bring-up failure arm, nil when the disconnected
// activity draws something else or nothing.
func startFailedLine(t *testing.T, h *harness) *frontendv1.FooterStatusActivityStartFailed {
	t.Helper()
	return h.view(t).GetStrip().GetStatus().GetDisconnected().GetActivity().GetSalient().GetStartFailed()
}

func TestTheBringUpFailureLineDrawsTheCauseItWasGiven(t *testing.T) {
	// The resolver never composes the cause: the site that opens the
	// shim_start_failed fault composes it out of that fault's own evidence, so
	// a spawn death and an adoption refusal reach the strip the same way and
	// differ only in what they say.
	cases := []struct {
		name   string
		detail string
	}{
		{name: "a failed spawn", detail: "exit 1: Error: Cannot find module '/opt/shim/main.js'"},
		{name: "a failed adoption", detail: "the lock's owner is unreachable"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			h.r.SetStartFailed(testWS, &StartFailed{Detail: tc.detail})
			h.r.OnLink(testWS, shimclient.LinkDead)

			// Assert
			line := startFailedLine(t, h)
			if line.GetDetail() != tc.detail {
				t.Fatalf("detail = %q, want %q", line.GetDetail(), tc.detail)
			}
		})
	}
}

func TestTheBringUpFailureLineStandsUnderTheStartFailedStep(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 1: boom"})
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetStartFailed() == nil {
		t.Fatalf("want the line under disconnected · start_failed, got %+v",
			h.view(t).GetStrip().GetStatus())
	}
}

func TestASuccessfulLinkClearsTheBringUpFailureLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 1: boom"})
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Act
	connected(h)
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if line := startFailedLine(t, h); line != nil {
		t.Fatalf("start_failed line = %+v, want it spent once the session served", line)
	}
}

func TestTheBringUpFailureLineIsRecordedOnceWhenItIsComposed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 1: boom"})
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Act
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if got := countOf(h.log.Records(), dlog.LevelInfo, "daemon.footer.start_failed_activity"); got != 1 {
		t.Fatalf("start_failed_activity records = %d, want exactly 1 for one standing line", got)
	}
}

// countOf counts the records at one level and operation.
func countOf(records []dlog.Record, level, operation string) int {
	n := 0
	for _, rec := range records {
		if rec.Level == level && rec.Operation == operation {
			n++
		}
	}
	return n
}

// ---- the tier each status arm resolves ------------------------------------

// queryDiedUpdate is the session's own statement that its vendor query died.
func queryDiedUpdate() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}}}
}

// deploying stands a deploy's progress on every strip.
func deploying(h *harness) {
	h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Building})
}

// lineName is the published view's activity line as the record names it.
func lineName(t *testing.T, h *harness) string {
	t.Helper()
	return activityLineOf(h.view(t).GetStrip().GetStatus()).name()
}

// rejectedFiveHour is the vendor refusing the five-hour allowance.
func rejectedFiveHour() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_RateLimitStatus{
		RateLimitStatus: &conversationv1.SessionRateLimitStatus{
			Status:        &conversationv1.SessionRateLimitStatus_Rejected{Rejected: &conversationv1.SessionRateLimitRejected{}},
			RateLimitType: fiveHourWindow(),
		}}}
}

func TestANonEscalatingFaultIsNeverSalient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OpenFault(testWS, faultOf(t, "f-1", health.KindClassifierFailed, false))

	// Assert
	if got := lineName(t, h); got != "transient.fault" {
		t.Fatalf("activity = %q, want the transient fault line", got)
	}
}

func TestEverySalientLineCarriesItsStandingInstant(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.clock.Advance(time.Minute)

	// Act
	h.r.OnSessionUpdate(testWS, vendorCompacting())

	// Assert
	at := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetAt()
	if at.GetAtMs() != instant.Add(time.Minute).UnixMilli() {
		t.Fatalf("at = %d, want the instant the line began standing", at.GetAtMs())
	}
}

func TestTheWaitingLineStandsFromWhenItsConditionOpened(t *testing.T) {
	// Arrange: the ask opens, then an unrelated fact re-renders later.
	h := newHarness(t)
	connected(h)
	h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "rm -rf"))
	h.clock.Advance(time.Minute)

	// Act
	h.r.SetParked(testWS, false)

	// Assert
	at := h.view(t).GetStrip().GetStatus().GetWaiting().GetActivity().GetSalient().GetAt()
	if at.GetAtMs() != instant.UnixMilli() {
		t.Fatalf("at = %d, want the instant the ask opened, not the render's", at.GetAtMs())
	}
}

// ---- the retry line's lifetime --------------------------------------------

func TestTheRetryLineEndsAtTheRetriedAgentsFirstResponse(t *testing.T) {
	tests := []struct {
		name string
		act  *conversationv1.AgentActivity
	}{
		{name: "its reasoning", act: thinkingActivity("th-1")},
		{name: "its prose", act: responseFrame("r-1", "start", nil)},
		{name: "a frame carrying the response's usage", act: subagentStartWithUsage()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})

			// Act
			h.r.OnActivity(testWS, mainAgent, tt.act)

			// Assert
			if got := h.view(t).GetStrip().GetStatus().GetBlocked(); got != nil {
				t.Fatalf("status = blocked %+v, want the retry ended by the response", got)
			}
		})
	}
}

func TestTheRetryLineSurvivesAToolCallWithNoUsage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})

	// Act
	h.r.OnActivity(testWS, mainAgent, subagentProgress("u-1", 10))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRetrying() == nil {
		t.Fatalf("the retry line ended on a frame that proves no response")
	}
}

func TestAnotherAgentsResponseDoesNotEndTheRetry(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})

	// Act
	h.r.OnActivity(testWS, &conversationv1.AgentId{Value: "agent-other"}, thinkingActivity("th-9"))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRetrying() == nil {
		t.Fatalf("another agent's reasoning ended the main agent's retry line")
	}
}

func TestTheNextTurnEndsTheRetryLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if got := lineName(t, h); got != "enduring" {
		t.Fatalf("activity = %q, want the retry gone at the next turn", got)
	}
}

// subagentStartWithUsage is a tool-call frame that carries an API response's
// usage, which is the vendor answering.
func subagentStartWithUsage() *conversationv1.AgentActivity {
	act := subagentProgress("u-1", 10)
	act.Usage = &conversationv1.TokenUsage{}
	return act
}

// ---- the dead-query line's lifetime ---------------------------------------

func TestTheDeadQueryLineStandsUntilTheNextTurnOpens(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnSessionUpdate(testWS, queryDiedUpdate())

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if got := lineName(t, h); got != "enduring" {
		t.Fatalf("activity = %q, want the dead-query line gone once the next turn opened", got)
	}
}

// THE RESTART THE DEATH ASKED FOR LIFTS THE LINE: the replacement shim's
// session start is a live query again.
func TestTheDeadQueryLineComesDownWhenTheSessionStartsAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnSessionUpdate(testWS, queryDiedUpdate())

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Assert
	if got := lineName(t, h); got == "salient.query_died" {
		t.Fatalf("activity = %q, want the dead-query line gone once the session started again", got)
	}
}

// AN ADOPTED SHIM RE-ANNOUNCES ITS START, THEN ITS DEATH: the line stands.
func TestADeathReannouncedAfterTheStartRaisesTheLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Act
	h.r.OnSessionUpdate(testWS, queryDiedUpdate())

	// Assert
	if got := lineName(t, h); got != "salient.query_died" {
		t.Fatalf("activity = %q, want the re-announced death's line", got)
	}
}

func TestTheDeadQueryLineSaysTheSessionIsRestarting(t *testing.T) {
	// Assert
	if deadQueryLine != "vendor query died — restarting the session" {
		t.Fatalf("deadQueryLine = %q, want it to say the daemon is restarting the session", deadQueryLine)
	}
}

func TestTheDeadQueryLineOutlivesATransient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnSessionUpdate(testWS, queryDiedUpdate())

	// Act
	h.r.OnActivity(testWS, mainAgent, notificationFrame("hello"))

	// Assert
	if got := lineName(t, h); got != "salient.query_died" {
		t.Fatalf("activity = %q, want the dead-query line over the transient", got)
	}
}

// ---- the recorded line ------------------------------------------------------

func TestActivityLineOfNamesTheTierAndKind(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    activityLine
	}{
		{"the enduring line", func(h *harness) {}, activityLine{tier: "enduring"}},
		{"a salient line", func(h *harness) {
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnSessionUpdate(testWS, vendorCompacting())
		}, activityLine{tier: "salient", kind: "compaction", text: "compacting the context…"}},
		{"a transient line", func(h *harness) {
			h.r.OnActivity(testWS, mainAgent, hookFrame("hello", true))
		}, activityLine{tier: "transient", kind: "hook", text: "name:\"hello\""}},
		{"the waiting cell, which has no tier oneof", func(h *harness) {
			h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "rm -rf"))
		}, activityLine{tier: "salient", kind: "gated_call", text: "Bash: rm -rf"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			tt.arrange(h)

			// Act
			got := activityLineOf(h.view(t).GetStrip().GetStatus())

			// Assert
			if got != tt.want {
				t.Fatalf("activityLineOf = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestActivityLineOfReadsNothingFromAnUnsetStatus(t *testing.T) {
	// Arrange, Act
	got := activityLineOf(&frontendv1.FooterStatus{})

	// Assert
	if got.name() != "none" {
		t.Fatalf("activityLineOf(unset) = %+v, want none", got)
	}
}

func TestActivityLineOfRendersATextlessKindsFields(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})

	// Act
	got := activityLineOf(h.view(t).GetStrip().GetStatus())

	// Assert
	if got.kind != "retrying" || !contains(got.text, "overloaded") {
		t.Fatalf("activityLineOf = %+v, want the retrying kind with its fields rendered", got)
	}
}

func TestATransientLineChangeIsRecordedAtDebug(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("hello", true))

	// Assert
	for _, rec := range lineChanges(h.log.Records()) {
		if rec.Context["kind"] == "transient.hook" && rec.Level != "debug" {
			t.Fatalf("a transient line change was recorded at %s, want debug", rec.Level)
		}
	}
	if !hasLevel(h.log.Records(), "debug", "daemon.footer.activity_line_changed") {
		t.Fatalf("no debug activity_line_changed record for the transient")
	}
}

// scheduledFailure is a connection failure carrying the vendor's retry
// schedule: retry ATTEMPT of MAX next, starting at NEXT.
func scheduledFailure(attempt, max uint32, next time.Time) *conversationv1.ApiRequestFailed {
	return &conversationv1.ApiRequestFailed{
		Message: "Can't reach the API server",
		Kind:    &conversationv1.ApiRequestFailed_Unmodeled{Unmodeled: &conversationv1.ApiUnmodeledError{Type: "connection/ENOTFOUND"}},
		Retry:   &conversationv1.ApiRetry{Attempt: attempt, MaxRetries: max, NextAttemptAtMs: next.UnixMilli()},
	}
}

func TestTheRetryLineCountsAttemptsAsTheVendorDoes(t *testing.T) {
	// Arrange: the vendor's eighth retry is next, of ten.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnApiError(testWS, mainAgent, scheduledFailure(8, 10, instant.Add(32*time.Second)))

	// Assert: attempt 9 of 11, as the request's own count runs.
	retry := h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRetrying()
	if retry.GetAttempt() != 9 || retry.GetMaxAttempt() != 11 {
		t.Fatalf("retry = %+v, want attempt 9 of 11", retry)
	}
}

func TestTheRetryLineCarriesTheNextAttemptsInstant(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	next := instant.Add(32 * time.Second)

	// Act
	h.r.OnApiError(testWS, mainAgent, scheduledFailure(8, 10, next))

	// Assert
	retry := h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRetrying()
	if retry.GetNextAttempt().GetAtMs() != next.UnixMilli() {
		t.Fatalf("next attempt = %v, want the vendor's stated instant", retry.GetNextAttempt())
	}
}

func TestAFurtherFailureMovesTheNextAttempt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, scheduledFailure(1, 10, instant.Add(time.Second)))
	later := instant.Add(10 * time.Second)

	// Act
	h.r.OnApiError(testWS, mainAgent, scheduledFailure(2, 10, later))

	// Assert
	retry := h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRetrying()
	if retry.GetAttempt() != 3 || retry.GetNextAttempt().GetAtMs() != later.UnixMilli() {
		t.Fatalf("retry = %+v, want attempt 3 due at the later instant", retry)
	}
}

func TestAnUnscheduledFailureCarriesNoNextAttempt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})

	// Assert
	retry := h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRetrying()
	if retry.NextAttempt != nil || retry.MaxAttempt != nil {
		t.Fatalf("retry = %+v, want no schedule the vendor never stated", retry)
	}
}

func TestTheRetriedCallsResponseAnnouncesTheRestoredAPI(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, scheduledFailure(8, 10, instant.Add(32*time.Second)))

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingActivity("th-1"))

	// Assert: the salient line ended, and a transient says the API answered.
	if got := h.view(t).GetStrip().GetStatus().GetBlocked(); got != nil {
		t.Fatalf("status = blocked %+v, want the retry ended", got)
	}
	if restored := transientOf(t, h).GetApiRestored(); restored.GetFailedAttempts() != 8 {
		t.Fatalf("transient = %v, want api_restored after 8 failed attempts", transientOf(t, h))
	}
}

func TestEachArmResolvesItsSalientKindsInPrecedenceThenUnpinnedWithNoStoppedMerge(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		arm     string
		want    string
	}{
		{"idle, nothing salient", func(h *harness) {}, "idle", "enduring"},
		{"idle under a deploy", deploying, "idle", "salient.update"},
		{"idle after a dead query with no turn", func(h *harness) {
			h.r.OnSessionUpdate(testWS, queryDiedUpdate())
		}, "idle", "salient.query_died"},
		{"a failed turn's dead query outranks a deploy", func(h *harness) {
			deploying(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnSessionUpdate(testWS, queryDiedUpdate())
		}, "turn_failed", "salient.query_died"},
		{"degraded shares the idle cell", func(h *harness) {
			deploying(h)
			h.r.SetStateUnreported(testWS, true)
		}, "degraded", "salient.update"},
		{"working, nothing salient", func(h *harness) {
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
		}, "working", "enduring"},
		{"working under a deploy", func(h *harness) {
			deploying(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
		}, "working", "salient.update"},
		{"a retry blocks the turn and outranks a deploy", func(h *harness) {
			deploying(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})
		}, "blocked", "salient.retrying"},
		{"a retry blocks a compacting turn", func(h *harness) {
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})
			h.r.OnSessionUpdate(testWS, vendorCompacting())
		}, "blocked", "salient.retrying"},
		{"interrupted, nothing salient", func(h *harness) {
			turn := testTurnID
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)
		}, "interrupted", "enduring"},
		{"interrupted under a deploy", func(h *harness) {
			turn := testTurnID
			deploying(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)
		}, "interrupted", "salient.update"},
		{"merging, nothing salient", func(h *harness) {
			h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepTesting})
		}, "merging", "enduring"},
		{"merging under a deploy", func(h *harness) {
			deploying(h)
			h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepTesting})
		}, "merging", "salient.update"},
		{"the merge step's line outranks a deploy", func(h *harness) {
			deploying(h)
			h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepCommitting, LineAt: instant, Line: &frontendv1.FooterStatusActivityMergeStep{
				Step: &frontendv1.FooterStatusActivityMergeStep_Committing{Committing: &frontendv1.FooterMergeStepCommitting{Subject: "merge(main): fix"}}}})
		}, "merging", "salient.merge_step"},
		{"merge_failed shares the merging cell", func(h *harness) {
			deploying(h)
			h.r.SetMerge(testWS, MergeFacts{State: "failed", FailedArea: FailedConflicts})
		}, "merge_failed", "salient.update"},
		{"merged shares the merging cell", func(h *harness) { h.r.SetMerge(testWS, MergeFacts{State: "merged"}) }, "merged", "enduring"},
		{"background, nothing salient", func(h *harness) {
			h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))
		}, "background", "enduring"},
		{"background under a deploy", func(h *harness) {
			deploying(h)
			h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))
		}, "background", "salient.update"},
		{"blocked on the account, the enduring usage figures explain it", func(h *harness) {
			h.r.OnSessionUpdate(testWS, rejectedFiveHour())
		}, "blocked", "enduring"},
		{"blocked on the account under a deploy", func(h *harness) {
			deploying(h)
			h.r.OnSessionUpdate(testWS, rejectedFiveHour())
		}, "blocked", "salient.update"},
		{"an escalating blocked fault outranks a deploy", func(h *harness) {
			deploying(h)
			h.r.OpenFault(testWS, faultOf(t, "f-1", health.KindStateUnreadable, false))
		}, "blocked", "salient.fault"},
		{"a severed link with no fault, nothing salient", func(h *harness) {
			h.r.OnLink(testWS, shimclient.LinkRedialing)
		}, "disconnected", "enduring"},
		{"a severed link under a deploy", func(h *harness) {
			deploying(h)
			h.r.OnLink(testWS, shimclient.LinkRedialing)
		}, "disconnected", "salient.update"},
		{"an escalating disconnected fault outranks a deploy", func(h *harness) {
			deploying(h)
			h.r.OpenFault(testWS, faultOf(t, "f-1", health.KindShimDied, false))
		}, "disconnected", "salient.fault"},
		{"the bring-up failure outranks a fault", func(h *harness) {
			h.r.OpenFault(testWS, faultOf(t, "f-1", health.KindShimDied, false))
			h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 1"})
		}, "disconnected", "salient.start_failed"},
		{"a refused close outranks a deploy", func(h *harness) {
			deploying(h)
			h.r.SetClosing(testWS, &CloseBlocked{Reason: "turn_in_flight", Detail: "a turn is in flight"})
		}, "closing", "salient.close_blocked"},
		{"loading, the injected item is the transient", func(h *harness) {
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnActivity(testWS, mainAgent, memoryInjection("CLAUDE.md"))
		}, "loading", "transient.context_injected"},
		{"loading under a deploy", func(h *harness) {
			deploying(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnActivity(testWS, mainAgent, memoryInjection("CLAUDE.md"))
		}, "loading", "salient.update"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tt.arrange(h)

			// Assert
			if got := h.status(t); got != tt.arm {
				t.Fatalf("status = %q, want %q", got, tt.arm)
			}
			if got := lineName(t, h); got != tt.want {
				t.Fatalf("activity = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheMergeStepsLineStandsWithTheInstantItBegan(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	line := &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_Rebasing{
		Rebasing: &frontendv1.FooterMergeStepRebasing{Line: &frontendv1.FooterMergeStepRebasing_Running{
			Running: &frontendv1.FooterMergeStepRebaseCommand{Text: "pick 1a2b3c fix the loop"}}}}}

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepRebasing, Replayed: 0, Total: 2, Line: line, LineAt: instant})

	// Assert
	salient := h.view(t).GetStrip().GetStatus().GetMerging().GetActivity().GetSalient()
	if got := salient.GetMergeStep().GetRebasing().GetRunning().GetText(); got != "pick 1a2b3c fix the loop" {
		t.Fatalf("merge step line = %q, want the rebase command", got)
	}
	if salient.GetAt() == nil {
		t.Fatalf("salient = %+v, want the instant the line began standing", salient)
	}
}

// THE QUIET TIER IS RETIRED (owner ruling, 2026-10-01): no line is composed
// from a feed item that landed, so a landing leaves the activity cell exactly
// as it stood before it.
func TestALandedFeedItemLeavesTheWorkingCellAsItWas(t *testing.T) {
	tests := []struct {
		name  string
		arm   protoreflect.Name
		phase protoreflect.Name
	}{
		{"a read finishing", "read", "success"},
		{"a shell failing", "bash", "failure"},
		{"a response finishing", "response", "success"},
		{"a hook succeeding", "hook", "succeeded"},
		{"a hook cancelled", "hook", "cancelled"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			inTurn(h)
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", tt.arm, "start"))
			before := working(t, h).GetActivity()

			// Act
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", tt.arm, tt.phase))

			// Assert
			if after := working(t, h).GetActivity(); !proto.Equal(before, after) {
				t.Fatalf("activity = %v, want it unchanged from %v: a landing composes no line", after, before)
			}
		})
	}
}

func TestADeliveredPromptComposesNoWorkingLine(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	inTurn(h)

	// Assert
	if got := activityLineOf(h.view(t).GetStrip().GetStatus()); got.tier != "enduring" {
		t.Fatalf("activity line = %+v, want the enduring line: a delivery composes no line", got)
	}
}

func TestADetachedLandingLeavesTheBackgroundCellAsItWas(t *testing.T) {
	tests := []struct {
		name string
		act  func(t *testing.T, h *harness)
	}{
		{"a detached subagent finishing", func(t *testing.T, h *harness) {
			h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(false))
		}},
		{"a detached subagent failing", func(t *testing.T, h *harness) {
			h.r.OnSubagent(testWS, workID("w-1"), subagentSettled(true))
		}},
		{"a detached shell failing", func(t *testing.T, h *harness) {
			h.r.OnBash(testWS, workID("w-1"), &conversationv1.AgentBash{
				Result: &conversationv1.AgentBash_Failure{Failure: &conversationv1.AgentBashFailure{}}})
		}},
		{"a subagent's own read landing", func(t *testing.T, h *harness) {
			h.r.OnActivity(testWS, detachedAgent, itemFrame(t, "u-9", "read", "success"))
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2", "w-2"}, nil, nil))
			before := h.view(t).GetStrip().GetStatus().GetBackground().GetActivity()
			if before == nil {
				t.Fatalf("status = %q, want background", h.status(t))
			}

			// Act
			tt.act(t, h)

			// Assert
			after := h.view(t).GetStrip().GetStatus().GetBackground().GetActivity()
			if !proto.Equal(before, after) {
				t.Fatalf("activity = %v, want it unchanged from %v: a landing composes no line", after, before)
			}
		})
	}
}

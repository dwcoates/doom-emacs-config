package footer

import (
	"testing"
	"time"

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
	retry := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetRetrying()
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
	retry := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetRetrying()
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

func TestTheBringUpFailureLineCarriesTheDroppedPromptCount(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 1: boom"})
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Act
	h.r.AddDroppedPrompts(testWS, 2)

	// Assert
	if got := startFailedLine(t, h).GetDroppedPrompts(); got != 2 {
		t.Fatalf("dropped_prompts = %d, want 2", got)
	}
}

func TestABringUpFailureThatDroppedNothingCountsZero(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 1: boom"})
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if got := startFailedLine(t, h).GetDroppedPrompts(); got != 0 {
		t.Fatalf("dropped_prompts = %d, want 0 for a failure that dropped none", got)
	}
}

func TestASecondBringUpFailureStartsItsOwnDroppedCount(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 1: boom"})
	h.r.AddDroppedPrompts(testWS, 3)

	// Act
	h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 2: boom again"})
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if got := startFailedLine(t, h).GetDroppedPrompts(); got != 0 {
		t.Fatalf("dropped_prompts = %d, want 0: the count belongs to the failure that dropped them", got)
	}
}

func TestDroppedPromptsWithNoStandingFailureAreRecordedLoudly(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.AddDroppedPrompts(testWS, 1)

	// Assert
	if !hasLevel(h.log.Records(), dlog.LevelWarn, "daemon.footer.dropped_prompts_unattributed") {
		t.Fatalf("records = %+v, want a WARN for a drop with no failure to attribute it to", h.log.Records())
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

// TestEachArmResolvesItsSalientKindsInPrecedenceThenUnpinned covers every
// status arm's cell: the kind that explains the step first, then an
// escalating fault, then a deploy's progress, and the enduring line beneath
// when nothing salient stands.
func TestEachArmResolvesItsSalientKindsInPrecedenceThenUnpinned(t *testing.T) {
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
		{"a retry outranks a deploy", func(h *harness) {
			deploying(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})
		}, "working", "salient.retrying"},
		{"a compaction outranks a retry", func(h *harness) {
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnApiError(testWS, mainAgent, &conversationv1.ApiRequestFailed{Message: "overloaded"})
			h.r.OnSessionUpdate(testWS, vendorCompacting())
		}, "working", "salient.compaction"},
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
		{"merging, nothing salient", func(h *harness) { h.r.SetMerge(testWS, MergeFacts{State: "merging"}) }, "merging", "enduring"},
		{"merging under a deploy", func(h *harness) {
			deploying(h)
			h.r.SetMerge(testWS, MergeFacts{State: "merging"})
		}, "merging", "salient.update"},
		{"merge_conflict shares the merging cell", func(h *harness) {
			deploying(h)
			h.r.SetMerge(testWS, MergeFacts{State: "conflict"})
		}, "merge_conflict", "salient.update"},
		{"merge_failed shares the merging cell", func(h *harness) { h.r.SetMerge(testWS, MergeFacts{State: "failed"}) }, "merge_failed", "enduring"},
		{"merged shares the merging cell", func(h *harness) { h.r.SetMerge(testWS, MergeFacts{State: "merged"}) }, "merged", "enduring"},
		{"background, nothing salient", func(h *harness) {
			h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))
		}, "background", "enduring"},
		{"background under a deploy", func(h *harness) {
			deploying(h)
			h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))
		}, "background", "salient.update"},
		{"blocked on the account, the vendor's refusal explains it", func(h *harness) {
			h.r.OnSessionUpdate(testWS, rejectedFiveHour())
		}, "blocked", "salient.rate_limit"},
		{"the refusal that explains the block outranks a deploy", func(h *harness) {
			deploying(h)
			h.r.OnSessionUpdate(testWS, rejectedFiveHour())
		}, "blocked", "salient.rate_limit"},
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
			if got := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetRetrying(); got != nil {
				t.Fatalf("retrying = %+v, want it ended by the response", got)
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
	if h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetRetrying() == nil {
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
	if h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetRetrying() == nil {
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
		{"the quiet-stretch line", func(h *harness) {
			h.r.OnMainAgent(testWS, mainAgent)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnTurnOpened(testWS, testTurnID)
		}, activityLine{tier: "quiet", text: "✅ Prompt delivered — awaiting response..."}},
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

func TestCoversEnduringReadsEveryTierAboveTheEnduringLine(t *testing.T) {
	transient := &frontendv1.FooterActivityTransient{Kind: &frontendv1.FooterActivityTransient_ToolCall{
		ToolCall: &frontendv1.FooterActivityTransientToolCall{}}}
	quiet := &frontendv1.FooterActivityQuietStretch{Text: "✅ Bash finished — handling result..."}
	tests := []struct {
		name     string
		salient  bool
		unpinned *frontendv1.FooterActivityTransientOverQuietOverEnduring
		want     bool
	}{
		{"a salient line", true, nil, true},
		{"a live transient", false, &frontendv1.FooterActivityTransientOverQuietOverEnduring{Transient: transient}, true},
		{"the quiet-stretch line", false, &frontendv1.FooterActivityTransientOverQuietOverEnduring{QuietStretch: quiet}, true},
		{"the enduring line alone", false, &frontendv1.FooterActivityTransientOverQuietOverEnduring{}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := coversEnduring(tt.salient, tt.unpinned)

			// Assert
			if got != tt.want {
				t.Fatalf("coversEnduring = %v, want %v", got, tt.want)
			}
		})
	}
}

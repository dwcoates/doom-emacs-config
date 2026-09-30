package footer

import (
	"testing"
	"time"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/deployprogress"
)

// lineNow is the activity line the last published view carries.
func lineNow(t *testing.T, h *harness) activityLine {
	t.Helper()
	return activityLineOf(h.view(t).GetStrip().GetStatus())
}

// rateEvent is a vendor rate-limit event about WINDOW with the named verdict:
// "allowed", "allowed_warning" or "rejected".
func rateEvent(window *conversationv1.SessionRateLimitType, verdict string) *conversationv1.SessionUpdate {
	update := rateLimitStatus(window, 85, time.Hour)
	switch verdict {
	case "allowed_warning":
		update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_AllowedWarning{
			AllowedWarning: &conversationv1.SessionRateLimitAllowedWarning{}}
	case "rejected":
		update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_Rejected{
			Rejected: &conversationv1.SessionRateLimitRejected{}}
	}
	return update
}

// ---- the push notification ------------------------------------------------

func TestAPushNotificationStandsAsASalientLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, notificationFrame("the agent needs you"))

	// Assert
	if got, want := lineNow(t, h), (activityLine{tier: "salient", kind: "notification", text: "the agent needs you"}); got != want {
		t.Fatalf("activity = %+v, want %+v", got, want)
	}
}

func TestAPushNotificationOutlivesItsTransientWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, notificationFrame("the agent needs you"))

	// Act
	h.clock.Advance(10 * DefaultTransientWindow)
	h.r.SetParked(testWS, false)

	// Assert
	if got := lineNow(t, h); got.kind != "notification" {
		t.Fatalf("activity = %+v, want the notification still standing: no timer ends a salient line", got)
	}
}

func TestTheNextPromptEndsThePushNotification(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *harness)
	}{
		{"a prompt delivered", func(h *harness) {
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Prompt: "next"})
		}},
		{"a prompt held in the queue", func(h *harness) {
			h.r.OnSubmission(testWS, Submission{Prompt: "next", Stage: StageHeld, Position: 1, Queued: 1})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.OnActivity(testWS, mainAgent, notificationFrame("the agent needs you"))

			// Act
			tt.act(h)

			// Assert
			if got := lineNow(t, h); got.kind == "notification" {
				t.Fatalf("activity = %+v, want the notification ended by the next prompt", got)
			}
		})
	}
}

// ---- the context-budget line -----------------------------------------------

func TestAContextBudgetWarningStandsAsASalientLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnContextBudgetWarning(testWS, mainAgent, &conversationv1.ContextBudgetWarning{Text: "context is filling"})

	// Assert
	if got, want := lineNow(t, h), (activityLine{tier: "salient", kind: "context_budget", text: "context is filling"}); got != want {
		t.Fatalf("activity = %+v, want %+v", got, want)
	}
}

func TestAFailedCompactionCutStandsTheBudgetLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act
	h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_CompactionFailed{
		CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "the summary was empty"}}})

	// Assert
	want := activityLine{tier: "salient", kind: "context_budget", text: "compaction failed — the summary was empty"}
	if got := lineNow(t, h); got != want {
		t.Fatalf("activity = %+v, want %+v", got, want)
	}
}

func TestACutThatShrinksTheContextEndsTheBudgetLine(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *harness)
	}{
		{"a compaction's cut", func(h *harness) {
			h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Compacted{
				Compacted: &conversationv1.ContextCompacted{}}})
		}},
		{"a /clear's cut", func(h *harness) {
			h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Cleared{
				Cleared: &conversationv1.ContextCleared{}}})
		}},
		{"a compaction that concluded and resumed", func(h *harness) {
			h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
				progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400, "")))
		}},
		{"a cold gate's compaction that concluded and resumed", func(h *harness) {
			h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compacted",
				Progress: progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400, "")})
			h.r.SetColdGateAnswer(testWS, nil)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.OnContextBudgetWarning(testWS, mainAgent, &conversationv1.ContextBudgetWarning{Text: "context is filling"})

			// Act
			tt.act(h)

			// Assert
			if got := lineNow(t, h); got.kind == "context_budget" {
				t.Fatalf("activity = %+v, want the budget line ended by the cut", got)
			}
		})
	}
}

func TestTheBudgetLineOutlivesAnyTimer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnContextBudgetWarning(testWS, mainAgent, &conversationv1.ContextBudgetWarning{Text: "context is filling"})

	// Act
	h.clock.Advance(time.Hour)
	h.r.SetParked(testWS, false)

	// Assert
	if got := lineNow(t, h); got.kind != "context_budget" {
		t.Fatalf("activity = %+v, want the budget line still standing", got)
	}
}

func TestASessionSwitchEndsTheBudgetLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "session-a"})
	h.r.OnContextBudgetWarning(testWS, mainAgent, &conversationv1.ContextBudgetWarning{Text: "context is filling"})

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "session-b"})

	// Assert
	if got := lineNow(t, h); got.kind == "context_budget" {
		t.Fatalf("activity = %+v, want the budget line ended by the switch", got)
	}
}

func TestARestartOfTheSameSessionKeepsTheBudgetLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "session-a"})
	h.r.OnContextBudgetWarning(testWS, mainAgent, &conversationv1.ContextBudgetWarning{Text: "context is filling"})

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "session-a"})

	// Assert
	if got := lineNow(t, h); got.kind != "context_budget" {
		t.Fatalf("activity = %+v, want the budget line kept: the context is unchanged", got)
	}
}

func TestASubagentsBudgetLineEndsWithItsRun(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnMainAgent(testWS, mainAgent)
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	h.r.OnContextBudgetWarning(testWS, sub, &conversationv1.ContextBudgetWarning{Text: "subagent context is filling"})

	// Act
	h.r.OnAgentTerminal(testWS, sub, nil, completed(), nil)

	// Assert
	if got := lineNow(t, h); got.kind == "context_budget" {
		t.Fatalf("activity = %+v, want the subagent's budget line ended with its run", got)
	}
}

func TestTheMainAgentsBudgetLineOutlivesASubagentsRun(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnMainAgent(testWS, mainAgent)
	h.r.OnContextBudgetWarning(testWS, mainAgent, &conversationv1.ContextBudgetWarning{Text: "context is filling"})

	// Act
	h.r.OnAgentTerminal(testWS, &conversationv1.AgentId{Value: "agent-sub"}, nil, completed(), nil)

	// Assert
	if got := lineNow(t, h); got.kind != "context_budget" {
		t.Fatalf("activity = %+v, want the main agent's budget line kept", got)
	}
}

// ---- the rate-limit event --------------------------------------------------

func TestARateLimitWarningStandsAsASalientLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, rateEvent(sevenDayWindow(), "allowed_warning"))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetSalient().GetRateLimit()
	if line.GetAllowedWarning() == nil || line.GetWindow().GetWeekly() == nil {
		t.Fatalf("rate limit = %+v, want the weekly allowance's warning", line)
	}
	if line.GetUtilization() != 0.85 || line.GetResetsAtS() != instant.Add(time.Hour).Unix() {
		t.Fatalf("rate limit = %+v, want the event's figures at 0.85, resetting in an hour", line)
	}
}

func TestARateLimitRefusalExplainsAUsageLimitBlock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, rateEvent(fiveHourWindow(), "rejected"))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetBlocked().GetActivity().GetSalient().GetRateLimit()
	if line.GetRejected() == nil || line.GetWindow().GetSession() == nil {
		t.Fatalf("rate limit = %+v, want the session allowance's refusal under the block", line)
	}
}

func TestAnAllowedEventEndsTheSameAllowancesRateLimitLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, rateEvent(sevenDayWindow(), "allowed_warning"))

	// Act
	h.r.OnSessionUpdate(testWS, rateEvent(sevenDayWindow(), "allowed"))

	// Assert
	if got := lineNow(t, h); got.kind == "rate_limit" {
		t.Fatalf("activity = %+v, want the rate-limit line ended", got)
	}
}

func TestAnAllowedEventForAnotherAllowanceKeepsTheLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, rateEvent(sevenDayWindow(), "allowed_warning"))

	// Act
	h.r.OnSessionUpdate(testWS, rateEvent(overageWindow(), "allowed"))

	// Assert
	if got := lineNow(t, h); got.kind != "rate_limit" {
		t.Fatalf("activity = %+v, want the weekly warning still standing", got)
	}
}

func TestAStatuslessRateEventChangesNoLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	update := rateEvent(sevenDayWindow(), "allowed")
	update.GetRateLimitStatus().Status = nil

	// Act
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	if got := lineNow(t, h); got.tier != "enduring" {
		t.Fatalf("activity = %+v, want the enduring line", got)
	}
}

func TestRateLimitWindowMapsEveryVendorWindow(t *testing.T) {
	tests := []struct {
		name   string
		window *conversationv1.SessionRateLimitType
		want   string
	}{
		{"five-hour", fiveHourWindow(), "session"},
		{"seven-day", sevenDayWindow(), "weekly"},
		{"seven-day opus", sevenDayOpusWindow(), "weekly_opus"},
		{"seven-day sonnet", sevenDaySonnetWindow(), "weekly_sonnet"},
		{"seven-day overage included", sevenDayOverageIncludedWindow(), "weekly_overage_included"},
		{"overage", overageWindow(), "overage"},
		{"none named", nil, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			window, name := rateLimitWindow(tt.window)

			// Assert
			if name != tt.want {
				t.Fatalf("name = %q, want %q", name, tt.want)
			}
			if tt.want == "" {
				if window != nil {
					t.Fatalf("window = %+v, want none", window)
				}
				return
			}
			m := window.ProtoReflect()
			if arm := m.WhichOneof(m.Descriptor().Oneofs().ByName("window")); arm == nil || string(arm.Name()) != tt.want {
				t.Fatalf("window arm = %v, want %s", arm, tt.want)
			}
		})
	}
}

// ---- precedence and the shared filling -------------------------------------

func TestTheSharedSalientLinesRankUpdateRateNotificationBudget(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{"a deploy outranks a rate-limit event", func(h *harness) {
			h.r.OnSessionUpdate(testWS, rateEvent(sevenDayWindow(), "allowed_warning"))
			h.r.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Installing})
		}, "update"},
		{"a rate-limit event outranks the notification", func(h *harness) {
			h.r.OnActivity(testWS, mainAgent, notificationFrame("look"))
			h.r.OnSessionUpdate(testWS, rateEvent(sevenDayWindow(), "allowed_warning"))
		}, "rate_limit"},
		{"the notification outranks the budget line", func(h *harness) {
			h.r.OnContextBudgetWarning(testWS, mainAgent, &conversationv1.ContextBudgetWarning{Text: "filling"})
			h.r.OnActivity(testWS, mainAgent, notificationFrame("look"))
		}, "notification"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tt.arrange(h)

			// Assert
			if got := lineNow(t, h); got.tier != "salient" || got.kind != tt.want {
				t.Fatalf("activity = %+v, want salient.%s", got, tt.want)
			}
		})
	}
}

func TestASalientSharedLineCoversALiveTransient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, notificationFrame("look"))

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Assert
	if got := lineNow(t, h); got.tier != "salient" {
		t.Fatalf("activity = %+v, want the notification over the newer transient", got)
	}
}

// salientMessages is every status arm's salient message.
var salientMessages = []proto.Message{
	&frontendv1.FooterStatusIdleSalient{},
	&frontendv1.FooterStatusWorkingSalient{},
	&frontendv1.FooterStatusWaitingSalient{},
	&frontendv1.FooterStatusInterruptedSalient{},
	&frontendv1.FooterStatusMergingSalient{},
	&frontendv1.FooterStatusBackgroundSalient{},
	&frontendv1.FooterStatusBlockedSalient{},
	&frontendv1.FooterStatusDisconnectedSalient{},
	&frontendv1.FooterStatusClosingSalient{},
	&frontendv1.FooterStatusLoadingSalient{},
}

func TestEverySalientMessageCarriesTheSharedKinds(t *testing.T) {
	shared := map[protoreflect.Name]protoreflect.FullName{
		"update":         "frontend.v1.FooterStatusActivityUpdate",
		"rate_limit":     "frontend.v1.FooterStatusActivityRateLimit",
		"notification":   "frontend.v1.FooterStatusActivityNotification",
		"context_budget": "frontend.v1.FooterStatusActivityContextBudget",
	}
	for _, msg := range salientMessages {
		desc := msg.ProtoReflect().Descriptor()
		t.Run(string(desc.Name()), func(t *testing.T) {
			for name, want := range shared {
				field := desc.Fields().ByName(name)
				if field == nil || field.ContainingOneof() == nil || field.ContainingOneof().Name() != "kind" {
					t.Fatalf("%s lacks the shared kind %s in its kind oneof", desc.Name(), name)
				}
				if got := field.Message().FullName(); got != want {
					t.Fatalf("%s.%s is %s, want %s", desc.Name(), name, got, want)
				}
			}
		})
	}
}

func TestFillSharedStampsTheLineAndItsInstant(t *testing.T) {
	// Arrange
	line := sharedLine{field: "notification", value: &frontendv1.FooterStatusActivityNotification{Text: "hi"}, at: instant}

	// Act
	got := fillShared(&frontendv1.FooterStatusMergingSalient{}, line)

	// Assert
	if got.GetNotification().GetText() != "hi" || got.GetAt().GetAtMs() != instant.UnixMilli() {
		t.Fatalf("salient = %+v, want the notification stamped at its instant", got)
	}
}

func TestFillSharedPanicsOnAMessageWithoutTheSharedKind(t *testing.T) {
	// Arrange
	line := sharedLine{field: "notification", value: &frontendv1.FooterStatusActivityNotification{Text: "hi"}, at: instant}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("fillShared on a message without the shared kind did not panic")
		}
	}()

	// Act
	fillShared(&frontendv1.FooterStatusActivityAt{}, line)
}

// ---- the submitting stages -------------------------------------------------

func TestEachSubmissionStageRaisesItsSubmittingLine(t *testing.T) {
	tests := []struct {
		name  string
		sub   Submission
		check func(*frontendv1.FooterActivityTransientSubmitting) bool
	}{
		{"held, with its place and the queue's size", Submission{Prompt: "fix it", Stage: StageHeld, Position: 2, Queued: 3},
			func(s *frontendv1.FooterActivityTransientSubmitting) bool {
				return s.GetHeld().GetPosition() == 2 && s.GetHeld().GetQueued() == 3
			}},
		{"classifying", Submission{Prompt: "fix it", Stage: StageClassifying},
			func(s *frontendv1.FooterActivityTransientSubmitting) bool { return s.GetClassifying() != nil }},
		{"interjecting", Submission{Prompt: "fix it", Stage: StageInterjecting},
			func(s *frontendv1.FooterActivityTransientSubmitting) bool { return s.GetInterjecting() != nil }},
		{"coalesced", Submission{Prompt: "fix it", Stage: StageCoalesced},
			func(s *frontendv1.FooterActivityTransientSubmitting) bool { return s.GetCoalesced() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.OnSubmission(testWS, tt.sub)

			// Assert
			got := transientOf(t, h).GetSubmitting()
			if got.GetPromptLead() != "fix it" || !tt.check(got) {
				t.Fatalf("submitting = %+v, want the %s stage for the prompt", got, tt.name)
			}
		})
	}
}

func TestADeliveredPromptRaisesTheDeliveredStage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Prompt: "fix it"})

	// Assert
	if got := transientOf(t, h).GetSubmitting(); got.GetDelivered() == nil || got.GetPromptLead() != "fix it" {
		t.Fatalf("submitting = %+v, want the delivered stage", got)
	}
}

func TestAnUndeclaredSubmissionStageIsRecordedAtErrorAndRaisesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSubmission(testWS, Submission{Prompt: "fix it", Stage: SubmissionStage(99)})

	// Assert
	if transientOf(t, h) != nil {
		t.Fatalf("transient = %+v, want none for an undeclared stage", transientOf(t, h))
	}
	recs := recordsOf(h.log.Records(), "daemon.footer.on_submission")
	var erred bool
	for _, rec := range recs {
		if rec.Level == "error" && rec.Context["stage"] == 99 {
			erred = true
		}
	}
	if !erred {
		t.Fatalf("records = %+v, want an ERROR naming stage 99", recs)
	}
}

// ---- the cold gate's compaction outcome ------------------------------------

func TestAColdGatesConcludedCompactionIsAnnounced(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compacted and resumed (101.6k → 12.4k)",
		Progress: progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400, "")})
	h.r.SetColdGateAnswer(testWS, nil)

	// Assert
	if got := transientOf(t, h).GetCompactionConcluded().GetText(); got != "compacted and resumed (101.6k → 12.4k)" {
		t.Fatalf("compaction_concluded = %q, want the gate's outcome announced", got)
	}
}

func TestAColdGatesFailedCompactionStandsTheBudgetLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compaction failed",
		Progress: progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED, 0, 0, "the summary was empty")})
	h.r.SetColdGateAnswer(testWS, nil)

	// Assert
	if got := lineNow(t, h); got.kind != "context_budget" {
		t.Fatalf("activity = %+v, want the failure standing as the budget line", got)
	}
}

func TestAColdGatesRunningPhaseAnnouncesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "summarizing",
		Progress: progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 101_600, 0, "")})

	// Assert
	if got := transientOf(t, h); got.GetCompactionConcluded() != nil {
		t.Fatalf("transient = %+v, want no outcome while the compaction runs", got)
	}
}

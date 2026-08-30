package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// notificationFrame is an outbound push notification, the highest-ranking
// activity line there is.
func notificationFrame(text string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "note-1"},
		Item: &conversationv1.AgentActivity_PushNotification{
			PushNotification: &conversationv1.AgentPushNotification{
				State: &conversationv1.AgentPushNotification_Start{
					Start: &conversationv1.AgentPushNotificationStart{Message: text},
				},
			},
		},
	}
}

// hookFrame is a hook firing or settling.
func hookFrame(name string, running bool) *conversationv1.AgentActivity {
	hook := &conversationv1.AgentHook{}
	if running {
		hook.Result = &conversationv1.AgentHook_Start{
			Start: &conversationv1.AgentHookStart{HookName: name},
		}
	} else {
		hook.Result = &conversationv1.AgentHook_Succeeded{
			Succeeded: &conversationv1.AgentHookSucceeded{},
		}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "hook-1"},
		Item:       &conversationv1.AgentActivity_Hook{Hook: hook},
	}
}

// accountUsage is one allowance sample with both windows.
func accountUsage(fiveHour, sevenDay float64) *conversationv1.SessionUpdate {
	seven := &conversationv1.SessionUsageWindow{
		UtilizationPercent: sevenDay,
		ResetsAtMs:         instant.Add(7 * 24 * time.Hour).UnixMilli(),
	}
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_AccountUsage{
			AccountUsage: &conversationv1.SessionAccountUsage{
				ObservedAtMs: instant.UnixMilli(),
				Outcome: &conversationv1.SessionAccountUsage_Available{
					Available: &conversationv1.SessionAccountUsageAvailable{
						FiveHour: &conversationv1.SessionUsageWindow{
							UtilizationPercent: fiveHour,
							ResetsAtMs:         instant.Add(5 * time.Hour).UnixMilli(),
						},
						SevenDay: seven,
					},
				},
			},
		},
	}
}

// budgetWarning is the vendor's own context-budget warning.
func budgetWarning(text string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextBudgetWarning{
			ContextBudgetWarning: &conversationv1.SessionContextBudgetWarning{Text: text},
		},
	}
}

func TestANotificationOutranksEveryCompetingActivity(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Act
	h.r.OnActivity(testWS, mainAgent, notificationFrame("the agent needs you"))

	// Assert
	activity := h.view(t).GetStrip().GetStatus().GetThinking().GetActivity()
	if activity.GetNotification().GetText() != "the agent needs you" {
		t.Fatalf("activity = %+v, want the notification to outrank the hook", activity.GetKind())
	}
}

func TestAStatusBoundHookOutranksTheRateReport(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnSessionUpdate(testWS, accountUsage(95, 40))

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Assert
	activity := h.view(t).GetStrip().GetStatus().GetThinking().GetActivity()
	if activity.GetHook().GetName() != "pre-commit" {
		t.Fatalf("activity = %+v, want the status-bound hook line", activity.GetKind())
	}
}

func TestTheRateReportOutranksTheContextBudget(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, budgetWarning("context is filling"))

	// Act
	h.r.OnSessionUpdate(testWS, accountUsage(95, 40))

	// Assert
	activity := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity()
	if activity.GetRateLimited() == nil {
		t.Fatalf("activity = %+v, want the rate report above the budget warning", activity.GetKind())
	}
}

func TestTheContextBudgetStandsWhenNothingElseDoes(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, budgetWarning("context is filling"))

	// Assert
	activity := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity()
	if activity.GetContextBudget().GetText() != "context is filling" {
		t.Fatalf("activity = %+v, want the budget warning", activity.GetKind())
	}
}

func TestAnUnremarkableAllowanceIsNotNews(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, accountUsage(10, 5))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity() != nil {
		t.Fatalf("an unremarkable allowance drew a line; only a newsworthy one is news")
	}
}

func TestAnAllowancePastTheThresholdIsNewsworthy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, accountUsage(90, 12))

	// Assert
	report := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if !report.GetSession().GetNewsworthy() {
		t.Fatalf("session allowance = %+v, want newsworthy at 90%%", report.GetSession())
	}
	if report.GetWeekly().GetNewsworthy() {
		t.Fatalf("weekly allowance = %+v, want unremarkable at 12%%", report.GetWeekly())
	}
}

func TestTheNewsworthyThresholdIsInjectable(t *testing.T) {
	// Arrange
	h := newHarness(t, WithRateLimitNewsworthyThreshold(0.05))
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, accountUsage(10, 5))

	// Assert
	report := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if !report.GetSession().GetNewsworthy() {
		t.Fatalf("session allowance = %+v, want newsworthy under a 5%% threshold", report.GetSession())
	}
}

func TestAnAllowanceIsCarriedAsAFractionAndEpochSeconds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, accountUsage(90, 90))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetUtilization() != 0.9 {
		t.Fatalf("utilization = %v, want the 0..1 fraction the contract carries", got.GetUtilization())
	}
	if got.GetResetsAtS() != instant.Add(5*time.Hour).Unix() {
		t.Fatalf("resets_at_s = %d, want epoch SECONDS", got.GetResetsAtS())
	}
}

func TestAnUnavailableAllowanceDrawsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, accountUsage(95, 95))

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_AccountUsage{
			AccountUsage: &conversationv1.SessionAccountUsage{
				Outcome: &conversationv1.SessionAccountUsage_Unavailable{
					Unavailable: &conversationv1.SessionAccountUsageUnavailable{},
				},
			},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity() != nil {
		t.Fatalf("an unavailable sample kept the previous report standing")
	}
}

func TestASampleMissingTheWeeklyWindowIsNotDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_AccountUsage{
			AccountUsage: &conversationv1.SessionAccountUsage{
				Outcome: &conversationv1.SessionAccountUsage_Available{
					Available: &conversationv1.SessionAccountUsageAvailable{
						FiveHour: &conversationv1.SessionUsageWindow{UtilizationPercent: 99},
					},
				},
			},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity() != nil {
		t.Fatalf("a one-window sample was drawn; the line states BOTH allowances or neither")
	}
}

func TestASettledHookLeavesTheActivityLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", true))

	// Act
	h.r.OnActivity(testWS, mainAgent, hookFrame("pre-commit", false))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetThinking().GetActivity() != nil {
		t.Fatalf("a settled hook kept its line standing")
	}
}

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
	retry := h.view(t).GetStrip().GetStatus().GetThinking().GetActivity().GetRetrying()
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
	retry := h.view(t).GetStrip().GetStatus().GetThinking().GetActivity().GetRetrying()
	if retry.GetAttempt() != 3 {
		t.Fatalf("attempt = %d, want 3 after two recorded failures", retry.GetAttempt())
	}
}

func TestEveryActivityCarriesItsStandingInstant(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, budgetWarning("context is filling"))

	// Assert
	at := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetAt()
	if at.GetAtMs() != instant.UnixMilli() {
		t.Fatalf("at = %d, want the instant the line began standing", at.GetAtMs())
	}
}

func TestTheWaitingActivityIsAlwaysPresent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: a waiting state with nothing composable of its own.
	h.r.SetColdGate(testWS, ColdGate{Standing: true})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetWaiting().GetActivity() == nil {
		t.Fatalf("the waiting activity is REQUIRED and must never be unset")
	}
}

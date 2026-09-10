package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
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

// rateLimitStatus is one vendor rate-limit event for one window.
func rateLimitStatus(window *conversationv1.SessionRateLimitType, utilization float64, resetsIn time.Duration) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_RateLimitStatus{
			RateLimitStatus: &conversationv1.SessionRateLimitStatus{
				Status:             &conversationv1.SessionRateLimitStatus_Allowed{Allowed: &conversationv1.SessionRateLimitAllowed{}},
				UtilizationPercent: &utilization,
				ResetsAtMs:         ptr(instant.Add(resetsIn).UnixMilli()),
				RateLimitType:      window,
			},
		},
	}
}

// fiveHourWindow and sevenDayWindow are the two windows the drawn line states.
func fiveHourWindow() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_FiveHour{
		FiveHour: &conversationv1.SessionRateLimitWindowFiveHour{},
	}}
}

func sevenDayWindow() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDay{
		SevenDay: &conversationv1.SessionRateLimitWindowSevenDay{},
	}}
}

// sevenDayOpusWindow, sevenDaySonnetWindow, sevenDayOverageIncludedWindow and
// overageWindow are the remaining declared windows an event can name.
func sevenDayOpusWindow() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDayOpus{
		SevenDayOpus: &conversationv1.SessionRateLimitWindowSevenDayOpus{},
	}}
}

func sevenDaySonnetWindow() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDaySonnet{
		SevenDaySonnet: &conversationv1.SessionRateLimitWindowSevenDaySonnet{},
	}}
}

func sevenDayOverageIncludedWindow() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDayOverageIncluded{
		SevenDayOverageIncluded: &conversationv1.SessionRateLimitWindowSevenDayOverageIncluded{},
	}}
}

func overageWindow() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_Overage{
		Overage: &conversationv1.SessionRateLimitWindowOverage{},
	}}
}

// ptr is the address of a value, which is how an optional scalar is set.
func ptr[T any](v T) *T { return &v }

// usageSample is one sampled account usage, the FIGURES' source. A negative
// seven-day utilization stands for "the vendor reported no weekly window".
func usageSample(fiveHour, sevenDay float64, observedAtMs int64) *conversationv1.SessionUpdate {
	available := &conversationv1.SessionAccountUsageAvailable{
		FiveHour: &conversationv1.SessionUsageWindow{
			UtilizationPercent: fiveHour,
			ResetsAtMs:         instant.Add(5 * time.Hour).UnixMilli(),
		},
	}
	if sevenDay >= 0 {
		available.SevenDay = &conversationv1.SessionUsageWindow{
			UtilizationPercent: sevenDay,
			ResetsAtMs:         instant.Add(7 * 24 * time.Hour).UnixMilli(),
		}
	}
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_AccountUsage{
			AccountUsage: &conversationv1.SessionAccountUsage{
				ObservedAtMs: observedAtMs,
				Outcome:      &conversationv1.SessionAccountUsage_Available{Available: available},
			},
		},
	}
}

// unavailableUsageSample is one sample that could read no figure at all.
func unavailableUsageSample(observedAtMs int64) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_AccountUsage{
			AccountUsage: &conversationv1.SessionAccountUsage{
				ObservedAtMs: observedAtMs,
				Outcome: &conversationv1.SessionAccountUsage_Unavailable{
					Unavailable: &conversationv1.SessionAccountUsageUnavailable{
						Reason: &conversationv1.SessionAccountUsageUnavailable_ServiceUnavailable{
							ServiceUnavailable: &conversationv1.SessionUsageServiceUnavailable{},
						},
					},
				},
			},
		},
	}
}

// bothAllowances files a usage sample carrying both windows' figures, which is
// what makes the line drawable.
func bothAllowances(h *harness, fiveHour, sevenDay float64) {
	h.r.OnSessionUpdate(testWS, usageSample(fiveHour, sevenDay, instant.UnixMilli()))
}

// budgetWarning is the vendor's own context-budget warning, an AGENT-PLANE
// fact the sidecar produces from the transcript.
func budgetWarning(h *harness, text string) {
	h.r.OnContextBudgetWarning(testWS, mainAgent, &conversationv1.ContextBudgetWarning{Text: text})
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
	bothAllowances(h, 95, 40)

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
	budgetWarning(h, "context is filling")

	// Act
	bothAllowances(h, 95, 40)

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
	budgetWarning(h, "context is filling")

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
	bothAllowances(h, 10, 5)

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
	bothAllowances(h, 90, 12)

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
	bothAllowances(h, 10, 5)

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
	bothAllowances(h, 90, 90)

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetUtilization() != 0.9 {
		t.Fatalf("utilization = %v, want the 0..1 fraction the contract carries", got.GetUtilization())
	}
	if got.GetResetsAtS() != instant.Add(5*time.Hour).Unix() {
		t.Fatalf("resets_at_s = %d, want epoch SECONDS", got.GetResetsAtS())
	}
}

func TestAnAllowanceCopiesTheVendorsStatusArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	bothAllowances(h, 95, 95)

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 95, 5*time.Hour))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetAllowed() == nil {
		t.Fatalf("status = %+v, want the vendor's allowed arm copied", got.GetStatus())
	}
}

func TestAnAllowanceWarningStatusIsCopied(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bothAllowances(h, 95, 95)

	// Act
	update := rateLimitStatus(fiveHourWindow(), 95, 5*time.Hour)
	update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_AllowedWarning{
		AllowedWarning: &conversationv1.SessionRateLimitAllowedWarning{},
	}
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetAllowedWarning() == nil {
		t.Fatalf("status = %+v, want the vendor's allowed_warning arm copied", got.GetStatus())
	}
}

func TestARejectedAllowanceStatusIsCopied(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bothAllowances(h, 95, 95)

	// Act
	update := rateLimitStatus(fiveHourWindow(), 95, 5*time.Hour)
	update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_Rejected{
		Rejected: &conversationv1.SessionRateLimitRejected{},
	}
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetRejected() == nil {
		t.Fatalf("status = %+v, want the vendor's rejected arm copied", got.GetStatus())
	}
}

func TestAStatusTheVendorLeftUnsetDrawsNoArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bothAllowances(h, 95, 95)

	// Act
	update := rateLimitStatus(fiveHourWindow(), 95, 5*time.Hour)
	update.GetRateLimitStatus().Status = nil
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetStatus() != nil {
		t.Fatalf("status = %+v, want no arm; an absent status is never defaulted", got.GetStatus())
	}
}

func TestTheFiguresComeFromTheUsageSample(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetSession().GetUtilization() != 0.95 || line.GetWeekly().GetUtilization() != 0.9 {
		t.Fatalf("allowances = %+v, want both figures from the sample", line)
	}
}

func TestTheResetComesFromTheUsageSampleInSeconds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetResetsAtS() != instant.Add(5*time.Hour).Unix() {
		t.Fatalf("resets_at_s = %d, want the sample's reset in seconds", got.GetResetsAtS())
	}
}

func TestTheVerdictIsUnsetBeforeAnyRateLimitEvent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetStatus() != nil {
		t.Fatalf("status = %+v, want no verdict until a rate-limit event is seen", got.GetStatus())
	}
}

func TestTheVerdictJoinsWhenTheRateLimitEventArrives(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act
	update := rateLimitStatus(fiveHourWindow(), 95, 5*time.Hour)
	update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_Rejected{
		Rejected: &conversationv1.SessionRateLimitRejected{},
	}
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetRejected() == nil {
		t.Fatalf("status = %+v, want the verdict to have joined the drawn allowance", got.GetStatus())
	}
}

func TestEveryRateLimitWindowMatchesItsAllowance(t *testing.T) {
	// Arrange
	tests := []struct {
		name   string
		window *conversationv1.SessionRateLimitType
		// weekly reports which allowance the verdict must land on.
		weekly bool
	}{
		{name: "five hour is the session allowance", window: fiveHourWindow()},
		{name: "seven day is the weekly allowance", window: sevenDayWindow(), weekly: true},
		{name: "seven day opus is the weekly allowance", window: sevenDayOpusWindow(), weekly: true},
		{name: "seven day sonnet is the weekly allowance", window: sevenDaySonnetWindow(), weekly: true},
		{name: "seven day overage included is the weekly allowance", window: sevenDayOverageIncludedWindow(), weekly: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			h := newHarness(t)
			connected(h)
			h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

			// Act
			h.r.OnSessionUpdate(testWS, rateLimitStatus(tc.window, 95, 5*time.Hour))

			// Assert
			line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
			landed, other := line.GetSession(), line.GetWeekly()
			if tc.weekly {
				landed, other = other, landed
			}
			if landed.GetAllowed() == nil || other.GetStatus() != nil {
				t.Fatalf("session = %+v weekly = %+v, want the verdict on one allowance only", line.GetSession(), line.GetWeekly())
			}
		})
	}
}

func TestTheOverageWindowIsLoggedAndDrawnNowhere(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act: the contract carries no overage cell.
	h.r.OnSessionUpdate(testWS, rateLimitStatus(overageWindow(), 95, 5*time.Hour))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetSession().GetStatus() != nil || line.GetWeekly().GetStatus() != nil {
		t.Fatalf("session = %+v weekly = %+v, want the overage verdict drawn nowhere", line.GetSession(), line.GetWeekly())
	}
}

func TestAnEarlierSampleDoesNotOverwriteALaterEvent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act: the event arrives after the sample, so its figure is the newest
	// sighting.
	h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 99, 5*time.Hour))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetUtilization() != 0.99 {
		t.Fatalf("utilization = %v, want the event's figure, which arrived last", got.GetUtilization())
	}
}

// A LATER SAMPLE OVERWRITES AN EARLIER EVENT'S FIGURE. This is the shape the
// footer got wrong: the sample's `observed_at_ms` is stamped by the shim
// before the update crosses the pipe, the event carried no stamp at all and so
// was stamped by the DAEMON on receipt, and comparing the two kept the event's
// retired 0.82 standing over every later sample.
func TestALaterSampleOverwritesAnEarlierEventsFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 82, time.Hour))

	// Act: the shim sampled just before the daemon received the event, and the
	// sample arrived after it.
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.Add(-5*time.Millisecond).UnixMilli()))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetUtilization() != 0.95 {
		t.Fatalf("utilization = %v, want the sample's figure, which arrived last", got.GetUtilization())
	}
}

// Two samples ARE comparable: both stamps come from the one shim's clock.
func TestAStaleSampleDoesNotOverwriteANewerSample(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act: a sample the shim observed a minute earlier arrives out of order.
	h.r.OnSessionUpdate(testWS, usageSample(81, 90, instant.Add(-time.Minute).UnixMilli()))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetUtilization() != 0.95 {
		t.Fatalf("utilization = %v, want the newer sample's figure kept", got.GetUtilization())
	}
}

// The unavailable arm states no figure, so it retires none either.
func TestAnUnavailableSampleLeavesTheStandingFiguresAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.Add(time.Minute).UnixMilli()))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if got.GetUtilization() != 0.95 {
		t.Fatalf("utilization = %v, want the standing figures left alone", got.GetUtilization())
	}
}

func TestASampleWithoutASevenDayWindowDrawsNoWeeklyAllowance(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: the vendor reported no weekly window at all.
	h.r.OnSessionUpdate(testWS, usageSample(95, -1, instant.UnixMilli()))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetSession() == nil || line.GetWeekly() != nil {
		t.Fatalf("line = %+v, want the session allowance drawn and the weekly one absent", line)
	}
}

func TestARateLimitEventAloneDrawsNoLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: the verdict's source carries no figures of its own to draw from
	// until a usage sample exists — but an event's utilization does seed one.
	h.r.OnSessionUpdate(testWS, rateLimitStatus(sevenDayWindow(), 10, 7*24*time.Hour))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity() != nil {
		t.Fatalf("an unremarkable allowance was drawn; only a newsworthy one is news")
	}
}

func TestAStatusNamingNoWindowIsDropped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act: a status the vendor gave no window is not filable.
	h.r.OnSessionUpdate(testWS, rateLimitStatus(nil, 99, 5*time.Hour))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetSession().GetStatus() != nil || line.GetWeekly().GetStatus() != nil {
		t.Fatalf("session = %+v weekly = %+v, want a windowless status filed nowhere", line.GetSession(), line.GetWeekly())
	}
}

// AMENDED BY LANDING 13, deliberately. This test used to assert that an
// unavailable sample drew NOTHING, which is the product gap the landing
// closes: a failed read left no trace, so the strip went on drawing the last
// percentage as though it were current and nothing on the wire could say
// otherwise. The line now draws to STATE THE UNREAD, and it still draws no
// FIGURE, because none was ever read.
func TestAnUnavailableUsageSampleDrawsItsOutcomeAndNoFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: a sample that could read no figure.
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.UnixMilli()))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if _, ok := line.GetSample().GetOutcome().(*frontendv1.FooterAllowanceSample_ServiceUnavailable); !ok {
		t.Fatalf("outcome = %+v, want the unread named", line.GetSample().GetOutcome())
	}
	if line.GetSession() != nil || line.GetWeekly() != nil {
		t.Fatalf("session = %+v weekly = %+v, want no figure invented for a read that never happened",
			line.GetSession(), line.GetWeekly())
	}
}

// The gate: an unremarkable allowance is not news, but an allowance NOBODY
// COULD READ is — otherwise the outcome cell would be unreachable from every
// session whose figures sit below the newsworthiness threshold.
func TestAnUnreadableSampleOpensTheLineBelowTheNewsworthinessGate(t *testing.T) {
	// Arrange: figures well under the 0.8 threshold, so nothing is news yet.
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(41, 63, instant.UnixMilli()))
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity() != nil {
		t.Fatal("an unremarkable allowance drew a line before the unread")
	}

	// Act
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.Add(time.Minute).UnixMilli()))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited() == nil {
		t.Fatal("an unread sample drew no line, so nothing can say the figures are stale")
	}
}

// THE STANDING CONTRACT, restated at the drawn surface: an unavailable
// outcome joins the figures on hand, it never clears them.
func TestAnUnreadableSampleKeepsTheStandingFiguresDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(41, 63, instant.UnixMilli()))

	// Act
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.Add(time.Minute).UnixMilli()))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetSession().GetUtilization() != 0.41 || line.GetWeekly().GetUtilization() != 0.63 {
		t.Fatalf("allowances = %+v, want the figures on hand left standing beside the unread", line)
	}
}

// An available sample RETIRES a standing unread: the figures beside it are
// fresh again, so the line stops saying they are not.
func TestAReadableSampleRetiresTheStandingUnread(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(41, 63, instant.UnixMilli()))
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.Add(time.Minute).UnixMilli()))

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(41, 63, instant.Add(2*time.Minute).UnixMilli()))

	// Assert: unremarkable AND readable is not news at all.
	if got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity(); got != nil {
		t.Fatalf("activity = %+v, want the unread retired and the line with it", got.GetKind())
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
	budgetWarning(h, "context is filling")

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

// THE THRESHOLD IS A FRACTION AND THE VENDOR'S FIGURE IS A PERCENTAGE. An
// unremarkable allowance must not be newsworthy, or the rate line would stand
// permanently and crowd out every lower-ranked line there is.
func TestAnAllowanceIsNewsworthyOnlyAboveTheThresholdOnceTheScalesAgree(t *testing.T) {
	tests := []struct {
		name           string
		fiveHour       float64
		wantNewsworthy bool
	}{
		{name: "an ordinary session is not news", fiveHour: 41, wantNewsworthy: false},
		{name: "an allowance past the threshold is", fiveHour: 88, wantNewsworthy: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			budgetWarning(h, "context is filling")

			// Act
			bothAllowances(h, tc.fiveHour, 10)

			// Assert
			activity := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity()
			if got := activity.GetRateLimited() != nil; got != tc.wantNewsworthy {
				t.Fatalf("rate line stands = %v, want %v (activity = %+v)",
					got, tc.wantNewsworthy, activity.GetKind())
			}
		})
	}
}

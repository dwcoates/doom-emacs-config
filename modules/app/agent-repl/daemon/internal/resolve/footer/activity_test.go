package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/shimclient"
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
		// want names the allowance the verdict must land on; every other
		// allowance must be left without a status arm.
		want string
	}{
		{name: "five hour is the session allowance", window: fiveHourWindow(), want: "session"},
		{name: "seven day is the weekly allowance", window: sevenDayWindow(), want: "weekly"},
		{name: "seven day opus is the weekly allowance", window: sevenDayOpusWindow(), want: "weekly"},
		{name: "seven day sonnet is the weekly allowance", window: sevenDaySonnetWindow(), want: "weekly"},
		{name: "seven day overage included is the weekly allowance", window: sevenDayOverageIncludedWindow(), want: "weekly"},
		{name: "overage is the overage allowance", window: overageWindow(), want: "overage"},
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
			drawn := map[string]*frontendv1.FooterAllowance{
				"session": line.GetSession(),
				"weekly":  line.GetWeekly(),
				"overage": line.GetOverage(),
			}
			for label, allowance := range drawn {
				verdicted := allowance.GetStatus() != nil
				if verdicted != (label == tc.want) {
					t.Fatalf("%s status = %+v, want the verdict on %s alone", label, allowance.GetStatus(), tc.want)
				}
			}
		})
	}
}

// THE OVERAGE WINDOW IS ITS OWN CELL. It used to be logged and dropped,
// because the contract had no allowance for it; the figure the vendor
// reported now lands where a reader can see it.
func TestTheOverageWindowCarriesItsOwnFigures(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(overageWindow(), 42, 3*time.Hour))

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetOverage()
	wantResetsAtS := instant.Add(3*time.Hour).UnixMilli() / 1000
	if got.GetUtilization() != 0.42 || got.GetResetsAtS() != wantResetsAtS {
		t.Fatalf("overage = %+v, want utilization 0.42 and reset %d", got, wantResetsAtS)
	}
}

// AN UNREPORTED OVERAGE WINDOW DRAWS ABSENT, like an unreported weekly one:
// most accounts never have one, and a synthesized zero would read as a
// figure the vendor stated.
func TestAnUnreportedOverageWindowDrawsNoAllowance(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetOverage() != nil {
		t.Fatalf("overage = %+v, want no allowance for a window the vendor never reported", line.GetOverage())
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

// THE UNREAD MESSAGE IS GONE. An unavailable sample with NO figure ever read
// draws no line at all — the strip falls back to its no-figures behavior
// rather than a "usage unread" caveat (owner ruling of 2026-09-15).
func TestAnUnreadableSampleWithNoPriorFiguresDrawsNoLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: a sample that could read no figure, with nothing read before it.
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.UnixMilli()))

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity(); got != nil {
		t.Fatalf("activity = %+v, want no line for an unread with no figures on hand", got.GetKind())
	}
}

// A readable sample stamps the instant the figures were READ, which the strip
// ticks the reading's age from.
func TestAReadableSampleStampsTheFiguresReadInstant(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	readAt := h.clock.Now()

	// Act: a newsworthy sample, so the line draws and exposes the instant.
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetFiguresReadAtMs() != readAt.UnixMilli() {
		t.Fatalf("figures_read_at_ms = %d, want the readable sample's read instant %d",
			line.GetFiguresReadAtMs(), readAt.UnixMilli())
	}
}

// An unreadable sample leaves the read instant standing — the age stays
// anchored to the last SUCCESSFUL read, never the failed attempt.
func TestAnUnreadableSampleDoesNotRestampTheFiguresReadInstant(t *testing.T) {
	// Arrange: a readable sample fixes the read instant.
	h := newHarness(t)
	connected(h)
	readAt := h.clock.Now()
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act: the clock moves on and a later sample reads nothing.
	h.clock.Advance(time.Minute)
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.Add(time.Minute).UnixMilli()))

	// Assert: the instant is still the readable sample's, not the failed one's.
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetFiguresReadAtMs() != readAt.UnixMilli() {
		t.Fatalf("figures_read_at_ms = %d, want the last successful read %d left standing",
			line.GetFiguresReadAtMs(), readAt.UnixMilli())
	}
}

// THE STANDING CONTRACT, restated at the drawn surface: an unreadable sample
// leaves the figures on hand standing, it never clears them. (Regression lock.)
func TestAnUnreadableSampleKeepsTheStandingFiguresDrawn(t *testing.T) {
	// Arrange: newsworthy figures, so the line draws.
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.Add(time.Minute).UnixMilli()))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited()
	if line.GetSession().GetUtilization() != 0.95 || line.GetWeekly().GetUtilization() != 0.90 {
		t.Fatalf("allowances = %+v, want the figures on hand left standing after an unreadable sample", line)
	}
}

// A SAMPLING FAILURE'S CAUSE IS RECORDED. The strip draws no unread caveat
// (owner ruling of 2026-09-15), so the breadcrumb is where a reader learns
// what the shim could not do — the reason alone would say only "it failed".
func TestAnUnreadableSamplingFailureRecordsTheShimsCause(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	sample := unavailableUsageSample(instant.UnixMilli())
	sample.GetAccountUsage().GetUnavailable().Reason = &conversationv1.SessionAccountUsageUnavailable_SamplingFailure{
		SamplingFailure: &conversationv1.SessionUsageSamplingFailure{Cause: "the transcript scan failed"},
	}

	// Act
	h.r.OnSessionUpdate(testWS, sample)

	// Assert
	for _, rec := range h.log.Records() {
		if rec.Operation == "daemon.footer.usage_sample_unreadable" {
			if rec.Context["reason"] != "sampling_failure" || rec.Context["cause"] != "the transcript scan failed" {
				t.Fatalf("record context = %+v, want reason sampling_failure and the shim's cause", rec.Context)
			}
			return
		}
	}
	t.Fatalf("no daemon.footer.usage_sample_unreadable record in %+v", h.log.Records())
}

// Every other unread arm names its reason and states no cause, since none was
// given.
func TestAnUnreadableServiceUnavailableRecordsNoCause(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.UnixMilli()))

	// Assert
	for _, rec := range h.log.Records() {
		if rec.Operation == "daemon.footer.usage_sample_unreadable" {
			if _, has := rec.Context["cause"]; has || rec.Context["reason"] != "service_unavailable" {
				t.Fatalf("record context = %+v, want reason service_unavailable and no cause", rec.Context)
			}
			return
		}
	}
	t.Fatalf("no daemon.footer.usage_sample_unreadable record in %+v", h.log.Records())
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

// ---- the bring-up failure line --------------------------------------------

// startFailedLine is the drawn bring-up failure arm, nil when the disconnected
// activity draws something else or nothing.
func startFailedLine(t *testing.T, h *harness) *frontendv1.FooterStatusActivityStartFailed {
	t.Helper()
	return h.view(t).GetStrip().GetStatus().GetDisconnected().GetActivity().GetStartFailed()
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

func TestTheBringUpFailureLineOutranksANotification(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.OnActivity(testWS, mainAgent, notificationFrame("the agent needs you"))

	// Act
	h.r.SetStartFailed(testWS, &StartFailed{Detail: "exit 1: boom"})
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	activity := h.view(t).GetStrip().GetStatus().GetDisconnected().GetActivity()
	if activity.GetStartFailed() == nil {
		t.Fatalf("activity = %+v, want the bring-up failure to outrank the notification", activity.GetKind())
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

func TestActivityLineOfReadsTheStandingLine(t *testing.T) {
	tests := []struct {
		name   string
		status *frontendv1.FooterStatus
		want   activityLine
	}{
		{
			name:   "no status arm at all",
			status: &frontendv1.FooterStatus{},
			want:   activityLine{},
		},
		{
			name: "a status arm with no activity",
			status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Idle{
				Idle: &frontendv1.FooterStatusIdle{}}},
			want: activityLine{},
		},
		{
			name: "a kind that carries its own text",
			status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Thinking{
				Thinking: &frontendv1.FooterStatusThinking{Activity: &frontendv1.FooterStatusThinkingActivity{
					Kind: &frontendv1.FooterStatusThinkingActivity_Compaction{
						Compaction: &frontendv1.FooterStatusActivityCompaction{Text: "compacting the context…"}},
				}}}},
			want: activityLine{kind: "compaction", text: "compacting the context…"},
		},
		{
			name: "a kind with no text field names the kind",
			status: &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Thinking{
				Thinking: &frontendv1.FooterStatusThinking{Activity: &frontendv1.FooterStatusThinkingActivity{
					Kind: &frontendv1.FooterStatusThinkingActivity_Hook{
						Hook: &frontendv1.FooterStatusActivityHook{Name: "PreToolUse"}},
				}}}},
			want: activityLine{kind: "hook"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act
			got := activityLineOf(tt.status)

			// Assert. A textless kind's rendering is prototext, whose spacing
			// is deliberately unstable, so only its kind is pinned exactly.
			if got.kind != tt.want.kind {
				t.Fatalf("kind = %q, want %q", got.kind, tt.want.kind)
			}
			if tt.want.text != "" && got.text != tt.want.text {
				t.Fatalf("text = %q, want %q", got.text, tt.want.text)
			}
		})
	}
}

func TestActivityLineOfRendersATextlessKindsFields(t *testing.T) {
	// Arrange
	status := &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Thinking{
		Thinking: &frontendv1.FooterStatusThinking{Activity: &frontendv1.FooterStatusThinkingActivity{
			Kind: &frontendv1.FooterStatusThinkingActivity_Hook{
				Hook: &frontendv1.FooterStatusActivityHook{Name: "PreToolUse"}},
		}}}}

	// Act
	got := activityLineOf(status)

	// Assert
	if !contains(got.text, "PreToolUse") {
		t.Fatalf("text = %q, want the hook's name", got.text)
	}
}

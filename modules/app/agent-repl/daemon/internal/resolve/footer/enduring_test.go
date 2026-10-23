package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

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

func TestAnAllowancePastTheThresholdIsNewsworthy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	bothAllowances(h, 90, 12)

	// Assert
	report := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if !report.GetSession().GetNewsworthy() {
		t.Fatalf("session allowance = %+v, want newsworthy at 90%%", report.GetSession())
	}
	if report.GetWeekly().GetOverall().GetNewsworthy() {
		t.Fatalf("weekly allowance = %+v, want unremarkable at 12%%", report.GetWeekly().GetOverall())
	}
}

func TestTheNewsworthyThresholdIsInjectable(t *testing.T) {
	// Arrange
	h := newHarness(t, WithRateLimitNewsworthyThreshold(0.05))
	connected(h)

	// Act
	bothAllowances(h, 10, 5)

	// Assert
	report := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
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
	got := enduringUsageOf(h).GetSession()
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
	got := enduringUsageOf(h).GetSession()
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
	got := enduringUsageOf(h).GetSession()
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
	// A rejected verdict blocks the session (ladder.RateLimitRejections), so the
	// allowance line stands under the block.
	got := enduringUsageOf(h).GetSession()
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
	got := enduringUsageOf(h).GetSession()
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
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if line.GetSession().GetUtilization() != 0.95 || line.GetWeekly().GetOverall().GetUtilization() != 0.9 {
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
	got := enduringUsageOf(h).GetSession()
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
	got := enduringUsageOf(h).GetSession()
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
	// A rejected verdict blocks the session (ladder.RateLimitRejections), so the
	// allowance line stands under the block.
	got := enduringUsageOf(h).GetSession()
	if got.GetRejected() == nil {
		t.Fatalf("status = %+v, want the verdict to have joined the drawn allowance", got.GetStatus())
	}
}

func TestEveryRateLimitWindowMatchesItsAllowance(t *testing.T) {
	// Arrange
	tests := []struct {
		name   string
		window *conversationv1.SessionRateLimitType
		// want names the allowance the verdict must land on, "" for none;
		// every other allowance must be left without a status arm.
		want string
	}{
		{name: "five hour is the session allowance", window: fiveHourWindow(), want: "session"},
		{name: "seven day is the overall weekly allowance", window: sevenDayWindow(), want: "weekly"},
		{name: "seven day opus is the opus weekly allowance", window: sevenDayOpusWindow(), want: "weekly_opus"},
		{name: "seven day sonnet is the sonnet weekly allowance", window: sevenDaySonnetWindow(), want: "weekly_sonnet"},
		{name: "seven day overage included is no drawn allowance", window: sevenDayOverageIncludedWindow(), want: ""},
		{name: "overage is the overage allowance", window: overageWindow(), want: "overage"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			h := newHarness(t)
			connected(h)
			h.r.OnSessionUpdate(testWS, usageSampleWithModels(95, 90, 40, 30, instant.UnixMilli()))
			// The overage window is figured only by an event; the statusless
			// second one leaves it figured with no verdict of its own.
			h.r.OnSessionUpdate(testWS, rateLimitStatus(overageWindow(), 10, 5*time.Hour))
			h.r.OnSessionUpdate(testWS, withStatus(rateLimitStatus(overageWindow(), 10, 5*time.Hour), nil))

			// Act
			h.r.OnSessionUpdate(testWS, rateLimitStatus(tc.window, 95, 5*time.Hour))

			// Assert
			line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
			drawn := map[string]*frontendv1.FooterAllowance{
				"session":       line.GetSession(),
				"weekly":        line.GetWeekly().GetOverall(),
				"weekly_opus":   line.GetWeekly().GetModels().GetOpus(),
				"weekly_sonnet": line.GetWeekly().GetModels().GetSonnet(),
				"overage":       line.GetOverage(),
			}
			for label, allowance := range drawn {
				if allowance == nil {
					t.Fatalf("%s allowance is not drawn; the arrangement figures every window", label)
				}
				verdicted := allowance.GetStatus() != nil
				if verdicted != (label == tc.want) {
					t.Fatalf("%s status = %+v, want the verdict on %q alone", label, allowance.GetStatus(), tc.want)
				}
			}
		})
	}
}

// withStatus replaces a rate-limit event's status arm; nil leaves it unset.
func withStatus(update *conversationv1.SessionUpdate, status any) *conversationv1.SessionUpdate {
	event := update.GetRateLimitStatus()
	event.Status = nil
	switch arm := status.(type) {
	case *conversationv1.SessionRateLimitStatus_Rejected:
		event.Status = arm
	case *conversationv1.SessionRateLimitStatus_Allowed:
		event.Status = arm
	}
	return update
}

// rejectedStatus is the rejected verdict arm.
func rejectedStatus() *conversationv1.SessionRateLimitStatus_Rejected {
	return &conversationv1.SessionRateLimitStatus_Rejected{Rejected: &conversationv1.SessionRateLimitRejected{}}
}

// usageSampleWithModels is usageSample with the per-model seven-day windows
// too; a negative utilization leaves that window unreported.
func usageSampleWithModels(fiveHour, sevenDay, opus, sonnet float64, observedAtMs int64) *conversationv1.SessionUpdate {
	update := usageSample(fiveHour, sevenDay, observedAtMs)
	available := update.GetAccountUsage().GetAvailable()
	week := instant.Add(7 * 24 * time.Hour).UnixMilli()
	if opus >= 0 {
		available.SevenDayOpus = &conversationv1.SessionUsageWindow{UtilizationPercent: opus, ResetsAtMs: week}
	}
	if sonnet >= 0 {
		available.SevenDaySonnet = &conversationv1.SessionUsageWindow{UtilizationPercent: sonnet, ResetsAtMs: week}
	}
	return update
}

func TestTheWeeklyElementCarriesThePerModelWindows(t *testing.T) {
	tests := []struct {
		name                       string
		sevenDay, opus, sonnet     float64
		wantOverall                float64
		wantOpus, wantSonnet       float64
		wantModels, wantOverallSet bool
	}{
		{name: "overall and both models", sevenDay: 40, opus: 12, sonnet: 30,
			wantOverallSet: true, wantOverall: 0.4, wantModels: true, wantOpus: 0.12, wantSonnet: 0.3},
		{name: "overall alone carries no models", sevenDay: 40, opus: -1, sonnet: -1,
			wantOverallSet: true, wantOverall: 0.4},
		{name: "overall and opus alone", sevenDay: 40, opus: 12, sonnet: -1,
			wantOverallSet: true, wantOverall: 0.4, wantModels: true, wantOpus: 0.12},
		{name: "models with no overall figure", sevenDay: -1, opus: 12, sonnet: 30,
			wantModels: true, wantOpus: 0.12, wantSonnet: 0.3},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.OnSessionUpdate(testWS, usageSampleWithModels(10, tc.sevenDay, tc.opus, tc.sonnet, instant.UnixMilli()))

			// Assert
			weekly := enduringUsageOf(h).GetWeekly()
			if (weekly.GetOverall() != nil) != tc.wantOverallSet || weekly.GetOverall().GetUtilization() != tc.wantOverall {
				t.Fatalf("overall = %+v, want set=%v at %v", weekly.GetOverall(), tc.wantOverallSet, tc.wantOverall)
			}
			models := weekly.GetModels()
			if (models != nil) != tc.wantModels {
				t.Fatalf("models = %+v, want set=%v", models, tc.wantModels)
			}
			if models.GetOpus().GetUtilization() != tc.wantOpus || models.GetSonnet().GetUtilization() != tc.wantSonnet {
				t.Fatalf("models = %+v, want opus %v sonnet %v", models, tc.wantOpus, tc.wantSonnet)
			}
		})
	}
}

// A VERDICT IS ABOUT ONE WINDOW INSTANCE. A rejection of the week that has
// since reset never colors the next week's figure.
func TestAVerdictLapsesWithTheWindowInstanceItWasAbout(t *testing.T) {
	tests := []struct {
		name    string
		advance time.Duration
		want    bool
	}{
		{name: "before the reset the verdict stands", advance: 30 * time.Minute, want: true},
		{name: "at the reset the verdict lapses", advance: time.Hour, want: false},
		{name: "after the reset the verdict lapses", advance: 2 * time.Hour, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the week resets in an hour, and the vendor rejected it.
			h := newHarness(t)
			connected(h)
			h.r.OnSessionUpdate(testWS, usageSample(10, 100, instant.UnixMilli()))
			h.r.OnSessionUpdate(testWS, withStatus(rateLimitStatus(sevenDayWindow(), 100, time.Hour), rejectedStatus()))

			// Act: a fresh sample of the next week, taken after the clock moved.
			h.clock.Advance(tc.advance)
			h.r.OnSessionUpdate(testWS, usageSample(10, 3, h.clock.Now().UnixMilli()))

			// Assert
			got := enduringUsageOf(h).GetWeekly().GetOverall().GetRejected() != nil
			if got != tc.want {
				t.Fatalf("rejected drawn = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAVerdictNamingNoResetLapsesWithTheFiguresItWasFiledAgainst(t *testing.T) {
	// Arrange: the sampled week resets in seven days; the event names no reset.
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(10, 100, instant.UnixMilli()))
	event := withStatus(rateLimitStatus(sevenDayWindow(), 100, time.Hour), rejectedStatus())
	event.GetRateLimitStatus().ResetsAtMs = nil
	h.r.OnSessionUpdate(testWS, event)

	// Act
	h.clock.Advance(7*24*time.Hour + time.Minute)
	h.r.OnSessionUpdate(testWS, usageSample(10, 3, h.clock.Now().UnixMilli()))

	// Assert
	if got := enduringUsageOf(h).GetWeekly().GetOverall(); got.GetStatus() != nil {
		t.Fatalf("weekly status = %+v, want the verdict lapsed with the week it was filed against", got.GetStatus())
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
	got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage().GetOverage()
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
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if line.GetOverage() != nil {
		t.Fatalf("overage = %+v, want no allowance for a window the vendor never reported", line.GetOverage())
	}
}

// AN EVENT NEVER OVERWRITES THE SAMPLED FIGURE (owner ruling, 2026-10-13:
// only the most recent figure is drawn). The event carries no instant, so it
// cannot be shown to be newer than the sample on hand.
func TestAnEventDoesNotOverwriteTheSampledFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 99, 5*time.Hour))

	// Assert
	got := enduringUsageOf(h).GetSession()
	if got.GetUtilization() != 0.95 {
		t.Fatalf("utilization = %v, want the sample's figure kept over the event's", got.GetUtilization())
	}
	if got.GetAllowed() == nil {
		t.Fatalf("status = %+v, want the event's verdict joined to the sampled figure", got.GetStatus())
	}
}

// THE SHAPE THE FOOTER GOT WRONG (2026-10-13): one workspace's session filed a
// newer sample, then another workspace's session on the same account relayed
// an event carrying an older, lower figure, and the drawn figure went down.
func TestAnotherWorkspacesEventDoesNotLowerTheAccountsSampledFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bindOther(t, h)
	h.r.SetAccount(testWS, personalRoot)
	h.r.SetAccount(otherWS, personalRoot)
	h.r.OnSessionUpdate(testWS, usageSample(98, 90, instant.UnixMilli()))

	// Act
	h.r.OnSessionUpdate(otherWS, rateLimitStatus(fiveHourWindow(), 97, 5*time.Hour))

	// Assert
	for _, ws := range []ids.WorkspaceID{testWS, otherWS} {
		if got := enduringOfWS(t, h, ws).GetUsage().GetSession().GetUtilization(); got != 0.98 {
			t.Fatalf("%s session utilization = %v, want the newest sample's 0.98", ws, got)
		}
	}
}

func TestAnEventAloneDoesNotFigureASampledWindow(t *testing.T) {
	tests := []struct {
		name   string
		window *conversationv1.SessionRateLimitType
		drawn  func(*frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance
	}{
		{"five hour", fiveHourWindow(), (*frontendv1.FooterActivityEnduringUsage).GetSession},
		{"seven day", sevenDayWindow(), func(u *frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance {
			return u.GetWeekly().GetOverall()
		}},
		{"seven day opus", sevenDayOpusWindow(), func(u *frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance {
			return u.GetWeekly().GetModels().GetOpus()
		}},
		{"seven day sonnet", sevenDaySonnetWindow(), func(u *frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance {
			return u.GetWeekly().GetModels().GetSonnet()
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.OnSessionUpdate(testWS, rateLimitStatus(tc.window, 50, 5*time.Hour))

			// Assert
			if got := tc.drawn(enduringUsageOf(h)); got != nil {
				t.Fatalf("allowance = %+v, want none until a sample figures the window", got)
			}
		})
	}
}

func TestEveryFigureTheFooterReceivesIsRecorded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		act     func(h *harness)
		want    dlog.RecordQuery
		source  string
		percent float64
	}{
		{
			name:    "a sample that moves the drawn figure",
			arrange: func(h *harness) { h.r.OnSessionUpdate(testWS, usageSample(40, 20, instant.UnixMilli())) },
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, usageSample(41, 20, instant.Add(time.Minute).UnixMilli()))
			},
			want:    dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.footer.account_usage_figure", Message: "a usage sample moved the drawn figure"},
			source:  "account_usage_sample",
			percent: 41,
		},
		{
			name:    "a sample that restates the drawn figure",
			arrange: func(h *harness) { h.r.OnSessionUpdate(testWS, usageSample(40, 20, instant.UnixMilli())) },
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, usageSample(40, 20, instant.Add(time.Minute).UnixMilli()))
			},
			want:    dlog.RecordQuery{Level: dlog.LevelDebug, Operation: "daemon.footer.account_usage_figure", Message: "a usage sample restated the drawn figure"},
			source:  "account_usage_sample",
			percent: 40,
		},
		{
			name:    "a sample older than the one on hand",
			arrange: func(h *harness) { h.r.OnSessionUpdate(testWS, usageSample(40, 20, instant.UnixMilli())) },
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, usageSample(39, 20, instant.Add(-time.Minute).UnixMilli()))
			},
			want:    dlog.RecordQuery{Level: dlog.LevelDebug, Operation: "daemon.footer.account_usage_stale"},
			source:  "account_usage_sample",
			percent: 39,
		},
		{
			name:    "an event on a sampled window",
			arrange: func(h *harness) { h.r.OnSessionUpdate(testWS, usageSample(40, 20, instant.UnixMilli())) },
			act:     func(h *harness) { h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 38, 5*time.Hour)) },
			want:    dlog.RecordQuery{Level: dlog.LevelDebug, Operation: "daemon.footer.account_usage_figure", Message: "a rate-limit event's utilization is not drawn on a sampled window; the newest sample's figure stands"},
			source:  "rate_limit_event",
			percent: 38,
		},
		{
			name:    "an event on the overage window",
			arrange: func(*harness) {},
			act:     func(h *harness) { h.r.OnSessionUpdate(testWS, rateLimitStatus(overageWindow(), 12, 3*time.Hour)) },
			want:    dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.footer.account_usage_figure", Message: "a rate-limit event figured the overage window"},
			source:  "rate_limit_event",
			percent: 12,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			tc.arrange(h)

			// Act
			tc.act(h)

			// Assert
			for _, rec := range recordsOf(h.log.Records(), tc.want.Operation) {
				if rec.Level != tc.want.Level || (tc.want.Message != "" && rec.Message != tc.want.Message) {
					continue
				}
				if rec.Context["source"] == tc.source && rec.Context["utilization_percent"] == tc.percent {
					return
				}
			}
			t.Fatalf("no %s record of the %s figure %v in %+v", tc.want.Level, tc.source, tc.percent, h.log.Records())
		})
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
	got := enduringUsageOf(h).GetSession()
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
	got := enduringUsageOf(h).GetSession()
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
	got := enduringUsageOf(h).GetSession()
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
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if line.GetSession() == nil || line.GetWeekly() != nil {
		t.Fatalf("line = %+v, want the session allowance drawn and the weekly one absent", line)
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
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if line.GetSession().GetStatus() != nil || line.GetWeekly().GetOverall().GetStatus() != nil {
		t.Fatalf("session = %+v weekly = %+v, want a windowless status filed nowhere", line.GetSession(), line.GetWeekly().GetOverall())
	}
}

// THE UNREAD MESSAGE IS GONE. An unavailable sample with NO figure ever read
// draws no usage at all — the enduring line has no figure to state, and no
// "usage unread" caveat is drawn (owner ruling of 2026-09-15).
func TestAnUnreadableSampleWithNoPriorFiguresDrawsNoLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: a sample that could read no figure, with nothing read before it.
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.UnixMilli()))

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage(); got != nil {
		t.Fatalf("usage = %+v, want none for an unread with no figures on hand", got)
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
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if line.GetSession().GetUtilization() != 0.95 || line.GetWeekly().GetOverall().GetUtilization() != 0.90 {
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

// EVERY UNREAD ARM IS NAMED IN THE BREADCRUMB, and the figures on hand stand
// whichever arm it was: the record is the one place a failing read stays
// visible, so a collapsed or dropped reason word would hide which read failed.
func TestEveryUnreadReasonIsNamedAndLeavesTheFiguresStanding(t *testing.T) {
	tests := []struct {
		name   string
		reason func(*conversationv1.SessionAccountUsageUnavailable)
		want   string
	}{
		{name: "service unavailable", want: "service_unavailable", reason: func(u *conversationv1.SessionAccountUsageUnavailable) {
			u.Reason = &conversationv1.SessionAccountUsageUnavailable_ServiceUnavailable{ServiceUnavailable: &conversationv1.SessionUsageServiceUnavailable{}}
		}},
		{name: "window unavailable", want: "window_unavailable", reason: func(u *conversationv1.SessionAccountUsageUnavailable) {
			u.Reason = &conversationv1.SessionAccountUsageUnavailable_WindowUnavailable{WindowUnavailable: &conversationv1.SessionUsageWindowUnavailable{}}
		}},
		{name: "utilization unavailable", want: "utilization_unavailable", reason: func(u *conversationv1.SessionAccountUsageUnavailable) {
			u.Reason = &conversationv1.SessionAccountUsageUnavailable_UtilizationUnavailable{UtilizationUnavailable: &conversationv1.SessionUsageUtilizationUnavailable{}}
		}},
		{name: "sampling failure", want: "sampling_failure", reason: func(u *conversationv1.SessionAccountUsageUnavailable) {
			u.Reason = &conversationv1.SessionAccountUsageUnavailable_SamplingFailure{SamplingFailure: &conversationv1.SessionUsageSamplingFailure{}}
		}},
		{name: "no stated reason", want: "unspecified", reason: func(u *conversationv1.SessionAccountUsageUnavailable) {
			u.Reason = nil
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: figures read earlier, so the line draws.
			h := newHarness(t)
			connected(h)
			h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))
			sample := unavailableUsageSample(instant.Add(time.Minute).UnixMilli())
			tc.reason(sample.GetAccountUsage().GetUnavailable())

			// Act
			h.r.OnSessionUpdate(testWS, sample)

			// Assert
			line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
			if line.GetSession().GetUtilization() != 0.95 || line.GetWeekly().GetOverall().GetUtilization() != 0.90 {
				t.Fatalf("allowances = %+v, want the figures on hand left standing", line)
			}
			if line.GetSample() != nil {
				t.Fatalf("sample = %+v, want unset: an unread sample draws no caveat", line.GetSample())
			}
			for _, rec := range h.log.Records() {
				if rec.Operation == "daemon.footer.usage_sample_unreadable" {
					if rec.Context["reason"] != tc.want {
						t.Fatalf("reason = %v, want %q", rec.Context["reason"], tc.want)
					}
					return
				}
			}
			t.Fatalf("no daemon.footer.usage_sample_unreadable record in %+v", h.log.Records())
		})
	}
}

// THE THRESHOLD IS A FRACTION AND THE VENDOR'S FIGURE IS A PERCENTAGE. An
// ordinary allowance must not be colored as a warning.
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

			// Act
			bothAllowances(h, tc.fiveHour, 10)

			// Assert
			session := enduringUsageOf(h).GetSession()
			if session.GetNewsworthy() != tc.wantNewsworthy {
				t.Fatalf("session newsworthy = %v, want %v (allowance = %+v)", session.GetNewsworthy(), tc.wantNewsworthy, session)
			}
		})
	}
}

// enduringOf is the idle cell's enduring line.
// enduringUsageOf is the usage the enduring line would draw now, read off the
// resolver's state: a standing rate-limit event pins the cell salient, so the
// pushed view carries no enduring line while it stands.
func enduringUsageOf(h *harness) *frontendv1.FooterActivityEnduringUsage {
	h.r.mu.Lock()
	defer h.r.mu.Unlock()
	return h.r.enduringUsage(h.r.stateLocked(testWS))
}

func enduringOf(t *testing.T, h *harness) *frontendv1.FooterActivityEnduring {
	t.Helper()
	return h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring()
}

// contextUsage is one context-usage report against a window.
func contextUsage(total, max int64) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ContextUsage{
		ContextUsage: &conversationv1.SessionContextUsage{TotalTokens: total, MaxTokens: max}}}
}

func TestAnUnremarkableAllowanceIsStillDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	bothAllowances(h, 10, 5)

	// Assert
	usage := enduringOf(t, h).GetUsage()
	if usage.GetSession() == nil || usage.GetWeekly() == nil || usage.GetSession().GetNewsworthy() {
		t.Fatalf("usage = %+v, want both allowances drawn and neither newsworthy", usage)
	}
}

func TestTheEnduringLineIsUnobservedBeforeAnyUsageFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	connected(h)

	// Assert
	if enduringOf(t, h).GetUnobserved() == nil {
		t.Fatalf("enduring = %+v, want unobserved before any usage figure", enduringOf(t, h))
	}
}

func TestAContextReportNeverClaimsTheEnduringLine(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{name: "no usage figure: still unobserved", arrange: func(*harness) {}, want: "unobserved"},
		{name: "a usage figure: still usage", arrange: func(h *harness) { bothAllowances(h, 10, 5) }, want: "usage"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			tt.arrange(h)

			// Act: a context window nearly full.
			h.r.OnSessionUpdate(testWS, contextUsage(190_000, 200_000))

			// Assert
			enduring := enduringOf(t, h)
			if got := string(enduring.ProtoReflect().WhichOneof(enduring.ProtoReflect().Descriptor().Oneofs().ByName("line")).Name()); got != tt.want {
				t.Fatalf("enduring line = %s, want %s", got, tt.want)
			}
		})
	}
}

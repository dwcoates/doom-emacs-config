package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
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
	// A rejected verdict blocks the session (ladder.RateLimitBlocks), so the
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
	// A rejected verdict blocks the session (ladder.RateLimitBlocks), so the
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
			line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
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

func TestAnEarlierSampleDoesNotOverwriteALaterEvent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(95, 90, instant.UnixMilli()))

	// Act: the event arrives after the sample, so its figure is the newest
	// sighting.
	h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 99, 5*time.Hour))

	// Assert
	got := enduringUsageOf(h).GetSession()
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
	if line.GetSession().GetStatus() != nil || line.GetWeekly().GetStatus() != nil {
		t.Fatalf("session = %+v weekly = %+v, want a windowless status filed nowhere", line.GetSession(), line.GetWeekly())
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
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
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
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
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
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
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

func TestTheEnduringLineIsDrawnWithNoFigures(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	connected(h)

	// Assert
	enduring := enduringOf(t, h)
	if enduring == nil {
		t.Fatalf("the enduring line is unset; it is always drawn")
	}
	if enduring.GetUsage() != nil || enduring.GetContextWindow() != nil {
		t.Fatalf("enduring = %+v, want both parts unset before any figure", enduring)
	}
}

func TestTheContextWindowIsDrawnWithItsFill(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, contextUsage(50_000, 200_000))

	// Assert
	window := enduringOf(t, h).GetContextWindow()
	if window.GetUsedTokens() != 50_000 || window.GetWindowTokens() != 200_000 || window.GetFill() != 0.25 {
		t.Fatalf("context window = %+v, want 50000 of 200000 at 0.25", window)
	}
}

func TestTheEightyPercentRuleChoosesTheEnduringLine(t *testing.T) {
	tests := []struct {
		name     string
		fiveHour float64
		weekly   float64
		used     int64
		want     string
	}{
		{"both under 80%: usage", 50, 30, 100_000, "usage"},
		{"only the context at or above 80%: the context", 50, 90, 166_000, "context_window"},
		{"only the five-hour allowance at or above 80%: usage", 86, 30, 166_000, "usage"},
		{"both at or above 80%, the context higher: the context", 81, 30, 180_000, "context_window"},
		{"both at or above 80%, usage higher: usage", 95, 30, 166_000, "usage"},
		{"both at 80% exactly: usage wins the tie", 80, 30, 160_000, "usage"},
		{"the weekly allowance never enters the choice", 50, 99, 100_000, "usage"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			bothAllowances(h, tt.fiveHour, tt.weekly)

			// Act
			h.r.OnSessionUpdate(testWS, contextUsage(tt.used, 200_000))

			// Assert
			enduring := enduringOf(t, h)
			if got := string(enduring.ProtoReflect().WhichOneof(enduring.ProtoReflect().Descriptor().Oneofs().ByName("line")).Name()); got != tt.want {
				t.Fatalf("enduring line = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestAnEnduringLineWithOneFigureObservedDrawsThatFigure(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{"usage alone", func(h *harness) { bothAllowances(h, 10, 5) }, "usage"},
		{"the context window alone, however low", func(h *harness) { h.r.OnSessionUpdate(testWS, contextUsage(10_000, 200_000)) }, "context_window"},
		{"neither", func(h *harness) {}, "unobserved"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tt.arrange(h)

			// Assert
			enduring := enduringOf(t, h)
			if got := string(enduring.ProtoReflect().WhichOneof(enduring.ProtoReflect().Descriptor().Oneofs().ByName("line")).Name()); got != tt.want {
				t.Fatalf("enduring line = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestContextClaimsEnduring(t *testing.T) {
	tests := []struct {
		name           string
		fiveHour, fill float64
		want           bool
	}{
		{"context below the threshold", 0.1, 0.79, false},
		{"context at the threshold, five-hour below it", 0.5, 0.8, true},
		{"both at the threshold: usage wins the tie", 0.8, 0.8, false},
		{"both above, context higher", 0.85, 0.9, true},
		{"both above, five-hour higher", 0.95, 0.9, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := contextClaimsEnduring(tt.fiveHour, tt.fill)

			// Assert
			if got != tt.want {
				t.Fatalf("contextClaimsEnduring(%v, %v) = %v, want %v", tt.fiveHour, tt.fill, got, tt.want)
			}
		})
	}
}

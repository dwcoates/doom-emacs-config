package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ratelimittest"
)

// rateLimitStatus is one vendor rate-limit event for one window.
func rateLimitStatus(window *conversationv1.SessionRateLimitType, utilization float64, resetsIn time.Duration) *conversationv1.SessionUpdate {
	return ratelimittest.Update(ratelimittest.Figured(ratelimittest.Allowed(window), utilization, instant.Add(resetsIn).UnixMilli()))
}

// week is how far ahead a seven-day window's reset lies.
const week = 7 * 24 * time.Hour

// fileWeekly files one allowed rate-limit event figuring the account-wide
// seven-day window at UTILIZATION percent, which is what makes the line
// drawable.
func fileWeekly(h *harness, utilization float64) {
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.SevenDay(), utilization, week))
}

// fileWeeklyWindows files one allowed rate-limit event per seven-day window
// figured; a negative utilization leaves that window unreported.
func fileWeeklyWindows(h *harness, sevenDay, opus, sonnet float64) {
	for _, w := range []struct {
		window      *conversationv1.SessionRateLimitType
		utilization float64
	}{
		{ratelimittest.SevenDay(), sevenDay},
		{ratelimittest.SevenDayOpus(), opus},
		{ratelimittest.SevenDaySonnet(), sonnet},
	} {
		if w.utilization >= 0 {
			h.r.OnSessionUpdate(testWS, rateLimitStatus(w.window, w.utilization, week))
		}
	}
}

func TestAnAllowancePastTheThresholdIsNewsworthy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	fileWeekly(h, 90)
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.Overage(), 12, 3*time.Hour))

	// Assert
	report := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if !report.GetWeekly().GetOverall().GetNewsworthy() {
		t.Fatalf("weekly allowance = %+v, want newsworthy at 90%%", report.GetWeekly().GetOverall())
	}
	if report.GetOverage().GetNewsworthy() {
		t.Fatalf("overage allowance = %+v, want unremarkable at 12%%", report.GetOverage())
	}
}

func TestTheNewsworthyThresholdIsInjectable(t *testing.T) {
	// Arrange
	h := newHarness(t, WithRateLimitNewsworthyThreshold(0.05))
	connected(h)

	// Act
	fileWeekly(h, 10)

	// Assert
	report := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if !report.GetWeekly().GetOverall().GetNewsworthy() {
		t.Fatalf("weekly allowance = %+v, want newsworthy under a 5%% threshold", report.GetWeekly().GetOverall())
	}
}

func TestAnAllowanceIsCarriedAsAFractionAndEpochSeconds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	fileWeekly(h, 90)

	// Assert
	got := enduringUsageOf(h).GetWeekly().GetOverall()
	if got.GetUtilization() != 0.9 {
		t.Fatalf("utilization = %v, want the 0..1 fraction the contract carries", got.GetUtilization())
	}
	if got.GetResetsAtS() != instant.Add(week).Unix() {
		t.Fatalf("resets_at_s = %d, want epoch SECONDS", got.GetResetsAtS())
	}
}

func TestAnAllowanceCopiesTheVendorsStatusArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	fileWeekly(h, 95)

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.SevenDay(), 95, 5*time.Hour))

	// Assert
	got := enduringUsageOf(h).GetWeekly().GetOverall()
	if got.GetAllowed() == nil {
		t.Fatalf("status = %+v, want the vendor's allowed arm copied", got.GetStatus())
	}
}

func TestAnAllowanceWarningStatusIsCopied(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	fileWeekly(h, 95)

	// Act
	update := rateLimitStatus(ratelimittest.SevenDay(), 95, 5*time.Hour)
	update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_AllowedWarning{
		AllowedWarning: &conversationv1.SessionRateLimitAllowedWarning{},
	}
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	got := enduringUsageOf(h).GetWeekly().GetOverall()
	if got.GetAllowedWarning() == nil {
		t.Fatalf("status = %+v, want the vendor's allowed_warning arm copied", got.GetStatus())
	}
}

func TestARejectedAllowanceStatusIsCopied(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	fileWeekly(h, 95)

	// Act
	update := rateLimitStatus(ratelimittest.SevenDay(), 95, 5*time.Hour)
	withVerdict(update, ratelimittest.Rejected)
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	// A rejected verdict blocks the session (ladder.RateLimitRejections), so the
	// allowance line stands under the block.
	got := enduringUsageOf(h).GetWeekly().GetOverall()
	if got.GetRejected() == nil {
		t.Fatalf("status = %+v, want the vendor's rejected arm copied", got.GetStatus())
	}
}

func TestAStatusTheVendorLeftUnsetDrawsNoArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	fileWeekly(h, 95)

	// Act
	update := rateLimitStatus(ratelimittest.SevenDay(), 95, 5*time.Hour)
	update.GetRateLimitStatus().Status = nil
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	got := enduringUsageOf(h).GetWeekly().GetOverall()
	if got.GetStatus() != nil {
		t.Fatalf("status = %+v, want no arm; an absent status is never defaulted", got.GetStatus())
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
		{name: "five hour is no drawn allowance", window: ratelimittest.FiveHour(), want: ""},
		{name: "seven day is the overall weekly allowance", window: ratelimittest.SevenDay(), want: "weekly"},
		{name: "seven day opus is the opus weekly allowance", window: ratelimittest.SevenDayOpus(), want: "weekly_opus"},
		{name: "seven day sonnet is the sonnet weekly allowance", window: ratelimittest.SevenDaySonnet(), want: "weekly_sonnet"},
		{name: "seven day overage included is no drawn allowance", window: ratelimittest.SevenDayOverageIncluded(), want: ""},
		{name: "overage is the overage allowance", window: ratelimittest.Overage(), want: "overage"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			h := newHarness(t)
			connected(h)
			// Every drawn window is figured by a statusless event, so it stands
			// with no verdict of its own.
			for _, window := range []*conversationv1.SessionRateLimitType{
				ratelimittest.SevenDay(), ratelimittest.SevenDayOpus(), ratelimittest.SevenDaySonnet(), ratelimittest.Overage(),
			} {
				h.r.OnSessionUpdate(testWS, withVerdict(rateLimitStatus(window, 10, 5*time.Hour), ratelimittest.Statusless))
			}

			// Act
			h.r.OnSessionUpdate(testWS, rateLimitStatus(tc.window, 95, 5*time.Hour))

			// Assert
			line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
			drawn := map[string]*frontendv1.FooterAllowance{
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

// withVerdict replaces a rate-limit event's status arm with VERDICT's
// (ratelimittest.Rejected, Allowed, AllowedWarning or Statusless).
func withVerdict(update *conversationv1.SessionUpdate,
	verdict func(*conversationv1.SessionRateLimitType) *conversationv1.SessionRateLimitStatus) *conversationv1.SessionUpdate {
	update.GetRateLimitStatus().Status = verdict(nil).Status
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
			fileWeeklyWindows(h, tc.sevenDay, tc.opus, tc.sonnet)

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
			h.r.OnSessionUpdate(testWS, withVerdict(rateLimitStatus(ratelimittest.SevenDay(), 100, time.Hour), ratelimittest.Rejected))

			// Act
			h.clock.Advance(tc.advance)

			// Assert
			got := enduringUsageOf(h).GetWeekly().GetOverall().GetRejected() != nil
			if got != tc.want {
				t.Fatalf("rejected drawn = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAVerdictNamingNoResetLapsesWithTheFiguresItWasFiledAgainst(t *testing.T) {
	// Arrange: the figured week resets in seven days; the rejection names no
	// reset and states no figure of its own.
	h := newHarness(t)
	connected(h)
	fileWeekly(h, 10)
	event := withVerdict(rateLimitStatus(ratelimittest.SevenDay(), 100, time.Hour), ratelimittest.Rejected)
	event.GetRateLimitStatus().ResetsAtMs = nil
	event.GetRateLimitStatus().UtilizationPercent = nil
	h.r.OnSessionUpdate(testWS, event)

	// Act
	h.clock.Advance(week + time.Minute)

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
	fileWeekly(h, 90)

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.Overage(), 42, 3*time.Hour))

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
	fileWeekly(h, 90)

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if line.GetOverage() != nil {
		t.Fatalf("overage = %+v, want no allowance for a window the vendor never reported", line.GetOverage())
	}
}

// THE LAST EVENT'S FIGURE IS DRAWN (owner ruling, 2026-10-13: only the most
// recent figure is drawn). The event carries no instant, and it is the only
// source, so the one to arrive last is the most recent.
func TestTheLastEventsFigureIsDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	fileWeekly(h, 90)

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.SevenDay(), 99, 5*time.Hour))

	// Assert
	got := enduringUsageOf(h).GetWeekly().GetOverall()
	if got.GetUtilization() != 0.99 {
		t.Fatalf("utilization = %v, want the last event's 0.99", got.GetUtilization())
	}
	if got.GetAllowed() == nil {
		t.Fatalf("status = %+v, want the event's verdict drawn with its figure", got.GetStatus())
	}
}

func TestAnEventAloneFiguresAWeeklyWindow(t *testing.T) {
	tests := []struct {
		name   string
		window *conversationv1.SessionRateLimitType
		drawn  func(*frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance
	}{
		{"seven day", ratelimittest.SevenDay(), func(u *frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance {
			return u.GetWeekly().GetOverall()
		}},
		{"seven day opus", ratelimittest.SevenDayOpus(), func(u *frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance {
			return u.GetWeekly().GetModels().GetOpus()
		}},
		{"seven day sonnet", ratelimittest.SevenDaySonnet(), func(u *frontendv1.FooterActivityEnduringUsage) *frontendv1.FooterAllowance {
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
			if got := tc.drawn(enduringUsageOf(h)).GetUtilization(); got != 0.5 {
				t.Fatalf("utilization = %v, want the event's 0.5", got)
			}
		})
	}
}

func TestAnEventNeverFiguresTheFiveHourWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.FiveHour(), 50, 5*time.Hour))

	// Assert
	if got := enduringUsageOf(h); got != nil {
		t.Fatalf("usage = %+v, want none: the five-hour window is not drawn", got)
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
			name:    "an event on a weekly window",
			arrange: func(*harness) {},
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.SevenDay(), 38, 5*time.Hour))
			},
			want:    dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.footer.account_usage_figure", Message: "a rate-limit event figured the window"},
			source:  "rate_limit_event",
			percent: 38,
		},
		{
			name:    "an event on the overage window",
			arrange: func(*harness) {},
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.Overage(), 12, 3*time.Hour))
			},
			want:    dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.footer.account_usage_figure", Message: "a rate-limit event figured the window"},
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

func TestAStatusNamingNoWindowIsDropped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, withVerdict(rateLimitStatus(ratelimittest.SevenDay(), 90, week), ratelimittest.Statusless))

	// Act: a status the vendor gave no window is not filable.
	h.r.OnSessionUpdate(testWS, rateLimitStatus(nil, 99, 5*time.Hour))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage()
	if line.GetWeekly().GetOverall().GetStatus() != nil || line.GetWeekly().GetOverall().GetUtilization() != 0.9 {
		t.Fatalf("weekly = %+v, want a windowless status filed nowhere", line.GetWeekly().GetOverall())
	}
}

// THE THRESHOLD IS A FRACTION AND THE VENDOR'S FIGURE IS A PERCENTAGE. An
// ordinary allowance must not be colored as a warning.
func TestAnAllowanceIsNewsworthyOnlyAboveTheThresholdOnceTheScalesAgree(t *testing.T) {
	tests := []struct {
		name           string
		weekly         float64
		wantNewsworthy bool
	}{
		{name: "an ordinary week is not news", weekly: 41, wantNewsworthy: false},
		{name: "an allowance past the threshold is", weekly: 88, wantNewsworthy: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			fileWeekly(h, tc.weekly)

			// Assert
			weekly := enduringUsageOf(h).GetWeekly().GetOverall()
			if weekly.GetNewsworthy() != tc.wantNewsworthy {
				t.Fatalf("weekly newsworthy = %v, want %v (allowance = %+v)", weekly.GetNewsworthy(), tc.wantNewsworthy, weekly)
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
	fileWeekly(h, 10)
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.Overage(), 5, 3*time.Hour))

	// Assert
	usage := enduringOf(t, h).GetUsage()
	if usage.GetOverage() == nil || usage.GetWeekly() == nil || usage.GetOverage().GetNewsworthy() || usage.GetWeekly().GetOverall().GetNewsworthy() {
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
		{name: "a usage figure: still usage", arrange: func(h *harness) { fileWeekly(h, 10) }, want: "usage"},
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

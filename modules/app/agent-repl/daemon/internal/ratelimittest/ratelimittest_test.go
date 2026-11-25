package ratelimittest

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/protoarm"
)

func TestEachWindowBuilderSetsItsOwnWindowArm(t *testing.T) {
	tests := []struct {
		name   string
		window *conversationv1.SessionRateLimitType
		want   string
	}{
		{name: "five hour", window: FiveHour(), want: "five_hour"},
		{name: "seven day", window: SevenDay(), want: "seven_day"},
		{name: "seven day opus", window: SevenDayOpus(), want: "seven_day_opus"},
		{name: "seven day sonnet", window: SevenDaySonnet(), want: "seven_day_sonnet"},
		{name: "seven day overage included", window: SevenDayOverageIncluded(), want: "seven_day_overage_included"},
		{name: "overage", window: Overage(), want: "overage"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := protoarm.Must(tt.window.ProtoReflect(), "window", "test")

			// Assert
			if string(got) != tt.want {
				t.Fatalf("window arm = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestEachVerdictBuilderSetsItsVerdictOnItsWindow(t *testing.T) {
	tests := []struct {
		name   string
		status *conversationv1.SessionRateLimitStatus
		want   string
	}{
		{name: "allowed", status: Allowed(SevenDay()), want: "allowed"},
		{name: "allowed with a warning", status: AllowedWarning(SevenDay()), want: "allowed_warning"},
		{name: "rejected", status: Rejected(SevenDay()), want: "rejected"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := protoarm.Must(tt.status.ProtoReflect(), "status", "test")

			// Assert
			if string(got) != tt.want || tt.status.GetRateLimitType().GetSevenDay() == nil {
				t.Fatalf("status arm = %q on %v, want %q on the seven-day window", got, tt.status.GetRateLimitType(), tt.want)
			}
		})
	}
}

func TestStatuslessCarriesItsWindowAndNoVerdict(t *testing.T) {
	// Act
	got := Statusless(Overage())

	// Assert
	if got.GetStatus() != nil || got.GetRateLimitType().GetOverage() == nil {
		t.Fatalf("status = %+v, want the overage window with no verdict", got)
	}
}

func TestFiguredSetsTheFiguresAndKeepsTheVerdict(t *testing.T) {
	// Arrange
	status := Rejected(SevenDay())

	// Act
	got := Figured(status, 42, 1_700_000_000_000)

	// Assert
	if got != status || got.GetUtilizationPercent() != 42 || got.GetResetsAtMs() != 1_700_000_000_000 || got.GetRejected() == nil {
		t.Fatalf("Figured = %+v, want the same status with 42%% resetting at 1700000000000 and its verdict kept", got)
	}
}

func TestUpdateWrapsTheStatus(t *testing.T) {
	// Arrange
	status := Rejected(FiveHour())

	// Act
	got := Update(status)

	// Assert
	if got.GetRateLimitStatus() != status {
		t.Fatalf("update = %+v, want the status wrapped as given", got)
	}
}

func TestPlanWindowCarriesItsFigures(t *testing.T) {
	// Act
	got := PlanWindow(8, 1_800_000_000_000)

	// Assert
	if got.GetUtilizationPercent() != 8 || got.GetResetsAtMs() != 1_800_000_000_000 {
		t.Fatalf("window = %+v, want 8%% resetting at 1800000000000", got)
	}
}

func TestPlanUsageFilesEachWindowUnderItsOwnField(t *testing.T) {
	// Arrange
	sevenDay, opus := PlanWindow(40, 1), PlanWindow(12, 2)

	// Act
	got := PlanUsage(sevenDay, opus, nil).GetPlanUsage()

	// Assert
	if got.GetSevenDay() != sevenDay || got.GetSevenDayOpus() != opus || got.SevenDaySonnet != nil {
		t.Fatalf("plan usage = %+v, want seven_day and opus as given and sonnet unset", got)
	}
}

// A PLAN-USAGE READ HAS ONE SPELLING in the daemon's tests, as an event does:
// a test file that builds one by hand fails here.
func TestTheSpendBuildersCarryTheirAmountsAndLimit(t *testing.T) {
	capLimit := int64(1_200_000)
	tests := []struct {
		name      string
		spend     *conversationv1.SessionPlanUsageMonthlySpend
		wantSpent int64
		wantLimit *int64
	}{
		{"capped", CappedSpend(199_396, 1_200_000), 199_396, &capLimit},
		{"uncapped", UncappedSpend(500), 500, nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			spent, capped, uncapped := tc.spend.GetSpent(), tc.spend.GetCapped(), tc.spend.GetUncapped()

			// Assert
			if spent.GetAmountMinor() != tc.wantSpent || spent.GetCurrency() != "USD" {
				t.Fatalf("spent = %+v, want %d USD", spent, tc.wantSpent)
			}
			switch {
			case tc.wantLimit == nil && (capped != nil || uncapped == nil):
				t.Fatalf("limit = %+v, want uncapped", tc.spend.GetLimit())
			case tc.wantLimit != nil && (capped.GetLimit().GetAmountMinor() != *tc.wantLimit || capped.GetLimit().GetCurrency() != "USD"):
				t.Fatalf("limit = %+v, want capped at %d USD", tc.spend.GetLimit(), *tc.wantLimit)
			}
		})
	}
}

func TestWithMonthlySpendSetsTheReadsSpend(t *testing.T) {
	// Arrange
	spend := UncappedSpend(500)

	// Act
	got := WithMonthlySpend(PlanUsage(nil, nil, nil), spend).GetPlanUsage()

	// Assert
	if got.GetMonthlySpend() != spend {
		t.Fatalf("monthly spend = %+v, want the one handed in", got.GetMonthlySpend())
	}
}

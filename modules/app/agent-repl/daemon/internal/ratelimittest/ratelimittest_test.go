package ratelimittest

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
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

// offendingTestFiles answers every daemon test file outside this package
// whose text contains any of NEEDLES: the one walk every spelling guard
// below shares. A test runs in its package's directory (runtime.Caller's file
// carries no absolute path under -trimpath).
func offendingTestFiles(t *testing.T, needles ...string) []string {
	t.Helper()
	here, err := os.Getwd()
	if err != nil {
		t.Fatalf("the working directory, which is this package's: %v", err)
	}
	root := filepath.Join(here, "..", "..")
	var offenders []string
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, "_test.go") || filepath.Dir(path) == here {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		text := string(body)
		for _, needle := range needles {
			if strings.Contains(text, needle) {
				offenders = append(offenders, path)
				break
			}
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk the daemon's tests: %v", err)
	}
	return offenders
}

// THE FIGURES HAVE ONE SPELLING in the daemon's tests, as the windows do: a
// test file that sets an event's utilization by hand fails here.
func TestNoTestHandRollsAnEventsFigures(t *testing.T) {
	// Act
	offenders := offendingTestFiles(t, "UtilizationPercent = &", "UtilizationPercent: &")

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("test files set a rate-limit event's figures by hand instead of through ratelimittest.Figured: %v", offenders)
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

// THE BUILDERS ARE THE ONE SPELLING of a rate-limit window in the daemon's
// tests: a test file that builds a window's oneof arm by hand fails here.
func TestNoTestHandRollsARateLimitWindow(t *testing.T) {
	// Act
	offenders := offendingTestFiles(t, "SessionRateLimitType_")

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("test files build a rate-limit window by hand instead of through ratelimittest: %v", offenders)
	}
}

// THE WALK FINDS A FILE THAT HOLDS A NEEDLE, so a guard above cannot pass by
// walking nothing.
func TestOffendingTestFilesFindsAFileHoldingTheNeedle(t *testing.T) {
	// Act: every daemon test file names the testing package.
	offenders := offendingTestFiles(t, "\"testing\"")

	// Assert
	if len(offenders) == 0 {
		t.Fatalf("the walk found no daemon test file importing testing; it walked nothing")
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
func TestNoTestHandRollsAPlanUsageRead(t *testing.T) {
	// Act
	offenders := offendingTestFiles(t, "SessionUpdate_PlanUsage{", "SessionPlanUsageWindow{")

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("test files build a plan-usage read by hand instead of through ratelimittest.PlanUsage: %v", offenders)
	}
}

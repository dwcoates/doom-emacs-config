package ratelimittest

import (
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
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
	// Arrange
	_, self, _, _ := runtime.Caller(0)
	here := filepath.Dir(self)
	root := filepath.Join(here, "..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
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
		if strings.Contains(string(body), "SessionRateLimitType_") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's tests: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("test files build a rate-limit window by hand instead of through ratelimittest: %v", offenders)
	}
}

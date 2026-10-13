package feed

import (
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE HEAD'S CLOCK: every tool card's clock is built by one of three
// builders, so every rule they share is pinned here once.

// clockOf states CLOCK as (start, end), end -1 while it runs, for comparison.
func clockOf(clock *frontendv1.FeedToolCardClock) (int64, int64) {
	if stopped := clock.GetStopped(); stopped != nil {
		return clock.GetStartedAtMs(), stopped.GetEndedAtMs()
	}
	return clock.GetStartedAtMs(), -1
}

func TestRunningClock(t *testing.T) {
	tests := []struct {
		name    string
		startMs int64
		wantNil bool
	}{
		{name: "a stated start runs from it", startMs: 1_000},
		{name: "no stated start draws no clock", startMs: 0, wantNil: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := runningClock(tc.startMs)

			// Assert.
			if (got == nil) != tc.wantNil {
				t.Fatalf("runningClock(%d) = %+v, want nil %v", tc.startMs, got, tc.wantNil)
			}
			if got != nil && (got.GetRunning() == nil || got.GetStartedAtMs() != tc.startMs) {
				t.Fatalf("runningClock(%d) = %+v, want running from %d", tc.startMs, got, tc.startMs)
			}
		})
	}
}

func TestStoppedClock(t *testing.T) {
	tests := []struct {
		name    string
		start   int64
		end     int64
		wantNil bool
	}{
		{name: "a span stops at its end", start: 1_000, end: 5_200},
		{name: "a zero-length span is a span", start: 1_000, end: 1_000},
		{name: "a missing start draws no clock", start: 0, end: 5_000, wantNil: true},
		{name: "a missing end draws no clock", start: 1_000, end: 0, wantNil: true},
		{name: "an end before the start draws no clock", start: 5_000, end: 1_000, wantNil: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := stoppedClock(tc.start, tc.end)

			// Assert.
			if (got == nil) != tc.wantNil {
				t.Fatalf("stoppedClock(%d, %d) = %+v, want nil %v", tc.start, tc.end, got, tc.wantNil)
			}
			if start, end := clockOf(got); got != nil && (start != tc.start || end != tc.end) {
				t.Fatalf("stoppedClock(%d, %d) spans %d..%d", tc.start, tc.end, start, end)
			}
		})
	}
}

func TestSettledClock(t *testing.T) {
	tests := []struct {
		name      string
		held      int64
		settled   *conversationv1.AgentActivitySettledAt
		wantNil   bool
		wantStart int64
		wantEnd   int64
	}{
		{name: "no settle instant draws no clock", held: 1_000, settled: nil, wantNil: true},
		{
			name: "the start held outranks the one the settle restates",
			held: 2_000,
			settled: &conversationv1.AgentActivitySettledAt{
				AtMs: 11_000, StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			},
			wantStart: 2_000, wantEnd: 11_000,
		},
		{
			name: "a replay holding no start counts from the one the settle restates",
			held: 0,
			settled: &conversationv1.AgentActivitySettledAt{
				AtMs: 11_000, StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			},
			wantStart: 1_000, wantEnd: 11_000,
		},
		{
			name:      "a settle restating no start stops the start held",
			held:      1_000,
			settled:   &conversationv1.AgentActivitySettledAt{AtMs: 3_000},
			wantStart: 1_000, wantEnd: 3_000,
		},
		{
			name:    "no start anywhere draws no clock",
			settled: &conversationv1.AgentActivitySettledAt{AtMs: 3_000},
			wantNil: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := settledClock(tc.held, tc.settled)

			// Assert.
			if (got == nil) != tc.wantNil {
				t.Fatalf("settledClock = %+v, want nil %v", got, tc.wantNil)
			}
			if start, end := clockOf(got); got != nil && (start != tc.wantStart || end != tc.wantEnd) {
				t.Fatalf("settledClock spans %d..%d, want %d..%d", start, end, tc.wantStart, tc.wantEnd)
			}
		})
	}
}

func TestToolCallsAndAsksShareOneStartPrecedence(t *testing.T) {
	// Arrange: the source of every settled card's clock start.
	src, err := os.ReadFile("toolcall.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act, Assert: the tool call's settle takes its start through clockStart,
	// the same rule settledClock applies to asks and skills.
	if !strings.Contains(string(src), "u.startedAtMs = clockStart(u.startedAtMs, settled)") {
		t.Fatal("settledOutcome does not take its start through clockStart")
	}
}

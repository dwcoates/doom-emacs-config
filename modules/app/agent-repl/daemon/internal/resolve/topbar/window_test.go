package topbar

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestTheWindowFillIsTheFigureOverTheVendorsWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, contextUsage(50_000, 200_000, 25, "claude-opus-5"))

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 0.25 {
		t.Errorf("window_fill = %v, want 0.25", got)
	}
}

func TestTheWindowFillAssumesAMillionTokensWhenTheVendorStatesNoWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, contextUsage(250_000, 0, 0, "claude-opus-5"))

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 0.25 {
		t.Errorf("window_fill = %v, want 0.25 of the assumed 1,000,000", got)
	}
}

func TestTheWindowFillIsClampedToOne(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, contextUsage(300_000, 200_000, 150, "claude-opus-5"))

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 1 {
		t.Errorf("window_fill = %v, want 1", got)
	}
}

func TestTheWindowFillIsZeroWithNoContext(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 0 {
		t.Errorf("window_fill = %v, want 0", got)
	}
}

func TestTheColdGatesFigureIsMeasuredAgainstTheAssumedWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Act.
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 100_000})

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 0.1 {
		t.Errorf("window_fill = %v, want 0.1", got)
	}
}

func TestTheWindowsSourceIsRecordedWhenItChanges(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Act.
	h.r.OnSessionStarted(testWS, sessionStarted("vend-1", "claude-opus-5"))
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{
			ContextUsage: &conversationv1.SessionContextUsage{TotalTokens: 1, MaxTokens: 200_000},
		},
	})

	// Assert.
	var sources []any
	for _, record := range h.log.Records() {
		if record.Operation == "daemon.topbar.context_window_source" {
			sources = append(sources, record.Context["source"])
		}
	}
	if len(sources) != 2 || sources[0] != windowSourceAssumed || sources[1] != windowSourceVendor {
		t.Errorf("sources = %v, want [assumed, vendor] once each", sources)
	}
}

func TestWindowFill(t *testing.T) {
	tests := []struct {
		name   string
		tokens int64
		window int64
		want   float64
	}{
		{"a fraction of the window", 50_000, 200_000, 0.25},
		{"an empty context", 0, 200_000, 0},
		{"exactly full", 200_000, 200_000, 1},
		{"past the window clamps to one", 300_000, 200_000, 1},
		{"a negative count clamps to zero", -5, 200_000, 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := WindowFill(tt.tokens, tt.window)

			// Assert.
			if got != tt.want {
				t.Errorf("WindowFill(%d, %d) = %v, want %v", tt.tokens, tt.window, got, tt.want)
			}
		})
	}
}

func TestWindowFillRefusesANonPositiveWindow(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Errorf("WindowFill(1, 0) did not panic")
		}
	}()

	// Act.
	WindowFill(1, 0)
}

func TestTheContextWindowIsTheVendorsWhenStated(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, contextUsage(50_000, 200_000, 25, "claude-opus-5"))

	// Assert.
	if got := h.r.ContextWindow(testWS); got != 200_000 {
		t.Errorf("ContextWindow = %d, want 200000", got)
	}
}

func TestTheContextWindowIsTheAssumedMillionWhenTheVendorStatesNone(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	got := h.r.ContextWindow(testWS)

	// Assert.
	if got != 1_000_000 {
		t.Errorf("ContextWindow = %d, want the assumed 1000000", got)
	}
}

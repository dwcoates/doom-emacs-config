package frontend

import (
	"testing"
	"time"
)

// fakeRefusalClock is a hand-advanced clock: the limiter's window must be
// exercised by moving time explicitly, never by waiting for it.
type fakeRefusalClock struct{ at time.Time }

func (c *fakeRefusalClock) now() time.Time      { return c.at }
func (c *fakeRefusalClock) add(d time.Duration) { c.at = c.at.Add(d) }

func TestClientLogRefusalLimiterFirstRefusalPerReason(t *testing.T) {
	tests := []struct {
		name      string
		workspace string
		reason    string
		wantFirst bool
	}{
		{name: "first refusal of the pair reports in full", workspace: "/ws/a", reason: "identity stale", wantFirst: true},
		{name: "same pair repeats without a full line", workspace: "/ws/a", reason: "identity stale", wantFirst: false},
		{name: "new reason on the same workspace reports in full", workspace: "/ws/a", reason: "workspace attribution", wantFirst: true},
		{name: "same reason on a new workspace reports in full", workspace: "/ws/b", reason: "identity stale", wantFirst: true},
	}
	// Arrange: one limiter observed across the whole table, since the state
	// under test is exactly what one refusal leaves behind for the next.
	clock := &fakeRefusalClock{at: time.Unix(0, 0)}
	limiter := newClientLogRefusalLimiter(clock.now, clientLogRefusalSummaryInterval)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := limiter.observe(tc.workspace, tc.reason)
			// Assert.
			if got.first != tc.wantFirst {
				t.Fatalf("observe(%q,%q).first = %v, want %v", tc.workspace, tc.reason, got.first, tc.wantFirst)
			}
		})
	}
}

func TestClientLogRefusalLimiterSummarizesAfterInterval(t *testing.T) {
	// Arrange: a first refusal opens the workspace's window.
	clock := &fakeRefusalClock{at: time.Unix(0, 0)}
	limiter := newClientLogRefusalLimiter(clock.now, 10*time.Second)
	if first := limiter.observe("/ws/a", "identity stale"); !first.first {
		t.Fatalf("opening refusal did not report as first: %+v", first)
	}
	// Act: three more refusals inside the window, then one past it.
	for i := 0; i < 3; i++ {
		clock.add(time.Second)
		if got := limiter.observe("/ws/a", "identity stale"); got.first || got.summary {
			t.Fatalf("in-window refusal %d logged: %+v", i, got)
		}
	}
	clock.add(10 * time.Second)
	got := limiter.observe("/ws/a", "identity stale")
	// Assert: the summary counts every refusal that got no line of its own,
	// and the running total counts the opening line too.
	if !got.summary {
		t.Fatalf("refusal past the interval did not summarize: %+v", got)
	}
	if got.suppressed != 4 {
		t.Fatalf("suppressed = %d, want 4", got.suppressed)
	}
	if got.total != 5 {
		t.Fatalf("total = %d, want 5", got.total)
	}
}

func TestClientLogRefusalLimiterPreservesCountsAcrossSummaries(t *testing.T) {
	// Arrange.
	clock := &fakeRefusalClock{at: time.Unix(0, 0)}
	limiter := newClientLogRefusalLimiter(clock.now, 10*time.Second)
	limiter.observe("/ws/a", "identity stale")
	// Act: 100 further refusals, one per second, so the ten-second window
	// closes repeatedly.
	summarized := 0
	lines := 1
	for i := 0; i < 100; i++ {
		clock.add(time.Second)
		got := limiter.observe("/ws/a", "identity stale")
		if got.summary {
			summarized += got.suppressed
			lines++
		}
	}
	// Assert: every refusal is accounted for — the ones a summary reported
	// plus the ones still waiting for the next — and the log carried an order
	// of magnitude fewer lines than there were refusals.
	final := limiter.observe("/ws/a", "identity stale")
	if final.total != 102 {
		t.Fatalf("total = %d, want 102", final.total)
	}
	if final.summary {
		t.Fatalf("refusal one second into a fresh window summarized: %+v", final)
	}
	if summarized != 100 {
		t.Fatalf("summarized = %d, want 100", summarized)
	}
	if lines != 11 {
		t.Fatalf("log lines = %d, want 11", lines)
	}
}

func TestClientLogRefusalReasonDropsPerRecordFacts(t *testing.T) {
	tests := []struct {
		name    string
		message string
		want    string
	}{
		{
			name:    "identity mismatch keys on its prose alone",
			message: `server: client log source session identity disagrees with the daemon registry: Claude session ID got="60f5" want="a1b2" workspace="/ws/a" request_id="r-1"`,
			want:    "server: client log source session identity disagrees with the daemon registry: Claude session ID",
		},
		{
			name:    "a different record of the same kind keys the same",
			message: `server: client log source session identity disagrees with the daemon registry: Claude session ID got="60f5" want="a1b2" workspace="/ws/a" request_id="r-2"`,
			want:    "server: client log source session identity disagrees with the daemon registry: Claude session ID",
		},
		{
			name:    "a fact-free message is its own reason",
			message: "client log request ID is required",
			want:    "client log request ID is required",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := clientLogRefusalReason(tc.message); got != tc.want {
				t.Fatalf("clientLogRefusalReason() = %q, want %q", got, tc.want)
			}
		})
	}
}

package footer

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
)

// vendorRetryScheduled is the shim stating its ATTEMPT-th retry at AT,
// retrying a failure worded STATUS.
func vendorRetryScheduled(attempt uint32, at time.Time, status string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_VendorErrorRetry{
		VendorErrorRetry: &conversationv1.SessionVendorErrorRetry{
			Scheduled: &conversationv1.SessionVendorErrorRetryScheduled{Attempt: attempt, NextAttemptAtMs: at.UnixMilli(), Status: status},
		},
	}}
}

// vendorRetryUnscheduled is the shim stating that no retry is due.
func vendorRetryUnscheduled() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_VendorErrorRetry{
		VendorErrorRetry: &conversationv1.SessionVendorErrorRetry{},
	}}
}

// overloadFailedTurn ends a turn on an overload, standing the vendor turn fault.
func overloadFailedTurn(h *harness) {
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	failTurn(h, overloaded(0))
}

// retryLine is the vendor fault's retrying line, nil when none stands.
func retryLine(t *testing.T, h *harness) *frontendv1.FooterStatusActivityRetrying {
	t.Helper()
	return h.view(t).GetStrip().GetStatus().GetVendorFault().GetActivity().GetSalient().GetRetrying()
}

func TestAScheduledRetryDrawsTheRetryLineInPlaceOfTheFailedTurnsSentence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	overloadFailedTurn(h)
	next := instant.Add(30 * time.Second)

	// Act
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(1, next, "Overloaded"))

	// Assert: the first retry is the second attempt, counted down to the
	// shim's own instant.
	line := retryLine(t, h)
	if line.GetAttempt() != 2 || line.GetNextAttempt().GetAtMs() != next.UnixMilli() || line.GetStatus() != "Overloaded" {
		t.Fatalf("retry line = %+v, want attempt 2 counting down to the shim's instant", line)
	}
}

func TestAScheduledRetryCarriesNoAttemptLimit(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	overloadFailedTurn(h)

	// Act
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(1, instant.Add(30*time.Second), "Overloaded"))

	// Assert
	if line := retryLine(t, h); line.MaxAttempt != nil {
		t.Fatalf("max attempt = %v, want none: agent-repl's retry is bounded by time", line.GetMaxAttempt())
	}
}

func TestAScheduledRetryStandsAsApiRetrying(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	overloadFailedTurn(h)

	// Act
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(1, instant.Add(30*time.Second), "Overloaded"))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetVendorFault().GetApiRetrying() == nil {
		t.Fatalf("status = %v, want vendor_fault · api_retrying", h.view(t).GetStrip().GetStatus())
	}
}

func TestAScheduledRetryUnderAUsageLimitKeepsTheUsageLimitStepAndCountsDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	usageLimited(h)
	reset := instant.Add(76 * time.Minute)

	// Act
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(1, reset, "usage limit reached"))

	// Assert
	fault := h.view(t).GetStrip().GetStatus().GetVendorFault()
	if fault.GetUsageLimit() == nil || fault.GetActivity().GetSalient().GetRetrying().GetNextAttempt().GetAtMs() != reset.UnixMilli() {
		t.Fatalf("status = %v, want usage_limit counting down to the reset", fault)
	}
}

func TestAnUnscheduledStatementTakesTheCountdownDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	overloadFailedTurn(h)
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(1, instant.Add(30*time.Second), "Overloaded"))

	// Act
	h.r.OnSessionUpdate(testWS, vendorRetryUnscheduled())

	// Assert: the failed turn's own sentence stands again.
	if retryLine(t, h) != nil || vendorTurnLine(h, t) == nil {
		t.Fatalf("activity = %q, want the failed turn's own line back", lineName(t, h))
	}
}

func TestASessionStartDropsTheScheduleOnHand(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	overloadFailedTurn(h)
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(1, instant.Add(30*time.Second), "Overloaded"))

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})

	// Assert
	if retryLine(t, h) != nil {
		t.Fatalf("activity = %q, want no retry line once a new session started", lineName(t, h))
	}
}

func TestAScheduledRetryNamingNoInstantIsRefusedAndTheScheduleOnHandStands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	overloadFailedTurn(h)
	next := instant.Add(30 * time.Second)
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(1, next, "Overloaded"))

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_VendorErrorRetry{
		VendorErrorRetry: &conversationv1.SessionVendorErrorRetry{Scheduled: &conversationv1.SessionVendorErrorRetryScheduled{Attempt: 2}},
	}})

	// Assert
	if got := retryLine(t, h).GetNextAttempt().GetAtMs(); got != next.UnixMilli() {
		t.Fatalf("next attempt = %d, want the schedule on hand kept", got)
	}
	if got := countOf(h.log.Records(), dlog.LevelError, "daemon.footer.vendor_retry_refused"); got != 1 {
		t.Fatalf("refusal ERROR records = %d, want 1", got)
	}
}

func TestAScheduledRetryNamingNoAttemptIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	overloadFailedTurn(h)

	// Act
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(0, instant.Add(30*time.Second), "Overloaded"))

	// Assert
	if retryLine(t, h) != nil {
		t.Fatalf("activity = %q, want no retry line from a refused statement", lineName(t, h))
	}
}

func TestAScheduledRetryIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	overloadFailedTurn(h)

	// Act
	h.r.OnSessionUpdate(testWS, vendorRetryScheduled(1, instant.Add(30*time.Second), "Overloaded"))

	// Assert
	if got := countOf(h.log.Records(), dlog.LevelInfo, "daemon.footer.vendor_retry"); got != 1 {
		t.Fatalf("vendor_retry INFO records = %d, want 1", got)
	}
}

func TestAttemptOfRetryCountsTheFirstRetryAsTheSecondAttempt(t *testing.T) {
	tests := []struct {
		name  string
		retry uint32
		want  int32
	}{
		{"the first retry", 1, 2},
		{"the tenth retry", 10, 11},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table. Act.
			got := attemptOfRetry(tt.retry)

			// Assert.
			if got != tt.want {
				t.Errorf("attemptOfRetry(%d) = %d, want %d", tt.retry, got, tt.want)
			}
		})
	}
}

// EVERY RETRY SCHEDULE THE FOOTER DRAWS CONVERTS ITS COUNT THROUGH ONE HELPER:
// a site adding one to a schedule's attempt by hand is a second spelling of
// the convention, which drifts.
func TestNoFooterSourceConvertsARetryCountByHand(t *testing.T) {
	// Arrange.
	sources, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act.
	var offenders []string
	for _, source := range sources {
		if strings.HasSuffix(source, "_test.go") {
			continue
		}
		body, err := os.ReadFile(source)
		if err != nil {
			t.Fatalf("read %s: %v", source, err)
		}
		if strings.Contains(string(body), "GetAttempt()) + 1") {
			offenders = append(offenders, source)
		}
	}

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("hand-converted retry counts in %v, want every site through attemptOfRetry", offenders)
	}
}

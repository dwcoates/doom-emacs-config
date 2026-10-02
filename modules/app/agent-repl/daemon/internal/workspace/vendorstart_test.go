package workspace

import (
	"context"
	"errors"
	"strconv"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/rollout"
	"claude-repld/internal/wsm"
)

func TestVendorRetryDelayFollowsTheOwnersSchedule(t *testing.T) {
	tests := []struct {
		failed uint32
		want   time.Duration
	}{
		{1, 200 * time.Millisecond},
		{2, 300 * time.Millisecond},
		{3, 450 * time.Millisecond},
		{4, 675 * time.Millisecond},
		{5, 1013 * time.Millisecond},
		{6, 1519 * time.Millisecond},
		{7, 2278 * time.Millisecond},
		{8, 3417 * time.Millisecond},
		{9, 5000 * time.Millisecond},
	}
	for _, tt := range tests {
		t.Run(strconv.Itoa(int(tt.failed)), func(t *testing.T) {
			// Arrange in the table. Act.
			got := vendorRetryDelay(tt.failed)

			// Assert.
			if got != tt.want {
				t.Fatalf("vendorRetryDelay(%d) = %s, want %s", tt.failed, got, tt.want)
			}
		})
	}
}

func TestVendorRetryDelayStaysAtTheCap(t *testing.T) {
	// Arrange / Act.
	got := vendorRetryDelay(40)

	// Assert.
	if got != 5*time.Second {
		t.Fatalf("vendorRetryDelay(40) = %s, want the 5s cap", got)
	}
}

// openKinds is the kinds of the faults standing in the fake store.
func openKinds(f *fleetFixture) map[string]int {
	out := map[string]int{}
	for _, fault := range f.db.dbFaults {
		out[fault.Kind]++
	}
	return out
}

func TestARetryableVendorStartIsRetriedUntilItStarts(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{
		vendorRefusal(retryableVendorStart(), "supportedModels did not answer in 3s"),
		vendorRefusal(retryableVendorStart(), "supportedModels did not answer in 3s"),
	}

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err != nil {
		t.Fatalf("Start = %v, want the session up on the third attempt", err)
	}
	if len(f.client.requests) != 3 {
		t.Fatalf("StartSession calls = %d, want 3", len(f.client.requests))
	}
}

func TestTheRetriesWaitOnTheBackoff(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{
		vendorRefusal(retryableVendorStart(), "silent"),
		vendorRefusal(retryableVendorStart(), "silent"),
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if len(f.retryWaits) != 2 || f.retryWaits[0] != 200*time.Millisecond || f.retryWaits[1] != 300*time.Millisecond {
		t.Fatalf("waits = %v, want [200ms 300ms]", f.retryWaits)
	}
}

func TestASuccessfulRetryLeavesNoVendorFaultStanding(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{
		vendorRefusal(retryableVendorStart(), "silent"),
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := openKinds(f); got[health.KindVendorStartRetrying] != 0 {
		t.Fatalf("open faults = %v, want no retrying fault once the session is up", got)
	}
}

// THE RETRYING FAULT IS REPLACED, never stacked: the second failure's fault
// is the only one standing, and it names the second attempt.
func TestTheRetryingFaultIsReplacedOnEachFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{
		vendorRefusal(retryableVendorStart(), "first"),
		vendorRefusal(retryableVendorStart(), "second"),
	}
	var standing []wsm.Fault
	f.retryAfter = func(time.Duration) <-chan time.Time {
		standing = append([]wsm.Fault(nil), f.db.dbFaults...)
		fired := make(chan time.Time, 1)
		fired <- f.now
		return fired
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert: as the second wait began.
	if len(standing) != 1 || standing[0].Kind != health.KindVendorStartRetrying {
		t.Fatalf("standing faults = %+v, want one retrying fault", standing)
	}
	if got := standing[0].Evidence[health.EvidenceFailedAttempts]; got != "2" {
		t.Fatalf("failed_attempts = %q, want 2", got)
	}
	if got := standing[0].Evidence[health.EvidenceCause]; got != "second" {
		t.Fatalf("cause = %q, want the latest attempt's", got)
	}
}

// THE WINDOW IS ANCHORED AT THE RUN'S FIRST FAILURE: a later attempt's fault
// keeps that instant, however much time the retries spent.
func TestTheRunsAnchorIsItsFirstFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{
		vendorRefusal(retryableVendorStart(), "first"),
		vendorRefusal(retryableVendorStart(), "second"),
	}
	var standing []wsm.Fault
	f.retryAfter = func(d time.Duration) <-chan time.Time {
		standing = append([]wsm.Fault(nil), f.db.dbFaults...)
		f.now = f.now.Add(d)
		fired := make(chan time.Time, 1)
		fired <- f.now
		return fired
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	want := strconv.FormatInt(fixedNow.UnixMilli(), 10)
	if len(standing) != 1 || standing[0].Evidence[health.EvidenceFailingSinceMs] != want {
		t.Fatalf("standing = %+v, want failing_since_ms %s on the second attempt", standing, want)
	}
}

// A SUCCESS ENDS THE RUN: the next failure opens a fresh window anchored at
// its own instant.
func TestASuccessResetsTheRunsAnchor(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.responses = []*shimv1.StartSessionResponse{
		vendorRefusal(retryableVendorStart(), "first run"),
		startedResponse("vendor-1"),
		vendorRefusal(retryableVendorStart(), "second run"),
	}
	if _, err := f.fleet.Resume(context.Background(), ws.ID, f.client); err != nil {
		t.Fatalf("first Resume: %v", err)
	}
	secondRunAt := f.now
	var standing []wsm.Fault
	f.retryAfter = func(time.Duration) <-chan time.Time {
		standing = append([]wsm.Fault(nil), f.db.dbFaults...)
		fired := make(chan time.Time, 1)
		fired <- f.now
		return fired
	}

	// Act.
	if _, err := f.fleet.Resume(context.Background(), ws.ID, f.client); err != nil {
		t.Fatalf("second Resume: %v", err)
	}

	// Assert.
	want := strconv.FormatInt(secondRunAt.UnixMilli(), 10)
	if len(standing) != 1 || standing[0].Evidence[health.EvidenceFailingSinceMs] != want ||
		standing[0].Evidence[health.EvidenceFailedAttempts] != "1" {
		t.Fatalf("standing = %+v, want a fresh run: attempt 1 anchored at %s", standing, want)
	}
}

func TestARejectedVendorStartIsNeverRetried(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil || len(f.client.requests) != 1 || len(f.retryWaits) != 0 {
		t.Fatalf("Start = %v after %d calls and waits %v, want one refused call and no retry", err, len(f.client.requests), f.retryWaits)
	}
}

func TestARejectedVendorStartFilesTheRejectionFault(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if len(f.db.dbFaults) != 1 || f.db.dbFaults[0].Kind != health.KindVendorStartRejected ||
		f.db.dbFaults[0].Evidence[health.EvidenceCause] != "invalid api key" {
		t.Fatalf("faults = %+v, want one vendor_start_rejected carrying the shim's cause", f.db.dbFaults)
	}
}

func TestARejectionAfterRetriesClosesTheRetryingFault(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{vendorRefusal(retryableVendorStart(), "silent")}
	f.client.response = vendorRefusal(rejectedVendorStart(), "model missing")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := openKinds(f); got[health.KindVendorStartRetrying] != 0 || got[health.KindVendorStartRejected] != 1 {
		t.Fatalf("open faults = %v, want the rejection alone", got)
	}
}

// AN UNLABELED vendor_start_failed IS MALFORMED: treated as a rejection, and
// said at ERROR.
func TestAnUnlabeledVendorStartFailureIsARejection(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(&shimv1.StartSessionVendorStartFailed{}, "no label")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if len(f.client.requests) != 1 || openKinds(f)[health.KindVendorStartRejected] != 1 {
		t.Fatalf("calls = %d, faults = %v, want one call and a rejection", len(f.client.requests), openKinds(f))
	}
}

func TestAnUnlabeledVendorStartFailureIsRecordedAtError(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(&shimv1.StartSessionVendorStartFailed{}, "no label")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelError && r.Context["invariant_violation"] == "StartSessionVendorStartFailed.retry is always set" {
			return
		}
	}
	t.Fatalf("records = %+v, want the malformed label at ERROR", f.log.logger.Records())
}

func TestAnUnavailableLockHolderIsRetried(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause: &shimv1.StartSessionFailure_LockHolderUnavailable{
				LockHolderUnavailable: &shimv1.StartSessionLockHolderUnavailable{Failure: exitedHolder()},
			},
		}},
	}}

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err != nil || len(f.client.requests) != 2 {
		t.Fatalf("Start = %v after %d calls, want the session up on the retry", err, len(f.client.requests))
	}
}

func TestAnOwnedConversationIsNotRetried(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause: &shimv1.StartSessionFailure_ConversationOwned{ConversationOwned: &shimv1.StartSessionConversationOwned{}},
		}},
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if len(f.client.requests) != 1 || openKinds(f)[health.KindResumeFailed] != 1 {
		t.Fatalf("calls = %d, faults = %v, want one call and resume_failed", len(f.client.requests), openKinds(f))
	}
}

func TestATransportErrorIsNotRetried(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.startErr = errors.New("unavailable: unexpected EOF")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if len(f.client.requests) != 1 || len(f.retryWaits) != 0 {
		t.Fatalf("calls = %d, waits = %v, want one call and no retry", len(f.client.requests), f.retryWaits)
	}
}

// THE WINDOW SPENT, the run stops: vendor_start_failed stands and the
// retrying fault is gone.
func TestAnExhaustedWindowFilesVendorStartFailed(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(retryableVendorStart(), "silent")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start = nil, want the exhausted run's failure")
	}
	if got := openKinds(f); got[health.KindVendorStartFailed] != 1 || got[health.KindVendorStartRetrying] != 0 {
		t.Fatalf("open faults = %v, want vendor_start_failed alone", got)
	}
}

func TestAnExhaustedWindowStopsAtTenMinutes(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(retryableVendorStart(), "silent")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert: the last attempt is the first one at or past the window.
	var waited time.Duration
	for _, d := range f.retryWaits {
		waited += d
	}
	if waited < DefaultVendorRetryWindow || waited-f.retryWaits[len(f.retryWaits)-1] >= DefaultVendorRetryWindow {
		t.Fatalf("waited %s over %d waits, want the run to stop on the first attempt past %s", waited, len(f.retryWaits), DefaultVendorRetryWindow)
	}
}

// A RESTART ENDS THE RUN: CancelVendorStart ends the wait, the bring-up
// answers the cancellation the relaunch engine relaunches over, and the
// retrying fault is closed.
func TestARestartCancelsTheRetryRun(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(retryableVendorStart(), "silent")
	waiting := make(chan struct{})
	f.retryAfter = func(time.Duration) <-chan time.Time {
		close(waiting)
		return make(chan time.Time) // never fires: the cancel ends the wait
	}
	result := make(chan error, 1)
	go func() { result <- f.fleet.Start(context.Background(), ws.ID) }()
	<-waiting

	// Act.
	cancelled := f.fleet.CancelVendorStart(context.Background(), ws.ID)

	// Assert.
	err := <-result
	if !cancelled || !errors.Is(err, ErrVendorStartCancelled) || !errors.Is(err, rollout.ErrResumeRestarted) {
		t.Fatalf("cancelled = %v, Start = %v, want the run ended by the restart", cancelled, err)
	}
	if got := openKinds(f); got[health.KindVendorStartRetrying] != 0 {
		t.Fatalf("open faults = %v, want the retrying fault closed", got)
	}
}

func TestCancelVendorStartWithNoRunReportsNone(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	got := f.fleet.CancelVendorStart(context.Background(), ws.ID)

	// Assert.
	if got {
		t.Fatal("CancelVendorStart = true, want false with no run in flight")
	}
}

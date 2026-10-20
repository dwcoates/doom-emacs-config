package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	shimv1 "agentrepl/proto/shim/v1"
)

// retried is the shim's answer that it made the retry.
func retried() *shimv1.RetryVendorErrorNowResponse {
	return &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_Retried{Retried: &shimv1.RetryVendorErrorNowRetried{}}}
}

func TestRetryVendorErrorAsksTheFleetForAnOwnedWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.retryResponse = retried()

	// Act.
	got, err := f.verbs.RetryVendorError(context.Background(), "w1")

	// Assert.
	if err != nil || got.GetRetried() == nil || len(f.fleet.retries) != 1 || f.fleet.retries[0] != "w1" {
		t.Fatalf("RetryVendorError = (%v, %v), fleet asks = %v; want the fleet's retried answer for w1", got, err, f.fleet.retries)
	}
}

func TestRetryVendorErrorRefusesAnUnknownWorkspaceWithoutAskingTheFleet(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.RetryVendorError(context.Background(), "absent")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.fleet.retries) != 0 {
		t.Fatalf("fleet asks = %v, want none", f.fleet.retries)
	}
}

func TestRetryVendorErrorSurfacesTheFleetsRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.retryErr = &ShimRefusal{Verb: "RetryVendorErrorNow", Arm: ArmShimNoSession, Detail: "no"}

	// Act.
	_, err := f.verbs.RetryVendorError(context.Background(), "w1")

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmShimNoSession {
		t.Fatalf("err = %v, want the fleet's no_session refusal", err)
	}
}

func TestFleetRetryVendorErrorRefusesNoSessionWithNoLiveShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	_, err := f.fleet.RetryVendorError(context.Background(), f.log.logger, ws.ID)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmShimNoSession {
		t.Fatalf("err = %v, want the no_session refusal", err)
	}
	if f.client.retryNowAsks != 0 {
		t.Errorf("shim asks = %d, want none", f.client.retryNowAsks)
	}
}

func TestFleetRetryVendorErrorRelaysTheShimsAnswer(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.retryNowResponse = retried()

	// Act.
	got, err := f.fleet.RetryVendorError(context.Background(), f.log.logger, ws.ID)

	// Assert.
	if err != nil || got.GetRetried() == nil || f.client.retryNowAsks != 1 {
		t.Fatalf("RetryVendorError = (%v, %v), asks = %d; want the retried answer from one ask", got, err, f.client.retryNowAsks)
	}
}

func TestFleetRetryVendorErrorRecordsTheShimsAnswerAtInfo(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.retryNowResponse = &shimv1.RetryVendorErrorNowResponse{
		Outcome: &shimv1.RetryVendorErrorNowResponse_Busy{Busy: &shimv1.RetryVendorErrorNowBusy{Detail: "a turn is in flight"}}}

	// Act.
	if _, err := f.fleet.RetryVendorError(context.Background(), f.log.logger, ws.ID); err != nil {
		t.Fatalf("RetryVendorError: %v", err)
	}

	// Assert.
	records := recordsAt(f.log, opRetryVendorError, "info")
	if len(records) != 1 || records[0]["outcome"] != "busy" || records[0]["detail"] != "a turn is in flight" {
		t.Fatalf("info records = %v, want one naming the busy outcome and its detail", records)
	}
}

func TestFleetRetryVendorErrorSurfacesAndRecordsATransportFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.retryNowErr = errors.New("socket closed")

	// Act.
	_, err := f.fleet.RetryVendorError(context.Background(), f.log.logger, ws.ID)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "socket closed") {
		t.Fatalf("err = %v, want the transport failure", err)
	}
	if records := recordsAt(f.log, opRetryVendorError, "error"); len(records) != 1 || records[0]["cause"] != "socket closed" {
		t.Errorf("error records = %v, want one carrying the cause", records)
	}
}

func TestFleetRetryVendorErrorRefusesAnAnswerNamingNoOutcome(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.retryNowResponse = &shimv1.RetryVendorErrorNowResponse{}

	// Act.
	_, err := f.fleet.RetryVendorError(context.Background(), f.log.logger, ws.ID)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no outcome") {
		t.Fatalf("err = %v, want the missing outcome named", err)
	}
	if records := recordsAt(f.log, opRetryVendorError, "error"); len(records) != 1 || records[0]["invariant_violation"] == nil {
		t.Errorf("error records = %v, want one naming the violated invariant", records)
	}
}

func TestRetryOutcomeNameNamesEachArm(t *testing.T) {
	tests := []struct {
		name     string
		response *shimv1.RetryVendorErrorNowResponse
		want     string
	}{
		{"retried", retried(), "retried"},
		{"nothing_scheduled", &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_NothingScheduled{NothingScheduled: &shimv1.RetryVendorErrorNowNothingScheduled{}}}, "nothing_scheduled"},
		{"busy", &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_Busy{Busy: &shimv1.RetryVendorErrorNowBusy{}}}, "busy"},
		{"ended", &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_Ended{Ended: &shimv1.RetryVendorErrorNowEnded{}}}, "ended"},
		{"unset", &shimv1.RetryVendorErrorNowResponse{}, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table. Act.
			got := retryOutcomeName(tt.response)

			// Assert.
			if got != tt.want {
				t.Errorf("retryOutcomeName = %q, want %q", got, tt.want)
			}
		})
	}
}

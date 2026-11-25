package server

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

func wrapWifi(v *agentreplv1.PersistentWifiState) *agentreplv1.WatchDaemonResponse {
	return &agentreplv1.WatchDaemonResponse{Push: &agentreplv1.WatchDaemonResponse_PersistentWifi{PersistentWifi: v}}
}

func TestStandingPushWrapsAValue(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	push, ended := standingPush(log, joinedOn(), true, "standing", wrapWifi)

	// Assert.
	if ended || push.GetPersistentWifi().GetOn() == nil {
		t.Fatalf("standingPush() = (%v, %v), want the wrapped standing", push, ended)
	}
}

func TestStandingPushEndsOnAClosedSubscription(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	push, ended := standingPush[*agentreplv1.PersistentWifiState](log, nil, false, "standing", wrapWifi)

	// Assert.
	if !ended || push != nil {
		t.Fatalf("standingPush() = (%v, %v), want ended with no frame", push, ended)
	}
}

func TestStandingPushRecordsAnEmptyValueAndSendsNothing(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	push, ended := standingPush[*agentreplv1.PersistentWifiState](log, nil, true, "persistent-wifi standing", wrapWifi)

	// Assert.
	if ended || push != nil {
		t.Fatalf("standingPush() = (%v, %v), want no frame and no end", push, ended)
	}
	records := log.Records()
	if len(records) != 1 || records[0].Level != "error" ||
		records[0].Message != "a publisher raised an empty persistent-wifi standing; it was not sent" {
		t.Fatalf("records = %+v, want one ERROR naming the empty standing", records)
	}
}

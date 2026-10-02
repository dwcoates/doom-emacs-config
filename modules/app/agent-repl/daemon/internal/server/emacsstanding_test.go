package server

import (
	"os"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
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

// TestEveryClientOnlyStandingTopicGoesThroughOneShape pins that the
// WatchDaemon loop takes each standing topic only one kind of client is told
// (the Emacs-only faults and persistent wifi, the webview-only news digest)
// through standingPush rather than a hand-rolled case of its own.
func TestEveryClientOnlyStandingTopicGoesThroughOneShape(t *testing.T) {
	// Arrange.
	src, err := os.ReadFile("streams.go")
	if err != nil {
		t.Fatalf("read streams.go: %v", err)
	}

	// Act.
	calls := strings.Count(string(src), "standingPush(s.log,")
	handRolled := strings.Count(string(src), `"a publisher raised an empty fault set`) +
		strings.Count(string(src), `"a publisher raised an empty persistent-wifi`) +
		strings.Count(string(src), `"a publisher raised an empty news digest`)

	// Assert.
	if calls != 3 || handRolled != 0 {
		t.Fatalf("standingPush call sites = %d (want 3), hand-rolled empty-value records = %d (want 0)", calls, handRolled)
	}
}

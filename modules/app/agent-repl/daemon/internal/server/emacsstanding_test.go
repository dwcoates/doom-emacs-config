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

func TestEmacsStandingPushWrapsAValue(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	push, ended := emacsStandingPush(log, joinedOn(), true, "standing", wrapWifi)

	// Assert.
	if ended || push.GetPersistentWifi().GetOn() == nil {
		t.Fatalf("emacsStandingPush() = (%v, %v), want the wrapped standing", push, ended)
	}
}

func TestEmacsStandingPushEndsOnAClosedSubscription(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	push, ended := emacsStandingPush[*agentreplv1.PersistentWifiState](log, nil, false, "standing", wrapWifi)

	// Assert.
	if !ended || push != nil {
		t.Fatalf("emacsStandingPush() = (%v, %v), want ended with no frame", push, ended)
	}
}

func TestEmacsStandingPushRecordsAnEmptyValueAndSendsNothing(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	push, ended := emacsStandingPush[*agentreplv1.PersistentWifiState](log, nil, true, "persistent-wifi standing", wrapWifi)

	// Assert.
	if ended || push != nil {
		t.Fatalf("emacsStandingPush() = (%v, %v), want no frame and no end", push, ended)
	}
	records := log.Records()
	if len(records) != 1 || records[0].Level != "error" ||
		records[0].Message != "a publisher raised an empty persistent-wifi standing; it was not sent" {
		t.Fatalf("records = %+v, want one ERROR naming the empty standing", records)
	}
}

// TestEveryEmacsOnlyStandingTopicGoesThroughOneShape pins that the WatchDaemon
// loop takes each Emacs-only standing topic through emacsStandingPush rather
// than a hand-rolled case of its own.
func TestEveryEmacsOnlyStandingTopicGoesThroughOneShape(t *testing.T) {
	// Arrange.
	src, err := os.ReadFile("streams.go")
	if err != nil {
		t.Fatalf("read streams.go: %v", err)
	}

	// Act.
	calls := strings.Count(string(src), "emacsStandingPush(s.log,")
	handRolled := strings.Count(string(src), `"a publisher raised an empty fault set`) +
		strings.Count(string(src), `"a publisher raised an empty persistent-wifi`)

	// Assert.
	if calls != 2 || handRolled != 0 {
		t.Fatalf("emacsStandingPush call sites = %d (want 2), hand-rolled empty-value records = %d (want 0)", calls, handRolled)
	}
}

package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

const daemonFaultTestOp = "daemon.workspace.daemon_fault_test"

// testDaemonFault is the fault the raiseDaemonFault tests raise.
func testDaemonFault() wsm.Fault {
	return wsm.Fault{Kind: "test_kind", Detail: "detail", Evidence: map[string]string{"key": "a"}}
}

// sameKey holds two faults the same when their `key` evidence matches.
func sameKey(f wsm.Fault) bool { return f.Evidence["key"] == "a" }

func TestRaiseDaemonFault(t *testing.T) {
	ws := ids.WorkspaceID("ws-1")
	tests := []struct {
		name     string
		standing []wsm.Fault
		want     int
	}{
		{name: "none standing opens the fault", want: 1},
		{name: "the same fault standing opens none", standing: []wsm.Fault{{ID: "f-1", Kind: "test_kind", Evidence: map[string]string{"key": "a"}}}, want: 1},
		{name: "another fault of the kind standing opens its own", standing: []wsm.Fault{{ID: "f-1", Kind: "test_kind", Evidence: map[string]string{"key": "b"}}}, want: 2},
		{name: "a workspace's fault of the kind standing opens the daemon's", standing: []wsm.Fault{{ID: "f-1", Kind: "test_kind", Workspace: &ws, Evidence: map[string]string{"key": "a"}}}, want: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.health.opened = append(f.health.opened, tt.standing...)
			v := f.verbs.(*verbs)

			// Act
			v.raiseDaemonFault(context.Background(), f.log.Global(), daemonFaultTestOp, testDaemonFault(), sameKey, nil)

			// Assert
			if len(f.health.opened) != tt.want {
				t.Fatalf("standing faults = %+v, want %d", f.health.opened, tt.want)
			}
			for _, opened := range f.health.opened[len(tt.standing):] {
				if opened.Kind != "test_kind" || opened.Workspace != nil {
					t.Fatalf("opened fault = %+v, want a daemon-scoped test_kind fault", opened)
				}
			}
		})
	}
}

func TestRaiseDaemonFaultRecordsWhatFailed(t *testing.T) {
	tests := []struct {
		name     string
		fail     func(h *fakeHealth)
		message  string
		wantOpen int
	}{
		{name: "a failed read is recorded and the fault opened anyway", fail: func(h *fakeHealth) { h.listErr = errors.New("the state database is locked") },
			message: "could not check whether the daemon fault already stands; it is recorded anyway", wantOpen: 1},
		{name: "a failed open is recorded", fail: func(h *fakeHealth) { h.openErr = errors.New("the state database is locked") },
			message: "could not record the daemon fault", wantOpen: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.fail(f.health)
			v := f.verbs.(*verbs)

			// Act
			v.raiseDaemonFault(context.Background(), f.log.Global(), daemonFaultTestOp, testDaemonFault(), sameKey, dlog.Context{"brief": "b"})

			// Assert
			if len(f.health.opened) != tt.wantOpen {
				t.Fatalf("opened faults = %+v, want %d", f.health.opened, tt.wantOpen)
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: daemonFaultTestOp})
			if !ok || record.Message != tt.message || record.Context["kind"] != "test_kind" || record.Context["brief"] != "b" || record.Context["cause"] == nil {
				t.Fatalf("records = %+v, want an ERROR %q with the kind, the caller's fields and the cause", f.log.logger.Records(), tt.message)
			}
		})
	}
}

// TestEveryDaemonFaultRaiserSharesRaiseDaemonFault pins that a fault the verbs
// raise once per condition goes through the one helper: a hand-rolled
// read-then-open would drift from it (another dedupe, a record left out).
func TestEveryDaemonFaultRaiserSharesRaiseDaemonFault(t *testing.T) {
	// Arrange / Act
	shared := sourcescan.Count(t, "v.raiseDaemonFault(")
	var byHand []string
	for _, file := range sourcescan.Production(t) {
		if file.Name != "daemonfault.go" && strings.Contains(string(file.Source), "Health.OpenFaults(ctx, wsm.FaultScope{Kind:") {
			byHand = append(byHand, file.Name)
		}
	}

	// Assert
	if shared != 1 || len(byHand) != 0 {
		t.Fatalf("shared raisers = %d, hand-rolled in %v; want 1 and none", shared, byHand)
	}
}

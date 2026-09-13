package main

import (
	"context"
	"errors"
	"fmt"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// fakeReporter records the fault opens and closes the lifecycle sink makes.
type fakeReporter struct {
	standing []wsm.Fault
	opened   []wsm.Fault
	closed   []ids.FaultID
	next     int
	// openErr scripts the state client's refusal of an OpenFault.
	openErr error
}

func (r *fakeReporter) Daemon(context.Context) (*agentreplv1.DaemonHealthResponse, error) {
	return nil, fmt.Errorf("not used")
}

func (r *fakeReporter) Session(context.Context, ids.WorkspaceID) (*agentreplv1.SessionHealthResponse, error) {
	return nil, fmt.Errorf("not used")
}

func (r *fakeReporter) OpenFault(_ context.Context, f wsm.Fault) (ids.FaultID, error) {
	if r.openErr != nil {
		return "", r.openErr
	}
	r.next++
	id := ids.FaultID(fmt.Sprintf("f%d", r.next))
	f.ID = id
	r.opened = append(r.opened, f)
	return id, nil
}

func (r *fakeReporter) CloseFault(_ context.Context, id ids.FaultID) error {
	r.closed = append(r.closed, id)
	return nil
}

func (r *fakeReporter) OpenFaults(context.Context, wsm.FaultScope) ([]wsm.Fault, error) {
	return r.standing, nil
}

func newDiagnosticsSink(t *testing.T, reporter health.Reporter) *lifecycleSink {
	t.Helper()
	ref := &healthForwarder{}
	ref.bind(reporter)
	return &lifecycleSink{health: ref, log: dlog.NewTestLogger()}
}

func unhealthyDiagnostics(faults ...*conversationv1.SessionFault) *conversationv1.SessionDiagnostics {
	return &conversationv1.SessionDiagnostics{
		Health: &conversationv1.SessionDiagnostics_Unhealthy{
			Unhealthy: &conversationv1.SessionUnhealthy{Faults: faults},
		},
	}
}

func TestAnUnhealthyDiagnosticsPushOpensAShimReportedFault(t *testing.T) {
	// Arrange
	reporter := &fakeReporter{}
	sink := newDiagnosticsSink(t, reporter)

	// Act
	sink.OnSessionDiagnostics("ws-1", unhealthyDiagnostics(&conversationv1.SessionFault{
		Component: "store client",
		Detail:    "the store socket went away",
		Kind: &conversationv1.SessionFault_StoreUnreachable{
			StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{},
		},
	}))

	// Assert
	if len(reporter.opened) != 1 {
		t.Fatalf("opened %d faults, want exactly 1", len(reporter.opened))
	}
	got := reporter.opened[0]
	if got.Kind != health.KindShimReported {
		t.Fatalf("the opened fault's kind = %q, want %q", got.Kind, health.KindShimReported)
	}
	if got.Evidence["component"] != "store client" || got.Evidence["kind"] != "store_unreachable" {
		t.Fatalf("the opened fault's evidence = %v, want the shim's component and kind", got.Evidence)
	}
}

func TestAHealthyDiagnosticsPushRetractsTheStandingShimReportedFault(t *testing.T) {
	// Arrange
	ws := wsm.WorkspaceID("ws-1")
	reporter := &fakeReporter{standing: []wsm.Fault{
		{ID: "f-old", Workspace: &ws, Kind: health.KindShimReported},
	}}
	sink := newDiagnosticsSink(t, reporter)

	// Act
	sink.OnSessionDiagnostics("ws-1", &conversationv1.SessionDiagnostics{
		Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
	})

	// Assert
	if len(reporter.closed) != 1 || reporter.closed[0] != "f-old" {
		t.Fatalf("closed %v, want exactly the standing shim-reported fault", reporter.closed)
	}
	if len(reporter.opened) != 0 {
		t.Fatalf("opened %v, want nothing on a healthy verdict", reporter.opened)
	}
}

func TestADiagnosticsPushLeavesAFaultOfAnotherKindStanding(t *testing.T) {
	// Arrange
	ws := wsm.WorkspaceID("ws-1")
	reporter := &fakeReporter{standing: []wsm.Fault{
		{ID: "f-link", Workspace: &ws, Kind: health.KindLinkSevered},
	}}
	sink := newDiagnosticsSink(t, reporter)

	// Act
	sink.OnSessionDiagnostics("ws-1", &conversationv1.SessionDiagnostics{
		Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
	})

	// Assert
	if len(reporter.closed) != 0 {
		t.Fatalf("closed %v, want a fault the shim never reported left standing", reporter.closed)
	}
}

func TestADiagnosticsPushWithNoBoundReporterIsRecordedNotDropped(t *testing.T) {
	// Arrange
	sink := &lifecycleSink{health: &healthForwarder{}, log: dlog.NewTestLogger()}

	// Act / Assert: an unbound forwarder must not panic; the boot-order defect
	// is surfaced through the error record instead.
	sink.OnSessionDiagnostics("ws-1", unhealthyDiagnostics())
}

// TestOnLinkFaultLevelsAForgottenWorkspaceAtDebug is the outermost layer of the
// shim-death cascade. This sink outlives the registry row, so a shim dying
// after its workspace was forgotten reaches it about a workspace nothing can
// carry a fault for -- three ERRORs deep, once per death, for a condition with
// nothing to remediate.
func TestOnLinkFaultLevelsAForgottenWorkspaceAtDebug(t *testing.T) {
	tests := []struct {
		name      string
		openErr   error
		wantLevel string
	}{
		{
			name:      "the workspace was forgotten",
			openErr:   fmt.Errorf("health: open fault %q: %w", "shim_died", wsm.ErrNotFound),
			wantLevel: "debug",
		},
		{
			name:      "the state client failed",
			openErr:   errors.New("disk is gone"),
			wantLevel: "error",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			sink := newDiagnosticsSink(t, &fakeReporter{openErr: tt.openErr})
			log, ok := sink.log.(*dlog.TestLogger)
			if !ok {
				t.Fatalf("sink logger is %T, want *dlog.TestLogger", sink.log)
			}

			// Act.
			sink.OnLinkFault("w1", sessionwatcher.LinkFault{
				Kind: sessionwatcher.LinkFaultDead, Detail: "the shim process is gone",
			})

			// Assert.
			var level string
			for _, record := range log.Records() {
				if record.Operation == "daemon.cmd.lifecycle" {
					level = record.Level
				}
			}
			if level != tt.wantLevel {
				t.Fatalf("lifecycle record level = %q, want %q", level, tt.wantLevel)
			}
		})
	}
}

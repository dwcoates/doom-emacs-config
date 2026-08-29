package health

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

func TestNewRefusesMissingCollaborators(t *testing.T) {
	tests := []struct {
		name string
		deps Deps
	}{
		{name: "no state client", deps: Deps{Live: alwaysLive, Log: newStubSurfaces()}},
		{name: "no liveness probe", deps: Deps{DB: &stubDB{}, Log: newStubSurfaces()}},
		{name: "no log surfaces", deps: Deps{DB: &stubDB{}, Live: alwaysLive}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			_, err := New(tt.deps)
			// Assert.
			if err == nil {
				t.Fatalf("New(%s) = nil error, want a refusal", tt.name)
			}
		})
	}
}

func TestDaemonHealthyWithNoDaemonScopeFaults(t *testing.T) {
	// Arrange.
	db := &stubDB{}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	got, err := r.Daemon(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Daemon: %v", err)
	}
	if got.GetSuccess().GetHealthy() == nil {
		t.Fatalf("Daemon() = %v, want the healthy arm", got)
	}
}

func TestDaemonUnhealthyReportsDaemonScopeFault(t *testing.T) {
	// Arrange.
	db := &stubDB{faults: []wsm.Fault{{Kind: "log_sink_poisoned", Detail: "fd 3 closed"}}}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	got, err := r.Daemon(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Daemon: %v", err)
	}
	faults := got.GetSuccess().GetUnhealthy().GetFaults()
	if len(faults) != 1 || faults[0].GetDetail() != "log_sink_poisoned: fd 3 closed" {
		t.Fatalf("Daemon() faults = %v, want one rendered daemon fault", faults)
	}
}

func TestDaemonIgnoresWorkspaceScopedFault(t *testing.T) {
	// Arrange: a workspace-bound fault is SessionHealth's answer, never the
	// daemon's.
	ws := ids.WorkspaceID("w1")
	db := &stubDB{faults: []wsm.Fault{{Kind: "store_unreachable", Workspace: &ws}}}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	got, err := r.Daemon(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Daemon: %v", err)
	}
	if got.GetSuccess().GetHealthy() == nil {
		t.Fatalf("Daemon() = %v, want the healthy arm", got)
	}
}

func TestDaemonSelfCheckAnswersUnhealthyWhenStateUnreadable(t *testing.T) {
	// Arrange: the liveness self-check's own failure is an ANSWER.
	db := &stubDB{faultsErr: errors.New("database is locked")}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	got, err := r.Daemon(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Daemon() returned an error; unhealthy is an answer: %v", err)
	}
	faults := got.GetSuccess().GetUnhealthy().GetFaults()
	if len(faults) != 1 || !strings.HasPrefix(faults[0].GetDetail(), FaultKindStateUnreadable) {
		t.Fatalf("Daemon() faults = %v, want the state-unreadable self-check fault", faults)
	}
}

func TestSessionUnknownWorkspaceIsAnError(t *testing.T) {
	// Arrange: an unknown workspace has no health to report.
	db := &stubDB{workspaces: map[ids.WorkspaceID]wsm.Workspace{}}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	_, err := r.Session(context.Background(), "nope")

	// Assert.
	if err == nil {
		t.Fatal("Session(unknown) = nil error, want an error")
	}
}

func TestSessionHealthyWhenLiveConnectedAndFaultless(t *testing.T) {
	// Arrange.
	db := &stubDB{workspaces: map[ids.WorkspaceID]wsm.Workspace{"w1": {ID: "w1", Dir: "/w1"}}}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	got, err := r.Session(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if got.GetSuccess().GetHealthy() == nil {
		t.Fatalf("Session() = %v, want the healthy arm", got)
	}
}

func TestSessionAbsentSessionIsAnUnhealthyAnswer(t *testing.T) {
	// Arrange.
	db := &stubDB{workspaces: map[ids.WorkspaceID]wsm.Workspace{"w1": {ID: "w1", Dir: "/w1"}}}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())
	rr := r.(*reporter)
	rr.live = func(ids.WorkspaceID) (bool, bool) { return false, false }

	// Act.
	got, err := rr.Session(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	faults := got.GetSuccess().GetUnhealthy().GetFaults()
	if len(faults) != 1 || !strings.HasPrefix(faults[0].GetDetail(), FaultKindSessionAbsent) {
		t.Fatalf("Session() faults = %v, want the session-absent fault", faults)
	}
}

func TestSessionSeveredLinkIsAnUnhealthyAnswer(t *testing.T) {
	// Arrange.
	db := &stubDB{workspaces: map[ids.WorkspaceID]wsm.Workspace{"w1": {ID: "w1", Dir: "/w1"}}}
	r := newReporter(t, db, func(ids.WorkspaceID) (bool, bool) { return true, false }, newStubSurfaces())

	// Act.
	got, err := r.Session(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	faults := got.GetSuccess().GetUnhealthy().GetFaults()
	if len(faults) != 1 || !strings.HasPrefix(faults[0].GetDetail(), FaultKindLinkSevered) {
		t.Fatalf("Session() faults = %v, want the link-severed fault", faults)
	}
}

func TestSessionReportsTheWorkspaceScopedFaults(t *testing.T) {
	// Arrange.
	ws := ids.WorkspaceID("w1")
	db := &stubDB{
		workspaces: map[ids.WorkspaceID]wsm.Workspace{ws: {ID: ws, Dir: "/w1"}},
		faults:     []wsm.Fault{{Kind: "store_unreachable", Detail: "dial refused", Workspace: &ws}},
	}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	got, err := r.Session(context.Background(), ws)

	// Assert.
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	faults := got.GetSuccess().GetUnhealthy().GetFaults()
	if len(faults) != 1 || faults[0].GetDetail() != "store_unreachable: dial refused" {
		t.Fatalf("Session() faults = %v, want the rendered workspace fault", faults)
	}
}

func TestSessionScopesTheFaultQueryToTheWorkspace(t *testing.T) {
	// Arrange.
	db := &stubDB{workspaces: map[ids.WorkspaceID]wsm.Workspace{"w1": {ID: "w1", Dir: "/w1"}}}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	if _, err := r.Session(context.Background(), "w1"); err != nil {
		t.Fatalf("Session: %v", err)
	}

	// Assert.
	if len(db.scopes) != 1 || db.scopes[0].Workspace == nil || *db.scopes[0].Workspace != "w1" {
		t.Fatalf("OpenFaults scopes = %v, want one scoped to w1", db.scopes)
	}
}

func TestSessionUnresolvableWorkspaceSinkIsSurfaced(t *testing.T) {
	// Arrange: failing to resolve a KNOWN workspace's sink is an invariant
	// violation, never a global write.
	db := &stubDB{workspaces: map[ids.WorkspaceID]wsm.Workspace{"w1": {ID: "w1", Dir: "/w1"}}}
	log := newStubSurfaces()
	log.workspaceErr = errors.New("symlink target missing")
	r := newReporter(t, db, alwaysLive, log)

	// Act.
	_, err := r.Session(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Session() = nil error, want the unresolvable-sink failure surfaced")
	}
}

func TestOpenFaultRefusesAKindlessFault(t *testing.T) {
	// Arrange.
	r := newReporter(t, &stubDB{}, alwaysLive, newStubSurfaces())

	// Act.
	_, err := r.OpenFault(context.Background(), wsm.Fault{Detail: "something"})

	// Assert.
	if err == nil {
		t.Fatal("OpenFault(no kind) = nil error, want a refusal")
	}
}

func TestOpenFaultStampsTheOpenedAtWhenAbsent(t *testing.T) {
	// Arrange.
	db := &stubDB{openedID: "f1"}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	id, err := r.OpenFault(context.Background(), wsm.Fault{Kind: "keepalive_failed"})

	// Assert.
	if err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	if id != "f1" {
		t.Fatalf("OpenFault() id = %q, want f1", id)
	}
	if !db.openedFault.OpenedAt.Equal(fixedNow) {
		t.Fatalf("OpenedAt = %v, want %v", db.openedFault.OpenedAt, fixedNow)
	}
}

func TestOpenFaultKeepsASuppliedOpenedAt(t *testing.T) {
	// Arrange.
	supplied := fixedNow.Add(-time.Hour)
	db := &stubDB{openedID: "f1"}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	if _, err := r.OpenFault(context.Background(), wsm.Fault{Kind: "k", OpenedAt: supplied}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Assert.
	if !db.openedFault.OpenedAt.Equal(supplied) {
		t.Fatalf("OpenedAt = %v, want the supplied %v", db.openedFault.OpenedAt, supplied)
	}
}

func TestCloseFaultRefusesAnUnnamedFault(t *testing.T) {
	// Arrange.
	r := newReporter(t, &stubDB{}, alwaysLive, newStubSurfaces())

	// Act.
	err := r.CloseFault(context.Background(), "")

	// Assert.
	if err == nil {
		t.Fatal("CloseFault(\"\") = nil error, want a refusal")
	}
}

func TestCloseFaultStampsTheResolvedAt(t *testing.T) {
	// Arrange.
	db := &stubDB{}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	if err := r.CloseFault(context.Background(), "f9"); err != nil {
		t.Fatalf("CloseFault: %v", err)
	}

	// Assert.
	if db.closedID != "f9" || !db.closedAt.Equal(fixedNow) {
		t.Fatalf("CloseFault stamped (%q, %v), want (f9, %v)", db.closedID, db.closedAt, fixedNow)
	}
}

func TestOpenFaultsSurfacesTheReadFailure(t *testing.T) {
	// Arrange.
	db := &stubDB{faultsErr: errors.New("corrupt row")}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	_, err := r.OpenFaults(context.Background(), wsm.FaultScope{})

	// Assert.
	if err == nil {
		t.Fatal("OpenFaults() = nil error, want the read failure surfaced")
	}
}

func TestOpenFaultsPassesTheScopeThrough(t *testing.T) {
	// Arrange.
	db := &stubDB{}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())
	scope := wsm.FaultScope{Kind: "converter_defect"}

	// Act.
	if _, err := r.OpenFaults(context.Background(), scope); err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}

	// Assert.
	if len(db.scopes) != 1 || db.scopes[0].Kind != "converter_defect" {
		t.Fatalf("OpenFaults scopes = %v, want the kind passed through", db.scopes)
	}
}

func TestDaemonLogsTheHealthyBranch(t *testing.T) {
	// Arrange.
	log := newStubSurfaces()
	r := newReporter(t, &stubDB{}, alwaysLive, log)

	// Act.
	if _, err := r.Daemon(context.Background()); err != nil {
		t.Fatalf("Daemon: %v", err)
	}

	// Assert.
	if !hasOperation(log.logger.Records(), opDaemon) {
		t.Fatalf("records = %v, want one under %s", log.logger.Records(), opDaemon)
	}
}

func hasOperation(records []dlog.Record, operation string) bool {
	for _, r := range records {
		if r.Operation == operation {
			return true
		}
	}
	return false
}

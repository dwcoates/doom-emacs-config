package health

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

func TestNewRefusesMissingCollaborators(t *testing.T) {
	complete := func() Deps {
		return Deps{
			DB: &stubDB{}, Live: alwaysLive, Log: newStubSurfaces(),
			Instance: "daemon-test", PID: 4242,
			BuildSHA: func() (string, error) { return "test-build", nil },
		}
	}
	tests := []struct {
		name string
		omit func(*Deps)
	}{
		{name: "no state client", omit: func(d *Deps) { d.DB = nil }},
		{name: "no liveness probe", omit: func(d *Deps) { d.Live = nil }},
		{name: "no log surfaces", omit: func(d *Deps) { d.Log = nil }},
		{name: "no instance id", omit: func(d *Deps) { d.Instance = "" }},
		{name: "no positive pid", omit: func(d *Deps) { d.PID = 0 }},
		{name: "no build sha reader", omit: func(d *Deps) { d.BuildSHA = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			deps := complete()
			tt.omit(&deps)
			// Act.
			_, err := New(deps)
			// Assert.
			if err == nil {
				t.Fatalf("New(%s) = nil error, want a refusal", tt.name)
			}
		})
	}
}

func TestDaemonHealthCarriesTheServingProcessIdentity(t *testing.T) {
	// Arrange.
	r := newReporter(t, &stubDB{}, alwaysLive, newStubSurfaces())

	// Act.
	got, err := r.Daemon(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Daemon: %v", err)
	}
	identity := got.GetSuccess().GetIdentity()
	if identity.GetInstanceId() != "daemon-test" || identity.GetPid() != 4242 || identity.GetBuildSha() != "test-build" {
		t.Fatalf("Daemon() identity = %v, want daemon-test pid 4242 at test-build", identity)
	}
}

func TestDaemonHealthRefusesAnUnreadableBuildIdentity(t *testing.T) {
	// Arrange.
	deps := Deps{
		DB: &stubDB{}, Live: alwaysLive, Log: newStubSurfaces(),
		Instance: "daemon-test", PID: 4242,
		BuildSHA: func() (string, error) { return "", errors.New("stamp denied") },
	}

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "stamp denied") {
		t.Fatalf("New() error = %v, want the build stamp failure", err)
	}
}

func TestDaemonHealthSnapshotsTheBuildIdentityAtProcessBoot(t *testing.T) {
	// Arrange.
	reads := 0
	r, err := New(Deps{
		DB: &stubDB{}, Live: alwaysLive, Log: newStubSurfaces(),
		Instance: "daemon-test", PID: 4242,
		BuildSHA: func() (string, error) {
			reads++
			return "boot-build", nil
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	first, firstErr := r.Daemon(context.Background())
	second, secondErr := r.Daemon(context.Background())

	// Assert.
	if firstErr != nil || secondErr != nil {
		t.Fatalf("Daemon errors = first %v, second %v; want successes", firstErr, secondErr)
	}
	if reads != 1 {
		t.Fatalf("build identity reads = %d, want one boot-time snapshot", reads)
	}
	if first.GetSuccess().GetIdentity().GetBuildSha() != "boot-build" ||
		second.GetSuccess().GetIdentity().GetBuildSha() != "boot-build" {
		t.Fatalf("health identities = first %v, second %v; want immutable boot-build", first, second)
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
	if len(faults) != 1 || !strings.HasPrefix(faults[0].GetDetail(), KindStateUnreadable) {
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
	// The session-absent kind has its OWN `SessionFault.kind' arm as of
	// 2026-09-12, so the probe's answer now reaches the caller instead of
	// being withheld for want of one.
	if got.GetSuccess().GetUnhealthy() == nil {
		t.Fatalf("Session() = %v, want the unhealthy arm", got.GetSuccess().GetHealth())
	}
	faults := got.GetSuccess().GetUnhealthy().GetFaults()
	if len(faults) != 1 || faults[0].GetSessionAbsent() == nil {
		t.Fatalf("Session() faults = %v, want the session_absent fault", faults)
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
	if len(faults) != 1 || !strings.HasPrefix(faults[0].GetDetail(), KindLinkSevered) {
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
	// `store_unreachable' names no arm, so the fault is withheld; the session is
	// still unhealthy because the fault STANDS.
	if got.GetSuccess().GetUnhealthy() == nil {
		t.Fatalf("Session() = %v, want the unhealthy arm", got.GetSuccess().GetHealth())
	}
	if faults := got.GetSuccess().GetUnhealthy().GetFaults(); len(faults) != 0 {
		t.Fatalf("Session() faults = %v, want the armless fault withheld", faults)
	}
}

// TestSessionReportsAnArmedWorkspaceScopedFault is the same read for a fault
// the wire CAN carry: it reaches the answer, rendered.
func TestSessionReportsAnArmedWorkspaceScopedFault(t *testing.T) {
	// Arrange.
	ws := ids.WorkspaceID("w1")
	db := &stubDB{
		workspaces: map[ids.WorkspaceID]wsm.Workspace{ws: {ID: ws, Dir: "/w1"}},
		faults:     []wsm.Fault{{Kind: KindShimDied, Detail: "exited", Workspace: &ws}},
	}
	r := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act.
	got, err := r.Session(context.Background(), ws)

	// Assert.
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	faults := got.GetSuccess().GetUnhealthy().GetFaults()
	if len(faults) != 1 || faults[0].GetShimDied() == nil {
		t.Fatalf("Session() faults = %v, want the rendered shim-died fault", faults)
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

// TestSessionAnswersEvenWhenTheWorkspaceOwnsNoSink pins that RESOLVING A NAMED
// WORKSPACE'S SINK IS A TOTAL FUNCTION here too.
//
// This test previously pinned the opposite -- the whole SessionHealth rpc
// failing when the sink would not open. Answering a workspace's HEALTH must not
// fail over WHERE the answer is narrated: a directory that cannot host a
// durable sink routes to the central sink carrying `unroutable_workspace'.
func TestSessionAnswersEvenWhenTheWorkspaceOwnsNoSink(t *testing.T) {
	// Arrange.
	db := &stubDB{workspaces: map[ids.WorkspaceID]wsm.Workspace{"w1": {ID: "w1", Dir: "/w1"}}}
	log := newStubSurfaces()
	log.workspaceErr = errors.New("symlink target missing")
	r := newReporter(t, db, alwaysLive, log)

	// Act.
	got, err := r.Session(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Session() = %v, want an answer despite the unresolvable sink", err)
	}
	if got.GetSuccess() == nil {
		t.Fatalf("Session() = %v, want a success answer", got)
	}
}

// TestSessionStillRefusesAnUnknownWorkspace pins the error that REMAINS: a
// workspace the state store will not name has nothing to report health about.
func TestSessionStillRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	r := newReporter(t, &stubDB{}, alwaysLive, newStubSurfaces())

	// Act.
	_, err := r.Session(context.Background(), "nobody")

	// Assert.
	if err == nil {
		t.Fatal("Session(unknown workspace) = nil error, want the refusal surfaced")
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

// Evict satisfies dlog.Surfaces for the merged seam (the bootinfra agent added it).
func (s *stubSurfaces) Evict(_ string) error { return nil }

// Retire satisfies dlog.Surfaces; the health checks never retire a directory.
func (s *stubSurfaces) Retire(_ string) error { return nil }

// TestOpenFaultLevelsAForgottenWorkspaceAtDebug is the middle layer of the
// shim-death cascade. A fault about a workspace the registry no longer holds
// has nowhere to stand, and that is an ordinary end for one: this reporter
// outlives the row, so a shim dying after its workspace was forgotten reaches
// it about a row nothing can carry. The error still reaches the caller.
func TestOpenFaultLevelsAForgottenWorkspaceAtDebug(t *testing.T) {
	tests := []struct {
		name      string
		openErr   error
		wantLevel string
	}{
		{
			name:      "the workspace was forgotten",
			openErr:   fmt.Errorf("wsm: fault workspace w1: %w", wsm.ErrNotFound),
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
			log := newStubSurfaces()
			r := newReporter(t, &stubDB{openErr: tt.openErr}, alwaysLive, log)

			// Act.
			_, err := r.OpenFault(context.Background(), wsm.Fault{Kind: "shim_died"})

			// Assert.
			if err == nil {
				t.Fatal("OpenFault = nil error, want the refusal surfaced")
			}
			var level string
			for _, record := range log.logger.Records() {
				if record.Operation == "daemon.health.open_fault" {
					level = record.Level
				}
			}
			if level != tt.wantLevel {
				t.Fatalf("open_fault record level = %q, want %q", level, tt.wantLevel)
			}
		})
	}
}

package boot

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// TestAdoptsASurvivingShimRatherThanRespawningIt pins the adoption path: a held
// workspace lock means a shim outlived the last daemon, and the sequence dials
// it instead of starting a second process against the same transcript.
func TestAdoptsASurvivingShimRatherThanRespawningIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got := h.supervisor.calls(); len(got) != 1 || got[0] != ws.ID {
		t.Fatalf("Adopt calls = %v, want exactly %v", got, ws.ID)
	}
	if len(report.Adopted) != 1 || report.Adopted[0] != ws.ID {
		t.Fatalf("report.Adopted = %v, want [%v]", report.Adopted, ws.ID)
	}
}

// TestAnAdoptedClientIsInstalled pins that an adoption reaches the session
// fleet: a client nothing installed would leave the daemon believing it serves
// a workspace it cannot reach.
func TestAnAdoptedClientIsInstalled(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if len(h.installed) != 1 || h.installed[0] != ws.ID {
		t.Fatalf("installed = %v, want [%v]", h.installed, ws.ID)
	}
}

// TestAFailedAdoptionFailsTheBoot pins the loudness rule: a shim that could not
// be adopted is not a workspace to carry on without.
func TestAFailedAdoptionFailsTheBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(_ *Deps, h *harness) { h.supervisor.err = errBoom })
	h.register(t, t.TempDir(), sessionlock.StateHeld)

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run error = %v, want it to wrap %v", err, errBoom)
	}
}

// TestAFailedInstallFailsTheBoot pins the same rule one step later: an adopted
// client the fleet refused is not a workspace to carry on without either.
func TestAFailedInstallFailsTheBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, _ *harness) {
		d.Adopted = func(context.Context, ids.WorkspaceID, shimclient.Client) error { return errBoom }
	})
	h.register(t, t.TempDir(), sessionlock.StateHeld)

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run error = %v, want it to wrap %v", err, errBoom)
	}
}

// TestAClientLessWorkspaceHasItsOrphanedTurnsClosed pins the queue's half of the
// split reconciliation: a free lock means nothing survives, so every turn
// without a terminal is closed in one transaction.
func TestAClientLessWorkspaceHasItsOrphanedTurnsClosed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	turn := wsm.NewTurnID()
	if err := h.db.PutTurn(context.Background(), wsm.Turn{ID: turn, Workspace: ws.ID, Text: "hi", Origin: "PROMPT_ORIGIN_USER", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Orphaned) != 1 || report.Orphaned[0] != turn {
		t.Fatalf("report.Orphaned = %v, want [%v]", report.Orphaned, turn)
	}
}

// TestAnAdoptedWorkspaceKeepsItsInFlightTurns pins the sessionwatcher's half of
// the same split: an adopted workspace's turns are still running, and closing
// them as orphans would retire a turn the shim is about to answer.
func TestAnAdoptedWorkspaceKeepsItsInFlightTurns(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)
	turn := wsm.NewTurnID()
	if err := h.db.PutTurn(context.Background(), wsm.Turn{ID: turn, Workspace: ws.ID, Text: "hi", Origin: "PROMPT_ORIGIN_USER", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Orphaned) != 0 {
		t.Fatalf("report.Orphaned = %v, want nothing closed for an adopted workspace", report.Orphaned)
	}
}

// TestAnUndeterminedProbeIsNeverReadAsFree pins the probe rule: a lock the
// daemon could not read is neither adopted nor orphan-closed, and it is
// recorded rather than counted.
func TestAnUndeterminedProbeIsNeverReadAsFree(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateUnknown)
	h.probeErrs[ws.Dir] = errBoom

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Undetermined) != 1 || report.Undetermined[0] != ws.ID {
		t.Fatalf("report.Undetermined = %v, want [%v]", report.Undetermined, ws.ID)
	}
	if got := h.supervisor.calls(); len(got) != 0 {
		t.Fatalf("Adopt calls = %v, want none for an unreadable lock", got)
	}
}

// TestAClosedWorkspaceIsSkipped pins that a torn-down workspace is not probed:
// it has no session to adopt and no turns to orphan.
func TestAClosedWorkspaceIsSkipped(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)
	if err := h.db.SetClosed(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("SetClosed: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Adopted) != 0 {
		t.Fatalf("report.Adopted = %v, want nothing for a closed workspace", report.Adopted)
	}
}

// TestEveryManifestDispositionIsCarriedWhole pins the bounce accounting: the
// four dispositions ride the report as values, so nothing downstream can
// collapse a DIED into a count of survivors.
func TestEveryManifestDispositionIsCarriedWhole(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	want := []rollout.Disposition{
		{Workspace: "a", Intent: rollout.IntentPreserve, Lock: sessionlock.StateHeld, Kind: rollout.DispositionPreserved},
		{Workspace: "b", Intent: rollout.IntentStandDown, Lock: sessionlock.StateFree, Kind: rollout.DispositionRolled},
		{Workspace: "c", Intent: rollout.IntentPreserve, Lock: sessionlock.StateFree, Kind: rollout.DispositionDied},
		{Workspace: "d", Intent: rollout.IntentStandDown, Lock: sessionlock.StateHeld, Kind: rollout.DispositionUnknown},
	}
	h.rollout.dispositions = want

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Dispositions) != len(want) {
		t.Fatalf("report.Dispositions = %v, want %v", report.Dispositions, want)
	}
	for i, d := range report.Dispositions {
		if d != want[i] {
			t.Fatalf("report.Dispositions[%d] = %v, want %v", i, d, want[i])
		}
	}
}

// TestAFailedManifestReconciliationFailsTheBoot pins that a manifest the daemon
// could not reconcile is not a startup to continue from.
func TestAFailedManifestReconciliationFailsTheBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(_ *Deps, h *harness) { h.rollout.reconcileErr = errBoom })

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run error = %v, want it to wrap %v", err, errBoom)
	}
}

// TestACorruptHoldStoreRefusesTheBoot pins the all-or-nothing rule: a restore
// that could not be completed loads nothing and fails loudly rather than
// silently losing what a user typed.
func TestACorruptHoldStoreRefusesTheBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(_ *Deps, h *harness) { h.queue.err = errBoom })

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run error = %v, want it to wrap %v", err, errBoom)
	}
}

// TestACorruptHoldReadRefusesTheBoot pins the same rule at the count: a durable
// record that cannot be read back is a refused load, never a zero.
func TestACorruptHoldReadRefusesTheBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, h *harness) { d.DB = failingHolds{DB: h.db, err: errBoom} })

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run error = %v, want it to wrap %v", err, errBoom)
	}
}

// TestTheRestoredHoldsAreCounted pins that the report answers for what came
// back, so a boot can say what it restored rather than only that it tried.
func TestTheRestoredHoldsAreCounted(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	turn := wsm.NewTurnID()
	if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{
		Turn:      turn,
		Workspace: ws.ID,
		Said:      userSaid("hello"),
		Origin:    "PROMPT_ORIGIN_USER",
		QueuedAt:  instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if report.HoldsRestored != 1 {
		t.Fatalf("report.HoldsRestored = %d, want 1", report.HoldsRestored)
	}
}

// TestAMergeHeldLeaseIsReportedAsRecovered pins the merge recovery's record: a
// workspace whose lease the merge orchestrator still holds is a merge that was
// in flight when the last daemon went away.
func TestAMergeHeldLeaseIsReportedAsRecovered(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	if _, err := h.db.AcquireLease(context.Background(), ws.ID, wsm.HolderMerge, wsm.PolicyRefuse); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.MergesRecovered) != 1 || report.MergesRecovered[0] != ws.ID {
		t.Fatalf("report.MergesRecovered = %v, want [%v]", report.MergesRecovered, ws.ID)
	}
}

// TestTheMergeRecoveryRuns pins that the orchestrator is asked to recover on
// every boot, not only when a lease happens to stand: a merge queued but never
// started is recovered by the same call.
func TestTheMergeRecoveryRuns(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if got := h.merge.recoveries(); got != 1 {
		t.Fatalf("Recover calls = %d, want 1", got)
	}
}

// TestAFailedMergeRecoveryFailsTheBoot pins that a merge the orchestrator could
// not resume is never silently abandoned.
func TestAFailedMergeRecoveryFailsTheBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(_ *Deps, h *harness) { h.merge.err = errBoom })

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run error = %v, want it to wrap %v", err, errBoom)
	}
}

// TestAJoiningDaemonRunsTheJoin pins the successor's path: it takes ownership
// workspace by workspace through the rollout's rendezvous.
func TestAJoiningDaemonRunsTheJoin(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, _ *harness) { d.JoiningAddress = "127.0.0.1:41111" })

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if got := h.rollout.joinCalls(); got != 1 {
		t.Fatalf("Join calls = %d, want 1", got)
	}
}

// TestAnIncumbentDoesNotJoin pins the other side: a daemon nobody spawned owns
// its workspaces already and has no incumbent to take them from.
func TestAnIncumbentDoesNotJoin(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if got := h.rollout.joinCalls(); got != 0 {
		t.Fatalf("Join calls = %d, want none for an incumbent", got)
	}
}

// TestAFailedJoinFailsTheBoot pins that a successor which could not take over
// exits rather than serving nothing at an address Emacs was told about.
func TestAFailedJoinFailsTheBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, h *harness) {
		d.JoiningAddress = "127.0.0.1:41111"
		h.rollout.joinErr = errBoom
	})

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run error = %v, want it to wrap %v", err, errBoom)
	}
}

// TestJoiningReportsTheSuccessorFlag pins that the joining answer comes from
// the explicit argument, never from a race that was lost elsewhere.
func TestJoiningReportsTheSuccessorFlag(t *testing.T) {
	// Arrange.
	tests := []struct {
		name    string
		address string
		want    bool
	}{
		{name: "an incumbent has no incumbent to join", address: "", want: false},
		{name: "a successor was spawned with one", address: "127.0.0.1:41111", want: true},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			h := newHarness(t, func(d *Deps, _ *harness) { d.JoiningAddress = test.address })

			// Act.
			got := h.seq.Joining()

			// Assert.
			if got != test.want {
				t.Fatalf("Joining() = %v, want %v", got, test.want)
			}
		})
	}
}

// TestNewRefusesAMissingCollaborator pins that a boot with a hole in its
// dependencies is refused at construction: a sequence that skipped a step would
// report a reconciliation it never ran.
func TestNewRefusesAMissingCollaborator(t *testing.T) {
	// Arrange.
	base := func(h *harness) Deps { return h.deps }
	tests := []struct {
		name     string
		breakDep func(*Deps)
	}{
		{name: "no state client", breakDep: func(d *Deps) { d.DB = nil }},
		{name: "no shim supervisor", breakDep: func(d *Deps) { d.Supervisor = nil }},
		{name: "no prompt queue", breakDep: func(d *Deps) { d.Queue = nil }},
		{name: "no merge orchestrator", breakDep: func(d *Deps) { d.Merge = nil }},
		{name: "no rollout controller", breakDep: func(d *Deps) { d.Rollout = nil }},
		{name: "no adoption installer", breakDep: func(d *Deps) { d.Adopted = nil }},
		{name: "no run directory", breakDep: func(d *Deps) { d.RunDir = "" }},
		{name: "no log surfaces", breakDep: func(d *Deps) { d.Log = nil }},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			h := newHarness(t)
			deps := base(h)
			test.breakDep(&deps)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil {
				t.Fatalf("New with %s returned no error", test.name)
			}
		})
	}
}

// TestTheOrphanCloseIsStampedWithTheInjectedInstant pins that the sequence
// stamps the clock it was given rather than reading the wall clock.
func TestTheOrphanCloseIsStampedWithTheInjectedInstant(t *testing.T) {
	// Arrange.
	stamp := instant.Add(3 * time.Hour)
	h := newHarness(t, func(d *Deps, _ *harness) { d.Now = func() time.Time { return stamp } })
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	turn := wsm.NewTurnID()
	if err := h.db.PutTurn(context.Background(), wsm.Turn{ID: turn, Workspace: ws.ID, Text: "hi", Origin: "PROMPT_ORIGIN_USER", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	turns, err := h.db.OpenTurns(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("OpenTurns: %v", err)
	}
	if len(turns) != 0 {
		t.Fatalf("OpenTurns = %v, want the orphan closed at %v", turns, stamp)
	}
}

// TestRunRefusesWhenTheWorkspaceRegistryCannotBeRead pins the first step's
// failure: a boot that could not read the registry does not start degraded,
// because it would go on to answer for state it never read.
func TestRunRefusesWhenTheWorkspaceRegistryCannotBeRead(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, h *harness) {
		d.DB = failingList{DB: h.db, err: errBoom}
	})

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run() = %v, want the registry read failure surfaced", err)
	}
	if h.merge.recoveries() != 0 {
		t.Fatal("the boot went on reconciling after the registry could not be read")
	}
}

// TestRunRefusesWhenAStaleSocketCannotBeCleared pins the adopt path's clear:
// a socket path the boot could not clear fails the BOOT, rather than leaving a
// path the next spawn will trip over for a reason that no longer exists.
func TestRunRefusesWhenAStaleSocketCannotBeCleared(t *testing.T) {
	// Arrange: nothing holds the lock and nothing listens, so the path is
	// swept — but what sits there is an ordinary FILE, which is not ours to
	// unlink and is never read as free.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	socket := h.deps.Layout.ShimSocket(string(ws.ID))
	if err := os.MkdirAll(filepath.Dir(socket), 0o755); err != nil {
		t.Fatalf("mkdir %q: %v", filepath.Dir(socket), err)
	}
	if err := os.WriteFile(socket, []byte("not a socket"), 0o600); err != nil {
		t.Fatalf("write %q: %v", socket, err)
	}

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if err == nil {
		t.Fatal("Run() = nil error, want the uncleanable socket path to fail the boot")
	}
	if !strings.Contains(err.Error(), string(ws.ID)) {
		t.Fatalf("err = %v, want it to name the workspace whose socket could not be cleared", err)
	}
}

// TestRunRefusesWhenAClientLessWorkspacesTurnsCannotBeClosed pins the orphan
// close: a turn left without a terminal is a conversation the daemon would
// answer for as if it were still running.
func TestRunRefusesWhenAClientLessWorkspacesTurnsCannotBeClosed(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, h *harness) {
		d.DB = failingCloseOrphans{DB: h.db, err: errBoom}
	})
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run() = %v, want the orphan close failure surfaced", err)
	}
	if !strings.Contains(err.Error(), string(ws.ID)) {
		t.Fatalf("err = %v, want it to name the workspace whose turns could not be closed", err)
	}
}

// TestRunRefusesWhenAWorkspacesLeaseCannotBeRead pins the merge recovery's
// first half: the leases say which merges were in flight across the crash, and
// a lease that could not be read is never guessed at.
func TestRunRefusesWhenAWorkspacesLeaseCannotBeRead(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, h *harness) {
		d.DB = failingLease{DB: h.db, err: errBoom}
	})
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run() = %v, want the lease read failure surfaced", err)
	}
	if !strings.Contains(err.Error(), string(ws.ID)) {
		t.Fatalf("err = %v, want it to name the workspace whose lease could not be read", err)
	}
}

// TestRunDoesNotRecoverMergesAfterAnUnreadableLease pins that the recovery
// itself never runs on a lease set the boot could not read: re-queueing a
// merge whose in-flight set is unknown is worse than refusing the boot.
func TestRunDoesNotRecoverMergesAfterAnUnreadableLease(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, h *harness) {
		d.DB = failingLease{DB: h.db, err: errBoom}
	})
	h.register(t, t.TempDir(), sessionlock.StateFree)

	// Act.
	if _, err := h.seq.Run(context.Background()); err == nil {
		t.Fatal("Run() = nil error, want the lease read failure surfaced")
	}

	// Assert.
	if got := h.merge.recoveries(); got != 0 {
		t.Fatalf("Recover calls = %d, want 0", got)
	}
}

// TestAnUnreachableSurvivorDoesNotWedgeTheBoot pins the bound that keeps the
// daemon serving. The listener is already bound and daemon.addr already
// published while this reconciliation runs, so an adoption that never answers
// is a daemon that listens and accepts nothing — observed on pid 31984, whose
// accept queue stood at 128/128 for ten hours because one workspace's lock
// read HELD for a shim whose socket path was gone.
func TestAnUnreachableSurvivorDoesNotWedgeTheBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(deps *Deps, _ *harness) { deps.AdoptBound = 20 * time.Millisecond })
	h.supervisor.hang = true
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run = error %v, want a completed boot: one unreachable survivor must not fail the boot", err)
	}
	if len(report.Undetermined) != 1 || report.Undetermined[0] != ws.ID {
		t.Fatalf("report.Undetermined = %v, want [%v]: the lock reads held, so the workspace is owned and undetermined", report.Undetermined, ws.ID)
	}
}

// TestAnUnreachableSurvivorKeepsItsInFlightTurns pins the other half of the
// undetermined disposition: the lock says a living process owns the
// conversation, so its turns are NOT closed as orphans just because this
// daemon could not reach it.
func TestAnUnreachableSurvivorKeepsItsInFlightTurns(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(deps *Deps, _ *harness) { deps.AdoptBound = 20 * time.Millisecond })
	h.supervisor.hang = true
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)
	turn := wsm.NewTurnID()
	if err := h.db.PutTurn(context.Background(), wsm.Turn{ID: turn, Workspace: ws.ID, Text: "hi", Origin: "PROMPT_ORIGIN_USER", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Orphaned) != 0 {
		t.Fatalf("report.Orphaned = %v, want nothing closed: a held lock is a living owner, reachable or not", report.Orphaned)
	}
}

// TestAnOverrunAdoptionIsReportedAtError pins that the bound's expiry is LOUD.
// It is the only record that says why a workspace the daemon was serving
// yesterday is undetermined today.
func TestAnOverrunAdoptionIsReportedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(deps *Deps, _ *harness) { deps.AdoptBound = 20 * time.Millisecond })
	h.supervisor.hang = true
	h.register(t, t.TempDir(), sessionlock.StateHeld)

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	var found bool
	for _, r := range h.log.Records() {
		if r.Level == "error" && r.Operation == "daemon.boot.adopt" {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want an error daemon.boot.adopt record for the overrun adoption", h.log.Records())
	}
}

// TestABoundedAdoptionIsNotCancelledByTheBoundWhenItAnswers pins that the
// bound does not cut an ordinary adoption short: a survivor that answers is
// adopted, and the report says so.
func TestABoundedAdoptionIsNotCancelledByTheBoundWhenItAnswers(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(deps *Deps, _ *harness) { deps.AdoptBound = time.Second })
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Adopted) != 1 || report.Adopted[0] != ws.ID {
		t.Fatalf("report.Adopted = %v, want [%v]", report.Adopted, ws.ID)
	}
}

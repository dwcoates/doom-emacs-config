package boot

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
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

// AN ADOPTION IS A HEALTHY ATTACH, and the boot fires its recovery edge AFTER
// the manifest is reconciled, because the reconcile is what records an
// undetermined bounce. Before, a `bounce_unknown` recorded for a workspace
// this very boot had adopted stood for over 30 minutes (2026-09-27).
func TestTheBootClosesAnAdoptedWorkspacesUndeterminedBounce(t *testing.T) {
	tests := []struct {
		name   string
		lock   sessionlock.State
		closes bool
	}{
		{"an adopted workspace attached healthy", sessionlock.StateHeld, true},
		{"a client-less workspace has not attached yet", sessionlock.StateFree, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			ws := h.register(t, t.TempDir(), tt.lock)
			workspace := ws.ID
			h.rollout.onReconcile = func() {
				if _, err := h.db.OpenFault(context.Background(), wsm.Fault{
					Workspace: &workspace, Kind: health.KindBounceUnknown,
				}); err != nil {
					t.Errorf("OpenFault: %v", err)
				}
			}

			// Act.
			if _, err := h.seq.Run(context.Background()); err != nil {
				t.Fatalf("Run: %v", err)
			}
			open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &workspace, Kind: health.KindBounceUnknown})

			// Assert.
			if err != nil {
				t.Fatalf("OpenFaults: %v", err)
			}
			if closed := len(open) == 0; closed != tt.closes {
				t.Fatalf("bounce_unknown closed = %v, want %v", closed, tt.closes)
			}
		})
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

// TestBootReleasesAClosedWorkspacesServing pins the boot heal: a closed row
// still naming a serving instance (498b3b658c074bf4, 2026-09-27) is released
// and stated at ERROR, while an open served row is left untouched.
func TestBootReleasesAClosedWorkspacesServing(t *testing.T) {
	tests := []struct {
		name        string
		closed      bool
		wantServed  bool
		wantHealed  bool
		wantErrLine bool
	}{
		{name: "a closed but served row is released and logged", closed: true, wantServed: false, wantHealed: true, wantErrLine: true},
		{name: "an open served row is untouched", closed: false, wantServed: true, wantHealed: false, wantErrLine: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			ctx := context.Background()
			h := newHarness(t)
			ws := h.register(t, t.TempDir(), sessionlock.StateHeld)
			if tt.closed {
				if err := h.db.SetClosed(ctx, ws.ID, true); err != nil {
					t.Fatalf("SetClosed: %v", err)
				}
			}
			// The stale claim a leaked close path left behind.
			if err := h.db.ClaimServing(ctx, ws.ID, wsm.NewInstanceID()); err != nil {
				t.Fatalf("ClaimServing: %v", err)
			}

			// Act.
			report, err := h.seq.Run(ctx)

			// Assert.
			if err != nil {
				t.Fatalf("Run: %v", err)
			}
			owner, err := h.db.Serving(ctx, ws.ID)
			if err != nil {
				t.Fatalf("Serving: %v", err)
			}
			if (owner != nil) != tt.wantServed {
				t.Fatalf("serving owner = %v, want present=%v", owner, tt.wantServed)
			}
			if healed := len(report.ClosedServingReleased) == 1; healed != tt.wantHealed {
				t.Fatalf("report.ClosedServingReleased = %v, want healed=%v", report.ClosedServingReleased, tt.wantHealed)
			}
			if got := h.hasRecord("error", "daemon.boot.release_closed_serving"); got != tt.wantErrLine {
				t.Fatalf("error record = %v, want %v", got, tt.wantErrLine)
			}
		})
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

// TestAdoptionDialsTheShimsRolledGeneration pins the path the boot dials. A
// relaunch moved the workspace's shim onto `<base>.nN.sock` and the counter
// that minted N lived in the last daemon's memory, so a boot that dials the
// layout's base name dials a path the survivor has not held since — while its
// lock reads HELD, which is what makes the redial ladder endless.
func TestAdoptionDialsTheShimsRolledGeneration(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)
	base := h.deps.Layout.ShimSocket(string(ws.ID))
	if err := os.MkdirAll(filepath.Dir(base), 0o755); err != nil {
		t.Fatalf("create the socket directory: %v", err)
	}
	rolled := strings.TrimSuffix(base, ".sock") + ".n1.sock"
	if err := os.WriteFile(rolled, nil, 0o600); err != nil {
		t.Fatalf("create the rolled socket path: %v", err)
	}
	h.socketProbes[rolled] = shimsocket.StateLive

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if got := h.supervisor.paths(); len(got) != 1 || got[0] != rolled {
		t.Fatalf("Adopt paths = %v, want [%q]: the survivor listens on its rolled generation", got, rolled)
	}
}

// TestSilentSurvivorsPayTheAdoptionBoundOnce pins the concurrency of the dial
// pass. Every instant this step spends is an instant the daemon's already-bound
// listener queues connections nobody accepts, so N survivors that never answer
// must cost ONE bound rather than N: the realtest-1 boot paid its full 10s
// before it served at all.
func TestSilentSurvivorsPayTheAdoptionBoundOnce(t *testing.T) {
	// Arrange: four survivors whose locks read held and whose dials hang.
	const bound = 100 * time.Millisecond
	const survivors = 4
	h := newHarness(t, func(deps *Deps, _ *harness) { deps.AdoptBound = bound })
	h.supervisor.hang = true
	for i := 0; i < survivors; i++ {
		h.register(t, t.TempDir(), sessionlock.StateHeld)
	}

	// Act.
	started := time.Now()
	report, err := h.seq.Run(context.Background())
	elapsed := time.Since(started)

	// Assert.
	if err != nil {
		t.Fatalf("Run = error %v, want a completed boot", err)
	}
	if len(report.Undetermined) != survivors {
		t.Fatalf("report.Undetermined = %v, want all %d survivors undetermined", report.Undetermined, survivors)
	}
	// Two bounds is the generous ceiling that still fails a SERIAL pass, which
	// would take four.
	if ceiling := 2 * bound; elapsed >= ceiling {
		t.Fatalf("the boot spent %v adopting %d silent survivors, want under %v: the bound is paid once for all of them",
			elapsed, survivors, ceiling)
	}
}

// TestSurvivorsAreReportedInWorkspaceOrder pins what the concurrent dial pass
// must not cost: the report is built on the boot's own goroutine in the order
// the registry lists the workspaces, never in the order the dials answered.
func TestSurvivorsAreReportedInWorkspaceOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	var want []ids.WorkspaceID
	for i := 0; i < 5; i++ {
		want = append(want, h.register(t, t.TempDir(), sessionlock.StateHeld).ID)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run = error %v, want a completed boot", err)
	}
	if len(report.Adopted) != len(want) {
		t.Fatalf("report.Adopted = %v, want %v", report.Adopted, want)
	}
	for i := range want {
		if report.Adopted[i] != want[i] {
			t.Fatalf("report.Adopted = %v, want the registry's order %v", report.Adopted, want)
		}
	}
}

// TestAWorkspaceWhoseDirectoryIsGoneIsClosed pins the owner's ruling: a
// registry row naming a directory that no longer exists is closed by the
// daemon, so Emacs never reads it as a live workspace and opens a tab it
// cannot then serve.
func TestAWorkspaceWhoseDirectoryIsGoneIsClosed(t *testing.T) {
	// Arrange: a registered workspace whose directory is then removed.
	h := newHarness(t)
	dir := t.TempDir()
	ws := h.register(t, dir, sessionlock.StateFree)
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run = error %v, want a completed boot", err)
	}
	if len(report.MissingDirClosed) != 1 || report.MissingDirClosed[0] != ws.ID {
		t.Fatalf("report.MissingDirClosed = %v, want [%v]", report.MissingDirClosed, ws.ID)
	}
	record, err := h.db.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace(%v): %v", ws.ID, err)
	}
	if !record.Closed {
		t.Fatalf("workspace %v is still open; a directory that is gone must close the row", ws.ID)
	}
}

// TestAWorkspaceWhoseDirectoryExistsIsLeftOpen is the negative arm: the step
// closes nothing it was not asked to.
func TestAWorkspaceWhoseDirectoryExistsIsLeftOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run = error %v, want a completed boot", err)
	}
	if len(report.MissingDirClosed) != 0 {
		t.Fatalf("report.MissingDirClosed = %v, want none", report.MissingDirClosed)
	}
	record, err := h.db.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace(%v): %v", ws.ID, err)
	}
	if record.Closed {
		t.Fatalf("workspace %v was closed though its directory is there", ws.ID)
	}
}

// TestAnAlreadyClosedMissingDirectoryIsNotReClosed pins the merged and nuked
// case: those rows keep their directory removed and are already closed, so the
// step passes over them and the report does not claim them.
func TestAnAlreadyClosedMissingDirectoryIsNotReClosed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	dir := t.TempDir()
	ws := h.register(t, dir, sessionlock.StateFree)
	if err := h.db.SetClosed(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("SetClosed: %v", err)
	}
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run = error %v, want a completed boot", err)
	}
	if len(report.MissingDirClosed) != 0 {
		t.Fatalf("report.MissingDirClosed = %v, want none: the row was already closed", report.MissingDirClosed)
	}
}

// TestAClosedMissingDirectoryIsNeverAdopted pins the ORDER: the close runs
// before the adopt walk, so a workspace whose directory is gone is never
// dialled for a surviving shim.
func TestAClosedMissingDirectoryIsNeverAdopted(t *testing.T) {
	// Arrange: the lock reads HELD, which is what would otherwise select the
	// adopt path.
	h := newHarness(t)
	dir := t.TempDir()
	ws := h.register(t, dir, sessionlock.StateHeld)
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run = error %v, want a completed boot", err)
	}
	if len(report.Adopted) != 0 {
		t.Fatalf("report.Adopted = %v, want none: a workspace with no directory is closed, not adopted", report.Adopted)
	}
	if got := len(h.supervisor.calls()); got != 0 {
		t.Fatalf("Adopt was called %d times for a workspace whose directory is gone, want 0", got)
	}
	if len(report.MissingDirClosed) != 1 || report.MissingDirClosed[0] != ws.ID {
		t.Fatalf("report.MissingDirClosed = %v, want [%v]", report.MissingDirClosed, ws.ID)
	}
}

// TestTheRuledAutomaticCloseIsRecordedAtInfo pins the LEVEL of the ruling
// above. The owner ruled the close automatic on 2026-09-11, so a close that
// happened is the ruling being carried out and not a condition to remediate;
// the arms beside it -- a stat that failed for any other reason, and a close
// that could not be written -- keep their WARN and their ERROR.
func TestTheRuledAutomaticCloseIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	dir := t.TempDir()
	h.register(t, dir, sessionlock.StateFree)
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run = error %v, want a completed boot", err)
	}

	// Assert.
	if h.hasRecord("warn", "daemon.boot.close_missing_dir") {
		t.Fatalf("the ruled automatic close was recorded at warn: %v", h.log.Records())
	}
	if !h.hasRecord("info", "daemon.boot.close_missing_dir") {
		t.Fatalf("no info record named the close: %v", h.log.Records())
	}
}

// TestABootAbandonedMidAdoptionRecordsAtInfo pins the level of an adoption
// that ended because the BOOT'S OWN CONTEXT was cancelled. The process is
// going away under the reconciliation, so there is nothing to remediate about
// a survivor nobody will serve; a shim that genuinely refused keeps its ERROR.
// The boot still fails either way -- the caller decides what an abandoned boot
// means -- it just stops reporting a defect that is not one.
func TestABootAbandonedMidAdoptionRecordsAtInfo(t *testing.T) {
	tests := []struct {
		name      string
		adoptErr  error
		wantLevel string
	}{
		{name: "the boot was abandoned", adoptErr: context.Canceled, wantLevel: "info"},
		{name: "the shim refused the dial", adoptErr: errors.New("connection refused"), wantLevel: "error"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.supervisor.err = tt.adoptErr
			h.register(t, t.TempDir(), sessionlock.StateHeld)

			// Act.
			if _, err := h.seq.Run(context.Background()); err == nil {
				t.Fatal("Run = nil, want the failed adoption to fail the boot")
			}

			// Assert.
			if !h.hasRecord(tt.wantLevel, "daemon.boot.adopt") {
				t.Fatalf("no %s record under daemon.boot.adopt: %v", tt.wantLevel, h.log.Records())
			}
			if tt.wantLevel == "info" && h.hasRecord("error", "daemon.boot.adopt") {
				t.Fatalf("an abandoned boot was reported at error: %v", h.log.Records())
			}
		})
	}
}

// TestABootStartsANonAdoptedOpenWorkspace pins the owner's ruling of
// 2026-09-13: an open workspace is never session-less, so a row the user left
// open whose shim did not survive gets its session started at boot.
func TestABootStartsANonAdoptedOpenWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)

	// Act.
	_, brought := h.runAndBringUp(t)

	// Assert.
	if len(h.started) != 1 || h.started[0] != ws.ID {
		t.Fatalf("started = %v, want [%v]", h.started, ws.ID)
	}
	if len(brought.BroughtUp) != 1 || brought.BroughtUp[0] != ws.ID {
		t.Fatalf("BroughtUp = %v, want [%v]", brought.BroughtUp, ws.ID)
	}
}

func TestABootLeavesAHibernatedWorkspaceAsleep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.hibernate(t, ws)

	// Act.
	_, brought := h.runAndBringUp(t)

	// Assert.
	if len(h.started) != 0 {
		t.Fatalf("started = %v, want none: hibernation is deliberate", h.started)
	}
	if len(brought.HibernatedLeft) != 1 || brought.HibernatedLeft[0] != ws.ID {
		t.Fatalf("HibernatedLeft = %v, want [%v]", brought.HibernatedLeft, ws.ID)
	}
}

func TestOneFailedBringUpDoesNotStopTheNext(t *testing.T) {
	// Arrange: two client-less workspaces, the FIRST of which will not start.
	h := newHarness(t)
	first := h.register(t, t.TempDir(), sessionlock.StateFree)
	second := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.startErrs[first.ID] = errBoom

	// Act.
	_, brought := h.runAndBringUp(t)

	// Assert.
	if len(brought.BringUpFailed) != 1 || brought.BringUpFailed[0] != first.ID {
		t.Fatalf("BringUpFailed = %v, want [%v]", brought.BringUpFailed, first.ID)
	}
	if len(brought.BroughtUp) != 1 || brought.BroughtUp[0] != second.ID {
		t.Fatalf("BroughtUp = %v, want [%v]", brought.BroughtUp, second.ID)
	}
}

func TestABootStartsNoSessionForAClosedRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	if err := h.db.SetClosed(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("SetClosed: %v", err)
	}

	// Act.
	_, brought := h.runAndBringUp(t)

	// Assert.
	if len(h.started) != 0 {
		t.Fatalf("started = %v, want none for a closed row", h.started)
	}
	if len(brought.BroughtUp) != 0 {
		t.Fatalf("BroughtUp = %v, want none for a closed row", brought.BroughtUp)
	}
}

// TestAnUndeterminedWorkspaceIsNeverStarted pins the other half of "could not
// tell is never read as free": spawning a second shim onto a conversation a
// survivor may still own is the loss that discipline prevents.
func TestAnUndeterminedWorkspaceIsNeverStarted(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateUnknown)
	h.probeErrs[ws.Dir] = errBoom

	// Act.
	report, _ := h.runAndBringUp(t)

	// Assert.
	if len(h.started) != 0 {
		t.Fatalf("started = %v, want none for an undetermined workspace", h.started)
	}
	if len(report.Undetermined) != 1 || report.Undetermined[0] != ws.ID {
		t.Fatalf("report.Undetermined = %v, want [%v]", report.Undetermined, ws.ID)
	}
}

func TestTheBringUpSummaryIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.register(t, t.TempDir(), sessionlock.StateFree)

	// Act.
	h.runAndBringUp(t)

	// Assert.
	if !h.hasRecord("info", "daemon.boot.bring_up") {
		t.Fatalf("records = %+v, want an info bring-up summary", h.log.Records())
	}
}

// TestTheReconciliationStartsNoSessionOfItsOwn pins the invariant the
// 2026-09-13 deploy regression named: the daemon answers no unary until the
// reconciliation has RETURNED, so a session start inside it is a shim spawn in
// front of the editor's 3s DaemonHealth bound. Run therefore only NAMES the
// workspaces to bring up.
func TestTheReconciliationStartsNoSessionOfItsOwn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(h.started) != 0 {
		t.Fatalf("started = %v, want none: the reconciliation must not spend a shim spawn before the daemon answers", h.started)
	}
	if len(report.PendingBringUp) != 1 || report.PendingBringUp[0].ID != ws.ID {
		t.Fatalf("report.PendingBringUp = %v, want the client-less workspace %v", report.PendingBringUp, ws.ID)
	}
}

// TestTheReconciliationDoesNotWaitOnASlowSessionStart is the same invariant
// stated as the failure it prevents: N slow starts used to be N delays in
// front of the first health answer. The start here never returns until the
// test lets it, and the reconciliation still completes.
func TestTheReconciliationDoesNotWaitOnASlowSessionStart(t *testing.T) {
	// Arrange: three workspaces whose starts BLOCK until released.
	release := make(chan struct{})
	entered := make(chan ids.WorkspaceID, 3)
	h := newHarness(t, func(deps *Deps, h *harness) {
		deps.StartSession = func(_ context.Context, ws ids.WorkspaceID) error {
			entered <- ws
			<-release
			return nil
		}
	})
	defer close(release)
	for i := 0; i < 3; i++ {
		h.register(t, t.TempDir(), sessionlock.StateFree)
	}

	// Act: the reconciliation, with nothing released.
	report, err := h.seq.Run(context.Background())

	// Assert: it returned, which is what lets the daemon serve.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.PendingBringUp) != 3 {
		t.Fatalf("report.PendingBringUp = %v, want all three workspaces", report.PendingBringUp)
	}
	select {
	case ws := <-entered:
		t.Fatalf("the reconciliation started %v, want no start until the bring-up runs", ws)
	default:
	}
}

// TestABringUpStartsNothingFurtherOnceTheDaemonIsLeaving pins the half of the
// split that the exit sees: the step now runs beside the accept loop, so a
// shutdown can land in the middle of it, and the workspaces it has not reached
// are simply not started.
func TestABringUpStartsNothingFurtherOnceTheDaemonIsLeaving(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.register(t, t.TempDir(), sessionlock.StateFree)
	h.register(t, t.TempDir(), sessionlock.StateFree)
	report, err := h.seq.Run(context.Background())
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	leaving, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	brought := h.seq.BringUp(leaving, report.PendingBringUp)

	// Assert.
	if len(h.started) != 0 {
		t.Fatalf("started = %v, want none once the daemon is leaving", h.started)
	}
	if len(brought.BroughtUp) != 0 {
		t.Fatalf("BroughtUp = %v, want none once the daemon is leaving", brought.BroughtUp)
	}
}

// TestAStartAlreadyBegunIsNotCutByTheExit pins the other half: a start that
// has begun writes a session record, a spawned shim and — on a failure — a
// fault, and an exit that cancelled it halfway left all three half-written.
func TestAStartAlreadyBegunIsNotCutByTheExit(t *testing.T) {
	// Arrange: the exit lands INSIDE the start, and the start reads its own
	// context back afterwards.
	leaving, cancel := context.WithCancel(context.Background())
	defer cancel()
	var startCtxErr error
	h := newHarness(t, func(deps *Deps, h *harness) {
		deps.StartSession = func(ctx context.Context, ws ids.WorkspaceID) error {
			cancel()
			startCtxErr = ctx.Err()
			h.started = append(h.started, ws)
			return nil
		}
	})
	h.register(t, t.TempDir(), sessionlock.StateFree)
	report, err := h.seq.Run(context.Background())
	if err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Act.
	brought := h.seq.BringUp(leaving, report.PendingBringUp)

	// Assert.
	if len(brought.BroughtUp) != 1 {
		t.Fatalf("BroughtUp = %v, want the one workspace started", brought.BroughtUp)
	}
	if startCtxErr != nil {
		t.Fatalf("the start ran under a context reading %v, want one the exit cannot cut", startCtxErr)
	}
}

// TestAStandDownDuringTheBringUpIsNotAFailedBringUp covers the exit landing
// inside the step. The bring-up runs BESIDE the accept loop, so a deploy's
// SIGTERM reaches the drain while a start is in flight; the drain force-stops
// every workspace session, and the start then comes back from a shim this same
// process just killed.
//
// MEASURED, realtest run 2026-09-13T16:20:34. Three daemon generations in a row
// (pids 58458, 68787, 80526) recorded `daemon.boot.bring_up: an open
// workspace's session did not come up` at ERROR, nine milliseconds after
// recording their own `daemon.shimclient.standdown` for the same shim at info.
func TestAStandDownDuringTheBringUpIsNotAFailedBringUp(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.startErrs[ws.ID] = fmt.Errorf("start session for %q: %w", ws.ID, shimclient.ErrStandDownOrdered)

	// Act.
	_, brought := h.runAndBringUp(t)

	// Assert.
	if len(brought.StoodDown) != 1 || brought.StoodDown[0] != ws.ID {
		t.Fatalf("StoodDown = %v, want [%v]", brought.StoodDown, ws.ID)
	}
	if len(brought.BringUpFailed) != 0 {
		t.Fatalf("BringUpFailed = %v, want none: the daemon ordered the stand-down", brought.BringUpFailed)
	}
}

// TestAStandDownDuringTheBringUpIsNotRecordedAsAnError is the log half of the
// edge above: the count and the record have to agree, or a harvest still reads
// the ordered teardown as a defect.
func TestAStandDownDuringTheBringUpIsNotRecordedAsAnError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.startErrs[ws.ID] = fmt.Errorf("start session for %q: %w", ws.ID, shimclient.ErrStandDownOrdered)

	// Act.
	h.runAndBringUp(t)

	// Assert.
	for _, r := range h.log.Records() {
		if r.Operation == "daemon.boot.bring_up" && (r.Level == "error" || r.Level == "warn") {
			t.Fatalf("the ordered stand-down was recorded at %q: %q", r.Level, r.Message)
		}
	}
}

// bootStepClock is a Clock that never sleeps: After fires at once and ADVANCES
// the clock, so a bounded poll pays its real number of passes in no wall time.
type bootStepClock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *bootStepClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *bootStepClock) After(d time.Duration) <-chan time.Time {
	c.mu.Lock()
	c.now = c.now.Add(d)
	fired := c.now
	c.mu.Unlock()
	ch := make(chan time.Time, 1)
	ch <- fired
	return ch
}

// bindAfter makes one socket path read ABSENT for the first n probes and LIVE
// from then on, which is a shim finishing its Node startup partway through the
// wait. Every other path keeps the harness's scripted answer.
func bindAfter(h *harness, path string, n int) func(*Deps, *harness) {
	var seen int
	return func(deps *Deps, _ *harness) {
		deps.SocketProbe = func(probed string) (shimsocket.State, error) {
			if probed != path {
				if state, ok := h.socketProbes[probed]; ok {
					return state, nil
				}
				return shimsocket.StateAbsent, nil
			}
			seen++
			if seen > n {
				return shimsocket.StateLive, nil
			}
			return shimsocket.StateAbsent, nil
		}
	}
}

// TestABootWaitsForAShimItsPredecessorSpawned pins the whole starting-shim
// invariant at the boot. A free lock and an absent socket are ALSO what a shim
// forked tens of milliseconds ago looks like -- it takes its conversation lock
// inside StartSession and Node binds its socket later still -- so a boot that
// read that as "no shim survives" spawned a SECOND shim onto one session
// socket and the shim refused the bind and died (2026-09-13: spawn at T+0,
// daemon killed at T+60ms, successor spawning at T+80ms, the survivor binding
// at T+110ms, the newcomer dying at T+190ms). The registry's recorded spawn
// pid is the fact that separates the two, and the three outcomes below are the
// whole of what it can say.
func TestABootWaitsForAShimItsPredecessorSpawned(t *testing.T) {
	tests := []struct {
		name string
		// arrange
		recordPID bool
		alive     bool
		bindAfter int // -1 never binds
		// assert
		wantAdopted      bool
		wantClientless   bool
		wantUndetermined bool
	}{
		{
			name:        "a live recorded spawn that announces itself is adopted, not spawned over",
			recordPID:   true,
			alive:       true,
			bindAfter:   2,
			wantAdopted: true,
		},
		{
			name:           "a recorded spawn whose process is dead leaves the workspace client-less",
			recordPID:      true,
			alive:          false,
			bindAfter:      -1,
			wantClientless: true,
		},
		{
			name:           "no recorded spawn at all is the ordinary client-less workspace",
			recordPID:      false,
			alive:          true,
			bindAfter:      -1,
			wantClientless: true,
		},
		{
			name:             "a live recorded spawn that never announces itself leaves the workspace undetermined",
			recordPID:        true,
			alive:            true,
			bindAfter:        -1,
			wantUndetermined: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			const recordedPID = 4242
			var h *harness
			h = newHarness(t, func(deps *Deps, harn *harness) {
				h = harn
				deps.Clock = &bootStepClock{now: instant}
				deps.ShimAlive = func(pid int) bool {
					if pid != recordedPID {
						t.Fatalf("liveness asked about pid %d, want the recorded %d", pid, recordedPID)
					}
					return tt.alive
				}
				deps.AdoptBound = 200 * time.Millisecond
			})
			ws := h.register(t, t.TempDir(), sessionlock.StateFree)
			socket := h.deps.Layout.ShimSocket(string(ws.ID))
			if tt.bindAfter >= 0 {
				bindAfter(h, socket, tt.bindAfter)(&h.deps, h)
				seq, err := New(h.deps)
				if err != nil {
					t.Fatalf("boot.New: %v", err)
				}
				h.seq = seq
			}
			if tt.recordPID {
				pid := recordedPID
				if err := h.db.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
					t.Fatalf("SetSpawnedShimPID: %v", err)
				}
			}

			// Act.
			report, err := h.seq.Run(context.Background())
			if err != nil {
				t.Fatalf("Run: %v", err)
			}

			// Assert.
			if got := contains(report.Adopted, ws.ID); got != tt.wantAdopted {
				t.Fatalf("report.Adopted contains %s = %v, want %v (%+v)", ws.ID, got, tt.wantAdopted, report.Adopted)
			}
			if got := containsWorkspace(report.PendingBringUp, ws.ID); got != tt.wantClientless {
				t.Fatalf("report.PendingBringUp contains %s = %v, want %v", ws.ID, got, tt.wantClientless)
			}
			if got := contains(report.Undetermined, ws.ID); got != tt.wantUndetermined {
				t.Fatalf("report.Undetermined contains %s = %v, want %v", ws.ID, got, tt.wantUndetermined)
			}
		})
	}
}

// TestAnAnnouncedStartingShimIsAdoptedAsTheInertSurvivorItIs pins WHICH
// adoption a starting shim gets. It has taken no conversation lock, so it
// carries no session and must not reach the bounce accounting as one.
func TestAnAnnouncedStartingShimIsAdoptedAsTheInertSurvivorItIs(t *testing.T) {
	// Arrange.
	const recordedPID = 4242
	var h *harness
	h = newHarness(t, func(deps *Deps, harn *harness) {
		h = harn
		deps.Clock = &bootStepClock{now: instant}
		deps.ShimAlive = func(int) bool { return true }
		deps.AdoptBound = 200 * time.Millisecond
	})
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	socket := h.deps.Layout.ShimSocket(string(ws.ID))
	bindAfter(h, socket, 1)(&h.deps, h)
	seq, err := New(h.deps)
	if err != nil {
		t.Fatalf("boot.New: %v", err)
	}
	h.seq = seq
	pid := recordedPID
	if err := h.db.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
		t.Fatalf("SetSpawnedShimPID: %v", err)
	}

	// Act.
	report, err := h.seq.Run(context.Background())
	if err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if !contains(report.AdoptedInert, ws.ID) {
		t.Fatalf("report.AdoptedInert = %v, want the starting shim carried as inert", report.AdoptedInert)
	}
	if len(report.AdoptedSessions) != 0 {
		t.Fatalf("report.AdoptedSessions = %+v, want none: a starting shim has no session", report.AdoptedSessions)
	}
}

// TestAStartingShimThatNeverAnnouncesItselfIsRecordedAtError pins the one loud
// record of this path. Every other outcome is the ordinary course of a boot
// and stays at INFO.
func TestAStartingShimThatNeverAnnouncesItselfIsRecordedAtError(t *testing.T) {
	// Arrange.
	var h *harness
	h = newHarness(t, func(deps *Deps, harn *harness) {
		h = harn
		deps.Clock = &bootStepClock{now: instant}
		deps.ShimAlive = func(int) bool { return true }
		deps.AdoptBound = 200 * time.Millisecond
	})
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	pid := 4242
	if err := h.db.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
		t.Fatalf("SetSpawnedShimPID: %v", err)
	}

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if !h.hasRecord("error", "daemon.boot.adopt") {
		t.Fatalf("no error record for a spawn that never announced itself: %v", h.log.Records())
	}
}

// TestAWaitedStartingShimIsAnnouncedAtInfo pins the two INFO records the wait
// states: that it is waiting, and that the shim announced itself. Without them
// a boot that paid its adoption bound reads on disk as an unexplained pause.
func TestAWaitedStartingShimIsAnnouncedAtInfo(t *testing.T) {
	// Arrange.
	var h *harness
	h = newHarness(t, func(deps *Deps, harn *harness) {
		h = harn
		deps.Clock = &bootStepClock{now: instant}
		deps.ShimAlive = func(int) bool { return true }
		deps.AdoptBound = 200 * time.Millisecond
	})
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	bindAfter(h, h.deps.Layout.ShimSocket(string(ws.ID)), 1)(&h.deps, h)
	seq, err := New(h.deps)
	if err != nil {
		t.Fatalf("boot.New: %v", err)
	}
	h.seq = seq
	pid := 4242
	if err := h.db.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
		t.Fatalf("SetSpawnedShimPID: %v", err)
	}

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	var waited, announced bool
	for _, r := range h.log.Records() {
		if r.Level != "info" || r.Operation != "daemon.boot.adopt" {
			continue
		}
		if strings.Contains(r.Message, "still starting") {
			waited = true
		}
		if strings.Contains(r.Message, "announced itself") {
			announced = true
		}
	}
	if !waited || !announced {
		t.Fatalf("waited=%v announced=%v, want both stated at info: %v", waited, announced, h.log.Records())
	}
}

// contains reports whether a list of workspace ids holds one.
func contains(list []ids.WorkspaceID, want ids.WorkspaceID) bool {
	for _, id := range list {
		if id == want {
			return true
		}
	}
	return false
}

// containsWorkspace reports whether a list of workspace records holds one id.
func containsWorkspace(list []wsm.Workspace, want ids.WorkspaceID) bool {
	for _, ws := range list {
		if ws.ID == want {
			return true
		}
	}
	return false
}

// TestTheBootBindsTheViewsBeforeItReconcilesTheManifest pins the order that
// keeps a reconciliation fault off an unbound footer: every disposition used to
// reach the footer before PublishRegistry had bound a single directory.
func TestTheBootBindsTheViewsBeforeItReconcilesTheManifest(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert
	if !h.rollout.boundAtReconcile {
		t.Fatalf("the manifest was reconciled before the views were bound")
	}
}

func TestAViewBindingFailureFailsTheBoot(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.bindErr = errors.New("the log sink could not be opened")

	// Act
	_, err := h.seq.Run(context.Background())

	// Assert
	if !errors.Is(err, h.bindErr) {
		t.Fatalf("Run = %v, want the binding failure", err)
	}
}

func TestNewRefusesASequenceWithNoViewBinder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.deps
	deps.BindViews = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a sequence that cannot bind the views")
	}
}

// TestTheBootReleasesALeaseItsOwnerLeftBehind pins invariant A's boot half: a
// lease a previous daemon took and never released -- it died without the
// orderly close that releases what it holds -- has no living owner once an
// incumbent holds the boot claim, and is released at ERROR with the queue
// told. Measured 2026-09-27: five quiesce leases of a failed handover
// survived a fresh boot and refused every prompt of their workspaces.
func TestTheBootReleasesALeaseItsOwnerLeftBehind(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	orphan := h.previousProcessLease(t, ws.ID, wsm.HolderRestart)

	// Act
	report, err := h.seq.Run(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if _, held, err := h.db.Lease(context.Background(), ws.ID); err != nil || held {
		t.Fatalf("Lease after the boot = (held %v, %v), want the orphan released", held, err)
	}
	if len(report.OrphanLeases) != 1 || report.OrphanLeases[0].ID != orphan.ID {
		t.Fatalf("report.OrphanLeases = %+v, want exactly %s", report.OrphanLeases, orphan.ID)
	}
	if changes := h.queue.leaseChanges(); len(changes) != 1 || changes[0] != ws.ID {
		t.Fatalf("the queue was told of lease changes %v, want [%s]", changes, ws.ID)
	}
	if !h.hasRecord("error", "daemon.boot.orphan_leases") {
		t.Fatalf("the orphan's release was not stated at ERROR: %v", h.log.Records())
	}
}

// TestTheBootKeepsALeaseItsOwnLiveOperationHolds pins the other side: a lease
// THIS process's handle took belongs to an operation that is still running,
// and the sweep leaves it standing.
func TestTheBootKeepsALeaseItsOwnLiveOperationHolds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	own, err := h.db.AcquireLease(context.Background(), ws.ID, wsm.HolderRestart, wsm.PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	report, err := h.seq.Run(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	got, held, err := h.db.Lease(context.Background(), ws.ID)
	if err != nil || !held || got.ID != own.ID {
		t.Fatalf("Lease after the boot = (%+v, held %v, %v), want this process's own %s kept", got, held, err, own.ID)
	}
	if len(report.OrphanLeases) != 0 || h.hasRecord("error", "daemon.boot.orphan_leases") {
		t.Fatalf("the boot released a live operation's lease: %+v", report.OrphanLeases)
	}
}

// TestAJoiningBootReleasesNoLease pins that a joining successor never sweeps:
// the incumbent that spawned it is alive and owns its leases.
func TestAJoiningBootReleasesNoLease(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps, _ *harness) { d.JoiningAddress = "127.0.0.1:41111" })
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	incumbents := h.previousProcessLease(t, ws.ID, wsm.HolderRestart)

	// Act
	report, err := h.seq.Run(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	got, held, err := h.db.Lease(context.Background(), ws.ID)
	if err != nil || !held || got.ID != incumbents.ID {
		t.Fatalf("Lease after a joining boot = (%+v, held %v, %v), want the incumbent's %s kept", got, held, err, incumbents.ID)
	}
	if len(report.OrphanLeases) != 0 {
		t.Fatalf("a joining boot released leases: %+v", report.OrphanLeases)
	}
}

func TestTheBootFailsWhenAnOrphanLeaseCannotBeReleased(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps, _ *harness) { d.DB = failingRelease{DB: d.DB, err: errBoom} })
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.previousProcessLease(t, ws.ID, wsm.HolderRestart)

	// Act
	_, err := h.seq.Run(context.Background())

	// Assert
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run = %v, want the release failure", err)
	}
}

func TestTheBootFailsWhenTheLeasesCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps, _ *harness) { d.DB = failingForeign{DB: d.DB, err: errBoom} })

	// Act
	_, err := h.seq.Run(context.Background())

	// Assert
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run = %v, want the read failure", err)
	}
}

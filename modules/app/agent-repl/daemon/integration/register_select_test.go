//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

func TestRegisterWorkspaceIsIdempotentAcrossSpellings(t *testing.T) {
	t.Parallel()
	// Arrange
	d := canonicalView(t)
	repo := harness.NewRepo(t)
	first := harness.Register(t, d, repo.Dir)
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(repo.Dir, link); err != nil {
		t.Fatalf("symlink the worktree: %v", err)
	}

	tests := []struct {
		name string
		dir  string
	}{
		{name: "trailing slash", dir: repo.Dir + string(filepath.Separator)},
		{name: "dot segments", dir: filepath.Join(repo.Dir, "..", filepath.Base(repo.Dir))},
		{name: "symlink", dir: link},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			again := harness.Register(t, d, tc.dir)

			// Assert
			if again.GetId() != first.GetId() {
				t.Fatalf("RegisterWorkspace(%s) = id %q, want the same id %q as %s", tc.dir, again.GetId(), first.GetId(), repo.Dir)
			}
		})
	}
}

// TestRegisterWorkspaceAfterARestartIsIdempotent pins registration across a
// daemon restart: re-registering the same directory on a fresh daemon
// process over the SAME state root (so wsm.db persists) answers the id the
// first daemon minted, and the roster carries no second row for it.
func TestRegisterWorkspaceAfterARestartIsIdempotent(t *testing.T) {
	t.Parallel()
	// Arrange: register once, then restart the daemon on the same state root.
	f := newRegistered(t, harness.Opts{DurableState: true})
	firstID := f.ws.GetId()
	f.d.Stop()
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})

	// Act: register the SAME directory again, post-restart.
	again := harness.Register(t, d2, f.repo.Dir)

	// Assert: the same id, minted once by the first boot.
	if again.GetId() != firstID {
		t.Fatalf("RegisterWorkspace(%s) after a restart = id %q, want the same id %q the first boot minted", f.repo.Dir, again.GetId(), firstID)
	}

	// Assert: no second roster row for it.
	roster := d2.WatchRoster()
	got := awaitRoster(t, d2, roster, "the roster after a post-restart re-registration", func(r *frontendv1.RosterPush) bool {
		return rosterRow(r, firstID) != nil
	})
	count := 0
	for _, id := range repoRowIDs(got) {
		if id == firstID {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("the roster carries %d rows for workspace %q after a post-restart re-registration, want exactly 1", count, firstID)
	}
}

func TestReselectingAWorkspaceProducesNoDuplicatePush(t *testing.T) {
	t.Parallel()
	// It changes the daemon's one selection.
	// Arrange
	f := newRegisteredInGroup(t, groupSelection)
	// A SELECT STARTS NO SHIM (owner ruling, 2026-10-28), so the window below
	// is the re-selection's alone.
	f.selectWorkspace()
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the latest roster", func(r *frontendv1.RosterPush) bool {
		return r.GetWorkspaces().GetCurrent().GetWorkspace().GetId() == f.ws.GetId()
	})

	// Act
	f.selectWorkspace()

	// Assert
	harness.ExpectNoPush(t, roster, "re-selecting the current workspace is a success that changes no view")
}

// TestSwitchingWorkspacesIsOneRosterPush pins that a switch reaches a roster
// subscriber as ONE push carrying the new selection. The registry republish
// and the selection used to be two pushes per switch, which Emacs and every
// page decoded, applied and repainted twice (2026-10-08).
func TestSwitchingWorkspacesIsOneRosterPush(t *testing.T) {
	t.Parallel()
	// Arrange: two opened workspaces, both ready, A selected.
	d := viewInGroup(t, groupSelection)
	opened := func(repo *harness.Repo) *fixture {
		f := &fixture{d: d, repo: repo, ws: harness.Register(t, d, repo.Dir), t: t}
		f.open()
		f.host = d.WatchHost(f.ws)
		f.web = d.WatchWeb(f.ws)
		return f
	}
	fa := opened(harness.NewRepo(t))
	fb := opened(harness.NewRepo(t))
	roster := d.WatchRoster()
	fa.selectWorkspace()
	awaitRoster(t, d, roster, "both ready with A current", func(r *frontendv1.RosterPush) bool {
		rowA, rowB := rosterRow(r, fa.ws.GetId()), rosterRow(r, fb.ws.GetId())
		return r.GetWorkspaces().GetCurrent().GetWorkspace().GetId() == fa.ws.GetId() &&
			rowA != nil && rowA.GetReady() != nil && rowB != nil && rowB.GetReady() != nil
	})

	// Act
	fb.selectWorkspace()

	// Assert
	ctx, cancel := d.WaitCtx()
	defer cancel()
	got := harness.AwaitNext(t, ctx, roster, "the switch's push")
	if got.GetWorkspaces().GetCurrent().GetWorkspace().GetId() != fb.ws.GetId() {
		t.Fatalf("the switch's push names current %v, want B %s", got.GetWorkspaces().GetCurrent(), fb.ws.GetId())
	}
	harness.ExpectNoPush(t, roster, "a switch is exactly one roster push")
}

// TestRegisterWorkspaceAnswersWhileTheBootsOwnBringUpIsStuck pins the fix
// landed in 71b59ab87: RegisterWorkspace's revival of an announced
// workspace's recorded conversation used to call Sessions.Start INLINE, and
// Start takes the workspace's per-workspace start gate -- the very gate the
// boot's own bring-up of that same workspace holds for the whole of its
// start. A shim that never answers StartSession therefore held
// RegisterWorkspace open indefinitely, and Emacs timed it out at its own 10s
// bound. The fix detaches the start (Fleet.StartDetached), so the register
// answers from the registry rather than from a session start.
func TestRegisterWorkspaceAnswersWhileTheBootsOwnBringUpIsStuck(t *testing.T) {
	t.Parallel()
	// Arrange: open a workspace to mint a vendor session, then force-kill it.
	// The workspace stays OPEN with a recorded conversation and no live shim
	// -- exactly what the boot's own reconciliation hands BringUp as
	// "pending bring-up" on the next boot.
	f := newOpened(t, harness.Opts{DurableState: true})
	f.d.ExpectWarnings("daemon.shimclient.exit", "daemon.shimclient.kill_session",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_session",
		"daemon.sessionwatcher.watch_agent", "daemon.workspace.kill",
		"daemon.shimclient.redial", "daemon.sessionwatcher.reopen")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.Stop()

	// A shim profile that never answers StartSession, filed BEFORE the
	// successor starts: its own boot bring-up is what spawns the shim that
	// reads it, which is exactly the race a control-socket script would lose.
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{HangStartSession: true})
	opts := f.d.SuccessorOpts()
	opts.ExtraArgs = append(opts.ExtraArgs, "--multi-repo-config-dir", f.d.MultiRepoConfigDir)
	successor := harness.StartDaemon(t, opts)
	// NOTHING IS DECLARED FOR THE STUCK START. The boot's own bring-up and the
	// register's detached revival both call Sessions.Start against a shim that
	// never answers, and both settle only when this daemon's own teardown
	// stands that shim down -- which is a teardown it ORDERED, and is recorded
	// as such rather than as a fault. The cleanup sweep is what pins that.

	// Act: RegisterWorkspace re-announces the SAME workspace, on a context
	// bounded by harness.DefaultTimeout -- the harness's ONE-WAIT failure
	// bound -- rather than the whole run budget.
	ctx, cancel := grantedclock.WithBound(successor.Ctx(), harness.DefaultTimeout)
	defer cancel()
	resp, err := successor.Client().RegisterWorkspace(ctx, connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: f.repo.Dir}))

	// Assert: it answers, naming the same workspace, well inside the bound --
	// even though the boot's own bring-up of this same workspace is stuck in
	// a StartSession the shim will never answer.
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s) = error %v, want a success within %s", f.repo.Dir, err, harness.DefaultTimeout)
	}
	if got := resp.Msg.GetSuccess().GetWorkspace().GetId(); got != f.ws.GetId() {
		t.Fatalf("RegisterWorkspace(%s) = workspace %q, want the same workspace %q", f.repo.Dir, got, f.ws.GetId())
	}
}

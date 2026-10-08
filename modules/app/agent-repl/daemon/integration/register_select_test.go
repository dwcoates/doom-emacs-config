//go:build integration

package integration

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

func TestRegisterWorkspaceIsIdempotentAcrossSpellings(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
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
	f := newRegistered(t, harness.Opts{})
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
	got := awaitRoster(t, d2, roster, "the roster after a post-restart re-registration", func(r *frontendv1.WorkspaceRoster) bool {
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

func TestRegisterWorkspaceRefusesANonWorktree(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	plain := t.TempDir()

	// Act
	resp, err := d.Client().RegisterWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: plain}))

	// Assert: `not_a_worktree` is a LANDED arm of RegisterWorkspaceError, so
	// the refusal is answered IN BAND rather than as a transport error. (This
	// assertion was written against the unlanded-arm path; the arm landed with
	// the proto, and an in-band arm is never also a Connect error.)
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s) = transport error %v, want the in-band not_a_worktree arm", plain, err)
	}
	if resp.Msg.GetError().GetNotAWorktree() == nil {
		t.Fatalf("RegisterWorkspace(%s) = %v, want RegisterWorkspaceError.not_a_worktree", plain, resp.Msg)
	}
}

func TestSelectWorkspaceStampsCurrentOnTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	roster := f.d.WatchRoster()

	// Act
	f.selectWorkspace()

	// Assert
	got := awaitRoster(t, f.d, roster, "the selected workspace stamped current", func(r *frontendv1.WorkspaceRoster) bool {
		return r.GetCurrent().GetWorkspace().GetId() == f.ws.GetId()
	})
	row := rosterRow(got, f.ws.GetId())
	if row == nil {
		t.Fatalf("the roster has no row for the selected workspace %s", f.ws.GetId())
	}
	if !row.GetCurrent().GetCurrent() {
		t.Fatalf("row.current = false for the selected workspace, want true")
	}
	// SELECTING DOES NOT DRIVE THE WHEN-COLUMN. The column shows last activity,
	// never last viewing, so a freshly-selected workspace that has taken no turn
	// falls back to its creation time. (The column's old last_selected arm is
	// reserved, so only the instant shown can regress.)
	if row.GetWhen().GetCreated().GetAtMs() == 0 {
		t.Fatalf("row.when.created = %v, want the creation instant for a never-active workspace", row.GetWhen())
	}
}

func TestReselectingAWorkspaceProducesNoDuplicatePush(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	f.selectWorkspace()
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the latest roster", func(r *frontendv1.WorkspaceRoster) bool {
		return r.GetCurrent().GetWorkspace().GetId() == f.ws.GetId()
	})

	// Act
	f.selectWorkspace()

	// Assert
	harness.ExpectNoPush(t, roster, harness.ProbeWindow, "re-selecting the current workspace is a success that changes no view")
}

// TestSwitchingWorkspacesIsOneRosterPush pins that a switch reaches a roster
// subscriber as ONE push carrying the new selection. The registry republish
// and the selection used to be two pushes per switch, which Emacs and every
// page decoded, applied and repainted twice (2026-10-08).
func TestSwitchingWorkspacesIsOneRosterPush(t *testing.T) {
	t.Parallel()
	// Arrange: two opened workspaces, both ready, A selected.
	d := newDaemon(t, harness.Opts{})
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
	awaitRoster(t, d, roster, "both ready with A current", func(r *frontendv1.WorkspaceRoster) bool {
		rowA, rowB := rosterRow(r, fa.ws.GetId()), rosterRow(r, fb.ws.GetId())
		return r.GetCurrent().GetWorkspace().GetId() == fa.ws.GetId() &&
			rowA != nil && rowA.GetReady() != nil && rowB != nil && rowB.GetReady() != nil
	})

	// Act
	fb.selectWorkspace()

	// Assert
	ctx, cancel := d.WaitCtx()
	defer cancel()
	got := harness.AwaitNext(t, ctx, roster, "the switch's push")
	if got.GetCurrent().GetWorkspace().GetId() != fb.ws.GetId() {
		t.Fatalf("the switch's push names current %v, want B %s", got.GetCurrent(), fb.ws.GetId())
	}
	harness.ExpectNoPush(t, roster, harness.ProbeWindow, "a switch is exactly one roster push")
}

// TestPerWorkspaceRpcRefusesAnUnknownWorkspace asserts the refusal IN BAND.
// `CloseWorkspaceError.unknown_workspace` is a LANDED arm
// (endpoint_close_workspace.proto), and a landed arm is never also a Connect
// error — the response's `error` result IS the refusal.
//
// (Written originally against the unlanded-arm path, which answered
// CodeNotFound with an `intended arm:` message. The arm landed with the
// contract, so the assertion moved onto it; the same amendment already stands
// on TestRegisterWorkspaceRefusesANonWorktree above.)
func TestPerWorkspaceRpcRefusesAnUnknownWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// unknown_workspace is a LANDED CloseWorkspaceError arm (see the comment
	// above), answered in band at DEBUG through server.refuse — never the
	// daemon.refusal.unlanded_arm WARN this path was originally written
	// against.
	unknown := &workspacev1.WorkspaceRef{Id: "no-such-workspace", Dir: t.TempDir()}

	// Act
	resp, err := d.Client().CloseWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: unknown}))

	// Assert
	if err != nil {
		t.Fatalf("CloseWorkspace on an unknown workspace = transport error %v, want the in-band unknown_workspace arm", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("CloseWorkspace on an unknown workspace = %v, want CloseWorkspaceError.unknown_workspace", resp.Msg)
	}
}

// TestPerWorkspaceRpcRefusesARefWhoseDirDisagrees pins the WorkspaceRef echo
// ruling: the daemon keys on `id` and REFUSES a ref whose `dir` disagrees with
// the registry. `workspace_ref_mismatch` is landed too, and its arm carries
// the dir the registry actually holds, so the client can correct its echo.
func TestPerWorkspaceRpcRefusesARefWhoseDirDisagrees(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	// workspace_ref_mismatch is a LANDED CloseWorkspaceError arm too, answered
	// in band at DEBUG — never the daemon.refusal.unlanded_arm WARN this path
	// was originally written against.
	mismatched := &workspacev1.WorkspaceRef{Id: f.ws.GetId(), Dir: t.TempDir()}

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: mismatched}))

	// Assert
	if err != nil {
		t.Fatalf("CloseWorkspace with a mismatched ref dir = transport error %v, want the in-band arm", err)
	}
	arm := resp.Msg.GetError().GetWorkspaceRefMismatch()
	if arm == nil {
		t.Fatalf("CloseWorkspace with a mismatched ref dir = %v, want CloseWorkspaceError.workspace_ref_mismatch", resp.Msg)
	}
	if arm.GetRegistryDir() != f.ws.GetDir() {
		t.Fatalf("workspace_ref_mismatch.registry_dir = %q, want the registry's dir %q", arm.GetRegistryDir(), f.ws.GetDir())
	}
}

func TestSubmitPromptWithoutSaidIsInvalidArgument(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	err := f.submitExpectingError(&agentreplv1.SubmitPromptRequest{
		Workspace:      f.ws,
		IdempotencyKey: "no-said",
		Origin:         conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
	})

	// Assert
	if connectCode(err) != connect.CodeInvalidArgument {
		t.Fatalf("SubmitPrompt without said = %v (code %v), want CodeInvalidArgument", err, connectCode(err))
	}
	if !containsField(err, "said") {
		t.Fatalf("SubmitPrompt refusal = %v, want it to name the unset field \"said\"", err)
	}
	// Validation refusals are plain connect.NewError(InvalidArgument, ...)
	// (internal/server/validate.go's `invalid`), with no logging at all.
}

// TestSelectWorkspaceRefusesABogusWorkspaceRef pins the IN-BAND
// unknown_workspace arm on SelectWorkspace itself (endpoint_select_workspace.proto:
// SelectWorkspaceError.unknown_workspace), mirroring the same arm already
// pinned on CloseWorkspace above.
func TestSelectWorkspaceRefusesABogusWorkspaceRef(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// unknown_workspace is answered in band at DEBUG through server.refuse,
	// never the daemon.refusal.unlanded_arm WARN.
	bogus := &workspacev1.WorkspaceRef{Id: "no-such-workspace", Dir: t.TempDir()}

	// Act
	resp, err := d.Client().SelectWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: bogus}))

	// Assert
	if err != nil {
		t.Fatalf("SelectWorkspace on a bogus workspace ref = transport error %v, want the in-band unknown_workspace arm", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("SelectWorkspace on a bogus workspace ref = %v, want SelectWorkspaceError.unknown_workspace", resp.Msg)
	}
}

// TestSelectingAnotherWorkspaceLeavesTheFirstsAttentionMarkerSet pins the
// attention-marker clearing rule precisely: SelectWorkspace clears the
// marker ONLY on the workspace it names. Selecting workspace B must leave
// workspace A's own marker standing.
func TestSelectingAnotherWorkspaceLeavesTheFirstsAttentionMarkerSet(t *testing.T) {
	t.Parallel()
	// Arrange: two workspaces in separate repos, A carrying a notification.
	d := newDaemon(t, harness.Opts{})
	repoA := harness.NewRepo(t)
	repoB := harness.NewRepo(t)
	a := harness.Register(t, d, repoA.Dir)
	b := harness.Register(t, d, repoB.Dir)
	roster := d.WatchRoster()

	fa := &fixture{d: d, repo: repoA, ws: a, t: t}
	fa.open()
	fa.host = d.WatchHost(a)
	fa.web = d.WatchWeb(a)
	fa.submit("go", "k-attn-a", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	fa.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-attn-a", "act-attn-a")},
	}))
	awaitRoster(t, d, roster, "workspace A's attention marker set", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, a.GetId())
		return row != nil && row.GetAttention() != nil
	})

	// Act: select B, never A.
	if _, err := d.Client().SelectWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: b})); err != nil {
		t.Fatalf("SelectWorkspace(B) = error %v, want a success", err)
	}

	// Assert: B is now current, and A's marker is UNTOUCHED.
	got := awaitRoster(t, d, roster, "B stamped current", func(r *frontendv1.WorkspaceRoster) bool {
		return r.GetCurrent().GetWorkspace().GetId() == b.GetId()
	})
	rowA := rosterRow(got, a.GetId())
	if rowA == nil {
		t.Fatalf("the roster has no row for workspace A %s after selecting B", a.GetId())
	}
	if rowA.GetAttention() == nil {
		t.Fatalf("workspace A's attention marker was cleared by selecting B, want it left set: only A's own SelectWorkspace clears it")
	}
}

// TestRegisteringAClosedWorkspaceReopensIt is the wire half of the register
// that produced a workspace with NO TAB.
//
// Registration is idempotent by dir and answers the row that is already there.
// A row a previous CLOSE had marked closed came back closed, and `closed` is
// the editor's whole tab-membership rule (lisp/roster.el's
// `agent-repl-roster-desired-tabs'): the tab was never drawn, the minted ref's
// landing waited for a tab that was not coming, and nothing could resolve the
// workspace by name to act on it.
func TestRegisteringAClosedWorkspaceReopensIt(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace, closed.
	f := newRegistered(t, harness.Opts{})
	if _, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("CloseWorkspace on a quiet workspace = error %v, want a success", err)
	}
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the closed row", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetClosed().GetClosed()
	})

	// Act: announce the same directory again.
	again := harness.Register(t, f.d, f.repo.Dir)

	// Assert: the same workspace, and its row is open again.
	if again.GetId() != f.ws.GetId() {
		t.Fatalf("re-registering %s minted %q, want the same workspace %q", f.repo.Dir, again.GetId(), f.ws.GetId())
	}
	awaitRoster(t, f.d, roster, "the re-registered row drawn open", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && !row.GetClosed().GetClosed()
	})
}

// TestTheRosterDropsARepositoryWhoseDirectoryIsGone covers the create that
// produced nothing at all.
//
// The roster's repository sections are `SPC TAB n”s CREATE TARGETS
// (lisp/verbs.el's `agent-repl-verbs--read-repository'), and nothing ever
// forgets a repository row. A repository whose tree has been deleted therefore
// stayed in the picker, drawing the same label as a live repository of the
// same base name, and the create that picked the dead one failed at git.
func TestTheRosterDropsARepositoryWhoseDirectoryIsGone(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace whose whole repository is then deleted.
	f := newRegistered(t, harness.Opts{})
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the registered row", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()) != nil
	})
	if err := os.RemoveAll(f.repo.Dir); err != nil {
		t.Fatalf("remove the repository directory: %v", err)
	}
	// The boot reconciliation warns once about the directory it closed, and
	// both daemons share this state root's log. That warning is the deleted
	// tree being reported, which is the point of the arrange.
	f.d.ExpectWarnings("daemon.boot.close_missing_dir")

	// Act: the opening publish is the walk that stats what is there, so the
	// roster is read from a daemon booting over the same state root.
	f.d.Stop()
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})
	d2.ExpectWarnings("daemon.boot.close_missing_dir")

	// Assert.
	got := awaitRoster(t, d2, d2.WatchRoster(), "a roster with no section for the deleted repository",
		func(r *frontendv1.WorkspaceRoster) bool { return len(r.GetRepository().GetSections()) == 0 })
	if rosterRow(got, f.ws.GetId()) != nil {
		t.Fatalf("the roster still carries a row for %q under a repository that is not on disk", f.ws.GetId())
	}
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
	f := newOpened(t, harness.Opts{})
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
	successor := harness.StartDaemon(t, harness.Opts{
		StateDir:   f.d.StateDir,
		ProfileDir: f.d.ProfileDir,
		ExtraArgs: []string{
			"--default-config-dir", f.d.DefaultConfigDir,
			"--multi-repo-config-dir", f.d.MultiRepoConfigDir,
		},
		ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
	})
	// NOTHING IS DECLARED FOR THE STUCK START. The boot's own bring-up and the
	// register's detached revival both call Sessions.Start against a shim that
	// never answers, and both settle only when this daemon's own teardown
	// stands that shim down -- which is a teardown it ORDERED, and is recorded
	// as such rather than as a fault. The cleanup sweep is what pins that.

	// Act: RegisterWorkspace re-announces the SAME workspace, on a context
	// bounded by harness.DefaultTimeout -- the harness's ONE-WAIT failure
	// bound -- rather than the whole run budget.
	ctx, cancel := context.WithTimeout(successor.Ctx(), harness.DefaultTimeout)
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

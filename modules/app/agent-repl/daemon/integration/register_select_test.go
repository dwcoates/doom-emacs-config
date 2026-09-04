//go:build integration

package integration

import (
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
	if row.GetWhen().GetLastSelected().GetAtMs() == 0 {
		t.Fatalf("row.when.last_selected = %v, want the selection instant stamped", row.GetWhen())
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
	d.WatchWorkspaceLogs(repoA.Dir)
	d.WatchWorkspaceLogs(repoB.Dir)
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

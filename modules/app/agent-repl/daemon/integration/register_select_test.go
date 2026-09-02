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

func TestRegisterWorkspaceRefusesANonWorktree(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	plain := t.TempDir()

	// Act
	_, err := d.Client().RegisterWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: plain}))

	// Assert
	if err == nil {
		t.Fatalf("RegisterWorkspace(%s) = success, want a refusal: the directory is not a git worktree", plain)
	}
	if !namesIntendedArm(err, "RegisterWorkspaceError.") {
		t.Fatalf("RegisterWorkspace refusal = %v, want it to name RegisterWorkspaceError.<arm>", err)
	}
	d.AwaitRunLogOperation("daemon.refusal.unlanded_arm")
	d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

func TestSelectWorkspaceStampsCurrentOnTheRoster(t *testing.T) {
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

func TestPerWorkspaceRpcRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	unknown := &workspacev1.WorkspaceRef{Id: "no-such-workspace", Dir: t.TempDir()}

	// Act
	_, err := d.Client().CloseWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: unknown}))

	// Assert
	if connectCode(err) != connect.CodeNotFound {
		t.Fatalf("CloseWorkspace on an unknown workspace = %v (code %v), want CodeNotFound", err, connectCode(err))
	}
	if !namesIntendedArm(err, "CloseWorkspaceError.") {
		t.Fatalf("CloseWorkspace refusal = %v, want it to name CloseWorkspaceError.<arm>", err)
	}
	d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

func TestPerWorkspaceRpcRefusesARefWhoseDirDisagrees(t *testing.T) {
	// Arrange
	f := newRegistered(t, harness.Opts{})
	mismatched := &workspacev1.WorkspaceRef{Id: f.ws.GetId(), Dir: t.TempDir()}

	// Act
	_, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: mismatched}))

	// Assert
	if err == nil {
		t.Fatal("CloseWorkspace with a mismatched ref dir = success, want a refusal")
	}
	if !namesIntendedArm(err, "workspace_ref_mismatch") {
		t.Fatalf("CloseWorkspace refusal = %v, want it to name the workspace_ref_mismatch arm", err)
	}
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")
}

func TestSubmitPromptWithoutSaidIsInvalidArgument(t *testing.T) {
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
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

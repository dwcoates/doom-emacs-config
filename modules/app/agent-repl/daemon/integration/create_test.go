//go:build integration

package integration

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// ---- Standard create ----

func TestCreateWorkspaceStandardDerivesTheSlugCreatesTheBranchAndSubmitsTheInitialPromptWithWorkspaceCreatedOrigin(t *testing.T) {
	t.Parallel()
	// Arrange
	d := canonicalView(t)
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Originator:    &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_UserOrWorkspace{UserOrWorkspace: &agentreplv1.WorkspaceOriginatorUserOrWorkspace{}}},
		Repository:    repository,
		Requester:     harness.User(),
		ContextOrigin: harness.Fresh(),
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Relationship:  harness.Peer(),
			InitialPrompt: said("fix the flaky reconnect test"),
		}},
	}))

	// Assert: minted ref, worktree and branch created off the default branch.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(standard) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if ws.GetId() == "" || ws.GetDir() == "" {
		t.Fatalf("CreateWorkspace success = %v, want a minted workspace ref", resp.Msg)
	}
	if !repo.HasWorktree(ws.GetDir()) {
		t.Fatalf("the repository's worktrees = %v, want the new worktree %q", repo.Worktrees(), ws.GetDir())
	}
	if got := len(repo.Branches()); got < 2 {
		t.Fatalf("the repository's branches = %v, want a new branch cut off %q for the workspace", repo.Branches(), harness.DefaultBranch)
	}

	// Assert: a slug was derived.
	host := d.WatchHost(ws)
	naming := harness.AwaitView(t, d.Ctx(), host, "a derived slug", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetNaming().GetSlug() != ""
	}).GetHost().GetNaming()
	if naming.GetSlug() == "" {
		t.Fatalf("the new workspace's naming.slug = %q, want a slug derived from the initial prompt", naming.GetSlug())
	}

	// Assert: the initial prompt was submitted, WORKSPACE_CREATED-attributed.
	shim := d.Shim(ws)
	fresh := shim.ExpectStartSession()
	if fresh.GetFresh() == nil {
		t.Fatalf("StartSession request = %v, want fresh for a brand-new workspace", fresh)
	}
	turn := shim.ExpectStartTurn()
	if want := "fix the flaky reconnect test" + d.OwedSummaryNote(t); text(turn.GetSaid()) != want {
		t.Fatalf("StartTurn.said = %q, want the initial prompt verbatim, closed by the owed-summary note %q", text(turn.GetSaid()), want)
	}
	if turn.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED {
		t.Fatalf("StartTurn.origin = %v, want WORKSPACE_CREATED", turn.GetOrigin())
	}
}

// ---- One-shot create ----

// ---- Deploy hooks ----

// ---- NukeWorkspace: kill-before-destroy ordering, and a git failure ----

// ---- CloseWorkspace: blocked by live detached work with no turn open ----

// ---- create* helpers (prefixed create* so they cannot collide) ----

// createRepositoryRef registers a fresh workspace under a repository and
// reads back the daemon-minted RepositoryRef from the roster's repo section,
// since CreateWorkspace needs the echo token and no direct mint RPC exists.
// newReportedCreateDaemon is a daemon of its own for a case whose create
// fails in a way the daemon reports to the repository's main worktree: the
// report reopens that workspace and queues a brief to it after the case is
// answered, on work no workspace owns, so a shared slice's retirement would
// race it.
func newReportedCreateDaemon(t *testing.T) *harness.Daemon {
	t.Helper()
	return newDaemon(t, harness.Opts{})
}

func createRepositoryRef(t *testing.T, d *harness.Daemon, repo *harness.Repo) *workspacev1.RepositoryRef {
	t.Helper()
	ws := harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()
	got := harness.AwaitView(t, d.Ctx(), roster, "the repository section", func(r *frontendv1.RosterPush) bool {
		return rosterRepoRow(r, ws.GetId()) != nil
	})
	for _, s := range got.GetWorkspaces().GetRepository().GetSections() {
		for _, open := range rosterwalk.SectionOpenRows(s) {
			if open.GetRow().GetWorkspace().GetWorkspace().GetId() == ws.GetId() {
				return s.GetKey().GetRepository()
			}
		}
	}
	t.Fatalf("no repository section found for %s", repo.Dir)
	return nil
}

// createArgsContain reports whether a git call's args contain an exact token.
// createGitDir answers the directory a recorded git call selected with -C.
func createGitDir(args []string) string {
	for i, a := range args {
		if a == "-C" && i+1 < len(args) {
			return args[i+1]
		}
	}
	return ""
}

func createArgsContain(args []string, want string) bool {
	for _, a := range args {
		if a == want {
			return true
		}
	}
	return false
}

// TestCreateWorkspaceUnderTheVendorGuardAloneSpawnsAFakeShim asserts a guarded
// daemon CREATES a workspace, and that the shim its bring-up spawned is running
// in fake mode.
//
// THIS IS THE REGRESSION THE WORKSPACE REALTESTS FOUND. The guard used to
// refuse the shim spawn outright, so a guarded daemon could not create a
// workspace at all: the spawn raised the guard's ForbiddenError, the bring-up
// answered `spawn_failed`, and every realtest that needed a second workspace
// died there. The guard means "never touch the real vendor", and a shim is our
// own process with a fake mode, so it now FORCES the fake instead of refusing.
//
// NoFake withholds the whole stack's fake mode and WithoutFakeShimsHook
// withholds AGENT_REPL_FAKE_SHIMS, which leaves the vendor guard as the ONLY
// thing that can make this spawn fake — so a shim that comes up fake here came
// up fake because of the guard and nothing else.
func TestCreateWorkspaceUnderTheVendorGuardAloneSpawnsAFakeShim(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{NoFake: true, WithoutFakeShimsHook: true})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Originator:    &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_UserOrWorkspace{UserOrWorkspace: &agentreplv1.WorkspaceOriginatorUserOrWorkspace{}}},
		Repository:    repository,
		Requester:     harness.User(),
		ContextOrigin: harness.Fresh(),
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Relationship:  harness.Peer(),
			InitialPrompt: said("fix the flaky reconnect test"),
			// A NAME IS SUPPLIED SO THE SUBJECT STAYS THE SHIM. This daemon is
			// started without AGENT_REPL_CLAUDE_BIN on purpose, so the
			// headless naming call a nameless create makes would be refused by
			// the very guard this test is about — and the create would never
			// reach the spawn it is asserting on.
			Name: strPtr("guarded-spawn"),
		}},
	}))

	// Assert: the create succeeded rather than answering the spawn refusal.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace under the vendor guard alone = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()

	// Assert: the bring-up spawned a shim, and it is a FAKE one.
	shim := d.Shim(ws)
	if !shim.Info().Fake {
		t.Fatalf("the spawned shim's argv = %v, want --fake forced by the vendor guard", shim.Info().Argv)
	}
}

// TestCreateWorkspaceUnderTheVendorGuardAloneStartsTheSession asserts the
// bring-up under the guard goes all the way to a started session, not merely to
// a live process: the fake-mode shim serves StartSession and the initial prompt
// reaches it.
func TestCreateWorkspaceUnderTheVendorGuardAloneStartsTheSession(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{NoFake: true, WithoutFakeShimsHook: true})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Originator:    &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_UserOrWorkspace{UserOrWorkspace: &agentreplv1.WorkspaceOriginatorUserOrWorkspace{}}},
		Repository:    repository,
		Requester:     harness.User(),
		ContextOrigin: harness.Fresh(),
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Relationship:  harness.Peer(),
			InitialPrompt: said("fix the flaky reconnect test"),
			// See the sibling test: the name is supplied so the guarded
			// naming call is never made and the subject stays the session.
			Name: strPtr("guarded-session"),
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace under the vendor guard alone = (%v, %v), want a success", resp, err)
	}

	// Assert
	shim := d.Shim(resp.Msg.GetSuccess().GetWorkspace())
	if fresh := shim.ExpectStartSession(); fresh.GetFresh() == nil {
		t.Fatalf("StartSession request = %v, want fresh for a brand-new workspace", fresh)
	}
	want := "fix the flaky reconnect test" + d.OwedSummaryNote(t)
	if turn := shim.ExpectStartTurn(); text(turn.GetSaid()) != want {
		t.Fatalf("StartTurn.said = %q, want the initial prompt verbatim, closed by the owed-summary note %q", text(turn.GetSaid()), want)
	}
}

// TestCreateWorkspaceWhoseShimWillNotComeUpAnswersTheSpawnFailedArm pins the
// gap the workspace realtests left behind, now closed: CreateWorkspaceError
// carries `spawn_failed` as of 2026-09-12, so a create whose bring-up cannot
// start a shim states its refusal IN BAND, as its OpenWorkspace twin
// (roster_test.go) always could.
//
// The refusal itself never changed — `workspace.ArmSpawnFailed`, renamed onto
// the rpc that raised it — and no daemon change followed the arm: `server.fill`
// already supplied `detail` for any arm that has the field, so the handler
// switched onto the arm by the arm simply existing. This test was written
// asserting the unlanded-arm Connect error and prescribing this very flip.
func TestCreateWorkspaceWhoseShimWillNotComeUpAnswersTheSpawnFailedArm(t *testing.T) {
	t.Parallel()
	// Arrange: the create mints the workspace dir, so the dying shim is
	// scripted through the profile EVERY spawn falls back to.
	d := newReportedCreateDaemon(t)
	// THE FAILED CREATE IS THEN REPORTED, ASYNCHRONOUSLY, to the repository's
	// main-worktree workspace: the report is queued, and its revival dies by
	// the same default profile, which the queue says at ERROR. Awaited rather
	// than left to race the test's end: whether that record landed before the
	// sweep used to decide the test's verdict.
	d.RequireWarnings("daemon.promptqueue.submit")
	// The sweep covers every test; the declared records are evidence of the bring-up death the test scripts.
	// The failed create is then reported to the repository's main-worktree
	// workspace (workspace.reportCreateFailure): the report is queued, and the
	// revival it occasions dies the same scripted death, which the queue
	// records at ERROR on its own goroutine.
	d.ExpectWarnings("daemon.shimclient.redial", "daemon.shimclient.exit", "daemon.shimclient.spawn",
		"daemon.workspace.bring_up", "daemon.workspace.create", "daemon.promptqueue.submit")
	d.WriteDefaultShimProfile(harness.ShimProfile{
		ExitOn: harness.ExitOnStartup, ExitCode: 7, Stderr: "boom: fake bring-up death",
	})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Originator:    &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_UserOrWorkspace{UserOrWorkspace: &agentreplv1.WorkspaceOriginatorUserOrWorkspace{}}},
		Repository:    repository,
		Requester:     harness.User(),
		ContextOrigin: harness.Fresh(),
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Relationship:  harness.Peer(),
			InitialPrompt: said("fix the flaky reconnect test"),
		}},
	}))

	// Assert: the refusal is the typed arm, carrying the spawn's own account.
	if err != nil {
		t.Fatalf("CreateWorkspace onto a dying shim: %v", err)
	}
	detail := resp.Msg.GetError().GetSpawnFailed().GetDetail()
	if detail == "" {
		t.Fatalf("CreateWorkspace onto a dying shim = %v, want the spawn_failed arm carrying a detail",
			resp.Msg.GetResult())
	}
	d.AwaitWorkspaceLogRecord(repo.Dir, "the failed create's report's revival failing", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.submit" && r.Level == "error"
	})
}

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/session"
	workspacecreate "claude-repld/internal/workspace/create"
)

type gitCall struct {
	args []string
	out  string
	exit int
	err  error
}

type fakeGitRunner struct {
	calls []gitCall
	seen  [][]string
}

type fakeProjectileMarker struct{ calls [][2]string }

func (f *fakeProjectileMarker) Ensure(path, name string) error {
	f.calls = append(f.calls, [2]string{path, name})
	return nil
}

func (f *fakeGitRunner) RunGit(_ context.Context, _ string, args ...string) (string, int, error) {
	f.seen = append(f.seen, args)
	if len(f.calls) == 0 {
		return "", -1, fmt.Errorf("unexpected git call %q", args)
	}
	call := f.calls[0]
	f.calls = f.calls[1:]
	if !reflect.DeepEqual(call.args, args) {
		return "", -1, fmt.Errorf("git args = %q, want %q", args, call.args)
	}
	return call.out, call.exit, call.err
}

func testGitRoot(t *testing.T) string {
	t.Helper()
	root := filepath.Join(t.TempDir(), "repo")
	if err := osMkdirAll(filepath.Join(root, ".git")); err != nil {
		t.Fatal(err)
	}
	return root
}

// osMkdirAll is a tiny test-only filesystem helper.  Its target is always a
// t.TempDir child, so tests never touch user state or invoke an external tool.
func osMkdirAll(path string) error { return os.MkdirAll(path, 0o755) }

func TestDaemonWorktreePlansCollisionDeterministically(t *testing.T) {
	root := testGitRoot(t)
	job := workspacecreate.Job{ID: "file-uuid:0", Request: workspacecreate.Request{Name: "DWC/feature", GitRoot: root, BaseCommit: "HEAD"}}
	git := &fakeGitRunner{calls: []gitCall{
		{args: []string{"rev-parse", "--verify", "HEAD^{commit}"}, out: "abc123\n"},
		{args: []string{"worktree", "list", "--porcelain"}, out: "worktree /tmp/other\nbranch refs/heads/DWC/feature\n\n"},
		{args: []string{"show-ref", "--verify", "--quiet", "refs/heads/" + candidateBranch(job.Request.Name, job.ID, 1)}, exit: 1},
	}}
	worktree := DaemonWorktree{Git: git, Marker: &fakeProjectileMarker{}, Logf: func(string, ...any) {}}
	plan, err := worktree.PlanWorktree(context.Background(), job)
	if err != nil {
		t.Fatal(err)
	}
	wantBranch := candidateBranch(job.Request.Name, job.ID, 1)
	if plan.FinalName != filepath.Base(wantBranch) || plan.Branch != wantBranch || plan.BaseCommit != "abc123" {
		t.Fatalf("plan = %#v", plan)
	}
	if filepath.Base(plan.Path) != filepath.Base(wantBranch) {
		t.Fatalf("path = %s, want tail %s", plan.Path, filepath.Base(wantBranch))
	}
}

func TestDaemonWorktreeForkUsesSourceWorkspaceHeadAndVendorSession(t *testing.T) {
	root := testGitRoot(t)
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "source-daemon", CWD: "/source-worktree", ClaudeSessionID: "vendor-source"}); err != nil {
		t.Fatal(err)
	}
	job := workspacecreate.Job{ID: "fork-job", Request: workspacecreate.Request{Name: "DWC/child", GitRoot: root, ForkFrom: "DWC/source-worktree"}}
	git := &fakeGitRunner{calls: []gitCall{
		{args: []string{"rev-parse", "--verify", "HEAD^{commit}"}, out: "source-head\n"},
		{args: []string{"worktree", "list", "--porcelain"}},
		{args: []string{"show-ref", "--verify", "--quiet", "refs/heads/DWC/child"}, exit: 1},
	}}
	plan, err := (DaemonWorktree{Git: git, Registry: reg, Marker: &fakeProjectileMarker{}, Logf: func(string, ...any) {}}).PlanWorktree(context.Background(), job)
	if err != nil {
		t.Fatal(err)
	}
	if plan.BaseCommit != "source-head" || plan.ForkSessionID != "vendor-source" {
		t.Fatalf("fork plan = %#v", plan)
	}
}

func TestDaemonWorktreeKeepsDivergentMaster(t *testing.T) {
	root := testGitRoot(t)
	git := &fakeGitRunner{calls: []gitCall{
		{args: []string{"fetch", "origin", "master"}},
		{args: []string{"merge-base", "--is-ancestor", "master", "origin/master"}, exit: 1},
	}}
	worktree := DaemonWorktree{Git: git, Logf: func(string, ...any) {}}
	if err := worktree.fastForwardMaster(context.Background(), root); err != nil {
		t.Fatal(err)
	}
	if len(git.calls) != 0 {
		t.Fatalf("unexpected calls: %#v", git.calls)
	}
}

func TestOSProjectileMarkerIsIdempotentAndRejectsMismatch(t *testing.T) {
	dir := t.TempDir()
	marker := osProjectileMarker{}
	if err := marker.Ensure(dir, "child"); err != nil {
		t.Fatal(err)
	}
	if err := marker.Ensure(dir, "child"); err != nil {
		t.Fatal(err)
	}
	if err := marker.Ensure(dir, "other"); err == nil {
		t.Fatal("mismatching projectile marker was accepted")
	}
}

func TestDaemonWorktreeResumesPersistedPlanWithoutSecondAdd(t *testing.T) {
	root := testGitRoot(t)
	path := filepath.Join(filepath.Dir(root), "repo-worktrees", "feature")
	job := workspacecreate.Job{ID: "j", Request: workspacecreate.Request{Name: "DWC/feature", GitRoot: root}, WorktreePath: path, FinalName: "DWC/feature", Branch: "DWC/feature", ResolvedBaseCommit: "HEAD"}
	git := &fakeGitRunner{calls: []gitCall{
		{args: []string{"worktree", "list", "--porcelain"}, out: "worktree " + path + "\nbranch refs/heads/DWC/feature\n\n"},
	}}
	if err := (DaemonWorktree{Git: git, Marker: &fakeProjectileMarker{}, Logf: func(string, ...any) {}}).EnsureWorktree(context.Background(), job); err != nil {
		t.Fatal(err)
	}
	if len(git.calls) != 0 {
		t.Fatalf("unexpected remaining git calls: %#v", git.calls)
	}
}

func TestDaemonWorktreeAddsPersistedPlan(t *testing.T) {
	root := testGitRoot(t)
	path := filepath.Join(filepath.Dir(root), "repo-worktrees", "feature")
	job := workspacecreate.Job{ID: "j", Request: workspacecreate.Request{Name: "DWC/feature", GitRoot: root}, WorktreePath: path, FinalName: "DWC/feature", Branch: "DWC/feature", ResolvedBaseCommit: "HEAD"}
	git := &fakeGitRunner{calls: []gitCall{
		{args: []string{"worktree", "list", "--porcelain"}},
		{args: []string{"worktree", "add", "-b", "DWC/feature", path, "HEAD"}},
	}}
	if err := (DaemonWorktree{Git: git, Marker: &fakeProjectileMarker{}, Logf: func(string, ...any) {}}).EnsureWorktree(context.Background(), job); err != nil {
		t.Fatal(err)
	}
	if len(git.calls) != 0 {
		t.Fatalf("unexpected remaining git calls: %#v", git.calls)
	}
}

// The start tag is no longer daemon business: Emacs creates it when it
// registers the daemon-created workspace.  Both worktree phases must therefore
// touch refs/tags not at all — reading one was the source of the exit=128
// planning failure that used to poison the whole inbox.
func TestDaemonWorktreeNeverTouchesStartTags(t *testing.T) {
	root := testGitRoot(t)
	path := filepath.Join(filepath.Dir(root), "repo-worktrees", "feature")
	tests := []struct {
		name  string
		calls []gitCall
		run   func(DaemonWorktree) error
	}{
		{
			name: "plan",
			calls: []gitCall{
				{args: []string{"rev-parse", "--verify", "HEAD^{commit}"}, out: "abc123\n"},
				{args: []string{"worktree", "list", "--porcelain"}},
				{args: []string{"show-ref", "--verify", "--quiet", "refs/heads/DWC/feature"}, exit: 1},
			},
			run: func(w DaemonWorktree) error {
				_, err := w.PlanWorktree(context.Background(), workspacecreate.Job{ID: "j", Request: workspacecreate.Request{Name: "DWC/feature", GitRoot: root, BaseCommit: "HEAD"}})
				return err
			},
		},
		{
			name: "ensure",
			calls: []gitCall{
				{args: []string{"worktree", "list", "--porcelain"}},
				{args: []string{"worktree", "add", "-b", "DWC/feature", path, "abc123"}},
			},
			run: func(w DaemonWorktree) error {
				return w.EnsureWorktree(context.Background(), workspacecreate.Job{ID: "j", Request: workspacecreate.Request{Name: "DWC/feature", GitRoot: root}, WorktreePath: path, FinalName: "DWC/feature", Branch: "DWC/feature", ResolvedBaseCommit: "abc123"})
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			git := &fakeGitRunner{calls: test.calls}
			worktree := DaemonWorktree{Git: git, Marker: &fakeProjectileMarker{}, Logf: func(string, ...any) {}}

			err := test.run(worktree)

			if err != nil {
				t.Fatal(err)
			}
			for _, args := range git.seen {
				for _, arg := range args {
					if arg == "tag" || strings.Contains(arg, "refs/tags/") {
						t.Fatalf("daemon invoked git %q, but start tags belong to Emacs", args)
					}
				}
			}
		})
	}
}

type fakeSessionCommands struct {
	calls int
	opts  server.CreateOpts
	id    string
	err   error
}

func (f *fakeSessionCommands) CreateSession(_ context.Context, opts server.CreateOpts) (string, error) {
	f.calls++
	f.opts = opts
	return f.id, f.err
}
func (*fakeSessionCommands) DeleteSession(string) error { return nil }

// The account is NOT taken from the request any more (it follows the path), so
// this pins what the request still carries: the model and the permission mode.
func TestDaemonSessionCreatorUsesExplicitModelAndPermissionMetadata(t *testing.T) {
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	commands := &fakeSessionCommands{id: "s_new"}
	// Match Server.CreateSession's durable registry effect without invoking the
	// server: the adapter's contract is to verify that durable side effect.
	commands.id = "s_new"
	creator := daemonSessionCreator{Commands: commands, Registry: reg, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{ID: "j", WorktreePath: "/worktree", Request: workspacecreate.Request{Model: "sonnet", ConfigDir: "/cfg", PermissionMode: "plan", AllowUngated: false}}
	// The command fake cannot write the registry itself, so use a wrapper that
	// observes the supplied opts and installs the expected record atomically.
	creator.Commands = sessionCommandFunc(func(_ context.Context, opts server.CreateOpts) (string, error) {
		commands.calls++
		commands.opts = opts
		if err := reg.Put(registry.Record{SessionID: "s_new", CWD: "/worktree"}); err != nil {
			return "", err
		}
		return "s_new", nil
	})
	id, err := creator.EnsureSession(context.Background(), job)
	if err != nil {
		t.Fatal(err)
	}
	if id != "s_new" || commands.calls != 1 {
		t.Fatalf("id=%s calls=%d", id, commands.calls)
	}
	// ConfigDir is empty because "/worktree" is not under $MULTI_REPO_ROOT —
	// the path answers, and the request's own "/cfg" no longer competes.
	if commands.opts.ConfigDir != "" || commands.opts.PermissionMode != "plan" || commands.opts.Model != "sonnet" || commands.opts.Resume != "" {
		t.Fatalf("opts = %#v", commands.opts)
	}
}

func TestDaemonSessionCreatorPassesResolvedForkVendorSessionAsResume(t *testing.T) {
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	var got server.CreateOpts
	creator := daemonSessionCreator{Registry: reg, Logf: func(string, ...any) {}}
	creator.Commands = sessionCommandFunc(func(_ context.Context, opts server.CreateOpts) (string, error) {
		got = opts
		if err := reg.Put(registry.Record{SessionID: "s_child", CWD: "/child"}); err != nil {
			return "", err
		}
		return "s_child", nil
	})
	if _, err := creator.EnsureSession(context.Background(), workspacecreate.Job{ID: "fork", WorktreePath: "/child", Request: workspacecreate.Request{ForkSessionID: "vendor-source"}}); err != nil {
		t.Fatal(err)
	}
	if got.Resume != "vendor-source" {
		t.Fatalf("CreateSession Resume=%q, want resolved fork vendor session", got.Resume)
	}
}

// A source workspace lends its PERMISSION MODE and nothing else. Its account
// is deliberately not consulted: the new worktree's path already answers that,
// and a parent under a different root would otherwise drag its account across.
func TestDaemonSessionCreatorInheritsPermissionModeButNotTheSourceAccount(t *testing.T) {
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "source", CWD: "/source", ConfigDir: "/cfg-source", PermissionMode: "plan"}); err != nil {
		t.Fatal(err)
	}
	creator := daemonSessionCreator{Registry: reg, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{ID: "child", WorktreePath: "/child", Request: workspacecreate.Request{SourceWorkspace: "source", SourceDir: "/source", PermissionMode: "plan"}}
	request, err := creator.ResolveSessionMetadata(context.Background(), job)
	if err != nil {
		t.Fatal(err)
	}
	if request.PermissionMode != "plan" {
		t.Fatalf("resolved request = %#v, want the source's permission mode", request)
	}
	if request.ConfigDir != "" {
		t.Fatalf("resolved request = %#v, want the path's account rather than the source's", request)
	}
	job.Request.PermissionMode = "bypassPermissions"
	if _, err := creator.ResolveSessionMetadata(context.Background(), job); err == nil {
		t.Fatal("mismatching source metadata assertion was accepted")
	}
}

type sessionCommandFunc func(context.Context, server.CreateOpts) (string, error)

func (f sessionCommandFunc) CreateSession(ctx context.Context, opts server.CreateOpts) (string, error) {
	return f(ctx, opts)
}
func (sessionCommandFunc) DeleteSession(string) error { return nil }

type fakePromptRouter struct{ calls [][]string }

func (f *fakePromptRouter) SubmitWorkspaceInitialPrompt(_ context.Context, workspace, jobID, text, permission string) error {
	f.calls = append(f.calls, []string{workspace, jobID, text, permission})
	return nil
}

func TestInitialPromptAdapterRequiresLiveRegisteredJobSession(t *testing.T) {
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/worktree"}); err != nil {
		t.Fatal(err)
	}
	router := &fakePromptRouter{}
	submitter := daemonInitialPromptSubmitter{Router: router, Registry: reg}
	job := workspacecreate.Job{ID: "job-1", SessionID: "s1", WorktreePath: "/worktree", Request: workspacecreate.Request{Prompt: "start", PermissionMode: "plan"}}
	if err := submitter.SubmitInitialPrompt(context.Background(), job); err != nil {
		t.Fatal(err)
	}
	want := []string{"/worktree", "job-1", "start", "plan"}
	if !reflect.DeepEqual(router.calls, [][]string{want}) {
		t.Fatalf("router calls = %#v", router.calls)
	}
}

func TestWorkspaceCreatePathsAndAssemblyDemandExplicitHealthAndPromptSeams(t *testing.T) {
	store, inbox, err := workspaceCreatePaths("/state")
	if err != nil || store != "/state/"+workspaceCreateStoreName || inbox != "/state/output" {
		t.Fatalf("paths store=%q inbox=%q err=%v", store, inbox, err)
	}
	if _, err := NewWorkspaceCreateAssembly(WorkspaceCreateAssemblyConfig{StateRoot: t.TempDir(), InboxInterval: time.Second}); err == nil {
		t.Fatal("assembly accepted missing durable collaborators")
	}
}

type fakeHealthSessionController struct {
	status *corev1.HealthStatus
	err    error
}

func (f fakeHealthSessionController) Health(context.Context, string, string, string) (*corev1.HealthStatus, error) {
	return f.status, f.err
}

func TestSessionControllerHealthProbeRejectsUnhealthyReply(t *testing.T) {
	probe := sessionControllerHealthProbe{Controller: fakeHealthSessionController{status: &corev1.HealthStatus{Healthy: false, Component: "shim", Reason: "store unavailable"}}, Logf: func(string, ...any) {}}
	if err := probe.CheckWorkspaceHealth(context.Background(), "/worktree", "s1", "job-1"); err == nil {
		t.Fatal("unhealthy health reply was accepted")
	}
	probe.Controller = fakeHealthSessionController{status: &corev1.HealthStatus{Healthy: true, Component: "shim"}}
	if err := probe.CheckWorkspaceHealth(context.Background(), "/worktree", "s1", "job-1"); err != nil {
		t.Fatalf("healthy health reply: %v", err)
	}
}

// A failed creation job reaches Emacs on the durable HostAction channel rather
// than dying in the daemon log.  The typed arm keeps the job id, requested
// name, and error legible instead of hiding them in an opaque legacy payload.
func TestToProtoActionMapsWorkspaceCreateFailure(t *testing.T) {
	payload, err := json.Marshal(workspacecreate.WorkspaceCreateFailure{JobID: "file:0", RequestedName: "DWC/feature", Error: "plan worktree: exit=128"})
	if err != nil {
		t.Fatal(err)
	}
	action := workspacecreate.HostAction{ID: "file:0:failed", Type: workspacecreate.HostActionTypeWorkspaceCreateFailed, Payload: payload}

	got := toProtoAction(action)

	failure := got.GetWorkspaceCreateFailed()
	if failure == nil {
		t.Fatalf("action = %#v, want the workspace_create_failed arm", got)
	}
	if got.GetActionId() != "file:0:failed" || failure.GetJobId() != "file:0" || failure.GetRequestedName() != "DWC/feature" || failure.GetError() != "plan worktree: exit=128" {
		t.Fatalf("failure arm = %#v (action id %q)", failure, got.GetActionId())
	}
}

// The tab's priority image is whatever the announcement says, so the
// announcement must carry the bare label the host looks images up by.  The
// request's priority arrives as JSON, and a quoted "\"p1\"" reaching the host
// would silently render no image at all.
func TestToProtoAvailableCarriesTheBarePriorityLabel(t *testing.T) {
	// Arrange
	var request workspacecreate.Request
	if err := json.Unmarshal([]byte(`{"name":"DWC/feature","git_root":"/repo","priority":"p1"}`), &request); err != nil {
		t.Fatalf("unmarshal request: %v", err)
	}
	job := workspacecreate.Job{ID: "file:0", Request: request}

	// Act
	got := toProtoAvailable(job)

	// Assert
	if got.GetPriority() != "p1" {
		t.Fatalf("priority = %q, want %q", got.GetPriority(), "p1")
	}
}

// A creation with no priority announces none: the host then paints no image
// rather than inventing one locally.
func TestToProtoAvailableOmitsAnAbsentPriority(t *testing.T) {
	// Arrange
	job := workspacecreate.Job{ID: "file:0", Request: workspacecreate.Request{Name: "DWC/feature", GitRoot: "/repo"}}

	// Act
	got := toProtoAvailable(job)

	// Assert
	if got.GetPriority() != "" {
		t.Fatalf("priority = %q, want empty", got.GetPriority())
	}
}

// Every sidebar gesture has its own typed HostAction arm, and Emacs accepts it
// ONLY there: HostLegacyCommand's contract names exactly eight verbs, so a
// gesture sent down the legacy arm is refused by the host and then redelivered
// forever ("unsupported HostAction legacyCommand type set-view"), leaving the
// rail reporting a failure for a click that can never land.
func TestToProtoActionMapsSidebarGesturesToTheirTypedArms(t *testing.T) {
	tests := []struct {
		name    string
		typ     string
		payload string
		assert  func(*testing.T, *frontendv1.HostAction)
	}{
		{
			name: "switch", typ: "switch", payload: `{"type":"switch","dir":"/tmp/ws"}`,
			assert: func(t *testing.T, got *frontendv1.HostAction) {
				if got.GetSwitchWorkspace().GetDir() != "/tmp/ws" {
					t.Fatalf("switch arm = %#v", got.GetAction())
				}
			},
		},
		{
			name: "fold", typ: "fold", payload: `{"type":"fold","repo_key":"doom","folded":true}`,
			assert: func(t *testing.T, got *frontendv1.HostAction) {
				fold := got.GetSetRepositoryFold()
				if fold.GetRepoKey() != "doom" || !fold.GetFolded() {
					t.Fatalf("fold arm = %#v", got.GetAction())
				}
			},
		},
		{
			name: "unfold keeps folded false", typ: "fold", payload: `{"type":"fold","repo_key":"doom","folded":false}`,
			assert: func(t *testing.T, got *frontendv1.HostAction) {
				if fold := got.GetSetRepositoryFold(); fold.GetRepoKey() != "doom" || fold.GetFolded() {
					t.Fatalf("fold arm = %#v", got.GetAction())
				}
			},
		},
		{
			name: "set-view", typ: "set-view", payload: `{"type":"set-view","view":"task"}`,
			assert: func(t *testing.T, got *frontendv1.HostAction) {
				if got.GetSetSidebarView().GetView() != "task" {
					t.Fatalf("set-view arm = %#v", got.GetAction())
				}
			},
		},
		{
			name: "task-create", typ: "task-create", payload: `{"type":"task-create"}`,
			assert: func(t *testing.T, got *frontendv1.HostAction) {
				if got.GetTaskCreate() == nil {
					t.Fatalf("task-create arm = %#v", got.GetAction())
				}
			},
		},
		{
			name: "task-toggle-done", typ: "task-toggle-done", payload: `{"type":"task-toggle-done","id":"t1"}`,
			assert: func(t *testing.T, got *frontendv1.HostAction) {
				if got.GetTaskToggleDone().GetId() != "t1" {
					t.Fatalf("task-toggle-done arm = %#v", got.GetAction())
				}
			},
		},
		{
			name: "task-open", typ: "task-open", payload: `{"type":"task-open","id":"t1"}`,
			assert: func(t *testing.T, got *frontendv1.HostAction) {
				if got.GetTaskOpen().GetId() != "t1" {
					t.Fatalf("task-open arm = %#v", got.GetAction())
				}
			},
		},
		{
			name: "task-add-workspace", typ: "task-add-workspace", payload: `{"type":"task-add-workspace","id":"t1"}`,
			assert: func(t *testing.T, got *frontendv1.HostAction) {
				if got.GetTaskAddWorkspace().GetId() != "t1" {
					t.Fatalf("task-add-workspace arm = %#v", got.GetAction())
				}
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			action := workspacecreate.HostAction{ID: "file:0", Type: tc.typ, Payload: json.RawMessage(tc.payload)}

			// Act
			got := toProtoAction(action)

			// Assert
			if got.GetActionId() != "file:0" {
				t.Fatalf("action id = %q", got.GetActionId())
			}
			if got.GetLegacyCommand() != nil {
				t.Fatalf("%s took the legacy arm, which the host refuses for it", tc.typ)
			}
			tc.assert(t, got)
		})
	}
}

// The verbs HostLegacyCommand's contract names still ride the legacy arm: they
// have no typed arm to move to, and the host dispatches them from that envelope
// alone. "merge" is deliberately NOT among them any more — it is routed
// daemon-side and never handed to the host at all.
func TestToProtoActionKeepsTheLegacyVerbsOnTheLegacyArm(t *testing.T) {
	for _, verb := range []string{"prompt", "finish", "close", "open", "clipboard", "send", "eval"} {
		t.Run(verb, func(t *testing.T) {
			// Arrange
			action := workspacecreate.HostAction{ID: "file:0", Type: verb, Payload: json.RawMessage(`{"type":"` + verb + `","workspace":"ws"}`)}

			// Act
			got := toProtoAction(action)

			// Assert
			legacy := got.GetLegacyCommand()
			if legacy == nil || legacy.GetType() != verb {
				t.Fatalf("action = %#v, want the legacy arm carrying %q", got.GetAction(), verb)
			}
		})
	}
}

// A boot-sweep verdict reaches Emacs on its OWN typed arm.  Falling through to
// HostLegacyCommand would post a type the host's eight-verb legacy contract
// does not name, which Emacs refuses and the daemon then redelivers forever.
func TestToProtoActionMapsTheBootSweepVerdict(t *testing.T) {
	// Arrange
	payload, err := json.Marshal(workspacecreate.BootSweepSessionUnwired{
		Workspace: "/ws", SessionID: "s_1",
		Verdict: "boot_sweep_no_live_shim",
		Reason:  "its agent process is gone (boot-sweep verdict boot_sweep_no_live_shim)",
	})
	if err != nil {
		t.Fatal(err)
	}
	action := workspacecreate.HostAction{
		ID:      "boot-sweep:s_1:boot_sweep_no_live_shim",
		Type:    workspacecreate.HostActionTypeBootSweepSessionUnwired,
		Payload: payload,
	}

	// Act
	got := toProtoAction(action)

	// Assert
	unwired := got.GetBootSweepSessionUnwired()
	if unwired == nil {
		t.Fatalf("action = %#v, want the boot_sweep_session_unwired arm", got)
	}
	if got.GetActionId() != "boot-sweep:s_1:boot_sweep_no_live_shim" ||
		unwired.GetWorkspace() != "/ws" || unwired.GetSessionId() != "s_1" {
		t.Fatalf("verdict arm = %#v (action id %q)", unwired, got.GetActionId())
	}
	if unwired.GetReason() != "its agent process is gone (boot-sweep verdict boot_sweep_no_live_shim)" {
		t.Fatalf("verdict reason = %q, want the sweep's sentence verbatim", unwired.GetReason())
	}
}

// ---------------------------------------------------------------------------
// A create with NO source session still has an account, and it is the one its
// worktree's path names. Inheritance was the only rule that existed here, so a
// one-shot pinned to a repo — which nominates no source workspace — fell
// through with an empty ConfigDir and ran under the CLI default. For a repo
// under $MULTI_REPO_ROOT that meant a workspace running under a different
// account than the repo it was cut from.
// ---------------------------------------------------------------------------

func TestSourcelessCreateTakesTheAccountItsWorktreePathNames(t *testing.T) {
	// Arrange
	home := t.TempDir()
	multiRoot := filepath.Join(home, "workspace", "ChessCom")
	multiConfig := filepath.Join(home, ".claude-chesscom")
	t.Setenv(session.MultiRepoRootEnv, multiRoot)
	t.Setenv(session.MultiRepoConfigDirEnv, multiConfig)
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	creator := daemonSessionCreator{Registry: reg, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{
		ID:           "oneshot",
		WorktreePath: filepath.Join(multiRoot, "explanation-engine-worktrees", "slack-thread-pr-link"),
		Request:      workspacecreate.Request{Name: "DWC/slack-thread-pr-link", GitRoot: filepath.Join(multiRoot, "explanation-engine")},
	}

	// Act
	request, err := creator.ResolveSessionMetadata(context.Background(), job)

	// Assert
	if err != nil {
		t.Fatalf("ResolveSessionMetadata: %v", err)
	}
	if request.ConfigDir != multiConfig {
		t.Fatalf("ConfigDir = %q, want the multi-repo account %q", request.ConfigDir, multiConfig)
	}
}

func TestSourcelessCreateOutsideTheMultiRepoRootKeepsTheDefaultAccount(t *testing.T) {
	// Arrange — the doom repo, whose account IS the CLI default. The empty
	// spelling is load-bearing: it is how every record spells that account.
	home := t.TempDir()
	t.Setenv(session.MultiRepoRootEnv, filepath.Join(home, "workspace", "ChessCom"))
	t.Setenv(session.MultiRepoConfigDirEnv, filepath.Join(home, ".claude-chesscom"))
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	creator := daemonSessionCreator{Registry: reg, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{
		ID:           "oneshot",
		WorktreePath: filepath.Join(home, ".config", "doom-worktrees", "some-branch"),
		Request:      workspacecreate.Request{Name: "some-branch", GitRoot: filepath.Join(home, ".config", "doom")},
	}

	// Act
	request, err := creator.ResolveSessionMetadata(context.Background(), job)

	// Assert
	if err != nil {
		t.Fatalf("ResolveSessionMetadata: %v", err)
	}
	if request.ConfigDir != "" {
		t.Fatalf("ConfigDir = %q, want the CLI default spelled empty", request.ConfigDir)
	}
}

func TestACreateCannotNameAnAccountThePathDisagreesWith(t *testing.T) {
	// Arrange — an emitter that named an account. It has no better information
	// than the path carries, and honoring it would reintroduce the second
	// determinant that put one workspace and its transcripts in two accounts.
	home := t.TempDir()
	multiRoot := filepath.Join(home, "workspace", "ChessCom")
	t.Setenv(session.MultiRepoRootEnv, multiRoot)
	t.Setenv(session.MultiRepoConfigDirEnv, filepath.Join(home, ".claude-chesscom"))
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	creator := daemonSessionCreator{Registry: reg, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{
		ID:           "named",
		WorktreePath: filepath.Join(multiRoot, "explanation-engine-worktrees", "x"),
		Request:      workspacecreate.Request{Name: "x", ConfigDir: "/named-by-the-caller"},
	}

	// Act
	request, err := creator.ResolveSessionMetadata(context.Background(), job)

	// Assert
	if err != nil {
		t.Fatalf("ResolveSessionMetadata: %v", err)
	}
	routed, err := session.AccountConfigDirFor(job.WorktreePath)
	if err != nil {
		t.Fatalf("AccountConfigDirFor: %v", err)
	}
	if request.ConfigDir != routed {
		t.Fatalf("ConfigDir = %q, want the path's account %q", request.ConfigDir, routed)
	}
}

func TestAChildInheritsItsParentsAccountSelection(t *testing.T) {
	// Arrange — a parent whose account a human switched in the webapp. That
	// SELECTION follows its children; the parent's merely-resolved account does
	// not (TestSourcelessCreateOutsideTheMultiRepoRootKeepsTheDefaultAccount
	// pins the other half).
	home := t.TempDir()
	multiRoot := filepath.Join(home, "workspace", "ChessCom")
	t.Setenv(session.MultiRepoRootEnv, multiRoot)
	t.Setenv(session.MultiRepoConfigDirEnv, filepath.Join(home, ".claude-chesscom"))
	chosen := filepath.Join(home, ".claude")
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	parent := filepath.Join(multiRoot, "explanation-engine")
	if err := reg.Put(registry.Record{
		SessionID: "s_parent", CWD: parent, ConfigDir: chosen, ConfigDirOverride: chosen,
		CreatedAt: "2026-08-09T10:00:00Z",
	}); err != nil {
		t.Fatal(err)
	}
	creator := daemonSessionCreator{Registry: reg, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{
		ID:           "child",
		WorktreePath: filepath.Join(multiRoot, "explanation-engine-worktrees", "child"),
		Request: workspacecreate.Request{
			Name: "child", SourceWorkspace: "explanation-engine", SourceDir: parent,
		},
	}

	// Act
	request, err := creator.ResolveSessionMetadata(context.Background(), job)

	// Assert — the child runs under the parent's selection, and carries it so
	// its own children inherit it too.
	if err != nil {
		t.Fatalf("ResolveSessionMetadata: %v", err)
	}
	if request.ConfigDir != chosen || request.ConfigDirOverride != chosen {
		t.Fatalf("resolved request = %#v, want the inherited selection %q", request, chosen)
	}
}

func TestSourcelessCreateFallsBackToTheGitRootBeforeItsWorktreeExists(t *testing.T) {
	// Arrange — the account must resolve even when asked before the worktree
	// path is planned, so the repo the workspace is cut from answers.
	home := t.TempDir()
	multiRoot := filepath.Join(home, "workspace", "ChessCom")
	multiConfig := filepath.Join(home, ".claude-chesscom")
	t.Setenv(session.MultiRepoRootEnv, multiRoot)
	t.Setenv(session.MultiRepoConfigDirEnv, multiConfig)
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	creator := daemonSessionCreator{Registry: reg, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{ID: "early", Request: workspacecreate.Request{Name: "x", GitRoot: filepath.Join(multiRoot, "explanation-engine")}}

	// Act
	request, err := creator.ResolveSessionMetadata(context.Background(), job)

	// Assert
	if err != nil {
		t.Fatalf("ResolveSessionMetadata: %v", err)
	}
	if request.ConfigDir != multiConfig {
		t.Fatalf("ConfigDir = %q, want the multi-repo account %q", request.ConfigDir, multiConfig)
	}
}

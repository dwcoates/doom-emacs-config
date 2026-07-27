package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"testing"
	"time"

	"claude-repld/internal/registry"
	"claude-repld/internal/server"
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
}

func (f *fakeGitRunner) RunGit(_ context.Context, _ string, args ...string) (string, int, error) {
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
		{args: []string{"worktree", "list", "--porcelain"}, out: "worktree /tmp/other\nbranch refs/heads/DWC/feature\n\n"},
		{args: []string{"show-ref", "--verify", "--quiet", "refs/heads/" + candidateBranch(job.Request.Name, job.ID, 1)}, exit: 1},
	}}
	worktree := DaemonWorktree{Git: git, Logf: func(string, ...any) {}}
	plan, err := worktree.PlanWorktree(context.Background(), job)
	if err != nil {
		t.Fatal(err)
	}
	wantBranch := candidateBranch(job.Request.Name, job.ID, 1)
	if plan.FinalName != wantBranch || plan.Branch != wantBranch || plan.BaseCommit != "HEAD" {
		t.Fatalf("plan = %#v", plan)
	}
	if filepath.Base(plan.Path) != filepath.Base(wantBranch) {
		t.Fatalf("path = %s, want tail %s", plan.Path, filepath.Base(wantBranch))
	}
}

func TestDaemonWorktreeResumesPersistedPlanWithoutSecondAdd(t *testing.T) {
	root := testGitRoot(t)
	path := filepath.Join(filepath.Dir(root), "repo-worktrees", "feature")
	job := workspacecreate.Job{ID: "j", Request: workspacecreate.Request{Name: "DWC/feature", GitRoot: root}, WorktreePath: path, FinalName: "DWC/feature", Branch: "DWC/feature", ResolvedBaseCommit: "HEAD"}
	git := &fakeGitRunner{calls: []gitCall{{args: []string{"worktree", "list", "--porcelain"}, out: "worktree " + path + "\nbranch refs/heads/DWC/feature\n\n"}}}
	if err := (DaemonWorktree{Git: git, Logf: func(string, ...any) {}}).EnsureWorktree(context.Background(), job); err != nil {
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
	if err := (DaemonWorktree{Git: git, Logf: func(string, ...any) {}}).EnsureWorktree(context.Background(), job); err != nil {
		t.Fatal(err)
	}
	if len(git.calls) != 0 {
		t.Fatalf("unexpected remaining git calls: %#v", git.calls)
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

func TestDaemonSessionCreatorUsesExplicitAccountAndPermissionMetadata(t *testing.T) {
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.json"), func(string, ...any) {})
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
	if commands.opts.ConfigDir != "/cfg" || commands.opts.PermissionMode != "plan" || commands.opts.Model != "sonnet" {
		t.Fatalf("opts = %#v", commands.opts)
	}
}

type sessionCommandFunc func(context.Context, server.CreateOpts) (string, error)

func (f sessionCommandFunc) CreateSession(ctx context.Context, opts server.CreateOpts) (string, error) {
	return f(ctx, opts)
}
func (sessionCommandFunc) DeleteSession(string) error { return nil }

type fakePromptRouter struct{ calls [][]string }

func (f *fakePromptRouter) SubmitWorkspaceInitialPrompt(_ context.Context, workspace, sessionID, jobID, text, permission string) error {
	f.calls = append(f.calls, []string{workspace, sessionID, jobID, text, permission})
	return nil
}

func TestInitialPromptAdapterRequiresLiveRegisteredJobSession(t *testing.T) {
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.json"), func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/worktree"}); err != nil {
		t.Fatal(err)
	}
	router := &fakePromptRouter{}
	submitter := daemonInitialPromptSubmitter{Router: router, Registry: reg}
	job := workspacecreate.Job{ID: "job-1", SessionID: "s1", WorktreePath: "/worktree", Request: workspacecreate.Request{Prompt: "start", PermissionMode: "plan"}}
	if err := submitter.SubmitInitialPrompt(context.Background(), job); err != nil {
		t.Fatal(err)
	}
	want := []string{"/worktree", "s1", "job-1", "start", "plan"}
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

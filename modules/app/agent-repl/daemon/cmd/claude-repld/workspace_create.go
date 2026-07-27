package main

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	workspacecreate "claude-repld/internal/workspace/create"
)

const workspaceCreateStoreName = "workspace-create-jobs.json"

// workspaceCreatePaths returns the two durable paths the creation subsystem
// owns under the shared state root.  The inbox is deliberately the established
// skill output directory; only ownership of its consumption moved to daemon.
func workspaceCreatePaths(stateRoot string) (storePath, inboxPath string, err error) {
	if stateRoot == "" {
		return "", "", fmt.Errorf("workspace create: state root is required")
	}
	return filepath.Join(stateRoot, workspaceCreateStoreName), filepath.Join(stateRoot, "output"), nil
}

// GitRunner is the only external-process boundary used by worktree creation.
// Tests inject it; production uses ExecGitRunner below.
type GitRunner interface {
	RunGit(context.Context, string, ...string) (output string, exitCode int, err error)
}

// ExecGitRunner invokes git in one nominated working directory.  It contains
// no worktree policy; planner/creator own validation and output interpretation.
type ExecGitRunner struct{}

func (ExecGitRunner) RunGit(ctx context.Context, dir string, args ...string) (string, int, error) {
	cmd := exec.CommandContext(ctx, "git", args...)
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err == nil {
		return string(out), 0, nil
	}
	if exit, ok := err.(*exec.ExitError); ok {
		return string(out), exit.ExitCode(), nil
	}
	return string(out), -1, err
}

// DaemonWorktree is the production two-phase worktree adapter.  PlanWorktree
// is read-only; Manager checkpoints its result before EnsureWorktree mutates.
type DaemonWorktree struct {
	Git  GitRunner
	Logf func(string, ...any)
}

func (w DaemonWorktree) PlanWorktree(ctx context.Context, job workspacecreate.Job) (workspacecreate.WorktreeResult, error) {
	if w.Git == nil {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: worktree planner has no git runner")
	}
	if w.Logf == nil {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: worktree planner has no logger")
	}
	if job.Request.GitRoot == "" || !filepath.IsAbs(job.Request.GitRoot) {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: job %s git_root must be absolute", job.ID)
	}
	if job.Request.BaseCommit == "" {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: job %s requires explicit base_commit", job.ID)
	}
	root := filepath.Clean(job.Request.GitRoot)
	info, err := os.Stat(root)
	if err != nil {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: stat git_root %s: %w", root, err)
	}
	if !info.IsDir() {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: git_root %s is not a directory", root)
	}
	if _, err := os.Stat(filepath.Join(root, ".git")); err != nil {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: git_root %s has no .git: %w", root, err)
	}
	listing, exit, err := w.Git.RunGit(ctx, root, "worktree", "list", "--porcelain")
	if err != nil || exit != 0 {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: list worktrees in %s exit=%d: %w (%s)", root, exit, err, strings.TrimSpace(listing))
	}
	usedPaths, usedBranches := parseWorktreeList(listing)
	for attempt := 0; attempt < 20; attempt++ {
		branch := candidateBranch(job.Request.Name, job.ID, attempt)
		path, err := candidateWorktreePath(root, branch)
		if err != nil {
			return workspacecreate.WorktreeResult{}, err
		}
		if _, used := usedPaths[path]; used {
			continue
		}
		if _, err := os.Stat(path); err == nil {
			continue
		} else if !os.IsNotExist(err) {
			return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: stat candidate %s: %w", path, err)
		}
		if _, used := usedBranches[branch]; used {
			continue
		}
		out, branchExit, branchErr := w.Git.RunGit(ctx, root, "show-ref", "--verify", "--quiet", "refs/heads/"+branch)
		if branchErr != nil {
			return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: check branch %s: %w", branch, branchErr)
		}
		if branchExit == 0 {
			continue
		}
		if branchExit != 1 {
			return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: check branch %s exit=%d: %s", branch, branchExit, strings.TrimSpace(out))
		}
		w.Logf("workspace-create: planned job=%s requested=%q final=%q branch=%q base=%q path=%s", job.ID, job.Request.Name, branch, branch, job.Request.BaseCommit, path)
		return workspacecreate.WorktreeResult{Path: path, FinalName: branch, Branch: branch, BaseCommit: job.Request.BaseCommit}, nil
	}
	return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: could not resolve a collision-free name for job %s after 20 attempts", job.ID)
}

func (w DaemonWorktree) EnsureWorktree(ctx context.Context, job workspacecreate.Job) error {
	if w.Git == nil || w.Logf == nil {
		return fmt.Errorf("workspace create: worktree creator is not fully configured")
	}
	if job.WorktreePath == "" || job.FinalName == "" || job.Branch == "" || job.ResolvedBaseCommit == "" {
		return fmt.Errorf("workspace create: job %s has no persisted worktree plan", job.ID)
	}
	root := filepath.Clean(job.Request.GitRoot)
	listing, exit, err := w.Git.RunGit(ctx, root, "worktree", "list", "--porcelain")
	if err != nil || exit != 0 {
		return fmt.Errorf("workspace create: list worktrees before create job=%s exit=%d: %w (%s)", job.ID, exit, err, strings.TrimSpace(listing))
	}
	usedPaths, usedBranches := parseWorktreeList(listing)
	path := filepath.Clean(job.WorktreePath)
	if branch, ok := usedPaths[path]; ok {
		if branch != job.Branch {
			return fmt.Errorf("workspace create: planned path %s belongs to branch %q, not job branch %q", path, branch, job.Branch)
		}
		w.Logf("workspace-create: worktree already exists for job=%s path=%s branch=%s", job.ID, path, job.Branch)
		return nil
	}
	if _, exists := usedBranches[job.Branch]; exists {
		return fmt.Errorf("workspace create: planned branch %q is already attached to another worktree", job.Branch)
	}
	if _, err := os.Stat(path); err == nil {
		return fmt.Errorf("workspace create: planned path %s exists but is not the planned git worktree", path)
	} else if !os.IsNotExist(err) {
		return fmt.Errorf("workspace create: stat planned path %s: %w", path, err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("workspace create: create worktree parent for %s: %w", path, err)
	}
	out, addExit, addErr := w.Git.RunGit(ctx, root, "worktree", "add", "-b", job.Branch, path, job.ResolvedBaseCommit)
	if addErr != nil || addExit != 0 {
		return fmt.Errorf("workspace create: git worktree add job=%s branch=%s exit=%d: %w (%s)", job.ID, job.Branch, addExit, addErr, strings.TrimSpace(out))
	}
	w.Logf("workspace-create: created worktree job=%s path=%s branch=%s base=%s", job.ID, path, job.Branch, job.ResolvedBaseCommit)
	return nil
}

func candidateBranch(requested, jobID string, attempt int) string {
	if attempt == 0 {
		return requested
	}
	digest := sha256.Sum256([]byte(jobID))
	suffix := hex.EncodeToString(digest[:])[:8]
	if attempt == 1 {
		return requested + "-" + suffix
	}
	return fmt.Sprintf("%s-%s-%d", requested, suffix, attempt)
}

func candidateWorktreePath(gitRoot, branch string) (string, error) {
	bare := filepath.Base(branch)
	if bare == "." || bare == string(filepath.Separator) || bare == "" {
		return "", fmt.Errorf("workspace create: branch %q has no safe worktree directory name", branch)
	}
	gitDir := filepath.Join(gitRoot, ".git")
	gitInfo, err := os.Stat(gitDir)
	if err != nil {
		return "", fmt.Errorf("workspace create: stat %s: %w", gitDir, err)
	}
	parent := filepath.Dir(gitRoot)
	if !gitInfo.Mode().IsRegular() {
		parent = filepath.Join(parent, filepath.Base(gitRoot)+"-worktrees")
	}
	return filepath.Join(parent, bare), nil
}

func parseWorktreeList(output string) (map[string]string, map[string]struct{}) {
	paths := map[string]string{}
	branches := map[string]struct{}{}
	var currentPath string
	for _, line := range strings.Split(output, "\n") {
		if rest, ok := strings.CutPrefix(line, "worktree "); ok {
			currentPath = filepath.Clean(rest)
			continue
		}
		if rest, ok := strings.CutPrefix(line, "branch refs/heads/"); ok {
			branches[rest] = struct{}{}
			if currentPath != "" {
				paths[currentPath] = rest
			}
		}
	}
	return paths, branches
}

// daemonSessionCreator uses the existing daemon create-session core.  Registry
// lookup makes the session stage restart-idempotent after CreateSession has
// persisted its record but before the creation job could checkpoint SessionID.
type daemonSessionCreator struct {
	Commands server.SessionCreateDeleter
	Registry *registry.Registry
	Logf     func(string, ...any)
}

func (c daemonSessionCreator) EnsureSession(ctx context.Context, job workspacecreate.Job) (string, error) {
	if c.Commands == nil || c.Registry == nil || c.Logf == nil {
		return "", fmt.Errorf("workspace create: session creator is not fully configured")
	}
	if job.WorktreePath == "" {
		return "", fmt.Errorf("workspace create: job %s has no worktree path for session creation", job.ID)
	}
	if existing, ok := (&server.SessionLocator{Reg: c.Registry}).Locate(job.WorktreePath); ok {
		c.Logf("workspace-create: reusing registered session job=%s session=%s cwd=%s", job.ID, existing, job.WorktreePath)
		return existing, nil
	}
	id, err := c.Commands.CreateSession(ctx, server.CreateOpts{
		CWD:            job.WorktreePath,
		Model:          job.Request.Model,
		ConfigDir:      job.Request.ConfigDir,
		PermissionMode: job.Request.PermissionMode,
		AllowUngated:   job.Request.AllowUngated,
	})
	if err != nil {
		return "", fmt.Errorf("workspace create: create session for job %s: %w", job.ID, err)
	}
	rec, ok := c.Registry.Get(id)
	if !ok || rec.CWD != job.WorktreePath || rec.Terminal {
		return "", fmt.Errorf("workspace create: session %s was not durably registered for job %s cwd=%s", id, job.ID, job.WorktreePath)
	}
	c.Logf("workspace-create: registered session job=%s session=%s cwd=%s model=%q config_dir=%q permission_mode=%q", job.ID, id, job.WorktreePath, job.Request.Model, job.Request.ConfigDir, job.Request.PermissionMode)
	return id, nil
}

// WorkspaceHealthProbe is deliberately stronger than a shim handshake.  Main
// binds it to sessiondrv.Manager.Health once that health path is assembled;
// this adapter refuses construction without the probe rather than weakening
// the workspace-ready invariant to Ensure/AwaitReady.
type WorkspaceHealthProbe interface {
	CheckWorkspaceHealth(context.Context, string, string, string) error
}

type daemonSessionHealth struct{ Probe WorkspaceHealthProbe }

func (h daemonSessionHealth) AwaitHealthy(ctx context.Context, job workspacecreate.Job) error {
	if h.Probe == nil {
		return fmt.Errorf("workspace create: session health probe is not configured")
	}
	return h.Probe.CheckWorkspaceHealth(ctx, job.WorktreePath, job.SessionID, job.ID)
}

// IdempotentInitialPromptRouter is the required session/outbox seam.  Its
// implementation must use JobID to make an acknowledged shim submit durable
// across the manager's PromptSubmitting checkpoint boundary.
type IdempotentInitialPromptRouter interface {
	SubmitWorkspaceInitialPrompt(context.Context, string, string, string, string, string) error
}

type daemonInitialPromptSubmitter struct {
	Router   IdempotentInitialPromptRouter
	Registry *registry.Registry
}

func (s daemonInitialPromptSubmitter) SubmitInitialPrompt(ctx context.Context, job workspacecreate.Job) error {
	if s.Router == nil || s.Registry == nil {
		return fmt.Errorf("workspace create: idempotent initial-prompt router is not configured")
	}
	rec, ok := s.Registry.Get(job.SessionID)
	if !ok || rec.Terminal || rec.CWD != job.WorktreePath {
		return fmt.Errorf("workspace create: job %s session %s is not the live registered session for %s", job.ID, job.SessionID, job.WorktreePath)
	}
	return s.Router.SubmitWorkspaceInitialPrompt(ctx, job.WorktreePath, job.SessionID, job.ID, job.Request.Prompt, job.Request.PermissionMode)
}

// WorkspaceCreateHostForwarder lets startup build the durable manager before
// the wire bridge exists.  The bridge binds targets once it is constructed;
// an unbound delivery fails loudly and leaves the job/action durable for retry.
type WorkspaceCreateHostForwarder struct {
	mu        sync.RWMutex
	available workspacecreate.WorkspaceAvailablePublisher
	actions   workspacecreate.HostActionSink
}

func (f *WorkspaceCreateHostForwarder) SetTargets(available workspacecreate.WorkspaceAvailablePublisher, actions workspacecreate.HostActionSink) error {
	if available == nil || actions == nil {
		return fmt.Errorf("workspace create: host forwarder requires available and action targets")
	}
	f.mu.Lock()
	f.available, f.actions = available, actions
	f.mu.Unlock()
	return nil
}

func (f *WorkspaceCreateHostForwarder) PublishWorkspaceAvailable(ctx context.Context, available workspacecreate.Available) error {
	f.mu.RLock()
	target := f.available
	f.mu.RUnlock()
	if target == nil {
		return fmt.Errorf("workspace create: available publisher is not bound")
	}
	return target.PublishWorkspaceAvailable(ctx, available)
}

func (f *WorkspaceCreateHostForwarder) PublishHostAction(ctx context.Context, action workspacecreate.HostAction) error {
	f.mu.RLock()
	target := f.actions
	f.mu.RUnlock()
	if target == nil {
		return fmt.Errorf("workspace create: host-action publisher is not bound")
	}
	return target.PublishHostAction(ctx, action)
}

// WorkspaceCreateAssembly is the main-independent startup result.  Main opens
// it with the daemon lifetime context, binds the wire bridge to Forwarder, and
// starts Inbox.Run only after both targets are set.
type WorkspaceCreateAssembly struct {
	Store     *workspacecreate.FileJobStore
	Manager   *workspacecreate.Manager
	Inbox     *workspacecreate.Inbox
	Forwarder *WorkspaceCreateHostForwarder
}

type WorkspaceCreateAssemblyConfig struct {
	StateRoot      string
	Commands       server.SessionCreateDeleter
	Registry       *registry.Registry
	Health         WorkspaceHealthProbe
	InitialPrompts IdempotentInitialPromptRouter
	Logf           func(string, ...any)
	InboxInterval  time.Duration
}

func NewWorkspaceCreateAssembly(cfg WorkspaceCreateAssemblyConfig) (*WorkspaceCreateAssembly, error) {
	if cfg.Commands == nil || cfg.Registry == nil || cfg.Health == nil || cfg.InitialPrompts == nil || cfg.Logf == nil {
		return nil, fmt.Errorf("workspace create: startup requires commands, registry, health probe, initial-prompt router, and logger")
	}
	storePath, inboxPath, err := workspaceCreatePaths(cfg.StateRoot)
	if err != nil {
		return nil, err
	}
	store, err := workspacecreate.OpenJobStore(storePath, cfg.Logf)
	if err != nil {
		return nil, err
	}
	forwarder := &WorkspaceCreateHostForwarder{}
	worktrees := DaemonWorktree{Git: ExecGitRunner{}, Logf: cfg.Logf}
	manager, err := workspacecreate.NewManager(workspacecreate.Config{
		Store: store, Planner: worktrees, Worktrees: worktrees,
		Sessions:  daemonSessionCreator{Commands: cfg.Commands, Registry: cfg.Registry, Logf: cfg.Logf},
		Health:    daemonSessionHealth{Probe: cfg.Health},
		Prompts:   daemonInitialPromptSubmitter{Router: cfg.InitialPrompts, Registry: cfg.Registry},
		Available: forwarder, HostActions: forwarder, Logf: cfg.Logf,
	})
	if err != nil {
		return nil, err
	}
	interval := cfg.InboxInterval
	if interval <= 0 {
		return nil, fmt.Errorf("workspace create: inbox interval must be positive")
	}
	return &WorkspaceCreateAssembly{Store: store, Manager: manager, Inbox: &workspacecreate.Inbox{Dir: inboxPath, Store: store, Manager: manager, Logf: cfg.Logf, Interval: interval}, Forwarder: forwarder}, nil
}

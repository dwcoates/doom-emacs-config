package main

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/gitexec"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	workspacecreate "claude-repld/internal/workspace/create"

	"google.golang.org/protobuf/types/known/structpb"
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
	// gitexec.Command strips inherited repository bindings (GIT_DIR and
	// friends) so the nominated dir is the only repository selector — a daemon
	// launched from a Git hook must never create worktrees in the hook's repo.
	cmd := gitexec.Command(ctx, dir, args...)
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
	Git      GitRunner
	Registry *registry.Registry
	Marker   ProjectileMarker
	Logf     func(string, ...any)
}

// ProjectileMarker is the daemon-owned project setup boundary.  Emacs only
// renders a workspace after this marker exists, so it cannot retain ownership
// of project initialization.
type ProjectileMarker interface {
	Ensure(worktreePath, bareName string) error
}

type osProjectileMarker struct{}

func (osProjectileMarker) Ensure(worktreePath, bareName string) error {
	path := filepath.Join(worktreePath, ".projectile")
	contents := []byte(bareName + "\n")
	existing, err := os.ReadFile(path)
	if err == nil {
		if string(existing) != string(contents) {
			return fmt.Errorf("workspace create: projectile marker %s has %q, want %q", path, strings.TrimSpace(string(existing)), bareName)
		}
		return nil
	}
	if !os.IsNotExist(err) {
		return fmt.Errorf("workspace create: read projectile marker %s: %w", path, err)
	}
	if err := os.WriteFile(path, contents, 0o644); err != nil {
		return fmt.Errorf("workspace create: write projectile marker %s: %w", path, err)
	}
	return nil
}

func (w DaemonWorktree) PlanWorktree(ctx context.Context, job workspacecreate.Job) (workspacecreate.WorktreeResult, error) {
	if w.Git == nil {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: worktree planner has no git runner")
	}
	if w.Logf == nil {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: worktree planner has no logger")
	}
	root, err := normalizeWorkspacePath(job.Request.GitRoot)
	if err != nil {
		return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: job %s git_root: %w", job.ID, err)
	}
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
	baseCommit, forkSessionID, err := w.resolveBase(ctx, root, job)
	if err != nil {
		return workspacecreate.WorktreeResult{}, err
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
		finalName := filepath.Base(branch)
		w.Logf("workspace-create: planned job=%s requested=%q final=%q branch=%q base=%q path=%s fork_session=%q", job.ID, job.Request.Name, finalName, branch, baseCommit, path, forkSessionID)
		return workspacecreate.WorktreeResult{Path: path, FinalName: finalName, Branch: branch, BaseCommit: baseCommit, ForkSessionID: forkSessionID}, nil
	}
	return workspacecreate.WorktreeResult{}, fmt.Errorf("workspace create: could not resolve a collision-free name for job %s after 20 attempts", job.ID)
}

func (w DaemonWorktree) resolveBase(ctx context.Context, root string, job workspacecreate.Job) (string, string, error) {
	base := job.Request.BaseCommit
	forkSessionID := ""
	resolveRoot := root
	if job.Request.ForkFrom != "" {
		source, err := w.resolveForkSession(job)
		if err != nil {
			return "", "", err
		}
		forkSessionID = source.ClaudeSessionID
		resolveRoot, err = normalizeWorkspacePath(source.CWD)
		if err != nil {
			return "", "", fmt.Errorf("workspace create: job %s fork source %q cwd: %w", job.ID, job.Request.ForkFrom, err)
		}
		base = "HEAD"
		w.Logf("workspace-create: resolved fork job=%s fork_from=%q vendor_session=%q source_root=%s base=HEAD", job.ID, job.Request.ForkFrom, forkSessionID, resolveRoot)
	}
	if base == "" {
		return "", "", fmt.Errorf("workspace create: job %s requires base_commit unless fork_from is set", job.ID)
	}
	if base == "master" {
		if err := w.fastForwardMaster(ctx, root); err != nil {
			return "", "", err
		}
	} else if strings.HasPrefix(base, "origin/") {
		ref := strings.TrimPrefix(base, "origin/")
		if ref == "" {
			return "", "", fmt.Errorf("workspace create: job %s has invalid origin base %q", job.ID, base)
		}
		if err := w.runGitOK(ctx, root, "fetch", "origin", ref); err != nil {
			return "", "", fmt.Errorf("workspace create: fetch %s: %w", base, err)
		}
	}
	commit, err := w.resolveCommit(ctx, resolveRoot, base)
	if err != nil {
		return "", "", err
	}
	return commit, forkSessionID, nil
}

func (w DaemonWorktree) resolveForkSession(job workspacecreate.Job) (registry.Record, error) {
	if w.Registry == nil {
		return registry.Record{}, fmt.Errorf("workspace create: fork resolution needs a registry")
	}
	want := filepath.Base(job.Request.ForkFrom)
	if want == "" || want == "." {
		return registry.Record{}, fmt.Errorf("workspace create: job %s has invalid fork_from %q", job.ID, job.Request.ForkFrom)
	}
	var matches []registry.Record
	for _, rec := range w.Registry.All() {
		if !rec.Terminal && filepath.Base(rec.CWD) == want {
			matches = append(matches, rec)
		}
	}
	if len(matches) != 1 {
		return registry.Record{}, fmt.Errorf("workspace create: job %s fork_from %q requires exactly one live source workspace, found %d", job.ID, job.Request.ForkFrom, len(matches))
	}
	rec := matches[0]
	if rec.ClaudeSessionID == "" {
		return registry.Record{}, fmt.Errorf("workspace create: job %s fork_from %q source session %s has no live vendor session id", job.ID, job.Request.ForkFrom, rec.SessionID)
	}
	if job.Request.ForkSessionID != "" && job.Request.ForkSessionID != rec.ClaudeSessionID {
		return registry.Record{}, fmt.Errorf("workspace create: job %s fork session assertion %q disagrees with source session %s vendor id %q", job.ID, job.Request.ForkSessionID, rec.SessionID, rec.ClaudeSessionID)
	}
	return rec, nil
}

func (w DaemonWorktree) fastForwardMaster(ctx context.Context, root string) error {
	if err := w.runGitOK(ctx, root, "fetch", "origin", "master"); err != nil {
		return fmt.Errorf("workspace create: fetch origin master: %w", err)
	}
	out, exit, err := w.Git.RunGit(ctx, root, "merge-base", "--is-ancestor", "master", "origin/master")
	if err != nil || (exit != 0 && exit != 1) {
		return fmt.Errorf("workspace create: local master cannot fast-forward to origin/master exit=%d: %w (%s)", exit, err, strings.TrimSpace(out))
	}
	if exit == 1 {
		w.Logf("workspace-create: keeping divergent local master root=%s; base retains local-only commits", root)
		return nil
	}
	listing, exit, err := w.Git.RunGit(ctx, root, "worktree", "list", "--porcelain")
	if err != nil || exit != 0 {
		return fmt.Errorf("workspace create: list worktrees for master fast-forward exit=%d: %w (%s)", exit, err, strings.TrimSpace(listing))
	}
	masterPath := worktreePathForBranch(listing, "master")
	if masterPath == "" {
		if err := w.runGitOK(ctx, root, "update-ref", "refs/heads/master", "origin/master", "master"); err != nil {
			return fmt.Errorf("workspace create: fast-forward unattached local master: %w", err)
		}
		w.Logf("workspace-create: fast-forwarded unattached master root=%s", root)
		return nil
	}
	if err := w.runGitOK(ctx, masterPath, "merge", "--ff-only", "origin/master"); err != nil {
		return fmt.Errorf("workspace create: fast-forward local master at %s: %w", masterPath, err)
	}
	w.Logf("workspace-create: fast-forwarded master root=%s master_worktree=%s", root, masterPath)
	return nil
}

func (w DaemonWorktree) resolveCommit(ctx context.Context, root, ref string) (string, error) {
	out, exit, err := w.Git.RunGit(ctx, root, "rev-parse", "--verify", ref+"^{commit}")
	if err != nil || exit != 0 || strings.TrimSpace(out) == "" {
		return "", fmt.Errorf("workspace create: resolve base %q exit=%d: %w (%s)", ref, exit, err, strings.TrimSpace(out))
	}
	return strings.TrimSpace(out), nil
}

func (w DaemonWorktree) runGitOK(ctx context.Context, dir string, args ...string) error {
	out, exit, err := w.Git.RunGit(ctx, dir, args...)
	if err != nil || exit != 0 {
		return fmt.Errorf("git %s in %s exit=%d: %w (%s)", strings.Join(args, " "), dir, exit, err, strings.TrimSpace(out))
	}
	return nil
}

// normalizeWorkspacePath accepts the skill contract's leading ~/ spelling but
// rejects every other relative path before any git operation can run.
func normalizeWorkspacePath(path string) (string, error) {
	if strings.HasPrefix(path, "~/") {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", fmt.Errorf("resolve home directory: %w", err)
		}
		path = filepath.Join(home, strings.TrimPrefix(path, "~/"))
	}
	if path == "" || !filepath.IsAbs(path) {
		return "", fmt.Errorf("must be an absolute path or begin with ~/")
	}
	return filepath.Clean(path), nil
}

func (w DaemonWorktree) EnsureWorktree(ctx context.Context, job workspacecreate.Job) error {
	if w.Git == nil || w.Marker == nil || w.Logf == nil {
		return fmt.Errorf("workspace create: worktree creator is not fully configured")
	}
	if job.WorktreePath == "" || job.FinalName == "" || job.Branch == "" || job.ResolvedBaseCommit == "" {
		return fmt.Errorf("workspace create: job %s has no persisted worktree plan", job.ID)
	}
	root, err := normalizeWorkspacePath(job.Request.GitRoot)
	if err != nil {
		return fmt.Errorf("workspace create: job %s git_root: %w", job.ID, err)
	}
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
		if err := w.Marker.Ensure(path, job.FinalName); err != nil {
			return err
		}
		w.Logf("workspace-create: worktree already exists for job=%s path=%s branch=%s projectile=%q", job.ID, path, job.Branch, job.FinalName)
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
	if err := w.Marker.Ensure(path, job.FinalName); err != nil {
		return err
	}
	w.Logf("workspace-create: created worktree job=%s path=%s branch=%s base=%s projectile=%q", job.ID, path, job.Branch, job.ResolvedBaseCommit, job.FinalName)
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

func worktreePathForBranch(output, branch string) string {
	paths, _ := parseWorktreeList(output)
	for path, candidate := range paths {
		if candidate == branch {
			return path
		}
	}
	return ""
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
	request, err := c.ResolveSessionMetadata(ctx, job)
	if err != nil {
		return "", err
	}
	if existing, ok := (&server.SessionLocator{Reg: c.Registry}).Locate(job.WorktreePath); ok {
		c.Logf("workspace-create: reusing registered session job=%s session=%s cwd=%s", job.ID, existing, job.WorktreePath)
		return existing, nil
	}
	id, err := c.Commands.CreateSession(ctx, server.CreateOpts{
		CWD:            job.WorktreePath,
		Model:          request.Model,
		ConfigDir:      request.ConfigDir,
		PermissionMode: request.PermissionMode,
		Resume:         request.ForkSessionID,
		AllowUngated:   request.AllowUngated,
	})
	if err != nil {
		return "", fmt.Errorf("workspace create: create session for job %s: %w", job.ID, err)
	}
	rec, ok := c.Registry.Get(id)
	if !ok || rec.CWD != job.WorktreePath || rec.Terminal {
		return "", fmt.Errorf("workspace create: session %s was not durably registered for job %s cwd=%s", id, job.ID, job.WorktreePath)
	}
	c.Logf("workspace-create: registered session job=%s session=%s cwd=%s model=%q config_dir=%q permission_mode=%q", job.ID, id, job.WorktreePath, request.Model, request.ConfigDir, request.PermissionMode)
	return id, nil
}

// ResolveSessionMetadata resolves and returns the durable request metadata
// before CreateSession.  An empty source ConfigDir is intentionally retained:
// it means the source uses the CLI default, not an unspecified value.
func (c daemonSessionCreator) ResolveSessionMetadata(_ context.Context, job workspacecreate.Job) (workspacecreate.Request, error) {
	if c.Registry == nil || c.Logf == nil {
		return workspacecreate.Request{}, fmt.Errorf("workspace create: session metadata resolver is not fully configured")
	}
	request := job.Request
	if request.SourceWorkspace == "" && request.SourceDir == "" {
		return request, nil
	}
	if request.SourceWorkspace == "" || request.SourceDir == "" {
		return workspacecreate.Request{}, fmt.Errorf("workspace create: job %s source workspace requires both name and directory", job.ID)
	}
	sourceID, ok := (&server.SessionLocator{Reg: c.Registry}).Locate(request.SourceDir)
	if !ok {
		return workspacecreate.Request{}, fmt.Errorf("workspace create: job %s source workspace %q has no live registered session at %s", job.ID, request.SourceWorkspace, request.SourceDir)
	}
	source, ok := c.Registry.Get(sourceID)
	if !ok || source.Terminal || source.CWD != request.SourceDir {
		return workspacecreate.Request{}, fmt.Errorf("workspace create: job %s source workspace %q session %s is not live at %s", job.ID, request.SourceWorkspace, sourceID, request.SourceDir)
	}
	if (request.ConfigDir != "" && request.ConfigDir != source.ConfigDir) || (request.PermissionMode != "" && request.PermissionMode != source.PermissionMode) {
		return workspacecreate.Request{}, fmt.Errorf("workspace create: job %s source workspace metadata conflicts with live source session %s", job.ID, sourceID)
	}
	request.ConfigDir, request.PermissionMode = source.ConfigDir, source.PermissionMode
	c.Logf("workspace-create: inherited job=%s source_workspace=%q source_session=%s config_dir=%q permission_mode=%q", job.ID, request.SourceWorkspace, sourceID, request.ConfigDir, request.PermissionMode)
	return request, nil
}

// WorkspaceHealthProbe is deliberately stronger than a shim handshake.  Main
// binds it to sessioncontroller.Manager.Health once that health path is assembled;
// this adapter refuses construction without the probe rather than weakening
// the workspace-ready invariant to Ensure/AwaitReady.
type WorkspaceHealthProbe interface {
	CheckWorkspaceHealth(context.Context, string, string, string) error
}

// sessionControllerHealthProbe turns the session controller's correlated shim health RPC into
// the create manager's hard readiness gate.  It deliberately rejects a nil or
// unhealthy reply; a shim connection alone is not evidence that the complete
// daemon-to-shim path is usable.
type sessionControllerHealthProbe struct {
	Controller interface {
		Health(context.Context, string, string, string) (*corev1.HealthStatus, error)
	}
	Logf func(string, ...any)
}

func (h sessionControllerHealthProbe) CheckWorkspaceHealth(ctx context.Context, workspace, sessionID, requestID string) error {
	if h.Controller == nil || h.Logf == nil {
		return fmt.Errorf("workspace create: session health probe is not fully configured")
	}
	status, err := h.Controller.Health(ctx, workspace, sessionID, requestID)
	if err != nil {
		return fmt.Errorf("workspace create: health rpc workspace=%s session=%s job=%s: %w", workspace, sessionID, requestID, err)
	}
	if status == nil {
		return fmt.Errorf("workspace create: health rpc workspace=%s session=%s job=%s returned no status", workspace, sessionID, requestID)
	}
	if !status.GetHealthy() {
		return fmt.Errorf("workspace create: health rpc workspace=%s session=%s job=%s component=%q unhealthy: %s", workspace, sessionID, requestID, status.GetComponent(), status.GetReason())
	}
	h.Logf("workspace-create: health confirmed workspace=%s session=%s job=%s component=%q", workspace, sessionID, requestID, status.GetComponent())
	return nil
}

type daemonSessionHealth struct{ Probe WorkspaceHealthProbe }

func (h daemonSessionHealth) AwaitHealthy(ctx context.Context, job workspacecreate.Job) error {
	if h.Probe == nil {
		return fmt.Errorf("workspace create: session health probe is not configured")
	}
	return h.Probe.CheckWorkspaceHealth(ctx, job.WorktreePath, job.SessionID, job.ID)
}

// InitialPromptRouter is the required session-controller seam.  The JobID is
// carried as a vendor-visible origin, but delivery is intentionally
// at-least-once: the job is checkpointed only after this call succeeds, so the
// narrow process-death window after a shim acknowledgement can repeat a
// prompt but can never silently lose one.
type InitialPromptRouter interface {
	SubmitWorkspaceInitialPrompt(context.Context, string, string, string, string) error
}

type daemonInitialPromptSubmitter struct {
	Router   InitialPromptRouter
	Registry *registry.Registry
}

func (s daemonInitialPromptSubmitter) SubmitInitialPrompt(ctx context.Context, job workspacecreate.Job) error {
	if s.Router == nil || s.Registry == nil {
		return fmt.Errorf("workspace create: initial-prompt router is not configured")
	}
	rec, ok := s.Registry.Get(job.SessionID)
	if !ok || rec.Terminal || rec.CWD != job.WorktreePath {
		return fmt.Errorf("workspace create: job %s session %s is not the live registered session for %s", job.ID, job.SessionID, job.WorktreePath)
	}
	return s.Router.SubmitWorkspaceInitialPrompt(ctx, job.WorktreePath, job.ID, job.Request.Prompt, job.Request.PermissionMode)
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
	Store   *workspacecreate.FileJobStore
	Manager *workspacecreate.Manager
	Inbox   *workspacecreate.Inbox
	// Merges is the inbox's late-bound merge route. main sets its target once
	// WireAgentShim has built the merge surface, before the inbox is started.
	Merges    *server.MergeDispatchBinding
	Forwarder *WorkspaceCreateHostForwarder
}

type WorkspaceCreateAssemblyConfig struct {
	StateRoot string
	Commands  server.SessionCreateDeleter
	Registry  *registry.Registry
	// Geometry durably records each created workspace's merge geometry.
	// Required: a workspace materialized without it can never be merged.
	Geometry       workspacecreate.WorkspaceGeometryRecorder
	Health         WorkspaceHealthProbe
	InitialPrompts InitialPromptRouter
	Logf           func(string, ...any)
	InboxInterval  time.Duration
}

// WorkspaceCreationBridge is the concrete server-local bridge over the
// durable create store.  It is deliberately in cmd: server stays transport
// only while this adapter maps the wire messages to daemon lifecycle facts.
type WorkspaceCreationBridge struct {
	manager *workspacecreate.Manager
	store   workspacecreate.JobStore
	ctx     context.Context
	mu      sync.Mutex
	avail   map[chan *frontendv1.WorkspaceAvailable]struct{}
	actions map[chan *frontendv1.HostAction]struct{}
}

func NewWorkspaceCreationBridge(ctx context.Context, manager *workspacecreate.Manager, store workspacecreate.JobStore) (*WorkspaceCreationBridge, error) {
	if ctx == nil || manager == nil || store == nil {
		return nil, fmt.Errorf("workspace create: bridge needs context, manager, and store")
	}
	return &WorkspaceCreationBridge{manager: manager, store: store, ctx: ctx, avail: map[chan *frontendv1.WorkspaceAvailable]struct{}{}, actions: map[chan *frontendv1.HostAction]struct{}{}}, nil
}

func (b *WorkspaceCreationBridge) MarkWorkspaceMaterialized(ctx context.Context, jobID string) error {
	return b.manager.MarkMaterialized(ctx, jobID)
}
func (b *WorkspaceCreationBridge) CompleteHostAction(_ context.Context, actionID string, ok bool, failure string) error {
	return b.manager.CompleteHostAction(actionID, ok, failure)
}

// PostprocessingPrompt satisfies postmerge.PostprocessingSource: it reports the
// postprocessing prompt the workspace at worktreePath was created with.
//
// It hangs off THIS bridge because the bridge already owns the durable job
// store, and the post-merge handoff must read the very records the create
// commands wrote. Giving the handoff its own store binding would let it answer
// from a file no creation ever wrote to.
func (b *WorkspaceCreationBridge) PostprocessingPrompt(worktreePath string) (string, error) {
	return workspacecreate.PostprocessingPromptFor(b.store, worktreePath)
}

func (b *WorkspaceCreationBridge) SnapshotHostWork() server.WorkspaceHostWorkSnapshot {
	jobs, err := b.store.List()
	if err != nil {
		panic(fmt.Sprintf("workspace create: snapshot jobs: %v", err))
	}
	result := server.WorkspaceHostWorkSnapshot{}
	for _, job := range jobs {
		if job.State == workspacecreate.StateAwaitingEmacs {
			result.WorkspaceAvailable = append(result.WorkspaceAvailable, toProtoAvailable(job))
		}
	}
	actions, err := b.store.PendingHostActions()
	if err != nil {
		panic(fmt.Sprintf("workspace create: snapshot actions: %v", err))
	}
	for _, action := range actions {
		result.HostActions = append(result.HostActions, toProtoAction(action))
	}
	return result
}

func (b *WorkspaceCreationBridge) SubscribeWorkspaceAvailable() (<-chan *frontendv1.WorkspaceAvailable, func()) {
	ch := make(chan *frontendv1.WorkspaceAvailable, 32)
	b.mu.Lock()
	b.avail[ch] = struct{}{}
	b.mu.Unlock()
	return ch, func() { b.mu.Lock(); delete(b.avail, ch); close(ch); b.mu.Unlock() }
}
func (b *WorkspaceCreationBridge) SubscribeHostActions() (<-chan *frontendv1.HostAction, func()) {
	ch := make(chan *frontendv1.HostAction, 32)
	b.mu.Lock()
	b.actions[ch] = struct{}{}
	b.mu.Unlock()
	return ch, func() { b.mu.Lock(); delete(b.actions, ch); close(ch); b.mu.Unlock() }
}
func (b *WorkspaceCreationBridge) PublishWorkspaceAvailable(_ context.Context, a workspacecreate.Available) error {
	value := toProtoAvailable(workspacecreate.Job{ID: a.JobID, FinalName: a.Name, Branch: a.Branch, ResolvedBaseCommit: a.BaseCommit, WorktreePath: a.WorktreePath, SessionID: a.SessionID, Request: a.Request})
	b.mu.Lock()
	defer b.mu.Unlock()
	for ch := range b.avail {
		ch <- value
	}
	return nil
}
func (b *WorkspaceCreationBridge) PublishHostAction(_ context.Context, action workspacecreate.HostAction) error {
	value := toProtoAction(action)
	b.mu.Lock()
	defer b.mu.Unlock()
	for ch := range b.actions {
		ch <- value
	}
	return nil
}
func toProtoAvailable(j workspacecreate.Job) *frontendv1.WorkspaceAvailable {
	return &frontendv1.WorkspaceAvailable{JobId: j.ID, FinalName: j.FinalName, WorktreePath: j.WorktreePath, Branch: j.Branch, GitRoot: j.Request.GitRoot, BaseCommit: j.ResolvedBaseCommit, SourceWorkspace: j.Request.SourceWorkspace, SourceDir: j.Request.SourceDir, ForkFrom: j.Request.ForkFrom, ForkSessionId: j.Request.ForkSessionID, SessionId: j.SessionID, Priority: string(j.Request.Priority), Model: j.Request.Model, InitialPromptQueued: j.Request.Prompt != "", ConfigDir: j.Request.ConfigDir, PermissionMode: j.Request.PermissionMode, AllowUngated: j.Request.AllowUngated}
}
func toProtoAction(a workspacecreate.HostAction) *frontendv1.HostAction {
	if a.Type == workspacecreate.HostActionTypeWorkspaceCreateFailed {
		var failure workspacecreate.WorkspaceCreateFailure
		if err := json.Unmarshal(a.Payload, &failure); err != nil {
			panic(fmt.Sprintf("workspace create: failure action %s payload: %v", a.ID, err))
		}
		return &frontendv1.HostAction{ActionId: a.ID, Action: &frontendv1.HostAction_WorkspaceCreateFailed{WorkspaceCreateFailed: &frontendv1.HostWorkspaceCreateFailed{JobId: failure.JobID, RequestedName: failure.RequestedName, Error: failure.Error}}}
	}
	// The sidebar's own gestures each have a typed arm, and the host accepts
	// them ONLY there: HostLegacyCommand's contract names exactly eight verbs,
	// so posting a sidebar gesture down the legacy arm is refused by Emacs and
	// then redelivered forever. Route each gesture to the arm it belongs in.
	if action, ok := toProtoSidebarAction(a); ok {
		return action
	}
	var raw map[string]any
	if err := json.Unmarshal(a.Payload, &raw); err != nil {
		panic(fmt.Sprintf("workspace create: action %s payload: %v", a.ID, err))
	}
	payload, err := structpb.NewStruct(raw)
	if err != nil {
		panic(fmt.Sprintf("workspace create: action %s payload struct: %v", a.ID, err))
	}
	return &frontendv1.HostAction{ActionId: a.ID, Action: &frontendv1.HostAction_LegacyCommand{LegacyCommand: &frontendv1.HostLegacyCommand{Type: a.Type, Payload: payload}}}
}

// toProtoSidebarAction maps a sidebar gesture onto its typed HostAction arm,
// reporting false for every other command type so the caller falls through to
// the legacy arm. The payload is the command entry as the webapp posted it, so
// a field the entry never carried is a contract breach the daemon must not
// paper over — POST /workspace-command already validated every one of them.
func toProtoSidebarAction(a workspacecreate.HostAction) (*frontendv1.HostAction, bool) {
	switch a.Type {
	case "switch", "fold", "set-view", "task-create", "task-toggle-done", "task-open", "task-add-workspace":
	default:
		return nil, false
	}
	var entry struct {
		Dir     string `json:"dir"`
		RepoKey string `json:"repo_key"`
		Folded  bool   `json:"folded"`
		View    string `json:"view"`
		ID      string `json:"id"`
	}
	if err := json.Unmarshal(a.Payload, &entry); err != nil {
		panic(fmt.Sprintf("workspace create: action %s payload: %v", a.ID, err))
	}
	action := &frontendv1.HostAction{ActionId: a.ID}
	switch a.Type {
	case "switch":
		action.Action = &frontendv1.HostAction_SwitchWorkspace{SwitchWorkspace: &frontendv1.HostSwitchWorkspace{Dir: entry.Dir}}
	case "fold":
		action.Action = &frontendv1.HostAction_SetRepositoryFold{SetRepositoryFold: &frontendv1.HostSetRepositoryFold{RepoKey: entry.RepoKey, Folded: entry.Folded}}
	case "set-view":
		action.Action = &frontendv1.HostAction_SetSidebarView{SetSidebarView: &frontendv1.HostSetSidebarView{View: entry.View}}
	case "task-create":
		action.Action = &frontendv1.HostAction_TaskCreate{TaskCreate: &frontendv1.HostTaskCreate{}}
	case "task-toggle-done":
		action.Action = &frontendv1.HostAction_TaskToggleDone{TaskToggleDone: &frontendv1.HostTaskById{Id: entry.ID}}
	case "task-open":
		action.Action = &frontendv1.HostAction_TaskOpen{TaskOpen: &frontendv1.HostTaskById{Id: entry.ID}}
	case "task-add-workspace":
		action.Action = &frontendv1.HostAction_TaskAddWorkspace{TaskAddWorkspace: &frontendv1.HostTaskById{Id: entry.ID}}
	}
	return action, true
}

func NewWorkspaceCreateAssembly(cfg WorkspaceCreateAssemblyConfig) (*WorkspaceCreateAssembly, error) {
	if cfg.Commands == nil || cfg.Registry == nil || cfg.Geometry == nil || cfg.Health == nil || cfg.InitialPrompts == nil || cfg.Logf == nil {
		return nil, fmt.Errorf("workspace create: startup requires commands, registry, geometry recorder, health probe, initial-prompt router, and logger")
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
	worktrees := DaemonWorktree{Git: ExecGitRunner{}, Registry: cfg.Registry, Marker: osProjectileMarker{}, Logf: cfg.Logf}
	manager, err := workspacecreate.NewManager(workspacecreate.Config{
		Store: store, Planner: worktrees, Worktrees: worktrees, Geometry: cfg.Geometry,
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
	merges := &server.MergeDispatchBinding{Logf: cfg.Logf}
	return &WorkspaceCreateAssembly{
		Store:   store,
		Manager: manager,
		Inbox: &workspacecreate.Inbox{
			Dir: inboxPath, Store: store, Manager: manager,
			Merges: merges, Logf: cfg.Logf, Interval: interval,
		},
		Merges:    merges,
		Forwarder: forwarder,
	}, nil
}

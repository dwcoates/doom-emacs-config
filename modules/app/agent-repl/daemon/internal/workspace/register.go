package workspace

import (
	"context"
	"fmt"
	"path/filepath"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// Register records a workspace Emacs announced. It is IDEMPOTENT by normalized
// directory: announcing the same tree twice yields the same workspace, with the
// same id, and mints nothing the second time.
//
// The facts the announcement omits are DERIVED FROM GIT rather than guessed:
// the repository is the tree's canonicalized common dir, the branch is what is
// checked out, and the parent branch is the repository's default branch. A
// directory that is not a git worktree at all is REFUSED — registering it would
// put a row in the roster that no git verb could ever act on.
func (v *verbs) Register(ctx context.Context, dir string, facts wsm.RegisterFacts) (wsm.Workspace, error) {
	global := v.deps.Log.Global().With(dlog.Context{"dir": dir})

	normalized, err := normalizeDir(dir)
	if err != nil {
		global.Error(opRegister, "the announced directory cannot be normalized", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("register %q: %w", dir, err)
	}
	if !IsWorktree(normalized) {
		return wsm.Workspace{}, refuse(global, "RegisterWorkspace", ArmNotAWorktree,
			fmt.Sprintf("%q is not a git worktree", normalized), false)
	}

	log, err := v.deps.Log.Workspace(normalized)
	if err != nil {
		global.Error(opRegister, "could not resolve the workspace log sink", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("register %q: resolve log sink: %w", normalized, err)
	}

	if facts.RepoDir == "" {
		common, err := v.deps.Git.CommonDir(ctx, normalized)
		if err != nil {
			log.Error(opRegister, "could not resolve the repository common dir", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("register %q: repository common dir: %w", normalized, err)
		}
		facts.RepoDir = common
		log.Debug(opRegister, "derived the repository from git", dlog.Context{"repo_dir": common})
	}
	if facts.Branch == "" {
		branch, err := v.deps.Git.CurrentBranch(ctx, normalized)
		if err != nil {
			log.Error(opRegister, "could not resolve the checked-out branch", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("register %q: current branch: %w", normalized, err)
		}
		facts.Branch = branch
		log.Debug(opRegister, "derived the branch from git", dlog.Context{"branch": branch})
	}
	if facts.ParentBranch == "" {
		parent, err := v.deps.Git.DefaultBranch(ctx, facts.RepoDir)
		if err != nil {
			log.Error(opRegister, "could not resolve the repository default branch", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("register %q: default branch: %w", normalized, err)
		}
		facts.ParentBranch = parent
		log.Debug(opRegister, "derived the parent branch from git", dlog.Context{"parent_branch": parent})
	}
	if facts.Name == "" {
		facts.Name = filepath.Base(normalized)
		log.Debug(opRegister, "derived the display name from the directory", dlog.Context{"name": facts.Name})
	}

	record, created, err := v.deps.DB.RegisterWorkspace(ctx, normalized, facts)
	if err != nil {
		log.Error(opRegister, "could not record the workspace", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("register %q: %w", normalized, err)
	}
	if created {
		log.Info(opRegister, "registered a new workspace", dlog.Context{
			"workspace": string(record.ID), "branch": record.Branch, "repo": string(record.Repo),
		})
	} else {
		log.Debug(opRegister, "the workspace was already registered", dlog.Context{
			"workspace": string(record.ID),
		})
	}
	v.republishRegistry(ctx, log, opRegister)
	return record, nil
}

// PublishRegistry publishes the roster's durable half once, from what the
// registry holds right now.
//
// It exists because the roster is EDITOR-GLOBAL and is otherwise published
// only as a side effect of a verb: a daemon that has just booted and has been
// asked for nothing yet would leave `WatchWorkspaceRoster` with no value to
// deliver, and the first client would wait for a workspace to change before it
// saw the roster at all — including the EMPTY roster, which is a roster.
func (v *verbs) PublishRegistry(ctx context.Context) error {
	log := v.deps.Log.Global()
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opRegister, "could not list the workspaces for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: list the workspaces: %w", err)
	}
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		log.Error(opRegister, "could not list the repositories for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: list the repositories: %w", err)
	}
	tasks, err := v.deps.DB.Tasks(ctx)
	if err != nil {
		log.Error(opRegister, "could not list the tasks for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: list the tasks: %w", err)
	}
	current, err := v.deps.DB.Current(ctx)
	if err != nil {
		log.Error(opRegister, "could not read the current workspace for the opening roster", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("publish the opening roster: read the current workspace: %w", err)
	}
	v.deps.Sidebar.SetRegistry(sidebarRegistry(workspaces, repositories, tasks, current))
	log.Debug(opRegister, "published the opening roster", dlog.Context{
		"workspaces": len(workspaces), "repositories": len(repositories), "tasks": len(tasks),
	})
	return nil
}

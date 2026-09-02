package workspace

import (
	"context"
	"fmt"
	"path/filepath"

	"claude-repld/internal/dlog"
	"claude-repld/internal/resolve/topbar"
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
		// THE REPOSITORY IS ITS MAIN WORKTREE, not its common dir.
		// workspace.v1's RepositoryRef.dir is "the repository's normalized
		// main-worktree directory", and the merge geometry a top-level
		// workspace targets is that same directory: keyed by the common dir,
		// every merge ran `git -C <repo>/.git` and the self-repo comparison
		// the rollout trigger keys on could never match.
		main, err := v.deps.Git.MainWorktree(ctx, normalized)
		if err != nil {
			log.Error(opRegister, "could not resolve the repository's main worktree", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("register %q: repository main worktree: %w", normalized, err)
		}
		facts.RepoDir = main
		log.Debug(opRegister, "derived the repository from git", dlog.Context{"repo_dir": main})
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
	if facts.DefaultBranch == "" {
		branch, err := v.deps.Git.DefaultBranch(ctx, facts.RepoDir)
		if err != nil {
			log.Error(opRegister, "could not resolve the repository default branch", dlog.Context{"cause": err.Error()})
			return wsm.Workspace{}, fmt.Errorf("register %q: default branch: %w", normalized, err)
		}
		facts.DefaultBranch = branch
		log.Debug(opRegister, "derived the repository default branch from git", dlog.Context{
			"default_branch": branch,
		})
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
	// THE RESOLVERS ARE BOUND HERE. The footer, the topbar and the hold tray
	// each write their records to the workspace's own log sink and resolve
	// nothing themselves, so a frame for a workspace they were never told the
	// directory of is an invariant violation they report and cannot serve.
	// Registration is the one moment every workspace passes through.
	if err := v.bindResolvers(log, record.ID, normalized); err != nil {
		return wsm.Workspace{}, fmt.Errorf("register %q: %w", normalized, err)
	}
	if err := v.publishNaming(ctx, log, record, facts.DefaultBranch); err != nil {
		return wsm.Workspace{}, fmt.Errorf("register %q: %w", normalized, err)
	}

	v.republishRegistry(ctx, log, opRegister)
	return record, nil
}

// bindResolvers binds one workspace's directory on every resolver that needs
// telling. A failure is the caller's, never absorbed: a resolver that does not
// know where a workspace is cannot serve its views at all.
func (v *verbs) bindResolvers(log dlog.Logger, id wsm.WorkspaceID, dir string) error {
	binds := []struct {
		name string
		bind func(wsm.WorkspaceID, string) error
	}{
		{"footer", v.deps.Footer.SetWorkspaceDir},
		{"topbar", v.deps.Topbar.SetWorkspaceDir},
		{"holds", v.deps.Holds.SetWorkspaceDir},
	}
	for _, b := range binds {
		if err := b.bind(id, dir); err != nil {
			log.Error(opRegister, "a resolver could not be bound to the workspace's directory", dlog.Context{
				"resolver": b.name, "workspace": string(id), "cause": err.Error(),
			})
			return fmt.Errorf("bind the %s resolver to %q: %w", b.name, dir, err)
		}
	}
	log.Debug(opRegister, "bound the workspace's directory on every resolver that needs it", dlog.Context{
		"workspace": string(id),
	})
	return nil
}

// publishNaming installs the topbar's naming and account lines: both are
// DAEMON facts (the registry's name and branch, the repo-under-root account
// rule) and no vendor frame carries either, so nothing else can state them.
// Until they are installed the topbar is not complete and publishes NO view at
// all, so this is part of registration rather than of a session's bring-up.
func (v *verbs) publishNaming(ctx context.Context, log dlog.Logger, record wsm.Workspace, defaultBranch string) error {
	configDir := v.deps.Accounts.ConfigDirFor(record.Dir)
	account, err := v.deps.Accounts.Read(ctx, configDir)
	if err != nil {
		log.Error(opRegister, "the account root could not be read", dlog.Context{
			"config_dir": configDir, "cause": err.Error(),
		})
		return fmt.Errorf("read the account root %q: %w", configDir, err)
	}
	v.deps.Topbar.SetNaming(record.ID, topbar.Naming{
		Slug:          record.Name,
		Title:         record.Name,
		Branch:        record.Branch,
		DefaultBranch: defaultBranch,
		ConfigDir:     configDir,
	})
	// An EMPTY email is the logged-out arm, which the topbar draws as a
	// warning rather than a blank label: it is an answer, never a gap.
	v.deps.Topbar.SetAccount(record.ID, account.Email)
	log.Debug(opRegister, "installed the topbar's naming and account lines", dlog.Context{
		"workspace": string(record.ID), "config_dir": configDir,
		"logged_in": account.LoggedIn, "default_branch": defaultBranch,
	})
	return nil
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
	// Every workspace the registry already holds is bound too: a daemon that
	// has just restarted did not run Register for the rows it inherited, and
	// an unbound resolver cannot serve their views.
	global := v.deps.Log.Global()
	defaults := make(map[wsm.RepoID]string, len(repositories))
	for _, repo := range repositories {
		defaults[repo.ID] = repo.DefaultBranch
	}
	for _, ws := range workspaces {
		if err := v.bindResolvers(global, ws.ID, ws.Dir); err != nil {
			return fmt.Errorf("publish the opening roster: %w", err)
		}
		if err := v.publishNaming(ctx, global, ws, defaults[ws.Repo]); err != nil {
			return fmt.Errorf("publish the opening roster: %w", err)
		}
	}
	v.deps.Sidebar.SetRegistry(sidebarRegistry(workspaces, repositories, tasks, current))
	log.Debug(opRegister, "published the opening roster", dlog.Context{
		"workspaces": len(workspaces), "repositories": len(repositories), "tasks": len(tasks),
	})
	return nil
}

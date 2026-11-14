package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prwatch"
	"agent-repld/internal/wsm"
)

// opPullTarget is the operation a workspace's pull request target is bound
// under.
const opPullTarget = "daemon.workspace.pull_target"

// bindPullTarget installs on the feed the GitHub repository RECORD's
// `#<number>` references link into: the owner of the account its directory is
// routed to (Deps.PullOwners), and the repository's name as its `origin`
// remote gives it. A thread, an account with no owner, or a repository whose
// origin is unreadable or off github.com links nowhere. A git workspace whose repository the
// registry does not hold is an error.
func (v *verbs) bindPullTarget(ctx context.Context, log dlog.Logger, record wsm.Workspace) error {
	if record.IsThread() {
		v.deps.Feed.SetPullTarget(record.ID, nil)
		return nil
	}
	repos, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		log.Error(opPullTarget, "could not list the repositories to bind the workspace's pull request target", dlog.Context{
			"workspace": string(record.ID), "cause": err.Error(),
		})
		return fmt.Errorf("bind the pull request target of %q: list the repositories: %w", record.ID, err)
	}
	repo, found := wsm.RepositoryWithID(repos, record.Repo)
	if !found {
		log.Error(opPullTarget, "the workspace's repository is not in the registry", dlog.Context{
			"workspace": string(record.ID), "repository": string(record.Repo),
		})
		return fmt.Errorf("bind the pull request target of %q: repository %q is not in the registry", record.ID, record.Repo)
	}
	configDir := v.deps.Accounts.ConfigDirFor(record.Dir)
	if _, owned := v.deps.PullOwners.OwnerFor(configDir); !owned {
		log.Debug(opPullTarget, "the workspace's account has no GitHub owner; its references link nowhere", dlog.Context{
			"workspace": string(record.ID), "config_dir": configDir, "repository_dir": repo.Dir,
		})
		v.deps.Feed.SetPullTarget(record.ID, nil)
		return nil
	}
	// THE GITHUB NAME IS THE ORIGIN'S, never the checkout directory's: the
	// two differ (the `doom` checkout's origin is `doom-emacs-config`), and a
	// name read off the directory linked every `#n` to the wrong repository.
	remote, err := v.deps.Git.RemoteURL(ctx, repo.Dir, "origin")
	if err != nil {
		// A repository with no readable origin (a local-only repository) has
		// no GitHub repository to link into: an ordinary fact, never a
		// failure of the registration.
		log.Info(opPullTarget, "could not read the repository's origin remote; its references link nowhere", dlog.Context{
			"workspace": string(record.ID), "repository_dir": repo.Dir, "cause": err.Error(),
		})
		v.deps.Feed.SetPullTarget(record.ID, nil)
		return nil
	}
	_, name, onGithub := prwatch.RepositoryOfRemote(remote)
	if !onGithub {
		log.Debug(opPullTarget, "the repository's origin is not on github.com; its references link nowhere", dlog.Context{
			"workspace": string(record.ID), "repository_dir": repo.Dir, "origin": remote,
		})
		v.deps.Feed.SetPullTarget(record.ID, nil)
		return nil
	}
	target, _ := v.deps.PullOwners.TargetFor(configDir, name)
	v.deps.Feed.SetPullTarget(record.ID, &target)
	return nil
}

package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// opPullTarget is the operation a workspace's pull request target is bound
// under.
const opPullTarget = "daemon.workspace.pull_target"

// bindPullTarget installs on the feed the GitHub repository RECORD's
// `#<number>` references link into: the owner of the account its directory is
// routed to (Deps.PullOwners), and its repository's name. A thread, or an
// account with no owner, links nowhere. A git workspace whose repository the
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
	target, ok := v.deps.PullOwners.TargetFor(configDir, repo.Dir)
	if !ok {
		log.Debug(opPullTarget, "the workspace's account has no GitHub owner; its references link nowhere", dlog.Context{
			"workspace": string(record.ID), "config_dir": configDir, "repository_dir": repo.Dir,
		})
		v.deps.Feed.SetPullTarget(record.ID, nil)
		return nil
	}
	v.deps.Feed.SetPullTarget(record.ID, &target)
	return nil
}

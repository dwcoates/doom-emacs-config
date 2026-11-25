package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/wsm"
)

// THE PRODUCTION-CHECKS CHIP (topbar productionchecks.go) states the
// repository's production-check standing, which the merge orchestrator owns
// (merge.Orchestrator.ProductionStanding). It rides the naming, and every
// pause or resume restates it on each open workspace of the repository
// (RestateProductionChecks).

const opProductionChecks = "daemon.workspace.production_checks"

// productionChecksOf resolves repository REPO's production-checks chip, the
// same for every workspace of it.
func (v *verbs) productionChecksOf(ctx context.Context, repo ids.RepoID) (topbar.ProductionChecks, error) {
	standing, err := v.deps.Merge.ProductionStanding(ctx, merge.RepositoryScope{ID: repo})
	if err != nil {
		return topbar.ProductionChecks{}, fmt.Errorf("read the production standing of repository %q: %w", repo, err)
	}
	if !standing.On {
		return topbar.ProductionChecks{}, nil
	}
	repos, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		return topbar.ProductionChecks{}, fmt.Errorf("list the repositories to name %q's: %w", repo, err)
	}
	repository, found := wsm.RepositoryNamed(repos, repo, "")
	if !found {
		return topbar.ProductionChecks{}, fmt.Errorf("the registry holds no repository %q, whose production standing was read", repo)
	}
	chip := topbar.ProductionChecks{On: true, Paused: standing.Paused, Repository: repo, RepositoryDir: repository.Dir}
	if standing.Mediator != "" {
		mediator, err := v.deps.DB.Workspace(ctx, standing.Mediator)
		if err != nil {
			return topbar.ProductionChecks{}, fmt.Errorf("read the production mediator %q's name: %w", standing.Mediator, err)
		}
		chip.Mediator = mediator.Name
	}
	return chip, nil
}

// RestateProductionChecks implements Verbs: every open workspace of
// repository REPO draws its repository's production-check standing as it
// now is.
func (v *verbs) RestateProductionChecks(ctx context.Context, repo ids.RepoID) error {
	log := v.deps.Log.Global()
	fields := dlog.Context{"repository": string(repo)}
	chip, err := v.productionChecksOf(ctx, repo)
	if err != nil {
		log.Error(opProductionChecks, "could not resolve the repository's production-checks chip; its workspaces' chips are stale", dlog.WithCause(fields, err))
		return err
	}
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opProductionChecks, "could not list the workspaces whose production-checks chips changed; their chips are stale", dlog.WithCause(fields, err))
		return fmt.Errorf("list the workspaces of repository %q: %w", repo, err)
	}
	restated := 0
	for _, record := range workspaces {
		if record.Repo != repo || record.Closed {
			continue
		}
		v.deps.Topbar.SetProductionChecks(record.ID, chip)
		restated++
	}
	log.Info(opProductionChecks, "restated the repository's production-checks chip on each of its open workspaces", dlog.Context{
		"repository": string(repo), "on": chip.On, "paused": chip.Paused, "workspaces": restated,
	})
	return nil
}

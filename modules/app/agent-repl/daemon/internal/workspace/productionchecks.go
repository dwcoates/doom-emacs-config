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

// productionStandings is every registered repository's production standing,
// read once (readProductionStandings) for a pass that names workspaces, so a
// pass naming every workspace resolves the repositories once rather than once
// per workspace. A repository absent from it checks no production.
type productionStandings map[ids.RepoID]merge.ProductionStanding

// readProductionStandings reads every repository's production standing.
func (v *verbs) readProductionStandings(ctx context.Context) (productionStandings, error) {
	standings, err := v.deps.Merge.ProductionStandings(ctx)
	if err != nil {
		return nil, fmt.Errorf("read the repositories' production standings: %w", err)
	}
	return standings, nil
}

// productionChecksOf resolves repository REPO's production-checks chip from
// STANDINGS, the same for every workspace of it.
func (v *verbs) productionChecksOf(ctx context.Context, standings productionStandings, repo ids.RepoID) (topbar.ProductionChecks, error) {
	standing := standings[repo]
	if !standing.On {
		return topbar.ProductionChecks{}, nil
	}
	chip := topbar.ProductionChecks{On: true, Paused: standing.Paused, Repository: repo, RepositoryDir: standing.Dir}
	if standing.Mediator != "" {
		mediator, err := v.deps.DB.Workspace(ctx, standing.Mediator)
		if err != nil {
			return topbar.ProductionChecks{}, fmt.Errorf("read the production mediator %q's name: %w", standing.Mediator, err)
		}
		chip.Mediator = mediator.Name
	}
	return chip, nil
}

// RestateProductionChecks implements Verbs: every open workspace of the
// repository SCOPE names draws its repository's production-check standing as
// it now is.
func (v *verbs) RestateProductionChecks(ctx context.Context, scope merge.RepositoryScope) error {
	log := v.deps.Log.Global()
	fields := dlog.Context{"repository": string(scope.ID), "dir": scope.Dir}
	repos, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		log.Error(opProductionChecks, "could not list the repositories to restate a production-checks chip; its workspaces' chips are stale", dlog.WithCause(fields, err))
		return fmt.Errorf("list the repositories: %w", err)
	}
	repository, found := wsm.RepositoryNamed(repos, scope.ID, scope.Dir)
	if !found {
		err := fmt.Errorf("the registry holds no repository %q at %q", scope.ID, scope.Dir)
		log.Error(opProductionChecks, "could not find the repository whose production-checks chip changed; its workspaces' chips are stale", dlog.WithCause(fields, err))
		return err
	}
	repo := repository.ID
	fields["repository"] = string(repo)
	standings, err := v.readProductionStandings(ctx)
	if err != nil {
		log.Error(opProductionChecks, "could not read the repositories' production standings; the repository's workspaces' chips are stale", dlog.WithCause(fields, err))
		return err
	}
	chip, err := v.productionChecksOf(ctx, standings, repo)
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
		if record.Repo != repo || record.Closed || record.IsThread() {
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

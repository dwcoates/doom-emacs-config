package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// opMediator is the operation the deploy mediator's making is filed under.
const opMediator = "daemon.workspace.deploy_mediator"

// mediatorModel and mediatorEffort are what the deploy mediator runs on
// (owner request, 2026-11-13: opus at medium effort).
const (
	mediatorModel  = DefaultLaunchModel
	mediatorEffort = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM
)

// mediatorSpec is the create of the deploy mediator NAME of the repository at
// REPODIR: hidden from the roster's rows (the merge queue draws it), on the
// mediator's model, with no initial prompt -- its first prompt is a deploy's
// mediation brief.
func mediatorSpec(repoDir, name string) CreateSpec {
	spec := NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	spec.RepoDir, spec.Name, spec.Hidden, spec.Model = repoDir, name, true, mediatorModel
	return spec
}

// CreateMediator implements Verbs: makes the repository's deploy mediator and
// sets its effort. A mediator made whose effort could not be set is answered
// as a failure, and recorded: it is found by its name next round, at the
// effort it was made with.
func (v *verbs) CreateMediator(ctx context.Context, repoDir, name string) (wsm.Workspace, error) {
	fields := dlog.Context{"repo": repoDir, "mediator": name}
	made, err := v.Create(ctx, mediatorSpec(repoDir, name))
	if err != nil {
		v.deps.Log.Global().Error(opMediator, "the deploy mediator could not be made", dlog.WithCause(fields, err))
		return wsm.Workspace{}, fmt.Errorf("make the deploy mediator %q: %w", name, err)
	}
	fields["workspace"] = string(made.ID)
	if err := v.SetEffort(ctx, made.ID, mediatorEffort); err != nil {
		v.deps.Log.Global().Error(opMediator, "the deploy mediator was made but its effort could not be set", dlog.WithCause(fields, err))
		return made, fmt.Errorf("set the deploy mediator %q's effort: %w", name, err)
	}
	v.deps.Log.Global().Info(opMediator, "made the deploy mediator", fields)
	return made, nil
}

// MediatesFrom answers whether a workspace is its repository's deploy
// mediator: its repository's lifecycle config, read through FILES, names it.
// A repository with no lifecycle config names none.
func MediatesFrom(files lifecycleconfig.Files, db wsm.DB) func(ctx context.Context, record wsm.Workspace) (bool, error) {
	return func(ctx context.Context, record wsm.Workspace) (bool, error) {
		repos, err := db.ListRepositories(ctx)
		if err != nil {
			return false, fmt.Errorf("list the repositories to find %q's: %w", record.Name, err)
		}
		for _, repo := range repos {
			if repo.ID != record.Repo {
				continue
			}
			cfg, err := files.Load(repo.Dir)
			var missing *lifecycleconfig.MissingError
			if errors.As(err, &missing) {
				return false, nil
			}
			if err != nil {
				return false, fmt.Errorf("read the lifecycle config of %s: %w", repo.Dir, err)
			}
			return cfg.GetDeployMediator().GetName() == record.Name, nil
		}
		return false, nil
	}
}

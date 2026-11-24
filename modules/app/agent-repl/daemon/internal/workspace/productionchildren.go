package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// THE PRODUCTION MEDIATOR'S CHILDREN (docs/investigations/
// 2026-11-23-production-mediator-plan.md). Each is a one-shot child of the
// mediator, cut from and landing on the main branch at the front of the
// merge queue, on productionChildModel whatever the mediator runs on (owner
// ruling, 2026-11-23), commissioned with its findings and its siblings.

// productionChildModel is every production child's model (owner ruling,
// 2026-11-23: "children should be opus by default, not the parent's model").
const productionChildModel = "opus"

// productionChildSpec is the create of a child of MEDIATOR in the repository
// at REPODIR, named NAME and commissioned with PROMPT.
func productionChildSpec(repoDir string, mediator ids.WorkspaceID, name, prompt string) CreateSpec {
	spec := NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	spec.RepoDir, spec.Name, spec.InitialPrompt = repoDir, name, prompt
	spec.OneShot, spec.FinishPlacement = true, wsm.FinishPlacementFront
	spec.Parent, spec.Requester = &mediator, &mediator
	spec.Landing = &wsm.LandingChoice{}
	spec.Model = productionChildModel
	return spec
}

// CreateProductionChild implements Verbs: makes a child of production
// mediator MEDIATOR (productionChildSpec).
func (v *verbs) CreateProductionChild(ctx context.Context, repoDir string, mediator ids.WorkspaceID, name, prompt string) (wsm.Workspace, error) {
	spec := productionChildSpec(repoDir, mediator, name, prompt)
	fields := dlog.Context{"repo": repoDir, "mediator": string(mediator), "name": name}
	made, err := v.Create(ctx, spec)
	if err != nil {
		v.deps.Log.Global().Warn(opDeployChecks, "a production child could not be made", dlog.WithCause(fields, err))
		return wsm.Workspace{}, fmt.Errorf("make the production child %q: %w", name, err)
	}
	v.deps.Log.Global().Info(opDeployChecks, "made a production child", withWorkspace(fields, made.ID))
	return made, nil
}

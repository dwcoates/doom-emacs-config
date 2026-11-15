package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// THE DEPLOY CHECKS' WORKSPACES (repoconfig.v1.LifecycleConfig.deploy_checks;
// docs/investigations/2026-11-14-deploy-intake-plan.md). The merge queue
// makes one INTAKE per chain of deploys and a REMEDIATION per fix the intake
// dispatches fresh. Both are made with no initial prompt, so no session comes
// up at the create. The queue records the workspace's role on its chain
// FIRST, then sets its effort (SetDeployCheckEffort, which starts a
// sessionless workspace's shim), so its session always comes up under the
// landing that role reads (sessionProfile).

// opDeployChecks is the operation the deploy checks' workspaces are made
// under.
const opDeployChecks = "daemon.workspace.deploy_checks"

// The models and efforts the owner ruled (2026-11-14): the intake states
// problems and decides, on sonnet at low effort; the remediation researches,
// designs and fixes, on opus at medium effort.
const (
	intakeModel       = WorkDefaultLaunchModel
	intakeEffort      = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW
	remediationModel  = DefaultLaunchModel
	remediationEffort = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM
)

// deployCheckSpec is the create of the deploy check workspace NAME of the
// repository at REPODIR on MODEL: hidden when HIDDEN, with no initial prompt.
func deployCheckSpec(repoDir, name, model string, hidden bool) CreateSpec {
	spec := NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	spec.RepoDir, spec.Name, spec.Hidden, spec.Model = repoDir, name, hidden, model
	return spec
}

// CreateDeployIntake implements Verbs: makes a chain's hidden intake
// workspace, unprompted.
func (v *verbs) CreateDeployIntake(ctx context.Context, repoDir, name string) (wsm.Workspace, error) {
	return v.createDeployCheck(ctx, "intake", deployCheckSpec(repoDir, name, intakeModel, true))
}

// CreateDeployRemediation implements Verbs: makes a visible remediation
// workspace, unprompted.
func (v *verbs) CreateDeployRemediation(ctx context.Context, repoDir, name string) (wsm.Workspace, error) {
	return v.createDeployCheck(ctx, "remediation", deployCheckSpec(repoDir, name, remediationModel, false))
}

// CreateDeployEscalation implements Verbs: makes a visible escalation
// workspace, unprompted, where the user settles an escalated deploy (owner
// ruling, 2026-11-15).
func (v *verbs) CreateDeployEscalation(ctx context.Context, repoDir, name string) (wsm.Workspace, error) {
	return v.createDeployCheck(ctx, "escalation", deployCheckSpec(repoDir, name, remediationModel, false))
}

// createDeployCheck makes SPEC, a ROLE workspace. A make that fails is
// recorded.
func (v *verbs) createDeployCheck(ctx context.Context, role string, spec CreateSpec) (wsm.Workspace, error) {
	fields := dlog.Context{"repo": spec.RepoDir, "role": role, "name": spec.Name, "model": spec.Model}
	made, err := v.Create(ctx, spec)
	if err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the deploy check workspace could not be made", dlog.WithCause(fields, err))
		return wsm.Workspace{}, fmt.Errorf("make the deploy %s %q: %w", role, spec.Name, err)
	}
	v.deps.Log.Global().Info(opDeployChecks, "made the deploy check workspace", withWorkspace(fields, made.ID))
	return made, nil
}

// SetDeployCheckEffort implements Verbs: sets WS to its role's effort, the
// intake's when INTAKE and the remediation's otherwise. A sessionless
// workspace's shim starts for it, so its role is recorded before.
func (v *verbs) SetDeployCheckEffort(ctx context.Context, ws wsm.WorkspaceID, intake bool) error {
	role, effort := "remediation", remediationEffort
	if intake {
		role, effort = "intake", intakeEffort
	}
	fields := dlog.Context{"workspace": string(ws), "role": role, "effort": effort.String()}
	if err := v.SetEffort(ctx, ws, effort); err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the deploy check workspace's effort could not be set", dlog.WithCause(fields, err))
		return fmt.Errorf("set the deploy %s %s's effort: %w", role, ws, err)
	}
	v.deps.Log.Global().Info(opDeployChecks, "set the deploy check workspace's effort", fields)
	return nil
}

// withWorkspace is FIELDS naming workspace WS.
func withWorkspace(fields dlog.Context, ws wsm.WorkspaceID) dlog.Context {
	fields["workspace"] = string(ws)
	return fields
}

// isDeployIntake answers whether WS is the intake of the chain its
// repository's queue owes a check of: its recorded role, never its name.
func isDeployIntake(ctx context.Context, db wsm.DB, ws wsm.WorkspaceID) (bool, error) {
	role, err := db.MediationRoleOf(ctx, ws)
	if err != nil {
		return false, err
	}
	return role != nil && role.Intake, nil
}

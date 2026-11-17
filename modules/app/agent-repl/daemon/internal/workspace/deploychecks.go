package workspace

import (
	"context"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/merge"
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

// The models and efforts the owner ruled: the intake states problems and
// decides, on sonnet at low effort (2026-11-14); the remediation fixes what
// it is dispatched, on opus at low effort, and the escalation settles a
// deploy with the user, on opus at medium effort (2026-11-16).
const (
	intakeModel       = WorkDefaultLaunchModel
	intakeEffort      = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW
	remediationModel  = DefaultLaunchModel
	remediationEffort = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW
	escalationEffort  = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM
)

// deployCheckEffort is ROLE's effort. A role outside the three is a
// caller's defect and panics.
func deployCheckEffort(role merge.DeployCheckRole) conversationv1.AgentEffortLevel {
	switch role {
	case merge.DeployCheckIntake:
		return intakeEffort
	case merge.DeployCheckRemediation:
		return remediationEffort
	case merge.DeployCheckEscalation:
		return escalationEffort
	}
	panic(fmt.Sprintf("workspace: no effort for deploy check role %v", role))
}

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
//
// IT IS MADE IN REMOTE CONTROL (owner ruling, 2026-11-16): the user settles
// an escalation from wherever they are, so its mode is recorded on before
// its first session starts, which puts the mode in force at bring-up.
func (v *verbs) CreateDeployEscalation(ctx context.Context, repoDir, name string) (wsm.Workspace, error) {
	made, err := v.createDeployCheck(ctx, "escalation", deployCheckSpec(repoDir, name, remediationModel, false))
	if err != nil {
		return wsm.Workspace{}, err
	}
	if _, err := v.deps.DB.SetRemoteControlMode(ctx, made.ID, wsm.RemoteControlOn); err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the escalation workspace was made but its remote control could not be turned on", dlog.WithCause(withWorkspace(dlog.Context{"repo": repoDir, "name": name}, made.ID), err))
		return wsm.Workspace{}, fmt.Errorf("turn remote control on for the deploy escalation %q: %w", name, err)
	}
	return made, nil
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

// SetDeployCheckEffort implements Verbs: sets WS to ROLE's effort
// (deployCheckEffort). A sessionless workspace's shim starts for it, so its
// role is recorded before.
func (v *verbs) SetDeployCheckEffort(ctx context.Context, ws wsm.WorkspaceID, role merge.DeployCheckRole) error {
	effort := deployCheckEffort(role)
	fields := dlog.Context{"workspace": string(ws), "role": role.String(), "effort": effort.String()}
	// A WORKSPACE WITH NO SESSION YET SERVES NO EFFORT SELECTOR, so its level
	// is picked rather than set: the session the role's brief starts comes up
	// at the pick (Fleet.reapplyEffort). Setting it through the selector
	// refused every fresh intake (2026-11-15 16:17, 0.444.0: "serves no
	// effort level for its model") and left the queue held for it.
	if _, served := v.deps.Cards.EffortLevels(ws); !served {
		v.deps.Topbar.SetPickedEffort(ws, effort)
		v.deps.Log.Global().Info(opDeployChecks, "picked the deploy check workspace's effort; its session starts at it", fields)
		return nil
	}
	if err := v.SetEffort(ctx, ws, effort); err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the deploy check workspace's effort could not be set", dlog.WithCause(fields, err))
		return fmt.Errorf("set the deploy %s %s's effort: %w", role, ws, err)
	}
	v.deps.Log.Global().Info(opDeployChecks, "set the deploy check workspace's effort", fields)
	return nil
}

// StopDeployCheck implements Verbs: ends deploy check workspace WS's session
// at once, killed mid-turn, never at its turn's end (owner ruling,
// 2026-11-16). An intake's decision is durable before the chain ends it, so
// what the turn would have said after it is nothing anyone reads: a turn
// left to end wrote a reply an approved, hidden intake should never have.
//
// THE WORKSPACE STAYS OPEN: a standing intake takes every check of its
// repository (owner ruling, 2026-11-17), its conversation kept, and the next
// check's brief starts its session again.
func (v *verbs) StopDeployCheck(ctx context.Context, ws wsm.WorkspaceID) error {
	_, log, err := v.owned(ctx, "StopDeployCheck", ws)
	if err != nil {
		return err
	}
	if _, err := v.endSession(ctx, log, ws); err != nil {
		return fmt.Errorf("stop the deploy check %s: %w", ws, err)
	}
	log.Info(opDeployChecks, "stopped the deploy check workspace's session at once; its turn is cut, not ended, and the workspace stays open", nil)
	return nil
}

// StandByDeployCheck implements Verbs: a fixer whose finish the daemon
// accepted is hidden and its session ended at once, its turn cut, before it
// can write a reply. The fixer is in sight only while it is at work holding
// its queue (owner ruling, 2026-11-17): from its finish on, its repository's
// deploy check label is how it is reached. It stays open and warm
// (wsm.Workspace.StandingDeployCheck), and its next brief shows it again.
func (v *verbs) StandByDeployCheck(ctx context.Context, ws wsm.WorkspaceID) error {
	_, log, err := v.owned(ctx, "StandByDeployCheck", ws)
	if err != nil {
		return err
	}
	if err := v.recordHidden(ctx, log, ws, true); err != nil {
		return fmt.Errorf("stand the deploy check %s by: hide: %w", ws, err)
	}
	if _, err := v.endSession(ctx, log, ws); err != nil {
		return fmt.Errorf("stand the deploy check %s by: %w", ws, err)
	}
	log.Info(opDeployChecks, "the finished deploy check workspace stands by: hidden, its session ended, open and warm for its next brief", nil)
	return nil
}

// HideDeployCheck implements Verbs: hides standing fixer WS, which no check
// holds its queue for any more (a released escalation, a fixer left in
// sight by an earlier build), its session untouched.
func (v *verbs) HideDeployCheck(ctx context.Context, ws wsm.WorkspaceID) error {
	record, log, err := v.owned(ctx, "HideDeployCheck", ws)
	if err != nil {
		return err
	}
	if record.Hidden || record.Closed {
		return nil
	}
	if err := v.recordHidden(ctx, log, ws, true); err != nil {
		return fmt.Errorf("hide the deploy check %s: %w", ws, err)
	}
	log.Info(opDeployChecks, "the deploy check workspace holds its queue no more; it is hidden, its session untouched", nil)
	return nil
}

// CompactDeployCheck implements Verbs: compacts standing deploy check WS's
// conversation by the cold gate's summarizer (Haiku) NOW, answering whether
// it did (owner ruling, 2026-11-17): the compaction is owed to the session's
// next start (wsm.OweCompaction), and the session is ended and started
// again at once to carry it out, so no later check waits on it. A workspace
// with no conversation has nothing to compact, and one running a turn (the
// user's, in the fixer) is never cut: its next start compacts it.
func (v *verbs) CompactDeployCheck(ctx context.Context, ws wsm.WorkspaceID) (bool, error) {
	_, log, err := v.owned(ctx, "CompactDeployCheck", ws)
	if err != nil {
		return false, err
	}
	if _, err := v.deps.DB.OweCompaction(ctx, ws); err != nil {
		log.Error(opDeployChecks, "the deploy check workspace's compaction could not be owed", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("compact the deploy check %s: owe the compaction: %w", ws, err)
	}
	owed, found, err := v.deps.DB.OwedStartWork(ctx, ws)
	if err != nil {
		log.Error(opDeployChecks, "could not read the deploy check workspace's owed start work", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("compact the deploy check %s: read the owed start work: %w", ws, err)
	}
	if !found || !owed.Compact {
		log.Info(opDeployChecks, "the deploy check workspace has no conversation to compact", dlog.Context{"owed": found})
		return false, nil
	}
	// A TURN RUNNING IS NEVER CUT FOR A COMPACTION: a turn the user started
	// in the fixer keeps it, and the compaction stays owed to its next start.
	open, err := v.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		log.Error(opDeployChecks, "could not read whether a turn runs in the deploy check workspace; it is not compacted now", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("compact the deploy check %s: read its open turns: %w", ws, err)
	}
	if len(open) > 0 {
		log.Info(opDeployChecks, "a turn runs in the deploy check workspace; its compaction waits for its next start", dlog.Context{"open_turns": len(open)})
		return false, nil
	}
	if v.deps.Sessions.Live(ws) {
		if _, err := v.endSession(ctx, log, ws); err != nil {
			return false, fmt.Errorf("compact the deploy check %s: end the session: %w", ws, err)
		}
	}
	if err := v.deps.Sessions.Start(ctx, ws); err != nil {
		log.Error(opDeployChecks, "the deploy check workspace's session could not start to compact", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("compact the deploy check %s: start the compacting session: %w", ws, err)
	}
	log.Info(opDeployChecks, "compacted the deploy check workspace's conversation; its session runs on the summary", nil)
	return true, nil
}

// AnswerDeployCheckColdGate implements Verbs: answers the cold gate that
// stands on standing deploy check WS with compact-and-resume at once, by the
// cold gate's default summarizer (Haiku), so a held brief goes on without
// the user (owner ruling, 2026-11-17: no deploy check waits on a cold gate).
// A workspace that is no standing deploy check, or has no gate standing, is
// left as it is.
func (v *verbs) AnswerDeployCheckColdGate(ctx context.Context, ws wsm.WorkspaceID) error {
	// EVERY GATE RAISED COMES HERE, so a workspace that is no standing deploy
	// check is passed over before any ownership is asked: a handover raises
	// gates again on workspaces this daemon has not adopted yet (2026-11-17
	// 13:24: five not_yet_adopted ERRORs, none a deploy check).
	if standing, err := v.deps.DB.Workspace(ctx, ws); err != nil || !standing.StandingDeployCheck {
		return err
	}
	_, log, err := v.owned(ctx, "AnswerDeployCheckColdGate", ws)
	if err != nil {
		return err
	}
	if _, standing := v.deps.Cards.ColdGate(ws); !standing {
		return nil
	}
	summarizer := defaultSummarizer()
	log.Info(opDeployChecks, "a cold gate stands on the standing deploy check; it is answered compact-and-resume at once", nil)
	return v.AnswerColdGate(ctx, ws, &agentreplv1.AnswerColdGateRequest{
		Choice: &agentreplv1.AnswerColdGateRequest_CompactAndResume{CompactAndResume: &agentreplv1.AnswerColdGateCompact{
			Model: summarizer.GetModel(), Scope: summarizer.GetScope()}},
	})
}

// RefreshDeployCheck implements Verbs: brings standing fixer WS's worktree
// to its parent branch's tip before a new brief (advanceToParent), so its
// work starts from every fix landed since. A worktree holding uncommitted
// work or commits the main branch lacks stays where it is, and says so.
func (v *verbs) RefreshDeployCheck(ctx context.Context, ws wsm.WorkspaceID) error {
	record, log, err := v.owned(ctx, "RefreshDeployCheck", ws)
	if err != nil {
		return err
	}
	return v.advanceToParent(ctx, log, record, opDeployChecks, "refresh", "standing deploy fixer")
}

// ResumeDeployCheck implements Verbs: shows standing-by deploy check
// workspace WS again, for the brief that sends it back to work; the brief's
// prompt brings its session up.
func (v *verbs) ResumeDeployCheck(ctx context.Context, ws wsm.WorkspaceID) error {
	_, log, err := v.owned(ctx, "ResumeDeployCheck", ws)
	if err != nil {
		return err
	}
	if err := v.recordHidden(ctx, log, ws, false); err != nil {
		return fmt.Errorf("resume the deploy check %s: show: %w", ws, err)
	}
	log.Info(opDeployChecks, "the deploy check workspace is shown again; its brief brings its session up", nil)
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

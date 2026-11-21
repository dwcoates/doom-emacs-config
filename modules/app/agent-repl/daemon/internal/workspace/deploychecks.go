package workspace

import (
	"context"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// THE DEPLOY MEDIATOR (repoconfig.v1.LifecycleConfig.deploy_checks; owner
// ruling, 2026-11-17). A repository whose queue checks its deploys keeps ONE
// standing workspace that checks every deploy, fixes what it dispatches,
// and settles an escalation with the user. It is made with no initial
// prompt, so no session comes up at the create, and recorded as its queue's
// standing mediator before its effort is set (SetDeployMediatorEffort, which
// starts a sessionless workspace's shim), so its session always comes up
// under the mediator's landing (sessionProfile).

// opDeployChecks is the operation the deploy mediator's verbs are recorded
// under.
const opDeployChecks = "daemon.workspace.deploy_checks"

// The mediator's model and effort (owner ruling, 2026-11-17: "an opus
// low-effort agent").
const (
	mediatorModel  = DefaultLaunchModel
	mediatorEffort = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW
)

// deployMediatorSpec is the create of the deploy mediator for slug NAME
// (`deploy-mediator`) of the repository at REPODIR: shown, since it stands in
// its repository's section for good (owner ruling, 2026-11-19), on the
// mediator's model, with no initial prompt. Its name and branch lie in the
// daemon's own namespace (StandingBranch), so no branch a person or an agent
// cut can ever hold them.
func deployMediatorSpec(repoDir, name string) CreateSpec {
	spec := NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	spec.RepoDir, spec.Name, spec.Model = repoDir, StandingBranch(name), mediatorModel
	spec.standing = true
	return spec
}

// CreateDeployMediator implements Verbs: makes a repository's deploy
// mediator, unprompted.
//
// IT IS MADE IN REMOTE CONTROL (owner ruling, 2026-11-16): the user settles
// an escalation from wherever they are, so its mode is recorded on before
// its first session starts, which puts the mode in force at bring-up.
func (v *verbs) CreateDeployMediator(ctx context.Context, repoDir, name string) (wsm.Workspace, error) {
	spec := deployMediatorSpec(repoDir, name)
	fields := dlog.Context{"repo": spec.RepoDir, "name": spec.Name, "model": spec.Model}
	made, err := v.Create(ctx, spec)
	if err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the deploy mediator could not be made", dlog.WithCause(fields, err))
		return wsm.Workspace{}, fmt.Errorf("make the deploy mediator %q: %w", spec.Name, err)
	}
	if _, err := v.deps.DB.SetRemoteControlMode(ctx, made.ID, wsm.RemoteControlOn); err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the deploy mediator was made but its remote control could not be turned on", dlog.WithCause(withWorkspace(fields, made.ID), err))
		return wsm.Workspace{}, fmt.Errorf("turn remote control on for the deploy mediator %q: %w", name, err)
	}
	v.deps.Log.Global().Info(opDeployChecks, "made the deploy mediator", withWorkspace(fields, made.ID))
	return made, nil
}

// SetDeployMediatorEffort implements Verbs: sets WS to the mediator's
// effort. A sessionless workspace's shim starts for it, so its standing is
// recorded before.
func (v *verbs) SetDeployMediatorEffort(ctx context.Context, ws wsm.WorkspaceID) error {
	fields := dlog.Context{"workspace": string(ws), "effort": mediatorEffort.String()}
	// A WORKSPACE WITH NO SESSION YET SERVES NO EFFORT SELECTOR, so its level
	// is picked rather than set: the session the brief starts comes up at the
	// pick (Fleet.reapplyEffort). Setting it through the selector refused
	// every fresh intake (2026-11-15 16:17, 0.444.0: "serves no effort level
	// for its model") and left the queue held for it.
	if _, served := v.deps.Cards.EffortLevels(ws); !served {
		v.deps.Topbar.SetPickedEffort(ws, mediatorEffort)
		v.deps.Log.Global().Info(opDeployChecks, "picked the deploy mediator's effort; its session starts at it", fields)
		return nil
	}
	if err := v.SetEffort(ctx, ws, mediatorEffort); err != nil {
		v.deps.Log.Global().Error(opDeployChecks, "the deploy mediator's effort could not be set", dlog.WithCause(fields, err))
		return fmt.Errorf("set the deploy mediator %s's effort: %w", ws, err)
	}
	v.deps.Log.Global().Info(opDeployChecks, "set the deploy mediator's effort", fields)
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

// DeployCheckContextTokens implements Verbs: the context chip's figure for
// deploy check WS, which the merge queue weighs against the bound past which
// a quiet check compacts the mediator.
func (v *verbs) DeployCheckContextTokens(ws wsm.WorkspaceID) int64 {
	return v.deps.Topbar.ContextTokens(ws)
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
	if standing, err := v.deps.DB.Workspace(ctx, ws); err != nil || !standing.DeployMediator {
		return err
	}
	return v.answerColdGateByCompaction(ctx, "AnswerDeployCheckColdGate", ws, true,
		"a cold gate stands on the standing deploy check; it is answered compact-and-resume at once")
}

// CompactRequeuedMergeColdGate implements Verbs: answers the cold gate that
// stands on WS, whose merge was just requeued, by a compaction that keeps
// the prompts held at the gate (owner request, 2026-11-20: a requeue at a
// cold gate compacts and goes in line, where it was refused). The held
// prompts are the user's to send: the merge runs first.
func (v *verbs) CompactRequeuedMergeColdGate(ctx context.Context, ws wsm.WorkspaceID) error {
	return v.answerColdGateByCompaction(ctx, "CompactRequeuedMergeColdGate", ws, false,
		"a cold gate stands on the workspace of a requeued merge; it is answered by a compaction at once")
}

// answerColdGateByCompaction answers the cold gate standing on WS, for VERB,
// by a compaction of the whole conversation with the gate's default
// summarizer (Haiku, coldCompactMenu), recording WHY. RESUME sends the
// prompts held at the gate once the session is up; otherwise they stay held.
// A workspace with no gate standing is left as it is.
func (v *verbs) answerColdGateByCompaction(ctx context.Context, verb string, ws wsm.WorkspaceID, resume bool, why string) error {
	_, log, err := v.owned(ctx, verb, ws)
	if err != nil {
		return err
	}
	if _, standing := v.deps.Cards.ColdGate(ws); !standing {
		return nil
	}
	compact := defaultSummarizerAnswer()
	answer := &agentreplv1.AnswerColdGateRequest{Choice: &agentreplv1.AnswerColdGateRequest_Compact{Compact: compact}}
	if resume {
		answer.Choice = &agentreplv1.AnswerColdGateRequest_CompactAndResume{CompactAndResume: compact}
	}
	log.Info(opDeployChecks, why, dlog.Context{"resume": resume})
	return v.AnswerColdGate(ctx, ws, answer)
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

// ResumeDeployCheck implements Verbs: shows hidden deploy check workspace WS
// again, for the brief that sends it back to work.
func (v *verbs) ResumeDeployCheck(ctx context.Context, ws wsm.WorkspaceID) error {
	_, log, err := v.owned(ctx, "ResumeDeployCheck", ws)
	if err != nil {
		return err
	}
	if err := v.showFamily(ctx, log, ws); err != nil {
		return fmt.Errorf("resume the deploy check %s: show: %w", ws, err)
	}
	log.Info(opDeployChecks, "the deploy check workspace is shown again; its brief brings its session up", nil)
	return nil
}

// showFamily shows standing deploy check WS and the workspaces hidden with
// it: a mediator an earlier build hid, with the workspaces it made (owner
// ruling, 2026-11-17: "hide its kids too").
func (v *verbs) showFamily(ctx context.Context, log dlog.Logger, ws wsm.WorkspaceID) error {
	members, err := v.deps.DB.ShowFamily(ctx, ws)
	if err != nil {
		if refusedByFamily(log, opSetHidden, err) {
			return err
		}
		log.Error(opSetHidden, "could not show the deploy check workspace with its family", dlog.Context{"cause": err.Error()})
		return err
	}
	log.Info(opSetHidden, "showed the deploy check workspace with its family", dlog.Context{"hidden": false, "family": familyIDs(members)})
	v.familyMoved(ctx, log, ws, members)
	return nil
}

// familyMoved is hiddenRecorded for ROOT and every member moved with it.
func (v *verbs) familyMoved(ctx context.Context, log dlog.Logger, root wsm.WorkspaceID, members []wsm.WorkspaceID) {
	v.hiddenRecorded(ctx, log, root, opSetHidden)
	for _, member := range members {
		v.deps.CacheLapses.VisibilityChanged(member)
	}
}

// familyIDs names a family's members for a record.
func familyIDs(members []wsm.WorkspaceID) []string {
	out := make([]string, len(members))
	for i, id := range members {
		out[i] = string(id)
	}
	return out
}

// withWorkspace is FIELDS naming workspace WS.
func withWorkspace(fields dlog.Context, ws wsm.WorkspaceID) dlog.Context {
	fields["workspace"] = string(ws)
	return fields
}

package merge

import (
	"context"
	"fmt"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE DISPATCH (owner rulings, 2026-11-14 and 2026-11-17). The intake hands
// the problems a deploy caused to the repository's STANDING remediation
// workspace (standingWorkspace), visible: the same one takes every dispatch,
// so it knows every fix it made and what it ruled out. Its fix's deploy
// closes it and the next dispatch reopens it, its worktree cut again at the
// main branch's tip. The chain's bound on remediations in a row is enforced
// here, so an intake that miscounts cannot exceed it.

// The briefs a remediation workspace is prompted with.
const (
	// BriefDeployRemediation is the standing remediation's first prompt,
	// stating its role in full.
	BriefDeployRemediation = "deploy-remediation"
	// BriefDeployRemediationRestored is every later one: only the new
	// findings.
	BriefDeployRemediationRestored = "deploy-remediation-restored"
)

// dispatch hands decision D's problems, on check M of queue Q, to a
// remediation workspace, as intake WS decided, and records the dispatch: the
// queue's hold is then owned by that workspace until its fix's deploy is
// checked in turn.
func (o *orchestrator) dispatch(ctx context.Context, q checkedQueue, m wsm.Mediation, ws ids.WorkspaceID, d Decision, fields dlog.Context) error {
	fields = withField(fields, "problems", len(d.Problems))
	if m.ChainRedeploys >= q.bound {
		o.deps.Log.Global().Warn(opMediation, "refused a dispatch past the chain's bound on remediations in a row; only an escalation is accepted", withField(withField(fields, "bound", q.bound), "in_a_row", m.ChainRedeploys))
		err := refuse(ArmRemediationBoundReached, ws, "the chain deployed %d remediations in a row, its repository's bound; escalate instead", m.ChainRedeploys)
		err.Fields = map[string]any{"remediations_before_escalation": uint32(q.bound)}
		return err
	}
	remediation, err := o.standingRemediation(ctx, q, fields)
	if err != nil {
		return err
	}
	remedy := remediation.ws
	fields["remediation"] = string(remedy)
	at := o.deps.Now()
	recorded, err := o.deps.DB.DispatchMediation(ctx, q.key, m.OwedAt, at, remedy)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the remediation workspace is ready but the dispatch could not be recorded; the queue stays held for the intake", withField(fields, "error", err.Error()))
		return err
	}
	if !recorded {
		o.deps.Log.Global().Warn(opMediation, "refused a dispatch on a check a newer deploy replaced or a decision ended meanwhile", fields)
		return refuse(ArmNoMediationOwed, ws, "the check this dispatch was for ended meanwhile")
	}
	o.observeDispatch(m, at)
	o.noteCheckWork(ctx, q.key, fields)
	// A MADE REMEDIATION'S EFFORT IS SET ONCE ITS DISPATCH IS RECORDED: it
	// starts the workspace's shim, whose landing reads the role.
	if remediation.made {
		if err := o.deps.DeployChecks.SetDeployCheckEffort(ctx, remedy, DeployCheckRemediation); err != nil {
			o.deps.Log.Global().Error(opMediation, "the remediation's effort could not be set; the dispatch stands with its workspace unprompted", withField(fields, "error", err.Error()))
			return err
		}
	}
	if err := o.briefRemediation(ctx, q.repository.Dir, remedy, remediation.briefed, m, d, fields); err != nil {
		return err
	}
	if !remediation.briefed {
		o.markBriefed(ctx, q, DeployCheckRemediation, fields)
	}
	o.ownProblems(ctx, q.key, remedy, d.Problems, nil, "dispatched to the remediation", fields)
	o.deps.Log.Global().Info(opMediation, "the intake dispatched the deploy's problems; the queue stays held for the remediation", fields)
	o.republishMediation(ctx, q.key)
	o.kick(q.key)
	return nil
}

// observeDispatch records check M's assessment ended dispatched at AT, and
// the chain's earlier remediation, if any, ended redispatched: its fix's
// deploy was still broken.
func (o *orchestrator) observeDispatch(m wsm.Mediation, at time.Time) {
	if m.PromptedAt != nil {
		o.deps.Meter.ObserveMediationSpan(MediationSpanAssessment, MediationDispatched, at.Sub(*m.PromptedAt))
	}
	if m.LastDispatchedAt != nil {
		o.deps.Meter.ObserveRemediation(RemediationRedispatched, at.Sub(*m.LastDispatchedAt))
	}
}

// standingRemediation answers the queue's standing remediation workspace,
// shown: its last finish stood it by hidden, and a reopen leaves it so
// (2026-11-16: a restored remediation worked hidden after its fix's deploy
// failed).
func (o *orchestrator) standingRemediation(ctx context.Context, q checkedQueue, fields dlog.Context) (standing, error) {
	remediation, err := o.standingWorkspace(ctx, q, DeployCheckRemediation, fields)
	if err != nil {
		return standing{}, err
	}
	if err := o.deps.DeployChecks.Resume(ctx, remediation.ws); err != nil {
		o.deps.Log.Global().Error(opMediation, "the standing remediation workspace could not be shown; the queue stays held for the intake", withField(withField(fields, "remediation", string(remediation.ws)), "error", err.Error()))
		return standing{}, fmt.Errorf("merge: show the standing remediation workspace %s: %w", remediation.ws, err)
	}
	return remediation, nil
}

// briefRemediation prompts the standing remediation workspace WS, of the
// repository at REPODIR, with decision D's problems on check M: its first
// brief states its role in full, and every one after, once BRIEFED, only the
// new findings. Its conversation is never cleared, only compacted by Haiku
// after a quiet run (compactStanding).
func (o *orchestrator) briefRemediation(ctx context.Context, repoDir string, ws ids.WorkspaceID, briefed bool, m wsm.Mediation, d Decision, fields dlog.Context) error {
	name := BriefDeployRemediation
	if briefed {
		name = BriefDeployRemediationRestored
	}
	brief, err := o.deps.Briefs(name, map[string]string{
		"repo_dir":       repoDir,
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"problems":       renderProblems(d.Problems),
	})
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the remediation's brief could not be composed; the dispatch stands with its workspace unprompted", withField(fields, "error", err.Error()))
		return err
	}
	if _, err := o.submitCheckPrompt(ctx, ws, wsm.NewTurnID(), brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		o.deps.Log.Global().Error(opMediation, "the remediation's brief could not be submitted; the dispatch stands with its workspace unprompted", withField(fields, "error", err.Error()))
		return err
	}
	return nil
}

// renderProblems is the problems as a brief quotes them: each statement,
// with its evidence verbatim beneath it.
func renderProblems(problems []DeployProblem) string {
	var b strings.Builder
	for i, p := range problems {
		if i > 0 {
			b.WriteString("\n")
		}
		b.WriteString(fmt.Sprintf("%d. %s\n", i+1, p.Statement))
		for _, e := range p.Evidence {
			b.WriteString(fmt.Sprintf("   > %s\n", e))
		}
	}
	return strings.TrimRight(b.String(), "\n")
}

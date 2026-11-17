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
// the problems a deploy caused to the repository's STANDING FIXER
// (standingFixer), in sight: the same workspace takes every dispatch and
// every escalation, so it knows every fix it made and what it ruled out. It
// stays open after its fix deploys; each brief first brings its worktree to
// the main branch's tip. The chain's bound on remediations in a row is
// enforced here, so an intake that miscounts cannot exceed it.

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
	remediation, err := o.standingFixer(ctx, q, DeployCheckRemediation, fields)
	if err != nil {
		return err
	}
	remedy := remediation.ws
	fields["remediation"] = string(remedy)
	brief, err := o.remediationBrief(q.repository.Dir, remediation.briefed, m, d)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the remediation's brief could not be composed; nothing is dispatched and the queue stays held for the intake", withField(fields, "error", err.Error()))
		return err
	}
	turn := wsm.FixerTurn{ID: wsm.NewTurnID(), Brief: brief, Origin: int32(conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION)}
	at := o.deps.Now()
	recorded, err := o.deps.DB.DispatchMediation(ctx, q.key, m.OwedAt, at, remedy, turn)
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
	// THE DISPATCH STANDS EITHER WAY: a brief that did not reach the queue is
	// submitted again by the round that waits on the fixer (watchFixer).
	if _, err := o.submitCheckPrompt(ctx, remedy, turn.ID, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		o.deps.Log.Global().Error(opMediation, "the remediation's brief could not be submitted; the round that waits on the fixer submits it again", withField(fields, "error", err.Error()))
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

// remediationBrief is the standing remediation workspace's brief, of the
// repository at REPODIR, with decision D's problems on check M: its first
// brief states its role in full, and every one after, once BRIEFED, only the
// new findings. Its conversation is never cleared, only compacted by Haiku
// after a quiet run (compactStanding).
func (o *orchestrator) remediationBrief(repoDir string, briefed bool, m wsm.Mediation, d Decision) (string, error) {
	name := BriefDeployRemediation
	if briefed {
		name = BriefDeployRemediationRestored
	}
	brief, err := o.deps.Briefs(name, map[string]string{
		"repo_dir":       repoDir,
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"problems":       renderProblems(d.Problems),
	})
	return brief, err
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

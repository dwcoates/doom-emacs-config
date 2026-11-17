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

// THE DISPATCH (owner rulings, 2026-11-14 and 2026-11-17). The deploy
// mediator dispatches the problems a deploy caused to ITSELF, and fixes them
// in the same turn: it knows every fix it made and what it ruled out. The
// dispatch shows it in its repository's section while it fixes, and its
// finish lands directly. The chain's bound on remediations in a row is
// enforced here, so a mediator that miscounts cannot exceed it.

// BriefDeployRemediationRestored is the prompt that stands for a
// dispatch's work: the mediator's turn the dispatch is recorded on is
// submitted with it again only if a restart cut that turn before it reached
// the session (watchFixer).
const BriefDeployRemediationRestored = "deploy-remediation-restored"

// dispatch records decision D's problems, on check M of queue Q, dispatched
// to the mediator WS that decided them, which fixes them in the turn it
// decided in: the queue's hold is then owned by WS until its fix's deploy is
// checked in turn. Its turn is the check's fixer turn (watchFixer): a turn
// that ends with no finish of WS's under way escalates the check.
func (o *orchestrator) dispatch(ctx context.Context, q checkedQueue, m wsm.Mediation, ws ids.WorkspaceID, d Decision, fields dlog.Context) error {
	fields = withField(withField(fields, "problems", len(d.Problems)), "remediation", string(ws))
	if m.ChainRedeploys >= q.bound {
		o.deps.Log.Global().Warn(opMediation, "refused a dispatch past the chain's bound on remediations in a row; only an escalation is accepted", withField(withField(fields, "bound", q.bound), "in_a_row", m.ChainRedeploys))
		err := refuse(ArmRemediationBoundReached, ws, "the chain deployed %d remediations in a row, its repository's bound; escalate instead", m.ChainRedeploys)
		err.Fields = map[string]any{"remediations_before_escalation": uint32(q.bound)}
		return err
	}
	brief, err := o.remediationBrief(q.repository.Dir, m, d)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the dispatch's brief could not be composed; nothing is dispatched and the queue stays held for the mediator", withField(fields, "error", err.Error()))
		return err
	}
	turn := wsm.FixerTurn{ID: m.Turn, Brief: brief, Origin: int32(conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION)}
	at := o.deps.Now()
	recorded, err := o.deps.DB.DispatchMediation(ctx, q.key, m.OwedAt, at, ws, turn)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the dispatch could not be recorded; the queue stays held for the mediator", withField(fields, "error", err.Error()))
		return err
	}
	if !recorded {
		o.deps.Log.Global().Warn(opMediation, "refused a dispatch on a check a newer deploy replaced or a decision ended meanwhile", fields)
		return refuse(ArmNoMediationOwed, ws, "the check this dispatch was for ended meanwhile")
	}
	o.observeDispatch(m, at)
	o.noteCheckWork(ctx, q.key, fields)
	// THE MEDIATOR IS IN SIGHT WHILE IT FIXES (owner ruling, 2026-11-17).
	if err := o.deps.DeployChecks.Resume(ctx, ws); err != nil {
		o.deps.Log.Global().Error(opMediation, "the dispatch is recorded but the mediator could not be shown while it fixes; the next pass shows it", withField(fields, "error", err.Error()))
	}
	o.ownProblems(ctx, q.key, ws, d.Problems, nil, "dispatched to the mediator", fields)
	o.deps.Log.Global().Info(opMediation, "the mediator dispatched the deploy's problems to itself; the queue stays held while it fixes them", fields)
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

// remediationBrief is the prompt that stands for decision D's work on check
// M of the repository at REPODIR (BriefDeployRemediationRestored).
func (o *orchestrator) remediationBrief(repoDir string, m wsm.Mediation, d Decision) (string, error) {
	return o.deps.Briefs(BriefDeployRemediationRestored, map[string]string{
		"repo_dir":       repoDir,
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"problems":       renderProblems(d.Problems),
	})
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

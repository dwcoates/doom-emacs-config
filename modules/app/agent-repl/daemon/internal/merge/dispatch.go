package merge

import (
	"context"
	"fmt"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessioncommand"
	"agent-repld/internal/wsm"
)

// THE DISPATCH (owner rulings, 2026-11-14). The intake hands the problems a
// deploy caused to a remediation workspace, visible, on opus at medium
// effort: a FRESH one when the problems are unrelated to any earlier fix of
// the chain, or the chain's last one RESTORED when they are in or caused by
// its own fix, since it knows what it changed and what it ruled out. The
// chain's bound on remediations in a row is enforced here, so an intake that
// miscounts cannot exceed it.

// The briefs a remediation workspace is prompted with.
const (
	// BriefDeployRemediation is a fresh remediation workspace's first prompt.
	BriefDeployRemediation = "deploy-remediation"
	// BriefDeployRemediationRestored is a restored one's: only the new
	// findings.
	BriefDeployRemediationRestored = "deploy-remediation-restored"
)

// restoredCompactPercent is the context use at which a restored remediation
// workspace compacts before its brief: past half its window, the earlier
// fix's research would crowd out the new findings' (owner plan, 2026-11-14:
// "it compacts first when its context is large").
const restoredCompactPercent = 50

// dispatch hands decision D's problems, on check M of queue Q, to a
// remediation workspace, as intake WS decided, and records the dispatch: the
// queue's hold is then owned by that workspace until its fix's deploy is
// checked in turn.
func (o *orchestrator) dispatch(ctx context.Context, q checkedQueue, m wsm.Mediation, ws ids.WorkspaceID, d Decision, fields dlog.Context) error {
	fields = withField(withField(fields, "problems", len(d.Problems)), "restore", d.Restore)
	if m.ChainRedeploys >= q.bound {
		o.deps.Log.Global().Warn(opMediation, "refused a dispatch past the chain's bound on remediations in a row; only an escalation is accepted", withField(withField(fields, "bound", q.bound), "in_a_row", m.ChainRedeploys))
		err := refuse(ArmRemediationBoundReached, ws, "the chain deployed %d remediations in a row, its repository's bound; escalate instead", m.ChainRedeploys)
		err.Fields = map[string]any{"remediations_before_escalation": uint32(q.bound)}
		return err
	}
	if d.Restore && m.Remediation == "" {
		o.deps.Log.Global().Warn(opMediation, "refused a dispatch restoring the chain's last remediation; the chain has none", fields)
		return refuse(ArmNoRemediationToRestore, ws, "the chain has no remediation workspace yet; dispatch it fresh")
	}
	remedy, err := o.remediationFor(ctx, q, m, d.Restore, fields)
	if err != nil {
		return err
	}
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
	// A FRESH REMEDIATION'S EFFORT IS SET ONCE ITS DISPATCH IS RECORDED: it
	// starts the workspace's shim.
	if !d.Restore {
		if err := o.deps.DeployChecks.SetDeployCheckEffort(ctx, remedy, DeployCheckRemediation); err != nil {
			o.deps.Log.Global().Error(opMediation, "the remediation's effort could not be set; the dispatch stands with its workspace unprompted", withField(fields, "error", err.Error()))
			return err
		}
	}
	if err := o.briefRemediation(ctx, q.repository.Dir, remedy, m, d, fields); err != nil {
		return err
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

// remediationFor answers the remediation workspace a dispatch hands its
// problems to: the chain's last one reopened, when RESTORE (its worktree,
// removed by its landing, is cut again on its branch at the main branch's
// tip, which holds its landed fix), or a fresh one made.
func (o *orchestrator) remediationFor(ctx context.Context, q checkedQueue, m wsm.Mediation, restore bool, fields dlog.Context) (ids.WorkspaceID, error) {
	if restore {
		if err := o.deps.DeployChecks.Reopen(ctx, m.Remediation); err != nil {
			o.deps.Log.Global().Error(opMediation, "the chain's last remediation workspace could not be reopened; the queue stays held for the intake", withField(withField(fields, "remediation", string(m.Remediation)), "error", err.Error()))
			return "", fmt.Errorf("merge: reopen the remediation workspace %s: %w", m.Remediation, err)
		}
		// A RESTORED REMEDIATION WORKS IN SIGHT, as a fresh one does: its
		// finish stood it by hidden, and a reopen leaves it so (2026-11-16:
		// a restored remediation worked hidden after its fix's deploy failed).
		if err := o.deps.DeployChecks.Resume(ctx, m.Remediation); err != nil {
			o.deps.Log.Global().Error(opMediation, "the restored remediation workspace could not be shown; the queue stays held for the intake", withField(withField(fields, "remediation", string(m.Remediation)), "error", err.Error()))
			return "", fmt.Errorf("merge: show the restored remediation workspace %s: %w", m.Remediation, err)
		}
		return m.Remediation, nil
	}
	name := "deploy-remediation-" + o.deps.Now().UTC().Format("20060102-150405")
	made, err := o.deps.DeployChecks.CreateDeployRemediation(ctx, q.repository.Dir, name)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the remediation workspace could not be made; the queue stays held for the intake", withField(withField(fields, "name", name), "error", err.Error()))
		return "", fmt.Errorf("merge: make the remediation workspace %s: %w", name, err)
	}
	return made.ID, nil
}

// briefRemediation prompts remediation workspace WS, of the repository at
// REPODIR, with decision D's problems on check M: a fresh workspace's brief states them in full, and a
// restored one's only the new findings, behind a compaction when its context
// is large.
func (o *orchestrator) briefRemediation(ctx context.Context, repoDir string, ws ids.WorkspaceID, m wsm.Mediation, d Decision, fields dlog.Context) error {
	name := BriefDeployRemediation
	if d.Restore {
		name = BriefDeployRemediationRestored
		if err := o.compactLarge(ctx, ws, fields); err != nil {
			return err
		}
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

// compactLarge compacts restored workspace WS's context first when its last
// reading is past restoredCompactPercent.
func (o *orchestrator) compactLarge(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) error {
	usage, found, err := o.deps.DB.WorkspaceContextUsage(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the restored remediation's context use; it is briefed uncompacted", withField(fields, "error", err.Error()))
		return err
	}
	if !found || usage.Usage == nil || usage.Usage.GetPercentage() < restoredCompactPercent {
		return nil
	}
	compact := sessioncommand.Literal(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	if _, err := o.submitCheckPrompt(ctx, ws, wsm.NewTurnID(), compact, conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT); err != nil {
		o.deps.Log.Global().Error(opMediation, "the restored remediation's large context could not be compacted before its brief", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Log.Global().Info(opMediation, "the restored remediation's context is large; it compacts before its brief", withField(fields, "context_percent", usage.Usage.GetPercentage()))
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

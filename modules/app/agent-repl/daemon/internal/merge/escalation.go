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

// THE ESCALATION (owner ruling, 2026-11-15). The intake never talks to the
// user. Every escalation of a check -- the intake's own, and the daemon's
// for it (a turn that decided nothing, a remediation given up or closed) --
// makes a NEW VISIBLE workspace, on the remediation's model and effort, whose
// prompt is the summary and the findings with their evidence, and posts a
// desktop notification whose click opens that workspace.

// BriefDeployEscalation is an escalation workspace's first prompt.
const BriefDeployEscalation = "deploy-escalation"

// escalate puts check M of queue Q before the user as
// SUMMARY with PROBLEMS, answering the escalation workspace. Each failure is
// recorded at ERROR and answered: the check is released already, so the
// escalation is lost to the user and the record is its account.
func (o *orchestrator) escalate(ctx context.Context, q checkedQueue, m wsm.Mediation, summary string, problems []DeployProblem, window wsm.CheckWindow, fields dlog.Context) (ids.WorkspaceID, error) {
	repoDir := q.repository.Dir
	name := "deploy-escalation-" + o.deps.Now().UTC().Format("20060102-150405")
	fields = withField(withField(fields, "escalation", name), "summary", summary)
	failed := func(message string, err error) (ids.WorkspaceID, error) {
		o.deps.Log.Global().Error(opMediation, message, withField(fields, "error", err.Error()))
		return "", err
	}
	made, err := o.deps.DeployChecks.CreateDeployEscalation(ctx, repoDir, name)
	if err != nil {
		return failed("the escalation workspace could not be made; the user is not told of the escalated deploy", err)
	}
	fields["workspace"] = string(made.ID)
	// ITS FINISH LANDS DIRECTLY, as a remediation's does (FinishRemediation):
	// recorded before it is briefed, so the role is the daemon's, never the
	// agent's to choose (owner ruling, 2026-11-15).
	if err := o.deps.DB.RecordDeployEscalation(ctx, made.ID, q.key, o.deps.Now()); err != nil {
		return failed("the escalation workspace could not be recorded as landing directly; it is not briefed and the user is not told", err)
	}
	if err := o.deps.DeployChecks.SetDeployCheckEffort(ctx, made.ID, DeployCheckEscalation); err != nil {
		return failed("the escalation workspace's effort could not be set; it is not briefed and the user is not told", err)
	}
	brief, err := o.deps.Briefs(BriefDeployEscalation, map[string]string{
		"repo_dir":       repoDir,
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"summary":        summary,
		"problems":       renderEscalationProblems(problems),
		"old_problems":   renderOldProblems(window),
	})
	if err != nil {
		return failed("the escalation workspace's brief could not be composed; it is not briefed and the user is not told", err)
	}
	if _, err := o.submitCheckPrompt(ctx, made.ID, wsm.NewTurnID(), brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		return failed("the escalation workspace's brief could not be submitted; the user is not told", err)
	}
	o.deps.Banners.Mediated(made.ID, summary)
	o.deps.Log.Global().Info(opMediation, "the deploy is put before the user in a new escalation workspace", withField(fields, "problems", len(problems)))
	return made.ID, nil
}

// renderOldProblems is every group of WINDOW that is not NEW, as the
// escalation's brief lists them beside the findings (owner ruling,
// 2026-11-16): an old warning or error still recurring is the user's to see
// whenever a deploy is put before them, whoever owns it.
func renderOldProblems(window wsm.CheckWindow) string {
	var b strings.Builder
	for _, g := range window.Groups {
		var standing string
		switch g.Class {
		case wsm.WindowNew:
			continue
		case wsm.WindowSeenBefore:
			standing = "seen before, owned by nobody; this workspace owns it now"
		case wsm.WindowRecurred:
			standing = "came back after its fix deployed; this workspace owns it now"
		case wsm.WindowOwned:
			standing = "owned by another workspace still at work on it"
		default:
			panic(fmt.Sprintf("merge: a window group with no class: %+v", g))
		}
		b.WriteString(fmt.Sprintf("- %s: %s (%s)\n", g.Operation, g.Message, standing))
	}
	if b.Len() == 0 {
		return "No older warning or error is still recurring."
	}
	return "Older warnings and errors still recurring in the window:\n\n" + b.String()
}

// renderEscalationProblems is the findings as the escalation's brief quotes
// them, or a line saying the summary is all there is.
func renderEscalationProblems(problems []DeployProblem) string {
	if len(problems) == 0 {
		return "The escalation states no separate findings; the summary above is all of it."
	}
	return fmt.Sprintf("The findings, each with its records verbatim:\n\n%s", renderProblems(problems))
}

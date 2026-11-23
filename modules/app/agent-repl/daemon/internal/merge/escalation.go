package merge

import (
	"context"
	"fmt"
	"strings"
	"time"

	"agentrepl/logging/logsite"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE ESCALATION (owner rulings, 2026-11-15 and 2026-11-17). Every
// escalation of a check -- the mediator's own, and the daemon's for it (a
// turn that decided nothing, a fix given up or closed) -- goes to the
// repository's deploy mediator, shown, in remote control, and posts a
// desktop notification whose click opens it. One escalated by the mediator
// in its own turn is told nothing it said; any other is sent the summary
// and the findings with their evidence. The mediator takes every
// escalation, so it knows each one it settled before.

// BriefDeployEscalation is the brief that tells the mediator of an
// escalation the daemon made for it.
const BriefDeployEscalation = "deploy-escalation"

// BriefDeployCheckNotRun is the brief that tells the mediator a check could
// not run (roundFailed): what failed and why, and that nothing has looked at
// the deploy. The escalation's own brief read as a finding about the deploy
// (owner request, 2026-11-20).
const BriefDeployCheckNotRun = "deploy-check-not-run"

// escalate puts check M of queue Q before the user as SUMMARY with
// PROBLEMS, in the queue's deploy mediator, answering it. SAID is whether
// the mediator escalated in its own turn. Each failure is recorded at ERROR
// and answered.
//
// IT NEVER NOTIFIES THE USER: the user was notified as the escalation was
// recorded (holdBrief, notifyHeld), whether or not this telling reaches the mediator, so
// a telling the next pass makes again (remakeHeldEscalation) notifies no one
// a second time.
func (o *orchestrator) escalate(ctx context.Context, q checkedQueue, m wsm.Mediation, told escalationBrief, window wsm.CheckWindow, said bool, fields dlog.Context) (ids.WorkspaceID, error) {
	repoDir := q.repository.Dir
	summary, problems := told.Summary, told.Problems
	fields = withField(fields, "summary", summary)
	// A TELLING THE DAEMON'S EXIT OR A HANDOVER CUT is no failure: the
	// next daemon's pass tells it again (mediationFailed records it at INFO).
	failed := func(message logsite.Message, err error) (ids.WorkspaceID, error) {
		o.mediationFailed(ctx, fields, message, err)
		return "", err
	}
	// THE MEDIATOR THAT ESCALATED IN ITS OWN TURN IS ONLY SHOWN, never
	// readied as for a brief: it is mid-turn, so its worktree holds its work
	// and its effort is set, and setting the effort waits on the very turn
	// that waits on this escalation (2026-11-18, a hang until the call was
	// cancelled).
	//
	// A CHECK THAT COULD NOT RUN IS TOLD TO THE MEDIATOR AS IT STANDS, shown
	// and never readied: readying it (its worktree refreshed, its effort set)
	// may be the very step whose failure it is told of, and a telling that
	// readied it again would fail the same way at every pass, leaving the
	// mediator told nothing (owner ruling, 2026-11-21: the mediator receives
	// every failure of a check's round). Its brief is queued behind any turn
	// it runs, so it is told whether or not it was told of the check before.
	var escalation standing
	var err error
	if said || told.NotRun != nil {
		escalation, err = o.showMediator(ctx, q, fields)
	} else {
		escalation, err = o.readyMediator(ctx, q, fields)
	}
	if err != nil {
		return failed(logsite.Msg("the deploy mediator could not be readied for the escalation; it is not told, and the next pass tells it"), err)
	}
	fields["workspace"] = string(escalation.ws)
	// THE MEDIATOR THAT ESCALATED IN ITS OWN TURN TELLS THE USER ITSELF, as
	// its rules say: it is shown, with no brief.
	if said {
		o.deps.Log.Global().Info(opMediation, "the mediator escalated in its own turn; it is shown and the user notified", withField(fields, "problems", len(problems)))
		return escalation.ws, nil
	}
	brief, err := o.escalationBriefText(repoDir, m, told, window)
	if err != nil {
		return failed(logsite.Msg("the escalation's brief could not be composed; the mediator is not briefed, and the next pass tells it"), err)
	}
	if _, err := o.submitCheckPrompt(ctx, escalation.ws, wsm.NewTurnID(), brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		return failed(logsite.Msg("the escalation's brief could not be submitted to the mediator; the next pass tells it"), err)
	}
	o.deps.Log.Global().Info(opMediation, "the deploy is put before the user in the deploy mediator", withField(fields, "problems", len(problems)))
	return escalation.ws, nil
}

// escalationBriefText is the brief that tells the mediator of escalated
// check M of the repository at REPODIR: what failed, for a check that could
// not run (BriefDeployCheckNotRun), and otherwise the summary and findings
// it was escalated with beside WINDOW's older groups (BriefDeployEscalation).
func (o *orchestrator) escalationBriefText(repoDir string, m wsm.Mediation, told escalationBrief, window wsm.CheckWindow) (string, error) {
	started := m.OwedAt.UTC().Format(time.RFC3339)
	if told.NotRun != nil {
		return o.deps.Briefs(BriefDeployCheckNotRun, map[string]string{
			"repo_dir":       repoDir,
			"deploy_started": started,
			"told_standing":  notRunStanding(told.NotRun.Told),
			"what":           told.NotRun.What,
			"cause":          told.NotRun.Cause,
		})
	}
	return o.deps.Briefs(BriefDeployEscalation, map[string]string{
		"repo_dir":       repoDir,
		"deploy_started": started,
		"summary":        told.Summary,
		"problems":       renderEscalationProblems(told.Problems),
		"old_problems":   renderOldProblems(window),
	})
}

// notRunStanding is how the brief of a check that could not run says where
// the mediator stood when it failed (BriefDeployCheckNotRun's
// {{told_standing}}): TOLD when it was told of the check before the failure
// (a lost await of its turn, a failed fixer watch), so it may have looked at
// the deploy already, and otherwise before anything looked at it.
func notRunStanding(told bool) string {
	if told {
		return "after you were told of it, so agent-repl no longer follows what you do with the check: decide it yourself as below."
	}
	return "before you were told of it, so nothing has looked at that deploy yet."
}

// renderOldProblems is every group of WINDOW that is not NEW, as the
// escalation's brief lists them beside the findings (owner ruling,
// 2026-11-16): an old warning or error still recurring is the user's to see
// whenever a deploy is put before them.
func renderOldProblems(window wsm.CheckWindow) string {
	var b strings.Builder
	for _, g := range window.Groups {
		var standing string
		switch g.Class {
		case wsm.WindowNew:
			continue
		case wsm.WindowSeenBefore:
			standing = "seen before, never taken on; taken on with this escalation"
		case wsm.WindowRecurred:
			standing = "came back after its fix deployed; taken on again with this escalation"
		case wsm.WindowOwned:
			standing = "taken on before, its fix still under way"
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

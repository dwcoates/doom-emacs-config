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

// BriefDeployEscalationUpdate is the prompt an open escalation workspace is
// sent when a later deploy's check finds again a problem it owns.
const BriefDeployEscalationUpdate = "deploy-escalation-update"

// AN OPEN ESCALATION KEEPS ITS PROBLEMS (owner ruling, 2026-11-16). A
// problem an escalation owns that a later deploy's check finds again goes
// back to that escalation as an update, never to a new escalation: one the
// intake escalates again, or one recurring after the escalation's fix
// deployed, as new findings; one still owned, as a confirmation that it
// persists. Before it, the Location Services grant was escalated anew on
// three deploys in one day. An escalation the user closed takes nothing
// back: a problem of its that returns is a new escalation.

// escalationUpdate is what one open escalation is told of one check.
type escalationUpdate struct {
	// problems are the intake's findings naming a group the escalation
	// owns, and recurred the window's groups it owns that no one is at work
	// on: both are new to it, and it owns them again.
	problems []DeployProblem
	recurred []wsm.ProblemKey
	// persisting are the window's groups it owns and is still at work on.
	persisting []wsm.ProblemKey
}

// news reports whether the update tells the escalation anything new, which
// the user is notified of; a confirmation alone is not.
func (u escalationUpdate) news() bool { return len(u.problems) > 0 || len(u.recurred) > 0 }

// escalationRoutes is one check's problems sorted between the open
// escalations that own them and a new escalation.
type escalationRoutes struct {
	updates map[ids.WorkspaceID]*escalationUpdate
	order   []ids.WorkspaceID
	// fresh and freshKeys are what a new escalation would take.
	fresh     []DeployProblem
	freshKeys []wsm.ProblemKey
}

func (r *escalationRoutes) to(ws ids.WorkspaceID) *escalationUpdate {
	u, ok := r.updates[ws]
	if !ok {
		u = &escalationUpdate{}
		r.updates[ws] = u
		r.order = append(r.order, ws)
	}
	return u
}

// openEscalationOwners answers, for REPO, the open escalation workspace
// owning each problem the deploy checks keep that one owns.
func (o *orchestrator) openEscalationOwners(ctx context.Context, repo wsm.RepoKey) (map[wsm.ProblemKey]ids.WorkspaceID, error) {
	problems, err := o.deps.DB.DeployProblems(ctx, repo)
	if err != nil {
		return nil, err
	}
	open := map[ids.WorkspaceID]bool{}
	out := map[wsm.ProblemKey]ids.WorkspaceID{}
	for _, p := range problems {
		escalation, known := open[p.Owner]
		if !known {
			_, isEscalation, err := o.deps.DB.DeployEscalationOf(ctx, p.Owner)
			if err != nil {
				return nil, err
			}
			if isEscalation {
				record, err := o.deps.DB.Workspace(ctx, p.Owner)
				if err != nil {
					return nil, err
				}
				escalation = !record.Closed
			}
			open[p.Owner] = escalation
		}
		if escalation {
			out[p.Key] = p.Owner
		}
	}
	return out, nil
}

// routeEscalation sorts check WINDOW's groups, and when ESCALATING the
// intake's PROBLEMS and every group no one is at work on, between the open
// escalations that own them (OWNERS) and a new escalation. A problem goes to
// the escalation owning the first group its evidence names.
func routeEscalation(owners map[wsm.ProblemKey]ids.WorkspaceID, window wsm.CheckWindow, problems []DeployProblem, escalating bool) escalationRoutes {
	r := escalationRoutes{updates: map[ids.WorkspaceID]*escalationUpdate{}}
	for _, g := range window.Groups {
		k := wsm.ProblemKey{Operation: g.Operation, Message: g.Message}
		owner, owned := owners[k]
		switch {
		case g.Class == wsm.WindowOwned && owned:
			r.to(owner).persisting = append(r.to(owner).persisting, k)
		case g.Class == wsm.WindowOwned || !escalating:
		case owned:
			r.to(owner).recurred = append(r.to(owner).recurred, k)
		default:
			r.freshKeys = append(r.freshKeys, k)
		}
	}
	if !escalating {
		return r
	}
	for _, p := range problems {
		var owner ids.WorkspaceID
		owned := false
		for _, k := range problemKeys([]DeployProblem{p}) {
			if owner, owned = owners[k]; owned {
				break
			}
		}
		if owned {
			r.to(owner).problems = append(r.to(owner).problems, p)
			continue
		}
		r.fresh = append(r.fresh, p)
	}
	return r
}

// tellEscalations sends each open escalation that owns a group of check M's
// WINDOW its update (routeEscalation), answering the problems a new
// escalation takes and whether the routes could be read. An update that
// cannot be sent is recorded at ERROR and answered: its groups stay with
// their owner, untold, for the next check.
func (o *orchestrator) tellEscalations(ctx context.Context, q checkedQueue, m wsm.Mediation, summary string, window wsm.CheckWindow, problems []DeployProblem, escalating bool, fields dlog.Context) (escalationRoutes, bool) {
	owners, err := o.openEscalationOwners(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read which open escalations own the deploy's problems; none is told, and nothing is escalated anew", withField(fields, "error", err.Error()))
		return escalationRoutes{}, false
	}
	routes := routeEscalation(owners, window, problems, escalating)
	for _, ws := range routes.order {
		o.updateEscalation(ctx, q, m, ws, summary, *routes.updates[ws], fields)
	}
	return routes, true
}

// updateEscalation sends open escalation WS update U of check M, which owns
// U's new groups again.
func (o *orchestrator) updateEscalation(ctx context.Context, q checkedQueue, m wsm.Mediation, ws ids.WorkspaceID, summary string, u escalationUpdate, fields dlog.Context) {
	fields = withField(withField(withField(fields, "escalation", string(ws)), "problems", len(u.problems)+len(u.recurred)), "persisting", len(u.persisting))
	brief, err := o.deps.Briefs(BriefDeployEscalationUpdate, map[string]string{
		"repo_dir":       q.repository.Dir,
		"deploy_started": m.OwedAt.UTC().Format(time.RFC3339),
		"summary":        summary,
		"problems":       renderUpdateProblems(u.problems),
		"recurred":       renderUpdateGroups("Recurring after this workspace's fix deployed, so this workspace owns them again:", u.recurred),
		"persisting":     renderUpdateGroups("Still recurring while this workspace settles them:", u.persisting),
	})
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "the escalation's update could not be composed; it is not told that its problems persist", withField(fields, "error", err.Error()))
		return
	}
	if _, err := o.submitCheckPrompt(ctx, ws, wsm.NewTurnID(), brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION); err != nil {
		o.deps.Log.Global().Error(opMediation, "the escalation's update could not be submitted; it is not told that its problems persist", withField(fields, "error", err.Error()))
		return
	}
	if u.news() {
		o.ownProblems(ctx, q.key, ws, u.problems, u.recurred, "found again by a later deploy's check", fields)
		o.deps.Banners.Mediated(ws, summary)
		o.deps.Log.Global().Info(opMediation, "a later deploy's problems went to the open escalation that owns them, not to a new one", fields)
		return
	}
	o.deps.Log.Global().Info(opMediation, "told the open escalation that the problems it owns persist", fields)
}

// renderUpdateProblems is an update's new findings, or a line saying the
// intake stated none.
func renderUpdateProblems(problems []DeployProblem) string {
	if len(problems) == 0 {
		return "The intake stated no new findings for this workspace."
	}
	return fmt.Sprintf("The intake's new findings, each with its records verbatim:\n\n%s", renderProblems(problems))
}

// renderUpdateGroups is HEADING over KEYS, or nothing when there are none.
func renderUpdateGroups(heading string, keys []wsm.ProblemKey) string {
	if len(keys) == 0 {
		return ""
	}
	var b strings.Builder
	b.WriteString(heading + "\n\n")
	for _, k := range keys {
		b.WriteString(fmt.Sprintf("- %s: %s\n", k.Operation, k.Message))
	}
	return b.String()
}

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

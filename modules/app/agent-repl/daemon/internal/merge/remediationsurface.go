package merge

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// THE MEDIATOR'S FINISH IS DRAWN AS A MERGE (owner ruling, 2026-11-17). A
// deploy mediator whose direct finish gates, lands or deploys is drawn at
// that work, never idle: its finish is surfaced as merge facts, "testing fix"
// while it gates and lands, "deploying fix" while it deploys, and nothing
// once it is sent back to fixing, deployed or ended, when the mediator's own
// session arms stand again.

// finishSurface is a finish under way as the surfaces draw it: its step, and
// the queue it lands in, so a queue's restatement touches only its own.
type finishSurface struct {
	step footer.MergeStep
	repo string
}

// finishStep is the merging step finish F under way is drawn at, and false
// for a finish at no step under way.
func finishStep(f wsm.RemediationFinish) (footer.MergeStep, bool) {
	switch f.Step {
	case wsm.RemediationGating, wsm.RemediationLanding:
		return footer.StepTestingFix, true
	case wsm.RemediationDeploying:
		return footer.StepDeployingFix, true
	}
	return "", false
}

// surfaceFinish restates on the surfaces the step of every finish of queue
// REPO, read from the store: each finish under way is drawn at its step, and
// each workspace drawn finishing before whose finish is no longer under way
// is drawn as its session stands. Every step change of a finish republishes
// its queue (republishMediation), which calls this, so the drawing follows
// the store. A store that cannot be read is recorded at ERROR, and the
// surfaces keep what they drew.
func (o *orchestrator) surfaceFinish(ctx context.Context, repo wsm.RepoKey) {
	finishes, err := o.deps.DB.RemediationFinishes(ctx)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the remediation finishes to draw them; the surfaces keep what they drew", dlog.Context{"repo": string(repo), "error": err.Error()})
		return
	}
	queue := canonicalKey(repo)
	under := map[ids.WorkspaceID]finishSurface{}
	for _, f := range finishes {
		if !sameQueue(f.Repo, repo) {
			continue
		}
		if step, ok := finishStep(f); ok {
			under[f.Workspace] = finishSurface{step: step, repo: queue}
		}
	}
	var changed []ids.WorkspaceID
	o.mu.Lock()
	for ws, drawn := range o.finishing {
		if _, still := under[ws]; !still && drawn.repo == queue {
			delete(o.finishing, ws)
			changed = append(changed, ws)
		}
	}
	for ws, now := range under {
		if o.finishing[ws] != now {
			o.finishing[ws] = now
			changed = append(changed, ws)
		}
	}
	o.mu.Unlock()
	for _, ws := range changed {
		o.surface(ws)
	}
}

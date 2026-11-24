package merge

import (
	"context"
	"fmt"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// THE MERGE'S DEPLOYING STAGE SPANS ITS DEPLOY'S CHECK (owner request,
// 2026-11-24). A landing whose deploy a deploy check is owed of leaves its
// run as its deploy starts (selfReload), but the queue shows it as deploying,
// at its front, from its deploying step's start until the check ends: the
// deploy mediator's approval, an escalation, a dispatch of what the deploy
// broke, or a release without the mediator's word (a deploy that changed
// nothing, a quiet window, a repository that checks no deploys now). Its
// record is durable (wsm.DeployingMerge), since the deploy replaces the
// daemon that wrote it.
//
// THIS IS DRAWING AND MEASURING ONLY. The queue's hold on the check
// (deployHold) decides when its head advances, and nothing here reads or
// moves it.
//
// THE STEP IS MEASURED ONCE (agentrepl_merge_step_seconds{step="deploying"}):
// at the check's end when the deploy is checked (endDeployingMerge), at the
// run's teardown otherwise (settleDeploying).

// deployingSpan is a run's stretch on its deploying step: when it began and
// when the run left it. Zero while the run never stood on the step.
type deployingSpan struct {
	began, left time.Time
}

// observeDeploying measures SPAN as the deploying step's, when the run stood
// on it.
func (o *orchestrator) observeDeploying(span deployingSpan) {
	if span.began.IsZero() {
		return
	}
	o.deps.Meter.ObserveMergeStep(string(footer.StepDeploying), span.left.Sub(span.began))
}

// settleDeploying ends the run's deploying step as its run leaves the queue:
// a landing whose deploy its queue checks is recorded as still deploying
// until the check ends, and any other run's step is measured now. A record
// that cannot be made is recorded at ERROR, and the step is measured now: the
// queue stops drawing the merge as its run ends, as before.
func (r *run) settleDeploying(ctx context.Context, out outcome) {
	const op = "daemon.merge.deploying"
	r.mu.Lock()
	span := r.deploying
	r.deploying = deployingSpan{}
	r.mu.Unlock()
	if r.landingDeploy(ctx, out) != LandingDeployOwed || r.target.cfg.GetDeployChecks() == nil {
		r.o.observeDeploying(span)
		return
	}
	fields := dlog.Context{"workspace": string(r.ws), "repo": string(r.repo), "lease": string(r.lease.ID)}
	m, err := r.deployingMerge(ctx, span)
	if err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not read the landed merge to draw it deploying until its check ends; its deploying step is measured to its run's end", withField(fields, "error", err.Error()))
		r.o.observeDeploying(span)
		return
	}
	replaced, err := r.o.deps.DB.RecordDeployingMerge(ctx, m)
	if err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not record the landed merge as deploying until its check ends; its deploying step is measured to its run's end", withField(fields, "error", err.Error()))
		r.o.observeDeploying(span)
		return
	}
	if replaced != nil {
		r.o.closeDeploying(*replaced, MediationSuperseded)
	}
	r.o.log(ctx, r.ws).Info(op, "the landed merge is drawn deploying until its deploy's check ends", withField(withField(fields, "timed", m.Timed), "deploying_since", m.DeployingSince.UTC().Format(time.RFC3339Nano)))
}

// deployingMerge is the run's landing as its queue's merge still deploying:
// SPAN's start, or its teardown, untimed, when the run never saw the step
// begin. Its queue entry still stands (the teardown reads it before the run
// leaves its line), and an admitted head's clock is active; one that is not
// is a broken invariant, answered.
func (r *run) deployingMerge(ctx context.Context, span deployingSpan) (wsm.DeployingMerge, error) {
	entry, err := r.o.entryOf(ctx, r.repo, r.ws)
	if err != nil {
		return wsm.DeployingMerge{}, err
	}
	if entry.Clock.Phase != wsm.MergeClockActive {
		return wsm.DeployingMerge{}, fmt.Errorf("merge: the landing merge %s in %s stands on a %s clock, not an active one", r.ws, r.repo, entry.Clock.Phase)
	}
	c, err := r.o.creditOf(ctx, r.ws)
	if err != nil {
		return wsm.DeployingMerge{}, err
	}
	since, timed := span.began, true
	if since.IsZero() {
		since, timed = r.o.deps.Now(), false
	}
	return wsm.DeployingMerge{
		Repo: r.repo, Workspace: r.ws, Lease: r.lease.ID, Credited: c.workspace, Async: c.async, Name: c.name,
		Branch: r.subject.branch, TargetDir: r.subject.targetDir,
		ActiveSince: entry.Clock.Since, DeployingSince: since, Timed: timed,
	}, nil
}

// endDeployingMerge ends REPO's landed merge still deploying, if one stands,
// as its deploy's check ended OUTCOME: its deploying step is measured to now.
// A failure is recorded at ERROR: the merge stays drawn deploying until the
// next check's end takes it.
func (o *orchestrator) endDeployingMerge(ctx context.Context, repo wsm.RepoKey, outcome string) {
	m, err := o.deps.DB.EndDeployingMerge(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error("daemon.merge.deploying", "could not end the landed merge's deploying stage as its check ended; it is drawn deploying until the next check ends", dlog.Context{
			"repo": string(repo), "outcome": outcome, "error": err.Error()})
		return
	}
	if m != nil {
		o.closeDeploying(*m, outcome)
	}
}

// closeDeploying measures M's deploying step, when its record is timed, to
// now, its check having ended OUTCOME.
func (o *orchestrator) closeDeploying(m wsm.DeployingMerge, outcome string) {
	took := o.deps.Now().Sub(m.DeployingSince)
	if m.Timed {
		o.deps.Meter.ObserveMergeStep(string(footer.StepDeploying), took)
	}
	o.deps.Log.Global().Info("daemon.merge.deploying", "the landed merge's deploying stage ended with its deploy's check", dlog.Context{
		"repo": string(m.Repo), "workspace": string(m.Workspace), "name": m.Name, "outcome": outcome, "timed": m.Timed, "seconds": took.Seconds()})
}

// deployingMember is M as the queue's status lists it: running, on its
// deploying step.
func deployingMember(m wsm.DeployingMerge) QueueMember {
	out := QueueMember{Workspace: m.Workspace, Name: m.Name, Branch: m.Branch, Started: true, Step: footer.StepDeploying}
	if m.Async {
		out.Requester = m.Credited
	}
	return out
}

// rosterDeploying is M as the roster's queue draws it: its head, being worked
// on, its stages read from its ledger with its deploy stage live.
func (o *orchestrator) rosterDeploying(ctx context.Context, m wsm.DeployingMerge) (*sidebar.QueueEntry, error) {
	ledgers, err := o.deps.DB.MergeLedger(ctx, m.Workspace)
	if err != nil {
		return nil, err
	}
	var intervals []wsm.TabInterval
	for _, ledger := range ledgers {
		if ledger.Lease == m.Lease {
			intervals = ledger.Intervals
		}
	}
	view, err := viewOf(intervals, false)
	if err != nil {
		return nil, err
	}
	out := &sidebar.QueueEntry{
		Workspace: m.Workspace,
		Credited:  m.Credited,
		Async:     m.Async,
		Reveal:    feedid.Encode(headRef(m.Credited, m.Lease)),
		Admitted:  true,
		Stages:    rosterStages(deployingView(view)),
		Clock:     wsm.MergeClock{Phase: wsm.MergeClockActive, Since: m.ActiveSince},
	}
	if m.Async {
		out.AsyncName = m.Name
	}
	return out, nil
}

// deployingView is VIEW with its last deploy stage live again: the stage
// stands until the deploy's check ends.
func deployingView(view mergeView) mergeView {
	stages := append([]viewStage(nil), view.stages...)
	for i := len(stages) - 1; i >= 0; i-- {
		if stages[i].kind == TabDeploy {
			stages[i].state, stages[i].endedMS = stageLive, 0
			break
		}
	}
	return mergeView{stages: stages}
}

// deployingTabEntry is M as every merge's queue tab draws it, ahead of the
// line: merging, on its deploy tab since its deploying step began.
func (o *orchestrator) deployingTabEntry(ctx context.Context, m wsm.DeployingMerge) (*frontendv1.FeedMergeQueueEntry, error) {
	record, err := o.deps.DB.Workspace(ctx, m.Workspace)
	if err != nil {
		return nil, err
	}
	out := &frontendv1.FeedMergeQueueEntry{
		Workspace: &frontendv1.FeedMergeQueueWorkspace{Ref: &workspacev1.WorkspaceRef{Id: string(m.Workspace), Dir: record.Dir}},
		Label:     &frontendv1.FeedMergeQueueLabel{Text: m.Name},
		Status: &frontendv1.FeedMergeQueueEntry_Merging{Merging: &frontendv1.FeedMergeQueueMerging{
			ActiveTab: tabLabel(TabDeploy, 1), StageEnteredAtMs: m.DeployingSince.UnixMilli()}},
		Target: queueTarget(m.TargetDir),
	}
	if m.Async {
		requester, err := o.deps.DB.Workspace(ctx, m.Credited)
		if err != nil {
			return nil, err
		}
		out.Bubble = &frontendv1.FeedMergeQueueBubble{Requester: requester.Ref(), Row: feedid.Encode(headRef(m.Credited, m.Lease))}
	}
	return out, nil
}

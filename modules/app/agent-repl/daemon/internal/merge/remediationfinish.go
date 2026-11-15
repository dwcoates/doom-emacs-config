package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// THE REMEDIATION'S FINISH (agentrepl.v1.FinishWorkspaceSuccess
// .landing_directly; owner ruling, 2026-11-14). A remediation workspace's
// `workspace finished` never joins the queue, which is held for it. The
// daemon:
//
//  1. GATES: the full suite runs in the workspace's worktree, as the queue's
//     gate runs it, on a branch that holds the main branch's tip. A branch
//     behind the tip, or a failing suite, resumes the workspace to rebase or
//     fix and finish again, up to MaxFixAttempts failed gates; past it, the
//     chain is escalated to the user.
//  2. LANDS directly on the main branch: the merge, changelog line and
//     version tag bin/land-direct.sh makes.
//  3. DEPLOYS: the workspace is closed as a landed merge closes it, and the
//     rollout starts. The deploy owes the chain's next check; only the
//     intake's approval of it releases the queue.
//
// EACH STEP IS RECORDED BEFORE IT STARTS (wsm.RemediationFinish): the deploy
// restarts the daemon, and the one that serves after it carries on where the
// finish stood (resumeRemediationFinishes).

// opRemediationFinish is the operation every remediation finish record is
// filed under.
const opRemediationFinish = "daemon.merge.deploy_remediation"

// The briefs a remediation finish resumes its workspace with.
const (
	// BriefRemediationGateFailed: the gate failed; fix and finish again.
	BriefRemediationGateFailed = "deploy-remediation-gate-failed"
	// BriefRemediationBehind: the branch does not hold the main branch's
	// tip, or did not merge cleanly; rebase and finish again.
	BriefRemediationBehind = "deploy-remediation-behind"
)

// The gate-round outcomes a remediation finish records
// (Meter.ObserveRemediationGateRounds).
const (
	RemediationGateLanded  = "landed"
	RemediationGateGivenUp = "given_up"
)

// landDirectScript is the direct landing's script, relative to agent-repl's
// module root.
const landDirectScript = "bin/land-direct.sh"

// landDirectBoundSeconds bounds one direct landing: a --no-ff merge, a
// changelog commit and a tag in the main worktree, measured at under 2s on the
// owner's machine (2026-11-14, bin/land-direct.sh landing deploy-mediator); 5s
// is that with headroom for a cold disk.
const landDirectBoundSeconds = 5

// LandsDirectly implements Orchestrator: WS is a deploy check workspace whose
// finish lands directly -- the remediation of an owed check a fix is
// dispatched for, or a deploy escalation workspace the daemon made.
func (o *orchestrator) LandsDirectly(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	repo, err := o.directLandingRepo(ctx, ws)
	return repo != "", err
}

// directLandingRepo answers the queue WS lands directly for, empty when its
// finish goes through the queue.
func (o *orchestrator) directLandingRepo(ctx context.Context, ws ids.WorkspaceID) (wsm.RepoKey, error) {
	role, err := o.deps.DB.MediationRoleOf(ctx, ws)
	if err != nil {
		return "", err
	}
	if role != nil && !role.Intake && role.Mediation.Dispatched {
		return role.Repo, nil
	}
	repo, escalation, err := o.deps.DB.DeployEscalationOf(ctx, ws)
	if err != nil || !escalation {
		return "", err
	}
	return repo, nil
}

// FinishRemediation implements Orchestrator: records WS's finish at its gate
// and runs it, refusing a finish already running and a version increment
// that does not match the repository.
func (o *orchestrator) FinishRemediation(ctx context.Context, ws ids.WorkspaceID, version *wsm.VersionIncrement) error {
	fields := dlog.Context{"workspace": string(ws)}
	repo, err := o.directLandingRepo(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opRemediationFinish, "could not read the role of the workspace finishing", withField(fields, "error", err.Error()))
		return err
	}
	if repo == "" {
		err := fmt.Errorf("merge: workspace %s is no remediation a fix is dispatched to and no deploy escalation", ws)
		o.deps.Log.Global().Error(opRemediationFinish, "a direct finish reached a workspace that lands through the queue", withField(fields, "error", err.Error()))
		return err
	}
	fields["repo"] = string(repo)
	target, err := o.remediationTarget(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opRemediationFinish, "could not read where the remediation lands", withField(fields, "error", err.Error()))
		return err
	}
	versioned := target.localMerge().GetVersionTag() != nil
	switch {
	case version == nil && versioned:
		return refuse(ArmVersionIncrementRequired, ws, "a remediation lands on the main branch of a versioned repository, so its finish names how it moves the version (minor or patch) and its changelog line")
	case version != nil && !versioned:
		return unexpectedVersion(ws, reasonUnversionedRepository, "the repository's deploy config %s versions nothing", target.repoRoot)
	}
	f, err := o.deps.DB.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: ws, Repo: repo, StartedAt: o.deps.Now(), Version: version})
	if errors.Is(err, wsm.ErrRemediationFinishing) {
		o.deps.Log.Global().Warn(opRemediationFinish, "refused a remediation finish while its finish runs", fields)
		return refuse(ArmAlreadyMerging, ws, "this remediation's finish is already running")
	}
	if err != nil {
		o.deps.Log.Global().Error(opRemediationFinish, "could not record the remediation's finish", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Log.Global().Info(opRemediationFinish, "the remediation finished; its gate runs", withField(fields, "gate_rounds", f.GateRounds))
	o.startRemediationFinish(f)
	return nil
}

// remediationTarget is the deploy config of the repository remediation WS
// lands in: its main worktree's.
func (o *orchestrator) remediationTarget(ctx context.Context, ws ids.WorkspaceID) (deployTarget, error) {
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return deployTarget{}, err
	}
	main, err := o.deps.Git.MainWorktree(ctx, record.Dir)
	if err != nil {
		return deployTarget{}, err
	}
	return o.deployTargetFor(ctx, ws, main)
}

// resumeRemediationFinishes takes up every remediation finish recorded: the
// daemon that serves the queues after a restart carries each on from the
// step it stood at.
func (o *orchestrator) resumeRemediationFinishes(ctx context.Context) {
	finishes, err := o.deps.DB.RemediationFinishes(ctx)
	if err != nil {
		o.mediationFailed(ctx, nil, "could not read the remediation finishes to take up", err)
		return
	}
	for _, f := range finishes {
		o.startRemediationFinish(f)
	}
}

// startRemediationFinish runs finish F from the step it stands at, inside
// the mediation's lifetime, unless a run of it is already going here.
func (o *orchestrator) startRemediationFinish(f wsm.RemediationFinish) {
	life, leave, entered := o.enterMediation()
	if !entered {
		o.deps.Log.Global().Info(opRemediationFinish, mediationNotRun, dlog.Context{"workspace": string(f.Workspace)})
		return
	}
	o.mu.Lock()
	if o.remediationRuns == nil {
		o.remediationRuns = map[ids.WorkspaceID]bool{}
	}
	if o.remediationRuns[f.Workspace] {
		o.mu.Unlock()
		leave()
		return
	}
	o.remediationRuns[f.Workspace] = true
	o.mu.Unlock()
	run := func() {
		defer leave()
		defer func() {
			o.mu.Lock()
			delete(o.remediationRuns, f.Workspace)
			o.mu.Unlock()
		}()
		o.runRemediationFinish(life, f)
	}
	if !o.async {
		run()
		return
	}
	inflight.Go(run)
}

// runRemediationFinish carries finish F on from its step to the deploy.
func (o *orchestrator) runRemediationFinish(ctx context.Context, f wsm.RemediationFinish) {
	fields := dlog.Context{"workspace": string(f.Workspace), "repo": string(f.Repo), "step": string(f.Step), "gate_rounds": f.GateRounds}
	var err error
	switch f.Step {
	case wsm.RemediationGating:
		err = o.remediationGate(ctx, f, fields)
	case wsm.RemediationLanding:
		err = o.remediationLand(ctx, f, fields)
	case wsm.RemediationDeploying:
		err = o.remediationDeploy(ctx, f, fields)
	case wsm.RemediationFixing, wsm.RemediationDeployed:
		// THE WORKSPACE'S NEXT FINISH TAKES IT ON, or the chain's end.
		return
	}
	if err != nil {
		o.mediationFailed(ctx, fields, "the remediation's finish could not go on; it is taken up again by the next daemon that serves the queues", err)
	}
}

// remediationGate runs the full gate on F's branch and moves the finish on:
// to the landing on a pass, back to its workspace to fix or rebase on a
// failure, to the user past MaxFixAttempts failed gates.
func (o *orchestrator) remediationGate(ctx context.Context, f wsm.RemediationFinish, fields dlog.Context) error {
	record, err := o.deps.DB.Workspace(ctx, f.Workspace)
	if err != nil {
		return err
	}
	target, err := o.remediationTarget(ctx, f.Workspace)
	if err != nil {
		return err
	}
	mainBranch, err := o.deps.Git.CurrentBranch(ctx, target.repoRoot)
	if err != nil {
		return err
	}
	tip, err := o.deps.Git.ResolveRef(ctx, target.repoRoot, "HEAD")
	if err != nil {
		return err
	}
	holds, err := o.deps.Git.IsAncestor(ctx, record.Dir, tip, "HEAD")
	if err != nil {
		return err
	}
	if !holds {
		why := fmt.Sprintf("your branch does not hold %s's tip %s", mainBranch, shortSHA(tip))
		return o.resumeRemediation(ctx, f, BriefRemediationBehind, map[string]string{"main_branch": mainBranch, "why": why}, withField(fields, "main_tip", tip))
	}
	argv := o.deps.TestCommand(record.Dir, target.testScript(), record.Name)
	if len(argv) == 0 {
		return fmt.Errorf("merge: no test command is configured for the remediation's gate")
	}
	argv = gateArgv(append([]string(nil), argv...), o.deps.TestMetricsRoot)
	archive := filepath.Join(LogsDir(o.deps.StateDir), fmt.Sprintf("%s-tests-%d.log", f.Workspace, f.GateRounds+1))
	o.deps.Log.Global().Info(opRemediationFinish, "the remediation's full gate started", withField(fields, "archive", archive))
	result, err := o.runRemediationGate(ctx, record.Dir, argv, archive)
	if err != nil {
		return err
	}
	if why, broken := gateDidNotRun(result.ExitCode); broken {
		return fmt.Errorf("merge: the remediation's gate did not run: %s; the whole run is archived at %s", why, archive)
	}
	if !result.Passed {
		failing := readFailingTests(result.Tail, filepath.Join(record.Dir, moduleRoot)).render(archive)
		o.deps.Log.Global().Info(opRemediationFinish, "the remediation's full gate failed", withField(withField(fields, "archive", archive), "exit_code", result.ExitCode))
		return o.resumeRemediation(ctx, f, BriefRemediationGateFailed, map[string]string{"archive_path": archive, "failing_tests": failing}, fields)
	}
	o.deps.Log.Global().Info(opRemediationFinish, "the remediation's full gate passed; it lands", withField(fields, "archive", archive))
	moved, err := o.deps.DB.AdvanceRemediationFinish(ctx, f.Workspace, wsm.RemediationGating, wsm.RemediationLanding, o.deps.Now())
	if err != nil || !moved {
		return finishMoved(err, moved, f, wsm.RemediationLanding)
	}
	f.Step = wsm.RemediationLanding
	return o.remediationLand(ctx, f, withField(fields, "step", string(f.Step)))
}

// runRemediationGate runs ARGV in DIR, writing every line to ARCHIVE, and
// answers the run.
func (o *orchestrator) runRemediationGate(ctx context.Context, dir string, argv []string, archive string) (GateResult, error) {
	measured, err := o.deps.TestRunner.RunLinesMeasured(ctx, dir, argv, func(string) {})
	if err != nil {
		return GateResult{}, fmt.Errorf("merge: run the remediation's gate: %w", err)
	}
	// THE ARCHIVE IS THE ONLY ACCOUNT OF A FAILURE THAT SURVIVES THE RUN, so
	// a gate whose log failed is an unrunnable gate rather than a lost log.
	if err := writeLog(archive, measured.Output); err != nil {
		return GateResult{}, fmt.Errorf("merge: write the remediation's gate log %s: %w", archive, err)
	}
	return GateResult{Passed: measured.ExitCode == 0, Tail: measured.Output, ArchivePath: archive, ExitCode: measured.ExitCode}, nil
}

// finishMoved is the error of a finish step that did not move as it should:
// ERR, or the finish having moved meanwhile (a second daemon's run, which
// is a defect: one daemon serves the queues).
func finishMoved(err error, moved bool, f wsm.RemediationFinish, to wsm.RemediationStep) error {
	if err != nil {
		return err
	}
	return fmt.Errorf("merge: the remediation finish of %s moved from %s before this run moved it to %s", f.Workspace, f.Step, to)
}

// resumeRemediation sends F's workspace back to work with BRIEF: its finish
// steps back to fixing, counting a failed gate, and its next finish gates
// again. Past MaxFixAttempts failed gates the chain is escalated to the user
// instead: the fix's approach is the problem, and the queue is let through.
func (o *orchestrator) resumeRemediation(ctx context.Context, f wsm.RemediationFinish, brief string, values map[string]string, fields dlog.Context) error {
	if f.GateRounds+1 > MaxFixAttempts {
		return o.giveUpRemediation(ctx, f, fields)
	}
	from := f.Step
	moved, err := o.deps.DB.AdvanceRemediationFinish(ctx, f.Workspace, from, wsm.RemediationFixing, o.deps.Now())
	if err != nil || !moved {
		return finishMoved(err, moved, f, wsm.RemediationFixing)
	}
	values["attempt"] = fmt.Sprint(f.GateRounds + 1)
	values["max_attempts"] = fmt.Sprint(MaxFixAttempts)
	text, err := o.deps.Briefs(brief, values)
	if err != nil {
		return err
	}
	if _, err := o.submitCheckPrompt(ctx, f.Workspace, wsm.NewTurnID(), text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); err != nil {
		return err
	}
	o.deps.Log.Global().Info(opRemediationFinish, "the remediation's gate sent it back to work; its next finish gates again", withField(withField(fields, "brief", brief), "attempt", f.GateRounds+1))
	return nil
}

// giveUpRemediation escalates F's chain to the user: its gate failed
// MaxFixAttempts times. The daemon escalates for the intake, so the queue is
// never left held by a remediation that cannot land.
func (o *orchestrator) giveUpRemediation(ctx context.Context, f wsm.RemediationFinish, fields dlog.Context) error {
	o.deps.Meter.ObserveRemediationGateRounds(RemediationGateGivenUp, f.GateRounds)
	name, err := o.workspaceName(ctx, opRemediationFinish, f.Workspace)
	if err != nil {
		return err
	}
	summary := fmt.Sprintf("the deploy remediation %s could not pass its gate after %d fixing attempts; the queue moved on", name, MaxFixAttempts)
	return o.escalateForIntake(ctx, f.Repo, f.Workspace, summary, withField(fields, "gave_up", true))
}

// escalateForIntake releases the check REPO owes as an escalation the daemon
// makes for its chain's intake, putting SUMMARY before the user on behalf of
// remediation workspace WS.
func (o *orchestrator) escalateForIntake(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, summary string, fields dlog.Context) error {
	m, err := o.deps.DB.MediationOf(ctx, repo)
	if err != nil {
		return err
	}
	if m == nil {
		o.deps.Log.Global().Info(opRemediationFinish, "the chain the remediation worked for ended already; nothing is escalated", fields)
		return nil
	}
	released, err := o.deps.DB.ReleaseMediation(ctx, repo, m.OwedAt)
	if err != nil {
		return err
	}
	if released {
		o.endChain(ctx, repo, *m, MediationEscalated)
	}
	o.deps.Log.Global().Error(opRemediationFinish, "the deploy remediation cannot land its fix; the chain is escalated to the user and the queue advances", withField(withField(fields, "summary", summary), "released", released))
	q, ok, err := o.checkedQueueOf(ctx, repo)
	if err != nil {
		return err
	}
	if !ok {
		o.deps.Log.Global().Error(opRemediationFinish, "the repository checks its deploys no more, so the given-up remediation is put before nobody", fields)
		return nil
	}
	// The failure is recorded by the escalation itself.
	_, _ = o.escalate(ctx, q, *m, summary, nil, fields)
	o.republishMediation(ctx, repo)
	o.kick(repo)
	return nil
}

// remediationLand lands F's branch directly on the main branch
// (bin/land-direct.sh) and moves the finish to the deploy, recording what
// the landing made. A branch that did not merge cleanly goes back to its
// workspace to rebase.
func (o *orchestrator) remediationLand(ctx context.Context, f wsm.RemediationFinish, fields dlog.Context) error {
	record, err := o.deps.DB.Workspace(ctx, f.Workspace)
	if err != nil {
		return err
	}
	target, err := o.remediationTarget(ctx, f.Workspace)
	if err != nil {
		return err
	}
	mainBranch, err := o.deps.Git.CurrentBranch(ctx, target.repoRoot)
	if err != nil {
		return err
	}
	before, err := o.deps.Git.ResolveRef(ctx, target.repoRoot, "HEAD")
	if err != nil {
		return err
	}
	// A LANDING A RESTART CUT AFTER ITS MERGE is not landed twice: the main
	// branch holding the branch's head is the landing done.
	head, err := o.deps.Git.ResolveRef(ctx, target.repoRoot, record.Branch)
	if err != nil {
		return err
	}
	landed, err := o.deps.Git.IsAncestor(ctx, target.repoRoot, head, before)
	if err != nil {
		return err
	}
	if !landed {
		part, line := "none", record.Branch
		if f.Version != nil {
			part, line = f.Version.Part.String(), f.Version.ChangelogLine
		}
		script := &repoconfigv1.HookScript{Path: filepath.Join(filepath.Dir(o.deps.PromptsDir), landDirectScript), TimeoutSeconds: landDirectBoundSeconds}
		logPath := LifecycleLogPath(o.deps.StateDir, lifecycleconfig.StageFinish, o.deps.Now(), 0)
		kept := RunKeptScript(ctx, o.deps.HookRunner, target.repoRoot, script, nil, logPath, target.repoRoot, part, line, record.Branch)
		if kept.Err != nil {
			return fmt.Errorf("merge: run the remediation's direct landing: %w", kept.Err)
		}
		if kept.LogErr != nil {
			o.deps.Log.Global().Error(opRemediationFinish, "the remediation's direct landing ran but its log could not be kept", withField(withField(fields, "log", logPath), "error", kept.LogErr.Error()))
		}
		run := kept.Run
		switch {
		case run.TimedOut:
			return fmt.Errorf("merge: the remediation's direct landing outlived its bound of %ds (measured under 2s): %s", landDirectBoundSeconds, strings.TrimSpace(run.Output))
		case run.ExitCode == 1:
			return o.resumeRemediation(ctx, f, BriefRemediationBehind, map[string]string{"main_branch": mainBranch, "why": strings.TrimSpace(run.Output)}, fields)
		case run.ExitCode != 0:
			return fmt.Errorf("merge: the remediation's direct landing failed (exit %d): %s", run.ExitCode, strings.TrimSpace(run.Output))
		}
	}
	after, err := o.deps.Git.ResolveRef(ctx, target.repoRoot, "HEAD")
	if err != nil {
		return err
	}
	paths, err := o.deps.Git.ChangedPaths(ctx, target.repoRoot, before+".."+after)
	if err != nil {
		return err
	}
	moved, err := o.deps.DB.RecordRemediationLanded(ctx, f.Workspace, after, paths, o.deps.Now())
	if err != nil || !moved {
		return finishMoved(err, moved, f, wsm.RemediationDeploying)
	}
	o.deps.Meter.ObserveRemediationGateRounds(RemediationGateLanded, f.GateRounds)
	o.deps.Log.Global().Info(opRemediationFinish, "the remediation landed directly on the main branch; it deploys", withField(withField(fields, "head", after), "paths", len(paths)))
	f.Step, f.StepAt = wsm.RemediationDeploying, o.deps.Now()
	return o.remediationDeploy(ctx, f, withField(fields, "step", string(f.Step)))
}

// remediationDeploy closes F's workspace as a landed merge closes it and
// starts the rollout, whose deploy owes the chain's next check
// (remediationDeployed moves the finish on when it does). A deploy that
// began already since the finish reached this step is not started again.
func (o *orchestrator) remediationDeploy(ctx context.Context, f wsm.RemediationFinish, fields dlog.Context) error {
	m, err := o.deps.DB.MediationOf(ctx, f.Repo)
	if err != nil {
		return err
	}
	if m != nil && m.OwedAt.After(f.StepAt) {
		return o.remediationDeployed(ctx, f.Repo, m.OwedAt)
	}
	if err := o.closeRemediation(ctx, f, fields); err != nil {
		return err
	}
	o.deps.Log.Global().Info(opRemediationFinish, "the remediation's deploy starts; it owes the chain's next check", fields)
	o.deps.Rollout.Landed(ctx, nil)
	return nil
}

// remediationDeployed moves every finish of REPO's queue that was deploying
// before the deploy that began at AT to deployed: that deploy is theirs.
func (o *orchestrator) remediationDeployed(ctx context.Context, repo wsm.RepoKey, at time.Time) error {
	finishes, err := o.deps.DB.RemediationFinishes(ctx)
	if err != nil {
		return err
	}
	var errs []error
	for _, f := range finishes {
		if !sameQueue(f.Repo, repo) || f.Step != wsm.RemediationDeploying || f.StepAt.After(at) {
			continue
		}
		if _, err := o.deps.DB.AdvanceRemediationFinish(ctx, f.Workspace, wsm.RemediationDeploying, wsm.RemediationDeployed, at); err != nil {
			errs = append(errs, err)
			continue
		}
		o.deps.Log.Global().Info(opRemediationFinish, "the remediation's deploy began; it owes the chain's next check", dlog.Context{"workspace": string(f.Workspace), "repo": string(repo)})
	}
	return errors.Join(errs...)
}

// closeRemediation closes F's workspace as a landed merge closes its own: it
// is stamped merged and closed, its session stopped, its worktree removed and
// its landed branch retired. A later dispatch restores it from its branch
// (workspace Open), cut again at the main branch's tip.
func (o *orchestrator) closeRemediation(ctx context.Context, f wsm.RemediationFinish, fields dlog.Context) error {
	record, err := o.deps.DB.Workspace(ctx, f.Workspace)
	if err != nil {
		return err
	}
	if record.Closed {
		return nil
	}
	if err := o.deps.DB.SetMergedAt(ctx, f.Workspace, o.deps.Now(), nil); err != nil {
		return err
	}
	if err := o.deps.DB.MarkClosed(ctx, f.Workspace, wsm.Closure{At: o.deps.Now(), By: wsm.CloseByMerge}); err != nil {
		return err
	}
	main, err := o.deps.Git.MainWorktree(ctx, record.Dir)
	if err != nil {
		return err
	}
	// A RETRY AFTER THE WORKTREE WENT finds nothing left to tear down.
	if _, err := os.Stat(record.Dir); err == nil {
		if err := o.retireLandedWorktree(ctx, o.deps.Log.Global(), opRemediationFinish, o.deps.Git, f.Workspace, main, record.Dir, main, record.Branch); err != nil {
			return fmt.Errorf("merge: tear down the landed remediation: %w", err)
		}
	}
	if err := o.deps.PublishRegistry(ctx); err != nil {
		o.deps.Log.Global().Error(opRemediationFinish, "could not republish the roster after the remediation closed", withField(fields, "error", err.Error()))
	}
	o.deps.Log.Global().Info(opRemediationFinish, "closed the landed remediation and removed its worktree", withField(fields, "worktree", record.Dir))
	return nil
}

// remediationClosed hands the hold back when remediation WS closes before
// its fix is deploying: nobody else fixes the deploy, so the daemon
// escalates the chain for its intake, and the queue is never left held with
// no owner.
func (o *orchestrator) remediationClosed(ctx context.Context, ws ids.WorkspaceID) {
	fields := dlog.Context{"workspace": string(ws)}
	role, err := o.deps.DB.MediationRoleOf(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opRemediationFinish, "could not read whether a closed workspace was a remediation at work", withField(fields, "error", err.Error()))
		return
	}
	if role == nil || role.Intake || !role.Mediation.Dispatched {
		return
	}
	f, err := o.deps.DB.RemediationFinishOf(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opRemediationFinish, "could not read a closed remediation's finish", withField(fields, "error", err.Error()))
		return
	}
	if f != nil && (f.Step == wsm.RemediationDeploying || f.Step == wsm.RemediationDeployed) {
		return
	}
	name, err := o.workspaceName(ctx, opRemediationFinish, ws)
	if err != nil {
		o.deps.Log.Global().Error(opRemediationFinish, "a remediation closed before landing, and its chain could not be escalated without its name; the queue stays held", withField(fields, "error", err.Error()))
		return
	}
	summary := fmt.Sprintf("the deploy remediation %s was closed before its fix landed; the queue moved on", name)
	if err := o.escalateForIntake(ctx, role.Repo, ws, summary, fields); err != nil {
		o.deps.Log.Global().Error(opRemediationFinish, "a remediation closed before landing, and its chain could not be escalated; the queue stays held", withField(fields, "error", err.Error()))
	}
}

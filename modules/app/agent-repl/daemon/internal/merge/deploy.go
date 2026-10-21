package merge

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/scriptrunner"
	"agent-repld/internal/wsm"
)

// This file is a merge's DEPLOY: the three stages its repository's
// `.agent-repl/deploy.yaml` (repoconfig.v1.DeployConfig) describes, and the
// hooks they run.
//
//	pre_deploy   its hooks, then the built-in local merge (process.go) when
//	             the config has one; never held
//	deploy       the one deploy hook; skipped while the repository is held
//	post_deploy  its hooks; never held
//
// A HOOK IS A SCRIPT OR A PROMPT, as equal arms. A prompt is a turn of the
// deploy's OWNER, the workspace the run runs in (r.ws), and of no other; a
// script runs from the repository's main checkout root. Closing the owner and
// removing its worktree are the teardown's (terminal.go), after every hook, so
// no hook reaches a closed owner or a removed worktree.
//
// A FAILED HOOK FAILS THE DEPLOY, in any stage, in the area "hook": its
// failure is handed back to the workspace that follows the deploy, which is
// prompted to investigate, and a failed deploy never closes its owner. A
// deploy whose deploy or post-deploy hook failed after its local merge landed
// keeps the commit it landed.

// The deploy's tab kinds, one tab round per hook run. They are ledger
// interval kinds like every other tab kind.
const (
	TabPreDeploy  = "pre_deploy"
	TabDeploy     = "deploy"
	TabPostDeploy = "post_deploy"
)

// The deploy config refusals' arms (agentrepl.v1.FinishWorkspaceError). An
// async request's are its CreateWorkspaceError arms, renamed by its handler.
const (
	// ArmNoDeployConfig refuses a deploy into a repository with no
	// `.agent-repl/deploy.yaml`. Its `path` field names the file.
	ArmNoDeployConfig = "no_deploy_config"
	// ArmDeployConfigInvalid refuses a deploy into a repository whose config
	// is unusable. Its `path` and `detail` fields name the file and why.
	ArmDeployConfigInvalid = "deploy_config_invalid"
)

// The environment a script hook runs in, beside the daemon's own.
const (
	// EnvCallingWorkspace is the full path of the deploy owner's worktree.
	EnvCallingWorkspace = "AGENT_REPL_CALLING_WORKSPACE"
	// EnvDeployLedger is the deploy's ledger (agentrepl.v1.DeployLedger), which
	// a script names when it calls back into the daemon about this deploy.
	EnvDeployLedger = "AGENT_REPL_DEPLOY_LEDGER"
	// EnvDeployStage is the stage the hook runs in.
	EnvDeployStage = "AGENT_REPL_DEPLOY_STAGE"
)

// scriptTailLines is how many of a script hook's last output lines its tab
// draws.
const scriptTailLines = 20

// deployTarget is where a deploy lands and what its repository's config says
// it does there, read once per run (takeSession) so every stage reads one
// config.
type deployTarget struct {
	// cfg is the repository's deploy config.
	cfg *repoconfigv1.DeployConfig
	// repoRoot is the repository's main checkout root: where its config lives
	// and where a script hook runs.
	repoRoot string
	// toMain reports that the target is the repository's main branch in its
	// main worktree, which `main` hooks and a version tag need.
	toMain bool
}

// deployTargetFor reads the deploy config of the repository TARGET is a
// checkout of. A repository with no config, or an unusable one, is the
// deploy's refusal, naming the file.
func (o *orchestrator) deployTargetFor(ctx context.Context, ws ids.WorkspaceID, target string) (deployTarget, error) {
	main, err := o.deps.Git.MainWorktree(ctx, target)
	if err != nil {
		return deployTarget{}, fmt.Errorf("merge: resolving the main worktree of %s's target %s: %w", ws, target, err)
	}
	cfg, err := o.deps.DeployConfig.Load(main)
	if err != nil {
		var missing *deployconfig.MissingError
		var invalid *deployconfig.InvalidError
		switch {
		case errors.As(err, &missing):
			refusal := refuse(ArmNoDeployConfig, ws, "the repository states no deploy: %s does not exist", missing.Path)
			refusal.Fields = map[string]any{"path": missing.Path}
			return deployTarget{}, refusal
		case errors.As(err, &invalid):
			refusal := refuse(ArmDeployConfigInvalid, ws, "the repository's deploy config %s is unusable: %s", invalid.Path, invalid.Detail)
			refusal.Fields = map[string]any{"path": invalid.Path, "detail": invalid.Detail}
			return deployTarget{}, refusal
		}
		return deployTarget{}, fmt.Errorf("merge: reading the deploy config of %s: %w", main, err)
	}
	return deployTarget{cfg: cfg, repoRoot: main, toMain: sameDir(target, main)}, nil
}

// localMerge is the config's built-in local merge, nil when nothing lands
// locally.
func (t deployTarget) localMerge() *repoconfigv1.DeployLocalMerge {
	return t.cfg.GetPreDeploy().GetLocalMerge()
}

// testScript is the local merge's test runner, relative to a checkout's root.
// Only a run with a local merge tests, so a run without one asking for it is a
// defect.
func (t deployTarget) testScript() string {
	path := t.localMerge().GetTests().GetAgentReplSuites().GetScriptPath()
	if path == "" {
		panic("merge: a run with no agent_repl_suites test step asked for its test script")
	}
	return path
}

// configured answers the config's hooks of one stage that run for this
// deploy's target.
func (t deployTarget) configured(entries []*repoconfigv1.DeployHookEntry) []*repoconfigv1.DeployHook {
	var out []*repoconfigv1.DeployHook
	for _, entry := range entries {
		if deployconfig.RunsFor(entry, t.toMain) {
			out = append(out, entry.GetHook())
		}
	}
	return out
}

// preDeployHooks are the hooks the pre-deploy stage runs: the ones the
// request or the owner's creation supplied, which REPLACE the repository's,
// and otherwise the repository's that run for this target.
func (r *run) preDeployHooks() []*repoconfigv1.DeployHook {
	if hooks := r.requestHooks().PreDeploy; len(hooks) > 0 {
		return hooks
	}
	return r.target.configured(r.target.cfg.GetPreDeploy().GetHooks())
}

// postDeployHooks are the post-deploy stage's hooks, chosen as the
// pre-deploy stage's are.
func (r *run) postDeployHooks() []*repoconfigv1.DeployHook {
	if hooks := r.requestHooks().PostDeploy; len(hooks) > 0 {
		return hooks
	}
	return r.target.configured(r.target.cfg.GetPostDeploy())
}

// requestHooks are the hooks the owner's creation recorded: for an async
// merge's merge workspace, the request's own (moved onto its creation job at
// hand-off); for any other workspace, what its creation supplied.
func (r *run) requestHooks() wsm.DeployHooks { return r.job.Hooks }

// deployHook is the repository's deploy hook when it runs for this target,
// nil otherwise.
func (r *run) deployHook() *repoconfigv1.DeployHook {
	entry := r.target.cfg.GetDeploy()
	if entry == nil || !deployconfig.RunsFor(entry, r.target.toMain) {
		return nil
	}
	return entry.GetHook()
}

// stageOf names a hook tab kind's stage as the footer, the environment and the
// tab's label spell it.
type stageOf struct {
	kind   string
	step   footer.MergeStep
	origin conversationv1.PromptOrigin
	env    string
}

var (
	stagePreDeploy  = stageOf{kind: TabPreDeploy, step: footer.StepPreDeploy, origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION, env: "pre_deploy"}
	stageDeploy     = stageOf{kind: TabDeploy, step: footer.StepDeploying, origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_HOOK, env: "deploy"}
	stagePostDeploy = stageOf{kind: TabPostDeploy, step: footer.StepPostDeploy, origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION, env: "post_deploy"}
)

// preDeploy runs the pre-deploy stage's hooks, in order. A failed hook fails
// the deploy before anything lands.
func (r *run) preDeploy(ctx context.Context) (outcome, bool, error) {
	for i, hook := range r.preDeployHooks() {
		failure, err := r.runHook(ctx, stagePreDeploy, i, hook, nil)
		if err != nil {
			return outcome{}, false, err
		}
		if failure != "" {
			return failedIn(footer.FailedHook, failure), true, nil
		}
	}
	return outcome{}, false, nil
}

// deployStage runs the repository's deploy hook, unless the repository is
// held. OUT is what the local merge landed, kept whatever the hook does: a
// failed hook fails the deploy but leaves the landing standing.
func (r *run) deployStage(ctx context.Context, out outcome) (outcome, error) {
	const op = "daemon.merge.deploy_stage"
	hook := r.deployHook()
	if hook == nil {
		return out, nil
	}
	log := r.o.log(ctx, r.ws)
	fields := dlog.Context{"workspace": string(r.ws), "repo": string(r.repo), "lease": string(r.lease.ID)}
	hold, err := r.o.deps.DB.DeployHoldOf(ctx, r.repo)
	switch {
	case err != nil:
		// A HOLD THAT CANNOT BE READ DEPLOYS NOTHING: a deploy not run is
		// recovered by running it by hand, a hand-deployed build replaced is
		// not.
		log.Info(op, "the repository's deploy hold could not be read; the deploy hook is not run", withField(fields, "cause", err.Error()))
		r.setDeploy(LandingHoldUnread)
		return out, nil
	case hold.Held:
		if err := r.o.deps.DB.RecordDeploySkip(ctx, r.repo); err != nil {
			log.Error(op, "could not record the deploy's skip on the repository's hold; the deploy hook is still not run", withField(fields, "error", err.Error()))
		}
		log.Info(op, "the repository's deploy stage is held; the deploy hook is skipped", withField(fields, "held_since", hold.Since))
		r.setDeploy(LandingRolloutsHeld)
		return out, nil
	}
	failure, err := r.runHook(ctx, stageDeploy, 0, hook, &out)
	if err != nil {
		return outcome{}, err
	}
	if failure != "" {
		return landedButFailed(out, failure), nil
	}
	return out, nil
}

// postDeploy runs the post-deploy stage's hooks, in order. OUT is what the
// deploy landed; a failed hook fails the deploy and keeps the landing.
func (r *run) postDeploy(ctx context.Context, out outcome) (outcome, error) {
	for i, hook := range r.postDeployHooks() {
		failure, err := r.runHook(ctx, stagePostDeploy, i, hook, &out)
		if err != nil {
			return outcome{}, err
		}
		if failure != "" {
			return landedButFailed(out, failure), nil
		}
	}
	return out, nil
}

// landedButFailed is a deploy whose local merge landed and whose later hook
// failed: it fails in the area "hook", and keeps what it landed.
func landedButFailed(out outcome, failure string) outcome {
	out.failed, out.area = failure, footer.FailedHook
	if out.landed != "" {
		out.failed = fmt.Sprintf("it landed as commit %s, but %s", shortSHA(out.landed), failure)
	}
	return out
}

// shortSHA is a commit's short hash as drawn.
func shortSHA(sha string) string {
	if len(sha) > 10 {
		return sha[:10]
	}
	return sha
}

// runHook runs hook i of a stage in its own tab round, and answers the
// failure's one-line account, empty when it succeeded. LANDED is what the
// deploy has landed so far, recorded with a prompt hook's turn so a resume
// carries it on; nil before anything landed.
func (r *run) runHook(ctx context.Context, stage stageOf, i int, hook *repoconfigv1.DeployHook, landed *outcome) (string, error) {
	switch h := hook.GetHook().(type) {
	case *repoconfigv1.DeployHook_Prompt:
		var doc *outcomeDoc
		if landed != nil {
			doc = toOutcomeDoc(*landed)
		}
		round, close, err := r.configuredPrompt(ctx, stage.kind, stage.step, i, h.Prompt.GetText(), stage.origin, doc)
		if round.kind == "" {
			return "", nil
		}
		if err != nil || close.Failed() {
			summary := fmt.Sprintf("its %s prompt hook did not complete", stageWords(stage.kind))
			r.upsert(round, hookPromptTab(round.settled(r.o.nowMS(), summary)))
			r.closeTab(ctx, round, "failed")
			if err != nil {
				return "", err
			}
			r.o.log(ctx, r.ws).Info("daemon.merge.hook", "a prompt hook did not complete; the deploy fails", dlog.Context{
				"workspace": string(r.ws), "stage": stage.kind, "hook": i, "close": close.String()})
			return summary, nil
		}
		r.upsert(round, hookPromptTab(round.settled(r.o.nowMS(), "")))
		r.closeTab(ctx, round, "succeeded")
		return "", nil
	case *repoconfigv1.DeployHook_Script:
		return r.runScriptHook(ctx, stage, i, h.Script)
	}
	panic("merge: a checked hook is neither a script nor a prompt")
}

// stageNamed is the stage a hook tab kind names.
func stageNamed(kind string) stageOf {
	switch kind {
	case TabPreDeploy:
		return stagePreDeploy
	case TabDeploy:
		return stageDeploy
	case TabPostDeploy:
		return stagePostDeploy
	}
	panic("merge: no stage is spelled " + kind)
}

// stageWords is a stage's tab kind in plain words.
func stageWords(kind string) string {
	switch kind {
	case TabPreDeploy:
		return "pre-deploy"
	case TabDeploy:
		return "deploy"
	case TabPostDeploy:
		return "post-deploy"
	}
	panic("merge: no stage is spelled " + kind)
}

// hookPromptTab is a prompt hook's tab arm.
func hookPromptTab(st tabState) *frontendv1.FeedMergeTab {
	live, settled := st.badge()
	inner := &frontendv1.FeedMergeTabHookPrompt{}
	if live != nil {
		inner.State = &frontendv1.FeedMergeTabHookPrompt_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabHookPrompt_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_HookPrompt{HookPrompt: inner}}
}

// scriptView is a script hook's tab as it stands: what it runs, its log, the
// tail of its output, and how it ended.
type scriptView struct {
	path    string
	logPath string
	tail    []string
	exit    *frontendv1.FeedMergeHookScriptExit
}

// hookScriptTab is a script hook's tab arm.
func hookScriptTab(st tabState, v scriptView) *frontendv1.FeedMergeTab {
	live, settled := st.badge()
	inner := &frontendv1.FeedMergeTabHookScript{
		Path: &frontendv1.FeedMergeHookScriptPath{Text: v.path},
		Log:  &frontendv1.FeedMergeHookScriptLog{Path: v.logPath},
		Exit: v.exit,
	}
	if live != nil {
		inner.State = &frontendv1.FeedMergeTabHookScript_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabHookScript_Settled{Settled: settled}
	}
	for _, line := range v.tail {
		inner.Tail = append(inner.Tail, &frontendv1.FeedMergeHookScriptLine{Text: line})
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_HookScript{HookScript: inner}}
}

// scriptLine is the footer's line while a script hook runs.
func scriptLine(stage stageOf, path string) *frontendv1.FooterStatusActivityMergeStep {
	return hookLine(stage, &frontendv1.FooterMergeStepHook{Hook: &frontendv1.FooterMergeStepHook_Script{Script: &frontendv1.FooterMergeStepScript{Path: path}}})
}

// hookLine is the footer's line of a stage's running hook.
func hookLine(stage stageOf, hook *frontendv1.FooterMergeStepHook) *frontendv1.FooterStatusActivityMergeStep {
	switch stage.kind {
	case TabPreDeploy:
		return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_PreDeploy{PreDeploy: hook}}
	case TabDeploy:
		return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_Deploying{Deploying: hook}}
	case TabPostDeploy:
		return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_PostDeploy{PostDeploy: hook}}
	}
	panic("merge: no stage is spelled " + stage.kind)
}

// runScriptHook runs a script hook from the repository's main checkout root,
// drawing its tab live as it writes, and answers its failure's account.
//
// A SCRIPT HOOK A RESTART CUT SHORT RUNS AGAIN on resume: the dead daemon's
// child cannot be reattached to, and a hook acts on the repository's current
// state, so running it again is what the skill documents a hook must bear.
func (r *run) runScriptHook(ctx context.Context, stage stageOf, i int, script *repoconfigv1.DeployHookScript) (string, error) {
	const op = "daemon.merge.hook"
	// ONE INDEX COUNTS A STAGE'S HOOKS, scripts and prompts alike
	// (progressDoc.PromptIndex), so a resume skips exactly the hooks before
	// the one the record stands at.
	if res, ok := r.resumingAt(stage.kind); ok {
		if i < res.PromptIndex {
			return "", nil
		}
		r.doneResuming()
	}
	round := r.openTab(ctx, stage.kind)
	view := scriptView{path: script.GetPath(), logPath: r.hookLogPath(stage, round)}
	r.setStep(ctx, stage.step, func(f *footer.MergeFacts) { f.Line = scriptLine(stage, script.GetPath()) })
	r.upsert(round, hookScriptTab(round.live(), view))
	if err := r.checkpoint(ctx, stage.kind, func(d *progressDoc) { d.PromptIndex, d.Turn = i, "" }); err != nil {
		return "", err
	}
	fields := dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "stage": stage.kind, "hook": i,
		"script": script.GetPath(), "repo_root": r.target.repoRoot, "log": view.logPath, "timeout_seconds": script.GetTimeoutSeconds()}
	logFile, err := createLog(view.logPath)
	if err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not open the script hook's log; the hook fails", withField(fields, "error", err.Error()))
		return r.settleScript(ctx, round, view, fmt.Sprintf("its %s script %s could not keep a log: %v", stageWords(stage.kind), script.GetPath(), err)), nil
	}
	defer logFile.Close()
	var mu sync.Mutex
	onLine := func(line string) {
		mu.Lock()
		defer mu.Unlock()
		fmt.Fprintln(logFile, line)
		view.tail = append(view.tail, line)
		if len(view.tail) > scriptTailLines {
			view.tail = view.tail[len(view.tail)-scriptTailLines:]
		}
		r.upsert(round, hookScriptTab(round.live(), view))
	}
	env := []string{EnvCallingWorkspace + "=" + r.subject.dir, EnvDeployLedger + "=" + string(r.lease.ID), EnvDeployStage + "=" + stage.env}
	argv := []string{"bash", filepath.Join(r.target.repoRoot, script.GetPath())}
	r.o.log(ctx, r.ws).Info(op, "a script hook started", fields)
	run, err := r.o.deps.HookRunner.RunLinesWith(ctx, r.target.repoRoot, argv,
		scriptrunner.Options{Env: env, Timeout: time.Duration(script.GetTimeoutSeconds()) * time.Second}, onLine)
	if err != nil {
		if r.exiting() {
			return "", err
		}
		r.o.log(ctx, r.ws).Error(op, "a script hook could not be run; the deploy fails", withField(fields, "error", err.Error()))
		return r.settleScript(ctx, round, view, fmt.Sprintf("its %s script %s could not be run: %v", stageWords(stage.kind), script.GetPath(), err)), nil
	}
	mu.Lock()
	defer mu.Unlock()
	fields["wall_ms"] = run.Wall.Milliseconds()
	switch {
	case run.TimedOut:
		view.exit = &frontendv1.FeedMergeHookScriptExit{How: &frontendv1.FeedMergeHookScriptExit_TimedOut{
			TimedOut: &frontendv1.FeedMergeHookScriptTimedOut{TimeoutSeconds: script.GetTimeoutSeconds()}}}
		r.o.log(ctx, r.ws).Info(op, "a script hook ran past its timeout and was killed; the deploy fails", fields)
		return r.settleScript(ctx, round, view, fmt.Sprintf("its %s script %s ran past its %ds timeout", stageWords(stage.kind), script.GetPath(), script.GetTimeoutSeconds())), nil
	case run.ExitCode != 0:
		view.exit = &frontendv1.FeedMergeHookScriptExit{How: &frontendv1.FeedMergeHookScriptExit_Exited{
			Exited: &frontendv1.FeedMergeHookScriptExited{Status: int32(run.ExitCode)}}}
		r.o.log(ctx, r.ws).Info(op, "a script hook exited non-zero; the deploy fails", withField(fields, "exit_code", run.ExitCode))
		return r.settleScript(ctx, round, view, fmt.Sprintf("its %s script %s exited %d", stageWords(stage.kind), script.GetPath(), run.ExitCode)), nil
	}
	view.exit = &frontendv1.FeedMergeHookScriptExit{How: &frontendv1.FeedMergeHookScriptExit_Exited{
		Exited: &frontendv1.FeedMergeHookScriptExited{Status: 0}}}
	r.o.log(ctx, r.ws).Info(op, "a script hook succeeded", fields)
	r.upsert(round, hookScriptTab(round.settled(r.o.nowMS(), ""), view))
	r.closeTab(ctx, round, "succeeded")
	return "", nil
}

// settleScript draws a script hook's tab failed with SUMMARY and answers it.
func (r *run) settleScript(ctx context.Context, round tabRound, view scriptView, summary string) string {
	r.upsert(round, hookScriptTab(round.settled(r.o.nowMS(), summary), view))
	r.closeTab(ctx, round, "failed")
	return summary
}

// hookLogPath is where one script hook round's output is kept: among the
// queue's logs (LogsDir), by the deploy's ledger, its stage and its round.
func (r *run) hookLogPath(stage stageOf, round tabRound) string {
	return filepath.Join(LogsDir(r.o.deps.StateDir), fmt.Sprintf("%s-%s-%d.log", r.lease.ID, stage.env, round.n))
}

// setDeploy records what the deploy decided about its rollout, once.
func (r *run) setDeploy(decision LandingDeploy) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.deploy = &decision
}

// RollOutLanding implements Orchestrator: a deploy hook asks for the rollout
// of what its deploy landed. With a ledger, the rollout is REGISTERED on that
// running deploy and fired at its teardown, after its report is owed to it;
// with none (a hold's release ran the script), it starts at once.
func (o *orchestrator) RollOutLanding(ctx context.Context, ledger ids.LeaseID) (bool, error) {
	const op = "daemon.merge.roll_out_landing"
	if ledger == "" {
		o.deps.Log.Global().Info(op, "a deploy hook asked for a rollout with no deploy named; it starts now", nil)
		o.deps.Rollout.Landed(ctx, nil)
		return false, nil
	}
	o.mu.Lock()
	runs := make([]*run, 0, len(o.runsByWorkspace))
	for _, r := range o.runsByWorkspace {
		runs = append(runs, r)
	}
	o.mu.Unlock()
	var found *run
	for _, r := range runs {
		r.mu.Lock()
		match := r.lease.ID == ledger
		r.mu.Unlock()
		if match {
			found = r
			break
		}
	}
	if found == nil {
		err := refuse(ArmUnknownDeploy, "", "no deploy running in this daemon has ledger %s", ledger)
		o.deps.Log.Global().Warn(op, "refused a rollout for a deploy this daemon is not running", dlog.Context{"ledger": string(ledger)})
		return false, err
	}
	found.mu.Lock()
	found.rolloutRequested = true
	found.mu.Unlock()
	o.log(ctx, found.ws).Info(op, "registered the rollout of the deploy's landing; it fires at the deploy's teardown", dlog.Context{
		"workspace": string(found.ws), "ledger": string(ledger)})
	return true, nil
}

// ArmUnknownDeploy refuses a rollout naming no running deploy
// (RollOutLandingError.unknown_deploy).
const ArmUnknownDeploy = "unknown_deploy"

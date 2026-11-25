package merge

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/logging/logsite"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/testsclient"
	"agent-repld/internal/wsm"
)

// THE RULE SUITES RUN BEFORE AN AGENT'S MERGE IS ADMITTED. The repository's
// rule suites (Deps.RuleSuites, roster.RuleSuites in production: the checks that read the module's sources,
// such as gofmt, the source scans, the byte-compiler and shellcheck) are
// named by no workspace's targeted run, so until 2026-11-25 they first ran in
// the merge gate, where they failed about half of all failed gates (44 of 93
// between 2026-11-22 and 2026-11-25), each failure costing a whole gate run
// and a fixing turn. Enqueue now runs them on the branch's worktree, in the
// maker's name and on the test service, before anything is recorded, and
// refuses the merge with the failing cases named, so the maker fixes them in
// its own turn, with its own context.
//
// ONLY AN AGENT'S ASK IS CHECKED. The user's own finish from the editor has
// no agent turn waiting to fix a failure, and its RPC cannot hold the editor
// for the run; the gate still runs every suite on it.

// opRuleCheck is the operation every record of the rule check is filed under.
const opRuleCheck = "daemon.merge.rule_check"

// ruleCheckBound bounds one rule check's run: twice its measured run.
const ruleCheckBound = ruleCheckMeasured * 2

// ruleCheckMeasured is the rule suites' run on a real branch's worktree
// (pre-queue-rule-tests, 2026-11-25), nothing reused, under the host's CPU
// governor with other workspaces' runs going: 6m48s of wall, most of it
// waiting for CPU and a RAM disk seat, for 86 CPU-seconds of units (repo-scans
// 21.4s of units, webapp-rules 15.2, byte-compile 15.0, ert-rules 13.6,
// shellcheck 13.0). A rerun reuses every suite but repo-scans.
const ruleCheckMeasured = 6*time.Minute + 48*time.Second

// ruleCheckIntro is what a failing rule check's cases open with, in place of
// the gate's (gateFailingIntro).
const ruleCheckIntro = `The failing tests of the repository's rule suites, run on this branch before its merge was admitted.
The whole run is archived at %s.
Run exactly these while fixing, through the daemon, with the command given
for each group or the one that runs them all.
Then commit and ask for the merge again; it runs the rule suites again first.
`

// errRuleCheckTimedOut is a rule check whose run outlived its bound.
var errRuleCheckTimedOut = errors.New("merge: the rule check's run outlived its bound")

// checkRules runs the repository's rule suites on the source branch's
// worktree before an agent's merge is admitted: nil when they pass or the
// request is not one the check covers, a refusal naming the failing cases
// when they fail (ArmRuleChecksFailed), or naming the branch when no worktree
// holds it (ArmRuleChecksNoWorktree), and an error when the run could not be
// had at all.
func (o *orchestrator) checkRules(ctx context.Context, requester wsm.Workspace, req Request, source wsm.MergeSource, deploy deployTarget, fields dlog.Context) error {
	if req.By != RequestedByAgent {
		return nil
	}
	ws := requester.ID
	log := o.log(ctx, ws)
	// A REPOSITORY WHOSE DEPLOY RUNS NO AGENT-REPL SUITES HAS NO RULE SUITES:
	// the roster is agent-repl's own.
	script := lifecycleconfig.TestScript(deploy.cfg)
	if script == "" {
		log.Debug(opRuleCheck, "the repository's deploy runs no agent-repl suites, so no rule suite runs before its merge", fields)
		return nil
	}
	tree, err := o.ruleCheckTree(ctx, requester, source)
	if err != nil {
		if _, refused := Refused(err); refused {
			logRefusal(log, opRuleCheck, logsite.Msg("refused a merge whose branch no worktree holds, so its rule suites could not run"), fields, err)
		} else {
			log.Error(opRuleCheck, "could not find the worktree the merge's rule suites run in", dlog.WithCause(fields, err))
		}
		return err
	}
	maker := ws
	if req.Detached != nil && req.Detached.Maker != "" {
		maker = req.Detached.Maker
	}
	name, err := o.workspaceName(ctx, opRuleCheck, maker)
	if err != nil {
		return err
	}
	suites := o.deps.RuleSuites()
	if len(suites) == 0 {
		log.Debug(opRuleCheck, "the roster declares no rule suite, so none runs before the merge", fields)
		return nil
	}
	fields = dlog.Merge(fields, dlog.Context{"maker": string(maker), "worktree": tree, "suites": strings.Join(suites, ","), "bound_seconds": ruleCheckBound.Seconds()})
	argv := append(o.deps.TestCommand(tree, script, name), "--suites", strings.Join(suites, ","))
	log.Info(opRuleCheck, "the merge's rule suites started on the branch's worktree", fields)
	output, end, err := o.runRuleCheck(ctx, log, tree, argv, name, ruleCheckBound, fields)
	if err != nil {
		return err
	}
	archive := filepath.Join(LogsDir(o.deps.StateDir), fmt.Sprintf("rule_check-%d-0.log", o.deps.Now().UnixNano()))
	if err := writeLog(archive, output); err != nil {
		log.Error(opRuleCheck, "the rule check's output could not be archived; the merge is not admitted", dlog.WithCause(withField(fields, "archive", archive), err))
		return err
	}
	fields = dlog.Merge(fields, dlog.Context{"archive": archive, "exit_code": end.ExitCode, "cpu_seconds": end.CPU.Seconds(), "wall_seconds": end.Wall.Seconds()})
	if end.Lost {
		err := fmt.Errorf("merge: the test service lost the rule check's run: %s", end.LostDetail)
		log.Error(opRuleCheck, "the test service lost the rule check's run; the merge is not admitted", dlog.WithCause(fields, err))
		return err
	}
	if end.ExitCode == 0 {
		log.Info(opRuleCheck, "the merge's rule suites passed; the merge goes on to be admitted", fields)
		return nil
	}
	found := readFailingTests(output, tree)
	if err := o.recordFailingCases(ctx, maker, found, fields); err != nil {
		return err
	}
	refusal := refuse(ArmRuleChecksFailed, ws, "the branch failed its rule suites (%s) in %s; fix the cases named, commit, and ask again", strings.Join(suites, ", "), tree)
	refusal.Fields = map[string]any{"worktree": tree, "suites": suites, "failing_tests": found.render(ruleCheckIntro, archive), "log": archive}
	logRefusal(log, opRuleCheck, logsite.Msg("refused a merge whose branch failed its rule suites"), fields, refusal)
	return refusal
}

// runRuleCheck runs ARGV in TREE on the test service, bounded by BOUND
// (ruleCheckBound), and answers its output and how it ended. A run the bound
// cut, or one the caller's context cut, is stopped: nothing will read it.
func (o *orchestrator) runRuleCheck(ctx context.Context, log dlog.Logger, tree string, argv []string, name string, bound time.Duration, fields dlog.Context) (string, testsclient.End, error) {
	bounded, cancel := context.WithTimeoutCause(ctx, bound, errRuleCheckTimedOut)
	defer cancel()
	run, err := o.deps.Tests.Start(bounded, testsclient.Spec{Dir: tree, Argv: argv, Env: o.deps.GateEnv, Label: name + " rule checks"})
	if err != nil {
		log.Error(opRuleCheck, "the rule check's run could not be started on the test service; the merge is not admitted", dlog.WithCause(fields, err))
		return "", testsclient.End{}, fmt.Errorf("merge: starting the rule check's run: %w", err)
	}
	fields = withField(fields, "run", run)
	var output strings.Builder
	end, err := o.deps.Tests.Watch(bounded, run, 0, func(_ uint64, line string) {
		output.WriteString(line + "\n")
	})
	if err != nil {
		if stopErr := o.deps.Tests.Stop(context.WithoutCancel(ctx), run); stopErr != nil && !errors.Is(stopErr, testsclient.ErrUnknownRun) {
			log.Error(opRuleCheck, "could not stop a rule check's run nothing watches; it runs on", dlog.WithCause(fields, stopErr))
		}
		if errors.Is(context.Cause(bounded), errRuleCheckTimedOut) {
			err = fmt.Errorf("%w (%s, measured %s)", errRuleCheckTimedOut, bound, ruleCheckMeasured)
			log.Error(opRuleCheck, "the rule check's run outlived its bound and was stopped; the merge is not admitted", dlog.WithCause(fields, err))
			return "", testsclient.End{}, err
		}
		log.Error(opRuleCheck, "could not watch the rule check's run; it was stopped and the merge is not admitted", dlog.WithCause(fields, err))
		return "", testsclient.End{}, fmt.Errorf("merge: watching the rule check's run %s: %w", run, err)
	}
	return output.String(), end, nil
}

// ruleCheckTree answers the worktree a merge's rule suites run in: the
// requester's own for a finish (its own branch is checked out there), and
// for an async merge the worktree that has its branch checked out, refused
// when none does.
func (o *orchestrator) ruleCheckTree(ctx context.Context, requester wsm.Workspace, source wsm.MergeSource) (string, error) {
	if source.Kind == wsm.MergeSourceOwnBranch {
		return requester.Dir, nil
	}
	dir, _, held, err := worktreeHolding(ctx, o.deps.Git, requester.Dir, source.Branch)
	if err != nil {
		return "", err
	}
	if held {
		return dir, nil
	}
	refusal := refuse(ArmRuleChecksNoWorktree, requester.ID, "no worktree has the branch %q checked out, so its rule suites have nowhere to run; check it out in the maker's worktree and ask again", source.Branch)
	refusal.Fields = map[string]any{"branch": source.Branch}
	return "", refusal
}

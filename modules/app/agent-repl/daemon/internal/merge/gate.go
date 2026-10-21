package merge

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/cpugov"
	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/footer"
)

// This file is the gate's RUN: the selected suites run on the rebased branch
// in its worktree, their edges streamed to the footer (the testing step's
// activity line) and to the bubble's tests tab as the script writes them, and the whole output written to the round's test
// log as it runs.
//
// THERE IS NO FLAKE RE-RUN: a failure goes to the fixing attempts. And A
// BROKEN GATE IS NOT A TEST FAILURE: a gate that could not run -- its script is
// not there, it could not be started, or the shell could not find or execute
// its command (exit 127 or 126) -- is not something the branch's author can
// repair, so the merge FAILS at once (area other) with a plain line and no
// fixing attempt (2026-09-28: three repair rounds on an exit-127 gate).

// gateVerdict is one gate run's answer: the run itself, or -- broken set -- a
// gate that could not run at all.
type gateVerdict struct {
	result GateResult
	round  int
	// broken is the failure line of a gate that FAILED TO RUN, empty when it
	// ran (to a pass or a failure).
	broken string
}

// gate runs the test gate on the rebased branch at head, rebased onto tip, in
// the tests round the caller opened (or, on a resume, the round the restart
// left live: the gate runs again from its start in it).
//
// ONE GATE OF A REPOSITORY RUNS AT A TIME (testturn.go). A gate that runs
// anything first takes its repository's test turn, waiting in line for it,
// and gives it back on every way out of here.
//
// THE GOVERNOR COUNTS EVERY GATE, and admits it only once it holds the turn.
// A gate behind the head (adm set) is governed; one whose merge was promoted
// before the governor admitted it answers errPromoted, its round settled, and
// runs again as the head's. The permit is released when the gate's script
// ends.
func (r *run) gate(ctx context.Context, round tabRound, tip, head string, adm *gateAdmission) (verdict gateVerdict, err error) {
	const op = "daemon.merge.tests"
	var permit *cpugov.Permit
	released := false
	release := func() {
		if !released && permit != nil {
			released = true
			permit.Release()
		}
	}
	defer release()
	begun := r.o.deps.Now()
	dir := r.subject.dir
	rangeSpec := tip + ".." + head
	g := &gateRun{r: r, round: round, log: r.o.testLog(r.lease.ID, round.n),
		states: map[string]suiteState{}, sources: map[string]string{}, counts: newSuiteCounts(), release: release, begun: begun}
	paths, err := r.git.ChangedPaths(ctx, dir, rangeSpec)
	if err != nil && (r.stopping(err) || r.cut(err)) {
		return gateVerdict{}, err
	}
	// A READ CUT BY THE GATE'S OWN END is no unreadable branch: the merge
	// ahead moved, or the gate's admission was taken back, and the gate ends
	// the way a cut wait for its turn does, with nothing to run.
	if err != nil && cutUnder(ctx, err) {
		return gateVerdict{}, g.unrun(ctx, err)
	}
	var selection SuiteSelection
	if err != nil {
		r.o.log(ctx, r.ws).Warn(op, "could not read the rebased branch's paths; every suite runs",
			dlog.Context{"workspace": string(r.ws), "range": rangeSpec, "error": err.Error()})
		selection = FullSelection(fmt.Sprintf("the changed paths of %s could not be read, so every suite runs", rangeSpec))
	} else {
		selection = SelectSuites(paths)
	}
	r.o.log(ctx, r.ws).Debug(op, "selected the merge's suites", dlog.Context{
		"workspace": string(r.ws), "suites": strings.Join(selection.Suites, ","),
		"full": selection.Full, "reason": selection.Reason})
	if selection.Empty() {
		return r.gateNothing(ctx, round, paths, selection, begun)
	}
	r.setStep(ctx, footer.StepTesting, nil)
	// THE RUN IS A SUBTAB OF THE ONE TESTS TAB, which stands live again for it.
	r.openTesting(round)
	defer func() { r.o.releaseTestTurn(context.WithoutCancel(ctx), r, gateEnded(ctx, verdict, err)) }()
	for permit == nil {
		if err := r.awaitTestTurn(ctx, round); err != nil {
			return gateVerdict{}, g.unrun(ctx, err)
		}
		permit, err = r.admitGate(ctx, adm)
		if errors.Is(err, errTurnToHead) {
			r.o.log(ctx, r.ws).Info(op, "the merge gives its test turn to its head, which stands in line: the governor has not admitted its gate yet", dlog.Context{
				"workspace": string(r.ws), "repo": string(r.repo), "round": round.n})
			r.o.releaseTestTurn(context.WithoutCancel(ctx), r, "gave the turn to its head before the governor admitted its gate")
			continue
		}
		if err != nil {
			return gateVerdict{}, g.unrun(ctx, err)
		}
	}
	// EVERY EDGE OF A GATE IS RECORDED AT INFO, timed: its start, the host's
	// suite slot taken, the runner's end, the verdict, and a cut. A tests tab
	// that ran long is explained from these, its interval included when it
	// spans a restart and the archive holds only the last run.
	speculative := r.isSpeculative()
	r.o.log(ctx, r.ws).Info(op, "the merge's test gate started", dlog.Context{
		"workspace": string(r.ws), "round": round.n, "suites": strings.Join(selection.Suites, ","),
		"full": selection.Full, "reason": selection.Reason, "speculative": speculative,
		"since_gate_start_seconds": g.since()})
	r.publishRun(round, round.live(), nil, nil)

	argv := r.o.deps.TestCommand(dir, r.target.testScript())
	if len(argv) == 0 {
		return g.brokenGate(ctx, GateResult{}, "no test command is configured for it"), nil
	}
	script := argv[len(argv)-1]
	if _, err := os.Stat(script); err != nil {
		return g.brokenGate(ctx, GateResult{}, fmt.Sprintf("its script %s is not there", script)), nil
	}
	if err := validateSuites(selection.Suites); err != nil {
		return gateVerdict{}, err
	}
	// A speculative gate (one behind the head) runs niced; see gateArgv.
	argv = gateArgv(append([]string(nil), argv...), speculative)
	// The FULL selection passes no narrowing: running everything is the
	// script's own default, and a repository whose script knows no --suites
	// flag is never handed one.
	if !selection.Full {
		argv = append(argv, "--suites", strings.Join(selection.Suites, ","))
	}
	result, err := g.run(ctx, dir, argv, selection.Suites)
	if err != nil {
		// A GATE THE DAEMON'S OWN EXIT STOPPED FROM STARTING IS NOT BROKEN: a
		// cancelled run or a draining daemon takes the ordinary error path,
		// which records the exit rather than a fault in the gate.
		var unstarted *gateUnstartedError
		if errors.As(err, &unstarted) && ctx.Err() == nil && !r.o.isDraining() {
			return g.brokenGate(ctx, GateResult{}, fmt.Sprintf("it could not be started (%v)", unstarted.err)), nil
		}
		return gateVerdict{}, g.unrun(ctx, err)
	}
	if why, broken := gateDidNotRun(result.ExitCode); broken {
		return g.brokenGate(ctx, result, fmt.Sprintf("%s; the whole run is archived at %s", why, result.ArchivePath)), nil
	}
	if result.Passed {
		r.endRun(ctx, round, "", result.Suites, g.link(), "succeeded")
		r.o.log(ctx, r.ws).Info(op, "the merge's suites passed", dlog.Context{
			"workspace": string(r.ws), "round": round.n, "archive": result.ArchivePath, "verdict": "passed",
			"total_seconds": g.since()})
		return gateVerdict{result: result, round: round.n}, nil
	}
	// A FAILED RUN LEAVES THE TESTS TAB LIVE: a fixing attempt follows it, or
	// the caller settles the tab as given up.
	summary := fmt.Sprintf("the test suite failed (exit %d); the whole run is archived at %s", result.ExitCode, result.ArchivePath)
	r.publishRun(round, round.settled(r.o.nowMS(), summary), result.Suites, g.link())
	r.closeTab(ctx, round, "failed")
	r.o.log(ctx, r.ws).Info(op, "the merge's suites failed", dlog.Context{
		"workspace": string(r.ws), "round": round.n, "exit_code": result.ExitCode, "archive": result.ArchivePath,
		"verdict": "failed", "total_seconds": g.since()})
	return gateVerdict{result: result, round: round.n}, nil
}

// gateAdmission is how a gate behind its chain's head asks the governor to
// admit it: under ctx, which a promotion cuts with errPromoted, at its rank.
type gateAdmission struct {
	ctx  context.Context
	rank int
}

// admitGate takes the governor's permit for the gate, holding its
// repository's test turn: a head's at once, a governed one behind the head
// once the governor admits it. A governed wait the head's place in line cut
// answers errTurnToHead; a merge behind the head promoted before it was
// admitted answers errPromoted, and its gate runs as the head's instead,
// from the head's own steps.
func (r *run) admitGate(ctx context.Context, adm *gateAdmission) (*cpugov.Permit, error) {
	if adm != nil {
		if !r.isSpeculative() {
			return nil, errPromoted
		}
		actx, cut := context.WithCancelCause(adm.ctx)
		stop := r.o.watchHeadInLine(actx, r, cut)
		permit, err := r.o.deps.Governor.Acquire(actx, cpugov.Request{Workspace: r.ws, Repo: string(r.repo), Rank: adm.rank})
		turnToHead := errors.Is(context.Cause(actx), errTurnToHead)
		stop()
		switch {
		case err == nil:
			return permit, nil
		case turnToHead && ctx.Err() == nil:
			return nil, errTurnToHead
		case errors.Is(context.Cause(adm.ctx), errPromoted) && ctx.Err() == nil:
			return nil, errPromoted
		}
		return nil, fmt.Errorf("merge: admitting the gate behind the head: %w", err)
	}
	permit, err := r.o.deps.Governor.Acquire(ctx, cpugov.Request{Workspace: r.ws, Repo: string(r.repo), Head: true})
	if err != nil {
		return nil, fmt.Errorf("merge: admitting the head's gate: %w", err)
	}
	return permit, nil
}

// unrun settles a gate that ended with ERR before its verdict -- cut while it
// waited for its turn, for the governor, or while it ran -- and answers ERR. A
// gate whose base moved was cut on purpose and runs again on the new base; a
// gate a suspension stopped (the daemon's exit, a restart, a transfer or the
// queue's pause) runs again from its start when the merge resumes; any other
// could not run.
func (g *gateRun) unrun(ctx context.Context, err error) error {
	r, round := g.r, g.round
	if errors.Is(err, errPromoted) {
		g.cut(ctx, "the merge's promotion to its queue's head", "it runs again from its start at the head")
		const promoted = "the merge reached its queue's head; its tests run there"
		r.endRun(ctx, round, promoted, nil, g.link(), "restarted")
		return err
	}
	if errors.Is(context.Cause(ctx), errBaseMoved) {
		g.cut(ctx, "the merge ahead moved", "it runs again from its start on the new tip")
		const moved = "the merge ahead moved; the tests run again on its new tip"
		r.endRun(ctx, round, moved, nil, g.link(), "restarted")
		return err
	}
	// THE CUT NAMES WHAT CUT IT. A suspension names itself (suspendedFor),
	// so a pause is never told as an exit; a stop the daemon's exit began
	// before the run was marked is the exit's.
	if why, suspended := r.suspendedFor(); suspended {
		g.cut(ctx, why.by, why.gateRerun)
	} else if r.o.isDraining() || errors.Is(err, errMergeStopping) {
		g.cut(ctx, suspendExit.by, suspendExit.gateRerun)
	} else if cause := r.abandonedFor(); cause != "" {
		g.cut(ctx, "the merge's abandonment ("+string(cause)+")", "the merge ends, so it does not run again")
	} else if ctx.Err() != nil {
		g.cut(ctx, "its run's cancellation ("+context.Cause(ctx).Error()+")", "the merge ends, so it does not run again")
	}
	const unrun = "the test gate could not run"
	r.endRun(ctx, round, unrun, nil, g.link(), "failed")
	return err
}

// gateDidNotRun reads the exit statuses that mean the gate's command never
// ran: the shell's own "command not found" (127) and "not executable" (126).
func gateDidNotRun(code int) (string, bool) {
	switch code {
	case 127:
		return "its command was not found (exit 127)", true
	case 126:
		return "its command could not be executed (exit 126)", true
	}
	return "", false
}

// gateNothing passes a gate whose selection is EMPTY without starting the
// runner (owner ruling, 2026-10-12): nothing the branch changed is something a
// suite tests. It is still drawn truthfully: the testing step, and a tests tab
// settled with no suite rows whose log says no suite was selected and why.
func (r *run) gateNothing(ctx context.Context, round tabRound, paths []string, selection SuiteSelection, begun time.Time) (gateVerdict, error) {
	log := r.o.testLog(r.lease.ID, round.n)
	r.setStep(ctx, footer.StepTesting, nil)
	// THE RUN IS A SUBTAB OF THE ONE TESTS TAB, which stands live again for it.
	r.openTesting(round)
	body := fmt.Sprintf("no suite selected: %s\nchanged paths (%d):\n", selection.Reason, len(paths))
	for _, path := range paths {
		body += "  " + path + "\n"
	}
	body += "nothing ran.\n"
	if err := writeTestLog(log, body); err != nil {
		return gateVerdict{}, err
	}
	r.endRun(ctx, round, "", nil, r.o.testLogLink(log), "succeeded")
	r.o.log(ctx, r.ws).Info("daemon.merge.tests", "no suite is selected for the merge's change; the gate passes without starting the runner", dlog.Context{
		"workspace": string(r.ws), "round": round.n, "paths": strings.Join(paths, ","), "reason": selection.Reason, "archive": log.path,
		"verdict": "no suite selected", "total_seconds": r.o.deps.Now().Sub(begun).Seconds()})
	return gateVerdict{result: GateResult{Passed: true, ArchivePath: log.path}, round: round.n}, nil
}

// gateRun is one gate run's live account.
type gateRun struct {
	r     *run
	round tabRound
	log   testLog
	// written reports that the log file exists, so the tab may link it.
	written bool

	// release gives the governor's permit back.
	release func()
	// begun is when the gate began; runnerSeen reports that the test
	// runner's first line was recorded (guarded by mu).
	begun      time.Time
	runnerSeen bool

	mu sync.Mutex
	// suites are the tab's live rows, in the order the suites started.
	suites []*frontendv1.FeedMergeTestSuite
	// states are each started suite's standing, by name.
	states map[string]suiteState
	// sources are the earlier runs the reused suites name, by suite.
	sources map[string]string
	// counts are each suite's unit counts as the lines have reported them.
	counts *suiteCounts
}

// link is the round's log link, nil until the run has written the log.
func (g *gateRun) link() *frontendv1.FeedMergeTestLog {
	if !g.written {
		return nil
	}
	return g.r.o.testLogLink(g.log)
}

// run runs the script, writing its output to the round's log and following
// its suites' edges as the script writes them, and answers the classified
// result. An error means the run could not be CLASSIFIED: the script could not
// be spawned, or its log could not be written.
func (g *gateRun) run(ctx context.Context, dir string, argv, selected []string) (GateResult, error) {
	file, err := createTestLog(g.log)
	if err != nil {
		return GateResult{}, err
	}
	g.written = true
	writer := bufio.NewWriter(file)
	var writeErr error
	g.r.publishRun(g.round, g.round.live(), nil, g.link())
	measured, runErr := g.r.o.deps.TestRunner.RunLinesMeasured(ctx, dir, argv, func(line string) {
		if _, err := writer.WriteString(line + "\n"); err != nil && writeErr == nil {
			writeErr = err
		}
		g.edge(line)
	})
	g.release()
	if runErr == nil {
		g.r.o.log(ctx, g.r.ws).Info("daemon.merge.tests", "the merge's test runner finished", dlog.Context{
			"workspace": string(g.r.ws), "round": g.round.n, "exit_code": measured.ExitCode,
			"run_seconds": measured.Wall.Seconds(), "since_gate_start_seconds": g.since()})
	}
	output, code := measured.Output, measured.ExitCode
	if err := writer.Flush(); err != nil && writeErr == nil {
		writeErr = err
	}
	if err := file.Close(); err != nil && writeErr == nil {
		writeErr = err
	}
	if runErr != nil {
		return GateResult{}, &gateUnstartedError{err: runErr}
	}
	// A GATE WHOSE LOG FAILED IS AN UNRUNNABLE GATE rather than a lost log,
	// because the file is the only account of a failure that survives the run.
	if writeErr != nil {
		return GateResult{}, fmt.Errorf("merge: writing the test log %s: %w", g.log.path, writeErr)
	}
	suites, err := g.r.o.paintSuites(selected, output)
	if err != nil {
		return GateResult{}, err
	}
	return GateResult{
		Passed:      code == 0,
		Suites:      suites,
		Tail:        clampTail(output, tailBytes),
		ArchivePath: g.log.path,
		ExitCode:    code,
	}, nil
}

// edge follows one line of the script: a suite starting, passing or failing
// moves its tab row and takes the testing step's line, and a unit's verdict
// or a suite's planned total moves the suite's counts.
func (g *gateRun) edge(line string) {
	g.runnerEdge(line)
	if name, counted := g.countLine(line); counted {
		g.mu.Lock()
		state, started := g.states[name]
		if started {
			g.setTabSuite(name, state)
		}
		suites := append([]*frontendv1.FeedMergeTestSuite(nil), g.suites...)
		g.mu.Unlock()
		if started {
			g.r.publishRun(g.round, g.round.live(), suites, g.link())
		}
		return
	}
	name, state, ok := suiteEdge(line)
	if !ok {
		return
	}
	g.mu.Lock()
	g.states[name] = state
	if _, source, reused := reusedSuite(line); reused {
		g.sources[name] = source
	}
	g.setTabSuite(name, state)
	suites := append([]*frontendv1.FeedMergeTestSuite(nil), g.suites...)
	g.mu.Unlock()
	// A REUSED SUITE TAKES NO FOOTER EDGE (owner ruling, 2026-10-14): it
	// neither started nor ended in this gate.
	if state != suiteStateReused {
		g.r.updateFacts(func(f *footer.MergeFacts) {
			f.Line = suiteLine(name, state)
		})
	}
	g.r.publishRun(g.round, g.round.live(), suites, g.link())
}

// runnerEdge records, once, the test runner's first line: the moment the
// gate's run actually began running suites.
func (g *gateRun) runnerEdge(line string) {
	if !strings.Contains(line, testrunPrefix) {
		return
	}
	g.mu.Lock()
	seen := g.runnerSeen
	g.runnerSeen = true
	g.mu.Unlock()
	if seen {
		return
	}
	g.r.o.log(context.Background(), g.r.ws).Info("daemon.merge.tests", "the merge's test runner began",
		dlog.Context{"workspace": string(g.r.ws), "round": g.round.n, "since_gate_start_seconds": g.since()})
}

// testrunPrefix opens every line the test runner itself prints.
const testrunPrefix = "[agent-repl-tests]"

// since is the seconds since the gate began.
func (g *gateRun) since() float64 { return g.r.o.deps.Now().Sub(g.begun).Seconds() }

// cut records a gate cut BY something before its verdict, and THEN, what
// becomes of the gate.
func (g *gateRun) cut(ctx context.Context, by, then string) {
	g.r.o.log(ctx, g.r.ws).Info("daemon.merge.tests", "the merge's test gate was cut before its verdict by "+by+"; "+then, dlog.Context{
		"workspace": string(g.r.ws), "round": g.round.n, "by": by, "since_gate_start_seconds": g.since()})
}

// countLine moves the counts one line reports, answering the suite it moved.
// A line it cannot read is recorded at WARN and moves nothing: the run goes
// on, and the gate's final parse of the whole output fails it loudly.
func (g *gateRun) countLine(line string) (string, bool) {
	g.mu.Lock()
	name, counted, err := g.counts.take(line)
	g.mu.Unlock()
	if err != nil {
		g.r.o.log(context.Background(), g.r.ws).Warn("daemon.merge.tests", "a test runner line could not be counted",
			dlog.Context{"workspace": string(g.r.ws), "round": g.round.n, "line": line, "error": err.Error()})
		return "", false
	}
	return name, counted
}

// setTabSuite stands a suite's tab row at its edge, with its counts so far.
// Called with g.mu held.
func (g *gateRun) setTabSuite(name string, state suiteState) {
	suite := tabSuite(name, state, g.counts.of(name), g.sources[name])
	for i, existing := range g.suites {
		if existing.GetName() == name {
			g.suites[i] = suite
			return
		}
	}
	g.suites = append(g.suites, suite)
}

// suiteEdge reads one line of the script as a suite's edge: starting, passed,
// failed or declined.
func suiteEdge(line string) (string, suiteState, bool) {
	if m := suiteStarting.FindStringSubmatch(line); m != nil {
		return m[1], suiteStateRunning, true
	}
	if name, state, settled := settledSuite(line); settled {
		return name, state, true
	}
	return "", suiteStateRunning, false
}

// brokenGate settles the tests tab on a gate that failed to run and answers
// the failure line it names.
func (g *gateRun) brokenGate(ctx context.Context, result GateResult, why string) gateVerdict {
	line := "the test gate itself failed to run: " + why
	g.r.endRun(ctx, g.round, line, result.Suites, g.link(), "gate_broken")
	g.r.o.log(ctx, g.r.ws).Warn("daemon.merge.tests", "the test gate itself failed to run; the merge fails with no fixing attempt", dlog.Context{
		"workspace": string(g.r.ws), "round": g.round.n, "why": why, "exit_code": result.ExitCode, "archive": result.ArchivePath,
		"verdict": "broken", "total_seconds": g.since()})
	return gateVerdict{result: result, round: g.round.n, broken: line}
}

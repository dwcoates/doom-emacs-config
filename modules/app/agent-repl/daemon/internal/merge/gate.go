package merge

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/resolve/footer"
)

// This file is the gate's RUN: the selected suites run on the rebased branch
// in its worktree, their edges streamed to the footer (the testing step's
// activity line and the merge tests panel) and to the bubble's tests tab as
// the script writes them, and the whole output written to the round's test
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

// gate runs the test gate on the rebased branch at head, rebased onto tip.
func (r *run) gate(ctx context.Context, tip, head string) (gateVerdict, error) {
	const op = "daemon.merge.tests"
	dir := r.subject.dir
	rangeSpec := tip + ".." + head
	paths, err := r.o.deps.Git.ChangedPaths(ctx, dir, rangeSpec)
	if err != nil {
		r.o.log(ctx, r.ws).Warn(op, "could not read the rebased branch's paths; every suite runs",
			dlog.Context{"workspace": string(r.ws), "range": rangeSpec, "error": err.Error()})
		paths = nil
	}
	selection := SelectSuites(paths)
	r.o.log(ctx, r.ws).Debug(op, "selected the merge's suites", dlog.Context{
		"workspace": string(r.ws), "suites": strings.Join(selection.Suites, ","),
		"full": selection.Full, "reason": selection.Reason})

	round := r.openTab(ctx, TabTests)
	g := &gateRun{r: r, round: round, log: r.o.testLog(r.lease.ID, round), started: map[string]time.Time{}}
	rows := make([]*frontendv1.FooterMergeTestRow, 0, len(selection.Suites))
	for _, suite := range selection.Suites {
		rows = append(rows, testRow(suite, waitingRowState()))
	}
	r.setStep(ctx, footer.StepTesting, func(f *footer.MergeFacts) {
		f.Tests = rows
		f.TestsRound++
	})
	r.upsert(TabTests, round, testsTab(live(), nil, nil, 0, ""))

	argv := r.o.deps.TestCommand(dir)
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
	argv = append([]string(nil), argv...)
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
		r.upsert(TabTests, round, testsTab(nil, nil, g.link(), r.o.nowMS(), "the test gate could not run"))
		r.closeTab(ctx, TabTests, round, "failed")
		return gateVerdict{}, err
	}
	if why, broken := gateDidNotRun(result.ExitCode); broken {
		return g.brokenGate(ctx, result, fmt.Sprintf("%s; the whole run is archived at %s", why, result.ArchivePath)), nil
	}
	if result.Passed {
		r.upsert(TabTests, round, testsTab(nil, result.Suites, g.link(), r.o.nowMS(), ""))
		r.closeTab(ctx, TabTests, round, "succeeded")
		r.o.log(ctx, r.ws).Debug(op, "the merge's suites passed", dlog.Context{
			"workspace": string(r.ws), "round": round, "archive": result.ArchivePath})
		return gateVerdict{result: result, round: round}, nil
	}
	summary := fmt.Sprintf("the test suite failed (exit %d); the whole run is archived at %s", result.ExitCode, result.ArchivePath)
	r.upsert(TabTests, round, testsTab(nil, result.Suites, g.link(), r.o.nowMS(), summary))
	r.closeTab(ctx, TabTests, round, "failed")
	r.o.log(ctx, r.ws).Warn(op, "the merge's suites failed", dlog.Context{
		"workspace": string(r.ws), "round": round, "exit_code": result.ExitCode, "archive": result.ArchivePath})
	return gateVerdict{result: result, round: round}, nil
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

// gateRun is one gate run's live account.
type gateRun struct {
	r     *run
	round int
	log   testLog
	// written reports that the log file exists, so the tab may link it.
	written bool

	mu sync.Mutex
	// started is when each suite started, which its clock is read from.
	started map[string]time.Time
	// suites are the tab's live rows, in the order the suites started.
	suites []*frontendv1.FeedMergeTestSuite
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
	if err := os.MkdirAll(filepath.Dir(g.log.path), 0o755); err != nil {
		return GateResult{}, fmt.Errorf("merge: creating the test log directory %s: %w", filepath.Dir(g.log.path), err)
	}
	file, err := os.Create(g.log.path)
	if err != nil {
		return GateResult{}, fmt.Errorf("merge: creating the test log %s: %w", g.log.path, err)
	}
	g.written = true
	writer := bufio.NewWriter(file)
	var writeErr error
	g.r.upsert(TabTests, g.round, testsTab(live(), nil, g.link(), 0, ""))
	output, code, runErr := g.r.o.deps.TestRunner.RunLines(ctx, dir, argv, func(line string) {
		if _, err := writer.WriteString(line + "\n"); err != nil && writeErr == nil {
			writeErr = err
		}
		g.edge(line)
	})
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
// moves its panel row and its tab row, and takes the testing step's line.
func (g *gateRun) edge(line string) {
	name, state, ok := suiteEdge(line)
	if !ok {
		return
	}
	now := g.r.o.deps.Now()
	g.mu.Lock()
	var row *frontendv1.FooterMergeTestRowState
	switch state {
	case suiteStatePassed, suiteStateFailed:
		began, known := g.started[name]
		if !known {
			began = now
		}
		row = settledRowState(state == suiteStatePassed, now.Sub(began))
	default:
		g.started[name] = now
		row = runningRowState(now)
	}
	g.setTabSuite(name, state)
	suites := append([]*frontendv1.FeedMergeTestSuite(nil), g.suites...)
	g.mu.Unlock()
	g.r.updateFacts(func(f *footer.MergeFacts) {
		for i, existing := range f.Tests {
			if existing.GetName().GetText() == name {
				f.Tests[i] = testRow(name, row)
			}
		}
		f.Line = suiteLine(name, state)
	})
	g.r.upsert(TabTests, g.round, testsTab(live(), suites, g.link(), 0, ""))
}

// setTabSuite stands a suite's tab row at its edge. Called with g.mu held.
func (g *gateRun) setTabSuite(name string, state suiteState) {
	suite := &frontendv1.FeedMergeTestSuite{Name: name}
	switch state {
	case suiteStatePassed:
		suite.State = &frontendv1.FeedMergeTestSuite_Passed{Passed: &frontendv1.FeedMergeTestSuitePassed{}}
	case suiteStateFailed:
		suite.State = &frontendv1.FeedMergeTestSuite_Failed{Failed: &frontendv1.FeedMergeTestSuiteFailed{}}
	default:
		suite.State = &frontendv1.FeedMergeTestSuite_Running{Running: &frontendv1.FeedMergeTestSuiteRunning{}}
	}
	for i, existing := range g.suites {
		if existing.GetName() == name {
			g.suites[i] = suite
			return
		}
	}
	g.suites = append(g.suites, suite)
}

// suiteEdge reads one line of the script as a suite's edge: starting, passed
// or failed.
func suiteEdge(line string) (string, suiteState, bool) {
	if m := suiteStarting.FindStringSubmatch(line); m != nil {
		return m[1], suiteStateRunning, true
	}
	if name, settled := settledSuite(line); settled {
		if suitePassed.MatchString(line) {
			return name, suiteStatePassed, true
		}
		return name, suiteStateFailed, true
	}
	return "", suiteStateRunning, false
}

// brokenGate settles the tests tab on a gate that failed to run and answers
// the failure line it names.
func (g *gateRun) brokenGate(ctx context.Context, result GateResult, why string) gateVerdict {
	line := "the test gate itself failed to run: " + why
	g.r.upsert(TabTests, g.round, testsTab(nil, result.Suites, g.link(), g.r.o.nowMS(), line))
	g.r.closeTab(ctx, TabTests, g.round, "gate_broken")
	g.r.o.log(ctx, g.r.ws).Warn("daemon.merge.tests", "the test gate itself failed to run; the merge fails with no fixing attempt", dlog.Context{
		"workspace": string(g.r.ws), "round": g.round, "why": why, "exit_code": result.ExitCode, "archive": result.ArchivePath})
	return gateVerdict{result: result, round: g.round, broken: line}
}

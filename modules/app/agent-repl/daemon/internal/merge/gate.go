package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/resolve/footer"

	"agentrepl/testrun/seatwait"
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
// ONE GATE OF A REPOSITORY RUNS AT A TIME, STRUCTURALLY: only the merge
// holding its queue's testing slot -- the head, which the pump alone grants
// (slot.go) -- runs one, and a gate of any other run is the defect
// requireTestingSlot fails on.
func (r *run) gate(ctx context.Context, round tabRound, tip, head string) (verdict gateVerdict, err error) {
	const op = "daemon.merge.tests"
	r.o.requireTestingSlot(ctx, r, round)
	begun := r.o.deps.Now()
	dir := r.subject.dir
	rangeSpec := tip + ".." + head
	g := &gateRun{r: r, round: round, log: r.o.testLog(r.lease.ID, round.n),
		tracker: newSuiteTracker(r.o.nowMS), begun: begun}
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
	// EVERY EDGE OF A GATE IS RECORDED AT INFO, timed: its start, the host's
	// suite slot taken, the runner's end, the verdict, and a cut. A tests tab
	// that ran long is explained from these, its interval included when it
	// spans a restart and the archive holds only the last run.
	r.o.log(ctx, r.ws).Info(op, "the merge's test gate started", dlog.Context{
		"workspace": string(r.ws), "round": round.n, "suites": strings.Join(selection.Suites, ","),
		"full": selection.Full, "reason": selection.Reason,
		"since_gate_start_seconds": g.since()})
	r.publishRun(round, round.live(), nil, nil)

	name, err := r.o.workspaceName(ctx, op, r.ws)
	if err != nil {
		return gateVerdict{}, err
	}
	argv := r.o.deps.TestCommand(dir, r.target.testScript(), name)
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
	argv = gateArgv(append([]string(nil), argv...), r.o.deps.TestMetricsRoot)
	// The FULL selection passes no narrowing: running everything is the
	// script's own default, and a repository whose script knows no --suites
	// flag is never handed one.
	if !selection.Full {
		argv = append(argv, "--suites", strings.Join(selection.Suites, ","))
	}
	// A GATE PROFILES ONLY WHEN ITS MERGE ASKED (wsm.MergeSource.ProfileGate):
	// every unit it runs writes its CPU profiles, and those of the production
	// processes its tests start, into the profiles directory beside the run's
	// log, which the run's output names (testrun.v1.TestRunRequest.profile).
	if r.source.ProfileGate {
		argv = append(argv, "--profile")
		r.o.log(ctx, r.ws).Info(op, "the merge's test gate profiles its units, as its merge asked", dlog.Context{
			"workspace": string(r.ws), "round": round.n})
	}
	result, err := g.run(ctx, dir, argv, selection.Suites)
	if err != nil {
		// A GATE THE DAEMON'S OWN EXIT STOPPED FROM STARTING IS NOT THE HOST'S
		// FAULT: a cancelled run or a draining daemon takes the ordinary error
		// path, which records the exit rather than a fault.
		var unstarted *gateUnstartedError
		if errors.As(err, &unstarted) && ctx.Err() == nil && !r.o.isDraining() {
			return gateVerdict{}, g.hostFault(ctx, GateResult{}, fmt.Sprintf("the test runner could not be started: %v", unstarted.err))
		}
		return gateVerdict{}, g.unrun(ctx, err)
	}
	if result.ExitCode == gateRunnerUnavailable {
		return gateVerdict{}, g.hostFault(ctx, result, runnerUnavailableCause(result.Tail))
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

// unrun settles a gate that ended with ERR before its verdict -- cut before
// or while it ran -- and answers ERR. A gate a suspension stopped (the daemon's exit, a restart, a transfer or the
// queue's pause) runs again from its start when the merge resumes; any other
// could not run.
func (g *gateRun) unrun(ctx context.Context, err error) error {
	r, round := g.r, g.round
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

// gateRunnerUnavailable is the exit status of a test run whose runner could
// not be built or started (bin/test-all.sh RUNNER_UNAVAILABLE): no suite ran,
// so nothing of the branch was judged.
const gateRunnerUnavailable = 75

// runnerUnavailableCause names what the runner's build or start hit, from the
// run's TAIL: its last error lines, which name the cause ("too many open
// files in system"), and the script's own ERROR line. A tail with none of
// them names the exit itself.
func runnerUnavailableCause(tail string) string {
	var lines []string
	for _, line := range strings.Split(strings.TrimSpace(tail), "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "ERROR") || strings.Contains(line, "too many open files") {
			lines = append(lines, line)
		}
	}
	if len(lines) == 0 {
		return fmt.Sprintf("the test runner could not be built or started (exit %d)", gateRunnerUnavailable)
	}
	const keep = 3
	if len(lines) > keep {
		lines = lines[len(lines)-keep:]
	}
	return strings.Join(lines, "; ")
}

// hostFault settles a gate whose runner could not be built or started: a
// fault of the HOST, never a verdict on the branch. It is recorded at ERROR
// naming CAUSE, spends no fixing attempt, and holds the merge's queue in the
// host's name (holdForHostFault); the pause suspends this run, whose gate
// runs again from its start once the queue is resumed. It answers the error
// the run stops on: the pause's suspension, or the hold's own failure.
//
// 2026-11-14 20:47:31, resolved-vendor-notices-clear round 5: the runner's
// build hit "too many open files in system" in 0.1s, and the queue sent the
// merge to a fixing round with nothing to fix.
func (g *gateRun) hostFault(ctx context.Context, result GateResult, cause string) error {
	r := g.r
	r.o.log(ctx, r.ws).Error("daemon.merge.tests", "the test gate's runner could not be built or started on this host; nothing of the branch was judged, no fixing attempt is spent, and the queue is held until it is resumed", dlog.Context{
		"workspace": string(r.ws), "round": g.round.n, "cause": cause, "exit_code": result.ExitCode,
		"archive": result.ArchivePath, "verdict": "host_fault", "total_seconds": g.since()})
	// THE FAULTED RUN IS NEVER WATCHED AGAIN: the queue's resume runs the
	// gate again from its start, on a host whose fault was fixed, so the
	// record drops the run before the hold suspends the merge.
	if err := r.checkpoint(ctx, TabTests, func(d *progressDoc) { d.TestRun = "" }); err != nil {
		return g.unrun(ctx, err)
	}
	held := make(chan error, 1)
	inflight.Go(func() { held <- r.o.holdForHostFault(context.WithoutCancel(ctx), r.repo, cause) })
	// THE HOLD'S PAUSE CUTS THIS RUN before it finishes: the pause suspends
	// every running merge of the queue and waits for each to let go, so the
	// cut is what returns here. A hold that failed, or found the queue
	// already paused, answers first.
	select {
	case <-ctx.Done():
		return g.unrun(ctx, context.Cause(ctx))
	case err := <-held:
		var refused *RefusalError
		if errors.As(err, &refused) && refused.Arm == ArmAlreadyPaused {
			// ANOTHER PAUSE STANDS, and it suspends this run as it suspends
			// every running merge of its queue.
			<-ctx.Done()
			return g.unrun(ctx, context.Cause(ctx))
		}
		if err == nil {
			panic(fmt.Sprintf("merge: the host's hold of %s's queue returned before suspending its gate's run", r.ws))
		}
		r.o.log(ctx, r.ws).Error("daemon.merge.tests", "could not hold the queue for the host's fault; the merge fails on it", dlog.Context{
			"workspace": string(r.ws), "round": g.round.n, "cause": cause, "error": err.Error()})
		return g.unrun(ctx, fmt.Errorf("merge: hold the queue for the host's fault (%s): %w", cause, err))
	}
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
	if err := writeLog(log.path, body); err != nil {
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

	// begun is when the gate began; runnerSeen reports that the test
	// runner's first line was recorded (guarded by mu).
	begun      time.Time
	runnerSeen bool

	mu sync.Mutex
	// tracker follows the run's lines into the tab's tests view.
	tracker *suiteTracker
}

// publishLive publishes the live run as its tracker stands, with the log
// link.
func (g *gateRun) publishLive() {
	g.r.publishRunOutput(g.round, g.round.live(), g.tracker.view(g.link()))
}

// link is the round's log link, nil until the run has written the log.
func (g *gateRun) link() *frontendv1.FeedMergeTestLog {
	if !g.written {
		return nil
	}
	return g.r.o.testLogLink(g.log)
}

// run runs the script on the test service (runOnService), writing its
// output to the round's log and following its suites' edges as the script
// writes them, and answers the classified result. An error means the run
// could not be CLASSIFIED: the script could not be started, or its log could
// not be written.
func (g *gateRun) run(ctx context.Context, dir string, argv, selected []string) (GateResult, error) {
	for {
		// THE RUN IS ON THE TEST SERVICE ONLY WHILE NO RESTART OF IT HOLDS
		// THE GATES (testservicehold.go): a restart cuts it, and the gate
		// runs again from its start once the service answers.
		watch, leave, err := g.enterTestService(ctx)
		if err != nil {
			return GateResult{}, err
		}
		measured, lost, err := g.runOnService(watch, dir, argv)
		if err != nil && ctx.Err() == nil && errors.Is(context.Cause(watch), errTestServiceRestart) {
			err := g.takenOff(ctx)
			leave()
			if err != nil {
				return GateResult{}, err
			}
			g.reset()
			continue
		}
		leave()
		if err != nil {
			return GateResult{}, err
		}
		if lost != "" {
			// THE SERVICE ITSELF RESTARTED UNDER THE RUN: nothing the run
			// would have written is known, so the gate runs again from its
			// start, its tab and its log drawn afresh.
			g.r.o.log(ctx, g.r.ws).Info("daemon.merge.tests", "the test service lost the gate's run; the gate runs again from its start", dlog.Context{
				"workspace": string(g.r.ws), "round": g.round.n, "lost": lost})
			g.reset()
			continue
		}
		g.r.o.log(ctx, g.r.ws).Info("daemon.merge.tests", "the merge's test runner finished", dlog.Context{
			"workspace": string(g.r.ws), "round": g.round.n, "exit_code": measured.ExitCode,
			"run_seconds": measured.Wall.Seconds(), "since_gate_start_seconds": g.since()})
		suites, err := g.r.o.paintSuites(selected, measured.Output)
		if err != nil {
			return GateResult{}, err
		}
		return GateResult{
			Passed:      measured.ExitCode == 0,
			Suites:      suites,
			Tail:        clampTail(measured.Output, tailBytes),
			ArchivePath: g.log.path,
			ExitCode:    measured.ExitCode,
		}, nil
	}
}

// reset puts the gate run back to its start: no suite seen, no runner line,
// no seat wait.
func (g *gateRun) reset() {
	g.tracker.reset()
	g.mu.Lock()
	defer g.mu.Unlock()
	g.runnerSeen, g.written = false, false
}

// edge follows one line of the script through the run's tracker: a suite
// starting, passing or failing moves its tab row and takes the testing step's
// line, and a unit's verdict, a suite's planned total or a seat line moves
// the tab. A line the tracker cannot read is recorded and moves nothing.
func (g *gateRun) edge(line string) {
	g.runnerEdge(line)
	took, err := g.tracker.take(line)
	recordUnreadLine(g.r.o.log(context.Background(), g.r.ws), "daemon.merge.tests", line, err,
		dlog.Context{"workspace": string(g.r.ws), "round": g.round.n})
	// A REUSED SUITE TAKES NO FOOTER EDGE (owner ruling, 2026-10-14): it
	// neither started nor ended in this gate.
	if took.suite != "" && took.state != suiteStateReused {
		g.r.updateFacts(func(f *footer.MergeFacts) {
			f.Line = suiteLine(took.suite, took.state)
		})
	}
	if took.moved {
		g.publishLive()
	}
}

// seatWaitView is a runner's seat wait as the tests tab draws it, begun at
// startedMS.
func seatWaitView(w seatwait.Wait, startedMS int64) *frontendv1.FeedMergeTestsRunSeatWait {
	view := &frontendv1.FeedMergeTestsRunSeatWait{StartedAtMs: startedMS}
	switch w.Reason {
	case seatwait.Ceiling:
		view.Reason = &frontendv1.FeedMergeTestsRunSeatWait_Ceiling{Ceiling: &frontendv1.FeedMergeTestsRunSeatWaitCeiling{CeilingMib: w.CeilingMiB}}
	case seatwait.Memory:
		view.Reason = &frontendv1.FeedMergeTestsRunSeatWait_Memory{Memory: &frontendv1.FeedMergeTestsRunSeatWaitMemory{AvailableMib: w.AvailableMiB, FloorMib: w.FloorMiB}}
	case seatwait.Outranked:
		view.Reason = &frontendv1.FeedMergeTestsRunSeatWait_Outranked{Outranked: &frontendv1.FeedMergeTestsRunSeatWaitOutranked{}}
	default:
		panic("merge: a seat wait with a reason seatwait.Parse does not answer: " + string(w.Reason))
	}
	for _, h := range w.Holders {
		holder := &frontendv1.FeedMergeTestsRunSeatHolder{}
		if h.Workspace != "" {
			holder.Holder = &frontendv1.FeedMergeTestsRunSeatHolder_Workspace{Workspace: &frontendv1.FeedMergeTestsRunSeatHolderWorkspace{WorkspaceName: h.Workspace}}
		} else {
			holder.Holder = &frontendv1.FeedMergeTestsRunSeatHolder_Outside{Outside: &frontendv1.FeedMergeTestsRunSeatHolderOutside{Pid: uint32(h.Pid)}}
		}
		view.Holders = append(view.Holders, holder)
	}
	return view
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

// suiteEdge reads one line of the script as a suite's edge: starting, passed
// or failed.
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

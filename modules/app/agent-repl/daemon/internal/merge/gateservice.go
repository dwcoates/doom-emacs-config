package merge

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/scriptrunner"
	"agent-repld/internal/testsclient"
)

// THE GATE RUNS ON THE TEST SERVICE (testsclient), NEVER IN THE DAEMON. A
// daemon deploy stops the daemon that started a gate, and a gate run as the
// daemon's own child died with it: every deploy restarted the head's gate
// from its start (2026-11-16: merge-queue-cold-gate on its seventh round).
// The service outlives every daemon, so the round's run is recorded in the
// merge's progress the moment it starts (progressDoc.TestRun), and a merge
// resumed on the next daemon watches that very run, from its first line, to
// its end.

// runOnService runs ARGV in DIR on the test service, or watches the run this
// round already started there, writing every line to the round's log and
// following its edges. It answers the run as the gate reads it, or LOST
// naming why the service lost the run -- the gate then runs again from its
// start -- or an error: one wrapping gateUnstartedError for a run that could
// not be started or watched, any other for a log that could not be written.
//
// A WATCH CUT BY THE GATE'S OWN END STOPS THE RUN; ONE CUT BY THE DAEMON'S
// EXIT LEAVES IT. A gate whose merge was taken back or whose target moved
// wants its run no more, so the run is stopped. A daemon draining for its
// exit, or a run suspended at its step, leaves the run going: the merge
// resumes on the next daemon and watches it again.
func (g *gateRun) runOnService(ctx context.Context, dir string, argv []string) (scriptrunner.Run, string, error) {
	r := g.r
	file, err := createLog(g.log.path)
	if err != nil {
		return scriptrunner.Run{}, "", err
	}
	g.written = true
	writer := bufio.NewWriter(file)
	var writeErr error
	g.publishLive(nil)
	run, err := g.serviceRun(ctx, dir, argv)
	if err != nil {
		_ = file.Close()
		return scriptrunner.Run{}, "", err
	}
	var output strings.Builder
	end, watchErr := r.o.deps.Tests.Watch(ctx, run, 0, func(_ uint64, line string) {
		output.WriteString(line + "\n")
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
	fields := dlog.Context{"workspace": string(r.ws), "round": g.round.n, "run": run}
	switch {
	case watchErr != nil && ctx.Err() != nil:
		g.leaveOrStop(ctx, run, fields)
		return scriptrunner.Run{}, "", ctx.Err()
	case errors.Is(watchErr, testsclient.ErrUnknownRun):
		g.forgetRun()
		return scriptrunner.Run{}, "the test service holds no record of the run", nil
	case watchErr != nil:
		// A RUN THE GATE CANNOT WATCH IS ONE NO RESUME COULD TRUST: the
		// queue is held in the host's name (gateUnstartedError), and its
		// resume runs the gate again from its start.
		g.r.retireTestRun(ctx, "the gate could not watch its run")
		return scriptrunner.Run{}, "", &gateUnstartedError{err: fmt.Errorf("watch the gate's run %s: %w", run, watchErr)}
	}
	g.forgetRun()
	if end.Lost {
		return scriptrunner.Run{}, end.LostDetail, nil
	}
	// A GATE WHOSE LOG FAILED IS AN UNRUNNABLE GATE rather than a lost log,
	// because the file is the only account of a failure that survives the run.
	if writeErr != nil {
		return scriptrunner.Run{}, "", fmt.Errorf("merge: writing the test log %s: %w", g.log.path, writeErr)
	}
	return scriptrunner.Run{Output: output.String(), ExitCode: end.ExitCode, CPU: end.CPU, Wall: end.Wall}, "", nil
}

// serviceRun answers the round's run on the test service: the one its
// progress records, or one started now and recorded before anything watches
// it. A run that cannot be recorded is stopped: a run no record names is one
// no later daemon could watch.
func (g *gateRun) serviceRun(ctx context.Context, dir string, argv []string) (string, error) {
	r := g.r
	r.mu.Lock()
	run := r.prog.TestRun
	r.mu.Unlock()
	fields := dlog.Context{"workspace": string(r.ws), "round": g.round.n}
	if run != "" {
		r.o.log(ctx, r.ws).Info("daemon.merge.tests", "the gate's run is already going on the test service; the gate watches it from its start", withField(fields, "run", run))
		return run, nil
	}
	name, err := r.o.workspaceName(ctx, "daemon.merge.tests", r.ws)
	if err != nil {
		return "", err
	}
	run, err = r.o.deps.Tests.Start(ctx, testsclient.Spec{Dir: dir, Argv: argv, Env: r.o.deps.GateEnv, Label: fmt.Sprintf("%s round %d", name, g.round.n)})
	if err != nil {
		return "", &gateUnstartedError{err: err}
	}
	if err := r.checkpoint(ctx, TabTests, func(d *progressDoc) { d.TestRun = run }); err != nil {
		if stopErr := r.o.deps.Tests.Stop(context.WithoutCancel(ctx), run); stopErr != nil {
			r.o.log(ctx, r.ws).Error("daemon.merge.tests", "the gate's run could not be recorded, and stopping it failed; it runs on with no record naming it", withField(withField(fields, "run", run), "error", stopErr.Error()))
		}
		return "", err
	}
	r.o.log(ctx, r.ws).Info("daemon.merge.tests", "started the gate's run on the test service", withField(fields, "run", run))
	return run, nil
}

// leaveOrStop ends the watch of RUN its context cut: the run is left going
// when a suspension the merge resumes from (keepsGateRun) or the daemon's
// exit took the merge -- the resumed merge watches the run again -- and
// stopped otherwise.
func (g *gateRun) leaveOrStop(ctx context.Context, run string, fields dlog.Context) {
	r := g.r
	why, suspended := r.suspendedFor()
	if (suspended && why.keepsGateRun) || (!suspended && r.o.isDraining()) {
		r.o.log(ctx, r.ws).Info("daemon.merge.tests", "a suspension took the merge mid-gate; its run goes on on the test service for the merge's resume to watch", fields)
		return
	}
	r.retireTestRun(ctx, "the gate ended before its run did")
}

// retireTestRun stops the run the merge's progress records, if any, and
// drops it from the progress in memory, so the next checkpoint records none:
// a run no gate will watch again holds the host's suites for nothing. WHY
// names what retired it.
func (r *run) retireTestRun(ctx context.Context, why string) {
	r.mu.Lock()
	run := r.prog.TestRun
	r.prog.TestRun = ""
	r.mu.Unlock()
	if run == "" {
		return
	}
	fields := dlog.Context{"workspace": string(r.ws), "run": run, "why": why}
	if err := r.o.deps.Tests.Stop(context.WithoutCancel(ctx), run); err != nil && !errors.Is(err, testsclient.ErrUnknownRun) {
		r.o.log(ctx, r.ws).Error("daemon.merge.tests", "could not stop a gate's run on the test service; the run goes on unwatched", withField(fields, "error", err.Error()))
		return
	}
	r.o.log(ctx, r.ws).Info("daemon.merge.tests", "stopped a gate's run on the test service no gate watches again", fields)
}

// forgetRun drops the round's run from the merge's progress in memory: the
// next checkpoint records the round with none.
func (g *gateRun) forgetRun() {
	g.r.mu.Lock()
	g.r.prog.TestRun = ""
	g.r.mu.Unlock()
}

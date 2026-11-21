package deploy

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/kernelevent"
)

// A RESTART OF THE TEST SERVICE TAKES EVERY MERGE GATE'S RUN OFF IT FIRST,
// AND ENDS ONLY ONCE THE SERVICE ANSWERS AGAIN. 2026-11-21 14:51:20: a
// landing's deploy restarted the test service under the running gate of the
// next merge, the gate read its ended watch as a fault of the host, and the
// queue was paused in the host's name with nothing to fix. The restart is the
// daemon's own, so it tells the gates before it acts (TestRuns), and releases
// them on the service's own answer, never on a duration. Every restart of the
// service goes through here: a deploy's, its rollback's, and a boot's.

// TestRuns holds the merge gates' runs off the test service across its
// restart (merge.Orchestrator.TakeTestRunsOff): every gate's run is taken off
// before it answers, and no gate runs on the service until its release.
type TestRuns interface {
	TakeTestRunsOff(ctx context.Context) (func(answered error), error)
}

// testsAnswerBound bounds the wait for the test service to answer once its
// restart was asked. It is two and a half times the longest of its six
// restarts from 2026-11-17 to 2026-11-21 in ~/.cache/agent-repl/log/tests.log,
// timed from "received a termination signal" to "listening on the tests
// socket": 41ms, 52ms, 82ms, 130ms, 260ms, and 10.07s (2026-11-18 09:47:29,
// the service manager's relaunch throttle).
const testsAnswerBound = 25 * time.Second

// restartTests restarts the test service S with every gate's run off it: the
// gates are taken off (TestRuns), the service restarted, its previous process
// waited out and its successor's answer awaited, and the gates released with
// how that ended. A hold that cannot be taken restarts nothing.
func (r *Restarter) restartTests(ctx context.Context, s Standalone) error {
	fields := dlog.Context{s.Short: s.Label}
	if r.TestRuns == nil || r.TestsAnswer == nil || r.TestsSocket == "" {
		err := errors.New("deploy: a restart of the test service needs the gates' hold, the service's socket and its answer")
		r.Log.Error(opServices, "the test service cannot be restarted without its gates' hold and its answer; it is NOT restarted", dlog.WithCause(fields, err))
		return err
	}
	release, err := r.TestRuns.TakeTestRunsOff(ctx)
	if err != nil {
		r.failed("the gates' runs could not be taken off the test service; it is NOT restarted", fields, err)
		return fmt.Errorf("deploy: take the gates' runs off %s: %w", s.Label, err)
	}
	err = r.restartAndAwaitAnswer(ctx, s, fields)
	release(err)
	return err
}

// restartAndAwaitAnswer restarts S and awaits its answer, the process it
// replaced gone first.
func (r *Restarter) restartAndAwaitAnswer(ctx context.Context, s Standalone, fields dlog.Context) error {
	_, before, err := r.Services.Status(ctx, s.Label)
	if err != nil {
		r.failed("could not read the test service's service-manager state; it is NOT restarted", fields, err)
		return fmt.Errorf("deploy: read %s: %w", s.Label, err)
	}
	if err := r.restartStandalone(ctx, s); err != nil {
		return err
	}
	return r.awaitTestsAnswer(ctx, before, fields)
}

// awaitTestsAnswer waits ON THE SERVICE, NOT ON A STOPWATCH: for the process
// BEFORE (zero for none) to exit, the kernel's report, so an answer can only
// be its successor's; then for the service to answer a call, asked again each
// time the socket's directory changes (the new process binds its socket
// there). testsAnswerBound is the failure bound, never a cadence.
func (r *Restarter) awaitTestsAnswer(ctx context.Context, before int, fields dlog.Context) error {
	fields = dlog.Merge(fields, dlog.Context{"socket": r.TestsSocket, "previous_pid": before})
	start := r.Clock.Now()
	if before != 0 {
		if err := r.awaitExit(ctx, before); err != nil {
			r.failed("the test service's previous process did not exit after its restart; it is not proven to answer", fields, err)
			return fmt.Errorf("deploy: %s (pid %d) did not exit: %w", TestsLabel, before, err)
		}
	}
	// THE WATCH IS TAKEN BEFORE THE FIRST LOOK, so a socket bound in between
	// is already reported.
	changed, stopWatching := kernelevent.Follow(ctx, filepath.Dir(r.TestsSocket))
	defer stopWatching()
	bound := r.Clock.After(testsAnswerBound)
	last := errors.New("its socket is not there")
	for {
		if socketExists(r.TestsSocket) {
			if last = r.TestsAnswer(ctx); last == nil {
				r.Log.Info(opServices, "the test service answers again after its restart", dlog.Merge(fields, dlog.Context{
					"after": r.Clock.Now().Sub(start).String()}))
				return nil
			}
		}
		select {
		case <-ctx.Done():
			r.failed("the wait for the test service's answer ended with its context", fields, ctx.Err())
			return fmt.Errorf("deploy: await the answer of %s: %w", TestsLabel, ctx.Err())
		case err := <-changed:
			if err != nil {
				r.Log.Error(opServices, "the watch on the test service's socket failed; it is not proven to answer", dlog.WithCause(fields, err))
				return fmt.Errorf("deploy: watch %s: %w", r.TestsSocket, err)
			}
		case <-bound:
			r.Log.Error(opServices, "the test service did not answer within its bound after its restart; the gates held for it go on, and one that finds it down holds its queue in the host's name",
				dlog.Merge(fields, dlog.Context{"bound": testsAnswerBound.String(), "last": last.Error()}))
			return fmt.Errorf("deploy: %s did not answer within %s of its restart: %w", TestsLabel, testsAnswerBound, last)
		}
	}
}

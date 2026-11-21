package merge

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/dlog"
)

// THE DAEMON'S OWN RESTART OF THE TEST SERVICE TAKES EVERY GATE'S RUN OFF IT
// FIRST (TakeTestRunsOff). 2026-11-21 14:51:20: the deploy of the
// desktop-notifications-missing landing restarted the test service under the
// running gate of stale-shim-pid-reuse's round 2; the gate's watch ended with
// the service's exit, the gate read the ended watch as a fault of the host,
// and the queue was paused in the host's name with nothing to fix. The
// restart is the daemon's own act, so the gates are told of it before it
// happens, never left to infer it from the error it leaves:
//
//   - THE HOLD STANDS FROM BEFORE THE RESTART UNTIL THE SERVICE ANSWERS
//     AGAIN. No gate starts or watches a run on the service while it stands:
//     a gate about to waits for its release (enterTestService).
//   - EVERY GATE ON THE SERVICE IS CUT, its run stopped and dropped from its
//     merge's record, and the hold is answered only once each let go: no gate
//     run is in flight across the restart.
//   - THE RELEASE IS THE SERVICE'S ANSWER, never a duration: the restart
//     ends once the service answers a call (deploy.Restarter), and each gate
//     held then runs again from its start, in the same round. Nothing of the
//     branch was judged, so no fixing attempt is spent and nothing pauses the
//     queue.
//   - A SERVICE THAT DOES NOT ANSWER WITHIN ITS BOUND ends the hold too, its
//     failure recorded at ERROR by the restart: a gate then finds the service
//     down as any gate would, and that is the host's fault (hostFault).

// errTestServiceRestart is the cause a restart of the test service cuts a
// gate's run on it with.
var errTestServiceRestart = errors.New("merge: the test service is restarting; the gate's run is taken off it, and the gate runs again from its start once the service answers")

// whyTestServiceRestart is why a gate's run the restart took off the service
// was stopped (retireTestRun).
const whyTestServiceRestart = "the test service is restarting"

// serviceGates are the gates whose runs are on the test service, and the
// restart's hold of them while one stands.
type serviceGates struct {
	mu sync.Mutex
	// hold is the restart standing now, nil while none does.
	hold *serviceHold
	// on are the gates that started or watch a run on the service.
	on map[*serviceGate]struct{}
}

// serviceHold is one restart of the test service, from its gates' cut to its
// release.
type serviceHold struct {
	// released closes once the restart ended; answered is then why the
	// service is not proven to answer, nil when it answers.
	released chan struct{}
	answered error
}

// serviceGate is one gate's run on the test service.
type serviceGate struct {
	// cut ends the run's start and watch with a cause.
	cut context.CancelCauseFunc
	// left closes once the gate let go of the run: it ended, or it was
	// stopped and dropped from the merge's record.
	left chan struct{}
}

// TakeTestRunsOff implements Orchestrator. It cuts every gate's run on the
// test service, waits for each to let go -- its run stopped and dropped from
// its merge's record -- within the drain's bound, and holds every gate off the
// service until the answered release: a restart of the test service runs
// between the two, and calls the release once the service answers again, or
// with why it does not.
//
// THE BOUND IS THE DRAIN'S (TerminalDrainBound). A gate lets go with one
// StopRun call and one progress write: the service's StartRun call took 10.5ms
// (2026-11-21 14:50:21.196 to .206, stale-shim-pid-reuse round 2's gate), and
// the write is the SQLite work the drain's bound is sized for. A gate that
// outlives it refuses the hold, recorded at ERROR, and the restart does not
// run.
func (o *orchestrator) TakeTestRunsOff(ctx context.Context) (func(answered error), error) {
	log := o.deps.Log.Global()
	s := &o.serviceGates
	s.mu.Lock()
	if s.hold != nil {
		s.mu.Unlock()
		err := errors.New("merge: a restart of the test service already holds its gates")
		log.Error(opTestService, "a restart of the test service asked to hold its gates while another holds them; it is refused, and it does not run", dlog.Context{"error": err.Error()})
		return nil, err
	}
	hold := &serviceHold{released: make(chan struct{})}
	s.hold = hold
	gates := make([]*serviceGate, 0, len(s.on))
	for g := range s.on {
		gates = append(gates, g)
	}
	s.mu.Unlock()
	release := o.serviceRelease(hold)
	log.Info(opTestService, "the test service is restarting: every gate's run is taken off it, and no gate runs on it until it answers again", dlog.Context{"gates": len(gates)})
	for _, g := range gates {
		g.cut(errTestServiceRestart)
	}
	bound := o.terminalDrainBound()
	expired := time.After(bound)
	for _, g := range gates {
		select {
		case <-g.left:
		case <-expired:
			err := fmt.Errorf("merge: a gate did not let go of its run on the test service within %s", bound)
			log.Error(opTestService, "a gate did not let go of its run on the test service within its bound; the restart is refused, and the gates go on", dlog.Context{
				"error": err.Error(), "bound": bound.String()})
			release(err)
			return nil, err
		case <-ctx.Done():
			err := context.Cause(ctx)
			log.Info(opTestService, "the restart of the test service was cut before every gate let go of its run; it does not run, and the gates go on", dlog.Context{"cause": err.Error()})
			release(err)
			return nil, err
		}
	}
	return release, nil
}

// serviceRelease answers HOLD's release: the hold ends, once, and every gate
// it held goes on. ANSWERED is why the service is not proven to answer, nil
// when it answers.
func (o *orchestrator) serviceRelease(hold *serviceHold) func(answered error) {
	var once sync.Once
	return func(answered error) {
		once.Do(func() {
			s := &o.serviceGates
			s.mu.Lock()
			hold.answered = answered
			s.hold = nil
			close(hold.released)
			s.mu.Unlock()
			if answered == nil {
				o.deps.Log.Global().Info(opTestService, "the test service answers again; every gate its restart held runs again from its start", nil)
				return
			}
			o.deps.Log.Global().Info(opTestService, "the hold of the test service's gates ends without the service proven to answer; the gates go on, and one that finds the service down holds the queue in the host's name", dlog.Context{
				"cause": answered.Error()})
		})
	}
}

// enterTestService waits out a restart's hold of the test service, then
// enters the gate's run on it. It answers the context the run's start and
// watch take, which a restart cuts with errTestServiceRestart, and the leave
// the gate calls once it let go of the run, however the run ended.
func (g *gateRun) enterTestService(ctx context.Context) (context.Context, func(), error) {
	s := &g.r.o.serviceGates
	for {
		s.mu.Lock()
		hold := s.hold
		if hold == nil {
			watch, cut := context.WithCancelCause(ctx)
			on := &serviceGate{cut: cut, left: make(chan struct{})}
			s.on[on] = struct{}{}
			s.mu.Unlock()
			return watch, func() {
				s.mu.Lock()
				delete(s.on, on)
				s.mu.Unlock()
				cut(nil)
				close(on.left)
			}, nil
		}
		s.mu.Unlock()
		fields := dlog.Context{"workspace": string(g.r.ws), "round": g.round.n}
		g.r.o.log(ctx, g.r.ws).Info(opTestService, "the test service is restarting; the gate waits for it to answer before its run starts", fields)
		if g.r.o.onServiceHoldWait != nil {
			g.r.o.onServiceHoldWait()
		}
		select {
		case <-hold.released:
			if hold.answered != nil {
				g.r.o.log(ctx, g.r.ws).Info(opTestService, "the test service's restart ended without its answer; the gate's run starts, and a service still down holds the queue in the host's name", withField(fields, "cause", hold.answered.Error()))
			}
		case <-ctx.Done():
			return nil, nil, context.Cause(ctx)
		}
	}
}

// takenOff settles a gate whose run a restart of the test service took off
// it: the run is stopped (already, when the cut found it watched) and the
// merge's record names none, so no resume watches a run the restart ends. The
// gate then runs again from its start once the service answers; nothing of
// the branch was judged, so no fixing attempt is spent and nothing holds the
// queue.
func (g *gateRun) takenOff(ctx context.Context) error {
	r := g.r
	r.retireTestRun(ctx, whyTestServiceRestart)
	if err := r.checkpoint(ctx, TabTests, func(d *progressDoc) { d.TestRun = "" }); err != nil {
		return err
	}
	r.o.log(ctx, r.ws).Info(opTestService, "the test service's restart took the gate's run off it; the gate runs again from its start once the service answers, and no fixing attempt is spent", dlog.Context{
		"workspace": string(r.ws), "round": g.round.n, "since_gate_start_seconds": g.since()})
	return nil
}

// opTestService is the operation the gates' records of the test service are
// filed under.
const opTestService = "daemon.merge.tests"

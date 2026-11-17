package merge

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/testsclient"
	"agent-repld/internal/wsm"
)

// fakeTestService is the test service the suite's gates run on: each run is
// played by RUNNER in a goroutine of its own, so -- as on the real service --
// a run outlives the watch that started it, and a resumed merge watches the
// same run again. Cleanup stops every run still going and waits for it.
type fakeTestService struct {
	t      *testing.T
	runner ScriptRunner

	mu   sync.Mutex
	runs map[string]*fakeServiceRun
	// starts and stops record every Start's spec and every Stop's run.
	starts []testsclient.Spec
	stops  []string
	// startErr, stopErr and watchErr, when set, are what the next calls
	// answer.
	startErr, stopErr, watchErr error
	// onStart, when set, is called with every started run's id.
	onStart func(run string)
	wg      sync.WaitGroup
}

// fakeServiceRun is one run on the fake service.
type fakeServiceRun struct {
	lines   []string
	end     *testsclient.End
	err     error
	changed chan struct{}
	cancel  context.CancelFunc
}

func newFakeTestService(t *testing.T, runner ScriptRunner) *fakeTestService {
	s := &fakeTestService{t: t, runner: runner, runs: map[string]*fakeServiceRun{}}
	t.Cleanup(func() {
		s.mu.Lock()
		for _, run := range s.runs {
			run.cancel()
		}
		s.mu.Unlock()
		s.wg.Wait()
	})
	return s
}

func (s *fakeTestService) Start(_ context.Context, spec testsclient.Spec) (string, error) {
	s.mu.Lock()
	if err := s.startErr; err != nil {
		s.mu.Unlock()
		return "", err
	}
	s.starts = append(s.starts, spec)
	id := fmt.Sprintf("run-%d", len(s.starts))
	ctx, cancel := context.WithCancel(context.Background())
	run := &fakeServiceRun{changed: make(chan struct{}), cancel: cancel}
	s.runs[id] = run
	onStart := s.onStart
	s.wg.Add(1)
	s.mu.Unlock()
	go func() {
		defer s.wg.Done()
		measured, err := s.runner.RunLinesMeasured(ctx, spec.Dir, spec.Argv, func(line string) {
			s.mu.Lock()
			defer s.mu.Unlock()
			if run.end == nil {
				run.lines = append(run.lines, line)
				s.wake(run)
			}
		})
		s.mu.Lock()
		defer s.mu.Unlock()
		switch {
		case run.end != nil:
			// The run was lost (lose): its play ends unheard.
		case ctx.Err() != nil:
			run.end = &testsclient.End{ExitCode: -1, Stopped: true}
		case err != nil:
			run.err = err
		default:
			run.end = &testsclient.End{ExitCode: measured.ExitCode, CPU: measured.CPU, Wall: measured.Wall}
		}
		s.wake(run)
	}()
	if onStart != nil {
		onStart(id)
	}
	return id, nil
}

// wake tells RUN's watchers it changed. Called with s.mu held.
func (s *fakeTestService) wake(run *fakeServiceRun) {
	close(run.changed)
	run.changed = make(chan struct{})
}

func (s *fakeTestService) Watch(ctx context.Context, id string, from uint64, onLine func(uint64, string)) (testsclient.End, error) {
	next := from
	for {
		s.mu.Lock()
		if err := s.watchErr; err != nil {
			s.mu.Unlock()
			return testsclient.End{}, err
		}
		run, ok := s.runs[id]
		if !ok {
			s.mu.Unlock()
			return testsclient.End{}, testsclient.ErrUnknownRun
		}
		lines := append([]string(nil), run.lines[min(int(next), len(run.lines)):]...)
		end, err, changed := run.end, run.err, run.changed
		s.mu.Unlock()
		for _, line := range lines {
			onLine(next, line)
			next++
		}
		if err != nil {
			return testsclient.End{}, err
		}
		if end != nil && int(next) >= s.lineCount(id) {
			return *end, nil
		}
		select {
		case <-changed:
		case <-ctx.Done():
			return testsclient.End{}, ctx.Err()
		}
	}
}

func (s *fakeTestService) lineCount(id string) int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return len(s.runs[id].lines)
}

func (s *fakeTestService) Stop(_ context.Context, id string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.stops = append(s.stops, id)
	if err := s.stopErr; err != nil {
		return err
	}
	run, ok := s.runs[id]
	if !ok {
		return testsclient.ErrUnknownRun
	}
	run.cancel()
	return nil
}

// lose ends run ID as the service's own restart would: lost, with DETAIL.
func (s *fakeTestService) lose(id, detail string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	run := s.runs[id]
	run.end = &testsclient.End{Lost: true, LostDetail: detail}
	run.lines = nil
	run.cancel()
	s.wake(run)
}

// forget drops run ID, as a service that pruned it would.
func (s *fakeTestService) forget(id string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	run := s.runs[id]
	delete(s.runs, id)
	run.cancel()
	s.wake(run)
}

// started answers every Start's spec so far.
func (s *fakeTestService) started() []testsclient.Spec {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]testsclient.Spec(nil), s.starts...)
}

// stopped answers every Stop's run so far.
func (s *fakeTestService) stopped() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.stops...)
}

// serviceRecord answers the gate's record of the test service at LEVEL
// reading MESSAGE.
func serviceRecord(h *harness, level, message string) (dlog.Record, bool) {
	return dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: level, Operation: "daemon.merge.tests", Message: message})
}

// holdOnce holds the harness runner's first run until the test ends or the
// run is stopped, and lets every later run straight through.
func holdOnce(t *testing.T, h *harness) (entered chan struct{}) {
	entered, release := make(chan struct{}), make(chan struct{})
	t.Cleanup(func() { close(release) })
	var once sync.Once
	h.runner.before = func() {
		first := false
		once.Do(func() { first = true })
		if first {
			close(entered)
			<-release
		}
	}
	return entered
}

// startedBefore starts a run on the harness's test service as a daemon that
// died mid-gate left it, answering its id.
func startedBefore(t *testing.T, h *harness) string {
	t.Helper()
	run, err := h.tests.Start(context.Background(), testsclient.Spec{Dir: h.sourceD, Argv: []string{"bash", h.script}, Label: "the dead daemon's gate"})
	if err != nil {
		t.Fatalf("starting the dead daemon's run: %v", err)
	}
	return run
}

// atTestsWithRun stands a record at tests round 1 naming RUN as its gate's.
func atTestsWithRun(run string) func(*progressDoc) {
	return func(d *progressDoc) {
		atStep(TabTests, 1)(d)
		d.TestRun = run
	}
}

// hostPauseCause answers the cause of the host's pause of the chain's queue.
func hostPauseCause(t *testing.T, c *chain) string {
	t.Helper()
	c.h.db.mu.Lock()
	defer c.h.db.mu.Unlock()
	pause := c.h.db.pauses[c.h.repoKey()]
	if pause == nil || pause.By.Kind != wsm.MergeQueuePauserHost {
		t.Fatalf("pause = %+v, want the host's", pause)
	}
	return pause.By.Cause
}

func TestAGateRecordsItsServiceRunBeforeItsMergeIsSuspended(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act: the daemon's exit suspends the merge mid-gate.
	heldInGate(t, h)

	// Assert.
	if got := recordedDoc(t, h).TestRun; got != "run-1" {
		t.Fatalf("recorded run %q, want run-1", got)
	}
}

func TestAGateTheDaemonsExitCutLeavesItsRunGoing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	heldInGate(t, h)

	// Assert.
	if stops := h.tests.stopped(); len(stops) != 0 {
		t.Fatalf("stopped %v, want the run left going for the resume", stops)
	}
	rec, ok := serviceRecord(h, "info", "a suspension took the merge mid-gate; its run goes on on the test service for the merge's resume to watch")
	if !ok || rec.Context["run"] != "run-1" {
		t.Fatalf("record = %+v (%v), want the kept run named", rec, ok)
	}
}

func TestAGateRunCarriesTheDaemonsToolchainPath(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	heldInGate(t, h)

	// Assert.
	started := h.tests.started()
	if len(started) != 1 || len(started[0].Env) != 1 || started[0].Env[0] != "PATH=/toolchains/bin" {
		t.Fatalf("started %+v, want the gate's run started with the daemon's PATH", started)
	}
}

func TestAGateLabelsItsRunWithItsWorkspaceAndRound(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	heldInGate(t, h)

	// Assert.
	started := h.tests.started()
	if len(started) != 1 || started[0].Label != h.gateWorkspaces[0]+" round 1" || started[0].Dir != h.gateTrees[0] {
		t.Fatalf("started %+v, want one run labeled %q in %s", started, h.gateWorkspaces[0]+" round 1", h.gateTrees[0])
	}
}

func TestAResumedGateWatchesItsRecordedRunRatherThanStartingOne(t *testing.T) {
	// Arrange: the dead daemon's gate run is going on the service.
	h := newHarness(t)
	landsAfterResume(h)
	run := startedBefore(t, h)
	interruptedAt(t, h, atTestsWithRun(run))

	// Act.
	resume(t, h)

	// Assert.
	if started := h.tests.started(); len(started) != 1 {
		t.Fatalf("started %d runs, want the dead daemon's alone", len(started))
	}
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q, want %q on the watched run's pass", facts.State, StateMerged)
	}
	if rec, ok := serviceRecord(h, "info", "the gate's run is already going on the test service; the gate watches it from its start"); !ok || rec.Context["run"] != run {
		t.Fatalf("record = %+v (%v), want the watched run named", rec, ok)
	}
}

func TestAResumeOntoAMovedTargetStopsTheRecordedRun(t *testing.T) {
	// Arrange: the dead daemon's gate run holds on the service.
	h := newHarness(t)
	holdOnce(t, h)
	landsAfterResume(h)
	run := startedBefore(t, h)
	interruptedAt(t, h, atTestsWithRun(run))
	targetMovedWhileDown(h)

	// Act.
	resume(t, h)

	// Assert.
	if stops := h.tests.stopped(); len(stops) != 1 || stops[0] != run {
		t.Fatalf("stopped %v, want the dead daemon's run %s", stops, run)
	}
	rec, ok := serviceRecord(h, "info", "stopped a gate's run on the test service no gate watches again")
	if !ok || !strings.Contains(rec.Context["why"].(string), "the target moved") {
		t.Fatalf("record = %+v (%v), want the stop naming the moved target", rec, ok)
	}
}

func TestAResumeThatGivesUpBeforeItsGateStopsTheRecordedRun(t *testing.T) {
	// Arrange: a rebase stands in the resumed merge's tree.
	h := newHarness(t)
	holdOnce(t, h)
	run := startedBefore(t, h)
	interruptedAt(t, h, atTestsWithRun(run))
	h.git.standing = true

	// Act.
	resume(t, h)

	// Assert.
	if stops := h.tests.stopped(); len(stops) != 1 || stops[0] != run {
		t.Fatalf("stopped %v, want the dead daemon's run %s", stops, run)
	}
}

func TestARunTheServiceLostRunsTheGateAgainFromItsStart(t *testing.T) {
	cases := []struct {
		name string
		drop func(s *fakeTestService, run string)
		want string
	}{
		{name: "a run the service lost", drop: func(s *fakeTestService, run string) { s.lose(run, "the test service restarted") },
			want: "the test service restarted"},
		{name: "a run the service holds no record of", drop: func(s *fakeTestService, run string) { s.forget(run) },
			want: "the test service holds no record of the run"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the service drops the gate's first run.
			h := newHarness(t)
			h.emacsRepo()
			h.landsCleanly("abc123def4567")
			h.gatePasses("daemon")
			h.gatePasses("daemon")
			h.tests.onStart = func(run string) {
				if run == "run-1" {
					tc.drop(h.tests, run)
				}
			}
			enqueue(t, h)

			// Act.
			if err := <-admitAsync(h, context.Background()); err != nil {
				t.Fatalf("pumpOnce: %v", err)
			}

			// Assert.
			if started := h.tests.started(); len(started) != 2 {
				t.Fatalf("started %d runs, want the dropped run and its rerun", len(started))
			}
			rec, ok := serviceRecord(h, "info", "the test service lost the gate's run; the gate runs again from its start")
			if !ok || rec.Context["lost"] != tc.want {
				t.Fatalf("record = %+v (%v), want the loss named %q", rec, ok, tc.want)
			}
		})
	}
}

func TestAGateTheServiceCannotStartHoldsTheQueueInTheHostsName(t *testing.T) {
	// Arrange.
	c := newChain(t)
	c.h.tests.startErr = fmt.Errorf("start run: %w", testsclient.ErrUnavailable)
	enqueue(t, c.h)

	// Act.
	if err := <-admitAsync(c.h, context.Background()); err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}

	// Assert.
	if cause := hostPauseCause(t, c); !strings.Contains(cause, "could not be started") || !strings.Contains(cause, testsclient.ErrUnavailable.Error()) {
		t.Fatalf("cause = %q, want the service's unavailability named", cause)
	}
}

func TestAGateWhoseRunCannotBeWatchedStopsItAndHoldsTheQueue(t *testing.T) {
	// Arrange.
	c := newChain(t)
	c.h.tests.watchErr = errors.New("stream reset")
	enqueue(t, c.h)

	// Act.
	if err := <-admitAsync(c.h, context.Background()); err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}

	// Assert.
	if stops := c.h.tests.stopped(); len(stops) != 1 || stops[0] != "run-1" {
		t.Fatalf("stopped %v, want the unwatchable run", stops)
	}
	if cause := hostPauseCause(t, c); !strings.Contains(cause, "watch the gate's run run-1: stream reset") {
		t.Fatalf("cause = %q, want the failed watch named", cause)
	}
	if doc := recordedDoc(t, c.h); doc.TestRun != "" {
		t.Fatalf("recorded run %q, want none for the resume to watch", doc.TestRun)
	}
}

func TestAHostFaultDropsTheRunFromTheRecord(t *testing.T) {
	// Arrange, Act.
	c := hostFaulted(t)

	// Assert.
	if doc := recordedDoc(t, c.h); doc.TestRun != "" {
		t.Fatalf("recorded run %q, want none: the resume runs the gate again from its start", doc.TestRun)
	}
}

func TestARunWhoseRecordCannotBeWrittenIsStopped(t *testing.T) {
	cases := []struct {
		name     string
		stopErr  error
		wantLogs string
	}{
		{name: "the stop succeeds"},
		{name: "the stop fails", stopErr: errors.New("connection refused"),
			wantLogs: "the gate's run could not be recorded, and stopping it failed; it runs on with no record naming it"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the record fails the moment the run starts.
			h := newHarness(t)
			h.emacsRepo()
			h.tests.stopErr = tc.stopErr
			h.tests.onStart = func(string) {
				h.db.mu.Lock()
				h.db.progressErr = errors.New("disk I/O error")
				h.db.mu.Unlock()
			}
			enqueue(t, h)

			// Act.
			<-admitAsync(h, context.Background())

			// Assert.
			if stops := h.tests.stopped(); len(stops) != 1 || stops[0] != "run-1" {
				t.Fatalf("stopped %v, want the unrecorded run", stops)
			}
			if tc.wantLogs == "" {
				return
			}
			if rec, ok := serviceRecord(h, "error", tc.wantLogs); !ok || rec.Context["run"] != "run-1" || rec.Context["error"] != "connection refused" {
				t.Fatalf("record = %+v (%v), want the failed stop named", rec, ok)
			}
		})
	}
}

func TestAGateEndedBeforeItsRunStopsTheRun(t *testing.T) {
	cases := []struct {
		name    string
		stopErr error
		level   string
		message string
	}{
		{name: "the stop succeeds", level: "info", message: "stopped a gate's run on the test service no gate watches again"},
		{name: "the stop fails", stopErr: errors.New("connection refused"), level: "error",
			message: "could not stop a gate's run on the test service; the run goes on unwatched"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the head holds in its gate.
			c, done := headInGate(t)
			c.h.tests.mu.Lock()
			c.h.tests.stopErr = tc.stopErr
			c.h.tests.mu.Unlock()

			// Act: the merge is taken back.
			if err := c.h.o.Evict(context.Background(), theWorkspace); err != nil {
				t.Fatalf("Evict: %v", err)
			}
			<-done

			// Assert.
			if stops := c.h.tests.stopped(); len(stops) != 1 || stops[0] != "run-1" {
				t.Fatalf("stopped %v, want the evicted merge's run", stops)
			}
			rec, ok := serviceRecord(c.h, tc.level, tc.message)
			if !ok || rec.Context["run"] != "run-1" || rec.Context["why"] != "the gate ended before its run did" {
				t.Fatalf("record = %+v (%v), want the stop and why", rec, ok)
			}
		})
	}
}

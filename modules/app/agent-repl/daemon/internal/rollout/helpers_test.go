package rollout

import (
	"claude-repld/internal/tempdirs/tempdirstest"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// instant is the fixed instant every test's arithmetic starts from: no test in
// this package reads the wall clock.
var instant = time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

// errFake is the failure a fake returns when a test asks one to fail.
var errFake = errors.New("the fake refused")

// selfInstance is the daemon instance every test's controller runs as.
const selfInstance = ids.InstanceID("daemon-outgoing")

// fakeClock is a Clock the test drives. Every After registers a channel under
// the duration it was asked for, so a test releases exactly the window it means
// to — the holdout cadence or the adoption window — and never waits one out.
type fakeClock struct {
	mu    sync.Mutex
	now   time.Time
	waits []time.Duration
	armed map[time.Duration][]chan time.Time
	// asked announces every After call, so a test synchronizes on a window
	// having been armed rather than polling for it.
	asked chan time.Duration
}

func newFakeClock(now time.Time) *fakeClock {
	return &fakeClock{
		now:   now,
		armed: make(map[time.Duration][]chan time.Time),
		asked: make(chan time.Duration, 256),
	}
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

// advance moves Now forward by d without firing anything.
func (c *fakeClock) advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = c.now.Add(d)
}

func (c *fakeClock) After(d time.Duration) <-chan time.Time {
	ch := make(chan time.Time, 1)
	c.mu.Lock()
	c.waits = append(c.waits, d)
	c.armed[d] = append(c.armed[d], ch)
	c.mu.Unlock()
	select {
	case c.asked <- d:
	default:
	}
	return ch
}

// Waits returns every duration After was asked for.
func (c *fakeClock) Waits() []time.Duration {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([]time.Duration(nil), c.waits...)
}

// Fire releases every window armed for exactly d.
func (c *fakeClock) Fire(d time.Duration) {
	c.mu.Lock()
	waiting := c.armed[d]
	delete(c.armed, d)
	now := c.now
	c.mu.Unlock()
	for _, ch := range waiting {
		ch <- now
	}
}

// FireAll releases every window armed so far, whatever its duration.
func (c *fakeClock) FireAll() {
	c.mu.Lock()
	all := c.armed
	c.armed = make(map[time.Duration][]chan time.Time)
	now := c.now
	c.mu.Unlock()
	for _, waiting := range all {
		for _, ch := range waiting {
			ch <- now
		}
	}
}

// awaitArmed blocks until a window of exactly d has been armed, which is how a
// test knows the flow reached that window without polling or sleeping.
func (c *fakeClock) awaitArmed(t *testing.T, d time.Duration) {
	t.Helper()
	for {
		select {
		case got := <-c.asked:
			if got == d {
				return
			}
		case <-time.After(10 * time.Second):
			t.Fatalf("no window of %s was ever armed", d)
			return
		}
	}
}

// awaitArmedCount blocks until n windows of exactly d are armed and not yet
// fired. Unlike awaitArmed it counts what is STANDING, not the arms it happens
// to observe: an arm another await already consumed (and discarded for its
// duration) still counts, so the answer does not depend on the order earlier
// waits drained `asked` in.
func (c *fakeClock) awaitArmedCount(t *testing.T, d time.Duration, n int) {
	t.Helper()
	for {
		c.mu.Lock()
		standing := len(c.armed[d])
		c.mu.Unlock()
		if standing >= n {
			return
		}
		select {
		case <-c.asked:
		case <-time.After(10 * time.Second):
			t.Fatalf("%d window(s) of %s armed, want %d", standing, d, n)
			return
		}
	}
}

// armedSignal answers a channel that CLOSES once a window of exactly d has
// been armed. It is awaitArmed's non-fatal form, for a test that has to race
// that edge against a call answering early: the early answer is the failure
// worth reporting, so it must not be pre-empted by this helper's own t.Fatalf.
//
// It consumes `asked`, so exactly one of it and awaitArmed is in flight at a
// time within one test.
func (c *fakeClock) armedSignal(d time.Duration) <-chan struct{} {
	ready := make(chan struct{})
	go func() {
		for {
			select {
			case got := <-c.asked:
				if got == d {
					close(ready)
					return
				}
			case <-time.After(10 * time.Second):
				return
			}
		}
	}()
	return ready
}

// fakeSpawner is the successor spawner. Every successor it answers is a
// fakeSuccessor it keeps, so a test counts how many are still standing.
type fakeSpawner struct {
	address string
	err     error
	// startedThenFailed makes a failing Spawn still answer a successor: the
	// process started and never reported.
	startedThenFailed bool
	// stopErr is what every successor's Stop answers.
	stopErr error
	// onSpawn, when set, runs at the instant of each spawn, before the
	// successor is answered, so a test observes what a booting successor
	// would find.
	onSpawn func()
	// readyErr is what every successor's Ready answers; nil proves it serving.
	readyErr error
	// replacementErr is what SpawnReplacement answers.
	replacementErr error
	// replacements counts the replacements spawned.
	replacements int
	// readies counts the Ready calls.
	readies int
	mu      sync.Mutex
	told    []string
	spawned []*fakeSuccessor
}

func (s *fakeSpawner) Spawn(_ context.Context, incumbent string) (Successor, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.told = append(s.told, incumbent)
	if s.onSpawn != nil {
		s.onSpawn()
	}
	if s.err != nil && !s.startedThenFailed {
		return nil, s.err
	}
	child := &fakeSuccessor{spawner: s}
	if s.err == nil {
		child.address = s.address
	}
	s.spawned = append(s.spawned, child)
	return child, s.err
}

func (s *fakeSpawner) Told() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.told...)
}

// Live counts the successors spawned and not confirmed stopped.
func (s *fakeSpawner) Live() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	n := 0
	for _, child := range s.spawned {
		if !child.stopped {
			n++
		}
	}
	return n
}

// Stops counts every Stop asked of any successor, answered or refused.
func (s *fakeSpawner) Stops() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	n := 0
	for _, child := range s.spawned {
		n += child.stops
	}
	return n
}

// fakeSuccessor is one spawned successor.
type fakeSuccessor struct {
	spawner *fakeSpawner
	address string
	stops   int
	stopped bool
}

func (c *fakeSuccessor) Address() string { return c.address }

// fakeSuccessorPID is the pid every fake successor answers.
const fakeSuccessorPID = 51345

func (c *fakeSuccessor) PID() int { return fakeSuccessorPID }

func (c *fakeSuccessor) Ready(context.Context) error {
	c.spawner.mu.Lock()
	defer c.spawner.mu.Unlock()
	c.spawner.readies++
	return c.spawner.readyErr
}

// SpawnReplacement records a replacement's spawn.
func (s *fakeSpawner) SpawnReplacement(context.Context) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.replacementErr != nil {
		return 0, s.replacementErr
	}
	s.replacements++
	return fakeReplacementPID, nil
}

// fakeReplacementPID is the pid every fake replacement answers.
const fakeReplacementPID = 51400

// Replacements counts the replacements spawned.
func (s *fakeSpawner) Replacements() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.replacements
}

// Readies counts the readiness waits the handover asked for.
func (s *fakeSpawner) Readies() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.readies
}

func (c *fakeSuccessor) Stop(context.Context) error {
	c.spawner.mu.Lock()
	defer c.spawner.mu.Unlock()
	c.stops++
	if c.spawner.stopErr != nil {
		return c.spawner.stopErr
	}
	c.stopped = true
	return nil
}

// fakeAnnouncer captures the WatchDaemon shutdown announcement.
type fakeAnnouncer struct {
	mu   sync.Mutex
	sent []*agentreplv1.DaemonShutdownAnnounced
}

func (a *fakeAnnouncer) ShutdownAnnounced(push *agentreplv1.DaemonShutdownAnnounced) {
	a.mu.Lock()
	defer a.mu.Unlock()
	a.sent = append(a.sent, push)
}

func (a *fakeAnnouncer) Sent() []*agentreplv1.DaemonShutdownAnnounced {
	a.mu.Lock()
	defer a.mu.Unlock()
	return append([]*agentreplv1.DaemonShutdownAnnounced(nil), a.sent...)
}

// pushCall is one recorded per-workspace push.
type pushCall struct {
	WS      ids.WorkspaceID
	Kind    string
	Address string
}

// fakePusher captures the transferred and reload_webapp pushes.
type fakePusher struct {
	mu    sync.Mutex
	calls []pushCall
}

func (p *fakePusher) PushTransferred(ws ids.WorkspaceID, address string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.calls = append(p.calls, pushCall{WS: ws, Kind: "transferred", Address: address})
}

func (p *fakePusher) PushReloadWebapp(ws ids.WorkspaceID) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.calls = append(p.calls, pushCall{WS: ws, Kind: "reload_webapp"})
}

func (p *fakePusher) Calls() []pushCall {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]pushCall(nil), p.calls...)
}

// fakeParticipants is the expected-participant snapshot.
type fakeParticipants struct {
	mu sync.Mutex
	by map[ids.WorkspaceID]Participants
}

func newFakeParticipants() *fakeParticipants {
	return &fakeParticipants{by: make(map[ids.WorkspaceID]Participants)}
}

func (p *fakeParticipants) Participants(ws ids.WorkspaceID) Participants {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.by[ws]
}

func (p *fakeParticipants) Set(ws ids.WorkspaceID, participants Participants) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.by[ws] = participants
}

// fakeFreeness answers freeness right now.
type fakeFreeness struct {
	mu   sync.Mutex
	free map[ids.WorkspaceID]bool
}

func newFakeFreeness() *fakeFreeness {
	return &fakeFreeness{free: make(map[ids.WorkspaceID]bool)}
}

func (f *fakeFreeness) Free(ws ids.WorkspaceID) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.free[ws]
}

func (f *fakeFreeness) SetFree(ws ids.WorkspaceID, free bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.free[ws] = free
}

// fakeShim is a shimclient.Client the test drives. Everything the relaunch
// engine touches is here; the embedded interface makes the rest panic loudly if
// anything reaches for it.
type fakeShim struct {
	shimclient.Client

	pid int

	mu       sync.Mutex
	detached bool
	killReq  []*shimv1.KillSessionRequest
	killed   []shimclient.KillAttribution
	// killAnswer is what KillSession answers; the zero value is success.
	killAnswer *shimv1.KillSessionResponse
	killErr    error
	// forceErr is what Kill answers, so a test can drive the branch where a
	// process refuses to go down.
	forceErr error
	// exited carries the one ExitInfo the test reaps it with.
	exited chan shimclient.ExitInfo
	// forced announces every force-kill, so a test synchronizes on one having
	// happened rather than spinning on a counter.
	forced chan shimclient.KillAttribution
	// order records the engine's steps against this shim, so a test asserts the
	// SEQUENCE and not merely that each happened.
	order *steps
	// died is the exit Reaped answers once the test has killed the process
	// out from under the engine; nil while it runs.
	died *shimclient.ExitInfo
	// onKillSession, when set, runs as KillSession is asked, so a test acts
	// at the point of no return.
	onKillSession func()
	// killHangs makes KillSession answer only when its context ends, as a
	// shim whose vendor is unreachable does.
	killHangs bool
}

func newFakeShim(pid int, order *steps) *fakeShim {
	return &fakeShim{
		pid:    pid,
		exited: make(chan shimclient.ExitInfo, 1),
		forced: make(chan shimclient.KillAttribution, 4),
		order:  order,
	}
}

func (s *fakeShim) PID() int { return s.pid }

func (s *fakeShim) Detach() {
	s.mu.Lock()
	s.detached = true
	s.mu.Unlock()
	s.order.record("detach")
}

func (s *fakeShim) Detached() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.detached
}

func (s *fakeShim) KillSession(ctx context.Context, req *shimv1.KillSessionRequest) (*shimv1.KillSessionResponse, error) {
	s.mu.Lock()
	s.killReq = append(s.killReq, req)
	answer, err, hangs := s.killAnswer, s.killErr, s.killHangs
	s.mu.Unlock()
	if hangs {
		<-ctx.Done()
		return nil, ctx.Err()
	}
	s.order.record("kill_session")
	if s.onKillSession != nil {
		s.onKillSession()
	}
	if err != nil {
		return nil, err
	}
	if answer != nil {
		return answer, nil
	}
	return &shimv1.KillSessionResponse{
		Result: &shimv1.KillSessionResponse_Success{Success: &shimv1.KillSessionSuccess{}},
	}, nil
}

func (s *fakeShim) KillRequests() []*shimv1.KillSessionRequest {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]*shimv1.KillSessionRequest(nil), s.killReq...)
}

func (s *fakeShim) Kill(_ context.Context, attr shimclient.KillAttribution) error {
	s.mu.Lock()
	s.killed = append(s.killed, attr)
	err := s.forceErr
	s.mu.Unlock()
	s.order.record("force_kill")
	s.forced <- attr
	return err
}

// SetForceError makes the next force-kill fail, the way a shim the kernel will
// not let go of does.
func (s *fakeShim) SetForceError(err error) {
	s.mu.Lock()
	s.forceErr = err
	s.mu.Unlock()
}

func (s *fakeShim) ForceKills() []shimclient.KillAttribution {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]shimclient.KillAttribution(nil), s.killed...)
}

func (s *fakeShim) Exited() <-chan shimclient.ExitInfo { return s.exited }

// AwaitDeath answers the exit Die recorded, at once: this suite stages no race.
func (s *fakeShim) AwaitDeath(context.Context) (shimclient.ExitInfo, time.Duration, bool) {
	info, ok := s.Reaped()
	return info, 0, ok
}

// Reaped answers the exit Die recorded.
func (s *fakeShim) Reaped() (shimclient.ExitInfo, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.died == nil {
		return shimclient.ExitInfo{}, false
	}
	return *s.died, true
}

// Die records the process as exited on its own, as a shim that could not
// bind its socket does.
func (s *fakeShim) Die() {
	s.mu.Lock()
	s.died = &shimclient.ExitInfo{PID: s.pid, Code: 1}
	s.mu.Unlock()
}

// Reap makes the process observably gone, which is what the engine's gate
// waits for.
func (s *fakeShim) Reap() { s.exited <- shimclient.ExitInfo{PID: s.pid, Code: 0} }

// steps records the engine's ordered steps.
type steps struct {
	mu sync.Mutex
	in []string
}

func (s *steps) record(step string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.in = append(s.in, step)
}

func (s *steps) Taken() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.in...)
}

// fakeFleet is the shim fleet.
type fakeFleet struct {
	mu sync.Mutex
	// reclaimed is every workspace given back by a reclaim, in order.
	reclaimed []ids.WorkspaceID
	// live is the workspace's current client.
	live map[ids.WorkspaceID]*fakeShim
	// prelaunched is what Prelaunch answers, per workspace.
	prelaunched map[ids.WorkspaceID]*fakeShim
	// adopted is what Adopt answers.
	adopted map[ids.WorkspaceID]*fakeShim

	prelaunchErr map[ids.WorkspaceID]error
	adoptErr     map[ids.WorkspaceID]error
	installErr   map[ids.WorkspaceID]error
	resumeErr    map[ids.WorkspaceID]error
	// resumeErrOnce is what the NEXT Resume of a workspace answers, once.
	resumeErrOnce map[ids.WorkspaceID]error
	resumeCold    map[ids.WorkspaceID]*conversationv1.SessionCold
	// handOverErr is what HandOver answers for a workspace with a session.
	handOverErr map[ids.WorkspaceID]error
	// onAdopt, when set, runs as each Adopt arrives, before it answers.
	onAdopt func(ws ids.WorkspaceID)
	// factsErr is what AwaitFacts answers, per workspace; nil is the adopted
	// shim's re-announcement having landed.
	factsErr map[ids.WorkspaceID]error
	// coldGates is the cold gate standing per workspace.
	coldGates map[ids.WorkspaceID]*conversationv1.SessionCold
	// raisedColdGates records every RaiseCarriedColdGate, with its gate.
	raisedColdGates map[ids.WorkspaceID]*conversationv1.SessionCold
	// raiseErr is what RaiseCarriedColdGate answers.
	raiseErr error
	// parkedAdoptions records every AdoptParked, with the gate it raised.
	parkedAdoptions map[ids.WorkspaceID]*conversationv1.SessionCold
	// factsAwaited records every AwaitFacts.
	factsAwaited []ids.WorkspaceID

	installs  []ids.WorkspaceID
	adoptions []ids.WorkspaceID
	resumes   []ids.WorkspaceID
	order     *steps
}

func newFakeFleet(order *steps) *fakeFleet {
	return &fakeFleet{
		live:          make(map[ids.WorkspaceID]*fakeShim),
		prelaunched:   make(map[ids.WorkspaceID]*fakeShim),
		adopted:       make(map[ids.WorkspaceID]*fakeShim),
		prelaunchErr:  make(map[ids.WorkspaceID]error),
		adoptErr:      make(map[ids.WorkspaceID]error),
		installErr:    make(map[ids.WorkspaceID]error),
		resumeErr:     make(map[ids.WorkspaceID]error),
		resumeErrOnce: make(map[ids.WorkspaceID]error),
		resumeCold:    make(map[ids.WorkspaceID]*conversationv1.SessionCold),
		handOverErr:   make(map[ids.WorkspaceID]error),
		factsErr:      make(map[ids.WorkspaceID]error),
		coldGates:     make(map[ids.WorkspaceID]*conversationv1.SessionCold),
		order:         order,

		parkedAdoptions: make(map[ids.WorkspaceID]*conversationv1.SessionCold),
	}
}

// AwaitFacts answers the scripted re-announcement.
func (f *fakeFleet) AwaitFacts(_ context.Context, ws ids.WorkspaceID) error {
	f.order.record("await_facts")
	f.mu.Lock()
	defer f.mu.Unlock()
	f.factsAwaited = append(f.factsAwaited, ws)
	return f.factsErr[ws]
}

// ColdGateStanding answers the scripted standing gate.
func (f *fakeFleet) ColdGateStanding(ws ids.WorkspaceID) (*conversationv1.SessionCold, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	cold, ok := f.coldGates[ws]
	return cold, ok
}

// AdoptParked records the parked adoption and holds the dialed shim live.
func (f *fakeFleet) AdoptParked(_ context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) (shimclient.Client, error) {
	f.order.record("adopt_parked")
	f.mu.Lock()
	defer f.mu.Unlock()
	f.parkedAdoptions[ws] = cold
	c := newFakeShim(4343, f.order)
	f.live[ws] = c
	return c, nil
}

// RaiseCarriedColdGate records the gate a fresh boot raised from a carry,
// answering the scripted failure.
func (f *fakeFleet) RaiseCarriedColdGate(_ context.Context, ws ids.WorkspaceID, cold *conversationv1.SessionCold) error {
	f.order.record("raise_carried_cold_gate")
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.raisedColdGates == nil {
		f.raisedColdGates = map[ids.WorkspaceID]*conversationv1.SessionCold{}
	}
	f.raisedColdGates[ws] = cold
	return f.raiseErr
}

// FactsAwaited answers every workspace AwaitFacts was asked of, in order.
func (f *fakeFleet) FactsAwaited() []ids.WorkspaceID {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]ids.WorkspaceID(nil), f.factsAwaited...)
}

func (f *fakeFleet) Client(ws ids.WorkspaceID) (shimclient.Client, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	c, ok := f.live[ws]
	if !ok {
		return nil, false
	}
	return c, true
}

func (f *fakeFleet) Prelaunch(_ context.Context, ws ids.WorkspaceID) (shimclient.Client, error) {
	f.order.record("prelaunch")
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.prelaunchErr[ws]; err != nil {
		return nil, err
	}
	c, ok := f.prelaunched[ws]
	if !ok {
		c = newFakeShim(9999, f.order)
		f.prelaunched[ws] = c
	}
	return c, nil
}

func (f *fakeFleet) Install(_ context.Context, ws ids.WorkspaceID, c shimclient.Client) error {
	f.order.record("install")
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.installErr[ws]; err != nil {
		return err
	}
	f.installs = append(f.installs, ws)
	if shim, ok := c.(*fakeShim); ok {
		f.live[ws] = shim
	}
	return nil
}

func (f *fakeFleet) Adopt(_ context.Context, ws ids.WorkspaceID) (shimclient.Client, error) {
	f.order.record("adopt")
	if f.onAdopt != nil {
		f.onAdopt(ws)
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.adoptErr[ws]; err != nil {
		return nil, err
	}
	f.adoptions = append(f.adoptions, ws)
	c, ok := f.adopted[ws]
	if !ok {
		c = newFakeShim(4242, f.order)
		f.adopted[ws] = c
	}
	return c, nil
}

// Reclaimed records the workspace given back.
func (f *fakeFleet) Reclaimed(ws ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.reclaimed = append(f.reclaimed, ws)
}

// HandOver mirrors the real fleet's: the watches close, then the shim is
// detached; a stated error refuses before the detach.
func (f *fakeFleet) HandOver(ws ids.WorkspaceID) (bool, error) {
	f.mu.Lock()
	shim := f.live[ws]
	err := f.handOverErr[ws]
	f.mu.Unlock()
	if shim == nil {
		return false, nil
	}
	if err != nil {
		return true, err
	}
	shim.Detach()
	// The detached shim leaves the fleet with the detach, as the real one's does.
	f.mu.Lock()
	if f.live[ws] == shim {
		delete(f.live, ws)
	}
	f.mu.Unlock()
	return true, nil
}

// StandDown mirrors the real fleet's: the session is ended, then the process.
func (f *fakeFleet) StandDown(ctx context.Context, ws ids.WorkspaceID) error {
	f.mu.Lock()
	shim := f.live[ws]
	f.mu.Unlock()
	if shim == nil {
		return nil
	}
	if _, err := shim.KillSession(ctx, &shimv1.KillSessionRequest{Force: true}); err != nil {
		return err
	}
	return shim.Kill(context.Background(), shimclient.KillAttribution{Actor: "test.standdown", Reason: "stand down", Force: true})
}

func (f *fakeFleet) Resume(_ context.Context, ws ids.WorkspaceID, _ shimclient.Client) (Resumed, error) {
	f.order.record("resume")
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.resumeErr[ws]; err != nil {
		return Resumed{}, err
	}
	if err := f.resumeErrOnce[ws]; err != nil {
		delete(f.resumeErrOnce, ws)
		return Resumed{}, err
	}
	f.resumes = append(f.resumes, ws)
	return Resumed{Cold: f.resumeCold[ws]}, nil
}

func (f *fakeFleet) Adoptions() []ids.WorkspaceID {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]ids.WorkspaceID(nil), f.adoptions...)
}

// fakeRegistry stands in for the prompt queue's bounce registry. It models the
// one behavior the rollout relies on: a request on a FREE workspace (or a
// forced one) runs at once, on a goroutine of its own; a request on a busy one
// is REGISTERED until the test frees the workspace. The real registry's lock
// discipline is the prompt queue's to test; this fake lets the rollout's own
// engine be driven step by step.
type fakeRegistry struct {
	mu       sync.Mutex
	freeness *fakeFreeness
	requests []registryCall
	pending  map[ids.WorkspaceID]bounce.Request
	err      error
	running  sync.WaitGroup
	// endedDrains records every EndKeptDrain.
	endedDrains []ids.WorkspaceID
	// runCtx is the context each bounce runs on; nil is context.Background(),
	// which is what the queue's own runs are detached onto.
	runCtx context.Context
	// sealed is what SealMove answers per workspace, and sealErr its failure.
	sealed  map[ids.WorkspaceID]bounce.Handoff
	across  map[ids.WorkspaceID][]bounce.Request
	sealErr error
	// unsealed and adoptedHandoffs record what was put back and installed.
	unsealed        map[ids.WorkspaceID]bounce.Handoff
	adoptedHandoffs map[ids.WorkspaceID]bounce.Handoff
	adoptHandoffErr error
	rejudged        []ids.WorkspaceID
	rejudgeErr      error
	// park registers every request unrun, forced or not, so a test asserts
	// what was asked without the bounce running.
	park bool
	// unsealedCh announces every UnsealMove.
	unsealedCh chan ids.WorkspaceID
	// order, when set, records every request as a step, so a test asserts
	// what came before it.
	order *steps
	// asked announces every request, so a test synchronizes on one having
	// been made rather than polling for it.
	asked chan registryCall
	// deferred records every run that answered bounce.ErrDeferred.
	deferred []error
}

// registryCall is one recorded request.
type registryCall struct {
	WS  ids.WorkspaceID
	Req bounce.Request
}

func newFakeRegistry(freeness *fakeFreeness) *fakeRegistry {
	return &fakeRegistry{
		freeness: freeness, pending: make(map[ids.WorkspaceID]bounce.Request),
		sealed: make(map[ids.WorkspaceID]bounce.Handoff), across: make(map[ids.WorkspaceID][]bounce.Request),
		unsealed: make(map[ids.WorkspaceID]bounce.Handoff), adoptedHandoffs: make(map[ids.WorkspaceID]bounce.Handoff),
		asked: make(chan registryCall, 64), unsealedCh: make(chan ids.WorkspaceID, 16),
	}
}

// SealMove answers the scripted queue memory and carried replacements.
func (r *fakeRegistry) SealMove(_ context.Context, ws ids.WorkspaceID) (bounce.Handoff, []bounce.Request, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.sealErr != nil {
		return bounce.Handoff{}, nil, r.sealErr
	}
	across := r.across[ws]
	delete(r.across, ws)
	return r.sealed[ws], across, nil
}

// UnsealMove records what was put back.
func (r *fakeRegistry) UnsealMove(_ context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.unsealed[ws] = handoff
	select {
	case r.unsealedCh <- ws:
	default:
	}
	return nil
}

// AdoptHandoff records what was installed.
func (r *fakeRegistry) AdoptHandoff(_ context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.adoptHandoffErr != nil {
		return r.adoptHandoffErr
	}
	r.adoptedHandoffs[ws] = handoff
	return nil
}

// RejudgeHeld records the re-judgement.
func (r *fakeRegistry) RejudgeHeld(_ context.Context, ws ids.WorkspaceID) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.rejudged = append(r.rejudged, ws)
	return r.rejudgeErr
}

func (r *fakeRegistry) RequestBounce(_ context.Context, ws ids.WorkspaceID, req bounce.Request) (bounce.Decision, error) {
	if r.order != nil {
		r.order.record("request:" + req.Reason + ":" + req.WaitFor.String())
	}
	r.mu.Lock()
	r.requests = append(r.requests, registryCall{WS: ws, Req: req})
	select {
	case r.asked <- registryCall{WS: ws, Req: req}:
	default:
	}
	if r.err != nil {
		err := r.err
		r.mu.Unlock()
		return bounce.Decision{}, err
	}
	if r.park {
		r.pending[ws] = req
		r.mu.Unlock()
		return bounce.Decision{}, nil
	}
	// A DISPATCH-QUIET MOVE runs at once, turn and detached work
	// notwithstanding: the delivery lock is the only thing it waits on, and
	// the fake has no delivery in flight.
	free := r.freeness.Free(ws) || req.WaitFor == bounce.GateDispatchQuiet
	if !free && !req.Force {
		r.pending[ws] = req
		r.mu.Unlock()
		return bounce.Decision{TurnInFlight: true}, nil
	}
	r.mu.Unlock()
	r.run(ws, req)
	return bounce.Decision{Now: true, Forced: !free}, nil
}

// EndKeptDrain records the ended drain.
func (r *fakeRegistry) EndKeptDrain(ws ids.WorkspaceID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.endedDrains = append(r.endedDrains, ws)
}

// EndedDrains answers the workspaces whose kept drain was ended, in order.
func (r *fakeRegistry) EndedDrains() []ids.WorkspaceID {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]ids.WorkspaceID(nil), r.endedDrains...)
}

// run performs one bounce the way the queue does: its own goroutine, then the
// completion callback.
func (r *fakeRegistry) run(ws ids.WorkspaceID, req bounce.Request) {
	r.running.Add(1)
	go func() {
		defer r.running.Done()
		ctx := r.runCtx
		if ctx == nil {
			ctx = context.Background()
		}
		err := req.Run(ctx, ws)
		if bounce.OutcomeOf(err) == bounce.OutcomeDeferred {
			// THE QUEUE'S DEFERRAL: the replacement is registered again
			// behind the work the shim refused to stand down over, its Done
			// kept for the rerun, and the test's free() takes it.
			r.mu.Lock()
			r.pending[ws] = req
			r.deferred = append(r.deferred, err)
			r.mu.Unlock()
			return
		}
		if req.Done != nil {
			req.Done(err)
		}
	}()
}

// Deferrals answers every run error the fake re-registered as a deferral.
func (r *fakeRegistry) Deferrals() []error {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]error(nil), r.deferred...)
}

// endUnrun drops a workspace's registered bounce unrun and tells its Done
// why: bounce.ErrUnregistered, as the queue does when the shim it would
// replace departs with nothing left to replace, or bounce.ErrHandedAcross, as
// a dispatch-quiet move does when it carries the replacement to the daemon it
// took the workspace to.
func (r *fakeRegistry) endUnrun(ws ids.WorkspaceID, why error) {
	r.mu.Lock()
	req, ok := r.pending[ws]
	delete(r.pending, ws)
	r.mu.Unlock()
	if ok && req.Done != nil {
		req.Done(why)
	}
}

// free marks a workspace free and takes its registered bounce, as the queue
// does on a freeness edge. It reports whether a bounce was registered.
func (r *fakeRegistry) free(ws ids.WorkspaceID) bool {
	r.freeness.SetFree(ws, true)
	r.mu.Lock()
	req, ok := r.pending[ws]
	delete(r.pending, ws)
	r.mu.Unlock()
	if ok {
		r.run(ws, req)
	}
	return ok
}

// awaitRequest blocks until a request matching want has been made.
func (r *fakeRegistry) awaitRequest(t *testing.T, want func(registryCall) bool) registryCall {
	t.Helper()
	for {
		select {
		case call := <-r.asked:
			if want(call) {
				return call
			}
		case <-time.After(10 * time.Second):
			t.Fatalf("no matching bounce request was ever made; requests %+v", r.Requests())
			return registryCall{}
		}
	}
}

// wait joins every bounce the fake started.
func (r *fakeRegistry) wait() { r.running.Wait() }

// Requests answers every request, in order.
func (r *fakeRegistry) Requests() []registryCall {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]registryCall(nil), r.requests...)
}

// Pending reports whether a bounce is registered for ws.
func (r *fakeRegistry) Pending(ws ids.WorkspaceID) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	_, ok := r.pending[ws]
	return ok
}

// harness is one controller under test with every fake reachable.
type harness struct {
	c *controller
	// merges records the merge mover's calls, in order, as "suspend <ws>" and
	// "adopt <ws>"; suspendErr and adoptErr fail them.
	merges       *fakeMerges
	progress     *fakeProgress
	db           wsm.DB
	clock        *fakeClock
	spawner      *fakeSpawner
	announcer    *fakeAnnouncer
	pusher       *fakePusher
	participants *fakeParticipants
	freeness     *fakeFreeness
	fleet        *fakeFleet
	registry     *fakeRegistry
	order        *steps
	log          *dlog.TestSurfaces
	state        string

	mu       sync.Mutex
	quiesced []ids.WorkspaceID
	// quiesceErr fails every quiesce.
	quiesceErr   error
	drained      []ids.WorkspaceID
	leaseChanged []ids.WorkspaceID
	published    []ids.WorkspaceID
	// unreported records every StateUnreported statement, in order.
	unreported []stateUnreportedCall
	addrWrites int
	// claimFree is closed by incumbentExits: until then the boot claim is the
	// incumbent's, and AwaitBootClaim waits, as the kernel's lock does.
	// claimErr is what the wait answers once it lands.
	claimFree    chan struct{}
	claimErr     error
	exits        chan struct{}
	lockStates   map[string]sessionlock.State
	lockErr      map[string]error
	shimBuild    string
	shimBuildErr error
	coldGateCall []ids.WorkspaceID
	// started records every session start, in order; startErr fails the
	// start of the workspaces it names.
	started  []ids.WorkspaceID
	startErr map[ids.WorkspaceID]error
	// startHold, when set, runs inside every session start before it is
	// recorded, so a test holds a bring-up in flight.
	startHold func(ws ids.WorkspaceID)
	// marker records every bring-up marker edge, in order.
	marker []markerEdge
}

// incumbentExits releases the boot claim, as the outgoing daemon's exit does.
func (h *harness) incumbentExits() { close(h.claimFree) }

// markerEdge is one BringingUp call.
type markerEdge struct {
	ws ids.WorkspaceID
	up bool
}

// Started answers the workspaces whose session was started, once every start
// the controller ran off its goroutine has finished.
func (h *harness) Started() []ids.WorkspaceID {
	h.c.bringUps.Wait()
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]ids.WorkspaceID(nil), h.started...)
}

// Marker answers every bring-up marker edge, once every start has finished.
func (h *harness) Marker() []markerEdge {
	h.c.bringUps.Wait()
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]markerEdge(nil), h.marker...)
}

// newHarness builds a controller over a real WSM store in the test's temp dir:
// the state client is the one collaborator worth exercising for real, because
// the lease, serving ownership and the fault records are what the rollout's
// behavior is made of.
func newHarness(t *testing.T, adjust ...func(*Deps)) *harness {
	t.Helper()
	log := dlog.NewTestSurfaces()
	state := t.TempDir()
	db, err := wsm.Open(context.Background(), filepath.Join(state, "wsm.db"), wsm.WithLogger(log.Global()), wsm.WithUnsyncedWrites(), wsm.WithTemporaryGuard(tempdirstest.Guard(t)))
	if err != nil {
		t.Fatalf("wsm.Open: %v", err)
	}
	t.Cleanup(func() {
		if err := db.Close(); err != nil {
			t.Errorf("close the test's state client: %v", err)
		}
	})

	// THE TEST IS THE DAEMON'S LIFETIME. Work the controller runs past the
	// call that started it (a handover followed to its end, a transfer the
	// registry runs) ends with the test and is joined before its state
	// directory is removed, so nothing writes into a directory being deleted.
	lifetime, endLifetime := context.WithCancel(context.Background())

	order := &steps{}
	freeness := newFakeFreeness()
	h := &harness{
		db:           db,
		clock:        newFakeClock(instant),
		spawner:      &fakeSpawner{address: "127.0.0.1:7788"},
		announcer:    &fakeAnnouncer{},
		pusher:       &fakePusher{},
		participants: newFakeParticipants(),
		freeness:     freeness,
		fleet:        newFakeFleet(order),
		registry:     newFakeRegistry(freeness),
		order:        order,
		log:          log,
		state:        state,
		exits:        make(chan struct{}, 4),
		claimFree:    make(chan struct{}),
		lockStates:   make(map[string]sessionlock.State),
		lockErr:      make(map[string]error),
		shimBuild:    "installed-build",
		progress:     &fakeProgress{},
		startErr:     make(map[ids.WorkspaceID]error),
		merges:       &fakeMerges{order: order},
	}
	deps := Deps{
		Merges:         h.merges,
		SelfExe:        filepath.Join(state, "claude-repld"),
		SelfAddress:    "127.0.0.1:7777",
		Instance:       selfInstance,
		StateDir:       state,
		IntentManifest: filepath.Join(state, "intent", "manifest.json"),
		DB:             db,
		Spawner:        h.spawner,
		Announcer:      h.announcer,
		Pusher:         h.pusher,
		Participants:   h.participants,
		// THE HOLD IS REAL: it is taken in the harness's state, exactly as
		// handover.Intake takes it, so a test reads what a failed transfer
		// or a reclaim left behind. A lease already standing is another
		// holder's, and the transfer is handed none of its own.
		Quiesce: func(ctx context.Context, ws ids.WorkspaceID) (ids.LeaseID, error) {
			order.record("quiesce")
			h.mu.Lock()
			h.quiesced = append(h.quiesced, ws)
			quiesceErr := h.quiesceErr
			h.mu.Unlock()
			if quiesceErr != nil {
				return "", quiesceErr
			}
			if _, held, err := db.Lease(ctx, ws); err != nil || held {
				return "", err
			}
			lease, err := db.AcquireLease(ctx, ws, wsm.HolderRestart, wsm.PolicyHold)
			if err != nil {
				return "", err
			}
			return lease.ID, nil
		},
		DrainIntake: func(_ context.Context, ws ids.WorkspaceID) error {
			order.record("drain_intake")
			h.mu.Lock()
			h.drained = append(h.drained, ws)
			h.mu.Unlock()
			return nil
		},
		LeaseChanged: func(ws ids.WorkspaceID) {
			order.record("lease_changed")
			h.mu.Lock()
			h.leaseChanged = append(h.leaseChanged, ws)
			h.mu.Unlock()
		},
		Bounces:  h.registry,
		Freeness: h.freeness,
		Shims:    h.fleet,
		LockProbe: func(dir string) (sessionlock.State, error) {
			h.mu.Lock()
			defer h.mu.Unlock()
			return h.lockStates[dir], h.lockErr[dir]
		},
		StartSession: func(_ context.Context, ws ids.WorkspaceID) error {
			h.mu.Lock()
			hold := h.startHold
			h.mu.Unlock()
			if hold != nil {
				hold(ws)
			}
			order.record("start_session")
			h.mu.Lock()
			defer h.mu.Unlock()
			h.started = append(h.started, ws)
			return h.startErr[ws]
		},
		BringingUp: func(ws ids.WorkspaceID, up bool) {
			order.record(fmt.Sprintf("bringing_up:%v", up))
			h.mu.Lock()
			defer h.mu.Unlock()
			h.marker = append(h.marker, markerEdge{ws: ws, up: up})
		},
		PublishViews: func(_ context.Context, ws ids.WorkspaceID) error {
			order.record("publish_views")
			h.mu.Lock()
			h.published = append(h.published, ws)
			h.mu.Unlock()
			return nil
		},
		StateUnreported: func(ws ids.WorkspaceID, unreported bool) {
			h.mu.Lock()
			h.unreported = append(h.unreported, stateUnreportedCall{ws: ws, unreported: unreported})
			h.mu.Unlock()
		},
		AwaitBootClaim: func(ctx context.Context) error {
			select {
			case <-h.claimFree:
				h.mu.Lock()
				defer h.mu.Unlock()
				return h.claimErr
			case <-ctx.Done():
				return ctx.Err()
			}
		},
		WriteDaemonAddr: func(context.Context) error {
			h.mu.Lock()
			h.addrWrites++
			h.mu.Unlock()
			return nil
		},
		ShimBuild: func() (string, error) {
			h.mu.Lock()
			defer h.mu.Unlock()
			return h.shimBuild, h.shimBuildErr
		},
		ColdGate: func(_ context.Context, ws ids.WorkspaceID, _ *conversationv1.SessionCold) error {
			order.record("cold_gate")
			h.mu.Lock()
			h.coldGateCall = append(h.coldGateCall, ws)
			h.mu.Unlock()
			return nil
		},
		Exit: func(context.Context) error {
			order.record("exit")
			h.exits <- struct{}{}
			return nil
		},
		ExpectedOutage:   5 * time.Second,
		AdoptionWindow:   30 * time.Second,
		HoldoutWarnEvery: 10 * time.Minute,
		StandDownWindow:  30 * time.Second,
		Clock:            h.clock,
		Progress:         h.progress,
		Log:              log,
		Lifetime:         lifetime,
	}
	h.registry.runCtx = lifetime
	for _, a := range adjust {
		a(&deps)
	}
	controllerAny, err := New(deps)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	c, ok := controllerAny.(*controller)
	if !ok {
		t.Fatalf("New returned %T, want *controller", controllerAny)
	}
	h.c = c
	t.Cleanup(func() {
		endLifetime()
		// THE CONTROLLER'S OWN GOROUTINES ARE JOINED FIRST: a handover asks
		// the registry for bounces, and each run is Added to `running` from
		// that goroutine. Waiting on `running` first raced those Adds (a
		// WaitGroup Add concurrent with its Wait at zero), which -race
		// reported on TestAMoveTakenBackAsksForItsCarriedReplacementAgainHere.
		c.handoverDone.Wait()
		c.stragglerAdoptions.Wait()
		c.bringUps.Wait()
		h.registry.running.Wait()
	})
	return h
}

// publishedWorkspaces answers the workspaces whose fresh views were published,
// which is the last step of an adoption and therefore proof it ran to the end.
func (h *harness) publishedWorkspaces() []ids.WorkspaceID {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]ids.WorkspaceID(nil), h.published...)
}

// workspace registers one workspace served by this daemon, with a live shim.
func (h *harness) workspace(t *testing.T) (ids.WorkspaceID, string) {
	t.Helper()
	dir := t.TempDir()
	ws, _, err := h.db.RegisterWorkspace(context.Background(), dir, wsm.RegisterFacts{
		Name: filepath.Base(dir), Branch: "feature", ParentBranch: "master", RepoDir: dir,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	pid := 4242
	if err := h.db.PutSession(context.Background(), wsm.Session{
		Workspace: ws.ID, HostSessionID: "host-" + string(ws.ID),
		VendorSessionID: "vendor-" + string(ws.ID), ConfigDir: dir,
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: instant,
		ShimPID: &pid,
	}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := h.db.ClaimServing(context.Background(), ws.ID, selfInstance); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	h.freeness.SetFree(ws.ID, true)
	h.fleet.live[ws.ID] = newFakeShim(pid, h.order)
	// The registry NORMALIZES the dir (macOS resolves /var to /private/var), and
	// the lock probe is called with the registry's spelling: keying the fake
	// probe by anything else would silently answer "could not tell".
	h.mu.Lock()
	h.lockStates[ws.Dir] = sessionlock.StateHeld
	h.mu.Unlock()
	return ws.ID, ws.Dir
}

// records returns every captured record whose operation matches.
func records(log *dlog.TestSurfaces, operation string) []dlog.Record {
	var out []dlog.Record
	for _, rec := range log.Records() {
		if rec.Operation == operation {
			out = append(out, rec)
		}
	}
	return out
}

// levelRecords narrows captured records to one level, in order.
func levelRecords(in []dlog.Record, level string) []dlog.Record {
	var out []dlog.Record
	for _, rec := range in {
		if rec.Level == level {
			out = append(out, rec)
		}
	}
	return out
}

// indexOf is the position of one step in a recorded sequence, or -1.
func indexOf(taken []string, step string) int {
	for i, s := range taken {
		if s == step {
			return i
		}
	}
	return -1
}

// writeFile replaces a file's whole content, for the tests that corrupt one.
func writeFile(path, content string) error {
	return os.WriteFile(path, []byte(content), 0o644)
}

// waitForForceKill blocks until the engine has force-killed a shim, which is
// how a test knows the stand-down window's expiry was acted on.
func waitForForceKill(t *testing.T, shim *fakeShim) {
	t.Helper()
	select {
	case <-shim.forced:
	case <-time.After(10 * time.Second):
		t.Fatalf("the shim was never force-killed")
	}
}

// joiningHandle swaps the controller's state client for a READ-ONLY handle on
// the harness's own store, which is what a joining successor boots with
// (wsm.OpenReadOnly): nothing may be written until the handle is promoted.
func (h *harness) joiningHandle(t *testing.T) {
	t.Helper()
	ro, err := wsm.OpenReadOnly(context.Background(), filepath.Join(h.state, "wsm.db"), wsm.WithLogger(h.log.Global()), wsm.WithUnsyncedWrites(), wsm.WithTemporaryGuard(tempdirstest.Guard(t)))
	if err != nil {
		t.Fatalf("wsm.OpenReadOnly: %v", err)
	}
	t.Cleanup(func() {
		if err := ro.Close(); err != nil {
			t.Errorf("close the joining handle: %v", err)
		}
	})
	h.c.deps.DB = ro
}

// successorAdopts completes the rendezvous of every workspace the handover has
// transferred so far, as the participants' adoption calls on the successor
// would: each adoption window then ends as adopted rather than expired.
func (h *harness) successorAdopts(t *testing.T) {
	t.Helper()
	for _, call := range h.pusher.Calls() {
		if call.Kind != "transferred" {
			continue
		}
		h.c.mu.Lock()
		e := h.c.rendezvous[call.WS]
		h.c.mu.Unlock()
		if e == nil {
			t.Fatalf("no rendezvous is armed for the transferred workspace %s", call.WS)
		}
		// The successor consumes the carry as its adoption lands.
		if err := os.Remove(h.c.carryPath(call.WS)); err != nil && !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("consume the carry of %s: %v", call.WS, err)
		}
		e.settle(nil)
	}
}

// fakeProgress records every statement the rollout made on the update line,
// nil (a take-down) included.
type fakeProgress struct {
	mu     sync.Mutex
	stated []*deployprogress.Progress
}

func (f *fakeProgress) SetDeployProgress(p *deployprogress.Progress) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.stated = append(f.stated, p)
}

// statements answers a copy of every statement, in order.
func (f *fakeProgress) statements() []*deployprogress.Progress {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]*deployprogress.Progress(nil), f.stated...)
}

// stateUnreportedCall is one StateUnreported statement.
type stateUnreportedCall struct {
	ws         ids.WorkspaceID
	unreported bool
}

// Unreported reads back the StateUnreported statements, in order.
func (h *harness) Unreported() []stateUnreportedCall {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]stateUnreportedCall(nil), h.unreported...)
}

// fakeMerges is the merge mover: it records each call on the harness's shared
// order, so a test asserts a merge stops before its shim moves.
type fakeMerges struct {
	order      *steps
	mu         sync.Mutex
	calls      []string
	suspendErr error
	adoptErr   error
}

func (f *fakeMerges) SuspendForTransfer(_ context.Context, ws ids.WorkspaceID) error {
	f.order.record("suspend_merge")
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "suspend "+string(ws))
	return f.suspendErr
}

func (f *fakeMerges) AdoptWorkspace(_ context.Context, ws ids.WorkspaceID) error {
	f.order.record("adopt_merges")
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "adopt "+string(ws))
	return f.adoptErr
}

// called reports the merge mover's calls.
func (f *fakeMerges) called() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.calls...)
}

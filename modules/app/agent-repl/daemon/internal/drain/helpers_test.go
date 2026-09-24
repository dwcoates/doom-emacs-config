package drain

import (
	"context"
	"errors"
	"path/filepath"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// instant is the fixed instant every test's arithmetic starts from: no test in
// this package reads the wall clock.
var instant = time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

// fakeClock is a Clock the test drives: Now is whatever the test set, and After
// hands out ONE channel the test ticks, so a loop iteration advances exactly
// when the test says so and never on a sleep.
type fakeClock struct {
	mu   sync.Mutex
	now  time.Time
	tick chan time.Time
	// waits records every duration After was asked for, so a test can assert
	// the wait was sized to the deadline rather than to the sweep cadence.
	waits []time.Duration
	// asked announces every After call, so a test synchronizes on the loop
	// having reached its wait instead of polling for it.
	asked chan time.Duration
}

func newFakeClock(now time.Time) *fakeClock {
	return &fakeClock{now: now, tick: make(chan time.Time, 1), asked: make(chan time.Duration, 16)}
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *fakeClock) Set(now time.Time) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = now
}

func (c *fakeClock) After(d time.Duration) <-chan time.Time {
	c.mu.Lock()
	c.waits = append(c.waits, d)
	ch := c.tick
	c.mu.Unlock()
	c.asked <- d
	return ch
}

// Waits returns every duration After was asked for.
func (c *fakeClock) Waits() []time.Duration {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]time.Duration, len(c.waits))
	copy(out, c.waits)
	return out
}

// Tick releases the one outstanding wait.
func (c *fakeClock) Tick() {
	c.mu.Lock()
	ch := c.tick
	c.mu.Unlock()
	ch <- c.Now()
}

// fakeAnnouncer captures the WatchDaemon pushes.
type fakeAnnouncer struct {
	mu        sync.Mutex
	scheduled []*agentreplv1.DaemonDrainScheduled
	cancelled int
	shutdowns []*agentreplv1.DaemonShutdownAnnounced
}

func (a *fakeAnnouncer) DrainScheduled(push *agentreplv1.DaemonDrainScheduled) {
	a.mu.Lock()
	defer a.mu.Unlock()
	a.scheduled = append(a.scheduled, push)
}

func (a *fakeAnnouncer) DrainCancelled(*agentreplv1.DaemonDrainCancelled) {
	a.mu.Lock()
	defer a.mu.Unlock()
	a.cancelled++
}

func (a *fakeAnnouncer) ShutdownAnnounced(push *agentreplv1.DaemonShutdownAnnounced) {
	a.mu.Lock()
	defer a.mu.Unlock()
	a.shutdowns = append(a.shutdowns, push)
}

func (a *fakeAnnouncer) Scheduled() []*agentreplv1.DaemonDrainScheduled {
	a.mu.Lock()
	defer a.mu.Unlock()
	return append([]*agentreplv1.DaemonDrainScheduled(nil), a.scheduled...)
}

func (a *fakeAnnouncer) Cancelled() int {
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.cancelled
}

func (a *fakeAnnouncer) Shutdowns() []*agentreplv1.DaemonShutdownAnnounced {
	a.mu.Lock()
	defer a.mu.Unlock()
	return append([]*agentreplv1.DaemonShutdownAnnounced(nil), a.shutdowns...)
}

// fakeReviving answers the prompt queue's revival state from a set the test
// controls.
type fakeReviving struct {
	mu       sync.Mutex
	reviving map[ids.WorkspaceID]bool
}

func newFakeReviving() *fakeReviving {
	return &fakeReviving{reviving: make(map[ids.WorkspaceID]bool)}
}

func (f *fakeReviving) Reviving(ws ids.WorkspaceID) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.reviving[ws]
}

func (f *fakeReviving) Set(ws ids.WorkspaceID, reviving bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.reviving[ws] = reviving
}

// fakeFreeness answers freeness from a set the test controls, and lets a test
// release a pending AwaitFree through a channel rather than a sleep.
type fakeFreeness struct {
	mu      sync.Mutex
	free    map[ids.WorkspaceID]bool
	release map[ids.WorkspaceID]chan struct{}
	awaited []ids.WorkspaceID
	// calls announces every AwaitFree entry, so a test synchronizes on the
	// wait having STARTED rather than polling for it.
	calls chan ids.WorkspaceID
}

func newFakeFreeness() *fakeFreeness {
	return &fakeFreeness{
		free:    make(map[ids.WorkspaceID]bool),
		release: make(map[ids.WorkspaceID]chan struct{}),
		calls:   make(chan ids.WorkspaceID, 16),
	}
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

// Gate installs a channel a pending AwaitFree for ws blocks on.
func (f *fakeFreeness) Gate(ws ids.WorkspaceID) chan struct{} {
	f.mu.Lock()
	defer f.mu.Unlock()
	gate := make(chan struct{})
	f.release[ws] = gate
	return gate
}

func (f *fakeFreeness) AwaitFree(ctx context.Context, ws ids.WorkspaceID) error {
	f.mu.Lock()
	f.awaited = append(f.awaited, ws)
	gate := f.release[ws]
	f.mu.Unlock()
	f.calls <- ws
	if gate == nil {
		return nil
	}
	select {
	case <-gate:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

func (f *fakeFreeness) Awaited() []ids.WorkspaceID {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]ids.WorkspaceID(nil), f.awaited...)
}

// fakeStand is the shim contact the sweep makes.
type fakeStand struct {
	mu sync.Mutex
	// answer is the Hibernate answer per workspace; the zero value is success.
	answer map[ids.WorkspaceID]*shimv1.HibernateResponse
	// hibernateErr fails the directive itself.
	hibernateErr map[ids.WorkspaceID]error
	// killErr fails the stand-down.
	killErr map[ids.WorkspaceID]error
	// wedge, when set, is the shim that ACCEPTS a call and never answers: both
	// verbs block on their own context until the caller's bound ends it. It is
	// the shape the idle-sweep hang was found in.
	wedge bool
	// wedgeKill wedges only the stand-down.
	wedgeKill bool
	// wedgeKillOnWS wedges the stand-down of NAMED workspaces only.
	wedgeKillOnWS map[ids.WorkspaceID]bool
	// notServing names the workspaces this stand holds no addressable shim
	// for. The zero value serves every workspace, so a test states the
	// absence rather than every presence.
	notServing map[ids.WorkspaceID]bool

	hibernated []ids.WorkspaceID
	killed     []killCall
	// kills announces every stand-down, so a test synchronizes on one having
	// happened rather than polling for it.
	kills chan killCall
}

type killCall struct {
	WS    ids.WorkspaceID
	Force bool
}

func newFakeStand() *fakeStand {
	return &fakeStand{
		answer:        make(map[ids.WorkspaceID]*shimv1.HibernateResponse),
		hibernateErr:  make(map[ids.WorkspaceID]error),
		killErr:       make(map[ids.WorkspaceID]error),
		wedgeKillOnWS: make(map[ids.WorkspaceID]bool),
		notServing:    make(map[ids.WorkspaceID]bool),
		kills:         make(chan killCall, 16),
	}
}

func (s *fakeStand) Serving(ws ids.WorkspaceID) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return !s.notServing[ws]
}

// standDown makes the stand hold no addressable shim for one workspace, the
// way the fleet answers a session it never installed or has already torn down.
func (s *fakeStand) standDown(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.notServing[ws] = true
}

func (s *fakeStand) Hibernate(ctx context.Context, ws ids.WorkspaceID) (*shimv1.HibernateResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.hibernated = append(s.hibernated, ws)
	if s.wedge {
		<-ctx.Done()
		return nil, ctx.Err()
	}
	if err := s.hibernateErr[ws]; err != nil {
		return nil, err
	}
	if answer := s.answer[ws]; answer != nil {
		return answer, nil
	}
	return &shimv1.HibernateResponse{
		Result: &shimv1.HibernateResponse_Success{Success: &shimv1.HibernateSuccess{}},
	}, nil
}

func (s *fakeStand) KillSession(ctx context.Context, ws ids.WorkspaceID, force bool) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	call := killCall{WS: ws, Force: force}
	s.killed = append(s.killed, call)
	err := s.killErr[ws]
	s.kills <- call
	// A DEAD CONTEXT IS ANSWERED, NOT IGNORED. The real stand is a Connect
	// round trip: handed a context that is already done it never reaches the
	// shim. A fake that stood a session down on an expired budget anyway would
	// hide exactly the defect the bound-derivation tests are about.
	if cerr := ctx.Err(); cerr != nil {
		return cerr
	}
	if s.wedge || s.wedgeKill || s.wedgeKillOnWS[ws] {
		<-ctx.Done()
		return ctx.Err()
	}
	return err
}

// wedgeKillOn wedges ONE workspace's stand-down, leaving its siblings
// answering. It is how a test states "this shim will not go" without also
// saying it of the workspaces the pass must still reach.
func (s *fakeStand) wedgeKillOn(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.wedgeKillOnWS[ws] = true
}

// wedgeKillOnly leaves the directive answering and wedges only the
// stand-down, which is the half the idle-sweep hang was observed in.
func (s *fakeStand) wedgeKillOnly() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.wedgeKill = true
}

// failKill makes one workspace's stand-down refuse.
func (s *fakeStand) failKill(ws ids.WorkspaceID, err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.killErr[ws] = err
}

func (s *fakeStand) Killed() []killCall {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]killCall(nil), s.killed...)
}

// fakeSpawns is the shim supervisor's spawn sweep. It records every call and
// can be made to refuse, which is how the "a spawn whose kill fails is
// reported" case is arranged.
type fakeSpawns struct {
	mu      sync.Mutex
	reasons []string
	// bounds records the deadline left on each call's context, so a test can
	// assert the sweep was given a bound at all rather than the caller's own.
	bounds []time.Duration
	err    error
	// wedge, when set, is the supervisor that accepts the sweep and never
	// answers until the caller's bound ends it.
	wedge bool
	// calls announces every sweep, so a test synchronizes on one having
	// happened rather than polling for it.
	calls chan string
	// witness reads how many registered sessions had been stood down at the
	// moment the sweep was asked for, which is how ORDER is asserted without a
	// clock.
	witness func() int
	// seen records that witness, once per call.
	seen []int
	// latched is the supervisor's stand-down latch, and latchedAt reads how
	// many registered sessions had been stood down when it went up -- which
	// is how the ORDER of the latch against the walk is asserted without a
	// clock.
	latched   bool
	latchedAt int
}

func newFakeSpawns() *fakeSpawns {
	return &fakeSpawns{calls: make(chan string, 16)}
}

// LatchedAt reads how many registered sessions had been stood down when the
// stand-down latched.
func (s *fakeSpawns) LatchedAt() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.latchedAt
}

// BeginStandDown latches the fake supervisor's stand-down, recording how far
// the caller's session walk had got when it did.
func (s *fakeSpawns) BeginStandDown() bool {
	s.mu.Lock()
	witness := s.witness
	already := s.latched
	s.latched = true
	s.mu.Unlock()
	if already {
		return false
	}
	at := 0
	if witness != nil {
		at = witness()
	}
	s.mu.Lock()
	s.latchedAt = at
	s.mu.Unlock()
	return true
}

// SpawnedFor answers the supervisor's live spawn registry. This fake holds no
// clients of its own, so it owns no spawn for any workspace.
func (s *fakeSpawns) SpawnedFor(ids.WorkspaceID) (int, bool) { return 0, false }

// StandingDown answers the fake supervisor's latch.
func (s *fakeSpawns) StandingDown() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.latched
}

func (s *fakeSpawns) StandDownEverySpawn(ctx context.Context, reason string) error {
	s.mu.Lock()
	s.reasons = append(s.reasons, reason)
	if deadline, ok := ctx.Deadline(); ok {
		s.bounds = append(s.bounds, time.Until(deadline))
	}
	err := s.err
	wedge := s.wedge
	witness := s.witness
	s.mu.Unlock()
	if witness != nil {
		s.mu.Lock()
		s.seen = append(s.seen, witness())
		s.mu.Unlock()
	}
	s.calls <- reason
	if wedge {
		<-ctx.Done()
		return ctx.Err()
	}
	return err
}

// Reasons returns every reason the sweep was called with.
func (s *fakeSpawns) Reasons() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.reasons...)
}

// Bounds returns the remaining budget observed on each call's context.
func (s *fakeSpawns) Bounds() []time.Duration {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]time.Duration(nil), s.bounds...)
}

// Seen returns, per call, how many registered stand-downs had already
// happened when the sweep ran.
func (s *fakeSpawns) Seen() []int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]int(nil), s.seen...)
}

// fail makes the sweep report a failure.
func (s *fakeSpawns) fail(err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.err = err
}

// harness is one controller under test with every fake reachable.
type harness struct {
	c         *controller
	db        wsm.DB
	clock     *fakeClock
	announcer *fakeAnnouncer
	freeness  *fakeFreeness
	reviving  *fakeReviving
	stand     *fakeStand
	spawns    *fakeSpawns
	log       *dlog.TestSurfaces
	exits     chan struct{}
}

// newHarness builds a controller over a real WSM store in the test's temp dir —
// the state client is the one collaborator worth exercising for real, because
// the lease and the schedule row are what the drain's behavior is made of.
func newHarness(t *testing.T, adjust ...func(*Deps)) *harness {
	t.Helper()
	t.Setenv(IdleCutoffEnv, "")
	log := dlog.NewTestSurfaces()
	db, err := wsm.Open(context.Background(), filepath.Join(t.TempDir(), "wsm.db"), wsm.WithLogger(log.Global()))
	if err != nil {
		t.Fatalf("wsm.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })

	h := &harness{
		db:        db,
		clock:     newFakeClock(instant),
		announcer: &fakeAnnouncer{},
		freeness:  newFakeFreeness(),
		reviving:  newFakeReviving(),
		stand:     newFakeStand(),
		spawns:    newFakeSpawns(),
		log:       log,
		exits:     make(chan struct{}, 4),
	}
	deps := Deps{
		DB:            db,
		IdleCutoff:    time.Hour,
		SweepEvery:    5 * time.Minute,
		RefusalWindow: time.Minute,
		Stand:         h.stand,
		Spawns:        h.spawns,
		Freeness:      h.freeness,
		Reviving:      h.reviving.Reviving,
		Announcer:     h.announcer,
		Exit:          func(context.Context) error { h.exits <- struct{}{}; return nil },
		Clock:         h.clock,
		Log:           log,
	}
	h.spawns.witness = func() int { return len(h.stand.Killed()) }
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
	return h
}

// workspace registers one workspace with a live session engaged at engagedAt.
func (h *harness) workspace(t *testing.T, engagedAt time.Time) ids.WorkspaceID {
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
		Model: "opus", PermissionMode: "default", StartedAt: instant, LastEngagementAt: engagedAt,
		ShimPID: &pid,
	}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	h.freeness.SetFree(ws.ID, true)
	return ws.ID
}

// bare registers one workspace with no session at all.
func (h *harness) bare(t *testing.T) ids.WorkspaceID {
	t.Helper()
	dir := t.TempDir()
	ws, _, err := h.db.RegisterWorkspace(context.Background(), dir, wsm.RegisterFacts{
		Name: filepath.Base(dir), Branch: "feature", ParentBranch: "master", RepoDir: dir,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	h.freeness.SetFree(ws.ID, true)
	return ws.ID
}

// deployReason is the typed reason every scheduling test uses.
func deployReason(t *testing.T) string {
	t.Helper()
	encoded, err := EncodeReason(&agentreplv1.DrainReason{
		Kind: &agentreplv1.DrainReason_Deploy{Deploy: &agentreplv1.DrainReasonDeploy{}},
	})
	if err != nil {
		t.Fatalf("EncodeReason: %v", err)
	}
	return encoded
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

// errFake is the failure the fakes return when a test asks one to fail.
var errFake = errors.New("the fake refused")

// warnRecords narrows captured records to the WARN ones, in order.
func warnRecords(in []dlog.Record) []dlog.Record {
	var out []dlog.Record
	for _, rec := range in {
		if rec.Level == "warn" {
			out = append(out, rec)
		}
	}
	return out
}

// waitForWait blocks until the drain loop has reached its next wait, which is
// how a test knows one iteration finished without polling or sleeping.
func waitForWait(t *testing.T, h *harness) time.Duration {
	t.Helper()
	select {
	case d := <-h.clock.asked:
		return d
	case <-time.After(5 * time.Second):
		t.Fatalf("the drain loop never reached a wait")
		return 0
	}
}

// waitForHibernation blocks until one workspace has been stood down.
func waitForHibernation(t *testing.T, h *harness, ws ids.WorkspaceID) {
	t.Helper()
	for {
		select {
		case call := <-h.stand.kills:
			if call.WS == ws {
				return
			}
		case <-time.After(5 * time.Second):
			t.Fatalf("workspace %s was never stood down", ws)
			return
		}
	}
}

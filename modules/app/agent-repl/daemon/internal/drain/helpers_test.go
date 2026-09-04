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
		answer:       make(map[ids.WorkspaceID]*shimv1.HibernateResponse),
		hibernateErr: make(map[ids.WorkspaceID]error),
		killErr:      make(map[ids.WorkspaceID]error),
		kills:        make(chan killCall, 16),
	}
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
	if s.wedge || s.wedgeKill {
		<-ctx.Done()
		return ctx.Err()
	}
	return err
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

// harness is one controller under test with every fake reachable.
type harness struct {
	c         *controller
	db        wsm.DB
	clock     *fakeClock
	announcer *fakeAnnouncer
	freeness  *fakeFreeness
	stand     *fakeStand
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
		stand:     newFakeStand(),
		log:       log,
		exits:     make(chan struct{}, 4),
	}
	deps := Deps{
		DB:            db,
		IdleCutoff:    time.Hour,
		SweepEvery:    5 * time.Minute,
		RefusalWindow: time.Minute,
		Stand:         h.stand,
		Freeness:      h.freeness,
		Announcer:     h.announcer,
		Exit:          func(context.Context) error { h.exits <- struct{}{}; return nil },
		Clock:         h.clock,
		Log:           log,
	}
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
		Workspace: ws.ID, VendorSessionID: "vendor-" + string(ws.ID), ConfigDir: dir,
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

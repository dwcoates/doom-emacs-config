package merge

import (
	"context"
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// --- fakes --------------------------------------------------------------

// sameRequest compares two requests IGNORING the run they carry.
//
// merge.Request grew a *RunStatus and the id it publishes under, both minted per
// run and therefore never equal across two values by construction. What these
// assertions are about is the request's IDENTITY — which workspace, which
// branch, which target — so the run is normalized away rather than every
// assertion being weakened to a field-by-field spot check.
// sameRequest compares two requests by the IDENTITY a merge is addressed by,
// ignoring the per-run bookkeeping the pipeline attaches as it goes: the live
// publisher, the run id, and the rebase worktree the coordinator threads through
// the steps that follow a park. A test asserting "this is the same merge" is
// asking about the workspace, branch and directories, never about which temp
// tree the run happened to make.
func sameRequest(a, b Request) bool {
	a.Run, b.Run = nil, nil
	a.RunID, b.RunID = "", ""
	a.WorkDir, b.WorkDir = "", ""
	a.BaseHead, b.BaseHead = "", ""
	return a == b
}

const errStopped = sentinelError("fake picker stopped")

type pickResult struct {
	res Result
	err error
}

type queuedRecord struct {
	ws    string
	cause string
}

// fakePicker stands in for merge.Driver. Every call announces itself on a
// channel and then waits for the test to hand it an outcome, which is what
// lets a test observe the coordinator's ordering without a sleep.
type fakePicker struct {
	merges        chan Request
	results       chan pickResult
	resumes       chan Request
	resumeResults chan pickResult
	// continues announces each ContinueAfterTestFix, and continueResults hands
	// it the outcome of the re-run suite plus the rest of the replay.
	continues       chan string
	continueResults chan pickResult
	// cleanups announces each Cleanup with the rebase worktree it was asked to
	// discard, which is how a test pins that the temp tree never outlives its
	// merge. It REPLACED the rollback channels: there is no rollback to observe
	// any more, because a failed merge never modified the target.
	cleanups   chan string
	cleanupErr error
	queued     chan queuedRecord
	queuedErr  error
	stop       chan struct{}
}

func newFakePicker(capacity int) *fakePicker {
	return &fakePicker{
		merges:          make(chan Request, capacity),
		results:         make(chan pickResult, capacity),
		resumes:         make(chan Request, capacity),
		resumeResults:   make(chan pickResult, capacity),
		continues:       make(chan string, capacity),
		continueResults: make(chan pickResult, capacity),
		cleanups:        make(chan string, capacity),
		queued:          make(chan queuedRecord, capacity),
		stop:            make(chan struct{}),
	}
}

// Merge HONORS ITS CONTEXT, exactly as merge.Driver does: every git command
// and every suite the real one runs is ctx-bound, so a cancelled run stops
// where it is. Without that case here a dequeue of a running merge would
// deadlock against a fake that only the test can release, and the abort path
// could not be exercised at all.
func (p *fakePicker) Merge(ctx context.Context, req Request) (Result, error) {
	p.merges <- req
	select {
	case r := <-p.results:
		return r.res, r.err
	case <-ctx.Done():
		return Result{}, ctx.Err()
	case <-p.stop:
		return Result{}, errStopped
	}
}

func (p *fakePicker) Resume(ctx context.Context, req Request) (Result, error) {
	p.resumes <- req
	select {
	case r := <-p.resumeResults:
		return r.res, r.err
	case <-ctx.Done():
		return Result{}, ctx.Err()
	case <-p.stop:
		return Result{}, errStopped
	}
}

func (p *fakePicker) ContinueAfterTestFix(ctx context.Context, _ Request, failingCommit string) (Result, error) {
	p.continues <- failingCommit
	select {
	case r := <-p.continueResults:
		return r.res, r.err
	case <-ctx.Done():
		return Result{}, ctx.Err()
	case <-p.stop:
		return Result{}, errStopped
	}
}

func (p *fakePicker) Cleanup(_ context.Context, req Request) error {
	p.cleanups <- req.WorkDir
	return p.cleanupErr
}

func (p *fakePicker) MarkQueued(ws, cause string) error {
	p.queued <- queuedRecord{ws: ws, cause: cause}
	return p.queuedErr
}

// fakeLease stands in for merge.Lease. Acquire and the returned release each
// announce themselves, so a test can assert the lease bracket around a merge.
type fakeLease struct {
	acquires   chan string
	releases   chan string
	err        error
	nilRelease bool

	mu   sync.Mutex
	held map[string]bool
}

func newFakeLease(capacity int) *fakeLease {
	return &fakeLease{
		acquires: make(chan string, capacity),
		releases: make(chan string, capacity),
		held:     map[string]bool{},
	}
}

func (l *fakeLease) Acquire(_ context.Context, ws string) (func(), error) {
	l.acquires <- ws
	if l.err != nil {
		return nil, l.err
	}
	if l.nilRelease {
		return nil, nil
	}
	l.mu.Lock()
	l.held[ws] = true
	l.mu.Unlock()
	return func() {
		l.mu.Lock()
		l.held[ws] = false
		l.mu.Unlock()
		l.releases <- ws
	}, nil
}

func (l *fakeLease) Held(ws string) bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.held[ws]
}

// fakeResolver stands in for merge.ConflictResolver. Like fakePicker it
// announces the call and then waits for the test to say how the resolution turn
// ended, so a test that never answers leaves the shim attempt pending — which is
// exactly the state every test about the HUMAN resume path wants.
type fakeResolver struct {
	calls   chan ConflictResolution
	results chan error
	stop    chan struct{}
}

func newFakeResolver(capacity int) *fakeResolver {
	return &fakeResolver{
		calls:   make(chan ConflictResolution, capacity),
		results: make(chan error, capacity),
		stop:    make(chan struct{}),
	}
}

func (r *fakeResolver) Resolve(_ context.Context, res ConflictResolution) error {
	r.calls <- res
	select {
	case err := <-r.results:
		return err
	case <-r.stop:
		return errStopped
	}
}

// fakeTestFailureResolver stands in for merge.TestFailureResolver. Like
// fakeResolver it announces the call and then waits for the test to say how the
// fix turn ended.
type fakeTestFailureResolver struct {
	calls   chan TestFailureResolution
	results chan error
	stop    chan struct{}
}

func newFakeTestFailureResolver(capacity int) *fakeTestFailureResolver {
	return &fakeTestFailureResolver{
		calls:   make(chan TestFailureResolution, capacity),
		results: make(chan error, capacity),
		stop:    make(chan struct{}),
	}
}

func (r *fakeTestFailureResolver) Resolve(ctx context.Context, res TestFailureResolution) error {
	r.calls <- res
	select {
	case err := <-r.results:
		return err
	case <-ctx.Done():
		return ctx.Err()
	case <-r.stop:
		return errStopped
	}
}

// fakePostMergeHook stands in for merge.PostMergeHook. Like fakePicker it
// announces the call and then waits for the test to release it, which is what
// lets a test hold the hook open and prove the repository's queue advances
// underneath it without a sleep.
type fakePostMergeHook struct {
	calls   chan Request
	results chan error
	stop    chan struct{}
}

func newFakePostMergeHook(capacity int) *fakePostMergeHook {
	return &fakePostMergeHook{
		calls:   make(chan Request, capacity),
		results: make(chan error, capacity),
		stop:    make(chan struct{}),
	}
}

// testAfterActionPrompt is the default creation-time postprocessing action.
//
// IT IS NON-EMPTY DELIBERATELY. The coordinator publishes the after_action
// phase IFF the workspace has one configured, so a double answering "" would
// describe a workspace with no after-action and silence the very phase these
// tests are about.
const testAfterActionPrompt = "finish the merged workspace's postprocessing"

func (h *fakePostMergeHook) AfterMerged(_ context.Context, req Request) error {
	h.calls <- req
	select {
	case err := <-h.results:
		return err
	case <-h.stop:
		return errStopped
	}
}

// autoPostMergeHook is the harness default: it records the call and returns
// immediately, so every test that is not ABOUT the hook is unaffected by it.
type autoPostMergeHook struct {
	calls chan Request
}

func newAutoPostMergeHook(capacity int) *autoPostMergeHook {
	return &autoPostMergeHook{calls: make(chan Request, capacity)}
}

func (h *autoPostMergeHook) AfterMerged(_ context.Context, req Request) error {
	select {
	case h.calls <- req:
	default:
	}
	return nil
}

// fakeKeyer maps a target worktree to a repository key, modeling the sibling
// worktrees that must collapse onto one queue.
type fakeKeyer struct {
	keys map[string]string
	err  error
}

func (k fakeKeyer) RepoKey(_ context.Context, dir string) (string, error) {
	if k.err != nil {
		return "", k.err
	}
	if key, ok := k.keys[dir]; ok {
		return key, nil
	}
	return testRepoKey, nil
}

// syncSink is a StateSink safe to read while a drain goroutine writes it.
type syncSink struct {
	mu       sync.Mutex
	got      []transition
	statuses []*frontendv1.MergeStatus
	ch       chan transition
	err      error
	// failPhase is the ONE phase whose record fails, and phaseErr is what it
	// fails with. failNext's blanket failure cannot serve a test about a
	// TERMINAL publication: every earlier phase of the run would fail with it,
	// and the run would die somewhere else entirely.
	failPhase Phase
	phaseErr  error
}

func newSyncSink(capacity int) *syncSink {
	return &syncSink{ch: make(chan transition, capacity)}
}

func (s *syncSink) RecordMergeTransition(ws string, phase Phase, cause string) error {
	s.mu.Lock()
	s.got = append(s.got, transition{ws, phase, cause})
	err := s.err
	if s.phaseErr != nil && phase == s.failPhase {
		err = s.phaseErr
	}
	s.mu.Unlock()
	s.ch <- transition{ws, phase, cause}
	return err
}

// RecordMergeStatus makes the double the StatusSink too, exactly as the
// production mergeSink is both: the two ends of one publication travel one
// object here for the same reason they travel one call there.
func (s *syncSink) RecordMergeStatus(ws string, phase Phase, cause string, status *frontendv1.MergeStatus) error {
	s.mu.Lock()
	s.statuses = append(s.statuses, status)
	s.mu.Unlock()
	return s.RecordMergeTransition(ws, phase, cause)
}

// awaitPhase drains the sink's channel until a transition on phase arrives.
//
// It exists because every run now announces its ADMISSION before anything else
// does (the `enqueued` status, on merge_enqueuing at the head and merge_queued
// behind it), so a test waiting for a LATER phase has a record ahead of it that
// is not the one under assertion. Draining the ordered channel the fakes already
// use is the wait; there is nothing to sleep on.
func (s *syncSink) awaitPhase(t *testing.T, phase Phase) transition {
	t.Helper()
	for {
		tr := <-s.ch
		if tr.phase == phase {
			return tr
		}
		t.Logf("sink: skipped %s on %s while waiting for %s", tr.phase, tr.ws, phase)
	}
}

// awaitQueueDrained waits until repo's durable queue holds nothing.
//
// THE LEASE-ACQUIRE FAILURE IS THE ONE TERMINAL PATH WITH NO RENDEZVOUS AFTER
// IT. Every other drained-to-zero assertion in this file waits on
// `<-h.lease.releases` first, and that is a real synchronization point:
// QueueCoordinator.finish drops the durable entry and THEN releases, so a
// received release proves the drop already happened. A run whose lease was
// never acquired has no release to wait on.
//
// And the terminal STATUS cannot stand in for one. It is published BEFORE the
// entry is dropped, deliberately (runstatus.go: emitting it after would let a
// bounce land between the two), so observing merge_failed says nothing about
// the queue and a test that read the snapshot straight after it was racing the
// drop it meant to assert.
//
// So the property is asserted as what it actually is — the one the test names,
// "a merge that can never run must not block the queue FOREVER". It is polled
// against a deadline and FAILS with the observed depth; a timeout is never a
// pass, so this cannot go green on a queue that stayed blocked.
func awaitQueueDrained(t *testing.T, h *harness, repo string) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for {
		got := len(h.queue.Snapshot()[repo])
		if got == 0 {
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("queue depth = %d, want 0", got)
		}
		time.Sleep(time.Millisecond)
	}
}

// transitions returns a copy of every transition recorded so far, in order.
func (s *syncSink) transitions() []transition {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]transition(nil), s.got...)
}

// failOnPhase makes the record of exactly one phase fail with err, which is how
// a test drives "the TERMINAL status could not be published" without taking
// every phase ahead of it down too.
func (s *syncSink) failOnPhase(phase Phase, err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.failPhase, s.phaseErr = phase, err
}

// failNext makes every subsequent record fail with err, which is how a test
// drives the "the transition could not be persisted" branch.
func (s *syncSink) failNext(err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.err = err
}

// publishedStatuses returns a copy of every status published so far, in order.
func (s *syncSink) publishedStatuses() []*frontendv1.MergeStatus {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]*frontendv1.MergeStatus, len(s.statuses))
	copy(out, s.statuses)
	return out
}

var (
	_ StateSink  = (*syncSink)(nil)
	_ StatusSink = (*syncSink)(nil)
)

// fakePhases is a PhaseSource: the workspaces a boot finds pinned on a phase.
type fakePhases struct {
	byPhase map[Phase][]string
	err     error
}

func (p fakePhases) WorkspacesAtPhase(phase Phase) ([]string, error) {
	if p.err != nil {
		return nil, p.err
	}
	return p.byPhase[phase], nil
}

// failingQueue is a DurableQueue whose Publish always fails.
type failingQueue struct{ err error }

func (q failingQueue) Publish(context.Context, string, Request) (Position, error) {
	return Position{}, q.err
}
func (q failingQueue) Subscribe(string) (<-chan Request, func()) {
	ch := make(chan Request)
	return ch, func() { close(ch) }
}
func (q failingQueue) Snapshot() map[string][]Request                     { return nil }
func (q failingQueue) Complete(string, Request) error                     { return nil }
func (q failingQueue) MarkTerminal(string, Request, TerminalStatus) error { return nil }
func (q failingQueue) RecordStatusWatermark(string, string, int64) error  { return nil }
func (q failingQueue) PendingTerminal(string, Request) (TerminalStatus, bool, error) {
	return TerminalStatus{}, false, nil
}
func (q failingQueue) EvictWaiting(string, string) ([]Request, error) { return nil, nil }

// harness bundles a coordinator with the fakes behind it.
type harness struct {
	coord        *QueueCoordinator
	queue        *FileQueue
	picker       *fakePicker
	lease        *fakeLease
	resolver     *fakeResolver
	testResolver *fakeTestFailureResolver
	sink         *syncSink
	sessions     *fakeSessionBringUp
	beforeRunner *fakeBeforeActionRunner
	afterRunner  *fakeAfterActionRunner
	dir          string

	// shutdown stops every fake and Closes the coordinator. Close WAITS on the
	// coordinator's WaitGroup, so it joins every goroutine the coordinator ever
	// spawned — the shim resolution goroutines included.
	//
	// A test asserting that some handoff was NOT dispatched calls quiesce
	// first: once it returns, "the resolver was never called again" is a
	// settled fact rather than a race against a goroutine that simply had not
	// got there yet.
	shutdown func()
	// once is what lets a test quiesce early and leaves the t.Cleanup that
	// always runs a no-op afterwards, rather than closing a closed channel.
	once sync.Once
}

// quiesce joins every coordinator goroutine, so whatever the fakes have
// recorded by the time it returns is everything they will ever record. It is
// the synchronization point for NEGATIVE assertions about dispatches, in place
// of sampling a channel's length and hoping.
func (h *harness) quiesce(t *testing.T) {
	t.Helper()
	h.once.Do(h.shutdown)
}

// harnessOpts varies the one dependency a given test cares about. Everything
// it leaves unset takes the harness default.
type harnessOpts struct {
	queue  *FileQueue
	dir    string
	keys   map[string]string
	keyErr error
	// postMerge replaces the auto-completing default hook for tests that are
	// about the post-merge handoff itself.
	postMerge PostMergeHook
	// phases replaces the empty default phase source for tests about the boot
	// sweep of orphaned merge_enqueuing marks.
	phases PhaseSource
	// sessions replaces the always-succeeding bring-up.
	sessions SessionBringUp
	// deaths replaces the nothing-was-deleted source.
	deaths SessionDeaths
	// beforeActions replaces the no-action-recorded source.
	beforeActions BeforeActionSource
	// afterActions replaces the default postprocessing-action source.
	afterActions AfterActionSource
	// beforeRunner replaces the always-succeeding action runner.
	beforeRunner BeforeActionRunner
	// afterRunner replaces the always-succeeding after-action runner.
	afterRunner AfterActionRunner
	// now pins the phase timestamps. Nil takes the wall clock.
	now func() int64
}

// fakeSessionBringUp is the merge.SessionBringUp double: it records the
// workspaces a run asked to bring up and can be told to refuse.
type fakeSessionBringUp struct {
	mu   sync.Mutex
	got  []string
	err  error
	done chan string
}

func (b *fakeSessionBringUp) EnsureLive(_ context.Context, ws string) error {
	b.mu.Lock()
	b.got = append(b.got, ws)
	done, err := b.done, b.err
	b.mu.Unlock()
	if done != nil {
		done <- ws
	}
	return err
}

func (b *fakeSessionBringUp) calls() []string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return append([]string(nil), b.got...)
}

// fakeSessionDeaths is the merge.SessionDeaths double: it reports one canned
// answer for every workspace.
type fakeSessionDeaths struct {
	sessionID string
	deleted   bool
	err       error
}

func (d fakeSessionDeaths) DeletedSession(string) (string, bool, error) {
	return d.sessionID, d.deleted, d.err
}

// fakeBeforeActions is the merge.BeforeActionSource double.
type fakeBeforeActions struct {
	prompt string
	err    error
}

func (s fakeBeforeActions) BeforeAction(string) (string, error) { return s.prompt, s.err }

type fakeAfterActions struct {
	prompt string
	err    error
}

func (s fakeAfterActions) AfterAction(Request) (string, error) { return s.prompt, s.err }

// fakeBeforeActionRunner is the merge.BeforeActionRunner double: it records the
// deliveries and can be told to fail the turn.
type fakeBeforeActionRunner struct {
	mu   sync.Mutex
	got  []BeforeAction
	err  error
	done chan BeforeAction
}

func (r *fakeBeforeActionRunner) Run(_ context.Context, act BeforeAction) error {
	r.mu.Lock()
	r.got = append(r.got, act)
	done, err := r.done, r.err
	r.mu.Unlock()
	if done != nil {
		done <- act
	}
	return err
}

func (r *fakeBeforeActionRunner) calls() []BeforeAction {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]BeforeAction(nil), r.got...)
}

// fakeAfterActionRunner is the merge.AfterActionRunner double: it records the
// deliveries and can be told to fail the turn.
type fakeAfterActionRunner struct {
	mu   sync.Mutex
	got  []AfterAction
	err  error
	done chan AfterAction
}

func (r *fakeAfterActionRunner) Run(_ context.Context, act AfterAction) error {
	r.mu.Lock()
	r.got = append(r.got, act)
	done, err := r.done, r.err
	r.mu.Unlock()
	if done != nil {
		done <- act
	}
	return err
}

func (r *fakeAfterActionRunner) calls() []AfterAction {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]AfterAction(nil), r.got...)
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	return newHarnessWith(t, harnessOpts{})
}

func newHarnessWith(t *testing.T, opts harnessOpts) *harness {
	t.Helper()
	q, dir := opts.queue, opts.dir
	if q == nil {
		q, dir = newTestQueue(t)
	}
	picker := newFakePicker(8)
	lease := newFakeLease(8)
	resolver := newFakeResolver(8)
	testResolver := newFakeTestFailureResolver(8)
	sink := newSyncSink(8)
	hook := opts.postMerge
	if hook == nil {
		hook = newAutoPostMergeHook(8)
	}
	phases := opts.phases
	if phases == nil {
		phases = fakePhases{}
	}
	sessions := opts.sessions
	if sessions == nil {
		sessions = &fakeSessionBringUp{}
	}
	deaths := opts.deaths
	if deaths == nil {
		deaths = fakeSessionDeaths{}
	}
	beforeActions := opts.beforeActions
	if beforeActions == nil {
		beforeActions = fakeBeforeActions{}
	}
	beforeRunner := opts.beforeRunner
	if beforeRunner == nil {
		beforeRunner = &fakeBeforeActionRunner{}
	}
	afterActions := opts.afterActions
	if afterActions == nil {
		afterActions = fakeAfterActions{prompt: testAfterActionPrompt}
	}
	afterRunner := opts.afterRunner
	if afterRunner == nil {
		afterRunner = &fakeAfterActionRunner{}
	}
	coord, err := NewCoordinator(CoordinatorConfig{
		Logf:         t.Logf,
		Sink:         sink,
		Queue:        q,
		Phases:       phases,
		Keyer:        fakeKeyer{keys: opts.keys, err: opts.keyErr},
		Picker:       picker,
		Lease:        lease,
		Resolver:     resolver,
		TestResolver: testResolver,
		PostMerge:    hook,
		Status:       sink,
		Sessions:     sessions,
		Deaths:       deaths,
		// The default harness workspace carries NO before-action, which is the
		// common case; a test that wants one overrides the source.
		BeforeActions:      beforeActions,
		AfterActions:       afterActions,
		BeforeActionRunner: beforeRunner,
		AfterActionRunner:  afterRunner,
		Now:                opts.now,
	})
	if err != nil {
		t.Fatalf("NewCoordinator: %v", err)
	}
	h := &harness{coord: coord, queue: q, picker: picker, lease: lease, resolver: resolver, testResolver: testResolver, sink: sink, dir: dir}
	h.shutdown = func() {
		close(picker.stop)
		close(resolver.stop)
		close(testResolver.stop)
		if err := coord.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	}
	t.Cleanup(func() { h.quiesce(t) })
	if b, ok := sessions.(*fakeSessionBringUp); ok {
		h.sessions = b
	}
	if r, ok := beforeRunner.(*fakeBeforeActionRunner); ok {
		h.beforeRunner = r
	}
	if r, ok := afterRunner.(*fakeAfterActionRunner); ok {
		h.afterRunner = r
	}
	return h
}

// --- construction -------------------------------------------------------

func TestNewCoordinatorRequiresEveryDependency(t *testing.T) {
	q, _ := newTestQueue(t)
	complete := func() CoordinatorConfig {
		return CoordinatorConfig{
			Logf:               func(string, ...any) {},
			Sink:               newSyncSink(1),
			Queue:              q,
			Phases:             fakePhases{},
			Keyer:              fakeKeyer{},
			Picker:             newFakePicker(1),
			Lease:              newFakeLease(1),
			Resolver:           newFakeResolver(1),
			TestResolver:       newFakeTestFailureResolver(1),
			PostMerge:          newAutoPostMergeHook(1),
			Status:             newSyncSink(1),
			Sessions:           &fakeSessionBringUp{},
			Deaths:             fakeSessionDeaths{},
			BeforeActions:      fakeBeforeActions{},
			AfterActions:       fakeAfterActions{prompt: testAfterActionPrompt},
			BeforeActionRunner: &fakeBeforeActionRunner{},
			AfterActionRunner:  &fakeAfterActionRunner{},
		}
	}
	tests := []struct {
		name    string
		mutate  func(*CoordinatorConfig)
		wantErr bool
	}{
		{name: "complete", mutate: func(*CoordinatorConfig) {}, wantErr: false},
		{name: "no logger", mutate: func(c *CoordinatorConfig) { c.Logf = nil }, wantErr: true},
		{name: "no sink", mutate: func(c *CoordinatorConfig) { c.Sink = nil }, wantErr: true},
		{name: "no queue", mutate: func(c *CoordinatorConfig) { c.Queue = nil }, wantErr: true},
		{name: "no phase source", mutate: func(c *CoordinatorConfig) { c.Phases = nil }, wantErr: true},
		{name: "no keyer", mutate: func(c *CoordinatorConfig) { c.Keyer = nil }, wantErr: true},
		{name: "no picker", mutate: func(c *CoordinatorConfig) { c.Picker = nil }, wantErr: true},
		{name: "no lease", mutate: func(c *CoordinatorConfig) { c.Lease = nil }, wantErr: true},
		{name: "no conflict resolver", mutate: func(c *CoordinatorConfig) { c.Resolver = nil }, wantErr: true},
		{name: "no test resolver", mutate: func(c *CoordinatorConfig) { c.TestResolver = nil }, wantErr: true},
		{name: "no status sink", mutate: func(c *CoordinatorConfig) { c.Status = nil }, wantErr: true},
		{name: "no session bring-up", mutate: func(c *CoordinatorConfig) { c.Sessions = nil }, wantErr: true},
		{name: "no session-deaths source", mutate: func(c *CoordinatorConfig) { c.Deaths = nil }, wantErr: true},
		{name: "no before-action source", mutate: func(c *CoordinatorConfig) { c.BeforeActions = nil }, wantErr: true},
		{name: "no after-action source", mutate: func(c *CoordinatorConfig) { c.AfterActions = nil }, wantErr: true},
		{name: "no before-action runner", mutate: func(c *CoordinatorConfig) { c.BeforeActionRunner = nil }, wantErr: true},
		{name: "no after-action runner", mutate: func(c *CoordinatorConfig) { c.AfterActionRunner = nil }, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cfg := complete()
			tc.mutate(&cfg)

			// Act.
			coord, err := NewCoordinator(cfg)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("NewCoordinator() error = nil, want error")
				}
				return
			}
			if err != nil {
				t.Fatalf("NewCoordinator() error = %v", err)
			}
			coord.Close()
		})
	}
}

// --- enqueue ------------------------------------------------------------

func TestEnqueueRejectsAnInvalidRequest(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	pos, err := h.coord.Enqueue(context.Background(), Request{Name: "a"})

	// Assert — refused before anything durable was written.
	if err == nil {
		t.Fatalf("Enqueue() error = nil, want error")
	}
	if pos != (Position{}) {
		t.Fatalf("Enqueue() position = %+v, want zero", pos)
	}
	if snap := h.queue.Snapshot(); len(snap) != 0 {
		t.Fatalf("queue = %+v, want empty", snap)
	}
}

func TestEnqueueSurfacesARepoKeyFailure(t *testing.T) {
	// Arrange — the repository identity cannot be resolved.
	h := newHarnessWith(t, harnessOpts{keyErr: sentinelError("no git here")})

	// Act.
	pos, err := h.coord.Enqueue(context.Background(), testRequest("a"))

	// Assert — nothing is queued under a guessed key.
	if err == nil {
		t.Fatalf("Enqueue() error = nil, want error")
	}
	if pos != (Position{}) {
		t.Fatalf("Enqueue() position = %+v, want zero", pos)
	}
	if snap := h.queue.Snapshot(); len(snap) != 0 {
		t.Fatalf("queue = %+v, want empty", snap)
	}
}

func TestEnqueueSurfacesAPublishFailure(t *testing.T) {
	// Arrange — the durable substrate refuses the write.
	picker := newFakePicker(1)
	coord, err := NewCoordinator(CoordinatorConfig{
		Logf: t.Logf, Sink: newSyncSink(1), Queue: failingQueue{err: sentinelError("disk full")},
		Phases: fakePhases{}, Keyer: fakeKeyer{}, Picker: picker, Lease: newFakeLease(1), Resolver: newFakeResolver(1),
		TestResolver: newFakeTestFailureResolver(1), PostMerge: newAutoPostMergeHook(1),
		Status: newSyncSink(1), Sessions: &fakeSessionBringUp{}, Deaths: fakeSessionDeaths{}, AfterActions: fakeAfterActions{}, AfterActionRunner: &fakeAfterActionRunner{},
		BeforeActions: fakeBeforeActions{}, BeforeActionRunner: &fakeBeforeActionRunner{},
	})
	if err != nil {
		t.Fatalf("NewCoordinator: %v", err)
	}
	defer coord.Close()

	// Act.
	pos, err := coord.Enqueue(context.Background(), testRequest("a"))

	// Assert.
	if err == nil {
		t.Fatalf("Enqueue() error = nil, want error")
	}
	if pos != (Position{}) {
		t.Fatalf("Enqueue() position = %+v, want zero", pos)
	}
	if len(picker.merges) != 0 {
		t.Fatalf("a merge was driven for an unpublished request")
	}
}

func TestEnqueueAtTheHeadDoesNotRecordMergeQueued(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	pos, err := h.coord.Enqueue(context.Background(), testRequest("a"))
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert — an immediately admitted merge goes straight to the driver.
	if pos.Index != 1 {
		t.Fatalf("position = %+v, want index 1", pos)
	}
	<-h.picker.merges
	// The enqueued phase is published by the RUN now, through the same sink every
	// other phase goes through, so its absence is read there.
	for _, tr := range h.sink.transitions() {
		if tr.phase == PhaseMergeQueued {
			t.Fatalf("merge_queued recorded for an immediately admitted merge: %+v", tr)
		}
	}
}

func TestEnqueueBehindTheHeadRecordsMergeQueued(t *testing.T) {
	// Arrange — the head is left mid-merge so the second entry cannot start.
	h := newHarness(t)
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges

	// Act.
	second := testRequest("b")
	pos, err := h.coord.Enqueue(context.Background(), second)
	if err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}

	// Assert — position 2, explained by a merge_queued transition.
	if pos.Index != 2 || pos.Depth != 2 {
		t.Fatalf("position = %+v, want index 2 depth 2", pos)
	}
	rec := h.sink.awaitPhase(t, PhaseMergeQueued)
	if rec.ws != second.Workspace {
		t.Fatalf("transition = %+v, want merge_queued on %q", rec, second.Workspace)
	}
}

// The enqueued phase carries the queue facts the user is waiting on, so its
// position and depth are the ones the publish actually returned.
func TestEnqueueBehindTheHeadPublishesItsQueuePosition(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("b")); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Assert — the head's own admission is published first, so the deferred
	// entry's is the second of the two.
	statuses := h.sink.publishedStatuses()
	if len(statuses) != 2 {
		t.Fatalf("published %d statuses, want 2 (one admission each)", len(statuses))
	}
	enq := statuses[1].GetEnqueued()
	if enq == nil {
		t.Fatalf("status phase = %T, want MergeStatusEnqueued", statuses[1].GetPhase())
	}
	if enq.GetPosition() != 2 || enq.GetDepth() != 2 {
		t.Fatalf("enqueued position/depth = %d/%d, want 2/2", enq.GetPosition(), enq.GetDepth())
	}
}

// A HEAD admission is published too, and it reports the head's own place. It is
// the run's FIRST status: a stream that began mid-cherry-pick would give a
// frontend nothing to correlate the run against.
func TestEnqueueAtTheHeadPublishesItsAdmission(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges

	// Assert.
	statuses := h.sink.publishedStatuses()
	if len(statuses) == 0 {
		t.Fatal("a head admission published no status at all")
	}
	enq := statuses[0].GetEnqueued()
	if enq == nil {
		t.Fatalf("the run's first status phase = %T, want MergeStatusEnqueued", statuses[0].GetPhase())
	}
	if enq.GetPosition() != 1 || enq.GetDepth() != 1 {
		t.Fatalf("enqueued position/depth = %d/%d, want 1/1", enq.GetPosition(), enq.GetDepth())
	}
}

// Every published phase carries a run id. Without one a frontend has nothing to
// correlate a run's progress on and would blend two attempts.
func TestAPublishedPhaseCarriesARunID(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("b")); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Assert.
	statuses := h.sink.publishedStatuses()
	if len(statuses) == 0 {
		t.Fatal("no status was published")
	}
	if statuses[0].GetRunId() == "" {
		t.Fatal("the published status carries no run id")
	}
}

func TestEnqueueSurfacesAMergeQueuedRecordFailure(t *testing.T) {
	// Arrange — the head is mid-merge and the queued transition cannot be
	// recorded.
	h := newHarness(t)
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges
	h.sink.failNext(errFakeSink)

	// Act.
	pos, err := h.coord.Enqueue(context.Background(), testRequest("b"))

	// Assert — the caller is told, and the durable entry is kept rather than
	// stranded (a state-write failure must not discard queued work).
	if err == nil {
		t.Fatalf("Enqueue() error = nil, want error")
	}
	if pos != (Position{}) {
		t.Fatalf("Enqueue() position = %+v, want zero", pos)
	}
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 2 {
		t.Fatalf("queue depth = %d, want 2", got)
	}
}

// --- draining -----------------------------------------------------------

func TestDrainRunsOneMergePerRepositoryAtATime(t *testing.T) {
	// Arrange — two requests on the same repository.
	h := newHarness(t)
	first, second := testRequest("a"), testRequest("b")
	if _, err := h.coord.Enqueue(context.Background(), first); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	if got := <-h.picker.merges; !sameRequest(got, first) {
		t.Fatalf("first merge = %+v, want %+v", got, first)
	}
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	// The deferred entry's admission travels the sink, not the picker: one
	// publication per event, through the same channel every other phase uses.
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Act — the second merge must not have started while the first is running.
	if len(h.picker.merges) != 0 {
		t.Fatalf("second merge started while the first was in flight")
	}
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}

	// Assert — it starts only once the first finished.
	if got := <-h.picker.merges; !sameRequest(got, second) {
		t.Fatalf("second merge = %+v, want %+v", got, second)
	}
}

func TestDrainCompletesTheDurableEntryOnATerminalOutcome(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-h.lease.releases

	// Assert — nothing is left for the next boot to replay.
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 0 {
		t.Fatalf("queue depth = %d, want 0", got)
	}
}

func TestDrainBracketsEachMergeWithTheShimLease(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act — the lease is taken before the driver runs.
	if got := <-h.lease.acquires; got != req.Workspace {
		t.Fatalf("lease acquired for %q, want %q", got, req.Workspace)
	}
	<-h.picker.merges
	if !h.lease.Held(req.Workspace) {
		t.Fatalf("lease not held during the merge")
	}
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}

	// Assert — and released after it.
	if got := <-h.lease.releases; got != req.Workspace {
		t.Fatalf("lease released for %q, want %q", got, req.Workspace)
	}
}

func TestDrainKeepsRepositoriesIndependent(t *testing.T) {
	// Arrange — two repositories, the first left mid-merge.
	h := newHarnessWith(t, harnessOpts{keys: map[string]string{"/other-target": "/repos/beta/.git"}})
	other := testRequest("b")
	other.TargetDir = "/other-target"
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), other); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}

	// Assert — a second repository's merge starts while the first is blocked.
	if got := <-h.picker.merges; !sameRequest(got, other) {
		t.Fatalf("second repo merge = %+v, want %+v", got, other)
	}
}

func TestDrainReconstructsADurableQueueAtBoot(t *testing.T) {
	// Arrange — an entry published by a previous daemon, then a new
	// coordinator over the same directory.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	h := newHarnessWith(t, harnessOpts{queue: next, dir: dir})

	// Act.
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert — the surviving entry is driven.
	if got := <-h.picker.merges; !sameRequest(got, req) {
		t.Fatalf("resumed merge = %+v, want %+v", got, req)
	}
}

// THE BOUNCE INTERRUPTS A RUN, IT DOES NOT START A NEW ONE. The replayed entry
// publishes under the id its admission published under, so the enqueued status a
// user watched before the bounce and everything after it are one run.
func TestDrainResumesTheRunIDTheEntryWasAdmittedUnder(t *testing.T) {
	// Arrange — an entry admitted (and named) by a previous daemon.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	run, err := NewRunStatus(&recordingSink{}, t.Logf, req.Workspace, testClock())
	if err != nil {
		t.Fatalf("NewRunStatus: %v", err)
	}
	req.Run = run
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	h := newHarnessWith(t, harnessOpts{queue: next, dir: dir})

	// Act.
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert.
	driven := <-h.picker.merges
	if got := driven.Run.RunID(); got != run.RunID() {
		t.Fatalf("resumed run id = %q, want the admitted %q", got, run.RunID())
	}
}

// An entry written before the durable record carried a run id still drains: a
// merge stranded because its record predates a field is work nobody gets back.
func TestDrainMintsARunIDForAnUnnamedDurableEntry(t *testing.T) {
	// Arrange — a durable entry with no run id at all.
	q, dir := newTestQueue(t)
	if _, err := q.Publish(context.Background(), testRepoKey, testRequest("a")); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	h := newHarnessWith(t, harnessOpts{queue: next, dir: dir})

	// Act.
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert.
	if got := (<-h.picker.merges).Run.RunID(); got == "" {
		t.Fatal("the replayed entry was driven under an empty run id")
	}
}

// --- failure paths ------------------------------------------------------

func TestLeaseAcquireFailureRecordsMergeFailed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.lease.err = sentinelError("shim gone")
	req := testRequest("a")

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert — a terminal state is recorded rather than a silent stall, and no
	// cherry-pick is attempted without the lease.
	got := h.sink.awaitPhase(t, PhaseMergeFailed)
	if got.ws != req.Workspace {
		t.Fatalf("transition = %+v, want merge_failed on %q", got, req.Workspace)
	}
	if len(h.picker.merges) != 0 {
		t.Fatalf("a merge ran without the lease")
	}
}

func TestLeaseAcquireFailureStillCompletesTheDurableEntry(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.lease.err = sentinelError("shim gone")

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.sink.awaitPhase(t, PhaseMergeFailed)

	// Assert — a merge that can never run must not block the queue forever.
	awaitQueueDrained(t, h, testRepoKey)
}

func TestDriverFailureRecordsMergeFailedAndReleasesTheLease(t *testing.T) {
	// Arrange — merge.Driver rejects a precondition, which emits no transition
	// of its own, so the workspace would otherwise sit at merge_queued forever.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{err: sentinelError("dirty source worktree")}

	// Assert.
	h.sink.awaitPhase(t, PhaseMergeFailed)
	if released := <-h.lease.releases; released != req.Workspace {
		t.Fatalf("lease released for %q, want %q", released, req.Workspace)
	}
}

// --- conflicts ----------------------------------------------------------

func TestConflictHoldsTheHeadAndTheLeaseUntilResumed(t *testing.T) {
	// Arrange — the head conflicts, and a second merge is queued behind it.
	h := newHarness(t)
	first, second := testRequest("a"), testRequest("b")
	if _, err := h.coord.Enqueue(context.Background(), first); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	// The deferred entry's admission travels the sink, not the picker: one
	// publication per event, through the same channel every other phase uses.
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Act — resolve the conflict.
	go func() {
		if err := h.coord.Resume(context.Background(), first); err != nil {
			t.Errorf("Resume: %v", err)
		}
	}()
	if got := <-h.picker.resumes; !sameRequest(got, first) {
		t.Fatalf("resume = %+v, want %+v", got, first)
	}

	// Assert — the lease was never released while parked, and the queue
	// advances only after the resume lands terminally.
	if !h.lease.Held(first.Workspace) {
		t.Fatalf("lease released while the conflict was parked")
	}
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-h.lease.releases
	if got := <-h.picker.merges; !sameRequest(got, second) {
		t.Fatalf("next merge = %+v, want %+v", got, second)
	}
}

// --- shim-driven conflict resolution ------------------------------------

func TestConflictHandsTheResolutionToTheWorkspacesOwnShim(t *testing.T) {
	// Arrange — a merge that conflicts on a named commit.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	got := <-h.resolver.calls

	// Assert — the resolver is handed the facts the resolving turn needs, and is
	// pointed at the REBASE WORKTREE: the conflict is parked there and the target
	// has nothing of this merge in it to resolve.
	if got.Workspace != req.Workspace || got.SourceBranch != req.SourceBranch || got.TargetDir != testRebaseWorkDir {
		t.Fatalf("resolution = %+v, want workspace/branch of %+v and the rebase worktree %s", got, req, testRebaseWorkDir)
	}
	if got.ConflictCommit != "abc1234" {
		t.Fatalf("resolution commit = %q, want abc1234", got.ConflictCommit)
	}
	if got.RequestID == "" {
		t.Fatalf("resolution carries no request id")
	}
}

func TestConflictDrivesTheShimExactlyOncePerConflictCommit(t *testing.T) {
	// Arrange — a conflict whose shim-driven resume comes back conflicted on
	// THE SAME COMMIT, which is the agent failing at what it was given.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Act — the resolution turn ends, the resume is still on abc1234.
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — a HUMAN resume is served, and the same conflict is never
	// re-prompted: a second identical attempt is a spin, not a strategy.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}
	h.quiesce(t)
	if got := len(h.resolver.calls); got != 0 {
		t.Fatalf("further resolver calls = %d, want 0 (one attempt per conflict commit)", got)
	}
}

func TestResumeUncoveringANewConflictHandsThatConflictToTheShim(t *testing.T) {
	// Arrange — the run's FIRST conflict, handed to the shim.
	//
	// This is the production miss: conflict A was resolved, the pick replayed
	// on and stopped at conflict B, and B was never handed to anybody. The run
	// sat conflict-parked and silent, holding the lease and the queue head,
	// until a daemon bounce replayed it.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "f9ddc28", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	first := <-h.resolver.calls

	// Act — the agent resolves A, and the resumed pick stops on a NEW commit B.
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "b269cb2", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — B goes to the shim too, under its own request id, pointed at the
	// rebase worktree where B is parked.
	second := <-h.resolver.calls
	if second.ConflictCommit != "b269cb2" {
		t.Fatalf("second resolution commit = %q, want b269cb2", second.ConflictCommit)
	}
	if second.RequestID == first.RequestID {
		t.Fatalf("second resolution reused request id %q, want a distinct one", second.RequestID)
	}
	if second.Workspace != req.Workspace || second.SourceBranch != req.SourceBranch || second.TargetDir != testRebaseWorkDir {
		t.Fatalf("second resolution = %+v, want workspace/branch of %+v and the rebase worktree %s", second, req, testRebaseWorkDir)
	}
}

func TestResolvedSecondConflictResumesTheCherryPickToTerminal(t *testing.T) {
	// Arrange — conflict A resolved, conflict B uncovered and handed on.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "f9ddc28", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "b269cb2", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Act — the agent resolves B as well, and the second resume lands.
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}

	// Assert — the merge retires on its own: the lease goes back and the queue
	// head is released, with no human ever touching it.
	<-h.lease.releases
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 0 {
		t.Fatalf("queue depth = %d, want 0 after the shim resolved both conflicts", got)
	}
}

func TestConflictReturningToAnAlreadyHandedCommitIsNotReHanded(t *testing.T) {
	// Arrange — A is handed and resolved, B is uncovered and handed, and B's
	// resume lands back on A. A has already had its one attempt.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "def5678", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Act — the second turn ends and the pick falls back onto abc1234.
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — the park stands for the human, and abc1234 is not prompted twice.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}
	h.quiesce(t)
	if got := len(h.resolver.calls); got != 0 {
		t.Fatalf("further resolver calls = %d, want 0 for a commit already handed once", got)
	}
}

func TestHumanResumeUncoveringANewConflictHandsThatConflictToTheShim(t *testing.T) {
	// Arrange — a conflict the shim could not be driven for, resolved by a
	// human's conflict_resolved_continue instead.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls
	h.resolver.results <- sentinelError("no live session to drive")

	// Act — the human resumes, and the pick stops on a commit nobody has been
	// asked about.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "def5678", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}

	// Assert — the new conflict reaches the shim through the same funnel: the
	// human is not left owning every conflict after the first.
	got := <-h.resolver.calls
	if got.ConflictCommit != "def5678" {
		t.Fatalf("resolution commit = %q, want def5678", got.ConflictCommit)
	}
}

func TestResumeConflictAdoptsTheWorktreeTheResumeReports(t *testing.T) {
	// Arrange — a conflict handed to the shim from the first rebase worktree.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Act — the resume reports the next conflict in a DIFFERENT worktree.
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "def5678", WorkDir: "/tmp/rebase-a-second", BaseHead: baseHeadOfFailure}}

	// Assert — the agent is sent to the tree the conflict is actually parked
	// in, not the one the park was entered with.
	got := <-h.resolver.calls
	if got.TargetDir != "/tmp/rebase-a-second" {
		t.Fatalf("resolution target dir = %q, want /tmp/rebase-a-second", got.TargetDir)
	}
}

func TestConflictWithNoCommitIsNeverHandedToTheShim(t *testing.T) {
	// Arrange — a conflict Result that names no commit, which no valid
	// merge.Driver produces: there is nothing to tell an agent to resolve.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — no prompt is sent, and the park still serves the human.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}
	if got := len(h.resolver.calls); got != 0 {
		t.Fatalf("resolver calls = %d, want 0 for a conflict with no commit", got)
	}
}

func TestResumeWaitsForTheResolutionTurnToComplete(t *testing.T) {
	// Arrange — a conflict handed to the shim.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Assert — nothing is resumed while the resolution turn is still running.
	if got := len(h.picker.resumes); got != 0 {
		t.Fatalf("resumes before the resolution turn ended = %d, want 0", got)
	}

	// Act — the turn completes.
	h.resolver.results <- nil

	// Assert — only now is the cherry-pick continued.
	if got := <-h.picker.resumes; !sameRequest(got, req) {
		t.Fatalf("resume = %+v, want %+v", got, req)
	}
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-h.lease.releases
}

func TestShimResolutionThatIsStillConflictedLeavesTheParkStanding(t *testing.T) {
	// Arrange — the shim resolves, and the resume conflicts again on the SAME
	// commit, so nothing is re-prompted and the human path is all that is left.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Act.
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — the lease and the queue head are still held, and a human resume
	// still drives the merge to its terminal outcome.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}
	<-h.lease.releases
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 0 {
		t.Fatalf("queue depth = %d, want 0 after the human resume landed", got)
	}
}

func TestShimResolutionFailureLeavesTheParkStanding(t *testing.T) {
	// Arrange — a conflict whose resolver refuses (no live session, no lease,
	// a submit the controller would not take).
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Act.
	h.resolver.results <- sentinelError("no live session to drive")

	// Assert — nothing was resumed and nothing was marked merged; the park is
	// still there for the human, who drives it to its terminal outcome.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	if got := <-h.picker.resumes; !sameRequest(got, req) {
		t.Fatalf("resume = %+v, want the human's %+v", got, req)
	}
	if !h.lease.Held(req.Workspace) {
		t.Fatalf("lease released after a failed shim resolution")
	}
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}
	<-h.lease.releases
}

func TestAbandonDuringAShimResolutionIsNotLost(t *testing.T) {
	// Arrange — a conflict with a shim resolution turn in flight.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls

	// Act — the user closes the workspace while the agent is still resolving.
	abandoned := make(chan error, 1)
	go func() {
		ok, err := h.coord.Abandon(context.Background(), req.Workspace)
		if !ok && err == nil {
			err = sentinelError("abandon reported nothing to give up")
		}
		abandoned <- err
	}()

	// Assert — the abandon is served, the lease goes back, and the shim's own
	// resume (arriving after) finds nothing left to continue rather than
	// reviving the merge.
	if err := <-abandoned; err != nil {
		t.Fatalf("Abandon() error = %v, want nil", err)
	}
	<-h.lease.releases
	h.resolver.results <- nil
	if got := len(h.picker.resumes); got != 0 {
		t.Fatalf("resumes after the abandon = %d, want 0", got)
	}
}

func TestResumeWithoutAParkedConflictIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	err := h.coord.Resume(context.Background(), testRequest("a"))

	// Assert.
	if err == nil {
		t.Fatalf("Resume() error = nil, want error")
	}
}

func TestResumeForAnotherWorkspaceIsRefused(t *testing.T) {
	// Arrange — the repository is parked on workspace a.
	h := newHarness(t)
	first := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), first); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges // the resume rendezvous is published before the merge runs

	// Act — workspace b asks to continue a conflict it does not own.
	err := h.coord.Resume(context.Background(), testRequest("b"))

	// Assert — refused, and no resume is driven for the wrong workspace.
	if err == nil {
		t.Fatalf("Resume() error = nil, want error")
	}
	if len(h.picker.resumes) != 0 {
		t.Fatalf("a resume was driven for the wrong workspace")
	}
}

func TestResumeArrivingBeforeTheConflictIsServedNotRefused(t *testing.T) {
	// Arrange — the merge is still cherry-picking, so nothing is parked yet.
	// merge.Driver pushes merge_conflict from inside Merge, so this is the
	// order a real frontend can produce.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act — the resume lands first, then the merge reports its conflict.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — the resume was served rather than refused for a conflict that
	// was about to exist.
	if got := <-h.picker.resumes; !sameRequest(got, req) {
		t.Fatalf("resume = %+v, want %+v", got, req)
	}
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}
}

func TestResumeAfterTheMergeEndedIsRefused(t *testing.T) {
	// Arrange — a merge that finished cleanly, with nothing left to continue.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-h.lease.releases

	// Act.
	err := h.coord.Resume(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("Resume() error = nil, want error")
	}
	if len(h.picker.resumes) != 0 {
		t.Fatalf("a resume was driven for a finished merge")
	}
}

func TestResumeDriverFailureKeepsTheConflictParked(t *testing.T) {
	// Arrange — a parked conflict whose resume fails.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Act.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{err: sentinelError("still conflicted")}

	// Assert — the caller is told, the lease stays held, and the entry stays.
	if err := <-done; err == nil {
		t.Fatalf("Resume() error = nil, want error")
	}
	if !h.lease.Held(req.Workspace) {
		t.Fatalf("lease released after a failed resume")
	}
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 1 {
		t.Fatalf("queue depth = %d, want 1", got)
	}
}

func TestResumeThatConflictsAgainStaysParked(t *testing.T) {
	// Arrange — a parked conflict.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Act — the next commit in the range conflicts too.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — reported as accepted, still parked and still holding the lease.
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}
	if !h.lease.Held(req.Workspace) {
		t.Fatalf("lease released while still conflicted")
	}
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 1 {
		t.Fatalf("queue depth = %d, want 1", got)
	}
}

// --- shutdown -----------------------------------------------------------

func TestAbandonReleasesAParkedConflictAndAdvancesTheQueue(t *testing.T) {
	// Arrange — a conflict parked at the head, another merge queued behind it.
	h := newHarness(t)
	first, second := testRequest("a"), testRequest("b")
	if _, err := h.coord.Enqueue(context.Background(), first); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	// The deferred entry's admission travels the sink, not the picker: one
	// publication per event, through the same channel every other phase uses.
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Act.
	abandoned, err := h.coord.Abandon(context.Background(), first.Workspace)

	// Assert — the lease went back and the queue advanced past the head.
	if err != nil || !abandoned {
		t.Fatalf("Abandon() = (%v, %v), want (true, nil)", abandoned, err)
	}
	if got := <-h.lease.releases; got != first.Workspace {
		t.Fatalf("lease released for %q, want %q", got, first.Workspace)
	}
	if got := <-h.picker.merges; !sameRequest(got, second) {
		t.Fatalf("next merge = %+v, want %+v", got, second)
	}
}

func TestAbandonWithNoMergeInFlightIsANoOp(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	abandoned, err := h.coord.Abandon(context.Background(), "/nowhere/ws")

	// Assert — closing a workspace with no merge is the ordinary case, not an
	// error and not an abandonment.
	if err != nil || abandoned {
		t.Fatalf("Abandon() = (%v, %v), want (false, nil)", abandoned, err)
	}
}

func TestAbandonArrivingBeforeTheConflictWaitsForIt(t *testing.T) {
	// Arrange — the cherry-pick is still running when the abandon arrives.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act — abandon first, then let the pick land on its conflict.
	abandoned := make(chan bool, 1)
	go func() {
		got, err := h.coord.Abandon(context.Background(), req.Workspace)
		if err != nil {
			t.Errorf("Abandon: %v", err)
		}
		abandoned <- got
	}()
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — the abandon was served by the park it waited for.
	if !<-abandoned {
		t.Fatalf("Abandon() = false, want true")
	}
	if got := <-h.lease.releases; got != req.Workspace {
		t.Fatalf("lease released for %q, want %q", got, req.Workspace)
	}
}

func TestCloseRetainsAParkedConflictForTheNextBoot(t *testing.T) {
	// Arrange — a conflict parked at the head.
	q, dir := newTestQueue(t)
	picker := newFakePicker(4)
	lease := newFakeLease(4)
	resolver := newFakeResolver(4)
	defer close(resolver.stop)
	coord, err := NewCoordinator(CoordinatorConfig{
		Logf: t.Logf, Sink: newSyncSink(4), Queue: q,
		Phases: fakePhases{}, Keyer: fakeKeyer{}, Picker: picker, Lease: lease, Resolver: resolver,
		TestResolver: newFakeTestFailureResolver(4), PostMerge: newAutoPostMergeHook(4),
		Status: newSyncSink(4), Sessions: &fakeSessionBringUp{}, Deaths: fakeSessionDeaths{}, AfterActions: fakeAfterActions{}, AfterActionRunner: &fakeAfterActionRunner{},
		BeforeActions: fakeBeforeActions{}, BeforeActionRunner: &fakeBeforeActionRunner{},
	})
	if err != nil {
		t.Fatalf("NewCoordinator: %v", err)
	}
	req := testRequest("a")
	if _, err := coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-picker.merges
	picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Act.
	if err := coord.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	close(picker.stop)

	// Assert — the lease went back and the durable entry survived.
	if got := <-lease.releases; got != req.Workspace {
		t.Fatalf("lease released for %q, want %q", got, req.Workspace)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	if got := len(next.Snapshot()[testRepoKey]); got != 1 {
		t.Fatalf("surviving depth = %d, want 1", got)
	}
}

func TestBootReplayOfAParkedConflictHandsItToTheShim(t *testing.T) {
	// Arrange — an entry that survived a bounce with its conflict standing.
	// This is the third way into a parked conflict, and it goes through the
	// same funnel as the other two: a bounce is not what unwedges a run.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	h := newHarnessWith(t, harnessOpts{queue: next, dir: dir})

	// Act — the boot drain replays the entry and the pick conflicts again.
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert.
	got := <-h.resolver.calls
	if got.ConflictCommit != "abc1234" {
		t.Fatalf("resolution commit = %q, want abc1234", got.ConflictCommit)
	}
}

// --- the per-commit test gate -------------------------------------------

// testFailure is the Result merge.Driver returns when a commit that landed ON
// THE REBASED LINE broke the suite. It carries the rebase worktree the failure
// is parked in, which is the only thing this merge has modified anywhere.
func testFailure(commit, tail, workDir string) Result {
	return Result{
		Outcome:       OutcomeTestFailed,
		FailingCommit: commit,
		// The SUBJECT is what every sentence about this failure names; the sha
		// beside it is correlation only. The fixture carries both so a test can
		// assert which of the two reached the user's copy.
		FailingSubject:  "subject of " + commit,
		TestFailureTail: tail,
		WorkDir:         workDir,
		BaseHead:        baseHeadOfFailure,
	}
}

// escalatedTestFailure is what merge.Driver returns when the remediation turn
// wrote the escalation record instead of a fix: the same failed gate, plus the
// agent's own statement that no local fix is correct.
func escalatedTestFailure(commit, escalation, workDir string) Result {
	res := testFailure(commit, "FAIL: suite", workDir)
	res.TestFailureEscalation = escalation
	return res
}

// baseHeadOfFailure is the target head every testFailure claims its rebase based
// itself on. The driver's landing step guards on it; nothing on the coordinator
// side ever resets anything to it.
const baseHeadOfFailure = "base0"

// testRebaseWorkDir is the rebase worktree every fixture Result claims to have
// been working in. It is the tree a conflict parks in, the tree a fix turn is
// pointed at, and the tree the coordinator discards at the terminal.
const testRebaseWorkDir = "/tmp/rebase-a"

func TestTestFailureIsHandedToTheWorkspacesOwnShim(t *testing.T) {
	// Arrange — a merge whose first landed commit breaks the suite.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	got := <-h.testResolver.calls

	// Assert — the fix turn is handed everything it needs to act, and is pointed
	// at the REBASE WORKTREE: the failing commit exists nowhere else, and the
	// target carries nothing of this merge to fix.
	if got.Workspace != req.Workspace || got.SourceBranch != req.SourceBranch || got.TargetDir != testRebaseWorkDir {
		t.Fatalf("resolution = %+v, want workspace/branch of %+v and the rebase worktree /tmp/rebase-a", got, req)
	}
	if got.FailingCommit != "abc1234" || got.FailureTail != "FAIL: suite" {
		t.Fatalf("resolution = %+v, want the failing commit and its tail", got)
	}
	if got.RequestID == "" {
		t.Fatalf("resolution carries no request id")
	}
}

func TestTestFailureFixedByTheShimContinuesTheMerge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	<-h.testResolver.calls

	// Act — the fix turn ends, and the continued replay finishes the range.
	h.testResolver.results <- nil
	if got := <-h.picker.continues; got != "abc1234" {
		t.Fatalf("ContinueAfterTestFix commit = %q, want abc1234", got)
	}
	h.picker.continueResults <- pickResult{res: Result{Outcome: OutcomeMerged, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — the merge landed, the queue advanced, and the rebase worktree
	// was discarded on the way out.
	<-h.lease.releases
	if got := <-h.picker.cleanups; got != testRebaseWorkDir {
		t.Fatalf("cleanup worktree = %q, want /tmp/rebase-a", got)
	}
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 0 {
		t.Fatalf("queue depth = %d, want 0", got)
	}
}

// AMENDED FROM TestTestFailureThatSurvivesTheFixRollsTheTargetBack, twice over.
// The rollback it asserted no longer exists: nothing of a merge reaches the
// target until the whole rebase has passed. And a repeat failure no longer ENDS
// the merge — the one-attempt rule is abolished — so the terminal it needs is now
// the agent's escalation. The guarantee it pins is unchanged in strength: the
// target was never written to at all, and the only tree this merge touched is
// discarded.
func TestAnEscalatedTestFailureLeavesTheTargetUntouched(t *testing.T) {
	// Arrange — the fix turn runs and comes back escalating.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	<-h.testResolver.calls

	// Act.
	h.testResolver.results <- nil
	<-h.picker.continues
	h.picker.continueResults <- pickResult{res: escalatedTestFailure("abc1234", "the suite assumes a single writer", testRebaseWorkDir)}

	// Assert — the rebase worktree is discarded and the cause says the target
	// was never modified.
	if got := <-h.picker.cleanups; got != testRebaseWorkDir {
		t.Fatalf("cleanup worktree = %q, want /tmp/rebase-a", got)
	}
	got := h.sink.awaitPhase(t, PhaseMergeFailed)
	if !strings.Contains(got.cause, "NEVER MODIFIED") {
		t.Fatalf("merge_failed cause = %q, want it to say the target was never modified", got.cause)
	}
}

// AMENDED: the terminal is reached by escalation rather than by a spent attempt
// budget, and the cause is asserted to name the failing commit's SUBJECT. The
// contract reason for both is the same ruling: the loop is bounded by the agent's
// judgement, and no sentence a user reads names a sha.
func TestAnEscalatedTestFailureRecordsMergeFailedWithTheTail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	<-h.testResolver.calls

	// Act.
	h.testResolver.results <- nil
	<-h.picker.continues
	h.picker.continueResults <- pickResult{res: escalatedTestFailure("abc1234", "the fix needs a new scheduler", testRebaseWorkDir)}

	// Assert — the failure the user sees names the commit by subject.
	got := h.sink.awaitPhase(t, PhaseMergeFailed)
	if !strings.Contains(got.cause, "subject of abc1234") {
		t.Fatalf("merge_failed cause = %q, want the failing commit's subject", got.cause)
	}
}

// THE LOOP TURNS UNTIL THE GATE PASSES. Two failed gates earn two remediation
// dispatches — no budget refuses the second — and the merge lands when the third
// gate is green.
func TestTheRemediationLoopTurnsUntilTheGatePasses(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act — fail, fix, fail again, fix again, pass.
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: one", testRebaseWorkDir)}
	<-h.testResolver.calls
	h.testResolver.results <- nil
	<-h.picker.continues
	h.picker.continueResults <- pickResult{res: testFailure("abc1234", "FAIL: two", testRebaseWorkDir)}
	<-h.testResolver.calls
	h.testResolver.results <- nil
	<-h.picker.continues
	h.picker.continueResults <- pickResult{res: Result{Outcome: OutcomeMerged, WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — it landed, so the second failure was remediated rather than
	// refused as a spent attempt.
	<-h.lease.releases
	if got := h.sink.awaitPhase(t, PhaseMerged); got.ws != req.Workspace {
		t.Fatalf("merged recorded for %q, want %q", got.ws, req.Workspace)
	}
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 0 {
		t.Fatalf("queue depth = %d, want 0", got)
	}
}

// The escalation is the loop's ONLY non-passing exit, and the agent's own words
// are what the user reads.
func TestAnEscalatedTestFailureCarriesTheAgentsExplanationAsTheCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	<-h.testResolver.calls

	// Act.
	h.testResolver.results <- nil
	<-h.picker.continues
	h.picker.continueResults <- pickResult{
		res: escalatedTestFailure("abc1234", "the store and the daemon disagree about ownership; fixing it means moving the lease", testRebaseWorkDir)}

	// Assert.
	got := h.sink.awaitPhase(t, PhaseMergeFailed)
	if !strings.Contains(got.cause, "the store and the daemon disagree about ownership") {
		t.Fatalf("merge_failed cause = %q, want the agent's own explanation", got.cause)
	}
}

// An escalation stops the loop DEAD: the escalating turn is not answered with
// another prompt.
func TestAnEscalatedTestFailureIsNotRePrompted(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	<-h.testResolver.calls

	// Act.
	h.testResolver.results <- nil
	<-h.picker.continues
	h.picker.continueResults <- pickResult{res: escalatedTestFailure("abc1234", "needs a redesign", testRebaseWorkDir)}
	<-h.picker.cleanups
	<-h.lease.releases

	// Assert.
	if got := len(h.testResolver.calls); got != 0 {
		t.Fatalf("test-fix resolver calls still pending = %d, want 0: an escalation is the agent's own last word", got)
	}
}

// NO SHA REACHES THE USER'S COPY. The failing sha is correlation only, and a
// merge_failed cause that pasted it in would be naming a commit in the one
// vocabulary the person reading it cannot use.
func TestAMergeFailedCauseNamesTheSubjectAndNotTheSha(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	<-h.testResolver.calls

	// Act.
	h.testResolver.results <- nil
	<-h.picker.continues
	h.picker.continueResults <- pickResult{res: escalatedTestFailure("abc1234", "needs a redesign", testRebaseWorkDir)}

	// Assert — the subject names the commit; the bare sha appears nowhere. The
	// fixture's subject deliberately embeds the sha, so the check is for the sha
	// standing on its own.
	got := h.sink.awaitPhase(t, PhaseMergeFailed)
	if strings.Contains(strings.ReplaceAll(got.cause, "subject of abc1234", ""), "abc1234") {
		t.Fatalf("merge_failed cause = %q, want no bare sha in the copy a user reads", got.cause)
	}
}

// AMENDED FROM TestAFailedTestResolverRollsBackWithoutContinuing, for the same
// reason as the rollback test above: the outcome it pinned (a refused resolution
// ends the merge without continuing the replay) is asserted unchanged, and the
// rollback half is replaced by the discard of the only tree that was written to.
func TestAFailedTestResolverEndsTheMergeWithoutContinuing(t *testing.T) {
	// Arrange — no live session takes the fix prompt.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	<-h.testResolver.calls

	// Act.
	h.testResolver.results <- sentinelError("no live session to drive")

	// Assert — the rebase worktree went, the run failed, and the replay was
	// never continued.
	if got := <-h.picker.cleanups; got != testRebaseWorkDir {
		t.Fatalf("cleanup worktree = %q, want /tmp/rebase-a", got)
	}
	<-h.lease.releases
	if got := len(h.picker.continues); got != 0 {
		t.Fatalf("ContinueAfterTestFix calls = %d, want 0 after a refused resolution", got)
	}
}

// AMENDED FROM TestAFailedRollbackStillFailsTheMergeAndSaysSo. That test pinned
// "a reset that itself failed must not become a merged (or a stalled) outcome".
// There is no reset; the equivalent hazard is a cleanup that fails, and the
// assertion is equal in strength: the run still reaches its terminal
// merge_failed, and the leftover directory changes nothing about the outcome.
func TestAFailedRebaseWorktreeCleanupStillFailsTheMerge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.picker.cleanupErr = sentinelError("worktree remove refused")
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", testRebaseWorkDir)}
	<-h.testResolver.calls

	// Act.
	h.testResolver.results <- sentinelError("no live session")
	<-h.picker.cleanups

	// Assert — the terminal word still lands, carrying the suite failure.
	got := h.sink.awaitPhase(t, PhaseMergeFailed)
	if !strings.Contains(got.cause, "abc1234") {
		t.Fatalf("merge_failed cause = %q, want the failing commit", got.cause)
	}
}

func TestTheFailedCauseNamesTheArchivedSuiteOutput(t *testing.T) {
	// Arrange — a test failure whose complete output was archived. The cause
	// carries a twice-clamped tail, which for a runner that keeps going after a
	// failure regularly holds no trace of it, so the path travels with it.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	failure := testFailure("abc1234", "a coverage table, not the failure", testRebaseWorkDir)
	failure.TestFailureOutputPath = "/tmp/agent-repl-merge-suite-42.log"
	h.picker.results <- pickResult{res: failure}
	<-h.testResolver.calls

	// Act.
	h.testResolver.results <- sentinelError("no live session")

	// Assert.
	got := h.sink.awaitPhase(t, PhaseMergeFailed)
	if !strings.Contains(got.cause, "/tmp/agent-repl-merge-suite-42.log") {
		t.Fatalf("merge_failed cause = %q, want it to name the archived suite output", got.cause)
	}
}

// AMENDED FROM TestATestFailureWithNoPreMergeHeadIsFailedAndNamed. That test
// covered a Result carrying no rollback point; there is no rollback point any
// more. Its edge case survives as its structural twin — a Result naming no
// rebase worktree — and the assertion is equal in strength: nothing is
// attempted against a directory that was not named, and the run still fails.
func TestATestFailureWithNoRebaseWorktreeCleansUpNothing(t *testing.T) {
	// Arrange — a Result naming no tree, which no valid merge.Driver produces.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", "")}

	// Assert — the run fails without prompting an agent to fix a tree nobody
	// named, and nothing is deleted on the strength of an empty path.
	h.sink.awaitPhase(t, PhaseMergeFailed)
	<-h.lease.releases
	if len(h.picker.cleanups) != 0 {
		t.Fatalf("a cleanup was attempted with no rebase worktree to clean")
	}
	if len(h.testResolver.calls) != 0 {
		t.Fatalf("a fix turn was handed a resolution with no worktree to fix in")
	}
}

func TestAConflictResolvedIntoATestFailureIsStillGated(t *testing.T) {
	// The two parking outcomes feed each other: a resume that finishes the pick
	// can leave the suite broken, and that failure enters the same remediation
	// loop every other gate failure does.
	// Arrange — a conflict, resolved by the shim.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	<-h.resolver.calls
	h.resolver.results <- nil
	<-h.picker.resumes

	// Act — the resume lands the commit and breaks the suite.
	h.picker.resumeResults <- pickResult{res: testFailure("abc1234", "FAIL: after resolve", testRebaseWorkDir)}

	// Assert — the test-fix path takes over, and the ONE rebase worktree both
	// stages worked in is the one discarded at the end.
	got := <-h.testResolver.calls
	if got.FailingCommit != "abc1234" {
		t.Fatalf("resolution commit = %q, want abc1234", got.FailingCommit)
	}
	h.testResolver.results <- sentinelError("cannot fix")
	if work := <-h.picker.cleanups; work != testRebaseWorkDir {
		t.Fatalf("cleanup worktree = %q, want the tree the conflict parked in", work)
	}
}

func TestATestFixThatConflictsLaterParksAgain(t *testing.T) {
	// Arrange — a fixed suite whose continued replay conflicts on a later
	// commit.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", "head0")}
	<-h.testResolver.calls
	h.testResolver.results <- nil
	<-h.picker.continues

	// Act.
	h.picker.continueResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "def5678", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}

	// Assert — the conflict path takes over, shim first as always.
	got := <-h.resolver.calls
	if got.ConflictCommit != "def5678" {
		t.Fatalf("conflict resolution commit = %q, want def5678", got.ConflictCommit)
	}
}

func TestAbandonDuringATestFixIsNotLost(t *testing.T) {
	// Arrange — a fix turn in flight.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: testFailure("abc1234", "FAIL: suite", "head0")}
	<-h.testResolver.calls

	// Act — the user closes the workspace while the agent is still fixing.
	abandoned, err := h.coord.Abandon(context.Background(), req.Workspace)

	// Assert — served without waiting for the turn, and the lease went back.
	if err != nil || !abandoned {
		t.Fatalf("Abandon() = (%v, %v), want (true, nil)", abandoned, err)
	}
	if got := <-h.lease.releases; got != req.Workspace {
		t.Fatalf("lease released for %q, want %q", got, req.Workspace)
	}
}

// --- a terminal status that did not publish -----------------------------

// THE MERGE STANDS AND SO DOES ITS ENTRY. A merged status the sink refused
// leaves the commits on the target and the durable entry in place, marked with
// the word that did not publish — which is the only thing a later boot could
// re-publish from.
func TestAMergedStatusThatDidNotPublishKeepsItsMarkedQueueEntry(t *testing.T) {
	// Arrange — a merge whose TERMINAL publication is the one that fails.
	h := newHarness(t)
	h.sink.failOnPhase(PhaseMerged, sentinelError("state store down"))
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}

	// Assert — the lease going back is the retirement's last step, so the mark
	// is durable by the time it arrives.
	if got := <-h.lease.releases; got != req.Workspace {
		t.Fatalf("lease released for %q, want %q", got, req.Workspace)
	}
	outstanding := h.queue.Snapshot()[testRepoKey]
	if len(outstanding) != 1 {
		t.Fatalf("outstanding entries = %+v, want the merge's entry KEPT", outstanding)
	}
	term, pending, err := h.queue.PendingTerminal(testRepoKey, outstanding[0])
	if err != nil {
		t.Fatalf("PendingTerminal: %v", err)
	}
	want := TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"}
	if !pending || term != want {
		t.Fatalf("PendingTerminal() = %+v, %v, want %+v, true", term, pending, want)
	}
}

// The failed terminal is durable on exactly the same terms as the merged one:
// a run that died and could not say so is as silent as a merge that landed and
// could not say so.
func TestAFailedStatusThatDidNotPublishKeepsItsMarkedQueueEntry(t *testing.T) {
	// Arrange — a driver rejection, with the terminal publication failing.
	h := newHarness(t)
	h.sink.failOnPhase(PhaseMergeFailed, sentinelError("state store down"))
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{err: sentinelError("source worktree is dirty")}

	// Assert.
	if got := <-h.lease.releases; got != req.Workspace {
		t.Fatalf("lease released for %q, want %q", got, req.Workspace)
	}
	outstanding := h.queue.Snapshot()[testRepoKey]
	if len(outstanding) != 1 {
		t.Fatalf("outstanding entries = %+v, want the run's entry KEPT", outstanding)
	}
	term, pending, err := h.queue.PendingTerminal(testRepoKey, outstanding[0])
	if err != nil {
		t.Fatalf("PendingTerminal: %v", err)
	}
	if !pending || term.Outcome != OutcomeFailed || !strings.Contains(term.Cause, "source worktree is dirty") {
		t.Fatalf("PendingTerminal() = %+v, %v, want a failed record naming the driver error", term, pending)
	}
}

// THE BOOT SAYS THE WORD, IT DOES NOT REDO THE MERGE. A marked entry belongs to
// a run whose outcome was already reached, so the replay publishes the terminal
// status and never touches the cherry-pick layer.
func TestBootReplaysAnUnpublishedTerminalStatus(t *testing.T) {
	tests := []struct {
		name string
		term TerminalStatus
		want string
	}{
		{
			name: "merged",
			term: TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"},
			want: armMerged,
		},
		{
			name: "merged carrying the after-action failure",
			term: TerminalStatus{
				Outcome:          OutcomeMerged,
				Cause:            "cherry-pick landed on target",
				AfterActionError: "the workspace's after-merge action did not complete: the turn never ended",
			},
			want: armMerged,
		},
		{
			name: "failed",
			term: TerminalStatus{Outcome: OutcomeFailed, Cause: "shim lease unavailable: shim gone"},
			want: armFailed,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a previous daemon's entry, marked with the word it could
			// not publish, then a new coordinator over the same directory.
			q, dir := newTestQueue(t)
			req := testRequest("a")
			if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
				t.Fatalf("Publish: %v", err)
			}
			if err := q.MarkTerminal(testRepoKey, req, tc.term); err != nil {
				t.Fatalf("MarkTerminal: %v", err)
			}
			next, err := NewFileQueue(dir, t.Logf)
			if err != nil {
				t.Fatalf("NewFileQueue: %v", err)
			}
			h := newHarnessWith(t, harnessOpts{queue: next, dir: dir})

			// Act.
			if err := h.coord.Drain(context.Background()); err != nil {
				t.Fatalf("Drain: %v", err)
			}

			// Assert — the terminal word reaches the sink, with its recorded
			// cause, and the merge itself is never re-run.
			tr := h.sink.awaitPhase(t, terminalPhase(tc.term.Outcome))
			if tr.cause != tc.term.Cause {
				t.Fatalf("replayed cause = %q, want the recorded %q", tr.cause, tc.term.Cause)
			}
			statuses := h.sink.publishedStatuses()
			last := statuses[len(statuses)-1]
			if got := statusArm(last); got != tc.want {
				t.Fatalf("replayed arm = %q, want %q", got, tc.want)
			}
			if got := last.GetMerged().GetAfterActionError(); got != tc.term.AfterActionError {
				t.Fatalf("replayed after_action_error = %q, want %q", got, tc.term.AfterActionError)
			}
			if got := len(h.picker.merges); got != 0 {
				t.Fatalf("picker Merge calls = %d, want the merge NOT re-run", got)
			}
		})
	}
}

// terminalPhase is the merge-axis phase a terminal outcome publishes on.
func terminalPhase(outcome Outcome) Phase {
	if outcome == OutcomeMerged {
		return PhaseMerged
	}
	return PhaseMergeFailed
}

// The run keeps its NAME across the silence: the terminal status arrives on the
// run the user watched go quiet, not on a stranger.
func TestAReplayedTerminalStatusKeepsTheRunItWasAdmittedUnder(t *testing.T) {
	// Arrange.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	run, err := NewRunStatus(&recordingSink{}, t.Logf, req.Workspace, testClock())
	if err != nil {
		t.Fatalf("NewRunStatus: %v", err)
	}
	req.Run = run
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if err := q.MarkTerminal(testRepoKey, req, TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"}); err != nil {
		t.Fatalf("MarkTerminal: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	h := newHarnessWith(t, harnessOpts{queue: next, dir: dir})

	// Act.
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert.
	h.sink.awaitPhase(t, PhaseMerged)
	statuses := h.sink.publishedStatuses()
	if got := statuses[len(statuses)-1].GetRunId(); got != run.RunID() {
		t.Fatalf("replayed run id = %q, want the admitted %q", got, run.RunID())
	}
}

// ONLY A PUBLICATION THAT LANDED ACKS THE ENTRY. Once the replayed word is on
// the wire the entry is dropped, so nothing replays it a second time.
func TestAReplayedTerminalStatusAcksItsQueueEntry(t *testing.T) {
	// Arrange — a marked entry and a hook that announces the post-merge handoff,
	// which runs strictly after the entry is dropped.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if err := q.MarkTerminal(testRepoKey, req, TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"}); err != nil {
		t.Fatalf("MarkTerminal: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	hook := newFakePostMergeHook(4)
	t.Cleanup(func() { close(hook.stop) })
	h := newHarnessWith(t, harnessOpts{queue: next, dir: dir, postMerge: hook})

	// Act.
	if err := h.coord.Drain(context.Background()); err != nil {
		t.Fatalf("Drain: %v", err)
	}

	// Assert — the hook only runs once the entry is gone, so its call is the
	// rendezvous that proves the ack happened.
	<-hook.calls
	hook.results <- nil
	if got := h.queue.Snapshot()[testRepoKey]; len(got) != 0 {
		t.Fatalf("outstanding entries after the replay = %+v, want empty", got)
	}
}

// ONE TERMINAL WORD, NOT TWO. A boot after the replay finds nothing to say: the
// entry was acked, so a second daemon does not publish the run's terminal status
// all over again.
func TestASecondBootAfterAReplayedTerminalPublishesNothing(t *testing.T) {
	// Arrange — a marked entry replayed to completion by one coordinator.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if err := q.MarkTerminal(testRepoKey, req, TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"}); err != nil {
		t.Fatalf("MarkTerminal: %v", err)
	}
	firstQueue, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	hook := newFakePostMergeHook(4)
	t.Cleanup(func() { close(hook.stop) })
	first := newHarnessWith(t, harnessOpts{queue: firstQueue, dir: dir, postMerge: hook})
	if err := first.coord.Drain(context.Background()); err != nil {
		t.Fatalf("first Drain: %v", err)
	}
	<-hook.calls
	hook.results <- nil

	// Act — the boot after the one that finally published the word.
	secondQueue, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	second := newHarnessWith(t, harnessOpts{queue: secondQueue, dir: dir})
	if err := second.coord.Drain(context.Background()); err != nil {
		t.Fatalf("second Drain: %v", err)
	}

	// Assert — nothing left to replay, and nothing published.
	if got := secondQueue.Snapshot()[testRepoKey]; len(got) != 0 {
		t.Fatalf("outstanding entries at the second boot = %+v, want empty", got)
	}
	if got := second.sink.transitions(); len(got) != 0 {
		t.Fatalf("second boot published %+v, want nothing", got)
	}
}

// --- eviction -----------------------------------------------------------

// A waiting merge comes off the queue, says so under the run the user has been
// watching, and never reaches the picker.
func TestEvictTakesAWaitingMergeOffTheQueue(t *testing.T) {
	// Arrange — a merge in flight and another queued behind it.
	h := newHarness(t)
	head, waiting, later := testRequest("a"), testRequest("b"), testRequest("c")
	if _, err := h.coord.Enqueue(context.Background(), head); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges
	if _, err := h.coord.Enqueue(context.Background(), waiting); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Act.
	evicted, err := h.coord.Evict(context.Background(), waiting.Workspace)

	// Assert — one entry dropped, the head untouched.
	if err != nil || evicted != 1 {
		t.Fatalf("Evict() = (%d, %v), want (1, nil)", evicted, err)
	}
	if got := h.queue.Snapshot()[testRepoKey]; len(got) != 1 || got[0].Name != head.Name {
		t.Fatalf("queue after eviction = %+v, want the head alone", got)
	}
	// The terminal word lands on the SAME run the admission published under.
	runID := enqueuedRunID(t, h.sink.publishedStatuses(), 2)
	failed := failedStatus(t, h.sink.publishedStatuses(), runID)
	if failed.GetCause() != evictedCause {
		t.Fatalf("failed cause = %q, want %q", failed.GetCause(), evictedCause)
	}

	// And the drain skips it: the next merge the picker sees is the one queued
	// AFTER the eviction, never the evicted one.
	if _, err := h.coord.Enqueue(context.Background(), later); err != nil {
		t.Fatalf("Enqueue(c): %v", err)
	}
	h.sink.awaitPhase(t, PhaseMergeQueued)
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if got := <-h.picker.merges; !sameRequest(got, later) {
		t.Fatalf("next merge = %+v, want %+v", got, later)
	}
}

// The merge ALREADY RUNNING is not evicted: it holds the shim lease and may be
// mid-cherry-pick, so taking its entry would strand it.
func TestEvictLeavesTheMergeInFlightAlone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	evicted, err := h.coord.Evict(context.Background(), req.Workspace)

	// Assert.
	if err != nil || evicted != 0 {
		t.Fatalf("Evict() = (%d, %v), want (0, nil)", evicted, err)
	}
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 1 {
		t.Fatalf("queue depth = %d, want the in-flight merge still on it", got)
	}
}

// A workspace with nothing queued is the ordinary case, not an error.
func TestEvictWithNothingQueuedIsANoOp(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	evicted, err := h.coord.Evict(context.Background(), "/nowhere/ws")

	// Assert.
	if err != nil || evicted != 0 {
		t.Fatalf("Evict() = (%d, %v), want (0, nil)", evicted, err)
	}
}

// An unnamed workspace is a construction bug at the call site, refused rather
// than swept across every repository on the queue.
func TestEvictRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	evicted, err := h.coord.Evict(context.Background(), "")

	// Assert.
	if err == nil || evicted != 0 {
		t.Fatalf("Evict() = (%d, %v), want (0, error)", evicted, err)
	}
}

// enqueuedRunID is the run id of the `enqueued` status published at position,
// which is how a test names the run an admission created without reaching into
// the coordinator for it.
func enqueuedRunID(t *testing.T, statuses []*frontendv1.MergeStatus, position int32) string {
	t.Helper()
	for _, s := range statuses {
		if arm := s.GetEnqueued(); arm != nil && arm.GetPosition() == position {
			return s.GetRunId()
		}
	}
	t.Fatalf("no enqueued status at position %d in %d published statuses", position, len(statuses))
	return ""
}

// failedStatus is runID's terminal `failed` arm.
func failedStatus(t *testing.T, statuses []*frontendv1.MergeStatus, runID string) *frontendv1.MergeStatusFailed {
	t.Helper()
	for _, s := range statuses {
		if s.GetRunId() == runID && s.GetFailed() != nil {
			return s.GetFailed()
		}
	}
	t.Fatalf("no failed status for run %s in %d published statuses", runID, len(statuses))
	return nil
}

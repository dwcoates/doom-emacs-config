package merge

import (
	"context"
	"sync"
	"testing"
)

// --- fakes --------------------------------------------------------------

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
	queued        chan queuedRecord
	queuedErr     error
	stop          chan struct{}
}

func newFakePicker(capacity int) *fakePicker {
	return &fakePicker{
		merges:        make(chan Request, capacity),
		results:       make(chan pickResult, capacity),
		resumes:       make(chan Request, capacity),
		resumeResults: make(chan pickResult, capacity),
		queued:        make(chan queuedRecord, capacity),
		stop:          make(chan struct{}),
	}
}

func (p *fakePicker) Merge(_ context.Context, req Request) (Result, error) {
	p.merges <- req
	select {
	case r := <-p.results:
		return r.res, r.err
	case <-p.stop:
		return Result{}, errStopped
	}
}

func (p *fakePicker) Resume(_ context.Context, req Request) (Result, error) {
	p.resumes <- req
	select {
	case r := <-p.resumeResults:
		return r.res, r.err
	case <-p.stop:
		return Result{}, errStopped
	}
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
	mu  sync.Mutex
	got []transition
	ch  chan transition
	err error
}

func newSyncSink(capacity int) *syncSink {
	return &syncSink{ch: make(chan transition, capacity)}
}

func (s *syncSink) RecordMergeTransition(ws string, phase Phase, cause string) error {
	s.mu.Lock()
	s.got = append(s.got, transition{ws, phase, cause})
	s.mu.Unlock()
	s.ch <- transition{ws, phase, cause}
	return s.err
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
func (q failingQueue) Snapshot() map[string][]Request { return nil }
func (q failingQueue) Complete(string, Request) error { return nil }

// harness bundles a coordinator with the fakes behind it.
type harness struct {
	coord    *QueueCoordinator
	queue    *FileQueue
	picker   *fakePicker
	lease    *fakeLease
	resolver *fakeResolver
	sink     *syncSink
	dir      string
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
	sink := newSyncSink(8)
	hook := opts.postMerge
	if hook == nil {
		hook = newAutoPostMergeHook(8)
	}
	coord, err := NewCoordinator(CoordinatorConfig{
		Logf:      t.Logf,
		Sink:      sink,
		Queue:     q,
		Keyer:     fakeKeyer{keys: opts.keys, err: opts.keyErr},
		Picker:    picker,
		Lease:     lease,
		Resolver:  resolver,
		PostMerge: hook,
	})
	if err != nil {
		t.Fatalf("NewCoordinator: %v", err)
	}
	t.Cleanup(func() {
		close(picker.stop)
		close(resolver.stop)
		if err := coord.Close(); err != nil {
			t.Fatalf("Close: %v", err)
		}
	})
	return &harness{coord: coord, queue: q, picker: picker, lease: lease, resolver: resolver, sink: sink, dir: dir}
}

// --- construction -------------------------------------------------------

func TestNewCoordinatorRequiresEveryDependency(t *testing.T) {
	q, _ := newTestQueue(t)
	complete := func() CoordinatorConfig {
		return CoordinatorConfig{
			Logf:      func(string, ...any) {},
			Sink:      newSyncSink(1),
			Queue:     q,
			Keyer:     fakeKeyer{},
			Picker:    newFakePicker(1),
			Lease:     newFakeLease(1),
			Resolver:  newFakeResolver(1),
			PostMerge: newAutoPostMergeHook(1),
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
		{name: "no keyer", mutate: func(c *CoordinatorConfig) { c.Keyer = nil }, wantErr: true},
		{name: "no picker", mutate: func(c *CoordinatorConfig) { c.Picker = nil }, wantErr: true},
		{name: "no lease", mutate: func(c *CoordinatorConfig) { c.Lease = nil }, wantErr: true},
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
		Keyer: fakeKeyer{}, Picker: picker, Lease: newFakeLease(1), Resolver: newFakeResolver(1),
		PostMerge: newAutoPostMergeHook(1),
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
	if len(h.picker.queued) != 0 {
		t.Fatalf("merge_queued recorded for an immediately admitted merge")
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
	rec := <-h.picker.queued
	if rec.ws != second.Workspace {
		t.Fatalf("merge_queued workspace = %q, want %q", rec.ws, second.Workspace)
	}
}

func TestEnqueueSurfacesAMergeQueuedRecordFailure(t *testing.T) {
	// Arrange — the head is mid-merge and the queued transition cannot be
	// recorded.
	h := newHarness(t)
	h.picker.queuedErr = errFakeSink
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges

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
	if got := <-h.picker.merges; got != first {
		t.Fatalf("first merge = %+v, want %+v", got, first)
	}
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	<-h.picker.queued

	// Act — the second merge must not have started while the first is running.
	if len(h.picker.merges) != 0 {
		t.Fatalf("second merge started while the first was in flight")
	}
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}

	// Assert — it starts only once the first finished.
	if got := <-h.picker.merges; got != second {
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
	if got := <-h.picker.merges; got != other {
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
	if got := <-h.picker.merges; got != req {
		t.Fatalf("resumed merge = %+v, want %+v", got, req)
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
	got := <-h.sink.ch
	if got.phase != PhaseMergeFailed || got.ws != req.Workspace {
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
	<-h.sink.ch

	// Assert — a merge that can never run must not block the queue forever.
	if got := len(h.queue.Snapshot()[testRepoKey]); got != 0 {
		t.Fatalf("queue depth = %d, want 0", got)
	}
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
	got := <-h.sink.ch
	if got.phase != PhaseMergeFailed {
		t.Fatalf("transition = %+v, want merge_failed", got)
	}
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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	<-h.picker.queued

	// Act — resolve the conflict.
	go func() {
		if err := h.coord.Resume(context.Background(), first); err != nil {
			t.Errorf("Resume: %v", err)
		}
	}()
	if got := <-h.picker.resumes; got != first {
		t.Fatalf("resume = %+v, want %+v", got, first)
	}

	// Assert — the lease was never released while parked, and the queue
	// advances only after the resume lands terminally.
	if !h.lease.Held(first.Workspace) {
		t.Fatalf("lease released while the conflict was parked")
	}
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-h.lease.releases
	if got := <-h.picker.merges; got != second {
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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	got := <-h.resolver.calls

	// Assert — the resolver is handed the facts the resolving turn needs.
	if got.Workspace != req.Workspace || got.SourceBranch != req.SourceBranch || got.TargetDir != req.TargetDir {
		t.Fatalf("resolution = %+v, want workspace/branch/target of %+v", got, req)
	}
	if got.ConflictCommit != "abc1234" {
		t.Fatalf("resolution commit = %q, want abc1234", got.ConflictCommit)
	}
	if got.RequestID == "" {
		t.Fatalf("resolution carries no request id")
	}
}

func TestConflictDrivesTheShimExactlyOnce(t *testing.T) {
	// Arrange — a conflict whose shim-driven resume conflicts again.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	<-h.resolver.calls

	// Act — the resolution turn ends, the resume still conflicts.
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "def5678"}}

	// Assert — a HUMAN resume is served, and no second shim attempt was made
	// (the human's resume is the next thing the park sees).
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if err := <-done; err != nil {
		t.Fatalf("Resume() error = %v, want nil", err)
	}
	if got := len(h.resolver.calls); got != 0 {
		t.Fatalf("resolver calls still pending = %d, want 0 (exactly one attempt)", got)
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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict}}

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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	<-h.resolver.calls

	// Assert — nothing is resumed while the resolution turn is still running.
	if got := len(h.picker.resumes); got != 0 {
		t.Fatalf("resumes before the resolution turn ended = %d, want 0", got)
	}

	// Act — the turn completes.
	h.resolver.results <- nil

	// Assert — only now is the cherry-pick continued.
	if got := <-h.picker.resumes; got != req {
		t.Fatalf("resume = %+v, want %+v", got, req)
	}
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-h.lease.releases
}

func TestShimResolutionThatIsStillConflictedLeavesTheParkStanding(t *testing.T) {
	// Arrange — the shim resolves, and the resume conflicts again.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	<-h.resolver.calls

	// Act.
	h.resolver.results <- nil
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "def5678"}}

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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	<-h.resolver.calls

	// Act.
	h.resolver.results <- sentinelError("no live session to drive")

	// Assert — nothing was resumed and nothing was marked merged; the park is
	// still there for the human, who drives it to its terminal outcome.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	if got := <-h.picker.resumes; got != req {
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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict}}

	// Assert — the resume was served rather than refused for a conflict that
	// was about to exist.
	if got := <-h.picker.resumes; got != req {
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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict}}

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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict}}

	// Act — the next commit in the range conflicts too.
	done := make(chan error, 1)
	go func() { done <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeConflict}}

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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict}}
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	<-h.picker.queued

	// Act.
	abandoned, err := h.coord.Abandon(context.Background(), first.Workspace)

	// Assert — the lease went back and the queue advanced past the head.
	if err != nil || !abandoned {
		t.Fatalf("Abandon() = (%v, %v), want (true, nil)", abandoned, err)
	}
	if got := <-h.lease.releases; got != first.Workspace {
		t.Fatalf("lease released for %q, want %q", got, first.Workspace)
	}
	if got := <-h.picker.merges; got != second {
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
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict}}

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
		Keyer: fakeKeyer{}, Picker: picker, Lease: lease, Resolver: resolver,
		PostMerge: newAutoPostMergeHook(4),
	})
	if err != nil {
		t.Fatalf("NewCoordinator: %v", err)
	}
	req := testRequest("a")
	if _, err := coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-picker.merges
	picker.results <- pickResult{res: Result{Outcome: OutcomeConflict}}

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

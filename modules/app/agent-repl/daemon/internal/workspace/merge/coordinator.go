package merge

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"claude-repld/internal/dlog"
)

// Picker is the git cherry-pick layer merge.Coordinator drives. *merge.Driver
// satisfies it; the interface exists so the coordinator's queueing, leasing and
// conflict-parking behavior is unit-testable without a git fixture per case.
type Picker interface {
	Merge(ctx context.Context, req Request) (Result, error)
	Resume(ctx context.Context, req Request) (Result, error)
	// ContinueAfterTestFix commits what a test-failure resolution turn staged
	// in the target worktree, re-runs the suite, and continues the per-commit
	// replay.
	ContinueAfterTestFix(ctx context.Context, req Request, failingCommit string) (Result, error)
	// Rollback resets the target worktree to head, undoing everything the merge
	// landed. It is the tail of a merge whose test gate failed for good.
	Rollback(ctx context.Context, req Request, head string) error
	MarkQueued(ws, cause string) error
}

// The git cherry-pick layer is the production Picker, and the coordinator is
// the production merge.Coordinator. Both bindings are asserted here so a
// contract drift is a compile error rather than a wiring surprise.
var (
	_ Picker      = (*Driver)(nil)
	_ Coordinator = (*QueueCoordinator)(nil)
)

// CoordinatorConfig constructs a merge.QueueCoordinator. EVERY field is
// required. A merge coordinator missing any one of them is not a degraded
// coordinator, it is a broken one: without the lease it drives merges into a
// session a user is still prompting, without the keyer it cannot tell sibling
// worktrees apart from separate repositories, and without the queue it has
// nothing durable to resume after the bounce a self-merge causes.
type CoordinatorConfig struct {
	// Logf receives the coordinator's queue and drain logging.
	Logf dlog.Logf
	// Sink receives the transitions the COORDINATOR itself owns, which are the
	// ones no merge.Driver call ever reaches: a lease that could not be taken,
	// and a merge.Driver precondition rejection that would otherwise leave a
	// workspace parked at merge_queued forever.
	Sink StateSink
	// Queue is the durable per-repository request channel.
	Queue DurableQueue
	// Phases reads back the phase a workspace currently rests on, which is what
	// Drain's boot sweep of orphaned merge_enqueuing marks needs. Required, on
	// the same footing as the rest: without it a merge attempt that died before
	// its durable enqueue would leave its workspace pinned on merge_enqueuing
	// forever, with no queue entry for anything to notice it by.
	Phases PhaseSource
	// Keyer resolves the queue key from a request's target worktree.
	Keyer RepoKeyer
	// Picker is the git cherry-pick layer (*merge.Driver in production).
	Picker Picker
	// Lease is the shim exclusivity claim held across each driven merge.
	Lease Lease
	// Resolver drives the merging workspace's OWN shim to resolve a conflicted
	// cherry-pick. Required, on the same footing as the rest: a coordinator
	// without it parks every conflict on a human who may not be watching, while
	// the session that wrote the conflicting commits sits idle under a lease
	// taken expressly to drive it.
	Resolver ConflictResolver
	// TestResolver drives the merging workspace's OWN shim to fix a test suite
	// a cherry-picked commit broke. Required, on the same footing as Resolver
	// and for the same reason: without it every suite failure would fail the
	// merge outright while the session that wrote the offending commit sits
	// idle under a lease taken expressly to drive it.
	TestResolver TestFailureResolver
	// PostMerge receives every `merged` terminal outcome once the queue entry
	// is dropped and the lease released. Required, on the same footing as the
	// rest: it carries the child-to-parent handoffs (the merge phone-home and
	// the workspace's postprocessing prompt), and a coordinator without it
	// silently drops them exactly the way the deleted Emacs handler did.
	PostMerge PostMergeHook
	// Status receives every phase-level MergeStatus the pipeline publishes.
	// Required, on the same footing as the rest: a coordinator that cannot
	// publish phases drives merges no frontend can watch, which is the state
	// this whole pipeline exists to end.
	Status StatusSink
	// Sessions brings the merging workspace's session up before the lease is
	// taken. Required: a merge drives that session for the before-action, the
	// conflict resolution and the test fix, and a hibernated workspace (which
	// the idle sweeper produces routinely) would fail all three.
	Sessions SessionBringUp
	// Deaths reports whether the merging workspace's newest session was DELETED.
	// Required, and asked before Sessions: without it the bring-up cannot tell a
	// recoverable hibernation from a session the user destroyed, so it would
	// resurrect the second while trying to rehydrate the first.
	Deaths SessionDeaths
	// BeforeActions resolves the before_ws_merge action a workspace was created
	// with. Required: a coordinator without it would silently skip an action the
	// user asked for at creation, which is indistinguishable from the action
	// having run and done nothing.
	BeforeActions BeforeActionSource
	// BeforeActionRunner delivers that action to the workspace's own session.
	// Required for the same reason.
	BeforeActionRunner BeforeActionRunner
	// AfterActionRunner delivers the workspace's postprocessing action to that
	// same session once every commit has landed. Required: without it a
	// workspace created with a postprocessing prompt merges and the prompt is
	// never run at all, which is indistinguishable from it having run and done
	// nothing.
	AfterActionRunner AfterActionRunner
	// Now returns unix millis for the phase timestamps. Nil uses the wall clock;
	// a test injects one so both stamps are pinned.
	Now func() int64
}

// QueueCoordinator is the merge.Coordinator implementation: one drain goroutine
// per REPOSITORY, each owning that repository's queue end to end.
//
// Single ownership is the whole design. One goroutine per repository takes the
// entries in order, holds the shim lease across each merge, and does not look
// at the next entry until the current one reaches a terminal outcome. Two
// cherry-picks against one target worktree are therefore unrepresentable rather
// than improbable — there is no lock to forget to take, because there is no
// second party to take one.
type QueueCoordinator struct {
	logf         dlog.Logf
	emit         *stateEmitter
	queue        DurableQueue
	phases       PhaseSource
	keyer        RepoKeyer
	picker       Picker
	lease        Lease
	resolver     ConflictResolver
	testResolver TestFailureResolver
	postMerge    PostMergeHook

	status         StatusSink
	sessions       SessionBringUp
	deaths         SessionDeaths
	beforeActions  BeforeActionSource
	beforeActionFn BeforeActionRunner
	afterActionFn  AfterActionRunner
	now            func() int64

	// ctx bounds every DRIVEN merge, and is deliberately not the caller's
	// Enqueue context: a cherry-pick outlives the frontend command that asked
	// for it, so binding git to the request context would cancel a merge the
	// instant the command returned.
	ctx  context.Context
	stop context.CancelFunc
	wg   sync.WaitGroup

	mu       sync.Mutex
	draining map[string]struct{}
	cancels  map[string]func()
	gates    map[string]*sync.Mutex
	parks    map[string]*conflictPark

	// postMergeMu guards the retained hook-failure record. It is deliberately
	// its own mutex: the hook runs off the drain goroutine, and recording its
	// failure must not contend with (let alone block behind) the queue
	// bookkeeping c.mu protects.
	postMergeMu       sync.Mutex
	postMergeFailures []PostMergeFailure
}

// conflictPark is the resume rendezvous for the merge a repository currently
// has in flight. A conflicted merge holds both its lease and its place at the
// head of the repository's queue until a human resolves it and Resume continues
// it.
//
// It keeps the head occupied on purpose: a worktree with a paused cherry-pick
// cannot receive another one, so a repository whose head is conflicted is
// genuinely unable to merge anything else.
//
// The park is installed BEFORE the cherry-pick starts, not when the conflict
// appears. That ordering is what makes the resume race unrepresentable: the
// frontend sends conflict_resolved_continue after seeing merge_conflict, and
// merge.Driver pushes merge_conflict from inside Merge — so a resume can and
// does arrive before the coordinator would otherwise have parked. Publishing
// the rendezvous first turns "did the park exist yet" into "the resume waits
// for the merge to reach a state that can accept it".
type conflictPark struct {
	req   Request
	calls chan resumeCall
	// abandons is the give-up rendezvous: closing the workspace a conflicted
	// merge was for means abandoning that merge, and the send completes only
	// once the merge is actually parked, exactly like calls.
	abandons chan abandonCall
	// ended closes when the merge leaves the coordinator, which is what tells
	// a waiting resume that there is no longer anything to continue.
	ended chan struct{}
}

type resumeCall struct {
	req   Request
	reply chan error
}

type abandonCall struct {
	reply chan error
}

// NewCoordinator validates cfg and returns the coordinator. Any missing
// dependency is a hard construction error.
func NewCoordinator(cfg CoordinatorConfig) (*QueueCoordinator, error) {
	switch {
	case cfg.Logf == nil:
		return nil, fmt.Errorf("merge: Coordinator Logf is required")
	case cfg.Sink == nil:
		return nil, fmt.Errorf("merge: Coordinator Sink is required")
	case cfg.Queue == nil:
		return nil, fmt.Errorf("merge: Coordinator Queue is required")
	case cfg.Phases == nil:
		return nil, fmt.Errorf("merge: Coordinator Phases source is required")
	case cfg.Keyer == nil:
		return nil, fmt.Errorf("merge: Coordinator Keyer is required")
	case cfg.Picker == nil:
		return nil, fmt.Errorf("merge: Coordinator Picker is required")
	case cfg.Lease == nil:
		return nil, fmt.Errorf("merge: Coordinator Lease is required")
	case cfg.Resolver == nil:
		return nil, fmt.Errorf("merge: Coordinator Resolver is required")
	case cfg.TestResolver == nil:
		return nil, fmt.Errorf("merge: Coordinator TestResolver is required")
	case cfg.PostMerge == nil:
		return nil, fmt.Errorf("merge: Coordinator PostMerge hook is required")
	case cfg.Status == nil:
		return nil, fmt.Errorf("merge: Coordinator Status sink is required")
	case cfg.Sessions == nil:
		return nil, fmt.Errorf("merge: Coordinator Sessions bring-up is required")
	case cfg.Deaths == nil:
		return nil, fmt.Errorf("merge: Coordinator Deaths source is required")
	case cfg.BeforeActions == nil:
		return nil, fmt.Errorf("merge: Coordinator BeforeActions source is required")
	case cfg.BeforeActionRunner == nil:
		return nil, fmt.Errorf("merge: Coordinator BeforeActionRunner is required")
	case cfg.AfterActionRunner == nil:
		return nil, fmt.Errorf("merge: Coordinator AfterActionRunner is required")
	}
	now := cfg.Now
	if now == nil {
		now = func() int64 { return time.Now().UnixMilli() }
	}
	ctx, stop := context.WithCancel(context.Background())
	return &QueueCoordinator{
		logf:         cfg.Logf,
		emit:         &stateEmitter{sink: cfg.Sink, logf: cfg.Logf},
		queue:        cfg.Queue,
		phases:       cfg.Phases,
		keyer:        cfg.Keyer,
		picker:       cfg.Picker,
		lease:        cfg.Lease,
		resolver:     cfg.Resolver,
		testResolver: cfg.TestResolver,
		postMerge:    cfg.PostMerge,

		status:         cfg.Status,
		sessions:       cfg.Sessions,
		deaths:         cfg.Deaths,
		beforeActions:  cfg.BeforeActions,
		beforeActionFn: cfg.BeforeActionRunner,
		afterActionFn:  cfg.AfterActionRunner,
		now:            now,

		ctx:      ctx,
		stop:     stop,
		draining: map[string]struct{}{},
		cancels:  map[string]func(){},
		gates:    map[string]*sync.Mutex{},
		parks:    map[string]*conflictPark{},
	}, nil
}

// Enqueue implements merge.Coordinator. It returns once the request is DURABLY
// on its repository's queue, not when the merge completes.
//
// The run's `enqueued` status is published here for EVERY admission, and it is
// published UNDER THE REPOSITORY'S ADVANCE GATE. That gate is also held across
// the drain loop's Complete, so the head cannot finish (and this entry cannot
// start merging) between the publish that observed the position and the status
// that reports it. The ordering is structural, not a hoped-for window.
//
// The coarse axis token that rides the status is merge_queued only when the
// entry did not land at the head; a head admission stays at merge_enqueuing
// until the cherry-pick moves it on.
func (c *QueueCoordinator) Enqueue(ctx context.Context, req Request) (Position, error) {
	if err := req.validate(); err != nil {
		c.logf("merge: enqueue REFUSED invalid request {ws=%s name=%s}: %v", req.Workspace, req.Name, err)
		return Position{}, err
	}
	repo, err := c.keyer.RepoKey(ctx, req.TargetDir)
	if err != nil {
		c.logf("merge: enqueue repo-key FAILED {ws=%s name=%s target=%s}: %v", req.Workspace, req.Name, req.TargetDir, err)
		return Position{}, fmt.Errorf("merge: enqueue %q: %w", req.Name, err)
	}

	// THE RUN IS MINTED HERE, at the admission, and lives until this entry
	// reaches a terminal outcome — so the `enqueued` phase a user watches and the
	// `merged` that ends it carry ONE id. The publisher rides the in-memory
	// request; its ID rides the DURABLE entry beside it, so a bounce mid-queue
	// interrupts the run rather than replacing it with one the user never
	// submitted.
	run, err := NewRunStatus(c.status, c.logf, req.Workspace, c.now)
	if err != nil {
		c.logf("merge: enqueue run construction FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		return Position{}, fmt.Errorf("merge: enqueue %q: %w", req.Name, err)
	}
	req.Run = run

	gate := c.gate(repo)
	gate.Lock()
	pos, err := c.queue.Publish(ctx, repo, req)
	if err != nil {
		gate.Unlock()
		c.logf("merge: enqueue publish FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		return Position{}, fmt.Errorf("merge: enqueue %q: %w", req.Name, err)
	}
	// THE ADMISSION IS ALWAYS PUBLISHED, at the head as well as behind another
	// merge. `enqueued` is the run's FIRST status and the only one that reports
	// where it landed, so skipping it for a head admission would leave the run's
	// stream starting mid-flight with no admission to correlate against.
	//
	// The coarse axis still distinguishes the two: a head admission stays at
	// merge_enqueuing (the handler's own mark, which the cherry-pick supersedes),
	// and only a deferred one is merge_queued.
	phase := PhaseMergeEnqueuing
	cause := fmt.Sprintf("admitted at the head of repo %s", repo)
	if pos.Index > 1 {
		phase = PhaseMergeQueued
		cause = fmt.Sprintf("queued at position %d of %d behind another merge on repo %s", pos.Index, pos.Depth, repo)
	}
	var queuedErr error
	if err := run.Enqueued(phase, int32(pos.Index), int32(pos.Depth), cause); err != nil {
		c.logf("merge: enqueue %s emit FAILED {repo=%s ws=%s name=%s}: %v", phase, repo, req.Workspace, req.Name, err)
		queuedErr = fmt.Errorf("merge: enqueue %q: record %s: %w", req.Name, phase, err)
	}
	gate.Unlock()

	// The entry is durable either way, so the drain must start even when the
	// merge_queued record failed: abandoning a durably queued merge because its
	// state write failed would strand the work with no one draining it.
	c.startDrain(repo)
	if queuedErr != nil {
		return Position{}, queuedErr
	}
	c.logf("merge: enqueued {repo=%s ws=%s name=%s index=%d depth=%d}", repo, req.Workspace, req.Name, pos.Index, pos.Depth)
	return pos, nil
}

// Drain implements merge.Coordinator: it reconstructs every repository queue
// from its durable record and resumes draining. It runs at daemon boot, which
// is what lets a merge survive the bounce a self-merge of the daemon causes.
func (c *QueueCoordinator) Drain(_ context.Context) error {
	snap := c.queue.Snapshot()
	for repo, reqs := range snap {
		if len(reqs) == 0 {
			continue
		}
		c.logf("merge: drain resuming {repo=%s depth=%d head_ws=%s}", repo, len(reqs), reqs[0].Workspace)
		c.startDrain(repo)
	}
	return c.sweepEnqueuing(snap)
}

// enqueuingLostCause is what an orphaned merge_enqueuing is failed with. It is
// deliberately a full sentence: it is the only thing the user will see about a
// merge attempt that left no other trace.
const enqueuingLostCause = "daemon restarted before the merge was durably enqueued"

// sweepEnqueuing fails every workspace resting on merge_enqueuing that has no
// durable queue entry behind it.
//
// WHY IT IS NEEDED AT ALL. merge_enqueuing is the one merge phase with nothing
// durable behind it: the command handler emits it on receipt, before the
// geometry is resolved and before the queue write. A daemon that dies in that
// window has genuinely lost the attempt — there is no entry to replay and no
// goroutine that will ever advance the phase — so a boot that stayed quiet
// would leave the workspace pinned on "enqueuing" for the rest of its life.
//
// WHY IT CHECKS THE QUEUE. The mark and the durable write are two steps, and a
// boot can land after both: a workspace whose entry IS on the queue is a merge
// that will be drained normally, and failing it here would contradict a merge
// that is about to run. The snapshot is therefore the authority on what is
// still alive, and only workspaces absent from it are swept.
//
// Every sweep decision is loud-logged, both the failures and the retentions,
// because a merge silently reclassified at boot is exactly the kind of event a
// user later needs to find in the log.
func (c *QueueCoordinator) sweepEnqueuing(snap map[string][]Request) error {
	pinned, err := c.phases.WorkspacesAtPhase(PhaseMergeEnqueuing)
	if err != nil {
		// The sweep could not run. That is not "nothing to sweep": any
		// workspace stuck at merge_enqueuing stays stuck, so the boot says so
		// and surfaces the failure rather than reporting a clean drain.
		c.logf("merge: drain enqueuing sweep FAILED to read the pinned workspaces: %v — any workspace left at merge_enqueuing by the previous daemon stays pinned", err)
		return fmt.Errorf("merge: drain: read workspaces at %s: %w", PhaseMergeEnqueuing, err)
	}
	queued := map[string]struct{}{}
	for _, reqs := range snap {
		for _, req := range reqs {
			queued[req.Workspace] = struct{}{}
		}
	}
	var failed error
	for _, ws := range pinned {
		if _, ok := queued[ws]; ok {
			c.logf("merge: drain enqueuing sweep RETAINED {ws=%s} — the workspace is at merge_enqueuing but its durable queue entry survived, so the merge is drained normally", ws)
			continue
		}
		c.logf("merge: drain enqueuing sweep FAILING {ws=%s} — merge_enqueuing with NO durable queue entry: the attempt died before it was recorded and nothing will ever advance it", ws)
		if err := c.emit.emit(ws, PhaseMergeFailed, enqueuingLostCause); err != nil {
			c.logf("merge: drain enqueuing sweep merge_failed record FAILED {ws=%s}: %v", ws, err)
			failed = errors.Join(failed, err)
		}
	}
	return failed
}

// Resume continues a merge parked on a conflict its human has resolved (the
// conflict_resolved_continue handoff).
//
// It is NOT on the frozen merge.Coordinator interface, which has no verb for
// the resolved-conflict handoff. It must nevertheless run through the
// coordinator: the conflicted merge still holds the head of its repository's
// queue and its shim lease, and only the goroutine that owns them can finish
// it. Routing a resume around the coordinator (straight to merge.Driver) would
// leave the head occupied forever and stall every later merge on that
// repository.
func (c *QueueCoordinator) Resume(ctx context.Context, req Request) error {
	if err := req.validate(); err != nil {
		c.logf("merge: resume REFUSED invalid request {ws=%s name=%s}: %v", req.Workspace, req.Name, err)
		return err
	}
	repo, err := c.keyer.RepoKey(ctx, req.TargetDir)
	if err != nil {
		c.logf("merge: resume repo-key FAILED {ws=%s name=%s target=%s}: %v", req.Workspace, req.Name, req.TargetDir, err)
		return fmt.Errorf("merge: resume %q: %w", req.Name, err)
	}
	c.mu.Lock()
	park := c.parks[repo]
	c.mu.Unlock()
	if park == nil {
		c.logf("merge: resume with NO MERGE IN FLIGHT {repo=%s ws=%s name=%s}", repo, req.Workspace, req.Name)
		return fmt.Errorf("merge: resume %q: repo %s has no merge to continue", req.Name, repo)
	}
	if park.req.Workspace != req.Workspace {
		c.logf("merge: resume WORKSPACE MISMATCH {repo=%s parked_ws=%s got_ws=%s}", repo, park.req.Workspace, req.Workspace)
		return fmt.Errorf("merge: resume %q: repo %s is merging workspace %q, not %q",
			req.Name, repo, park.req.Workspace, req.Workspace)
	}

	// The send is the rendezvous: it completes only once the merge is actually
	// parked on a conflict, so a resume that races ahead of the park waits for
	// it instead of being refused for a conflict that is about to exist.
	reply := make(chan error, 1)
	select {
	case park.calls <- resumeCall{req: req, reply: reply}:
	case <-park.ended:
		c.logf("merge: resume after the merge ENDED {repo=%s ws=%s name=%s}", repo, req.Workspace, req.Name)
		return fmt.Errorf("merge: resume %q: repo %s has no conflicted merge to continue", req.Name, repo)
	case <-ctx.Done():
		return ctx.Err()
	case <-c.ctx.Done():
		return fmt.Errorf("merge: resume %q: coordinator is shutting down", req.Name)
	}
	select {
	case err := <-reply:
		return err
	case <-ctx.Done():
		return ctx.Err()
	}
}

// Abandon gives up the merge workspace currently has in flight, if any. It is
// the abandonment ingress for a parked conflict: closing the workspace a
// conflicted merge was for is the user action that means "nobody is resolving
// this", and it has no command of its own on the wire.
//
// Like Resume, it is NOT on the frozen merge.Coordinator interface, and like
// Resume it rendezvouses with the park: an abandon that arrives while the
// cherry-pick is still running waits for its outcome rather than racing it. A
// merge that reaches a clean terminal on its own closes the park, and the
// abandon reports (false, nil) — there was nothing left to give up.
//
// It returns (true, nil) once the parked merge released its lease and left the
// queue. The conflicted cherry-pick is LEFT IN THE TARGET TREE, loudly logged:
// abandoning the merge must not also destroy the half-resolved state a human
// may still want to finish by hand.
//
// A workspace whose merge is QUEUED but not yet at the head is not abandoned:
// its entry stays durable and merges when its turn comes, since the work lives
// in git and does not need the closed workspace's frontend.
func (c *QueueCoordinator) Abandon(ctx context.Context, workspace string) (bool, error) {
	c.mu.Lock()
	var park *conflictPark
	for _, p := range c.parks {
		if p.req.Workspace == workspace {
			park = p
			break
		}
	}
	c.mu.Unlock()
	if park == nil {
		return false, nil
	}
	call := abandonCall{reply: make(chan error, 1)}
	select {
	case park.abandons <- call:
	case <-park.ended:
		// The merge reached its own terminal while this abandon waited; the
		// lease and the queue entry are already handled.
		return false, nil
	case <-ctx.Done():
		return false, ctx.Err()
	case <-c.ctx.Done():
		return false, fmt.Errorf("merge: abandon %q: coordinator is shutting down", workspace)
	}
	select {
	case err := <-call.reply:
		return err == nil, err
	case <-ctx.Done():
		return false, ctx.Err()
	}
}

// Close stops every drain goroutine and waits for them. A merge in flight when
// Close lands keeps its durable queue entry, so the next daemon's Drain resumes
// it rather than losing it.
func (c *QueueCoordinator) Close() error {
	c.stop()
	c.mu.Lock()
	cancels := make([]func(), 0, len(c.cancels))
	for _, cancel := range c.cancels {
		cancels = append(cancels, cancel)
	}
	c.cancels = map[string]func(){}
	c.mu.Unlock()
	for _, cancel := range cancels {
		cancel()
	}
	c.wg.Wait()
	return nil
}

// startDrain ensures exactly one drain goroutine owns repo. It is idempotent:
// the second caller for a repository finds the owner already installed and
// returns, which is what keeps one repository to one owner.
func (c *QueueCoordinator) startDrain(repo string) {
	c.mu.Lock()
	if _, ok := c.draining[repo]; ok {
		c.mu.Unlock()
		return
	}
	if c.ctx.Err() != nil {
		c.mu.Unlock()
		c.logf("merge: drain NOT started, coordinator closed {repo=%s}", repo)
		return
	}
	c.draining[repo] = struct{}{}
	c.mu.Unlock()

	ch, cancel := c.queue.Subscribe(repo)
	c.mu.Lock()
	c.cancels[repo] = cancel
	c.mu.Unlock()
	c.wg.Add(1)
	go func() {
		defer c.wg.Done()
		c.drainLoop(repo, ch)
	}()
	c.logf("merge: drain started {repo=%s}", repo)
}

// drainLoop takes repo's entries one at a time. It exits — leaving the entry
// durable — when the coordinator closes mid-merge or the queue cannot advance,
// so the next boot's Drain picks up exactly where this one stopped.
func (c *QueueCoordinator) drainLoop(repo string, ch <-chan Request) {
	for req := range ch {
		if !c.runOne(repo, req) {
			c.logf("merge: drain stopping {repo=%s ws=%s}", repo, req.Workspace)
			return
		}
	}
	c.logf("merge: drain stream closed {repo=%s}", repo)
}

// finish retires a terminal merge: the queue entry is dropped FIRST, then the
// lease is released.
//
// That order is load-bearing. Releasing the lease is what re-pushes the
// workspace's state, and the queue facts on that state (merge_queue_position /
// merge_queue_depth) are read from the queue at push time. Releasing first
// would publish a terminal merge that still claims position 1 on a queue it has
// already left, with no later event to correct it.
//
// The lease is released even when the entry could not be dropped: a lease must
// never outlive the merge that took it, whatever the queue's storage did.
func (c *QueueCoordinator) finish(repo string, req Request, release func()) bool {
	gate := c.gate(repo)
	gate.Lock()
	err := c.queue.Complete(repo, req)
	gate.Unlock()
	if release != nil {
		release()
	}
	if err != nil {
		// The durable record could not be dropped. Advancing anyway would
		// replay this merge at the next boot, so the queue stops here and
		// says so rather than spinning on a storage failure.
		c.logf("merge: drain HALTED, queue complete failed {repo=%s ws=%s}: %v", repo, req.Workspace, err)
		return false
	}
	return true
}

// completeMergedRun runs the workspace's after-action and publishes the run's
// ONE terminal `merged` status, carrying whatever the action did.
//
// IT RUNS UNDER THE LEASE, BEFORE THE QUEUE ENTRY IS RETIRED, and that is the
// whole ordering: the after-action is a turn in the MERGED WORKSPACE'S OWN
// session, admissible only on the prompt path the merge lease holds open. The
// parent handoff that follows is the opposite — it prompts ANOTHER workspace's
// session and therefore must wait until the lease is gone (finishTerminal).
//
// THE AFTER-ACTION CANNOT FAIL THE RUN. Every commit is on the target before the
// action starts, and reporting `failed` because a courtesy turn did not land
// would deny a merge that demonstrably happened. Its error rides on the terminal
// merged status as after_action_error, which is why the status is published from
// HERE and not from merge.Driver: publishing it as the last commit landed put
// the run's terminal word on the wire before the after_action phase existed.
//
// It returns the after-action's failure text so the post-merge hook's own
// republication can carry it too: the merged FACT is set-once, but a hook that
// fails later must not drop the action's error off the status while adding its
// own.
func (c *QueueCoordinator) completeMergedRun(repo string, req Request, res Result) string {
	afterActionErr := c.runAfterAction(repo, req)
	cause := "cherry-pick landed on target"
	if res.AlreadyIncorporated {
		cause = "range already incorporated (no-op merge)"
	}
	if err := req.Run.Merged(afterActionErr, cause); err != nil {
		c.logf("merge: terminal merged status FAILED {repo=%s ws=%s run=%s}: %v — the merge STANDS (the commits are on the target); only its final status did not publish",
			repo, req.Workspace, req.Run.RunID(), err)
	}
	return afterActionErr
}

// runAfterAction resolves, publishes and DELIVERS the workspace's postprocessing
// action, and reports its failure as the text the terminal status carries ("" when
// there was nothing to do or the turn ended cleanly).
//
// A WORKSPACE WITH NO ACTION IS THE COMMON CASE and publishes no phase at all:
// an empty after_action arm would tell every frontend the run was executing a
// turn that does not exist. A lookup FAILURE still publishes the phase — the
// action may well be configured and the daemon merely unable to read it, so
// suppressing it there would downgrade a broken read to "this workspace has
// none" — and the read failure is carried onto the merged status rather than
// swallowed.
func (c *QueueCoordinator) runAfterAction(repo string, req Request) string {
	prompt, err := c.postMerge.AfterAction(req)
	if err != nil {
		c.logf("merge: after-action prompt lookup FAILED {repo=%s ws=%s name=%s run=%s}: %v — the phase is published without its text; the merge STANDS",
			repo, req.Workspace, req.Name, req.Run.RunID(), err)
		// Published without text rather than with invented text.
		c.publishAfterActionPhase(repo, req, "")
		return "the workspace's after-merge action could not be read: " + err.Error()
	}
	if prompt == "" {
		c.logf("merge: no after-action recorded {repo=%s ws=%s name=%s run=%s} — no after_action phase is published",
			repo, req.Workspace, req.Name, req.Run.RunID())
		return ""
	}

	act := AfterAction{
		Workspace: req.Workspace,
		RequestID: newAfterActionRequestID(),
		Prompt:    prompt,
	}
	if err := act.validate(); err != nil {
		c.logf("merge: after-action NOT HANDED to the shim {repo=%s ws=%s name=%s}: %v — the merge STANDS", repo, req.Workspace, req.Name, err)
		c.publishAfterActionPhase(repo, req, prompt)
		return "the workspace's after-merge action is not deliverable: " + err.Error()
	}
	c.publishAfterActionPhase(repo, req, prompt)

	c.logf("merge: after-action HANDED TO THE SHIM {repo=%s ws=%s name=%s run=%s request_id=%s}",
		repo, req.Workspace, req.Name, req.Run.RunID(), act.RequestID)
	if err := c.afterActionFn.Run(c.ctx, act); err != nil {
		c.logf("merge: after-action FAILED {repo=%s ws=%s name=%s run=%s request_id=%s}: %v — the merge STANDS (the commits are on the target); the failure rides on the terminal merged status",
			repo, req.Workspace, req.Name, req.Run.RunID(), act.RequestID, err)
		return "the workspace's after-merge action did not complete: " + err.Error()
	}
	c.logf("merge: after-action TURN COMPLETE {repo=%s ws=%s name=%s run=%s request_id=%s}",
		repo, req.Workspace, req.Name, req.Run.RunID(), act.RequestID)
	return ""
}

// publishAfterActionPhase puts the after_action arm on the wire. A publication
// failure is loud-logged and never fatal: the commits are already on the target.
func (c *QueueCoordinator) publishAfterActionPhase(repo string, req Request, prompt string) {
	if err := req.Run.AfterAction(prompt, "running the workspace's after-merge action"); err != nil {
		c.logf("merge: after-action status FAILED {repo=%s ws=%s run=%s}: %v", repo, req.Workspace, req.Run.RunID(), err)
	}
}

// finishTerminal retires a merge that reached a terminal OUTCOME and fires the
// post-merge hook when that outcome is `merged`.
//
// The hook fires strictly after finish, which is what gives merge.PostMergeHook
// its "queue entry dropped, lease released" precondition: the hook prompts the
// parent workspace's session, and doing that under a lease the merge still held
// would have the prompt refused by the very exclusivity the merge took.
//
// A merge whose queue entry could NOT be dropped does not fire the hook. That
// entry is replayed by the next boot's Drain, so notifying now would phone the
// parent home twice for one child.
func (c *QueueCoordinator) finishTerminal(repo string, req, driven Request, release func(), outcome Outcome, afterActionErr string) bool {
	ok := c.finish(repo, req, release)
	if outcome != OutcomeMerged {
		return ok
	}
	if !ok {
		c.logf("merge: post-merge hook NOT RUN, the durable entry could not be dropped {repo=%s ws=%s name=%s} — the next boot's Drain replays this merge, and notifying now would hand the parent the same child twice",
			repo, req.Workspace, req.Name)
		return ok
	}
	c.firePostMerge(repo, driven, afterActionErr)
	return ok
}

// firePostMerge runs the post-merge hook on its OWN goroutine.
//
// OFF THE DRAIN PATH IS THE POINT. The hook talks to another workspace's agent
// session, which can be slow, unreachable, or mid-turn. Running it inline would
// make every later merge on this repository wait behind a courtesy notification
// — a queue stalled by a nicety. The goroutine is tracked on c.wg and bounded
// by c.ctx, so Close still waits for a hook that honors cancellation rather
// than leaking it.
func (c *QueueCoordinator) firePostMerge(repo string, req Request, afterActionErr string) {
	c.wg.Add(1)
	go func() {
		defer c.wg.Done()
		c.publishPostMerge(repo, req, afterActionErr)
	}()
}

// publishPostMerge runs the child-to-parent handoff and republishes the terminal
// merged status when it fails.
//
// THE HOOK CANNOT FAIL THE RUN either: the commits are on the target, and
// recording a merge as failed because its notification did not land would make
// the pushed state lie about the tree. Its error is retained as a
// merge.PostMergeFailure and carried onto the merged status BESIDE the
// after-action's, never instead of it.
func (c *QueueCoordinator) publishPostMerge(repo string, req Request, afterActionErr string) {
	c.logf("merge: post-merge hook RUNNING {repo=%s ws=%s name=%s run=%s target=%s}", repo, req.Workspace, req.Name, req.Run.RunID(), req.TargetDir)
	hookErr := c.postMerge.AfterMerged(c.ctx, req)
	if hookErr == nil {
		c.logf("merge: post-merge hook COMPLETE {repo=%s ws=%s name=%s run=%s}", repo, req.Workspace, req.Name, req.Run.RunID())
		return
	}
	c.recordPostMergeFailure(repo, req, hookErr)

	// The merged FACT is set-once and does not move; only the error text on it
	// grows. Replacing afterActionErr instead of joining it would drop the
	// action's failure off the very status a frontend reads it from.
	combined := hookErr.Error()
	if afterActionErr != "" {
		combined = afterActionErr + "; " + combined
	}
	if err := req.Run.Merged(combined, "the merge landed"); err != nil {
		c.logf("merge: terminal merged status FAILED {repo=%s ws=%s run=%s}: %v — the merge STANDS (the commits are on the target); only its final status did not publish",
			repo, req.Workspace, req.Run.RunID(), err)
	}
}

// recordPostMergeFailure loud-logs a hook error and retains it.
//
// It deliberately does NOT emit merge_failed. The commits are on the target
// worktree; a failed handoff does not put them back, and recording a merge as
// failed because its notification did not land would make the pushed state lie
// about the tree.
func (c *QueueCoordinator) recordPostMergeFailure(repo string, req Request, err error) {
	c.logf("merge: post-merge hook FAILED {repo=%s ws=%s name=%s target=%s}: %v — the merge STANDS (the commits are on the target); only the post-merge handoff did not land",
		repo, req.Workspace, req.Name, req.TargetDir, err)
	c.postMergeMu.Lock()
	defer c.postMergeMu.Unlock()
	c.postMergeFailures = append(c.postMergeFailures, PostMergeFailure{
		Repo: repo, Workspace: req.Workspace, Name: req.Name, At: time.Now(), Err: err,
	})
	if len(c.postMergeFailures) > maxRetainedPostMergeFailures {
		evicted := c.postMergeFailures[0]
		c.postMergeFailures = c.postMergeFailures[1:]
		c.logf("merge: post-merge failure record EVICTED the oldest entry {repo=%s ws=%s name=%s}: %v — the retained record is capped at %d; the canonical log still carries every failure",
			evicted.Repo, evicted.Workspace, evicted.Name, evicted.Err, maxRetainedPostMergeFailures)
	}
}

// PostMergeFailures returns a copy of the retained hook-failure record, oldest
// first. It is the in-process view of merges that landed whose post-merge
// handoff did not.
func (c *QueueCoordinator) PostMergeFailures() []PostMergeFailure {
	c.postMergeMu.Lock()
	defer c.postMergeMu.Unlock()
	out := make([]PostMergeFailure, len(c.postMergeFailures))
	copy(out, c.postMergeFailures)
	return out
}

// runOne drives one request end to end and reports whether draining continues.
// False means this repository's drain stops with its entry left durable, for
// the next boot's Drain to pick up.
func (c *QueueCoordinator) runOne(repo string, req Request) bool {
	// The resume rendezvous is published before anything else happens, so a
	// conflict_resolved_continue that arrives while the cherry-pick is still
	// running waits for the conflict rather than racing the park that will
	// accept it.
	park := &conflictPark{req: req, calls: make(chan resumeCall), abandons: make(chan abandonCall), ended: make(chan struct{})}
	c.mu.Lock()
	c.parks[repo] = park
	c.mu.Unlock()
	defer func() {
		c.mu.Lock()
		delete(c.parks, repo)
		c.mu.Unlock()
		close(park.ended)
	}()

	// The run was minted at Enqueue and rides the in-memory request, so the
	// `enqueued` phase a user already saw and everything that follows carry ONE
	// id. An entry replayed from disk by a boot Drain has no PUBLISHER — that died
	// with the process executing it — but it does carry the run's ID, so the
	// publisher rebuilt here resumes the same run: a bounce interrupts a merge, it
	// does not start a different one.
	driven := req
	if driven.Run == nil {
		run, err := c.rebuildRun(repo, req)
		if err != nil {
			c.logf("merge: run status construction FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
			c.failed(req, "merge run could not be published: "+err.Error())
			return c.finish(repo, req, nil)
		}
		// `req` is left EXACTLY as the queue delivered it: FileQueue.Complete
		// identifies the head by struct equality, so the request the drain loop
		// retires must be the one it received. The run rides a copy.
		driven.Run = run
	}
	c.logf("merge: run START {repo=%s ws=%s name=%s run=%s}", repo, req.Workspace, req.Name, driven.Run.RunID())

	// DELETION FIRST, BRING-UP SECOND. A hibernated session and a deleted one
	// are indistinguishable to EnsureLive — neither is live — and they resolve
	// in OPPOSITE directions. Asking afterwards would mean asking about a
	// session the bring-up had already resurrected.
	if !c.refuseDeletedSession(repo, req, driven) {
		return c.finish(repo, req, nil)
	}

	// SESSION FIRST, LEASE SECOND. The merge drives this workspace's own session
	// (the before-action, a conflict resolution, a test fix), and a bring-up can
	// be slow — holding the exclusivity lease across it would lock the user out
	// of their session for the whole of it.
	if !c.bringUpSession(repo, req, driven) {
		return c.finish(repo, req, nil)
	}

	release, err := c.lease.Acquire(c.ctx, req.Workspace)
	if err != nil {
		c.logf("merge: lease acquire FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		c.failedRun(driven, "shim lease unavailable: "+err.Error())
		// A merge that can never run must not hold the head forever.
		return c.finish(repo, req, nil)
	}
	if release == nil {
		// A successful Acquire that returns no release func would leak the
		// lease forever. That is a merge.Lease implementation bug, and there is
		// nothing sane to do with the shim it just claimed.
		panic(fmt.Sprintf("merge: Lease.Acquire returned a nil release for workspace %q", req.Workspace))
	}

	// THE BEFORE-ACTION RUNS UNDER THE LEASE AND BEFORE THE PLAN. It may create
	// commits, so computing the cherry-pick plan first would replay a range that
	// does not include the work the action was asked to produce. The plan is
	// computed inside picker.Merge, which is why this sits above it.
	if !c.runBeforeAction(repo, driven) {
		return c.finish(repo, req, release)
	}

	res, err := c.picker.Merge(c.ctx, driven)
	if err != nil {
		// merge.Driver rejects a bad precondition BEFORE emitting anything, so
		// without this the workspace would sit at merge_queued with no terminal
		// state ever arriving. The coordinator owns the queued state, so it
		// owns clearing it.
		c.logf("merge: driver FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		c.failedRun(driven, "merge driver failed: "+err.Error())
		return c.finish(repo, req, release)
	}

	return c.settle(repo, req, driven, park, release, res)
}

// rebuildRun makes a publisher for an entry the queue delivered without one:
// a boot replay, whose RunStatus died with the process that admitted it.
//
// THE ID IS RESUMED WHENEVER THE ENTRY CARRIES ONE, which is every entry this
// daemon's Enqueue wrote. An entry with no id is one written before the durable
// record carried it; it is loud-logged and replayed under a fresh id, because a
// run with no name at all is one no frontend can follow.
func (c *QueueCoordinator) rebuildRun(repo string, req Request) (*RunStatus, error) {
	if req.RunID == "" {
		c.logf("merge: replayed entry carries NO run id {repo=%s ws=%s name=%s} — the run is renamed, so anything watching the admission loses track of it",
			repo, req.Workspace, req.Name)
		run, err := NewRunStatus(c.status, c.logf, req.Workspace, c.now)
		if err != nil {
			return nil, err
		}
		c.logf("merge: run MINTED for an unnamed replayed entry {repo=%s ws=%s name=%s run=%s}", repo, req.Workspace, req.Name, run.RunID())
		return run, nil
	}
	run, err := ResumeRunStatus(c.status, c.logf, req.Workspace, c.now, req.RunID)
	if err != nil {
		return nil, err
	}
	c.logf("merge: run RESUMED for a replayed entry {repo=%s ws=%s name=%s run=%s}", repo, req.Workspace, req.Name, run.RunID())
	return run, nil
}

// refuseDeletedSession fails the run when the workspace's newest session was
// DELETED, and reports whether the merge may continue.
//
// THE CAUSE NAMES THE DELETION, and that wording is the contract rather than a
// nicety: every terminal cause reaches a user, and "the merge failed" is
// unactionable where "the session you deleted was the one this merge needed" is
// a next step. A generic cause here is the defect.
//
// A LOOKUP FAILURE ALSO FAILS THE RUN. "The records could not be read" and "the
// session is fine" differ by exactly the resurrection this refuses, so the
// unreadable case must not proceed into a bring-up.
func (c *QueueCoordinator) refuseDeletedSession(repo string, req, driven Request) bool {
	sessionID, deleted, err := c.deaths.DeletedSession(req.Workspace)
	if err != nil {
		c.logf("merge: session-deletion lookup FAILED {repo=%s ws=%s name=%s run=%s}: %v — the merge is failed rather than brought up, because a session that was deleted must not be resurrected to run a merge action",
			repo, req.Workspace, req.Name, driven.Run.RunID(), err)
		c.failedRun(driven, "the workspace's session records could not be read: "+err.Error())
		return false
	}
	if !deleted {
		return true
	}
	c.logf("merge: session DELETED, merge REFUSED {repo=%s ws=%s name=%s run=%s session=%s} — bringing it back to run a merge action would resurrect exactly what the user destroyed",
		repo, req.Workspace, req.Name, driven.Run.RunID(), sessionID)
	c.failedRun(driven, fmt.Sprintf("the agent session this merge needed was DELETED (session %s), and a deleted session is not brought back to run a merge action", sessionID))
	return false
}

// bringUpSession makes the workspace's session driveable and reports whether
// the merge may continue.
//
// THREE OUTCOMES, NOT TWO:
//
//   - a session came up (or was already up), and the run proceeds with it;
//   - the workspace has NO session record, and the run proceeds SESSIONLESS.
//     A plain worktree nobody ever opened a session on is mergeable — the
//     cherry-pick, the test gate and the rollback are git — so failing here
//     would make "was this ever opened in the editor?" a precondition of
//     merging. Every step that genuinely needs a session fails loudly on its
//     own path, which is the only place that knows whether the absence matters;
//   - a bring-up for a workspace that HAS a session failed, and the run FAILS.
//     That stays loud and terminal: the merge is about to submit turns into
//     that session, and there is no session to submit them to.
func (c *QueueCoordinator) bringUpSession(repo string, req, driven Request) bool {
	err := c.sessions.EnsureLive(c.ctx, req.Workspace)
	switch {
	case err == nil:
		return true
	case errors.Is(err, ErrNoSession):
		c.logf("merge: no session recorded {repo=%s ws=%s name=%s run=%s}: %v — the merge proceeds SESSIONLESS; a before-action, a conflict resolution or a test fix would each fail loudly on its own path, and this merge asks for none of them unless it does",
			repo, req.Workspace, req.Name, driven.Run.RunID(), err)
		return true
	default:
		c.logf("merge: session bring-up FAILED {repo=%s ws=%s name=%s run=%s}: %v — the merge cannot drive a session it cannot reach",
			repo, req.Workspace, req.Name, driven.Run.RunID(), err)
		c.failedRun(driven, "the workspace's agent session could not be brought up: "+err.Error())
		return false
	}
}

// runBeforeAction resolves and delivers the workspace's before_ws_merge action,
// reporting whether the merge may continue.
//
// A WORKSPACE WITH NO ACTION IS THE COMMON CASE and passes straight through. An
// action that cannot be READ, and an action whose turn does not end cleanly, both
// FAIL the run: the whole point of the action is to change what gets
// cherry-picked, so proceeding after it did not happen would land a plan the
// user did not ask for.
func (c *QueueCoordinator) runBeforeAction(repo string, req Request) bool {
	prompt, err := c.beforeActions.BeforeAction(req.Workspace)
	if err != nil {
		c.logf("merge: before-action lookup FAILED {repo=%s ws=%s name=%s run=%s}: %v — the merge is failed rather than run without an action the workspace may have been created with",
			repo, req.Workspace, req.Name, req.Run.RunID(), err)
		c.failedRun(req, "the workspace's before-merge action could not be read: "+err.Error())
		return false
	}
	if prompt == "" {
		c.logf("merge: no before-action recorded {repo=%s ws=%s name=%s run=%s}", repo, req.Workspace, req.Name, req.Run.RunID())
		return true
	}

	act := BeforeAction{
		Workspace: req.Workspace,
		RequestID: newBeforeActionRequestID(),
		Prompt:    prompt,
	}
	if err := act.validate(); err != nil {
		c.logf("merge: before-action NOT HANDED to the shim {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		c.failedRun(req, "the workspace's before-merge action is not deliverable: "+err.Error())
		return false
	}
	if err := req.Run.BeforeAction(prompt, "running the workspace's before-merge action"); err != nil {
		c.logf("merge: before-action status FAILED {repo=%s ws=%s run=%s}: %v", repo, req.Workspace, req.Run.RunID(), err)
		c.failedRun(req, "the before-merge action's phase could not be published: "+err.Error())
		return false
	}

	c.logf("merge: before-action HANDED TO THE SHIM {repo=%s ws=%s name=%s run=%s request_id=%s}",
		repo, req.Workspace, req.Name, req.Run.RunID(), act.RequestID)
	if err := c.beforeActionFn.Run(c.ctx, act); err != nil {
		c.logf("merge: before-action FAILED {repo=%s ws=%s name=%s run=%s request_id=%s}: %v — the merge is failed; cherry-picking a plan the action was meant to change would land the wrong commits",
			repo, req.Workspace, req.Name, req.Run.RunID(), act.RequestID, err)
		c.failedRun(req, "the workspace's before-merge action did not complete: "+err.Error())
		return false
	}
	c.logf("merge: before-action TURN COMPLETE {repo=%s ws=%s name=%s run=%s request_id=%s} — computing the cherry-pick plan now",
		repo, req.Workspace, req.Name, req.Run.RunID(), act.RequestID)
	return true
}

// settle drives a merge Result to a terminal outcome and reports whether
// draining continues.
//
// IT IS A LOOP BECAUSE THE TWO PARKING OUTCOMES FEED EACH OTHER. A conflict
// resolved by a resume continues the per-commit replay, which can break the
// suite; a test fix continues the same replay, which can conflict on a later
// commit. Expressing that as one loop over a Result — rather than as recursion
// between the conflict path and the test path — is what keeps "how did this
// merge end" answerable in one place.
//
// PreMergeHead is read ONCE, from the Result of the original Merge, and carried
// across every later step. Resume and ContinueAfterTestFix each start from a
// target that already carries landed commits, so their own view of HEAD is not
// a rollback point.
func (c *QueueCoordinator) settle(repo string, req, driven Request, park *conflictPark, release func(), res Result) bool {
	preHead := res.PreMergeHead
	// attempted records which failing commits have already had their ONE
	// resolution attempt, so a suite that fails again on the same commit fails
	// the merge instead of re-prompting an agent that already tried.
	attempted := map[string]bool{}
	for {
		switch res.Outcome {
		case OutcomeConflict:
			c.logf("merge: parked on conflict {repo=%s ws=%s name=%s commit=%s}", repo, req.Workspace, req.Name, res.ConflictCommit)
			next, handled, cont := c.awaitResolution(repo, req, driven, park, release, res.ConflictCommit)
			if handled {
				return cont
			}
			res = next
		case OutcomeTestFailed:
			c.logf("merge: test gate failed {repo=%s ws=%s name=%s commit=%s}", repo, req.Workspace, req.Name, res.FailingCommit)
			next, handled, cont := c.awaitTestFix(repo, req, driven, park, release, preHead, res, attempted)
			if handled {
				return cont
			}
			res = next
		default:
			c.logf("merge: terminal {repo=%s ws=%s name=%s run=%s outcome=%s}", repo, req.Workspace, req.Name, driven.Run.RunID(), res.Outcome)
			// THE AFTER-ACTION RUNS BEFORE THE MERGE IS RETIRED, because it is a
			// turn in this workspace's own session and the lease that admits it is
			// still held here. finishTerminal releases that lease.
			var afterActionErr string
			if res.Outcome == OutcomeMerged {
				afterActionErr = c.completeMergedRun(repo, driven, res)
			}
			return c.finishTerminal(repo, req, driven, release, res.Outcome, afterActionErr)
		}
	}
}

// awaitResolution serves park's resume calls until one drives the merge to a
// terminal outcome. The lease and the queue head are both held throughout,
// because a target worktree with a paused cherry-pick can host no other merge.
//
// The FIRST resolution attempt is the workspace's own shim (driveShimResolution
// below), and it reaches this loop through the very same park.calls rendezvous a
// human's conflict_resolved_continue uses. That is what serializes the two: a
// human Resume or an Abandon arriving mid-attempt is not racing the shim, it is
// queueing behind (or ahead of) it at this one select.
// It returns (next, handled, cont). handled=true means the merge was RETIRED
// inside this call (abandoned, or the coordinator shut down) and cont is the
// drain verdict. handled=false means the resume produced a non-conflict Result
// that settle must take from here.
//
// The resume caller is replied to as soon as the resume is CLASSIFIED, before
// settle finishes the merge. That is deliberate: everything settle may still do
// (a suite run, a resolution turn, more picks) is reported through the pushed
// merge state, and blocking a frontend command on it would make
// conflict_resolved_continue hang for the length of a test suite.
func (c *QueueCoordinator) awaitResolution(repo string, req, driven Request, park *conflictPark, release func(), conflictCommit string) (Result, bool, bool) {
	c.driveShimResolution(repo, req, park, conflictCommit)
	for {
		select {
		case <-c.ctx.Done():
			// Shutdown with the conflict still in the tree. The lease goes
			// back (it must never outlive its holder), and the entry stays
			// durable so the next daemon's Drain re-parks it.
			c.logf("merge: conflict park abandoned on shutdown {repo=%s ws=%s}", repo, req.Workspace)
			release()
			return Result{}, true, false
		case call := <-park.abandons:
			// The user closed the workspace this conflict belongs to. The
			// conflicted cherry-pick stays in the target tree for a human to
			// finish or abort by hand; the lease and the queue head do not.
			c.logf("merge: conflict park ABANDONED {repo=%s ws=%s name=%s} — the conflicted cherry-pick is left in the target tree for hand cleanup", repo, req.Workspace, req.Name)
			ok := c.finish(repo, req, release)
			call.reply <- nil
			return Result{}, true, ok
		case call := <-park.calls:
			// The resume carries the RUN this merge has been publishing all
			// along. A human's conflict_resolved_continue arrives with none —
			// the frontend has no run to name — and continuing a merge under a
			// second run id would restart the progress a user is watching.
			resumeReq := call.req
			resumeReq.Run = driven.Run
			res, err := c.picker.Resume(c.ctx, resumeReq)
			if err != nil {
				c.logf("merge: resume driver FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
				// The conflict is still in the tree, so the park survives and
				// the caller is told why their resume did not take.
				call.reply <- fmt.Errorf("merge: resume %q: %w", call.req.Name, err)
				continue
			}
			if res.Outcome == OutcomeConflict {
				c.logf("merge: still conflicted after resume {repo=%s ws=%s commit=%s}", repo, req.Workspace, res.ConflictCommit)
				call.reply <- nil
				continue
			}
			c.logf("merge: resume classified {repo=%s ws=%s outcome=%s}", repo, req.Workspace, res.Outcome)
			call.reply <- nil
			return res, false, false
		}
	}
}

// awaitTestFix drives the ONE resolution attempt a broken test suite gets, and
// reports what the merge should do next.
//
// EXACTLY ONE ATTEMPT PER FAILING COMMIT. A second identical prompt to an agent
// that already failed to fix the suite is a spin, not a strategy, so a repeat
// failure on the same commit goes straight to the rollback path. A failure on a
// LATER commit is a different failure and gets its own attempt.
//
// THE ATTEMPT RUNS ON ITS OWN GOROUTINE even though nothing else needs to serve
// the park meanwhile. That is what keeps an Abandon responsive: closing the
// workspace while its agent is fixing tests must not block until the fix and
// the re-run finish. The attempt's context is cancelled on that abandon, so the
// cancelled fix cannot go on cherry-picking into a target the merge has already
// given up.
func (c *QueueCoordinator) awaitTestFix(repo string, req, driven Request, park *conflictPark, release func(), preHead string, res Result, attempted map[string]bool) (Result, bool, bool) {
	if attempted[res.FailingCommit] {
		c.logf("merge: test failure NOT RE-ATTEMPTED {repo=%s ws=%s name=%s commit=%s} — its one resolution attempt already ran and the suite still fails",
			repo, req.Workspace, req.Name, res.FailingCommit)
		return Result{}, true, c.failTestGate(repo, req, driven, release, preHead, res)
	}
	attempted[res.FailingCommit] = true

	fix := TestFailureResolution{
		Workspace:     req.Workspace,
		RequestID:     newTestFixRequestID(),
		FailingCommit: res.FailingCommit,
		SourceBranch:  req.SourceBranch,
		TargetDir:     req.TargetDir,
		FailureTail:   res.TestFailureTail,
	}
	if err := fix.validate(); err != nil {
		// The facts the driver reported do not describe a fixable failure,
		// which no valid Result produces. Prompting an agent with a half-empty
		// instruction is worse than failing the merge with the reason.
		c.logf("merge: test failure NOT HANDED to the shim {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		return Result{}, true, c.failTestGate(repo, req, driven, release, preHead, res)
	}

	fixCtx, cancelFix := context.WithCancel(c.ctx)
	defer cancelFix()
	done := make(chan pickResultOrError, 1)
	c.wg.Add(1)
	go func() {
		defer c.wg.Done()
		done <- c.driveTestFix(fixCtx, repo, driven, fix)
	}()

	select {
	case out := <-done:
		if out.err != nil {
			return Result{}, true, c.failTestGate(repo, req, driven, release, preHead, res)
		}
		return out.res, false, false
	case call := <-park.abandons:
		c.logf("merge: test-fix attempt ABANDONED {repo=%s ws=%s name=%s commit=%s} — the workspace was closed mid-fix; the target keeps what landed",
			repo, req.Workspace, req.Name, res.FailingCommit)
		cancelFix()
		<-done
		ok := c.finish(repo, req, release)
		call.reply <- nil
		return Result{}, true, ok
	case <-c.ctx.Done():
		c.logf("merge: test-fix attempt abandoned on shutdown {repo=%s ws=%s name=%s}", repo, req.Workspace, req.Name)
		<-done
		release()
		return Result{}, true, false
	}
}

// pickResultOrError is the test-fix goroutine's return channel payload.
type pickResultOrError struct {
	res Result
	err error
}

// driveTestFix runs the resolution turn and then continues the replay. Every
// failure inside it is loud-logged and returned as an error, which settle turns
// into the rollback path.
func (c *QueueCoordinator) driveTestFix(ctx context.Context, repo string, req Request, fix TestFailureResolution) pickResultOrError {
	c.logf("merge: test failure HANDED TO THE SHIM {repo=%s ws=%s name=%s commit=%s branch=%s target=%s request_id=%s}",
		repo, req.Workspace, req.Name, fix.FailingCommit, fix.SourceBranch, fix.TargetDir, fix.RequestID)
	if err := c.testResolver.Resolve(ctx, fix); err != nil {
		c.logf("merge: shim test-failure resolution FAILED {repo=%s ws=%s name=%s commit=%s request_id=%s}: %v — the merge is failed and the target rolled back",
			repo, req.Workspace, req.Name, fix.FailingCommit, fix.RequestID, err)
		return pickResultOrError{err: err}
	}
	c.logf("merge: shim test-failure resolution TURN COMPLETE {repo=%s ws=%s name=%s commit=%s request_id=%s} — committing the fix and re-running the suite",
		repo, req.Workspace, req.Name, fix.FailingCommit, fix.RequestID)
	res, err := c.picker.ContinueAfterTestFix(ctx, req, fix.FailingCommit)
	if err != nil {
		c.logf("merge: continue-after-test-fix driver FAILED {repo=%s ws=%s name=%s commit=%s}: %v",
			repo, req.Workspace, req.Name, fix.FailingCommit, err)
		return pickResultOrError{err: err}
	}
	return pickResultOrError{res: res}
}

// failTestGate is the terminal tail of a merge whose test gate failed for good:
// roll the target back to its pre-merge HEAD, record merge_failed with the
// failing commit and the suite's output tail, and retire the entry.
//
// THE ROLLBACK IS THE POINT. The target worktree is the tree every other
// workspace cuts from, so leaving it carrying commits that break its suite
// turns one workspace's failure into everyone's. Nothing is lost: the source
// branch still holds every commit, and the merge can be retried once the work is
// fixed there.
func (c *QueueCoordinator) failTestGate(repo string, req, driven Request, release func(), preHead string, res Result) bool {
	cause := fmt.Sprintf("test suite failed after cherry-picking %s and the one resolution attempt did not fix it: %s",
		res.FailingCommit, dlog.Clamp(res.TestFailureTail, testFailureCauseBytes))
	switch {
	case preHead == "":
		// No rollback point was recorded, which no merge.Driver Merge omits.
		// The merge still fails, and the missing point is named rather than
		// papered over — a target left carrying broken commits is exactly what
		// a user must be told about.
		c.logf("merge: test-gate rollback IMPOSSIBLE {repo=%s ws=%s name=%s} — no pre-merge HEAD was recorded, so the target KEEPS the commits that failed the suite",
			repo, req.Workspace, req.Name)
		cause += " (NOT rolled back: no pre-merge HEAD was recorded)"
	default:
		if err := c.picker.Rollback(c.ctx, req, preHead); err != nil {
			c.logf("merge: test-gate rollback FAILED {repo=%s ws=%s name=%s head=%s}: %v — the target is LEFT carrying the commits that failed the suite",
				repo, req.Workspace, req.Name, preHead, err)
			cause += fmt.Sprintf(" (rollback to %s FAILED: %v)", preHead, err)
		} else {
			c.logf("merge: test-gate rollback COMPLETE {repo=%s ws=%s name=%s head=%s} — the target is back where the merge found it; the workspace branch still holds all the work",
				repo, req.Workspace, req.Name, preHead)
			cause += fmt.Sprintf(" (target rolled back to %s)", preHead)
		}
	}
	c.failedRun(driven, cause)
	return c.finish(repo, req, release)
}

// testFailureCauseBytes bounds how much of the suite's output tail travels into
// the merge_failed cause the frontends render.
const testFailureCauseBytes = 1200

// driveShimResolution hands the parked conflict to the merging workspace's OWN
// agent session, EXACTLY ONCE, and then asks the park to resume the pick.
//
// WHY THE WORKSPACE'S OWN SHIM. The session that produced the conflicting
// commits is the one party that already holds their context, and the
// coordinator is holding that session's lease for the duration of this merge
// precisely so it can drive it. Parking on a human while that session sits idle
// under a lease taken to use it is the state this exists to end.
//
// EXACTLY ONE ATTEMPT. A resume that is still conflicted, an attempt that
// errors, and a submit that is refused all end the same way: loud logging, no
// state claimed, and the park LEFT STANDING for the human path
// (conflict_resolved_continue, or abandonment by closing the workspace). The
// coordinator does not re-prompt an agent that already failed to resolve the
// conflict — a second identical attempt is a spin, not a strategy.
//
// It runs on its own goroutine because the drain goroutine is the one that
// serves park.calls: driving the resolution inline would mean waiting for a
// resolution turn from inside the loop that has to accept its resume.
func (c *QueueCoordinator) driveShimResolution(repo string, req Request, park *conflictPark, conflictCommit string) {
	res := ConflictResolution{
		Workspace:      req.Workspace,
		RequestID:      newResolutionRequestID(),
		ConflictCommit: conflictCommit,
		SourceBranch:   req.SourceBranch,
		TargetDir:      req.TargetDir,
	}
	if err := res.validate(); err != nil {
		// The conflict facts the coordinator was handed do not describe a
		// resolvable conflict. That is merge.Driver reporting a conflict without
		// naming its commit, which no valid Result does, so it is loud and the
		// park stands for the human rather than prompting an agent with a
		// half-empty instruction.
		c.logf("merge: conflict resolution NOT HANDED to the shim {repo=%s ws=%s name=%s}: %v — the conflict stays parked for a human (conflict_resolved_continue or workspace close)",
			repo, req.Workspace, req.Name, err)
		return
	}
	c.wg.Add(1)
	go func() {
		defer c.wg.Done()
		c.logf("merge: conflict resolution HANDED TO THE SHIM {repo=%s ws=%s name=%s commit=%s branch=%s target=%s request_id=%s}",
			repo, req.Workspace, req.Name, res.ConflictCommit, res.SourceBranch, res.TargetDir, res.RequestID)
		if err := c.resolver.Resolve(c.ctx, res); err != nil {
			c.logf("merge: shim conflict resolution FAILED {repo=%s ws=%s name=%s commit=%s request_id=%s}: %v — NOTHING was resumed and nothing is marked merged; the conflict stays parked for a human (conflict_resolved_continue or workspace close)",
				repo, req.Workspace, req.Name, res.ConflictCommit, res.RequestID, err)
			return
		}
		c.logf("merge: shim conflict resolution TURN COMPLETE {repo=%s ws=%s name=%s commit=%s request_id=%s} — resuming the cherry-pick",
			repo, req.Workspace, req.Name, res.ConflictCommit, res.RequestID)

		reply := make(chan error, 1)
		select {
		case park.calls <- resumeCall{req: req, reply: reply}:
		case <-park.ended:
			// A human resume or an abandon reached the park first and retired the
			// merge while the resolution turn ran. Nothing to continue, and
			// nothing lost.
			c.logf("merge: shim resolution resume DROPPED, the merge already ended {repo=%s ws=%s name=%s}", repo, req.Workspace, req.Name)
			return
		case <-c.ctx.Done():
			c.logf("merge: shim resolution resume DROPPED, coordinator shutting down {repo=%s ws=%s name=%s}", repo, req.Workspace, req.Name)
			return
		}
		select {
		case err := <-reply:
			if err != nil {
				c.logf("merge: shim resolution resume REFUSED {repo=%s ws=%s name=%s}: %v — the conflict stays parked for a human",
					repo, req.Workspace, req.Name, err)
			}
		case <-park.ended:
		case <-c.ctx.Done():
		}
	}()
}

// failed records the merge_failed the coordinator itself owns. A sink failure
// here is loud-logged: the transition it was meant to record is already lost,
// and there is no second channel to try.
func (c *QueueCoordinator) failed(req Request, cause string) {
	if err := c.emit.emit(req.Workspace, PhaseMergeFailed, cause); err != nil {
		c.logf("merge: merge_failed record FAILED {ws=%s name=%s cause=%s}: %v", req.Workspace, req.Name, cause, err)
	}
}

// failedRun records merge_failed for a request that HAS a run, so the terminal
// status carries the run's identity and the commit context it died with.
//
// It is separate from failed() because the coordinator also fails requests that
// never reached a run (the boot sweep of orphaned merge_enqueuing marks), and a
// fabricated run id on those would let a frontend correlate two unrelated things.
func (c *QueueCoordinator) failedRun(req Request, cause string) {
	if err := req.Run.Failed(cause); err != nil {
		c.logf("merge: merge_failed status FAILED {ws=%s name=%s run=%s cause=%s}: %v",
			req.Workspace, req.Name, req.Run.RunID(), cause, err)
	}
}

// gate returns repo's advance gate, the mutex that makes "publish observed
// index > 1" and "merge_queued was recorded" atomic with respect to the drain
// loop's Complete. It is held only across those short operations, never across
// a cherry-pick.
func (c *QueueCoordinator) gate(repo string) *sync.Mutex {
	c.mu.Lock()
	defer c.mu.Unlock()
	g, ok := c.gates[repo]
	if !ok {
		g = &sync.Mutex{}
		c.gates[repo] = g
	}
	return g
}

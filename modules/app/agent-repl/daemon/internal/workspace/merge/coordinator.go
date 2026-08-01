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
	// PostMerge receives every `merged` terminal outcome once the queue entry
	// is dropped and the lease released. Required, on the same footing as the
	// rest: it carries the child-to-parent handoffs (the merge phone-home and
	// the workspace's postprocessing prompt), and a coordinator without it
	// silently drops them exactly the way the deleted Emacs handler did.
	PostMerge PostMergeHook
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
	logf      dlog.Logf
	emit      *stateEmitter
	queue     DurableQueue
	phases    PhaseSource
	keyer     RepoKeyer
	picker    Picker
	lease     Lease
	resolver  ConflictResolver
	postMerge PostMergeHook

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
	case cfg.PostMerge == nil:
		return nil, fmt.Errorf("merge: Coordinator PostMerge hook is required")
	}
	ctx, stop := context.WithCancel(context.Background())
	return &QueueCoordinator{
		logf:      cfg.Logf,
		emit:      &stateEmitter{sink: cfg.Sink, logf: cfg.Logf},
		queue:     cfg.Queue,
		phases:    cfg.Phases,
		keyer:     cfg.Keyer,
		picker:    cfg.Picker,
		lease:     cfg.Lease,
		resolver:  cfg.Resolver,
		postMerge: cfg.PostMerge,

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
// merge_queued is emitted for any request that does not land at the head, and
// it is emitted UNDER THE REPOSITORY'S ADVANCE GATE. That gate is also held
// across the drain loop's Complete, so the head cannot finish (and this entry
// cannot start merging) between the publish that observed index > 1 and the
// merge_queued that explains it. The ordering is structural, not a hoped-for
// window.
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

	gate := c.gate(repo)
	gate.Lock()
	pos, err := c.queue.Publish(ctx, repo, req)
	if err != nil {
		gate.Unlock()
		c.logf("merge: enqueue publish FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		return Position{}, fmt.Errorf("merge: enqueue %q: %w", req.Name, err)
	}
	var queuedErr error
	if pos.Index > 1 {
		cause := fmt.Sprintf("queued at position %d of %d behind another merge on repo %s", pos.Index, pos.Depth, repo)
		if err := c.picker.MarkQueued(req.Workspace, cause); err != nil {
			c.logf("merge: enqueue merge_queued emit FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
			queuedErr = fmt.Errorf("merge: enqueue %q: record merge_queued: %w", req.Name, err)
		}
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
func (c *QueueCoordinator) finishTerminal(repo string, req Request, release func(), outcome Outcome) bool {
	ok := c.finish(repo, req, release)
	if outcome != OutcomeMerged {
		return ok
	}
	if !ok {
		c.logf("merge: post-merge hook NOT RUN, the durable entry could not be dropped {repo=%s ws=%s name=%s} — the next boot's Drain replays this merge, and notifying now would hand the parent the same child twice",
			repo, req.Workspace, req.Name)
		return ok
	}
	c.firePostMerge(repo, req)
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
func (c *QueueCoordinator) firePostMerge(repo string, req Request) {
	c.wg.Add(1)
	go func() {
		defer c.wg.Done()
		c.logf("merge: post-merge hook RUNNING {repo=%s ws=%s name=%s target=%s}", repo, req.Workspace, req.Name, req.TargetDir)
		if err := c.postMerge.AfterMerged(c.ctx, req); err != nil {
			c.recordPostMergeFailure(repo, req, err)
			return
		}
		c.logf("merge: post-merge hook COMPLETE {repo=%s ws=%s name=%s}", repo, req.Workspace, req.Name)
	}()
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

	release, err := c.lease.Acquire(c.ctx, req.Workspace)
	if err != nil {
		c.logf("merge: lease acquire FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		c.failed(req, "shim lease unavailable: "+err.Error())
		// A merge that can never run must not hold the head forever.
		return c.finish(repo, req, nil)
	}
	if release == nil {
		// A successful Acquire that returns no release func would leak the
		// lease forever. That is a merge.Lease implementation bug, and there is
		// nothing sane to do with the shim it just claimed.
		panic(fmt.Sprintf("merge: Lease.Acquire returned a nil release for workspace %q", req.Workspace))
	}

	res, err := c.picker.Merge(c.ctx, req)
	if err != nil {
		// merge.Driver rejects a bad precondition BEFORE emitting anything, so
		// without this the workspace would sit at merge_queued with no terminal
		// state ever arriving. The coordinator owns the queued state, so it
		// owns clearing it.
		c.logf("merge: driver FAILED {repo=%s ws=%s name=%s}: %v", repo, req.Workspace, req.Name, err)
		c.failed(req, "merge driver failed: "+err.Error())
		return c.finish(repo, req, release)
	}

	if res.Outcome == OutcomeConflict {
		c.logf("merge: parked on conflict {repo=%s ws=%s name=%s commit=%s}", repo, req.Workspace, req.Name, res.ConflictCommit)
		return c.awaitResolution(repo, req, park, release, res.ConflictCommit)
	}
	c.logf("merge: terminal {repo=%s ws=%s name=%s outcome=%s}", repo, req.Workspace, req.Name, res.Outcome)
	return c.finishTerminal(repo, req, release, res.Outcome)
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
func (c *QueueCoordinator) awaitResolution(repo string, req Request, park *conflictPark, release func(), conflictCommit string) bool {
	c.driveShimResolution(repo, req, park, conflictCommit)
	for {
		select {
		case <-c.ctx.Done():
			// Shutdown with the conflict still in the tree. The lease goes
			// back (it must never outlive its holder), and the entry stays
			// durable so the next daemon's Drain re-parks it.
			c.logf("merge: conflict park abandoned on shutdown {repo=%s ws=%s}", repo, req.Workspace)
			release()
			return false
		case call := <-park.abandons:
			// The user closed the workspace this conflict belongs to. The
			// conflicted cherry-pick stays in the target tree for a human to
			// finish or abort by hand; the lease and the queue head do not.
			c.logf("merge: conflict park ABANDONED {repo=%s ws=%s name=%s} — the conflicted cherry-pick is left in the target tree for hand cleanup", repo, req.Workspace, req.Name)
			ok := c.finish(repo, req, release)
			call.reply <- nil
			return ok
		case call := <-park.calls:
			res, err := c.picker.Resume(c.ctx, call.req)
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
			c.logf("merge: terminal after resume {repo=%s ws=%s outcome=%s}", repo, req.Workspace, res.Outcome)
			// A merge that landed only after a conflict resolution is merged
			// all the same, so the post-merge handoff is owed here exactly as
			// it is on the clean path.
			ok := c.finishTerminal(repo, req, release, res.Outcome)
			call.reply <- nil
			return ok
		}
	}
}

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

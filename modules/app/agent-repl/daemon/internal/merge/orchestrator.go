package merge

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
	"time"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file holds the orchestrator's shape: its state, its construction, and
// the fact publication every other file reaches through.
//
// The orchestrator owns no view. It publishes FACTS — to the footer, to the
// roster, and as synthesized rows on the merge bubble's sub-feed — and the
// resolvers decide how a fact is drawn. Only the footer resolver and this
// package know "merge" as a concept at all; the feed resolver draws each turn
// at the generic output address recorded on it and never learns what a merge
// is.

// The merge states MergeFacts.State carries. They are the vocabulary the footer
// and the roster resolve their arms from, so they are spelled once here. There
// is no state before "queued": nothing about a merge is told until the turn
// that asked for it has ended. And nothing parks: a merge that gives up is
// "failed".
const (
	// StateQueued is waiting in line behind the merge being worked on.
	StateQueued = "queued"
	// StateMerging is running; MergeFacts.Step says which step.
	StateMerging = "merging"
	// StateFailed is a merge that gave up; MergeFacts.FailedArea says where.
	StateFailed = "failed"
	// StateMerged is a merge that landed.
	StateMerged = "merged"
)

// The tab kinds, in the order a run opens them. They are the ledger's interval
// kinds, so a replayed ledger names the same step a live bubble drew.
const (
	TabQueue      = "queue"
	TabPick       = "pick"
	TabRebasing   = "rebasing"
	TabConflicts  = "conflicts"
	TabTests      = "tests"
	TabFixes      = "fixes"
	TabCommitting = "committing"
)

// MaxFixAttempts is THE ONE BOUND on fixing: the most fixing attempts a merge
// makes IN A ROW, drawn as the y of "fixing attempt x/y", and the bound that
// decides the merge has failed its tests. Stated once so the two can never
// disagree.
//
// THE INTENDED DESIGN, OWNER-RULED (2026-10-12): modules/app/agent-repl/AGENTS.md,
// "The merge queue's test gate and remediation contract". Five consecutive
// fixing attempts that leave the gate failing fail the merge; a passing gate
// starts the count over, and an attempt the daemon's own exit, a restart, a
// transfer or a speculative base move cut never counts. Do not change it
// without a conversation with the owner first.
const MaxFixAttempts = 5

// holderMerge is the occupancy guard's holder name, matching the lease holder.
const holderMerge = "merge"

// orchestrator is the merge queue's one implementation.
type orchestrator struct {
	deps Deps
	// openHookLog creates a script hook's log: createLog, which a test
	// replaces to exercise a log that cannot be kept.
	openHookLog func(path string) (io.WriteCloser, error)
	// lockDir is where the per-repo queue locks live. It is under the state
	// root so a test's state root isolates its locks with everything else.
	lockDir string
	// rosterQueue serializes the roster's queues (publishRosterQueue).
	rosterQueue rosterQueuePublisher

	mu sync.Mutex
	// facts is every workspace's last published merge facts, which is what
	// Facts answers from without re-deriving anything.
	facts map[ids.WorkspaceID]MergeFacts
	// running is the run HOLDING each repository's slot: the HEAD, the one
	// merge that may move the target. A second admission for one repository
	// is impossible in this process as well as across processes (the
	// repository's kernel lock is held while any run of its chain stands).
	running map[wsm.RepoKey]*run
	// chains are each repository's runs IN QUEUE ORDER (chain.go): the head
	// first, when it stands, then the merges rebased and tested ahead of their
	// turn behind it, never more than the window in all (queueWindow). A run joins
	// it before it starts and leaves it at its one release (releaseSlot).
	chains map[wsm.RepoKey][]*run
	// chainSignals are each repository's chain-change broadcast: closed and
	// replaced, under mu, whenever a member joins, leaves, is promoted or
	// publishes a new tip, so a waiter that read the chain under mu and then
	// waits on the channel it read misses no change.
	chainSignals map[wsm.RepoKey]chan struct{}
	// targetTips is each repository's target tip as a run last read it: the
	// base the merge behind the head rebases onto (race.go). Guarded by mu.
	targetTips map[wsm.RepoKey]string
	// overtakings are the races to land being won by the merge behind the
	// head, by repository (race.go). Guarded by mu.
	overtakings map[wsm.RepoKey]*overtaking
	// testTurns are each repository's test turn and the line for it
	// (testturn.go): only one merge of a repository runs its test gate at a
	// time. A repository is here only while some run holds its turn. Guarded
	// by mu.
	testTurns map[wsm.RepoKey]*turnLine
	// waitingPass marks the repositories whose waiting members are being
	// rebased now, and waitingAgain those whose landing asked for one more
	// pass (rebasewaiting.go). Guarded by mu.
	waitingPass, waitingAgain map[wsm.RepoKey]bool
	// onWaitingRebased, when set, is called each time a waiting member's
	// rebase settled back into line. TEST-ONLY.
	onWaitingRebased func(ws ids.WorkspaceID)
	// onWaitingPass, when set, is called each time a pass over a queue's
	// waiting members ended. TEST-ONLY.
	onWaitingPass func(repo wsm.RepoKey)
	// repoLocks are the repositories' kernel locks this process holds: taken
	// when a repository's first run is admitted, released when its chain and
	// slot are both empty again.
	repoLocks map[wsm.RepoKey]*repoLock
	// kicked records a kick that arrived while the repository's pump was
	// running. The pump reads it under the same lock before it goes idle, so
	// a kick is never lost between the pump's last look and its exit.
	kicked map[wsm.RepoKey]bool
	// displaces records the workspaces whose merge the USER asked for, which
	// alone may displace the turn in flight at admission (see Requester).
	displaces map[ids.WorkspaceID]bool
	// live counts the goroutines the orchestrator starts: the admission pumps
	// and the requests waiting for their turn to end. Nothing in production
	// waits on it; a test does, so nothing it started outlives the test.
	live sync.WaitGroup
	// events serializes the queue's reported changes (subscriptions.go). It
	// is its own lock, never taken with mu.
	events *queueEvents
	// runsByWorkspace addresses the in-flight run of one requester.
	runsByWorkspace map[ids.WorkspaceID]*run
	// requested are the requests waiting for their requesting turn to end.
	requested map[ids.WorkspaceID]*requestWait
	// offers records the workspaces with a merge-queue question standing.
	offers map[ids.WorkspaceID]bool
	// repoOf remembers which queue a workspace was enqueued on, so an evict
	// does not have to re-derive geometry that may no longer resolve.
	repoOf map[ids.WorkspaceID]wsm.RepoKey
	// ledgerOf is the merge's LEDGER identity, minted when the merge is put in
	// line. It addresses the merge bubble -- which a QUEUED merge already has,
	// showing its queue tab -- and it is the identity the occupancy lease is
	// later acquired under, so the queued bubble and the admitted one are one
	// bubble.
	ledgerOf map[ids.WorkspaceID]ids.LeaseID
	// failed is each queue's last FAILED merge as the roster draws it
	// (sidebar.MergeQueue.Failed): set when a merge fails (noteFailed), and
	// retired once another merge is in line on its queue (retireFailed).
	failed map[wsm.RepoKey]failedMerge
	// originsOf caches, per merging workspace, its merge origin: set
	// (non-nil) for a merge workspace, nil for every other. A workspace's
	// merge origin and dir are fixed at its registration, so the answer never
	// changes once read.
	originsOf map[ids.WorkspaceID]*mergeOrigin
	// heads orders every head publication (headBook). It is its own lock,
	// taken BEFORE mu and never under it.
	heads headBook
	// picks are the async picks running now, keyed by merge workspace, so an
	// ask change redraws the live pick tab.
	picks map[ids.WorkspaceID]*pick
	// resumes are the merges the boot recovery found a progress record for,
	// keyed by workspace: the admission pump runs each from its record, ahead
	// of anything waiting in its repository's line (Recover, startResumed).
	resumes map[ids.WorkspaceID]progressDoc
	// takenAway are the workspaces a suspension took the merges of
	// (suspendFor), keyed to that suspension: a handover moved them to
	// another daemon, or a pause, a restart, a race or the power is holding
	// them until their take-back (AdoptWorkspace) clears the mark. This
	// daemon admits nothing of them meanwhile (transfer.go).
	takenAway map[ids.WorkspaceID]suspension
	// abandonsAtTakeBack are the abandonments that reached a merge after a
	// suspension took it: the suspension stands, and the take-back that
	// clears takenAway drops the merge instead of resuming it. Recorded and
	// consumed under the same lock as takenAway (dequeue, AdoptWorkspace).
	abandonsAtTakeBack map[ids.WorkspaceID]AbandonCause
	// pumping guards one admission pump per repository.
	pumping map[wsm.RepoKey]bool
	// headLeases are the merge leases admitFront took for the heads it
	// admitted, handed to start: the lease goes first and the admission
	// after, so an admitted head always has its run's lease.
	headLeases map[ids.WorkspaceID]wsm.Lease
	// leaseWaits are the queue fronts whose workspace another holder (a
	// restart, a drain, a hibernation) held when they came to be admitted,
	// by workspace, with their repository: each waits, unadmitted, and its
	// workspace's next lease change kicks its repository (LeaseChanged).
	leaseWaits map[ids.WorkspaceID]wsm.RepoKey
	// admissions counts the admission steps in flight -- the store reads and
	// the admission write the pump makes before a run starts. It is held under
	// mu: a step is added only while not draining (enterAdmission) and
	// removed by leaveAdmission, so Drain reads the count under the SAME lock
	// it sets draining under, and no admission read reaches a state client the
	// exit has closed.
	//
	// IT IS A COUNT UNDER mu, NOT A sync.WaitGroup, so "nothing in flight" is
	// known synchronously. With a WaitGroup the drain could only learn that
	// through a goroutine's Wait, which raced the drain's own bound: under
	// load the bound fired first and the drain reported an admission that did
	// not exist (measured: 2 in 2000 runs of
	// TestTheDrainWarnsWhatOneExpiredBoundLeftUnstamped under 48 busy loops).
	admissions int
	// admissionsIdle is made by Drain when it finds admissions in flight, and
	// closed by the leaveAdmission that brings the count to zero. Nil while
	// nothing waits.
	admissionsIdle chan struct{}
	// async reports whether Enqueue starts the admission pump and the request
	// waits itself.
	async bool
	// draining reports that the daemon is on its way out: the admission pump
	// admits NOTHING more, so the bounded shutdown drain cannot be outrun by
	// a merge that started inside it.
	draining bool
	// terminals are the runs that have reached their TERMINAL — the durable
	// stamps and the teardown they share — and they are the only work the
	// shutdown drain waits for. A run still in a long step is NOT here: it is
	// abandoned to the boot recovery. The map is built lazily, so nothing
	// about construction has to know about draining.
	terminals map[ids.WorkspaceID]*terminal
	// drainBound overrides TerminalDrainBound. It is a TEST-ONLY seam: a suite
	// asserting what the bound does when it EXPIRES must not wait the
	// production bound to see it, and zero means the production value.
	drainBound time.Duration
	// gitBound overrides MergeGitStopBound, TEST-ONLY as drainBound is.
	gitBound time.Duration
	// window overrides the governor's window, TEST-ONLY as drainBound is: the
	// package's tests of one merge at a time run with a window of one, so no
	// merge behind the head starts; zero means the production window.
	window int
	// onChainIdle, when set, is called each time a merge behind its head goes
	// idle between steps, waiting for its chain to change. Nil in production;
	// a test reads where the chain stands from it.
	onChainIdle func(ws ids.WorkspaceID)
	// onRunStart, when set, is called with each run that may ask for its
	// repository's test turn, as it is built and before any of its steps;
	// onTestTurnAsk, when set, each time a gate of run r is about to ask for
	// that turn, and may hold it there until ctx is done; onTestTurnLine,
	// when set, each time a gate joined the line for it. Nil in production; a
	// test orders the gates of two merges through them, holding one run's
	// ask and never a later run's of the same workspace.
	onRunStart     func(r *run)
	onTestTurnAsk  func(ctx context.Context, r *run)
	onTestTurnLine func(ws ids.WorkspaceID)
	// onKick, when set, is called by every kick of a synchronous (test)
	// orchestrator, whose kick starts no pump: a test that pumps by hand
	// wakes on it, so an edge that would have pumped production's queue
	// (a waiting member's rebase settling) is never lost to it. Nil in
	// production.
	onKick func(repo wsm.RepoKey)
	// onGitStopWait, when set, is called once the drain has stopped every
	// suspended run's git gate, immediately before it waits for the commands
	// in flight: a test releases a held command from inside that wait.
	onGitStopWait func()
	// onRaceClaimed, when set, is called with the head once the merge behind
	// it claimed the race to land, before the race is recorded (race.go).
	onRaceClaimed func(head ids.WorkspaceID)
	// onBoundaryAsk, when set, is called once a restart has asked a running
	// merge to stop at its next step boundary, before the restart waits for
	// it. Nil in production; tests release the step in flight from it.
	onBoundaryAsk func()
	// onDrainWait, when set, is called once the drain has stopped admitting
	// and snapshotted the terminal work, immediately before it begins waiting.
	// It exists so a test releases a held terminal from inside the drain's own
	// wait rather than guessing when the drain got there.
	onDrainWait func()
}

// New builds the orchestrator. Its admission pump runs on its own goroutine:
// Enqueue answers as soon as the merge is durably queued, and the queue's front
// is admitted behind that answer.
func New(deps Deps) (Orchestrator, error) {
	o, err := newOrchestrator(deps)
	if err != nil {
		return nil, err
	}
	o.async = true
	return o, nil
}

// newOrchestrator builds the orchestrator with the admission pump left to the
// caller. New turns the pump on; the package's own tests drive admission a step
// at a time instead, which is what lets the queue's ordering be asserted
// without waiting on a goroutine.
func newOrchestrator(deps Deps) (*orchestrator, error) {
	if err := deps.validate(); err != nil {
		return nil, err
	}
	if deps.Now == nil {
		deps.Now = time.Now
	}
	if deps.DeployConfig == nil {
		deps.DeployConfig = deployconfig.OnDisk{}
	}
	if deps.Home == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return nil, fmt.Errorf("merge: resolving the home directory a test log's label shortens: %w", err)
		}
		deps.Home = home
	}
	o := &orchestrator{
		deps:               deps,
		openHookLog:        func(path string) (io.WriteCloser, error) { return createLog(path) },
		lockDir:            filepath.Join(deps.StateDir, "merge-locks"),
		facts:              map[ids.WorkspaceID]MergeFacts{},
		running:            map[wsm.RepoKey]*run{},
		chains:             map[wsm.RepoKey][]*run{},
		chainSignals:       map[wsm.RepoKey]chan struct{}{},
		targetTips:         map[wsm.RepoKey]string{},
		overtakings:        map[wsm.RepoKey]*overtaking{},
		testTurns:          map[wsm.RepoKey]*turnLine{},
		waitingPass:        map[wsm.RepoKey]bool{},
		waitingAgain:       map[wsm.RepoKey]bool{},
		repoLocks:          map[wsm.RepoKey]*repoLock{},
		kicked:             map[wsm.RepoKey]bool{},
		displaces:          map[ids.WorkspaceID]bool{},
		runsByWorkspace:    map[ids.WorkspaceID]*run{},
		requested:          map[ids.WorkspaceID]*requestWait{},
		ledgerOf:           map[ids.WorkspaceID]ids.LeaseID{},
		failed:             map[wsm.RepoKey]failedMerge{},
		originsOf:          map[ids.WorkspaceID]*mergeOrigin{},
		heads:              newHeadBook(),
		picks:              map[ids.WorkspaceID]*pick{},
		resumes:            map[ids.WorkspaceID]progressDoc{},
		takenAway:          map[ids.WorkspaceID]suspension{},
		abandonsAtTakeBack: map[ids.WorkspaceID]AbandonCause{},
		offers:             map[ids.WorkspaceID]bool{},
		repoOf:             map[ids.WorkspaceID]wsm.RepoKey{},
		pumping:            map[wsm.RepoKey]bool{},
		headLeases:         map[ids.WorkspaceID]wsm.Lease{},
		leaseWaits:         map[ids.WorkspaceID]wsm.RepoKey{},
		events:             newQueueEvents(),
	}
	// EVERY WRITE THAT ADDS OR REMOVES A QUEUE ROW REPUBLISHES THE ROSTER'S
	// QUEUE (rosterqueue.go): the orchestrator only ever writes through the
	// wrapped DB.
	//
	// AND EVERY WRITE THAT CHANGES A QUEUE'S LINE REPORTS ITSELF to the
	// subscriptions' observer (subscriptions.go), beneath that: the report is
	// made under the queue events' lock, and the roster's republish after it,
	// outside it.
	o.deps.DB = rosterQueueDB{DB: queueEventsDB{DB: deps.DB, o: o}, o: o}
	deps.Asks.bind(o.askChanged)
	deps.Governor.OnModeChange(o.modeChanged)
	return o, nil
}

// askChanged hands a workspace's change of open asks to its run in flight, if
// any: a pick, conflict-resolution or fixing tab standing live is redrawn
// waiting on the user, or live again. A workspace with no run in flight has no
// tab to draw it on; the ask set keeps the fact for a run that reaches such a
// tab.
//
// AN ASK OPENED IN A HIDDEN MERGE WORKSPACE REVEALS IT (owner ruling,
// 2026-10-11): the agent needs the user, so its workspace is drawn from now
// on.
func (o *orchestrator) askChanged(ws ids.WorkspaceID, waiting bool) {
	if waiting {
		if _, async := o.requesterOf(ws); async {
			o.reveal(context.Background(), ws, "its agent asked the user something")
		}
	}
	if o.redrawPick(ws, waiting) {
		return
	}
	r, ok := o.runFor(ws)
	if !ok {
		return
	}
	r.setWaitingOnUser(waiting)
}

// validate refuses a dependency set the orchestrator cannot run with. Missing
// collaborators are found at construction rather than mid-merge, where the
// failure would land on a half-applied merge.
func (d Deps) validate() error {
	missing := []string{}
	check := func(ok bool, name string) {
		if !ok {
			missing = append(missing, name)
		}
	}
	check(d.DB != nil, "DB")
	check(d.Timings != nil, "Timings")
	check(d.Git != nil, "Git")
	check(d.Queue != nil, "Queue")
	check(d.Feed != nil, "Feed")
	check(d.Footer != nil, "Footer")
	check(d.Sidebar != nil, "Sidebar")
	check(d.Banners != nil, "Banners")
	check(d.Asks != nil, "Asks")
	check(d.Names != nil, "Names")
	check(d.InterruptTurn != nil, "InterruptTurn")
	check(d.Briefs != nil, "Briefs")
	check(d.SelfAddress != "", "SelfAddress")
	check(d.Painter != nil, "Painter")
	check(d.TestRunner != nil, "TestRunner")
	check(d.Governor != nil, "Governor")
	check(d.AwaitTurnEnd != nil, "AwaitTurnEnd")
	check(d.Occupy != nil, "Occupy")
	check(d.StartSession != nil, "StartSession")
	check(d.StopSession != nil, "StopSession")
	check(d.HoldStarts != nil, "HoldStarts")
	check(d.CaptureDisplaced != nil, "CaptureDisplaced")
	check(d.Freeness != nil, "Freeness")
	check(d.TurnInFlight != nil, "TurnInFlight")
	check(d.Rollout != nil, "Rollout")
	check(d.QueueEvents != nil, "QueueEvents")
	check(d.Log != nil, "Log")
	check(d.StateDir != "", "StateDir")
	check(d.HookRunner != nil, "HookRunner")
	check(d.TestCommand != nil, "TestCommand")
	if len(missing) > 0 {
		return fmt.Errorf("merge: the orchestrator is missing %v", missing)
	}
	return nil
}

// log resolves the workspace's own logger. A workspace-bound record never goes
// to the global sink: failing to resolve a known workspace is an invariant
// violation, not a reason to write globally.
func (o *orchestrator) log(ctx context.Context, ws ids.WorkspaceID) dlog.Logger {
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err == nil {
		if l, lerr := o.deps.Log.Workspace(record.Dir); lerr == nil {
			return l
		}
	}
	return o.deps.Log.Global()
}

// Facts reports a workspace's merge facts; the bool is false when it has none.
func (o *orchestrator) Facts(ws ids.WorkspaceID) (MergeFacts, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	facts, ok := o.facts[ws]
	return facts, ok
}

// publish installs one workspace's merge facts on both surfaces that draw them.
// The footer and the roster read the SAME value, so the two can never disagree
// about a merge.
//
// A STARTED MERGE MOVING TO A NEW STEP is reported to the subscribers of
// REPO, the queue the merge stands in, from here (noteFacts), since a step
// is no queue write. Every caller names the queue it publishes for, so a
// step can never be reported against no queue.
func (o *orchestrator) publish(repo wsm.RepoKey, ws ids.WorkspaceID, facts MergeFacts) {
	checkQueuedAt(o.log(context.Background(), ws), ws, facts)
	o.mu.Lock()
	_, hadFacts := o.facts[ws]
	o.facts[ws] = facts
	o.mu.Unlock()
	o.footerSetMerge(ws, facts)
	o.deps.Sidebar.SetMerge(ws, facts)
	o.publishHost(ws)
	o.noteFacts(ws, repo, hadFacts, facts)
}

// checkQueuedAt records a merge standing in line or in flight that carries no
// queue instant. Every such publish site stamps the queue entry's EnqueuedAt
// (MergeFacts.QueuedAt), so a missing one is a site that forgot to, and the
// roster's when-column would count that merge from the epoch.
func checkQueuedAt(log dlog.Logger, ws ids.WorkspaceID, facts MergeFacts) {
	if facts.State != StateQueued && facts.State != StateMerging {
		return
	}
	if facts.QueuedAt.UnixMilli() > 0 {
		return
	}
	log.Error("daemon.merge.publish", "a merge in line or in flight was published with no queue instant",
		dlog.Context{
			"workspace":           string(ws),
			"state":               facts.State,
			"step":                string(facts.Step),
			"queued_at_ms":        facts.QueuedAt.UnixMilli(),
			"invariant_violation": "every queued or merging merge carries its queue entry's EnqueuedAt",
		})
}

// publishHost republishes the workspace's host view when a surface is wired.
// Every merge state change moves the host composer's gate, and `publish` and
// `forget` are the only two places a merge's state changes.
func (o *orchestrator) publishHost(ws ids.WorkspaceID) {
	if o.deps.PublishHost == nil {
		return
	}
	o.deps.PublishHost(ws)
}

// forget drops a workspace's facts entirely — the abandon path, where the merge
// never existed as far as any surface is concerned.
func (o *orchestrator) forget(ws ids.WorkspaceID) {
	o.mu.Lock()
	delete(o.facts, ws)
	// The merge is gone as far as every surface is concerned, and so is the
	// bubble its ledger identity addressed.
	delete(o.ledgerOf, ws)
	o.mu.Unlock()
	o.footerSetMerge(ws, MergeFacts{State: "none"})
	o.deps.Sidebar.SetMerge(ws, MergeFacts{State: "none"})
	o.publishHost(ws)
}

// footerSetMerge hands a merge's facts to WS's footer, except a merge
// workspace's: its footer shows the ordinary statuses of a workspace (owner
// ruling, 2026-10-11), and the merge is read in the requester's bubble.
func (o *orchestrator) footerSetMerge(ws ids.WorkspaceID, facts MergeFacts) {
	if _, async := o.requesterOf(ws); async {
		return
	}
	o.deps.Footer.SetMerge(ws, facts)
}

// reveal draws hidden merge workspace WS in the tab bar and the sidebar from
// now on, CAUSE naming why, and restates its bubble's head with the switch to
// it. A reveal that fails is recorded at ERROR: the merge goes on, and the
// workspace stays hidden until a later reveal succeeds.
func (o *orchestrator) reveal(ctx context.Context, ws ids.WorkspaceID, cause string) {
	const op = "daemon.merge.reveal"
	revealed, err := o.deps.Detached.Reveal(ctx, ws)
	if err != nil {
		o.log(ctx, ws).Error(op, "could not reveal the merge workspace; it stays hidden", dlog.Context{
			"workspace": string(ws), "cause": cause, "error": err.Error()})
		return
	}
	if !revealed {
		return
	}
	o.log(ctx, ws).Info(op, "revealed the merge workspace: it needs the user", dlog.Context{"workspace": string(ws), "cause": cause})
	o.restateHead(ws, headReveal)
}

// RevealMergeWorkspace reveals hidden merge workspace WS (Orchestrator).
func (o *orchestrator) RevealMergeWorkspace(ctx context.Context, ws ids.WorkspaceID, cause string) error {
	if _, async := o.requesterOf(ws); !async {
		return nil
	}
	o.reveal(ctx, ws, cause)
	return nil
}

// runFor addresses one workspace's in-flight run.
func (o *orchestrator) runFor(ws ids.WorkspaceID) (*run, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	r, ok := o.runsByWorkspace[ws]
	return r, ok
}

// repoKeyFor resolves the queue a merge belongs on: the repository's canonical
// common dir, read off a directory inside it. Every source a request can name
// is in the requester's own repository, so the requester's directory keys it.
func (o *orchestrator) repoKeyFor(ctx context.Context, dir string) (wsm.RepoKey, error) {
	common, err := o.deps.Git.CommonDir(ctx, dir)
	if err != nil {
		return "", fmt.Errorf("merge: resolving the repository of %s: %w", dir, err)
	}
	return wsm.RepoKey(common), nil
}

// layoutFor loads a workspace's recorded merge geometry, refusing the merge
// when the creation job holds none. Geometry is recorded at creation and NEVER
// inferred later: a workspace materialized without it can never be merged, and
// guessing would land a merge somewhere nobody chose.
func (o *orchestrator) layoutFor(ctx context.Context, ws ids.WorkspaceID) (wsm.CreationJob, error) {
	job, found, err := o.deps.DB.CreationJob(ctx, ws)
	if err != nil {
		return wsm.CreationJob{}, err
	}
	if !found {
		return wsm.CreationJob{}, refuse(ArmNoLayoutFacts, ws, "no creation job records this workspace's merge geometry")
	}
	if job.Layout.SourceBranch == "" || job.Layout.SourceDir == "" || job.Layout.TargetDir == "" {
		return wsm.CreationJob{}, refuse(ArmNoLayoutFacts, ws, "the creation job's merge geometry is incomplete")
	}
	return job, nil
}

// TestRunMarkerEnv marks a test run the daemon started. bin/background.sh
// refuses every test run without it (owner ruling, 2026-10-24: no test runs
// by hand), and every process of the run inherits it.
const TestRunMarkerEnv = "AGENT_REPL_TEST_RUN_BY_DAEMON"

// TestCommandFor resolves a test run's command line for the tree it tests:
// the tree's OWN copy of the deploy config's test script (SCRIPT, relative to
// a checkout's root), because the script tests the checkout it lives in, so
// running any other copy of it would test that copy's tree instead of the
// merge. AGENT_REPL_TEST_ALL_SCRIPT overrides it for tests, and the script is
// run through bash so a fixture script needs no execute bit. The command
// carries TestRunMarkerEnv, so every test run the daemon starts (the merge
// queue's gates, RunTests, RunRedundancy) is one bin/background.sh admits;
// this is the one place it is set. The script is the LAST element, which is
// what the gate's preflight reads.
func TestCommandFor(tree, script string) []string {
	path := os.Getenv("AGENT_REPL_TEST_ALL_SCRIPT")
	if path == "" {
		path = filepath.Join(tree, script)
	}
	return []string{"env", TestRunMarkerEnv + "=1", "bash", path}
}

// errNoRun is returned when a verb addresses an in-flight merge that is not
// there. It is an internal seam failure rather than a user-facing refusal: the
// caller lost track of the run.
var errNoRun = errors.New("merge: no merge is in flight for that workspace")

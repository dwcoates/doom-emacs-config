package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// This file holds the orchestrator's shape: its state, its construction, and
// the fact publication every other file reaches through.
//
// The orchestrator owns no view. It publishes FACTS — to the footer, to the
// roster, and as synthesized rows on the merge bubble's sub-feed — and the
// resolvers decide how a fact is drawn. Only the footer resolver and this
// package know "merge" as a concept at all; the feed resolver honors a generic
// output address and never learns what a merge is.

// The merge states MergeFacts.State carries. They are the vocabulary the footer
// and the roster resolve their arms from, so they are spelled once here.
const (
	// StateEnqueuing is the first instant of a merge, before it is on a queue.
	StateEnqueuing = "enqueuing"
	// StateQueued is waiting behind another workspace's merge.
	StateQueued = "queued"
	// StateMerging is running; ActiveTab says which phase.
	StateMerging = "merging"
	// StateParked is stopped, awaiting the user's guidance.
	StateParked = "parked"
	// StateConflict is stopped on conflicts the agent could not resolve.
	StateConflict = "conflict"
	// StateFailed is a merge that gave up.
	StateFailed = "failed"
	// StateMerged is a merge that landed.
	StateMerged = "merged"
)

// The tab kinds, in the order a run opens them. They are the ledger's interval
// kinds and MergeFacts.ActiveTab's vocabulary at once, so a replayed ledger and
// a live footer name the same phase.
const (
	TabQueue      = "queue"
	TabPrePrompt  = "pre_prompt"
	TabMerge      = "merge"
	TabConflicts  = "conflicts"
	TabTests      = "tests"
	TabFixes      = "fixes"
	TabPostPrompt = "post_prompt"
)

// holderMerge is the occupancy guard's holder name, matching the lease holder.
const holderMerge = "merge"

// orchestrator is the merge queue's one implementation.
type orchestrator struct {
	deps Deps
	// lockDir is where the per-repo queue locks live. It is under the state
	// root so a test's state root isolates its locks with everything else.
	lockDir string

	mu sync.Mutex
	// facts is every workspace's last published merge facts, which is what
	// Facts answers from without re-deriving anything.
	facts map[ids.WorkspaceID]MergeFacts
	// running is the run HOLDING each repository's slot: the one merge that
	// may make a queue tree, run the gate and move the target. A second
	// admission for one repository is impossible in this process as well as
	// across processes (the slot's run also holds the repository's kernel
	// lock). A PARKED run holds no slot: it yields it the moment it parks, so
	// the merges behind it proceed (owner ruling, 2026-09-28).
	running map[wsm.RepoKey]*run
	// waiters are the parked runs whose guidance turn ended and who want
	// their repository's slot back, in the order they asked. Only the pump
	// grants a slot, so a waiter and a queue front never race for one.
	waiters map[wsm.RepoKey][]*run
	// kicked records a kick that arrived while the repository's pump was
	// running. The pump reads it under the same lock before it goes idle, so
	// a kick is never lost between the pump's last look and its exit.
	kicked map[wsm.RepoKey]bool
	// displaces records the workspaces whose merge the USER asked for, which
	// alone may displace the turn in flight at admission (see Requester).
	displaces map[ids.WorkspaceID]bool
	// live counts the run goroutines. Nothing in production waits on it; a
	// test does, so a run it left behind cannot outlive the test.
	live sync.WaitGroup
	// runsByWorkspace addresses the in-flight run of one workspace, which is
	// what a parked route and an interrupt need.
	runsByWorkspace map[ids.WorkspaceID]*run
	// offers records the workspaces with a dequeue offer standing.
	offers map[ids.WorkspaceID]bool
	// repoOf remembers which queue a workspace was enqueued on, so an evict
	// does not have to re-derive geometry that may no longer resolve.
	repoOf map[ids.WorkspaceID]wsm.RepoKey
	// ledgerOf is the merge's LEDGER identity, minted at ENQUEUE. It addresses
	// the merge bubble -- which a QUEUED merge already has, showing its queue
	// tab -- and it is the identity the occupancy lease is later acquired
	// under, so the queued bubble and the admitted one are one bubble.
	ledgerOf map[ids.WorkspaceID]ids.LeaseID
	// pumping guards one admission pump per repository.
	pumping map[wsm.RepoKey]bool
	// admissions counts the admission steps in flight -- the store reads and
	// the admission write the pump makes before a run starts. A step is added
	// only under mu while not draining (enterAdmission), and Drain waits for
	// them, so no admission read reaches a state client the exit has closed.
	admissions sync.WaitGroup
	// async reports whether Enqueue starts the admission pump itself.
	async bool
	// draining reports that the daemon is on its way out: the admission pump
	// admits NOTHING more, so the bounded shutdown drain cannot be outrun by
	// a merge that started inside it.
	draining bool
	// terminals are the runs that have reached their TERMINAL — the durable
	// stamps and the teardown they share — and they are the only work the
	// shutdown drain waits for. A run still in a long phase is NOT here: it is
	// abandoned to the boot recovery, which is what it was before. The map is
	// built lazily, so nothing about construction has to know about draining.
	terminals map[ids.WorkspaceID]*terminal
	// drainBound overrides TerminalDrainBound. It is a TEST-ONLY seam: a suite
	// asserting what the bound does when it EXPIRES must not wait the
	// production bound to see it, and zero means the production value.
	drainBound time.Duration
	// onDrainWait, when set, is called once the drain has stopped admitting
	// and snapshotted the terminal work, immediately before it begins waiting.
	// It exists so a test releases a held terminal from inside the drain's own
	// wait rather than guessing when the drain got there.
	onDrainWait func()
	// onPark, when set, is signalled the moment a run parks. It exists so a
	// test synchronizes on the park itself rather than on elapsed time: a
	// parked merge is a state, and waiting for a state by sleeping is how a
	// suite becomes flaky.
	onPark func(ids.WorkspaceID)
	// onWait, when set, is signalled the moment a parked run whose guidance
	// turn ended asks for its repository's slot back. Like onPark it exists so
	// a test synchronizes on the state rather than on elapsed time.
	onWait func(ids.WorkspaceID)
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
	if deps.Policy == nil {
		deps.Policy = prompts.OnDisk{}
	}
	return &orchestrator{
		deps:            deps,
		lockDir:         filepath.Join(deps.StateDir, "merge-locks"),
		facts:           map[ids.WorkspaceID]MergeFacts{},
		running:         map[wsm.RepoKey]*run{},
		waiters:         map[wsm.RepoKey][]*run{},
		kicked:          map[wsm.RepoKey]bool{},
		displaces:       map[ids.WorkspaceID]bool{},
		runsByWorkspace: map[ids.WorkspaceID]*run{},
		ledgerOf:        map[ids.WorkspaceID]ids.LeaseID{},
		offers:          map[ids.WorkspaceID]bool{},
		repoOf:          map[ids.WorkspaceID]wsm.RepoKey{},
		pumping:         map[wsm.RepoKey]bool{},
	}, nil
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
	check(d.Git != nil, "Git")
	check(d.Queue != nil, "Queue")
	check(d.Feed != nil, "Feed")
	check(d.Footer != nil, "Footer")
	check(d.Sidebar != nil, "Sidebar")
	check(d.Holds != nil, "Holds")
	check(d.Briefs != nil, "Briefs")
	check(d.Painter != nil, "Painter")
	check(d.TestRunner != nil, "TestRunner")
	check(d.AwaitTurnEnd != nil, "AwaitTurnEnd")
	check(d.Occupy != nil, "Occupy")
	check(d.StartSession != nil, "StartSession")
	check(d.StopSession != nil, "StopSession")
	check(d.CaptureDisplaced != nil, "CaptureDisplaced")
	check(d.Freeness != nil, "Freeness")
	check(d.ParkedRoute != nil, "ParkedRoute")
	check(d.Rollout != nil, "Rollout")
	check(d.Log != nil, "Log")
	check(d.StateDir != "", "StateDir")
	check(d.SelfRepoDir != "", "SelfRepoDir")
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
func (o *orchestrator) publish(ws ids.WorkspaceID, facts MergeFacts) {
	o.mu.Lock()
	o.facts[ws] = facts
	o.mu.Unlock()
	o.deps.Footer.SetMerge(ws, facts)
	o.deps.Sidebar.SetMerge(ws, facts)
	o.publishHost(ws)
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
	o.deps.Footer.SetMerge(ws, MergeFacts{State: "none"})
	o.deps.Sidebar.SetMerge(ws, MergeFacts{State: "none"})
	o.publishHost(ws)
}

// runFor addresses one workspace's in-flight run.
func (o *orchestrator) runFor(ws ids.WorkspaceID) (*run, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	r, ok := o.runsByWorkspace[ws]
	return r, ok
}

// repoKeyFor resolves the queue a workspace's merge belongs on: the TARGET
// repository's canonical common dir. It is derived from the layout facts the
// creation job recorded, never guessed from the worktree.
func (o *orchestrator) repoKeyFor(ctx context.Context, job wsm.CreationJob) (wsm.RepoKey, error) {
	common, err := o.deps.Git.CommonDir(ctx, job.Layout.TargetDir)
	if err != nil {
		return "", fmt.Errorf("merge: resolving the target repository of %s: %w", job.Layout.TargetDir, err)
	}
	return wsm.RepoKey(common), nil
}

// policyFor answers where a workspace's repository states its merge policy:
// the daemon's own corpus for the ONE repository the daemon's checkout lives
// in, and the repository's own `.agent-repl/prompts` for every other. The
// corpus is never a fallback for another repository.
//
// The repository is the workspace's OWN, read off the registry, not the merge
// TARGET: a child workspace targets its parent's worktree, and a repository's
// policy belongs to the repository rather than to whichever tree a particular
// merge lands in.
func (o *orchestrator) policyFor(ctx context.Context, ws ids.WorkspaceID) (prompts.Source, error) {
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return prompts.Source{}, fmt.Errorf("merge: reading %s: %w", ws, err)
	}
	repositories, err := o.deps.DB.ListRepositories(ctx)
	if err != nil {
		return prompts.Source{}, fmt.Errorf("merge: reading the repository registry: %w", err)
	}
	if repository, found := wsm.RepositoryWithID(repositories, record.Repo); found {
		return prompts.SourceFor(repository.Dir, o.deps.CheckoutRoot, o.deps.PromptsDir), nil
	}
	return prompts.Source{}, fmt.Errorf("merge: workspace %s names repository %q, which is not registered", ws, record.Repo)
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

// TestCommandFor resolves the gate's command line for the tree it tests: the
// tree's OWN entrypoint, because `bin/test-all.sh` tests the checkout it lives
// in, so running any other copy of it would test that copy's tree instead of
// the merge. AGENT_REPL_TEST_ALL_SCRIPT overrides it for tests, and the script
// is run through bash so a fixture script needs no execute bit. The script is
// the LAST element, which is what the gate's preflight reads.
func TestCommandFor(tree string) []string {
	script := os.Getenv("AGENT_REPL_TEST_ALL_SCRIPT")
	if script == "" {
		script = filepath.Join(tree, "modules", "app", "agent-repl", "bin", "test-all.sh")
	}
	return []string{"bash", script}
}

// errNoRun is returned when a verb addresses an in-flight merge that is not
// there. It is an internal seam failure rather than a user-facing refusal: the
// caller lost track of the run.
var errNoRun = errors.New("merge: no merge is in flight for that workspace")

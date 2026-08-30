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
	// running is the merge in flight per repository, so a second admission for
	// one repository is impossible in this process as well as across processes.
	running map[wsm.RepoKey]*run
	// runsByWorkspace addresses the in-flight run of one workspace, which is
	// what a parked route and an interrupt need.
	runsByWorkspace map[ids.WorkspaceID]*run
	// offers records the workspaces with a dequeue offer standing.
	offers map[ids.WorkspaceID]bool
	// repoOf remembers which queue a workspace was enqueued on, so an evict
	// does not have to re-derive geometry that may no longer resolve.
	repoOf map[ids.WorkspaceID]wsm.RepoKey
	// pumping guards one admission pump per repository.
	pumping map[wsm.RepoKey]bool
	// async reports whether Enqueue starts the admission pump itself.
	async bool
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
	return &orchestrator{
		deps:            deps,
		lockDir:         filepath.Join(deps.StateDir, "merge-locks"),
		facts:           map[ids.WorkspaceID]MergeFacts{},
		running:         map[wsm.RepoKey]*run{},
		runsByWorkspace: map[ids.WorkspaceID]*run{},
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
	check(d.CaptureDisplaced != nil, "CaptureDisplaced")
	check(d.CommitMerge != nil, "CommitMerge")
	check(d.ParkedRoute != nil, "ParkedRoute")
	check(d.Rollout != nil, "Rollout")
	check(d.Log != nil, "Log")
	check(d.StateDir != "", "StateDir")
	check(d.SelfRepoDir != "", "SelfRepoDir")
	check(len(d.TestCommand) > 0, "TestCommand")
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
}

// forget drops a workspace's facts entirely — the abandon path, where the merge
// never existed as far as any surface is concerned.
func (o *orchestrator) forget(ws ids.WorkspaceID) {
	o.mu.Lock()
	delete(o.facts, ws)
	o.mu.Unlock()
	o.deps.Footer.SetMerge(ws, MergeFacts{State: "none"})
	o.deps.Sidebar.SetMerge(ws, MergeFacts{State: "none"})
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

// selfRepoDir resolves the daemon's own checkout. AGENT_REPL_SELF_REPO_DIR
// overrides it for tests; the self-reload trigger STAYS ON under the override,
// whose test safety is AGENT_REPL_DEPLOY_SCRIPT naming a fake deploy script.
func SelfRepoDir(daemonCheckout string) string {
	if override := os.Getenv("AGENT_REPL_SELF_REPO_DIR"); override != "" {
		return override
	}
	return daemonCheckout
}

// TestCommandFor resolves the gate's command line. AGENT_REPL_TEST_ALL_SCRIPT
// overrides the repository's own entrypoint for tests, and the script is run
// through bash so a fixture script needs no execute bit.
func TestCommandFor(checkout string) []string {
	script := os.Getenv("AGENT_REPL_TEST_ALL_SCRIPT")
	if script == "" {
		script = filepath.Join(checkout, "modules", "app", "agent-repl", "bin", "test-all.sh")
	}
	return []string{"bash", script}
}

// errNoRun is returned when a verb addresses an in-flight merge that is not
// there. It is an internal seam failure rather than a user-facing refusal: the
// caller lost track of the run.
var errNoRun = errors.New("merge: no merge is in flight for that workspace")

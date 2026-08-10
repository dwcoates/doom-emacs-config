package main

import (
	"context"
	"errors"
	"path/filepath"
	"testing"

	workspacecreate "claude-repld/internal/workspace/create"
)

// ---------------------------------------------------------------------------
// THE PUBLICATION GATE'S MEMORY. It holds a workspace's session frames until
// the creation job's release opens it, and it is keyed by worktree — so its
// entry must carry the JOB's identity and nothing else. It used to memoize the
// identity of whichever FRAME asked first, including frames that arrive before
// the job has a session at all, and a release could then never match the entry
// it found. One workspace sat behind that permanently-closed gate for 25
// minutes and 6294 hold records with nothing above info severity to say so.
// ---------------------------------------------------------------------------

// gateFixture builds a bridge over a real durable job store holding one
// unmaterialized job for worktree, plus the single release subscriber the
// release protocol requires.
func gateFixture(t *testing.T, job workspacecreate.Job) *WorkspaceCreationBridge {
	t.Helper()
	logf := func(string, ...any) {}
	store, err := workspacecreate.OpenJobStore(filepath.Join(t.TempDir(), "jobs.json"), logf)
	if err != nil {
		t.Fatalf("OpenJobStore: %v", err)
	}
	if _, _, err := store.Enqueue(job); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	// The gate reads the DURABLE STORE; the manager is wired only because the
	// bridge refuses construction without one. Every collaborator below is an
	// inert stand-in these tests never drive.
	inert := inertCreateStage{}
	manager, err := workspacecreate.NewManager(workspacecreate.Config{
		Store: store, Planner: inert, Worktrees: inert, Geometry: inert, Sessions: inert,
		Health: inert, Prompts: inert, Available: inert, Releases: inert, Publication: inert,
		HostActions: inert, Logf: logf, Errorf: logf,
	})
	if err != nil {
		t.Fatalf("NewManager: %v", err)
	}
	bridge, err := NewWorkspaceCreationBridge(context.Background(), manager, store)
	if err != nil {
		t.Fatalf("NewWorkspaceCreationBridge: %v", err)
	}
	return bridge
}

// heldJob is the durable shape of a workspace whose session exists and whose
// host acknowledgement has not arrived.
func heldJob() workspacecreate.Job {
	return workspacecreate.Job{
		ID:           "held",
		Request:      workspacecreate.Request{Name: "DWC/held", GitRoot: "/repo"},
		State:        workspacecreate.StateAwaitingEmacs,
		WorktreePath: "/worktrees/held",
		SessionID:    "s_held",
	}
}

func TestTheGateReportsTheJobsSessionNotTheAskingFrames(t *testing.T) {
	// Arrange — a frame that carries no session id at all, which is exactly
	// what arrives between PrepareSessionPublication and the create that gives
	// the job its session.
	bridge := gateFixture(t, heldJob())

	// Act
	decision, err := bridge.SessionPublicationDecision("/worktrees/held", "")

	// Assert
	if err != nil {
		t.Fatalf("SessionPublicationDecision: %v", err)
	}
	if decision.SessionID != "s_held" || decision.Materialized {
		t.Fatalf("decision = %#v, want a hold naming the job's own session", decision)
	}
}

func TestAnUnsettledHoldIsNotMemoized(t *testing.T) {
	// Arrange — a job with no session yet. A hold cached under an identity that
	// is not settled is the one the release can never match.
	job := heldJob()
	job.SessionID = ""
	bridge := gateFixture(t, job)

	// Act
	if _, err := bridge.SessionPublicationDecision("/worktrees/held", ""); err != nil {
		t.Fatalf("SessionPublicationDecision: %v", err)
	}

	// Assert — nothing frozen, so the next frame re-derives from the store.
	bridge.mu.Lock()
	_, cached := bridge.publication["/worktrees/held"]
	bridge.mu.Unlock()
	if cached {
		t.Fatal("an unsettled hold was memoized")
	}
}

func TestTheReleaseOpensAGateThatWasNeverMemoized(t *testing.T) {
	// Arrange — the release is authoritative: a hold that was never cached is
	// not evidence that this release is wrong.
	bridge := gateFixture(t, heldJob())
	releases, cancel := bridge.SubscribeSessionPublicationReleases()
	defer cancel()
	go func() {
		release := <-releases
		release.Completion <- release.Open()
	}()

	// Act
	err := bridge.ReleaseSessionPublication(context.Background(), workspacecreate.PublicationDecision{
		JobID: "held", WorktreePath: "/worktrees/held", SessionID: "s_held", Materialized: true,
	})

	// Assert
	if err != nil {
		t.Fatalf("ReleaseSessionPublication: %v", err)
	}
	decision, err := bridge.SessionPublicationDecision("/worktrees/held", "s_held")
	if err != nil || !decision.Materialized {
		t.Fatalf("decision after release = %#v err=%v, want the gate open", decision, err)
	}
}

func TestTheReleaseOpensAGateHeldForAFrameWithNoSession(t *testing.T) {
	// Arrange — the exact wedge: a frame with no session asks first, then the
	// job's own release arrives carrying the real session id.
	bridge := gateFixture(t, heldJob())
	if _, err := bridge.SessionPublicationDecision("/worktrees/held", ""); err != nil {
		t.Fatalf("SessionPublicationDecision: %v", err)
	}
	releases, cancel := bridge.SubscribeSessionPublicationReleases()
	defer cancel()
	go func() {
		release := <-releases
		release.Completion <- release.Open()
	}()

	// Act
	err := bridge.ReleaseSessionPublication(context.Background(), workspacecreate.PublicationDecision{
		JobID: "held", WorktreePath: "/worktrees/held", SessionID: "s_held", Materialized: true,
	})

	// Assert — this is the call that used to fail with "release open lost
	// publication decision" and leave the workspace blank forever.
	if err != nil {
		t.Fatalf("ReleaseSessionPublication after a session-less frame: %v", err)
	}
}

func TestTheReleaseRefusesToOpenAnotherJobsGate(t *testing.T) {
	// Arrange — one live workspace has exactly one creation identity, so a
	// release for a different job than the one holding this worktree is an
	// invariant violation rather than a stale entry to overwrite.
	bridge := gateFixture(t, heldJob())
	if _, err := bridge.SessionPublicationDecision("/worktrees/held", "s_held"); err != nil {
		t.Fatalf("SessionPublicationDecision: %v", err)
	}
	releases, cancel := bridge.SubscribeSessionPublicationReleases()
	defer cancel()
	go func() {
		release := <-releases
		release.Completion <- release.Open()
	}()

	// Act
	err := bridge.ReleaseSessionPublication(context.Background(), workspacecreate.PublicationDecision{
		JobID: "someone-else", WorktreePath: "/worktrees/held", SessionID: "s_other", Materialized: true,
	})

	// Assert
	if err == nil {
		t.Fatal("a release for a different creation job opened this workspace's gate")
	}
}

func TestAnAbandonedHoldOpensItsWorktreesGate(t *testing.T) {
	// Arrange — a creation that died before it was ever materialized, whose
	// hold has been given its terminal disposition. Nothing will ever
	// acknowledge it, so the gate must not go on holding its frames.
	job := heldJob()
	job.State = workspacecreate.StateFailed
	job.PublicationAbandoned = true
	job.PublicationAbandonedReason = string(workspacecreate.AbandonTerminalFailure)
	bridge := gateFixture(t, job)

	// Act
	decision, err := bridge.SessionPublicationDecision("/worktrees/held", "s_held")

	// Assert
	if err != nil {
		t.Fatalf("SessionPublicationDecision: %v", err)
	}
	if !decision.Materialized {
		t.Fatal("an abandoned hold is still gating its worktree's frames")
	}
}

func TestAnAbandonedJobIsNotReplayedToTheHost(t *testing.T) {
	// Arrange — the connect snapshot is the second path to the host and the one
	// a daemon restart cannot silence, so it must honour the disposition too.
	job := heldJob()
	job.PublicationAbandoned = true
	job.PublicationAbandonedReason = string(workspacecreate.AbandonWorktreeGone)
	bridge := gateFixture(t, job)

	// Act
	work := bridge.SnapshotHostWork()

	// Assert
	if len(work.WorkspaceAvailable) != 0 {
		t.Fatalf("replayed workspace-available frames = %d, want 0 for an abandoned job", len(work.WorkspaceAvailable))
	}
}

func TestAParkedJobIsStillReplayedToTheHost(t *testing.T) {
	// Arrange — the replay must keep carrying the jobs it exists for.
	bridge := gateFixture(t, heldJob())

	// Act
	work := bridge.SnapshotHostWork()

	// Assert
	if len(work.WorkspaceAvailable) != 1 {
		t.Fatalf("replayed workspace-available frames = %d, want 1", len(work.WorkspaceAvailable))
	}
}

// inertCreateStage satisfies every collaborator the create manager demands at
// construction. These tests exercise the publication gate, which reads the
// durable store directly, so nothing here is ever called.
type inertCreateStage struct{}

func (inertCreateStage) PlanWorktree(context.Context, workspacecreate.Job) (workspacecreate.WorktreeResult, error) {
	return workspacecreate.WorktreeResult{}, errInertCreateStage
}
func (inertCreateStage) EnsureWorktree(context.Context, workspacecreate.Job) error {
	return errInertCreateStage
}
func (inertCreateStage) RecordWorkspaceGeometry(context.Context, workspacecreate.Job) error {
	return errInertCreateStage
}
func (inertCreateStage) EnsureSession(context.Context, workspacecreate.Job) (string, error) {
	return "", errInertCreateStage
}
func (inertCreateStage) AwaitHealthy(context.Context, workspacecreate.Job) error {
	return errInertCreateStage
}
func (inertCreateStage) SubmitInitialPrompt(context.Context, workspacecreate.Job) error {
	return errInertCreateStage
}
func (inertCreateStage) PublishWorkspaceAvailable(context.Context, workspacecreate.Available) error {
	return errInertCreateStage
}
func (inertCreateStage) ReleaseSessionPublication(context.Context, workspacecreate.PublicationDecision) error {
	return errInertCreateStage
}
func (inertCreateStage) PrepareSessionPublication(context.Context, workspacecreate.Job) error {
	return errInertCreateStage
}
func (inertCreateStage) PublishHostAction(context.Context, workspacecreate.HostAction) error {
	return errInertCreateStage
}

var errInertCreateStage = errors.New("inert create stage: these tests drive the publication gate only")

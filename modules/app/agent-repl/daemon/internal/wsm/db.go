package wsm

import (
	"context"
	"time"

	// The daemon's SQLite driver. Registered here because wsm is the sole
	// owner of the database handle; nothing else opens it.
	_ "modernc.org/sqlite"
)

// DB is the daemon's durable state. One handle, one writer: the
// implementation opens a single *sql.DB with SetMaxOpenConns(1), the DSN
// carrying _pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)&
// _txlock=immediate. A layout version table gates the open; a newer layout
// refuses to open. Every load is all-or-nothing — a corrupt row fails the
// whole read, and nothing is loaded.
type DB interface {
	// Close releases the handle.
	Close() error
	// ReadOnly reports whether this handle was opened read-only.
	ReadOnly() bool
	// Promote turns a READ-ONLY handle into a writing one, in place. It exists
	// for the handover's successor, which opens read-only because the
	// incumbent is still the sole writer and becomes a writer at its first
	// adoption. Promoting a handle that already writes is success.
	Promote(ctx context.Context) error

	// RegisterWorkspace records a workspace, idempotent by normalized dir,
	// minting a WorkspaceID and a RepoID on first sight. The bool reports
	// whether the record was created.
	RegisterWorkspace(ctx context.Context, dir string, facts RegisterFacts) (Workspace, bool, error)
	// Workspace loads one workspace by id.
	Workspace(ctx context.Context, id WorkspaceID) (Workspace, error)
	// WorkspaceByDir loads one workspace by its normalized worktree directory.
	WorkspaceByDir(ctx context.Context, dir string) (Workspace, error)
	// ListWorkspaces loads every workspace, all-or-nothing.
	ListWorkspaces(ctx context.Context) ([]Workspace, error)
	// ListRepositories loads every repository, all-or-nothing.
	ListRepositories(ctx context.Context) ([]Repository, error)
	// SetClosed records whether a workspace's editor state is torn down.
	SetClosed(ctx context.Context, id WorkspaceID, closed bool) error
	// SetCurrent records the user's selection of a workspace at an instant.
	SetCurrent(ctx context.Context, id WorkspaceID, at time.Time) error
	// Current reports the currently selected workspace, nil when none is.
	Current(ctx context.Context) (*WorkspaceID, error)
	// SetPriority sets or clears a workspace's roster priority.
	SetPriority(ctx context.Context, id WorkspaceID, p *Priority) error
	// SetAttention sets or clears the roster's attention marker.
	SetAttention(ctx context.Context, id WorkspaceID, on bool) error
	// SetMergedAt records that the workspace's merge landed.
	SetMergedAt(ctx context.Context, id WorkspaceID, at time.Time) error
	// Forget deletes a workspace's every record — the nuke's durable half.
	Forget(ctx context.Context, id WorkspaceID) error

	// PutCreationJob records a workspace's merge geometry, configured actions
	// and materialization state.
	PutCreationJob(ctx context.Context, job CreationJob) error
	// CreationJob loads one workspace's creation job; the bool reports
	// existence. Merge refuses rather than guessing when it is absent.
	CreationJob(ctx context.Context, id WorkspaceID) (CreationJob, bool, error)

	// PutSession records a workspace's session binding and spawn identity.
	PutSession(ctx context.Context, s Session) error
	// Session loads one workspace's session; the bool reports existence.
	Session(ctx context.Context, id WorkspaceID) (Session, bool, error)
	// SetSessionTerminal records a session's death with its cause. A deleted
	// session refuses resurrection.
	SetSessionTerminal(ctx context.Context, id WorkspaceID, t SessionTerminal) error
	// TouchEngagement records last engagement — the idle sweep's input.
	TouchEngagement(ctx context.Context, id WorkspaceID, at time.Time) error
	// SetShimPID records, or clears with nil, the pid of the shim process
	// serving a workspace's session. The rollout's intent manifest names it.
	SetShimPID(ctx context.Context, id WorkspaceID, pid *int) error

	// AcquireLease takes the workspace's occupancy lease for holder under
	// policy, refusing when it is already held.
	AcquireLease(ctx context.Context, id WorkspaceID, holder LeaseHolder, policy LeasePolicy) (Lease, error)
	// AcquireLeaseAs acquires it under an identity the CALLER minted. The
	// merge's lease id is also its LEDGER identity -- the merge bubble is
	// addressed by it and drawn from the moment the merge is QUEUED, before
	// any occupancy is taken -- so it cannot be minted at acquisition.
	AcquireLeaseAs(ctx context.Context, id WorkspaceID, lease LeaseID, holder LeaseHolder, policy LeasePolicy) (Lease, error)
	// ReleaseLease releases one acquisition.
	ReleaseLease(ctx context.Context, leaseID LeaseID) error
	// Lease loads a workspace's current lease; the bool reports whether one is
	// held.
	Lease(ctx context.Context, id WorkspaceID) (Lease, bool, error)
	// SetLeasePolicy changes what a held lease projects onto new submissions
	// (a merge moving from refusing to parked).
	SetLeasePolicy(ctx context.Context, leaseID LeaseID, p LeasePolicy) error

	// PutHeldPrompt records a parked submission. WSM is the one durable hold
	// store.
	PutHeldPrompt(ctx context.Context, h HeldPrompt) error
	// UpdateHeldPromptClassification records the classifier's verdict.
	UpdateHeldPromptClassification(ctx context.Context, turn TurnID, c Classification) error
	// UpdateHeldPromptHold changes or clears the daemon-side condition holding a
	// prompt. scheduleID is the drain schedule a HoldShutdown waits on and is
	// required for that arm, empty for every other kind.
	UpdateHeldPromptHold(ctx context.Context, turn TurnID, h *HoldKind, scheduleID string) error
	// SetHeldPromptAccepted records the user's acceptance of the tray's offer to
	// let the prompt wait for the turn's end. Legal ONLY on a hold_for_turn_end
	// verdict.
	SetHeldPromptAccepted(ctx context.Context, turn TurnID) error
	// TombstoneHeldPrompt retires a held prompt with its reason.
	TombstoneHeldPrompt(ctx context.Context, turn TurnID, why Tombstone) error
	// HeldPrompts loads one workspace's standing holds, all-or-nothing.
	HeldPrompts(ctx context.Context, id WorkspaceID) ([]HeldPrompt, error)
	// AllHeldPrompts loads every standing hold for the boot restore,
	// all-or-nothing: a corrupt row fails the read and nothing is loaded.
	AllHeldPrompts(ctx context.Context) ([]HeldPrompt, error)

	// PutTurn records a turn's durable origin and address.
	PutTurn(ctx context.Context, t Turn) error
	// CloseTurn stamps a turn's close.
	CloseTurn(ctx context.Context, turn TurnID, at time.Time, how TurnClose) error
	// OpenTurns loads a workspace's turns that have no terminal.
	OpenTurns(ctx context.Context, id WorkspaceID) ([]Turn, error)
	// AllDisplacedTurns loads every turn still marked displaced, across every
	// workspace and REGARDLESS of whether the turn is still open: a merge
	// displaces a turn by ending it, so the record a boot has to put back is
	// normally a closed one. It is the boot recovery's whole input.
	AllDisplacedTurns(ctx context.Context) ([]Turn, error)
	// RetireDisplacedTurn clears a turn's displaced mark and, for a turn still
	// open, stamps its close in the SAME transaction. Retiring is what makes
	// the resubmission exactly-once: a record whose mark is down is nobody's
	// to put back.
	RetireDisplacedTurn(ctx context.Context, turn TurnID, at time.Time) error
	// ClaimIdempotencyKey binds a client's key to a turn. When the key is
	// already claimed it returns the existing turn and mints nothing.
	ClaimIdempotencyKey(ctx context.Context, id WorkspaceID, key string, turn TurnID) (*TurnID, error)

	// CloseOrphans closes every turn without a terminal in one transaction and
	// reports what it closed.
	CloseOrphans(ctx context.Context, id WorkspaceID, at time.Time) (OrphanReport, error)

	// CreateTask records a new user task.
	CreateTask(ctx context.Context, title string) (Task, error)
	// UpdateTask applies one mutation to a task.
	UpdateTask(ctx context.Context, id TaskID, change TaskChange) error
	// Tasks loads every task, all-or-nothing.
	Tasks(ctx context.Context) ([]Task, error)
	// AssignWorkspaceTask assigns a workspace to a task, or unassigns it when
	// task is nil.
	AssignWorkspaceTask(ctx context.Context, id WorkspaceID, task *TaskID) error

	// OpenMergeLedger opens a workspace's ledger for one merge lease.
	OpenMergeLedger(ctx context.Context, id WorkspaceID, lease LeaseID) error
	// RecordTabInterval appends one round's tab interval to a lease's ledger.
	RecordTabInterval(ctx context.Context, lease LeaseID, interval TabInterval) error
	// MergeLedger loads a workspace's ledger entries, all-or-nothing.
	MergeLedger(ctx context.Context, id WorkspaceID) ([]MergeLedgerEntry, error)

	// EnqueueMerge puts a workspace in its target repository's DURABLE merge
	// queue and returns its one-based position, refusing a workspace already
	// queued there.
	EnqueueMerge(ctx context.Context, repo RepoKey, id WorkspaceID, at time.Time) (int, error)
	// AdmitMerge marks the entry the orchestrator is running now.
	AdmitMerge(ctx context.Context, repo RepoKey, id WorkspaceID) error
	// RemoveMergeQueueEntry drops one entry with the cause it was dropped for.
	RemoveMergeQueueEntry(ctx context.Context, repo RepoKey, id WorkspaceID, cause string) error
	// MergeQueue loads one repository's queue in order, all-or-nothing.
	MergeQueue(ctx context.Context, repo RepoKey) ([]MergeQueueEntry, error)
	// AllMergeQueues loads every repository's queue for the boot re-enqueue,
	// all-or-nothing.
	AllMergeQueues(ctx context.Context) (map[RepoKey][]MergeQueueEntry, error)
	// SetMergeQueuePaused pauses or resumes one repository's queue.
	SetMergeQueuePaused(ctx context.Context, repo RepoKey, paused bool) error
	// MergeQueuePaused reports whether one repository's queue is paused.
	MergeQueuePaused(ctx context.Context, repo RepoKey) (bool, error)

	// OpenFault records a fault and returns its id.
	OpenFault(ctx context.Context, f Fault) (FaultID, error)
	// CloseFault stamps a fault's persisted resolved-at.
	CloseFault(ctx context.Context, id FaultID, at time.Time) error
	// OpenFaults loads the open faults matching scope, all-or-nothing.
	OpenFaults(ctx context.Context, scope FaultScope) ([]Fault, error)
	// Fault loads one fault by id, open or resolved — the read that proves a
	// closing edge was persisted rather than reopening on the next boot.
	Fault(ctx context.Context, id FaultID) (Fault, error)

	// PutDrainSchedule puts a drain schedule in force, replacing any current
	// one.
	PutDrainSchedule(ctx context.Context, s DrainSchedule) error
	// ClearDrainSchedule cancels the schedule in force.
	ClearDrainSchedule(ctx context.Context) error
	// DrainSchedule loads the schedule in force, nil when none is.
	DrainSchedule(ctx context.Context) (*DrainSchedule, error)

	// ClaimServing records this daemon instance as the workspace's serving
	// owner — the handover's per-workspace transfer.
	ClaimServing(ctx context.Context, id WorkspaceID, daemon InstanceID) error
	// Serving reports which daemon instance serves a workspace, nil when none
	// does.
	Serving(ctx context.Context, id WorkspaceID) (*InstanceID, error)
	// ReleaseServing gives up serving ownership, refusing when this instance
	// does not hold it.
	ReleaseServing(ctx context.Context, id WorkspaceID, daemon InstanceID) error
}

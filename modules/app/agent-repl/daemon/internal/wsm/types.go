// Package wsm is the daemon's state client: the sole owner of the durable
// fact inventory and of the occupancy lease's policy metadata.
//
// It holds no orchestration logic. Peers call it; it calls only the database.
// See docs/overhaul/daemon.md decision 3 "WSM, THE STATE CLIENT" and
// ARCHITECTURE.md "wsm (internal/wsm)".
package wsm

import (
	"time"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// The identity newtypes are aliases of internal/ids, the leaf that holds one
// spelling of each. Aliasing rather than redeclaring is what lets feedid —
// which sits below wsm and may not import it — name the same types.
type (
	// WorkspaceID is the daemon-minted, opaque workspace identity.
	WorkspaceID = ids.WorkspaceID
	// RepoID is the daemon-minted repository identity.
	RepoID = ids.RepoID
	// InstanceID identifies one daemon process.
	InstanceID = ids.InstanceID
	// LeaseID identifies one acquisition of a workspace's occupancy lease.
	LeaseID = ids.LeaseID
	// TurnID is the daemon-minted turn identity.
	TurnID = ids.TurnID
	// TaskID identifies one user task.
	TaskID = ids.TaskID
	// FaultID identifies one recorded fault.
	FaultID = ids.FaultID
)

// Workspace is one registered workspace's durable record.
type Workspace struct {
	ID WorkspaceID
	// Repo is the repository the worktree belongs to.
	Repo RepoID
	// Dir is the normalized worktree directory.
	Dir string
	// Name is the workspace's display name (the creation slug, or the
	// directory's base name for a registered one).
	Name string
	// Branch is the worktree's branch.
	Branch string
	// ParentBranch is the base the branch was cut from.
	ParentBranch string
	// Closed reports whether the workspace's editor state has been torn down.
	Closed bool
	// Attention marks the roster's attention badge; set on a host
	// notification, cleared on SelectWorkspace.
	Attention bool
	// Priority is the roster's ordering priority, nil when unset.
	Priority *Priority
	// Task is the task this workspace is assigned to, nil when unassigned.
	Task *TaskID
	// LastSelectedAt is when the user last selected the workspace.
	LastSelectedAt *time.Time
	// MergedAt is when the workspace's merge landed, nil when it has not.
	MergedAt *time.Time
	// CreatedAt is when the record was minted.
	CreatedAt time.Time
}

// Repository is one repository's durable record.
type Repository struct {
	ID RepoID
	// Dir is the repository's canonicalized common dir (symlinks resolved).
	Dir string
	// Name is the repository's display name.
	Name string
	// DefaultBranch is the repository's default branch.
	DefaultBranch string
}

// RegisterFacts are the facts Emacs supplies when it announces a workspace.
// Registration is idempotent by normalized dir; the daemon mints the ids.
type RegisterFacts struct {
	// Name is the display name, empty to let the daemon derive one.
	Name string
	// Branch is the worktree's current branch.
	Branch string
	// ParentBranch is the base the branch was cut from, empty when unknown.
	ParentBranch string
	// RepoDir is the repository's canonicalized common dir.
	RepoDir string
}

// Priority is the roster's ordering priority, mirroring
// agentrepl.v1.WorkspacePriority's arms.
type Priority int

// The priority values, highest first.
const (
	PriorityP05 Priority = iota
	PriorityP1
	PriorityP2
	PriorityP3
)

// CreationJob is a workspace's lifecycle record from before any session
// exists: where it is being materialized, and what the merge orchestrator must
// read back later. Merge geometry is REFUSED rather than guessed when absent.
type CreationJob struct {
	Workspace WorkspaceID
	// Layout is the merge geometry recorded at creation.
	Layout MergeLayout
	// Actions are the configured before/after merge actions.
	Actions MergeActions
	// BaseRef is the resolved base the branch was cut from.
	BaseRef string
	// Materialized reports whether the worktree exists on disk. Registration
	// happens only after materialization.
	Materialized bool
	// OneShot marks a one-shot workspace (created, prompted, merged, closed).
	OneShot bool
	// InitialPrompt is the prompt the workspace was created with, empty when
	// created without one.
	InitialPrompt string
	// ConsentedUngatedMode records the permission mode the user consented to at
	// creation, empty when none was.
	ConsentedUngatedMode string
	// CreatedAt is when the job was recorded.
	CreatedAt time.Time
}

// MergeLayout is a workspace's merge geometry: recorded at creation, never
// inferred later.
type MergeLayout struct {
	// SourceBranch is the branch that merges.
	SourceBranch string
	// SourceDir is the worktree that branch lives in.
	SourceDir string
	// TargetDir is the checkout the merge lands in.
	TargetDir string
	// Origin names where the geometry came from (the creating verb).
	Origin string
}

// MergeActions are the configured prompts run in the workspace before and
// after its merge.
type MergeActions struct {
	// Before are prompt names run in the source workspace before the merge.
	Before []string
	// After are prompt names run after the merge lands.
	After []string
}

// Session is a workspace's session binding plus the facts that outlive one
// shim process.
type Session struct {
	Workspace WorkspaceID
	// VendorSessionID is the vendor's session identity, for resume.
	VendorSessionID string
	// ConfigDir is the account root the session was spawned under.
	ConfigDir string
	// Model is the last-writer-wins model fact, in shim order.
	Model string
	// PermissionMode is the session's current permission mode.
	PermissionMode string
	// StartedAt is when the session was first started.
	StartedAt time.Time
	// LastEngagementAt is the idle sweep's input.
	LastEngagementAt time.Time
	// Terminal is the session's death, nil while it lives. A deleted session
	// REFUSES resurrection.
	Terminal *SessionTerminal
}

// SessionTerminal is a session's death with its cause.
type SessionTerminal struct {
	// Kind names the terminal ("deleted", "killed", "shim_died", "superseded").
	Kind string
	// Detail is the cause, kept as evidence.
	Detail string
	// At is when the session died.
	At time.Time
}

// Lease is the workspace's occupancy lease row: the POLICY metadata behind the
// kernel lock that does the arbitration. The lock decides, the row describes.
type Lease struct {
	ID        LeaseID
	Workspace WorkspaceID
	// Holder is the peer that holds it.
	Holder LeaseHolder
	// Policy is what the holder projects onto new submissions.
	Policy LeasePolicy
	// AcquiredAt is when it was taken.
	AcquiredAt time.Time
}

// LeaseHolder is the peer holding a workspace's occupancy lease.
type LeaseHolder int

// The lease holders.
const (
	// HolderMerge is the merge orchestrator.
	HolderMerge LeaseHolder = iota
	// HolderRestart is a shim relaunch (restart-pending).
	HolderRestart
	// HolderDrain is the shutdown drain.
	HolderDrain
	// HolderHibernate is the idle sweep's hibernation.
	HolderHibernate
)

// LeasePolicy is what a lease projects onto new prompt submissions.
type LeasePolicy int

// The lease policies.
const (
	// PolicyRefuse errors new submissions (the merge lease: post-merge-start
	// work would be orphaned because a merged workspace closes).
	PolicyRefuse LeasePolicy = iota
	// PolicyHold parks new submissions until release (restart, drain).
	PolicyHold
	// PolicyParked routes new submissions to the resolution agent (a merge
	// parked on conflicts or test failures).
	PolicyParked
)

// OutputAddress is where a lease holder wants the session's output to land:
// the feed resolver applies it to every row the session produces while the
// lease is held.
type OutputAddress struct {
	// Feed is the feed rows land in.
	Feed feedid.Feed
	// Parent is the row they nest under, nil for top-level rows.
	Parent *feedid.Ref
}

// HeldPrompt is one parked submission. WSM is the ONE durable hold store.
type HeldPrompt struct {
	Workspace WorkspaceID
	// Turn is the minted turn the prompt will run as when released.
	Turn TurnID
	// Text is the submission's full text (the metaprompt sentinel spans are
	// stripped only for display, never on the record).
	Text string
	// Origin is the prompt's origin, as conversation.v1.PromptOrigin names it.
	Origin string
	// Target, when set, is the bubble composer's addressed row.
	Target *feedid.Ref
	// Hold is why it is held, nil once it is deliverable.
	Hold *HoldKind
	// Classification is the classifier's verdict, nil until judged.
	Classification *Classification
	// Tombstone is why it was retired, nil while it stands.
	Tombstone *Tombstone
	// QueuedAt is when it was submitted.
	QueuedAt time.Time
}

// HoldKind is why a prompt is held, mirroring frontend.v1's held-prompt arms.
type HoldKind int

// The hold kinds.
const (
	// HoldClassifying is awaiting the classifier's verdict.
	HoldClassifying HoldKind = iota
	// HoldForTurnEnd waits for the running turn to end.
	HoldForTurnEnd
	// HoldUninterruptibleTurn waits because the running turn refuses interrupts.
	HoldUninterruptibleTurn
	// HoldClassificationError holds after the classifier failed.
	HoldClassificationError
	// HoldShutdown holds for the shutdown drain's lease.
	HoldShutdown
	// HoldKeepAlive holds inside a keep-alive window.
	HoldKeepAlive
	// HoldSessionStarting holds while the session is still coming up.
	HoldSessionStarting
	// HoldBuildRefresh holds across a build-staleness bounce.
	HoldBuildRefresh
)

// Classification is the classifier's verdict on a held prompt.
type Classification struct {
	// Interject reports whether the prompt interrupts the running turn.
	Interject bool
	// Reason is the judge's stated reason, kept as evidence.
	Reason string
	// Failed reports that judging errored; the prompt holds with
	// HoldClassificationError.
	Failed bool
	// At is when the verdict landed.
	At time.Time
}

// Tombstone is why a held prompt was retired.
type Tombstone struct {
	// Kind names the retirement ("delivered", "dropped", "session_deleted").
	Kind string
	// At is when it was retired.
	At time.Time
}

// Turn is a turn's durable record: the origin that survives a restart, the
// displaced capture a merge takes, and the idempotency claim.
type Turn struct {
	ID        TurnID
	Workspace WorkspaceID
	// Text is the submission's full text.
	Text string
	// Origin is the prompt's origin.
	Origin string
	// Address is where the turn's output lands, nil for the root feed.
	Address *OutputAddress
	// Displaced marks a turn captured by a lease holder for exactly-once
	// resubmission at release.
	Displaced bool
	// StartedAt is when the turn was opened.
	StartedAt time.Time
	// ClosedAt is when it closed, nil while it runs.
	ClosedAt *time.Time
	// Close is how it ended, nil while it runs.
	Close *TurnClose
}

// TurnClose is how a turn ended.
type TurnClose int

// The turn closes.
const (
	// CloseCompleted is an ordinary agent-side completion.
	CloseCompleted TurnClose = iota
	// CloseFailed is an agent-side failure.
	CloseFailed
	// CloseKilled is a user interrupt.
	CloseKilled
	// CloseOrphaned is a close written for a turn that had no terminal when the
	// daemon reconciled.
	CloseOrphaned
)

// OrphanReport is what CloseOrphans closed, in one transaction.
type OrphanReport struct {
	// Turns are the turns closed as orphans.
	Turns []TurnID
	// At is the instant stamped on every close.
	At time.Time
}

// Task is one user task the roster's task view renders.
type Task struct {
	ID TaskID
	// Title is the task's text.
	Title string
	// Done reports whether it is complete.
	Done bool
	// CreatedAt is when it was created.
	CreatedAt time.Time
}

// TaskChange is one mutation of a task; a nil field is left alone.
type TaskChange struct {
	// Title retitles the task when set.
	Title *string
	// Done marks it done or reopens it when set.
	Done *bool
}

// TabInterval is one merge round's tab on the merge bubble's sub-feed:
// append-only, round-numbered.
type TabInterval struct {
	// Round is the tab's round number, from one.
	Round int
	// Kind names the tab ("pre_prompt", "merge", "conflicts", "tests", "fixes",
	// "rollout", "post_prompt").
	Kind string
	// StartedAt is when the round opened.
	StartedAt time.Time
	// EndedAt is when it closed, nil while it runs.
	EndedAt *time.Time
	// Outcome is the round's result, empty while it runs.
	Outcome string
}

// MergeLedgerEntry is one workspace's merge-lease ledger row: the lease and
// every tab interval recorded under it.
type MergeLedgerEntry struct {
	Workspace WorkspaceID
	Lease     LeaseID
	// Intervals are the rounds recorded under that lease, in order.
	Intervals []TabInterval
	// OpenedAt is when the ledger was opened.
	OpenedAt time.Time
}

// FaultScope narrows an OpenFaults query.
type FaultScope struct {
	// Workspace, when set, restricts to that workspace's faults.
	Workspace *WorkspaceID
	// Kind, when set, restricts to that fault kind.
	Kind string
}

// Fault is one recorded fault, open until it is explicitly closed. The
// resolved-at instant is persisted, so a fault's history survives a restart.
type Fault struct {
	ID FaultID
	// Workspace is the faulting workspace, nil for a daemon-scoped fault.
	Workspace *WorkspaceID
	// Kind names the fault, from the frontend.v1 failure vocabulary.
	Kind string
	// Detail is the evidence.
	Detail string
	// OpenedAt is when it was raised.
	OpenedAt time.Time
	// ResolvedAt is when it was closed, nil while open.
	ResolvedAt *time.Time
}

// DrainSchedule is the deploy tooling's drain-and-exit control: at most one is
// in force at a time.
type DrainSchedule struct {
	// Reason is why the daemon is draining, from agentrepl.v1.DrainReason.
	Reason string
	// Deadline is when the daemon exits regardless of outstanding work.
	Deadline time.Time
	// SetAt is when the schedule was put in force.
	SetAt time.Time
}

// RepoKey identifies one repository's merge queue: the TARGET repository's
// canonical common dir. It is a path rather than a RepoID because the queue is
// keyed by what a merge lands in, which the orchestrator knows before it knows
// any workspace's registered repository.
type RepoKey string

// MergeQueueState is where one queue entry stands.
type MergeQueueState int

// The merge queue states.
const (
	// MergeQueued is waiting for its repository's turn.
	MergeQueued MergeQueueState = iota
	// MergeAdmitted is the entry the orchestrator is running now.
	MergeAdmitted
)

// MergeQueueEntry is one workspace's place in its repository's merge queue. The
// queue is DURABLE so a restart re-enqueues exactly what was waiting, in the
// order it was waiting in.
type MergeQueueEntry struct {
	// Repo is the queue's repository key.
	Repo RepoKey
	// Workspace is the workspace whose merge is queued.
	Workspace WorkspaceID
	// Position is the entry's one-based place in the queue's order.
	Position int
	// State is where the entry stands.
	State MergeQueueState
	// EnqueuedAt is when it joined the queue.
	EnqueuedAt time.Time
}

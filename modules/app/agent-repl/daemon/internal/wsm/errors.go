package wsm

import (
	"errors"
	"fmt"
)

// ErrNotFound is returned when a lookup names a record that does not exist.
// It is never softened into a zero value: an unknown id is a refusal.
var ErrNotFound = errors.New("wsm: not found")

// ErrReadOnly is returned by every write on a handle opened with
// OpenReadOnly. The read-only mode is guaranteed to change nothing, so the
// refusal happens before any statement is prepared.
var ErrReadOnly = errors.New("wsm: handle is read-only")

// ErrSessionDeleted is returned when a session whose terminal is "deleted" is
// asked to come back. A deleted session REFUSES resurrection.
var ErrSessionDeleted = errors.New("wsm: session was deleted and refuses resurrection")

// ErrSessionIdentityMissing is returned when a session row is written with no
// host session identity. The identity is what Emacs correlates transcripts,
// health probes and fault windows against, and a row without one is not a
// session the host view can ever be composed from — so the write is REFUSED
// here rather than persisted to fail every later compose forever.
var ErrSessionIdentityMissing = errors.New("wsm: a session record carries no host session id")

// ErrTombstoned is returned when a retired held prompt is written to. A
// tombstoned hold never resurrects.
var ErrTombstoned = errors.New("wsm: held prompt is tombstoned")

// ErrMergeLeaseGone refuses a merge hold recorded after the merge it waits on
// released its lease: the merge has already decided whether its requester
// closes, so the prompt must take the path a workspace with no merge takes.
var ErrMergeLeaseGone = errors.New("wsm: no merge lease stands to hold the prompt")

// LayoutError refuses a database file whose layout version this build cannot
// interpret. It is NOT the ordinary answer to a version mismatch: a file
// stamped OLDER is migrated forward (see migrate.go), because the workspace
// state is the user's data. This refusal is what is left over — a file stamped
// NEWER, which a downgrade would silently strip, and a file so old that no
// chain of migrations in this build reaches it.
type LayoutError struct {
	// Path is the database file that was refused.
	Path string
	// File is the layout version stamped in the file.
	File int
	// Binary is the layout version this build writes.
	Binary int
	// Reason says why this particular layout could not be interpreted, so the
	// message a person reads names the actual obstacle rather than the
	// mismatch they can already see.
	Reason string
}

// Error implements error.
func (e *LayoutError) Error() string {
	rel := "older than"
	if e.File > e.Binary {
		rel = "newer than"
	}
	reason := e.Reason
	if reason == "" {
		reason = "this build cannot interpret it"
	}
	return fmt.Sprintf("wsm: database %q has layout version %d, %s this build's %d; %s", e.Path, e.File, rel, e.Binary, reason)
}

// MigrationError refuses an open because a migration step failed. The step ran
// in one transaction, so the file is exactly as it was before the step; Backup
// names the copy taken before ANY step ran, so a person has somewhere to go
// back to even if the file is later touched by something else.
type MigrationError struct {
	// Path is the database being migrated.
	Path string
	// From is the layout version the file carried when the open began.
	From int
	// To is the layout version the failing step was carrying it to.
	To int
	// Backup is the pre-migration copy of the file.
	Backup string
	// Err is the underlying cause.
	Err error
}

// Error implements error.
func (e *MigrationError) Error() string {
	return fmt.Sprintf("wsm: migrating database %q from layout version %d to %d failed and was rolled back: %v; the file still stands at layout %d and a pre-migration copy is at %q",
		e.Path, e.From, e.To, e.Err, e.From, e.Backup)
}

// Unwrap exposes the cause.
func (e *MigrationError) Unwrap() error { return e.Err }

// DecodeError fails a WHOLE load because one row could not be decoded. Reads
// are all-or-nothing: a corrupt row never degrades into a partial list or a
// fabricated default, so this error names the table and the row that failed.
type DecodeError struct {
	// Table is the table the bad row lives in.
	Table string
	// Row is the row's primary key, as text.
	Row string
	// Field is the column that could not be decoded, empty when the row failed
	// as a whole.
	Field string
	// Err is the underlying cause.
	Err error
}

// Error implements error.
func (e *DecodeError) Error() string {
	if e.Field != "" {
		return fmt.Sprintf("wsm: decode %s row %q field %q: %v", e.Table, e.Row, e.Field, e.Err)
	}
	return fmt.Sprintf("wsm: decode %s row %q: %v", e.Table, e.Row, e.Err)
}

// Unwrap exposes the cause.
func (e *DecodeError) Unwrap() error { return e.Err }

// LeaseHeldError refuses an acquisition because the workspace's occupancy
// lease is already held. It carries the CURRENT holder so the caller can say
// who is in the way rather than retrying blindly.
type LeaseHeldError struct {
	// Workspace is the contended workspace.
	Workspace WorkspaceID
	// Lease is the acquisition in force.
	Lease LeaseID
	// Holder is the peer that holds it.
	Holder LeaseHolder
	// Policy is what that holder projects onto new submissions.
	Policy LeasePolicy
}

// Error implements error.
func (e *LeaseHeldError) Error() string {
	return fmt.Sprintf("wsm: workspace %s lease already held by %s (lease %s, policy %s)", e.Workspace, e.Holder, e.Lease, e.Policy)
}

// ServingError refuses a serving-ownership release by an instance that does
// not hold it — the handover's per-workspace transfer never lets a bystander
// give away another daemon's workspace.
type ServingError struct {
	// Workspace is the workspace whose ownership was contested.
	Workspace WorkspaceID
	// Holder is the instance that actually serves it, empty when none does.
	Holder InstanceID
	// Claimant is the instance that tried to release it.
	Claimant InstanceID
}

// Error implements error.
func (e *ServingError) Error() string {
	if e.Holder == "" {
		return fmt.Sprintf("wsm: workspace %s is served by no instance; %s cannot release it", e.Workspace, e.Claimant)
	}
	return fmt.Sprintf("wsm: workspace %s is served by %s, not %s", e.Workspace, e.Holder, e.Claimant)
}

// String names a lease holder, for logs and refusals.
func (h LeaseHolder) String() string {
	switch h {
	case HolderMerge:
		return "merge"
	case HolderRestart:
		return "restart"
	case HolderDrain:
		return "drain"
	case HolderHibernate:
		return "hibernate"
	default:
		return fmt.Sprintf("holder(%d)", int(h))
	}
}

// valid reports whether the holder is one of the declared arms.
func (h LeaseHolder) valid() bool { return h >= HolderMerge && h <= HolderHibernate }

// String names a lease policy, for logs and refusals.
func (p LeasePolicy) String() string {
	switch p {
	case PolicyRefuse:
		return "refuse"
	case PolicyHold:
		return "hold"
	case PolicyParked:
		return "parked"
	default:
		return fmt.Sprintf("policy(%d)", int(p))
	}
}

// valid reports whether the policy is one of the declared arms.
func (p LeasePolicy) valid() bool { return p >= PolicyRefuse && p <= PolicyParked }

// valid reports whether the priority is one of the declared arms.
func (p Priority) valid() bool { return p >= PriorityP05 && p <= PriorityP3 }

// valid reports whether the hold kind is one of the declared arms.
func (h HoldKind) valid() bool { return h >= HoldShutdown && h <= HoldMerge }

// String names a hold kind, for logs and refusals.
func (h HoldKind) String() string {
	switch h {
	case HoldShutdown:
		return "shutdown"
	case HoldSessionStarting:
		return "session_starting"
	case HoldBuildRefresh:
		return "build_refresh"
	case HoldMerge:
		return "merge"
	default:
		return fmt.Sprintf("hold_kind(%d)", int(h))
	}
}

// valid reports whether the delivery is one of the declared deliveries.
func (d Delivery) valid() bool { return d >= DeliveryOrdinary && d <= DeliveryDeferred }

// String names a delivery, for logs.
func (d Delivery) String() string {
	switch d {
	case DeliveryOrdinary:
		return "ordinary"
	case DeliveryDeferred:
		return "deferred"
	default:
		return fmt.Sprintf("delivery(%d)", int(d))
	}
}

// valid reports whether the classification arm is one of the declared arms.
func (a ClassificationArm) valid() bool { return a >= ArmClassifying && a <= ArmAfterToolCall }

// String names a classification arm, for logs and refusals.
func (a ClassificationArm) String() string {
	switch a {
	case ArmClassifying:
		return "classifying"
	case ArmInterject:
		return "interject"
	case ArmHoldForTurnEnd:
		return "hold_for_turn_end"
	case ArmUninterruptibleTurn:
		return "uninterruptible_turn"
	case ArmClassificationError:
		return "classification_error"
	case ArmAfterToolCall:
		return "after_tool_call"
	default:
		return fmt.Sprintf("classification_arm(%d)", int(a))
	}
}

// ErrAcceptNotOffered refuses an accept on a verdict that never offered one.
// Accepting is legal ONLY on a hold_for_turn_end verdict.
var ErrAcceptNotOffered = errors.New("wsm: accept is legal only on a hold_for_turn_end verdict")

// valid reports whether the turn close is one of the declared arms.
func (c TurnClose) valid() bool { return c >= CloseCompleted && c <= CloseFolded }

// String names a turn close for a log record; an undeclared close is named by
// its number.
func (c TurnClose) String() string {
	switch c {
	case CloseCompleted:
		return "completed"
	case CloseFailed:
		return "failed"
	case CloseKilled:
		return "killed"
	case CloseOrphaned:
		return "orphaned"
	case CloseAgentDied:
		return "agent_died"
	case CloseFolded:
		return "folded"
	default:
		return fmt.Sprintf("close(%d)", int(c))
	}
}

// String names a merge queue state, for logs and refusals.
func (s MergeQueueState) String() string {
	switch s {
	case MergeQueued:
		return "queued"
	case MergeAdmitted:
		return "admitted"
	case MergeRequested:
		return "requested"
	default:
		return fmt.Sprintf("merge_queue_state(%d)", int(s))
	}
}

// valid reports whether the merge queue state is one of the declared arms.
func (s MergeQueueState) valid() bool { return s >= MergeQueued && s <= MergeRequested }

// String names a merge source kind, for logs and refusals.
func (k MergeSourceKind) String() string {
	switch k {
	case MergeSourceOwnBranch:
		return "own_branch"
	case MergeSourceWorkspace:
		return "workspace"
	case MergeSourceBranch:
		return "branch"
	case MergeSourceMergedUpstream:
		return "merged_upstream"
	default:
		return fmt.Sprintf("merge_source(%d)", int(k))
	}
}

// valid reports whether the source kind is one of the declared arms.
func (k MergeSourceKind) valid() bool {
	return k >= MergeSourceOwnBranch && k <= MergeSourceMergedUpstream
}

// validate refuses a source whose fields contradict its arm: each arm carries
// exactly its own field, so a stored row cannot name a merge nobody asked for.
func (m MergeSource) validate() error {
	if !m.Kind.valid() {
		return fmt.Errorf("unknown merge source kind %d", int(m.Kind))
	}
	if m.KeepOpen && !m.Kind.ClosesRequester() {
		return fmt.Errorf("a %s source cannot keep the requester open", m.Kind)
	}
	if (m.Workspace != "") != (m.Kind == MergeSourceWorkspace) {
		return fmt.Errorf("a %s source names workspace %q", m.Kind, m.Workspace)
	}
	// A WORKSPACE'S BRANCH IS RECORDED WITH THE REQUEST (the branch checked out
	// in its worktree), so every arm may name one, and the branch arm must.
	if m.Kind == MergeSourceBranch && m.Branch == "" {
		return fmt.Errorf("a %s source names no branch", m.Kind)
	}
	return nil
}

// MergeQueuedError refuses a duplicate enqueue, carrying the place the
// workspace already holds so the caller reports it rather than queueing twice.
type MergeQueuedError struct {
	// Repo is the queue the workspace is already in.
	Repo RepoKey
	// Workspace is the workspace already queued.
	Workspace WorkspaceID
	// Position is the place it already holds.
	Position int
	// State is where that entry stands.
	State MergeQueueState
}

// Error implements error.
func (e *MergeQueuedError) Error() string {
	return fmt.Sprintf("wsm: workspace %s is already %s in repo %q merge queue at position %d", e.Workspace, e.State, e.Repo, e.Position)
}

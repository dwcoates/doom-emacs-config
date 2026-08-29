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

// ErrTombstoned is returned when a retired held prompt is written to. A
// tombstoned hold never resurrects.
var ErrTombstoned = errors.New("wsm: held prompt is tombstoned")

// LayoutError refuses a database file whose layout version is not exactly this
// binary's. A NEWER file is the deploy/rollback silent-corruption class; an
// OLDER one is refused too, because the store is nuked and recreated, never
// migrated.
type LayoutError struct {
	// Path is the database file that was refused.
	Path string
	// File is the layout version stamped in the file.
	File int
	// Binary is the layout version this build writes.
	Binary int
}

// Error implements error.
func (e *LayoutError) Error() string {
	rel := "older than"
	if e.File > e.Binary {
		rel = "newer than"
	}
	return fmt.Sprintf("wsm: database %q has layout version %d, %s this build's %d; the store is recreated, never migrated", e.Path, e.File, rel, e.Binary)
}

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
func (h HoldKind) valid() bool { return h >= HoldClassifying && h <= HoldBuildRefresh }

// valid reports whether the turn close is one of the declared arms.
func (c TurnClose) valid() bool { return c >= CloseCompleted && c <= CloseOrphaned }

// String names a merge queue state, for logs and refusals.
func (s MergeQueueState) String() string {
	switch s {
	case MergeQueued:
		return "queued"
	case MergeAdmitted:
		return "admitted"
	default:
		return fmt.Sprintf("merge_queue_state(%d)", int(s))
	}
}

// valid reports whether the merge queue state is one of the declared arms.
func (s MergeQueueState) valid() bool { return s >= MergeQueued && s <= MergeAdmitted }

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

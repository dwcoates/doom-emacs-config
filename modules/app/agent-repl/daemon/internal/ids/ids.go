// Package ids holds the daemon's identity newtypes.
//
// It is a leaf: it imports nothing, and it sits below both wsm and feedid so
// the two can share one spelling of every identity rather than each declaring
// its own. Every other package aliases these (`type WorkspaceID =
// ids.WorkspaceID`) rather than redeclaring them, so an id minted anywhere is
// the same type everywhere. See ARCHITECTURE.md's package map entry for
// `ids/`.
//
// Identifier spaces are never conflated: none of these is a vendor
// identifier, and none is ever derived from a path or parsed by a client.
package ids

// WorkspaceID is the daemon-minted, opaque workspace identity. Clients echo
// it byte-wise; it is never derived from a directory.
type WorkspaceID string

// RepoID is the daemon-minted identity of a repository — one per
// canonicalized common dir, shared by every workspace cut from it.
type RepoID string

// InstanceID identifies one daemon process, for serving ownership across a
// handover.
type InstanceID string

// LeaseID identifies one acquisition of a workspace's occupancy lease. A
// merge bubble's sub-feed is keyed by the merge lease that owns it.
type LeaseID string

// TurnID is the daemon-minted turn identity, distinct from every vendor
// identifier.
type TurnID string

// TaskID identifies one user task.
type TaskID string

// FaultID identifies one recorded fault.
type FaultID string

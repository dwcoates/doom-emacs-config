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

// SubsectionID identifies one user-named subsection of a repository's
// sidebar section.
type SubsectionID string

// FaultID identifies one recorded fault.
type FaultID string

// ProcessIdentity names ONE process across pid reuse: its pid and the
// kernel's start stamp for it (procmetrics.Identity.StartUnixMicros). A pid
// alone names whichever process holds the number now, and the kernel hands a
// freed number to the next process it starts: a pid recorded for a shim read
// back hours later once named an unrelated root daemon (2026-11-21). The
// stamp is compared for equality only; a process the same pid names with any
// other stamp is not the one recorded.
type ProcessIdentity struct {
	PID             int
	StartUnixMicros int64
}

// Optional spells an optional id for a record or a log field: the id, or
// empty for none.
func Optional[ID ~string](id *ID) string {
	if id == nil {
		return ""
	}
	return string(*id)
}

// Strings spells typed ids -- or any string-kinded values -- as plain
// strings, in order, for a record's context or a wire field. A nil or empty
// IN answers an empty, non-nil slice, so a record states "none" as [] rather
// than null.
func Strings[ID ~string](in []ID) []string {
	out := make([]string, 0, len(in))
	for _, id := range in {
		out = append(out, string(id))
	}
	return out
}

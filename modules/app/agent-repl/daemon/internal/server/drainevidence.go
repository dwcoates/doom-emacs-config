// drainevidence.go supplies the DURABLE evidence a RESTORED drain lease judges
// quiescence against.
//
// WHAT WENT WRONG WITHOUT IT. Restore runs at boot, before the boot sweeper and
// before any shim has reattached, so the session fleet it would ask "who holds
// the drain open" is structurally EMPTY at that instant. A lease restored
// mid-drain therefore saw zero holds, concluded the fleet was quiescent, cleared
// its durable row, broadcast idle, and bounced the daemon immediately — cutting
// the very mid-turn shims the lease exists to protect. The fleet was not
// quiescent; it was UNWIRED, and the two were indistinguishable.
//
// THE FIX IS TO MAKE THEM DISTINGUISHABLE. "No hold" and "not yet resolved" are
// separate states, and the second one HOLDS. The evidence below is what the
// unresolved set is derived from: the registry (which survived the crash) plus
// the same two probes the boot sweeper classifies with.
package server

import (
	"claude-repld/internal/registry"
)

// RegisteredSession names one session the registry remembers. It is the unit of
// the pessimistic UNRESOLVED set: a workspace and a session id, which is exactly
// what a ShutdownHold needs to be broadcast.
type RegisteredSession struct {
	Workspace string
	SessionID string
}

// DrainEvidenceSource is the boot-time evidence a restored lease seeds itself
// from. It is deliberately NOT the session fleet: the fleet is what has not
// happened yet at Restore, and asking it would reproduce the defect.
//
// The two probes are the SAME PAIR the boot sweeper uses (bootsweep.go), and for
// the same reason neither is sufficient alone: the parked connection answers for
// a shim that has already dialled in, the session lock answers for one that is
// alive but still inside its reconnect backoff. An ERROR from either is "I could
// not tell", which is never read as free.
type DrainEvidenceSource interface {
	// RegisteredSessions returns every non-terminal registered session that has
	// a working directory — the same set the boot sweeper walks.
	RegisteredSessions() []RegisteredSession
	// ShimConnected reports whether a shim for the session has already dialled
	// in to this daemon's listener.
	ShimConnected(sessionID string) (bool, error)
	// ShimLockHeld reports whether a live process holds the session's lock.
	ShimLockHeld(sessionID string) (bool, error)
}

// RegistryDrainEvidence is the production DrainEvidenceSource: the daemon's own
// registry plus the listener's parked map and the session lock dir.
//
// It exists so main and the boot harness read the SAME three facts through one
// adapter rather than each assembling their own trio, which is how the sweeper's
// classification and the lease's classification stay one classification.
type RegistryDrainEvidence struct {
	// Reg is the persistent session registry (required).
	Reg *registry.Registry
	// Connected is the listener's parked-connection probe (required).
	Connected func(sessionID string) (bool, error)
	// Held is the session-lock probe (required).
	Held func(sessionID string) (bool, error)
}

// RegisteredSessions walks the registry with the boot sweeper's own filter: a
// terminal record is a session that is over, and a record with no cwd names no
// workspace to hold anything open.
func (e RegistryDrainEvidence) RegisteredSessions() []RegisteredSession {
	var out []RegisteredSession
	for _, rec := range e.Reg.All() {
		if rec.Terminal || rec.CWD == "" {
			continue
		}
		out = append(out, RegisteredSession{Workspace: rec.CWD, SessionID: rec.SessionID})
	}
	return out
}

func (e RegistryDrainEvidence) ShimConnected(sessionID string) (bool, error) {
	return e.Connected(sessionID)
}

func (e RegistryDrainEvidence) ShimLockHeld(sessionID string) (bool, error) {
	return e.Held(sessionID)
}

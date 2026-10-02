package wsm

import (
	"encoding/hex"

	"github.com/google/uuid"

	"claude-repld/internal/ids"
)

// IDLength is how many characters every minted identifier has.
//
// It is 16 because the state root's socket layout is budgeted against a
// 16-character workspace id: stateroot.CheckSocketPathBudget refuses a state
// root where "sock/<16 chars>.sock" would overflow a unix-domain socket path.
// Widening an id here silently invalidates that check, so the two constants
// move together or not at all.
const IDLength = 16

// mint returns a fresh opaque identifier: the first eight bytes of a random
// UUID, hex-encoded. Opaque, byte-wise comparable, never parsed, and never
// derived from a path.
func mint() string {
	u := uuid.New()
	return hex.EncodeToString(u[:IDLength/2])
}

// NewWorkspaceID mints a workspace identity.
func NewWorkspaceID() ids.WorkspaceID { return ids.WorkspaceID(mint()) }

// NewRepoID mints a repository identity.
func NewRepoID() ids.RepoID { return ids.RepoID(mint()) }

// NewInstanceID mints this daemon process's identity.
func NewInstanceID() ids.InstanceID { return ids.InstanceID(mint()) }

// NewLeaseID mints one occupancy-lease acquisition's identity.
func NewLeaseID() ids.LeaseID { return ids.LeaseID(mint()) }

// NewTurnID mints a turn identity. It is the daemon's own, never a vendor
// identifier.
func NewTurnID() ids.TurnID { return ids.TurnID(mint()) }

// NewVendorSessionID mints a VENDOR session id. It is the one identifier here
// the daemon mints on the vendor's behalf rather than for itself, and it is a
// full uuid because that is the shape the vendor's own transcript file names
// use: a FORK never resumes the parent's id (shim.v1 StartSession has no fork
// arm and a vendor session id is single-occupancy under the session lock), so
// the daemon mints one and files the copied transcript under it.
func NewVendorSessionID() string { return uuid.New().String() }

// NewHostSessionID mints one SESSION's host-facing identity: the echo token
// the host stream carries and Emacs correlates transcripts, health probes and
// fault windows against. Sessions rotate under one workspace, and this is what
// distinguishes them; it is the daemon's own, never the vendor's.
func NewHostSessionID() string { return mint() }

// NewTaskID mints a task identity.
func NewTaskID() ids.TaskID { return ids.TaskID(mint()) }

// NewFaultID mints a fault identity.
func NewFaultID() ids.FaultID { return ids.FaultID(mint()) }

// NewNewsDigestID mints a news digest's identity: the opaque echo token a
// dismiss names (frontend.v1.NewsDigestId).
func NewNewsDigestID() string { return mint() }

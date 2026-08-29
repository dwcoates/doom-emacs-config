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

// NewTaskID mints a task identity.
func NewTaskID() ids.TaskID { return ids.TaskID(mint()) }

// NewFaultID mints a fault identity.
func NewFaultID() ids.FaultID { return ids.FaultID(mint()) }

package rollout

import (
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE SERVING STANDING.
//
// Every per-workspace rpc refuses before it delegates when this daemon is not
// the one serving the workspace, and the two ways that happens are both facts
// the rollout controller already holds: a workspace it TRANSFERRED AWAY during
// a handover, and a workspace a JOINING successor has been told about but has
// not adopted yet. Nothing else in the daemon knows either, so the standing is
// answered here rather than re-derived from the durable serving row — which
// says who owns the workspace but not that a handover is in flight, and
// carries no successor address for the arm that needs one.

// Standing is a workspace's serving standing on THIS daemon.
type Standing int

// The standings.
const (
	// StandingOwned is the ordinary case: this daemon serves the workspace.
	StandingOwned Standing = iota
	// StandingTransferringAway is a workspace this daemon handed to a
	// successor. The successor's address is what the arm carries.
	StandingTransferringAway
	// StandingNotYetAdopted is a workspace a joining daemon knows about from
	// the intent manifest but has not adopted yet.
	StandingNotYetAdopted
)

// String names a standing for a record.
func (s Standing) String() string {
	switch s {
	case StandingTransferringAway:
		return "transferring_away"
	case StandingNotYetAdopted:
		return "not_yet_adopted"
	default:
		return "owned"
	}
}

// Standing answers a workspace's serving standing on this daemon. An unknown
// workspace is OWNED: a daemon that never transferred it and is not joining is
// the one serving it, and refusing on ignorance would refuse every ordinary
// rpc.
func (c *controller) Standing(ws ids.WorkspaceID) Standing {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.transferred[ws] != "" {
		return StandingTransferringAway
	}
	if (c.joiningMode || c.joining[ws]) && !c.owned[ws] {
		return StandingNotYetAdopted
	}
	return StandingOwned
}

// SuccessorAddress answers the address this daemon's workspaces moved to,
// which is `transferring_away{address}`'s only field. It is the empty string
// when no handover is in flight.
func (c *controller) SuccessorAddress() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.successor
}

// recordTransfer marks a workspace as handed to the successor at the address
// the announcement named. It is called once per workspace, at the moment the
// transfer notice is pushed: from then on this daemon serves nothing for it.
func (c *controller) recordTransfer(ws ids.WorkspaceID, successor string) {
	c.mu.Lock()
	if c.transferred == nil {
		c.transferred = map[ids.WorkspaceID]string{}
	}
	previous := c.transferred[ws]
	c.transferred[ws] = successor
	c.successor = successor
	c.mu.Unlock()
	c.logTransition(opTransfer, ws, "serving_standing", previous, successor, nil)
	c.log.Debug(opTransfer, "the workspace's serving standing is transferring_away", dlog.Context{
		"workspace": string(ws), "successor": successor,
	})
}

// untransfer undoes recordTransfer for a workspace this daemon took back: its
// per-workspace rpcs are served here again, and the rendezvous its
// announcement armed is settled and disarmed so nothing waits on an adoption
// that will not happen here.
func (c *controller) untransfer(ws ids.WorkspaceID) {
	c.mu.Lock()
	previous := c.transferred[ws]
	delete(c.transferred, ws)
	e, armed := c.rendezvous[ws]
	delete(c.rendezvous, ws)
	c.mu.Unlock()
	if armed {
		e.settle(ErrReclaimed)
	}
	c.logTransition(opTransfer, ws, "serving_standing", previous, "", dlog.Context{"reclaimed": true})
}

package rollout

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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

// Standing answers a workspace's serving standing on this daemon.
//
// EACH NON-OWNED STANDING DERIVES FROM A FACT THAT NAMES THE WORKSPACE, never
// from how this daemon booted (2026-10-30 17:41-17:54: a successor answered
// not_yet_adopted for workspaces it had CREATED itself, because "joining" was
// read as "owns nothing", and they could not be opened until the handover
// ended).
//   - transferring_away: this daemon handed the workspace over, OR it is
//     handing over and the workspace is outside the served set it fixed
//     (the successor minted it after the create gate closed here).
//   - not_yet_adopted: this daemon is still joining (the outgoing daemon
//     lives and may serve any workspace it did not hand over) or the
//     predecessor's manifest names the workspace, and the workspace is
//     neither adopted nor one this daemon created itself. A created
//     workspace is in no manifest and no other daemon serves it, so it is
//     owned from its creation.
//
// Anything else is OWNED: a daemon refusing on ignorance would refuse every
// ordinary rpc.
func (c *controller) Standing(ws ids.WorkspaceID) Standing {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.transferred[ws] != "" {
		return StandingTransferringAway
	}
	if c.successorsLocked(ws) {
		return StandingTransferringAway
	}
	if c.owned[ws] || c.created[ws] {
		return StandingOwned
	}
	if c.joining[ws] || c.joiningMode {
		return StandingNotYetAdopted
	}
	return StandingOwned
}

// successorsLocked reports that a handover in flight fixed a served set this
// workspace is outside of. The caller holds c.mu.
func (c *controller) successorsLocked(ws ids.WorkspaceID) bool {
	slot := c.handover
	return slot != nil && slot.served != nil && !slot.served[ws]
}

// Created implements Controller.
func (c *controller) Created(ws ids.WorkspaceID) {
	c.mu.Lock()
	if c.created == nil {
		c.created = map[ids.WorkspaceID]bool{}
	}
	was := c.created[ws]
	c.created[ws] = true
	joining := c.joiningMode
	c.mu.Unlock()
	if !was {
		c.logTransition(opJoin, ws, "serving_standing", "unknown", "owned", dlog.Context{
			"created_here": true, "joining": joining,
		})
	}
}

// SuccessorAddress answers the address this daemon's workspaces moved to,
// which is `transferring_away{address}`'s only field. It is the empty string
// when no handover is in flight.
func (c *controller) SuccessorAddress() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.successor == "" && c.handover != nil {
		return c.handover.servedSuccessor
	}
	return c.successor
}

// IncumbentAddress implements Controller.
//
// THE INCUMBENT SERVES WHAT THIS DAEMON HAS NOT ADOPTED for exactly as long as
// this daemon is joining: joining mode ends when the incumbent's exit hands
// over the boot claim (takeOverOnClaim), and from then no other daemon serves
// anything, so the arm names none and a client waits for the adoption.
func (c *controller) IncumbentAddress() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	if !c.joiningMode {
		return ""
	}
	return c.deps.IncumbentAddress
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
func (c *controller) untransfer(ws ids.WorkspaceID) (wasTransferred bool) {
	c.mu.Lock()
	previous, wasTransferred := c.transferred[ws]
	delete(c.transferred, ws)
	e, armed := c.rendezvous[ws]
	delete(c.rendezvous, ws)
	c.mu.Unlock()
	if armed {
		e.settle(ErrReclaimed)
	}
	c.logTransition(opTransfer, ws, "serving_standing", previous, "", dlog.Context{"reclaimed": true})
	return wasTransferred
}

// ServesIntake implements Controller.
//
// THE TWO DAEMONS OF A HANDOVER NEVER BOTH ANSWER TRUE. The incumbent answers
// false from the moment it claims the rollout slot, before it spawns anything;
// the successor answers false until it owns every workspace it was handed,
// which is only after the incumbent released each of them. A handover that is
// abandoned empties the slot, so the incumbent takes intake again. One whose
// transfers partly failed keeps its slot (it does not exit), and the successor
// never finishes joining, so NEITHER takes intake: the entries wait on disk,
// beside the ERROR that handover already stated, rather than being taken by a
// daemon that may not serve their workspace.
func (c *controller) ServesIntake() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return !c.stillJoiningLocked() && c.handover == nil
}

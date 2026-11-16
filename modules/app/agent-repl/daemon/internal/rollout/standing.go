package rollout

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/changesignal"
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
	defer c.unlock()
	return c.standingLocked(ws)
}

// DefaultAdoptionWaitBound is how long AwaitAdopted waits for a workspace a joining
// daemon has not adopted yet. Measured 2026-11-16 (the 13:32 rollout, 121
// workspaces): a workspace's adoption landed 3.3s after its transfer was
// announced at the median and 8.05s at the worst, so the bound is about 2.5
// times that worst.
const DefaultAdoptionWaitBound = 20 * time.Second

// AwaitAdopted implements Controller.
func (c *controller) AwaitAdopted(ctx context.Context, ws ids.WorkspaceID) (Standing, error) {
	limit := c.deps.AdoptionWaitBound
	bound := time.NewTimer(limit)
	defer bound.Stop()
	for {
		c.mu.Lock()
		standing := c.standingLocked(ws)
		changed := c.standings.Changed()
		c.unlock()
		if standing != StandingNotYetAdopted {
			return standing, nil
		}
		if c.onAdoptionWait != nil {
			c.onAdoptionWait()
		}
		select {
		case <-changed:
		case <-ctx.Done():
			return standing, ctx.Err()
		case <-bound.C:
			err := fmt.Errorf("rollout: workspace %q was not adopted within %s (its adoption is expected within about 8s)", ws, limit)
			c.log.Error(opAdopt, "a workspace a caller waited on was not adopted before the bound", dlog.Context{
				"workspace": string(ws), "bound": limit.String(), "expected": "8.05s", "error": err.Error()})
			return standing, err
		}
	}
}

// standingLocked is Standing under the held lock.
func (c *controller) standingLocked(ws ids.WorkspaceID) Standing {
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
	c.unlock()
	if !was {
		c.logTransition(opJoin, ws, "serving_standing", "unknown", "owned", dlog.Context{
			"created_here": true, "joining": joining,
		})
		c.mirrorOwnershipChanged()
	}
}

// SuccessorAddress answers the address this daemon's workspaces moved to,
// which is `transferring_away{address}`'s only field. It is the empty string
// when no handover is in flight.
func (c *controller) SuccessorAddress() string {
	c.mu.Lock()
	defer c.unlock()
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
	defer c.unlock()
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
	c.unlock()
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
	c.unlock()
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
	defer c.unlock()
	return c.servesIntakeLocked()
}

// servesIntakeLocked is ServesIntake under the held lock.
func (c *controller) servesIntakeLocked() bool {
	return !c.stillJoiningLocked() && c.handover == nil
}

// IntakeChanged implements Controller.
func (c *controller) IntakeChanged() <-chan struct{} {
	c.mu.Lock()
	defer c.unlock()
	return c.intake.changed.Changed()
}

// ServesQueue implements Controller.
//
// THE MERGE QUEUE NEVER ADVANCES ACROSS A HANDOVER (owner ruling,
// 2026-11-13). The incumbent answers false from the moment it claims the
// rollout slot, before it spawns anything, so no merge lands while the two
// daemons trade workspaces; the successor answers true from the moment it
// owns the first workspace it was handed, which is the moment it is fully
// up, without waiting for the rest (ServesWorkspace holds each merge to its
// own workspace's transfer). A handover that is abandoned empties the slot,
// and the incumbent's queue runs again.
func (c *controller) ServesQueue() bool {
	c.mu.Lock()
	defer c.unlock()
	return c.servesQueueLocked()
}

// servesQueueLocked is ServesQueue under the held lock.
func (c *controller) servesQueueLocked() bool {
	if c.handover != nil {
		return false
	}
	if !c.joiningMode {
		return true
	}
	if !c.manifestSeen {
		return false
	}
	for ws := range c.joining {
		if c.owned[ws] {
			return true
		}
	}
	return len(c.joining) == 0
}

// QueueChanged implements Controller.
func (c *controller) QueueChanged() <-chan struct{} {
	c.mu.Lock()
	defer c.unlock()
	return c.queue.changed.Changed()
}

// ServesWorkspace implements Controller: false for a workspace this daemon
// handed to a successor, and, while joining, for one it was handed and has
// not finished adopting.
func (c *controller) ServesWorkspace(ws ids.WorkspaceID) bool {
	c.mu.Lock()
	defer c.unlock()
	if _, gone := c.transferred[ws]; gone {
		return false
	}
	if c.joiningMode && c.joining[ws] && !c.owned[ws] {
		return false
	}
	return true
}

// answerLatch is one standing answer as of the last unlock, and the
// broadcast its changes are announced on. Its zero value is ready; the
// caller sets the answer before any waiter reads it.
type answerLatch struct {
	answer  bool
	changed changesignal.Signal
}

// set records ANSWER, announcing it when it changed.
func (l *answerLatch) set(answer bool) {
	if answer == l.answer {
		return
	}
	l.answer = answer
	l.changed.Notify()
}

// unlock releases mu, first signaling a change of ServesIntake's or
// ServesQueue's answer that the critical section made: the one place their
// waiters are told, so every mutation of the fields the answers read tells
// them.
func (c *controller) unlock() {
	c.intake.set(c.servesIntakeLocked())
	c.standings.Notify()
	c.queue.set(c.servesQueueLocked())
	c.mu.Unlock()
}

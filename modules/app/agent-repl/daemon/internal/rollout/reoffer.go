package rollout

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE SUCCESSOR FOLLOWS EACH TRANSFER, NOT ONLY THE ANNOUNCEMENT. The intent
// manifest names who held each workspace's streams when the stand-down was
// announced, and the rendezvous used to wait on exactly those participants
// for the life of the handover. A workspace transferred later -- at its
// freeness, or again after a refused or expired adoption -- may by then have
// lost a participant: a page closed, or went quiet. The rendezvous then waited
// on a call that could never come, and a workspace left with no participant
// at all was never adopted by anybody (2026-10-12, workspace
// 80eeafb361fa49e3: the incumbent served it alone for nine hours).
//
// Each transfer's carry records the occupancy it was sealed with
// (CarriedParticipants). The successor takes its expectations from the
// newest carry: at a participant's own call, and on a look every
// carryFollowPoll for a transfer nobody is left to call about.

// carryFollowPoll is how often a joining successor looks for a transfer that
// re-stated a workspace's participants. Like adoptionPoll it is a real ticker
// rather than the injected clock: it only ever shortens a wait the
// incumbent's adoption window bounds.
const carryFollowPoll = 250 * time.Millisecond

// carriedParticipants answers the occupancy the incumbent's newest transfer of
// ws was sealed with, and the incumbent that sealed it. It answers false for a
// carry that states no participants or that this daemon has refused, neither
// of which may be followed. A carry that cannot be read is recorded and not
// followed.
func (c *controller) carriedParticipants(ws ids.WorkspaceID) (Participants, ids.InstanceID, bool) {
	carry, found, err := c.readCarry(ws)
	if err != nil {
		c.log.Error(opJoin, "could not read the handover carry for its participants; the rendezvous keeps the expectations it has",
			dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
		return Participants{}, "", false
	}
	if !found || carry.Participants == nil || c.refusedMidWork(ws) {
		return Participants{}, "", false
	}
	return Participants{Host: carry.Participants.Host, Web: carry.Participants.Web}, carry.Daemon, true
}

// followCarriedLocked moves an unadopted entry's expectations to the occupancy
// a carry from the incumbent it adopts from was sealed with, and records the
// change. The caller holds c.mu.
func (c *controller) followCarriedLocked(ws ids.WorkspaceID, e *entry, carried Participants, sealedBy ids.InstanceID) {
	if e.adopted || e.adopting || sealedBy != e.outgoing || e.expected == carried {
		return
	}
	before := e.expected
	e.expected = carried
	// INFO: who the rendezvous waits on is the handover's one silent wait.
	c.log.Info(opJoin, "a transfer re-stated who holds the workspace's streams; the rendezvous now waits on them", dlog.Context{
		"workspace":     string(ws),
		"expected_host": carried.Host, "expected_web": carried.Web,
		"before_host": before.Host, "before_web": before.Web,
	})
}

// followCarries keeps every armed, unadopted workspace's expectations on its
// newest transfer's carry for as long as this daemon is joining, and adopts
// any workspace a transfer left with no participant to call.
func (c *controller) followCarries(ctx context.Context) {
	look := time.NewTicker(carryFollowPoll)
	defer look.Stop()
	tookOver := c.tookOverSignal()
	for {
		select {
		case <-ctx.Done():
			return
		case <-tookOver:
			// adoptStragglers takes whatever is left at the takeover.
			return
		case <-look.C:
		}
		if headless := c.followCarriesOnce(); len(headless) > 0 {
			c.log.Info(opJoin, "a transfer left workspaces with no participant to call; adopting them headless",
				dlog.Context{"workspaces": len(headless)})
			c.adoptClaimed(ctx, headless)
		}
	}
}

// followCarriesOnce is one look: it follows every carry that re-stated an
// armed workspace's participants, and answers the headless workspaces it
// claimed for adoption.
func (c *controller) followCarriesOnce() []ids.WorkspaceID {
	c.mu.Lock()
	var candidates []ids.WorkspaceID
	for ws, e := range c.rendezvous {
		if !e.adopted && !e.adopting && !e.adoptionClaimed {
			candidates = append(candidates, ws)
		}
	}
	c.mu.Unlock()
	for _, ws := range candidates {
		carried, sealedBy, ok := c.carriedParticipants(ws)
		if !ok {
			continue
		}
		// A CARRY WITH NOBODY TO CALL IS FOLLOWED ONLY ONCE ITS TRANSFER HAS
		// RELEASED THE ROW. A take-back claims the row before it retires the
		// carry, so a carry seen beside a row the incumbent still holds may be
		// the one being retired, and adopting headless on it would race the
		// next transfer's participants.
		if carried.Count() == 0 && !c.servingReleased(ws) {
			continue
		}
		c.mu.Lock()
		if e, armed := c.rendezvous[ws]; armed && !e.adoptionClaimed {
			c.followCarriedLocked(ws, e, carried, sealedBy)
		}
		c.mu.Unlock()
	}
	return c.claimArmed()
}

// servingReleased reports whether no daemon holds the workspace's serving row:
// the durable edge a transfer crosses once its carry is written. A row that
// cannot be read is recorded and read as held.
func (c *controller) servingReleased(ws ids.WorkspaceID) bool {
	owner, err := c.deps.DB.Serving(c.lifetime(context.Background()), ws)
	if err != nil {
		c.log.Error(opJoin, "could not read serving ownership while following a transfer's carry",
			dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
		return false
	}
	return owner == nil
}

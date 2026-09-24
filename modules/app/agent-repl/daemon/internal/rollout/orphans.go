package rollout

import (
	"context"
	"errors"
	"io/fs"
	"os"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
)

// opOrphans is the operation the takeover's orphan recovery is recorded under.
const opOrphans = "daemon.rollout.orphans"

// recoverOrphans adopts every shim a gone daemon left running that NOTHING
// adopted: an open workspace whose kernel lock is still HELD while this daemon
// holds no client for it and no handover is bringing it over.
//
// WHERE THESE COME FROM. A handover moves only what its incumbent's `served`
// lists, and before every bring-up claimed serving (workspace claimServing) a
// cold-started daemon listed none of the sessions it had brought up: its
// successor armed no rendezvous for them, took over, and served them never
// (2026-09-24, shims 43503, 43570 and 43666 of daemon pid 43501). Every later
// handover reported `live_shims: 0`, because no daemon held a client.
//
// WHY AT THE TAKEOVER. A COLD boot already adopts every held lock
// (boot.sequence.adopt), but a JOINING successor reconciles nothing and adopts
// only what the manifest names. The takeover is the one instant this daemon
// knows no other daemon serves anything -- daemon.addr was written, so the
// incumbent's boot claim is gone -- so a held lock with no client here is a
// shim nobody serves, and it is this daemon's.
//
// ADOPTED, NEVER KILLED. The shim is healthy and holds its conversation; the
// adoption goes through the fleet's ordinary adopt (dial, install, watches),
// which is the same door the handover's successor uses and which claims
// serving. A lock that could not be read is never read as held or free: it is
// named and left. No stand-down is attempted here, forced or otherwise.
func (c *controller) recoverOrphans(fields dlog.Context) {
	if c.deps.LockProbe == nil || c.deps.Shims == nil {
		return
	}
	lifetime := c.lifetime(context.Background())
	workspaces, err := c.deps.DB.ListWorkspaces(lifetime)
	if err != nil {
		c.log.Error(opOrphans, "could not list the workspaces to look for shims nothing adopted", withCause(fields, err))
		return
	}
	c.mu.Lock()
	handedOver := make(map[ids.WorkspaceID]bool, len(c.joining))
	for ws := range c.joining {
		handedOver[ws] = true
	}
	c.mu.Unlock()

	var orphans []ids.WorkspaceID
	for _, ws := range workspaces {
		wsFields := merge(fields, dlog.Context{"workspace": string(ws.ID)})
		switch {
		case ws.Closed:
			continue
		case handedOver[ws.ID]:
			// The rendezvous (or the straggler adoption) owns it.
			continue
		}
		if _, live := c.deps.Shims.Client(ws.ID); live {
			continue
		}
		if _, statErr := os.Stat(ws.Dir); statErr != nil {
			if errors.Is(statErr, fs.ErrNotExist) {
				c.log.Debug(opOrphans, "the workspace's worktree is gone; there is no session here to recover", wsFields)
			} else {
				c.log.Warn(opOrphans, "the workspace's worktree could not be stat-ed; its shim is not recovered", withCause(wsFields, statErr))
			}
			continue
		}
		state, probeErr := c.deps.LockProbe(ws.Dir)
		switch {
		case probeErr != nil:
			// "Could not tell" is never read as held or free.
			c.log.Warn(opOrphans, "the workspace lock probe could not tell; a shim that may be running is not recovered", withCause(wsFields, probeErr))
		case state == sessionlock.StateUnknown:
			c.log.Warn(opOrphans, "the workspace lock probe could not tell; a shim that may be running is not recovered", wsFields)
		case state == sessionlock.StateHeld:
			orphans = append(orphans, ws.ID)
		default:
			c.log.Debug(opOrphans, "no shim holds this workspace's lock; nothing to recover", wsFields)
		}
	}
	if len(orphans) == 0 {
		c.log.Debug(opOrphans, "no shim was left running unadopted", fields)
		return
	}
	c.log.Info(opOrphans, "adopting the shims a gone daemon left running that nothing adopted",
		merge(fields, dlog.Context{"workspaces": len(orphans)}))
	for _, ws := range orphans {
		c.stragglerAdoptions.Add(1)
		go func(ws ids.WorkspaceID) {
			defer c.stragglerAdoptions.Done()
			c.recoverOrphan(lifetime, ws, merge(fields, dlog.Context{"workspace": string(ws)}))
		}(ws)
	}
}

// recoverOrphan adopts one unadopted shim and claims its workspace. Both
// failures are that workspace's own loud error; the shim is left running
// either way, because a live conversation is never ended to tidy a record.
func (c *controller) recoverOrphan(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) {
	client, err := c.deps.Shims.Adopt(ctx, ws)
	if err != nil {
		c.log.Error(opOrphans, "a shim nothing adopted could not be adopted; it is left running unserved", withCause(fields, err))
		return
	}
	if err := c.deps.DB.ClaimServing(ctx, ws, c.deps.Instance); err != nil {
		c.log.Error(opOrphans, "an adopted orphan's workspace could not be claimed; a handover will reclaim it from the live session", withCause(fields, err))
		return
	}
	c.log.Info(opOrphans, "adopted a shim a gone daemon left running", merge(fields, dlog.Context{"shim_pid": client.PID()}))
	// A GONE DAEMON'S SHIM IS JUDGED like every other adopted one: the
	// takeover's own staleness pass ran before this adoption finished.
	if _, err := c.checkStale(ctx, ws, false); err != nil {
		c.log.Error(opStaleness, "an adopted orphan could not be judged against the installed build", withCause(fields, err))
	}
}

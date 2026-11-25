package rollout

import (
	"context"
	"errors"
	"io/fs"
	"os"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/sessionlock"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
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
//
// EACH WORKSPACE IT DECIDES IS SETTLED BY THAT DECISION (sidebar carry.go):
// the row's carried arm is let go once its started session's start has ended,
// once its recovered shim's adoption has ended, or at once when the pass
// starts nothing for it. A workspace handed over is its adoption's to settle.
//
// AND A FREE LOCK HAS ITS SESSION STARTED. An open workspace this daemon was
// never handed, with no client here and a lock nobody holds, is served by this
// daemon from the takeover on and has nothing behind it: its feed would stay
// empty until a prompt revived it. It is started through the boot's own
// bring-up (sessionless.go), exactly as a fresh boot starts a client-less
// workspace.
func (c *controller) recoverOrphans(fields dlog.Context) {
	if c.deps.LockProbe == nil || c.deps.Shims == nil {
		return
	}
	lifetime := c.lifetime(context.Background())
	workspaces, err := c.deps.DB.ListWorkspaces(lifetime)
	if err != nil {
		c.log.Error(opOrphans, "could not list the workspaces to look for shims nothing adopted", dlog.WithCause(fields, err))
		return
	}
	c.mu.Lock()
	handedOver := make(map[ids.WorkspaceID]bool, len(c.joining))
	for ws := range c.joining {
		handedOver[ws] = true
	}
	c.unlock()

	var orphans []ids.WorkspaceID
	var sessionless []wsm.Workspace
	for _, ws := range workspaces {
		if handedOver[ws.ID] {
			// The rendezvous (or the straggler adoption) owns it, and settles it.
			continue
		}
		switch c.orphanVerdict(ws, fields) {
		case orphanHeld:
			orphans = append(orphans, ws.ID)
		case orphanFree:
			sessionless = append(sessionless, ws)
		default:
			c.deps.FactsSettled(ws.ID)
		}
	}
	left, ok := c.bringUpSessionless(sessionless, dlog.Merge(fields, dlog.Context{"source": "orphans", "lock": sessionlock.StateFree.String()}))
	if ok {
		c.standDownWithheldInert(lifetime, left, fields)
	}
	// WHAT THE TAKEOVER DOES NOT START IS SETTLED NOW: nothing more will be
	// established for it. A split that could not be read decided nothing,
	// and nothing will be started for any of them either.
	unstarted := left
	if !ok {
		unstarted = sessionless
	}
	for _, ws := range unstarted {
		c.deps.FactsSettled(ws.ID)
	}
	if len(orphans) == 0 {
		c.log.Debug(opOrphans, "no shim was left running unadopted", fields)
		return
	}
	c.log.Info(opOrphans, "adopting the shims a gone daemon left running that nothing adopted",
		dlog.Merge(fields, dlog.Context{"workspaces": len(orphans)}))
	for _, ws := range orphans {
		c.stragglerAdoptions.Add(1)
		inflight.Go(func() {
			defer c.stragglerAdoptions.Done()
			defer c.deps.FactsSettled(ws)
			c.recoverOrphan(lifetime, ws, dlog.Merge(fields, dlog.Context{"workspace": string(ws)}))
		})
	}
}

// orphanVerdict is what the takeover's orphan pass does with one workspace
// nobody handed over.
type orphanVerdict int

const (
	// orphanNone: nothing is recovered or started.
	orphanNone orphanVerdict = iota
	// orphanHeld: a shim nothing adopted holds its lock, and is adopted.
	orphanHeld
	// orphanFree: no shim holds its lock, and its session is started.
	orphanFree
)

// orphanVerdict judges WS, one workspace nobody handed over, recording why
// nothing is done for it where nothing is.
func (c *controller) orphanVerdict(ws wsm.Workspace, fields dlog.Context) orphanVerdict {
	probeUntold := logsite.Msg("the workspace lock probe could not tell; a shim that may be running is not recovered")
	wsFields := dlog.Merge(fields, dlog.Context{"workspace": string(ws.ID)})
	if ws.Closed {
		return orphanNone
	}
	if _, live := c.deps.Shims.Client(ws.ID); live {
		return orphanNone
	}
	if _, statErr := os.Stat(ws.Dir); statErr != nil {
		if errors.Is(statErr, fs.ErrNotExist) {
			c.log.Debug(opOrphans, "the workspace's worktree is gone; there is no session here to recover", wsFields)
		} else {
			c.log.Warn(opOrphans, "the workspace's worktree could not be stat-ed; its shim is not recovered", dlog.WithCause(wsFields, statErr))
		}
		return orphanNone
	}
	state, probeErr := c.deps.LockProbe(ws.Dir)
	switch {
	case probeErr != nil:
		// "Could not tell" is never read as held or free.
		c.log.Log(dlog.LevelWarn, opOrphans, probeUntold, dlog.WithCause(wsFields, probeErr))
		return orphanNone
	case state == sessionlock.StateUnknown:
		c.log.Log(dlog.LevelWarn, opOrphans, probeUntold, wsFields)
		return orphanNone
	case state == sessionlock.StateHeld:
		return orphanHeld
	default:
		c.log.Debug(opOrphans, "no shim holds this workspace's lock; its session is started", wsFields)
		return orphanFree
	}
}

// recoverOrphan adopts one unadopted shim and claims its workspace. Both
// failures are that workspace's own loud error; the shim is left running
// either way, because a live conversation is never ended to tidy a record.
func (c *controller) recoverOrphan(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) {
	client, err := c.deps.Shims.Adopt(ctx, ws)
	if err != nil {
		c.log.Error(opOrphans, "a shim nothing adopted could not be adopted; it is left running unserved", dlog.WithCause(fields, err))
		return
	}
	if err := c.deps.DB.ClaimServing(ctx, ws, c.deps.Instance); err != nil {
		c.log.Error(opOrphans, "an adopted orphan's workspace could not be claimed; a handover will reclaim it from the live session", dlog.WithCause(fields, err))
		return
	}
	c.log.Info(opOrphans, "adopted a shim a gone daemon left running", dlog.Merge(fields, dlog.Context{"shim_pid": client.PID()}))
	// A GONE DAEMON'S SHIM IS JUDGED like every other adopted one: the
	// takeover's own staleness pass ran before this adoption finished.
	if _, err := c.checkStale(ctx, ws, false, judgedForOrphan); err != nil {
		c.log.Error(opStaleness, "an adopted orphan could not be judged against the installed build", dlog.WithCause(fields, err))
	}
}

// standDownWithheldInert stands down, off the caller's goroutine, the inert
// survivor of every one of WITHHELD, the workspaces the takeover does not
// bring up (bringUpSessionless's left: its shim was not up when the last
// daemon stopped), whose lock reads free while a shim still
// listens. No bring-up reaches such a shim, so without this it would run on
// served by no daemon at all (2026-10-23: three shims parked since 17:50).
func (c *controller) standDownWithheldInert(ctx context.Context, withheld []wsm.Workspace, fields dlog.Context) {
	for _, ws := range withheld {
		c.stragglerAdoptions.Add(1)
		inflight.Go(func() {
			ws := ws.ID
			defer c.stragglerAdoptions.Done()
			c.standDownInert(ctx, ws, dlog.Merge(fields, dlog.Context{"workspace": string(ws)}))
		})
	}
}

// standDownInert adopts WS's inert survivor, when one listens, and stands it
// down: WS runs nothing, and the survivor holds no session to keep. A failure
// is the workspace's own loud error, and the survivor is left as it is.
func (c *controller) standDownInert(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) {
	client, adopted, err := c.deps.Shims.AdoptInert(ctx, ws)
	if err != nil {
		c.log.Error(opOrphans, "could not adopt the inert survivor of a workspace that runs nothing; it is left running", dlog.WithCause(fields, err))
		return
	}
	if !adopted {
		return
	}
	pid := client.PID()
	if err := c.deps.Shims.StandDown(ctx, ws); err != nil {
		c.log.Error(opOrphans, "could not stand down the inert survivor of a workspace that runs nothing", dlog.WithCause(dlog.Merge(fields, dlog.Context{"shim_pid": pid}), err))
		return
	}
	c.log.Info(opOrphans, "stood down the inert survivor of a workspace that runs nothing", dlog.Merge(fields, dlog.Context{"shim_pid": pid}))
}

// settleUndecided settles every workspace nobody handed over when the orphan
// pass that would decide each of them will not run, because WHY: nothing more
// will be established for any of them, so each is drawn from the facts this
// daemon holds rather than from the incumbent's last arm for good (sidebar
// carry.go). The failure itself was recorded at ERROR by the caller.
func (c *controller) settleUndecided(why string, fields dlog.Context) {
	workspaces, err := c.deps.DB.ListWorkspaces(c.lifetime(context.Background()))
	if err != nil {
		c.log.Error(opOrphans, "could not list the workspaces whose rows the orphan pass would have settled; each is settled by its next start", dlog.WithCause(dlog.Merge(fields, dlog.Context{"why": why}), err))
		return
	}
	c.mu.Lock()
	var undecided []ids.WorkspaceID
	for _, ws := range workspaces {
		if !c.joining[ws.ID] {
			undecided = append(undecided, ws.ID)
		}
	}
	c.unlock()
	c.log.Info(opOrphans, "the orphan pass will not run; the rows it would have decided are drawn from this daemon's own facts",
		dlog.Merge(fields, dlog.Context{"why": why, "workspaces": len(undecided)}))
	for _, ws := range undecided {
		c.deps.FactsSettled(ws)
	}
}

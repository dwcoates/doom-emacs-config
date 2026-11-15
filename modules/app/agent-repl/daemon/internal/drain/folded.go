package drain

import (
	"context"
	"time"

	"agent-repld/internal/bringup"
	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opFolded is the operation a folded workspace's wait records under.
const opFolded = "daemon.drain.folded"

// FoldStandDownDelay is how long a session in a collapsed section keeps
// running before it is stood down (owner request, 2026-10-30), unless
// Deps.FoldDelay names another. Its page is
// unloaded at the collapse; its shim waits this long, so a section folded
// and unfolded again within it finds every session still up.
const FoldStandDownDelay = 5 * time.Minute

// A COLLAPSE STANDS ITS SECTION'S SESSIONS DOWN Deps.FoldDelay AFTER THE
// LATEST COLLAPSE, never sooner. Every collapse that holds a workspace starts
// a fresh wait for it and supersedes the one before, so a section folded,
// unfolded and folded again is stood down the delay after the LAST fold: a
// wait that ends superseded does nothing. A wait that ends as the latest asks
// for the withheld stand-down (StandDownWithheld), whose attempt reads the
// workspace's visibility afresh, so a section unfolded meanwhile, or a
// workspace selected (which unfolds the sections holding it), keeps its
// session: nothing needs to cancel a wait.

// StandDownFolded asks for the stand-down of a workspace a collapse folded,
// Deps.FoldDelay from now. It returns at once; the wait runs on the
// detached lifetime, and the daemon's exit ends it with nothing stood down.
func (c *controller) StandDownFolded(ws ids.WorkspaceID) {
	c.mu.Lock()
	if c.folded == nil {
		c.folded = map[ids.WorkspaceID]uint64{}
	}
	c.folded[ws]++
	wait := c.folded[ws]
	c.mu.Unlock()
	fields := dlog.Context{"workspace": string(ws), "wait": wait, "delay": c.deps.FoldDelay.String()}
	c.log.Debug(opFolded, "a folded workspace's stand-down waits out the fold's delay", fields)
	c.deps.Detach(func(ctx context.Context) {
		select {
		case <-ctx.Done():
			c.log.Info(opFolded, "the daemon is leaving; a folded workspace's stand-down is not taken", dlog.WithCause(fields, ctx.Err()))
			return
		case <-c.deps.Clock.After(c.deps.FoldDelay):
		}
		c.mu.Lock()
		latest := c.folded[ws] == wait
		if latest {
			delete(c.folded, ws)
		}
		c.mu.Unlock()
		if !latest {
			c.log.Debug(opFolded, "a later collapse superseded this wait; its own wait stands the session down", fields)
			return
		}
		c.log.Info(opFolded, "a folded workspace's delay is over; its stand-down is asked for", fields)
		c.StandDownWithheld(ws)
	})
}

// A FOLD'S WAIT OUTLIVES THE DAEMON THAT STARTED IT. The wait above, and a
// stand-down it handed the withheld path that a busy turn holds back, live in
// this process's memory alone, so a restart or a handover lost every one of
// them and a folded session ran on until the idle sweep's cutoff (2026-10-30).
// The fold itself is durable (the arrangement), so the daemon that comes to
// own the workspaces derives the pending stand-downs from it again rather than
// persisting a timer: every open, unspared workspace a fold withholds whose
// session is still up waits the fold's delay afresh from here.

// ResumeFolded starts the fold's wait for every running session a fold
// withholds. It is the daemon's call once it owns every workspace it serves:
// a fresh boot's settle and a successor's takeover (rollout.Deps.ServingSettled).
// A read that fails is recorded at ERROR and resumes nothing; the next
// collapse asks afresh. A read cut by this daemon's context ending (its exit
// cancels the serving context under the settle) is recorded at INFO: nobody
// is left to resume anything for.
func (c *controller) ResumeFolded(ctx context.Context) {
	log := c.log
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		ctxend.Log(err, log.Info, log.Error, opFolded, "the workspaces' list for the folds' stand-downs ended with this daemon's context; none is resumed",
			"could not list the workspaces to resume the folds' stand-downs; none is resumed", dlog.WithCause(nil, err))
		return
	}
	folded, err := bringup.Folds(ctx, c.deps.DB, log)
	if err != nil {
		ctxend.Log(err, log.Info, log.Error, opFolded, "the folds' read for the folds' stand-downs ended with this daemon's context; none is resumed",
			"could not read the folds to resume their stand-downs; none is resumed", dlog.WithCause(nil, err))
		return
	}
	sessions, err := wsm.SessionsByWorkspace(ctx, c.deps.DB)
	if err != nil {
		ctxend.Log(err, log.Info, log.Error, opFolded, "the sessions' read for the folds' stand-downs ended with this daemon's context; none is resumed",
			"could not read the sessions to resume the folds' stand-downs; none is resumed", dlog.WithCause(nil, err))
		return
	}
	resumed, spared := []string{}, map[string]string{}
	for _, ws := range workspaces {
		session, found := sessions[ws.ID]
		if !folded[ws.ID] || ws.Closed || !found || session.Terminal != nil {
			continue
		}
		if spare, ok := ws.FoldSpare(); ok {
			spared[string(ws.ID)] = string(spare)
			continue
		}
		resumed = append(resumed, string(ws.ID))
		c.StandDownFolded(ws.ID)
	}
	log.Info(opFolded, "resumed the folds' stand-downs for the running sessions they withhold",
		dlog.Context{"resumed": resumed, "spared": spared})
}

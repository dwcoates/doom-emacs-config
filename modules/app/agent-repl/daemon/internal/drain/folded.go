package drain

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// opFolded is the operation a folded workspace's wait records under.
const opFolded = "daemon.drain.folded"

// FoldStandDownDelay is how long a session in a collapsed section keeps
// running before it is stood down (owner request, 2026-10-30). Its page is
// unloaded at the collapse; its shim waits this long, so a section folded
// and unfolded again within it finds every session still up.
const FoldStandDownDelay = 5 * time.Minute

// A COLLAPSE STANDS ITS SECTION'S SESSIONS DOWN FoldStandDownDelay AFTER THE
// LATEST COLLAPSE, never sooner. Every collapse that holds a workspace starts
// a fresh wait for it and supersedes the one before, so a section folded,
// unfolded and folded again is stood down the delay after the LAST fold: a
// wait that ends superseded does nothing. A wait that ends as the latest asks
// for the withheld stand-down (StandDownWithheld), whose attempt reads the
// workspace's visibility afresh, so a section unfolded meanwhile, or a
// workspace selected (which unfolds the sections holding it), keeps its
// session: nothing needs to cancel a wait.

// StandDownFolded asks for the stand-down of a workspace a collapse folded,
// FoldStandDownDelay from now. It returns at once; the wait runs on the
// detached lifetime, and the daemon's exit ends it with nothing stood down.
func (c *controller) StandDownFolded(ws ids.WorkspaceID) {
	c.mu.Lock()
	if c.folded == nil {
		c.folded = map[ids.WorkspaceID]uint64{}
	}
	c.folded[ws]++
	wait := c.folded[ws]
	c.mu.Unlock()
	fields := dlog.Context{"workspace": string(ws), "wait": wait, "delay": FoldStandDownDelay.String()}
	c.log.Debug(opFolded, "a folded workspace's stand-down waits out the fold's delay", fields)
	c.deps.Detach(func(ctx context.Context) {
		select {
		case <-ctx.Done():
			c.log.Info(opFolded, "the daemon is leaving; a folded workspace's stand-down is not taken", dlog.WithCause(fields, ctx.Err()))
			return
		case <-c.deps.Clock.After(FoldStandDownDelay):
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

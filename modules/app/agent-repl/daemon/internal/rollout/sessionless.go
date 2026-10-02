package rollout

import (
	"context"

	"claude-repld/internal/bringup"
	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// opBringUp is the operation a successor's session starts are recorded under.
const opBringUp = "daemon.rollout.bring_up"

// A SUCCESSOR NEVER SERVES AN OPEN WORKSPACE SESSION-LESS (owner ruling,
// 2026-10-02: "when the daemon bounces while emacs is still live, workspaces
// should always be started"). A fresh boot already starts every client-less
// workspace (internal/boot's BringUp), but a JOINING daemon reconciles nothing
// at boot and takes its workspaces over one by one, and two of the ways it
// comes to serve one left it with no session at all:
//
//   - a workspace HANDED OVER with a free lock: the outgoing daemon's shim had
//     died, or was inert, so the adoption had nothing to dial and adopted the
//     WSM facts alone;
//   - a workspace NEVER HANDED OVER, whose lock reads free: the takeover
//     looked for orphaned shims to adopt and, finding none, recovered
//     nothing.
//
// Either way the daemon served the workspace with nothing behind it. The feed
// draws history only from a live session's watch, so the page showed an EMPTY
// feed until the user's next prompt revived the session under a "starting up"
// hold (measured: workspace ship-gns, 2026-10-01 17:55:21 takeover, feed
// placed 0 rows, revived by a prompt at 17:57:06). Both paths now start the
// session through the boot's own bring-up.

// bringUpSessionless raises the bring-up marker on each pending workspace AT
// ONCE -- before the caller publishes its views, so no surface draws it idle
// and usable -- and starts their sessions off the caller's goroutine. The
// starts are joined by bringUps; each start's own failure is that workspace's
// fault, raised by the start and counted by bringup.Run.
func (c *controller) bringUpSessionless(pending []wsm.Workspace, fields dlog.Context) {
	if len(pending) == 0 {
		return
	}
	for _, ws := range pending {
		c.deps.BringingUp(ws.ID, true)
	}
	names := make([]string, len(pending))
	for i, ws := range pending {
		names[i] = string(ws.ID)
	}
	c.log.Info(opBringUp, "starting the sessions of open workspaces this daemon serves with no session behind them",
		merge(fields, dlog.Context{"workspaces": names}))
	lifetime := c.lifetime(context.Background())
	c.bringUps.Add(1)
	go func() {
		defer c.bringUps.Done()
		bringup.Run(lifetime, bringup.Deps{
			DB:           c.deps.DB,
			StartSession: c.deps.StartSession,
			BringingUp:   c.deps.BringingUp,
			Log:          c.log,
			Operation:    opBringUp,
		}, pending)
	}()
}

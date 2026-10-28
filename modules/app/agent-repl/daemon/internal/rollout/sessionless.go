package rollout

import (
	"context"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/wsm"
)

// opBringUp is the operation a successor's session starts are recorded under.
const opBringUp = "daemon.rollout.bring_up"

// A SUCCESSOR NEVER SERVES AN OPEN VISIBLE WORKSPACE SESSION-LESS (owner ruling,
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

// ONLY VISIBLE WORKSPACES WHOSE SHIM WAS UP RUN (owner principle, 2026-10-12;
// owner ruling, 2026-10-28): a workspace the user hid, or whose repository
// section is collapsed, or whose shim was parked, killed or never started, is
// not started here; a prompt brings it up. A workspace owed a cut turn is the
// exception: the owed turn is a prompt. bringUpFilter is the takeover's one
// door onto the shared filters (bringup.OnlyVisible, OnlyWasUp, OnlyOwed).

// bringUpFilter keeps the pending workspaces a takeover brings up: the
// visible ones whose shim was up when the last daemon stopped
// (bringup.OnlyVisible, bringup.OnlyWasUp, owner ruling 2026-10-28), and any
// owed a cut turn whatever its visibility or park (bringup.OnlyOwed). A read
// that fails starts none of them, at ERROR: each still starts on its next
// prompt.
func (c *controller) bringUpFilter(ctx context.Context, pending []wsm.Workspace, fields dlog.Context) []wsm.Workspace {
	failed := func(err error) []wsm.Workspace {
		c.log.Error(opBringUp, "could not tell which session-less workspaces to bring up; none of them is started",
			dlog.WithCause(dlog.Merge(fields, dlog.Context{"workspaces": bringup.IDs(pending)}), err))
		return nil
	}
	visible, withheld, err := bringup.OnlyVisible(ctx, c.deps.DB, pending)
	if err != nil {
		return failed(err)
	}
	up, left, err := bringup.OnlyWasUp(ctx, c.deps.DB, visible)
	if err != nil {
		return failed(err)
	}
	owed, _, err := bringup.OnlyOwed(ctx, c.deps.DB, append(append([]wsm.Workspace(nil), withheld...), left...))
	if err != nil {
		return failed(err)
	}
	if len(withheld) > 0 {
		c.log.Info(opBringUp, "hidden or collapsed workspaces this daemon serves are not started; a prompt brings each up",
			dlog.Merge(fields, dlog.Context{"workspaces": bringup.IDs(withheld)}))
	}
	if len(left) > 0 {
		c.log.Info(opBringUp, "workspaces whose shim was not up when the last daemon stopped are not started; a prompt brings each up",
			dlog.Merge(fields, dlog.Context{"workspaces": bringup.IDs(left)}))
	}
	if len(owed) > 0 {
		c.log.Info(opBringUp, "workspaces owed a turn the last stop cut are started to resume it, whatever their visibility or park",
			dlog.Merge(fields, dlog.Context{"workspaces": bringup.IDs(owed)}))
	}
	return append(up, owed...)
}

// bringUpSessionless raises the bring-up marker on each pending workspace the
// takeover brings up (bringUpFilter) AT ONCE -- before the caller publishes
// its views, so no surface draws it idle and usable -- and starts their
// sessions (startSessionless).
func (c *controller) bringUpSessionless(pending []wsm.Workspace, fields dlog.Context) {
	pending = c.bringUpFilter(c.lifetime(context.Background()), pending, fields)
	for _, ws := range pending {
		c.deps.BringingUp(ws.ID, true)
	}
	c.startSessionless(pending, fields)
}

// startSessionless starts the sessions of pending workspaces whose bring-up
// marker the caller has ALREADY raised, off the caller's goroutine. The starts
// are joined by bringUps; each start's own failure is that workspace's fault,
// raised by the start and counted by bringup.Run, which lowers each marker.
func (c *controller) startSessionless(pending []wsm.Workspace, fields dlog.Context) {
	if len(pending) == 0 {
		return
	}
	names := make([]string, len(pending))
	for i, ws := range pending {
		names[i] = string(ws.ID)
	}
	c.log.Info(opBringUp, "starting the sessions of open workspaces this daemon serves with no session behind them",
		dlog.Merge(fields, dlog.Context{"workspaces": names}))
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
			Via:          restarttiming.StartTakeover,
			Restarts:     c.deps.Restarts,
			Now:          c.deps.Clock.Now,
		}, pending)
	}()
}

package rollout

import (
	"context"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
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

// ONLY A SHIM THAT WAS UP WHEN THE LAST DAEMON STOPPED COMES BACK (owner
// ruling, 2026-10-28), hidden or not: the session record states it at every
// park and unpark (bringup.OnlyWasUp), and nothing else decides. A workspace
// whose shim was parked, killed or never started is not started here; a
// prompt brings it up.
//
// EACH DECISION IS RECORDED WITH THE FACTS THAT MADE IT, one record per
// workspace (takeoverSplit): the rule, what the records say of its shim (a
// live session, its terminal and why, a recorded cold gate), its visibility
// and standing, and the lock the caller read. On 2026-11-23 a handover
// started 26 session-less workspaces and left 54, and no record said which
// fact put each one in which set.

// takeoverRule names why a takeover started or left one workspace.
type takeoverRule string

const (
	// takeoverColdGate starts a workspace standing at a recorded cold gate:
	// the start raises the gate from its record.
	takeoverColdGate takeoverRule = "cold_gate"
	// takeoverWasUp starts a workspace whose session record says its shim
	// was up when the last daemon stopped: nothing stood it down.
	takeoverWasUp takeoverRule = "was_up"
	// takeoverNotUp leaves a workspace whose shim was parked, killed or
	// never started.
	takeoverNotUp takeoverRule = "not_up"
)

// takeoverRuleOf is the rule the takeover applies to one workspace, by what
// its records say (bringup.WasUp.Up).
func takeoverRuleOf(facts bringup.WasUp) takeoverRule {
	switch {
	case facts.ColdGate:
		return takeoverColdGate
	case facts.Live:
		return takeoverWasUp
	default:
		return takeoverNotUp
	}
}

// takeoverSplit splits PENDING into the workspaces the takeover brings up
// and the ones it leaves, each in pending's order, recording each decision
// with the facts that made it. A read that fails splits nothing, at ERROR,
// and answers ok false: each still starts on its next prompt.
func (c *controller) takeoverSplit(ctx context.Context, pending []wsm.Workspace, fields dlog.Context) (up, left []wsm.Workspace, ok bool) {
	if len(pending) == 0 {
		return nil, nil, true
	}
	facts, err := bringup.WasUpFacts(ctx, c.deps.DB, pending)
	if err != nil {
		c.log.Error(opBringUp, "could not tell which session-less workspaces' shims were up; none of them is started",
			dlog.WithCause(dlog.Merge(fields, dlog.Context{"workspaces": bringup.IDs(pending)}), err))
		return nil, nil, false
	}
	for _, ws := range pending {
		rule := takeoverRuleOf(facts[ws.ID])
		decided := dlog.Merge(fields, takeoverFacts(ws, facts[ws.ID], rule))
		switch rule {
		case takeoverColdGate:
			c.log.Info(opBringUp, "the workspace stands at a recorded cold gate; the takeover starts it, which raises the gate", decided)
			up = append(up, ws)
		case takeoverWasUp:
			c.log.Info(opBringUp, "the workspace's session record says its shim was up when the last daemon stopped; the takeover starts it", decided)
			up = append(up, ws)
		default:
			c.log.Info(opBringUp, "the workspace's shim was not up when the last daemon stopped; the takeover does not start it, and a prompt brings it up", decided)
			left = append(left, ws)
		}
	}
	return up, left, true
}

// takeoverFacts states, for one workspace's takeover decision, the rule and
// every fact it was decided on: what the records say of its shim, and its
// visibility and standing in the roster.
func takeoverFacts(ws wsm.Workspace, facts bringup.WasUp, rule takeoverRule) dlog.Context {
	ctx := dlog.Merge(facts.Context(), dlog.Context{
		"workspace":       string(ws.ID),
		"rule":            string(rule),
		"visibility":      ws.Visibility.String(),
		"hidden":          ws.Hidden,
		"current":         ws.Current,
		"deploy_mediator": ws.DeployMediator,
	})
	if ws.Priority != nil {
		ctx["priority"] = int(*ws.Priority)
	}
	return ctx
}

// bringUpSessionless splits PENDING (takeoverSplit), raises the bring-up
// marker on each workspace it brings up AT ONCE -- before the caller
// publishes its views, so no surface draws it idle and usable -- and starts
// their sessions (startSessionless). It answers the workspaces left, and ok
// false when the split could not be read and nothing was decided.
func (c *controller) bringUpSessionless(pending []wsm.Workspace, fields dlog.Context) (left []wsm.Workspace, ok bool) {
	up, left, ok := c.takeoverSplit(c.lifetime(context.Background()), pending, fields)
	for _, ws := range up {
		c.deps.BringingUp(ws.ID, true)
	}
	c.startSessionless(up, fields)
	return left, ok
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
	inflight.Go(func() {
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
	})
}

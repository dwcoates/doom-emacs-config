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

// A HANDOVER PICKS UP WHERE THINGS WERE LEFT AND STARTS NOTHING THAT WAS NOT
// ALREADY STARTED (owner ruling, 2026-11-23, replacing the 2026-10-02 ruling
// that a successor "always" started a session-less workspace). A session
// running when the incumbent stopped is handed over running: its shim holds
// the workspace's lock and the successor dials it. Every workspace that
// reaches this file reads a FREE lock instead (a handover adopted with nothing
// to dial, or a takeover's orphan pass finding no shim), so nothing was
// running behind it:
//
//   - a workspace standing at a recorded cold gate is started, which only
//     raises the gate from its record and runs no shim (takeoverColdGate);
//   - a parked or hibernated session stays parked, and a workspace with no
//     session stays without one, until a prompt or the user opening it brings
//     it up (takeoverNotUp);
//   - a record that still reads running is a session whose end went
//     unrecorded (takeoverRecordStale): the takeover records the end, as a
//     park, and starts nothing.
//
// The decision reads one recorded fact, whether the session was running when
// the incumbent stopped, which every path that starts, parks, hibernates or
// ends a session writes (internal/workspace/terminalrecord.go). The stale arm
// repairs what that fact said before every shim end wrote it: at the
// 2026-11-23 15:35 handover five visible workspaces whose keep-alive parks
// had failed minutes earlier read as running, and their shims were spawned
// under the handover's load. A boot cannot make the same repair: after a
// host restart every shim is gone and every lock reads free, and a session
// running when the daemon stopped must come back.
//
// EACH DECISION IS RECORDED WITH THE FACTS THAT MADE IT, one record per
// workspace (takeoverSplit): the rule, what the records say of its shim (a
// live session, its terminal and why, a recorded cold gate), its visibility
// and standing, and the lock the caller read.

// takeoverRule names why a takeover started or left one workspace.
type takeoverRule string

const (
	// takeoverColdGate starts a workspace standing at a recorded cold gate:
	// the start raises the gate from its record.
	takeoverColdGate takeoverRule = "cold_gate"
	// takeoverRecordStale leaves a workspace whose session record reads
	// running with no shim holding its lock, and records its end.
	takeoverRecordStale takeoverRule = "record_stale"
	// takeoverNotUp leaves a workspace whose shim was parked, killed or
	// never started.
	takeoverNotUp takeoverRule = "not_up"
)

// staleDetail is the terminal detail of a session end the takeover recorded
// for a record that still read running (takeoverRecordStale).
const staleDetail = "stale: the session record read running, but no shim held the workspace's lock at the handover"

// takeoverRuleOf is the rule the takeover applies to one workspace whose lock
// reads free, by what its records say.
func takeoverRuleOf(facts bringup.WasUp) takeoverRule {
	switch {
	case facts.ColdGate:
		return takeoverColdGate
	case facts.Live:
		return takeoverRecordStale
	default:
		return takeoverNotUp
	}
}

// takeoverSplit splits PENDING, workspaces whose lock reads free, into the
// ones the takeover brings up and the ones it leaves, each in pending's order,
// recording each decision with the facts that made it, and recording the end
// of each stale record (takeoverRecordStale). A read that fails splits
// nothing, at ERROR, and answers ok false: each still starts on its next
// prompt.
func (c *controller) takeoverSplit(ctx context.Context, pending []wsm.Workspace, fields dlog.Context) (up, left []wsm.Workspace, facts map[wsm.WorkspaceID]bringup.WasUp, ok bool) {
	if len(pending) == 0 {
		return nil, nil, nil, true
	}
	facts, err := bringup.WasUpFacts(ctx, c.deps.DB, pending)
	if err != nil {
		c.log.Error(opBringUp, "could not tell which session-less workspaces' shims were up; none of them is started",
			dlog.WithCause(dlog.Merge(fields, dlog.Context{"workspaces": bringup.IDs(pending)}), err))
		return nil, nil, nil, false
	}
	for _, ws := range pending {
		rule := takeoverRuleOf(facts[ws.ID])
		decided := dlog.Merge(fields, takeoverFacts(ws, facts[ws.ID], rule))
		switch rule {
		case takeoverColdGate:
			c.log.Info(opBringUp, "the workspace stands at a recorded cold gate; the takeover starts it, which raises the gate", decided)
			up = append(up, ws)
		case takeoverRecordStale:
			c.log.Info(opBringUp, "the session record reads running but no shim held the workspace's lock at the handover; the takeover records the session's end and does not start it", decided)
			c.recordStale(ctx, ws, decided)
			left = append(left, ws)
		default:
			c.log.Info(opBringUp, "the workspace's shim was not up when the last daemon stopped; the takeover does not start it, and a prompt brings it up", decided)
			left = append(left, ws)
		}
	}
	return up, left, facts, true
}

// recordStale records the end of WS's session, whose record still read
// running with no shim behind it, as a park. A failure is recorded at ERROR
// and starts nothing: the next takeover repairs it again, and a prompt brings
// the workspace up meanwhile.
func (c *controller) recordStale(ctx context.Context, ws wsm.Workspace, fields dlog.Context) {
	// THROUGH THE FLEET, under the lock that orders it against a session's
	// clear: a prompt drained during the adoption may revive the workspace
	// meanwhile, and its session's record must never read parked.
	if err := c.deps.Shims.RecordEndedUnlessHeld(ctx, c.log.With(fields), ws.ID, wsm.TerminalParked, staleDetail); err != nil {
		c.log.Error(opBringUp, "could not record the end of a session whose record read running with no shim behind it; it is not started",
			dlog.WithCause(fields, err))
	}
}

// takeoverFacts states, for one workspace's takeover decision, the rule and
// every fact it was decided on: what the records say of its shim, and its
// visibility and standing in the roster.
func takeoverFacts(ws wsm.Workspace, facts bringup.WasUp, rule takeoverRule) dlog.Context {
	ctx := dlog.Merge(facts.Context(), dlog.Context{
		"workspace":  string(ws.ID),
		"rule":       string(rule),
		"visibility": ws.Visibility.String(),
		"backlogged": ws.Backlogged,
		"current":    ws.Current,
		"mediator":   string(ws.Mediator),
	})
	if ws.Priority != nil {
		ctx["priority"] = int(*ws.Priority)
	}
	return ctx
}

// bringUpSessionless splits PENDING (takeoverSplit), raises the bring-up
// marker AT ONCE on each workspace it brings up whose start runs a shim --
// before the caller publishes its views, so no surface draws it idle and
// usable -- and starts their sessions (startSessionless). A workspace at its
// recorded cold gate, the one kind a takeover starts, runs no shim, so no
// marker is raised for it (bringup.RaiseMarkers). It answers the workspaces
// left, and ok false when the split could not be read and nothing was
// decided.
func (c *controller) bringUpSessionless(pending []wsm.Workspace, fields dlog.Context) (left []wsm.Workspace, ok bool) {
	up, left, facts, ok := c.takeoverSplit(c.lifetime(context.Background()), pending, fields)
	bringup.RaiseMarkers(c.deps.BringingUp, up, facts)
	c.startSessionless(up, fields)
	return left, ok
}

// startSessionless starts the sessions of pending workspaces whose bring-up
// marker the caller has ALREADY raised where one stands, off the caller's
// goroutine. The starts
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

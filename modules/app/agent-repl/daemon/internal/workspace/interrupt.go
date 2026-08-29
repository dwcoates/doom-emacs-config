package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// InterruptOutcome is what an interrupt did, which is an ANSWER in every case —
// "nothing was running" is a success, not a failure.
type InterruptOutcome struct {
	// Turn reports that the running turn was interrupted.
	Turn bool
	// DetachedCount is how many detached items were stopped, zero when none
	// were.
	DetachedCount int
	// NothingRunning reports that there was nothing to interrupt.
	NothingRunning bool
}

// ConfirmRequired is the challenge an unconfirmed turn interrupt raises while
// detached agents are live: killing the turn would take them with it, so the
// user is asked once, with the count, and answers by resending with confirm.
type ConfirmRequired struct {
	// LiveAgentCount is how many detached agents the interrupt would take.
	LiveAgentCount int
}

// Error renders the challenge as the intended arm, so a transport that has no
// typed arm for it still answers the exact sentence the ledger prescribes.
func (c *ConfirmRequired) Error() string {
	return fmt.Sprintf("intended arm: InterruptError.confirm_required: %d detached agents are live", c.LiveAgentCount)
}

// Interrupt stops what the target names.
//
//   - TURN: KillTurn{force: confirm}. When detached agents are live and the
//     caller has not confirmed, the confirm_required challenge is raised FIRST
//     and nothing is killed.
//   - DETACHED: the addressed bubble decodes to either a detached subagent
//     (UpdateAgent.stop) or a detached shell (StopBash) — the row kind decides,
//     never a guess.
//   - ALL AGENTS: every live detached agent is stopped and the count is
//     reported.
//
// The footer's waiting-interrupting status fires the MOMENT the interrupt
// registers, before the real turn end arrives, because the turn end is what the
// shim reports and the user asked now.
func (v *verbs) Interrupt(ctx context.Context, ws ids.WorkspaceID, target InterruptTarget, confirm bool) (InterruptOutcome, error) {
	_, log, err := v.owned(ctx, "Interrupt", ws)
	if err != nil {
		return InterruptOutcome{}, err
	}

	running, live := v.deps.Freeness(ws)
	shim, hasShim := v.deps.Shim(ws)
	if !live || !hasShim {
		log.Debug(opInterrupt, "nothing is running", dlog.Context{"reason": "no live session"})
		return InterruptOutcome{NothingRunning: true}, nil
	}

	switch {
	case target.Turn:
		return v.interruptTurn(ctx, log, ws, shim, running, confirm)
	case target.Detached != nil:
		return v.interruptDetached(ctx, log, ws, shim, *target.Detached)
	case target.AllAgents:
		return v.interruptAllAgents(ctx, log, ws, shim, running)
	default:
		return InterruptOutcome{}, fmt.Errorf("interrupt %q: the target names nothing", ws)
	}
}

// interruptTurn kills the open turn, raising the confirm challenge first when
// detached agents would go with it.
func (v *verbs) interruptTurn(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, shim Shim, running Running, confirm bool) (InterruptOutcome, error) {
	if running.Turn == nil {
		log.Debug(opInterrupt, "nothing is running", dlog.Context{"target": "turn"})
		return InterruptOutcome{NothingRunning: true}, nil
	}
	liveAgents := len(running.LiveWork.Agents)
	if liveAgents > 0 && !confirm {
		log.Warn(opInterrupt, "refused an unconfirmed turn interrupt while agents are live", dlog.Context{
			"live_agent_count": liveAgents,
		})
		return InterruptOutcome{}, &ConfirmRequired{LiveAgentCount: liveAgents}
	}

	// The status fires now, not at the turn's real end.
	v.deps.Footer.SetInterrupting(ws, true)

	if err := shim.KillTurn(ctx, *running.Turn, confirm); err != nil {
		v.deps.Footer.SetInterrupting(ws, false)
		log.Error(opInterrupt, "the turn kill failed", dlog.Context{
			"turn": string(*running.Turn), "force": confirm, "cause": err.Error(),
		})
		return InterruptOutcome{}, fmt.Errorf("interrupt %q: kill turn %q: %w", ws, *running.Turn, err)
	}

	// A workspace whose merge is queued raises the dequeue offer on an
	// interrupt: the user interrupting a queued workspace is asking about the
	// merge as much as about the turn.
	v.deps.Merge.OnInterrupt(ctx, ws)

	log.Info(opInterrupt, "interrupted the running turn", dlog.Context{
		"turn": string(*running.Turn), "force": confirm, "live_agent_count": liveAgents,
	})
	return InterruptOutcome{Turn: true}, nil
}

// interruptDetached stops ONE detached bubble. The row's kind decides which
// stop verb it is: a subagent bubble stops through UpdateAgent, a shell bubble
// through StopBash, and any other kind is not a detached item at all.
func (v *verbs) interruptDetached(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, shim Shim, ref feedid.Ref) (InterruptOutcome, error) {
	fields := dlog.Context{"row_kind": string(ref.Row.Kind), "row_id": ref.Row.ID}
	switch ref.Row.Kind {
	case feedid.KindDetachedSubagent:
		agent := &conversationv1.AgentId{Value: ref.Row.ID}
		if err := shim.StopAgent(ctx, agent); err != nil {
			log.Error(opInterrupt, "could not stop the detached agent", withCause(fields, err))
			return InterruptOutcome{}, fmt.Errorf("interrupt %q: stop agent %q: %w", ws, ref.Row.ID, err)
		}
		log.Info(opInterrupt, "stopped a detached agent", fields)
		return InterruptOutcome{DetachedCount: 1}, nil
	case feedid.KindDetachedShell:
		work := &conversationv1.DetachedWorkId{Value: ref.Row.ID}
		if err := shim.StopBash(ctx, work); err != nil {
			log.Error(opInterrupt, "could not stop the detached shell", withCause(fields, err))
			return InterruptOutcome{}, fmt.Errorf("interrupt %q: stop shell %q: %w", ws, ref.Row.ID, err)
		}
		log.Info(opInterrupt, "stopped a detached shell", fields)
		return InterruptOutcome{DetachedCount: 1}, nil
	default:
		return InterruptOutcome{}, refuse(log, "Interrupt", ArmUnservedAnswer,
			fmt.Sprintf("row kind %q addresses no detached work", ref.Row.Kind), true)
	}
}

// interruptAllAgents stops every live detached agent and reports the count. It
// is fan-wide over AGENTS only: a detached shell is stopped by naming it, never
// by a sweep.
func (v *verbs) interruptAllAgents(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, shim Shim, running Running) (InterruptOutcome, error) {
	if len(running.LiveWork.Agents) == 0 {
		log.Debug(opInterrupt, "nothing is running", dlog.Context{"target": "all_agents"})
		return InterruptOutcome{NothingRunning: true}, nil
	}
	stopped := 0
	for _, agent := range running.LiveWork.Agents {
		if err := shim.StopAgent(ctx, agent); err != nil {
			log.Error(opInterrupt, "could not stop a detached agent", dlog.Context{
				"agent": agent.GetValue(), "stopped_so_far": stopped, "cause": err.Error(),
			})
			return InterruptOutcome{}, fmt.Errorf("interrupt %q: stop agent %q: %w", ws, agent.GetValue(), err)
		}
		stopped++
	}
	log.Info(opInterrupt, "stopped every live detached agent", dlog.Context{"count": stopped})
	return InterruptOutcome{DetachedCount: stopped}, nil
}

// withCause adds a failure's cause to a record's context without mutating the
// caller's map.
func withCause(fields dlog.Context, err error) dlog.Context {
	out := make(dlog.Context, len(fields)+1)
	for k, val := range fields {
		out[k] = val
	}
	out["cause"] = err.Error()
	return out
}

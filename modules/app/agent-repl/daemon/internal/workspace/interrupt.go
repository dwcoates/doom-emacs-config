package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
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
//   - ALL AGENTS: every live detached item — agents AND shells — is stopped
//     and the count is reported.
//
// The footer's waiting-interrupting status fires the MOMENT the interrupt
// registers, before the real turn end arrives, because the turn end is what the
// shim reports and the user asked now.
func (v *verbs) Interrupt(ctx context.Context, ws ids.WorkspaceID, target InterruptTarget, confirm bool) (InterruptOutcome, error) {
	_, log, err := v.owned(ctx, "Interrupt", ws)
	if err != nil {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "err != nil"})
		return InterruptOutcome{}, err
	}

	// A TURN STOP DURING A BRING-UP WITHDRAWS THE ACCEPTED TURN. The status
	// surfaces show a turn, and its stop, from the moment the prompt is
	// accepted, while the session it will run in is still coming up. Asked
	// FIRST, before the session is read: the queue answers false once the turn
	// is the session's, and by then the session already has it in flight.
	if target.Turn {
		withdrawn, err := v.deps.RevivalTurns.WithdrawRevivalTurn(ctx, ws)
		if err != nil {
			return InterruptOutcome{}, fmt.Errorf("interrupt %q: %w", ws, err)
		}
		if withdrawn {
			v.deps.Merge.OnInterrupt(ctx, ws)
			log.Debug(opInterrupt, "withdrew the accepted turn before its session came up", nil)
			return InterruptOutcome{Turn: true}, nil
		}
	}

	running, live := v.deps.Freeness(ws)
	shim, hasShim := v.deps.Shim(ws)
	if !live || !hasShim {
		if target.Turn {
			v.deps.Merge.OnInterrupt(ctx, ws)
		}
		log.Debug(opInterrupt, "nothing is running", dlog.Context{"reason": "no live session"})
		return InterruptOutcome{NothingRunning: true}, nil
	}

	switch {
	case target.Turn:
		// A workspace whose merge is queued raises the dequeue offer on an
		// interrupt WHATEVER the turn is doing: the user interrupting a queued
		// workspace is asking about the merge as much as about the turn, and
		// an idle queued workspace is precisely the case where the turn has
		// nothing to answer with. The offer is a no-op on a workspace with no
		// queued merge.
		v.deps.Merge.OnInterrupt(ctx, ws)
		return v.interruptTurn(ctx, log, ws, shim, running, confirm)
	case target.Detached != nil:
		return v.interruptDetached(ctx, log, ws, shim, *target.Detached)
	case target.AllAgents:
		return v.interruptAllAgents(ctx, log, ws, shim, running)
	default:
		return InterruptOutcome{}, fmt.Errorf("interrupt %q: the target names nothing", ws)
	}
}

// directCommand is a direct stop's HOW: the person stopped the turn itself.
func directCommand() *conversationv1.AgentInterruptedByUser {
	return &conversationv1.AgentInterruptedByUser{
		Command: &conversationv1.AgentInterruptedByUser_Direct{
			Direct: &conversationv1.AgentInterruptedByUserDirect{},
		},
	}
}

// interruptTurn kills the open turn, raising the confirm challenge first when
// detached agents would go with it.
func (v *verbs) interruptTurn(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, shim Shim, running Running, confirm bool) (InterruptOutcome, error) {
	if running.Turn == nil {
		log.Debug(opInterrupt, "nothing is running", dlog.Context{"target": "turn"})
		return InterruptOutcome{NothingRunning: true}, nil
	}

	// THE CHALLENGE COUNTS LIVE AGENTS, AND ONLY THEM. The arm's field is
	// `live_agent_count` and it means what it says: a detached SHELL is not an
	// agent, so it neither raises the challenge nor is counted by it. An
	// unconfirmed interrupt's kill is unforced, so it ends the synchronous turn
	// only and the shell runs on; a confirmed interrupt stops it — that is the
	// `detached` count below, which is a different question from how many
	// agents the user is being asked about.
	liveAgents := len(running.LiveWork.Agents)
	detached := liveAgents + len(running.LiveWork.Shells)
	if liveAgents > 0 && !confirm {
		log.Warn(opInterrupt, "refused an unconfirmed turn interrupt while detached work is live", dlog.Context{
			"live_agent_count": liveAgents,
		})
		return InterruptOutcome{}, &ConfirmRequired{LiveAgentCount: liveAgents}
	}

	// A CONFIRMED interrupt stops the detached work ITSELF, before the turn
	// kill: the confirmation is the user answering "also stop them", and the
	// stop is what makes that answer true rather than relying on the vendor to
	// reap the detached units as a side effect of the query dying.
	if detached > 0 {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "detached > 0"})
		if _, err := v.stopEveryDetached(ctx, log, ws, shim, running); err != nil {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "_, err := v.stopEveryDetached(ctx, log, ws, shim, running); err != nil"})
			return InterruptOutcome{}, err
		}
	}

	// The status fires now, not at the turn's real end.
	v.deps.Footer.SetInterrupting(ws, true)

	// THE USER STOPPED THE TURN ITSELF, and the record says so: the feed draws
	// the interruption bubble for a direct stop.
	if err := shim.KillTurn(ctx, *running.Turn, confirm, directCommand()); err != nil {
		v.deps.Footer.SetInterrupting(ws, false)
		if refusal, ok := AsShimRefusal(err); ok {
			if refusal.Benign() {
				// The turn ended between the freeness read and the kill. That
				// is an ANSWER, not a failure.
				log.Debug(opInterrupt, "nothing is running", dlog.Context{
					"target": "turn", "shim_arm": refusal.Arm,
				})
				return InterruptOutcome{NothingRunning: true}, nil
			}
			if refusal.Arm == ArmShimNotTheOpenTurn {
				// THE SHIM'S OPEN TURN IS NOT THE ONE THE DAEMON NAMED. The daemon
				// is the only queue and the shim's own keep-alive never shows on
				// this wire (a kill for a turn still being started waits in the
				// shim for the start to settle), so this is the daemon and the
				// shim disagreeing about which turn is open: a defect, recorded
				// at ERROR and answered as the `shim_refused` arm with the shim's
				// own words. InterruptError has no `not_the_open_turn` arm, so
				// relaying the name verbatim would reach the caller as an
				// unlanded-arm transport failure rather than a typed refusal.
				log.Error(opInterrupt, "the shim's open turn is not the turn the daemon interrupted", dlog.Context{
					"turn": string(*running.Turn), "shim_arm": refusal.Arm, "shim_detail": refusal.Detail,
				})
				return InterruptOutcome{}, refuse(log, "Interrupt", ArmShimRefused, refusal.Detail, false)
			}
			if refusal.Arm == ArmShimUnspecified {
				log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "refusal.Arm == ArmShimUnspecified"})
				// A failure whose kind oneof is unset names no landed arm. The
				// contract has a home for exactly that — shim_refused, "a typed
				// shim refusal relayed" — so the refusal is answered rather
				// than guessed at or dropped into an unlanded-arm error.
				return InterruptOutcome{}, refuse(log, "Interrupt", ArmShimRefused, refusal.Detail, false)
			}
			// The shim's own arm is propagated verbatim: the caller learns
			// WHICH refusal it was, not just that something refused.
			return InterruptOutcome{}, refuse(log, "Interrupt", refusal.Arm, refusal.Detail, false)
		}
		// THE FALLTHROUGH IS shim_refused, NOT A RAW TRANSPORT ERROR. The shim
		// would not perform the kill, and which layer said so — a typed failure
		// or the transport under it — is not something the caller can act on.
		// The record is still made at ERROR, so nothing is quietly downgraded.
		detail := shimclient.Detail(err)
		log.Error(opInterrupt, "the turn kill failed", dlog.Context{
			"turn": string(*running.Turn), "force": confirm, "cause": err.Error(),
		})
		return InterruptOutcome{}, refuse(log, "Interrupt", ArmShimRefused, detail, false)
	}

	log.Info(opInterrupt, "interrupted the running turn", dlog.Context{
		"turn": string(*running.Turn), "force": confirm, "live_agent_count": liveAgents,
	})
	return InterruptOutcome{Turn: true}, nil
}

// stopEveryDetached stops every live detached item — agents AND shells — and
// answers how many it reached. It is the ONE sweep both fan-wide stops use: the
// `all_agents` target and a CONFIRMED turn interrupt are the same user act
// ("stop the detached work"), so they must not drift apart in what they reach
// or in how they answer a refusal.
//
// A refusal that says the item is GONE — it finished on its own, or the shim no
// longer knows it at all, between the freeness read and the stop — is the state
// the caller asked for, so it is skipped rather than counted; every other
// refusal fails the sweep, because a user who asked for the work to stop must
// not be told it is gone when it is not.
func (v *verbs) stopEveryDetached(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, shim Shim, running Running) (int, error) {
	stopped := 0
	for _, agent := range running.LiveWork.Agents {
		fields := dlog.Context{"agent": agent.GetValue(), "stopped_so_far": stopped}
		if err := shim.StopAgent(ctx, agent); err != nil {
			if refusal, ok := AsShimRefusal(err); ok {
				if refusal.GoneFromTheSweep() {
					log.Debug(opInterrupt, "a detached agent was already not running", withArm(fields, refusal))
					continue
				}
				return stopped, refuse(log, "Interrupt", refusal.Arm, refusal.Detail, false)
			}
			log.Error(opInterrupt, "could not stop a detached agent", withCause(fields, err))
			return stopped, fmt.Errorf("interrupt %q: stop agent %q: %w", ws, agent.GetValue(), err)
		}
		log.Debug(opInterrupt, "stopped a detached agent", fields)
		stopped++
	}
	for _, shell := range running.LiveWork.Shells {
		fields := dlog.Context{"shell": shell.GetValue(), "stopped_so_far": stopped}
		if err := shim.StopBash(ctx, shell); err != nil {
			if refusal, ok := AsShimRefusal(err); ok {
				if refusal.GoneFromTheSweep() {
					log.Debug(opInterrupt, "a detached shell was already not running", withArm(fields, refusal))
					continue
				}
				return stopped, refuse(log, "Interrupt", refusal.Arm, refusal.Detail, false)
			}
			log.Error(opInterrupt, "could not stop a detached shell", withCause(fields, err))
			return stopped, fmt.Errorf("interrupt %q: stop shell %q: %w", ws, shell.GetValue(), err)
		}
		log.Debug(opInterrupt, "stopped a detached shell", fields)
		stopped++
	}
	return stopped, nil
}

// interruptDetached stops ONE detached bubble. The row decides which stop verb
// it is: a subagent bubble stops through UpdateAgent, a shell bubble through
// StopBash, and any other row is not a detached item at all.
//
// A SUBAGENT BUBBLE IS SERVED AS AN ACTIVITY ROW. The feed mints it as
// RowKey{Kind: activity, ID: <spawn unit>, Sub: <created agent id>} — feedid's
// own RowKey doc says exactly that, and resolve/feed/subagent.go is the one
// site that mints it; `detached_subagent` is a kind no resolver produces at
// all. endpoint_interrupt.proto addresses the detached target "by the bubble
// row's FeedId exactly as the feed served it", so the activity-with-a-subagent
// form is the form that actually arrives, and the agent to stop is the row's
// Sub.
func (v *verbs) interruptDetached(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, shim Shim, ref feedid.Ref) (InterruptOutcome, error) {
	fields := dlog.Context{"row_kind": string(ref.Row.Kind), "row_id": ref.Row.ID}
	switch {
	case ref.Row.Kind == feedid.KindDetachedSubagent || subagentBubble(ref.Row):
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "case ref.Row.Kind == feedid.KindDetachedSubagent || subagentBubble(ref.Row)"})
		agent := &conversationv1.AgentId{Value: subagentOf(ref.Row)}
		fields["agent"] = agent.GetValue()
		if err := shim.StopAgent(ctx, agent); err != nil {
			if outcome, refusal, handled := v.shimOutcome(log, opInterrupt, "Interrupt", fields, err); handled {
				log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "outcome, refusal, handled := v.shimOutcome(log, opInterrupt, \"Interrupt\", fields, err); handled"})
				return outcome, refusal
			}
			log.Error(opInterrupt, "could not stop the detached agent", withCause(fields, err))
			return InterruptOutcome{}, fmt.Errorf("interrupt %q: stop agent %q: %w", ws, ref.Row.ID, err)
		}
		log.Info(opInterrupt, "stopped a detached agent", fields)
		return InterruptOutcome{DetachedCount: 1}, nil
	case ref.Row.Kind == feedid.KindShellHead:
		// THE STOP LIVES ON THE HEAD. A detached shell's stop button is drawn on
		// its bubble HEAD (KindShellHead), whose ID is the run's own work id; the
		// spool BODY row (KindDetachedShell) on the sub-feed carries no control.
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "case ref.Row.Kind == feedid.KindShellHead"})
		work := &conversationv1.DetachedWorkId{Value: ref.Row.ID}
		if err := shim.StopBash(ctx, work); err != nil {
			if outcome, refusal, handled := v.shimOutcome(log, opInterrupt, "Interrupt", fields, err); handled {
				log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "outcome, refusal, handled := v.shimOutcome(log, opInterrupt, \"Interrupt\", fields, err); handled"})
				return outcome, refusal
			}
			log.Error(opInterrupt, "could not stop the detached shell", withCause(fields, err))
			return InterruptOutcome{}, fmt.Errorf("interrupt %q: stop shell %q: %w", ws, ref.Row.ID, err)
		}
		log.Info(opInterrupt, "stopped a detached shell", fields)
		return InterruptOutcome{DetachedCount: 1}, nil
	default:
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "default"})
		return InterruptOutcome{}, refuse(log, "Interrupt", ArmNotDetachedWork,
			fmt.Sprintf("row kind %q addresses no detached work", ref.Row.Kind), true)
	}
}

// interruptAllAgents stops EVERY live detached item at once — the fan-wide stop
// — and reports how many it reached. Fan-wide means the whole live set: a
// detached shell is detached work exactly as a detached subagent is, and a stop
// that left one behind would not have emptied anything, which is precisely what
// the caller asked for. (The turn interrupt's confirm challenge is a different
// question — how many AGENTS the user is being asked about — and counts agents
// only; see interruptTurn.)
func (v *verbs) interruptAllAgents(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, shim Shim, running Running) (InterruptOutcome, error) {
	if len(running.LiveWork.Agents) == 0 && len(running.LiveWork.Shells) == 0 {
		log.Debug(opInterrupt, "nothing is running", dlog.Context{"target": "all_agents"})
		return InterruptOutcome{NothingRunning: true}, nil
	}
	stopped, err := v.stopEveryDetached(ctx, log, ws, shim, running)
	if err != nil {
		return InterruptOutcome{}, err
	}
	if stopped == 0 {
		// Every item in the freeness snapshot turned out already gone, so the
		// sweep reached nothing. That is the `nothing_running` ANSWER, not an
		// `interrupted_detached` of zero: the stop found the session quiet.
		log.Debug(opInterrupt, "nothing is running", dlog.Context{"target": "all_agents", "reason": "every live item was already gone"})
		return InterruptOutcome{NothingRunning: true}, nil
	}
	log.Info(opInterrupt, "stopped every live detached item", dlog.Context{"count": stopped})
	return InterruptOutcome{DetachedCount: stopped}, nil
}

// subagentBubble reports whether a row is a subagent bubble: the feed mints one
// as an activity row whose secondary key is the created agent's id, so a Sub on
// an activity row is exactly what names a subagent.
func subagentBubble(row feedid.RowKey) bool {
	return row.Kind == feedid.KindActivity && row.Sub != ""
}

// subagentOf answers the agent id a subagent-addressing row names: the bubble's
// Sub when it has one, and the row's own id for the `detached_subagent` kind,
// whose primary key IS the agent.
func subagentOf(row feedid.RowKey) string {
	if row.Sub != "" {
		return row.Sub
	}
	return row.ID
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

// shimOutcome translates a single-target stop's error into an interrupt answer.
// It reports handled=false for anything that is not a typed shim refusal, so
// the caller still surfaces a transport failure as a failure.
//
// A BENIGN refusal is the "nothing running" answer: the thing the caller asked
// to stop had already stopped, which is exactly what they wanted. Every other
// arm is propagated by NAME, because "the row is stale" and "the SDK has no
// route" are different answers and the caller acts on them differently.
func (v *verbs) shimOutcome(log dlog.Logger, operation, rpc string, fields dlog.Context, err error) (InterruptOutcome, error, bool) {
	refusal, ok := AsShimRefusal(err)
	if !ok {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!ok"})
		return InterruptOutcome{}, nil, false
	}
	if refusal.Benign() {
		log.Debug(operation, "nothing is running", withArm(fields, refusal))
		return InterruptOutcome{NothingRunning: true}, nil, true
	}
	return InterruptOutcome{}, refuse(log, rpc, refusal.Arm, refusal.Detail, false), true
}

// withArm adds a shim refusal's arm to a record's context without mutating the
// caller's map.
func withArm(fields dlog.Context, refusal *ShimRefusal) dlog.Context {
	out := make(dlog.Context, len(fields)+2)
	for k, val := range fields {
		out[k] = val
	}
	out["shim_arm"] = refusal.Arm
	out["shim_detail"] = refusal.Detail
	return out
}

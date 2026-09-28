package promptqueue

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// opHandoff is the operation the queue's half of a mid-work handover records
// under: the seal on the daemon a workspace leaves, the install on the daemon
// that adopts it, and the take-back of a move that did not land.
const opHandoff = "daemon.promptqueue.handoff"

// ErrNoMoveToSeal refuses a seal asked of a workspace no dispatch-quiet move
// is running for: only the move's own action may seal what it carries.
var ErrNoMoveToSeal = errors.New("promptqueue: no dispatch-quiet move is running for the workspace, so there is nothing to seal")

// A MID-WORK HANDOVER CARRIES WHAT THE QUEUE HELD ONLY IN MEMORY (owner ruling,
// 2026-09-27: a daemon handover moves a workspace WITHOUT waiting for its work
// to end). The work itself survives by construction -- the shim runs on,
// detached, and every row it wrote is in the store -- but the queue's own
// ordering facts about that work are process memory: the session acts queued
// behind it, the context cut it IS, the semantic head an interjection
// installed. Lost, the next daemon would run a queued /compact never, or
// behind a prompt that was meant to follow it, or interrupt a /clear.
//
// So the move SEALS the queue (SealMove): under the delivery lock, then the
// verdict lock, it takes those facts out of this daemon's memory and hands
// them to the move, which writes them into the handover carry before it
// releases the workspace. The daemon that adopts it installs them before it
// dials the shim (AdoptHandoff), so no turn end it observes can pass them by.
// A move that does not land puts them back (UnsealMove).
//
// A VERDICT STILL BEING JUDGED IS NOT CARRIED, IT IS SUPERSEDED. The seal bumps
// the content epoch of every held prompt still stamped `classifying`, so a
// judge that answers on this daemon after the seal settles as a discard and
// can neither stamp nor interject a workspace this daemon has let go; the
// adopting daemon re-judges those prompts against the turn its adopted shim
// names (RejudgeHeld).

// SealMove implements Queue.
func (q *queue) SealMove(ctx context.Context, ws ids.WorkspaceID) (bounce.Handoff, []bounce.Request, error) {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return bounce.Handoff{}, nil, err
	}
	state := q.state(ws)
	state.drain.Lock()
	defer state.drain.Unlock()

	q.mu.Lock()
	pending := state.bounce
	running := pending != nil && pending.draining && pending.quietMove() && pending.move.started && !pending.sealed && !pending.kept
	q.mu.Unlock()
	if !running {
		log.Error(opHandoff, "a seal was asked of a workspace no dispatch-quiet move is running for", nil)
		return bounce.Handoff{}, nil, fmt.Errorf("seal %q: %w", ws, ErrNoMoveToSeal)
	}

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opHandoff, "could not read the standing holds to supersede their verdicts in flight; the move is not sealed", dlog.Context{"cause": err.Error()})
		return bounce.Handoff{}, nil, fmt.Errorf("seal %q: read the standing holds: %w", ws, err)
	}

	// THE VERDICT LOCK IS TAKEN AFTER THE DELIVERY LOCK, the one order the
	// queue takes them in: a verdict settling now finishes first (its record
	// and any interjection land before the seal), and every one after it
	// meets the bumped epoch.
	state.verdicts.Lock()
	superseded := 0
	for _, h := range standing {
		if h.Tombstone != nil || h.Classification == nil || h.Classification.Arm != wsm.ArmClassifying {
			continue
		}
		if state.epochs == nil {
			state.epochs = map[ids.TurnID]uint64{}
		}
		state.epochs[h.Turn]++
		superseded++
	}

	q.mu.Lock()
	handoff := bounce.Handoff{Interrupting: state.interrupting}
	for _, act := range state.acts {
		handoff.Acts = append(handoff.Acts, bounce.HandoffAct{
			Kind: act.Kind, Value: act.Value, Turn: string(act.Turn), Origin: int32(act.Origin),
		})
	}
	if state.cut != nil {
		handoff.Cut = &bounce.HandoffCut{Turn: string(state.cut.turn), Command: int32(state.cut.command)}
	}
	if state.head != nil {
		handoff.Head = string(*state.head)
	}
	state.acts, state.cut, state.head, state.interrupting = nil, nil, nil, false
	carried := pending.across
	pending.across = nil
	pending.sealed = true
	q.mu.Unlock()
	state.verdicts.Unlock()

	requests := make([]bounce.Request, 0, len(carried))
	for _, stage := range carried {
		requests = append(requests, carriedRequest(stage))
	}
	log.Info(opHandoff, "sealed the workspace's queue for its move; what it held in memory travels with the move", dlog.Context{
		"acts": len(handoff.Acts), "running_cut": handoff.Cut != nil, "semantic_head": handoff.Head,
		"interrupting": handoff.Interrupting, "verdicts_superseded": superseded, "replacements_carried": len(requests),
	})
	return handoff, requests, nil
}

// carriedRequest answers a carried stage as one request whose Done tells every
// requester that joined it.
func carriedRequest(stage *bounceStage) bounce.Request {
	req := stage.req
	dones := stage.dones
	req.Done = func(err error) {
		for _, done := range dones {
			done(err)
		}
	}
	return req
}

// UnsealMove implements Queue.
func (q *queue) UnsealMove(ctx context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	state := q.state(ws)
	state.drain.Lock()
	defer state.drain.Unlock()
	q.mu.Lock()
	if pending := state.bounce; pending != nil {
		pending.sealed = false
	}
	q.mu.Unlock()
	q.installLocked(ws, state, handoff, true)
	log.Info(opHandoff, "the move did not land; what the seal took is this daemon's again", dlog.Context{
		"acts": len(handoff.Acts), "running_cut": handoff.Cut != nil, "semantic_head": handoff.Head,
	})
	return nil
}

// AdoptHandoff implements Queue.
//
// IT RUNS BEFORE THE ADOPTED SHIM IS DIALED, so no turn end this daemon
// observes on it can overtake the acts or pass the cut by. A carried fact the
// store no longer bears out is dropped rather than installed: a cut whose turn
// is no longer open ended while no daemon watched it (its close is the
// adoption's own reconciliation), and a head whose prompt no longer stands was
// delivered or dropped.
func (q *queue) AdoptHandoff(ctx context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	if handoff.Empty() {
		log.Debug(opHandoff, "the handover carried nothing the queue held in memory", nil)
		return nil
	}
	state := q.state(ws)
	state.drain.Lock()
	defer state.drain.Unlock()

	if handoff.Cut != nil {
		open, err := q.deps.DB.OpenTurns(ctx, ws)
		if err != nil {
			log.Error(opHandoff, "could not read the open turns to install the carried context cut", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("adopt the handoff of %q: read the open turns: %w", ws, err)
		}
		if !openTurn(open, ids.TurnID(handoff.Cut.Turn)) {
			log.Info(opHandoff, "the carried context cut's turn is no longer open; it ended while no daemon watched it", dlog.Context{
				"session_act_turn": handoff.Cut.Turn,
			})
			handoff.Cut = nil
		}
	}
	if handoff.Head != "" {
		standing, err := q.deps.DB.HeldPrompts(ctx, ws)
		if err != nil {
			log.Error(opHandoff, "could not read the standing holds to install the carried semantic head", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("adopt the handoff of %q: read the standing holds: %w", ws, err)
		}
		if !standingTurn(standing, ids.TurnID(handoff.Head)) {
			log.Info(opHandoff, "the carried semantic head no longer stands; it is not installed", dlog.Context{"turn": handoff.Head})
			handoff.Head = ""
		}
	}
	q.installLocked(ws, state, handoff, false)
	log.Info(opHandoff, "installed what the previous daemon's queue held in memory for the workspace", dlog.Context{
		"acts": len(handoff.Acts), "running_cut": handoff.Cut != nil, "semantic_head": handoff.Head,
		"interrupting": handoff.Interrupting,
	})
	return nil
}

// installLocked puts a handoff into a workspace's memory. front puts the acts
// AHEAD of any queued since (a take-back: they were queued first); otherwise
// they are appended. The caller holds the delivery lock.
func (q *queue) installLocked(ws ids.WorkspaceID, state *wsState, handoff bounce.Handoff, front bool) {
	acts := make([]Act, 0, len(handoff.Acts))
	for _, a := range handoff.Acts {
		acts = append(acts, Act{
			Kind: a.Kind, Value: a.Value, Turn: ids.TurnID(a.Turn), Origin: conversationv1.PromptOrigin(a.Origin),
		})
	}
	q.mu.Lock()
	if front {
		state.acts = append(acts, state.acts...)
	} else {
		state.acts = append(state.acts, acts...)
	}
	if handoff.Cut != nil {
		state.cut = &runningCut{turn: ids.TurnID(handoff.Cut.Turn), command: conversationv1.SessionCommand(handoff.Cut.Command)}
	}
	if handoff.Head != "" {
		head := ids.TurnID(handoff.Head)
		state.head = &head
	}
	if handoff.Interrupting {
		state.interrupting = true
	}
	q.mu.Unlock()
	if handoff.Interrupting {
		q.deps.Footer.SetInterrupting(ws, true)
	}
}

// RejudgeHeld implements Queue.
func (q *queue) RejudgeHeld(ctx context.Context, ws ids.WorkspaceID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opHandoff, "could not read the standing holds to re-judge the verdicts the move superseded", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("re-judge the holds of %q: %w", ws, err)
	}
	var running *ids.TurnID
	if watcher, ok := q.deps.Watcher(ws); ok {
		running = watcher.TurnInFlight()
	}
	rejudged := 0
	for _, h := range standing {
		if h.Tombstone != nil || h.Hold != nil || h.Classification == nil || h.Classification.Arm != wsm.ArmClassifying {
			continue
		}
		if running == nil {
			// NOTHING RUNS TO JUDGE AGAINST: the turn ended during the move,
			// and the prompt is delivered in order as the held intake drains.
			log.Info(opHandoff, "a prompt the move superseded the verdict of has no running turn to be judged against; it is delivered in order", dlog.Context{"turn": string(h.Turn)})
			continue
		}
		q.classifyHeld(ctx, submissionOf(h), *running, log)
		rejudged++
	}
	if rejudged > 0 {
		log.Info(opHandoff, "re-judged the prompts whose verdicts the move superseded, against the adopted turn", dlog.Context{
			"rejudged": rejudged, "running_turn": string(*running),
		})
	}
	return nil
}

// openTurn reports whether turn is among the open turn rows.
func openTurn(open []wsm.Turn, turn ids.TurnID) bool {
	for _, t := range open {
		if t.ID == turn {
			return true
		}
	}
	return false
}

// standingTurn reports whether turn is a standing (untombstoned) hold.
func standingTurn(standing []wsm.HeldPrompt, turn ids.TurnID) bool {
	for _, h := range standing {
		if h.Turn == turn && h.Tombstone == nil {
			return true
		}
	}
	return false
}

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
var ErrNoMoveToSeal = errors.New("promptqueue: no move is running for the workspace, so there is nothing to seal")

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
	d := q.lockDelivery(ws)
	defer d.unlock()
	state := d.state

	q.mu.Lock()
	pending := state.bounce
	running := pending != nil && pending.draining && pending.move != nil && pending.move.started && !pending.sealed && !pending.kept
	q.mu.Unlock()
	if !running {
		log.Error(opHandoff, "a seal was asked of a workspace no move is running for", nil)
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
		state.bumpEpochLocked(h.Turn)
		superseded++
	}

	q.mu.Lock()
	handoff := bounce.Handoff{Interrupting: state.interrupting}
	if state.cut != nil {
		handoff.Cut = &bounce.HandoffCut{Turn: string(state.cut.turn), Command: int32(state.cut.command)}
	}
	if state.head != nil {
		handoff.Head = string(*state.head)
	}
	state.cut, state.head, state.interrupting = nil, nil, false
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
	d := q.lockDelivery(ws)
	defer d.unlock()
	state := d.state
	q.mu.Lock()
	if pending := state.bounce; pending != nil {
		pending.sealed = false
	}
	q.mu.Unlock()
	q.installLocked(ws, state, handoff)
	if err := q.verifyInstalledLocked(ctx, ws, handoff, log); err != nil {
		return err
	}
	log.Info(opHandoff, "the move did not land; what the seal took is this daemon's again", dlog.Context{
		"acts": len(handoff.Acts), "running_cut": handoff.Cut != nil, "semantic_head": handoff.Head,
	})
	return nil
}

// AdoptHandoff implements Queue.
//
// It runs under the delivery lock and BEFORE the handover hold is released, so
// nothing it installs can be overtaken: no held prompt is deliverable yet,
// and the release is what runs the carried acts ahead of them. A carried fact
// the store no longer bears out is retired rather than left standing: a cut
// whose turn is no longer open ended while no daemon watched it, and a head
// whose prompt no longer stands was delivered or dropped.
func (q *queue) AdoptHandoff(ctx context.Context, ws ids.WorkspaceID, handoff bounce.Handoff) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	if handoff.Empty() {
		log.Debug(opHandoff, "the handover carried nothing the queue held in memory", nil)
		return nil
	}
	d := q.lockDelivery(ws)
	defer d.unlock()
	state := d.state
	q.installLocked(ws, state, handoff)
	if err := q.verifyInstalledLocked(ctx, ws, handoff, log); err != nil {
		return err
	}
	if err := q.holdCarriedActs(ctx, ws, handoff.Acts, log); err != nil {
		return err
	}
	log.Info(opHandoff, "installed what the previous daemon's queue held in memory for the workspace", dlog.Context{
		"acts": len(handoff.Acts), "running_cut": handoff.Cut != nil, "semantic_head": handoff.Head,
		"interrupting": handoff.Interrupting,
	})
	return nil
}

// holdCarriedActs holds the session acts a handover from a daemon built before
// acts were durable held entries carried in its memory. They are held exactly
// as a new act that must wait is, at the tail of the queue: their submission
// instants were never recorded, so no truer place exists. A handover this
// build writes carries no acts, because its acts are already holds.
func (q *queue) holdCarriedActs(ctx context.Context, ws ids.WorkspaceID, carried []bounce.HandoffAct, log dlog.Logger) error {
	for _, a := range carried {
		act := Act{Kind: a.Kind, Value: a.Value, Turn: ids.TurnID(a.Turn), Origin: conversationv1.PromptOrigin(a.Origin)}
		switch act.Kind {
		case ActClear, ActCompact, ActSetModel, ActSetPermissionMode:
		default:
			log.Error(opHandoff, "the handover carried an act of a kind the queue does not carry; it is not held", dlog.Context{
				"act": a.Kind, "value": a.Value,
			})
			return fmt.Errorf("adopt %q: carried act %q is not a kind the queue carries", ws, a.Kind)
		}
		sub := heldActSubmission(ws, act)
		if _, err := q.hold(ctx, sub, "", nil, log); err != nil {
			return err
		}
		log.Info(opHandoff, "held a session act the previous daemon carried in memory", dlog.Context{
			"act": act.Kind, "value": act.Value, "held_turn": string(sub.Turn),
		})
	}
	return nil
}

// verifyInstalledLocked retires an installed cut whose turn is no longer open
// and an installed head whose prompt no longer stands.
//
// INSTALL, THEN VERIFY -- never the other way round. A turn's close retires
// the cut it finds (retireCutIf, from the turn-close door), and an unobserved
// close of the cut's turn runs off the delivery lock. Checked first and
// installed after, a close landing between the two finds no cut to retire and
// the cut is installed stale, holding every later prompt behind an act that is
// no longer running. Installed first, either the close finds it or this read
// sees the close. The caller holds the delivery lock.
func (q *queue) verifyInstalledLocked(ctx context.Context, ws ids.WorkspaceID, handoff bounce.Handoff, log dlog.Logger) error {
	if handoff.Cut != nil {
		open, err := q.deps.DB.OpenTurns(ctx, ws)
		if err != nil {
			log.Error(opHandoff, "could not read the open turns to verify the carried context cut; it is left installed", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("verify the handoff of %q: read the open turns: %w", ws, err)
		}
		if turn := ids.TurnID(handoff.Cut.Turn); !openTurn(open, turn) {
			q.retireCutIf(ws, turn, func() (dlog.Logger, bool) { return log, true })
			log.Info(opHandoff, "the carried context cut's turn is no longer open; it ended while no daemon watched it", dlog.Context{
				"session_act_turn": handoff.Cut.Turn,
			})
		}
	}
	if handoff.Head != "" {
		standing, err := q.deps.DB.HeldPrompts(ctx, ws)
		if err != nil {
			log.Error(opHandoff, "could not read the standing holds to verify the carried semantic head; it is left installed", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("verify the handoff of %q: read the standing holds: %w", ws, err)
		}
		if turn := ids.TurnID(handoff.Head); !standingTurn(standing, turn) {
			q.clearHeadIf(ws, turn)
			log.Info(opHandoff, "the carried semantic head no longer stands; it is retired", dlog.Context{"turn": handoff.Head})
		}
	}
	return nil
}

// installLocked puts a handoff into a workspace's memory. front puts the acts
// AHEAD of any queued since (a take-back: they were queued first); otherwise
// they are appended. The caller holds the delivery lock.
func (q *queue) installLocked(ws ids.WorkspaceID, state *wsState, handoff bounce.Handoff) {
	q.mu.Lock()
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

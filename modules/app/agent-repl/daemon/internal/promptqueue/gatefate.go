package promptqueue

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A COLD GATE'S ANSWER DOES EXACTLY WHAT ITS BUTTON SAYS (owner ruling,
// 2026-10-13; docs/protobuf-design/cold-gate-answer-holds.md). The gate exists
// so that nothing spends tokens without the user's say, so no answer sends a
// prompt held at it unless the button names the sending:
//
//   - clear DISCARDS them: clear is the gate's big red button, and spends
//     nothing at all;
//   - pay and compact KEEP them, on wsm.HoldColdGateAnswered, which nothing
//     delivers but the user's own release;
//   - compact-and-resume SENDS them: they stand on wsm.HoldColdGate, which
//     the compacted session coming up releases (releaseReconnectHolds).
//
// THE FATE IS SETTLED BEFORE THE SESSION IS BROUGHT BACK. The gate's answer
// calls ColdGateAnswered before it starts the re-open, and the re-open is the
// only thing that raises the session-up release, which delivers HoldColdGate
// entries alone. So a kept or discarded prompt is never delivered by the
// release the answer itself causes: the ordering is the call order, not a
// race the release usually loses.
//
// "Held at the gate" is either cold-gate hold: a prompt kept by an earlier
// pay or compact is still unsent, so a later gate's clear discards it and its
// compact-and-resume sends it.

// GateFate is what a cold gate's answer does to the prompts held at it.
type GateFate int

// The fates.
const (
	// GateDiscard discards them (the clear answer).
	GateDiscard GateFate = iota + 1
	// GateKeep keeps them for the user to send (the pay and compact answers).
	GateKeep
	// GateSend sends them once the remediated session is up (the
	// compact-and-resume answer).
	GateSend
)

// String names a fate, for logs.
func (f GateFate) String() string {
	switch f {
	case GateDiscard:
		return "discard"
	case GateKeep:
		return "keep"
	case GateSend:
		return "send"
	default:
		return fmt.Sprintf("gate_fate(%d)", int(f))
	}
}

// tombstoneCleared retires a prompt held at a cold gate the user answered with
// clear.
const tombstoneCleared = "cleared_at_cold_gate"

const opColdGate = "daemon.promptqueue.cold_gate_answered"

// heldAtGate reports whether a standing entry is held at a cold gate: under
// the gate's own hold, or kept by an earlier answer of one.
func heldAtGate(h wsm.HeldPrompt) bool {
	return h.Tombstone == nil && h.Hold != nil &&
		(*h.Hold == wsm.HoldColdGate || *h.Hold == wsm.HoldColdGateAnswered)
}

// countHeldAtGate counts the entries of STANDING held at a cold gate.
func countHeldAtGate(standing []wsm.HeldPrompt) int {
	n := 0
	for _, h := range standing {
		if heldAtGate(h) {
			n++
		}
	}
	return n
}

// ColdGateAnswered implements Queue.
func (q *queue) ColdGateAnswered(ctx context.Context, ws ids.WorkspaceID, fate GateFate) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"fate": fate.String()})
	d := q.lockDelivery(ws)
	defer d.unlock()

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opColdGate, "the prompts held at the cold gate could not be read; the answer is refused", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	var at []wsm.HeldPrompt
	for _, h := range standing {
		if heldAtGate(h) {
			at = append(at, h)
		}
	}
	if len(at) == 0 {
		log.Info(opColdGate, "no prompt is held at the cold gate; the answer has nothing to settle", nil)
		return nil
	}
	switch fate {
	case GateDiscard:
		return q.discardAtGate(ctx, d, at, log)
	case GateKeep:
		return q.restampAtGate(ctx, d, at, wsm.HoldColdGateAnswered, "the prompts held at the cold gate are kept until the user sends them", log)
	case GateSend:
		return q.restampAtGate(ctx, d, at, wsm.HoldColdGate, "the prompts held at the cold gate are sent once the remediated session is up", log)
	default:
		log.Error(opColdGate, "the cold gate's answer named no fate for its held prompts", dlog.Context{
			"invariant_violation": "GateFate is not one of the declared fates",
			"remediation":         "map every AnswerColdGate arm to a GateFate",
		})
		return fmt.Errorf("cold gate answer on %q: unknown fate %s", ws, fate)
	}
}

// discardAtGate retires AT in one store write, ALL OR NOTHING: a clear that
// discarded some of the held prompts and left others would send the rest into
// the cleared session.
func (q *queue) discardAtGate(ctx context.Context, d *delivery, at []wsm.HeldPrompt, log dlog.Logger) error {
	turns := make([]ids.TurnID, len(at))
	for i, h := range at {
		turns[i] = h.Turn
	}
	if err := q.deps.DB.TombstoneHeldPrompts(ctx, turns, wsm.Tombstone{Kind: tombstoneCleared, At: q.deps.Now()}); err != nil {
		log.Error(opColdGate, "the prompts held at the cold gate could not be discarded; the clear is refused", dlog.Context{
			"held": len(turns), "cause": err.Error(),
		})
		return fmt.Errorf("discard the holds at the cold gate of %q: %w", d.ws, err)
	}
	for _, turn := range turns {
		q.clearHeadIf(d.ws, turn)
		q.retireEditIf(ctx, d, turn, tombstoneCleared, log)
	}
	log.Info(opColdGate, "the clear discarded the prompts held at the cold gate", dlog.Context{"discarded": len(turns)})
	return q.pushTray(ctx, d.ws, log)
}

// restampAtGate puts every entry of AT on KIND. A store failure refuses the
// answer: the caller does not bring the session back, so an entry left on
// the other hold is never delivered by a release the answer caused.
func (q *queue) restampAtGate(ctx context.Context, d *delivery, at []wsm.HeldPrompt, kind wsm.HoldKind, said string, log dlog.Logger) error {
	moved := 0
	for _, h := range at {
		if *h.Hold == kind {
			continue
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, &kind, ""); err != nil {
			log.Error(opColdGate, "a prompt held at the cold gate could not be moved to its answer's hold; the answer is refused", dlog.Context{
				"turn": string(h.Turn), "hold": kind.String(), "moved": moved, "cause": err.Error(),
			})
			return fmt.Errorf("hold %q at the cold gate of %q: %w", h.Turn, d.ws, err)
		}
		moved++
	}
	log.Info(opColdGate, said, dlog.Context{"held": len(at), "moved": moved, "hold": kind.String()})
	if moved == 0 {
		return nil
	}
	return q.pushTray(ctx, d.ws, log)
}

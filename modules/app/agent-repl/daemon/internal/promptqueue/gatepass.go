package promptqueue

import (
	"context"
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// AN AGENT'S PROMPT PASSES THE COLD GATE IT WAITS AT (owner ruling,
// 2026-11-25: "an attempt to reopen a workspace and send it a prompt should
// do two things automatically: immediately compact the workspace with Haiku,
// and queue the prompt sent as a held prompt for after compaction").
//
// A prompt another workspace's agent sent (PROMPT_ORIGIN_AGENT_SENT) that
// meets a standing cold gate waits there under wsm.WaitColdGate, and the SAME
// step that records that wait starts the gate's pass: the daemon does on the
// sender's behalf exactly what `workspace pass-gate` does, through the one
// compaction path (Deps.GatePass, workspace.Verbs.PassColdGateForPrompts):
// Haiku, the whole context, the prompts the user left held at the gate kept
// for the user. The gate's answer then drives the wait (arrivalGateAnswered),
// and the session the compaction brings up delivers it.
//
// THE PASS IS STRUCTURAL, NOT TIMED. The wait is recorded and the pass is
// raised under the workspace's delivery lock, in one step, so there is no
// instant at which an agent's prompt waits at the gate with no pass running;
// a second prompt arriving while one runs finds it raised and joins it, so
// two prompts start one compaction.
//
// EACH PROMPT PASSES THE GATE AT MOST ONCE. A pass that fails stands the gate
// again (the answer's own re-raise), tells every sender whose prompt waits at
// the gate, and leaves the prompts waiting for the user's answer: a failing
// re-open is never retried in a loop (the merge queue's 2026-11-21 lesson).
//
// WHAT EACH ORIGIN DOES AT A STANDING GATE:
//
//   - an agent's prompt waits at the gate and passes it (passesColdGate);
//   - a daemon notice that waits out the gate (waitsOutColdGate) waits there
//     and passes nothing: the daemon composed it, so no one asked to spend;
//   - a prompt the user typed is refused, since the user answers the gate on
//     its card, and paying the cold read or clearing the context stay the
//     user's answers alone.

// opGatePass is every record of a cold gate's pass for an agent's prompt.
const opGatePass = "daemon.promptqueue.gate_pass"

// passesColdGate reports whether a prompt of ORIGIN, arriving at a standing
// cold gate, passes it: only a prompt another workspace's agent sent.
func passesColdGate(origin conversationv1.PromptOrigin) bool {
	return origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
}

// awaitColdGateLocked records SUB's wait at the standing cold gate, in words
// WHY, and, for a prompt whose origin passes the gate, starts the gate's pass
// in the same step (passGateLocked). The caller holds the delivery lock (d).
func (q *queue) awaitColdGateLocked(ctx context.Context, d *delivery, sub Submission, why string, log dlog.Logger) (Disposition, error) {
	disposition, err := q.awaitArrival(ctx, sub, wsm.WaitColdGate, why, log)
	if err != nil || !passesColdGate(sub.Origin) {
		return disposition, err
	}
	q.passGateLocked(d, sub.Turn, log)
	return disposition, nil
}

// passGateLocked starts the pass of the gate D's workspace stands at for
// TURN, or joins the one running. A turn whose pass already ran starts none.
// The caller holds the delivery lock (d).
func (q *queue) passGateLocked(d *delivery, turn ids.TurnID, log dlog.Logger) {
	ws := d.ws
	fields := dlog.Context{"turn": string(turn)}
	s := q.state(ws)
	q.mu.Lock()
	if _, spent := s.gatePassTurns[turn]; spent && !s.gatePass {
		q.mu.Unlock()
		log.Info(opGatePass, "the prompt's own pass of the cold gate already ran; it waits at the gate for the user's answer", fields)
		return
	}
	if s.gatePassTurns == nil {
		s.gatePassTurns = map[ids.TurnID]struct{}{}
	}
	s.gatePassTurns[turn] = struct{}{}
	running := s.gatePass
	s.gatePass = true
	q.mu.Unlock()
	if running {
		log.Info(opGatePass, "an agent's prompt waits at the cold gate a pass is already compacting; it joins that pass", fields)
		return
	}
	log.Info(opGatePass, "an agent's prompt waits at the cold gate; the gate is passed at once by a Haiku compaction of the whole context", fields)
	q.gatePasses.Add(1)
	inflight.ForWorkspace(string(ws)).Go(func() {
		defer q.gatePasses.Done()
		q.runGatePass(ws, log)
	})
}

// runGatePass runs the pass of WS's cold gate to its end, off the delivery
// lock: the gate's answer settles the queue's own waits under that lock
// (ColdGateAnswered).
func (q *queue) runGatePass(ws ids.WorkspaceID, log dlog.Logger) {
	up, err := q.deps.GatePass.PassColdGateForPrompts(q.life, ws)
	s := q.state(ws)
	q.mu.Lock()
	s.gatePass = false
	if err == nil {
		// THE GATE IS GONE OR ANSWERED: a turn that meets a gate again meets
		// a new one, which it may pass once.
		s.gatePassTurns = nil
	}
	q.mu.Unlock()
	switch {
	case err != nil:
		log.Error(opGatePass, "the pass of the cold gate an agent's prompt started failed; the prompts wait at the gate for the user's answer", dlog.Context{"cause": err.Error()})
		q.gatePassFailed(ws, err, log)
	case up:
		log.Info(opGatePass, "the pass of the cold gate brought the session up; the prompts that waited at it are delivered as it serves", nil)
	default:
		log.Info(opGatePass, "the pass of the cold gate found no gate to answer or was abandoned; the prompts wait for the gate's next answer", nil)
	}
}

// gatePassFailed puts the prompts the failed pass's answer drove back at the
// gate it stood again, and tells each sender whose prompt waits there.
func (q *queue) gatePassFailed(ws ids.WorkspaceID, cause error, log dlog.Logger) {
	ctx := context.WithoutCancel(q.life)
	d := q.lockDelivery(ws)
	// THE ANSWER DROVE THE WAITS AT THE GATE ON TO THE SESSION it was
	// bringing up; the gate stands again, so they wait at it again.
	q.driveArrivalsLocked(ctx, d, arrivalGateRaised, log)
	senders, err := q.gateSenders(ctx, ws)
	d.unlock()
	if err != nil {
		log.Error(opGatePass, "could not read the prompts waiting at the cold gate; no sender is told the pass failed", dlog.Context{"cause": err.Error()})
		return
	}
	for _, sender := range senders {
		q.deps.GatePass.TellColdGatePassFailed(ctx, sender, ws, cause)
	}
}

// gateSenders answers, once each and in a stable order, the workspaces whose
// agents' prompts wait at WS's cold gate.
func (q *queue) gateSenders(ctx context.Context, ws ids.WorkspaceID) ([]ids.WorkspaceID, error) {
	waits, err := q.deps.DB.ArrivalWaits(ctx, ws)
	if err != nil {
		return nil, err
	}
	seen := map[ids.WorkspaceID]bool{}
	var senders []ids.WorkspaceID
	for _, w := range waits {
		if w.Reason != wsm.WaitColdGate || w.SentBy == nil || seen[*w.SentBy] {
			continue
		}
		seen[*w.SentBy] = true
		senders = append(senders, *w.SentBy)
	}
	sort.Slice(senders, func(i, j int) bool { return senders[i] < senders[j] })
	return senders, nil
}

// forgetGatePass drops TURN from the prompts that passed WS's gate, once it
// was delivered.
func (q *queue) forgetGatePass(ws ids.WorkspaceID, turn ids.TurnID) {
	s := q.state(ws)
	q.mu.Lock()
	delete(s.gatePassTurns, turn)
	q.mu.Unlock()
}

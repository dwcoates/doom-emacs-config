package merge

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// This file is a merge whose workspace stands at its COLD GATE (owner ruling,
// 2026-11-11, reversing the 2026-10-13 eviction). The gate runs no session
// until the user answers it, and a merge's agentic steps run on that session:
//
//   - THE MERGE KEEPS ITS PLACE IN LINE AND IS SKIPPED. A waiting merge is
//     marked (wsm.SetMergeColdGated), and every admission decision reads the
//     line through admissible, so merges behind it overtake it. A merge
//     being worked on when the gate rises is taken off its run and put back
//     at its own place, marked, under a new bubble (wsm.RequeueColdGatedMerge):
//     it runs again from its start once it heads the queue again.
//   - THE QUEUE ANSWERS THE GATE ITSELF (owner ruling, 2026-11-21). A gate
//     stands on a queued merge's workspace whether it was raised live or
//     raised again from its record after a restart or a handover, and nobody
//     is waiting to answer it: a deploy once left every queued merge gated
//     and the queue admitted nothing for hours. So the raise's handling
//     passes the gate at once with the compaction pass-gate makes
//     (Deps.CompactColdGate), recorded under opColdGate. The merge stays
//     marked and skipped for as long as the compaction takes, and the
//     session it brings up retires the gate. The answer is made once per
//     place in line: a compaction that fails is an ERROR, and the merge
//     leaves its queue as failed with the gate standing for the user
//     (answerQueuedColdGate). THE QUEUE ANSWERS A GATE AT MOST ONCE: a
//     failed answer's re-open stands the gate again
//     (edgeStoodAfterFailedAnswer), and that stand is never answered by the
//     queue, which once answered, failed and stood the same gate every 10s
//     for 40 minutes (2026-11-21). A stand that arrives while the answer
//     runs is left to the answer's own record, and a stand on a merge that
//     waits in line says at ERROR that the gate stands for the user.
//   - A MERGE TURN NEVER MEETS A STANDING GATE (2026-11-22). Every turn the
//     orchestrator hands a merge's workspace passes the gate first
//     (passColdGate, from handOff): a pick or a repair brings the session
//     up itself, and its submission once raced the queue's answer to the
//     raise and lost. The turn joins the answer running, or makes the one
//     answer itself; a failed one fails the turn, which ends the merge.
//   - PASSING THE GATE BY AN ANSWER RESUMES IT THERE. Pay, clear and compact
//     bring a session up, which retires the gate's record and with it the
//     mark (wsm.ClearMergeColdGated); a merge the merges worked on meanwhile
//     overtook goes right behind them, ahead of every merge still waiting.
//     Its workspace's one-shot standing is never touched.
//   - PASSING THE GATE BY HIDING THE WORKSPACE TAKES IT OFF THE QUEUE
//     (OnBacklogged, CauseBackloggedAtColdGate); killing it does through the close
//     every kill makes (OnWorkspaceClosed).
//
// THE GATE'S EDGES ARE APPLIED IN THE ORDER THEY HAPPENED. A raise and the
// retirement after it arrive from the fleet's goroutines, and the raise's
// handling waits for a running merge to let go; applied concurrently, a
// retirement could be applied before the raise it follows and leave a merge
// marked under a live session. So every edge is queued in order and applied
// by one worker (coldGateEdge), off the fleet's goroutine: the merge being
// taken off may itself be what is bringing the session up.

// opColdGate is every record of a merge at its workspace's cold gate.
const opColdGate = "daemon.merge.cold_gate"

// coldGateEdgeKind is what one edge of a workspace's cold gate is.
type coldGateEdgeKind int

const (
	// edgeRaised is the workspace's session parking at its cold gate.
	edgeRaised coldGateEdgeKind = iota + 1
	// edgeRetired is the gate's record retired: the gate was passed.
	edgeRetired
	// edgeBacklogged is the workspace backlogged, which passes a standing gate
	// without an answer.
	edgeBacklogged
	// edgeStoodAfterFailedAnswer is the gate standing again because an
	// answer to it failed: kept as a raise is, never answered by the queue.
	edgeStoodAfterFailedAnswer
)

// coldGateEdge is one edge of a workspace's cold gate, as the queue applies it.
type coldGateEdge struct {
	ws   ids.WorkspaceID
	kind coldGateEdgeKind
}

// ColdGateRaised is told that WS's session parked at its cold gate.
func (o *orchestrator) ColdGateRaised(ws ids.WorkspaceID) {
	o.queueColdGateEdge(coldGateEdge{ws: ws, kind: edgeRaised})
}

// ColdGateStoodAfterFailedAnswer is told that WS's cold gate stood again
// because an answer to it failed.
func (o *orchestrator) ColdGateStoodAfterFailedAnswer(ws ids.WorkspaceID) {
	o.queueColdGateEdge(coldGateEdge{ws: ws, kind: edgeStoodAfterFailedAnswer})
}

// ColdGateRetired is told that WS's recorded cold gate was retired: a session
// came up, or the workspace was pointed at another conversation.
func (o *orchestrator) ColdGateRetired(ws ids.WorkspaceID) {
	o.queueColdGateEdge(coldGateEdge{ws: ws, kind: edgeRetired})
}

// queueColdGateEdge queues EDGE behind every edge before it and starts the
// one worker that applies them when none runs. A synchronous orchestrator (a
// test's) applies it at once, on the caller's goroutine.
func (o *orchestrator) queueColdGateEdge(edge coldGateEdge) {
	if !o.async {
		o.applyColdGateEdge(context.Background(), edge)
		return
	}
	o.coldMu.Lock()
	o.coldEdges = append(o.coldEdges, edge)
	if o.coldApplying {
		o.coldMu.Unlock()
		return
	}
	o.coldApplying = true
	o.coldMu.Unlock()
	o.live.Add(1)
	inflight.Standing(func() {
		defer o.live.Done()
		for {
			o.coldMu.Lock()
			if len(o.coldEdges) == 0 {
				o.coldApplying = false
				o.coldMu.Unlock()
				return
			}
			next := o.coldEdges[0]
			o.coldEdges = o.coldEdges[1:]
			o.coldMu.Unlock()
			o.applyColdGateEdge(context.Background(), next)
		}
	})
}

// applyColdGateEdge applies one edge.
func (o *orchestrator) applyColdGateEdge(ctx context.Context, edge coldGateEdge) {
	switch edge.kind {
	case edgeRaised:
		if o.coldGateRaised(ctx, edge.ws) {
			o.compactQueuedColdGate(ctx, edge.ws, false)
		}
	case edgeStoodAfterFailedAnswer:
		o.coldMu.Lock()
		o.coldFailed[edge.ws] = true
		o.coldMu.Unlock()
		if o.coldGateRaised(ctx, edge.ws) {
			o.leaveFailedColdGateStanding(ctx, edge.ws)
		}
	case edgeRetired:
		o.coldMu.Lock()
		delete(o.coldFailed, edge.ws)
		o.coldMu.Unlock()
		o.coldGateRetired(ctx, edge.ws)
	case edgeBacklogged:
		o.backloggedAtColdGate(ctx, edge.ws)
	default:
		panic(fmt.Sprintf("merge: cold gate edge kind %d names no edge", int(edge.kind)))
	}
}

// coldGateRaised keeps WS's merge at its place in line, skipped. A merge only
// requested is in nobody's line, a merge remediating out of line waits on its
// repairing turn and is taken back at the head when it returns, and a merge
// admitted with no run here is driven by another daemon: each is left as it
// stands. A merge whose run is taken (running, suspended for its take-back,
// or taken back to resume) is put back in line at its own place
// (CauseColdGate). It answers whether the merge stands in line at the gate,
// which is the merge whose gate the queue answers (compactQueuedColdGate).
func (o *orchestrator) coldGateRaised(ctx context.Context, ws ids.WorkspaceID) bool {
	log := o.log(ctx, ws)
	repo, ok := o.coldGateQueue(ctx, ws, "raised")
	if !ok {
		return false
	}
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo)}
	marked, taken, waiting := false, false, false
	var state wsm.MergeQueueState
	err := o.lineWrite(repo, func() error {
		entry, err := o.entryOf(ctx, repo, ws)
		if err != nil {
			return err
		}
		state = entry.State
		o.mu.Lock()
		r, running := o.runsByWorkspace[ws]
		_, away := o.takenAway[ws]
		_, pending := o.resumes[ws]
		// A WAITING MERGE'S BACKGROUND REBASE needs no session: its entry is
		// still waiting, and is marked where it stands.
		background := running && r.background
		o.mu.Unlock()
		// A MERGE BACK IN LINE FROM ITS REPAIR waits like any merge: its
		// pending resume is no run, and it is marked where it stands.
		if (running && !background) || away || (pending && entry.State != wsm.MergeQueued) {
			taken = true
			waiting = true
			return nil
		}
		if entry.State != wsm.MergeQueued {
			return nil
		}
		waiting = true
		marked, err = o.deps.DB.SetMergeColdGated(ctx, repo, ws)
		return err
	})
	if errors.Is(err, wsm.ErrNotFound) {
		log.Debug(opColdGate, "the workspace's merge left its queue before its cold gate was applied", fields)
		return false
	}
	if err != nil {
		log.Error(opColdGate, "could not keep the merge of a workspace at its cold gate at its place in line; the queue may admit it", withField(fields, "error", err.Error()))
		return false
	}
	if taken {
		if err := o.dequeue(ctx, ws, CauseColdGate); err != nil {
			log.Error(opColdGate, "could not put the merge of a workspace whose session parked at its cold gate back at its place in line", withField(fields, "error", err.Error()))
			return false
		}
		return waiting
	}
	if !marked {
		log.Debug(opColdGate, "the merge is left as it stands at its workspace's cold gate", withField(fields, "state", state.String()))
		return waiting
	}
	log.Info(opColdGate, "the session parked at its cold gate; its merge keeps its place in line and is skipped until the gate is passed", fields)
	if err := o.republishQueue(ctx, repo); err != nil {
		log.Error(opColdGate, "could not republish the queue of a merge kept at its cold gate", withField(fields, "error", err.Error()))
	}
	o.kick(repo)
	return waiting
}

// compactQueuedColdGate answers the cold gate standing on WS, whose merge
// waits in line skipped at it (answerQueuedColdGate). The answer runs off
// the edge worker, so the edges behind it are not held for as long as it
// takes. FRESH is a requeue's own answer, which a failed earlier answer does
// not spend.
func (o *orchestrator) compactQueuedColdGate(ctx context.Context, ws ids.WorkspaceID, fresh bool) {
	if !o.async {
		o.answerQueuedColdGate(ctx, ws, fresh)
		return
	}
	o.live.Add(1)
	inflight.Standing(func() {
		defer o.live.Done()
		o.answerQueuedColdGate(ctx, ws, fresh)
	})
}

// leaveFailedColdGateStanding records that the cold gate standing again on
// WS, whose merge waits in line skipped at it, is left for the user: an
// answer to it failed, and the queue never answers a gate again after one
// did. The failed answer's own cause is recorded where its re-open failed
// (daemon.workspace.answer_cold_gate).
func (o *orchestrator) leaveFailedColdGateStanding(ctx context.Context, ws ids.WorkspaceID) {
	fields := dlog.Context{"workspace": string(ws)}
	if repo, err := o.queueOf(ctx, ws); err == nil {
		fields["repo"] = string(repo)
	}
	// A stand that arrives while the queue's answer to this gate still runs
	// is that answer's own failed re-open: the answer records it once.
	o.coldMu.Lock()
	_, answering := o.coldAnswering[ws]
	o.coldMu.Unlock()
	if answering {
		o.log(ctx, ws).Debug(opColdGate, "the cold gate stood again while its answer runs; the answer records its failure", fields)
		return
	}
	o.log(ctx, ws).Error(opColdGate, "an answer to the cold gate of a queued merge's workspace failed and the gate stands again; the queue does not answer it again, and the merge stays skipped until the user answers it", fields)
}

// answerQueuedColdGate is the queue's ONE answer to the cold gate standing
// on WS, whose merge waits in line skipped at it: the compaction pass-gate
// makes (Deps.CompactColdGate), waited on until its re-open settles. The
// session it brings up retires the gate, which resumes the merge
// (ColdGateRetired). FRESH is a requeue's answer (Requeue), which owns one
// answer even though an earlier one failed.
//
// THE ANSWER IS SPENT WITH THE MERGE'S PLACE IN LINE (owner ruling,
// 2026-11-21: a requeued merge's gate answer once retried a failed bring-up
// every ten seconds for most of an hour). A failed answer raises the gate
// again, and a gate raised on a merge in line is answered, so a failure that
// left the merge in line would be answered again forever. So:
//
//   - WS IS CLAIMED for as long as its answer runs, compaction and re-open
//     included (coldAnswering): a gate raised meanwhile, the failed
//     re-open's own included, is not answered, and a merge turn that meets
//     the gate waits on this same answer (passColdGate).
//   - A FAILURE IS FINAL. It is recorded once at ERROR (settleColdAnswer),
//     the gate is kept as failed (coldFailed) so no raise after it answers
//     it again, and the merge leaves its queue as a failed merge
//     (CauseColdGateAnswerFailed), its bubble and its requester told, BEFORE
//     the claim is let go -- unless a merge turn waits on the answer, whose
//     own failure path ends the merge. The gate stands for the user, and
//     only a requeue puts the merge back in line, with one answer of its own.
func (o *orchestrator) answerQueuedColdGate(ctx context.Context, ws ids.WorkspaceID, fresh bool) {
	log := o.log(ctx, ws)
	fields := o.coldGateFields(ctx, ws)
	o.coldMu.Lock()
	if _, answering := o.coldAnswering[ws]; answering {
		o.coldMu.Unlock()
		log.Debug(opColdGate, "the cold gate of a queued merge's workspace is being answered already; the gate raised again is not answered twice", fields)
		return
	}
	if fresh {
		delete(o.coldFailed, ws)
	} else if o.coldFailed[ws] {
		o.coldMu.Unlock()
		log.Debug(opColdGate, "the cold gate of a queued merge's workspace was answered once already and the answer failed; the gate raised again is not answered twice", fields)
		return
	}
	a := &coldAnswer{done: make(chan struct{})}
	o.coldAnswering[ws] = a
	o.coldMu.Unlock()
	o.settleColdAnswer(ctx, ws, a, fields, "the queue")
}

// coldAnswer is the one answer running to a workspace's cold gate, which
// every merge turn meeting the gate waits on (passColdGate).
type coldAnswer struct {
	// done is closed once the answer settled; up and err are set before.
	done chan struct{}
	// waiters counts the merge turns still waiting on the answer, guarded
	// by coldMu: a failure one of them waits on is ended by that turn's own
	// failure path, never by the queue.
	waiters int
	// settled is whether the answer settled, guarded by coldMu.
	settled bool
	// up is whether the answer brought the session up.
	up bool
	// err is the answer's failure.
	err error
}

// coldGateFields are the fields of every record of WS's gate's answer: the
// workspace, its queue and its merge's ledger, where known.
func (o *orchestrator) coldGateFields(ctx context.Context, ws ids.WorkspaceID) dlog.Context {
	fields := dlog.Context{"workspace": string(ws)}
	if repo, err := o.queueOf(ctx, ws); err == nil {
		fields["repo"] = string(repo)
	}
	o.mu.Lock()
	fields["ledger"] = string(o.ledgerOf[ws])
	o.mu.Unlock()
	return fields
}

// settleColdAnswer runs A, the claimed answer to WS's cold gate that ASKER
// (the queue, or a merge turn's origin) made, to its settling: the
// compaction pass-gate makes (Deps.CompactColdGate), never cut by the wait
// of whoever asked, since a compaction still running is no failure. It
// records how the answer ended, lets the claim go and tells every waiting
// merge turn. A FAILURE no merge turn waits on takes the merge off its queue
// as failed (CauseColdGateAnswerFailed); one a merge turn waits on is ended
// by that turn's failure path (passColdGate).
func (o *orchestrator) settleColdAnswer(ctx context.Context, ws ids.WorkspaceID, a *coldAnswer, fields dlog.Context, asker string) {
	log := o.log(ctx, ws)
	fields = withField(fields, "asker", asker)
	// THE ATTEMPT ENDS WHEN ITS RE-OPEN SETTLES (Deps.CompactColdGate), never
	// at a deadline of the queue's own: a compaction still running is no
	// failure, and the claim stands for as long as it runs.
	up, err := o.deps.CompactColdGate(context.WithoutCancel(ctx), ws)
	o.coldMu.Lock()
	a.up, a.err, a.settled = up, err, true
	waited := a.waiters > 0
	if o.coldAnswering[ws] == a {
		delete(o.coldAnswering, ws)
	}
	if err != nil {
		o.coldFailed[ws] = true
	}
	o.coldMu.Unlock()
	close(a.done)
	switch {
	case err == nil && up:
		log.Info(opColdGate, "the cold gate of a queued merge's workspace was answered by a compaction; the session is up, and the merge resumes at its place", fields)
	case err == nil:
		log.Info(opColdGate, "the cold gate of a queued merge's workspace was not answered: no gate stood, or the answer's re-open was abandoned; the merge stays at its place", fields)
	case waited:
		log.Error(opColdGate, "the cold gate of a merge's workspace could not be answered by a compaction; the merge turn waiting on the answer fails the merge, and the gate stands for the user", withField(fields, "error", err.Error()))
	default:
		log.Error(opColdGate, "the cold gate of a queued merge's workspace could not be answered by a compaction; the merge leaves its queue as failed, and the gate stands for the user", withField(fields, "error", err.Error()))
		if err := o.dequeue(context.WithoutCancel(ctx), ws, CauseColdGateAnswerFailed); err != nil {
			if _, refused := Refused(err); refused {
				log.Info(opColdGate, "the merge whose cold gate could not be answered left its queue before the answer failed", withField(fields, "cause", err.Error()))
				return
			}
			log.Error(opColdGate, "could not take the merge whose cold gate could not be answered off its queue; it stays skipped at its place", withField(fields, "error", err.Error()))
		}
	}
}

// passColdGate passes the cold gate standing on WS before the merge turn
// TURN of ORIGIN is handed to its prompt queue (handOff, bringTurnOn).
//
// A MERGE NEVER FAILS ON ITS OWN WORKSPACE'S COLD GATE (2026-11-22 11:32:58:
// an async merge's pick brought its merge workspace up overnight-cold, the
// bring-up parked at the gate, and the pick's submission was refused by the
// gate's name while the queue's own answer to the raise was still starting,
// so the merge failed and the answer that then succeeded passed a gate no
// merge was left to use). So every merge turn the orchestrator submits reads
// the gate once its session was asked up: a gate standing is answered by the
// compaction the queue's answer makes, and the turn waits for it. THE GATE
// HAS ONE ANSWER (coldAnswering): a turn that finds the queue's answer, or
// another turn's, running waits on that one; a gate whose answer failed and
// still stands (coldFailed) is not answered again, and the turn fails, which
// ends the merge with the gate standing for the user, as the queue's own
// failed answer does. A failed answer fails the turn the same way.
//
// A DEPLOY CHECK'S TURN IS PASSED OVER: its gate is answered at its raise by
// the deploy checks' own answer (AnswerDeployCheckColdGate), and a second
// answerer would break the gate's one answer.
func (o *orchestrator) passColdGate(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, origin conversationv1.PromptOrigin) error {
	if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION || !o.deps.AtColdGate(ws) {
		return nil
	}
	log := o.log(ctx, ws)
	fields := withField(withField(o.coldGateFields(ctx, ws), "turn", string(turn)), "origin", origin.String())
	o.coldMu.Lock()
	a, joined := o.coldAnswering[ws]
	if !joined && o.coldFailed[ws] {
		o.coldMu.Unlock()
		err := fmt.Errorf("merge: the cold gate standing on %s was answered once and the answer failed; it is not answered again, and stands for the user", ws)
		log.Error(opColdGate, "a merge turn met its workspace's cold gate standing after its one answer failed; the gate is not answered again, and the merge fails with the gate standing for the user", withField(fields, "error", err.Error()))
		return err
	}
	if !joined {
		a = &coldAnswer{done: make(chan struct{})}
		o.coldAnswering[ws] = a
	}
	a.waiters++
	o.coldMu.Unlock()
	if joined {
		log.Info(opColdGate, "a merge turn met its workspace's cold gate while the gate's answer runs; the turn waits for that answer before it is submitted", fields)
	} else {
		log.Info(opColdGate, "a merge turn met its workspace's cold gate; the gate is answered by a compaction before the turn is submitted", fields)
		answer := func() {
			o.settleColdAnswer(context.WithoutCancel(ctx), ws, a, o.coldGateFields(ctx, ws), origin.String())
		}
		if o.async {
			o.live.Add(1)
			inflight.Standing(func() {
				defer o.live.Done()
				answer()
			})
		} else {
			answer()
		}
	}
	select {
	case <-a.done:
	case <-ctx.Done():
		o.coldMu.Lock()
		if !a.settled {
			a.waiters--
			o.coldMu.Unlock()
			log.Info(opColdGate, "the merge turn's wait for its workspace's cold gate answer was withdrawn; the answer runs on", fields)
			return fmt.Errorf("merge: the wait for the answer to the cold gate on %s ended: %w", ws, context.Cause(ctx))
		}
		o.coldMu.Unlock()
	}
	if a.err != nil {
		return fmt.Errorf("merge: the cold gate standing on %s could not be answered by a compaction: %w", ws, a.err)
	}
	if !a.up && o.deps.AtColdGate(ws) {
		err := fmt.Errorf("merge: the answer to the cold gate on %s brought no session up and the gate still stands", ws)
		log.Error(opColdGate, "the answer to a merge's workspace cold gate settled with no session up and the gate still standing; the merge's turn is not submitted, and the merge fails", withField(fields, "error", err.Error()))
		return err
	}
	log.Info(opColdGate, "the merge's workspace passed its cold gate; the merge's turn is submitted", withField(fields, "session_up", a.up))
	return nil
}

// coldGateRetired resumes WS's merge where it stands in line once its gate was
// passed: the mark is retired, and a merge the merges worked on overtook goes
// right behind them.
func (o *orchestrator) coldGateRetired(ctx context.Context, ws ids.WorkspaceID) {
	repo, ok := o.coldGateQueue(ctx, ws, "retired")
	if !ok {
		return
	}
	o.unmarkColdGated(ctx, repo, ws, "its cold gate was passed")
}

// unmarkColdGated retires WS's cold-gate mark in REPO's queue, WHY naming the
// reason, and republishes and kicks the queue when it changed.
func (o *orchestrator) unmarkColdGated(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, why string) {
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "why": why}
	cleared := false
	err := o.lineWrite(repo, func() error {
		var err error
		cleared, err = o.deps.DB.ClearMergeColdGated(ctx, repo, ws, o.workedOn(repo))
		return err
	})
	if errors.Is(err, wsm.ErrNotFound) {
		log.Debug(opColdGate, "the workspace's merge left its queue before its cold gate's retirement was applied", fields)
		return
	}
	if err != nil {
		log.Error(opColdGate, "could not resume the merge of a workspace whose cold gate was passed; it stays skipped", withField(fields, "error", err.Error()))
		return
	}
	if !cleared {
		log.Debug(opColdGate, "the workspace's merge was not kept at a cold gate", fields)
		return
	}
	log.With(fields).Info(opColdGate, "the merge resumes at its place in line: {why}", dlog.Context{"why": why})
	if err := o.republishQueue(ctx, repo); err != nil {
		log.Error(opColdGate, "could not republish the queue of a merge resumed past its cold gate", withField(fields, "error", err.Error()))
	}
	o.kick(repo)
}

// coldGateQueue answers the queue WS's merge stands on, false when it has
// none (recorded at DEBUG, the ordinary answer for most workspaces) or when
// it cannot be read (recorded at ERROR). EDGE names the gate's edge.
func (o *orchestrator) coldGateQueue(ctx context.Context, ws ids.WorkspaceID, edge string) (wsm.RepoKey, bool) {
	repo, err := o.queueOf(ctx, ws)
	if err == nil {
		return repo, true
	}
	fields := dlog.Context{"workspace": string(ws), "edge": edge}
	if _, refused := Refused(err); refused {
		o.log(ctx, ws).Debug(opColdGate, "the workspace has no merge on any queue; its gate's edge changes nothing there", fields)
		return "", false
	}
	o.log(ctx, ws).Error(opColdGate, "could not read whether the workspace whose cold gate changed has a queued merge", withField(fields, "error", err.Error()))
	return "", false
}

// OnBacklogged is told WS was backlogged. It is applied in order with WS's cold
// gate's edges (backloggedAtColdGate).
func (o *orchestrator) OnBacklogged(ws ids.WorkspaceID) {
	o.queueColdGateEdge(coldGateEdge{ws: ws, kind: edgeBacklogged})
}

// backloggedAtColdGate takes WS's merge off its queue when WS was backlogged while
// its merge stood at its cold gate (CauseBackloggedAtColdGate): hiding the
// workspace is passing its gate without an answer. A workspace with no merge,
// or whose merge is not at a cold gate, is left as it is.
func (o *orchestrator) backloggedAtColdGate(ctx context.Context, ws ids.WorkspaceID) {
	repo, ok := o.coldGateQueue(ctx, ws, "backlogged")
	if !ok {
		return
	}
	entry, err := o.entryOf(ctx, repo, ws)
	if errors.Is(err, wsm.ErrNotFound) {
		return
	}
	if err != nil {
		o.log(ctx, ws).Error(opColdGate, "could not read the queue entry of the backlogged workspace", dlog.Context{"workspace": string(ws), "repo": string(repo), "error": err.Error()})
		return
	}
	if !entry.ColdGated {
		return
	}
	if err := o.dequeue(ctx, ws, CauseBackloggedAtColdGate); err != nil {
		o.log(ctx, ws).Error(opColdGate, "could not take the merge of a workspace backlogged at its cold gate off its queue", dlog.Context{"workspace": string(ws), "repo": string(repo), "error": err.Error()})
	}
}

// recoverColdGated reads, at the take-back of ENTRY's waiting merge (a boot or
// an adoption), whether its workspace's cold gate still stands. THE GATE'S
// RECORD IS ITS DURABLE TRUTH across daemons (wsm.ColdGateRecord): a mark
// whose gate has no record was passed where no edge could reach this queue
// (the daemon that saw the session come up was exiting), so the merge resumes
// at its place. A gate that still stands keeps the mark.
func (o *orchestrator) recoverColdGated(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) {
	ws := entry.Workspace
	_, standing, err := o.deps.DB.ColdGate(ctx, ws)
	if err != nil {
		o.log(ctx, ws).Error(opColdGate, "could not read whether the cold gate of a merge kept at it still stands; it stays skipped",
			dlog.Context{"workspace": string(ws), "repo": string(repo), "error": err.Error()})
		return
	}
	if standing {
		o.log(ctx, ws).Debug(opColdGate, "the taken-back merge's cold gate still stands; it stays skipped at its place", dlog.Context{
			"workspace": string(ws), "repo": string(repo)})
		return
	}
	o.unmarkColdGated(ctx, repo, ws, "its cold gate has no record any more")
}

package merge

import (
	"context"
	"errors"
	"fmt"

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
//     session it brings up retires the gate. A compaction that fails is an
//     ERROR and leaves the merge marked, skipped, with the gate standing.
//   - PASSING THE GATE BY AN ANSWER RESUMES IT THERE. Pay, clear and compact
//     bring a session up, which retires the gate's record and with it the
//     mark (wsm.ClearMergeColdGated); a merge the merges worked on meanwhile
//     overtook goes right behind them, ahead of every merge still waiting.
//     Its workspace's one-shot standing is never touched.
//   - PASSING THE GATE BY HIDING THE WORKSPACE TAKES IT OFF THE QUEUE
//     (OnHidden, CauseHiddenAtColdGate); killing it does through the close
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
	// edgeHidden is the workspace hidden, which passes a standing gate
	// without an answer.
	edgeHidden
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
			o.compactQueuedColdGate(ctx, edge.ws)
		}
	case edgeRetired:
		o.coldGateRetired(ctx, edge.ws)
	case edgeHidden:
		o.hiddenAtColdGate(ctx, edge.ws)
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
// waits in line skipped at it, with the compaction pass-gate makes
// (Deps.CompactColdGate). The session the compaction brings up retires the
// gate, which resumes the merge (ColdGateRetired). A compaction that fails is
// recorded at ERROR and leaves the merge marked and skipped, the gate
// standing for the user. The compaction runs off the edge worker, so the
// edges behind it are not held for as long as it takes.
func (o *orchestrator) compactQueuedColdGate(ctx context.Context, ws ids.WorkspaceID) {
	compact := func() {
		log := o.log(ctx, ws)
		fields := dlog.Context{"workspace": string(ws)}
		if repo, err := o.queueOf(ctx, ws); err == nil {
			fields["repo"] = string(repo)
		}
		if err := o.deps.CompactColdGate(ctx, ws); err != nil {
			log.Error(opColdGate, "the cold gate of a queued merge's workspace could not be answered by a compaction; the merge stays skipped until the gate is answered", withField(fields, "error", err.Error()))
			return
		}
		log.Info(opColdGate, "the cold gate of a queued merge's workspace was answered by a compaction; the merge resumes at its place once the session is up", fields)
	}
	if !o.async {
		compact()
		return
	}
	o.live.Add(1)
	inflight.Standing(func() {
		defer o.live.Done()
		compact()
	})
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
	log.Info(opColdGate, "the merge resumes at its place in line: "+why, fields)
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

// OnHidden is told WS was hidden. It is applied in order with WS's cold
// gate's edges (hiddenAtColdGate).
func (o *orchestrator) OnHidden(ws ids.WorkspaceID) {
	o.queueColdGateEdge(coldGateEdge{ws: ws, kind: edgeHidden})
}

// hiddenAtColdGate takes WS's merge off its queue when WS was hidden while
// its merge stood at its cold gate (CauseHiddenAtColdGate): hiding the
// workspace is passing its gate without an answer. A workspace with no merge,
// or whose merge is not at a cold gate, is left as it is.
func (o *orchestrator) hiddenAtColdGate(ctx context.Context, ws ids.WorkspaceID) {
	repo, ok := o.coldGateQueue(ctx, ws, "hidden")
	if !ok {
		return
	}
	entry, err := o.entryOf(ctx, repo, ws)
	if errors.Is(err, wsm.ErrNotFound) {
		return
	}
	if err != nil {
		o.log(ctx, ws).Error(opColdGate, "could not read the queue entry of the hidden workspace", dlog.Context{"workspace": string(ws), "repo": string(repo), "error": err.Error()})
		return
	}
	if !entry.ColdGated {
		return
	}
	if err := o.dequeue(ctx, ws, CauseHiddenAtColdGate); err != nil {
		o.log(ctx, ws).Error(opColdGate, "could not take the merge of a workspace hidden at its cold gate off its queue", dlog.Context{"workspace": string(ws), "repo": string(repo), "error": err.Error()})
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

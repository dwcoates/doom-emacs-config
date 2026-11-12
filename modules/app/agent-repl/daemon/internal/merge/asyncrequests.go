package merge

import (
	"context"
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// This file is the hand-off of a requester's ASYNC REQUESTS: each recorded on
// its own (wsm.AsyncMergeRequest), any number per turn, and each handed to its
// merge workspace once the turn that asked for it has ended.
//
// THE ORDER IS STRUCTURAL. One hand-off runs per requester at a time
// (handOffLock), and it moves the requester's ready requests oldest first, so
// the requests of one turn join the line in the order they were asked for,
// and a request placed behind an earlier one of the same turn finds it in
// line when it is placed. The own merge a requester asked for in the same
// turn is put in line after them (waitThenQueue), so it may be placed behind
// one of them as well.
//
// A REQUEST IS READY when its requesting turn is not the requester's turn in
// flight: every request a turn asks for is recorded before that turn ends, so
// once it has ended the hand-off finds all of them. A request recorded with no
// turn (none was in flight, or a layout-127 file moved it) waits for whatever
// turn is in flight, as a restart's re-armed request always did. A turn the
// wait has seen end (AwaitTurnEnd answered) is ended, whatever the turn in
// flight still reads, so no wait ever awaits a turn twice.

// asyncWait is one requester's standing wait for its async requests' turns to
// end, then their hand-off.
type asyncWait struct {
	ws     ids.WorkspaceID
	ctx    context.Context
	cancel context.CancelFunc
	// more is set when a request is recorded while the wait stands, so the
	// wait reads the requests again before it retires (o.mu guards it).
	more bool
}

// awaitAsyncRequests makes sure a wait stands for WS's async requests. A
// request recorded while one stands is picked up by it: the wait reads the
// requests again before it retires, under the lock this marks it under.
func (o *orchestrator) awaitAsyncRequests(ws ids.WorkspaceID) *asyncWait {
	o.mu.Lock()
	if standing, ok := o.asyncWaits[ws]; ok {
		standing.more = true
		o.mu.Unlock()
		return standing
	}
	ctx, cancel := context.WithCancel(context.Background())
	wait := &asyncWait{ws: ws, ctx: ctx, cancel: cancel}
	o.asyncWaits[ws] = wait
	o.mu.Unlock()
	if o.async {
		o.live.Add(1)
		inflight.Standing(func() {
			defer o.live.Done()
			o.runAsyncWait(wait)
		})
	}
	return wait
}

// runAsyncWait is the wait: for each turn its requests wait on, the turn's
// end, then the hand-off of every request ready by then. It retires once no
// request is left, or when it is withdrawn.
func (o *orchestrator) runAsyncWait(wait *asyncWait) {
	const op = "daemon.merge.async_request"
	ctx, ws := wait.ctx, wait.ws
	log := o.log(ctx, ws)
	defer wait.cancel()
	ended := map[ids.TurnID]bool{}
	for ctx.Err() == nil {
		if o.isDraining() {
			log.Info(op, "the daemon is draining; the async merge requests stay recorded for the next boot", dlog.Context{"workspace": string(ws)})
			o.retireAsyncWait(wait, true)
			return
		}
		turn, inFlight := o.turnInFlight(ws, ended)
		requests, err := o.deps.DB.AsyncMergeRequestsOf(ctx, ws)
		if err != nil {
			if ctx.Err() == nil {
				log.Error(op, "could not read the async merge requests waiting for their turn; they stay recorded for the next boot",
					dlog.Context{"workspace": string(ws), "error": err.Error()})
			}
			o.retireAsyncWait(wait, true)
			return
		}
		if len(readyRequests(requests, turn, inFlight)) > 0 {
			if err := o.handOffReady(ctx, ws, ended); err != nil {
				log.Error(op, "could not hand the async merge requests whose turn ended to their merge workspaces; they stay recorded for the next boot",
					dlog.Context{"workspace": string(ws), "error": err.Error()})
				o.retireAsyncWait(wait, true)
				return
			}
			continue
		}
		if len(requests) == 0 {
			if o.retireAsyncWait(wait, false) {
				return
			}
			continue
		}
		log.Info(op, "the async merge requests wait for the turn that asked for them to end; nothing about them is reported before then",
			dlog.Context{"workspace": string(ws), "turn": string(turn), "requests": len(requests)})
		if _, err := o.deps.AwaitTurnEnd(ctx, ws, turn); err != nil {
			if ctx.Err() != nil {
				log.Info(op, "the async merge requests' wait was withdrawn before the requesting turn ended",
					dlog.Context{"workspace": string(ws), "turn": string(turn), "cause": err.Error()})
				return
			}
			log.Error(op, "could not wait for the requesting turn to end; the async merge requests stay recorded for the next boot",
				dlog.Context{"workspace": string(ws), "turn": string(turn), "error": err.Error()})
			o.retireAsyncWait(wait, true)
			return
		}
		ended[turn] = true
	}
}

// turnInFlight answers WS's turn in flight, as none when ENDED holds it: a
// turn the caller has seen end.
func (o *orchestrator) turnInFlight(ws ids.WorkspaceID, ended map[ids.TurnID]bool) (ids.TurnID, bool) {
	turn, inFlight := o.deps.TurnInFlight(ws)
	if !inFlight || ended[turn] {
		return "", false
	}
	return turn, true
}

// retireAsyncWait retires WAIT and answers true, unless a request was
// recorded since the wait last read them (and FORCE is false): then it answers
// false, and the wait reads them again.
func (o *orchestrator) retireAsyncWait(wait *asyncWait, force bool) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	if wait.more && !force {
		wait.more = false
		return false
	}
	if o.asyncWaits[wait.ws] == wait {
		delete(o.asyncWaits, wait.ws)
	}
	return true
}

// withdrawAsync ends WS's standing async wait, if one stands: its requests stay
// recorded, for the next daemon that serves the workspace, or go with the
// workspace when it is removed (they cascade with it).
func (o *orchestrator) withdrawAsync(ws ids.WorkspaceID) bool {
	o.mu.Lock()
	wait, standing := o.asyncWaits[ws]
	if standing {
		delete(o.asyncWaits, ws)
	}
	o.mu.Unlock()
	if standing {
		wait.cancel()
	}
	return standing
}

// readyRequests answers the leading run of REQUESTS whose requesting turn has
// ended, given the requester's turn in flight: the requests the hand-off moves
// now, in the order they were asked for.
func readyRequests(requests []wsm.AsyncMergeRequest, turn ids.TurnID, inFlight bool) []wsm.AsyncMergeRequest {
	for i, req := range requests {
		if inFlight && (req.Turn == "" || req.Turn == turn) {
			return requests[:i]
		}
	}
	return requests
}

// handOffLock answers the lock WS's hand-offs are serialized by.
func (o *orchestrator) handOffLock(ws ids.WorkspaceID) *sync.Mutex {
	o.mu.Lock()
	defer o.mu.Unlock()
	lock, ok := o.handOffLocks[ws]
	if !ok {
		lock = &sync.Mutex{}
		o.handOffLocks[ws] = lock
	}
	return lock
}

// handOffReady hands each of WS's ready requests to its merge workspace,
// oldest first, one hand-off of WS at a time, counting every turn in ENDED as
// ended. A request the hand-off drops
// (its merge workspace could not be forked) does not stop the ones after it.
// A daemon draining hands off nothing: the requests stay recorded for the next
// boot.
func (o *orchestrator) handOffReady(ctx context.Context, ws ids.WorkspaceID, ended map[ids.TurnID]bool) error {
	const op = "daemon.merge.async_request"
	lock := o.handOffLock(ws)
	lock.Lock()
	defer lock.Unlock()
	if o.isDraining() {
		o.log(ctx, ws).Info(op, "the daemon is draining; the async merge requests stay recorded for the next boot",
			dlog.Context{"workspace": string(ws)})
		return nil
	}
	turn, inFlight := o.turnInFlight(ws, ended)
	requests, err := o.deps.DB.AsyncMergeRequestsOf(ctx, ws)
	if err != nil {
		return err
	}
	for _, req := range readyRequests(requests, turn, inFlight) {
		if err := o.handOffDetached(ctx, ws, req.Repo, req.Source); err != nil {
			return err
		}
	}
	return nil
}

// rearmAsyncRequests re-arms the hand-off of every requester with an async
// request standing in REQUESTS that THIS daemon serves (all of them when
// SERVES is nil): the boot's recovery and a take-back's alike.
func (o *orchestrator) rearmAsyncRequests(requests []wsm.AsyncMergeRequest, serves func(ids.WorkspaceID) bool) {
	const op = "daemon.merge.recover"
	seen := map[ids.WorkspaceID]bool{}
	for _, req := range requests {
		if seen[req.Requester] || (serves != nil && !serves(req.Requester)) {
			continue
		}
		seen[req.Requester] = true
		o.deps.Log.Global().Info(op, "re-armed the async merge requests still waiting for their turn to end", dlog.Context{
			"workspace": string(req.Requester), "repo": string(req.Repo)})
		o.awaitAsyncRequests(req.Requester)
	}
}

// dropAsyncRequestsOf drops every async request WS has not handed off, as its
// workspace closes: nothing was reported of them, so nothing is drawn, and each
// merge placed behind one is settled by the close. A failure is recorded and
// the rest are still dropped.
func (o *orchestrator) dropAsyncRequestsOf(ctx context.Context, ws ids.WorkspaceID) {
	const op = "daemon.merge.workspace_closed"
	o.withdrawAsync(ws)
	log := o.log(ctx, ws)
	requests, err := o.deps.DB.AsyncMergeRequestsOf(ctx, ws)
	if err != nil {
		log.Error(op, "could not read a closed workspace's async merge requests to drop them", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return
	}
	for _, req := range requests {
		fields := dlog.Context{"workspace": string(ws), "repo": string(req.Repo), "request": string(req.Source.Detach), "name": req.Source.DetachName}
		if err := o.deps.DB.DropAsyncMergeRequest(ctx, req.Repo, req.Source.Detach, string(CauseWorkspaceClosed)); err != nil {
			log.Error(op, "could not drop a closed workspace's async merge request", withField(fields, "error", err.Error()))
			continue
		}
		log.Info(op, "dropped an async merge request its closed workspace never handed off", fields)
	}
}

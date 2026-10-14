package promptqueue

import (
	"context"
	"fmt"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

const opExclusive = "daemon.promptqueue.exclusive"

// Exclusively runs PERFORM while it OWNS THE WORKSPACE'S TURN SEQUENCE: as the
// workspace's one standing shim call (call.go), so no prompt is delivered to
// it — a new turn, an interjection, a held prompt's release — until PERFORM
// returns. A submission made meanwhile is held, and is delivered once the call
// settles, exactly as during a rollback.
//
// WHAT names the call for the record. PERFORM's error is returned as it is.
//
// A BOUNCE IS AHEAD OF IT, as it is ahead of every act: PERFORM never runs
// while a bounce drains the workspace, because the shim that bounce is standing
// down would answer it with a stand-down. It runs once the bounce has finished,
// on whatever then serves the workspace (awaitUndrained).
//
// A FORK IS ITS CALLER: the fork point is read from the parent's shim and the
// vendor's fork made from the parent's transcript while the parent can start no
// turn, so the fork carries exactly the turns that were finished when it was
// taken (docs/protobuf-design/fork-lineage.md).
func (q *queue) Exclusively(ctx context.Context, ws ids.WorkspaceID, what string, perform func(context.Context) error) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"call": what})

	d, err := q.awaitUndrained(ctx, ws, log)
	if err != nil {
		return err
	}
	defer d.unlock()
	log.Info(opExclusive, "holding the workspace's turn sequence for the call", nil)

	d.outside(shimCall{what: what}, log, func() { err = perform(ctx) })
	if err != nil {
		log.Info(opExclusive, "the work done while the workspace's turn sequence was held failed; deliveries resume", dlog.Context{"cause": err.Error()})
		return err
	}
	log.Debug(opExclusive, "the work done while the workspace's turn sequence was held finished; deliveries resume", nil)
	return nil
}

// awaitUndrained answers the workspace's delivery lock, held, once no bounce
// drains the workspace. A bounce that is draining is waited out on its own
// latch with the lock released, so the bounce can finish; the state is judged
// again after, since a bounce's end may start the next one. A workspace a
// move took off this daemon is refused with bounce.ErrMovedAway: what would
// run on it is its new owner's to run.
func (q *queue) awaitUndrained(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) (*delivery, error) {
	for {
		d := q.lockDelivery(ws)
		q.mu.Lock()
		pending := d.state.bounce
		switch {
		case pending == nil || !pending.draining:
			q.mu.Unlock()
			return d, nil
		case pending.kept:
			q.mu.Unlock()
			d.unlock()
			log.Info(opExclusive, "the workspace was moved off this daemon; the call is refused so it is asked of the daemon it moved to", nil)
			return nil, fmt.Errorf("promptqueue: %s: %w", ws, bounce.ErrMovedAway)
		}
		ended := pending.drainEnded
		reason := pending.reason()
		q.mu.Unlock()
		d.unlock()
		log.Info(opExclusive, "a bounce is draining the workspace; the call waits for it to finish so it runs on what serves the workspace after", dlog.Context{"reason": reason})
		if q.onDrainWait != nil {
			q.onDrainWait()
		}
		select {
		case <-ended:
		case <-ctx.Done():
			return nil, fmt.Errorf("promptqueue: waiting for the bounce on %s to finish: %w", ws, context.Cause(ctx))
		}
	}
}

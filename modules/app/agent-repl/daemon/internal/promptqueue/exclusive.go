package promptqueue

import (
	"context"

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

	d := q.lockDelivery(ws)
	defer d.unlock()

	d.outside(shimCall{what: what}, log, func() { err = perform(ctx) })
	if err != nil {
		log.Info(opExclusive, "the work done while the workspace's turn sequence was held failed; deliveries resume", dlog.Context{"cause": err.Error()})
		return err
	}
	log.Debug(opExclusive, "the work done while the workspace's turn sequence was held finished; deliveries resume", nil)
	return nil
}

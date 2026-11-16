package merge

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A MERGE IS HELD exactly while a resume record waits for it: from the
// take-back that registers the record (recoverEntry, after a restart, a
// handover or a pause cut its run) until the resume that runs it
// (resumeInto). holdRound is the ONE caller of the store's round hold
// (wsm.HoldMergeRound), so the ledger's live round says it is held for
// exactly that span, and every view of the merge draws the hold from there.

// opHold is the operation a merge round's hold is recorded under.
const opHold = "daemon.merge.hold"

// holdRound marks the live round of WS's merge, under ledger LEASE, held from
// now when HELD, or running again when not. A hold that cannot be written is
// an ERROR: the merge is still held or resumed, and only its drawing is
// wrong until the next hold write.
//
// THE HOLD IS REDRAWN AT ONCE: the roster's republish of REPO's queue draws
// its stages and its bubble from it (publishRosterQueue).
func (o *orchestrator) holdRound(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, lease ids.LeaseID, held bool) {
	fields := dlog.Context{"workspace": string(ws), "lease": string(lease), "held": held}
	var at *time.Time
	if held {
		now := o.deps.Now()
		at = &now
	}
	marked, err := o.deps.DB.HoldMergeRound(context.WithoutCancel(ctx), lease, at)
	if err != nil {
		o.log(ctx, ws).Error(opHold, "could not record whether the merge's live round is held; its views draw the last hold recorded", withField(fields, "error", err.Error()))
		return
	}
	o.log(ctx, ws).Debug(opHold, "recorded whether the merge's live round is held", withField(fields, "live_round", marked))
	o.publishRosterQueue(context.WithoutCancel(ctx), repo)
}

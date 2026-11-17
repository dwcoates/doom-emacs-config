package merge

import (
	"context"
	"slices"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// repairOrigins are the origins of the turns the queue gives a merge's own
// agent to repair it: each repair's (AsideReason.origin), a fixing attempt
// and a conflict resolution.
var repairOrigins = func() []string {
	var out []string
	for _, why := range asideReasons {
		out = append(out, why.origin().String())
	}
	return out
}()

// GiveUp implements Orchestrator: WS's own agent gives its merge up from a
// repair turn the queue gave it, because it cannot finish the repair, and the
// merge leaves the queue at once, as an eviction takes it (dequeue,
// CauseAgentGaveUp): a running merge is abandoned and the rebase it left
// standing aborted, and one repairing out of line is dropped. No test run
// starts on its account. IT IS THE ONE WAY AN AGENT ENDS ITS OWN MERGE (owner
// ruling, 2026-11-17): a repair left broken on purpose spent a whole test run
// to fail the merge. A workspace running no repair turn is refused
// (ArmNoRepairTurn): giving up is the repair's answer, never a dequeue.
func (o *orchestrator) GiveUp(ctx context.Context, ws ids.WorkspaceID, reason string) error {
	const op = "daemon.merge.give_up"
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "reason": reason}
	open, err := o.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		log.Error(op, "could not read whether the workspace runs a repair turn; its merge is not given up", withField(fields, "error", err.Error()))
		return err
	}
	if !slices.ContainsFunc(open, func(t wsm.Turn) bool { return slices.Contains(repairOrigins, t.Origin) }) {
		log.Info(op, "refused a give-up from a workspace running no repair turn of its merge", withField(fields, "open_turns", len(open)))
		return refuse(ArmNoRepairTurn, ws, "no conflict resolution or fixing attempt of this workspace's merge is running")
	}
	log.Info(op, "the merge's own agent gave it up from its repair turn; it leaves the queue", fields)
	return o.dequeue(ctx, ws, CauseAgentGaveUp)
}

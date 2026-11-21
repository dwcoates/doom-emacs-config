package cachelapse

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A KEEP-ALIVE RUN'S SPEND IS ITS KEEP-ALIVES' OWN, READ AT EACH ANSWER.
//
// The shim counts every keep-alive's call under the keep-alive cause and
// states it on the session stream before the keep-alive's answer
// (shim.v1.ShimMeasurements vendor_spend), so by the time the keeper settles
// an answer the metrics surface already holds that keep-alive's spend
// (Deps.KeepaliveSpent). The keeper adds what the workspace's keep-alive
// spend grew by since the last answer it settled to the run on the row
// (wsm.AddKeepaliveRunSpend), where it outlives the many daemon restarts a
// run spans, and records the run when it ends (wsm.EndKeepaliveRun):
// resumed, capped or closed.

// markRunSpend takes WS's keep-alive spend as the mark the next answer's
// growth is read from, when no mark stands: the first keep-alive this
// daemon sends it.
func (k *Keeper) markRunSpend(ws ids.WorkspaceID) {
	if _, ok := k.runSpendMark[ws]; ok {
		return
	}
	k.runSpendMark[ws] = k.deps.KeepaliveSpent(ws)
}

// addRunSpend adds what WS's keep-alive spend grew by since its mark to its
// standing run, and moves the mark. A failed write is recorded at ERROR: the
// run's spend then misses that keep-alive, which the spend series still
// count.
func (k *Keeper) addRunSpend(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	mark, ok := k.runSpendMark[ws]
	if !ok {
		return
	}
	now := k.deps.KeepaliveSpent(ws)
	k.runSpendMark[ws] = now
	grew := wsm.KeepaliveRunTokens{
		Uncached:  growth(mark.GetUncachedInput(), now.GetUncachedInput()),
		CacheRead: growth(mark.GetCacheRead(), now.GetCacheRead()),
		Write5m:   growth(mark.GetCacheWrite_5M(), now.GetCacheWrite_5M()),
		Write1h:   growth(mark.GetCacheWrite_1H(), now.GetCacheWrite_1H()),
		Output:    growth(mark.GetOutput(), now.GetOutput()),
	}
	if grew == (wsm.KeepaliveRunTokens{}) {
		return
	}
	took, err := k.deps.DB.AddKeepaliveRunSpend(ctx, ws, grew)
	switch {
	case err != nil && !leaving(ctx, log, opDrive, "adding the keep-alive's spend to its run", err):
		log.Error(opDrive, "could not add the keep-alive's spend to its run; the run's spend misses it", dlog.Context{"spent": grew, "cause": err.Error()})
	case err == nil && !took:
		log.Debug(opDrive, "a keep-alive's spend was answered after a turn ended its run; it belongs to no run", dlog.Context{"spent": grew})
	}
}

// growth is how much a monotonic figure grew from WAS to NOW. The keep-alive
// spend only grows within one daemon's life, so a figure below its mark is a
// broken invariant.
func growth(was, now uint64) uint64 {
	if now < was {
		panic("cachelapse: a workspace's keep-alive spend went down within one daemon's life")
	}
	return now - was
}

// endRun records WS's keep-alive run's end asked at ENDING, when it has
// ended, into the telemetry. A failed read is recorded at ERROR, and the run
// is asked again at its next moment.
func (k *Keeper) endRun(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, ending wsm.KeepaliveRunEnding) {
	run, ended, err := k.deps.DB.EndKeepaliveRun(ctx, ws, ending)
	if err != nil {
		if !leaving(ctx, log, opDrive, "recording the end of the keep-alive run", err) {
			log.Error(opDrive, "could not record the end of the workspace's keep-alive run; it is asked again at its next moment", dlog.Context{
				"ending": int(ending), "cause": err.Error(),
			})
		}
		return
	}
	if !ended {
		return
	}
	log.Info(opDrive, "the workspace's keep-alive run ended", dlog.Context{
		"end": string(run.End), "run_beats": run.Beats, "spent": run.Spent,
	})
	k.deps.Telemetry.ObserveKeepaliveRun(ws, run)
}

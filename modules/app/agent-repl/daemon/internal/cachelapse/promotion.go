package cachelapse

import (
	"context"
	"maps"
	"slices"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A JOINING SUCCESSOR WRITES NOTHING, AND WRITES WHAT IT WITHHELD AT ITS
// PROMOTION.
//
// A deploy's successor opens the state handle read-only while the incumbent is
// still the sole writer, and promotes it at its first adoption
// (wsm.DB.Promote). This keeper's boot pass runs before that, so a decision it
// takes then cannot go on the row: a keep-alive run's end (endRun) and whether
// a workspace owes keep-alives (recordOwing). Each is WITHHELD here instead,
// and the loop writes every one the moment the handle's promotion latch
// closes (wsm.DB.Promoted, Run). Before this, a run capped at boot was refused
// at ERROR and left unrecorded until some unrelated later moment, which then
// recorded it as resumed (2026-11-21 deploy: four workspaces at
// keepalive_run_beats=19, keepalive_run_ended=0).
//
// Every withheld set is owned by Run's goroutine alone: endRun and
// recordOwing are reached only from the loop (its boot pass, its edges and
// its deadlines), and the promotion is taken on the same loop.

// withheldWhileJoining answers whether this keeper's state handle is a joining
// successor's, not yet promoted, and when it is records at DEBUG that WHAT is
// written once it writes, and calls HOLD to remember it for the promotion. It
// is THE ONE read-only check of this package's writes, so endRun and
// recordOwing cannot drift apart.
func (k *Keeper) withheldWhileJoining(log dlog.Logger, what string, fields dlog.Context, hold func()) bool {
	if !k.deps.DB.ReadOnly() {
		return false
	}
	log.Debug(opDrive, "the state handle is not promoted yet; "+what+" is recorded once it writes", fields)
	hold()
	return true
}

// withholdEnd remembers WS's keep-alive run's end asked at ENDING for the
// promotion, keeping the ending that decides more when one is already
// withheld.
func (k *Keeper) withholdEnd(ws ids.WorkspaceID, ending wsm.KeepaliveRunEnding) {
	if held, ok := k.withheldEnds[ws]; ok && decisiveness(held) >= decisiveness(ending) {
		return
	}
	k.withheldEnds[ws] = ending
}

// decisiveness ranks the moments a run's end is asked at by how many rows
// each one decides, read off wsm.EndKeepaliveRun's switch: a run a turn moved
// past ends resumed whatever the moment; a run still standing is ended by
// RunEndingCap always, by RunEndingClosure only on a closed row, and by
// RunEndingBeat and RunEndingTurnClosed never. So the cap decides every row
// the closure decides and more, and the closure every row the other two
// decide and more: the cap outranks the closure, which outranks the rest. On
// a standing closed row the cap records run_cap where the closure would
// record closed, and run_cap is the truer end: the run had already spent its
// break-even, which a close after it does not undo.
func decisiveness(ending wsm.KeepaliveRunEnding) int {
	switch ending {
	case wsm.RunEndingCap:
		return 2
	case wsm.RunEndingClosure:
		return 1
	default:
		return 0
	}
}

// withholdOwing remembers WS for a reconcile at the promotion, which takes
// its owing decision afresh and writes it then.
func (k *Keeper) withholdOwing(ws ids.WorkspaceID) {
	k.withheldOwing[ws] = struct{}{}
}

// promoted writes everything a joining successor withheld, once its state
// handle's promotion latch has closed: every withheld run end is asked again
// at its remembered ending, and every workspace whose owing decision was
// withheld is reconciled. A store that wrote from the start withheld
// nothing, which is recorded at DEBUG alone.
//
// A LATCH CLOSED ON A HANDLE THAT STILL READS AS READ-ONLY is a broken
// invariant of the state store (wsm.DB.Promoted): it is recorded at ERROR,
// and what was withheld stays withheld rather than being withheld again
// with nothing left to ask for it.
func (k *Keeper) promoted(ctx context.Context) {
	log := k.deps.Log.Global()
	if len(k.withheldEnds) == 0 && len(k.withheldOwing) == 0 {
		log.Debug(opDrive, "the state handle writes; this keeper withheld nothing while it joined", nil)
		return
	}
	fields := dlog.Context{"run_ends": len(k.withheldEnds), "owing_decisions": len(k.withheldOwing)}
	if k.deps.DB.ReadOnly() {
		log.Error(opDrive, "the state handle's promotion latch closed while the handle still reads as read-only; what this keeper withheld is not written", fields)
		return
	}
	ends, owing := k.withheldEnds, k.withheldOwing
	k.withheldEnds, k.withheldOwing = map[ids.WorkspaceID]wsm.KeepaliveRunEnding{}, map[ids.WorkspaceID]struct{}{}
	for _, ws := range slices.Sorted(maps.Keys(ends)) {
		k.endRun(ctx, log.With(dlog.Context{"workspace": string(ws)}), ws, ends[ws])
	}
	for _, ws := range slices.Sorted(maps.Keys(owing)) {
		k.reconcile(ctx, ws, "the state handle was promoted")
	}
	// AFTER THE WRITES, so the record says they were asked; each one records
	// its own outcome above.
	log.Info(opDrive, "the state handle was promoted; asked again for the keep-alive records withheld while this daemon joined", fields)
}

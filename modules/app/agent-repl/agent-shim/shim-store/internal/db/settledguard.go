package db

import (
	"google.golang.org/protobuf/proto"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// settledguard.go — a concluded unit is never reopened by a late write of it.
//
// A UNIT MOVES ONE WAY: it starts, may report progress, and concludes. The two
// planes write the same unit under one upsert key, each when it can, so their
// copies arrive in whatever order their writers flush. On 2026-10-16 the
// sidecar stored a skill's loaded document, and the shim's copy of the skill's
// START, queued behind a slow batch, landed 128ms later and superseded the
// row whole: the loaded card went back to loading and never loaded again
// (TestSkillInvocation). A write that does not conclude the unit, arriving at
// a row that already holds its conclusion, is therefore older news, and the
// stored conclusion stands.
//
// THE LATE WRITE STILL CARRIES WHAT IT ALONE KNOWS: the turn it was written
// under and the place it states, which carryStoredStamps has already folded
// into it (the stored stamp wins, and a row stored unstamped takes the write's).
// Only its content is replaced by the stored one, so the row is written once
// more with the conclusion and the stamps, and a watcher sees the stamp land.

// keepSettledUnit rewrites R in place to the stored concluded frame of its
// unit when R is a page-line activity that does not conclude it and PRIOR, the
// row R supersedes, does. It answers whether it did. R's turn and place, as
// carryStoredStamps left them, are kept.
func (d *DB) keepSettledUnit(fields logging.Fields, r *routed, prior *storedRow) (bool, error) {
	if prior == nil || r.kind != kindPageLine || prior.kind != kindPageLine {
		return false, nil
	}
	incoming := r.pageLine.GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
	if incoming == nil || activityIsTerminal(incoming) {
		return false, nil
	}
	stored := &storev1.StoreEntry{}
	if err := proto.Unmarshal(prior.frame, stored); err != nil {
		return false, storagef(err, "the stored frame of row %q cannot be decoded to read whether its unit concluded", r.upsertKey)
	}
	line := stored.GetAgentUpdate().GetServeableFrame()
	settled := line.GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
	if settled == nil || !activityIsTerminal(settled) {
		return false, nil
	}
	stored.Turn = r.entry.GetTurn()
	stored.Place = r.entry.GetPlace()
	frame, err := proto.Marshal(stored)
	if err != nil {
		return false, storagef(err, "re-serializing row %q with its stored conclusion", r.upsertKey)
	}
	r.entry = stored
	r.pageLine = line
	r.frame = frame
	r.plane = prior.plane
	d.log.LogVerbose(fields, "a write that does not conclude its unit reached a row holding the conclusion; the conclusion stands and only the write's stamps land entries_index=%d", r.index)
	return true, nil
}

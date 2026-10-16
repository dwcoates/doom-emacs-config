package db

import (
	"google.golang.org/protobuf/proto"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// settledguard.go — a concluded unit is never left reopened by a late write.
//
// A UNIT MOVES ONE WAY: it starts, may report progress, concludes, and may
// carry a consequence after its conclusion (an edit's IDE diagnostics). The two
// planes write the same unit under one upsert key, each when it can, so their
// copies arrive in whatever order their writers flush. On 2026-10-16 the
// sidecar stored a skill's loaded document, and the shim's copy of the skill's
// START, queued behind a slow batch, landed 128ms later and superseded the row
// whole: the loaded card went back to loading and never loaded again
// (TestSkillInvocation).
//
// THE LATE WRITE STILL LANDS, AND THE CONCLUSION IS SAID AGAIN AFTER IT. The
// late write carries facts the conclusion may not (a push's message lives only
// on its start, and the footer raises its line from it), so it is delivered to
// every reader as written. The store then re-lands the stored conclusion, with
// the late write's turn and place (as carryStoredStamps merged them), so every
// reader sees the unit's lifecycle in order and the row ends concluded.

// concludedAfter answers the stored conclusion to re-land after R, when R is a
// page-line activity that neither concludes its unit nor follows its
// conclusion and PRIOR, the row R supersedes, holds the unit concluded (or a
// consequence after it). It answers nil otherwise.
func (d *DB) concludedAfter(fields logging.Fields, r routed, prior *storedRow) (*routed, error) {
	if prior == nil || r.kind != kindPageLine || prior.kind != kindPageLine {
		return nil, nil
	}
	incoming := r.pageLine.GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
	// A CONSEQUENCE FOLLOWS THE TERMINAL BY DESIGN (an edit's IDE
	// diagnostics), so it is the unit's next word, never a reopening.
	if incoming == nil || activityIsTerminal(incoming) || activityIsConsequence(incoming) {
		return nil, nil
	}
	stored := &storev1.StoreEntry{}
	if err := proto.Unmarshal(prior.frame, stored); err != nil {
		return nil, storagef(err, "the stored frame of row %q cannot be decoded to read whether its unit concluded", r.upsertKey)
	}
	line := stored.GetAgentUpdate().GetServeableFrame()
	settled := line.GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
	if settled == nil || !(activityIsTerminal(settled) || activityIsConsequence(settled)) {
		return nil, nil
	}
	stored.Turn = r.entry.GetTurn()
	stored.Place = r.entry.GetPlace()
	frame, err := proto.Marshal(stored)
	if err != nil {
		return nil, storagef(err, "re-serializing row %q with its stored conclusion", r.upsertKey)
	}
	again := r
	again.entry = stored
	again.pageLine = line
	again.frame = frame
	again.plane = prior.plane
	again.writeID = stored.GetWriteId()
	d.log.LogVerbose(fields, "a write that does not conclude its unit reached a row holding the conclusion; the write lands and the conclusion is re-landed after it entries_index=%d", r.index)
	return &again, nil
}

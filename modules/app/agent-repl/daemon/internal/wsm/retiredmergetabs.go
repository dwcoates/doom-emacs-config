package wsm

import (
	"context"
	"database/sql"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
)

// LAYOUT 79 DROPS THE STORED MERGE TABS OF RETIRED KINDS. A merge bubble
// recorded before its tabs nested (2026-10-12) stored `conflicts` (5) and
// `fixes` (7) tabs, whose content today's bubble draws inside a commit's
// subtab and a test run's; the retirement rewrote none of them, so each is a
// tab with no kind that nothing can draw, and every read of its bubble
// recorded it at ERROR. The owner ruled them dropped (2026-10-25): they leave
// the old bubbles rather than being redrawn as a kind they never were.

// The retired merge tab kinds this step drops.
const (
	retiredMergeTabConflicts protowire.Number = 5
	retiredMergeTabFixes     protowire.Number = 7
)

// dropRetiredMergeTabRows is layout 79's row edit.
func dropRetiredMergeTabRows(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	return dropDurableRows(ctx, tx, log, "dropped the stored merge tabs of retired kinds", isRetiredMergeTabRow)
}

// isRetiredMergeTabRow reports whether RAW is a merge tab row whose kind is
// one of the retired ones: no kind of today's, and a retired kind's field.
func isRetiredMergeTabRow(raw []byte) (bool, error) {
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil {
		return false, err
	}
	tab := row.GetMergeTab()
	if tab == nil || tab.GetKind() != nil {
		return false, nil
	}
	retired := false
	err := eachUnknown(tab.ProtoReflect(), func(num protowire.Number, _ protowire.Type, _ int, _ []byte) {
		if num == retiredMergeTabConflicts || num == retiredMergeTabFixes {
			retired = true
		}
	})
	return retired, err
}

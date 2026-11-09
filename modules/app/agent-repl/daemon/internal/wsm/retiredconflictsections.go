package wsm

import (
	"context"
	"database/sql"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
)

// LAYOUT 112 DROPS THE STORED COMMIT CONFLICT SECTIONS. A conflict
// resolution was drawn in its commit's subtab of the rebasing tab, as the
// commit tab's `conflict` (7) section, until agents drew only in their own
// tabs (2026-11-09): today a resolution is its own subtab of the
// resolving-conflicts tab, and FeedMergeTabRebaseCommit reserves the field.
// Every stored commit tab still carrying it would be refused by each read of
// its bubble, so the field is dropped. As with layout 79's retired kinds, a
// bubble recorded before the change draws its commits' narration and no
// longer draws their resolutions: they leave the old bubbles rather than
// being redrawn as subtabs they never were.

// The retired commit tab field this step drops.
const retiredRebaseCommitConflictTag protowire.Number = 7 // FeedMergeTabRebaseCommit.conflict

// dropRetiredConflictSectionRows is layout 112's row rewrite.
func dropRetiredConflictSectionRows(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	return rewriteDurableRows(ctx, tx, log, "dropped the stored commit tabs' retired conflict sections", dropRetiredConflictSection)
}

// dropRetiredConflictSection re-encodes one stored frontend.v1.FeedRow whose
// rebase commit tab carries the retired conflict section without it, and
// reports whether it did.
func dropRetiredConflictSection(raw []byte) ([]byte, bool, error) {
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil {
		return nil, false, err
	}
	commit := row.GetMergeTab().GetRebaseCommit()
	if commit == nil {
		return raw, false, nil
	}
	dropped, err := dropUnknown(commit.ProtoReflect(), retiredRebaseCommitConflictTag)
	if err != nil || !dropped {
		return raw, false, err
	}
	out, err := proto.Marshal(row)
	if err != nil {
		return nil, false, err
	}
	return out, true, nil
}

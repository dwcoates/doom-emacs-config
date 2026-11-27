package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agent-repld/internal/dlog"
)

// THE MODEL A MERGE'S WORKSPACE RETURNS TO. A workspace whose own merge enters
// the merge queue is compacted and switched to Sonnet first
// (conversation.v1.PromptOrigin.PROMPT_ORIGIN_MERGE_ENQUEUE_COMPACTION), and a
// merge that ends without landing hands the workspace back on the model it
// ran before. That model is this column, so a daemon that restarts between
// the switch and the handback restores the same model its predecessor
// recorded.

// mergeRestoreModelDDL is the layout-224 addition: merge_restore_model, the
// model a workspace ran on before its merge switched it, NULL when no merge
// switched it. IT IS ADDITIVE: the previous build names no such column. The
// fresh-file schema appends this same text (schema.go).
const mergeRestoreModelDDL = `
ALTER TABLE workspaces ADD COLUMN merge_restore_model TEXT;
`

// HoldMergeRestoreModel records MODEL as the model workspace ID returns to
// when its merge ends without landing. A model already held is kept: a
// request the daemon re-runs after a restart reads the session's model
// AFTER the switch, and the model to restore is the one from before it. An
// empty MODEL is a caller defect, refused; a workspace no longer registered
// is refused.
func (s *store) HoldMergeRestoreModel(ctx context.Context, id WorkspaceID, model string) error {
	if model == "" {
		return fmt.Errorf("wsm: hold the restore model of workspace %s: no model was named", id)
	}
	return s.write(ctx, "daemon.wsm.hold_merge_restore_model", dlog.Context{"workspace": string(id), "model": model}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET merge_restore_model = COALESCE(merge_restore_model, ?) WHERE id = ?`, model, id)
		if err != nil {
			return fmt.Errorf("wsm: hold the restore model of workspace %s: %w", id, err)
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}

// TakeMergeRestoreModel answers the model workspace ID returns to and clears
// it in the same write, false when no merge switched it. A workspace no
// longer registered is refused.
func (s *store) TakeMergeRestoreModel(ctx context.Context, id WorkspaceID) (string, bool, error) {
	var model sql.NullString
	err := s.write(ctx, "daemon.wsm.take_merge_restore_model", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		if err := scanWorkspaceRow(ctx, tx, id, "merge_restore_model", &model); err != nil {
			return err
		}
		if !model.Valid {
			return nil
		}
		_, err := tx.ExecContext(ctx, `UPDATE workspaces SET merge_restore_model = NULL WHERE id = ?`, id)
		return err
	})
	if err != nil {
		return "", false, err
	}
	return model.String, model.Valid, nil
}

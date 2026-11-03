package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// lifecycleHooksDDL is the layout-102 addition, everything a repository's
// lifecycle hooks (repoconfig.v1.LifecycleConfig) need stored:
//
//   - workspace_originators: who created each workspace
//     (agentrepl.v1.WorkspaceOriginator), recorded once at creation, which
//     decides which kind's hooks its stages run;
//   - workspace_links.announces: the stored link an owned link announces
//     (agentrepl.v1.WorkspaceLinkOwned.announces), NULL for one announcing
//     nothing;
//   - merge_finish_progress: how far a finishing workspace's finish stage has
//     run, so a restart resumes at the recorded hook and reattaches to its
//     prompt's turn rather than sending it again.
//
// IT IS ADDITIVE: two new tables and a nullable column the previous build
// never names.
const lifecycleHooksDDL = `
CREATE TABLE workspace_originators (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  originator   TEXT NOT NULL
);
ALTER TABLE workspace_links ADD COLUMN announces TEXT;
CREATE TABLE merge_finish_progress (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  hook_index   INTEGER NOT NULL,
  turn         TEXT NOT NULL
);
`

// SetWorkspaceOriginator records ORIGINATOR (its config spelling,
// lifecycleconfig.Originator.String) as who created workspace ID. A workspace
// is created once, so a second record for it is a caller's defect and is
// refused.
func (s *store) SetWorkspaceOriginator(ctx context.Context, id WorkspaceID, originator string) error {
	const op = "daemon.wsm.set_workspace_originator"
	fields := dlog.Context{"workspace": string(id), "originator": originator}
	if originator == "" {
		err := fmt.Errorf("wsm: originator of %s: an originator is named", id)
		s.log.Error(op, "refused an originator its caller should never have made", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `INSERT INTO workspace_originators (workspace_id, originator) VALUES (?, ?)`, id, originator)
		return err
	})
}

// WorkspaceOriginator answers who created workspace ID, false when nothing
// recorded it: a workspace registered rather than created (a main worktree,
// one Emacs announced) or created before originators were recorded.
func (s *store) WorkspaceOriginator(ctx context.Context, id WorkspaceID) (string, bool, error) {
	var originator string
	found := false
	err := s.read(ctx, "daemon.wsm.workspace_originator", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT originator FROM workspace_originators WHERE workspace_id = ?`, id).Scan(&originator)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		found = err == nil
		return err
	})
	return originator, found, err
}

// UnknownAnnouncedLinkError is a link set announcing a url the workspace does
// not hold. It is the store answering what was asked, never a failure.
type UnknownAnnouncedLinkError struct {
	// URL is the announced url as the caller named it.
	URL string
}

func (e *UnknownAnnouncedLinkError) Error() string {
	return fmt.Sprintf("wsm: the workspace holds no link %q to announce", e.URL)
}

// FinishProgress is how far a workspace's finish stage has run.
type FinishProgress struct {
	// HookIndex is the hook the stage stands at: every hook before it
	// succeeded.
	HookIndex int
	// Turn is the turn the prompt hook at HookIndex runs as, empty when that
	// hook is a script or its prompt was not yet submitted.
	Turn TurnID
}

// PutFinishProgress records that workspace ID's finish stage stands at
// PROGRESS, replacing what was recorded.
func (s *store) PutFinishProgress(ctx context.Context, id WorkspaceID, progress FinishProgress) error {
	const op = "daemon.wsm.put_finish_progress"
	fields := dlog.Context{"workspace": string(id), "hook_index": progress.HookIndex, "turn": string(progress.Turn)}
	if progress.HookIndex < 0 {
		err := fmt.Errorf("wsm: finish progress of %s at hook %d", id, progress.HookIndex)
		s.log.Error(op, "refused finish progress its caller should never have made", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO merge_finish_progress (workspace_id, hook_index, turn) VALUES (?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET hook_index = excluded.hook_index, turn = excluded.turn`,
			id, progress.HookIndex, string(progress.Turn))
		return err
	})
}

// WorkspaceFinishProgress answers where workspace ID's finish stage stands,
// false when no finish stage is under way.
func (s *store) WorkspaceFinishProgress(ctx context.Context, id WorkspaceID) (FinishProgress, bool, error) {
	var progress FinishProgress
	var turn string
	found := false
	err := s.read(ctx, "daemon.wsm.workspace_finish_progress", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT hook_index, turn FROM merge_finish_progress WHERE workspace_id = ?`, id).Scan(&progress.HookIndex, &turn)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		found = err == nil
		return err
	})
	progress.Turn = TurnID(turn)
	return progress, found, err
}

// ClearFinishProgress forgets workspace ID's finish stage: it finished, or
// its deploy was withdrawn. Clearing one with none is no change.
func (s *store) ClearFinishProgress(ctx context.Context, id WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.clear_finish_progress", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM merge_finish_progress WHERE workspace_id = ?`, id)
		return err
	})
}

package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// slackOriginsDDL is the layout-93 addition: the Slack thread a workspace was
// created to act on (agentrepl.v1.CreateWorkspaceSlackOrigin), which the
// daemon posts into when the work begins and when a finish's deploy ends. A
// workspace has at most one, recorded once at creation. IT IS ADDITIVE: a new
// table the previous build never names.
const slackOriginsDDL = `
CREATE TABLE workspace_slack_origins (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  thread_url   TEXT NOT NULL
);
`

// SetWorkspaceSlackOrigin records URL as the Slack thread workspace ID was
// created for. A workspace is created once, so a second record for it is a
// caller's defect and is refused.
func (s *store) SetWorkspaceSlackOrigin(ctx context.Context, id WorkspaceID, url string) error {
	const op = "daemon.wsm.set_workspace_slack_origin"
	fields := dlog.Context{"workspace": string(id), "thread_url": url}
	if url == "" {
		err := fmt.Errorf("wsm: slack origin of %s: an origin names its thread", id)
		s.log.Error(op, "refused a slack origin its caller should never have made", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `INSERT INTO workspace_slack_origins (workspace_id, thread_url) VALUES (?, ?)`, id, url)
		return err
	})
}

// WorkspaceSlackOrigin answers the Slack thread workspace ID was created for,
// false when it was created for none.
func (s *store) WorkspaceSlackOrigin(ctx context.Context, id WorkspaceID) (string, bool, error) {
	var url string
	found := false
	err := s.read(ctx, "daemon.wsm.workspace_slack_origin", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT thread_url FROM workspace_slack_origins WHERE workspace_id = ?`, id).Scan(&url)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		found = err == nil
		return err
	})
	return url, found, err
}

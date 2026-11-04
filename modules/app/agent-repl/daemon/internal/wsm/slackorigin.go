package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// slackOriginsDDL is the layout-93 addition: the Slack thread a workspace was
// created to act on (agentrepl.v1.CreateWorkspaceSlackOrigin), which a
// repository's hooks read (AGENT_REPL_SLACK_THREAD, `{{slack_thread}}`). A
// workspace has at
// most one, recorded once at creation. IT IS ADDITIVE: a new table the
// previous build never names.
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

// slackPostsDDL is the layout-100 addition: what the daemon has posted into a
// workspace's originating thread, so a restart neither posts anything twice
// nor loses the message a merged pull request's reaction goes on.
//
//   - workspace_slack_origins.opened is the CLAIM on the opening post, the one
//     that tells the thread the task in the workspace's own summary. It is
//     taken once, atomically, by whichever of the two edges (the origin
//     recorded, the first summary installed) finds both standing. The origins
//     recorded before the step already had their opening posted, so the step
//     backfills them as opened.
//   - workspace_slack_link_posts is one row per owned link posted into the
//     thread: inserted as the claim before the post (message_ts NULL), the ts
//     recorded once Slack answers it. merged marks the link's pull request
//     merged; the reaction goes on the message exactly once, from whichever of
//     the two writes (the ts recorded, the merge marked) finds both.
//
// IT IS ADDITIVE: a new column with a default, a backfill of it the previous
// build never reads, and a new table it never names. Layout 103 drops both
// again (slackPostsDropDDL): the daemon no longer posts to Slack.
const slackPostsDDL = `
ALTER TABLE workspace_slack_origins ADD COLUMN opened INTEGER NOT NULL DEFAULT 0;
UPDATE workspace_slack_origins SET opened = 1;
CREATE TABLE workspace_slack_link_posts (
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  url          TEXT NOT NULL,
  message_ts   TEXT,
  merged       INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (workspace_id, url)
);
`

// slackPostsDropDDL is the layout-103 step: what layout 100 added for the
// daemon's own Slack posts leaves the store, because the daemon posts nothing
// to Slack any more (a repository's lifecycle hooks say what a thread is
// told, repoconfig.v1.LifecycleConfig).
//
// The step is BREAKING: the build before it reads and writes both the column
// and the table on every create and owned link.
const slackPostsDropDDL = `
DROP TABLE workspace_slack_link_posts;
ALTER TABLE workspace_slack_origins DROP COLUMN opened;
`

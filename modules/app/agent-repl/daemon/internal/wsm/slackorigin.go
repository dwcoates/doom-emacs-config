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
// daemon posts into (internal/workspace/slackorigin.go). A workspace has at
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
// build never reads, and a new table it never names.
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

// SlackLinkPost is one owned link posted, or claimed to be posted, into a
// workspace's originating thread.
type SlackLinkPost struct {
	// URL is the link, verbatim as it was set.
	URL string
	// MessageTS is the posted message's Slack ts; empty while the post is
	// claimed and unanswered, and for good when it failed.
	MessageTS string
	// Merged marks the link's pull request merged.
	Merged bool
}

// ClaimSlackOpening claims the opening post into the thread workspace ID was
// created for, answering the thread and whether this call took the claim.
// False is a workspace created for no thread, or one whose opening is already
// claimed: the claim is taken once, ever.
func (s *store) ClaimSlackOpening(ctx context.Context, id WorkspaceID) (string, bool, error) {
	var (
		url     string
		claimed bool
	)
	err := s.write(ctx, "daemon.wsm.claim_slack_opening", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspace_slack_origins SET opened = 1 WHERE workspace_id = ? AND opened = 0`, id)
		if err != nil {
			return err
		}
		if claimed, err = touchedRow(res); err != nil || !claimed {
			return err
		}
		return tx.QueryRowContext(ctx, `SELECT thread_url FROM workspace_slack_origins WHERE workspace_id = ?`, id).Scan(&url)
	})
	return url, claimed, err
}

// ClaimSlackLinkPost claims the post of LINK into the thread workspace ID was
// created for, answering the thread and whether this call took the claim.
// False is a workspace created for no thread, or a link already claimed: a
// link is posted once, however often it is set again.
func (s *store) ClaimSlackLinkPost(ctx context.Context, id WorkspaceID, link string) (string, bool, error) {
	var (
		url     string
		claimed bool
	)
	err := s.write(ctx, "daemon.wsm.claim_slack_link_post", dlog.Context{"workspace": string(id), "url": link}, func(ctx context.Context, tx *sql.Tx) error {
		err := tx.QueryRowContext(ctx, `SELECT thread_url FROM workspace_slack_origins WHERE workspace_id = ?`, id).Scan(&url)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `INSERT INTO workspace_slack_link_posts (workspace_id, url) VALUES (?, ?) ON CONFLICT DO NOTHING`, id, link)
		if err != nil {
			return err
		}
		claimed, err = touchedRow(res)
		return err
	})
	if !claimed {
		url = ""
	}
	return url, claimed, err
}

// RecordSlackLinkPost records TS as the message LINK was posted as, answering
// whether the link's pull request is already marked merged: then the reaction
// is this caller's to add, since the merge found no message to react to. A
// link never claimed, or one whose ts is already recorded, is a caller's
// defect and is refused.
func (s *store) RecordSlackLinkPost(ctx context.Context, id WorkspaceID, link, ts string) (bool, error) {
	const op = "daemon.wsm.record_slack_link_post"
	fields := dlog.Context{"workspace": string(id), "url": link, "message_ts": ts}
	if ts == "" {
		err := fmt.Errorf("wsm: slack post of %s in %s: a post records its message ts", link, id)
		s.log.Error(op, "refused a slack post its caller should never have recorded", withError(fields, err))
		return false, err
	}
	var merged bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspace_slack_link_posts SET message_ts = ? WHERE workspace_id = ? AND url = ? AND message_ts IS NULL`, ts, id, link)
		if err != nil {
			return err
		}
		touched, err := touchedRow(res)
		if err != nil {
			return err
		}
		if !touched {
			return fmt.Errorf("wsm: slack post of %s in %s: no unanswered claim to record a ts on", link, id)
		}
		return tx.QueryRowContext(ctx, `SELECT merged FROM workspace_slack_link_posts WHERE workspace_id = ? AND url = ?`, id, link).Scan(&merged)
	})
	return merged, err
}

// MarkSlackLinkMerged marks LINK's pull request merged, answering the ts of
// the message the reaction goes on when it is this caller's to add: empty
// when the link was already marked, or its post has no ts yet (the post's
// own record then adds the reaction) or never will (its post failed).
func (s *store) MarkSlackLinkMerged(ctx context.Context, id WorkspaceID, link string) (string, error) {
	var ts sql.NullString
	err := s.write(ctx, "daemon.wsm.mark_slack_link_merged", dlog.Context{"workspace": string(id), "url": link}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspace_slack_link_posts SET merged = 1 WHERE workspace_id = ? AND url = ? AND merged = 0`, id, link)
		if err != nil {
			return err
		}
		if touched, err := touchedRow(res); err != nil || !touched {
			return err
		}
		return tx.QueryRowContext(ctx, `SELECT message_ts FROM workspace_slack_link_posts WHERE workspace_id = ? AND url = ?`, id, link).Scan(&ts)
	})
	return ts.String, err
}

// WorkspaceSlackLinkPosts answers every link posted, or claimed to be posted,
// into the thread workspace ID was created for, in the order they were
// claimed.
func (s *store) WorkspaceSlackLinkPosts(ctx context.Context, id WorkspaceID) ([]SlackLinkPost, error) {
	var posts []SlackLinkPost
	err := s.read(ctx, "daemon.wsm.workspace_slack_link_posts", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, `SELECT url, message_ts, merged FROM workspace_slack_link_posts WHERE workspace_id = ? ORDER BY rowid`, id)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				post SlackLinkPost
				ts   sql.NullString
			)
			if err := rows.Scan(&post.URL, &ts, &post.Merged); err != nil {
				return err
			}
			post.MessageTS = ts.String
			posts = append(posts, post)
		}
		return rows.Err()
	})
	return posts, err
}

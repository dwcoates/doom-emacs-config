package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/pullrefs"
)

// This file is the durable STATE OF EVERY LINKED PULL REQUEST: whether each
// GitHub pull request a workspace's link names is a draft, open, merged or
// closed, as the pull-request watcher's latest successful read of it found
// (prwatch). It is one row per pull request, whoever links it, so two
// workspaces linking one pull request draw one state.
//
// IT IS APART FROM THE SUBSCRIPTIONS (pullrequests.go). A subscription is a
// workspace told about a pull request's activity; a reference link is told
// nothing, yet its menu row still says how its pull request stands, and an
// owned link's row still does once its subscription ended or was ended.

// pullRequestStatesDDL is the layout-176 addition: the state of each linked
// pull request. IT IS ADDITIVE: one new table the previous build never names.
// The step's rewrite (backfillPullRequestStates) fills the link columns and
// seeds the states the store already knows.
const pullRequestStatesDDL = `
CREATE TABLE pull_request_states (
  owner   TEXT NOT NULL,
  repo    TEXT NOT NULL,
  number  INTEGER NOT NULL,
  state   TEXT NOT NULL CHECK (state IN ('draft', 'open', 'merged', 'closed')),
  read_at INTEGER NOT NULL,
  PRIMARY KEY (owner, repo, number)
);
`

// PullRequestState is how a pull request stands on GitHub.
type PullRequestState string

// The states. The zero value is a state never read.
const (
	PullRequestStateDraft  PullRequestState = "draft"
	PullRequestStateOpen   PullRequestState = "open"
	PullRequestStateMerged PullRequestState = "merged"
	PullRequestStateClosed PullRequestState = "closed"
)

// Valid reports whether the state is one of the four.
func (s PullRequestState) Valid() bool {
	switch s {
	case PullRequestStateDraft, PullRequestStateOpen, PullRequestStateMerged, PullRequestStateClosed:
		return true
	}
	return false
}

// LinkedPullRequests answers every pull request a link of an open workspace
// names whose state can still change: never read, a draft, open, or closed
// (a closed pull request may be reopened). A MERGED PULL REQUEST IS NEVER
// READ AGAIN, since nothing unmerges it. Ordered by key, so each tick reads
// them in one order.
func (s *store) LinkedPullRequests(ctx context.Context) ([]PullRequestKey, error) {
	var out []PullRequestKey
	err := s.read(ctx, "daemon.wsm.linked_pull_requests", nil, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT DISTINCT l.pr_owner, l.pr_repo, l.pr_number FROM workspace_links AS l
			JOIN workspaces AS w ON w.id = l.workspace_id
			LEFT JOIN pull_request_states AS p ON p.owner = l.pr_owner AND p.repo = l.pr_repo AND p.number = l.pr_number
			WHERE l.pr_number IS NOT NULL AND w.closed = 0 AND (p.state IS NULL OR p.state <> 'merged')
			ORDER BY l.pr_owner, l.pr_repo, l.pr_number`,
			func(row interface{ Scan(...any) error }) (PullRequestKey, error) {
				var k PullRequestKey
				err := row.Scan(&k.Owner, &k.Repo, &k.Number)
				return k, err
			})
		return err
	})
	return out, err
}

// RecordPullRequestState records that a read of PR made AT found it STATE,
// and answers every open workspace with a link naming PR when the state is
// new or changed, so each one's menu is drawn again; none when the state
// stands as it was. An invalid key or state is a caller's defect.
func (s *store) RecordPullRequestState(ctx context.Context, pr PullRequestKey, state PullRequestState, at time.Time) ([]WorkspaceID, error) {
	const op = "daemon.wsm.record_pull_request_state"
	fields := dlog.Context{"pull_request": pr.String(), "state": string(state)}
	if !pr.Valid() || !state.Valid() {
		err := fmt.Errorf("wsm: record the state %q of pull request %s", state, pr)
		s.log.Error(op, "refused a pull request state its caller should never have recorded", withError(fields, err))
		return nil, err
	}
	var changed []WorkspaceID
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var was string
		err := tx.QueryRowContext(ctx, `SELECT state FROM pull_request_states WHERE owner = ? AND repo = ? AND number = ?`,
			pr.Owner, pr.Repo, pr.Number).Scan(&was)
		if err != nil && !errors.Is(err, sql.ErrNoRows) {
			return err
		}
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO pull_request_states (owner, repo, number, state, read_at) VALUES (?, ?, ?, ?, ?)
			 ON CONFLICT(owner, repo, number) DO UPDATE SET state = excluded.state, read_at = excluded.read_at`,
			pr.Owner, pr.Repo, pr.Number, string(state), nanos(at)); err != nil {
			return err
		}
		if was == string(state) {
			return nil
		}
		changed, err = listRows(ctx, tx, `SELECT DISTINCT l.workspace_id FROM workspace_links AS l
			JOIN workspaces AS w ON w.id = l.workspace_id
			WHERE l.pr_owner = ? AND l.pr_repo = ? AND l.pr_number = ? AND w.closed = 0 ORDER BY l.workspace_id`,
			func(row interface{ Scan(...any) error }) (WorkspaceID, error) {
				var id WorkspaceID
				err := row.Scan(&id)
				return id, err
			}, pr.Owner, pr.Repo, pr.Number)
		return err
	})
	if err != nil {
		return nil, err
	}
	return changed, nil
}

// backfillPullRequestStates is the layout-176 rewrite. Every link stored
// before links recorded the pull request their url names has it filled from
// its url (pullrefs.ParseURL), so the watcher reads its state like any other
// link's; and every pull request a subscription saw merge or close is seeded
// with that end, the latest read the store already holds.
func backfillPullRequestStates(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	rows, err := tx.QueryContext(ctx, `SELECT rowid, url FROM workspace_links WHERE pr_number IS NULL`)
	if err != nil {
		return err
	}
	type unkeyed struct {
		rowid int64
		url   string
	}
	var links []unkeyed
	for rows.Next() {
		var l unkeyed
		if err := rows.Scan(&l.rowid, &l.url); err != nil {
			rows.Close()
			return err
		}
		links = append(links, l)
	}
	rows.Close()
	if err := rows.Err(); err != nil {
		return err
	}
	keyed := 0
	for _, l := range links {
		owner, repo, number, ok := pullrefs.ParseURL(l.url)
		if !ok {
			continue
		}
		if _, err := tx.ExecContext(ctx, `UPDATE workspace_links SET pr_owner = ?, pr_repo = ?, pr_number = ? WHERE rowid = ?`,
			owner, repo, number, l.rowid); err != nil {
			return err
		}
		keyed++
	}
	// A pull request two subscriptions saw end is merged when either saw it
	// merge: 'merged' sorts after 'closed', and a merge is final.
	res, err := tx.ExecContext(ctx, `INSERT INTO pull_request_states (owner, repo, number, state, read_at)
		SELECT owner, repo, number, MAX(ended), MAX(COALESCE(last_read, subscribed_at))
		FROM pull_request_subscriptions WHERE ended IS NOT NULL GROUP BY owner, repo, number`)
	if err != nil {
		return err
	}
	seeded, err := res.RowsAffected()
	if err != nil {
		return err
	}
	log.Info("daemon.wsm.migrate", "keyed the links stored before links recorded their pull request, and seeded the ended pull requests' states",
		dlog.Context{"links_keyed": keyed, "states_seeded": seeded})
	return nil
}

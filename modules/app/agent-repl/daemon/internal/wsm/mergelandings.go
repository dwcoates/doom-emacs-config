package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"slices"
	"time"

	"agent-repld/internal/dlog"
)

// This file is EVERY LANDING ONTO A REPOSITORY'S MAIN BRANCH, one row each,
// written at landing time and naming the path the landing took, so "how many
// merged through the queue" and "what landed otherwise" are reads of what the
// daemon recorded, never reconstructions from commit messages
// (agentrepl.v1.ListMergeLandings; owner request, 2026-11-20: master's
// first-parent history had to be counted by hand, and the queue-bypass
// landings were told apart only by their paired changelog commits).
//
// THE ROW OUTLIVES EVERYTHING ELSE ABOUT THE MERGE: the queue row, the
// undeployed landing (undeployedlandings.go) and the merge workspace all go
// when the merge ends, so none of them can answer for a past window.
//
// ONE ROW PER (repository, head). A landing reported twice (the deploy
// mediator's direct landing is recorded by the daemon's finish and reported
// by land-direct.sh it runs) keeps the first row, so a repeated report never
// counts twice.

// mergeLandingsDDL is the layout-183 addition: merge_landings, and
// merge_landings_recording, the one instant recording began, stamped by the
// statement that creates it, so a window reaching back before it is told
// apart from one in which nothing landed. IT IS ADDITIVE: two new tables the
// previous build never names.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const mergeLandingsDDL = `
CREATE TABLE merge_landings (
  repo_key       TEXT NOT NULL,
  head           TEXT NOT NULL,
  path           TEXT NOT NULL,
  branch         TEXT NOT NULL,
  workspace_id   TEXT NOT NULL DEFAULT '',
  workspace_name TEXT NOT NULL DEFAULT '',
  landed_at      INTEGER NOT NULL,
  PRIMARY KEY (repo_key, head)
);
CREATE INDEX merge_landings_by_time ON merge_landings(repo_key, landed_at);

CREATE TABLE merge_landings_recording (
  id       INTEGER PRIMARY KEY CHECK (id = 1),
  began_at INTEGER NOT NULL
);
INSERT INTO merge_landings_recording (id, began_at)
  VALUES (1, CAST((julianday('now') - 2440587.5) * 86400000000000 AS INTEGER));
`

// LandingPath is the way a landing reached the main branch.
type LandingPath string

// The landing paths.
const (
	// LandingThroughQueue is a merge the repository's merge queue ran and
	// landed: a workspace's finish, a one-shot's, an async merge.
	LandingThroughQueue LandingPath = "queue"
	// LandingQueueBypass is land-direct.sh run by the workspace holding the
	// repository's queue bypass mode.
	LandingQueueBypass LandingPath = "queue_bypass"
	// LandingDeployMediator is the deploy mediator's own fix, landed directly
	// by the daemon's remediation finish.
	LandingDeployMediator LandingPath = "deploy_mediator"
	// LandingDirect is land-direct.sh run by a caller holding neither role.
	LandingDirect LandingPath = "direct"
)

// LandingPaths is every landing path, the queue's first.
var LandingPaths = []LandingPath{LandingThroughQueue, LandingQueueBypass, LandingDeployMediator, LandingDirect}

// valid reports whether p is one of LandingPaths.
func (p LandingPath) valid() bool {
	return slices.Contains(LandingPaths, p)
}

// MergeLanding is one landing onto a repository's main branch.
type MergeLanding struct {
	// Repo is the queue key of the repository landed on.
	Repo RepoKey
	// Head is the main branch's commit once the landing was done.
	Head string
	// Path is the way it landed.
	Path LandingPath
	// Branch is the branch that landed.
	Branch string
	// Workspace and WorkspaceName are the workspace whose landing it was,
	// both empty for a direct landing no workspace asked for.
	Workspace     WorkspaceID
	WorkspaceName string
	// LandedAt is when it landed.
	LandedAt time.Time
}

// ErrIncompleteLanding refuses a landing record missing what names it.
var ErrIncompleteLanding = errors.New("wsm: a landing names its repository, head, path, branch and instant")

// RecordMergeLanding records LANDING, answering whether it was new: a second
// record of the same repository and head keeps the first. A record missing
// its repository, head, branch or instant, or naming no known path, is
// refused with ErrIncompleteLanding and writes nothing.
func (s *store) RecordMergeLanding(ctx context.Context, landing MergeLanding) (bool, error) {
	const op = "daemon.wsm.record_merge_landing"
	fields := dlog.Context{
		"repo": string(landing.Repo), "head": landing.Head, "path": string(landing.Path), "branch": landing.Branch,
		"workspace": string(landing.Workspace), "landed_at": landing.LandedAt}
	if landing.Repo == "" || landing.Head == "" || landing.Branch == "" || landing.LandedAt.IsZero() || !landing.Path.valid() {
		s.log.Error(op, "refused an incomplete landing record", withError(fields, ErrIncompleteLanding))
		return false, ErrIncompleteLanding
	}
	repo, err := s.normalizedRepoKey(op, landing.Repo, fields)
	if err != nil {
		return false, err
	}
	added := false
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`INSERT INTO merge_landings (repo_key, head, path, branch, workspace_id, workspace_name, landed_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT(repo_key, head) DO NOTHING`,
			string(repo), landing.Head, string(landing.Path), landing.Branch, string(landing.Workspace), landing.WorkspaceName, nanos(landing.LandedAt))
		if err != nil {
			return err
		}
		added, err = touchedRow(res)
		return err
	})
	return added, err
}

// MergeLandingsIn answers REPO's landings with since <= landed_at <= until,
// oldest first, and the instant recording began.
func (s *store) MergeLandingsIn(ctx context.Context, repo RepoKey, since, until time.Time) ([]MergeLanding, time.Time, error) {
	const op = "daemon.wsm.merge_landings_in"
	fields := dlog.Context{"repo": string(repo), "since": since, "until": until}
	if until.Before(since) {
		err := fmt.Errorf("wsm: a landing window ends (%s) before it starts (%s)", until, since)
		s.log.Error(op, "refused a reversed landing window", withError(fields, err))
		return nil, time.Time{}, err
	}
	repo, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, time.Time{}, err
	}
	var (
		out   []MergeLanding
		began int64
	)
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		if err := s.q(ctx).QueryRowContext(ctx, `SELECT began_at FROM merge_landings_recording WHERE id = 1`).Scan(&began); err != nil {
			return fmt.Errorf("wsm: read when landings began to be recorded: %w", err)
		}
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT head, path, branch, workspace_id, workspace_name, landed_at FROM merge_landings
			 WHERE repo_key = ? AND landed_at >= ? AND landed_at <= ? ORDER BY landed_at, head`,
			string(repo), nanos(since), nanos(until))
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			l := MergeLanding{Repo: repo}
			var path string
			var at int64
			if err := rows.Scan(&l.Head, &path, &l.Branch, &l.Workspace, &l.WorkspaceName, &at); err != nil {
				return err
			}
			l.Path, l.LandedAt = LandingPath(path), fromNanos(at)
			if !l.Path.valid() {
				return &DecodeError{Table: "merge_landings", Row: l.Head, Field: "path", Err: fmt.Errorf("unknown landing path %q", path)}
			}
			out = append(out, l)
		}
		return rows.Err()
	})
	if err != nil {
		return nil, time.Time{}, err
	}
	return out, fromNanos(began), nil
}

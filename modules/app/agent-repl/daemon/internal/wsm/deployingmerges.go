package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is each queue's LANDED MERGE STILL DEPLOYING (owner request,
// 2026-11-24): the merge whose landing's deploy a deploy check is owed of.
// Its run leaves the queue as its deploy starts, but the queue shows it as
// deploying, at its front, until the deploy mediator's verdict ends the check
// (merge.endDeployingMerge): an approval, an escalation, a dispatch, or a
// release without the mediator's word.
//
// THE ROW LIVES FROM THE LANDING'S TEARDOWN TO THE CHECK'S END, in the store,
// because the deploy it waits on replaces the daemon that wrote it: the
// successor draws it and ends it. One per queue: a queue's head lands nothing
// while a check is owed, so a second landing's row replaces the first only
// when that landing's deploy replaces the first's check
// (RecordDeployingMerge answers the row it replaced).

// deployingMergesDDL is the layout-205 addition: one row per queue whose
// landed merge is still deploying.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const deployingMergesDDL = `
CREATE TABLE deploying_merges (
  repo_key        TEXT PRIMARY KEY,
  workspace       TEXT NOT NULL CHECK (workspace <> ''),
  lease           TEXT NOT NULL CHECK (lease <> ''),
  credited        TEXT NOT NULL CHECK (credited <> ''),
  async           INTEGER NOT NULL,
  name            TEXT NOT NULL,
  branch          TEXT NOT NULL,
  target_dir      TEXT NOT NULL CHECK (target_dir <> ''),
  active_since    INTEGER NOT NULL,
  deploying_since INTEGER NOT NULL,
  timed           INTEGER NOT NULL
);
`

// DeployingMerge is a queue's landed merge whose deploy's check is still
// owed.
type DeployingMerge struct {
	// Repo is the queue's key.
	Repo RepoKey
	// Workspace is the workspace the merge ran in: an async merge's merge
	// workspace.
	Workspace WorkspaceID
	// Lease is the merge's ledger, which its bubble and its stages are read
	// under.
	Lease LeaseID
	// Credited is the workspace the merge is credited to: the requester of
	// an async merge, Workspace itself otherwise.
	Credited WorkspaceID
	// Async reports whether the merge is an async merge.
	Async bool
	// Name is Workspace's name.
	Name string
	// Branch is the branch it landed.
	Branch string
	// TargetDir is the worktree it landed in.
	TargetDir string
	// ActiveSince is when its run was admitted, which its head clock counts
	// from.
	ActiveSince time.Time
	// DeployingSince is when its deploying step began, or its teardown when
	// its run never saw that step begin (Timed false).
	DeployingSince time.Time
	// Timed reports whether DeployingSince is the step's own start, so the
	// step's span is measured when the check ends.
	Timed bool
}

// RecordDeployingMerge records M as its queue's landed merge still
// deploying, and answers the row it replaced, nil when none stood.
func (s *store) RecordDeployingMerge(ctx context.Context, m DeployingMerge) (*DeployingMerge, error) {
	const op = "daemon.wsm.record_deploying_merge"
	fields := dlog.Context{"repo": string(m.Repo), "workspace": string(m.Workspace), "lease": string(m.Lease)}
	key, err := s.normalizedRepoKey(op, m.Repo, fields)
	if err != nil {
		return nil, err
	}
	if m.ActiveSince.IsZero() || m.DeployingSince.IsZero() {
		err := fmt.Errorf("wsm: the deploying merge %s of %s names no instant (active since %v, deploying since %v)", m.Workspace, key, m.ActiveSince, m.DeployingSince)
		s.log.Error(op, "refused a deploying merge that names no instant", withError(fields, err))
		return nil, err
	}
	m.Repo = key
	var replaced *DeployingMerge
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		prior, err := scanDeployingMerge(tx.QueryRowContext(ctx, deployingMergeQuery+` WHERE repo_key = ?`, string(key)))
		if err != nil {
			return err
		}
		replaced = prior
		_, err = tx.ExecContext(ctx,
			`INSERT INTO deploying_merges (repo_key, workspace, lease, credited, async, name, branch, target_dir, active_since, deploying_since, timed)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(repo_key) DO UPDATE SET workspace = excluded.workspace, lease = excluded.lease, credited = excluded.credited,
			   async = excluded.async, name = excluded.name, branch = excluded.branch, target_dir = excluded.target_dir,
			   active_since = excluded.active_since, deploying_since = excluded.deploying_since, timed = excluded.timed`,
			string(key), string(m.Workspace), string(m.Lease), string(m.Credited), m.Async, m.Name, m.Branch, m.TargetDir,
			nanos(m.ActiveSince), nanos(m.DeployingSince), m.Timed)
		return err
	})
	if err != nil {
		return nil, err
	}
	return replaced, nil
}

// DeployingMergeOf answers REPO's landed merge still deploying, nil for none.
func (s *store) DeployingMergeOf(ctx context.Context, repo RepoKey) (*DeployingMerge, error) {
	const op = "daemon.wsm.deploying_merge_of"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	var out *DeployingMerge
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		m, err := scanDeployingMerge(s.q(ctx).QueryRowContext(ctx, deployingMergeQuery+` WHERE repo_key = ?`, string(key)))
		out = m
		return err
	})
	return out, err
}

// EndDeployingMerge deletes and answers REPO's landed merge still deploying,
// nil when none stood: the one ender of a row takes it, so its span is
// measured once.
func (s *store) EndDeployingMerge(ctx context.Context, repo RepoKey) (*DeployingMerge, error) {
	const op = "daemon.wsm.end_deploying_merge"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	var out *DeployingMerge
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		m, err := scanDeployingMerge(tx.QueryRowContext(ctx,
			`DELETE FROM deploying_merges WHERE repo_key = ? RETURNING `+deployingMergeColumns, string(key)))
		out = m
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// deployingMergeColumns are a deploying merge's columns, in scan order.
const deployingMergeColumns = `repo_key, workspace, lease, credited, async, name, branch, target_dir, active_since, deploying_since, timed`

// deployingMergeQuery reads deploying merges; a caller appends its WHERE.
const deployingMergeQuery = `SELECT ` + deployingMergeColumns + ` FROM deploying_merges`

// scanDeployingMerge decodes ROW of deployingMergeColumns: nil when there is
// no row. It is the one decoding path.
func scanDeployingMerge(row *sql.Row) (*DeployingMerge, error) {
	var (
		m                       DeployingMerge
		repo, ws, lease, credit string
		active, deploying       int64
	)
	err := row.Scan(&repo, &ws, &lease, &credit, &m.Async, &m.Name, &m.Branch, &m.TargetDir, &active, &deploying, &m.Timed)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	m.Repo, m.Workspace, m.Lease, m.Credited = RepoKey(repo), WorkspaceID(ws), LeaseID(lease), WorkspaceID(credit)
	m.ActiveSince, m.DeployingSince = fromNanos(active), fromNanos(deploying)
	return &m, nil
}

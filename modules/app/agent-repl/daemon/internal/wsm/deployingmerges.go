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
//
// THE ROW HOLDS ITS QUEUE'S HEAD (merge.deployHold) from the instant the
// landing's teardown records it, before the teardown kicks the queue's pump,
// until the check of the deploy that carries it ends. Its STAGE says which
// deploy that is, each move made in a store write ordered with the deploy's
// own:
//
//   - DeployingLanded: recorded; no deploy has committed over it yet.
//   - DeployingCovered: a deploy committed over it (CoverDeployingMerges, at
//     the deploy's commit, before it reads the checkout), so that deploy's
//     build carries the landing.
//   - DeployingChecked: the covering deploy owes its check (OweMediation
//     moves it in the write that owes the check), so that check's end is the
//     row's end (ReleaseMediation deletes it in the write that releases the
//     check; an escalation or a dispatch ends it as the check stays owed).
//
// A check owed by an earlier deploy therefore never ends a landing its deploy
// did not carry, and the head is held at every instant from the teardown to
// the carrying deploy's check's end: there is no instant between two holds.

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

// deployingMergeStageDDL is the layout-206 addition: each deploying merge's
// stage (DeployingStage). A row the previous build wrote was recorded once
// its deploy's check was owed, so it reads as checked.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const deployingMergeStageDDL = `
ALTER TABLE deploying_merges ADD COLUMN stage INTEGER NOT NULL DEFAULT 2 CHECK (stage IN (0, 1, 2));
`

// DeployingStage is how far a deploying merge's deploy has come: which
// deploy carries it, and whether that deploy's check is owed. The stages
// only move forward.
type DeployingStage int

const (
	// DeployingLanded: no deploy has committed over the landing yet.
	DeployingLanded DeployingStage = iota
	// DeployingCovered: a deploy committed over the landing, and has not owed
	// its check.
	DeployingCovered
	// DeployingChecked: the deploy that covers the landing owes its check.
	DeployingChecked
)

// String names the stage in records.
func (s DeployingStage) String() string {
	switch s {
	case DeployingLanded:
		return "landed"
	case DeployingCovered:
		return "covered"
	case DeployingChecked:
		return "checked"
	}
	return fmt.Sprintf("DeployingStage(%d)", int(s))
}

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
	// Stage is how far its deploy has come. RecordDeployingMerge records
	// every row at DeployingLanded, whatever M says.
	Stage DeployingStage
}

// RecordDeployingMerge records M as its queue's landed merge still
// deploying, at DeployingLanded, and answers the row it replaced, nil when
// none stood.
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
	m.Repo, m.Stage = key, DeployingLanded
	var replaced *DeployingMerge
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		prior, err := scanDeployingMerge(tx.QueryRowContext(ctx, deployingMergeQuery+` WHERE repo_key = ?`, string(key)))
		if err != nil {
			return err
		}
		replaced = prior
		_, err = tx.ExecContext(ctx,
			`INSERT INTO deploying_merges (repo_key, workspace, lease, credited, async, name, branch, target_dir, active_since, deploying_since, timed, stage)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(repo_key) DO UPDATE SET workspace = excluded.workspace, lease = excluded.lease, credited = excluded.credited,
			   async = excluded.async, name = excluded.name, branch = excluded.branch, target_dir = excluded.target_dir,
			   active_since = excluded.active_since, deploying_since = excluded.deploying_since, timed = excluded.timed, stage = excluded.stage`,
			string(key), string(m.Workspace), string(m.Lease), string(m.Credited), m.Async, m.Name, m.Branch, m.TargetDir,
			nanos(m.ActiveSince), nanos(m.DeployingSince), m.Timed, int(m.Stage))
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

// EndDeployingMerge deletes and answers REPO's landed merge still deploying
// when its stage is FROM or later, nil when none stood so far: the one ender
// of a row takes it, so its span is measured once. A check's escalation or
// dispatch ends only the merge its check is owed of (DeployingChecked); a
// queue whose repository checks no deploys ends one its deploy covered
// (DeployingCovered).
func (s *store) EndDeployingMerge(ctx context.Context, repo RepoKey, from DeployingStage) (*DeployingMerge, error) {
	const op = "daemon.wsm.end_deploying_merge"
	fields := dlog.Context{"repo": string(repo), "from": from.String()}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	var out *DeployingMerge
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		m, err := endDeployingMergeInTx(ctx, tx, key, from)
		out = m
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// endDeployingMergeInTx deletes and answers KEY's deploying merge at FROM or
// later, nil for none, inside TX.
func endDeployingMergeInTx(ctx context.Context, tx *sql.Tx, key RepoKey, from DeployingStage) (*DeployingMerge, error) {
	return scanDeployingMerge(tx.QueryRowContext(ctx,
		`DELETE FROM deploying_merges WHERE repo_key = ? AND stage >= ? RETURNING `+deployingMergeColumns, string(key), int(from)))
}

// CoverDeployingMerges moves every deploying merge no deploy covered yet to
// DeployingCovered: the deploy committing now carries it. It answers how
// many it moved. The deploy calls it after it committed and before it reads
// the checkout, so a row recorded before the call names a landing on the
// checkout that deploy builds, and one recorded after waits for the deploy
// its own landing asks for.
func (s *store) CoverDeployingMerges(ctx context.Context) (int, error) {
	const op = "daemon.wsm.cover_deploying_merges"
	covered := 0
	err := s.write(ctx, op, nil, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE deploying_merges SET stage = ? WHERE stage = ?`, int(DeployingCovered), int(DeployingLanded))
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		covered = int(n)
		return err
	})
	return covered, err
}

// UncheckedDeployingMerges answers every deploying merge whose deploy owes no
// check yet (DeployingLanded or DeployingCovered), by queue.
func (s *store) UncheckedDeployingMerges(ctx context.Context) ([]DeployingMerge, error) {
	const op = "daemon.wsm.unchecked_deploying_merges"
	var out []DeployingMerge
	err := s.read(ctx, op, nil, func(ctx context.Context) error {
		merges, err := listRows(ctx, s.q(ctx), deployingMergeQuery+` WHERE stage < ? ORDER BY repo_key`, scanDeployingMergeRow, int(DeployingChecked))
		out = merges
		return err
	})
	return out, err
}

// checkDeployingMergeInTx moves KEY's deploying merge a deploy covered to
// DeployingChecked, inside the write that owes that deploy's check.
func checkDeployingMergeInTx(ctx context.Context, tx *sql.Tx, key RepoKey) error {
	_, err := tx.ExecContext(ctx, `UPDATE deploying_merges SET stage = ? WHERE repo_key = ? AND stage = ?`,
		int(DeployingChecked), string(key), int(DeployingCovered))
	return err
}

// deployingMergeColumns are a deploying merge's columns, in scan order.
const deployingMergeColumns = `repo_key, workspace, lease, credited, async, name, branch, target_dir, active_since, deploying_since, timed, stage`

// deployingMergeQuery reads deploying merges; a caller appends its WHERE.
const deployingMergeQuery = `SELECT ` + deployingMergeColumns + ` FROM deploying_merges`

// scanDeployingMerge decodes ROW of deployingMergeColumns: nil when there is
// no row.
func scanDeployingMerge(row *sql.Row) (*DeployingMerge, error) {
	m, err := scanDeployingMergeRow(row)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	return &m, nil
}

// scanDeployingMergeRow decodes one row of deployingMergeColumns. It is the
// one decoding path.
func scanDeployingMergeRow(row interface{ Scan(...any) error }) (DeployingMerge, error) {
	var (
		m                       DeployingMerge
		repo, ws, lease, credit string
		active, deploying       int64
		stage                   int
	)
	if err := row.Scan(&repo, &ws, &lease, &credit, &m.Async, &m.Name, &m.Branch, &m.TargetDir, &active, &deploying, &m.Timed, &stage); err != nil {
		return DeployingMerge{}, err
	}
	m.Repo, m.Workspace, m.Lease, m.Credited = RepoKey(repo), WorkspaceID(ws), LeaseID(lease), WorkspaceID(credit)
	m.ActiveSince, m.DeployingSince, m.Stage = fromNanos(active), fromNanos(deploying), DeployingStage(stage)
	return m, nil
}

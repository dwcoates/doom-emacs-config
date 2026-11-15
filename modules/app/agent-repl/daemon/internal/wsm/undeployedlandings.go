package wsm

import (
	"context"
	"database/sql"
	"sort"
	"time"

	"agent-repld/internal/dlog"
)

// This file is every landing in the daemon's own checkout waiting for the
// deploy that carries it, so that deploy's notice names each one: a one-shot's
// landing as much as an async merge's (2026-10-27: four deploys of one-shot
// landings said only "a deploy finished", because the notice named only the
// landings with a report owed, and a one-shot owes none).
//
// THE ROW LIVES FROM THE LANDING TO THE DEPLOY'S END, as an owed report does
// (owedhandbacks.go): written before the landing tells the deploy, covered by
// the deploy committing to its build, and taken by whoever finishes that
// deploy, which names it.

// undeployedLandingsDDL adds undeployed_landings: one row per landing whose
// deploy has not ended.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const undeployedLandingsDDL = `
CREATE TABLE undeployed_landings (
  merge_workspace TEXT PRIMARY KEY,
  merge_name      TEXT NOT NULL,
  landed_at       INTEGER NOT NULL,
  covered         INTEGER NOT NULL DEFAULT 0
);
`

// UndeployedLanding is one landing waiting for the deploy that carries it.
type UndeployedLanding struct {
	// MergeWorkspace is the workspace the merge ran in; it names the row.
	MergeWorkspace WorkspaceID
	// MergeName is that workspace's name, which the deploy's notice quotes.
	MergeName string
	// LandedAt is when the merge landed.
	LandedAt time.Time
}

// RecordUndeployedLanding records a landing as waiting for its deploy.
// Recording the same merge workspace twice keeps the first row.
func (s *store) RecordUndeployedLanding(ctx context.Context, landing UndeployedLanding) error {
	const op = "daemon.wsm.record_undeployed_landing"
	fields := dlog.Context{"merge_workspace": string(landing.MergeWorkspace), "merge_name": landing.MergeName, "landed_at": landing.LandedAt}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO undeployed_landings (merge_workspace, merge_name, landed_at) VALUES (?, ?, ?)
			 ON CONFLICT(merge_workspace) DO NOTHING`,
			landing.MergeWorkspace, landing.MergeName, nanos(landing.LandedAt))
		return err
	})
}

// CoverUndeployedLandings marks every waiting landing as covered by the
// deploy committing now, and answers how many it newly covered.
func (s *store) CoverUndeployedLandings(ctx context.Context) (int, error) {
	const op = "daemon.wsm.cover_undeployed_landings"
	var covered int
	err := s.write(ctx, op, nil, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE undeployed_landings SET covered = 1 WHERE covered = 0`)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		covered = int(n)
		return err
	})
	if err != nil {
		return 0, err
	}
	return covered, nil
}

// TakeDeployedLandings deletes and answers, oldest first, the landings a
// deploy that ended carries: only the covered ones when coveredOnly (a
// deploy's own end), else every one (a fresh boot, whose build carries
// everything that landed before it).
func (s *store) TakeDeployedLandings(ctx context.Context, coveredOnly bool) ([]UndeployedLanding, error) {
	const op = "daemon.wsm.take_deployed_landings"
	var out []UndeployedLanding
	err := s.write(ctx, op, dlog.Context{"covered_only": coveredOnly}, func(ctx context.Context, tx *sql.Tx) error {
		out = nil
		rows, err := tx.QueryContext(ctx,
			`DELETE FROM undeployed_landings WHERE covered = 1 OR NOT ?
			 RETURNING merge_workspace, merge_name, landed_at`, coveredOnly)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				landing  UndeployedLanding
				landedAt int64
			)
			if err := rows.Scan(&landing.MergeWorkspace, &landing.MergeName, &landedAt); err != nil {
				return err
			}
			landing.LandedAt = fromNanos(landedAt)
			out = append(out, landing)
		}
		return rows.Err()
	})
	if err != nil {
		return nil, err
	}
	sortLandings(out)
	return out, nil
}

// sortLandings orders landings oldest first, by merge workspace on a tie:
// DELETE ... RETURNING promises no order.
func sortLandings(landings []UndeployedLanding) {
	sort.Slice(landings, func(i, j int) bool {
		if !landings[i].LandedAt.Equal(landings[j].LandedAt) {
			return landings[i].LandedAt.Before(landings[j].LandedAt)
		}
		return landings[i].MergeWorkspace < landings[j].MergeWorkspace
	})
}

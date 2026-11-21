package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
)

// A MERGE WHOSE WORKSPACE STANDS AT ITS COLD GATE KEEPS ITS PLACE IN LINE
// (owner ruling, 2026-11-11, reversing the 2026-10-13 eviction). The gate
// runs no session until the user answers it, and a merge's agentic steps run
// on that session, so the queue SKIPS the entry: it is never admitted, and
// every merge behind it may overtake it. The mark is the queue's own durable
// fact, written under the orchestrator's line lock in the same transaction
// as any move it implies, so the admission, the chain and the slot stamps
// read one consistent line:
//
//   - SetMergeColdGated marks a WAITING entry when its workspace's gate
//     rises.
//   - RequeueColdGatedMerge puts an ADMITTED entry whose workspace parked at
//     its gate back in line at its own place, waiting and marked.
//   - ClearMergeColdGated retires the mark once the gate is passed. An entry
//     the merges worked on meanwhile overtook stands ahead of them; it goes
//     right behind the last of them, ahead of every merge still waiting
//     behind it, so it never evicts a merge being worked on.

// mergeQueueColdGatedDDL is the layout-123 addition: whether an entry's
// workspace stands at its cold gate. IT IS ADDITIVE: the previous build's
// INSERT names no such column, so a row it writes takes the default 0, a
// merge the queue admits as it always did.
const mergeQueueColdGatedDDL = `
ALTER TABLE merge_queue ADD COLUMN cold_gated INTEGER NOT NULL DEFAULT 0;
`

// SetMergeColdGated marks ID's WAITING entry in REPO's queue as standing at
// its cold gate, answering whether the mark changed (false when it was
// already marked). An entry that is not waiting is a refusal: the caller
// lost track of the queue. A workspace with no entry is ErrNotFound.
func (s *store) SetMergeColdGated(ctx context.Context, repo RepoKey, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.set_merge_cold_gated"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return false, err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id)}
	changed := false
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		_, state, gated, err := gatedEntry(ctx, tx, key, id)
		if err != nil {
			return err
		}
		if state != MergeQueued {
			return fmt.Errorf("wsm: workspace %s in repo %q is marked at its cold gate while %s, not waiting", id, key, state)
		}
		if gated {
			return nil
		}
		changed = true
		_, err = tx.ExecContext(ctx, `UPDATE merge_queue SET cold_gated = 1 WHERE repo_key = ? AND workspace_id = ?`, key, id)
		return err
	})
	return changed, err
}

// ClearMergeColdGated retires ID's cold-gate mark in REPO's queue, answering
// whether it changed (false when the entry was not marked). An entry standing
// ahead of a merge WORKED ON -- the caller's word, as ReturnRemediatedMerge
// takes it -- goes right behind the last of them. A workspace with no entry is
// ErrNotFound.
func (s *store) ClearMergeColdGated(ctx context.Context, repo RepoKey, id WorkspaceID, workedOn []WorkspaceID) (bool, error) {
	const op = "daemon.wsm.clear_merge_cold_gated"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return false, err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "worked_on": len(workedOn)}
	changed := false
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		own, _, gated, err := gatedEntry(ctx, tx, key, id)
		if err != nil {
			return err
		}
		if !gated {
			return nil
		}
		changed = true
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET cold_gated = 0 WHERE repo_key = ? AND workspace_id = ?`, key, id); err != nil {
			return err
		}
		if len(workedOn) == 0 {
			return nil
		}
		args := []any{key}
		for _, ws := range workedOn {
			args = append(args, ws)
		}
		var last sql.NullInt64
		if err := tx.QueryRowContext(ctx, `SELECT max(seq) FROM merge_queue WHERE repo_key = ? AND workspace_id IN (?`+strings.Repeat(`, ?`, len(workedOn)-1)+`)`, args...).Scan(&last); err != nil {
			return err
		}
		if !last.Valid || last.Int64 < own {
			return nil
		}
		return moveBack(ctx, tx, key, id, last.Int64)
	})
	return changed, err
}

// RequeueColdGatedMerge puts ID's ADMITTED entry in REPO's queue back in line
// at its own place, waiting and marked at its cold gate, under LEDGER, the new
// bubble's identity. LEASE, when not empty, is the merge lease the entry still
// holds: it goes in the same transaction with its progress record, keeping
// the invariant DropAdmittedMerge keeps (a merge lease stands with exactly
// one admitted entry). An entry that is not admitted is a refusal.
func (s *store) RequeueColdGatedMerge(ctx context.Context, repo RepoKey, id WorkspaceID, lease, ledger LeaseID) error {
	const op = "daemon.wsm.requeue_cold_gated_merge"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "lease": string(lease), "ledger": string(ledger)}
	if ledger == "" {
		err := fmt.Errorf("wsm: a merge put back in line at its cold gate carries its new bubble's ledger identity")
		s.log.Error(op, "refused to put a merge back in line with no ledger identity", withError(fields, err))
		return err
	}
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE merge_queue SET state = ?, cold_gated = 1, ledger_id = ? WHERE repo_key = ? AND workspace_id = ? AND state = ?`,
			int(MergeQueued), ledger, key, id, int(MergeAdmitted))
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: admitted merge of workspace %s in repo %q", id, key)); err != nil {
			return err
		}
		if lease == "" {
			return nil
		}
		return dropMergeLease(ctx, tx, id, lease)
	})
	if err != nil {
		return err
	}
	if lease != "" {
		s.disown(lease)
	}
	return nil
}

// gatedEntry answers the sequence number, the state and the cold-gate mark of
// ID's queue entry. A workspace with no entry is refused with ErrNotFound.
func gatedEntry(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID) (int64, MergeQueueState, bool, error) {
	seq, state, err := entryPlace(ctx, tx, key, id)
	if err != nil {
		return 0, 0, false, err
	}
	var gated bool
	if err := tx.QueryRowContext(ctx, `SELECT cold_gated FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).Scan(&gated); err != nil {
		return 0, 0, false, err
	}
	return seq, state, gated, nil
}

// moveBack moves ID's row back to place TARGET, shifting every row after it,
// up to and including TARGET, one place ahead: moveAhead's mirror. The moved
// row parks on 0 (every sequence is at least 1) and the shifted rows pass
// through negative sequences, so the unique (repo_key, seq) index never sees
// two rows on one place mid-statement.
func moveBack(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, target int64) error {
	var own int64
	if err := tx.QueryRowContext(ctx, `SELECT seq FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).Scan(&own); err != nil {
		return err
	}
	if target <= own {
		return fmt.Errorf("wsm: moving workspace %s in repo %q back to place %d, but it already holds place %d", id, key, target, own)
	}
	steps := []struct {
		query string
		args  []any
	}{
		{`UPDATE merge_queue SET seq = 0 WHERE repo_key = ? AND workspace_id = ?`, []any{key, id}},
		{`UPDATE merge_queue SET seq = -(seq - 1) WHERE repo_key = ? AND seq > ? AND seq <= ?`, []any{key, own, target}},
		{`UPDATE merge_queue SET seq = -seq WHERE repo_key = ? AND seq < 0`, []any{key}},
		{`UPDATE merge_queue SET seq = ? WHERE repo_key = ? AND workspace_id = ?`, []any{target, key, id}},
	}
	for _, step := range steps {
		if _, err := tx.ExecContext(ctx, step.query, step.args...); err != nil {
			return err
		}
	}
	return nil
}

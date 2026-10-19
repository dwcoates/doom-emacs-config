package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// normalizeRepoKey is the ONE spelling of a merge queue's repository key. It is
// a canonical common dir, so it normalizes exactly as a worktree dir does and
// two spellings of one repository can never grow two queues.
func normalizeRepoKey(repo RepoKey) (RepoKey, error) {
	normalized, err := normalizeDir(string(repo))
	if err != nil {
		return "", fmt.Errorf("wsm: merge queue repo key: %w", err)
	}
	return RepoKey(normalized), nil
}

// RequestMerge RECORDS a workspace's merge in its target repository's durable
// queue, in the REQUESTED state: in nobody's line and reported to nobody. A
// merge is recorded the moment it is asked for, so a daemon exit before the
// turn that asked for it ends does not lose it; it takes its place in line
// only at QueueMerge. A workspace already in that queue in ANY state is
// REFUSED with the place it holds: a merge is asked for once.
func (s *store) RequestMerge(ctx context.Context, repo RepoKey, id WorkspaceID, source MergeSource, at time.Time) error {
	return s.requestMerge(ctx, "daemon.wsm.request_merge", repo, id, source, at, nil)
}

// RequestOneShotMerge records a ONE-SHOT merge: RequestMerge's request, and
// the requester marked a one-shot on its creation job (CreationJob.OneShot),
// in ONE transaction. The mark is the fact every surface reads a one-shot
// from, so it can never stand without the merge it announces, nor a refused
// request leave it behind.
//
// A one-shot merge lands the requester's OWN branch IN PLACE and closes the
// requester once it lands, so any other source is refused before anything is
// written. A requester with no creation job is refused too: the merge reads
// its geometry from that job, so there is nothing to mark and nothing to
// merge.
func (s *store) RequestOneShotMerge(ctx context.Context, repo RepoKey, id WorkspaceID, source MergeSource, at time.Time) error {
	const op = "daemon.wsm.request_one_shot_merge"
	if !source.IsOneShot() {
		err := fmt.Errorf("a one-shot merge is an own-branch merge that runs in place and closes the requester, not a %s source with keep_open %v and detach %q",
			source.Kind, source.KeepOpen, source.Detach)
		s.log.Error(op, "refused a one-shot merge request whose source is no one-shot", withError(dlog.Context{
			"repo": string(repo), "workspace": string(id), "source": source.Kind.String()}, err))
		return fmt.Errorf("wsm: one-shot merge request for %s: %w", id, err)
	}
	return s.requestMerge(ctx, op, repo, id, source, at, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE creation_jobs SET one_shot = 1 WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		marked, err := res.RowsAffected()
		if err != nil {
			return err
		}
		if marked != 1 {
			return fmt.Errorf("wsm: one-shot merge request for %s: no creation job records the workspace, so it cannot be marked a one-shot", id)
		}
		return nil
	})
}

// requestMerge is the one body every merge request is recorded through:
// the key normalized, the source validated, a workspace already in the queue
// refused with the place it holds, a request whose landing would close a
// workspace with open children refused (HasOpenChildrenError, family.go),
// then the request row written and MARK (when non-nil) run in the same
// transaction.
func (s *store) requestMerge(ctx context.Context, op string, repo RepoKey, id WorkspaceID, source MergeSource, at time.Time, mark func(context.Context, *sql.Tx) error) error {
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "source": source.Kind.String(), "requested_at": at}
	if err := source.validate(); err != nil {
		s.log.Error(op, "refused a merge request whose source contradicts itself", withError(fields, err))
		return fmt.Errorf("wsm: merge request for %s: %w", id, err)
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var (
			seq   int64
			state int64
		)
		err := tx.QueryRowContext(ctx, `SELECT seq, state FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).Scan(&seq, &state)
		if err == nil {
			existing, err := positionOf(ctx, tx, key, seq)
			if err != nil {
				return err
			}
			return &MergeQueuedError{Repo: key, Workspace: id, Position: existing, State: MergeQueueState(state)}
		}
		if !errors.Is(err, sql.ErrNoRows) {
			return err
		}
		// A MERGE NEVER CLOSES A PARENT OVER ITS OPEN CHILDREN (family.go):
		// every workspace its landing closes is checked in this transaction,
		// so no child can be made under it between the check and the row.
		origin, err := mergeOriginOf(ctx, tx, id)
		if err != nil {
			return err
		}
		for _, closes := range source.Closes(id, origin) {
			if err := refuseOpenChildren(ctx, tx, closes); err != nil {
				return err
			}
		}
		if err := insertRequest(ctx, tx, key, id, source, at); err != nil {
			return err
		}
		if mark == nil {
			return nil
		}
		return mark(ctx, tx)
	})
}

// insertRequest writes one REQUESTED merge row at the back of KEY's queue. It
// is the one shape a request row is written in, for a request the user or an
// agent made (RequestMerge) and for the merge workspace a detached request
// hands off to (HandOffDetachedMerge).
func insertRequest(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, source MergeSource, at time.Time) error {
	next, err := nextSeq(ctx, tx, key)
	if err != nil {
		return err
	}
	before, err := encodeActions(source.Actions.Before)
	if err != nil {
		return err
	}
	after, err := encodeActions(source.Actions.After)
	if err != nil {
		return err
	}
	_, err = tx.ExecContext(ctx,
		`INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, source_keep_open, source_workspace, source_branch, source_detach_id, source_front, source_detach_name, source_pick, pick_started_at, pick_turn, source_compact_after, source_close_on_landing, source_cleanup_worktree, source_cleanup_dir, source_version_part, source_changelog_line, source_actions_before, source_actions_after)
		 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		key, id, next, int(MergeRequested), nanos(at),
		int(source.Kind), source.KeepOpen, nullableText(string(source.Workspace)), nullableText(source.Branch), nullableText(string(source.Detach)), source.Front,
		nullableText(source.DetachName), nullableText(source.Pick), nullNanos(source.PickStartedAt), nullableText(string(source.PickTurn)), source.CompactAfter,
		nullableText(string(source.CloseOnLanding)), source.CleanupWorktree, nullableText(source.CleanupDir), versionPart(source.Version), versionLine(source.Version),
		before, after)
	return err
}

// QueueMerge moves a REQUESTED merge into line, AT THE BACK: its place is
// decided when it is queued, not when it was asked for, because a merge in line
// is one every client may see and the requesting turn's end is what lets it be
// seen. It answers the merge's one-based place among the entries in line. A
// merge that is not requested is refused: the caller lost track of it.
//
// THE BUBBLE'S IDENTITY GOES ON THE ROW WITH ITS PLACE: a merge in line is
// drawn from here on, so its ledger identity is as durable as the place
// itself, and a queued merge is never in line without one.
func (s *store) QueueMerge(ctx context.Context, repo RepoKey, id WorkspaceID, ledger LeaseID) (int, error) {
	const op = "daemon.wsm.queue_merge"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return 0, err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "ledger": string(ledger)}
	if ledger == "" {
		err := errors.New("wsm: a merge is put in line with its bubble's ledger identity")
		s.log.Error(op, "refused to queue a merge with no ledger identity", withError(fields, err))
		return 0, err
	}
	var position int
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		next, err := placeInLine(ctx, tx, key, id)
		if err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx,
			`UPDATE merge_queue SET state = ?, seq = ?, ledger_id = ? WHERE repo_key = ? AND workspace_id = ? AND state = ?`,
			int(MergeQueued), next, ledger, key, id, int(MergeRequested))
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: requested merge of workspace %s in repo %q", id, key)); err != nil {
			return err
		}
		position, err = inLinePositionOf(ctx, tx, key, next)
		return err
	})
	if err != nil {
		return 0, err
	}
	return position, nil
}

// placeInLine answers the sequence number a requested merge takes as it is put
// in line: one past the queue's last, or -- for a merge that asked for the
// FRONT (MergeSource.Front) -- the place of the first ORDINARY merge still
// WAITING, with that merge and every row behind it moved back one. So front
// merges keep their own arrival order ahead of every ordinary waiting merge,
// and a merge already ADMITTED is never passed: it holds the repository's one
// slot. A queue with no ordinary merge waiting puts a front merge at the back,
// which is behind every earlier front merge and ahead of nothing it should
// pass.
//
// The requested row goes to the back, then moves up through moveAhead.
func placeInLine(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID) (int64, error) {
	var front bool
	if err := tx.QueryRowContext(ctx, `SELECT source_front FROM merge_queue WHERE repo_key = ? AND workspace_id = ? AND state = ?`,
		key, id, int(MergeRequested)).Scan(&front); err != nil && !errors.Is(err, sql.ErrNoRows) {
		return 0, err
	}
	var waiting sql.NullInt64
	if front {
		if err := tx.QueryRowContext(ctx, `SELECT min(seq) FROM merge_queue WHERE repo_key = ? AND state = ? AND source_front = 0`,
			key, int(MergeQueued)).Scan(&waiting); err != nil {
			return 0, err
		}
	}
	if !waiting.Valid {
		return nextSeq(ctx, tx, key)
	}
	// A REQUESTED ROW'S SEQUENCE IS NOT A PLACE IN LINE: it was taken when the
	// merge was asked for, and merges queued since may sit behind it. The row
	// goes to the back first, so the move ahead passes exactly the rows from
	// the target on.
	back, err := nextSeq(ctx, tx, key)
	if err != nil {
		return 0, err
	}
	if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET seq = ? WHERE repo_key = ? AND workspace_id = ?`, back, key, id); err != nil {
		return 0, err
	}
	if err := moveAhead(ctx, tx, key, id, waiting.Int64); err != nil {
		return 0, err
	}
	return waiting.Int64, nil
}

// moveAhead moves one row up to the place target, moving every row from target
// up to the row's own old place back one. Rows behind the moved row keep their
// places.
//
// THE SHIFT GOES THROUGH NEGATIVE SEQUENCES, so the unique (repo_key, seq)
// index never sees two rows on one place mid-statement: the moved row parks on
// -1 (no shifted row can land there, since every sequence is at least 1), every
// row in the shifted range moves to -(seq+1), and the shifted rows then come
// back positive, one place later. The last of them lands on the moved row's
// vacated place.
func moveAhead(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, target int64) error {
	var own int64
	if err := tx.QueryRowContext(ctx, `SELECT seq FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).Scan(&own); err != nil {
		return err
	}
	if target >= own {
		return fmt.Errorf("wsm: moving workspace %s in repo %q ahead to place %d, but it already holds place %d", id, key, target, own)
	}
	steps := []struct {
		query string
		args  []any
	}{
		{`UPDATE merge_queue SET seq = -1 WHERE repo_key = ? AND workspace_id = ?`, []any{key, id}},
		{`UPDATE merge_queue SET seq = -(seq + 1) WHERE repo_key = ? AND seq >= ? AND seq < ?`, []any{key, target, own}},
		{`UPDATE merge_queue SET seq = -seq WHERE repo_key = ? AND seq < -1`, []any{key}},
		{`UPDATE merge_queue SET seq = ? WHERE repo_key = ? AND workspace_id = ?`, []any{target, key, id}},
	}
	for _, step := range steps {
		if _, err := tx.ExecContext(ctx, step.query, step.args...); err != nil {
			return err
		}
	}
	return nil
}

// MoveMerge moves a merge WAITING in line one place up or down its queue,
// swapping it with the nearest waiting merge on that side. It answers whether
// the merge moved.
//
// ONLY WAITING MERGES TRADE PLACES. The admitted merge holds the repository's
// one slot and is never passed, and a merge only REQUESTED is in nobody's line
// (it takes its place when it is queued), so neither is the neighbor a move
// swaps with, and neither moves itself. A merge with no waiting neighbor on
// that side, or a merge not waiting, is left where it is.
//
// THE MOVED MERGE TAKES THE PASSED MERGE'S FRONT MARK, so a move can never
// put an ordinary merge ahead of a front merge or a front merge behind an
// ordinary one: every front merge stays ahead of every ordinary merge waiting,
// which is the order placeInLine puts a newly queued front merge into.
//
// A workspace with no queue entry is refused.
func (s *store) MoveMerge(ctx context.Context, repo RepoKey, id WorkspaceID, move MergeMove) (bool, error) {
	const op = "daemon.wsm.move_merge"
	fields := dlog.Context{"repo": string(repo), "workspace": string(id), "move": move.String()}
	if !move.valid() {
		err := fmt.Errorf("wsm: undeclared merge move %d", int(move))
		s.log.Error(op, "refused an undeclared merge move", withError(fields, err))
		return false, err
	}
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(fields, err))
		return false, err
	}
	fields["repo"] = string(key)
	var moved bool
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		seq, state, err := entryPlace(ctx, tx, key, id)
		if err != nil || state != MergeQueued {
			return err
		}
		query := `SELECT seq, source_front FROM merge_queue WHERE repo_key = ? AND state = ? AND seq < ? ORDER BY seq DESC LIMIT 1`
		if move == MergeMoveDown {
			query = `SELECT seq, source_front FROM merge_queue WHERE repo_key = ? AND state = ? AND seq > ? ORDER BY seq LIMIT 1`
		}
		var neighbor int64
		var neighborFront bool
		err = tx.QueryRowContext(ctx, query, key, int(MergeQueued), seq).Scan(&neighbor, &neighborFront)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		moved = true
		return swapPlaces(ctx, tx, key, id, seq, neighbor, neighborFront)
	})
	if err != nil {
		return false, err
	}
	return moved, nil
}

// PrioritizeMerge moves a merge WAITING in line to the front of the merges
// waiting in its queue: it runs next, right behind the merge that holds the
// repository's slot. It answers whether the merge moved.
//
// ONLY A WAITING MERGE MOVES, and it passes only waiting merges, for the
// reasons MoveMerge gives: the admitted merge holds the slot and is never
// passed, and a merge only requested is in nobody's line. A merge already
// first among the waiting, or not waiting, is left where it is.
//
// THE MOVED MERGE TAKES THE FRONT MARK. It now stands ahead of every waiting
// merge, front merges included, so marking it front keeps every front merge
// ahead of every ordinary merge waiting (placeInLine's order).
//
// A workspace with no queue entry is refused.
func (s *store) PrioritizeMerge(ctx context.Context, repo RepoKey, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.prioritize_merge"
	fields := dlog.Context{"repo": string(repo), "workspace": string(id)}
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(fields, err))
		return false, err
	}
	fields["repo"] = string(key)
	var moved bool
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		seq, state, err := entryPlace(ctx, tx, key, id)
		if err != nil || state != MergeQueued {
			return err
		}
		var first int64
		if err := tx.QueryRowContext(ctx, `SELECT min(seq) FROM merge_queue WHERE repo_key = ? AND state = ?`,
			key, int(MergeQueued)).Scan(&first); err != nil {
			return err
		}
		if first == seq {
			return nil
		}
		if err := moveAhead(ctx, tx, key, id, first); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET source_front = 1 WHERE repo_key = ? AND workspace_id = ?`, key, id); err != nil {
			return err
		}
		moved = true
		return nil
	})
	if err != nil {
		return false, err
	}
	return moved, nil
}

// entryPlace answers the sequence number and the state of ID's queue entry, the
// two facts every reordering checks before it moves a merge. A workspace with
// no queue entry is refused with ErrNotFound.
func entryPlace(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID) (int64, MergeQueueState, error) {
	var state int
	var seq int64
	err := tx.QueryRowContext(ctx, `SELECT state, seq FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).
		Scan(&state, &seq)
	if errors.Is(err, sql.ErrNoRows) {
		return 0, 0, fmt.Errorf("wsm: merge queue entry for workspace %s in repo %q: %w", id, key, ErrNotFound)
	}
	if err != nil {
		return 0, 0, err
	}
	return seq, MergeQueueState(state), nil
}

// swapPlaces trades the places of the row for ID (at own) and the row at
// other, giving ID's row FRONT as its front mark. The moved row parks on -1
// first, so the unique (repo_key, seq) index never sees two rows on one place
// (every sequence is at least 1).
func swapPlaces(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, own, other int64, front bool) error {
	steps := []struct {
		query string
		args  []any
	}{
		{`UPDATE merge_queue SET seq = -1 WHERE repo_key = ? AND workspace_id = ?`, []any{key, id}},
		{`UPDATE merge_queue SET seq = ? WHERE repo_key = ? AND seq = ?`, []any{own, key, other}},
		{`UPDATE merge_queue SET seq = ?, source_front = ? WHERE repo_key = ? AND workspace_id = ?`, []any{other, front, key, id}},
	}
	for _, step := range steps {
		if _, err := tx.ExecContext(ctx, step.query, step.args...); err != nil {
			return err
		}
	}
	return nil
}

// nextSeq answers the sequence number one past a queue's last.
func nextSeq(ctx context.Context, tx *sql.Tx, key RepoKey) (int64, error) {
	var last sql.NullInt64
	if err := tx.QueryRowContext(ctx, `SELECT max(seq) FROM merge_queue WHERE repo_key = ?`, key).Scan(&last); err != nil {
		return 0, err
	}
	return last.Int64 + 1, nil
}

// positionOf reports the one-based place a sequence number holds in its queue.
func positionOf(ctx context.Context, tx *sql.Tx, key RepoKey, seq int64) (int, error) {
	var ahead int
	if err := tx.QueryRowContext(ctx, `SELECT count(*) FROM merge_queue WHERE repo_key = ? AND seq < ?`, key, seq).Scan(&ahead); err != nil {
		return 0, err
	}
	return ahead + 1, nil
}

// inLinePositionOf reports the one-based place a sequence number holds among
// the entries IN LINE: a requested entry is in nobody's line.
func inLinePositionOf(ctx context.Context, tx *sql.Tx, key RepoKey, seq int64) (int, error) {
	var ahead int
	if err := tx.QueryRowContext(ctx, `SELECT count(*) FROM merge_queue WHERE repo_key = ? AND seq < ? AND state != ?`,
		key, seq, int(MergeRequested)).Scan(&ahead); err != nil {
		return 0, err
	}
	return ahead + 1, nil
}

// nullableText stores an empty field as NULL, so a source arm that carries no
// such field has none on its row.
func nullableText(v string) any {
	if v == "" {
		return nil
	}
	return v
}

// setMergeQueueState is the one state-write path for a queue entry, refusing an
// entry that is not in the queue rather than affecting no rows.
func (s *store) setMergeQueueState(ctx context.Context, op string, repo RepoKey, id WorkspaceID, state MergeQueueState, fields dlog.Context) error {
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(fields, err))
		return err
	}
	fields["repo"] = string(key)
	fields["workspace"] = string(id)
	fields["state"] = state.String()
	if !state.valid() {
		err := fmt.Errorf("wsm: undeclared merge queue state %d", int(state))
		s.log.Error(op, "refused an undeclared merge queue state", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE merge_queue SET state = ? WHERE repo_key = ? AND workspace_id = ?`, int(state), key, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: merge queue entry for workspace %s in repo %q", id, key))
	})
}

// YieldMergeHead records a race to land the head lost: winner, the merge
// right behind the head that passed its gate on the target's tip first, takes
// the head's place at the front of the line, and loser, the head it overtook,
// stands right behind it, still admitted: it is suspended at the step it was
// on, and resumes there once the winner is done. Both must be admitted, with
// loser ahead of winner; anything else is a refusal, because it means the
// caller lost track of the queue.
func (s *store) YieldMergeHead(ctx context.Context, repo RepoKey, loser, winner WorkspaceID) error {
	const op = "daemon.wsm.yield_merge_head"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "loser": string(loser), "winner": string(winner)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "loser": string(loser), "winner": string(winner)}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		seqOf := func(id WorkspaceID) (int64, error) {
			seq, state, err := entryPlace(ctx, tx, key, id)
			if err != nil {
				return 0, err
			}
			if state != MergeAdmitted {
				return 0, fmt.Errorf("wsm: workspace %s in repo %q yields or wins a race to land while %s, not admitted", id, key, state)
			}
			return seq, nil
		}
		loserSeq, err := seqOf(loser)
		if err != nil {
			return err
		}
		winnerSeq, err := seqOf(winner)
		if err != nil {
			return err
		}
		if loserSeq >= winnerSeq {
			return fmt.Errorf("wsm: workspace %s in repo %q cannot overtake %s, which already stands behind it", winner, key, loser)
		}
		return moveAhead(ctx, tx, key, winner, loserSeq)
	})
}

// AdmitMerge marks the entry the orchestrator is running now.
func (s *store) AdmitMerge(ctx context.Context, repo RepoKey, id WorkspaceID) error {
	return s.setMergeQueueState(ctx, "daemon.wsm.admit_merge", repo, id, MergeAdmitted, dlog.Context{})
}

// RemoveMergeQueueEntry drops one entry, recording the cause it was dropped for
// in the operation's log record. Removing an entry that is not queued is a
// refusal, never a no-op: it means the caller lost track of the queue.
func (s *store) RemoveMergeQueueEntry(ctx context.Context, repo RepoKey, id WorkspaceID, cause string) error {
	const op = "daemon.wsm.remove_merge_queue_entry"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "cause": cause}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: merge queue entry for workspace %s in repo %q", id, key))
	})
}

// DropAdmittedMerge takes an ADMITTED merge that has no run off its queue:
// its queue entry, its merge lease and that lease's progress record go in ONE
// transaction, recording the cause in the operation's log record. It is the
// one way such a merge is dropped, because it keeps the invariant every
// prompt held under a merge relies on (bindMergeHold): a merge lease stands
// with exactly one queue entry. Removing the entry alone would leave the
// lease standing over none, and every later prompt on the workspace would be
// refused. An entry that is not admitted, or a lease that is not that
// workspace's merge lease, is a refusal and changes nothing.
func (s *store) DropAdmittedMerge(ctx context.Context, repo RepoKey, id WorkspaceID, lease LeaseID, cause string) error {
	const op = "daemon.wsm.drop_admitted_merge"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "lease": string(lease), "cause": cause}
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM merge_queue WHERE repo_key = ? AND workspace_id = ? AND state = ?`, key, id, int(MergeAdmitted))
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: admitted merge queue entry for workspace %s in repo %q", id, key)); err != nil {
			return err
		}
		res, err = tx.ExecContext(ctx, `DELETE FROM leases WHERE id = ? AND workspace_id = ? AND holder = ?`, lease, id, int(HolderMerge))
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: merge lease %s of workspace %s", lease, id)); err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx, `DELETE FROM merge_progress WHERE lease_id = ?`, lease)
		return err
	})
	if err != nil {
		return err
	}
	s.disown(lease)
	return nil
}

// MergeQueue loads one repository's queue in order, all-or-nothing.
func (s *store) MergeQueue(ctx context.Context, repo RepoKey) ([]MergeQueueEntry, error) {
	const op = "daemon.wsm.merge_queue"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return nil, err
	}
	var out []MergeQueueEntry
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		loaded, err := s.scanMergeQueue(ctx, `WHERE repo_key = ? ORDER BY seq`, key)
		if err != nil {
			return err
		}
		out = loaded[key]
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// AllMergeQueues loads every repository's queue for the boot re-enqueue,
// all-or-nothing: one corrupt entry loads no queue at all.
func (s *store) AllMergeQueues(ctx context.Context) (map[RepoKey][]MergeQueueEntry, error) {
	var out map[RepoKey][]MergeQueueEntry
	err := s.read(ctx, "daemon.wsm.all_merge_queues", dlog.Context{}, func(ctx context.Context) error {
		loaded, err := s.scanMergeQueue(ctx, `ORDER BY repo_key, seq`)
		if err != nil {
			return err
		}
		out = loaded
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// mergeQueueColumns are the merge_queue columns every queue read selects, in
// the order scanMergeQueue scans them: the one list, so a read cannot select
// columns its scan does not expect.
const mergeQueueColumns = `repo_key, workspace_id, state, enqueued_at, source_kind, source_keep_open, source_workspace, source_branch, source_detach_id, ledger_id, source_front, source_detach_name, source_pick, pick_started_at, pick_turn, source_compact_after, source_close_on_landing, source_cleanup_worktree, source_cleanup_dir, source_version_part, source_changelog_line, source_actions_before, source_actions_after`

// scanMergeQueue is the one queue-decoding path, so both readers reject an
// undeclared state identically and number positions the same way. TAIL is the
// read's own WHERE and ORDER BY; the columns are always mergeQueueColumns. The rows
// arrive in sequence order, so the position is the index within its repository.
func (s *store) scanMergeQueue(ctx context.Context, tail string, args ...any) (map[RepoKey][]MergeQueueEntry, error) {
	rows, err := s.db().QueryContext(ctx, `SELECT `+mergeQueueColumns+` FROM merge_queue `+tail, args...)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	loaded := map[RepoKey][]MergeQueueEntry{}
	for rows.Next() {
		var (
			entry     MergeQueueEntry
			state     int64
			enqueued  int64
			kind      int64
			keepOpen  bool
			sourceWS  sql.NullString
			sourceRef sql.NullString
			detach    sql.NullString
			name      sql.NullString
			pick      sql.NullString
			picked    sql.NullInt64
			pickTurn  sql.NullString
			ledger    sql.NullString
			front     bool
			compact   bool
			closes    sql.NullString
			cleanup   bool
			cleanDir  sql.NullString
			verPart   sql.NullInt64
			verLine   sql.NullString
			before    string
			after     string
		)
		if err := rows.Scan(&entry.Repo, &entry.Workspace, &state, &enqueued, &kind, &keepOpen, &sourceWS, &sourceRef, &detach, &ledger, &front, &name, &pick, &picked, &pickTurn, &compact, &closes, &cleanup, &cleanDir, &verPart, &verLine, &before, &after); err != nil {
			return nil, err
		}
		row := fmt.Sprintf("%s/%s", entry.Repo, entry.Workspace)
		entry.State = MergeQueueState(state)
		if !entry.State.valid() {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "state", Err: fmt.Errorf("unknown merge queue state %d", state)}
		}
		entry.Source = MergeSource{
			Kind: MergeSourceKind(kind), KeepOpen: keepOpen, Workspace: WorkspaceID(sourceWS.String), Branch: sourceRef.String,
			Detach: DetachedMergeID(detach.String), Front: front, DetachName: name.String, Pick: pick.String, PickStartedAt: optTime(picked),
			PickTurn: TurnID(pickTurn.String), CompactAfter: compact,
			CloseOnLanding: WorkspaceID(closes.String), CleanupWorktree: cleanup, CleanupDir: cleanDir.String,
		}
		version, err := scanVersion(verPart, verLine)
		if err != nil {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "version", Err: err}
		}
		entry.Source.Version = version
		if entry.Source.Actions.Before, err = decodeActions("merge_queue", row, "source_actions_before", before); err != nil {
			return nil, err
		}
		if entry.Source.Actions.After, err = decodeActions("merge_queue", row, "source_actions_after", after); err != nil {
			return nil, err
		}
		if err := entry.Source.validate(); err != nil {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "source", Err: err}
		}
		entry.EnqueuedAt = fromNanos(enqueued)
		entry.Ledger = LeaseID(ledger.String)
		entry.Position = len(loaded[entry.Repo]) + 1
		loaded[entry.Repo] = append(loaded[entry.Repo], entry)
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return loaded, nil
}

// SetMergeQueuePaused pauses or resumes one repository's queue.
func (s *store) SetMergeQueuePaused(ctx context.Context, repo RepoKey, paused bool) error {
	const op = "daemon.wsm.set_merge_queue_paused"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return err
	}
	return s.write(ctx, op, dlog.Context{"repo": string(key), "paused": paused}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO merge_queue_repos (repo_key, paused) VALUES (?, ?)
			 ON CONFLICT(repo_key) DO UPDATE SET paused = excluded.paused`, key, paused)
		return err
	})
}

// MergeQueuePaused reports whether one repository's queue is paused. A
// repository with no pause row has never been paused, which is not the same
// fact as a corrupt row and is why the absence is reported as false rather than
// refused.
func (s *store) MergeQueuePaused(ctx context.Context, repo RepoKey) (bool, error) {
	const op = "daemon.wsm.merge_queue_paused"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	var paused bool
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		err := s.db().QueryRowContext(ctx, `SELECT paused FROM merge_queue_repos WHERE repo_key = ?`, key).Scan(&paused)
		if errors.Is(err, sql.ErrNoRows) {
			paused = false
			return nil
		}
		return err
	})
	return paused, err
}

package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
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
// refused with the place it holds, then the request row written and MARK (when
// non-nil) run in the same transaction.
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
	_, err = tx.ExecContext(ctx,
		`INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, source_keep_open, source_workspace, source_branch, source_detach_id, source_front)
		 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		key, id, next, int(MergeRequested), nanos(at),
		int(source.Kind), source.KeepOpen, nullableText(string(source.Workspace)), nullableText(source.Branch), nullableText(string(source.Detach)), source.Front)
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

// MoveMergeToFront marks a merge a front merge and, when it is waiting in
// line, moves it ahead of every ordinary merge waiting, behind the admitted
// merge and every front merge already in line. It answers whether the merge
// moved. A merge already at the front, or already admitted, is left where it
// is; a merge only REQUESTED is marked and takes its front place when it is
// queued (placeInLine). A workspace with no queue entry is refused.
func (s *store) MoveMergeToFront(ctx context.Context, repo RepoKey, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.move_merge_to_front"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return false, err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id)}
	var moved bool
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var state int
		var seq int64
		var front bool
		err := tx.QueryRowContext(ctx, `SELECT state, seq, source_front FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).
			Scan(&state, &seq, &front)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: merge queue entry for workspace %s in repo %q: %w", id, key, ErrNotFound)
		}
		if err != nil {
			return err
		}
		if MergeQueueState(state) == MergeAdmitted || front {
			return nil
		}
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET source_front = 1 WHERE repo_key = ? AND workspace_id = ?`, key, id); err != nil {
			return err
		}
		if MergeQueueState(state) != MergeQueued {
			return nil
		}
		var waiting sql.NullInt64
		if err := tx.QueryRowContext(ctx, `SELECT min(seq) FROM merge_queue WHERE repo_key = ? AND state = ? AND source_front = 0 AND seq < ?`,
			key, int(MergeQueued), seq).Scan(&waiting); err != nil {
			return err
		}
		if !waiting.Valid {
			return nil
		}
		moved = true
		return moveAhead(ctx, tx, key, id, waiting.Int64)
	})
	if err != nil {
		return false, err
	}
	return moved, nil
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
		loaded, err := s.scanMergeQueue(ctx,
			`SELECT repo_key, workspace_id, state, enqueued_at, source_kind, source_keep_open, source_workspace, source_branch, source_detach_id, ledger_id, source_front FROM merge_queue WHERE repo_key = ? ORDER BY seq`, key)
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
		loaded, err := s.scanMergeQueue(ctx, `SELECT repo_key, workspace_id, state, enqueued_at, source_kind, source_keep_open, source_workspace, source_branch, source_detach_id, ledger_id, source_front FROM merge_queue ORDER BY repo_key, seq`)
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

// scanMergeQueue is the one queue-decoding path, so both readers reject an
// undeclared state identically and number positions the same way. The rows
// arrive in sequence order, so the position is the index within its repository.
func (s *store) scanMergeQueue(ctx context.Context, query string, args ...any) (map[RepoKey][]MergeQueueEntry, error) {
	rows, err := s.db().QueryContext(ctx, query, args...)
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
			ledger    sql.NullString
			front     bool
		)
		if err := rows.Scan(&entry.Repo, &entry.Workspace, &state, &enqueued, &kind, &keepOpen, &sourceWS, &sourceRef, &detach, &ledger, &front); err != nil {
			return nil, err
		}
		row := fmt.Sprintf("%s/%s", entry.Repo, entry.Workspace)
		entry.State = MergeQueueState(state)
		if !entry.State.valid() {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "state", Err: fmt.Errorf("unknown merge queue state %d", state)}
		}
		entry.Source = MergeSource{Kind: MergeSourceKind(kind), KeepOpen: keepOpen, Workspace: WorkspaceID(sourceWS.String), Branch: sourceRef.String, Detach: DetachedMergeID(detach.String), Front: front}
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

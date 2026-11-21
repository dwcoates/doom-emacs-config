package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"
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

// normalizedRepoKey is normalizeRepoKey for a store operation: a key that
// cannot be normalized is recorded under OP with FIELDS at ERROR, once, and
// answered.
func (s *store) normalizedRepoKey(op string, repo RepoKey, fields dlog.Context) (RepoKey, error) {
	normalized, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(fields, err))
		return "", err
	}
	return normalized, nil
}

// RequestMerge RECORDS a workspace's merge in its target repository's durable
// queue, in the REQUESTED state: in nobody's line and reported to nobody. A
// merge is recorded the moment it is asked for, so a daemon exit before the
// turn that asked for it ends does not lose it; it takes its place in line
// only at QueueMerge. A workspace already in that queue in ANY state is
// REFUSED with the place it holds, and one whose request stands off its
// queue with MergeOffQueueError: a merge is asked for once.
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
// refused with the place it holds, a workspace whose request stands off its
// queue refused with why it left (MergeOffQueueError), a request whose landing would close a
// workspace with open children refused (HasOpenChildrenError, family.go),
// then the request row written and MARK (when non-nil) run in the same
// transaction.
func (s *store) requestMerge(ctx context.Context, op string, repo RepoKey, id WorkspaceID, source MergeSource, at time.Time, mark func(context.Context, *sql.Tx) error) error {
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "source": source.Kind.String(), "requested_at": at}
	if err := source.validate(); err != nil {
		s.log.Error(op, "refused a merge request whose source contradicts itself", withError(fields, err))
		return fmt.Errorf("wsm: merge request for %s: %w", id, err)
	}
	// AN ASYNC REQUEST IS NEVER A QUEUE ROW: it is recorded on its own
	// (RequestAsyncMerge), so every merge_queue row is a workspace's own merge.
	if source.Detach != "" {
		err := fmt.Errorf("an async merge request (detach %q) is recorded by RequestAsyncMerge, never on the requester's queue row", source.Detach)
		s.log.Error(op, "refused to record an async merge request on the requester's queue row", withError(fields, err))
		return fmt.Errorf("wsm: merge request for %s: %w", id, err)
	}
	return s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		if err := refuseQueued(ctx, tx, key, id); err != nil {
			return err
		}
		// A WORKSPACE HOLDS ONE MERGE, IN LINE OR OFF IT (offqueue.go): a
		// request standing off its queue is requeued or discarded, never
		// asked for again, so a second request beside it is refused here,
		// in the transaction that would write it.
		if err := refuseOffQueue(ctx, tx, id); err != nil {
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

// insertRequest writes one REQUESTED merge row at the back of KEY's queue. A
// request placed behind another records the wait (MergeQueueEntry.WaitsOn)
// with the row, so the wait stands from the request on, before the merge is
// even in line, and a restart reads it back. It
// is the one shape a request row is written in, for a request the user or an
// agent made (RequestMerge) and for the merge workspace a detached request
// hands off to (HandOffDetachedMerge).
func insertRequest(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, source MergeSource, at time.Time) error {
	next, err := nextSeq(ctx, tx, key)
	if err != nil {
		return err
	}
	before, err := encodeHooks(source.Hooks.PreDeploy)
	if err != nil {
		return err
	}
	after, err := encodeHooks(source.Hooks.PostDeploy)
	if err != nil {
		return err
	}
	_, err = tx.ExecContext(ctx,
		`INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, source_kind, source_keep_open, source_workspace, source_branch, source_detach_id, source_front, source_detach_name, source_pick, pick_started_at, pick_turn, source_compact_after, source_close_on_landing, source_cleanup_worktree, source_cleanup_dir, source_version_part, source_changelog_line, source_pre_deploy_hooks, source_post_deploy_hooks, source_detach_maker, source_detach_target_maker, source_behind, waits_on, waits_on_name)
		 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		key, id, next, int(MergeRequested), nanos(at),
		int(source.Kind), source.KeepOpen, nullableText(string(source.Workspace)), nullableText(source.Branch), nullableText(string(source.Detach)), source.Front,
		nullableText(source.DetachName), nullableText(source.Pick), nullNanos(source.PickStartedAt), nullableText(string(source.PickTurn)), source.CompactAfter,
		nullableText(string(source.CloseOnLanding)), source.CleanupWorktree, nullableText(source.CleanupDir), versionPart(source.Version), versionLine(source.Version),
		before, after, nullableText(string(source.DetachMaker)), source.DetachTargetMaker, nullableText(string(source.Behind)),
		nullableText(string(source.Behind)), nullableText(source.BehindName))
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
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return 0, err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "ledger": string(ledger)}
	if ledger == "" {
		err := errors.New("wsm: a merge is put in line with its bubble's ledger identity")
		s.log.Error(op, "refused to queue a merge with no ledger identity", withError(fields, err))
		return 0, err
	}
	var position int
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
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
// WAITING, with that merge and every row behind it moved back one, or -- for a
// merge that asked to go BEHIND another (MergeSource.Behind) -- the place
// directly behind it (placeBehind). So front merges keep their own arrival
// order ahead of every ordinary waiting merge, and a merge already ADMITTED is
// never passed: it holds the repository's one slot. A queue with no ordinary
// merge waiting puts a front merge at the back, which is behind every earlier
// front merge and ahead of nothing it should pass.
//
// The requested row goes to the back, then moves up through moveAhead.
func placeInLine(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID) (int64, error) {
	var (
		front  bool
		behind sql.NullString
	)
	if err := tx.QueryRowContext(ctx, `SELECT source_front, source_behind FROM merge_queue WHERE repo_key = ? AND workspace_id = ? AND state = ?`,
		key, id, int(MergeRequested)).Scan(&front, &behind); err != nil && !errors.Is(err, sql.ErrNoRows) {
		return 0, err
	}
	var waiting sql.NullInt64
	switch {
	case behind.Valid:
		target, mark, err := placeBehind(ctx, tx, key, WorkspaceID(behind.String))
		if err != nil {
			return 0, err
		}
		// THE NEIGHBOR'S FRONT MARK, so no front merge waits behind an
		// ordinary one and moves keep the order placeInLine makes. The ask
		// is spent once placed: a merge in line holds its place by its mark
		// and its sequence alone, as every other does.
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET source_front = ?, source_behind = NULL WHERE repo_key = ? AND workspace_id = ?`, mark, key, id); err != nil {
			return 0, err
		}
		waiting = target
	case front:
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

// placeBehind answers the place directly behind AHEAD in KEY's line, and the
// front mark a merge put there takes: the first waiting merge after a waiting
// AHEAD, with AHEAD's own mark; the first waiting place behind an ADMITTED
// AHEAD (the running merge), marked front, since it passes every waiting
// merge; and the same for an AHEAD no longer in line, whose place was the
// head of the waiting line once it was taken. No place answered (an invalid
// target) is the back, past the last waiting merge.
func placeBehind(ctx context.Context, tx *sql.Tx, key RepoKey, ahead WorkspaceID) (sql.NullInt64, bool, error) {
	var (
		seq   int64
		state int64
		front bool
	)
	err := tx.QueryRowContext(ctx, `SELECT seq, state, source_front FROM merge_queue WHERE repo_key = ? AND workspace_id = ? AND state IN (?, ?)`,
		key, ahead, int(MergeQueued), int(MergeAdmitted)).Scan(&seq, &state, &front)
	var target sql.NullInt64
	switch {
	case errors.Is(err, sql.ErrNoRows), err == nil && MergeQueueState(state) == MergeAdmitted:
		if err := tx.QueryRowContext(ctx, `SELECT min(seq) FROM merge_queue WHERE repo_key = ? AND state = ?`, key, int(MergeQueued)).Scan(&target); err != nil {
			return sql.NullInt64{}, false, err
		}
		return target, true, nil
	case err != nil:
		return sql.NullInt64{}, false, err
	}
	if err := tx.QueryRowContext(ctx, `SELECT min(seq) FROM merge_queue WHERE repo_key = ? AND state = ? AND seq > ?`, key, int(MergeQueued), seq).Scan(&target); err != nil {
		return sql.NullInt64{}, false, err
	}
	return target, front, nil
}

// moveAhead moves one row up to the place target, moving every row from target
// up to the row's own old place back one place in the order. Rows behind the
// moved row keep their places.
func moveAhead(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, target int64) error {
	var own int64
	if err := tx.QueryRowContext(ctx, `SELECT seq FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).Scan(&own); err != nil {
		return err
	}
	if target >= own {
		return fmt.Errorf("wsm: moving workspace %s in repo %q ahead to place %d, but it already holds place %d", id, key, target, own)
	}
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, seq FROM merge_queue WHERE repo_key = ? AND seq >= ? AND seq <= ? ORDER BY seq`, key, target, own)
	if err != nil {
		return err
	}
	var places []int64
	order := []WorkspaceID{id}
	for rows.Next() {
		var ws WorkspaceID
		var seq int64
		if err := rows.Scan(&ws, &seq); err != nil {
			rows.Close()
			return err
		}
		places = append(places, seq)
		if ws != id {
			order = append(order, ws)
		}
	}
	if err := rows.Close(); err != nil {
		return err
	}
	return assignQueuePlaces(ctx, tx, key, places, order)
}

// admittedAhead moves ID, a merge just admitted, ahead of every merge still
// waiting in line before it (moveAhead): the admission passed them over
// because another daemon still serves their workspaces (owner ruling,
// 2026-11-15). The line's order is the order merges run in, so the roster's
// head is the merge running and every place the line reads stays true. The
// merges passed over keep their order, directly behind it.
func admittedAhead(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID) error {
	seq, _, err := entryPlace(ctx, tx, key, id)
	if err != nil {
		return err
	}
	var first sql.NullInt64
	if err := tx.QueryRowContext(ctx, `SELECT min(seq) FROM merge_queue WHERE repo_key = ? AND state = ? AND seq < ?`, key, int(MergeQueued), seq).Scan(&first); err != nil {
		return err
	}
	if !first.Valid {
		return nil
	}
	return moveAhead(ctx, tx, key, id, first.Int64)
}

// MoveMerge moves a merge WAITING in line one place up or down its queue,
// swapping it with the nearest waiting merge on that side. It answers what it
// did: moved, or why the merge stays where it is (MergeMoveOutcome), never a
// bare "did not move".
//
// ONLY WAITING MERGES TRADE PLACES. The admitted merge holds the repository's
// one slot and is never passed, and a merge only REQUESTED is in nobody's line
// (it takes its place when it is queued), so neither is the neighbor a move
// swaps with, and neither moves itself. A merge not waiting stays
// (MergeMoveNotWaiting); one with no waiting merge on that side stays too,
// blocked by the admitted entry directly on that side when one stands there
// (MergeMoveBlockedByAdmitted), and at the edge of the line otherwise
// (MergeMoveAtEdge).
//
// THE MOVED MERGE TAKES THE PASSED MERGE'S FRONT MARK, so a move can never
// put an ordinary merge ahead of a front merge or a front merge behind an
// ordinary one: every front merge stays ahead of every ordinary merge waiting,
// which is the order placeInLine puts a newly queued front merge into.
//
// A workspace with no queue entry is refused.
func (s *store) MoveMerge(ctx context.Context, repo RepoKey, id WorkspaceID, move MergeMove) (MergeMoveOutcome, error) {
	const op = "daemon.wsm.move_merge"
	fields := dlog.Context{"repo": string(repo), "workspace": string(id), "move": move.String()}
	if !move.valid() {
		err := fmt.Errorf("wsm: undeclared merge move %d", int(move))
		s.log.Error(op, "refused an undeclared merge move", withError(fields, err))
		return MergeMoveOutcome{}, err
	}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return MergeMoveOutcome{}, err
	}
	fields["repo"] = string(key)
	var outcome MergeMoveOutcome
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		seq, state, err := entryPlace(ctx, tx, key, id)
		if err != nil {
			return err
		}
		if state != MergeQueued {
			outcome = MergeMoveOutcome{Result: MergeMoveNotWaiting}
			return nil
		}
		side, order := `seq < ?`, `ORDER BY seq DESC LIMIT 1`
		if move == MergeMoveDown {
			side, order = `seq > ?`, `ORDER BY seq LIMIT 1`
		}
		var neighbor int64
		var neighborFront bool
		err = tx.QueryRowContext(ctx, `SELECT seq, source_front FROM merge_queue WHERE repo_key = ? AND state = ? AND `+side+` `+order,
			key, int(MergeQueued), seq).Scan(&neighbor, &neighborFront)
		if errors.Is(err, sql.ErrNoRows) {
			outcome, err = unmovedOutcome(ctx, tx, key, seq, side, order)
			return err
		}
		if err != nil {
			return err
		}
		outcome = MergeMoveOutcome{Result: MergeMoved}
		return swapPlaces(ctx, tx, key, id, seq, neighbor, neighborFront)
	})
	if err != nil {
		return MergeMoveOutcome{}, err
	}
	return outcome, nil
}

// unmovedOutcome answers why a waiting merge at SEQ in KEY's queue, with no
// waiting merge on the side SIDE and ORDER select, stays where it is: the
// entry in line directly on that side, which is then admitted, blocks it;
// with none, it stands at the edge of the line.
func unmovedOutcome(ctx context.Context, tx *sql.Tx, key RepoKey, seq int64, side, order string) (MergeMoveOutcome, error) {
	var blocker WorkspaceID
	err := tx.QueryRowContext(ctx, `SELECT workspace_id FROM merge_queue WHERE repo_key = ? AND state = ? AND `+side+` `+order,
		key, int(MergeAdmitted), seq).Scan(&blocker)
	if errors.Is(err, sql.ErrNoRows) {
		return MergeMoveOutcome{Result: MergeMoveAtEdge}, nil
	}
	if err != nil {
		return MergeMoveOutcome{}, err
	}
	return MergeMoveOutcome{Result: MergeMoveBlockedByAdmitted, Blocker: blocker}, nil
}

// MoveMergeTo moves a merge WAITING in line to the place directly ahead of or
// behind ANCHOR, another merge waiting in the same queue. It answers whether
// the merge moved.
//
// ONLY WAITING MERGES CHANGE PLACES, as MoveMerge's do: the waiting merges are
// reordered among the places they already hold, so the admitted merge and a
// merge only requested keep theirs and are never passed. A merge or an anchor
// not waiting, or a merge already standing in that place, is left where it is.
//
// THE MOVED MERGE TAKES THE ANCHOR'S FRONT MARK, so every front merge stays
// ahead of every ordinary merge waiting: a place directly beside a front merge
// is a front place, and one directly beside an ordinary merge an ordinary one.
//
// A workspace or an anchor with no entry on this queue is refused.
func (s *store) MoveMergeTo(ctx context.Context, repo RepoKey, id WorkspaceID, anchor WorkspaceID, side MergeSide) (bool, error) {
	const op = "daemon.wsm.move_merge_to"
	fields := dlog.Context{"repo": string(repo), "workspace": string(id), "anchor": string(anchor), "side": side.String()}
	if !side.valid() {
		err := fmt.Errorf("wsm: undeclared merge side %d", int(side))
		s.log.Error(op, "refused an undeclared merge side", withError(fields, err))
		return false, err
	}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return false, err
	}
	fields["repo"] = string(key)
	var moved bool
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		_, state, err := entryPlace(ctx, tx, key, id)
		if err != nil {
			return err
		}
		_, anchorState, err := entryPlace(ctx, tx, key, anchor)
		if err != nil || state != MergeQueued || anchorState != MergeQueued || id == anchor {
			return err
		}
		waiting, err := waitingInLine(ctx, tx, key)
		if err != nil {
			return err
		}
		order, anchorFront := placedBeside(waiting, id, anchor, side)
		for i, row := range order {
			if row.workspace != waiting[i].workspace {
				moved = true
				break
			}
		}
		if !moved {
			return nil
		}
		places := make([]int64, len(waiting))
		workspaces := make([]WorkspaceID, len(order))
		for i := range waiting {
			places[i], workspaces[i] = waiting[i].seq, order[i].workspace
		}
		if err := assignQueuePlaces(ctx, tx, key, places, workspaces); err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx, `UPDATE merge_queue SET source_front = ? WHERE repo_key = ? AND workspace_id = ?`, anchorFront, key, id)
		return err
	})
	if err != nil {
		return false, err
	}
	return moved, nil
}

// assignQueuePlaces puts ORDER[i] on PLACES[i] in KEY's queue, for every i: the
// places are the ones those merges already hold between them, so the move
// is a reordering among them and no other merge's place changes. It is the
// one place a queue reorders its merges.
//
// THE PLACES GO THROUGH NEGATIVE SEQUENCES, so the unique (repo_key, seq)
// index never sees two rows on one place mid-transaction: each row that
// changes place parks on the negative of its new place, and every parked row
// then comes back positive.
func assignQueuePlaces(ctx context.Context, tx *sql.Tx, key RepoKey, places []int64, order []WorkspaceID) error {
	if len(places) != len(order) {
		panic(fmt.Sprintf("wsm: %d merges put on %d places", len(order), len(places)))
	}
	for i, ws := range order {
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET seq = ? WHERE repo_key = ? AND workspace_id = ? AND seq != ?`,
			-places[i], key, ws, places[i]); err != nil {
			return err
		}
	}
	_, err := tx.ExecContext(ctx, `UPDATE merge_queue SET seq = -seq WHERE repo_key = ? AND seq < 0`, key)
	return err
}

// waitingRow is one merge waiting in line: its workspace, its place and its
// front mark.
type waitingRow struct {
	workspace WorkspaceID
	seq       int64
	front     bool
}

// waitingInLine answers every merge waiting in KEY's queue, front first.
func waitingInLine(ctx context.Context, tx *sql.Tx, key RepoKey) ([]waitingRow, error) {
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, seq, source_front FROM merge_queue WHERE repo_key = ? AND state = ? ORDER BY seq`, key, int(MergeQueued))
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []waitingRow
	for rows.Next() {
		var row waitingRow
		if err := rows.Scan(&row.workspace, &row.seq, &row.front); err != nil {
			return nil, err
		}
		out = append(out, row)
	}
	return out, rows.Err()
}

// placedBeside answers WAITING reordered with ID taken out and put on SIDE of
// ANCHOR, and ANCHOR's front mark. Both are in WAITING: the caller read their
// states as waiting in the same transaction.
func placedBeside(waiting []waitingRow, id, anchor WorkspaceID, side MergeSide) ([]waitingRow, bool) {
	var moved waitingRow
	rest := make([]waitingRow, 0, len(waiting))
	for _, row := range waiting {
		if row.workspace == id {
			moved = row
			continue
		}
		rest = append(rest, row)
	}
	order := make([]waitingRow, 0, len(waiting))
	anchorFront := false
	for _, row := range rest {
		if row.workspace != anchor {
			order = append(order, row)
			continue
		}
		anchorFront = row.front
		if side == MergeSideAheadOf {
			order = append(order, moved, row)
		} else {
			order = append(order, row, moved)
		}
	}
	if len(order) != len(waiting) {
		panic(fmt.Sprintf("wsm: placing %s beside %s lost a waiting merge: %d of %d placed", id, anchor, len(order), len(waiting)))
	}
	return order, anchorFront
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
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return false, err
	}
	fields["repo"] = string(key)
	var moved bool
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
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
// other, giving ID's row FRONT as its front mark.
func swapPlaces(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, own, other int64, front bool) error {
	var neighbor WorkspaceID
	if err := tx.QueryRowContext(ctx, `SELECT workspace_id FROM merge_queue WHERE repo_key = ? AND seq = ?`, key, other).Scan(&neighbor); err != nil {
		return err
	}
	if err := assignQueuePlaces(ctx, tx, key, []int64{own, other}, []WorkspaceID{neighbor, id}); err != nil {
		return err
	}
	_, err := tx.ExecContext(ctx, `UPDATE merge_queue SET source_front = ? WHERE repo_key = ? AND workspace_id = ?`, front, key, id)
	return err
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
	if err := tx.QueryRowContext(ctx, `SELECT count(*) FROM merge_queue WHERE repo_key = ? AND seq < ? AND state NOT IN (?, ?)`,
		key, seq, int(MergeRequested), int(MergeRemediating)).Scan(&ahead); err != nil {
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

// setMergeQueueState is the one state-write path for a queue entry, moving it
// from FROM to STATE. An entry that is not in the queue is refused
// (ErrNotFound) rather than affecting no rows, and one in any state but FROM
// is refused rather than overwritten: a write that met an entry in another
// state (a remediating merge admitted) would silently pull it back into line.
// ALSO runs in the same transaction once the entry is written.
func (s *store) setMergeQueueState(ctx context.Context, op string, repo RepoKey, id WorkspaceID, from, state MergeQueueState, fields dlog.Context, also func(context.Context, *sql.Tx, RepoKey) error) error {
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	fields["repo"] = string(key)
	fields["workspace"] = string(id)
	fields["state"] = state.String()
	fields["from"] = from.String()
	if !state.valid() {
		err := fmt.Errorf("wsm: undeclared merge queue state %d", int(state))
		s.log.Error(op, "refused an undeclared merge queue state", withError(fields, err))
		return err
	}
	return s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		_, prior, err := entryPlace(ctx, tx, key, id)
		if err != nil {
			return err
		}
		if prior != from {
			return fmt.Errorf("wsm: workspace %s in repo %q moves to %s while %s, not %s", id, key, state, prior, from)
		}
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET state = ? WHERE repo_key = ? AND workspace_id = ?`, int(state), key, id); err != nil {
			return err
		}
		// AN ADMITTED MERGE STANDS AHEAD OF EVERY MERGE WAITING (admittedAhead).
		if state == MergeAdmitted {
			if err := admittedAhead(ctx, tx, key, id); err != nil {
				return err
			}
		}
		return also(ctx, tx, key)
	})
}

// StepAsideMerge takes the merge ID out of REPO's line for ASIDE's repair
// (MergeRemediating): it goes to the back of the sequence, where it passes
// nobody, and keeps whatever lease and progress record it holds. It answers
// the slot the write gave it: the repair's slot when nobody held it, or the
// place awaiting it behind every merge that stepped aside for that repair
// before it (mergeSlotsOf). A merge steps aside from the head, admitted, for
// tests or conflicts, and from its waiting place, queued, when its
// background rebase conflicted (MergeAsideWaitingConflicts); anything else is
// a refusal.
func (s *store) StepAsideMerge(ctx context.Context, repo RepoKey, id WorkspaceID, aside MergeAside) (MergeSlot, error) {
	const op = "daemon.wsm.step_aside_merge"
	fields := dlog.Context{"repo": string(repo), "workspace": string(id), "aside": aside.String()}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return MergeSlotNone, err
	}
	fields["repo"] = string(key)
	from := MergeAdmitted
	switch aside {
	case MergeAsideTests, MergeAsideConflicts:
	case MergeAsideWaitingConflicts:
		from = MergeQueued
	default:
		err := fmt.Errorf("wsm: workspace %s in repo %q steps aside for %s, which names no repair", id, key, aside)
		s.log.Error(op, "refused a step aside that names no repair", withError(fields, err))
		return MergeSlotNone, err
	}
	var slot MergeSlot
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		_, state, err := entryPlace(ctx, tx, key, id)
		if err != nil {
			return err
		}
		if state != from {
			return fmt.Errorf("wsm: workspace %s in repo %q steps aside for %s while %s, not %s", id, key, aside, state, from)
		}
		back, err := nextSeq(ctx, tx, key)
		if err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx, `UPDATE merge_queue SET state = ?, aside = ?, seq = ? WHERE repo_key = ? AND workspace_id = ?`,
			int(MergeRemediating), int(aside), back, key, id)
		return err
	}, func(ctx context.Context, tx *sql.Tx) error {
		var stamped int64
		if err := tx.QueryRowContext(ctx, `SELECT slot FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).Scan(&stamped); err != nil {
			return err
		}
		slot = MergeSlot(stamped)
		return nil
	})
	return slot, err
}

// ReturnRemediatedMerge puts the remediating merge ID back in REPO's line as
// an ORDINARY WAITING MERGE at the FIRST WAITING PLACE: ahead of every merge
// waiting in line, behind every merge being worked on, which it never evicts
// (owner rulings, 2026-10-31 and 2026-11-20). With no merge waiting it stays
// at the back, which is then the first place after whatever runs. Its repair
// slot is released in the same write, and granted to the next merge awaiting
// it (mergeSlotsOf). Anything but a remediating entry is a refusal. It
// answers where the merge now stands.
//
// THE PLACE IS AN ORDINARY POSITION, HELD ON NO LEASE (owner ruling,
// 2026-11-20): the return writes the merge's sequence once, and nothing
// remembers it was a returner. A front placement, a hotfix, a later returner
// and every move the user makes pass it as they pass any waiting merge, and
// the order the queue runs is the order it reports, both read off the one
// sequence. The merge takes the front mark of the merge it passed
// (takeFirstWaitingPlace), so every front merge still stands ahead of every
// ordinary one.
//
// A MERGE THAT STEPPED ASIDE FROM THE HEAD (MergeAsideTests,
// MergeAsideConflicts) RETURNS WAITING, keeping its progress record and
// dropping LEASE, its merge lease, in the same write: a waiting merge owns no
// lease, and the record is what the merge resumes from once it heads the
// queue and is admitted under its ledger again (the record's lease is that
// ledger). One that stepped aside from its waiting place
// (MergeAsideWaitingConflicts) holds no lease and no record, and LEASE is
// empty. A lease given where none is held, or none given where one is, is
// refused and changes nothing.
//
// A MERGE BACK FROM RESOLVING CONFLICTS STANDS BEHIND every merge back from
// fixing its tests that is first in the waiting line AT THAT MOMENT, as the
// ranks it replaces put it (2026-11-13: a later conflict return was put ahead
// of a merge that had fixed its tests). That is decided from those merges'
// own records, once, here; it holds them no place afterwards.
//
// THE RETURN LEAVES THE RESOLVED REBASE AS IT STANDS: the rebase the
// merge's own conflict repair works on is recorded when its turn is handed
// over (RecordResolvedRebase), so the return neither writes nor clears it.
//
// WORKED ON IS THE CALLER'S WORD, not the entries' states: a merge the
// orchestrator has taken up (its chain's member joining behind the head) is
// still waiting in the store until its admission is written, and the return
// that read it as waiting stood ahead of a merge already being worked on.
func (s *store) ReturnRemediatedMerge(ctx context.Context, repo RepoKey, id WorkspaceID, lease LeaseID, workedOn []WorkspaceID) (MergeReturn, error) {
	const op = "daemon.wsm.return_remediated_merge"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return MergeReturn{}, err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "lease": string(lease)}
	var placed MergeReturn
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		_, state, err := entryPlace(ctx, tx, key, id)
		if err != nil {
			return err
		}
		if state != MergeRemediating {
			return fmt.Errorf("wsm: workspace %s in repo %q returns from remediating while %s", id, key, state)
		}
		var aside int64
		if err := tx.QueryRowContext(ctx, `SELECT aside FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).Scan(&aside); err != nil {
			return err
		}
		reason := MergeAside(aside)
		switch reason {
		case MergeAsideTests, MergeAsideConflicts:
			if lease == "" {
				return fmt.Errorf("wsm: workspace %s in repo %q returns from %s with no merge lease to drop", id, key, reason)
			}
			if err := releaseMergeLease(ctx, tx, id, lease); err != nil {
				return err
			}
		case MergeAsideWaitingConflicts:
			if lease != "" {
				return fmt.Errorf("wsm: workspace %s in repo %q returns from %s, which holds no lease, with lease %s", id, key, reason, lease)
			}
		default:
			return fmt.Errorf("wsm: workspace %s in repo %q returns from remediating for %s, which names no repair", id, key, reason)
		}
		back, err := nextSeq(ctx, tx, key)
		if err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET state = ?, aside = ?, seq = ?, source_front = 0
WHERE repo_key = ? AND workspace_id = ?`,
			int(MergeQueued), int(MergeAsideNone), back, key, id); err != nil {
			return err
		}
		if err := takeFirstWaitingPlace(ctx, tx, key, id, reason != MergeAsideTests, workedOn); err != nil {
			return err
		}
		placed, err = returnPlaceOf(ctx, tx, key, id)
		placed.Lease = lease
		return err
	})
	if err != nil {
		return MergeReturn{}, err
	}
	if lease != "" {
		s.disown(lease)
	}
	return placed, nil
}

// MergeReturn is where ReturnRemediatedMerge put a merge back in line.
type MergeReturn struct {
	// Position is the merge's one-based place among the entries in line.
	Position int
	// Behind is the merge directly ahead of it in line, empty when it heads.
	Behind WorkspaceID
	// Lease is the merge lease the return dropped, empty when it held none.
	Lease LeaseID
}

// takeFirstWaitingPlace moves ID, standing at the back of KEY's queue, to the
// first waiting place: the place of the first merge waiting in line that the
// caller does not name WORKED ON, taking that merge's front mark. BEHINDFIXERS
// passes over the merges at the front of the waiting line whose own record
// says they are back from fixing their tests (a waiting merge carries a
// record only while it is back in line from its repair, at the step it
// repaired), so a merge back from resolving conflicts stands behind them. No
// such merge leaves ID at the back, ordinary.
func takeFirstWaitingPlace(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, behindFixers bool, workedOn []WorkspaceID) error {
	query := `SELECT q.seq, q.source_front, COALESCE(json_extract(p.document, '$.step'), '') FROM merge_queue AS q
		LEFT JOIN merge_progress AS p ON p.workspace_id = q.workspace_id
		WHERE q.repo_key = ? AND q.workspace_id <> ? AND q.state = ?`
	args := []any{key, id, int(MergeQueued)}
	if len(workedOn) > 0 {
		query += ` AND q.workspace_id NOT IN (?` + strings.Repeat(`, ?`, len(workedOn)-1) + `)`
		for _, ws := range workedOn {
			args = append(args, ws)
		}
	}
	rows, err := tx.QueryContext(ctx, query+` ORDER BY q.seq`, args...)
	if err != nil {
		return err
	}
	var (
		target int64
		front  bool
		found  bool
	)
	for rows.Next() {
		var (
			seq  int64
			mark bool
			step string
		)
		if err := rows.Scan(&seq, &mark, &step); err != nil {
			rows.Close()
			return err
		}
		if behindFixers && step == MergeStepFixes {
			continue
		}
		target, front, found = seq, mark, true
		break
	}
	if err := rows.Close(); err != nil {
		return err
	}
	if !found {
		return nil
	}
	if err := moveAhead(ctx, tx, key, id, target); err != nil {
		return err
	}
	_, err = tx.ExecContext(ctx, `UPDATE merge_queue SET source_front = ? WHERE repo_key = ? AND workspace_id = ?`, front, key, id)
	return err
}

// MergeStepFixes is the step a merge's progress record names while it fixes
// its tests and once it is back in line from doing so. It is THE ONE
// SPELLING: the merge package's TabFixes is this constant, so the record the
// merge package writes and the store's read of it (takeFirstWaitingPlace)
// cannot drift apart. The layout-126 step reads the same spelling, as
// shipped.
const MergeStepFixes = "fixes"

// returnPlaceOf answers where ID stands in KEY's line: its one-based place
// among the entries in line, and the entry in line directly ahead of it.
func returnPlaceOf(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID) (MergeReturn, error) {
	seq, _, err := entryPlace(ctx, tx, key, id)
	if err != nil {
		return MergeReturn{}, err
	}
	position, err := inLinePositionOf(ctx, tx, key, seq)
	if err != nil {
		return MergeReturn{}, err
	}
	var behind sql.NullString
	err = tx.QueryRowContext(ctx, `SELECT workspace_id FROM merge_queue WHERE repo_key = ? AND seq < ? AND state NOT IN (?, ?) ORDER BY seq DESC LIMIT 1`,
		key, seq, int(MergeRequested), int(MergeRemediating)).Scan(&behind)
	if err != nil && !errors.Is(err, sql.ErrNoRows) {
		return MergeReturn{}, err
	}
	return MergeReturn{Position: position, Behind: WorkspaceID(behind.String)}, nil
}

// AdmitMerge marks the entry the orchestrator is running now, moves it ahead
// of every merge it was admitted past (admittedAhead), and marks its
// repository as owing its queue_drained scripts IN THE SAME WRITE, so no
// admitted merge leaves the repository without the debt. Only a waiting
// (queued) entry is admitted; any other state is a refusal, as StepAsideMerge
// and ReturnRemediatedMerge refuse a wrong prior state.
func (s *store) AdmitMerge(ctx context.Context, repo RepoKey, id WorkspaceID) error {
	return s.setMergeQueueState(ctx, "daemon.wsm.admit_merge", repo, id, MergeQueued, MergeAdmitted, dlog.Context{}, oweQueueDrain)
}

// ReturnAdmittedMerge puts an ADMITTED merge that has no run back in line AT
// ITS OWN PLACE, waiting, under LEDGER: a merge a restart interrupted before it
// took a step runs again from where it stood -- the head of the line -- never
// from the back. Its source, its front mark and its place are kept. An entry
// that is not admitted is refused: the caller lost track of the queue.
func (s *store) ReturnAdmittedMerge(ctx context.Context, repo RepoKey, id WorkspaceID, ledger LeaseID) error {
	const op = "daemon.wsm.return_admitted_merge"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "ledger": string(ledger)}
	if ledger == "" {
		err := errors.New("wsm: a merge is put back in line with its bubble's ledger identity")
		s.log.Error(op, "refused to return a merge to its line with no ledger identity", withError(fields, err))
		return err
	}
	return s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		// THE ACTIVE STRETCH IS HELD ACROSS THE RESTART (MergeClock.Held): the
		// merge goes back to its place only because the daemon restarted.
		res, err := tx.ExecContext(ctx, `UPDATE merge_queue SET state = ?, ledger_id = ?, clock_held = 1 WHERE repo_key = ? AND workspace_id = ? AND state = ?`,
			int(MergeQueued), ledger, key, id, int(MergeAdmitted))
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: admitted merge of workspace %s in repo %q", id, key))
	})
}

// RemoveMergeQueueEntry takes one entry off its queue, recording the cause
// it left for in the operation's log record. Removing an entry that is not
// queued is a refusal, never a no-op: it means the caller lost track of the
// queue.
//
// THE EXIT GOES THROUGH THE ONE TRANSITION (leaveQueueInTx): a landing ends
// the request, and every other cause keeps it off the queue, to be requeued
// or discarded. No exit closes the workspace: an async merge's merge
// workspace stays open under its name while its request stands.
func (s *store) RemoveMergeQueueEntry(ctx context.Context, repo RepoKey, id WorkspaceID, cause string) error {
	const op = "daemon.wsm.remove_merge_queue_entry"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "cause": cause}
	var (
		settled int64
		kept    bool
	)
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		settled, kept, err = leaveQueueInTx(ctx, tx, key, id, cause, s.now(), "")
		if err != nil {
			return err
		}
		// A RECORD NO LEASE HOLDS LEAVES WITH ITS ENTRY: it is the record of
		// a merge back in line from its repair (ReturnRemediatedMerge), which
		// holds no lease while it waits, and nothing resumes it once its
		// entry is gone. A record a lease still holds is an admitted merge's,
		// which its lease's own end drops.
		_, err = tx.ExecContext(ctx, `DELETE FROM merge_progress WHERE workspace_id = ? AND lease_id NOT IN (SELECT id FROM leases)`, id)
		return err
	})
	if err != nil {
		return err
	}
	s.logLeftQueue(op, fields, settled, kept)
	return nil
}

// settleWaitsOn records, in the transaction that removes ID's entry, the cause
// it left for on every entry of KEY's queue still waiting on it, and answers
// how many it settled: MergeLeftLanded releases them, any other cause fails
// them (MergeQueueEntry.AheadFailed). A merge taken out only to be put
// straight back (MergeLeftToReturn) has not left, and settles nothing.
//
// THE WAIT IS SETTLED WHERE THE MERGE AHEAD LEAVES, in one write, so no reader
// ever sees the merge ahead gone while a merge behind it still waits on it,
// and a restart reads back exactly what the removal decided.
func settleWaitsOn(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, cause string) (int64, error) {
	if cause == MergeLeftToReturn {
		return 0, nil
	}
	if cause == "" {
		return 0, fmt.Errorf("wsm: the merge of workspace %s in repo %q left its queue with no cause to settle the merges behind it by", id, key)
	}
	// A REQUEST NOT YET HANDED OFF waits too (async_merge_requests): it is
	// settled in the same write, and carries the settlement onto its merge
	// workspace at the hand-off.
	var settled int64
	for _, stmt := range []string{
		`UPDATE merge_queue SET waits_on_left = ? WHERE repo_key = ? AND waits_on = ? AND waits_on_left IS NULL`,
		`UPDATE async_merge_requests SET waits_on_left = ? WHERE repo_key = ? AND waits_on = ? AND waits_on_left IS NULL`,
	} {
		res, err := tx.ExecContext(ctx, stmt, cause, key, id)
		if err != nil {
			return 0, err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return 0, err
		}
		settled += n
	}
	return settled, nil
}

// logSettledWaits records the waits a committed removal settled, if any.
func (s *store) logSettledWaits(op string, fields dlog.Context, settled int64) {
	if settled == 0 {
		return
	}
	s.log.Info(op, "the merge left its queue; the merges placed behind it are settled by the cause it left for",
		dlog.Merge(fields, dlog.Context{"settled": settled}))
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
//
// The entry leaves through the one transition (leaveQueueInTx), as for
// RemoveMergeQueueEntry.
func (s *store) DropAdmittedMerge(ctx context.Context, repo RepoKey, id WorkspaceID, lease LeaseID, cause string) error {
	const op = "daemon.wsm.drop_admitted_merge"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "lease": string(lease), "cause": cause}
	var (
		settled int64
		kept    bool
	)
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		// A REMEDIATING MERGE IS ADMITTED WORK OUT OF LINE: it holds its lease
		// and its progress record just as an admitted one does, and ends the
		// same way.
		settled, kept, err = leaveQueueInTx(ctx, tx, key, id, cause, s.now(), ` AND state IN (?, ?)`, int(MergeAdmitted), int(MergeRemediating))
		if err != nil {
			return err
		}
		return dropMergeLease(ctx, tx, id, lease)
	})
	if err != nil {
		return err
	}
	s.disown(lease)
	s.logLeftQueue(op, fields, settled, kept)
	return nil
}

// RequeuePausedMerge puts the ADMITTED merge of workspace ID back in line at
// its own place, waiting under LEDGER, and drops LEASE, its merge lease, with
// that lease's progress record, in one write: the merge a handover paused on
// a daemon that keeps its workspace until the workspace's work ends starts
// again from the queue once the workspace is served by the daemon that
// serves the queue, and holds nobody behind it meanwhile. An entry that is
// not admitted, or a lease that is not ID's merge lease, is refused and
// changes nothing.
func (s *store) RequeuePausedMerge(ctx context.Context, repo RepoKey, id WorkspaceID, lease LeaseID, ledger LeaseID) error {
	const op = "daemon.wsm.requeue_paused_merge"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo), "workspace": string(id)})
	if err != nil {
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "lease": string(lease), "ledger": string(ledger)}
	if ledger == "" {
		err := errors.New("wsm: a paused merge is put back in line with its bubble's ledger identity")
		s.log.Error(op, "refused to put a paused merge back in line with no ledger identity", withError(fields, err))
		return err
	}
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		// THE ACTIVE STRETCH IS HELD ACROSS THE RESTART (MergeClock.Held): the
		// merge goes back to its place only because the daemon restarted.
		res, err := tx.ExecContext(ctx, `UPDATE merge_queue SET state = ?, ledger_id = ?, clock_held = 1 WHERE repo_key = ? AND workspace_id = ? AND state = ?`,
			int(MergeQueued), ledger, key, id, int(MergeAdmitted))
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: admitted merge of workspace %s in repo %q", id, key)); err != nil {
			return err
		}
		return dropMergeLease(ctx, tx, id, lease)
	})
	if err != nil {
		return err
	}
	s.disown(lease)
	return nil
}

// dropMergeLease deletes LEASE, workspace ID's merge lease, and that lease's
// progress record in TX: the half of a merge's end that keeps "a merge lease
// stands with exactly one queue entry" when its entry leaves or goes back in
// line (DropAdmittedMerge, RequeueColdGatedMerge). A lease that is not ID's
// merge lease is refused and deletes nothing. The caller
// disowns the lease once TX commits.
func dropMergeLease(ctx context.Context, tx *sql.Tx, id WorkspaceID, lease LeaseID) error {
	if err := releaseMergeLease(ctx, tx, id, lease); err != nil {
		return err
	}
	_, err := tx.ExecContext(ctx, `DELETE FROM merge_progress WHERE lease_id = ?`, lease)
	return err
}

// releaseMergeLease deletes LEASE, workspace ID's merge lease, in TX, keeping
// its progress record: the half of dropMergeLease a merge back in line from
// its repair takes (ReturnRemediatedMerge), since the record is what it
// resumes from. A lease that is not ID's merge lease is refused and deletes
// nothing. The caller disowns the lease once TX commits.
func releaseMergeLease(ctx context.Context, tx *sql.Tx, id WorkspaceID, lease LeaseID) error {
	res, err := tx.ExecContext(ctx, `DELETE FROM leases WHERE id = ? AND workspace_id = ? AND holder = ?`, lease, id, int(HolderMerge))
	if err != nil {
		return err
	}
	return requireOneRow(res, fmt.Sprintf("wsm: merge lease %s of workspace %s", lease, id))
}

// MergeQueue loads one repository's queue in order, all-or-nothing.
func (s *store) MergeQueue(ctx context.Context, repo RepoKey) ([]MergeQueueEntry, error) {
	const op = "daemon.wsm.merge_queue"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
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
const mergeQueueColumns = `repo_key, workspace_id, state, enqueued_at, ledger_id, source_front, source_behind, pick_started_at, pick_turn, slot, slot_since, aside, clock_phase, clock_since, clock_waited, clock_held, waits_on, waits_on_name, waits_on_left, cold_gated, resolved_tip, resolved_branch_head, resolved_turn, ` + requestColumns

// scanMergeQueue is the one queue-decoding path, so both readers reject an
// undeclared state identically and number positions the same way. TAIL is the
// read's own WHERE and ORDER BY; the columns are always mergeQueueColumns. The rows
// arrive in sequence order, so the position is the index within its repository.
func (s *store) scanMergeQueue(ctx context.Context, tail string, args ...any) (map[RepoKey][]MergeQueueEntry, error) {
	rows, err := s.q(ctx).QueryContext(ctx, `SELECT `+mergeQueueColumns+` FROM merge_queue `+tail, args...)
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
			ledger    sql.NullString
			front     bool
			behind    sql.NullString
			picked    sql.NullInt64
			pickTurn  sql.NullString
			slot      int64
			slotSince int64
			aside     int64
			phase     int64
			since     int64
			waited    int64
			held      bool
			waitsOn   sql.NullString
			waitsName sql.NullString
			waitsLeft sql.NullString
			resTip    sql.NullString
			resHead   sql.NullString
			resTurn   sql.NullString
			req       requestScan
		)
		if err := rows.Scan(append([]any{&entry.Repo, &entry.Workspace, &state, &enqueued, &ledger, &front, &behind, &picked, &pickTurn, &slot, &slotSince, &aside, &phase, &since, &waited, &held, &waitsOn, &waitsName, &waitsLeft, &entry.ColdGated, &resTip, &resHead, &resTurn}, req.targets()...)...); err != nil {
			return nil, err
		}
		row := fmt.Sprintf("%s/%s", entry.Repo, entry.Workspace)
		entry.State = MergeQueueState(state)
		if !entry.State.valid() {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "state", Err: fmt.Errorf("unknown merge queue state %d", state)}
		}
		// THE REQUEST DECODES AS IT DOES OFF THE QUEUE (requestScan); the
		// place's columns ride on top of it.
		req.front, req.behind, req.picked, req.pickTurn = front, behind, picked, pickTurn
		source, err := req.source("merge_queue", row)
		if err != nil {
			return nil, err
		}
		entry.Source = source
		entry.EnqueuedAt = fromNanos(enqueued)
		entry.Ledger = LeaseID(ledger.String)
		entry.Slot = MergeSlot(slot)
		if !entry.Slot.valid() {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "slot", Err: fmt.Errorf("unknown merge slot %d", slot)}
		}
		entry.SlotSince = fromNanos(slotSince)
		entry.Aside = MergeAside(aside)
		if !entry.Aside.valid() {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "aside", Err: fmt.Errorf("unknown merge aside %d", aside)}
		}
		entry.Clock = MergeClock{Phase: MergeClockPhase(phase), Waited: time.Duration(waited), Held: held}
		if !entry.Clock.Phase.valid() {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "clock_phase", Err: fmt.Errorf("unknown merge clock phase %d", phase)}
		}
		if since != 0 {
			entry.Clock.Since = fromNanos(since)
		}
		entry.WaitsOn, entry.WaitsOnName, entry.WaitsOnLeft = WorkspaceID(waitsOn.String), waitsName.String, waitsLeft.String
		if entry.WaitsOnLeft != "" && entry.WaitsOn == "" {
			return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "waits_on_left", Err: fmt.Errorf("records the cause %q a merge ahead left for, but waits on no merge", entry.WaitsOnLeft)}
		}
		if entry.Resolved, err = decodeResolvedRebase(row, resTip, resHead, resTurn); err != nil {
			return nil, err
		}
		entry.Position = len(loaded[entry.Repo]) + 1
		loaded[entry.Repo] = append(loaded[entry.Repo], entry)
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return loaded, nil
}

// SetMergeQueuePaused pauses one repository's queue with PAUSE's record, or
// resumes it when PAUSE is nil, clearing the record. A pause written over a
// standing pause replaces its record: a hard stop of a paused queue is a new
// act, by whoever asked for it, at its own instant.
func (s *store) SetMergeQueuePaused(ctx context.Context, repo RepoKey, pause *MergeQueuePause) error {
	const op = "daemon.wsm.set_merge_queue_paused"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
		return err
	}
	fields := dlog.Context{"repo": string(key), "paused": pause != nil}
	var at sql.NullInt64
	var by, name, pauser, cause sql.NullString
	hard, toLand := false, false
	if pause != nil {
		if err := pause.validate(); err != nil {
			s.log.Error(op, "refused a merge queue pause whose record is malformed", withError(fields, err))
			return err
		}
		at = sql.NullInt64{Int64: nanos(pause.At), Valid: true}
		by = sql.NullString{String: pause.By.Kind.column(), Valid: true}
		if pause.By.Kind == MergeQueuePauserWorkspace {
			name = sql.NullString{String: pause.By.Name, Valid: true}
			pauser = sql.NullString{String: string(pause.By.Workspace), Valid: true}
		}
		if pause.By.Kind == MergeQueuePauserHost {
			cause = sql.NullString{String: pause.By.Cause, Valid: true}
		}
		hard, toLand = pause.Hard, pause.ToLand
		fields["by"] = pause.By.Kind.column()
		fields["hard"] = hard
		fields["to_land"] = toLand
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO merge_queue_repos (repo_key, paused, paused_at, paused_by, paused_by_name, paused_by_workspace, paused_hard, paused_to_land, paused_cause) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(repo_key) DO UPDATE SET paused = excluded.paused, paused_at = excluded.paused_at,
			   paused_by = excluded.paused_by, paused_by_name = excluded.paused_by_name,
			   paused_by_workspace = excluded.paused_by_workspace, paused_hard = excluded.paused_hard,
			   paused_to_land = excluded.paused_to_land, paused_cause = excluded.paused_cause`,
			key, pause != nil, at, by, name, pauser, hard, toLand, cause)
		return err
	})
}

// MergeQueuePaused reports whether one repository's queue is paused. A
// repository with no pause row has never been paused, which is not the same
// fact as a corrupt row and is why the absence is reported as false rather than
// refused.
func (s *store) MergeQueuePaused(ctx context.Context, repo RepoKey) (bool, error) {
	pause, err := s.MergeQueuePause(ctx, repo)
	return pause != nil, err
}

// MergeQueuePause answers one repository's standing pause, nil while its
// queue runs. A pause recorded before its record was kept (layout 83) answers
// a zero At and an unrecorded pauser.
func (s *store) MergeQueuePause(ctx context.Context, repo RepoKey) (*MergeQueuePause, error) {
	const op = "daemon.wsm.merge_queue_pause"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
		return nil, err
	}
	var pause *MergeQueuePause
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		var paused, hard, toLand bool
		var at sql.NullInt64
		var by, name, pauser, cause sql.NullString
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT paused, paused_at, paused_by, paused_by_name, paused_by_workspace, paused_hard, paused_to_land, paused_cause FROM merge_queue_repos WHERE repo_key = ?`, key).
			Scan(&paused, &at, &by, &name, &pauser, &hard, &toLand, &cause)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil || !paused {
			return err
		}
		kind, err := mergeQueuePauserKindOf(by)
		if err != nil {
			return &DecodeError{Table: "merge_queue_repos", Row: string(key), Field: "paused_by", Err: err}
		}
		pause = &MergeQueuePause{By: MergeQueuePauser{Kind: kind, Name: name.String}, Hard: hard, ToLand: toLand}
		switch kind {
		case MergeQueuePauserWorkspace:
			pause.By.Workspace = WorkspaceID(pauser.String)
		case MergeQueuePauserHost:
			pause.By.Cause = cause.String
		}
		if at.Valid {
			pause.At = fromNanos(at.Int64)
		}
		return nil
	})
	return pause, err
}

// returnRankDDL is the layout-130 addition: merge_queue.return_rank, the rank
// a merge came back to the line with from repairing out of line. IT IS READ
// BY NOTHING since the owner's ruling of 2026-11-20: a returner is placed by
// its sequence once, at its return, and holds no standing after it
// (ReturnRemediatedMerge). The column stays, written and read by nothing
// here, so the step stays additive; the build before it still writes it.
const returnRankDDL = `
ALTER TABLE merge_queue ADD COLUMN return_rank INTEGER NOT NULL DEFAULT 0;
`

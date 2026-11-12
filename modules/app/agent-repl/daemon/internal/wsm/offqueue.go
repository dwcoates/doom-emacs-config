package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is a merge's REQUEST OFF ITS QUEUE (owner design, 2026-11-12): a
// merge's request lives until it lands or is explicitly discarded, so a merge
// that left its queue without landing can be put back in line AS ITSELF.
//
// THE QUEUE ROW IS THE PLACE, AND ITS REQUEST COLUMNS ARE THE REQUEST. Every
// exit from a queue goes through leaveQueueInTx, the ONE transition: a
// landing deletes the row and the request with it, and every other cause
// moves the row's request columns into off_queue_merges, with the cause and
// the instant, in the same transaction. No exit can delete a request it did
// not land, because no other statement deletes a queue row.
//
// A REQUEST NEVER OUTLIVES ITS WORKSPACE. Closing the workspace (killing or
// nuking it, or the discard of an async merge) is itself an explicit end, so
// markClosedInTx deletes the record in the close's own transaction, and a
// closed workspace's exit records none.
//
// THE RECORD IS KEYED BY WORKSPACE ALONE. A workspace holds one merge, in
// line or off it, and the queue it belongs to is its repository's, so the
// record carries no queue key that could disagree with the workspace's own.

// offQueueMergesDDL is the layout-129 table. Kept apart for the reason
// feedTextScaleDDL is: a fresh file and the 128 -> 129 migration write the one
// text.
const offQueueMergesDDL = `
CREATE TABLE off_queue_merges (
  workspace_id               TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  cause                      TEXT NOT NULL,
  left_at                    INTEGER NOT NULL,
  adopted                    INTEGER NOT NULL,
  enqueued_at                INTEGER NOT NULL,
  ` + requestColumnsDDL + `
);
`

// requestColumnsDDL declares a request's columns on off_queue_merges, named
// and typed exactly as merge_queue's, so one column list (requestColumns)
// moves a request between the two tables in both directions.
const requestColumnsDDL = `source_kind                INTEGER NOT NULL,
  source_keep_open           INTEGER NOT NULL,
  source_workspace           TEXT,
  source_branch              TEXT,
  source_detach_id           TEXT,
  source_detach_name         TEXT,
  source_pick                TEXT,
  source_compact_after       INTEGER NOT NULL,
  source_close_on_landing    TEXT,
  source_cleanup_worktree    INTEGER NOT NULL,
  source_cleanup_dir         TEXT,
  source_version_part        INTEGER,
  source_changelog_line      TEXT,
  source_pre_deploy_hooks    TEXT NOT NULL,
  source_post_deploy_hooks   TEXT NOT NULL,
  source_detach_maker        TEXT,
  source_detach_target_maker INTEGER NOT NULL`

// requestColumns is THE REQUEST: every merge_queue column a merge was asked
// for with, and nothing of its place in line. Its placement (source_front,
// source_behind, the wait) and a pick in flight (pick_started_at, pick_turn)
// are the place's: the exit ended them, and a requeue names its own.
const requestColumns = `source_kind, source_keep_open, source_workspace, source_branch, source_detach_id, source_detach_name, source_pick, source_compact_after, source_close_on_landing, source_cleanup_worktree, source_cleanup_dir, source_version_part, source_changelog_line, source_pre_deploy_hooks, source_post_deploy_hooks, source_detach_maker, source_detach_target_maker`

// adoptOffQueueMergesDML adopts every merge workspace left open by an exit
// from before requests outlived their place: open, a merge workspace
// (merge_of set), with no queue row, no record, and no async request still
// standing for it (a hand-off a crash interrupted, which the next hand-off
// finishes). Its request's values were never kept, so it is adopted as the
// own-branch merge of its branch with none (adopted = 1).
const adoptOffQueueMergesDML = `
INSERT INTO off_queue_merges (workspace_id, cause, left_at, adopted, enqueued_at, ` + requestColumns + `)
SELECT w.id, '` + MergeLeftAdopted + `', CAST(strftime('%s', 'now') AS INTEGER) * 1000000000, 1, w.created_at,
       0, 0, NULL, w.branch, NULL, NULL, NULL, 0, NULL, 0, NULL, NULL, NULL, '[]', '[]', NULL, 0
  FROM workspaces w
 WHERE w.merge_of IS NOT NULL AND w.closed = 0
   AND NOT EXISTS (SELECT 1 FROM merge_queue q WHERE q.workspace_id = w.id)
   AND NOT EXISTS (SELECT 1 FROM off_queue_merges o WHERE o.workspace_id = w.id)
   AND NOT EXISTS (SELECT 1 FROM async_merge_requests r WHERE r.request_id = w.merge_request_id);
`

// MergeLeftAdopted is the cause an adopted record carries: a merge workspace
// found open with no request standing, its request's values lost.
const MergeLeftAdopted = "adopted"

// OffQueueMerge is one merge's request standing off its queue.
type OffQueueMerge struct {
	// Workspace is the merge's workspace: the one landing its own branch, or
	// an async merge's merge workspace.
	Workspace WorkspaceID
	// Cause is why it left its queue (merge.AbandonCause and the queue's
	// other causes, or MergeLeftAdopted).
	Cause string
	// LeftAt is when it left.
	LeftAt time.Time
	// RequestedAt is when the request was first put on its queue.
	RequestedAt time.Time
	// Adopted reports that the record was adopted with no request values: it
	// records no version increment, changelog line or hooks of its own.
	Adopted bool
	// Source is the request, with no placement: Front, Behind and the pick's
	// progress are unset.
	Source MergeSource
}

// leaveQueueInTx is THE ONE TRANSITION a queue row leaves its queue through,
// in the exit's own transaction: a cause that ends the request (EndsRequest:
// a landing, the workspace's close) deletes the row and its request; every
// other cause moves the request off the queue first,
// with CAUSE and AT, unless the workspace is closed (a closed workspace holds
// no request). WHERE narrows the row (the caller's state guard) and must match
// exactly one row, or nothing changes. It settles the merges waiting on it
// (settleWaitsOn) and answers how many it settled and whether it kept the
// request.
func leaveQueueInTx(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID, cause string, at time.Time, where string, args ...any) (int64, bool, error) {
	if cause == "" {
		return 0, false, fmt.Errorf("wsm: the merge of workspace %s in repo %q left its queue with no cause", id, key)
	}
	keeps := !EndsRequest(cause)
	if keeps {
		var closed bool
		if err := scanWorkspaceRow(ctx, tx, id, "closed", &closed); err != nil {
			return 0, false, err
		}
		keeps = !closed
	}
	if keeps {
		res, err := tx.ExecContext(ctx,
			`INSERT INTO off_queue_merges (workspace_id, cause, left_at, adopted, enqueued_at, `+requestColumns+`)
			 SELECT workspace_id, ?, ?, 0, enqueued_at, `+requestColumns+` FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`+where,
			append([]any{cause, nanos(at), key, id}, args...)...)
		if err != nil {
			return 0, false, err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: merge queue entry for workspace %s in repo %q", id, key)); err != nil {
			return 0, false, err
		}
	}
	res, err := tx.ExecContext(ctx, `DELETE FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`+where, append([]any{key, id}, args...)...)
	if err != nil {
		return 0, false, err
	}
	if err := requireOneRow(res, fmt.Sprintf("wsm: merge queue entry for workspace %s in repo %q", id, key)); err != nil {
		return 0, false, err
	}
	settled, err := settleWaitsOn(ctx, tx, key, id, cause)
	if err != nil {
		return 0, false, err
	}
	return settled, keeps, nil
}

// logLeftQueue records a committed exit's settled waits and whether its
// request was kept off the queue.
func (s *store) logLeftQueue(op string, fields dlog.Context, settled int64, kept bool) {
	s.logSettledWaits(op, fields, settled)
	if kept {
		s.log.Info(op, "the merge left its queue without landing; its request stands off the queue until it is requeued or discarded", fields)
	}
}

// OffQueueMerges loads every request standing off its queue, oldest
// departure first, all-or-nothing: one corrupt record loads none.
func (s *store) OffQueueMerges(ctx context.Context) ([]OffQueueMerge, error) {
	var out []OffQueueMerge
	err := s.read(ctx, "daemon.wsm.off_queue_merges", dlog.Context{}, func(ctx context.Context) error {
		loaded, err := scanOffQueue(ctx, s.q(ctx), `ORDER BY left_at, workspace_id`)
		out = loaded
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// OffQueueMergeOf answers workspace ID's request off its queue; the bool is
// false when it holds none.
func (s *store) OffQueueMergeOf(ctx context.Context, id WorkspaceID) (OffQueueMerge, bool, error) {
	var (
		out   OffQueueMerge
		found bool
	)
	err := s.read(ctx, "daemon.wsm.off_queue_merge_of", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		loaded, err := scanOffQueue(ctx, s.q(ctx), `WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		if len(loaded) == 1 {
			out, found = loaded[0], true
		}
		return nil
	})
	if err != nil {
		return OffQueueMerge{}, false, err
	}
	return out, found, nil
}

// scanOffQueue is the one off_queue_merges decoding path, all-or-nothing.
func scanOffQueue(ctx context.Context, q querier, tail string, args ...any) ([]OffQueueMerge, error) {
	query := `SELECT workspace_id, cause, left_at, adopted, enqueued_at, ` + requestColumns + ` FROM off_queue_merges ` + tail
	return listRows(ctx, q, query, func(row interface{ Scan(...any) error }) (OffQueueMerge, error) {
		var (
			rec      OffQueueMerge
			left     int64
			enqueued int64
			req      requestScan
		)
		if err := row.Scan(append([]any{&rec.Workspace, &rec.Cause, &left, &rec.Adopted, &enqueued}, req.targets()...)...); err != nil {
			return OffQueueMerge{}, err
		}
		source, err := req.source("off_queue_merges", string(rec.Workspace))
		if err != nil {
			return OffQueueMerge{}, err
		}
		rec.Source, rec.LeftAt, rec.RequestedAt = source, fromNanos(left), fromNanos(enqueued)
		return rec, nil
	}, args...)
}

// requestScan is THE ONE DECODING of a request's columns (requestColumns),
// shared by the queue's reader (scanMergeQueue) and the off-queue reader
// (scanOffQueue), so a request reads back identically wherever it stands.
type requestScan struct {
	kind     int64
	keepOpen bool
	sourceWS sql.NullString
	branch   sql.NullString
	detach   sql.NullString
	name     sql.NullString
	pick     sql.NullString
	compact  bool
	closes   sql.NullString
	cleanup  bool
	cleanDir sql.NullString
	verPart  sql.NullInt64
	verLine  sql.NullString
	before   string
	after    string
	maker    sql.NullString
	toMaker  bool
	// The PLACE's columns a queue row carries beside its request: set by
	// scanMergeQueue before source, and zero off the queue, where the exit
	// ended them.
	front    bool
	behind   sql.NullString
	picked   sql.NullInt64
	pickTurn sql.NullString
}

// targets are the scan destinations, in requestColumns order.
func (r *requestScan) targets() []any {
	return []any{&r.kind, &r.keepOpen, &r.sourceWS, &r.branch, &r.detach, &r.name, &r.pick, &r.compact, &r.closes, &r.cleanup, &r.cleanDir, &r.verPart, &r.verLine, &r.before, &r.after, &r.maker, &r.toMaker}
}

// source builds the request's MergeSource with the place's columns (none off
// the queue), refusing a version, hooks or a source that will not decode. ROW
// names the row in a DecodeError.
func (r *requestScan) source(table, row string) (MergeSource, error) {
	src := MergeSource{
		Kind: MergeSourceKind(r.kind), KeepOpen: r.keepOpen, Workspace: WorkspaceID(r.sourceWS.String), Branch: r.branch.String,
		Detach: DetachedMergeID(r.detach.String), DetachName: r.name.String, Pick: r.pick.String, CompactAfter: r.compact,
		CloseOnLanding: WorkspaceID(r.closes.String), CleanupWorktree: r.cleanup, CleanupDir: r.cleanDir.String,
		DetachMaker: WorkspaceID(r.maker.String), DetachTargetMaker: r.toMaker,
		Front: r.front, Behind: WorkspaceID(r.behind.String), PickStartedAt: optTime(r.picked), PickTurn: TurnID(r.pickTurn.String),
	}
	version, err := scanVersion(r.verPart, r.verLine)
	if err != nil {
		return MergeSource{}, &DecodeError{Table: table, Row: row, Field: "version", Err: err}
	}
	src.Version = version
	if src.Hooks.PreDeploy, err = decodeHooks(table, row, "source_pre_deploy_hooks", r.before); err != nil {
		return MergeSource{}, err
	}
	if src.Hooks.PostDeploy, err = decodeHooks(table, row, "source_post_deploy_hooks", r.after); err != nil {
		return MergeSource{}, err
	}
	if err := src.validate(); err != nil {
		return MergeSource{}, &DecodeError{Table: table, Row: row, Field: "source", Err: err}
	}
	return src, nil
}

// RequeuePlacement is where a requeued merge joins its queue: at the back
// (the zero value), at the front, or behind a merge in line (Behind, the
// merge's workspace, and BehindName, the name it was asked behind by).
type RequeuePlacement struct {
	Front      bool
	Behind     WorkspaceID
	BehindName string
}

// ErrOffQueueVersionRecorded refuses a requeue that supplies a version
// increment for a request that recorded its own.
var ErrOffQueueVersionRecorded = errors.New("wsm: the request recorded its own version increment")

// RequeueOffQueueMerge puts workspace ID's request off its queue back on
// REPO's queue as itself, REQUESTED (the caller puts it in line with
// QueueMerge), at PLACE, in ONE transaction: the record is deleted and the
// request row written in its place (insertRequest), so the request stands in
// exactly one place at every instant. VERSION, when set, is the increment of
// an ADOPTED request, which recorded none; supplying one for any other request
// is ErrOffQueueVersionRecorded. MODEL, when set, is recorded as the session's
// model in the same transaction (SetSessionModel's write), so the session's
// next start resumes on it; a workspace with no session row refuses it. A
// workspace with no record is ErrNotFound;
// one with a queue row already is MergeQueuedError, as a second request is.
// It answers the request as it was put back.
func (s *store) RequeueOffQueueMerge(ctx context.Context, repo RepoKey, id WorkspaceID, place RequeuePlacement, version *VersionIncrement, model string, at time.Time) (MergeSource, error) {
	const op = "daemon.wsm.requeue_off_queue_merge"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return MergeSource{}, err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "front": place.Front, "behind": string(place.Behind), "version_supplied": version != nil, "model": model}
	var source MergeSource
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		if err := refuseQueued(ctx, tx, key, id); err != nil {
			return err
		}
		loaded, err := scanOffQueue(ctx, tx, `WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		if len(loaded) == 0 {
			return fmt.Errorf("wsm: requeue of workspace %s: no request stands off its queue: %w", id, ErrNotFound)
		}
		rec := loaded[0]
		source = rec.Source
		if version != nil {
			if !rec.Adopted {
				return ErrOffQueueVersionRecorded
			}
			source.Version = version
		}
		source.Front, source.Behind, source.BehindName = place.Front, place.Behind, place.BehindName
		if err := source.validate(); err != nil {
			return fmt.Errorf("wsm: requeue of workspace %s: %w", id, err)
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM off_queue_merges WHERE workspace_id = ?`, id); err != nil {
			return err
		}
		if model != "" {
			if err := setSessionModelInTx(ctx, tx, id, model); err != nil {
				return err
			}
		}
		return insertRequest(ctx, tx, key, id, source, rec.RequestedAt)
	})
	if err != nil {
		return MergeSource{}, err
	}
	s.log.Info(op, "put a request that stood off its queue back on it, as itself", fields)
	return source, nil
}

// refuseQueued refuses, with the place it holds, a workspace that already
// stands on KEY's queue.
func refuseQueued(ctx context.Context, tx *sql.Tx, key RepoKey, id WorkspaceID) error {
	var (
		seq   int64
		state int64
	)
	err := tx.QueryRowContext(ctx, `SELECT seq, state FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, id).Scan(&seq, &state)
	if errors.Is(err, sql.ErrNoRows) {
		return nil
	}
	if err != nil {
		return err
	}
	existing, err := positionOf(ctx, tx, key, seq)
	if err != nil {
		return err
	}
	return &MergeQueuedError{Repo: key, Workspace: id, Position: existing, State: MergeQueueState(state)}
}

// DiscardOffQueueMerge ends workspace ID's request off its queue without
// landing it: the record goes, and nothing else. A workspace with no record
// is ErrNotFound. An async merge's discard closes its merge workspace instead
// (markClosedInTx deletes the record with the close), so this is a workspace's
// own merge's discard.
func (s *store) DiscardOffQueueMerge(ctx context.Context, id WorkspaceID) error {
	const op = "daemon.wsm.discard_off_queue_merge"
	fields := dlog.Context{"workspace": string(id)}
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM off_queue_merges WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		touched, err := touchedRow(res)
		if err != nil {
			return err
		}
		if !touched {
			return fmt.Errorf("wsm: discard of workspace %s: no request stands off its queue: %w", id, ErrNotFound)
		}
		return nil
	})
	if err != nil {
		return err
	}
	s.log.Info(op, "discarded a request that stood off its queue", fields)
	return nil
}

// dropOffQueueInTx deletes a closing workspace's request off its queue, if it
// holds one: a request never outlives its workspace's close.
func dropOffQueueInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID) (bool, error) {
	res, err := tx.ExecContext(ctx, `DELETE FROM off_queue_merges WHERE workspace_id = ?`, id)
	if err != nil {
		return false, err
	}
	return touchedRow(res)
}

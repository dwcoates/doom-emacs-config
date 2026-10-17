package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
)

// This file is the durable half of SYNC AND ASYNC MERGES
// (docs/protobuf-design/sync-async-merges.md): whether a merge workspace has
// been revealed, the name an async request reserves and the pick it may
// carry, and the mirror a merge workspace's turn is also drawn at.

// syncAsyncMergesDDL adds:
//
//   - merge_queue.source_detach_name: an async request's merge workspace name,
//     reserved while the request stands. Every async request an earlier build
//     recorded is given the name that build would have forked under
//     ("<branch>-merge"), so a request recorded across the upgrade still forks.
//   - merge_queue.source_pick, merge_queue.pick_started_at and
//     merge_queue.pick_turn: the pick instruction an async request (and then
//     its merge workspace's request) carries, and when the merge workspace's
//     pick turn began and which turn it is. NULL on every existing row, none
//     of which picks.
//   - workspaces.merge_revealed: whether a merge workspace has been revealed.
//     Every existing merge workspace was drawn like any workspace, so each is
//     migrated as revealed.
//   - turns.mirror: the mirror a turn of a merge workspace is also drawn at
//     (MirrorAddress). NULL on every existing turn.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ; on a fresh file both UPDATEs touch nothing.
const syncAsyncMergesDDL = `
ALTER TABLE merge_queue ADD COLUMN source_detach_name TEXT;
ALTER TABLE merge_queue ADD COLUMN source_pick TEXT;
ALTER TABLE merge_queue ADD COLUMN pick_started_at INTEGER;
ALTER TABLE merge_queue ADD COLUMN pick_turn TEXT;
UPDATE merge_queue SET source_detach_name = COALESCE(source_branch, workspace_id) || '-merge'
 WHERE source_detach_id IS NOT NULL;
ALTER TABLE workspaces ADD COLUMN merge_revealed INTEGER NOT NULL DEFAULT 0;
UPDATE workspaces SET merge_revealed = 1 WHERE merge_of IS NOT NULL;
ALTER TABLE turns ADD COLUMN mirror TEXT;
`

// MergeHidden reports whether the workspace is a HIDDEN merge workspace: an
// async merge's workspace that has been neither revealed nor landed. It is
// drawn in no tab bar and no sidebar section, and selecting it reveals it. A
// landed merge workspace is an ordinary closed workspace whatever its revealed
// flag, and every other workspace is never hidden.
func (ws Workspace) MergeHidden() bool {
	return ws.MergeOf != nil && !ws.MergeRevealed && ws.MergedAt == nil
}

// RevealMergeWorkspace records that merge workspace ID has been revealed, and
// answers whether this call revealed it (false when it already was). Revealing
// is one-way. A workspace that is no merge workspace is refused: only a merge
// workspace is ever hidden, so a caller revealing another holds a wrong id.
func (s *store) RevealMergeWorkspace(ctx context.Context, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.reveal_merge_workspace"
	fields := dlog.Context{"workspace": string(id)}
	revealed := false
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var (
			mergeOf sql.NullString
			already bool
		)
		if err := scanWorkspaceRow(ctx, tx, id, "merge_of, merge_revealed", &mergeOf, &already); err != nil {
			return err
		}
		if !mergeOf.Valid {
			return fmt.Errorf("wsm: reveal merge workspace %s: the workspace is no merge workspace", id)
		}
		if already {
			return nil
		}
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET merge_revealed = 1 WHERE id = ?`, id); err != nil {
			return err
		}
		revealed = true
		return nil
	})
	return revealed, err
}

// ReservedMergeNames answers every merge workspace name an async request
// standing in any queue has reserved, each mapped to the workspace whose
// request reserved it. Every name allocation reads it, so nothing takes a
// reserved name before its merge workspace is created under it.
func (s *store) ReservedMergeNames(ctx context.Context) (map[string]WorkspaceID, error) {
	const op = "daemon.wsm.reserved_merge_names"
	out := map[string]WorkspaceID{}
	err := s.read(ctx, op, dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT source_detach_name, workspace_id FROM merge_queue WHERE source_detach_name IS NOT NULL`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				name string
				ws   WorkspaceID
			)
			if err := rows.Scan(&name, &ws); err != nil {
				return err
			}
			out[name] = ws
		}
		return rows.Err()
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// StartMergePick records that merge workspace ID's pick began at AT as TURN,
// on its REQUESTED queue row, together with the bubble's ledger identity: the
// pick tab is the bubble's first content, so the bubble exists from here and a
// restart draws the same one and reattaches to the same turn. It is written
// BEFORE the turn is submitted. A row that is not a requested pick that has
// not begun is refused: the caller lost track of it.
func (s *store) StartMergePick(ctx context.Context, repo RepoKey, id WorkspaceID, ledger LeaseID, turn TurnID, at time.Time) error {
	const op = "daemon.wsm.start_merge_pick"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "ledger": string(ledger), "turn": string(turn), "started_at": at}
	if ledger == "" || turn == "" {
		err := errors.New("wsm: a pick starts with its bubble's ledger identity and its turn")
		s.log.Error(op, "refused to start a pick with no ledger identity or no turn", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE merge_queue SET pick_started_at = ?, pick_turn = ?, ledger_id = ?
			  WHERE repo_key = ? AND workspace_id = ? AND state IN (?, ?) AND source_pick IS NOT NULL AND pick_started_at IS NULL`,
			nanos(at), turn, ledger, key, id, int(MergeRequested), int(MergeQueued))
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: requested pick of workspace %s that has not started", id))
	})
}

// MirrorAddress is where a turn of one workspace is ALSO drawn, in another
// workspace's feed: an async merge workspace's merge turns are drawn in its
// own feed as ordinary turns and mirrored, re-identified, into the bubble's
// agentic tab in the requester's feed. The feed resolver writes the mirrored
// rows; this is only where they go.
type MirrorAddress struct {
	// Workspace is the workspace whose feed also draws the turn.
	Workspace WorkspaceID
	// Feed is the feed within that workspace the mirrored rows land in.
	Feed feedid.Feed
	// Parent is the row the turn's top-level rows nest under there.
	Parent feedid.Ref
}

// storedMirror is a mirror address's persisted form.
type storedMirror struct {
	// WS is the workspace whose feed also draws the turn.
	WS string `json:"ws"`
	// Feed is the feed within it.
	Feed storedFeed `json:"feed"`
	// Parent is the row the turn's top-level rows nest under.
	Parent storedRef `json:"parent"`
}

// encodeMirror renders an optional mirror address for its nullable column.
func encodeMirror(m *MirrorAddress) (any, error) {
	if m == nil {
		return nil, nil
	}
	if m.Workspace == "" {
		return nil, errors.New("wsm: a mirror address names its workspace")
	}
	feed, err := toStoredFeed(m.Feed)
	if err != nil {
		return nil, err
	}
	parent, err := toStoredRef(m.Parent)
	if err != nil {
		return nil, err
	}
	raw, err := json.Marshal(storedMirror{WS: string(m.Workspace), Feed: feed, Parent: parent})
	if err != nil {
		return nil, fmt.Errorf("wsm: encode mirror address: %w", err)
	}
	return string(raw), nil
}

// decodeMirror parses a mirror address from its column.
func decodeMirror(table, row string, raw string) (*MirrorAddress, error) {
	var stored storedMirror
	if err := json.Unmarshal([]byte(raw), &stored); err != nil {
		return nil, &DecodeError{Table: table, Row: row, Field: "mirror", Err: err}
	}
	if stored.WS == "" {
		return nil, &DecodeError{Table: table, Row: row, Field: "mirror", Err: errors.New("a mirror address names its workspace")}
	}
	feed, err := fromStoredFeed(table, row, stored.Feed)
	if err != nil {
		return nil, err
	}
	parent, err := fromStoredRef(table, row, stored.Parent)
	if err != nil {
		return nil, err
	}
	return &MirrorAddress{Workspace: WorkspaceID(stored.WS), Feed: feed, Parent: parent}, nil
}

// TurnMirrors answers the recorded mirror address of each named turn of one
// workspace that has one, all-or-nothing; a turn with none, or one this
// workspace never recorded, is absent from the answer.
func (s *store) TurnMirrors(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]*MirrorAddress, error) {
	out := make(map[TurnID]*MirrorAddress, len(turns))
	if len(turns) == 0 {
		return out, nil
	}
	err := s.read(ctx, "daemon.wsm.turn_mirrors", dlog.Context{"workspace": string(id), "turns": len(turns)}, func(ctx context.Context) error {
		for _, turn := range turns {
			t, err := scanTurn(s.db().QueryRowContext(ctx, `SELECT `+turnColumns+` FROM turns WHERE id = ? AND workspace_id = ?`, turn, id))
			if errors.Is(err, sql.ErrNoRows) {
				continue
			}
			if err != nil {
				return err
			}
			if t.Mirror != nil {
				out[turn] = t.Mirror
			}
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}
